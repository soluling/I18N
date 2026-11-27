package com.soluling.ota;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * The downloaded translations: checks the project's manifest, downloads the changed files of one
 * language into the application's files folder (soluling-ota), and parses them for OtaResources.
 *
 * The manifest lists the project's published files with their language, hash, and CDN URL. A file
 * is saved by its hash, so a file with the same hash is never downloaded again. state.json records
 * the files in use, the language, and the time of the last check.
 */
final class OtaStore
{
  private static final String TAG = "SolulingOta";
  private static final String FOLDER = "soluling-ota";
  private static final String STATE = "state.json";

  private static final Handler MAIN = new Handler(Looper.getMainLooper());

  private final Context context;
  private final SolulingOta.Options options;
  private final File folder;
  private final ExecutorService executor = Executors.newSingleThreadExecutor(runnable ->
  {
    Thread thread = new Thread(runnable, "SolulingOta");
    thread.setDaemon(true);
    return thread;
  });

  // The files in use, replaced as a whole. strings is parsed from them the first time it is needed.
  private volatile State state = new State();
  private volatile OtaStrings strings;
  private volatile SolulingOta.Result lastResult = SolulingOta.Result.NOT_CHECKED;

  static final class State
  {
    long lastCheck;     // Milliseconds since the epoch, 0 if never
    String locale;      // The language that was checked: a language tag
    String language;    // The published language of the files, null if none
    final List<FileEntry> files = new ArrayList<>();
  }

  static final class FileEntry
  {
    String fileName;
    String hash;
  }

  // One published file of the manifest
  private static final class ManifestItem
  {
    String fileName;
    String language;
    String hash;
    String url;
  }

  OtaStore(Context context, SolulingOta.Options options)
  {
    this.context = context;
    this.options = options;
    this.folder = new File(context.getFilesDir(), FOLDER);
  }

  static void post(Runnable runnable)
  {
    MAIN.post(runnable);
  }

  /** The default file filter: the .xml files in a values folder (values-fi/strings.xml), or with no folder */
  static boolean isValuesFile(String fileName)
  {
    String name = fileName.replace('\\', '/').toLowerCase(Locale.ROOT);

    if (!name.endsWith(".xml"))
      return false;

    String[] parts = name.split("/");

    return (parts.length == 1) || parts[parts.length - 2].startsWith("values");
  }

  SolulingOta.Result getLastResult()
  {
    return lastResult;
  }

  String getLanguage()
  {
    return state.language;
  }

  /** Reads the state of the earlier downloads */
  synchronized void load()
  {
    File file = new File(folder, STATE);

    if (!file.exists())
      return;

    try
    {
      JSONObject json = new JSONObject(readText(new FileInputStream(file)));
      State loaded = new State();

      loaded.lastCheck = json.optLong("lastCheck");
      loaded.locale = json.optString("locale", null);
      loaded.language = json.optString("language", null);

      JSONArray files = json.optJSONArray("files");

      if (files != null)
      {
        for (int i = 0; i < files.length(); i++)
        {
          JSONObject item = files.getJSONObject(i);
          FileEntry entry = new FileEntry();

          entry.fileName = item.getString("fileName");
          entry.hash = item.getString("hash");
          loaded.files.add(entry);
        }
      }

      state = loaded;
      strings = null;
    }
    catch (IOException | JSONException e)
    {
      Log.w(TAG, "Cannot read the downloaded translations", e);
    }
  }

  /**
   * The downloaded strings to use for the locale: null if no file has been downloaded for its
   * language, a parent language of it (fi for fi-FI), or for another language.
   */
  OtaStrings getStrings(Locale locale)
  {
    State current = state;

    if ((current.language == null) || !candidates(locale.toLanguageTag()).contains(normalize(current.language)))
      return null;

    OtaStrings result = strings;

    if (result == null)
    {
      synchronized (this)
      {
        if ((strings == null) && (state == current))
          strings = parse(current);

        result = strings;
      }
    }

    return result;
  }

  private OtaStrings parse(State current)
  {
    OtaStrings result = new OtaStrings();

    for (FileEntry entry : current.files)
    {
      try (InputStream stream = new FileInputStream(file(entry.hash)))
      {
        result.parse(stream);
      }
      catch (Exception e)
      {
        Log.w(TAG, "Cannot read " + entry.fileName, e);
      }
    }

    return result;
  }

  void checkAsync(boolean force, SolulingOta.Callback callback)
  {
    executor.execute(() ->
    {
      SolulingOta.Result result = check(force);

      if (callback != null)
        post(() -> callback.onDone(result));
    });
  }

  // Runs on the background thread
  private SolulingOta.Result check(boolean force)
  {
    String locale = (options.language != null) ? options.language : context.getResources().getConfiguration().getLocales().get(0).toLanguageTag();
    State current = state;
    long now = System.currentTimeMillis();

    if (!force && (current.lastCheck > 0) && (now >= current.lastCheck) && (now - current.lastCheck < options.checkInterval) && locale.equalsIgnoreCase(current.locale))
      return SolulingOta.Result.NOT_CHECKED;

    SolulingOta.Result result;

    try
    {
      result = download(locale, now) ? SolulingOta.Result.UPDATED : SolulingOta.Result.UP_TO_DATE;
    }
    catch (IOException | JSONException e)
    {
      Log.w(TAG, "Cannot check for new translations", e);
      result = SolulingOta.Result.FAILED;
    }

    lastResult = result;

    if ((result == SolulingOta.Result.UPDATED) && (options.listener != null))
    {
      String language = state.language;
      post(() -> options.listener.onUpdated(language));
    }

    return result;
  }

  // Downloads the changed files of the best published language for the locale. True if the files in
  // use changed.
  private boolean download(String locale, long now) throws IOException, JSONException
  {
    List<ManifestItem> manifest = readManifest();
    List<ManifestItem> chosen = new ArrayList<>();
    String language = null;

    for (String candidate : candidates(locale))
    {
      for (ManifestItem item : manifest)
        if ((item.language != null) && normalize(item.language).equals(candidate) && options.fileFilter.accept(item.fileName))
          chosen.add(item);

      if (!chosen.isEmpty())
      {
        language = chosen.get(0).language;
        break;
      }
    }

    if (!folder.exists() && !folder.mkdirs())
      throw new IOException("Cannot create " + folder);

    State updated = new State();
    updated.lastCheck = now;
    updated.locale = locale;
    updated.language = language;

    for (ManifestItem item : chosen)
    {
      File file = file(item.hash);

      if (!file.exists())
        downloadFile(item, file);

      FileEntry entry = new FileEntry();
      entry.fileName = item.fileName;
      entry.hash = item.hash;
      updated.files.add(entry);
    }

    State current = state;
    boolean changed = !hashes(current).equals(hashes(updated));

    saveState(updated);

    synchronized (this)
    {
      state = updated;

      if (changed)
        strings = null;
    }

    deleteUnused(updated);
    return changed;
  }

  private static Set<String> hashes(State value)
  {
    Set<String> result = new HashSet<>();

    for (FileEntry entry : value.files)
      result.add(entry.hash);

    return result;
  }

  // The static manifest on the CDN first - it costs nothing on Soluling's servers however many
  // applications check it - and the API's if the CDN doesn't have it yet
  private List<ManifestItem> readManifest() throws IOException, JSONException
  {
    String text = null;

    if (options.cdnUrl != null)
    {
      try
      {
        text = fetchText(options.cdnUrl + "/" + options.projectGuid + "/manifest.json");
      }
      catch (IOException e)
      {
        Log.d(TAG, "No manifest on the CDN", e);
      }
    }

    if ((text == null) && (options.apiUrl != null))
      text = fetchText(options.apiUrl + "/artifacts/" + options.projectGuid + "/manifest");

    if (text == null)
      throw new IOException("No manifest");

    JSONArray items = new JSONArray(text);
    List<ManifestItem> result = new ArrayList<>();

    for (int i = 0; i < items.length(); i++)
    {
      JSONObject json = items.getJSONObject(i);
      ManifestItem item = new ManifestItem();

      item.fileName = json.optString("fileName", "");
      item.language = json.isNull("language") ? null : json.optString("language", null);
      item.hash = json.optString("hash", "");
      item.url = json.optString("url", "");

      if (!item.hash.isEmpty() && !item.url.isEmpty())
        result.add(item);
    }

    return result;
  }

  // The body of a 200 response, null for 404, and an exception for other errors
  private String fetchText(String url) throws IOException
  {
    HttpURLConnection connection = open(url);

    try
    {
      int status = connection.getResponseCode();

      if (status == HttpURLConnection.HTTP_NOT_FOUND)
        return null;

      if (status != HttpURLConnection.HTTP_OK)
        throw new IOException(url + ": HTTP " + status);

      return readText(connection.getInputStream());
    }
    finally
    {
      connection.disconnect();
    }
  }

  // Downloads a file next to its final name, checks that it can be parsed, and renames it
  private void downloadFile(ManifestItem item, File file) throws IOException
  {
    File temporary = new File(folder, file.getName() + ".tmp");
    HttpURLConnection connection = open(item.url);

    try
    {
      int status = connection.getResponseCode();

      if (status != HttpURLConnection.HTTP_OK)
        throw new IOException(item.url + ": HTTP " + status);

      try (InputStream input = connection.getInputStream(); OutputStream output = new FileOutputStream(temporary))
      {
        byte[] buffer = new byte[16384];
        int count;

        while ((count = input.read(buffer)) > 0)
          output.write(buffer, 0, count);
      }
    }
    finally
    {
      connection.disconnect();
    }

    try (InputStream stream = new FileInputStream(temporary))
    {
      new OtaStrings().parse(stream);
    }
    catch (Exception e)
    {
      temporary.delete();
      throw new IOException(item.fileName + " is not a valid resource file", e);
    }

    if (!temporary.renameTo(file))
    {
      temporary.delete();
      throw new IOException("Cannot save " + file);
    }
  }

  private HttpURLConnection open(String url) throws IOException
  {
    HttpURLConnection connection = (HttpURLConnection) new URL(url).openConnection();

    connection.setConnectTimeout(options.timeout);
    connection.setReadTimeout(options.timeout);
    connection.setInstanceFollowRedirects(true);
    return connection;
  }

  private void saveState(State value) throws IOException, JSONException
  {
    JSONObject json = new JSONObject();
    JSONArray files = new JSONArray();

    for (FileEntry entry : value.files)
    {
      JSONObject item = new JSONObject();
      item.put("fileName", entry.fileName);
      item.put("hash", entry.hash);
      files.put(item);
    }

    json.put("lastCheck", value.lastCheck);
    json.put("locale", value.locale);
    json.put("language", (value.language != null) ? value.language : JSONObject.NULL);
    json.put("files", files);

    File temporary = new File(folder, STATE + ".tmp");

    try (OutputStream output = new FileOutputStream(temporary))
    {
      output.write(json.toString().getBytes(StandardCharsets.UTF_8));
    }

    if (!temporary.renameTo(new File(folder, STATE)))
      throw new IOException("Cannot save the state");
  }

  private void deleteUnused(State value)
  {
    Set<String> used = new HashSet<>();

    for (FileEntry entry : value.files)
      used.add(file(entry.hash).getName());

    File[] files = folder.listFiles();

    if (files == null)
      return;

    for (File file : files)
      if (file.getName().endsWith(".xml") && !used.contains(file.getName()))
        file.delete();
  }

  /** Deletes the downloaded files, so the built-in strings are used until the next check */
  void clear()
  {
    executor.execute(() ->
    {
      synchronized (this)
      {
        state = new State();
        strings = null;
      }

      File[] files = folder.listFiles();

      if (files != null)
        for (File file : files)
          file.delete();
    });
  }

  // The file of a hash. A hash may have characters that a file name can't.
  private File file(String hash)
  {
    return new File(folder, hash.replaceAll("[^A-Za-z0-9_-]", "_") + ".xml");
  }

  private static String normalize(String language)
  {
    return language.replace('_', '-').toLowerCase(Locale.ROOT);
  }

  /** The language and its parents, most specific first: zh-Hant-TW, zh-Hant, zh-TW, zh */
  static List<String> candidates(String languageTag)
  {
    Locale locale = Locale.forLanguageTag(languageTag.replace('_', '-'));
    Set<String> result = new LinkedHashSet<>();
    String language = locale.getLanguage();
    String script = locale.getScript();
    String country = locale.getCountry();

    result.add(normalize(locale.toLanguageTag()));

    if (!script.isEmpty())
      result.add(normalize(language + "-" + script));

    if (!country.isEmpty())
      result.add(normalize(language + "-" + country));

    if (!language.isEmpty())
      result.add(normalize(language));

    return new ArrayList<>(result);
  }

  private static String readText(InputStream stream) throws IOException
  {
    try (InputStream input = stream)
    {
      ByteArrayOutputStream output = new ByteArrayOutputStream();
      byte[] buffer = new byte[16384];
      int count;

      while ((count = input.read(buffer)) > 0)
        output.write(buffer, 0, count);

      return output.toString("UTF-8");
    }
  }
}
