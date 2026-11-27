package com.soluling.ota;

import android.content.Context;

/**
 * Over-the-air translation updates from Soluling Cloud.
 *
 * <p>When you translate in Soluling Cloud and publish the project's files, the application gets the
 * new translations without a new release. Once a day, the library reads the project's manifest from
 * Soluling's CDN, downloads the changed resource files ({@code values-fi/strings.xml}) of the
 * device's language, and keeps them in the application's files folder. The strings of the
 * downloaded files are used instead of the strings built into the application; a string that the
 * files don't have, or whose placeholders differ from the built-in one, comes from the application
 * as before. Without a connection, or before the first download, the built-in strings are used.
 *
 * <p>Initialize once in {@code Application.onCreate}, and wrap the context of each activity:
 *
 * <pre>
 * class App : Application() {
 *   override fun onCreate() {
 *     super.onCreate()
 *     SolulingOta.initialize(this, "project-guid")
 *   }
 * }
 *
 * class MainActivity : AppCompatActivity() {
 *   override fun attachBaseContext(base: Context) {
 *     super.attachBaseContext(SolulingOta.wrap(base))
 *   }
 * }
 * </pre>
 *
 * <p>The wrapped context returns the downloaded strings from {@code getString},
 * {@code getQuantityString}, {@code getStringArray}, Jetpack Compose's {@code stringResource}, and
 * the {@code android:text}, {@code android:hint}, and {@code android:contentDescription} attributes
 * of layouts. Menus, the application's label, and other attributes keep the built-in strings.
 * Downloaded strings are used from the next time a string is read; texts already on the screen
 * don't change, so use {@link Options#setListener} to recreate the activity if you want them to.
 */
public final class SolulingOta
{
  /** The published files on Soluling's CDN, where the manifest is read first */
  public static final String DEFAULT_CDN_URL = "https://www.soluling.com/artifacts";

  /** Soluling's API, for the manifest when the CDN doesn't have it */
  public static final String DEFAULT_API_URL = "https://www.soluling.com/api";

  /** Milliseconds between the checks for newer translations: a day */
  public static final long DEFAULT_CHECK_INTERVAL = 24L * 60 * 60 * 1000;

  /** Milliseconds to wait for Soluling's servers */
  public static final int DEFAULT_TIMEOUT = 10000;

  /** The result of a check for newer translations */
  public enum Result
  {
    /** Not checked: the last check is more recent than the check interval, or not initialized */
    NOT_CHECKED,
    /** Checked, and the downloaded translations are the latest */
    UP_TO_DATE,
    /** Checked, and new translations were downloaded, or the published ones were removed */
    UPDATED,
    /** Soluling's servers could not be reached, or a file could not be downloaded */
    FAILED
  }

  /** Called on the main thread when a check has downloaded new translations */
  public interface Listener
  {
    /**
     * @param language the language of the downloaded translations, such as "fi", or null if the
     *                 published translations of the language were removed
     */
    void onUpdated(String language);
  }

  /** Called on the main thread when a check started by {@link #update(boolean, Callback)} ends */
  public interface Callback
  {
    void onDone(Result result);
  }

  /** Chooses the published files that are the application's, when the project has others too */
  public interface FileFilter
  {
    boolean accept(String fileName);
  }

  /** The options of {@link #initialize(Context, Options)} */
  public static final class Options
  {
    final String projectGuid;
    String cdnUrl = DEFAULT_CDN_URL;
    String apiUrl = DEFAULT_API_URL;
    long checkInterval = DEFAULT_CHECK_INTERVAL;
    int timeout = DEFAULT_TIMEOUT;
    String language;
    FileFilter fileFilter = OtaStore::isValuesFile;
    Listener listener;

    /** @param projectGuid the GUID of the Soluling Cloud project, from its Manage page */
    public Options(String projectGuid)
    {
      if ((projectGuid == null) || projectGuid.trim().isEmpty())
        throw new IllegalArgumentException("The project GUID is missing");

      this.projectGuid = projectGuid.trim();
    }

    /** The published files on Soluling's CDN. Default {@link #DEFAULT_CDN_URL}; null reads the manifest from the API only */
    public Options setCdnUrl(String value)
    {
      cdnUrl = trimSlash(value);
      return this;
    }

    /** Soluling's API, for the manifest when the CDN doesn't have it. Default {@link #DEFAULT_API_URL} */
    public Options setApiUrl(String value)
    {
      apiUrl = trimSlash(value);
      return this;
    }

    /** Milliseconds between the checks. Default a day; 0 checks every time the application starts */
    public Options setCheckInterval(long value)
    {
      checkInterval = Math.max(0, value);
      return this;
    }

    /** Milliseconds to wait for Soluling's servers. Default 10 seconds */
    public Options setTimeout(int value)
    {
      timeout = Math.max(1, value);
      return this;
    }

    /**
     * The language to download, such as "fi". Default null: the language of the application's
     * configuration, which follows the device and the per-app language. Set it if the application
     * chooses its language itself.
     */
    public Options setLanguage(String value)
    {
      language = ((value == null) || value.trim().isEmpty()) ? null : value.trim();
      return this;
    }

    /** Which published files to download. Default: the .xml files in a values folder, such as values-fi/strings.xml */
    public Options setFileFilter(FileFilter value)
    {
      fileFilter = (value != null) ? value : OtaStore::isValuesFile;
      return this;
    }

    /** Called on the main thread when new translations were downloaded */
    public Options setListener(Listener value)
    {
      listener = value;
      return this;
    }

    private static String trimSlash(String value)
    {
      if ((value == null) || value.trim().isEmpty())
        return null;

      value = value.trim();

      while (value.endsWith("/"))
        value = value.substring(0, value.length() - 1);

      return value;
    }
  }

  private static volatile OtaStore store;

  private SolulingOta()
  {
  }

  /**
   * Starts using the downloaded translations, and checks for newer ones in the background if the
   * last check is older than a day. Call once, in {@code Application.onCreate}.
   *
   * @param projectGuid the GUID of the Soluling Cloud project, from its Manage page
   */
  public static void initialize(Context context, String projectGuid)
  {
    initialize(context, new Options(projectGuid));
  }

  /** Starts using the downloaded translations with the given options. Calls after the first are ignored. */
  public static synchronized void initialize(Context context, Options options)
  {
    if (store != null)
      return;

    Context application = context.getApplicationContext();
    OtaStore newStore = new OtaStore((application != null) ? application : context, options);

    newStore.load();
    store = newStore;
    newStore.checkAsync(false, null);
  }

  /**
   * Returns a context whose resources and layout inflater use the downloaded translations. Call it
   * in {@code attachBaseContext} of each activity, and of the application if you read strings
   * through the application context.
   */
  public static Context wrap(Context base)
  {
    if ((base == null) || (base instanceof OtaContextWrapper))
      return base;

    return new OtaContextWrapper(base);
  }

  /**
   * Checks for newer translations in the background.
   *
   * @param force true checks even if the last check is more recent than the check interval
   */
  public static void update(boolean force)
  {
    update(force, null);
  }

  /** Checks for newer translations in the background, and calls callback on the main thread when done */
  public static void update(boolean force, Callback callback)
  {
    OtaStore current = store;

    if (current != null)
      current.checkAsync(force, callback);
    else if (callback != null)
      OtaStore.post(() -> callback.onDone(Result.NOT_CHECKED));
  }

  /** Deletes the downloaded translations, so the built-in ones are used until the next check */
  public static void clear()
  {
    OtaStore current = store;

    if (current != null)
      current.clear();
  }

  /** The result of the last check */
  public static Result getLastResult()
  {
    OtaStore current = store;
    return (current != null) ? current.getLastResult() : Result.NOT_CHECKED;
  }

  /** The language of the downloaded translations, such as "fi", or null if there are none */
  public static String getLanguage()
  {
    OtaStore current = store;
    return (current != null) ? current.getLanguage() : null;
  }

  static OtaStore store()
  {
    return store;
  }

  // For the tests: forget the initialization, so initialize can be called again
  static synchronized void reset()
  {
    store = null;
  }
}
