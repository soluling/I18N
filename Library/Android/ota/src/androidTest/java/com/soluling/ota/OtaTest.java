package com.soluling.ota;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import android.content.Context;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.test.ext.junit.runners.AndroidJUnit4;
import androidx.test.platform.app.InstrumentationRegistry;

import com.soluling.ota.test.R;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.io.File;
import java.util.Arrays;
import java.util.Locale;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

@RunWith(AndroidJUnit4.class)
public class OtaTest
{
  private static final String GUID = "11111111-2222-3333-4444-555555555555";
  private static final String CDN_MANIFEST = "/artifacts/" + GUID + "/manifest.json";
  private static final String API_MANIFEST = "/api/artifacts/" + GUID + "/manifest";
  private static final String FI_FILE = "/artifacts/12/h1/values-fi/strings.xml";
  private static final String FI_FILE2 = "/artifacts/12/h2/values-fi/strings.xml";
  private static final String DE_FILE = "/artifacts/12/h3/values-de/strings.xml";
  private static final String LAYOUT_FILE = "/artifacts/12/h4/layout-fi/main.xml";

  private static final String FINNISH =
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
    "<resources xmlns:xliff=\"urn:oasis:names:tc:xliff:document:1.2\">\n" +
    "  <string name=\"hello\">Moi maailma</string>\n" +
    "  <string name=\"time\">Ajoaika on <xliff:g id=\"hours\">%1$s</xliff:g> ja %2$s</string>\n" +
    "  <string name=\"greeting\">Hei %2$s</string>\n" +
    "  <string name=\"escaped\">\\\"Lainaus\\\" ja \\'heittomerkki\\'\\nrivi</string>\n" +
    "  <string name=\"styled\">Lihavoitu <b>teksti</b></string>\n" +
    "  <plurals name=\"files\">\n" +
    "    <item quantity=\"one\">%d tiedosto</item>\n" +
    "    <item quantity=\"other\">%d tiedostoa</item>\n" +
    "  </plurals>\n" +
    "  <string-array name=\"days\">\n" +
    "    <item>maanantai</item>\n" +
    "    <item>tiistai</item>\n" +
    "  </string-array>\n" +
    "  <string-array name=\"colors\">\n" +
    "    <item>punainen</item>\n" +
    "    <item>vihreä</item>\n" +
    "    <item>sininen</item>\n" +
    "  </string-array>\n" +
    "</resources>\n";

  private TestServer server;
  private Context context;

  @Before
  public void setUp() throws Exception
  {
    SolulingOta.reset();
    context = InstrumentationRegistry.getInstrumentation().getContext();
    deleteFolder(new File(context.getApplicationContext().getFilesDir(), "soluling-ota"));

    server = new TestServer();
    server.put(CDN_MANIFEST, 200, manifest("h1", FI_FILE));
    server.put(FI_FILE, 200, FINNISH);
    server.put(DE_FILE, 200, "<resources><string name=\"hello\">Hallo Welt</string></resources>");
    server.put(LAYOUT_FILE, 500, "Must not be downloaded");
  }

  @After
  public void tearDown() throws Exception
  {
    server.close();
    SolulingOta.reset();
  }

  private String manifest(String finnishHash, String finnishPath)
  {
    return "[" +
      item("layout-fi/main.xml", "fi", "h4", LAYOUT_FILE) + "," +
      item("values-de/strings.xml", "de", "h3", DE_FILE) + "," +
      item("values-fi/strings.xml", "fi", finnishHash, finnishPath) + "," +
      "{\"fileName\":\"README.md\",\"language\":null,\"hash\":\"h5\",\"size\":null,\"url\":\"" + server.url() + "/artifacts/12/h5/README.md\"}" +
      "]";
  }

  private String item(String fileName, String language, String hash, String path)
  {
    return "{\"fileName\":\"" + fileName + "\",\"language\":\"" + language + "\",\"hash\":\"" + hash + "\",\"size\":100,\"url\":\"" + server.url() + path + "\"}";
  }

  private SolulingOta.Options options()
  {
    return new SolulingOta.Options(GUID)
      .setCdnUrl(server.url() + "/artifacts")
      .setApiUrl(server.url() + "/api/")
      .setLanguage("fi")
      .setTimeout(5000);
  }

  // Initializes, and waits for the check that initialize starts
  private SolulingOta.Result initialize(SolulingOta.Options options) throws InterruptedException
  {
    SolulingOta.initialize(context, options);
    update(false);
    return SolulingOta.getLastResult();
  }

  private static SolulingOta.Result update(boolean force) throws InterruptedException
  {
    CountDownLatch latch = new CountDownLatch(1);
    AtomicReference<SolulingOta.Result> result = new AtomicReference<>();

    SolulingOta.update(force, value ->
    {
      result.set(value);
      latch.countDown();
    });

    assertTrue("The check did not end", latch.await(20, TimeUnit.SECONDS));
    return result.get();
  }

  private Context localized(String languageTag)
  {
    Configuration configuration = new Configuration(context.getResources().getConfiguration());
    configuration.setLocale(Locale.forLanguageTag(languageTag));
    return SolulingOta.wrap(context.createConfigurationContext(configuration));
  }

  private static void deleteFolder(File folder)
  {
    File[] files = folder.listFiles();

    if (files != null)
      for (File file : files)
        file.delete();

    folder.delete();
  }

  @Test
  public void notInitialized_UsesTheBuiltInStrings()
  {
    Resources resources = localized("fi").getResources();

    assertEquals("Hello World", resources.getString(R.string.hello));
    assertEquals(SolulingOta.Result.NOT_CHECKED, SolulingOta.getLastResult());
    assertNull(SolulingOta.getLanguage());
  }

  @Test
  public void downloadedStrings_ReplaceTheBuiltInOnes() throws Exception
  {
    assertEquals(SolulingOta.Result.UPDATED, initialize(options()));
    assertEquals("fi", SolulingOta.getLanguage());

    Resources resources = localized("fi").getResources();

    assertEquals("Moi maailma", resources.getString(R.string.hello));
    assertEquals("Moi maailma", resources.getText(R.string.hello).toString());
    assertEquals("Ajoaika on 2 tuntia ja 30 minuuttia", resources.getString(R.string.time, "2 tuntia", "30 minuuttia"));
    assertEquals("\"Lainaus\" ja 'heittomerkki'\nrivi", resources.getString(R.string.escaped));
    assertEquals("1 tiedosto", resources.getQuantityString(R.plurals.files, 1, 1));
    assertEquals("5 tiedostoa", resources.getQuantityString(R.plurals.files, 5, 5));
    assertArrayEquals(new String[] { "maanantai", "tiistai" }, resources.getStringArray(R.array.days));

    // Not in the file
    assertEquals("Untouched", resources.getString(R.string.untouched));

    // Rejected: other placeholders, styling, and another number of items than the built-in ones
    assertEquals("Hello Maija", resources.getString(R.string.greeting, "Maija"));
    assertEquals("Normal", resources.getString(R.string.styled));
    assertArrayEquals(new String[] { "Red", "Green" }, resources.getStringArray(R.array.colors));

    // Android's own resources are never replaced
    assertEquals(context.getResources().getString(android.R.string.ok), resources.getString(android.R.string.ok));

    // Only the Finnish strings file: not the layout file, the German file, or the README
    assertEquals(1, server.count(FI_FILE));
    assertEquals(0, server.count(LAYOUT_FILE));
    assertEquals(0, server.count(DE_FILE));
  }

  @Test
  public void otherLanguages_UseTheBuiltInStrings() throws Exception
  {
    initialize(options());

    assertEquals("Hello World", localized("en").getResources().getString(R.string.hello));
    assertEquals("Hello World", localized("de").getResources().getString(R.string.hello));

    // A country of the language uses the language's file
    assertEquals("Moi maailma", localized("fi-FI").getResources().getString(R.string.hello));
  }

  @Test
  public void layouts_GetTheDownloadedStrings() throws Exception
  {
    initialize(options());

    Context localized = localized("fi");
    View root = LayoutInflater.from(localized).inflate(R.layout.ota_test, null);

    assertEquals("Moi maailma", ((TextView) root.findViewById(R.id.text)).getText().toString());
    assertEquals("Untouched", ((TextView) root.findViewById(R.id.text)).getHint().toString());
    assertEquals("Moi maailma", ((EditText) root.findViewById(R.id.edit)).getHint().toString());
    assertEquals("Moi maailma", ((CheckBox) root.findViewById(R.id.check)).getText().toString());
    assertEquals("Literal", ((TextView) root.findViewById(R.id.literal)).getText().toString());
    assertEquals("Moi maailma", root.findViewById(R.id.image).getContentDescription().toString());
    assertTrue(root.findViewById(R.id.image) instanceof ImageView);
  }

  // A view that a factory created, as AppCompat's creates AppCompatTextView
  public static class FactoryTextView extends TextView
  {
    public FactoryTextView(Context context, AttributeSet attrs)
    {
      super(context, attrs);
    }
  }

  @Test
  public void layouts_WithTheApplicationsFactory() throws Exception
  {
    initialize(options());

    Context localized = localized("fi");
    LayoutInflater inflater = LayoutInflater.from(localized).cloneInContext(localized);

    // AppCompat sets its factory only if the inflater has none
    assertNull(inflater.getFactory());

    inflater.setFactory2(new LayoutInflater.Factory2()
    {
      @Override
      public View onCreateView(View parent, String name, Context context, AttributeSet attrs)
      {
        return "TextView".equals(name) ? new FactoryTextView(context, attrs) : null;
      }

      @Override
      public View onCreateView(String name, Context context, AttributeSet attrs)
      {
        return onCreateView(null, name, context, attrs);
      }
    });

    View root = inflater.inflate(R.layout.ota_test, null);
    TextView text = root.findViewById(R.id.text);

    assertTrue(text instanceof FactoryTextView);
    assertEquals("Moi maailma", text.getText().toString());
    assertEquals("Moi maailma", ((CheckBox) root.findViewById(R.id.check)).getText().toString());

    // A fragment's inflater is a clone of the activity's: it has the factory, and translates
    View child = inflater.cloneInContext(localized).inflate(R.layout.ota_test, null);
    assertTrue(child.findViewById(R.id.text) instanceof FactoryTextView);
    assertEquals("Moi maailma", ((TextView) child.findViewById(R.id.text)).getText().toString());
  }

  @Test
  public void inflatersOwnFactory_IsNotCloned() throws Exception
  {
    initialize(options());

    Context localized = localized("fi");
    LayoutInflater inflater = LayoutInflater.from(localized);

    inflater.inflate(R.layout.ota_test, null);

    // inflate set a factory on this inflater, but a clone (an activity's) is left free for AppCompat's
    assertFalse(inflater.getFactory() == null);
    assertNull(inflater.cloneInContext(localized).getFactory());
  }

  @Test
  public void checkInterval_ChecksOnceADay() throws Exception
  {
    initialize(options());

    assertEquals(SolulingOta.Result.NOT_CHECKED, update(false));
    assertEquals(1, server.count(CDN_MANIFEST));

    // Forced: checks, but the file with the same hash is not downloaded again
    assertEquals(SolulingOta.Result.UP_TO_DATE, update(true));
    assertEquals(2, server.count(CDN_MANIFEST));
    assertEquals(1, server.count(FI_FILE));

    // Interval 0: every start and every update checks - here initialize's and the helper's
    SolulingOta.reset();
    assertEquals(SolulingOta.Result.UP_TO_DATE, initialize(options().setCheckInterval(0)));
    assertEquals(4, server.count(CDN_MANIFEST));
  }

  @Test
  public void noManifestOnTheCdn_ReadsTheApi() throws Exception
  {
    server.remove(CDN_MANIFEST);
    server.put(API_MANIFEST, 200, manifest("h1", FI_FILE));

    assertEquals(SolulingOta.Result.UPDATED, initialize(options()));
    assertEquals(1, server.count(CDN_MANIFEST));
    assertEquals(1, server.count(API_MANIFEST));
    assertEquals("Moi maailma", localized("fi").getResources().getString(R.string.hello));
  }

  @Test
  public void downloadedStrings_AreKeptForTheNextStart() throws Exception
  {
    initialize(options());

    SolulingOta.reset();
    server.setFailAll(true);

    // Checked a moment ago: no check, and the saved strings are used
    assertEquals(SolulingOta.Result.NOT_CHECKED, initialize(options()));
    assertEquals("Moi maailma", localized("fi").getResources().getString(R.string.hello));
  }

  @Test
  public void failedCheck_KeepsTheDownloadedStrings() throws Exception
  {
    initialize(options());
    server.setFailAll(true);

    assertEquals(SolulingOta.Result.FAILED, update(true));
    assertEquals(SolulingOta.Result.FAILED, SolulingOta.getLastResult());
    assertEquals("Moi maailma", localized("fi").getResources().getString(R.string.hello));
  }

  @Test
  public void noConnection_UsesTheBuiltInStrings() throws Exception
  {
    server.setFailAll(true);

    assertEquals(SolulingOta.Result.FAILED, initialize(options()));
    assertEquals("Hello World", localized("fi").getResources().getString(R.string.hello));
  }

  @Test
  public void newPublish_IsDownloaded() throws Exception
  {
    CountDownLatch updated = new CountDownLatch(2);
    AtomicReference<String> language = new AtomicReference<>();

    initialize(options().setListener(value ->
    {
      language.set(value);
      updated.countDown();
    }));

    server.put(CDN_MANIFEST, 200, manifest("h2", FI_FILE2));
    server.put(FI_FILE2, 200, FINNISH.replace("Moi maailma", "Hei maailma"));

    assertEquals(SolulingOta.Result.UPDATED, update(true));
    assertTrue(updated.await(5, TimeUnit.SECONDS));
    assertEquals("fi", language.get());
    assertEquals("Hei maailma", localized("fi").getResources().getString(R.string.hello));

    // The old file is deleted
    File folder = new File(context.getApplicationContext().getFilesDir(), "soluling-ota");
    assertFalse(new File(folder, "h1.xml").exists());
    assertTrue(new File(folder, "h2.xml").exists());
  }

  @Test
  public void unpublishedLanguage_UsesTheBuiltInStrings() throws Exception
  {
    initialize(options());
    server.put(CDN_MANIFEST, 200, "[]");

    assertEquals(SolulingOta.Result.UPDATED, update(true));
    assertNull(SolulingOta.getLanguage());
    assertEquals("Hello World", localized("fi").getResources().getString(R.string.hello));
  }

  @Test
  public void invalidFile_IsNotUsed() throws Exception
  {
    server.put(FI_FILE, 200, "<resources><string name=\"hello\">Broken");

    assertEquals(SolulingOta.Result.FAILED, initialize(options()));
    assertEquals("Hello World", localized("fi").getResources().getString(R.string.hello));
  }

  @Test
  public void clear_DeletesTheDownloadedStrings() throws Exception
  {
    initialize(options());
    SolulingOta.clear();

    // The clear runs on the same thread as the checks. The next check downloads the files again,
    // so this one fails.
    server.setFailAll(true);
    assertEquals(SolulingOta.Result.FAILED, update(false));
    assertNull(SolulingOta.getLanguage());
    assertEquals("Hello World", localized("fi").getResources().getString(R.string.hello));
  }

  @Test
  public void unescape_FollowsAapt()
  {
    assertEquals("two words", OtaStrings.unescape("  two \n  words  "));
    assertEquals("  kept  ", OtaStrings.unescape("\"  kept  \""));
    assertEquals("it's \"x\"\t@?\\", OtaStrings.unescape("it\\'s \\\"x\\\"\\t\\@\\?\\\\"));
    assertEquals("ä", OtaStrings.unescape("\\u00e4"));
  }

  @Test
  public void placeholders_ArePositions()
  {
    assertEquals(OtaStrings.placeholders("%1$s and %2$d"), OtaStrings.placeholders("%2$d ja %1$s"));
    assertEquals(OtaStrings.placeholders("%s and %d"), OtaStrings.placeholders("%1$s ja %2$d"));
    assertFalse(OtaStrings.placeholders("%1$s").equals(OtaStrings.placeholders("%2$s")));
    assertTrue(OtaStrings.placeholders("100%% %n").isEmpty());
  }

  @Test
  public void candidates_GoFromTheLocaleToTheLanguage()
  {
    assertEquals(Arrays.asList("zh-hant-tw", "zh-hant", "zh-tw", "zh"), OtaStore.candidates("zh-Hant-TW"));
    assertEquals(Arrays.asList("fi-fi", "fi"), OtaStore.candidates("fi_FI"));
    assertEquals(Arrays.asList("fi"), OtaStore.candidates("fi"));
  }

  @Test
  public void fileFilter_TakesTheValuesFiles()
  {
    assertTrue(OtaStore.isValuesFile("values-fi/strings.xml"));
    assertTrue(OtaStore.isValuesFile("app/src/main/res/values-fi/arrays.xml"));
    assertTrue(OtaStore.isValuesFile("strings.xml"));
    assertFalse(OtaStore.isValuesFile("layout-fi/main.xml"));
    assertFalse(OtaStore.isValuesFile("values-fi/strings.json"));
  }
}
