package com.soluling.ota;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import android.content.Context;
import android.widget.CheckBox;
import android.widget.TextView;

import androidx.appcompat.widget.AppCompatTextView;
import androidx.test.core.app.ActivityScenario;
import androidx.test.ext.junit.runners.AndroidJUnit4;
import androidx.test.platform.app.InstrumentationRegistry;

import com.soluling.ota.test.R;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.io.File;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

// A real AppCompat activity in Finnish through an override configuration: AppCompat's own factory
// and the configuration context both keep the downloaded strings
@RunWith(AndroidJUnit4.class)
public class OtaActivityTest
{
  private static final String GUID = "11111111-2222-3333-4444-555555555555";

  private TestServer server;

  @Before
  public void setUp() throws Exception
  {
    SolulingOta.reset();

    Context context = InstrumentationRegistry.getInstrumentation().getContext();
    File folder = new File(context.getApplicationContext().getFilesDir(), "soluling-ota");
    File[] files = folder.listFiles();

    if (files != null)
      for (File file : files)
        file.delete();

    server = new TestServer();
    server.put("/artifacts/" + GUID + "/manifest.json", 200,
      "[{\"fileName\":\"values-fi/strings.xml\",\"language\":\"fi\",\"hash\":\"a1\",\"size\":null,\"url\":\"" + server.url() + "/files/a1/strings.xml\"}]");
    server.put("/files/a1/strings.xml", 200,
      "<resources><string name=\"hello\">Moi maailma</string>" +
      "<plurals name=\"files\"><item quantity=\"one\">%d tiedosto</item><item quantity=\"other\">%d tiedostoa</item></plurals></resources>");

    CountDownLatch latch = new CountDownLatch(1);

    SolulingOta.initialize(context, new SolulingOta.Options(GUID).setCdnUrl(server.url() + "/artifacts").setApiUrl(null).setLanguage("fi"));
    SolulingOta.update(false, result -> latch.countDown());
    assertTrue(latch.await(20, TimeUnit.SECONDS));
    assertEquals(SolulingOta.Result.UPDATED, SolulingOta.getLastResult());
  }

  @After
  public void tearDown() throws Exception
  {
    server.close();
    SolulingOta.reset();
  }

  @Test
  public void appCompatActivity_GetsTheDownloadedStrings()
  {
    try (ActivityScenario<OtaTestActivity> scenario = ActivityScenario.launch(OtaTestActivity.class))
    {
      AtomicReference<String> language = new AtomicReference<>();

      scenario.onActivity(activity ->
      {
        language.set(activity.getResources().getConfiguration().getLocales().get(0).getLanguage());
        assertEquals("fi", language.get());
        assertTrue(activity.getResources().getClass().getName(), activity.getResources() instanceof OtaResources);

        // Code
        assertEquals("Moi maailma", activity.getString(R.string.hello));
        assertEquals("3 tiedostoa", activity.getResources().getQuantityString(R.plurals.files, 3, 3));
        assertEquals("Untouched", activity.getString(R.string.untouched));

        // The layout: AppCompat's factory created the views, and they got the strings
        TextView text = activity.findViewById(R.id.text);
        CheckBox check = activity.findViewById(R.id.check);

        assertTrue(text instanceof AppCompatTextView);
        assertEquals("Moi maailma", text.getText().toString());
        assertEquals("Moi maailma", check.getText().toString());
        assertEquals("Moi maailma", ((TextView) activity.findViewById(R.id.edit)).getHint().toString());
        assertEquals("Moi maailma", activity.findViewById(R.id.image).getContentDescription().toString());
      });

      assertEquals("fi", language.get());
    }
  }
}
