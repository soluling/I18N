package com.soluling.ota;

import android.content.Context;
import android.content.res.Configuration;
import android.os.Bundle;

import java.util.Locale;

import androidx.appcompat.app.AppCompatActivity;

import com.soluling.ota.test.R;

// An application's activity: AppCompat, with its context wrapped as the documentation says
public class OtaTestActivity extends AppCompatActivity
{
  @Override
  protected void attachBaseContext(Context base)
  {
    super.attachBaseContext(SolulingOta.wrap(base));

    // Finnish as an override configuration, the way AppCompat applies a per-app language or night
    // mode: the activity's resources come from the wrapper's createConfigurationContext
    Configuration configuration = new Configuration();
    configuration.setLocale(Locale.forLanguageTag("fi"));
    applyOverrideConfiguration(configuration);
  }

  @Override
  protected void onCreate(Bundle savedInstanceState)
  {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.ota_test);
  }
}
