package com.soluling.ota;

import android.content.Context;
import android.content.ContextWrapper;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.view.LayoutInflater;

/**
 * The context of SolulingOta.wrap: its resources return the downloaded strings, and its layout
 * inflater sets them on the views of a layout. A context made for another configuration (such as
 * AppCompat's for a per-app language or night mode) is wrapped too.
 */
final class OtaContextWrapper extends ContextWrapper
{
  private Resources base;
  private Configuration configuration;
  private OtaResources resources;
  private LayoutInflater inflater;

  OtaContextWrapper(Context base)
  {
    super(base);
  }

  @Override
  public Resources getResources()
  {
    Resources current = super.getResources();

    synchronized (this)
    {
      // The application's resources change their configuration in place (a new device language)
      if ((resources == null) || (current != base) || !current.getConfiguration().equals(configuration))
      {
        base = current;
        configuration = new Configuration(current.getConfiguration());
        resources = new OtaResources(current);
      }

      return resources;
    }
  }

  @Override
  public Object getSystemService(String name)
  {
    if (LAYOUT_INFLATER_SERVICE.equals(name))
    {
      synchronized (this)
      {
        if (inflater == null)
          inflater = new OtaLayoutInflater((LayoutInflater) super.getSystemService(name), this);

        return inflater;
      }
    }

    return super.getSystemService(name);
  }

  @Override
  public Context createConfigurationContext(Configuration overrideConfiguration)
  {
    return SolulingOta.wrap(super.createConfigurationContext(overrideConfiguration));
  }
}
