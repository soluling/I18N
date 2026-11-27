package com.soluling.ota;

import android.content.Context;
import android.content.res.Resources;
import android.os.Build;
import android.util.AttributeSet;
import android.view.InflateException;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import org.xmlpull.v1.XmlPullParser;

/**
 * A layout inflater that sets the downloaded strings of android:text, android:hint, and
 * android:contentDescription on the views it creates. A view keeps the attributes only while it is
 * created, so the strings are set by a factory that sees both: it wraps the factory that the
 * application sets (AppCompat's, a fragment manager's), and creates the views that no factory
 * creates itself.
 */
final class OtaLayoutInflater extends LayoutInflater
{
  private static final String ANDROID = "http://schemas.android.com/apk/res/android";

  // The packages of the views that a layout names without a package, as PhoneLayoutInflater
  private static final String[] PREFIXES = { "android.widget.", "android.webkit.", "android.app." };

  // The inflater this one was made from, without the factory that inflate sets
  private final LayoutInflater source;

  // Whether inflate set the factory because nothing else did
  private boolean ownFactory;

  OtaLayoutInflater(LayoutInflater original, Context context)
  {
    super(original, context);
    source = original;
  }

  @Override
  public LayoutInflater cloneInContext(Context context)
  {
    // A clone copies the factories. One that inflate set would keep AppCompat from setting its own
    // on the clone, so the clone is made from the inflater without it.
    return new OtaLayoutInflater(ownFactory ? source : this, context);
  }

  @Override
  public void setFactory(Factory factory)
  {
    super.setFactory2(new OtaFactory(factory, null, getFactory() == null));
  }

  @Override
  public void setFactory2(Factory2 factory)
  {
    // The factory set first is called last (a later one is merged in front of it), so it is the
    // one that creates the views that no factory creates
    super.setFactory2(new OtaFactory(null, factory, getFactory() == null));
  }

  @Override
  public View inflate(XmlPullParser parser, ViewGroup root, boolean attachToRoot)
  {
    if (getFactory() == null)
    {
      super.setFactory2(new OtaFactory(null, null, true));
      ownFactory = true;
    }

    return super.inflate(parser, root, attachToRoot);
  }

  @Override
  protected View onCreateView(String name, AttributeSet attrs) throws ClassNotFoundException
  {
    for (String prefix : PREFIXES)
    {
      try
      {
        View view = createView(name, prefix, attrs);

        if (view != null)
          return view;
      }
      catch (ClassNotFoundException e)
      {
        // Try the next package
      }
    }

    return super.onCreateView(name, attrs);
  }

  @Override
  public View onCreateView(Context viewContext, View parent, String name, AttributeSet attrs) throws ClassNotFoundException
  {
    // Android 10 and later: create the view in its own context, which has its android:theme
    for (String prefix : PREFIXES)
    {
      try
      {
        View view = createView(viewContext, name, prefix, attrs);

        if (view != null)
          return view;
      }
      catch (ClassNotFoundException e)
      {
        // Try the next package
      }
    }

    return createView(viewContext, name, "android.view.", attrs);
  }

  // Creates a view that no factory created: the activity's own factory first (fragments), then
  // the view itself
  private View createOwnView(View parent, String name, Context context, AttributeSet attrs)
  {
    Context owner = getContext();

    if (owner instanceof Factory2)
    {
      View view = ((Factory2) owner).onCreateView(parent, name, context, attrs);

      if (view != null)
        return view;
    }

    // A fragment the activity didn't create is left to the inflater, which reports the error
    if ("fragment".equals(name))
      return null;

    try
    {
      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q)
        return (name.indexOf('.') < 0) ? onCreateView(context, parent, name, attrs) : createView(context, name, null, attrs);
      else
        return (name.indexOf('.') < 0) ? onCreateView(parent, name, attrs) : createView(name, null, attrs);
    }
    catch (ClassNotFoundException | InflateException e)
    {
      // The inflater tries again, and reports the error
      return null;
    }
  }

  // Sets the downloaded strings of the view's attributes
  private void translate(View view, AttributeSet attrs)
  {
    Resources resources = getContext().getResources();

    if (!(resources instanceof OtaResources))
      resources = view.getContext().getResources();

    if (!(resources instanceof OtaResources))
      return;

    OtaResources ota = (OtaResources) resources;

    if (view instanceof TextView)
    {
      TextView textView = (TextView) view;
      String text = find(ota, attrs, "text");
      String hint = find(ota, attrs, "hint");

      if (text != null)
        textView.setText(text);

      if (hint != null)
        textView.setHint(hint);
    }

    String description = find(ota, attrs, "contentDescription");

    if (description != null)
      view.setContentDescription(description);
  }

  private static String find(OtaResources resources, AttributeSet attrs, String attribute)
  {
    int id = attrs.getAttributeResourceValue(ANDROID, attribute, 0);
    return (id != 0) ? resources.findString(id) : null;
  }

  private final class OtaFactory implements Factory2
  {
    private final Factory factory;
    private final Factory2 factory2;
    private final boolean create;

    OtaFactory(Factory factory, Factory2 factory2, boolean create)
    {
      this.factory = factory;
      this.factory2 = factory2;
      this.create = create;
    }

    @Override
    public View onCreateView(View parent, String name, Context context, AttributeSet attrs)
    {
      View view = null;

      if (factory2 != null)
        view = factory2.onCreateView(parent, name, context, attrs);
      else if (factory != null)
        view = factory.onCreateView(name, context, attrs);

      if ((view == null) && create)
        view = createOwnView(parent, name, context, attrs);

      if (view != null)
        translate(view, attrs);

      return view;
    }

    @Override
    public View onCreateView(String name, Context context, AttributeSet attrs)
    {
      return onCreateView(null, name, context, attrs);
    }
  }
}
