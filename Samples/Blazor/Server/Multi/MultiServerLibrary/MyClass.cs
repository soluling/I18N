using Microsoft.Extensions.Localization;

namespace MultiServerLibrary
{
  public class MyClass
  {
    private IStringLocalizer _localizer;

    public MyClass(IStringLocalizer<MyClass> localizer)
    {
      _localizer = localizer;
    }

    public string GetValue()
    {
      return _localizer["This is a sample"];
    }
  }
}
