using Microsoft.AspNetCore.Components;
using Microsoft.Extensions.Localization;

namespace MultiServerLibrary
{
  public class AnotherClass
  {
    public string GetValue()
    {
      return Localizer["This is another sample"];
    }

    [Inject] 
    public IStringLocalizer<MyClass> Localizer { get; set; }
  }
}
