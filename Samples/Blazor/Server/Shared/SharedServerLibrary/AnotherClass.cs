using Microsoft.AspNetCore.Components;
using Microsoft.Extensions.Localization;

namespace SharedServerLibrary
{
  public class AnotherClass
  {
    public string GetValue()
    {
      return Localizer["This is another sample"];
    }

    [Inject] 
    public IStringLocalizer<SharedStrings> Localizer { get; set; }
  }
}
