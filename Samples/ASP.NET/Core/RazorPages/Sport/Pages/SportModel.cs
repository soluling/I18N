using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Localization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;
using Microsoft.AspNetCore.Mvc.Rendering;
using Microsoft.Extensions.Localization;
using Soluling.Sport;

namespace RazorSport.Pages
{
  public class SportModel: PageModel
  {
    protected readonly SportService _sportService;

    public SportModel(SportService sportService)
    {
      _sportService = sportService;
    }

    [BindProperty]
    public Sport Value { get; set; }

    [TempData]
    public string Message { get; set; }

    private string GetLanguagePart(string id)
    {
      return id.Split('-')[0];
    }

    public string AcceptLanguage
    {
      get
      {
        if (Request.Headers.ContainsKey("Accept-Language"))
          return GetLanguagePart(Request.Headers["Accept-Language"][0]);
        else
          return "en";
      }
    }

    public string CookieLanguage
    {
      get
      {
        var cookieValue = Request.Cookies[CookieRequestCultureProvider.DefaultCookieName];

        if (cookieValue != null)
          return CookieRequestCultureProvider.ParseCookieValue(cookieValue).Cultures[0].Value;
        else
          return "";
      }
    }

    public string ActiveLanguage
    {
      get
      {
        if (CookieLanguage != "")
          return CookieLanguage;
        else
          return AcceptLanguage;
      }
    }

    public List<SelectListItem> GetOlympicEnumList(IStringLocalizer localizer)
    {
      var list = new List<SelectListItem>();

      foreach (var item in Enum.GetValues(typeof(Olympic)))
      {
        list.Add(new SelectListItem
        {
          Text = localizer[Enum.GetName(typeof(Olympic), item)],
          Value = item.ToString()
        });
      }

      return list.OrderBy(x => x.Text).ToList();
    }

    public string GetLanguageName(string language)
    {
      language = GetLanguagePart(language);

      switch (language)
      {
        case "de": return LanguageNames.German;
        case "en": return LanguageNames.English;
        case "fi": return LanguageNames.Finnish;
        case "fr": return LanguageNames.French;
        case "et": return LanguageNames.Estonian;
        case "ja": return LanguageNames.Japanese;
        case "sv": return LanguageNames.Swedish;

        default: 
          return new CultureInfo(language).DisplayName;
      }
    }

    public IActionResult OnPostSetLanguage(string culture)
    {
      if (!string.IsNullOrEmpty(culture))
      {
        Response.Cookies.Append(
          CookieRequestCultureProvider.DefaultCookieName,
          CookieRequestCultureProvider.MakeCookieValue(new RequestCulture(culture)),
          new CookieOptions { Expires = DateTimeOffset.UtcNow.AddYears(1) }
        );
      }
      else
      {
        Response.Cookies.Delete(CookieRequestCultureProvider.DefaultCookieName);
      }

      return RedirectToPage();
    }
  }
}
