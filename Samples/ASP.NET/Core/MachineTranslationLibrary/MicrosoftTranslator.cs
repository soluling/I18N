using System;
using System.Collections.Generic;
using System.IO;
using System.Net.Http;
using System.Text;
using System.Web;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace Soluling.MachineTranslation
{
  public class LanguageItem
  {
    public string Id { get; set; }
    public string Name { get; set; }

    public LanguageItem(string id, string name)
    {
      Id = id;
      Name = name;
    }

    public override string ToString()
    {
      return Name;
    }
  }

  public class MicrosoftTranslator: LanguageMachineTranslator
  {
    private static List<EngineLanguageData> languageDatas = new List<EngineLanguageData>();
    private string accessToken;
    private DateTime acessTokenExpires;
    private string endpoint;

    static MicrosoftTranslator()
    {
      languageDatas.Add(new EngineLanguageData("zh-CHS", "zh", "Hans"));
      languageDatas.Add(new EngineLanguageData("zh-CHT", "zh", "Hant"));
      languageDatas.Add(new EngineLanguageData("no", "nb"));
    }

    public MicrosoftTranslator(string key, string endpoint = null): base(key)
    {
      if (string.IsNullOrEmpty(endpoint))
        endpoint = "https://api.cognitive.microsoft.com/sts/v1.0/issueToken";

      this.endpoint = endpoint;
    }

    public string Category { get; set; } = "";

    private string MicrosoftIdToId(string id)
    {
      return EngineIdToId(languageDatas, id);
    }

    private string IdToMicrosoftId(string id)
    {
      return IdToEngineId(languageDatas, id);
    }

    public override string ToString()
    {
      return "Microsoft";
    }

    protected override void LoadLanguages()
    {
      if (Languages.Count > 0)
         return;

      using (var client = GetHttpClient())
      {
        client.DefaultRequestHeaders.Add("Accept-Language", "en");

        var builder = GetUriBuilder("languages");

        try
        {
          var query = HttpUtility.ParseQueryString(builder.Query);
          query["scope"] = "translation";
          builder.Query = query.ToString();

          var url = builder.ToString();
          var result = client.GetStringAsync(url).Result;

          // {
          //   "translation":
          //   {
          //     "af":
          //     {
          //       "name":"Afrikaans",
          //       "nativeName":"Afrikaans",
          //       "dir":"ltr"
          //     },
          //     ...
          //   }
          // }

          var root = JObject.Parse(result);
          var translations = (JObject)root["translation"];

          foreach (var item in translations)
          {
            JObject language = (JObject)item.Value;
            Languages.Add(new Language { Id = MicrosoftIdToId(item.Key), Name = language["name"].Value<string>() });
          }
        }
        catch (Exception)
        {
          Languages.Clear();
        }
      }
    }

    public override string[] Translate(string[] strings, string formId, string toId)
    {
      var result = new List<string>();

      using (var client = GetHttpClient())
      {
        var builder = GetUriBuilder("translate");
        var query = HttpUtility.ParseQueryString(builder.Query);
        query["from"] = IdToMicrosoftId(formId);
        query["to"] = IdToMicrosoftId(toId);

        if (Category != "")
          query["category"] = Category;

        builder.Query = query.ToString();
        var url = builder.ToString();

        try
        {
          var sb = new StringBuilder();
          var sw = new StringWriter(sb);

          using (var writer = new JsonTextWriter(sw))
          {
            writer.Formatting = Newtonsoft.Json.Formatting.Indented;

            writer.WriteStartArray();

            foreach (var str in strings)
            {
              writer.WriteStartObject();
              writer.WritePropertyName("Text");
              writer.WriteValue(str);
              writer.WriteEndObject();
            }

            writer.WriteEndArray();
          }

          var payload = sb.ToString();
          var content = new StringContent(payload, Encoding.UTF8, "application/json");
          var response = client.PostAsync(url, content).Result;

          var root = JArray.Parse(response.Content.ReadAsStringAsync().Result);

          foreach (var item in root)
          {
            var translations = (JArray)item["translations"];

            if (translations.Count > 0)
              result.Add(translations[0]["text"].Value<string>());
            else
              result.Add("");
          }

          return result.ToArray();
        }
        catch (Exception)
        {
          result.Clear();
          return result.ToArray();
        }
      }
    }

    public override string DetectLanguage(string value)
    {
      using (var client = GetHttpClient())
      {
        var builder = GetUriBuilder("detect");
        var query = HttpUtility.ParseQueryString(builder.Query);
        query["text"] = value;
        builder.Query = query.ToString();
        var url = builder.ToString();

        try
        {
          var sb = new StringBuilder();
          var sw = new StringWriter(sb);

          using (var writer = new JsonTextWriter(sw))
          {
            writer.Formatting = Newtonsoft.Json.Formatting.Indented;

            writer.WriteStartArray();

            writer.WriteStartObject();
            writer.WritePropertyName("Text");
            writer.WriteValue(value);
            writer.WriteEndObject();

            writer.WriteEndArray();
          }

          var payload = sb.ToString();
          var content = new StringContent(payload, Encoding.UTF8, "application/json");
          var response = client.PostAsync(url, content).Result;

          var root = JArray.Parse(response.Content.ReadAsStringAsync().Result);

          if (root.Count > 0)
            return root[0]["language"].Value<string>();
          else
            return "";
        }
        catch (Exception)
        {
          return "";
        }
      }
    }

    private string AccessToken
    {
      get
      {
        if ((accessToken != null) && (DateTime.Now > acessTokenExpires))
          accessToken = null;

        if (accessToken == null)
        {
          using (var client = new HttpClient())
          {
            client.DefaultRequestHeaders.Add("Ocp-Apim-Subscription-Key", Key);

            var result = client.PostAsync(endpoint, null).Result;
            accessToken = result.Content.ReadAsStringAsync().Result;
            acessTokenExpires = DateTime.Now.AddMinutes(9);
          }
        }

        return accessToken;
      }
    }

    private UriBuilder GetUriBuilder(string command)
    {
      return new UriBuilder($"https://api.cognitive.microsofttranslator.com/{command}?api-version=3.0");
    }

    private HttpClient GetHttpClient()
    {
      var client = new HttpClient();
      client.DefaultRequestHeaders.Add("Authorization", $"Bearer {AccessToken}");
      return client;
    }
  }
}