// [MachineTranslator]
//   AmazonTranslate              Amazon Translate
//   DeepLTranslator              DeepL Translator
//   [LanguageMachineTranslator]  Machine translator that can query a list of supported languages
//     GoogleTranslate            Google Translate
//     MicrosoftTranslator        Microsoft Translator
//     YandexTransle              Yandex Translate

using System;
using System.Collections.Generic;
using System.Globalization;

namespace Soluling.MachineTranslation
{
  public class Language
  {
    private string name;

    public string Id { get; set; }
    
    public string Name 
    { 
      get
      {
        if (string.IsNullOrEmpty(name))
          return new CultureInfo(Id).DisplayName;
        else
          return name;
      }
      
      set { name = value;}
    }
  }

  public class EngineLanguageData
  {
    public string Id { get; set; }
    public string Script { get; set; }
    public string EngineId { get; set; }

    public string FullId
    {
      get
      {
        if (!string.IsNullOrEmpty(Script))
          return Id + "-" + Script;
        else
          return Id;
      }
    }

    public EngineLanguageData(string id)
    {
      EngineId = id;
      Id = id;
      Script = "";
    }

    public EngineLanguageData(string engineId, string id, string script = "")
    {
      EngineId = engineId;
      Id = id;
      Script = script;
    }
  }

  public class LanguagePair
  {
    public string From { get; set; }
    public string To { get; set; }
    public bool Bidirectional { get; set; } = false;

    public LanguagePair(string from, string to, bool bidirectional = false)
    {
      From = from;
      To = to;
      Bidirectional = bidirectional;
    }
  }

  public abstract class MachineTranslator
  {
    public MachineTranslator(string key)
    {
      Key = key;
    }

    public string Key { get; set; }

    public abstract Language[] GetLanguages();
    public abstract string DetectLanguage(string value);
    public abstract string[] Translate(string[] strings, string fromId, string toId);

    public bool SupportLanguagePair(string from, string to)
    {
      if (string.IsNullOrEmpty(from) || string.IsNullOrEmpty(to))
        return true;

      var languages = GetLanguages();

      return 
        (Array.Find(languages, language => language.Id == from) != null) && 
        (Array.Find(languages, language => language.Id == to) != null);
    }

    public virtual string Translate(string str, string formId, string toId)
    {
      var strings = new string[1];
      strings[0] = str;
      var result = Translate(strings, formId, toId);

      if (result.Length > 0)
        return result[0];
      else
        return "";
    }

    protected string EngineIdToId(List<EngineLanguageData> datas, string id)
    {
      foreach (var data in datas)
        if (data.EngineId == id)
          return data.FullId;

      return id;
    }

    protected string IdToEngineId(List<EngineLanguageData> datas, string id)
    {
      var parts = id.Split('-');
      var language = parts[0];
      var script = "";

      if (parts.Length > 1)
        script = parts[1];

      foreach (var data in datas)
        if ((data.Id == id) && ((data.Script == "") || (data.Script == script)))
          return data.EngineId;

      return id;
    }
  }

  public abstract class LanguageMachineTranslator: MachineTranslator
  {
    protected List<Language> Languages = new List<Language>();

    public LanguageMachineTranslator(string key): base(key)
    {
    }

    protected abstract void LoadLanguages();

    public override Language[] GetLanguages()
    {
      if (Languages.Count == 0)
        LoadLanguages();

      return Languages.ToArray();
    }
  }

  public class Translation
  {
    public MachineTranslator Translator { get; set; }
    public string Value { get; set; }
    public string Original { get; set; }
    public string OriginalBack { get; set; }
  }
}
