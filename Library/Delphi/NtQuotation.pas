unit NtQuotation;

// Note! This need to be in UTF8
// http://cldr.unicode.org/development/development-process/design-proposals/delimiter-quotation-mark-proposal

interface

uses
  Generics.Collections;

type
  TQuotationKind =
  (
    qkAscii,     // "..."
    qkEnglish,   // “...”
    qkSwedish,   // ”...”
    qkGerman,    // „...“
    qkPolish,    // „...”
    gkFrench,    // «...»
    qkDanish,    // »...«
    qkChinese    // 「...」
  );

  TQuotationFixer = class(TObject)
  private
    class var Languages: TDictionary<String, TQuotationKind>;

    procedure Populate;

  public
    function IsQuote(c: Char): Boolean;
    function GetKind(const id: String): TQuotationKind;

    function GetStartQuote(kind: TQuotationKind): Char;
    function GetEndQuote(kind: TQuotationKind): Char;

    function Fix(
      const originalLanguage, translatedLanguage, originalText: String;
      var translatedText: String): Integer;
  end;

var
  QuotationFixer: TQuotationFixer;

implementation

uses
  NtBase;

type
  TQuoteData = record
    StartChar: Char;
    EndChar: Char;
    Languages: array of String;
  end;

const
  QUOTES_C: array[TQuotationKind] of TQuoteData =
  (
    // ASCII
    (
      StartChar: '"';
      EndChar: '"';
      Languages: []
    ),

    // English
    (
      StartChar: '“';
      EndChar: '”';
      Languages:
      [
        'en', 'ar', 'hy', 'as', 'bn', 'bs', 'my', 'chr', 'zh-Hans', 'zh', 'kw', 'eo', 'fo', 'fil', 'gl', 'haw', 'id', 'ga', 'kl',
        'kn', 'kk-Cyrl', 'kk', 'km', 'kok', 'mgh', 'ms', 'ml', 'mt', 'gv', 'mfe', 'nb', 'nus', 'or', 'om', 'ps', 'pt', 'pa-Arab',
        'pa-Guru', 'pa', 'sr-Cyrl', 'ii', 'si', 'so', 'ta', 'te', 'th', 'bo', 'ti', 'tr', 'uz-Arab', 'uz-Cyrl', 'uz-Latn', 'uz', 'vi', 'cy',
        'af',
        'he',
        'kea', 'khq', 'ses', 'twq', 'to', 'dje',
        'seh', 'sw', 'yo',
        'brx', 'gu', 'hi', 'mr', 'ne',
        'bem', 'ee', 'ha-Latn', 'ha', 'jmc', 'rof', 'rwk', 'vun',
        'asa', 'ebu', 'lg', 'ig', 'kln', 'kam', 'ki', 'ln', 'kde', 'mer', 'naq', 'nd', 'saq', 'sbp', 'ksb', 'dav',
        'cgg', 'nyn', 'xog',
        'guz',
        'teo',
        'lu',
        'ak', 'bez', 'tzm-Latn', 'tzm', 'nl', 'ko', 'luo', 'vai-Latn', 'vai-Vaii', 'vai', 'zu',
        'ur',
        'swc', 'mas'
      ]
    ),

    // Swedish
    (
      StartChar: '”';
      EndChar: '”';
      Languages:
      [
        'sv', 'fi',
        'lag',
        'rn', 'sn'
      ]
    ),

    // German
    (
      StartChar: '„';
      EndChar: '“';
      Languages:
      [
        'de', 'cs', 'is',
        'bg',
        'et', 'lt',
        'luy',
        'hr',
        'sk',
        'sq', 'ka', 'mk', 'sr', 'sr-Latn',
        'sl'
      ]
    ),

    // Polish
    (
      StartChar: '„';
      EndChar: '”';
      Languages: [
        'pl',
        'ro',
        'ff',
        'hu',
        'agq',
        'nmg'
      ]
    ),

    // French
    (
      StartChar: '«';
      EndChar: '»';
      Languages: [
        'fr',
        'am', 'fa', 'rm', 'gsw',
        'bm', 'ewo', 'dyo', 'kab', 'mg', 'mua',
        'ru', 'uk',
        'shi-Latn', 'shi-Tfng', 'shi',
        'yav',
        'dua',
        'nn',
        'el',
        'sg',
        'bas',
        'ksf',
        'rw',
        'it-CH',
        'az-Cyrl', 'az-Latn', 'az', 'eu', 'br',
        'de-CH',
        'be',
        'pt-PT',
        'lv',
        //'nb',
        'it', 'es', 'ca'
      ]
    ),

    // Danish
    (
      StartChar: '»';
      EndChar: '«';
      Languages: ['da']
    ),

    // Chinese
    (
      StartChar: '「';
      EndChar: '」';
      Languages: ['zh-Hant', 'ja']
    )
  );


function TQuotationFixer.IsQuote(c: Char): Boolean;
var
  k: TQuotationKind;
begin
  for k := Low(k) to High(k) do
  begin
    Result := (QUOTES_C[k].StartChar = c) or (QUOTES_C[k].EndChar = c);

    if Result then
      Exit;
  end;

  Result := False;
end;

function TQuotationFixer.GetKind(const id: String): TQuotationKind;

  function GetValue(const id: String): TQuotationKind;
  begin
    if Languages.ContainsKey(id) then
      Result := Languages[id]
    else
      Result := qkAscii;
  end;

var
  language, script, country, variant: String;
begin
  Populate;

  Result := GetValue(id);

  if Result <> qkAscii then
    Exit;

  TNtBase.ParseLocaleId(id, language, script, country, variant);

  if language = id then
    Exit;

  if script <> '' then
    Result := Languages[language + '-' + script];

  if Result <> qkAscii then
    Exit;

  Result := Languages[language];
end;

function TQuotationFixer.GetStartQuote(kind: TQuotationKind): Char;
begin
  Result := QUOTES_C[kind].StartChar;
end;

function TQuotationFixer.GetEndQuote(kind: TQuotationKind): Char;
begin
  Result := QUOTES_C[kind].EndChar;
end;

function TQuotationFixer.Fix(
  const originalLanguage, translatedLanguage, originalText: String;
  var translatedText: String): Integer;
begin
  Result := 0;
end;

procedure TQuotationFixer.Populate;

  procedure Process(values: array of String; kind: TQuotationKind);
  var
    i: Integer;
  begin
    for i := Low(values) to High(values) do
      Languages.Add(values[i], kind);
  end;

var
  k: TQuotationKind;
begin
  if Languages.Count > 0 then
    Exit;

  for k := Low(k) to High(k) do
    Process(QUOTES_C[k].Languages, k);
end;

initialization
  TQuotationFixer.Languages := TDictionary<String, TQuotationKind>.Create;
  QuotationFixer := TQuotationFixer.Create;
finalization
  QuotationFixer.Free;
  TQuotationFixer.Languages.Free;
end.
