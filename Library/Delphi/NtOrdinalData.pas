// Generated from XML data
unit NtOrdinalData;

interface

implementation

uses
  SysUtils,
  NtOrdinal,
  NtPattern;


// Many European languages use just period to indicate an ordinal number
function GetPeriodShort(ordinal: TOrdinal; plural: TPlural; gender: TGender): String;
begin
  Result := IntToStr(ordinal) + '.';
end;


// English
function GetEnglishShort(ordinal: TOrdinal; plural: TPlural; gender: TGender): String;
var
  str: String;
begin
  if (ordinal mod 10 = 1) and (ordinal mod 100 <> 11) then
    str := 'st'
  else if (ordinal mod 10 = 2) and (ordinal mod 100 <> 12) then
    str := 'nd'
  else if (ordinal mod 10 = 3) and (ordinal mod 100 <> 13) then
    str := 'rd'
  else
    str := 'th';

  Result := IntToStr(ordinal) + str;
end;

function GetEnglishLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
const
  VALUES: TOrdinalArray =
  (
    'first',
    'second',
    'third',
    'fourth',
    'fifth',
    'sixth',
    'seventh',
    'eighth',
    'ninth',
    'tenth'
  );
begin
  Result := VALUES[ordinal];
end;


// German
function GetGermanLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
const
  VALUES: TOrdinalArray =
  (
    'erstens',
    'zweitens',
    'drittens',
    'viertens',
    'fünftens',
    'sechstens',
    'siebtens',
    'achtens',
    'neuntens',
    'zehntens'
  );
begin
  if ordinal = 1 then
  begin
    if plural = pfOther then
      Result := 'ersten'
    else if gender = geNeutral then
      Result := 'erstes'
    else if gender = geFemale then
      Result := 'erste'
    else if gender = geMale then
      Result := 'erster'
    else
      Result := VALUES[ordinal];
  end
  else if ordinal = 2 then
  begin
    if plural = pfOther then
      Result := 'zweiten'
    else if gender = geNeutral then
      Result := 'zweites'
    else if gender = geFemale then
      Result := 'zweite'
    else if gender = geMale then
      Result := 'zweiter'
    else
      Result := VALUES[ordinal];
  end
  else if ordinal = 3 then
  begin
    if plural = pfOther then
      Result := 'dritten'
    else if gender = geNeutral then
      Result := 'drittes'
    else if gender = geFemale then
      Result := 'dritte'
    else if gender = geMale then
      Result := 'dritter'
    else
      Result := VALUES[ordinal];
  end
  else
    Result := VALUES[ordinal];
end;


// Dutch
function GetDutchShort(ordinal: TOrdinal; plural: TPlural; gender: TGender): String;
begin
  Result := IntToStr(ordinal) + 'e';
end;

function GetDutchLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
const
  VALUES: TOrdinalArray =
  (
    'eerste',
    'tweede',
    'derde',
    'vierde',
    'vijfde',
    'zesde',
    'zevende',
    'achtste',
    'negende',
    'tiende'
  );
begin
  Result := VALUES[ordinal];
end;


// French
function GetFrenchShort(ordinal: TOrdinal; plural: TPlural; gender: TGender): String;
var
  str: String;
begin
  if ordinal = 1 then
  begin
    if gender = geMale then
      str := 'er'
    else
      str := 're'
  end
  else
    str := 'e';

  Result := IntToStr(ordinal) + str;
end;

function GetFrenchLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
const
  VALUES: TOrdinalArray =
  (
    'premier',
    'deuxième',
    'troisième',
    'quatrième',
    'cinquième',
    'sixième',
    'septième',
    'huitième',
    'neuvième',
    'dixième'
  );
begin
  if ordinal = 1 then
  begin
    if gender = geMale then
      Result := 'premier'
    else
      Result := 'première'
  end
  else
    Result := VALUES[ordinal];
end;


// Finnish
function GetFinnishLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
const
  SINGULARS: TOrdinalArray =
  (
    'ensimmäinen',
    'toinen',
    'kolmas',
    'neljäs',
    'viides',
    'kuudes',
    'seitsemäs',
    'kahdeksas',
    'yhdeksäs',
    'kymmenes'
  );

  PLURALS: TOrdinalArray =
  (
    'ensimmäiset',
    'toiset',
    'kolmannet',
    'neljännet',
    'viidennet',
    'kuudennet',
    'seitsemännet',
    'kahdeksannet',
    'yhdeksännet',
    'kymmenennet'
  );
begin
  if plural = pfOne then
    Result := SINGULARS[ordinal]
  else
    Result := PLURALS[ordinal]
end;


// Estonian, TODO: plural forms
function GetEstonianLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
const
  SINGULARS: TOrdinalArray =
  (
    'esimene',
    'teine',
    'kolmas',
    'neljas',
    'viies',
    'kuues',
    'seitsmes',
    'kaheksas',
    'üheksas',
    'kümnes'
  );
begin
  Result := SINGULARS[ordinal];
end;


// Danish
function GetDanishLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
const
  VALUES: TOrdinalArray =
  (
    'første',
    'anden',
    'tredje',
    'fjerde',
    'femte',
    'sjette',
    'syvende',
    'ottende',
    'niende',
    'tiende'
  );
begin
  Result := VALUES[ordinal];
end;


// Swedish
function GetSwedishLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
const
  VALUES: TOrdinalArray =
  (
    'första',
    'andra',
    'tredje',
    'fjärde',
    'femte',
    'sjätte',
    'sjunde',
    'åttonde',
    'nionde',
    'tionde'
  );
begin
  Result := VALUES[ordinal];
end;


// Norwegian, Bokmål
function GetNorwegianBokmalLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
const
  VALUES: TOrdinalArray =
  (
    'første',
    'annen',
    'tredje',
    'fjerde',
    'femte',
    'sjette',
    'sjuende',
    'åttende',
    'niende',
    'tiende'
  );
begin
  Result := VALUES[ordinal];
end;


// Norwegian, Nynorsk
function GetNorwegianNynorskLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
const
  VALUES: TOrdinalArray =
  (
    'første',
    'andre',
    'tredje',
    'fjerde',
    'femte',
    'sjette',
    'sjuande',
    'åttande',
    'niande',
    'tiande'
  );
begin
  Result := VALUES[ordinal];
end;


// Icelandic
function GetIcelandicLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
const
  ICELANDIC: TOrdinalArray =
  (
    'fyrsti',
    'annar',
    'þriðji',
    'fjórði',
    'fimmti',
    'sjötti',
    'sjöundi',
    'áttundi',
    'níundi',
    'tíundi'
  );
begin
  Result := ICELANDIC[ordinal];
end;


// Japanese
function GetJapaneseLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
const
  JAPANESE: TOrdinalArray =
  (
    '一つ目',
    '二つ目',
    '三つ目',
    '四つ目',
    '五つ目',
    '六つ目',
    '七つ目',
    '八つ目',
    '九つ目',
    '十'
  );
begin
  Result := JAPANESE[ordinal];
end;

function GetJapaneseShort(ordinal: TOrdinal; plural: TPlural; gender: TGender): String;
begin
  if TNtOrdinal.IsShortOrdinal(ordinal) then
    Result := GetJapaneseLong(TSmallOrdinal(ordinal), plural, gender)
  else
    Result := IntToStr(ordinal) + '目';
end;


// Korean
function GetKoreanLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
const
  KOREAN: TOrdinalArray =
  (
    '첫째',
    '둘째',
    '셋째',
    '넷째',
    '다섯째',
    '여섯째',
    '일곱째',
    '여덟째',
    '아홉째',
    '열째'
  );
begin
  Result := KOREAN[ordinal];
end;

function GetKoreanShort(ordinal: TOrdinal; plural: TPlural; gender: TGender): String;
begin
  if TNtOrdinal.IsShortOrdinal(ordinal) then
    Result := GetKoreanLong(TSmallOrdinal(ordinal), plural, gender)
  else
    Result := IntToStr(ordinal) + '째';
end;


// Simplified Chinese
function GetSimplifiedChineseLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
const
  CHINESE: TOrdinalArray =
  (
    '第一',
    '第二',
    '第三',
    '第四',
    '第五',
    '第六',
    '第七',
    '第八',
    '第九',
    '第十'
  );
begin
  Result := CHINESE[ordinal];
end;

function GetSimplifiedChineseShort(ordinal: TOrdinal; plural: TPlural; gender: TGender): String;
begin
  if TNtOrdinal.IsShortOrdinal(ordinal) then
    Result := GetSimplifiedChineseLong(TSmallOrdinal(ordinal), plural, gender)
  else
    Result := '第' + IntToStr(ordinal);
end;


initialization
  TNtOrdinal.Register('en', @GetEnglishLong, @GetEnglishShort);
  TNtOrdinal.Register('de', @GetGermanLong, @GetPeriodShort);
  TNtOrdinal.Register('nl', @GetDutchLong, @GetDutchShort);
  TNtOrdinal.Register('fr', @GetFrenchLong, @GetFrenchShort);

  TNtOrdinal.Register('fi', @GetFinnishLong, @GetPeriodShort);
  TNtOrdinal.Register('et', @GetEstonianLong, @GetPeriodShort);
  TNtOrdinal.Register('sv', @GetSwedishLong, @GetPeriodShort);
  TNtOrdinal.Register('da', @GetDanishLong, @GetPeriodShort);
  TNtOrdinal.Register('no', @GetNorwegianBokmalLong, @GetPeriodShort);
  TNtOrdinal.Register('nb', @GetNorwegianBokmalLong, @GetPeriodShort);
  TNtOrdinal.Register('nn', @GetNorwegianNynorskLong, @GetPeriodShort);
  TNtOrdinal.Register('is', @GetIcelandicLong, @GetPeriodShort);

  TNtOrdinal.Register('ja', @GetJapaneseLong, @GetJapaneseShort);
  TNtOrdinal.Register('ko', @GetKoreanLong, @GetKoreanShort);
  TNtOrdinal.Register('zh', @GetSimplifiedChineseLong, @GetSimplifiedChineseShort);
  TNtOrdinal.Register('zh-Hans', @GetSimplifiedChineseLong, @GetSimplifiedChineseShort);
end.

