{
  @abstract Implements an ordinal number API.

  If you have something like this

  @code(str := Format('This is the %d attempt', [attemptCount]);)

  and you want to localize it convert code to

  @longCode(#
resourcestring
  SAttemptCount = 'This is the %s attempt';
...
  str := Format(SAttemptCount, [TNtOrdinal.FormatShort(attemptCount)]);
#)

  As you can see instead of passing the attempt count to the Format function
  we first format the attempt count into string and pass that to Format.

  This unit supports following languages:
  en, fr, de, nl, fi, et, sv, da, no, nb, nn, is, ja, ko, zh-Hans

  You can add your own languages by implementing conversion functions
  and registering them by using @link(TNtOrdinal.Register).

  Delphi XE or later required.
}
unit NtOrdinal;

// Note! This need to be in UTF8

{$I NtVer.inc}

interface

uses
  NtPattern;

type
  { @abstract Ordinal number type. }
  TOrdinal = 1..MaxInt;

  { @abstract Small ordinal number type.
    A subset of ordinal numbers containing the first 10 ordinals.
    When an ordinal number belongs to this subset it also can be written as word.
    For example "first" vs "1st". }
  TSmallOrdinal = 1..10;

  { @abstract Enumeration that specifies the string form of an ordinal. }
  TOrdinalForm =
  (
    ofShort, //< Short string form such as "1st" and "4th"
    ofLong   //< Long string form such as "first" and "fourth"
  );

  { @abstract Function that returns ordinal in a short form such as "1st" and "4th".
    @param ordinal Ordinal number.
    @param plural  Plural form.
    @param gender  Gender.
    @return The ordinal in a short string form. }
  TOrdinalShortProc = function(ordinal: TOrdinal; plural: TPlural; gender: TGender): String;

  { @abstract Function that returns ordinal in a long form (e.g. "first").
    @param ordinal Small ordinal number.
    @return The ordinal in a long string form. }
  TOrdinalLongProc = function(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;

  { @abstract Array type to hold the small ordinal words. }
  TOrdinalArray = array[TSmallOrdinal] of String;

  { @abstract Static class that contains ordinal number routines.
    @seealso(NtOrdinal) }
  TNtOrdinal = class
  public
    class function IsShortOrdinal(value: TOrdinal): Boolean;

    { Converts an ordinal number to a string such as "1st" or "first".
      @param form        String form to be used.
      @param ordinal     Ordinal number.
      @param plural      Specifies the plural form.
      @param gender      Specifies the gender.
      @return            The formatted string. }
    class function Format(
      form: TOrdinalForm;
      ordinal: TOrdinal;
      plural: TPlural = pfOne;
      gender: TGender = geNeutral): String;

    { Converts an ordinal number to a string using the short format such as "1st" and "4th".
      @param ordinal     Ordinal number.
      @param plural      Specifies the plural form.
      @param gender      Specifies the gender.
      @return            The formatted string. }
    class function FormatShort(
      ordinal: TOrdinal;
      plural: TPlural = pfOne;
      gender: TGender = geNeutral): String;

    { Converts an ordinal number to a string using the long format such as "first" and "fourth".
      Only the first 10 ordinals can be formatted to the long format. The rest of the ordinals will always
      be formatted using the short format such as "21st" and "24th".
      @param ordinal     Ordinal number.
      @param plural      Specifies the plural form.
      @param gender      Specifies the gender.
      @return            The formatted string. }
    class function FormatLong(
      ordinal: TOrdinal;
      plural: TPlural = pfOne;
      gender: TGender = geNeutral): String;

    { Register formatting functions for a language.
      @param id          IETF language tag of the language/locale.
      @param shortProc   Function that converts an ordianal to a short string.
      @param longProc    Function that converts a small ordianal to a long string. }
    class procedure Register(
      const id: String;
      longProc: TOrdinalLongProc;
      shortProc: TOrdinalShortProc);
  end;

implementation

uses
  SysUtils,
  Generics.Collections,
  NtBase;

type
  TData = class
    LongProc: TOrdinalLongProc;
    ShortProc: TOrdinalShortProc;
  end;

var
  FDatas: TDictionary<String, TData>;


// These are used if there is no language specific ordinal function
function GetDefaultShort(ordinal: TOrdinal; plural: TPlural; gender: TGender): String;
begin
  Result := IntToStr(ordinal);
end;

function GetDefaultLong(ordinal: TSmallOrdinal; plural: TPlural; gender: TGender): String;
begin
  Result := IntToStr(ordinal);
end;


// TNtOrdinal

class function TNtOrdinal.IsShortOrdinal(value: TOrdinal): Boolean;
begin
  // Checks if the ordinal is a small ordinal.
  Result := (value >= TOrdinal(Low(TSmallOrdinal))) and (value <= TOrdinal(High(TSmallOrdinal)));
end;

class function TNtOrdinal.Format(
  form: TOrdinalForm;
  ordinal: TOrdinal;
  plural: TPlural;
  gender: TGender): String;
begin
  if form = ofShort then
    Result := FormatShort(ordinal, plural, gender)
  else
    Result := FormatLong(ordinal, plural, gender)
end;

function GetData: TData;
var
  id: String;
begin
  id := TNtBase.GetActiveLocale;

  if not FDatas.ContainsKey(id) then
    id := '';

  Result := FDatas.Items[id];
end;

class function TNtOrdinal.FormatShort(
  ordinal: TOrdinal;
  plural: TPlural;
  gender: TGender): String;
begin
  Result := GetData.ShortProc(ordinal, plural, gender);
end;

class function TNtOrdinal.FormatLong(
  ordinal: TOrdinal;
  plural: TPlural;
  gender: TGender): String;
begin
  if IsShortOrdinal(ordinal) then
    Result := GetData.LongProc(TSmallOrdinal(ordinal), plural, gender)
  else
    Result := FormatShort(ordinal, plural, gender);
end;

class procedure TNtOrdinal.Register(
  const id: String;
  longProc: TOrdinalLongProc;
  shortProc: TOrdinalShortProc);
var
  data: TData;
begin
  if not FDatas.TryGetValue(id, data) then
  begin
    data := TData.Create;
    FDatas.Add(id, data);
  end;

  data.LongProc := longProc;
  data.ShortProc := shortProc;
end;


initialization
  FDatas := TDictionary<String, TData>.Create;
  TNtOrdinal.Register('', @GetDefaultLong, @GetDefaultShort);
end.
