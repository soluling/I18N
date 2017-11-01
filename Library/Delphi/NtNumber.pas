{
  @abstract Implements an API to abbreviated numbers.

  If you have something like this

  @code(str := Format('There are %d hits', [hitCount]);)

  and you want to localize it and use abbreviated numbers convert code to

  @longCode(#
resourcestring
  SHits = 'There are %s hits';
...
  str := Format(SHits, [TAbbreviatedNumber.FormatShort(hitCount)]);

  Delphi XE or later required.
#)
}
unit NtNumber;

interface

uses
  NtPattern;

const
  DEFAULT_PRECISION = 2;

type
  { @abstract Record that stores one abbreviation rule.
    Abbreviation rules use CLDR syntax where there is a set of rules for each language
    and abbreviation type. }
  TAbbreviationRule = record
    { Minimum value where this rule is used. }
    Range: UInt64;
    { Plural form of the rule. }
    Plural: TPlural;
    { The abbreviation rule value. }
    Value: String;
  end;

  { @abstract Type that stores all abbreviation rules. }
  TAbbreviationRules = array of TAbbreviationRule;

  { @abstract Enumeration that specifies the format of the abbreviation type. }
  TAbbreviatedNumberForm =
  (
    anLong,     //< Long string form such as "1 thousand" and "10 millions"
    anShort,    //< Short string form such as "1K" or "10M"
    anCurrency  //< Currency string form such as "$1K" and "$10M"
  );

  { @abstract Static class that contains abbreviated number routines. }
  TAbbreviatedNumber = class
  public
    { Check of the value can be abbreviated.
      @param form  Abbreviation type.
      @param value Number value to be abbreviated.
      @return True if the value can be abbreviated. }
    class function CanBeAbbreviated(
      form: TAbbreviatedNumberForm;
      value: Double): Boolean;

    { Converts a number to its abbreviated form.
      @param form      Abbreviation type.
      @param value     Number value to be abbreviated.
      @param precision Number of digits that are used to express a value.
      @return True if the value can be abbreviated. }
    class function Format(
      form: TAbbreviatedNumberForm;
      value: Double;
      precision: Integer = DEFAULT_PRECISION): String;

    { Converts a number to its abbreviated long form.
      @param value     Number value to be abbreviated.
      @param precision Number of digits that are used to express a value.
      @return True if the value can be abbreviated. }
    class function FormatLong(
      value: Double;
      precision: Integer = DEFAULT_PRECISION): String;

    { Converts a number to its abbreviated short form.
      @param value     Number value to be abbreviated.
      @param precision Number of digits that are used to express a value.
      @return True if the value can be abbreviated. }
    class function FormatShort(
      value: Double;
      precision: Integer = DEFAULT_PRECISION): String;

    { Converts a number to its abbreviated currency form.
      @param value     Number value to be abbreviated.
      @param precision Number of digits that are used to express a value.
      @return True if the value can be abbreviated. }
    class function FormatCurrency(
      value: Double;
      precision: Integer = DEFAULT_PRECISION): String;

    { Register formatting functions for a language.
      @param id            IETF language tag of the language/locale.
      @param longRules     Rules to abbreviate a number to a long form.
      @param shortRules    Rules to abbreviate a number to a short form.
      @param currentyRules Rules to abbreviate a number to a currency form. }
    class procedure Register(
      const id: String;
      longRules, shortRules, currencyRules: array of TAbbreviationRule);
  end;

implementation

uses
  SysUtils,
  Math,
  Generics.Collections,
  Generics.Defaults,
  NtBase;

type
  TData = class
    Long: TAbbreviationRules;
    Short: TAbbreviationRules;
    Currency: TAbbreviationRules;
  end;

var
  FDatas: TDictionary<String, TData>;

function GetData: TData;
var
  id, language, script, country, variant: String;
begin
  id := TNtBase.GetActiveLocale;

  if not FDatas.ContainsKey(id) then
  begin
    TNtBase.ParseLocaleId(id, language, script, country, variant);
    id := language;

    if not FDatas.ContainsKey(id) then
      id := '';
  end;

  Result := FDatas.Items[id];
end;

function DoFormat(
  value: Double;
  form: TAbbreviatedNumberForm;
  precision: Integer;
  rules: TAbbreviationRules): String;

  function GetValuePrecision(value: Double): Integer;
  var
   valueRange: UInt64;
  begin
    Result := 0;
    valueRange := 1;

    while valueRange <= value do
    begin
      valueRange := 10*valueRange;
      Inc(Result);
    end;
  end;

  procedure DeleteTrailingZeros(var value: String);
  var
    i: Integer;
  begin
    if Pos(FormatSettings.DecimalSeparator, value) = 0 then
      Exit;

    i := Length(value);

    while (i > 0) and not ((value[i] = FormatSettings.DecimalSeparator) or ((value[i] >= '0') and (value[i] <= '9'))) do
      Dec(i);

    while (i > 0) and (value[i] = '0') do
    begin
      Delete(value, i, 1);
      Dec(i);
    end;

    if (i > 0) and (value[i] = FormatSettings.DecimalSeparator) then
      Delete(value, i, 1);
  end;

  function GetStr(value: Double): String;
  var
    valuePrecision: Integer;
  begin
    valuePrecision := GetValuePrecision(value);
    value := RoundTo(value, valuePrecision - precision);
    Result := FloatToStrF(value, ffFixed, 10, Max(0, precision - valuePrecision));
    DeleteTrailingZeros(Result);
  end;

  function GetNotAbbreviatedStr(value: Double): String;
  var
    valuePrecision: Integer;
  begin
    if form = anCurrency then
    begin
      valuePrecision := GetValuePrecision(value);
      value := RoundTo(value, valuePrecision - precision);

      if value = Round(value) then
        Result := CurrToStrF(value, ffCurrency, 0)
      else
        Result := CurrToStrF(value, ffCurrency, Max(0, precision - valuePrecision));

      DeleteTrailingZeros(Result);
    end
    else
      Result := GetStr(value);
  end;

  function Find(range: Int64; plural: TPlural; var rule: TAbbreviationRule): Boolean;
  var
    thisRule: TAbbreviationRule;
  begin
    for thisRule in rules do
    begin
      Result := (thisRule.Range = range) and (thisRule.Plural = plural);

      if Result then
      begin
        rule := thisRule;
        Exit;
      end;
    end;

    Result := False;
  end;

var
  p: Integer;
  count, range: Int64;
  displayValue: Double;
  str: String;
  rule, newRule: TAbbreviationRule;
  proc: TPluralProc;
  plural: TPlural;
begin
  for rule in rules do
  begin
    if rule.Range <= value then
    begin
      range := rule.Range;
      Result := rule.Value;
      p := Pos('0', Result);
      Delete(Result, p, 1);

      while (Result <> '') and (Result[p] = '0') do
      begin
        Delete(Result, p, 1);
        range := range div 10;
      end;

      if Result <> '' then
      begin
        str := GetStr(value/range);
        displayValue := StrToFloat(str);
      end
      else
      begin
        str := GetNotAbbreviatedStr(value);
        displayValue := value;
      end;

      count := Round(displayValue);

      if displayValue = count then
      begin
        proc := TMultiPattern.GetProc();
        plural := proc(count, count, 0, 0, 0, 0);

        if plural <> rule.Plural then
        begin
          // Wrong plural form. Find the correct one and get abbreviation again
          if Find(rule.Range, plural, newRule) then
          begin
            range := newRule.Range;
            Result := newRule.Value;
            p := Pos('0', Result);
            Delete(Result, p, 1);

            while (Result <> '') and (Result[p] = '0') do
            begin
              Delete(Result, p, 1);
              range := range div 10;
            end;

            str := GetStr(value/range);
          end;
        end;
      end;

      Result := Copy(Result, 1, p - 1) + str + Copy(Result, p, Length(Result));
      p := Pos('¤', Result);

      if p > 0 then
        Result := Copy(Result, 1, p - 1) + FormatSettings.CurrencyString + Copy(Result, p + 1, Length(Result));

      Exit;
    end;
  end;

  Result := GetNotAbbreviatedStr(value);
end;

class function TAbbreviatedNumber.CanBeAbbreviated(form: TAbbreviatedNumberForm; value: Double): Boolean;
var
  rule: TAbbreviationRule;
  rules: TAbbreviationRules;
begin
  case form of
    anLong: rules := GetData.Long;
    anShort: rules := GetData.Short;
    anCurrency: rules := GetData.Currency;
  end;

  for rule in rules do
  begin
    if rule.Range <= value then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

class function TAbbreviatedNumber.FormatShort(
  value: Double;
  precision: Integer): String;
begin
  Result := DoFormat(value, anShort, precision, GetData.Short);
end;

class function TAbbreviatedNumber.FormatLong(
  value: Double;
  precision: Integer): String;
begin
  Result := DoFormat(value, anLong, precision, GetData.Long);
end;

class function TAbbreviatedNumber.FormatCurrency(
  value: Double;
  precision: Integer): String;
begin
  Result := DoFormat(value, anCurrency, precision, GetData.Currency);
end;

class function TAbbreviatedNumber.Format(
  form: TAbbreviatedNumberForm;
  value: Double;
  precision: Integer): String;
begin
  case form of
    anLong: Result := FormatLong(value, precision);
    anShort: Result := FormatShort(value, precision);
    anCurrency: Result := FormatCurrency(value, precision);
  end;
end;

function CompareRules(const left, right: TAbbreviationRule): Integer;
begin
  if left.Range = right.Range then
    Result := TComparer<TPlural>.Default.Compare(left.Plural, right.Plural)
  else
    Result := TComparer<Int64>.Default.Compare(left.Range, right.Range);
end;

function ReverseCompareRules(const left, right: TAbbreviationRule): Integer;
begin
  Result := -CompareRules(left, right);
end;

class procedure TAbbreviatedNumber.Register(
  const id: String;
  longRules, shortRules, currencyRules: array of TAbbreviationRule);

  function GetArray(value: array of TAbbreviationRule): TAbbreviationRules;
  begin
    SetLength(Result, Length(value));
    Move(value[Low(value)], Result[Low(Result)], SizeOf(value));
    TArray.Sort<TAbbreviationRule>(Result, TDelegatedComparer<TAbbreviationRule>.Construct(ReverseCompareRules));
  end;

var
  data: TData;
begin
  if not FDatas.TryGetValue(id, data) then
  begin
    data := TData.Create;
    FDatas.Add(id, data);
  end;

  data.Long := GetArray(longRules);
  data.Short := GetArray(shortRules);
  data.Currency := GetArray(currencyRules);
end;


const
  EMPTY: array[0..0] of TAbbreviationRule =
  (
    (Range: 0; Plural: pfOther; Value: '')
  );
initialization
  FDatas := TDictionary<String, TData>.Create;
  TAbbreviatedNumber.Register('', EMPTY, EMPTY, EMPTY);
end.
