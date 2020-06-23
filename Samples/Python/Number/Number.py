import gettext
import locale
from enum import Enum

gettext.bindtextdomain('Number', 'locale')
gettext.textdomain('Number')
_ = gettext.gettext

class AbbreviatedNumberForm(Enum):
  Long = 1
  Short = 2
  Currenty = 3

class PluralForm(Enum):
  One = 1
  Other = 2

class AbbreviationRule
  Range
  Plural
  Value

  def __init__(range, plural, value):
    Range = range
    Plural = plural
    Value = value

  EN_LONG = []
  EN_LONG.append(AbbreviationRule(1000, PluralForm.One, '0 thousand'))
  EN_LONG.append(AbbreviationRule(1000, PluralForm.pfOther, '0 thousand'))
  EN_LONG.append(AbbreviationRule(10000, PluralForm.One, '00 thousand'))
  EN_LONG.append(AbbreviationRule(10000, PluralForm.pfOther, '00 thousand'))
  EN_LONG.append(AbbreviationRule(100000, PluralForm.One, '000 thousand'))
  EN_LONG.append(AbbreviationRule(100000, PluralForm.pfOther, '000 thousand'))
  EN_LONG.append(AbbreviationRule(1000000, PluralForm.One, '0 million'))
  EN_LONG.append(AbbreviationRule(1000000, PluralForm.pfOther, '0 million'))
  EN_LONG.append(AbbreviationRule(10000000, PluralForm.One, '00 million'))
  EN_LONG.append(AbbreviationRule(10000000, PluralForm.pfOther, '00 million'))
  EN_LONG.append(AbbreviationRule(100000000, PluralForm.One, '000 million'))
  EN_LONG.append(AbbreviationRule(100000000, PluralForm.pfOther, '000 million'))
  EN_LONG.append(AbbreviationRule(1000000000, PluralForm.One, '0 billion'))
  EN_LONG.append(AbbreviationRule(1000000000, PluralForm.pfOther, '0 billion'))
  EN_LONG.append(AbbreviationRule(10000000000, PluralForm.One, '00 billion'))
  EN_LONG.append(AbbreviationRule(10000000000, PluralForm.pfOther, '00 billion'))
  EN_LONG.append(AbbreviationRule(100000000000, PluralForm.One, '000 billion'))
  EN_LONG.append(AbbreviationRule(100000000000, PluralForm.pfOther, '000 billion'))
  EN_LONG.append(AbbreviationRule(1000000000000, PluralForm.One, '0 trillion'))
  EN_LONG.append(AbbreviationRule(1000000000000, PluralForm.pfOther, '0 trillion'))
  EN_LONG.append(AbbreviationRule(10000000000000, PluralForm.One, '00 trillion'))
  EN_LONG.append(AbbreviationRule(10000000000000, PluralForm.pfOther, '00 trillion'))
  EN_LONG.append(AbbreviationRule(100000000000000, PluralForm.One, '000 trillion'))
  EN_LONG.append(AbbreviationRule(100000000000000, PluralForm.pfOther, '000 trillion'))


def GetValuePrecision(value):
  result = 0
  valueRange = 1

  while valueRange <= value:
    valueRange = 10*valueRange
    result = result + 1

  return result

def GetStr(value):
  decimalSeparator = locale.localeconv('decimal_point')

  valuePrecision = GetValuePrecision(value)
  value = RoundTo(value, valuePrecision - precision)
  result = FloatToStrF(value, ffFixed, 10, Max(0, precision - valuePrecision))

  if result.find(decimalSeparator) >= 0:
    while result[-1] == '0':
      result = result[1:]

    if result[-1] == decimalSeparator:
      result = result[1:]
  
  return result


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
        Result := CurrToStrF(value, ffCurrency, Max(0, precision - valuePrecision))
    end
    else
      Result := GetStr(value);
  end;

  function Find(range: Int64; plural: TPluralForm; var rule: TAbbreviationRule): Boolean;
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

def DoFormat(value, form, precision, rules):

var
  p: Integer;
  count, range: Int64;
  displayValue: Double;
  str: String;
  rule, newRule: TAbbreviationRule;
  proc: TPluralIndexProc;
  plural: TPluralForm;
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
        proc := TNtPlural.GetIndexProc('');
        plural := proc(count);

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

def FormatLong(value, precision):
  return value

def FormatShort(value, precision):
  return value

def FormatCurrency(value, precision):
  return value

def NumberFormat(form, value, precision):
  if form == AbbreviatedNumberForm.Long:
    return FormatLong(value, precision)
  elif form == AbbreviatedNumberForm.Short:
    return FormatShort(value, precision)
  else:
    return FormatCurrency(value, precision)

def process(value):
  '''function_docstring'''
  print(NumberFormat(AbbreviatedNumberForm.Long, value, 1), end='')
  print(" " + str(value))
  

print(_("Number Abbreviation Sample"))

for value in [1, 2.4, 121, 1000, 1650, 27450, 190394, 3400000, 54000000, 670000000, 1200000000, 20200000000, 410000000000, 9520000000000]:
  process(value)