unit LocalizationTests;

interface

uses
  TestFramework;

type
  TLocalizationTests = class(TTestCase)
  published
    procedure IsLocaleAsian;
  end;

implementation

uses
  NtLocalization;

procedure TLocalizationTests.IsLocaleAsian;

  procedure Process(const value: String; expected: Boolean);
  begin
    CheckEquals(expected, TNtLocale.IsLocaleAsian(value), value);
  end;

begin
  // ISO ids used to be not detected
  Process('ja', True);
  Process('ja-JP', True);
  Process('zh', True);
  Process('zh-CN', True);
  Process('zh-Hant-TW', True);
  Process('zh_TW', True);
  Process('ko', True);
  Process('ko-KR', True);

  // Windows language abbreviations
  Process('JPN', True);
  Process('CHS', True);
  Process('CHT', True);
  Process('KOR', True);
  Process('JP', True);
  Process('CH', True);

  Process('en', False);
  Process('en-US', False);
  Process('fi', False);
  Process('chr-US', False);  // Cherokee
  Process('', False);
end;

initialization
  RegisterTest(TLocalizationTests.Suite);
end.
