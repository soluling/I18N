unit PatternTests;

interface

uses
  System.Classes,
  TestFramework,
  NtPattern;

type
  TPatternTests = class(TTestCase)
  private
    procedure Same(const value1, value2: String);
    procedure Form(count: Integer; plural: TPlural);
    procedure Pattern(count: Integer; const format: String; const pattern: String);
    procedure Format(count: Integer; const format: String; const value: String);

  published
    procedure StandardIcu;
    procedure StandardLegacy;
    procedure EscapeIcu;
    procedure EscapeLegacy;
    procedure CustomIcu;
    procedure CustomLegacy;

    procedure OperatorRangeIcu;
    procedure OperatorIcu;
    procedure OperatorLessThanIcu;

    procedure FormatsIcu;
    procedure FormatsLegacy;

    procedure GendersIcu;
    procedure GendersLegacy;

    procedure MultiIcu;
    procedure MultiLegacy;

    procedure EnglishForm;
    procedure EnglishIcu;
    procedure EnglishCompact;
    procedure EnglishShared;
    procedure EnglishLegacy;

    procedure FrenchForm;
    procedure FrenchIcu;
    procedure FrenchLegacy;

    procedure JapaneseForm;
    procedure JapaneseIcu;
    procedure JapaneseLegacy;

    procedure RussianForm;
    procedure RussianIcu;
    procedure RussianLegacy;

    procedure CzechForm;
    procedure CzechIcu;
    procedure CzechLegacy;

    procedure IrishForm;
    procedure IrishIcu;
    procedure IrishLegacy;

    procedure ArabicForm;
    procedure ArabicIcu;
    procedure ArabicLegacy;

    procedure IcelandicForm;
    procedure IcelandicIcu;
    procedure IcelandicLegacy;

    procedure LatvianForm;
    procedure LatvianIcu;
    procedure LatvianLegacy;

    procedure LithuanianForm;
    procedure LithuanianIcu;
    procedure LithuanianLegacy;

    procedure MacedonianForm;
    procedure MacedonianIcu;
    procedure MacedonianLegacy;

    procedure MalteseForm;
    procedure MalteseIcu;
    procedure MalteseLegacy;

    procedure PolishForm;
    procedure PolishIcu;
    procedure PolishLegacy;

    procedure RomanianForm;
    procedure RomanianIcu;
    procedure RomanianLegacy;

    procedure SlovenianForm;
    procedure SlovenianIcu;
    procedure SlovenianLegacy;

    procedure WelshForm;
    procedure WelshIcu;
    procedure WelshLegacy;

    procedure SinglePlural;
    procedure ZeroSinglePlural;
    procedure Range;
    procedure Operators;
    procedure Gender;
    procedure Multi2;
  end;

implementation

uses
  NtBase;

const
  ZERO = 'No files';
  ONE = '%d file';
  TWO = 'Two files';
  FEW = 'Few files';
  MANY = 'Many files';
  OTHER = '%d files';

  OneOtherIcu = '{files, plural, one {%d file} other {%d files}}';
  OneOtherLegacy = 'one;%d file;other;%d files';

  ZeroOneOtherIcu = '{files, plural, zero {No files} one {%d file} other {%d files}}';
  ZeroOneOtherLegacy = 'zero;No files;one;%d file;other;%d files';

  OneFewManyOtherIcu = '{files, plural, one {%d file} few {Few files} many {Many files} other {%d files}}';
  OneFewManyOtherLegacy = 'one;%d file;few;Few files;many;Many files;other;%d files';

  OneFewManyIcu = '{files, plural, one {%d file} few {Few files} many {Many files}}';
  OneFewManyLegacy = 'one;%d file;few;Few files;many;Many files';

  OneFewOtherIcu = '{files, plural, one {%d file} few {Few files} other {%d files}}';
  OneFewOtherLegacy = 'one;%d file;few;Few files;other;%d files';

  OneTwoFewManyOtherIcu = '{files, plural, one {%d file} two {Two files} few {Few files} many {Many files} other {%d files}}';
  OneTwoFewManyOtherLegacy = 'one;%d file;two;Two files;few;Few files;many;Many files;other;%d files';

  OneTwoFewOtherIcu = '{files, plural, one {%d file} two {Two files} few {Few files} other {%d files}}';
  OneTwoFewOtherLegacy = 'one;%d file;two;Two files;few;Few files;other;%d files';

  OneTwoOtherIcu = '{files, plural, one {%d file} two {Two files} other {%d files}}';
  OneTwoOtherLegacy = 'one;%d file;two;Two files;other;%d files';

  OtherIcu = '{files, plural, other {%d files}}';
  OtherLegacy = 'other;%d files';

  ZeroOneFewManyOtherIcu = '{files, plural, zero {No files} one {%d file} few {Few files} many {Many files} other {%d files}}';
  ZeroOneFewManyOtherLegacy = 'zero;No files;one;%d file;few;Few files;many;Many files;other;%d files';

  ZeroOneFewManyIcu = '{files, plural, zero {No files} one {%d file} few {Few files} many {Many files}}';
  ZeroOneFewManyLegacy = 'zero;No files;one;%d file;few;Few files;many;Many files';

  ZeroOneFewOtherIcu = '{files, plural, zero {No files} one {%d file} few {Few files} other {%d files}}';
  ZeroOneFewOtherLegacy = 'zero;No files;one;%d file;few;Few files;other;%d files';

  ZeroOneTwoFewManyOtherIcu = '{files, plural, zero {No files} one {%d file} two {Two files} few {Few files} many {Many files} other {%d files}}';
  ZeroOneTwoFewManyOtherLegacy = 'zero;No files;one;%d file;two;Two files;few;Few files;many;Many files;other;%d files';

  ZeroOneTwoFewOtherIcu = '{files, plural, zero {No files} one {%d file} two {Two files} few {Few files} other {%d files}}';
  ZeroOneTwoFewOtherLegacy = 'zero;No files;one;%d file;two;Two files;few;Few files;other;%d files';

  ZeroOneTwoOtherIcu = '{files, plural, zero {No files} one {%d file} two {Two files} other {%d files}}';
  ZeroOneTwoOtherLegacy = 'zero;No files;one;%d file;two;Two files;other;%d files';

  ZeroOtherIcu = '{files, plural, zero {No files} other {%d files}}';
  ZeroOtherLegacy = 'zero;No files;other;%d files';


procedure TPatternTests.Same(const value1, value2: String);
begin
  CheckEquals(value1, value2);
end;

procedure TPatternTests.Form(count: Integer; plural: TPlural);
var
  func: TPluralProc;
begin
  func := TMultiPattern.GetProc();
  CheckEquals(Ord(func(count, count, 0, 0, 0, 0)), Ord(plural));
end;

procedure TPatternTests.Pattern(count: Integer; const format: String; const pattern: String);
begin
  CheckEquals(TMultiPattern.GetPattern(format, count), pattern);
end;

procedure TPatternTests.Format(count: Integer; const format: String; const value: String);
begin
  CheckEquals(TMultiPattern.Format(format, count), value);
end;


procedure TPatternTests.StandardIcu;
const
  VALUE = '{file, plural, one {%d file} other {%d files}}';
begin
  DefaultLocale := 'en';
  Pattern(0, VALUE, '%d files');
  Pattern(1, VALUE, '%d file');
  Pattern(2, VALUE, '%d files');
end;

procedure TPatternTests.StandardLegacy;
const
  VALUE = 'one;%d file;other;%d files';
begin
  DefaultLocale := 'en';
  Pattern(0, VALUE, '%d files');
  Pattern(1, VALUE, '%d file');
  Pattern(2, VALUE, '%d files');
end;

procedure TPatternTests.EscapeIcu;
const
  VALUE = 'I have \{ \\  \} {files, plural, one {%d \{ \\ \} file} other {%d \{ \\ \} files}}';
begin
  DefaultLocale := 'en';
  Pattern(0, VALUE, '%d { \ } files');
  Pattern(1, VALUE, '%d { \ } file');
  Pattern(2, VALUE, '%d { \ } files');
end;

procedure TPatternTests.EscapeLegacy;
const
  VALUE = 'one;Item;;%d car;other;Item;;%d cars';
begin
  DefaultLocale := 'en';
  Pattern(0, VALUE, 'Item;%d cars');
  Pattern(1, VALUE, 'Item;%d car');
  Pattern(2, VALUE, 'Item;%d cars');
end;

procedure TPatternTests.OperatorRangeIcu;
const
  VALUE = 'I have {file, plural, 3..4 {few cars} other {%d cars}}.';
begin
  DefaultLocale := 'en';
  Pattern(3, VALUE, 'few cars');
  Pattern(4, VALUE, 'few cars');
  Pattern(5, VALUE, '%d cars');
end;

procedure TPatternTests.OperatorIcu;
const
  VALUE = 'I have {file, plural, =0 {no cars} =1 {one car} =2 {two cars} 3..4 {few cars} ~12 {dozen cars} other {%d cars}}.';
begin
  DefaultLocale := 'en';
  Same(TMultiPattern.Format(VALUE, 0), 'I have no cars.');
  Same(TMultiPattern.Format(VALUE, 1), 'I have one car.');
  Same(TMultiPattern.Format(VALUE, 2), 'I have two cars.');
  Same(TMultiPattern.Format(VALUE, 3), 'I have few cars.');
  Same(TMultiPattern.Format(VALUE, 4), 'I have few cars.');
  Same(TMultiPattern.Format(VALUE, 5), 'I have 5 cars.');
  Same(TMultiPattern.Format(VALUE, 10), 'I have 10 cars.');
  Same(TMultiPattern.Format(VALUE, 11), 'I have dozen cars.');
  Same(TMultiPattern.Format(VALUE, 12), 'I have dozen cars.');
  Same(TMultiPattern.Format(VALUE, 13), 'I have dozen cars.');
end;

procedure TPatternTests.OperatorLessThanIcu;
const
  VALUE = 'I have {file, plural, <5 {a few cars} >=5 {many cars} >=10 {plenty of cars}}.';
begin
  DefaultLocale := 'en';
  Same(TMultiPattern.Format(VALUE, 0), 'I have a few cars.');
  Same(TMultiPattern.Format(VALUE, 1), 'I have a few cars.');
  Same(TMultiPattern.Format(VALUE, 2), 'I have a few cars.');
  Same(TMultiPattern.Format(VALUE, 3), 'I have a few cars.');
  Same(TMultiPattern.Format(VALUE, 4), 'I have a few cars.');
  Same(TMultiPattern.Format(VALUE, 5), 'I have many cars.');
  Same(TMultiPattern.Format(VALUE, 6), 'I have many cars.');
  Same(TMultiPattern.Format(VALUE, 10), 'I have plenty of cars.');
  Same(TMultiPattern.Format(VALUE, 11), 'I have plenty of cars.');
end;

procedure TPatternTests.CustomIcu;
const
  VALUE = '{files, plural, zero {No files} one {One file} two {Two files} other {%d files}}';
begin
  DefaultLocale := 'en';
  Pattern(0, VALUE, 'No files');
  Pattern(1, VALUE, 'One file');
  Pattern(2, VALUE, 'Two files');
  Pattern(3, VALUE, '%d files');
end;

procedure TPatternTests.CustomLegacy;
const
  VALUE = 'zero;No files;one;One file;two;Two files;other;%d files';
begin
  DefaultLocale := 'en';
  Pattern(0, VALUE, 'No files');
  Pattern(1, VALUE, 'One file');
  Pattern(2, VALUE, 'Two files');
  Pattern(3, VALUE, '%d files');
end;

procedure TPatternTests.FormatsIcu;
const
  VALUE = '{cars, plural, zero {I have no cars} one {I have one car} other {I have %d cars}}';
begin
  DefaultLocale := 'en';
  Same(TMultiPattern.Format(VALUE, 0), 'I have no cars');
  Same(TMultiPattern.Format(VALUE, 1), 'I have one car');
  Same(TMultiPattern.Format(VALUE, 2), 'I have 2 cars');
end;

procedure TPatternTests.FormatsLegacy;
const
  VALUE = 'zero;I have no cars;one;I have one car;other;I have %d cars';
begin
  DefaultLocale := 'en';
  Same(TMultiPattern.Format(VALUE, 0), 'I have no cars');
  Same(TMultiPattern.Format(VALUE, 1), 'I have one car');
  Same(TMultiPattern.Format(VALUE, 2), 'I have 2 cars');
end;

procedure TPatternTests.GendersIcu;
const
  VALUE = '{name, gender, neutral {%s will bring the car} male {%s will bring his car} female {%s will bring her car}}';
begin
  DefaultLocale := 'en';
  Same(TMultiPattern.Format(VALUE, geMale, ['John']), 'John will bring his car');
  Same(TMultiPattern.Format(VALUE, geFemale, ['Jill']), 'Jill will bring her car');
  Same(TMultiPattern.Format(VALUE, geNeutral, ['Somebody']), 'Somebody will bring the car');
end;

procedure TPatternTests.GendersLegacy;
const
  VALUE = 'neutral;%s will bring the car;male;%s will bring his car;female;%s will bring her car';
begin
  DefaultLocale := 'en';
  Same(TMultiPattern.Format(VALUE, geMale, ['John']), 'John will bring his car');
  Same(TMultiPattern.Format(VALUE, geFemale, ['Jill']), 'Jill will bring her car');
  Same(TMultiPattern.Format(VALUE, geNeutral, ['Somebody']), 'Somebody will bring the car');
end;

const
  MALE_C = 'John';
  FEMALE_C = 'Jill';
  ALMA_C = 'Alma';


procedure TPatternTests.MultiIcu;
const
  MULTI_C = '{name, gender, male {%s will bring his} female {%s will bring her}} {cars, plural, one {car} other {%d cars}}.';

  JOHN_C: array[0..1] of TFormatParameter =
  (
    (Kind: fpGender; Gender: geMale; Value: 'John'),
    (Kind: fpPlural; Plural: 2)
  );

  BILL_C: array[0..1] of TFormatParameter =
  (
    (Kind: fpGender; Gender: geMale; Value: 'Bill'),
    (Kind: fpPlural; Plural: 1)
  );

  JILL_C: array[0..1] of TFormatParameter =
  (
    (Kind: fpGender; Gender: geFemale; Value: 'Jill'),
    (Kind: fpPlural; Plural: 3)
  );

  ALMA_C: array[0..1] of TFormatParameter =
  (
    (Kind: fpGender; Gender: geFemale; Value: 'Alma'),
    (Kind: fpPlural; Plural: 1)
  );
begin
  DefaultLocale := 'en';

  Same(
    TMultiPattern.FormatMulti(MULTI_C, JOHN_C),
    'John will bring his 2 cars.');

  Same(
    TMultiPattern.FormatMulti(MULTI_C, BILL_C),
    'Bill will bring his car.');

  Same(
    TMultiPattern.FormatMulti(MULTI_C, JILL_C),
    'Jill will bring her 3 cars.');

  Same(
    TMultiPattern.FormatMulti(MULTI_C, ALMA_C),
    'Alma will bring her car.');
end;

procedure TPatternTests.MultiLegacy;
const
  MULTI_C = 'male;%s will bring his %s;female;%s will bring her %s;next;one;car.;other;%d cars.';

  JOHN_C: array[0..1] of TFormatParameter =
  (
    (Kind: fpGender; Gender: geMale; Value: 'John'),
    (Kind: fpPlural; Plural: 2)
  );

  BILL_C: array[0..1] of TFormatParameter =
  (
    (Kind: fpGender; Gender: geMale; Value: 'Bill'),
    (Kind: fpPlural; Plural: 1)
  );

  JILL_C: array[0..1] of TFormatParameter =
  (
    (Kind: fpGender; Gender: geFemale; Value: 'Jill'),
    (Kind: fpPlural; Plural: 3)
  );

  ALMA_C: array[0..1] of TFormatParameter =
  (
    (Kind: fpGender; Gender: geFemale; Value: 'Alma'),
    (Kind: fpPlural; Plural: 1)
  );
begin
  DefaultLocale := 'en';

  Same(
    TMultiPattern.FormatMulti(MULTI_C, JOHN_C),
    'John will bring his 2 cars.');

  Same(
    TMultiPattern.FormatMulti(MULTI_C, BILL_C),
    'Bill will bring his car.');

  Same(
    TMultiPattern.FormatMulti(MULTI_C, JILL_C),
    'Jill will bring her 3 cars.');

  Same(
    TMultiPattern.FormatMulti(MULTI_C, ALMA_C),
    'Alma will bring her car.');
end;

procedure TPatternTests.EnglishForm;
begin
  DefaultLocale := 'en';
  Form(0, pfOther);
  Form(1, pfOne);
  Form(2, pfOther);
  Form(10, pfOther);
end;

procedure TPatternTests.EnglishIcu;
begin
  DefaultLocale := 'en';
  Pattern(0, OneOtherIcu, OTHER);
  Pattern(1, OneOtherIcu, ONE);
  Pattern(2, OneOtherIcu, OTHER);
  Pattern(10, OneOtherIcu, OTHER);

  Pattern(0, ZeroOneOtherIcu, ZERO);
  Pattern(1, ZeroOneOtherIcu, ONE);
  Pattern(2, ZeroOneOtherIcu, OTHER);
  Pattern(10, ZeroOneOtherIcu, OTHER);
end;

procedure TPatternTests.EnglishLegacy;
begin
  DefaultLocale := 'en';
  Pattern(0, OneOtherLegacy, OTHER);
  Pattern(1, OneOtherLegacy, ONE);
  Pattern(2, OneOtherLegacy, OTHER);
  Pattern(10, OneOtherLegacy, OTHER);

  Pattern(0, ZeroOneOtherLegacy, ZERO);
  Pattern(1, ZeroOneOtherLegacy, ONE);
  Pattern(2, ZeroOneOtherLegacy, OTHER);
  Pattern(10, ZeroOneOtherLegacy, OTHER);
end;

procedure TPatternTests.EnglishShared;
const
  PATTERN_C = 'I have {file, plural, one {%d file} other {%d files}}.';
  ZERO_PATTERN_C = 'I have {file, plural, =0 {no files} one {%d file} other {%d files}}.';
begin
  DefaultLocale := 'en';
  Format(0, PATTERN_C, 'I have 0 files.');
  Format(1, PATTERN_C, 'I have 1 file.');
  Format(2, PATTERN_C, 'I have 2 files.');
  Format(10, PATTERN_C, 'I have 10 files.');

  Format(0, ZERO_PATTERN_C, 'I have no files.');
  Format(1, ZERO_PATTERN_C, 'I have 1 file.');
  Format(2, ZERO_PATTERN_C, 'I have 2 files.');
  Format(10, ZERO_PATTERN_C, 'I have 10 files.');
end;

procedure TPatternTests.EnglishCompact;
const
  PATTERN_C = '{,plural,one{%d file}other{%d files}}';
begin
  DefaultLocale := 'en';
  Pattern(0, PATTERN_C, OTHER);
  Pattern(1, PATTERN_C, ONE);
  Pattern(2, PATTERN_C, OTHER);
  Pattern(10, PATTERN_C, OTHER);
end;

procedure TPatternTests.FrenchForm;
begin
  DefaultLocale := 'fr';
  Form(0, pfOne);
  Form(1, pfOne);
  Form(2, pfOther);
  Form(10, pfOther);
end;

procedure TPatternTests.FrenchIcu;
begin
  DefaultLocale := 'fr';
  Pattern(0, OneOtherIcu, ONE);
  Pattern(1, OneOtherIcu, ONE);
  Pattern(2, OneOtherIcu, OTHER);
  Pattern(10, OneOtherIcu, OTHER);

  Pattern(0, ZeroOneOtherIcu, ZERO);
  Pattern(1, ZeroOneOtherIcu, ONE);
  Pattern(2, ZeroOneOtherIcu, OTHER);
  Pattern(10, ZeroOneOtherIcu, OTHER);
end;

procedure TPatternTests.FrenchLegacy;
begin
  DefaultLocale := 'fr';
  Pattern(0, OneOtherLegacy, ONE);
  Pattern(1, OneOtherLegacy, ONE);
  Pattern(2, OneOtherLegacy, OTHER);
  Pattern(10, OneOtherLegacy, OTHER);

  Pattern(0, ZeroOneOtherLegacy, ZERO);
  Pattern(1, ZeroOneOtherLegacy, ONE);
  Pattern(2, ZeroOneOtherLegacy, OTHER);
  Pattern(10, ZeroOneOtherLegacy, OTHER);
end;

procedure TPatternTests.JapaneseForm;
begin
  DefaultLocale := 'ja';

  Form(0, pfOther);
  Form(1, pfOther);
  Form(2, pfOther);
  Form(10, pfOther);
end;

procedure TPatternTests.JapaneseIcu;
begin
  DefaultLocale := 'ja';

  Pattern(0, OtherIcu, OTHER);
  Pattern(1, OtherIcu, OTHER);
  Pattern(2, OtherIcu, OTHER);
  Pattern(10, OtherIcu, OTHER);

  Pattern(0, ZeroOtherIcu, ZERO);
  Pattern(1, ZeroOtherIcu, OTHER);
  Pattern(2, ZeroOtherIcu, OTHER);
  Pattern(10, ZeroOtherIcu, OTHER);
end;

procedure TPatternTests.JapaneseLegacy;
begin
  DefaultLocale := 'ja';

  Pattern(0, OtherLegacy, OTHER);
  Pattern(1, OtherLegacy, OTHER);
  Pattern(2, OtherLegacy, OTHER);
  Pattern(10, OtherLegacy, OTHER);

  Pattern(0, ZeroOtherLegacy, ZERO);
  Pattern(1, ZeroOtherLegacy, OTHER);
  Pattern(2, ZeroOtherLegacy, OTHER);
  Pattern(10, ZeroOtherLegacy, OTHER);
end;

procedure TPatternTests.RussianForm;
begin
  DefaultLocale := 'ru';

  Form(0, pfMany);
  Form(1, pfOne);
  Form(2, pfFew);
  Form(3, pfFew);
  Form(4, pfFew);
  Form(5, pfMany);
  Form(10, pfMany);

  Form(11, pfMany);
  Form(12, pfMany);
  Form(13, pfMany);
  Form(14, pfMany);
  Form(15, pfMany);

  Form(21, pfOne);
  Form(22, pfFew);
  Form(23, pfFew);
  Form(24, pfFew);
  Form(25, pfMany);

  Form(101, pfOne);
  Form(102, pfFew);
  Form(103, pfFew);
  Form(104, pfFew);
  Form(105, pfMany);

  Form(111, pfMany);
  Form(112, pfMany);
  Form(113, pfMany);
  Form(114, pfMany);
  Form(115, pfMany);

  Form(121, pfOne);
  Form(122, pfFew);
  Form(123, pfFew);
  Form(124, pfFew);
  Form(125, pfMany);
end;

procedure TPatternTests.RussianIcu;
begin
  DefaultLocale := 'ru';

  Pattern(0, OneFewManyIcu, MANY);
  Pattern(1, OneFewManyIcu, ONE);
  Pattern(2, OneFewManyIcu, FEW);
  Pattern(10, OneFewManyIcu, MANY);

  Pattern(0, ZeroOneFewManyIcu, ZERO);
  Pattern(1, ZeroOneFewManyIcu, ONE);
  Pattern(2, ZeroOneFewManyIcu, FEW);
  Pattern(10, ZeroOneFewManyIcu, MANY);
end;

procedure TPatternTests.RussianLegacy;
begin
  DefaultLocale := 'ru';

  Pattern(0, OneFewManyLegacy, MANY);
  Pattern(1, OneFewManyLegacy, ONE);
  Pattern(2, OneFewManyLegacy, FEW);
  Pattern(10, OneFewManyLegacy, MANY);

  Pattern(0, ZeroOneFewManyLegacy, ZERO);
  Pattern(1, ZeroOneFewManyLegacy, ONE);
  Pattern(2, ZeroOneFewManyLegacy, FEW);
  Pattern(10, ZeroOneFewManyLegacy, MANY);
end;

procedure TPatternTests.CzechForm;
begin
  DefaultLocale := 'cs';

  Form(0, pfOther);
  Form(1, pfOne);
  Form(2, pfFew);
  Form(3, pfFew);
  Form(4, pfFew);
  Form(5, pfOther);
  Form(10, pfOther);

  Form(11, pfOther);
  Form(12, pfOther);
  Form(13, pfOther);
  Form(14, pfOther);
  Form(15, pfOther);

  Form(101, pfOther);
  Form(102, pfOther);
  Form(103, pfOther);
  Form(104, pfOther);
  Form(105, pfOther);
end;

procedure TPatternTests.CzechIcu;
begin
  DefaultLocale := 'cs';

  Pattern(0, OneFewOtherIcu, OTHER);
  Pattern(1, OneFewOtherIcu, ONE);
  Pattern(2, OneFewOtherIcu, FEW);
  Pattern(10, OneFewOtherIcu, OTHER);

  Pattern(0, ZeroOneFewOtherIcu, ZERO);
  Pattern(1, ZeroOneFewOtherIcu, ONE);
  Pattern(2, ZeroOneFewOtherIcu, FEW);
  Pattern(10, ZeroOneFewOtherIcu, OTHER);
end;

procedure TPatternTests.CzechLegacy;
begin
  DefaultLocale := 'cs';

  Pattern(0, OneFewOtherLegacy, OTHER);
  Pattern(1, OneFewOtherLegacy, ONE);
  Pattern(2, OneFewOtherLegacy, FEW);
  Pattern(10, OneFewOtherLegacy, OTHER);

  Pattern(0, ZeroOneFewOtherLegacy, ZERO);
  Pattern(1, ZeroOneFewOtherLegacy, ONE);
  Pattern(2, ZeroOneFewOtherLegacy, FEW);
  Pattern(10, ZeroOneFewOtherLegacy, OTHER);
end;

procedure TPatternTests.IrishForm;
begin
  DefaultLocale := 'ga';

  Form(0, pfOther);
  Form(1, pfOne);
  Form(2, pfTwo);
  Form(3, pfFew);
  Form(4, pfFew);
  Form(5, pfFew);
  Form(10, pfMany);
  Form(20, pfOther);
end;

procedure TPatternTests.IrishIcu;
begin
  DefaultLocale := 'ga';

  Pattern(0, OneTwoFewManyOtherIcu, OTHER);
  Pattern(1, OneTwoFewManyOtherIcu, ONE);
  Pattern(2, OneTwoFewManyOtherIcu, TWO);
  Pattern(3, OneTwoFewManyOtherIcu, FEW);
  Pattern(10, OneTwoFewManyOtherIcu, MANY);
  Pattern(20, OneTwoFewManyOtherIcu, OTHER);

  Pattern(0, ZeroOneTwoFewManyOtherIcu, ZERO);
  Pattern(1, ZeroOneTwoFewManyOtherIcu, ONE);
  Pattern(2, ZeroOneTwoFewManyOtherIcu, TWO);
  Pattern(3, ZeroOneTwoFewManyOtherIcu, FEW);
  Pattern(10, ZeroOneTwoFewManyOtherIcu, MANY);
  Pattern(20, ZeroOneTwoFewManyOtherIcu, OTHER);
end;


procedure TPatternTests.IrishLegacy;
begin
  DefaultLocale := 'ga';

  Pattern(0, OneTwoFewManyOtherLegacy, OTHER);
  Pattern(1, OneTwoFewManyOtherLegacy, ONE);
  Pattern(2, OneTwoFewManyOtherLegacy, TWO);
  Pattern(3, OneTwoFewManyOtherLegacy, FEW);
  Pattern(10, OneTwoFewManyOtherLegacy, MANY);
  Pattern(20, OneTwoFewManyOtherLegacy, OTHER);

  Pattern(0, ZeroOneTwoFewManyOtherLegacy, ZERO);
  Pattern(1, ZeroOneTwoFewManyOtherLegacy, ONE);
  Pattern(2, ZeroOneTwoFewManyOtherLegacy, TWO);
  Pattern(3, ZeroOneTwoFewManyOtherLegacy, FEW);
  Pattern(10, ZeroOneTwoFewManyOtherLegacy, MANY);
  Pattern(20, ZeroOneTwoFewManyOtherLegacy, OTHER);
end;

procedure TPatternTests.ArabicForm;
begin
  DefaultLocale := 'ar';

  Form(0, pfZero);
  Form(1, pfOne);
  Form(2, pfTwo);
  Form(3, pfFew);
  Form(4, pfFew);
  Form(5, pfFew);
  Form(10, pfFew);

  Form(11, pfMany);
  Form(20, pfMany);
  Form(99, pfMany);
  Form(100, pfOther);
  Form(101, pfOther);
end;

procedure TPatternTests.ArabicIcu;
begin
  DefaultLocale := 'ar';

  Pattern(0, ZeroOneTwoFewManyOtherIcu, ZERO);
  Pattern(1, ZeroOneTwoFewManyOtherIcu, ONE);
  Pattern(2, ZeroOneTwoFewManyOtherIcu, TWO);
  Pattern(3, ZeroOneTwoFewManyOtherIcu, FEW);
  Pattern(11, ZeroOneTwoFewManyOtherIcu, MANY);
  Pattern(100, ZeroOneTwoFewManyOtherIcu, OTHER);
end;

procedure TPatternTests.ArabicLegacy;
begin
  DefaultLocale := 'ar';

  Pattern(0, ZeroOneTwoFewManyOtherLegacy, ZERO);
  Pattern(1, ZeroOneTwoFewManyOtherLegacy, ONE);
  Pattern(2, ZeroOneTwoFewManyOtherLegacy, TWO);
  Pattern(3, ZeroOneTwoFewManyOtherLegacy, FEW);
  Pattern(11, ZeroOneTwoFewManyOtherLegacy, MANY);
  Pattern(100, ZeroOneTwoFewManyOtherLegacy, OTHER);
end;

procedure TPatternTests.IcelandicForm;
begin
  DefaultLocale := 'is';

  Form(0, pfOther);
  Form(1, pfOne);
  Form(2, pfOther);
  Form(3, pfOther);
  Form(4, pfOther);
  Form(5, pfOther);
  Form(10, pfOther);

  Form(11, pfOther);
  Form(21, pfOne);
end;

procedure TPatternTests.IcelandicIcu;
begin
  DefaultLocale := 'is';

  Pattern(0, OneOtherIcu, OTHER);
  Pattern(1, OneOtherIcu, ONE);
  Pattern(2, OneOtherIcu, OTHER);
  Pattern(10, OneOtherIcu, OTHER);

  Pattern(0, ZeroOneOtherIcu, ZERO);
  Pattern(1, ZeroOneOtherIcu, ONE);
  Pattern(2, ZeroOneOtherIcu, OTHER);
  Pattern(10, ZeroOneOtherIcu, OTHER);
end;

procedure TPatternTests.IcelandicLegacy;
begin
  DefaultLocale := 'is';

  Pattern(0, OneOtherLegacy, OTHER);
  Pattern(1, OneOtherLegacy, ONE);
  Pattern(2, OneOtherLegacy, OTHER);
  Pattern(10, OneOtherLegacy, OTHER);

  Pattern(0, ZeroOneOtherLegacy, ZERO);
  Pattern(1, ZeroOneOtherLegacy, ONE);
  Pattern(2, ZeroOneOtherLegacy, OTHER);
  Pattern(10, ZeroOneOtherLegacy, OTHER);
end;

procedure TPatternTests.LatvianForm;
begin
  DefaultLocale := 'lv';

  Form(0, pfZero);
  Form(1, pfOne);
  Form(2, pfOther);
  Form(3, pfOther);
  Form(4, pfOther);
  Form(5, pfOther);
  Form(10, pfZero);

  Form(21, pfOne);
  Form(31, pfOne);
  Form(101, pfOne);
  Form(111, pfZero);
  Form(221, pfOne);
end;

procedure TPatternTests.LatvianIcu;
begin
  DefaultLocale := 'lv';

  Pattern(0, ZeroOneOtherIcu, ZERO);
  Pattern(1, ZeroOneOtherIcu, ONE);
  Pattern(2, ZeroOneOtherIcu, OTHER);
  Pattern(10, ZeroOneOtherIcu, ZERO);
end;

procedure TPatternTests.LatvianLegacy;
begin
  DefaultLocale := 'lv';

  Pattern(0, ZeroOneOtherLegacy, ZERO);
  Pattern(1, ZeroOneOtherLegacy, ONE);
  Pattern(2, ZeroOneOtherLegacy, OTHER);
  Pattern(10, ZeroOneOtherLegacy, ZERO);
end;

procedure TPatternTests.LithuanianForm;
begin
  DefaultLocale := 'lt';

  Form(0, pfOther);
  Form(1, pfOne);
  Form(2, pfFew);
  Form(3, pfFew);
  Form(4, pfFew);
  Form(5, pfFew);
  Form(10, pfOther);

  Form(11, pfOther);
  Form(21, pfOne);
  Form(31, pfOne);

  Form(12, pfOther);
  Form(22, pfFew);
  Form(32, pfFew);
  Form(102, pfFew);
  Form(112, pfOther);
  Form(222, pfFew);
end;

procedure TPatternTests.LithuanianIcu;
begin
  DefaultLocale := 'lt';

  Pattern(0, OneFewOtherIcu, OTHER);
  Pattern(1, OneFewOtherIcu, ONE);
  Pattern(2, OneFewOtherIcu, FEW);
  Pattern(10, OneFewOtherIcu, OTHER);

  Pattern(0, ZeroOneFewOtherIcu, ZERO);
  Pattern(1, ZeroOneFewOtherIcu, ONE);
  Pattern(2, ZeroOneFewOtherIcu, FEW);
  Pattern(10, ZeroOneFewOtherIcu, OTHER);
end;

procedure TPatternTests.LithuanianLegacy;
begin
  DefaultLocale := 'lt';

  Pattern(0, OneFewOtherLegacy, OTHER);
  Pattern(1, OneFewOtherLegacy, ONE);
  Pattern(2, OneFewOtherLegacy, FEW);
  Pattern(10, OneFewOtherLegacy, OTHER);

  Pattern(0, ZeroOneFewOtherLegacy, ZERO);
  Pattern(1, ZeroOneFewOtherLegacy, ONE);
  Pattern(2, ZeroOneFewOtherLegacy, FEW);
  Pattern(10, ZeroOneFewOtherLegacy, OTHER);
end;

procedure TPatternTests.MacedonianForm;
begin
  DefaultLocale := 'mk';

  Form(0, pfOther);
  Form(1, pfOne);
  Form(2, pfOther);
  Form(3, pfOther);
  Form(4, pfOther);
  Form(5, pfOther);
  Form(10, pfOther);

  Form(11, pfOne);
  Form(12, pfOther);
  Form(13, pfOther);

  Form(21, pfOne);
  Form(22, pfOther);
  Form(23, pfOther);
end;

procedure TPatternTests.MacedonianIcu;
begin
  DefaultLocale := 'mk';

  Pattern(0, OneTwoOtherIcu, OTHER);
  Pattern(1, OneTwoOtherIcu, ONE);
  Pattern(2, OneTwoOtherIcu, TWO);
  Pattern(10, OneTwoOtherIcu, OTHER);

  Pattern(0, ZeroOneTwoOtherIcu, ZERO);
  Pattern(1, ZeroOneTwoOtherIcu, ONE);
  Pattern(2, ZeroOneTwoOtherIcu, TWO);
  Pattern(10, ZeroOneTwoOtherIcu, OTHER);
end;

procedure TPatternTests.MacedonianLegacy;
begin
  DefaultLocale := 'mk';

  Pattern(0, OneTwoOtherLegacy, OTHER);
  Pattern(1, OneTwoOtherLegacy, ONE);
  Pattern(2, OneTwoOtherLegacy, TWO);
  Pattern(10, OneTwoOtherLegacy, OTHER);

  Pattern(0, ZeroOneTwoOtherLegacy, ZERO);
  Pattern(1, ZeroOneTwoOtherLegacy, ONE);
  Pattern(2, ZeroOneTwoOtherLegacy, TWO);
  Pattern(10, ZeroOneTwoOtherLegacy, OTHER);
end;

procedure TPatternTests.MalteseForm;
begin
  DefaultLocale := 'mt';

  Form(0, pfFew);
  Form(1, pfOne);
  Form(2, pfFew);
  Form(3, pfFew);
  Form(4, pfFew);
  Form(5, pfFew);
  Form(10, pfFew);

  Form(11, pfMany);
  Form(12, pfMany);
  Form(13, pfMany);
  Form(14, pfMany);
  Form(15, pfMany);
  Form(16, pfMany);
  Form(17, pfMany);
  Form(18, pfMany);
  Form(19, pfMany);
  Form(20, pfOther);

  Form(100, pfOther);
  Form(101, pfOther);
  Form(110, pfFew);
  Form(111, pfMany);
end;

procedure TPatternTests.MalteseIcu;
begin
  DefaultLocale := 'mt';

  Pattern(0, OneFewManyOtherIcu, FEW);
  Pattern(1, OneFewManyOtherIcu, ONE);
  Pattern(11, OneFewManyOtherIcu, MANY);
  Pattern(100, OneFewManyOtherIcu, OTHER);

  Pattern(0, ZeroOneFewManyOtherIcu, ZERO);
  Pattern(1, ZeroOneFewManyOtherIcu, ONE);
  Pattern(11, ZeroOneFewManyOtherIcu, MANY);
  Pattern(100, ZeroOneFewManyOtherIcu, OTHER);
end;

procedure TPatternTests.MalteseLegacy;
begin
  DefaultLocale := 'mt';

  Pattern(0, OneFewManyOtherLegacy, FEW);
  Pattern(1, OneFewManyOtherLegacy, ONE);
  Pattern(11, OneFewManyOtherLegacy, MANY);
  Pattern(100, OneFewManyOtherLegacy, OTHER);

  Pattern(0, ZeroOneFewManyOtherLegacy, ZERO);
  Pattern(1, ZeroOneFewManyOtherLegacy, ONE);
  Pattern(11, ZeroOneFewManyOtherLegacy, MANY);
  Pattern(100, ZeroOneFewManyOtherLegacy, OTHER);
end;

procedure TPatternTests.PolishForm;
begin
  DefaultLocale := 'pl';

  Form(0, pfMany);
  Form(1, pfOne);
  Form(2, pfFew);
  Form(3, pfFew);
  Form(4, pfFew);
  Form(5, pfMany);
  Form(10, pfMany);

  Form(11, pfMany);
  Form(12, pfMany);
  Form(13, pfMany);
  Form(14, pfMany);
  Form(15, pfMany);

  Form(21, pfMany);
  Form(22, pfFew);
  Form(23, pfFew);
  Form(24, pfFew);
  Form(25, pfMany);

  Form(101, pfMany);
  Form(102, pfFew);
  Form(103, pfFew);
  Form(104, pfFew);
  Form(105, pfMany);

  Form(111, pfMany);
  Form(112, pfMany);
  Form(113, pfMany);
  Form(114, pfMany);
  Form(115, pfMany);

  Form(121, pfMany);
  Form(122, pfFew);
  Form(123, pfFew);
  Form(124, pfFew);
  Form(125, pfMany);
end;

procedure TPatternTests.PolishIcu;
begin
  DefaultLocale := 'pl';

  Pattern(0, OneFewManyIcu, MANY);
  Pattern(1, OneFewManyIcu, ONE);
  Pattern(2, OneFewManyIcu, FEW);
  Pattern(10, OneFewManyIcu, MANY);

  Pattern(0, ZeroOneFewManyIcu, ZERO);
  Pattern(1, ZeroOneFewManyIcu, ONE);
  Pattern(2, ZeroOneFewManyIcu, FEW);
  Pattern(10, ZeroOneFewManyIcu, MANY);
end;

procedure TPatternTests.PolishLegacy;
begin
  DefaultLocale := 'pl';

  Pattern(0, OneFewManyLegacy, MANY);
  Pattern(1, OneFewManyLegacy, ONE);
  Pattern(2, OneFewManyLegacy, FEW);
  Pattern(10, OneFewManyLegacy, MANY);

  Pattern(0, ZeroOneFewManyLegacy, ZERO);
  Pattern(1, ZeroOneFewManyLegacy, ONE);
  Pattern(2, ZeroOneFewManyLegacy, FEW);
  Pattern(10, ZeroOneFewManyLegacy, MANY);
end;

procedure TPatternTests.RomanianForm;
begin
  DefaultLocale := 'ro';

  Form(0, pfFew);
  Form(1, pfOne);
  Form(2, pfFew);
  Form(3, pfFew);
  Form(4, pfFew);
  Form(5, pfFew);
  Form(10, pfFew);

  Form(11, pfFew);
  Form(12, pfFew);
  Form(13, pfFew);
  Form(14, pfFew);
  Form(15, pfFew);
  Form(16, pfFew);
  Form(17, pfFew);
  Form(18, pfFew);
  Form(19, pfFew);
  Form(20, pfOther);

  Form(21, pfOther);

  Form(110, pfFew);
  Form(111, pfFew);
  Form(120, pfOther);
  Form(121, pfOther);
end;

procedure TPatternTests.RomanianIcu;
begin
  DefaultLocale := 'ro';

  Pattern(0, OneFewOtherIcu, FEW);
  Pattern(1, OneFewOtherIcu, ONE);
  Pattern(2, OneFewOtherIcu, FEW);
  Pattern(21, OneFewOtherIcu, OTHER);

  Pattern(0, ZeroOneFewOtherIcu, ZERO);
  Pattern(1, ZeroOneFewOtherIcu, ONE);
  Pattern(2, ZeroOneFewOtherIcu, FEW);
  Pattern(21, ZeroOneFewOtherIcu, OTHER);
end;

procedure TPatternTests.RomanianLegacy;
begin
  DefaultLocale := 'ro';

  Pattern(0, OneFewOtherLegacy, FEW);
  Pattern(1, OneFewOtherLegacy, ONE);
  Pattern(2, OneFewOtherLegacy, FEW);
  Pattern(21, OneFewOtherLegacy, OTHER);

  Pattern(0, ZeroOneFewOtherLegacy, ZERO);
  Pattern(1, ZeroOneFewOtherLegacy, ONE);
  Pattern(2, ZeroOneFewOtherLegacy, FEW);
  Pattern(21, ZeroOneFewOtherLegacy, OTHER);
end;

procedure TPatternTests.SlovenianForm;
begin
  DefaultLocale := 'sl';

  Form(0, pfOther);
  Form(1, pfOne);
  Form(2, pfTwo);
  Form(3, pfFew);
  Form(4, pfFew);
  Form(5, pfOther);

  Form(10, pfOther);
  Form(11, pfOther);
  Form(12, pfOther);
  Form(13, pfOther);
  Form(14, pfOther);
  Form(15, pfOther);

  Form(100, pfOther);
  Form(101, pfOne);
  Form(102, pfTwo);
  Form(103, pfFew);
  Form(104, pfFew);
  Form(105, pfOther);

  Form(110, pfOther);
  Form(111, pfOther);
  Form(112, pfOther);
  Form(113, pfOther);
  Form(114, pfOther);
  Form(115, pfOther);
end;

procedure TPatternTests.SlovenianIcu;
begin
  DefaultLocale := 'sl';

  Pattern(0, OneTwoFewOtherIcu, OTHER);
  Pattern(1, OneTwoFewOtherIcu, ONE);
  Pattern(2, OneTwoFewOtherIcu, TWO);
  Pattern(3, OneTwoFewOtherIcu, FEW);
  Pattern(10, OneTwoFewOtherIcu, OTHER);

  Pattern(0, ZeroOneTwoFewOtherIcu, ZERO);
  Pattern(1, ZeroOneTwoFewOtherIcu, ONE);
  Pattern(2, ZeroOneTwoFewOtherIcu, TWO);
  Pattern(3, ZeroOneTwoFewOtherIcu, FEW);
  Pattern(10, ZeroOneTwoFewOtherIcu, OTHER);
end;

procedure TPatternTests.SlovenianLegacy;
begin
  DefaultLocale := 'sl';

  Pattern(0, OneTwoFewOtherLegacy, OTHER);
  Pattern(1, OneTwoFewOtherLegacy, ONE);
  Pattern(2, OneTwoFewOtherLegacy, TWO);
  Pattern(3, OneTwoFewOtherLegacy, FEW);
  Pattern(10, OneTwoFewOtherLegacy, OTHER);

  Pattern(0, ZeroOneTwoFewOtherLegacy, ZERO);
  Pattern(1, ZeroOneTwoFewOtherLegacy, ONE);
  Pattern(2, ZeroOneTwoFewOtherLegacy, TWO);
  Pattern(3, ZeroOneTwoFewOtherLegacy, FEW);
  Pattern(10, ZeroOneTwoFewOtherLegacy, OTHER);
end;

procedure TPatternTests.WelshForm;
begin
  DefaultLocale := 'cy';

  Form(0, pfZero);
  Form(1, pfOne);
  Form(2, pfTwo);
  Form(3, pfFew);
  Form(4, pfOther);
  Form(5, pfOther);
  Form(6, pfMany);
  Form(7, pfOther);
end;

procedure TPatternTests.WelshIcu;
begin
  DefaultLocale := 'cy';

  Pattern(0, ZeroOneTwoFewManyOtherIcu, ZERO);
  Pattern(1, ZeroOneTwoFewManyOtherIcu, ONE);
  Pattern(2, ZeroOneTwoFewManyOtherIcu, TWO);
  Pattern(3, ZeroOneTwoFewManyOtherIcu, FEW);
  Pattern(6, ZeroOneTwoFewManyOtherIcu, MANY);
  Pattern(10, ZeroOneTwoFewManyOtherIcu, OTHER);
end;

procedure TPatternTests.WelshLegacy;
begin
  DefaultLocale := 'cy';

  Pattern(0, ZeroOneTwoFewManyOtherLegacy, ZERO);
  Pattern(1, ZeroOneTwoFewManyOtherLegacy, ONE);
  Pattern(2, ZeroOneTwoFewManyOtherLegacy, TWO);
  Pattern(3, ZeroOneTwoFewManyOtherLegacy, FEW);
  Pattern(6, ZeroOneTwoFewManyOtherLegacy, MANY);
  Pattern(10, ZeroOneTwoFewManyOtherLegacy, OTHER);
end;




procedure TPatternTests.SinglePlural;
const
  PATTERN_C = 'one;I have one ski;other;I have %d skis';
begin
  DefaultLocale := 'en';

  CheckEquals(TMultiPattern.Format(PATTERN_C, [0]), 'I have 0 skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [1]), 'I have one ski');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [2]), 'I have 2 skis');
end;

procedure TPatternTests.ZeroSinglePlural;
const
  PATTERN_C = 'zero;I have no skis;one;I have one ski;other;I have %d skis';
begin
  CheckEquals(TMultiPattern.Format(PATTERN_C, [0]), 'I have no skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [1]), 'I have one ski');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [2]), 'I have 2 skis');
end;

procedure TPatternTests.Range;
const
  PATTERN_C = '0;I have no skis;1..3;I have few skis;4..9;I have many skis;>=10;I have lots of skis';
begin
  CheckEquals(TMultiPattern.Format(PATTERN_C, [0]), 'I have no skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [1]), 'I have few skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [2]), 'I have few skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [3]), 'I have few skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [5]), 'I have many skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [9]), 'I have many skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [10]), 'I have lots of skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [20]), 'I have lots of skis');
end;

procedure TPatternTests.Operators;
const
  PATTERN_C = '=0;I have no skis;=1;I have one ski;~12;I have a dozen skis;<10;I have few skis;>5;I have many skis;>=20;I have lots of skis;other;I have %d skis';
begin
  CheckEquals(TMultiPattern.Format(PATTERN_C, [0]), 'I have no skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [1]), 'I have one ski');

  CheckEquals(TMultiPattern.Format(PATTERN_C, [2]), 'I have few skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [5]), 'I have few skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [6]), 'I have many skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [9]), 'I have few skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [10]), 'I have many skis');

  CheckEquals(TMultiPattern.Format(PATTERN_C, [11]), 'I have a dozen skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [12]), 'I have a dozen skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [13]), 'I have a dozen skis');

  CheckEquals(TMultiPattern.Format(PATTERN_C, [19]), 'I have many skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [20]), 'I have lots of skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [21]), 'I have lots of skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [100]), 'I have lots of skis');
end;

procedure TPatternTests.Gender;
const
  PATTERN_C = 'male;%s brings his skis;female;%s brings her skis';
begin
  CheckEquals(TMultiPattern.Format(PATTERN_C, geMale, [MALE_C]), MALE_C + ' brings his skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, geFemale, [FEMALE_C]), FEMALE_C + ' brings her skis');
end;

procedure TPatternTests.Multi2;
const
  PATTERN_C = 'male;%s will bring his %s;female;%s will bring her %s;next;one;car.;other;%d cars.';

  MALE_0_C: array[0..1] of TFormatParameter =
  (
    (Kind: fpGender; Gender: geMale; Value: MALE_C),
    (Kind: fpPlural; Plural: 0)
  );

  MALE_1_C: array[0..1] of TFormatParameter =
  (
    (Kind: fpGender; Gender: geMale; Value: MALE_C),
    (Kind: fpPlural; Plural: 1)
  );

  MALE_2_C: array[0..1] of TFormatParameter =
  (
    (Kind: fpGender; Gender: geMale; Value: MALE_C),
    (Kind: fpPlural; Plural: 2)
  );

  FEMALE_1_C: array[0..1] of TFormatParameter =
  (
    (Kind: fpGender; Gender: geFemale; Value: FEMALE_C),
    (Kind: fpPlural; Plural: 1)
  );

  FEMALE_2_C: array[0..1] of TFormatParameter =
  (
    (Kind: fpGender; Gender: geFemale; Value: FEMALE_C),
    (Kind: fpPlural; Plural: 2)
  );
begin
  CheckEquals(TMultiPattern.FormatMulti(PATTERN_C, MALE_0_C), MALE_C + ' will bring his 0 cars.');
  CheckEquals(TMultiPattern.FormatMulti(PATTERN_C, MALE_1_C), MALE_C + ' will bring his car.');
  CheckEquals(TMultiPattern.FormatMulti(PATTERN_C, MALE_2_C), MALE_C + ' will bring his 2 cars.');

  CheckEquals(TMultiPattern.FormatMulti(PATTERN_C, FEMALE_1_C), FEMALE_C + ' will bring her car.');
  CheckEquals(TMultiPattern.FormatMulti(PATTERN_C, FEMALE_2_C), FEMALE_C + ' will bring her 2 cars.');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TPatternTests.Suite);
end.

