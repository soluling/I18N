unit PatternTest;

interface

uses
  Classes,
  TestFramework,
  NtPattern;

type
  TPatternTest = class(TTestCase)
  published
    procedure SinglePlural;
    procedure ZeroSinglePlural;
    procedure Range;
    procedure Operators;
    procedure Gender;
    procedure Multi;
  end;

implementation

procedure TPatternTest.SinglePlural;
const
  PATTERN_C = 'one;I have one ski;other;I have %d skis';
begin
  CheckEquals(TMultiPattern.Format(PATTERN_C, [0]), 'I have 0 skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [1]), 'I have one ski');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [2]), 'I have 2 skis');
end;

procedure TPatternTest.ZeroSinglePlural;
const
  PATTERN_C = 'zero;I have no skis;one;I have one ski;other;I have %d skis';
begin
  CheckEquals(TMultiPattern.Format(PATTERN_C, [0]), 'I have no skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [1]), 'I have one ski');
  CheckEquals(TMultiPattern.Format(PATTERN_C, [2]), 'I have 2 skis');
end;

procedure TPatternTest.Range;
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

procedure TPatternTest.Operators;
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

const
  MALE_C = 'John';
  FEMALE_C = 'Jill';

procedure TPatternTest.Gender;
const
  PATTERN_C = 'male;%s brings his skis;female;%s brings her skis';
begin
  CheckEquals(TMultiPattern.Format(PATTERN_C, geMale, [MALE_C]), MALE_C + ' brings his skis');
  CheckEquals(TMultiPattern.Format(PATTERN_C, geFemale, [FEMALE_C]), FEMALE_C + ' brings her skis');
end;

procedure TPatternTest.Multi;
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
  RegisterTest(TPatternTest.Suite);
end.

