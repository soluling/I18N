unit HiddenTests;

interface

uses
  System.Classes,
  TestFramework,
  NtHiddenId;

type
  THiddenTests = class(TTestCase)
  private
    FValue: THiddenId;
    FCustomValue: THiddenId;

    procedure Encode(value: Integer);
    procedure EncodeCustom(value: Integer);

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure EncodeZero;
    procedure EncodeVeryShort;
    procedure EncodeShort;
    procedure EncodeMedium;
    procedure EncodeLong;
    procedure EncodeVeryLong;

    procedure EncodeCustomShort;

    procedure Parse;
    procedure ParseTriple;
  end;

implementation

uses
  System.Generics.Collections;

procedure THiddenTests.SetUp;
begin
  inherited;

  FValue := THiddenId.Create;

  FCustomValue := THiddenId.Create;
  FCustomValue.Value := [zwInvisibleTimes, zwNoBreakSpace, zwPopDirectionalFormatting, zwWordJoiner];
end;

procedure THiddenTests.TearDown;
begin
  FCustomValue.Free;
  FValue.Free;
  inherited;
end;

procedure THiddenTests.Encode(value: Integer);
var
  encoded: String;
  decoded: Integer;
begin
  encoded := FValue.Encode(value);
  decoded := FValue.Decode(encoded);
  CheckEquals(value, decoded);
end;

procedure THiddenTests.EncodeCustom(value: Integer);
var
  encoded: String;
  decoded: Integer;
begin
  encoded := FCustomValue.Encode(value);
  decoded := FCustomValue.Decode(encoded);
  CheckEquals(value, decoded);
end;

procedure THiddenTests.EncodeZero;
begin
  Encode(0);
end;

procedure THiddenTests.EncodeVeryShort;
begin
  Encode(1);
end;

procedure THiddenTests.EncodeShort;
begin
  Encode(28);
end;

procedure THiddenTests.EncodeCustomShort;
begin
  EncodeCustom(28);
end;

procedure THiddenTests.EncodeMedium;
begin
  Encode(4151);
end;

procedure THiddenTests.EncodeLong;
begin
  Encode(90123);
end;

procedure THiddenTests.EncodeVeryLong;
begin
  Encode(650123);
end;

procedure THiddenTests.Parse;
const
  VALUE = 'Hello';
  ID = 123;
var
  encoded: String;
  part: TDecodedString;
begin
  encoded := FValue.Inject(VALUE, ID);
  part := FValue.Parse(encoded);

  CheckEquals(VALUE, part.Value);
  CheckEquals(ID, part.Id);
end;

procedure THiddenTests.ParseTriple;
const
  VALUE1 = 'First part';
  VALUE2 = 'Second part';
  VALUE3 = 'Third part';
  ID1 = 123;
  ID2 = 234;
  ID3 = 3456;
var
  encoded: String;
  parts: TList<TDecodedString>;
begin
  encoded := FValue.Inject(VALUE1, ID1) + FValue.Inject(VALUE2, ID2) + FValue.Inject(VALUE3, ID3);

  parts := TList<TDecodedString>.Create;
  try
    FValue.Parse(encoded, parts);

    CheckEquals(3, parts.Count);

    CheckEquals(VALUE1, parts[0].Value);
    CheckEquals(ID1, parts[0].Id);

    CheckEquals(VALUE2, parts[1].Value);
    CheckEquals(ID2, parts[1].Id);

    CheckEquals(VALUE3, parts[2].Value);
    CheckEquals(ID3, parts[2].Id);
  finally
    parts.Free;
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(THiddenTests.Suite);
end.
