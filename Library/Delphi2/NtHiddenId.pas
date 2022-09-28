(*
  @abstract Implements a class thats injects a hidden id into a string.
*)
unit NtHiddenId;

{$I NtVer.inc}

interface

uses
  System.Generics.Collections;

const
  REQUIRED_ZERO_WIDTH_CHARACTER_COUNT_C = 4;

type
  TZeroWidthCharacter =
  (
    zwSpace,                     //< 200B
    zwNonJoiner,                 //< 200C
    zwJoiner,                    //< 200D
    zwLeftToRightMark,           //< 200E
    zwLeftToRightEmbedding,      //< 202A
    zwPopDirectionalFormatting,  //< 202C
    zwLeftToRightOverwrite,      //< 202D
    zwWordJoiner,                //< 2060
    zwInvisibleTimes,            //< 2062
    zwInvisibleSeparator,        //< 2063
    zwNoBreakSpace               //< FEFF
  );

  TZeroWidthCharacters = set of TZeroWidthCharacter;

  { @abstract Class that contains one decoded string that is the string id and the string value. }
  TDecodedString = record
    Id: Integer;    //< String id.
    Value: String;  //< String value.
  end;

  { @abstract Class that provided the API methods. }
  THiddenId = class
  private
    FChars: array[0..REQUIRED_ZERO_WIDTH_CHARACTER_COUNT_C - 1] of Char;
    FValue: TZeroWidthCharacters;

    procedure SetValue(thisValue: TZeroWidthCharacters);

    function IsZeroWidthChar(c: Char): Boolean;
    function GetZeroWidthIndex(c: Char): Integer;

  public
    constructor Create;

    { @abstract. Encode an id.
      @param value Id to be encoded.
      @return Encoded id as a string. }
    function Encode(value: Integer): String;

    { @abstract. Decode an encoded id.
      @param value Encoded id to be decoded.
      @return Decoded id. }
    function Decode(value: String): Integer;

    { @abstract. Injects an id into a string.
      @param value String where the id will be injected.
      @param id Id to be injected.
      @return String with injected id. }
    function Inject(const value: String; id: Integer): String;

    { @abstract. Parse a string that may contain injected id(s).
      @param str String that may contain injected id.
      @return Array of decoded strings. }
    function Parse(const str: String; list: TList<TDecodedString>): Integer; overload;

    { @abstract. Parse a string that may contain injected id. If the string contains multiple strings return the first one.
      @param str String that may contain injected id.
      @return First decoded string. }
    function Parse(const str: String): TDecodedString; overload;

    class function GetCount(value: TZeroWidthCharacters): Integer; static;
    class function GetChar(value: TZeroWidthCharacter): Char; static;
    class function GetName(value: TZeroWidthCharacter; includeCode: Boolean = False): String; static;

    property Value: TZeroWidthCharacters read FValue write SetValue;
  end;

const
  DEFAULT_ZERO_WIDTH_CHARACTERS_C = [zwNonJoiner, zwJoiner, zwPopDirectionalFormatting, zwLeftToRightOverwrite];

implementation

uses
  SysUtils;

const
  CHARS: array[TZeroWidthCharacter] of Char =
  (
    #$200B,
    #$200C,
    #$200D,
    #$200E,
    #$202A,
    #$202C,
    #$202D,
    #$2060,
    #$2062,
    #$2063,
    #$FEFF
  );

constructor THiddenId.Create;
begin
  inherited;
  Value := DEFAULT_ZERO_WIDTH_CHARACTERS_C;
end;

procedure THiddenId.SetValue(thisValue: TZeroWidthCharacters);
var
  index: Integer;
  c: TZeroWidthCharacter;
begin
  if GetCount(thisValue) <> REQUIRED_ZERO_WIDTH_CHARACTER_COUNT_C then
    thisValue := DEFAULT_ZERO_WIDTH_CHARACTERS_C;

  FValue := thisValue;

  index := 0;

  for c := Low(c) to High(c) do
  begin
    if c in FValue then
    begin
      FChars[index] := GetChar(c);
      Inc(index);
    end;

    if index >= 4 then
      Break;
  end;
end;

function THiddenId.IsZeroWidthChar(c: Char): Boolean;
var
  zc: Char;
begin
  for zc in FChars do
  begin
    if c = zc then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

function THiddenId.GetZeroWidthIndex(c: Char): Integer;
var
  i: Integer;
begin
  for i := 0 to High(FChars) do
  begin
    if FChars[i] = c then
    begin
      Result := i;
      Exit;
    end;
  end;

  Result := 0;
end;

function THiddenId.Encode(value: Integer): String;
var
  digit: Integer;
begin
  if value = 0 then
  begin
    Result := FChars[0] + FChars[0]; //FI:W510
    Exit;
  end;

  Result := '';

  while value > 0 do
  begin
    digit := value mod 16;

    if digit < 4 then
      Result := FChars[0] + FChars[digit] + Result
    else if digit < 8 then
      Result := FChars[1] + FChars[digit - 4] + Result
    else if digit < 12 then
      Result := FChars[2] + FChars[digit - 8] + Result
    else
      Result := FChars[3] + FChars[digit - 12] + Result;

    value := value div 16;
  end;
end;

function THiddenId.Decode(value: String): Integer;
var
  digit: Integer;
begin
  if (value = '') or ((Length(value) mod 2) <> 0) then
  begin
    Result := -1;
    Exit;
  end;

  Result := 0;

  while value <> '' do
  begin
    digit := GetZeroWidthIndex(value[2]);

    if value[1] = FChars[1] then
      Inc(digit, 4)
    else if value[1] = FChars[2] then
      Inc(digit, 8)
    else if value[1] = FChars[3] then
      Inc(digit, 12);

    Result := 16*Result + digit;
    Delete(value, 1, 2);
  end;
end;

function THiddenId.Inject(const value: String; id: Integer): String;
begin
  Result := value + Encode(id);
end;

function THiddenId.Parse(const str: String; list: TList<TDecodedString>): Integer;
var
  id, thisValue: String;

  procedure Add;
  var
    decodedString: TDecodedString;
  begin
    decodedString.Value := thisValue;
    decodedString.Id := Decode(id);
    list.Add(decodedString);

    Inc(Result);

    id := '';
    thisValue := '';
  end;

var
  i, len: Integer;
  c: Char;
begin
  Result := 0;
  id := '';
  thisValue := '';
  len := Length(str);

  i := 1;

  while i <= len do
  begin
    c := str[i];

    if IsZeroWidthChar(c) then
      id := id + c
    else
    begin
      if id <> '' then
        Add;

      thisValue := thisValue + c;
    end;

    Inc(i);
  end;

  if thisValue <> '' then
    Add;
end;

function THiddenId.Parse(const str: String): TDecodedString;
var
  list: TList<TDecodedString>;
begin
  list := TList<TDecodedString>.Create;
  try
    if Parse(str, list) > 0 then
      Result := list[0]
    else
    begin
      Result.Value := str;
      Result.Id := 0;
    end;
  finally
    list.Free;
  end;
end;

class function THiddenId.GetCount(value: TZeroWidthCharacters): Integer;
var
  c: TZeroWidthCharacter;
begin
  Result := 0;

  for c := Low(c) to High(c) do
    if c in value then
      Inc(Result);
end;

class function THiddenId.GetChar(value: TZeroWidthCharacter): Char;
begin
  Result := CHARS[value];
end;

class function THiddenId.GetName(value: TZeroWidthCharacter; includeCode: Boolean): String;
resourcestring
  SZeroWidthSpace = 'Zero width space';
  SZeroWidthNonJoiner = 'Zero width non joiner';
  SZeroWidthJoiner = 'Zero width joiner';
  SLeftToRightMark = 'Left to right mark';
  SLeftToRightEmbedding = 'Left to right embedding';
  SPopDirectionalFormatting = 'Pop directional formatting';
  SLeftToRightOverwrite = 'Left to right overwrite';
  SWordJoiner = 'Word joiner';
  SInvisibleTimes = 'Invisible times';
  SInvisibleSeparator = 'Invisible separator';
  SNoBreakSpace = 'Zero width no-break space';
var
  code: Integer;
begin
  case value of
    zwSpace: Result := SZeroWidthSpace;
    zwNonJoiner: Result := SZeroWidthNonJoiner;
    zwJoiner: Result := SZeroWidthJoiner;
    zwLeftToRightMark: Result := SLeftToRightMark;
    zwLeftToRightEmbedding: Result := SLeftToRightEmbedding;
    zwPopDirectionalFormatting: Result := SPopDirectionalFormatting;
    zwLeftToRightOverwrite: Result := SLeftToRightOverwrite;
    zwWordJoiner: Result := SWordJoiner;
    zwInvisibleTimes: Result := SInvisibleTimes;
    zwInvisibleSeparator: Result := SInvisibleSeparator;
    zwNoBreakSpace: Result := SNoBreakSpace;
  else
    raise Exception.Create('Not implemented');
  end;

  if includeCode then
  begin
    code := Integer(GetChar(value));
    Result := Format('%s (0x%s, %s)', [Result, IntToHex(code, 4), IntToStr(code)]);
  end;
end;

end.
