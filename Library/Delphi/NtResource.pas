{
  @abstract Implements multilingual resource format for FireMonkey and VCL.

  FireMonkey does not support resource DLLs in Android and iOS. This is why the standard
  resource DLL based localization method cannot be used with FireMonkey if mobiles platforms
  are used. This format is designed to make it possible to localize FireMonkey applications.
  Even this method is ment to be used to localize FireMonkey applications you can also
  use it with VLC application although the resource DLL based method is the prefered way.

  The basic idea is to store all localized forms, strings and resources into a
  single .ntres file that can be added as custom RCDATA resource inside the application
  executable.

  You will have full control of form localization. This mean that you can override
  any form properties including dimension, colors and fonts.

  Note! If you use Delphi 10.3 (Rio) or earlier you cannot use the resourcestring feature.
  If your code is like this.

  @longCode(#
resourcestring
  SStr = 'Hello';
begin
  Result := SStr;
end;
#)

  You need to change the code to use _T function.

  @longCode(#
begin
  Result := _T('Hello', 'SStr');
end;
#)
}

{
  File Format Specificaiton

  This format is designed to store localized form, strings and other resources
  in several languages. The resource is added as a RCDATA resource into the
  Delphi project and will be embedded inside the application executable.

  Numbers and offsets are 4 bit integers.
  All strings are without null termination. Strings lengths are specified in a word.
  String length is length in characters NOT in bytes.
  Language ids are in ASCII. All other strings are UTF16-LE strings.
  ~ means variable length.


  File format:
  [header]      4  'NTRE'
  [version]     4  File format version (2 or 1)
  [count]       4  Language count
  [idIndex1]    8  Language id offset + length
  [idIndex2]    8
  ...
  [idIndexN]    8
  [dataIndex1]  8  Language data offset + length
  [dataIndex2]  8
  ...
  [dataIndexN]  8
  [id1]         ~  Language id (Ansi string)
  [id2]         ~
  ...
  [idN]         ~
  [data1]       ~  Language data (binary). See language data.
  [data2]       ~
  ...
  [dataN]       ~


  Language data version 2

  There is one language data or file per language.
  Format:
  [header]       4  'NTLB'
  [version]      4  File format version (2)
  [id]           16 IETF language tag in Ansi characters (e.g. fi-FI)
  [win32idcount] 4  String Win32 id count
  [win64idcount] 4  String Win64 id count
  [symbolcount]  4  String symbol name count
  [stringcount]  4  String count
  [formcount]    4  Form count
  [rescount]     4  Resource count

  [stringId1]    4  Sorted string Win32 id index: offset to string id data: string id + offset to string data
  [stringId2]    4
  ...
  [stringIdN]    4

  [stringId1]    4  Sorted string Win64 id index: offset to string id data: string id + offset to string data
  [stringId2]    4
  ...
  [stringIdN]    4

  [stringName1]  4  Sorted string symbol name index: offset to string symbol name data: string symbol name + offset to string data
  [stringName2]  4
  ...
  [stringNameN]  4

  [formName1]    4  Sorted form name index: offset to form data: name + length + data
  [formName2]    4
  ...
  [formNameN]    4

  [resName1]     4  Sorted resource name index: offet to resource data: name + length + data
  [resName2]     4
  ...
  [resNameN]     4

  [idValue1]     6  String Win32 id data: id (Word) + offset to string data
  [idValue2]     6
  ...
  [idValueN]     6

  [idValue1]     6  String Win64 id data: id (Word) + offset to string data
  [idValue2]     6
  ...
  [idValueN]     6

  [symbolName1]  ~  String symbol name data: name + offset to string data
  [symbolName2]  ~
  ...
  [symbolNameN]  ~

  [string1]      ~  String data: length + characters
  [string2]      ~
  ...
  [stringN]      ~

  [form1]        ~  Form data: name + length + Standard binary form data: TPF0...
  [form2]        ~
  ...
  [formN]        ~

  [resource1]    ~  Resource data: name + length + Plain resource data such as PNG or MP3
  [resource2]    ~
  ...
  [resourceN]    ~


  Language data version 1 (will be deprected in 2024)

  There is one language data per language.
  Format:
  [header]       4  'NTLA'
  [formcount]    4  Form count
  [stringcount]  4  String group count
  [rescount]     4  Resource count

  [formName1]    8  Form name offset + length
  [formName2]    8
  ...
  [formNameN]    8
  [formIndex1]   8  Form data offset + length
  [formIndex2]   8
  ...
  [formIndexN]   8

  [stringName1]  8  String group name offset + length
  [stringName2]  8
  ...
  [stringNameN]  8
  [stringIndex1] 8  String group data offset + length
  [stringIndex2] 8
  ...
  [stringIndexN] 8

  [resName1]     8  Resource name offset + length
  [resName2]     8
  ...
  [resNameN]     8
  [resIndex1]    8  Resource data offset + length
  [resIndex2]    8
  ...
  [resIndexN]    8

  [form1]        ~  Form data. Standard binary form data: TPF0...
  [form2]        ~
  ...
  [formN]        ~

  [string1]      ~  String data. See String group data.
  [string2]      ~
  ...
  [stringN]      ~

  [resource1]    ~  Resource data. Plain resource data such as PNG or MP3.
  [resource2]    ~
  ...
  [resourceN]    ~


  String group data:
  [count]        4  String count
  [idIndex1]     8  Id offset + length. Ids are alphapetically sorted
  [idIndex2]     8
  ...
  [idIndexN]     8
  [strIndex1]    8  Value offset + length. Ids are alphapetically sorted
  [strIndex2]    8
  ...
  [strIndexN]    8
  [id1]          ~  Id
  [id2]          ~
  ...
  [idN]          ~
  [str1]         ~  Value
  [str2]         ~
  ...
  [strN]         ~

}

unit NtResource;

{$I NtVer.inc}

interface

uses
  Types,
  SysUtils,
  Classes,
  Generics.Collections;

const
  NTRES_RESOURCE_NAME_C = 'NtLangRes';
  NTRES_MAGIC_C: array[0..3] of Byte = ($4e, $54, $52, $45);  // 'NTRE'
  NTLANG1_MAGIC_C: array[0..3] of Byte = ($4e, $54, $4c, $41);  // 'NTLA'
  NTLANG2_MAGIC_C: array[0..3] of Byte = ($4e, $54, $4c, $42);  // 'NTLB'
  LANG_ID_LENGTH_C = 16;

  //NTRES_VALUE_VERSION_C = 1;
  //NTRES_RESOURCE_STRING_VERSION_C = 2;

type
  TNtResourceVersion =
  (
    nr1 = 1,
    nr2
  );

  TNtResourceStringId =
  (
    rsWin32,
    rsWin64,
    rsSymbol
  );

  TNtResourceStringIds = set of TNtResourceStringId;

  TNtDelphiResources = class;

  { @abstract Contains information about the language such as id, name and image. }
  TNtResourceLanguage = class(TObject)
  private
    FOriginal: String;
    FNative: String;
    FLocalized: String;
    FId: String;
    FImage: String;

  public
    { @abstract Add a language image resource id.
      If specified the image will be drawn with the language name.
      @param value  Image resource id.
      @return This same TNtResourceLanguage object. }
    function AddImage(const value: String): TNtResourceLanguage;

    property Original: String read FOriginal;
    property Native: String read FNative;
    property Localized: String read FLocalized;
    property Id: String read FId;
    property Image: String read FImage;
  end;

  TNtDelphiResource = class(TObject)
  private
    FVersion: TNtResourceVersion;
    FLangId: String;
    FOffset: Integer;
    FStream: TStream;
    FFileName: String;
    FId: String;

    // All versions
    FFormCount: Integer;
    FResourceCount: Integer;

    // Version 1
    FFormNameOffset: Integer;
    FStringGroupCount: Integer;
    FStringGroupNameOffset : Integer;
    FResourceNameOffset: Integer;

    // Version 2
    FWin32StringCount: Integer;
    FWin64StringCount: Integer;
    FSymbolStringCount: Integer;
    FStringCount: Integer;
    FWin32IndexOffset: Integer;
    FWin64IndexOffset: Integer;
    FSymbolIndexOffset: Integer;
    FFormIndexOffset: Integer;
    FResourceIndexOffset: Integer;

    procedure SetOffset(value: Integer);

    procedure CheckStream;
    procedure ReadHeader;

{$IFDEF DELPHIDX4}
  {$IF defined(EXTERNALLINKER)}
    function FindStringSymbol(const unitName, itemName: String): String;
  {$ELSE}
    function FindStringWin(id: Word): String;
  {$IFEND}
{$ENDIF}

  public
    function FindForm(const name: String): TStream;
    procedure GetFormNames(names: TStrings);

{$IFDEF DELPHIDX4}
    function FindString(resStringRec: PResStringRec): String; overload;
{$ENDIF}
    procedure GetWin32Strings(values: TStrings);
    procedure GetWin64Strings(values: TStrings);
    procedure GetSymbolStrings(ids, values: TStrings);
    function FindString(const original: String; id: String; const group: String): String; overload;

    function FindResource(const id: String): TStream;
    procedure GetResourceNames(names: TStrings);

    //class procedure ParseKey(const key: String; var group, id: String);
    class function ParseKey(name: String; var unitName, itemName: String): Boolean;

    property Id: String read FId;
    property Offset: Integer read FOffset write SetOffset;
    property Stream: TStream read FStream;
    property FormCount: Integer read FFormCount;
    property ResourceCount: Integer read FResourceCount;
    property Version: TNtResourceVersion read FVersion;
    property Win32StringCount: Integer read FWin32StringCount;
    property Win64StringCount: Integer read FWin64StringCount;
    property SymbolStringCount: Integer read FSymbolStringCount;
  end;

  TTranslationSource =
  (
    tsNone,
    tsResource,
    tsFile,
    tsDirectory
  );

  TNtDelphiResources = class(TObject)
  private
    FVersion: TNtResourceVersion;
    FLoaded: Boolean;
    FResourceName: String;
    FResourcePath: String;
    FStream: TStream;
    FCascadingEnabled: Boolean;
    FLanguageIndex: Integer;
    FLanguages: TList<TNtDelphiResource>;
    FLanguageNames: TList<TNtResourceLanguage>;
    FTranslationSource: TTranslationSource;
    FTranslationSourceValue: String;

    function GetCurrent: TNtDelphiResource;
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetLanguage(i: Integer): TNtDelphiResource;
    function GetLanguageId: String;
    function GetOriginal(const id: String): String;
    function GetNative(const id: String): String;
    function GetLocalized(const id: String): String;
    function GetLanguageImage(const id: String): String;
    function GetResourceDirectories: TStringDynArray;

    procedure SetLanguageId(const value: String);

    procedure CheckLoad;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(resourceFileName: String = '');

    function Find(const id: String): Integer;
    function FindLanguage(const id: String): TNtResourceLanguage;

    function FindForm(const name: String): TStream;
    function FormExists(const name: String): Boolean;

{$IFDEF DELPHIDX4}
    function GetString(resStringRec: PResStringRec): String; overload;

    function GetStringInLanguage(
      const language: String;
      resStringRec: PResStringRec): String; overload;
{$ENDIF}

    function GetString(const original, id, group: String): String; overload;

    function GetStringInLanguage(
      const language: String;
      const original, id, group: String): String; overload;

    function GetResource(const id: String; resType: PChar = RT_RCDATA): TStream;

    { @abstract Add a language name to the resources.
      Language names are used for example in the select language dialogs.
      The function name is _T because we also want to the string will be extracted.
      @param original  Original language name such as "English".
      @param id        Language id such as "en".
      @return Language resource. }
    function Translate(const original, id: String): TNtResourceLanguage;

    function Add(const original, native, localized, id: String): TNtResourceLanguage;

    class function GetResources: TNtDelphiResources;

    property CascadingEnabled: Boolean read FCascadingEnabled write FCascadingEnabled;
    property Count: Integer read GetCount;
    property Current: TNtDelphiResource read GetCurrent;
    property Enabled: Boolean read GetEnabled;
    property LanguageId: String read GetLanguageId write SetLanguageId;
    property Languages[i: Integer]: TNtDelphiResource read GetLanguage; default;
    property ResourceName: String read FResourceName write FResourceName;
    property ResourcePath: String read FResourcePath write FResourcePath;
    property Stream: TStream read FStream;
    property ResourceDirectories: TStringDynArray read GetResourceDirectories;
    property Originals[const id: String]: String read GetOriginal;
    property Natives[const id: String]: String read GetNative;
    property Localizeds[const id: String]: String read GetLocalized;
    property LanguageImages[const id: String]: String read GetLanguageImage;
    property TranslationSource: TTranslationSource read FTranslationSource;
    property TranslationSourceValue: String read FTranslationSourceValue;
    property Version: TNtResourceVersion read FVersion;
  end;

{$IFDEF DELPHIDX4}
// _T has been moved to NtResourceEx.pas

procedure InitializeResourceStringTranslation;
{$ENDIF}

function Translate(
  const original: String;
  const id: String = '';
  const group: String = '';
  const language: String = ''): String; overload;

{ Get the string value in the current language.
  @param original Original string value.
  @param group    String group.
  @param language Optional language id. If not present the current language is used.
  @return String value in the current languages.}
function _TG(
  const original, group: String;
  const language: String = ''): String;

function TranslateGroup(
  const original, group: String;
  const language: String = ''): String;

var
  NtResources: TNtDelphiResources;

implementation

uses
{$IF CompilerVersion = 22}
  Windows,
{$IFEND}
  RTLConsts,
{$IFDEF DELPHIXE2}
  System.IOUtils,
{$ENDIF}
  NtBase,
  NtResourceEx;


// TStreamHelper

type
  TStreamHelper = class helper for TStream
    function ReadBytes(length: Integer): TBytes;

    function ReadAnsi(length: Integer): String; overload;
    function ReadAnsi(offset, length: Integer): String; overload;

    function ReadString: String; overload;
    function ReadString(length: Integer): String; overload;
    function ReadString(offset, length: Integer): String; overload;

    function ReadWord: Word;
    function ReadInt32: Int32;
  end;

function TStreamHelper.ReadBytes(length: Integer): TBytes;
begin
  SetLength(Result, length);
  Read(Result[0], length)
end;

function TStreamHelper.ReadAnsi(length: Integer): String;
var
  data: TBytes;
begin
  SetLength(data, length);
  Read(data, length);
  Result := TEncoding.ANSI.GetString(data);
end;

function TStreamHelper.ReadAnsi(offset, length: Integer): String;
var
  currentOffset: Int64;
begin
  currentOffset := Position;
  Position := offset;
  Result := ReadAnsi(length);
  Position := currentOffset;
end;

function TStreamHelper.ReadString: String;
begin
  Result := ReadString(ReadWord);
end;

function TStreamHelper.ReadString(length: Integer): String;
begin
  SetLength(Result, length);

  if length = 0 then
    Exit;

{$IFDEF DELPHIXE3}
  Read(Result[Low(String)], 2*length)
{$ELSE}
  Read(Result[1], 2*length)
{$ENDIF}
end;

function TStreamHelper.ReadString(offset, length: Integer): String;
var
  currentOffset: Int64;
begin
  currentOffset := Position;
  Position := offset;
  Result := ReadString(length);
  Position := currentOffset;
end;

function TStreamHelper.ReadWord: Word;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamHelper.ReadInt32: Int32;
begin
  Read(Result, SizeOf(Result));
end;

function Translate(
  const original: String;
  const id: String = '';
  const group: String = '';
  const language: String = ''): String;
begin
  Result := _T(original, id, group, language);
end;

function _TG(
  const original, group: String;
  const language: String = ''): String;
begin
  if language <> '' then
    Result := NtResources.GetStringInLanguage(language, original, '', group)
  else
    Result := NtResources.GetString(original, '', group);
end;

function TranslateGroup(
  const original, group: String;
  const language: String = ''): String;
begin
  Result := _TG(original, group, language);
end;


// TNtResourceLanguage

function TNtResourceLanguage.AddImage(const value: String): TNtResourceLanguage;
begin
  FImage := value;
  Result := Self;
end;


// TNtDelphiResource

function Convert(const bytes: TBytes): String;
var
  b: Byte;
begin
  Result := '';

  for b in bytes do
  begin
    if b = 0 then
      Exit;

    Result := Result + Char(b)
  end;
end;

procedure TNtDelphiResource.ReadHeader;
var
  header: TBytes;
begin
  header := FStream.ReadBytes(Length(NTLANG1_MAGIC_C));

  if CompareMem(@NTLANG1_MAGIC_C, @header[0], Length(NTLANG1_MAGIC_C)) then
  begin
    FVersion := nr1;
    FLangId := '';

    FFormCount := FStream.ReadInt32;
    FStringGroupCount := FStream.ReadInt32;
    FResourceCount := FStream.ReadInt32;

    FFormNameOffset := FStream.Position;
    FStringGroupNameOffset := FFormNameOffset + 16*FFormCount;
    FResourceNameOffset := FStringGroupNameOffset + 16*FStringGroupCount;
  end
  else if CompareMem(@NTLANG2_MAGIC_C, @header[0], Length(NTLANG2_MAGIC_C)) then
  begin
    FVersion := TNtResourceVersion(FStream.ReadInt32);
    FLangId := Convert(FStream.ReadBytes(LANG_ID_LENGTH_C));

    FWin32StringCount := FStream.ReadInt32;
    FWin64StringCount := FStream.ReadInt32;
    FSymbolStringCount := FStream.ReadInt32;
    FStringCount := FStream.ReadInt32;
    FFormCount := FStream.ReadInt32;
    FResourceCount := FStream.ReadInt32;

    FWin32IndexOffset := FStream.Position;
    FWin64IndexOffset := FWin32IndexOffset + 4*FWin32StringCount;
    FSymbolIndexOffset := FWin64IndexOffset + 4*FWin64StringCount;
    FFormIndexOffset := FSymbolIndexOffset + 4*FSymbolStringCount;
    FResourceIndexOffset := FFormIndexOffset + 4*FFormCount;
  end
  else
    raise EInvalidImage.CreateRes(@SInvalidImage);
end;

procedure TNtDelphiResource.CheckStream;
begin
  if (FStream = nil) and FileExists(FFileName) then
  begin
    FStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
    ReadHeader;
  end;
end;

procedure TNtDelphiResource.SetOffset(value: Integer);
var
  position: Integer;
begin
  position := FStream.Position;
  FOffset := value;
  FStream.Position := FOffset;
  ReadHeader;
  FStream.Position := position;
end;

procedure TNtDelphiResource.GetFormNames(names: TStrings);
var
  i, offsetValue, len: Integer;
  formOffset: Integer;
  str: String;
begin
  CheckStream;

  if FVersion = nr1 then
  begin
    FStream.Position := FFormNameOffset;

    for i := 0 to FFormCount - 1 do
    begin
      offsetValue := FStream.ReadInt32;
      len := FStream.ReadInt32 div 2;

      str := FStream.ReadString(FOffset + offsetValue, len);
      names.Add(str);
    end;
  end
  else
  begin
    for i := 0 to FFormCount - 1 do
    begin
      FStream.Position := FFormIndexOffset + 4*i;
      formOffset := FOffset + FStream.ReadInt32;

      FStream.Position := formOffset;
      str := FStream.ReadString;
      names.Add(str);
    end;
  end;
end;

function TNtDelphiResource.FindForm(const name: String): TStream;
var
  i, offsetValue, len: Integer;
  left, right, mid: Integer;
  formOffset, formSize: Integer;
  formData: TBytes;
  str: String;
begin
  CheckStream;

  if FVersion = nr1 then
  begin
    FStream.Position := FFormNameOffset;

    for i := 0 to FFormCount - 1 do
    begin
      offsetValue := FStream.ReadInt32;
      len := FStream.ReadInt32 div 2;

      str := FStream.ReadString(FOffset + offsetValue, len);

      if str = name then
      begin
        FStream.Position := FFormNameOffset + 8*FFormCount + 8*i;
        formOffset := FOffset + FStream.ReadInt32;
        formSize := FStream.ReadInt32;

        FStream.Position := formOffset;
        formData := FStream.ReadBytes(formSize);

        Result := TMemoryStream.Create;
        Result.Write(formData[0], formSize);
        Result.Position := 0;
        Exit;
      end;
    end;
  end
  else
  begin
    // Use the binary search to find the form
    left := 0;
    right := FFormCount - 1;

    while left <= right do
    begin
      mid := (left + right) div 2;

      FStream.Position := FFormIndexOffset + 4*mid;
      formOffset := FOffset + FStream.ReadInt32;

      FStream.Position := formOffset;
      str := FStream.ReadString;

      if str = name then
      begin
        FStream.Position := formOffset;
        FStream.ReadString;
        formSize := FStream.ReadInt32;
        formData := FStream.ReadBytes(formSize);

        Result := TMemoryStream.Create;
        Result.Write(formData[0], formSize);
        Result.Position := 0;
        Exit;
      end
      else if str < name then
        left := mid + 1
      else
        right := mid - 1;
    end;
  end;

  Result := nil;
end;

procedure TNtDelphiResource.GetResourceNames(names: TStrings);
var
  i, offsetValue, len: Integer;
  resourceOffset: Integer;
  str: String;
begin
  CheckStream;

  if FVersion = nr1 then
  begin
    FStream.Position := FResourceNameOffset;

    for i := 0 to FResourceCount - 1 do
    begin
      offsetValue := FStream.ReadInt32;
      len := FStream.ReadInt32 div 2;

      str := FStream.ReadString(FOffset + offsetValue, len);
      names.Add(str);
    end;
  end
  else
  begin
    for i := 0 to FResourceCount - 1 do
    begin
      FStream.Position := FResourceIndexOffset + 4*i;
      resourceOffset := FOffset + FStream.ReadInt32;

      FStream.Position := resourceOffset;
      str := FStream.ReadString;
      names.Add(str);
    end;
  end;
end;

function TNtDelphiResource.FindResource(const id: String): TStream;
var
  i, offsetValue, len: Integer;
  left, right, mid: Integer;
  resourceOffset, resourceSize: Integer;
  resourceData: TBytes;
  str: String;
begin
  CheckStream;

  if FVersion = nr1 then
  begin
    FStream.Position := FResourceNameOffset;

    for i := 0 to FResourceCount - 1 do
    begin
      offsetValue := FStream.ReadInt32;
      len := FStream.ReadInt32 div 2;

      str := FStream.ReadString(FOffset + offsetValue, len);

      if str = id then
      begin
        FStream.Position := FResourceNameOffset + 8*FResourceCount + 8*i;
        resourceOffset := FOffset + FStream.ReadInt32;
        resourceSize := FStream.ReadInt32;

        FStream.Position := resourceOffset;
        resourceData := FStream.ReadBytes(resourceSize);

        Result := TMemoryStream.Create;
        Result.Write(resourceData[0], resourceSize);
        Result.Position := 0;
        Exit;
      end;
    end;
  end
  else
  begin
    // Use the binary search to find the resource
    left := 0;
    right := FResourceCount - 1;

    while left <= right do
    begin
      mid := (left + right) div 2;

      FStream.Position := FResourceIndexOffset + 4*mid;
      resourceOffset := FOffset + FStream.ReadInt32;

      FStream.Position := resourceOffset;
      str := FStream.ReadString;

      if str = id then
      begin
        FStream.Position := resourceOffset;
        FStream.ReadString;
        resourceSize := FStream.ReadInt32;
        resourceData := FStream.ReadBytes(resourceSize);

        Result := TMemoryStream.Create;
        Result.Write(resourceData[0], resourceSize);
        Result.Position := 0;
        Exit;
      end
      else if str < id then
        left := mid + 1
      else
        right := mid - 1;
    end;
  end;

  Result := nil;
end;

class function TNtDelphiResource.ParseKey(name: String; var unitName, itemName: String): Boolean;

  function IsDigit(c: Char): Boolean;
  begin
    Result := (c >= '0') and (c <= '9');
  end;

  function GetNumber(var str: String): Integer;
  begin
    Result := 0;

    while (str <> '') and IsDigit(str[1]) do
    begin
      Result := 10*Result + (Ord(str[1]) - Ord('0'));
      Delete(str, 1, 1);
    end;
  end;

  function Take(var str: String; first, count: Integer): String;
  var
    temp: String;
  begin
    Result := System.Copy(str, first, count);
    temp := System.Copy(str, 1, first - 1);

    if first + count <= Length(str) then
      temp := temp + System.Copy(str, first + count, Length(str));

    str := temp;
  end;

  function TakeLeft(var str: String; count: Integer): String;
  begin
    Result := Take(str, 1, count);
  end;

  function Check(const tag: String): Boolean;
  var
    p: Integer;
  begin
    p := Pos(tag, name);
    Result := p > 0;

    if Result then
      Delete(name, 1, Length(tag) + p - 1);
  end;

const
  RSRC_C = '__rsrc_N';
  RSTR_ZN_C = '__rstr__ZN';
  RSTR_ZZN_C = '__rstr__ZZN';
var
  ignoreRest: Boolean;
  len: Integer;
  str: String;
begin
  // ___rstr__ZN3Fmx6Consts10_SEditCopyE   00AA39A6
  // ___rstr__ZN6System9Rtlconsts24_SCentimetersDescriptionE
  // ___rstr__ZN6System9Rtlconsts24_SConvUnknownDescriptionE
  // ___rstr__ZN5Unit16_SStr1E
  // ___rstr__ZZN5Unit13OneEiE5SStr1
  // ___rstr__ZZN5Unit13OneEvE5SStr1
  // ___rstr__ZZN5Unit13OneEiN6System13UnicodeStringEE5SStr1
  // ___rstr__ZZN5Unit16TForm110FormCreateEPN6System7TObjectEE4SStr
  // ___rstr__ZZN5Unit16TForm110FormCreateEPN6System7TObjectEE13SThisIsSample
  unitName := '';
  itemName := '';

  if Check(RSRC_C) then
  begin
    Result := True;

    len := GetNumber(name);
    unitName := TakeLeft(name, len);

    itemName := name;
    Delete(itemName, Length(itemName), 1);
  end
  else if Check(RSTR_ZN_C) then
  begin
    // ___rstr__ZN5Unit16_SStr1E
    Result := True;

    while (name <> '') and (name[1] <> 'E') do
    begin
      len := GetNumber(name);
      str := TakeLeft(name, len);

      if name[1] = 'E' then
      begin
        itemName := str;

        if (itemName[1] = '_') then
          Delete(itemName, 1, 1);

        Break;
      end
      else
      begin
        if unitName <> '' then
          unitName := unitName + '.';

        unitName := unitName + str;
      end;
    end;
  end
  else if Check(RSTR_ZZN_C) then
  begin
    // ___rstr__ZZN5Unit13OneEiE5SStr1
    // ___rstr__ZZN5Unit13OneEvE5SStr1
    // ___rstr__ZZN5Unit13OneEiN6System13UnicodeStringEE5SStr1
    // ___rstr__ZZN5Unit16TForm110FormCreateEPN6System7TObjectEE4SStr
    // ___rstr__ZZN5Unit16TForm110FormCreateEPN6System7TObjectEE13SThisIsSample
    Result := True;
    str := '';
    ignoreRest := False;

    while (name <> '') do
    begin
      if name[1] = 'E' then
      begin
        ignoreRest := True;

        Delete(name, 1, 1);
        str := '';

        while (name <> '') and not IsDigit(name[1]) do
        begin
          str := str + name[1];
          Delete(name, 1, 1);
        end;

        if str = 'E' then
          str := '';
      end
      else
      begin
        len := GetNumber(name);
        str := TakeLeft(name, len);
      end;

      if name = '' then
      begin
        itemName := str;

        if (itemName[1] = '_') then
          Delete(itemName, 1, 1);

        Break;
      end
      else if (str <> '') and not ignoreRest and (unitName = '') then
      begin
        if unitName <> '' then
          unitName := unitName + '.';

        unitName := unitName + str;
      end;
    end;
  end
  else
  begin
    Result := False;
  end;
end;

{$IFDEF DELPHIDX4}

{$IF defined(EXTERNALLINKER)}
function TNtDelphiResource.FindStringSymbol(const unitName, itemName: String): String;
var
  id, thisId: String;
  left, right, mid: Integer;
begin
  id := unitName + '.' + itemName;

  // Use the binary search to find the string
  left := 0;
  right := FSymbolStringCount - 1;

  while left <= right do
  begin
    mid := (left + right) div 2;

    FStream.Position := FSymbolIndexOffset + 4*mid;

    FStream.Position := FOffset + FStream.ReadInt32;
    thisId := FStream.ReadString;

    if thisId = id then
    begin
      FStream.Position := FOffset + FStream.ReadInt32;
      Result := FStream.ReadString;
      Exit;
    end
    else if thisId < id then
      left := mid + 1
    else
      right := mid - 1;
  end;

  Result := '';
end;
{$ELSE}
function TNtDelphiResource.FindStringWin(id: Word): String;
var
  thisId: Word;
  left, right, mid: Integer;
begin
  // Use the binary search to find the string
  left := 0;
  right := {$IFDEF WIN32}FWin32StringCount{$ELSE}FWin64StringCount{$ENDIF} - 1;

  while left <= right do
  begin
    mid := (left + right) div 2;

    FStream.Position := {$IFDEF WIN32}FWin32IndexOffset{$ELSE}FWin64IndexOffset{$ENDIF} + 4*mid;

    FStream.Position := FOffset + FStream.ReadInt32;
    thisId := FStream.ReadWord;

    if thisId = id then
    begin
      FStream.Position := FOffset + FStream.ReadInt32;
      Result := FStream.ReadString;
      Exit;
    end
    else if thisId < id then
      left := mid + 1
    else
      right := mid - 1;
  end;

  Result := '';
end;
{$IFEND}

function TNtDelphiResource.FindString(resStringRec: PResStringRec): String;
{$IF defined(EXTERNALLINKER)}
var
  unitName, itemName: String;
{$IFEND}
begin
  CheckStream;

{$IF defined(EXTERNALLINKER)}
  ParseKey(String(resStringRec.Key), unitName, itemName);
  Result := FindStringSymbol(unitName, itemName);
{$ELSE}
  Result := FindStringWin(resStringRec.Identifier);
{$IFEND}
end;
{$ENDIF}

procedure TNtDelphiResource.GetWin32Strings(values: TStrings);
var
  i, stringOffset: Integer;
  str: String;
begin
  CheckStream;

  if FVersion = nr1 then
  begin
  end
  else
  begin
    for i := 0 to FWin32StringCount - 1 do
    begin
      FStream.Position := FWin32IndexOffset + 4*i;
      stringOffset := FOffset + FStream.ReadInt32;

      FStream.Position := stringOffset;
      FStream.ReadWord;

      FStream.Position := FOffset + FStream.ReadInt32;
      str := FStream.ReadString;

      values.Add(str);
    end;
  end;
end;

procedure TNtDelphiResource.GetWin64Strings(values: TStrings);
var
  i, stringOffset: Integer;
  str: String;
begin
  CheckStream;

  if FVersion = nr1 then
  begin
  end
  else
  begin
    for i := 0 to FWin64StringCount - 1 do
    begin
      FStream.Position := FWin64IndexOffset + 4*i;
      stringOffset := FOffset + FStream.ReadInt32;

      FStream.Position := stringOffset;
      FStream.ReadWord;

      FStream.Position := FOffset + FStream.ReadInt32;
      str := FStream.ReadString;

      values.Add(str);
    end;
  end;
end;

procedure TNtDelphiResource.GetSymbolStrings(ids, values: TStrings);
var
  i, stringOffset: Integer;
  id, str: String;
begin
  CheckStream;

  if FVersion = nr1 then
  begin
  end
  else
  begin
    for i := 0 to FSymbolStringCount - 1 do
    begin
      FStream.Position := FSymbolIndexOffset + 4*i;
      stringOffset := FOffset + FStream.ReadInt32;

      FStream.Position := stringOffset;

      id := FStream.ReadString;
      ids.Add(id);

      FStream.Position := FOffset + FStream.ReadInt32;
      str := FStream.ReadString;
      values.Add(str);
    end;
  end;
end;

function TNtDelphiResource.FindString(const original: String; id: String; const group: String): String;  //FI:C103
var
  i, j, stringCount: Integer;
  offsetValue, len, thisOffset: Integer;
  groupOffset: Integer;
  str: String;
begin
  if id = '' then
    id := original;

  // Find group
  CheckStream;
  FStream.Position := FStringGroupNameOffset;

  for i := 0 to FStringGroupCount - 1 do
  begin
    offsetValue := FStream.ReadInt32;
    len := FStream.ReadInt32 div 2;

    str := FStream.ReadString(FOffset + offsetValue, len);

    if str = group then
    begin
      FStream.Position := FStringGroupNameOffset + 8*FStringGroupCount + 8*i;
      groupOffset := FOffset + FStream.ReadInt32;
      FStream.Position := groupOffset;
      stringCount := FStream.ReadInt32;

      for j := 0 to stringCount - 1 do
      begin
        offsetValue := FStream.ReadInt32;
        len := FStream.ReadInt32 div 2;

        str := FStream.ReadString(groupOffset + offsetValue, len);

        if str = id then
        begin
          FStream.Position := groupOffset + 4 + 8*stringCount + 8*j;
          thisOffset := FStream.ReadInt32;
          len := FStream.ReadInt32 div 2;

          FStream.Position := groupOffset + thisOffset;
          Result := FStream.ReadString(len);
          Exit;
        end;
      end;
    end;
  end;

  Result := original;
end;


// TNtDelphiResources

constructor TNtDelphiResources.Create;
begin
  inherited;
  FLanguages := TList<TNtDelphiResource>.Create;
  FLanguageNames := TList<TNtResourceLanguage>.Create;
  FLanguageIndex := -1;
  FResourceName := NTRES_RESOURCE_NAME_C;
  FLoaded := False;
end;

destructor TNtDelphiResources.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  while FLanguages.Count > 0 do
  begin
    FLanguages[0].Free;
    FLanguages.Delete(0);
  end;
{$ENDIF}

  FLanguages.Free;

{$IFNDEF AUTOREFCOUNT}
  while FLanguageNames.Count > 0 do
  begin
    FLanguageNames[0].Free;
    FLanguageNames.Delete(0);
  end;
{$ENDIF}

  FLanguageNames.Free;
  FStream.Free;

  inherited;
end;

function TNtDelphiResources.FindLanguage(const id: String): TNtResourceLanguage;
var
  i: Integer;
begin
  for i := 0 to FLanguageNames.Count - 1 do
  begin
    Result := FLanguageNames[i];

    if Result.Id = id then
      Exit;
  end;

  Result := nil;
end;

function TNtDelphiResources.GetOriginal(const id: String): String;
var
  language: TNtResourceLanguage;
begin
  language := FindLanguage(id);

  if language <> nil then
    Result := language.Original
  else
    Result := '';
end;

function TNtDelphiResources.GetNative(const id: String): String;
var
  language: TNtResourceLanguage;
begin
  language := FindLanguage(id);

  if language <> nil then
    Result := language.Native
  else
    Result := '';
end;

function TNtDelphiResources.GetLocalized(const id: String): String;
var
  language: TNtResourceLanguage;
begin
  language := FindLanguage(id);

  if language <> nil then
    Result := language.Localized
  else
    Result := '';
end;

function TNtDelphiResources.GetLanguageImage(const id: String): String;
var
  i: Integer;
  language: TNtResourceLanguage;
begin
  for i := 0 to FLanguageNames.Count - 1 do
  begin
    language := FLanguageNames[i];

    if language.FId = id then
    begin
      Result := language.FImage;
      Exit;
    end;
  end;

  Result := '';
end;

function TNtDelphiResources.Translate(const original, id: String): TNtResourceLanguage;
begin
  Result := TNtResourceLanguage.Create;
  Result.FOriginal := original;
  Result.FId := id;

  FLanguageNames.Add(Result);
end;

function TNtDelphiResources.Add(const original, native, localized, id: String): TNtResourceLanguage;
begin
  Result := TNtResourceLanguage.Create;
  Result.FOriginal := original;
  Result.FNative := native;
  Result.FLocalized := localized;
  Result.FId := id;

  FLanguageNames.Add(Result);
end;

function TNtDelphiResources.GetCount: Integer;
begin
  CheckLoad;
  Result := FLanguages.Count;
end;

function TNtDelphiResources.GetEnabled: Boolean;
begin
  Result := Count > 0;
end;

function TNtDelphiResources.GetLanguage(i: Integer): TNtDelphiResource;
begin
  Result := FLanguages[i];
end;

function TNtDelphiResources.GetCurrent: TNtDelphiResource;
begin
  CheckLoad;

  if FLanguageIndex >= 0 then
    Result := Languages[FLanguageIndex]
  else
    Result := nil
end;

function TNtDelphiResources.GetLanguageId: String;
begin
  if Current <> nil then
    Result := Current.FId
  else
    Result := '';
end;

procedure TNtDelphiResources.SetLanguageId(const value: String);

  function Process(const id: String): Boolean;
  var
    i: Integer;
    language: TNtDelphiResource;
  begin
    for i := 0 to Count - 1 do
    begin
      language := Languages[i];

      if language.FId = id then
      begin
        FLanguageIndex := i;
        LoadedResourceLocale := language.FId;
        Result := True;
        Exit;
      end;
    end;

    Result := False;
  end;

var
  id, language, script, country, variant: String;
begin
  if Process(value) then
    Exit;

  TNtBase.ParseLocaleId(value, language, script, country, variant);

  if country <> '' then
  begin
    id := language;

    if script <> '' then
      id := id + '-' + script;

    if Process(id) then
      Exit;
  end;

  if script <> '' then
  begin
    if Process(language) then
      Exit;
  end;

  FLanguageIndex := -1;
end;

procedure TNtDelphiResources.CheckLoad;
begin
  if not FLoaded then
    Load;
end;

function TNtDelphiResources.GetResourceDirectories: TStringDynArray;
var
  values: TStringList;

  function Add(dir: String): Boolean;
  begin
    Result := (dir <> '') and DirectoryExists(dir);

    if Result then
      values.Add(dir);
  end;

var
  i: Integer;
  applicationName: String;
begin
  values := TStringList.Create;
  try
    Add(FResourcePath);

    applicationName := ParamStr(0);

{$IFDEF MSWINDOWS}
    Add(ExtractFileDir(applicationName));
{$ENDIF}

{$IFDEF DELPHIXE4}
    applicationName := TPath.GetFileNameWithoutExtension(applicationName);

    if (applicationName = '') or not Add(TPath.GetDocumentsPath + PathDelim + applicationName) then
      Add(TPath.GetDocumentsPath);
{$ENDIF}

    SetLength(Result, values.Count);

    for i := 0 to values.Count - 1 do
      Result[i] := values[i];
  finally
    values.Free;
  end;
end;

procedure TNtDelphiResources.Load(resourceFileName: String);

  function FindResourceFile(directories: TStringDynArray; var fileName: String): Boolean;
  var
    i: Integer;
  begin
    // If a resource file is specified and it exists use it.
    // Otherwise try to find the resource file from the default directories
    if FileExists(FResourcePath) then
    begin
      fileName := FResourcePath;
      Result := True;
    end
    else
    begin
      for i := 0 to Length(directories) - 1 do
      begin
        fileName := directories[i] + PathDelim + ResourceName + '.ntres';
        Result := FileExists(fileName);

        if Result then
          Exit;
      end;

      Result := False;
    end;
  end;

{$IFDEF DELPHIXE2}
  function FindLanguagesFiles(directories: TStringDynArray; var files: TStringDynArray): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to Length(directories) - 1 do
    begin
      files := TDirectory.GetFiles(directories[i], '*.ntlang');
      Result := Length(files) > 0;

      if Result then
        Exit;
    end;

    Result := False;
  end;
{$ENDIF}

  procedure LoadResourceFile(stream: TStream);
  var
    i, idCount: Integer;
    offsetValue, len: Integer;
    header: TBytes;
    item: TNtDelphiResource;
  begin
    FStream.Free;
    FStream := stream;

    header := FStream.ReadBytes(Length(NTRES_MAGIC_C));

    if not CompareMem(@NTRES_MAGIC_C, @header[0], Length(NTRES_MAGIC_C)) then
      raise EInvalidImage.CreateRes(@SInvalidImage);

    FVersion := TNtResourceVersion(FStream.ReadInt32);

    // Read language ids
    idCount := FStream.ReadInt32;

    for i := 0 to idCount - 1 do  //FI:W528
    begin
      offsetValue := FStream.ReadInt32;
      len := FStream.ReadInt32;

      item := TNtDelphiResource.Create;
      item.FId := FStream.ReadAnsi(offsetValue, len);
      item.FStream := FStream;
      FLanguages.Add(item);
    end;

    // Read language data offsets
    for i := 0 to idCount - 1 do
    begin
      Languages[i].Offset := FStream.ReadInt32;
      FStream.ReadInt32;
    end;
  end;

{$IFDEF DELPHIXE2}
  procedure LoadLanguageFiles(languageFileNames: TStringDynArray);
  var
    i: Integer;
    languageFileName: String;
    item: TNtDelphiResource;
  begin
    for i := 0 to Length(languageFileNames) - 1 do
    begin
      languageFileName := languageFileNames[i];

      item := TNtDelphiResource.Create;
      item.FId := TPath.GetFileNameWithoutExtension(languageFileName);
      item.FFileName := languageFileName;
      FLanguages.Add(item);
    end;
  end;
{$ENDIF}

var
  directories: TStringDynArray;
{$IFDEF DELPHIXE2}
  languageFileNames: TStringDynArray;
{$ENDIF}
begin
  FLoaded := True;
  directories := GetResourceDirectories;

  if (resourceFileName <> '') or FindResourceFile(directories, resourceFileName) then
  begin
    // Load translations from a .ntres file
    LoadResourceFile(TFileStream.Create(resourceFileName, fmOpenRead or fmShareDenyWrite));
    FTranslationSource := tsFile;
    FTranslationSourceValue := resourceFileName;
  end
{$IFDEF DELPHIXE2}
  else if FindLanguagesFiles(directories, languageFileNames) then
  begin
    // Load translations from .ntlang files
    LoadLanguageFiles(languageFileNames);
    FTranslationSource := tsDirectory;
    FTranslationSourceValue := ExtractFileDir(languageFileNames[0]);
  end
{$ENDIF}
  else if FindResource(HInstance, PChar(FResourceName), RT_RCDATA) > 0 then
  begin
    // Load translations from an embedded .ntres resource
    LoadResourceFile(TResourceStream.Create(HInstance, FResourceName, RT_RCDATA));
    FTranslationSource := tsResource;
    FTranslationSourceValue := FResourceName;
  end
  else
  begin
    // No translation file or resource found
    if not SameText(FResourceName, NTRES_RESOURCE_NAME_C) then
      raise EReadError.CreateResFmt(@SResNotFound, [FResourceName]);

    Exit;
  end;

  if DefaultLocale <> '' then
    LanguageId := DefaultLocale
  else
    FLanguageIndex := -1;
end;

function TNtDelphiResources.Find(const id: String): Integer;
begin
  for Result := 0 to Count - 1 do
    if Languages[Result].Id = id then
      Exit;

  Result := -1;
end;

function TNtDelphiResources.FormExists(const name: String): Boolean;
var
  thisStream: TStream;
begin
  thisStream := FindForm(name);
  Result := thisStream <> nil;
  thisStream.Free;
end;

function TNtDelphiResources.FindForm(const name: String): TStream;
begin
  CheckLoad;

  if Current <> nil then
    Result := Current.FindForm(name)
  else
    Result := nil;
end;

{$IFDEF DELPHIDX4}
function TNtDelphiResources.GetString(resStringRec: PResStringRec): String;
begin
  CheckLoad;

  if Current <> nil then
    Result := Current.FindString(resStringRec)
  else
    Result := '';
end;
{$ENDIF}

function TNtDelphiResources.GetString(const original, id, group: String): String;
begin
  CheckLoad;

  if Current <> nil then
    Result := Current.FindString(original, id, group)
  else
    Result := original;
end;

{$IFDEF DELPHIDX4}
function TNtDelphiResources.GetStringInLanguage(
  const language: String;
  resStringRec: PResStringRec): String;
var
  index: Integer;
begin
  CheckLoad;

  index := Find(language);

  if index >= 0 then
    Result := Languages[index].FindString(resStringRec)
  else
    Result := '';
end;
{$ENDIF}

function TNtDelphiResources.GetStringInLanguage(
  const language: String;
  const original, id, group: String): String;
var
  index: Integer;
begin
  CheckLoad;

  index := Find(language);

  if index >= 0 then
    Result := Languages[index].FindString(original, id, group)
  else
    Result := original;
end;

function TNtDelphiResources.GetResource(const id: String; resType: PChar): TStream;
begin
  CheckLoad;

  if Current <> nil then
    Result := Current.FindResource(id)
  else
    Result := TResourceStream.Create(HInstance, id, resType);
end;

class function TNtDelphiResources.GetResources: TNtDelphiResources;
begin
  if NtResources = nil then
    NtResources := TNtDelphiResources.Create;

  Result := NtResources;
end;

{$IFDEF DELPHIDX4}
function TranslateResourceString(resStringRec: PResStringRec): String;
begin
  Result := NtResourceEx._T(resStringRec);
end;

procedure InitializeResourceStringTranslation;
begin
  LoadResStringFunc := TranslateResourceString;
end;
{$ENDIF}

initialization
  TNtDelphiResources.GetResources;
finalization
  NtResources.Free;
end.
