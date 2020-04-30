{
  @abstract Implements multilingual resource format for FireMonkey and VCL.

  FireMonkey does not support resource DLLs in Android and iOS. This is why the standard
  resource DLL based localization method cannot be used with FireMonkey if mobiles platforms
  are used. This format is designed to make it possible to localize FireMonkey applications.
  Even this method is ment to be used to localize FireMonkey applications you can also
  use it with VLC application although the resource DLL based method is the prefered way.

  The basic idea is to store all localized forms, strings and resources into a
  single file that can be added as custom RCDATA resource inside the application
  executable.

  You will have full control of form localization. This mean that you can override
  any form properties including dimension, colors and fonts. However you cannot
  use resourcestring feature. If your code likes like this.

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
  All strings are without null termination. Strings lengths are specified in the index.
  Length is length in bytes NOT charcters.
  Language id ara in ASCII. All other strings are UTF16-LE strings.
  ~ means variable length.


  File format:
  [header]      4  'NTRE'
  [version]     4  File format version
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


  Language data

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
  NTLANG_MAGIC_C: array[0..3] of Byte = ($4e, $54, $4c, $41);  // 'NTLA'

type
  TNtDelphiResources = class;

  { @abstract Contains information about the language such as id, name and image. }
  TNtResourceLanguage = class(TObject)
  private
    FOriginal: String;
    FId: String;
    FImage: String;

  public
    { @abstract Add a language image resource id.
      If specified the image will be drawn with the language name.
      @param value  Image resource id.
      @return This same TNtResourceLanguage object. }
    function AddImage(const value: String): TNtResourceLanguage;

    property Original: String read FOriginal;
    property Id: String read FId;
    property Image: String read FImage;
  end;

  TNtDelphiResource = class(TObject)
  private
    FOffset: Integer;
    FStream: TStream;
    FFileName: String;
    FId: String;

    FFormCount: Integer;
    FFormNameOffset: Integer;
    FStringGroupCount: Integer;
    FStringGroupNameOffset : Integer;
    FResourceCount: Integer;
    FResourceNameOffset: Integer;

    procedure SetOffset(value: Integer);

    procedure CheckStream;
    procedure ReadHeader;

  public
    function FindForm(const name: String): TStream;
    function FindString(const original: String; id: String; const group: String): String;
    function FindResource(const id: String): TStream;

    property Id: String read FId;
    property Offset: Integer read FOffset write SetOffset;
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
    function GetLanguageImage(const id: String): String;
    function GetResourceDirectories: TStringDynArray;

    procedure SetLanguageId(const value: String);

    procedure CheckLoad;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Load;

    function Find(const id: String): Integer;

    function FindForm(const name: String): TStream;
    function FormExists(const name: String): Boolean;

    function GetString(const original, id, group: String): String;

    function GetStringInLanguage(
      const language: String;
      const original, id, group: String): String;

    function GetResource(const id: String; resType: PChar = RT_RCDATA): TStream;

    { @abstract Add a language name to the resources.
      Language names are used for example in the select language dialogs.
      The function name is _T because we also want to the string will be extracted.
      @param original  Original language name such as "English".
      @param id        Language id such as "en".
      @return Language resource. }
    function _T(const original, id: String): TNtResourceLanguage;

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
    property LanguageImages[const id: String]: String read GetLanguageImage;
    property TranslationSource: TTranslationSource read FTranslationSource;
    property TranslationSourceValue: String read FTranslationSourceValue;
  end;

{ Get the string value in the current language.
  @param original Original string value.
  @param id       Optional id value. If not present the original value is used.
  @param group    String group.
  @return String value in the current languages.}
function _T(
  const original: String;
  const id: String = '';
  const group: String = ''): String; overload;

function Translate(
  const original: String;
  const id: String = '';
  const group: String = ''): String; overload;

{ Get the string value in the current language.
  @param original Original string value.
  @param group    String group.
  @return String value in the current languages.}
function _TG(const original, group: String): String;

function TranslateGroup(const original, group: String): String;

var
  NtResources: TNtDelphiResources;

implementation

uses
{$IF CompilerVersion = 22}
  Windows,
{$IFEND}
  RTLConsts,
  System.IOUtils,
  NtBase;


// TStreamHelper

type
  TStreamHelper = class helper for TStream
    function ReadBytes(length: Integer): TBytes;

    function ReadAnsi(length: Integer): String; overload;
    function ReadAnsi(offset, length: Integer): String; overload;

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
  Result := TEncoding.Default.GetString(data);
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

function TStreamHelper.ReadString(length: Integer): String;
begin
  SetLength(Result, length);
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


function _T(
  const original: String;
  const id: String = '';
  const group: String = ''): String;
begin
  Result := NtResources.GetString(original, id, group);
end;

function Translate(
  const original: String;
  const id: String = '';
  const group: String = ''): String;
begin
  Result := _T(original, id, group);
end;

function _TG(const original, group: String): String;
begin
  Result := NtResources.GetString(original, '', group);
end;

function TranslateGroup(const original, group: String): String;
begin
  Result := _TG(original, group);
end;


// TNtResourceLanguage

function TNtResourceLanguage.AddImage(const value: String): TNtResourceLanguage;
begin
  FImage := value;
  Result := Self;
end;


// TNtDelphiResource

procedure TNtDelphiResource.ReadHeader;
var
  header: TBytes;
begin
  header := FStream.ReadBytes(Length(NTLANG_MAGIC_C));

  if not CompareMem(@NTLANG_MAGIC_C, @header[0], Length(NTLANG_MAGIC_C)) then
    raise EInvalidImage.CreateRes(@SInvalidImage);

  FFormCount := FStream.ReadInt32;
  FStringGroupCount := FStream.ReadInt32;
  FResourceCount := FStream.ReadInt32;

  FFormNameOffset := FStream.Position;
  FStringGroupNameOffset := FFormNameOffset + 16*FFormCount;
  FResourceNameOffset := FStringGroupNameOffset + 16*FStringGroupCount;
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

function TNtDelphiResource.FindForm(const name: String): TStream;
var
  i, offsetValue, len: Integer;
  formOffset, formSize: Integer;
  formData: TBytes;
  str: String;
begin
  CheckStream;
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

  Result := nil;
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

function TNtDelphiResource.FindResource(const id: String): TStream;
var
  i, offsetValue, len: Integer;
  resourceOffset, resourceSize: Integer;
  resourceData: TBytes;
  str: String;
begin
  CheckStream;
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

  Result := nil;
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

function TNtDelphiResources.GetOriginal(const id: String): String;
var
  i: Integer;
  language: TNtResourceLanguage;
begin
  for i := 0 to FLanguageNames.Count - 1 do
  begin
    language := FLanguageNames[i];

    if language.Id = id then
    begin
      Result := language.Original;
      Exit;
    end;
  end;

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

function TNtDelphiResources._T(
  const original, id: String): TNtResourceLanguage;
begin
  Result := TNtResourceLanguage.Create;
  Result.FOriginal := original;
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

procedure TNtDelphiResources.Load;

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

    FStream.ReadInt32;  // Version number

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

var
  resourceFileName: String;
  languageFileNames: TStringDynArray;
  directories: TStringDynArray;
begin
  FLoaded := True;
  directories := GetResourceDirectories;

  if FindLanguagesFiles(directories, languageFileNames) then
  begin
    // Load translations from local .ntlang files
    LoadLanguageFiles(languageFileNames);
    FTranslationSource := tsDirectory;
    FTranslationSourceValue := ExtractFileDir(languageFileNames[0]);
  end
  else if FindResourceFile(directories, resourceFileName) then
  begin
    // Load translations from a local .ntres file
    LoadResourceFile(TFileStream.Create(resourceFileName, fmOpenRead or fmShareDenyWrite));
    FTranslationSource := tsFile;
    FTranslationSourceValue := resourceFileName;
  end
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

function TNtDelphiResources.GetString(const original, id, group: String): String;
begin
  CheckLoad;

  if Current <> nil then
    Result := Current.FindString(original, id, group)
  else
    Result := original;
end;

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

initialization
  TNtDelphiResources.GetResources;
finalization
  NtResources.Free;
end.
