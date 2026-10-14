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


  Language data version 2 (using resource strings)

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


  Language data version 1 (not using resource strings)

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
  SyncObjs,
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

  { @abstract Translation data of a .ntres file, a .ntlang file or an embedded resource.
    The languages of the same file or resource share the source.
    An embedded resource is read directly from memory, so reading it needs no lock.
    A file is read through a stream that is protected by a lock. }
  TNtResourceSource = class(TInterfacedObject)
  private
    FStream: TStream;      // File stream. nil if the data is in memory.
    FOwner: TStream;       // Stream that owns FMemory
    FMemory: PByte;
    FSize: Integer;
    FLock: TCriticalSection;

  public
    constructor CreateFile(const fileName: String);
    constructor CreateMemory(stream: TCustomMemoryStream);
    destructor Destroy; override;
  end;

  { @abstract Parsed header of one language.
    The object is never modified after it has been created, so several threads
    can read it at the same time. }
  TNtDelphiResourceData = class(TObject)
  private
    FSource: TNtResourceSource;
    FSourceRef: IInterface;  // Keeps the source alive
    FStrings: TBytes;        // String area of a file source. Empty if the source is in memory.
    FStringsOffset: Integer;
    FOffset: Integer;
    FVersion: TNtResourceVersion;
    FLangId: String;

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

  public
    constructor Create(source: TNtResourceSource; offset, size: Integer);
  end;

  TNtDelphiResource = class(TObject)
  private
    FFileName: String;
    FId: String;
    FData: TNtDelphiResourceData;

    function GetData: TNtDelphiResourceData;
    function GetOffset: Integer;
    function GetFormCount: Integer;
    function GetResourceCount: Integer;
    function GetVersion: TNtResourceVersion;
    function GetWin32StringCount: Integer;
    function GetWin64StringCount: Integer;
    function GetSymbolStringCount: Integer;

    function DoFindForm(const name: String; readData: Boolean; out stream: TStream): Boolean;

{$IFDEF DELPHIDX4}
  {$IF defined(EXTERNALLINKER)}
    function FindStringSymbol(data: TNtDelphiResourceData; const unitName, itemName: String): String;
  {$ELSE}
    function FindStringWin(data: TNtDelphiResourceData; id: Word): String;
  {$IFEND}
{$ENDIF}

  public
    destructor Destroy; override;

    function FindForm(const name: String): TStream;
    function FormExists(const name: String): Boolean;
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
    property Offset: Integer read GetOffset;
    property FormCount: Integer read GetFormCount;
    property ResourceCount: Integer read GetResourceCount;
    property Version: TNtResourceVersion read GetVersion;
    property Win32StringCount: Integer read GetWin32StringCount;
    property Win64StringCount: Integer read GetWin64StringCount;
    property SymbolStringCount: Integer read GetSymbolStringCount;
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
    FResourceName: String;
    FResourcePath: String;
    FCascadingEnabled: Boolean;
    FLanguageIndex: Integer;
    FLanguages: TList<TNtDelphiResource>;  // nil until loaded, never modified after it has been published
    FRetiredLanguages: TList<TList<TNtDelphiResource>>;
    FLanguageNames: TList<TNtResourceLanguage>;
    FTranslationSource: TTranslationSource;
    FTranslationSourceValue: String;

    function GetLanguages: TList<TNtDelphiResource>;
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
    procedure DoLoad(resourceFileName: String);

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

{ Enables resource string translation.
  Deprecated: add NtResourceString to the uses clause instead. }
procedure InitializeResourceStringTranslation; deprecated 'Add NtResourceString to the uses clause instead';
{$ENDIF}

function Translate(
  const original: String;
  const id: String = '';
  const group: String = '';
  const language: String = ''): String; overload;

// _TG has been moved to NtResourceEx.pas

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

var
  // Serializes loading. Lookups do not take this lock.
  ResourceLock: TCriticalSection;


// TNtReader

type
  { Read cursor over translation data. It reads either from memory or from a file stream.
    Every lookup uses its own cursor, so concurrent lookups never share a read position.
    A cursor that reads a stream must only be used while the lock of the source is held. }
  TNtReader = record
  private
    FMemory: PByte;     // Memory that starts at the FMemoryOffset position of the data
    FMemoryOffset: Integer;
    FMemorySize: Integer;
    FStream: TStream;   // Used instead of memory if not nil
    FPosition: Integer;

    procedure Read(var buffer; count: Integer);

  public
    constructor Create(memory: PByte; memoryOffset, memorySize: Integer); overload;
    constructor Create(stream: TStream); overload;

    function ReadBytes(length: Integer): TBytes;
    function ReadStream(size: Integer): TStream;

    function ReadAnsi(length: Integer): String; overload;
    function ReadAnsi(offset, length: Integer): String; overload;

    function ReadString: String; overload;
    function ReadString(length: Integer): String; overload;
    function ReadString(offset, length: Integer): String; overload;

    function ReadWord: Word;
    function ReadInt32: Int32;

    property Position: Integer read FPosition write FPosition;
  end;

constructor TNtReader.Create(memory: PByte; memoryOffset, memorySize: Integer);
begin
  FMemory := memory;
  FMemoryOffset := memoryOffset;
  FMemorySize := memorySize;
  FStream := nil;
  FPosition := 0;
end;

constructor TNtReader.Create(stream: TStream);
begin
  FMemory := nil;
  FMemoryOffset := 0;
  FMemorySize := 0;
  FStream := stream;
  FPosition := 0;
end;

procedure TNtReader.Read(var buffer; count: Integer);
begin
  if count < 0 then
    raise EReadError.CreateRes(@SReadError);

  if count = 0 then
    Exit;

  if FStream <> nil then
  begin
    FStream.Position := FPosition;
    FStream.ReadBuffer(buffer, count);
  end
  else
  begin
    // Int64 so that a corrupted offset cannot overflow and pass the check
    if (FPosition < FMemoryOffset) or (Int64(FPosition) - FMemoryOffset + count > FMemorySize) then
      raise EReadError.CreateRes(@SReadError);

    Move((FMemory + (FPosition - FMemoryOffset))^, buffer, count);
  end;

  Inc(FPosition, count);
end;

function TNtReader.ReadBytes(length: Integer): TBytes;
begin
  SetLength(Result, length);

  if length > 0 then
    Read(Result[0], length);
end;

function TNtReader.ReadStream(size: Integer): TStream;
var
  stream: TMemoryStream;
begin
  if size < 0 then
    raise EReadError.CreateRes(@SReadError);

  stream := TMemoryStream.Create;
  try
    stream.Size := size;

    if size > 0 then
      Read(stream.Memory^, size);
  except
    stream.Free;
    raise;
  end;

  Result := stream;
end;

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

function TNtReader.ReadAnsi(length: Integer): String;
var
  data: TBytes;
begin
  data := ReadBytes(length);
{$IFDEF DELPHIXE3}
  Result := TEncoding.ANSI.GetString(data);
{$ELSE}
  Result := Convert(data);
{$ENDIF}
end;

function TNtReader.ReadAnsi(offset, length: Integer): String;
var
  currentPosition: Integer;
begin
  currentPosition := FPosition;
  FPosition := offset;
  Result := ReadAnsi(length);
  FPosition := currentPosition;
end;

function TNtReader.ReadString: String;
begin
  Result := ReadString(ReadWord);
end;

function TNtReader.ReadString(length: Integer): String;
begin
  SetLength(Result, length);

  if length > 0 then
    Read(PChar(Result)^, 2*length);
end;

function TNtReader.ReadString(offset, length: Integer): String;
var
  currentPosition: Integer;
begin
  currentPosition := FPosition;
  FPosition := offset;
  Result := ReadString(length);
  FPosition := currentPosition;
end;

function TNtReader.ReadWord: Word;
begin
  Read(Result, SizeOf(Result));
end;

function TNtReader.ReadInt32: Int32;
begin
  Read(Result, SizeOf(Result));
end;


// TNtResourceSource

constructor TNtResourceSource.CreateFile(const fileName: String);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FStream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  FSize := FStream.Size;
end;

constructor TNtResourceSource.CreateMemory(stream: TCustomMemoryStream);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FOwner := stream;
  FMemory := stream.Memory;
  FSize := stream.Size;
end;

destructor TNtResourceSource.Destroy;
begin
  FStream.Free;
  FOwner.Free;
  FLock.Free;
  inherited;
end;

{ Get a cursor for reading the whole source. Use it only while there is no other
  thread that can read the source, or between BeginRead and EndRead. }
function CreateReader(source: TNtResourceSource): TNtReader;
begin
  if source.FStream <> nil then
    Result := TNtReader.Create(source.FStream)
  else
    Result := TNtReader.Create(source.FMemory, 0, source.FSize);
end;

{ Get a cursor for reading the language data.
  @param data     Language data.
  @param strings  True if only the string area (header, string indexes and strings) is read.
  @param reader   Returns the cursor.
  @return True if the source was locked. Then EndRead must be called when reading is done. }
function BeginRead(data: TNtDelphiResourceData; strings: Boolean; out reader: TNtReader): Boolean;
begin
  if strings and (Length(data.FStrings) > 0) then
  begin
    // String area of a file source has been read into memory
    reader := TNtReader.Create(@data.FStrings[0], data.FStringsOffset, Length(data.FStrings));
    Result := False;
  end
  else if data.FSource.FStream = nil then
  begin
    // The source is in memory
    reader := CreateReader(data.FSource);
    Result := False;
  end
  else
  begin
    // The source is a file. The stream position is shared, so lock it.
    data.FSource.FLock.Enter;
    reader := CreateReader(data.FSource);
    Result := True;
  end;
end;

procedure EndRead(data: TNtDelphiResourceData; locked: Boolean);
begin
  if locked then
    data.FSource.FLock.Leave;
end;

function Translate(
  const original: String;
  const id: String = '';
  const group: String = '';
  const language: String = ''): String;
begin
  Result := _T(original, id, group, language);
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


// TNtDelphiResourceData

constructor TNtDelphiResourceData.Create(source: TNtResourceSource; offset, size: Integer);
var
  i, stringsEnd, dataOffset: Integer;
  reader: TNtReader;
  header: TBytes;
begin
  inherited Create;
  FSource := source;
  FSourceRef := source;
  FOffset := offset;

  if (size <= 0) or (FOffset + size > FSource.FSize) then
    size := FSource.FSize - FOffset;

  // The source is not yet shared with other threads, so it can be read without the lock
  reader := CreateReader(FSource);
  reader.Position := FOffset;
  header := reader.ReadBytes(Length(NTLANG1_MAGIC_C));

  if CompareMem(@NTLANG1_MAGIC_C, @header[0], Length(NTLANG1_MAGIC_C)) then
  begin
    FVersion := nr1;
    FLangId := '';

    FFormCount := reader.ReadInt32;
    FStringGroupCount := reader.ReadInt32;
    FResourceCount := reader.ReadInt32;

    FFormNameOffset := reader.Position;
    FStringGroupNameOffset := FFormNameOffset + 16*FFormCount;
    FResourceNameOffset := FStringGroupNameOffset + 16*FStringGroupCount;
  end
  else if CompareMem(@NTLANG2_MAGIC_C, @header[0], Length(NTLANG2_MAGIC_C)) then
  begin
    FVersion := TNtResourceVersion(reader.ReadInt32);
    FLangId := Convert(reader.ReadBytes(LANG_ID_LENGTH_C));

    FWin32StringCount := reader.ReadInt32;
    FWin64StringCount := reader.ReadInt32;
    FSymbolStringCount := reader.ReadInt32;
    FStringCount := reader.ReadInt32;
    FFormCount := reader.ReadInt32;
    FResourceCount := reader.ReadInt32;

    FWin32IndexOffset := reader.Position;
    FWin64IndexOffset := FWin32IndexOffset + 4*FWin32StringCount;
    FSymbolIndexOffset := FWin64IndexOffset + 4*FWin64StringCount;
    FFormIndexOffset := FSymbolIndexOffset + 4*FSymbolStringCount;
    FResourceIndexOffset := FFormIndexOffset + 4*FFormCount;

    if FSource.FStream <> nil then
    begin
      // Strings are stored before forms and resources. Read the header, indexes and strings
      // into memory so that string lookups do not need the lock. Forms and resources are
      // read from the file when they are needed.
      stringsEnd := FOffset + size;
      reader.Position := FFormIndexOffset;

      // Form index is followed by resource index
      for i := 0 to FFormCount + FResourceCount - 1 do
      begin
        dataOffset := FOffset + reader.ReadInt32;

        if dataOffset < stringsEnd then
          stringsEnd := dataOffset;
      end;

      FStringsOffset := FOffset;
      reader.Position := FStringsOffset;
      FStrings := reader.ReadBytes(stringsEnd - FStringsOffset);
    end;
  end
  else
    raise EInvalidImage.CreateRes(@SInvalidImage);
end;


// TNtDelphiResource

destructor TNtDelphiResource.Destroy;
begin
  FData.Free;
  inherited;
end;

function TNtDelphiResource.GetData: TNtDelphiResourceData;
{$IFDEF DELPHIXE2}
var
  source: TNtResourceSource;
  sourceRef: IInterface;
{$ENDIF}
begin
  // The data object is never modified after it has been published, so it can be read without a lock.
  Result := FData;

{$IFDEF DELPHIXE2}
  if (Result <> nil) or (FFileName = '') then
    Exit;

  // Data of a .ntlang file is loaded when it is used for the first time
  ResourceLock.Enter;
  try
    Result := FData;

    if (Result = nil) and FileExists(FFileName) then
    begin
      source := TNtResourceSource.CreateFile(FFileName);
      sourceRef := source;
      Result := TNtDelphiResourceData.Create(source, 0, source.FSize);

      // Exchange is a full memory barrier: other threads that read FData without
      // the lock see the object only after it has been completely created.
      TInterlocked.Exchange<TNtDelphiResourceData>(FData, Result);
    end;
  finally
    ResourceLock.Leave;
  end;
{$ENDIF}
end;

function TNtDelphiResource.GetOffset: Integer;
var
  data: TNtDelphiResourceData;
begin
  data := GetData;

  if data <> nil then
    Result := data.FOffset
  else
    Result := 0;
end;

function TNtDelphiResource.GetFormCount: Integer;
var
  data: TNtDelphiResourceData;
begin
  data := GetData;

  if data <> nil then
    Result := data.FFormCount
  else
    Result := 0;
end;

function TNtDelphiResource.GetResourceCount: Integer;
var
  data: TNtDelphiResourceData;
begin
  data := GetData;

  if data <> nil then
    Result := data.FResourceCount
  else
    Result := 0;
end;

function TNtDelphiResource.GetVersion: TNtResourceVersion;
var
  data: TNtDelphiResourceData;
begin
  data := GetData;

  if data <> nil then
    Result := data.FVersion
  else
    Result := nr1;
end;

function TNtDelphiResource.GetWin32StringCount: Integer;
var
  data: TNtDelphiResourceData;
begin
  data := GetData;

  if data <> nil then
    Result := data.FWin32StringCount
  else
    Result := 0;
end;

function TNtDelphiResource.GetWin64StringCount: Integer;
var
  data: TNtDelphiResourceData;
begin
  data := GetData;

  if data <> nil then
    Result := data.FWin64StringCount
  else
    Result := 0;
end;

function TNtDelphiResource.GetSymbolStringCount: Integer;
var
  data: TNtDelphiResourceData;
begin
  data := GetData;

  if data <> nil then
    Result := data.FSymbolStringCount
  else
    Result := 0;
end;

procedure TNtDelphiResource.GetFormNames(names: TStrings);
var
  i, offsetValue, len: Integer;
  formOffset: Integer;
  str: String;
  locked: Boolean;
  data: TNtDelphiResourceData;
  reader: TNtReader;
begin
  data := GetData;

  if data = nil then
    Exit;

  locked := BeginRead(data, False, reader);
  try
    if data.FVersion = nr1 then
    begin
      reader.Position := data.FFormNameOffset;

      for i := 0 to data.FFormCount - 1 do //FI:W528
      begin
        offsetValue := reader.ReadInt32;
        len := reader.ReadInt32 div 2;

        str := reader.ReadString(data.FOffset + offsetValue, len);
        names.Add(str);
      end;
    end
    else
    begin
      for i := 0 to data.FFormCount - 1 do
      begin
        reader.Position := data.FFormIndexOffset + 4*i;
        formOffset := data.FOffset + reader.ReadInt32;

        reader.Position := formOffset;
        str := reader.ReadString;
        names.Add(str);
      end;
    end;
  finally
    EndRead(data, locked);
  end;
end;

function TNtDelphiResource.FindForm(const name: String): TStream;
begin
  DoFindForm(name, True, Result);
end;

function TNtDelphiResource.FormExists(const name: String): Boolean;
var
  stream: TStream;
begin
  // Only the name is searched. The form data is not read.
  Result := DoFindForm(name, False, stream);
end;

function TNtDelphiResource.DoFindForm(const name: String; readData: Boolean; out stream: TStream): Boolean; //FI:C103
var
  offsetValue, len: Integer;
  left, right, mid: Integer;
  formOffset, formSize: Integer;
  str: String;
  locked: Boolean;
  data: TNtDelphiResourceData;
  reader: TNtReader;
begin //FI:C101
  Result := False;
  stream := nil;
  data := GetData;

  if data = nil then
    Exit;

  locked := BeginRead(data, False, reader);
  try
    if data.FVersion = nr1 then
    begin
      // Form names are alphabetically sorted, so use a binary search.
      left := 0;
      right := data.FFormCount - 1;

      while left <= right do
      begin
        mid := (left + right) div 2;

        reader.Position := data.FFormNameOffset + 8*mid;
        offsetValue := reader.ReadInt32;
        len := reader.ReadInt32 div 2;

        str := reader.ReadString(data.FOffset + offsetValue, len);

        if str = name then
        begin
          Result := True;

          if readData then
          begin
            reader.Position := data.FFormNameOffset + 8*data.FFormCount + 8*mid;
            formOffset := data.FOffset + reader.ReadInt32;
            formSize := reader.ReadInt32;

            reader.Position := formOffset;
            stream := reader.ReadStream(formSize);
          end;

          Exit;
        end
        else if str < name then
          left := mid + 1
        else
          right := mid - 1;
      end;
    end
    else
    begin
      // Use the binary search to find the form
      left := 0;
      right := data.FFormCount - 1;

      while left <= right do
      begin
        mid := (left + right) div 2;

        reader.Position := data.FFormIndexOffset + 4*mid;
        formOffset := data.FOffset + reader.ReadInt32;

        reader.Position := formOffset;
        str := reader.ReadString;

        if str = name then
        begin
          Result := True;

          if readData then
          begin
            formSize := reader.ReadInt32;
            stream := reader.ReadStream(formSize);
          end;

          Exit;
        end
        else if str < name then
          left := mid + 1
        else
          right := mid - 1;
      end;
    end;
  finally
    EndRead(data, locked);
  end;
end;

procedure TNtDelphiResource.GetResourceNames(names: TStrings);
var
  i, offsetValue, len: Integer;
  resourceOffset: Integer;
  str: String;
  locked: Boolean;
  data: TNtDelphiResourceData;
  reader: TNtReader;
begin
  data := GetData;

  if data = nil then
    Exit;

  locked := BeginRead(data, False, reader);
  try
    if data.FVersion = nr1 then
    begin
      reader.Position := data.FResourceNameOffset;

      for i := 0 to data.FResourceCount - 1 do //FI:W528
      begin
        offsetValue := reader.ReadInt32;
        len := reader.ReadInt32 div 2;

        str := reader.ReadString(data.FOffset + offsetValue, len);
        names.Add(str);
      end;
    end
    else
    begin
      for i := 0 to data.FResourceCount - 1 do
      begin
        reader.Position := data.FResourceIndexOffset + 4*i;
        resourceOffset := data.FOffset + reader.ReadInt32;

        reader.Position := resourceOffset;
        str := reader.ReadString;
        names.Add(str);
      end;
    end;
  finally
    EndRead(data, locked);
  end;
end;

function TNtDelphiResource.FindResource(const id: String): TStream; //FI:C103
var
  offsetValue, len: Integer;
  left, right, mid: Integer;
  resourceOffset, resourceSize: Integer;
  str: String;
  locked: Boolean;
  data: TNtDelphiResourceData;
  reader: TNtReader;
begin //FI:C101
  Result := nil;
  data := GetData;

  if data = nil then
    Exit;

  locked := BeginRead(data, False, reader);
  try
    if data.FVersion = nr1 then
    begin
      // Resource names are alphabetically sorted, so use a binary search.
      left := 0;
      right := data.FResourceCount - 1;

      while left <= right do
      begin
        mid := (left + right) div 2;

        reader.Position := data.FResourceNameOffset + 8*mid;
        offsetValue := reader.ReadInt32;
        len := reader.ReadInt32 div 2;

        str := reader.ReadString(data.FOffset + offsetValue, len);

        if str = id then
        begin
          reader.Position := data.FResourceNameOffset + 8*data.FResourceCount + 8*mid;
          resourceOffset := data.FOffset + reader.ReadInt32;
          resourceSize := reader.ReadInt32;

          reader.Position := resourceOffset;
          Result := reader.ReadStream(resourceSize);
          Exit;
        end
        else if str < id then
          left := mid + 1
        else
          right := mid - 1;
      end;
    end
    else
    begin
      // Use the binary search to find the resource
      left := 0;
      right := data.FResourceCount - 1;

      while left <= right do
      begin
        mid := (left + right) div 2;

        reader.Position := data.FResourceIndexOffset + 4*mid;
        resourceOffset := data.FOffset + reader.ReadInt32;

        reader.Position := resourceOffset;
        str := reader.ReadString;

        if str = id then
        begin
          resourceSize := reader.ReadInt32;
          Result := reader.ReadStream(resourceSize);
          Exit;
        end
        else if str < id then
          left := mid + 1
        else
          right := mid - 1;
      end;
    end;
  finally
    EndRead(data, locked);
  end;
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
  ignoreRest, isName: Boolean;
  len: Integer;
  str: String;
begin //FI:C101
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
    while (name <> '') and (name[1] <> 'E') do
    begin
      len := GetNumber(name);

      // Unknown format: there is no name length or the name is too short
      if (len = 0) or (len > Length(name)) then
        Break;

      str := TakeLeft(name, len);

      // Unknown format: no E after the item name
      if name = '' then
        Break;

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

    Result := itemName <> '';
  end
  else if Check(RSTR_ZZN_C) then
  begin
    // ___rstr__ZZN5Unit13OneEiE5SStr1
    // ___rstr__ZZN5Unit13OneEvE5SStr1
    // ___rstr__ZZN5Unit13OneEiN6System13UnicodeStringEE5SStr1
    // ___rstr__ZZN5Unit16TForm110FormCreateEPN6System7TObjectEE4SStr
    // ___rstr__ZZN5Unit16TForm110FormCreateEPN6System7TObjectEE13SThisIsSample
    str := '';
    ignoreRest := False;

    while (name <> '') do
    begin
      if name[1] = 'E' then
      begin
        ignoreRest := True;

        Delete(name, 1, 1);
        str := '';
        isName := False;

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

        // Unknown format: there is no name length or the name is too short
        if (len = 0) or (len > Length(name)) then
          Break;

        str := TakeLeft(name, len);
        isName := True;
      end;

      if name = '' then
      begin
        // The item name is the last length prefixed name
        if isName then
        begin
          itemName := str;

          if (itemName[1] = '_') then
            Delete(itemName, 1, 1);
        end;

        Break;
      end
      else if (str <> '') and not ignoreRest and (unitName = '') then
      begin
        if unitName <> '' then
          unitName := unitName + '.';

        unitName := unitName + str;
      end;
    end;

    Result := itemName <> '';
  end
  else
  begin
    Result := False;
  end;
end;

{$IFDEF DELPHIDX4}

{$IF defined(EXTERNALLINKER)}
function TNtDelphiResource.FindStringSymbol(data: TNtDelphiResourceData; const unitName, itemName: String): String;
var
  id, thisId: String;
  left, right, mid: Integer;
  locked: Boolean;
  reader: TNtReader;
begin
  Result := '';
  id := unitName + '.' + itemName;

  locked := BeginRead(data, True, reader);
  try
    // Use the binary search to find the string
    left := 0;
    right := data.FSymbolStringCount - 1;

    while left <= right do
    begin
      mid := (left + right) div 2;

      reader.Position := data.FSymbolIndexOffset + 4*mid;

      reader.Position := data.FOffset + reader.ReadInt32;
      thisId := reader.ReadString;

      if thisId = id then
      begin
        reader.Position := data.FOffset + reader.ReadInt32;
        Result := reader.ReadString;
        Exit;
      end
      else if thisId < id then
        left := mid + 1
      else
        right := mid - 1;
    end;
  finally
    EndRead(data, locked);
  end;
end;
{$ELSE}
function TNtDelphiResource.FindStringWin(data: TNtDelphiResourceData; id: Word): String;
var
  thisId: Word;
  left, right, mid: Integer;
  locked: Boolean;
  reader: TNtReader;
begin
  Result := '';

  locked := BeginRead(data, True, reader);
  try
    // Use the binary search to find the string
    left := 0;
    right := {$IFDEF WIN32}data.FWin32StringCount{$ELSE}data.FWin64StringCount{$ENDIF} - 1;

    while left <= right do
    begin
      mid := (left + right) div 2;

      reader.Position := {$IFDEF WIN32}data.FWin32IndexOffset{$ELSE}data.FWin64IndexOffset{$ENDIF} + 4*mid;

      reader.Position := data.FOffset + reader.ReadInt32;
      thisId := reader.ReadWord;

      if thisId = id then
      begin
        reader.Position := data.FOffset + reader.ReadInt32;
        Result := reader.ReadString;
        Exit;
      end
      else if thisId < id then
        left := mid + 1
      else
        right := mid - 1;
    end;
  finally
    EndRead(data, locked);
  end;
end;
{$IFEND}

function TNtDelphiResource.FindString(resStringRec: PResStringRec): String;
var
  data: TNtDelphiResourceData;
{$IF defined(EXTERNALLINKER)}
  unitName, itemName: String;
{$IFEND}
begin
  data := GetData;

  if data = nil then
  begin
    Result := '';
    Exit;
  end;

{$IF defined(EXTERNALLINKER)}
  ParseKey(String(resStringRec.Key), unitName, itemName);
  Result := FindStringSymbol(data, unitName, itemName);
{$ELSE}
  Result := FindStringWin(data, resStringRec.Identifier);
{$IFEND}
end;
{$ENDIF}

procedure TNtDelphiResource.GetWin32Strings(values: TStrings);
var
  i, stringOffset: Integer;
  str: String;
  locked: Boolean;
  data: TNtDelphiResourceData;
  reader: TNtReader;
begin
  data := GetData;

  if (data = nil) or (data.FVersion = nr1) then
    Exit;

  locked := BeginRead(data, True, reader);
  try
    for i := 0 to data.FWin32StringCount - 1 do
    begin
      reader.Position := data.FWin32IndexOffset + 4*i;
      stringOffset := data.FOffset + reader.ReadInt32;

      reader.Position := stringOffset;
      reader.ReadWord;

      reader.Position := data.FOffset + reader.ReadInt32;
      str := reader.ReadString;

      values.Add(str);
    end;
  finally
    EndRead(data, locked);
  end;
end;

procedure TNtDelphiResource.GetWin64Strings(values: TStrings);
var
  i, stringOffset: Integer;
  str: String;
  locked: Boolean;
  data: TNtDelphiResourceData;
  reader: TNtReader;
begin
  data := GetData;

  if (data = nil) or (data.FVersion = nr1) then
    Exit;

  locked := BeginRead(data, True, reader);
  try
    for i := 0 to data.FWin64StringCount - 1 do
    begin
      reader.Position := data.FWin64IndexOffset + 4*i;
      stringOffset := data.FOffset + reader.ReadInt32;

      reader.Position := stringOffset;
      reader.ReadWord;

      reader.Position := data.FOffset + reader.ReadInt32;
      str := reader.ReadString;

      values.Add(str);
    end;
  finally
    EndRead(data, locked);
  end;
end;

procedure TNtDelphiResource.GetSymbolStrings(ids, values: TStrings);
var
  i, stringOffset: Integer;
  id, str: String; //FI:W517
  locked: Boolean;
  data: TNtDelphiResourceData;
  reader: TNtReader;
begin
  data := GetData;

  if (data = nil) or (data.FVersion = nr1) then
    Exit;

  locked := BeginRead(data, True, reader);
  try
    for i := 0 to data.FSymbolStringCount - 1 do
    begin
      reader.Position := data.FSymbolIndexOffset + 4*i;
      stringOffset := data.FOffset + reader.ReadInt32;

      reader.Position := stringOffset;

      id := reader.ReadString;
      ids.Add(id);

      reader.Position := data.FOffset + reader.ReadInt32;
      str := reader.ReadString;
      values.Add(str);
    end;
  finally
    EndRead(data, locked);
  end;
end;

function TNtDelphiResource.FindString(const original: String; id: String; const group: String): String;  //FI:C103
var
  left, right, mid, stringCount: Integer;
  offsetValue, len, thisOffset: Integer;
  groupOffset: Integer;
  found, locked: Boolean;
  str: String;
  data: TNtDelphiResourceData;
  reader: TNtReader;
begin
  Result := original;

  if id = '' then
    id := original;

  data := GetData;

  // Only version 1 data contains string groups
  if (data = nil) or (data.FStringGroupCount = 0) then
    Exit;

  // Version 1 string groups are stored after the forms. They are not in the string area.
  locked := BeginRead(data, False, reader);
  try
    // Find group. Groups are alphabetically sorted by id, so use a binary search.
    found := False;
    groupOffset := 0;
    left := 0;
    right := data.FStringGroupCount - 1;

    while left <= right do
    begin
      mid := (left + right) div 2;

      reader.Position := data.FStringGroupNameOffset + 8*mid;
      offsetValue := reader.ReadInt32;
      len := reader.ReadInt32 div 2;

      str := reader.ReadString(data.FOffset + offsetValue, len);

      if str = group then
      begin
        reader.Position := data.FStringGroupNameOffset + 8*data.FStringGroupCount + 8*mid;
        groupOffset := data.FOffset + reader.ReadInt32;
        found := True;
        Break;
      end
      else if str < group then
        left := mid + 1
      else
        right := mid - 1;
    end;

    if found then
    begin
      reader.Position := groupOffset;
      stringCount := reader.ReadInt32;

      // Ids within a group are alphabetically sorted too, so use a binary search.
      left := 0;
      right := stringCount - 1;

      while left <= right do
      begin
        mid := (left + right) div 2;

        reader.Position := groupOffset + 4 + 8*mid;
        offsetValue := reader.ReadInt32;
        len := reader.ReadInt32 div 2;

        str := reader.ReadString(groupOffset + offsetValue, len);

        if str = id then
        begin
          reader.Position := groupOffset + 4 + 8*stringCount + 8*mid;
          thisOffset := reader.ReadInt32;
          len := reader.ReadInt32 div 2;

          reader.Position := groupOffset + thisOffset;
          Result := reader.ReadString(len);
          Exit;
        end
        else if str < id then
          left := mid + 1
        else
          right := mid - 1;
      end;
    end;
  finally
    EndRead(data, locked);
  end;
end;


// TNtDelphiResources

procedure FreeLanguages(languages: TList<TNtDelphiResource>);
{$IFNDEF AUTOREFCOUNT}
var
  i: Integer;
{$ENDIF}
begin
  if languages = nil then
    Exit;

{$IFNDEF AUTOREFCOUNT}
  for i := 0 to languages.Count - 1 do
    languages[i].Free;
{$ENDIF}

  languages.Free;
end;

function IndexOfLanguage(languages: TList<TNtDelphiResource>; const value: String): Integer;

  function Process(const id: String): Integer;
  begin
    for Result := 0 to languages.Count - 1 do
      if languages[Result].FId = id then
        Exit;

    Result := -1;
  end;

var
  id, language, script, country, variant: String;
begin
  Result := Process(value);

  if Result >= 0 then
    Exit;

  TNtBase.ParseLocaleId(value, language, script, country, variant);

  if country <> '' then
  begin
    id := language;

    if script <> '' then
      id := id + '-' + script;

    Result := Process(id);

    if Result >= 0 then
      Exit;
  end;

  if script <> '' then
    Result := Process(language);
end;

constructor TNtDelphiResources.Create;
begin
  inherited;
  FLanguages := nil;
  FRetiredLanguages := TList<TList<TNtDelphiResource>>.Create;
  FLanguageNames := TList<TNtResourceLanguage>.Create;
  FLanguageIndex := -1;
  FResourceName := NTRES_RESOURCE_NAME_C;
end;

destructor TNtDelphiResources.Destroy;
var
  i: Integer;
begin
  FreeLanguages(FLanguages);

  for i := 0 to FRetiredLanguages.Count - 1 do
    FreeLanguages(FRetiredLanguages[i]);

  FRetiredLanguages.Free;

{$IFNDEF AUTOREFCOUNT}
  while FLanguageNames.Count > 0 do
  begin
    FLanguageNames[0].Free;
    FLanguageNames.Delete(0);
  end;
{$ENDIF}

  FLanguageNames.Free;

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

function TNtDelphiResources.GetLanguages: TList<TNtDelphiResource>;
begin
  // The list is never modified after it has been published, so it can be read without a lock.
  Result := FLanguages;

  if Result = nil then
  begin
    CheckLoad;
    Result := FLanguages;
  end;
end;

function TNtDelphiResources.GetCount: Integer;
begin
  Result := GetLanguages.Count;
end;

function TNtDelphiResources.GetEnabled: Boolean;
begin
  Result := Count > 0;
end;

function TNtDelphiResources.GetLanguage(i: Integer): TNtDelphiResource;
begin
  Result := GetLanguages[i];
end;

function TNtDelphiResources.GetCurrent: TNtDelphiResource;
var
  index: Integer;
  languages: TList<TNtDelphiResource>;
begin
  languages := GetLanguages;
  index := FLanguageIndex;

  // The index is checked against the list because Load may have replaced the list
  if (index >= 0) and (index < languages.Count) then
    Result := languages[index]
  else
    Result := nil
end;

function TNtDelphiResources.GetLanguageId: String;
var
  language: TNtDelphiResource;
begin
  language := Current;

  if language <> nil then
    Result := language.FId
  else
    Result := '';
end;

procedure TNtDelphiResources.SetLanguageId(const value: String);
var
  index: Integer;
  languages: TList<TNtDelphiResource>;
begin
  languages := GetLanguages;
  index := IndexOfLanguage(languages, value);

  if index >= 0 then
    LoadedResourceLocale := languages[index].FId;

  FLanguageIndex := index;
end;

procedure TNtDelphiResources.CheckLoad;
begin
  if FLanguages <> nil then
    Exit;

  ResourceLock.Enter;
  try
    if FLanguages = nil then
      DoLoad('');
  finally
    ResourceLock.Leave;
  end;
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
begin
  ResourceLock.Enter;
  try
    DoLoad(resourceFileName);
  finally
    ResourceLock.Leave;
  end;
end;

procedure TNtDelphiResources.DoLoad(resourceFileName: String); //FI:O801

  function FindResourceFile(directories: TStringDynArray; var fileName: String): Boolean;
  var
    i: Integer;
    thisFileName: String;
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
        thisFileName := directories[i] + PathDelim + ResourceName + '.ntres';
        Result := FileExists(thisFileName);

        if Result then
        begin
          fileName := thisFileName;
          Exit;
        end;
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

  procedure LoadResourceFile(source: TNtResourceSource; languages: TList<TNtDelphiResource>);
  var
    i, idCount: Integer;
    offsetValue, len: Integer;
    header: TBytes;
    reader: TNtReader;
    item: TNtDelphiResource;
    sourceRef: IInterface;
  begin
    // Frees the source if it is not used by any language
    sourceRef := source;

    // The source is not yet shared with other threads, so it can be read without the lock
    reader := CreateReader(source);
    header := reader.ReadBytes(Length(NTRES_MAGIC_C));

    if not CompareMem(@NTRES_MAGIC_C, @header[0], Length(NTRES_MAGIC_C)) then
      raise EInvalidImage.CreateRes(@SInvalidImage);

    FVersion := TNtResourceVersion(reader.ReadInt32);

    // Read language ids
    idCount := reader.ReadInt32;

    for i := 0 to idCount - 1 do  //FI:W528
    begin
      offsetValue := reader.ReadInt32;
      len := reader.ReadInt32;

      item := TNtDelphiResource.Create;
      item.FId := reader.ReadAnsi(offsetValue, len);
      languages.Add(item);
    end;

    // Read language data offsets and sizes. All languages share the same source.
    for i := 0 to idCount - 1 do
    begin
      offsetValue := reader.ReadInt32;
      len := reader.ReadInt32;
      languages[i].FData := TNtDelphiResourceData.Create(source, offsetValue, len);
    end;
  end;

{$IFDEF DELPHIXE2}
  procedure LoadLanguageFiles(languageFileNames: TStringDynArray; languages: TList<TNtDelphiResource>);
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
      languages.Add(item);
    end;
  end;
{$ENDIF}

  procedure Publish(languages: TList<TNtDelphiResource>);
  var
    oldLanguages: TList<TNtDelphiResource>;
  begin
{$IFDEF DELPHIXE}
    // Exchange is a full memory barrier: other threads that read FLanguages without
    // the lock see the list only after it has been completely built.
    oldLanguages := TInterlocked.Exchange<TList<TNtDelphiResource>>(FLanguages, languages);
{$ELSE}
    oldLanguages := FLanguages;
    FLanguages := languages;
{$ENDIF}

    // Other threads may still be reading the old list, so it is freed only when the resources are destroyed.
    if oldLanguages <> nil then
      FRetiredLanguages.Add(oldLanguages);
  end;

var
  index: Integer;
  directories: TStringDynArray;
  languages: TList<TNtDelphiResource>;
{$IFDEF DELPHIXE2}
  languageFileNames: TStringDynArray;
{$ENDIF}
begin
  languages := TList<TNtDelphiResource>.Create;
  try
    directories := GetResourceDirectories;

    if (resourceFileName <> '') or FindResourceFile(directories, resourceFileName) then
    begin
      // Load translations from a .ntres file
      LoadResourceFile(TNtResourceSource.CreateFile(resourceFileName), languages);
      FTranslationSource := tsFile;
      FTranslationSourceValue := resourceFileName;
    end
{$IFDEF DELPHIXE2}
    else if FindLanguagesFiles(directories, languageFileNames) then
    begin
      // Load translations from .ntlang files
      LoadLanguageFiles(languageFileNames, languages);
      FTranslationSource := tsDirectory;
      FTranslationSourceValue := ExtractFileDir(languageFileNames[0]);
    end
{$ENDIF}
    else if FindResource(HInstance, PChar(FResourceName), RT_RCDATA) > 0 then
    begin
      // Load translations from an embedded .ntres resource
      LoadResourceFile(TNtResourceSource.CreateMemory(TResourceStream.Create(HInstance, FResourceName, RT_RCDATA)), languages);
      FTranslationSource := tsResource;
      FTranslationSourceValue := FResourceName;
    end
    else
    begin
      // No translation file or resource found
      FTranslationSource := tsNone;
      FTranslationSourceValue := '';

      if not SameText(FResourceName, NTRES_RESOURCE_NAME_C) then
        raise EReadError.CreateResFmt(@SResNotFound, [FResourceName]);
    end;
  except
    // Publish an empty list so that a failed load is not retried on every lookup
    FreeLanguages(languages);
    FLanguageIndex := -1;
    Publish(TList<TNtDelphiResource>.Create);
    raise;
  end;

  index := -1;

  if DefaultLocale <> '' then
  begin
    index := IndexOfLanguage(languages, DefaultLocale);

    if index >= 0 then
      LoadedResourceLocale := languages[index].FId;
  end;

  FLanguageIndex := index;
  Publish(languages);
end;

function TNtDelphiResources.Find(const id: String): Integer;
var
  languages: TList<TNtDelphiResource>;
begin
  languages := GetLanguages;

  for Result := 0 to languages.Count - 1 do
    if languages[Result].Id = id then
      Exit;

  Result := -1;
end;

function TNtDelphiResources.FormExists(const name: String): Boolean;
var
  language: TNtDelphiResource;
begin
  language := Current;
  Result := (language <> nil) and language.FormExists(name);
end;

function TNtDelphiResources.FindForm(const name: String): TStream;
var
  language: TNtDelphiResource;
begin
  language := Current;

  if language <> nil then
    Result := language.FindForm(name)
  else
    Result := nil;
end;

{$IFDEF DELPHIDX4}
function TNtDelphiResources.GetString(resStringRec: PResStringRec): String;
var
  language: TNtDelphiResource;
begin
  language := Current;

  if language <> nil then
    Result := language.FindString(resStringRec)
  else
    Result := '';
end;
{$ENDIF}

function TNtDelphiResources.GetString(const original, id, group: String): String;
var
  language: TNtDelphiResource;
begin
  language := Current;

  if language <> nil then
    Result := language.FindString(original, id, group)
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
  index := Find(language);

  if index >= 0 then
    Result := Languages[index].FindString(original, id, group)
  else
    Result := original;
end;

function TNtDelphiResources.GetResource(const id: String; resType: PChar): TStream;
var
  language: TNtDelphiResource;
begin
  language := Current;

  if language <> nil then
    Result := language.FindResource(id)
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
procedure InitializeResourceStringTranslation;
begin
  // Same as NtResourceString. Returns the original string if there is no translation.
  LoadResStringFunc := NtResourceEx.TranslateResourceString;
end;
{$ENDIF}

initialization
  ResourceLock := TCriticalSection.Create;
  TNtDelphiResources.GetResources;
finalization
  NtResources.Free;
  ResourceLock.Free;
end.
