unit Sport;

interface

uses
  SysUtils, DB, Classes;

type
  TSport = class(TObject)
  private
    FDescription: String;
    FGoalie: Boolean;
    FId: String;
    FName: String;
    FOrigin: String;
    FPlayers: Integer;

  public
    property Description: String read FDescription;
    property Goalie: Boolean read FGoalie;
    property Id: String read FId;
    property Name: String read FName;
    property Origin: String read FOrigin;
    property Players: Integer read FPlayers;
  end;

  TSports = class(TObject)
  private
    FItems: TList;

    function GetCount: Integer;
    function GetItem(i: Integer): TSport;

    procedure ClearItems;

  public
    constructor Create;
    destructor Destroy; override;

    // Load sports from API
    procedure LoadApi(const language: String);

    // Load sports from a dataset
    procedure LoadDatabase(dataset: TDataset);

    // Load sports from stringtable resource
    procedure LoadStringTable(base: Integer = 0);

    // Load sports from a text stream, standalone file or file resource
    procedure LoadTextStream(stream: TStream);
    procedure LoadTextFile(const fileName: String);

    procedure LoadTextResource(
      const resourceName: String;
      const resourceType: String = 'SPORT');

    // Load sports from a JSON stream, standalone file or file resource
    procedure LoadJsonStream(
      stream: TStream;
      language: String = '';
      freeStream: Boolean = True);

    procedure LoadJsonFile(const fileName: String);

    procedure LoadJsonResource(
      const resourceName: String;
      const resourceType: String = 'SPORT');

    // Load sports from a XML stream, standalone file or file resource
    procedure LoadXmlStream(stream: TStream);
    procedure LoadXmlFile(const fileName: String);

    procedure LoadXmlResource(
      const resourceName: String;
      const resourceType: String = 'SPORT');

    // Load sports from a INI stream, standalone file or file resource
    procedure LoadIniStream(stream: TStream);
    procedure LoadIniFile(const fileName: String);

    procedure LoadIniResource(
      const resourceName: String;
      const resourceType: String = 'SPORT');

    property Count: Integer read GetCount;
    property Items[i: Integer]: TSport read GetItem; default;
  end;

implementation

uses
  System.IniFiles,
  System.Json,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  System.Net.URLClient,
  Xml.XMLIntf,
  Xml.XmlDoc,
  NtBase,
  NtLocalization;

constructor TSports.Create;
begin
  inherited;
  FItems := TList.Create;
end;

destructor TSports.Destroy;
begin
  ClearItems;
  FItems.Free;
  inherited;
end;

procedure TSports.ClearItems;
begin
  while FItems.Count > 0 do
  begin
    TSport(FItems[0]).Free;
    FItems.Delete(0);
  end;
end;

function TSports.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TSports.GetItem(i: Integer): TSport;
begin
  Result := FItems[i];
end;

procedure TSports.LoadDatabase(dataset: TDataset);
const
  NAME_C = 'Name';
  PLAYERS_C = 'FieldPlayers';
  GOALIE_C = 'Goalie';
  ORIGIN_C = 'Origin';
  DESCRIPTION_C = 'Description';
var
  item: TSport;
begin
  ClearItems;

  while not dataset.Eof do
  begin
    item := TSport.Create;
    item.FName := dataset.FieldByName(NAME_C).AsString;
    item.FPlayers := dataset.FieldByName(PLAYERS_C).AsInteger;
    item.FGoalie := dataset.FieldByName(GOALIE_C).AsBoolean;
    item.FOrigin := dataset.FieldByName(ORIGIN_C).AsString;
    item.FDescription := dataset.FieldByName(DESCRIPTION_C).AsString;

    FItems.Add(item);
    dataset.Next;
  end;
end;

procedure TSports.LoadStringTable(base: Integer);
var
  current: Integer;

  function GetString(item: Integer): String;
  begin
    Result := LoadStr(base + 10*current + item);
  end;

const
  NAME_C = 0;
  PLAYERS_C = 1;
  GOALIE_C = 2;
  ORIGIN_C = 3;
  DESCRIPTION_C = 4;
var
  item: TSport;
begin
  ClearItems;
  current := 0;

  // Reads country properties until find an empty string (i.e. no resource string value)
  while GetString(NAME_C) <> '' do
  begin
    item := TSport.Create;
    item.FName := GetString(NAME_C);
    item.FPlayers := StrToIntDef(GetString(PLAYERS_C), 0);
    item.FGoalie := GetString(GOALIE_C) <> '0';
    item.FOrigin := GetString(ORIGIN_C);
    item.FDescription := GetString(DESCRIPTION_C);

    FItems.Add(item);
    Inc(current);
  end;
end;


// Text

procedure TSports.LoadTextStream(stream: TStream);

  function IsEof: Boolean;
  begin
    Result := stream.Position >= stream.Size;
  end;

  function GetString: String;
  var
    c: AnsiChar;
    utf8: RawByteString;
  begin
    utf8 := '';

    repeat
      stream.Read(c, 1);

      if not (c in [#9, #10, #13]) then
        utf8 := utf8 + c;
    until IsEof or (c = #9) or (c = #10);

    Result := Utf8ToString(utf8);
  end;

  function GetInteger: Integer;
  begin
    Result := StrToIntDef(GetString, 0);
  end;

var
  header: RawByteString;
  item: TSport;
begin
  ClearItems;

  SetLength(header, 3);
  stream.Read(header[1], 3);

  if header <> #$EF#$BB#$BF then
    raise Exception.Create('Not valid UTF-8');

  try
    while not IsEof do
    begin
      item := TSport.Create;
      item.FId := GetString;
      item.FName := GetString;
      item.FPlayers := GetInteger;
      item.FGoalie := GetInteger > 0;
      item.FOrigin := GetString;
      item.FDescription := GetString;

      FItems.Add(item);
    end;
  finally
    stream.Free;
  end;
end;

procedure TSports.LoadTextFile(const fileName: String);
begin
  LoadTextStream(TFileStream.Create(fileName, fmOpenRead));
end;

procedure TSports.LoadTextResource(const resourceName: String; const resourceType: String);
begin
  LoadTextStream(TNtResources.GetResourceStream(PChar(resourceType), PChar(resourceName)));
end;


// JSON

procedure TSports.LoadJsonStream(
  stream: TStream;
  language: String;
  freeStream: Boolean);
var
  activeJsonItem: TJSONValue;

  function GetString(const name: String): String;
  begin
    Result := activeJsonItem.GetValue<String>(name);
  end;

  function GetInteger(const name: String): Integer;
  begin
    if not activeJsonItem.TryGetValue<Integer>(name, Result) then
      Result := 0;
  end;

  function GetBoolean(const name: String): Boolean;
  begin
    if not activeJsonItem.TryGetValue<Boolean>(name, Result) then
      Result := False;
  end;

var
  languagesArray: TJSONArray;

  function FindLanguage: TJSONValue;
  begin
    for Result in languagesArray do
    begin
      if Result.GetValue<String>('language') = language then
        Exit;
    end;

    Result := nil;
  end;

var
  item: TSport;
  bytes: TArray<Byte>;
  json: TJSONArray;
  jsonItem: TJSONValue;
begin
  ClearItems;

  SetLength(bytes, stream.Size);
  stream.Read(bytes, Length(bytes));

  json := TJSONObject.ParseJSONValue(bytes, 0) as TJSONArray;
  try
    for jsonItem in json do
    begin
      activeJsonItem := jsonItem;
      item := TSport.Create;

      if language = '' then
      begin
        item.FId := GetString('id');
        item.FPlayers := GetInteger('fieldplayers');
        item.FGoalie := GetBoolean('goalie');
        item.FName := GetString('name');
        item.FOrigin := GetString('origin');
        item.FDescription := GetString('description');
      end
      else
      begin
        item.FId := GetString('id');
        item.FPlayers := GetInteger('fieldPlayers');
        item.FGoalie := GetBoolean('goalie');

        languagesArray := jsonItem.FindValue('languages') as TJSONArray;
        activeJsonItem := FindLanguage;

        if activeJsonItem = nil then
          activeJsonItem := languagesArray[0];

        item.FName := GetString('name');
        item.FOrigin := GetString('origin');
        item.FDescription := GetString('description');
      end;

      FItems.Add(item);
    end;
  finally
    json.Free;

    if freeStream then
      stream.Free;
  end;
end;

procedure TSports.LoadJsonResource(const resourceName: String; const resourceType: String);
begin
  LoadJsonStream(TNtResources.GetResourceStream(PChar(resourceType), PChar(resourceName)));
end;

procedure TSports.LoadJsonFile(const fileName: String);
begin
  LoadJsonStream(TFileStream.Create(fileName, fmOpenRead));
end;

procedure TSports.LoadApi(const language: String);
var
  http: TNetHttpClient;
  response: IHTTPResponse;
  responseStream: TMemoryStream;
  headers: TNetHeaders;
begin
  http := TNetHTTPClient.Create(nil);
  responseStream := TMemoryStream.Create;
  try
    http.Accept := 'application/json';
    http.AcceptEncoding := '';
    http.ContentType := 'application/json';

    response := http.Get('https://soluling.com/SportApi/sports', responseStream, headers);

    if response.StatusCode = 200 then
      LoadJsonStream(responseStream, language, False);
  finally
    responseStream.Free;
    http.Free;
  end;

end;


// XML

procedure TSports.LoadXmlStream(stream: TStream);
var
  xmlItem: IXMLNode;

  function GetString(const name: String): String;
  begin
    Result := xmlItem.Attributes[name];
  end;

  function GetInteger(const name: String): Integer;
  begin
    Result := xmlItem.Attributes[name];
  end;

  function GetBoolean(const name: String): Boolean;
  begin
    Result := xmlItem.Attributes[name];
  end;

var
  i: Integer;
  item: TSport;
  document: TXMLDocument;
  sports: IXMLNode;
begin
  ClearItems;

  document := TXMLDocument.Create(nil);
  try
    document.LoadFromStream(stream);

    sports := document.ChildNodes.FindNode('sports');

    for i := 0 to sports.ChildNodes.Count - 1 do
    begin
      xmlItem := sports.ChildNodes[i];

      item := TSport.Create;
      item.FId := GetString('id');
      item.FName := GetString('name');
      item.FPlayers := GetInteger('fieldplayers');
      item.FGoalie := GetBoolean('goalie');
      item.FOrigin := GetString('origin');
      item.FDescription := xmlItem.ChildNodes.FindNode('description').Text;

      FItems.Add(item);
    end;
  finally
    document.Free;
    stream.Free;
  end;
end;

procedure TSports.LoadXmlResource(const resourceName: String; const resourceType: String);
begin
  LoadXmlStream(TNtResources.GetResourceStream(PChar(resourceType), PChar(resourceName)));
end;

procedure TSports.LoadXmlFile(const fileName: String);
begin
  LoadXmlStream(TFileStream.Create(fileName, fmOpenRead));
end;


// INI

procedure TSports.LoadIniStream(stream: TStream);
var
  section: String;
  iniFile: TMemIniFile;

  function GetString(const name: String): String;
  begin
    Result := iniFile.ReadString(section, name, '');
  end;

  function GetInteger(const name: String): Integer;
  begin
    Result := StrToIntDef(GetString(name), 0);
  end;

  function GetBoolean(const name: String): Boolean;
  begin
    Result := GetInteger(name) = 1;
  end;

  function GetCodePage: Integer;
  var
    language: String;
  begin
    language := LoadedResourceLocale;

    if language = '' then
      language := 'en';

    Result := TNtLocale.LocaleToCodePage(TNtLocale.ExtensionToLocale(language));
  end;

var
  i: Integer;
  item: TSport;
  stringList, sections: TStringList;
  encoding: TEncoding;
begin
  ClearItems;

  encoding := TEncoding.GetEncoding(GetCodePage);
  iniFile := TMemIniFile.Create('');
  stringList := TStringList.Create;
  sections := TStringList.Create;

  try
    stringList.LoadFromStream(stream, encoding);
    iniFile.SetStrings(stringList);

    iniFile.ReadSections(sections);

    for i := 0 to sections.Count - 1 do
    begin
      section := sections[i];

      item := TSport.Create;
      item.FId := section;
      item.FName := GetString('name');
      item.FPlayers := GetInteger('fieldplayers');
      item.FGoalie := GetBoolean('goalie');
      item.FOrigin := GetString('origin');
      item.FDescription := GetString('description');

      FItems.Add(item);
    end;
  finally
    sections.Free;
    stringList.Free;
    iniFile.Free;
    stream.Free;
    encoding.Free;
  end;
end;

procedure TSports.LoadIniResource(const resourceName: String; const resourceType: String);
begin
  LoadIniStream(TNtResources.GetResourceStream(PChar(resourceType), PChar(resourceName)));
end;

procedure TSports.LoadIniFile(const fileName: String);
begin
  LoadIniStream(TFileStream.Create(fileName, fmOpenRead));
end;

end.
