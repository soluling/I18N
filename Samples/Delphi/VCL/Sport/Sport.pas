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

    property Count: Integer read GetCount;
    property Items[i: Integer]: TSport read GetItem; default;
  end;

implementation

uses
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

    Result := String(Utf8String(utf8));
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
  LoadTextStream(TResourceStream.Create(HInstance, resourceName, PChar(resourceType)));
end;

end.
