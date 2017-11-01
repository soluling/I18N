unit Data;

interface

uses
  SysUtils,
  Classes;

type
  TResourceItem = class(TObject)
  private
    FResourceType: String;
    FResourceName: String;

    function GetData: TBytes;

  public
    constructor Create(const resourceType, resourceName: String);

    function GetStream: TResourceStream;

    property Data: TBytes read GetData;
    property ResourceType: String read FResourceType write FResourceType;
    property ResourceName: String read FResourceName write FResourceName;
  end;

  TResourceItems = class(TObject)
  private
    FItems: TList;
    FIndex: Integer;

    function GetCount: Integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const resourceType, resourceName: String);

    function Current: TResourceItem;
    function Next: TResourceItem;
    function Previous: TResourceItem;

    property Count: Integer read GetCount;
    property Index: Integer read FIndex;
  end;

implementation

uses
  NtBase;


// TResourceItem

constructor TResourceItem.Create(const resourceType, resourceName: String);
begin
  inherited Create;
  FResourceType := resourceType;
  FResourceName := resourceName;
end;

function TResourceItem.GetData: TBytes;
begin
  Result := TNtResources.LoadResource(PChar(FResourceType), PChar(FResourceName));
end;

function TResourceItem.GetStream: TResourceStream;
begin
  Result := TNtResources.GetResourceStream(PChar(FResourceType), PChar(FResourceName));
end;


// TResourceItems

constructor TResourceItems.Create;
begin
  inherited;
  FItems := TList.Create;
  FIndex := -1;
end;

destructor TResourceItems.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TResourceItems.GetCount: Integer;
begin
  result := FItems.Count;
end;

procedure TResourceItems.Add(const resourceType, resourceName: String);
begin
  FItems.Add(TResourceItem.Create(resourceType, resourceName));

  if FIndex = -1 then
    FIndex := 0;
end;

function TResourceItems.Current: TResourceItem;
begin
  if FIndex >= 0 then
    Result := FItems[FIndex]
  else
    Result := nil;
end;

function TResourceItems.Next: TResourceItem;
begin
  if FIndex < FItems.Count - 1 then
    Inc(FIndex)
  else
    FIndex := 0;

  Result := Current;
end;

function TResourceItems.Previous: TResourceItem;
begin
  if FIndex > 0 then
    Dec(FIndex)
  else
    FIndex := FItems.Count - 1;

  Result := Current;
end;

end.
