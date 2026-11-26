{
  @abstract The part of SoluPick's in-app picking that doesn't depend on VCL or FireMonkey.

  Don't use this unit directly. Add NtInContext (VCL) or FMX.NtInContext (FireMonkey) to the
  in-context build of your application. They register a finder here: a function that tells the
  texts with hidden ids at a point of the screen.

  The protocol, for SoluPick:
  - The unit creates a message-only window of class SolulingInContext.
  - SoluPick sends it WM_COPYDATA with dwData = $534F4C51 ('SOLQ') and a TNtInContextQuery.
  - The finder finds the control at the point (screen pixels) and its texts with hidden ids.
  - The unit sends WM_COPYDATA to Query.ReplyWindow with dwData = $534F4C52 ('SOLR'): a
    TNtInContextReplyHeader, then for each text its length in characters (Integer) and its
    UTF-16 characters. The rectangle is the control's, in screen pixels.
  - The query's result is 1 if a reply was sent, otherwise 0.

  SoluPick's screenshot capture asks for every text of a window at once:
  - SoluPick sends WM_COPYDATA with dwData =  ('SOLE') and a TNtInContextEnumerateQuery.
  - The enumerator finds every visible control of the window that has texts with hidden ids.
  - The unit sends WM_COPYDATA to Query.ReplyWindow with dwData =  ('SOLL'): Version and
    Count (Integers), then for each control its Left, Top, Right, Bottom (screen pixels) and
    TextCount (Integers), and for each text its length in characters (Integer) and its UTF-16
    characters.

  It needs Delphi 2009 or later. With an older Delphi the unit does nothing.
}
unit NtInContextBase;

{$I NtVer.inc}

interface

uses
  Windows, Classes;

const
  NT_INCONTEXT_WINDOW_CLASS = 'SolulingInContext';
  NT_INCONTEXT_QUERY = $534F4C51;  // 'SOLQ'
  NT_INCONTEXT_REPLY = $534F4C52;  // 'SOLR'
  NT_INCONTEXT_ENUMERATE = $534F4C45;  // 'SOLE'
  NT_INCONTEXT_LIST = $534F4C4C;  // 'SOLL'
  NT_INCONTEXT_VERSION = 1;

type
  TNtInContextQuery = packed record
    Version: Integer;
    X: Integer;
    Y: Integer;
    ReplyWindow: Int64;
  end;
  PNtInContextQuery = ^TNtInContextQuery;

  TNtInContextReplyHeader = packed record
    Version: Integer;
    Left: Integer;
    Top: Integer;
    Right: Integer;
    Bottom: Integer;
    Count: Integer;
  end;

  TNtInContextEnumerateQuery = packed record
    Version: Integer;
    Window: Int64;
    ReplyWindow: Int64;
  end;
  PNtInContextEnumerateQuery = ^TNtInContextEnumerateQuery;

  { The controls an enumerator found: each one's rectangle and texts with hidden ids. }
  TNtInContextItems = class
  private
    FRects: array of TRect;
    FTexts: array of TStringList;

    function GetCount: Integer;

  public
    destructor Destroy; override;

    { Adds a control. The texts are copied. }
    procedure Add(const rect: TRect; texts: TStrings);

    property Count: Integer read GetCount;
  end;

  { Finds every visible control of a window that has texts with hidden ids, for a screenshot of
    the window. Called on the main thread. Rectangles in physical screen pixels. }
  TNtInContextEnumerator = procedure(window: HWND; items: TNtInContextItems);

  { Finds the texts with hidden ids at a point of the screen. Called on the main thread.
    @param point  The point in physical screen pixels.
    @param texts  The list where the texts are added, with their hidden ids.
    @param rect   The rectangle of the control whose texts were found, in physical screen pixels.
    @return True if texts were found. }
  TNtInContextFinder = function(const point: TPoint; texts: TStrings; var rect: TRect): Boolean;

{ Registers the finder of the application's framework and creates the window that SoluPick asks.
  Call from the initialization of the framework's unit, on the main thread. }
procedure NtInContextRegister(finder: TNtInContextFinder; enumerator: TNtInContextEnumerator = nil);

{ True if the text has a character that a hidden id can be made of. }
function NtInContextHasHiddenId(const value: String): Boolean;

{ Adds the texts of the string properties Caption, Text and Hint of an object that have a hidden
  id. Without the hint for a screenshot, which shows only the visible texts. }
procedure NtInContextAddTexts(obj: TObject; texts: TStrings; includeHint: Boolean = True);

implementation

{$IFDEF UNICODE}
uses
  Messages, SysUtils, TypInfo;

const
  HWND_MESSAGE_ = HWND(-3);
  REPLY_TIMEOUT = 1000;

  // Properties whose texts are read
  TEXT_PROPERTIES: array[0..2] of String = ('Caption', 'Text', 'Hint');

var
  InContextWindow: HWND;
  Finder: TNtInContextFinder;
  Enumerator: TNtInContextEnumerator;

// Declared here because its last parameter differs between Delphi versions
function SendMessageTimeoutW_(window: HWND; msg: UINT; wp: WPARAM; lp: LPARAM; flags, timeout: UINT; result: Pointer): LRESULT; stdcall;
  external user32 name 'SendMessageTimeoutW';

function NtInContextHasHiddenId(const value: String): Boolean;
var
  i: Integer;
begin
  for i := 1 to Length(value) do
    case Ord(value[i]) of
      $200B..$200E, $202A, $202C, $202D, $2060, $2062, $2063, $FEFF:
      begin
        Result := True;
        Exit;
      end;
    end;

  Result := False;
end;

procedure NtInContextAddTexts(obj: TObject; texts: TStrings; includeHint: Boolean);
var
  i: Integer;
  info: PPropInfo;
  value: String;
begin
  for i := Low(TEXT_PROPERTIES) to High(TEXT_PROPERTIES) do
  begin
    if not includeHint and (TEXT_PROPERTIES[i] = 'Hint') then
      Continue;

    info := GetPropInfo(obj, TEXT_PROPERTIES[i]);

    if (info = nil) or not (info^.PropType^.Kind in [tkString, tkLString, tkWString, tkUString]) then
      Continue;

    value := GetStrProp(obj, info);

    if NtInContextHasHiddenId(value) and (texts.IndexOf(value) = -1) then
      texts.Add(value);
  end;
end;

function SendReply(replyWindow: HWND; const rect: TRect; texts: TStrings): Boolean;
var
  header: TNtInContextReplyHeader;
  stream: TMemoryStream;
  data: TCopyDataStruct;
  i, len: Integer;
  answer: NativeUInt;
begin
  header.Version := NT_INCONTEXT_VERSION;
  header.Left := rect.Left;
  header.Top := rect.Top;
  header.Right := rect.Right;
  header.Bottom := rect.Bottom;
  header.Count := texts.Count;

  stream := TMemoryStream.Create;
  try
    stream.WriteBuffer(header, SizeOf(header));

    for i := 0 to texts.Count - 1 do
    begin
      len := Length(texts[i]);
      stream.WriteBuffer(len, SizeOf(len));

      if len > 0 then
        stream.WriteBuffer(PChar(texts[i])^, len*SizeOf(Char));
    end;

    data.dwData := NT_INCONTEXT_REPLY;
    data.cbData := stream.Size;
    data.lpData := stream.Memory;

    // SoluPick's thread waits for the query, so it takes the reply at once
    Result := SendMessageTimeoutW_(replyWindow, WM_COPYDATA, WPARAM(InContextWindow), LPARAM(@data), SMTO_ABORTIFHUNG, REPLY_TIMEOUT, @answer) <> 0;
  finally
    stream.Free;
  end;
end;

function ProcessQuery(const query: TNtInContextQuery): Boolean;
var
  texts: TStringList;
  point: TPoint;
  rect: TRect;
begin
  Result := False;

  if (query.Version < 1) or not Assigned(Finder) then
    Exit;

  point.X := query.X;
  point.Y := query.Y;

  texts := TStringList.Create;
  try
    texts.CaseSensitive := True;

    if Finder(point, texts, rect) and (texts.Count > 0) then
      Result := SendReply(HWND(query.ReplyWindow), rect, texts);
  finally
    texts.Free;
  end;
end;

// TNtInContextItems

destructor TNtInContextItems.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FTexts) - 1 do
    FTexts[i].Free;

  inherited;
end;

function TNtInContextItems.GetCount: Integer;
begin
  Result := Length(FRects);
end;

procedure TNtInContextItems.Add(const rect: TRect; texts: TStrings);
var
  i: Integer;
begin
  i := Length(FRects);
  SetLength(FRects, i + 1);
  SetLength(FTexts, i + 1);
  FRects[i] := rect;
  FTexts[i] := TStringList.Create;
  FTexts[i].Assign(texts);
end;

procedure WriteInteger(stream: TStream; value: Integer);
begin
  stream.WriteBuffer(value, SizeOf(value));
end;

procedure WriteText(stream: TStream; const value: String);
begin
  WriteInteger(stream, Length(value));

  if value <> '' then
    stream.WriteBuffer(PChar(value)^, Length(value)*SizeOf(Char));
end;

function ProcessEnumerate(const query: TNtInContextEnumerateQuery): Boolean;
var
  items: TNtInContextItems;
  stream: TMemoryStream;
  data: TCopyDataStruct;
  i, j: Integer;
  answer: NativeUInt;
begin
  Result := False;

  if (query.Version < 1) or not Assigned(Enumerator) then
    Exit;

  items := TNtInContextItems.Create;
  stream := TMemoryStream.Create;
  try
    Enumerator(HWND(query.Window), items);

    WriteInteger(stream, NT_INCONTEXT_VERSION);
    WriteInteger(stream, items.Count);

    for i := 0 to items.Count - 1 do
    begin
      WriteInteger(stream, items.FRects[i].Left);
      WriteInteger(stream, items.FRects[i].Top);
      WriteInteger(stream, items.FRects[i].Right);
      WriteInteger(stream, items.FRects[i].Bottom);
      WriteInteger(stream, items.FTexts[i].Count);

      for j := 0 to items.FTexts[i].Count - 1 do
        WriteText(stream, items.FTexts[i][j]);
    end;

    data.dwData := NT_INCONTEXT_LIST;
    data.cbData := stream.Size;
    data.lpData := stream.Memory;

    Result := SendMessageTimeoutW_(HWND(query.ReplyWindow), WM_COPYDATA, WPARAM(InContextWindow), LPARAM(@data), SMTO_ABORTIFHUNG, REPLY_TIMEOUT, @answer) <> 0;
  finally
    stream.Free;
    items.Free;
  end;
end;

function InContextWindowProc(window: HWND; msg: UINT; wp: WPARAM; lp: LPARAM): LRESULT; stdcall;
var
  data: PCopyDataStruct;
begin
  if msg = WM_COPYDATA then
  begin
    data := PCopyDataStruct(lp);

    if (data <> nil) and (data^.dwData = NT_INCONTEXT_QUERY) and (data^.cbData >= SizeOf(TNtInContextQuery)) then
    begin
      try
        Result := Ord(ProcessQuery(PNtInContextQuery(data^.lpData)^));
      except
        // Never let an exception out of a window procedure
        Result := 0;
      end;

      Exit;
    end;

    if (data <> nil) and (data^.dwData = NT_INCONTEXT_ENUMERATE) and (data^.cbData >= SizeOf(TNtInContextEnumerateQuery)) then
    begin
      try
        Result := Ord(ProcessEnumerate(PNtInContextEnumerateQuery(data^.lpData)^));
      except
        Result := 0;
      end;

      Exit;
    end;
  end;

  Result := DefWindowProc(window, msg, wp, lp);
end;

procedure NtInContextRegister(finder: TNtInContextFinder; enumerator: TNtInContextEnumerator);
var
  windowClass: TWndClass;
begin
  NtInContextBase.Finder := finder;
  NtInContextBase.Enumerator := enumerator;

  if InContextWindow <> 0 then
    Exit;

  FillChar(windowClass, SizeOf(windowClass), 0);
  windowClass.lpfnWndProc := @InContextWindowProc;
  windowClass.hInstance := HInstance;
  windowClass.lpszClassName := NT_INCONTEXT_WINDOW_CLASS;
  Windows.RegisterClass(windowClass);

  InContextWindow := CreateWindowEx(0, NT_INCONTEXT_WINDOW_CLASS, '', 0, 0, 0, 0, 0, HWND_MESSAGE_, 0, HInstance, nil);
end;

initialization
finalization
  if InContextWindow <> 0 then
    DestroyWindow(InContextWindow);
{$ELSE}
destructor TNtInContextItems.Destroy;
begin
  inherited;
end;

function TNtInContextItems.GetCount: Integer;
begin
  Result := 0;
end;

procedure TNtInContextItems.Add(const rect: TRect; texts: TStrings);
begin
end;

procedure NtInContextRegister(finder: TNtInContextFinder; enumerator: TNtInContextEnumerator);
begin
end;

function NtInContextHasHiddenId(const value: String): Boolean;
begin
  Result := False;
end;

procedure NtInContextAddTexts(obj: TObject; texts: TStrings; includeHint: Boolean);
begin
end;
{$ENDIF}
end.
