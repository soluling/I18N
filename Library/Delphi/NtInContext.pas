{
  @abstract Lets SoluPick pick the texts of VCL controls that Windows can't see, such as labels.

  SoluPick, Soluling's picker for in-context translation of Windows applications, reads the texts
  of an application through Windows UI Automation. VCL controls that have no window handle, such
  as TLabel, TSpeedButton and other TGraphicControl descendants, are not visible there. This unit
  answers SoluPick's question "what text is at this point" from inside the application.

  Add the unit to the in-context build of your application. It does not expose any functions.

  @longCode(#
uses
  NtInContext;
#)

  The unit answers only when the texts at the point contain hidden ids, so it does nothing in a
  normal build. The control at the point is tried first, then its parents, a few levels up, but
  not the form: its caption and menus are visible to UI Automation. It needs Delphi 2009 or
  later. With an older Delphi the unit does nothing.

  For FireMonkey use FMX.NtInContext. NtInContextBase has the protocol.
}
unit NtInContext;

{$I NtVer.inc}

interface

implementation

{$IFDEF UNICODE}
uses
  Windows, Classes, Controls, Forms, NtInContextBase;

const
  MAX_PARENT_LEVELS = 5;

type
  TConvertPoint = function(window: HWND; var point: TPoint): BOOL; stdcall;
  TWindowFromPhysicalPoint = function(point: TPoint): HWND; stdcall;

var
  // Windows 8.1 and later. Loaded dynamically to work with every Delphi version.
  PhysicalToLogicalPoint: TConvertPoint;
  LogicalToPhysicalPoint: TConvertPoint;
  WindowFromPhysicalPointFunc: TWindowFromPhysicalPoint;

// The deepest control at a screen point, including the controls that have no window handle
function FindControlAt(const point: TPoint): TControl;
var
  child: TControl;
begin
  Result := FindVCLWindow(point);

  while Result is TWinControl do
  begin
    child := TWinControl(Result).ControlAtPos(TWinControl(Result).ScreenToClient(point), True, True, True);

    if (child = nil) or (child = Result) then
      Break;

    Result := child;
  end;
end;

// Only changes the point if the application isn't per-monitor DPI aware
function ToLogical(const point: TPoint): TPoint;
var
  window: HWND;
begin
  Result := point;

  if Assigned(WindowFromPhysicalPointFunc) then
    window := WindowFromPhysicalPointFunc(point)
  else
    window := WindowFromPoint(point);

  if (window <> 0) and Assigned(PhysicalToLogicalPoint) then
    PhysicalToLogicalPoint(window, Result);
end;

function ToPhysical(window: HWND; const point: TPoint): TPoint;
begin
  Result := point;

  if (window <> 0) and Assigned(LogicalToPhysicalPoint) then
    LogicalToPhysicalPoint(window, Result);
end;

function GetScreenRect(control: TControl): TRect;
var
  window: HWND;
begin
  Result.TopLeft := control.ClientOrigin;
  Result.Right := Result.Left + control.Width;
  Result.Bottom := Result.Top + control.Height;

  if control is TWinControl then
    window := TWinControl(control).Handle
  else if control.Parent <> nil then
    window := control.Parent.Handle
  else
    window := 0;

  Result.TopLeft := ToPhysical(window, Result.TopLeft);
  Result.BottomRight := ToPhysical(window, Result.BottomRight);
end;

function FindTexts(const point: TPoint; texts: TStrings; var rect: TRect): Boolean;
var
  control: TControl;
  level: Integer;
begin
  control := FindControlAt(ToLogical(point));
  level := 0;

  // Not a form: its caption, menu and the rest of its window are visible to UI Automation, and
  // answering with the caption would hide its menu items from SoluPick
  while (control <> nil) and not (control is TCustomForm) and (level <= MAX_PARENT_LEVELS) do
  begin
    NtInContextAddTexts(control, texts);

    if texts.Count > 0 then
    begin
      rect := GetScreenRect(control);
      Result := True;
      Exit;
    end;

    control := control.Parent;
    Inc(level);
  end;

  Result := False;
end;

// Every visible control of a form that has visible texts with hidden ids, for a screenshot of the
// form. Not the form itself: its caption is visible to UI Automation.
procedure EnumerateTexts(window: HWND; items: TNtInContextItems);
var
  texts: TStringList;

  procedure Walk(control: TControl);
  var
    i: Integer;
  begin
    if not control.Visible then
      Exit;

    if not (control is TCustomForm) then
    begin
      texts.Clear;
      NtInContextAddTexts(control, texts, False);

      if texts.Count > 0 then
        items.Add(GetScreenRect(control), texts);
    end;

    if control is TWinControl then
      for i := 0 to TWinControl(control).ControlCount - 1 do
        Walk(TWinControl(control).Controls[i]);
  end;

var
  control: TWinControl;
begin
  control := FindControl(window);

  if (control = nil) or not control.Showing then
    Exit;

  texts := TStringList.Create;
  try
    texts.CaseSensitive := True;
    Walk(control);
  finally
    texts.Free;
  end;
end;

procedure Initialize;
var
  module: HMODULE;
begin
  module := GetModuleHandle(user32);
  @PhysicalToLogicalPoint := GetProcAddress(module, 'PhysicalToLogicalPointForPerMonitorDPI');
  @LogicalToPhysicalPoint := GetProcAddress(module, 'LogicalToPhysicalPointForPerMonitorDPI');
  @WindowFromPhysicalPointFunc := GetProcAddress(module, 'WindowFromPhysicalPoint');

  // On the main thread, so the queries are handled there, where the VCL may be used
  NtInContextRegister(FindTexts, EnumerateTexts);
end;

initialization
  Initialize;
{$ENDIF}
end.
