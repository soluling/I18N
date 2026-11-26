{
  @abstract Lets SoluPick pick the texts of FireMonkey controls.

  SoluPick, Soluling's picker for in-context translation of Windows applications, reads the texts
  of an application through Windows UI Automation. FireMonkey draws its controls itself, so
  Windows sees only the window. This unit answers SoluPick's question "what text is at this point"
  from inside the application.

  Add the unit to the in-context build of your application. It does not expose any functions.

  @longCode(#
uses
  FMX.NtInContext;
#)

  The unit answers only when the texts at the point contain hidden ids, so it does nothing in a
  normal build. The topmost visible control at the point is tried first, then its parents, a few
  levels up. It works on Windows only, which is where SoluPick runs.

  For VCL use NtInContext. NtInContextBase has the protocol.
}
unit FMX.NtInContext;

interface

implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows,
  System.Types,
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Platform.Win,
  NtInContextBase;

const
  MAX_PARENT_LEVELS = 5;

// The topmost visible control under a point of the form, in form coordinates. Not
// ObjectAtPoint: it skips controls whose HitTest is off, which is the default of TLabel.
function FindControlAt(parent: TFmxObject; const point: TPointF): TControl;
var
  i: Integer;
  child: TFmxObject;
  control: TControl;
begin
  Result := nil;

  // The last child is drawn on top
  for i := parent.ChildrenCount - 1 downto 0 do
  begin
    child := parent.Children[i];

    if not (child is TControl) then
      Continue;

    control := TControl(child);

    if not control.Visible or not control.AbsoluteRect.Contains(point) then
      Continue;

    Result := FindControlAt(control, point);

    if Result = nil then
      Result := control;

    Exit;
  end;
end;

function FindTexts(const point: TPoint; texts: TStrings; var rect: TRect): Boolean;
var
  window: HWND;
  form: TCommonCustomForm;
  clientPoint: TPoint;
  scale: Single;
  control: TControl;
  obj: TFmxObject;
  bounds: TRectF;
  topLeft, bottomRight: TPoint;
  level: Integer;
begin
  Result := False;
  window := WindowFromPoint(point);

  if window = 0 then
    Exit;

  // The form under the point: also a popup, such as a combo box's list, which is a form of its own
  form := FMX.Platform.Win.FindWindow(GetAncestor(window, GA_ROOT));

  if form = nil then
    Exit;

  window := FormToHWND(form);

  // Physical pixels to the form's own units
  scale := form.Handle.Scale;

  if scale <= 0 then
    scale := 1;

  clientPoint := point;
  Winapi.Windows.ScreenToClient(window, clientPoint);

  control := FindControlAt(form, TPointF.Create(clientPoint.X/scale, clientPoint.Y/scale));
  obj := control;
  level := 0;

  // Up to the form, but not the form: its caption is visible to UI Automation. A style object,
  // such as the TText that shows a label's text, has the same text as its control.
  while (obj is TControl) and (level <= MAX_PARENT_LEVELS) do
  begin
    NtInContextAddTexts(obj, texts);

    if texts.Count > 0 then
    begin
      bounds := TControl(obj).AbsoluteRect;
      topLeft := TPoint.Create(Round(bounds.Left*scale), Round(bounds.Top*scale));
      bottomRight := TPoint.Create(Round(bounds.Right*scale), Round(bounds.Bottom*scale));
      Winapi.Windows.ClientToScreen(window, topLeft);
      Winapi.Windows.ClientToScreen(window, bottomRight);
      rect := TRect.Create(topLeft, bottomRight);
      Result := True;
      Exit;
    end;

    obj := obj.Parent;
    Inc(level);
  end;
end;

// Every visible control of a form that has visible texts with hidden ids, for a screenshot of the
// form. A style object that shows its control's text (a TText in a TLabel's style) has the same
// text, so only the outermost control with a given text is kept.
procedure EnumerateTexts(window: HWND; items: TNtInContextItems);
var
  form: TCommonCustomForm;
  scale: Single;
  texts: TStringList;

  function ScreenRect(control: TControl): TRect;
  var
    bounds: TRectF;
    topLeft, bottomRight: TPoint;
  begin
    bounds := control.AbsoluteRect;
    topLeft := TPoint.Create(Round(bounds.Left*scale), Round(bounds.Top*scale));
    bottomRight := TPoint.Create(Round(bounds.Right*scale), Round(bounds.Bottom*scale));
    Winapi.Windows.ClientToScreen(window, topLeft);
    Winapi.Windows.ClientToScreen(window, bottomRight);
    Result := TRect.Create(topLeft, bottomRight);
  end;

  procedure Walk(parent: TFmxObject; const shown: TStrings);
  var
    i, j: Integer;
    child: TFmxObject;
    control: TControl;
    found, newTexts: TStringList;
  begin
    for i := 0 to parent.ChildrenCount - 1 do
    begin
      child := parent.Children[i];

      if not (child is TControl) then
        Continue;

      control := TControl(child);

      if not control.Visible then
        Continue;

      texts.Clear;
      NtInContextAddTexts(control, texts, False);

      found := TStringList.Create;
      newTexts := TStringList.Create;
      try
        found.CaseSensitive := True;
        found.Assign(shown);

        // Only the texts a parent didn't already show
        for j := 0 to texts.Count - 1 do
          if found.IndexOf(texts[j]) < 0 then
          begin
            newTexts.Add(texts[j]);
            found.Add(texts[j]);
          end;

        if newTexts.Count > 0 then
          items.Add(ScreenRect(control), newTexts);

        Walk(control, found);
      finally
        newTexts.Free;
        found.Free;
      end;
    end;
  end;
var
  shown: TStringList;
begin
  form := FMX.Platform.Win.FindWindow(window);

  if form = nil then
    Exit;

  window := FormToHWND(form);
  scale := form.Handle.Scale;

  if scale <= 0 then
    scale := 1;

  texts := TStringList.Create;
  shown := TStringList.Create;
  try
    texts.CaseSensitive := True;
    Walk(form, shown);
  finally
    shown.Free;
    texts.Free;
  end;
end;

initialization
  // On the main thread, so the queries are handled there, where FireMonkey may be used
  NtInContextRegister(FindTexts, EnumerateTexts);
{$ENDIF}
end.
