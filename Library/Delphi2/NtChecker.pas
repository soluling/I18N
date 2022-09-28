{
  @abstract Implements form checker that finds controls that are overlapped or out of bounds, or has text that is truncated.

  The unit contains following classes: @link(TFormIssue), @link(TTruncationIssue),
  @link(TOverlapIssue), @link(TOutOfBoundsIssue) and @link(TFormChecker).
  They are used to check the forms and report issues.

  The class will automatically take screenshots of every issue and writes them
  to the output directory specified by @link(TFormChecker.OutputDir) property.
  If your application uses a localized resource DLL then the output is written
  into a language specific sub directory of @link(TFormChecker.OutputDir).

  For example if you application is C:\Files\Sample\Win32\Debug\Project1.exe then
  the default output directory is C:\Files\Sample\Win32\Debug and the German
  output directory is C:\Files\Sample\Win32\Debug\de.

  The class is available if you use Delphi XE2 or later. If you are using Delphi XE6 or
  later the class also writes a JSON file that contains information about the issues found.

  You can customize how checking is done by using @link(TFormChecker.OnControl) and
  @link(TFormChecker.OnIssue) events.

  See @italic(Samples\Delphi\VCL\Checker), @italic(Samples\Delphi\VCL\Raize\Checker),
  @italic(Samples\Delphi\VCL\TMS\Checker) and @italic(Samples\Delphi\VCL\DevExpress\Checker)
  samples to see how to use this unit and the extension units.
}

unit NtChecker;

{$I NtVer.inc}

interface

uses
  Windows,
  Classes,
  Graphics,
  Controls,
  ComCtrls,
  StdCtrls,
  Forms,
  ExtCtrls,
{$IFDEF DELPHIXE6}
  Json,
{$ENDIF}
  PngImage,
  NtBase;

type
  { Form issue type. }
  TFormIssueType =
  (
    itTruncation,  //< Part of the text is truncated.
    itOverlap,     //< Two or more controls overlap.
    itOutOfBounds  //< Part of the control is out ot parent pounds.
  );

  TFormIssueTypes = set of TFormIssueType;

  TFormChecker = class;

  TFormIssueClass = class of TFormIssue;

  { @abstract Specifies an abstract issue class. }
  TFormIssue = class(TObject)
  private
    FChecker: TFormChecker;
    FControl: TControl;
    FForm: TWinControl;
    FScreenshot: TPngImage;

    function GetScreenshotFileName: String;

  protected
    function GetIssueType: TFormIssueType; virtual; abstract;
    function GetName: String; virtual; abstract;

{$IFDEF DELPHIXE6}
    procedure WriteJson(json: TJSONObject); virtual;
{$ENDIF}

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure TakeScreenshot(highlightRect: TRect);
    procedure SaveScreenshot;
    procedure RemoveScreenshot;

    function ShouldWriteIssue: Boolean;

    property IssueType: TFormIssueType read GetIssueType;            //< Issue type.
    property Control: TControl read FControl write FControl;         //< Control where the issues happens.
    property Form: TWinControl read FForm write FForm;               //< Form where the issues happens.
    property Name: String read GetName;                              //< Issue type name.
    property Screenshot: TPngImage read FScreenshot;                 //< Screenshot about the issue.
    property ScreenshotFileName: String read GetScreenshotFileName;  //< Screenshot file name.
  end;

  { @abstract Truncation issue that is used when the text is not completely shown on the control. }
  TTruncationIssue = class(TFormIssue)
  private
    FTextWidth: Integer;

  protected
    function GetIssueType: TFormIssueType; override;
    function GetName: String; override;

  public
    { The text width in pixels. }
    property TextWidth: Integer read FTextWidth write FTextWidth;
  end;

  { @abstract Specifies an overlap issue. }
  TOverlapIssue = class(TFormIssue)
  private
    FControls: TList;

    function GetCount: Integer;
    function GetControl(i: Integer): TControl;

  protected
    function GetIssueType: TFormIssueType; override;
    function GetName: String; override;

{$IFDEF DELPHIXE6}
    procedure WriteJson(json: TJSONObject); override;
{$ENDIF}

  public
    constructor Create; override;
    destructor Destroy; override;

    { Add an overlapped control.
      @param value A control that overlaps the control. }
    procedure Add(value: TControl);

    function Exists(control: TControl): Boolean;

    property Count: Integer read GetCount;                             //< Amount of overlapped controls.
    property Controls[i: Integer]: TControl read GetControl; default;  //< Overlapped controls.
  end;

  { @abstract Specifies a truncation issue. }
  TOutOfBoundsIssue = class(TFormIssue)
  protected
    function GetIssueType: TFormIssueType; override;
    function GetName: String; override;
  end;

  { This event is called before checking a control.
    @param checker  Either form or data module where the object belongs to.
    @param control  Component or a sub component that is currenty being translated.
    @param skip     Set to True if you want that the checker skips this control. }
  TFormControlEvent = procedure(checker: TFormChecker; control: TControl; var skip: Boolean) of object;

  { This event is called before storing the issue and writing the screenshot.
    @param checker  Either form or data module where the object belongs to.
    @param issue    Issues object.
    @param ignore   Set to True if you want that the checker ignore this issue. }
  TFormIssueEvent = procedure(checker: TFormChecker; issue: TFormIssue; var ignore: Boolean) of object;

  { Array that specifies the highlight color of each issue type. }
  THighlightColors = array[TFormIssueType] of TColor;

  { @abstract Class that check overlapped controls and truncated text on VCL forms. }
  TFormChecker = class(TObject)
  private
    FIssues: TList;
    FControl: TWinControl;
    FDisabledTypes: TFormIssueTypes;
    FForm: TCustomForm;
    FHighlightColors: THighlightColors;
    FHighlightIssues: Boolean;
    FHighlightMargin: Integer;
    FHighlightWidth: Integer;
    FOutputDir: String;
    FActiveOutputDir: String;
    FScreenshots: Boolean;
    FTimer: TTimer;
{$IFDEF DELPHIXE6}
    FResults: TJSONArray;
    FResultsForm: TJSONObject;
    FResultsIssues: TJSONArray;
    FResultsFileName: String;
{$ENDIF}
    FOnControl: TFormControlEvent;
    FOnIssue: TFormIssueEvent;

    function GetCharset: TFontCharSet;
    function GetCount: Integer;
    function GetIssue(i: Integer): TFormIssue;

    function GetScreenshotControl(control: TControl): TWinControl;

    function AllocIssue(
      issueClass: TFormIssueClass;
      control: TControl;
      hightlightRect: TRect): TFormIssue;

    procedure AddIssue(issue: TFormIssue);

    function IsOverlapIssueUnique(issue: TOverlapIssue): Boolean;

    function IsTruncationDisabled(control: TControl): Boolean;

    procedure ProcessTruncation(control: TControl; width: Integer);
    procedure CheckOverlap(control: TControl);
    procedure CheckOutOfBounds(control: TControl);

    procedure CheckLabel(control: TCustomLabel);
    procedure CheckCheckBox(control: TCheckBox);
    procedure CheckRadioButton(control: TRadioButton);
    procedure CheckButton(control: TButton);
    procedure CheckComboBox(control: TCustomComboBox);
    procedure CheckListBox(control: TCustomListBox);
    procedure Process(control: TControl);

    function CalculateWidthInPixels(
      canvas: TCanvas;
      const str: String;
      charset: TFontCharSet = 0): Integer;

    procedure ProcessForm(Sender: TObject);

    procedure Open;
    procedure Close;

  public
    constructor Create;
    destructor Destroy; override;

    { Checks a control, form or frame.
      @param control A control that will be checked. }
    procedure Check(control: TWinControl);

    { Call this when language of the applications changes. }
    procedure LanguageChanged;

    { Get the first ancestor (e.g. parent, parent's parent, etc) matching th given type.
      @param control     Control whose ancestor is to be found.
      @param parentClass Type of the ancestor.
      @return Control. }
    function GetParent(control: TControl; parentClass: TControlClass): TControl;

    { Active output directory. If your application use resource DLLs then this is
      a language specific sub directory of @link(TFormChecker.OutputDir). }
    property ActiveOutputDir: String read FActiveOutputDir;

    property Charset: TFontCharSet read GetCharset;                                           //< Default charset.
    property Count: Integer read GetCount;                                                    //< Amount of issues.
    property DisabledTypes: TFormIssueTypes read FDisabledTypes write FDisabledTypes;         //< Issue types that are not checked
    property Issues[i: Integer]: TFormIssue read GetIssue; default;                           //< Issues.
    property HighlightColors: THighlightColors read FHighlightColors write FHighlightColors;  //< Specifies the hightlist colors.
    property HighlightIssues: Boolean read FHighlightIssues write FHighlightIssues;           //< If True each issues is highlighted in the screenshot.
    property HighlightMargin: Integer read FHighlightMargin write FHighlightMargin;           //< Margin between of control(s) and highlight rectangle in pixels.
    property HighlightWidth: Integer read FHighlightWidth write FHighlightWidth;              //< Width of the highlight rectange.
    property OutputDir: String read FOutputDir write FOutputDir;                              //< Directory where screenshots and result file are written.
    property Screenshots: Boolean read FScreenshots write FScreenshots;                       //< If True screenshot fiels are writting into output directory.
    property OnControl: TFormControlEvent read FOnControl write FOnControl;                   //< Event that let you control if a control is checked or not.
    property OnIssue: TFormIssueEvent read FOnIssue write FOnIssue;                           //< Event that let you act each time when an issues is found.
  end;

  { @abstract Class that extends @link(TFormChecker).
    @link(TFormChecker) can check all controls but can not propely take a
    screenshot of controls that belong to a container that is not currently visible.
    The purpose of this class is to provide code to temporary show the hidden panel
    so a proper screenshot can be taken and then to restore the control into its
    original state.
    If you have a 3rd party components that contain multi panel container you may have to
    implement an extension for it. Derive your extension from this class and
    implement @link(TNtCheckerExtension.Show) and @link(TNtCheckerExtension.Restore) functions.
    You may also have to implement @link(TNtCheckerExtension.Ignore) function to
    prevent false issues.
    @seealso(TNtTabControlChecker)
    @seealso(TNtRaizeChecker) }
  TNtCheckerExtension = class(TNtExtension)
  public
    { If the container control of the passed control is hidden and it is a part of multi panel control shows a parent so the control becomes
      visible for screenshot purposes.
      @param checker         Form checker.
      @param control         Control that is has an issues.
      @param restoreControl  Return the panel that was put topmost if any.
      @return @true if a panel was moved topmost, @false if not. }
    function Show(checker: TFormChecker; control: TControl; var restoreControl: TControl): Boolean; virtual;

    { Restores a shown panel into its previous state.
      @param control         Control that is has an issues.
      @param restoreControl  The object to be transalted.
      @return @true if the panel as restored, @false if not. }
    function Restore(control, restoreControl: TControl): Boolean; virtual;

    { Checks if a control checking should be ignored.
      @param control    Control to be checked.
      @param issueTypes Issue type to be checked.
      @return @true if checking should be ignored, @false if control should be checked normally. }
    function Ignore(control: TControl; issueTypes: TFormIssueTypes): Boolean; virtual;
  end;

  { Class type of the checker extension class. }
  TNtCheckerExtensionClass = class of TNtCheckerExtension;

  { @abstract Checker extension class that checks TTabControl. }
  TNtTabControlChecker = class(TNtCheckerExtension)
  public
    { @seealso(TNtCheckerExtension.Show) }
    function Show(checker: TFormChecker; control: TControl; var restoreControl: TControl): Boolean; override;

    { @seealso(TNtCheckerExtension.Restore) }
    function Restore(control, restoreControl: TControl): Boolean; override;

    { @seealso(TNtCheckerExtension.Ignore) }
    function Ignore(control: TControl; issueTypes: TFormIssueTypes): Boolean; override;
  end;

  { @abstract Class that stores installed checker extension.

    If you derive a new extension from @link(TNtCheckerExtension) you have to register
    it in order to take it in use. A good place to register an extension is to
    call the @link(TNtExtensions.Register) in the initialization block of your
    extension class unit.

    @longCode(#
initialization
  NtCheckerExtensions.Register(TYourCheckerExtension);
end.
#) }
  TNtCheckerExtensions = class(TNtExtensions)
  private
    FRestoreControl: TControl;

    function GetItem(i: Integer): TNtCheckerExtension;

    function Show(checker: TFormChecker; control: TControl): Boolean;
    function Restore(control: TControl): Boolean;
    function Ignore(control: TControl; issueTypes: TFormIssueTypes): Boolean;

  public
    { Array of the registered extension. }
    property Items[i: Integer]: TNtCheckerExtension read GetItem; default;
  end;

var
  { Form checker. Use this to check your forms. }
  NtFormChecker: TFormChecker;

  { Form checker extension. Use this to register your custom checker extensions. }
  NtCheckerExtensions: TNtCheckerExtensions;

implementation

uses
  SysUtils,
  IOUtils,
  Dialogs;

const
  DEFAULT_OUTPUT_DIR_C = 'Check';
  DEFAULT_REPORT_FILE_C = 'issues.json';


// TFormIssue

constructor TFormIssue.Create;
begin
  inherited;
  FScreenshot := nil;
end;

destructor TFormIssue.Destroy;
begin
  FScreenshot.Free;
  inherited;
end;

function TFormIssue.GetScreenshotFileName: String;
begin
  Result := FChecker.ActiveOutputDir + Format('\%s_%s_%s.png', [Form.Name, Control.Name, Name]);
end;

procedure TFormIssue.SaveScreenshot;
begin
  Screenshot.SaveToFile(ScreenshotFileName);
end;

procedure TFormIssue.RemoveScreenshot;
begin
  FreeAndNil(FScreenshot);
end;

function TFormIssue.ShouldWriteIssue: Boolean;
var
  ignore: Boolean;
begin
  if Assigned(FChecker.OnIssue) then
  begin
    ignore := False;
    FChecker.OnIssue(FChecker, Self, ignore);
    Result := not ignore;
  end
  else
    Result := True;
end;

procedure TFormIssue.TakeScreenshot(highlightRect: TRect);
var
  bitmap: TBitmap;
  dc: HDC;
begin
  bitmap := TBitmap.Create;
  try
    bitmap.Width := form.Width;
    bitmap.Height := form.Height;

    dc := GetWindowDC(FForm.Handle);
    try
      BitBlt(bitmap.Canvas.Handle, 0, 0, form.Width, form.Height, dc, 0, 0, SRCCOPY);
    finally
      ReleaseDC(form.Handle, dc);
    end;

    FScreenshot.Free;
    FScreenshot := TPngImage.Create;
    FScreenshot.Assign(bitmap);

    if FChecker.HighlightIssues then
    begin
      FScreenshot.Canvas.Brush.Style := bsClear;
      FScreenshot.Canvas.Pen.Color := FChecker.HighlightColors[IssueType];
      FScreenshot.Canvas.Pen.Width := FChecker.HighlightWidth;
      FScreenshot.Canvas.Rectangle(highlightRect);
    end;
  finally
    bitmap.Free;
  end;
end;

{$IFDEF DELPHIXE6}
procedure TFormIssue.WriteJson(json: TJSONObject);
begin
end;
{$ENDIF}


// TTruncationIssue

function TTruncationIssue.GetIssueType: TFormIssueType;
begin
  Result := itTruncation;
end;

function TTruncationIssue.GetName: String;
begin
  Result := 'truncation';
end;


// TOverlapIssue

constructor TOverlapIssue.Create;
begin
  inherited;
  FControls := TList.Create;
end;

destructor TOverlapIssue.Destroy;
begin
  FControls.Free;
  inherited;
end;

function TOverlapIssue.GetIssueType: TFormIssueType;
begin
  Result := itOverlap;
end;

function TOverlapIssue.GetName: String;
begin
  Result := 'overlap';
end;

function TOverlapIssue.GetCount: Integer;
begin
  Result := FControls.Count;
end;

function TOverlapIssue.GetControl(i: Integer): TControl;
begin
  Result := FControls[i];
end;

procedure TOverlapIssue.Add(value: TControl);
begin
  FControls.Add(value);
end;

function TOverlapIssue.Exists(control: TControl): Boolean;
begin
  Result := (control = FControl) or (FControls.IndexOf(control) >= 0);
end;

{$IFDEF DELPHIXE6}
procedure TOverlapIssue.WriteJson(json: TJSONObject);
var
  i: Integer;
  jsonControls: TJSONArray;
begin
  jsonControls := TJSONArray.Create;

  for i := 0 to Count - 1 do
    jsonControls.Add(Controls[i].Name);

  json.AddPair(TJSONPair.Create('controls', jsonControls));
end;
{$ENDIF}


// TOutOfBoundsIssue

function TOutOfBoundsIssue.GetIssueType: TFormIssueType;
begin
  Result := itOutOfBounds;
end;

function TOutOfBoundsIssue.GetName: String;
begin
  Result := 'outofbounds';
end;


// TFormChecker

constructor TFormChecker.Create;
begin
  inherited;

  FScreenshots := True;

  FHighlightColors[itOverlap] := clRed;
  FHighlightColors[itOutOfBounds] := clNavy;
  FHighlightColors[itTruncation] := clGreen;

  FHighlightIssues := True;
  FHighlightMargin := 3;
  FHighlightWidth := 2;

  FIssues := TList.Create;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 500;
  FTimer.Enabled := False;
  FTimer.OnTimer := ProcessForm;

  Open;
end;

destructor TFormChecker.Destroy;
var
  i: Integer;
begin
  Close;

  // Free issues
  for i := 0 to FIssues.Count - 1 do
    TFormIssue(FIssues[i]).Free;

  FIssues.Free;

  // Free other
  FTimer.Free;

  inherited;
end;

procedure TFormChecker.Open;
var
  dir: String;
begin
  if OutputDir = '' then
    FOutputDir := ExtractFilePath(Application.ExeName) + DEFAULT_OUTPUT_DIR_C;

  CreateDir(FOutputDir);

  dir := FOutputDir;

  if LoadedResourceLocale <> '' then
    dir := dir + '\' + LoadedResourceLocale;

  FActiveOutputDir := dir;
  CreateDir(FActiveOutputDir);

{$IFDEF DELPHIXE6}
  FResultsFileName := FActiveOutputDir + '\' + DEFAULT_REPORT_FILE_C;
  FResults := TJSONArray.Create;
{$ENDIF}
end;

procedure TFormChecker.Close;
begin
{$IFDEF DELPHIXE6}
  // Writes resource JSON into a file
  TFile.WriteAllText(FResultsFileName, FResults.ToString);

  FreeAndNil(FResults);
{$ENDIF}
end;

function TFormChecker.GetCount: Integer;
begin
  Result := FIssues.Count;
end;

function TFormChecker.GetIssue(i: Integer): TFormIssue;
begin
  Result := FIssues[i];
end;

function TFormChecker.IsOverlapIssueUnique(issue: TOverlapIssue): Boolean;
var
  i, j: Integer;
  match: Boolean;
  thisIssue: TFormIssue;
  overlapIssue: TOverlapIssue;
begin
  for i := 0 to Count - 1 do
  begin
    thisIssue := Issues[i];

    if thisIssue is TOverlapIssue then
    begin
      overlapIssue := TOverlapIssue(thisIssue);

      if overlapIssue.Count = issue.Count then
      begin
        match := overlapIssue.Exists(issue.Control);

        if match then
        begin
          for j := 0 to issue.Count - 1 do
          begin
            if not overlapIssue.Exists(issue[j]) then
            begin
              match := False;
              Break;
            end;
          end;
        end;

        if match then
        begin
          Result := False;
          Exit;
        end;
      end;
    end;
  end;

  Result := True;
end;

function TFormChecker.CalculateWidthInPixels(
  canvas: TCanvas;
  const str: String;
  charset: TFontCharSet): Integer;
var
  rect: TRect;
begin
  if str = '' then
  begin
    Result := 0;
    Exit;
  end;

  if charset <> 0 then
    canvas.Font.Charset := charset;

  rect.Left := 0;
  rect.Top := 0;
  rect.Right := 0;
  rect.Bottom := 0;

  Windows.DrawText(
    canvas.Handle,
    PChar(str),
    Length(str),
    rect,
    DT_LEFT or DT_CALCRECT);

  Result := rect.right - rect.left;
end;

function TFormChecker.GetScreenshotControl(control: TControl): TWinControl;
begin
  Result := control.Parent;

  while not (Result is TCustomForm) do
    Result := Result.Parent;
end;

function TFormChecker.GetParent(control: TControl; parentClass: TControlClass): TControl;
begin
  Result := control.Parent;

  while Result <> nil do
  begin
    if Result is parentClass then
      Exit
    else
      Result := Result.Parent;
  end;

  Result := nil;
end;

function TFormChecker.AllocIssue(
  issueClass: TFormIssueClass;
  control: TControl;
  hightlightRect: TRect): TFormIssue;
var
  borderWidth, headerHeight: Integer;
  width, height: Integer;
  point: TPoint;
begin
  Result := issueClass.Create;
  Result.FChecker := Self;
  Result.Form := GetScreenshotControl(control);
  Result.Control := control;

  // Adjust rect from client coordinates to bitmap coordinates
  width := hightlightRect.Right - hightlightRect.Left;
  height := hightlightRect.Bottom - hightlightRect.Top;
  point.X := hightlightRect.Left;
  point.Y := hightlightRect.Top;

  point := control.Parent.ClientToScreen(point);
  point := Result.Form.ScreenToClient(point);

  borderWidth := (Result.Form.Width - Result.Form.ClientWidth) div 2;
  headerHeight := Result.Form.Height - Result.Form.ClientHeight - 2*borderWidth;

  hightlightRect.Left := point.X + borderWidth - HighlightMargin;
  hightlightRect.Top := point.Y + headerHeight + borderWidth - HighlightMargin;
  hightlightRect.Right := hightlightRect.Left + width + 2*HighlightMargin;
  hightlightRect.Bottom := hightlightRect.Top + height + 2*HighlightMargin;

  // Take screenshot
  NtCheckerExtensions.Show(Self, control);
  Result.TakeScreenshot(hightlightRect);
  NtCheckerExtensions.Restore(control);
end;

procedure TFormChecker.AddIssue(issue: TFormIssue);
{$IFDEF DELPHIXE6}
var
  jsonIssue: TJSONObject;
{$ENDIF}
begin
  issue.SaveScreenshot;
  issue.RemoveScreenshot;
  FIssues.Add(issue);

{$IFDEF DELPHIXE6}
  jsonIssue := TJSONObject.Create;
  jsonIssue.AddPair(TJSONPair.Create('type', issue.Name));
  jsonIssue.AddPair(TJSONPair.Create('control', issue.Control.Name));
  jsonIssue.AddPair(TJSONPair.Create('screenshot', ExtractFileName(issue.ScreenshotFileName)));

  issue.WriteJson(jsonIssue);

  FResultsIssues.AddElement(jsonIssue);
{$ENDIF}
end;

procedure TFormChecker.ProcessTruncation(control: TControl; width: Integer);
var
  issue: TTruncationIssue;
begin
  issue := AllocIssue(TTruncationIssue, control, control.BoundsRect) as TTruncationIssue;
  issue.TextWidth := width;

  if issue.ShouldWriteIssue then
    AddIssue(issue)
  else
    issue.Free;
end;

procedure TFormChecker.CheckOutOfBounds(control: TControl);
var
  issue: TOutOfBoundsIssue;
begin
  if (control.Parent = nil) or (itOverlap in DisabledTypes) or NtCheckerExtensions.Ignore(control, [itOverlap]) then
    Exit;

  if (control.Left + control.Width > control.Parent.ClientWidth) or
    (control.Top + control.Height > control.Parent.ClientHeight) then
  begin
    issue := AllocIssue(TOutOfBoundsIssue, control, control.BoundsRect) as TOutOfBoundsIssue;

    if issue.ShouldWriteIssue then
      AddIssue(issue)
    else
      issue.Free;
  end;
end;

procedure TFormChecker.CheckOverlap(control: TControl);
var
  i: Integer;
  thisControl: TControl;
  controls: TList;
  issue: TOverlapIssue;
  rect, hightlightRect: TRect;
begin
  if (control.Parent = nil) or (itOverlap in DisabledTypes) or NtCheckerExtensions.Ignore(control, [itOverlap]) then
    Exit;

  controls := TList.Create;
  try
    rect := control.BoundsRect;
    rect.Inflate(-1, -1);

    for i := 0 to control.Parent.ControlCount - 1 do
    begin
      thisControl := control.Parent.Controls[i];

      if (thisControl <> control) and rect.IntersectsWith(thisControl.BoundsRect) then
        controls.Add(thisControl);
    end;

    if controls.Count > 0 then
    begin
      hightlightRect := control.BoundsRect;

      for i := 0 to controls.Count - 1 do
        hightlightRect.Union(TControl(controls[i]).BoundsRect);

      issue := AllocIssue(TOverlapIssue, control, hightlightRect) as TOverlapIssue;

      for i := 0 to controls.Count - 1 do
        issue.Add(TControl(controls[i]));

      if IsOverlapIssueUnique(issue) and issue.ShouldWriteIssue then
        AddIssue(issue)
      else
        issue.Free;
    end;
  finally
    controls.Free;
  end;
end;

function TFormChecker.GetCharset: TFontCharSet;
begin
  if FForm <> nil then
    Result := FForm.Font.Charset
  else
    Result := DEFAULT_CHARSET;
end;

function TFormChecker.IsTruncationDisabled(control: TControl): Boolean;
begin
  Result := (itTruncation in DisabledTypes) or NtCheckerExtensions.Ignore(control, [itTruncation]);
end;

procedure TFormChecker.CheckLabel(control: TCustomLabel);

  function GetCharset: TFontCharSet;
  begin
    if control is TLabel then
      Result := TLabel(control).Font.Charset
    else
      Result := DEFAULT_CHARSET;
  end;

var
  width: Integer;
begin
  if IsTruncationDisabled(control) then
    Exit;

  width := CalculateWidthInPixels(control.Canvas, control.Caption, GetCharset);

  if width > control.Width then
    ProcessTruncation(control, width);
end;

procedure TFormChecker.CheckCheckBox(control: TCheckBox);
var
  width: Integer;
begin
  if IsTruncationDisabled(control) then
    Exit;

  width := CalculateWidthInPixels(FForm.Canvas, control.Caption, Charset) + GetSystemMetrics(SM_CXMENUCHECK) + GetSystemMetrics(SM_CXEDGE);

  if (FForm <> nil) and (width > control.Width) then
    ProcessTruncation(control, width);
end;

procedure TFormChecker.CheckRadioButton(control: TRadioButton);
var
  width: Integer;
begin
  if IsTruncationDisabled(control) then
    Exit;

  width := CalculateWidthInPixels(FForm.Canvas, control.Caption, Charset) + GetSystemMetrics(SM_CXMENUCHECK) + GetSystemMetrics(SM_CXEDGE);

  if (FForm <> nil) and (width > control.Width) then
    ProcessTruncation(control, width);
end;

procedure TFormChecker.CheckButton(control: TButton);
var
  width: Integer;
begin
  if IsTruncationDisabled(control) then
    Exit;

  width := CalculateWidthInPixels(FForm.Canvas, control.Caption, Charset) + 4*GetSystemMetrics(SM_CXEDGE);

  if (FForm <> nil) and (width > control.Width) then
    ProcessTruncation(control, width);
end;

procedure TFormChecker.CheckComboBox(control: TCustomComboBox);
var
  i, width: Integer;
begin
  if (FForm = nil) or IsTruncationDisabled(control) then
    Exit;

  for i := 0 to control.Items.Count - 1 do
  begin
    width := CalculateWidthInPixels(FForm.Canvas, control.Items[i], Charset) + GetSystemMetrics(SM_CXVSCROLL) + 2*GetSystemMetrics(SM_CXBORDER);

    if width > control.Width then
    begin
      ProcessTruncation(control, width);
      Break;
    end;
  end;
end;

procedure TFormChecker.CheckListBox(control: TCustomListBox);
var
  i, width: Integer;
begin
  if (FForm = nil) or IsTruncationDisabled(control) then
    Exit;

  for i := 0 to control.Items.Count - 1 do
  begin
    width := CalculateWidthInPixels(FForm.Canvas, control.Items[i], Charset) + 2*GetSystemMetrics(SM_CXBORDER);

    if width > control.Width then
    begin
      ProcessTruncation(control, width);
      Break;
    end;
  end;
end;

procedure TFormChecker.Process(control: TControl);
var
  i: Integer;
  skip: Boolean;
  container: TWinControl;
begin
  // Some control are always ignored
  if NtCheckerExtensions.Ignore(control, []) then
    Exit;

  // OnControl even lets the use to skip checking of any control
  if Assigned(FOnControl) then
  begin
    skip := False;
    FOnControl(Self, control, skip);

    if skip then
      Exit;
  end;

  // Check truncations
  if control is TCustomLabel then
    CheckLabel(TCustomLabel(control))
  else if control is TCheckBox then
    CheckCheckBox(TCheckBox(control))
  else if control is TRadioButton then
    CheckRadioButton(TRadioButton(control))
  else if control is TButton then
    CheckButton(TButton(control))
  else if control is TCustomComboBox then
    CheckComboBox(TCustomComboBox(control))
  else if control is TCustomListBox then
    CheckListBox(TCustomListBox(control));

  // Check if this control overlaps with any other control
  CheckOverlap(control);

  // Check if this control is out of parent's bounds
  CheckOutOfBounds(control);

  // Check the child controls
  if control is TWinControl then
  begin
    container := TWinControl(control);

    for i := 0 to container.ControlCount - 1 do
      Process(container.Controls[i]);
  end;
end;

procedure TFormChecker.ProcessForm(Sender: TObject);
begin
  FTimer.Enabled := False;

{$IFDEF DELPHIXE6}
  FResultsForm := TJSONObject.Create;
  FResultsForm.AddPair(TJSONPair.Create('formname', FControl.Name));
  FResultsForm.AddPair(TJSONPair.Create('formtype', FControl.ClassName));
  FResults.AddElement(FResultsForm);

  FResultsIssues := TJSONArray.Create;
{$ENDIF}

  Process(FControl);

{$IFDEF DELPHIXE6}
  FResultsForm.AddPair('issues', FResultsIssues);
{$ENDIF}
end;

procedure TFormChecker.Check(control: TWinControl);
begin
  // Save the control and enable the timer. Checking will be done in the OnTimer event
  FControl := control;

  if control is TCustomForm then
    FForm := TCustomForm(control)
  else
    FForm := nil;

  FTimer.Enabled := True;
end;

procedure TFormChecker.LanguageChanged;
begin
  Close;
  Open;
end;


// TNtCheckerExtension

function TNtCheckerExtension.Show(checker: TFormChecker; control: TControl; var restoreControl: TControl): Boolean;
begin
  Result := False;
end;

function TNtCheckerExtension.Restore(control, restoreControl: TControl): Boolean;
begin
  Result := False;
end;

function TNtCheckerExtension.Ignore(control: TControl; issueTypes: TFormIssueTypes): Boolean;
begin
  Result := False;
end;


// TNtTabControlChecker

function TNtTabControlChecker.Show(checker: TFormChecker; control: TControl; var restoreControl: TControl): Boolean;
var
  tabSheet: TTabSheet;
begin
  tabSheet := checker.GetParent(control, TTabSheet) as TTabSheet;

  if tabSheet <> nil then
  begin
    restoreControl := tabSheet.PageControl.ActivePage;
    tabSheet.PageControl.ActivePage := tabSheet;
    Result := True;
  end
  else
    Result := False;
end;

function TNtTabControlChecker.Restore(control, restoreControl: TControl): Boolean;
var
  tabSheet: TTabSheet;
begin
  if restoreControl is TTabSheet then
  begin
    tabSheet := TTabSheet(restoreControl);
    tabSheet.PageControl.ActivePage := tabSheet;
    Result := True;
  end
  else
    Result := False;
end;

function TNtTabControlChecker.Ignore(control: TControl; issueTypes: TFormIssueTypes): Boolean;
begin
  Result := (control is TTabSheet) and (itOverlap in issueTypes);
end;


// TNtCheckerExtensions

function TNtCheckerExtensions.GetItem(i: Integer): TNtCheckerExtension;
begin
  Result := inherited Items[i] as TNtCheckerExtension;
end;

function TNtCheckerExtensions.Show(checker: TFormChecker; control: TControl): Boolean;
var
  i: Integer;
  restoreControl: TControl;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].Show(checker, control, restoreControl);

    if Result then
    begin
      FRestoreControl := restoreControl;
      Exit;
    end;
  end;

  Result := False;
end;

function TNtCheckerExtensions.Restore(control: TControl): Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].Restore(control, FRestoreControl);

    if Result then
      Exit;
  end;

  Result := False;
end;

function TNtCheckerExtensions.Ignore(control: TControl; issueTypes: TFormIssueTypes): Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].Ignore(control, issueTypes);

    if Result then
      Exit;
  end;

  Result := False;
end;


initialization
  NtFormChecker := TFormChecker.Create;

  NtCheckerExtensions := TNtCheckerExtensions.Create;
  NtCheckerExtensions.Register(TNtTabControlChecker);
finalization
  NtFormChecker.Free;
end.
