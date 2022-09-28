{
  @abstract Implements @link(TNtDevExpressChecker) checker extension class that checks DevExpress controls.

  To enable checking just add this unit into your project or add unit into any uses block.

  @longCode(#
implementation

uses
  NtDevExpressChecker;
#)

  See @italic(Samples\Delphi\VCL\DevExpress\Checker) sample to see how to use the unit.
}
unit NtDevExpressChecker;

interface

uses
  Controls,
  NtChecker;

type
  { @abstract Checker extension class that checks some complex DevExpress controls. }
  TNtDevExpressChecker = class(TNtCheckerExtension)
  public
    { @seealso(TNtCheckerExtension.Show) }
    function Show(checker: TFormChecker; control: TControl; var restoreControl: TControl): Boolean; override;

    { @seealso(TNtCheckerExtension.Restore) }
    function Restore(control, restoreControl: TControl): Boolean; override;

    { @seealso(TNtCheckerExtension.Ignore) }
    function Ignore(control: TControl; issueTypes: TFormIssueTypes): Boolean; override;
  end;

implementation

uses
  cxPC;

function TNtDevExpressChecker.Show(checker: TFormChecker; control: TControl; var restoreControl: TControl): Boolean;
var
  tabSheet: TcxTabSheet;
begin
  tabSheet := checker.GetParent(control, TcxTabSheet) as TcxTabSheet;

  if tabSheet <> nil then
  begin
    restoreControl := tabSheet.PageControl.ActivePage;
    tabSheet.PageControl.ActivePage := tabSheet;
    tabSheet.PageControl.Update;
    Result := True;
  end
  else
    Result := False;
end;

function TNtDevExpressChecker.Restore(control, restoreControl: TControl): Boolean;
var
  tabSheet: TcxTabSheet;
begin
  if restoreControl is TcxTabSheet then
  begin
    tabSheet := TcxTabSheet(restoreControl);
    tabSheet.PageControl.ActivePage := tabSheet;
    tabSheet.PageControl.Update;
    Result := True;
  end
  else
    Result := False;
end;

function TNtDevExpressChecker.Ignore(control: TControl; issueTypes: TFormIssueTypes): Boolean;
begin
  Result :=
    (control is TcxTabSheet) and (itOverlap in issueTypes);
end;

initialization
  NtCheckerExtensions.Register(TNtDevExpressChecker);
end.
