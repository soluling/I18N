{
  @abstract Implements @link(TNtRaizeChecker) checker extension class that checks some complex Raize controls.

  To enable checking just add this unit into your project or add unit into any uses block.

  @longCode(#
implementation

uses
  NtRaizeChecker;
#)

  See @italic(Samples\Delphi\VCL\Raize\Checker) sample to see how to use the unit.
}
unit NtRaizeChecker;

interface

uses
  Controls,
  NtChecker;

type
  { @abstract Checker extension class that checks some complex Raize controls. }
  TNtRaizeChecker = class(TNtCheckerExtension)
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
  RzGroupBar,
  RzTabs;

function TNtRaizeChecker.Show(checker: TFormChecker; control: TControl; var restoreControl: TControl): Boolean;
var
  tabSheet: TRzTabSheet;
begin
  tabSheet := checker.GetParent(control, TRzTabSheet) as TRzTabSheet;

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

function TNtRaizeChecker.Restore(control, restoreControl: TControl): Boolean;
var
  tabSheet: TRzTabSheet;
begin
  if restoreControl is TRzTabSheet then
  begin
    tabSheet := TRzTabSheet(restoreControl);
    tabSheet.PageControl.ActivePage := tabSheet;
    tabSheet.PageControl.Update;
    Result := True;
  end
  else
    Result := False;
end;

function TNtRaizeChecker.Ignore(control: TControl; issueTypes: TFormIssueTypes): Boolean;
begin
  Result :=
    (control is TRzTabSheet) and (itOverlap in issueTypes) or
    (control is TRzGroupBar);
end;

initialization
  NtCheckerExtensions.Register(TNtRaizeChecker);
end.
