{
  @abstract Implements @link(TNtTmsChecker) checker extension class that checks some comples TMS controls.

  To enable checking just add this unit into your project or add unit into any uses block.

  @longCode(#
implementation

uses
  NtTmsChecker;
#)

  See @italic(Samples\Delphi\VCL\TMS\Checker) sample to see how to use the unit.
}
unit NtTmsChecker;

interface

uses
  Controls,
  NtChecker;

type
  { @abstract Checker extension class that checks some complex TMS controls. }
  TNtTmsChecker = class(TNtCheckerExtension)
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
  AdvNavBar,
  AdvPageControl;

function TNtTmsChecker.Show(checker: TFormChecker; control: TControl; var restoreControl: TControl): Boolean;
var
  tabSheet: TAdvTabSheet;
  navBarPanel: TAdvNavBarPanel;
begin
  tabSheet := checker.GetParent(control, TAdvTabSheet) as TAdvTabSheet;

  if tabSheet <> nil then
  begin
    restoreControl := tabSheet.AdvPageControl.ActivePage;
    tabSheet.AdvPageControl.ActivePage := tabSheet;
    tabSheet.AdvPageControl.Update;
    Result := True;
    Exit;
  end;

  navBarPanel := checker.GetParent(control, TAdvNavBarPanel) as TAdvNavBarPanel;

  if navBarPanel <> nil then
  begin
    restoreControl := navBarPanel.AdvNavBar.ActivePanel;
    navBarPanel.AdvNavBar.ActivePanel := navBarPanel;
    navBarPanel.AdvNavBar.Update;
    Result := True;
    Exit;
  end;

  Result := False;
end;

function TNtTmsChecker.Restore(control, restoreControl: TControl): Boolean;
var
  tabSheet: TAdvTabSheet;
  navBarPanel: TAdvNavBarPanel;
begin
  if restoreControl is TAdvTabSheet then
  begin
    tabSheet := TAdvTabSheet(restoreControl);
    tabSheet.AdvPageControl.ActivePage := tabSheet;
    tabSheet.AdvPageControl.Update;
    Result := True;
  end
  else if restoreControl is TAdvNavBarPanel then
  begin
    navBarPanel := TAdvNavBarPanel(restoreControl);
    navBarPanel.AdvNavBar.ActivePanel := navBarPanel;
    navBarPanel.AdvNavBar.Update;
    Result := True;
  end
  else
    Result := False;
end;

function TNtTmsChecker.Ignore(control: TControl; issueTypes: TFormIssueTypes): Boolean;
begin
  Result :=
    (control is TAdvTabSheet) and (itOverlap in issueTypes) or
    (control is TAdvNavBarPanel) and (itOverlap in issueTypes);
end;

initialization
  NtCheckerExtensions.Register(TNtTmsChecker);
end.
