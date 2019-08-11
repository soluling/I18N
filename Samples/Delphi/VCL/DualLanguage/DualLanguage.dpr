program DualLanguage;

uses
  NtInitialLocale,
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  BaseDlg in 'BaseDlg.pas' {BaseDialog},
  MainOptionsDlg in 'MainOptionsDlg.pas' {MainOptionsDialog},
  ClientOptionsDlg in 'ClientOptionsDlg.pas' {ClientOptionsDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMainOptionsDialog, MainOptionsDialog);
  Application.CreateForm(TClientOptionsDialog, ClientOptionsDialog);
  Application.Run;
end.
