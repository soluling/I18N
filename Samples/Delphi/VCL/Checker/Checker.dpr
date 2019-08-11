program Checker;

uses
  NtInitialLocale,
  Vcl.Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  SampleFra in 'SampleFra.pas' {Frame1: TFrame},
  SampleDlg in 'SampleDlg.pas' {SampleDialog},
  BaseFrm in 'BaseFrm.pas' {BaseForm},
  SimpleFrm in 'SimpleFrm.pas' {SimpleForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
