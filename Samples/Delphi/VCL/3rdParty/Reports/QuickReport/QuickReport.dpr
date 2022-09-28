program QuickReport;

uses
  Vcl.Forms,
  ReportFrm in 'ReportFrm.pas' {ReportForm},
  MainFrm in 'MainFrm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TReportForm, ReportForm);
  Application.Run;
end.
