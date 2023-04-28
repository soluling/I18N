program DatabaseGrid;

uses
  NtInitialLocale,
  Forms,
  Unit1 in 'Unit1.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
