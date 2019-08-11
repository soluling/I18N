program CustomDir;

uses
  NtBase,
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  // By default resource DLL directory is the same as the application dir.
  // if you want to use some other directory set the resource DLL directory before calling Initialize or CreateForm.
  TNtBase.SetResourceDllDir('Output', lsSettings);

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
