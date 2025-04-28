program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  SharedFrm in 'SharedFrm.pas' {SharedForm},
  SharedInheritedFrm in 'SharedInheritedFrm.pas' {SharedInheritedForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
