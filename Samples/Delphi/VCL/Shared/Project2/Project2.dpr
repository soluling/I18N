program Project2;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  SharedFrm in '..\Project1\SharedFrm.pas' {SharedForm},
  SharedInheritedFrm in '..\Project1\SharedInheritedFrm.pas' {SharedInheritedForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
