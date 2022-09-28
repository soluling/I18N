program PackageFormApplication;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  ApplicationDerivedFor in 'ApplicationDerivedFor.pas' {DerivedForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
