program IniResources;



{$R 'Custom.res' 'Custom.rc'}

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Data in '..\Data.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
