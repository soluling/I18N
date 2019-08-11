program Number;

uses
  NtInitialLocale,
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  NtNumber in '..\..\..\..\Library\Delphi\NtNumber.pas',
  NtNumberData in '..\..\..\..\Library\Delphi\NtNumberData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
