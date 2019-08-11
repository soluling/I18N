program Ordinal;

uses
  NtInitialLocale,
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  NtOrdinalData in '..\..\..\..\Library\Delphi\NtOrdinalData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
