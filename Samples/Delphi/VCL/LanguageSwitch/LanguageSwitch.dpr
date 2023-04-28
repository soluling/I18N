program LanguageSwitch;

uses
  NtInitialLocale,
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  NtTranslatorEx in '..\..\..\..\Library\Delphi\NtTranslatorEx.pas',
  NtResourceEx in '..\..\..\..\Library\Delphi\NtResourceEx.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
