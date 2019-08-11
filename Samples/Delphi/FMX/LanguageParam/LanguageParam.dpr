program LanguageParam;

{$R *.dres}

uses
  FMX.Forms,
  FMX.NtTranslator,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

procedure ProcessLanguageParam;
begin
  if ParamCount >= 1 then
    TNtTranslator.SetNew(ParamStr(1))
end;

begin
  ProcessLanguageParam;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
