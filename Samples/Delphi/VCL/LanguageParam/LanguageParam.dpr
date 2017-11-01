program LanguageParam;

uses
  Forms,
  NtBase,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

procedure ProcessLanguageParam;
begin
  if ParamCount >= 1 then
    TNtBase.LoadNew(ParamStr(1));
end;

begin
  ProcessLanguageParam;
  
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
