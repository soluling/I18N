program ActiveLanguage;



{$R *.dres}

uses
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  FMX.NtLanguageDlg in '..\..\..\..\Library\Delphi\FMX.NtLanguageDlg.pas' {NtLanguageDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
