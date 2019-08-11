program Project1;

uses
  LaDefaultLocale,  // This is Sisulizer unit initialize the default language
  //NtInitialLocale,  // This is same unit for NT. Uncomment this and comment out the above line when compiling it
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
