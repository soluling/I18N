program FormsSample;

uses
  NtInitialLocale,
  Vcl.Forms,
  CarFrm in 'CarFrm.pas' {CarForm},
  MainFrm in 'MainFrm.pas' {MainForm},
  ImageFrm in 'ImageFrm.pas' {ImageForm},
  AirplaneFrm in 'AirplaneFrm.pas' {AirplaneForm},
  MessagesFrm in 'MessagesFrm.pas' {MessagesForm},
  BaseFrm in 'BaseFrm.pas' {BaseForm},
  StringsFrm in 'StringsFrm.pas' {StringsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TCarForm, CarForm);
  Application.CreateForm(TAirplaneForm, AirplaneForm);
  Application.CreateForm(TMessagesForm, MessagesForm);
  Application.CreateForm(TStringsForm, StringsForm);
  Application.Run;
end.
