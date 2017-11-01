unit MainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.Buttons;

type
  TMainForm = class(TForm)
    CarButton: TBitBtn;
    AirplaneButton: TBitBtn;
    MessagesButton: TBitBtn;
    LanguageButton: TBitBtn;
    StringsButton: TBitBtn;
    procedure CarButtonClick(Sender: TObject);
    procedure AirplaneButtonClick(Sender: TObject);
    procedure MessagesButtonClick(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);
    procedure StringsButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  NtLanguageDlg,
  CarFrm,
  AirplaneFrm,
  MessagesFrm,
  StringsFrm;

procedure TMainForm.CarButtonClick(Sender: TObject);
begin
  CarForm.Show;
end;

procedure TMainForm.AirplaneButtonClick(Sender: TObject);
begin
  AirplaneForm.Show;
end;

procedure TMainForm.MessagesButtonClick(Sender: TObject);
begin
  MessagesForm.Show;
end;

procedure TMainForm.StringsButtonClick(Sender: TObject);
begin
  StringsForm.Show;
end;

procedure TMainForm.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select then
    MessagesForm.UpdateStrings;
end;

end.
