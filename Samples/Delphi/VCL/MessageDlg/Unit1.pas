{
  This application uses runtime packages. It means that all VCL resource strings such
  as message dialog text are not compiled into MessageDlg.exe but are in vclXXX.bpl (vcl250.bpl in Delphi 10.2 Tokyo).
  This is why you also have to localize teh package file.

  - MessageDlg.exe  Original English EXE.
  - MessageDlg.fi   Finnish resources for the EXE.
  - vcl250.bpl      Original English package file.
  - vcl250.fi       Finnish resources for the package file.
}
unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    LanguageButton: TButton;
    ShowButton: TButton;
    procedure LanguageButtonClick(Sender: TObject);
    procedure ShowButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  UITypes,
  NtTranslator,
  NtLanguageDlg;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  TNtLanguageDialog.Select('en');
end;

procedure TForm1.ShowButtonClick(Sender: TObject);
resourcestring
  SMsg = 'Hello World';
begin
  MessageDlg(SMsg, mtInformation, [mbYes, mbNo, mbHelp], 0);
end;

end.
