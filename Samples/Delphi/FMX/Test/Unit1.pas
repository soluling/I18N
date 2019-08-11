unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    MessageButton: TButton;
    LanguageButton: TButton;
    MessageLabel: TLabel;
    DeviceLabel: TLabel;
    procedure MessageButtonClick(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    procedure UpdateStrings;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}
{$R *.SmXhdpiPh.fmx ANDROID}
{$R *.XLgXhdpiTb.fmx ANDROID}
{$R *.GGlass.fmx ANDROID}
{$R *.Windows.fmx MSWINDOWS}
{$R *.LgXhdpiTb.fmx ANDROID}

uses
  NtResource,
  FMX.NtLanguageDlg,
  FMX.NtTranslator;

procedure TForm1.UpdateStrings;
begin
  DeviceLabel.Text := Format(_T('Device is "%s"'), [TNtTranslator.GetDevice(Self)]);
  MessageLabel.Text := _T('This is a sample');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  NtResources._T('English', 'en');
  NtResources._T('Finnish', 'fi');
  NtResources._T('German', 'de');
  NtResources._T('French', 'fr');
  NtResources._T('Japanese', 'ja');

  _T(Self);
  UpdateStrings
end;

procedure TForm1.MessageButtonClick(Sender: TObject);
begin
  ShowMessage(_T('Hello world'));
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  TNtLanguageDialog.Select(
    procedure
    begin
      UpdateStrings;
    end);
end;

end.
