unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label0: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label11: TLabel;
    Label21: TLabel;
    Label101: TLabel;
    Label111: TLabel;
    InfoLabel0: TLabel;
    InfoLabel1: TLabel;
    InfoLabel2: TLabel;
    InfoLabel3: TLabel;
    InfoLabel4: TLabel;
    InfoLabel5: TLabel;
    InfoLabel11: TLabel;
    InfoLabel21: TLabel;
    InfoLabel101: TLabel;
    InfoLabel111: TLabel;
    ZeroCheckBox: TCheckBox;
    LanguageButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ZeroCheckBoxClick(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

  private
    procedure UpdateValues;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtPattern, NtLanguageDlg;

procedure TForm1.UpdateValues;
resourcestring
  // Contains two patterns: one and other.
  SMessagePlural = '{plural, one {%d file} other {%d files}}';  //loc 0: Amount of files
  // Contains three patterns: zero, one and other.
  SZeroMessagePlural = '{plural, zero {No files} one {%d file} other {%d files}}'; //loc 0: Amount of files

  procedure Process(count: Integer; messageLabel, infoLabel: TLabel);
  var
    str, pattern: String;
    form, customForm: TPlural;
  begin
    // Update message label
    if ZeroCheckBox.Checked then
      pattern := SZeroMessagePlural
    else
      pattern := SMessagePlural;

    messageLabel.Caption := TMultiPattern.Format(pattern, count, [count]);

    // Update info label
    TMultiPattern.GetMatchingPlural(count, pattern, form, customForm);

    str := TMultiPattern.GetPluralName(customForm);

    if form <> customForm then
      infoLabel.Font.Style := [fsBold]
    else
      infoLabel.Font.Style := [];

    infoLabel.Caption := str;
  end;

begin
  Process(0, Label0, InfoLabel0);
  Process(1, Label1, InfoLabel1);
  Process(2, Label2, InfoLabel2);
  Process(3, Label3, InfoLabel3);
  Process(4, Label4, InfoLabel4);
  Process(5, Label5, InfoLabel5);
  Process(11, Label11, InfoLabel11);
  Process(21, Label21, InfoLabel21);
  Process(101, Label101, InfoLabel101);
  Process(111, Label111, InfoLabel111);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateValues;
end;

procedure TForm1.ZeroCheckBoxClick(Sender: TObject);
begin
  UpdateValues;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select then
    UpdateValues;
end;

// If your original language is not English remove comments and set the default language here.
{
initialization
  DefaultLocale := 'de';
}
end.
