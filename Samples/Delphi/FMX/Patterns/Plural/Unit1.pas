unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation;

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
    ZeroCheck: TCheckBox;
    LanguageButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);
    procedure ZeroCheckChange(Sender: TObject);

  private
    procedure UpdateStrings;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  NtPattern,
  NtResource,
  NtResourceString,  // Turns on resource string translation
  FMX.NtLanguageDlg,
  FMX.NtTranslator;

procedure TForm1.UpdateStrings;

  function Process(count: Integer): String;
  resourcestring
    // Contains two patterns: one and other.
    SMessagePlural = '{plural, one {%d file} other {%d files}}';  //loc 0: file count

    // Contains three patterns: zero, one and other.
    SZeroMessagePlural = '{plural, zero {No files} one {%d file} other {%d files}}';  //loc 0: file count
  begin
    if not ZeroCheck.IsChecked then
      Result := TMultiPattern.Format(SMessagePlural, count, [count])
    else
      Result := TMultiPattern.Format(SZeroMessagePlural, count, [count]);
  end;

begin
  label0.Text := Process(0);
  label1.Text := Process(1);
  label2.Text := Process(2);
  label3.Text := Process(3);
  label4.Text := Process(4);
  label5.Text := Process(5);
  label11.Text := Process(11);
  label21.Text := Process(21);
  label101.Text := Process(101);
  label111.Text := Process(111);
end;

procedure TForm1.FormCreate(Sender: TObject);
resourcestring
  SEnglish = 'English';
  SFinnish = 'Finnish';
  SGerman = 'German';
  SFrench = 'French';
  SJapanese = 'Japanese';
begin
  NtResources.Add('English', 'English', SEnglish, 'en');
  NtResources.Add('Finnish', 'suomi', SFinnish, 'fi');
  NtResources.Add('German', 'Deutsch', SGerman, 'de');
  NtResources.Add('French', 'français', SFrench, 'fr');
  NtResources.Add('Japanese', '日本語', SJapanese, 'ja');

  _T(Self);
  UpdateStrings;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select then
    UpdateStrings;
end;

procedure TForm1.ZeroCheckChange(Sender: TObject);
begin
  UpdateStrings;
end;

end.
