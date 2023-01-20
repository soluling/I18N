unit Unit1;

{$DEFINE ZERO}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Label0_0: TLabel;
    Label0_1: TLabel;
    Label1_1: TLabel;
    Label0_2: TLabel;
    Label1_2: TLabel;
    Label2_2: TLabel;
    Label1_3: TLabel;
    Label2_3: TLabel;
    Label1_10: TLabel;
    Label5_10: TLabel;
    ZeroCheck: TCheckBox;
    LanguageButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ZeroCheckChange(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

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

  function Process(ski, bicycle: Integer): String;
  resourcestring
    // Contains five patterns:
    // - Pattern 0 is the top level pattern without pluralized parameters.
    // - Patterns 1-2 are one and other for ski parameter.
    //   These contain additional second placeholder (%s or %1:s) for the rest of the message
    // - Patterns 3-4 are one and other for bicycle parameter.
    SMessagePlural = 'I have {plural, one {one ski} other {%d skis}} {plural, one {and one bicycle} other {and %d bicycles}}';  //loc 0: ski or bicyle count

    // Same as above but with special zero case. Contains seven patterns:
    // - Pattern 0 is the top level pattern without pluralized parameters.
    // - Patterns 1-3 are zero, one and other for ski parameter.
    //   These contain additional second placeholder (%s or %1:s) for the rest of the message
    // - Patterns 4-6 are zero, one and other for bicycle parameter.
    SZeroMessagePlural = 'I have {plural, zero {no skis} one {one ski} other {%d skis}} {plural, zero {and no bicycles} one {and one bicycle} other {and %d bicycles}}';  //loc 0: ski or bicyle count
  begin
    if not ZeroCheck.IsChecked then
      Result := TMultiPattern.Format(SMessagePlural, [ski, bicycle])
    else
      Result := TMultiPattern.Format(SZeroMessagePlural, [ski, bicycle]);
  end;

begin
  Label0_0.Text := Process(0, 0);
  Label0_1.Text := Process(0, 1);
  Label1_1.Text := Process(1, 1);
  Label0_2.Text := Process(0, 2);
  Label1_2.Text := Process(1, 2);
  Label2_2.Text := Process(2, 2);
  Label1_3.Text := Process(1, 3);
  Label2_3.Text := Process(2, 3);
  Label1_10.Text := Process(1, 10);
  Label5_10.Text := Process(5, 10);
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
