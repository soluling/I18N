unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, NtBase;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    NativeRadio: TRadioButton;
    LocalizedRadio: TRadioButton;
    EnglishRadio: TRadioButton;
    SystemRadio: TRadioButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);

  private
    function GetLanguageName: TNtLanguageName;

    property LanguageName: TNtLanguageName read GetLanguageName;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  NtResource,
  NtResourceString,
  FMX.NtLanguageDlg,
  FMX.NtTranslator;

function TForm1.GetLanguageName: TNtLanguageName;
begin
  if NativeRadio.IsChecked then
    Result := lnNative
  else if LocalizedRadio.IsChecked then
    Result := lnLocalized
  else if EnglishRadio.IsChecked then
    Result := lnEnglish
  else
    Result := lnSystem
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

  NativeRadio.IsChecked := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TNtLanguageDialog.Select('', LanguageName);
end;

end.
