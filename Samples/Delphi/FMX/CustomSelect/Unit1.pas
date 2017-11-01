unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, NtBase;

type
  TForm1 = class(TForm)
    LanguageGroup: TGroupBox;
    NameGroup: TGroupBox;
    NativeRadio: TRadioButton;
    LocalizedRadio: TRadioButton;
    EnglishRadio: TRadioButton;
    SystemRadio: TRadioButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure NameRadioChange(Sender: TObject);

  private
    FUpdating: Boolean;
    FLanguages: TNtLanguages;
    FRadioButtons: array of TRadioButton;

    function GetLanguageName: TNtLanguageName;

    procedure UpdateLanguages;
    procedure LanguageRadioChanged(sender: TObject);

    property LanguageName: TNtLanguageName read GetLanguageName;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  NtBaseTranslator,
  NtResource,
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

procedure TForm1.UpdateLanguages;
var
  i: Integer;
  locale: String;
  language: TNtLanguage;
  radio: TRadioButton;
begin
  FUpdating := True;

  locale := TNtBase.GetActiveLocale;

  if locale = '' then
    locale := OriginalLanguage;

  for i := 0 to FLanguages.Count - 1 do
  begin
    language := FLanguages[i];
    radio := FRadioButtons[i];
    radio.Text := language.Names[LanguageName];
    radio.IsChecked := language.Code = locale;
  end;

  FUpdating := False;
end;

procedure TForm1.LanguageRadioChanged(sender: TObject);
var
  language: TNtLanguage;
  radio: TRadioButton;
begin
  if FUpdating then
    Exit;

  radio := sender as TRadioButton;

  if not radio.IsChecked then
    Exit;

  language := radio.TagObject as TNtLanguage;

  TNtTranslator.SetNew(language.Code);
  UpdateLanguages;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i, radioWidth: Integer;
  radio: TRadioButton;
  language: TNtLanguage;
begin
  NtResources._T('English', 'en');
  NtResources._T('Finnish', 'fi');
  NtResources._T('German', 'de');
  NtResources._T('French', 'fr');
  NtResources._T('Japanese', 'ja');

  _T(Self);

  FLanguages := TNtLanguages.Create;
  FLanguages.Add('en', 9);
  TNtBase.GetAvailable(FLanguages);

  SetLength(FRadioButtons, FLanguages.Count);
  radioWidth := Round((LanguageGroup.Width - 16)/FLanguages.Count) - 8;

  for i := 0 to FLanguages.Count - 1 do
  begin
    language := FLanguages[i];

    radio := TRadioButton.Create(Self);
    radio.Parent := LanguageGroup;
    radio.TagObject := language;
    radio.Width := radioWidth;
    radio.Position.Y := 24;
    radio.Position.X := 8 + i*radioWidth;
    radio.OnChange := LanguageRadioChanged;

    FRadioButtons[i] := radio;
  end;

  NativeRadio.IsChecked := True;
  UpdateLanguages;
end;

procedure TForm1.NameRadioChange(Sender: TObject);
begin
  UpdateLanguages;
end;

initialization
  NtEnabledProperties := STRING_TYPES;
end.
