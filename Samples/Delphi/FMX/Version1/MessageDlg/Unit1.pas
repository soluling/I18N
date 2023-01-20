unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, NtBase;

type
  TForm1 = class(TForm)
    LanguageGroup: TGroupBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);

  private
    FUpdating: Boolean;
    FLanguages: TNtLanguages;
    FRadioButtons: array of TRadioButton;

    procedure UpdateLanguages;
    procedure LanguageRadioChanged(sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  NtBaseTranslator,
  NtResource,
  FMX.NtTranslator;

procedure TForm1.UpdateLanguages;
var
  i: Integer;
  locale: String;
begin
  FUpdating := True;

  locale := TNtBase.GetActiveLocale;

  if locale = '' then
    locale := OriginalLanguage;

  for i := 0 to FLanguages.Count - 1 do
    FRadioButtons[i].IsChecked := FLanguages[i].Code = locale;

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

  UpdateLanguages;
end;

end.
