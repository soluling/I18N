unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LanguageButton: TButton;
    ShowButton: TButton;
    Label8: TLabel;
    Label9: TLabel;
    CarsLabel: TLabel;
    CarsEdit: TEdit;
    CarsUpDown: TUpDown;
    SkisLabel: TLabel;
    SkisEdit: TEdit;
    SkisUpDown: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);
    procedure ShowButtonClick(Sender: TObject);
    procedure CountEditChange(Sender: TObject);

  private
    function GetCars: Integer;
    function GetSkis: Integer;

    procedure SetCars(value: Integer);
    procedure SetSkis(value: Integer);

    procedure Translate;
    procedure UpdatePatterns;

    property Cars: Integer read GetCars write SetCars;
    property Skis: Integer read GetSkis write SetSkis;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtBase,
  NtPattern,
  NtResource,
  NtResourceEx,
  NtResourceString,
  NtTranslatorEx,
  NtLanguageDlg,
  Unit2;

function TForm1.GetCars: Integer;
begin
  Result := CarsUpDown.Position;
end;

procedure TForm1.SetCars(value: Integer);
begin
  CarsUpDown.Position := value;
end;

function TForm1.GetSkis: Integer;
begin
  Result := SkisUpDown.Position;
end;

procedure TForm1.SetSkis(value: Integer);
begin
  SkisUpDown.Position := value;
end;

procedure TForm1.Translate;
resourcestring
  SHello1 = 'Hello %s';
  SHello2 = 'Hello %s and %s';
  SNoId = 'This has no id';
  SHasId = 'This has an id';
  SHasGroupAndId = 'This has a group and an id';
  SHasGroupButNoId = 'This has an group but no id';
var
  thisCars, thisSkis: Integer;
begin
  // Translating the form sets the properties from the form resource. This sets
  // also the texts of the count edits to their design time values. Keep the counts.
  thisCars := Cars;
  thisSkis := Skis;

  _T(Self);

  Cars := thisCars;
  Skis := thisSkis;

  // Patterns
  Label2.Caption := Format(SHello1, ['John']);
  Label3.Caption := Format(SHello2, ['John', 'Jill']);

  // Strings wit or without id and group
  Label4.Caption := SNoId;
  Label5.Caption := SHasId;
  Label6.Caption := SHasGroupAndId;
  Label7.Caption := SHasGroupButNoId;

  UpdatePatterns;
end;

procedure TForm1.UpdatePatterns;
resourcestring
  SCars = '{plural, one {I have %d car} other {I have %d cars}}';
  SSkis = '{plural, one {I have %d ski} other {I have %d skis}}';
begin
  // ICU patterns. These depend on the counts so update them also when a count changes.
  Label8.Caption := TMultiPattern.Format(SCars, Cars, [Cars]);
  Label9.Caption := TMultiPattern.Format(SSkis, Skis, [Skis]);
end;

procedure TForm1.CountEditChange(Sender: TObject);
begin
  // Called when the up-down arrows change the count or the user types a count
  UpdatePatterns;
end;

procedure TForm1.FormCreate(Sender: TObject);
resourcestring
  SGerman = 'German';
  SEnglish = 'English';
  SFinnish = 'Finnish';
  SFrench = 'French';
  SJapanese = 'Japanese';
begin
  // Adds language names to the language resource
  NtResources.Add('German', 'Deutsch', SGerman, 'de');
  NtResources.Add('English', 'English', SEnglish, 'en');
  NtResources.Add('Finnish', 'suomi', SFinnish, 'fi');
  NtResources.Add('French', 'français', SFrench, 'fr');
  NtResources.Add('Japanese', '日本語', SJapanese, 'ja');

  // Set the initial counts and language
  Cars := 1;
  Skis := 2;
  NtResources.LanguageId := 'en';
  Translate;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
var
  language: String;
begin
  // Show a dialog to choose the active language and set it active
  if TNtLanguageDialog.Choose(language, 'en', '', lnBoth) then
  begin
    NtResources.LanguageId := language;
    Translate;
  end;
end;

procedure TForm1.ShowButtonClick(Sender: TObject);
var
  form2: TForm2;
begin
  form2 := TForm2.Create(Self);
  Form2.Show;
end;

end.
