unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, NtBase;

type
  TForm1 = class(TForm)
    PatternLabel: TLabel;
    PatternEdit: TComboBox;
    ResultLabel: TLabel;
    ParseButton: TButton;
    LanguageLabel: TLabel;
    LanguageCombo: TComboBox;
    ItemsList: TListView;
    ParameterLabel: TLabel;
    ParameterEdit: TEdit;
    ParameterUpDown: TUpDown;
    ClearButton: TButton;
    DefaultButton: TButton;
    StatusLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure PatternEditChange(Sender: TObject);
    procedure ParseButtonClick(Sender: TObject);
    procedure LanguageComboChange(Sender: TObject);
    procedure ParameterEditChange(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure DefaultButtonClick(Sender: TObject);

  private
    FLanguages: TNtLanguages;

    function GetLanguageId: String;
    function GetLanguageCount: Integer;
    function GetLanguage(i: Integer): TNtLanguage;
    function GetPattern: String;
    function GetParameter: Integer;

    procedure SetLanguageId(const value: String);
    procedure SetPattern(const value: String);
    procedure SetParameter(value: Integer);

    procedure Process;
    procedure SetDefault;

    procedure HideList;

  public
    property LanguageId: String read GetLanguageId write SetLanguageId;
    property LanguageCount: Integer read GetLanguageCount;
    property Languages[i: Integer]: TNtLanguage read GetLanguage;
    property Pattern: String read GetPattern write SetPattern;
    property Parameter: Integer read GetParameter write SetParameter;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  // This contains TMultiPattern.Format
  NtPattern,
  // This is not normally needed with plurals. It is used here to the plular forms that active language required
  NtPluralsData;

procedure TForm1.SetDefault;
begin
  PatternEdit.Text := PatternEdit.Items[0];
  LanguageId := 'en';
  PatternEdit.ItemIndex := 0;
  Parameter := 1;
  PatternEditChange(Self);
end;

function TForm1.GetLanguageCount: Integer;
begin
  Result := LanguageCombo.Items.Count;
end;

function TForm1.GetLanguage(i: Integer): TNtLanguage;
begin
  Result := TNtLanguage(LanguageCombo.Items.Objects[i])
end;

function TForm1.GetLanguageId: String;
begin
  if LanguageCombo.ItemIndex >= 0 then
    Result := Languages[LanguageCombo.ItemIndex].Code
  else
    Result := '';
end;

procedure TForm1.SetLanguageId(const value: String);
var
  i: Integer;
begin
  for i := 0 to LanguageCount - 1 do
  begin
    if Languages[i].Code = value then
    begin
      LanguageCombo.ItemIndex := i;
      LanguageComboChange(Self);
      Exit;
    end;
  end;

  LanguageCombo.ItemIndex := -1;
end;

function TForm1.GetPattern: String;
begin
  Result := PatternEdit.Text;
end;

procedure TForm1.SetPattern(const value: String);
begin
  PatternEdit.Text := value;
  PatternEditChange(Self);
end;

function TForm1.GetParameter: Integer;
begin
  Result := ParameterUpDown.Position;
end;

procedure TForm1.SetParameter(value: Integer);
begin
  ParameterUpDown.Position := value;
end;

procedure TForm1.Process;
var
  str: String;
  kind: TFormatParameterKind;
  plural: TPlural;
  operatorKind: TOperatorKind;
  operand, operand2: Integer;
  pat: TPattern;
  part: TFormatPart;
  requiredPlurals, actualPlurals, missingPlurals: TPlurals;
  formatString: TFormatString;
begin
  // Get the result
  if Pattern <> '' then
  begin
    str := TMultiPattern.Format(Pattern, [Parameter]);
    TMultiPattern.GetNumber(Pattern, Parameter, kind, plural, operatorKind, operand, operand2);

    if kind = fpPlural then
      ResultLabel.Caption := TMultiPattern.PluralToString(plural) + ': ' + str
    else
      ResultLabel.Caption := TMultiPattern.OperatorToString(operatorKind, operand, operand2) + ': ' + str
  end
  else
  begin
    ResultLabel.Caption := '';
  end;

  // Check that pattern contains all items
  requiredPlurals := TMultiPattern.GetPlurals(puInteger);
  actualPlurals := [];

  formatString := TFormatString.Create;
  try
    formatString.ParsePattern(Pattern);

    for part in formatString do
    begin
      for pat in part do
      begin
        if pat is TPluralPattern then
          Include(actualPlurals, TPluralPattern(pat).Plural);
      end;
    end;
  finally
    formatString.Free;
  end;

  if actualPlurals >= requiredPlurals then
  begin
    StatusLabel.Caption := 'All required plurals forms are present';
    StatusLabel.Font.Color := clGreen;
  end
  else
  begin
    missingPlurals := requiredPlurals - actualPlurals;

    str := '';

    for plural := Low(plural) to High(plural) do
      if plural in missingPlurals then
      begin
        if str <> '' then
          str := str + ', ';

        str := str + TMultiPattern.PluralToString(plural);
      end;

    StatusLabel.Caption := Format('Following plurals forms are missing: %s', [str]);
    StatusLabel.Font.Color := clRed;
  end;
end;

var
  enumLanugages: TNtLanguages;

function EnumLocalesEx(localeStr: PChar; flags: DWord; param: LParam): Integer; stdcall;
begin
  if (localeStr <> '') and (Pos('-', localeStr) = 0) then
    enumLanugages.Add(localeStr);

  Result := 1;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
  language: TNtLanguage;
begin
  // Get all languages supported by Windows
  FLanguages := TNtLanguages.Create;
  enumLanugages := FLanguages;
  EnumSystemLocalesEx(@EnumLocalesEx, LOCALE_ALL, 0, nil);

  for i := 0 to FLanguages.Count - 1 do
  begin
    language := FLanguages[i];
    LanguageCombo.Items.AddObject(language.SystemName, language);
  end;

  LanguageCombo.Sorted := True;

  // Populate pattern list
  // Standard English pattern
  PatternEdit.Items.Add('{plural, one {I have %d car} other {I have %d cars}}');
  // English pattern with optional zero part
  PatternEdit.Items.Add('{plural, zero {I have no cars} one {I have one car} other {I have %d cars}}');
  // Pattern with several custom operator parts
  PatternEdit.Items.Add('{plural, =0 {I have no cars} one {I have %d car} =2 {I have two cars} ~12 {I have a dozen cars} >=20 {I must be a car dealer} other {I have %d cars}}');

  HideList;

  SetDefault;
end;

procedure TForm1.HideList;
begin
  ItemsList.Items.Clear;
  ItemsList.Hide;
end;

procedure TForm1.PatternEditChange(Sender: TObject);
begin
  ParseButton.Enabled := Pattern <> '';
  ClearButton.Enabled := Pattern <> '';
  ResultLabel.Visible := Pattern <> '';

  if Pattern = '' then
    ItemsList.Hide;

  Process;
end;

procedure TForm1.LanguageComboChange(Sender: TObject);
begin
  // Plural engine needs to know the language that the application uses.
  HideList;
  DefaultLocale := LanguageId;
  PatternEditChange(Self);
end;

procedure TForm1.ParameterEditChange(Sender: TObject);
begin
  Process;
end;

procedure TForm1.ParseButtonClick(Sender: TObject);
var
  pat: TPattern;
  part: TFormatPart;
  formatString: TFormatString;
  item: TListItem;
begin
  formatString := TFormatString.Create;
  try
    formatString.ParsePattern(Pattern);

    for part in formatString do
    begin
      for pat in part do
      begin
        item := ItemsList.Items.Add;
        item.Caption := pat.ToString;
        item.SubItems.Add(pat.Value);
      end;
    end;
  finally
    formatString.Free;
  end;

  ItemsList.Show;
end;

procedure TForm1.ClearButtonClick(Sender: TObject);
begin
  Pattern := '';
end;

procedure TForm1.DefaultButtonClick(Sender: TObject);
begin
  SetDefault;
end;

initialization
  RaiseExceptionOnInvalidPattern := False;
end.
