unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, NtBase;

type
  TMainForm = class(TForm)
    MainLabel: TLabel;
    MainCombo: TComboBox;
    MainGroup: TGroupBox;
    PlanLabel: TLabel;
    PlanList: TListBox;
    NowCheck: TCheckBox;
    MainButton: TButton;
    ClientLabel: TLabel;
    ClientCombo: TComboBox;
    ClientGroup: TGroupBox;
    NameLabel: TLabel;
    NameEdit: TEdit;
    AgeLabel: TLabel;
    AgeEdit: TEdit;
    AgeUpDown: TUpDown;
    NewCheck: TCheckBox;
    ClientButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure MainComboChange(Sender: TObject);
    procedure ClientComboChange(Sender: TObject);
    procedure MainButtonClick(Sender: TObject);
    procedure ClientButtonClick(Sender: TObject);

  private
    FLanguages: TNtLanguages;

    function GetMainLanguage: String;
    function GetClientLanguage: String;

    property MainLanguage: String read GetMainLanguage;
    property ClientLanguage: String read GetClientLanguage;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  TypInfo, NtBaseTranslator, NtTranslator, MainOptionsDlg, ClientOptionsDlg;

const
  UI_LANGUAGE = 'en';

function IsClientItem(
  host: TComponent;
  obj: TObject): Boolean;
begin
  // Item is a a client item if it belong to ClientDialog or to ClientGroup grout box of Form1
  Result :=
    (host = ClientOptionsDialog) or
    (host = MainForm) and ((obj = MainForm.ClientGroup) or (obj is TControl) and (TControl(obj).Parent = MainForm.ClientGroup));
end;

procedure TranslateMain(
  host: TComponent;
  obj: TObject;
  propertyInfo: PPropInfo;
  const currentValue: Variant;
  var newValue: Variant;
  var cancel: Boolean);
begin
  if IsClientItem(host, obj) then
    cancel := True;
end;

procedure TranslateClient(
  host: TComponent;
  obj: TObject;
  propertyInfo: PPropInfo;
  const currentValue: Variant;
  var newValue: Variant;
  var cancel: Boolean);
begin
  if not IsClientItem(host, obj) then
    cancel := True;
end;

function TMainForm.GetMainLanguage: String;
begin
  if MainCombo.ItemIndex >= 0 then
    Result := FLanguages[MainCombo.ItemIndex].Code
  else
    Result := '';
end;

function TMainForm.GetClientLanguage: String;
begin
  if ClientCombo.ItemIndex >= 0 then
    Result := FLanguages[ClientCombo.ItemIndex].Code
  else
    Result := '';
end;

procedure TMainForm.FormCreate(Sender: TObject);

  procedure SetIndex(value: Integer);
  begin
    MainCombo.ItemIndex := value;
    ClientCombo.ItemIndex := value;
  end;

  procedure Process(language: TNtLanguage);
  begin
    MainCombo.Items.AddObject(language.NativeName, language);
    ClientCombo.Items.AddObject(language.NativeName, language);

    if SameText(TNtBase.GetActiveLocale, language.Code) then
      SetIndex(MainCombo.Items.Count - 1);
  end;

var
  i: Integer;
begin
  // Get the available languages
  FLanguages := TNtLanguages.Create;
  FLanguages.Add(UI_LANGUAGE, LANG_ENGLISH);
  TNtBase.GetAvailable(FLanguages);

  // Populate the language combo boxes
  for i := 0 to FLanguages.Count - 1 do
    Process(FLanguages[i]);

  if MainCombo.ItemIndex = -1 then
    SetIndex(0);
end;

procedure TMainForm.MainComboChange(Sender: TObject);
begin
  // Set the NtBeforeTranslate event to accept only items that belong to main
  // and translate the application. This translates all but client items.
  NtBeforeTranslate := TranslateMain;
  TNtTranslator.SetNew(MainLanguage);
end;

procedure TMainForm.ClientComboChange(Sender: TObject);
begin
  // Set the NtBeforeTranslate event to accept only items that belong to client
  // and translate the application. This translate only client items.
  NtBeforeTranslate := TranslateClient;
  TNtTranslator.SetNew(ClientLanguage);
end;

procedure TMainForm.MainButtonClick(Sender: TObject);
begin
  MainOptionsDialog.ShowModal;
end;

procedure TMainForm.ClientButtonClick(Sender: TObject);
begin
  ClientOptionsDialog.ShowModal;
end;

initialization
  DefaultLocale := UI_LANGUAGE;
  NtEnabledProperties := STRING_TYPES;
end.
