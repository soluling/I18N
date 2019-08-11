{
  @abstract Implements @link(TNtLanguageDialog) dialog that lets the user to choose a new language.

  VCL only.

  You don't have to manually create a dialog and call its ShowModal function but
  you can use @link(TNtLanguageDialog.Select) function that creates a language dialog,
  shows it, and finally turn on the selected language. If you want only to select the
  language but not turn it on call @link(TNtLanguageDialog.Choose) function.
  Then you have to call @link(TNtTranslator.SetNew) to turn on the language.

  By default the dialog shows the language names in native language. So German
  will appear as Deutsch. You can change this by passing languageName parameter to
  @link(TNtLanguageDialog.Select) or set @link(TNtLanguageDialog.LanguageName).
  In order to show the language names in the current language each language name
  must be found from the resource string. To add language names into resource
  you have to create a .rc file that contains the language names in a string table.

  @longCode(#
STRINGTABLE
BEGIN
  0x07 "German";
  0x09 "English";
  0x0c "French";
  0x11 "Japanese";
END
#)

  The id of each string must match the locale id of the lanugage. Add the .rc
  file into the project and recompile. Next time you scan your project the above
  strings will appear and you will be able to translate them. If you do not add
  the language name strings then dialog shows the language names in the language
  of the operating system.

  @italic(Library\Delphi\Languages.rc) is a string table resource file that
  contains all languages and locales supported by Windows. You can copy paste
  those languages that your project uses into your own Languages.rc file.
}

unit NtLanguageDlg;

{$I NtVer.inc}

interface

uses
  Classes, Controls, StdCtrls, Forms, NtBase, NtLocalization;

type
  // Specifies options for @link(TNtLanguageDialog).
  TNtLanguageDialogOption =
  (
    ldShowErrorIfNoDll,   //< Show an error if no resource DLL exists
    ldCompatible,         //< Show only languages that are compatible to the system. Not used in Delphi 2009 and later.
    ldCheckVersion        //< Show only languages that are compatible to the current version of the application.
  );

  // Set of language dialog options.
  TNtLanguageDialogOptions = set of TNtLanguageDialogOption;

  { @abstract Dialog that shows the languages of the available resource files.
    Dialog shows available languages and lets you choose a new language.
    @seealso(NtLanguageDlg) }
  TNtLanguageDialog = class(TForm)
    LanguageList: TListBox;
    OkButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(sender: TObject);
    procedure FormShow(sender: TObject);
    procedure LanguageListDblClick(sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    FCheckVersion: Boolean;
    FCompatible: Boolean;
    FLanguageName: TNtLanguageName;
    FOriginalLanguage: String;
    FOriginalLanguageName: String;

    function GetLanguage(i: Integer): String;
    function GetLanguageCount: Integer;
    function GetSelectedLanguage: String;

    procedure SetSelectedLanguage(const value: String);

  public
    { Shows a dialog that contains a list of available languages. After user
      has selected a new language the function turns that language active.
      @param languageName      Language name kind.
      @param dialogOptions     Dialog options.
      @param languageOptions   Language change options.
      @param position          Initial position of the form.
      @return @true on success, @false if cancelled by user. }
    class function Select(
      languageName: TNtLanguageName = lnNative;
      dialogOptions: TNtLanguageDialogOptions = [];
      languageOptions: TNtResourceOptions = [];
      position: TPosition = poMainFormCenter): Boolean; overload;

    { Shows a dialog that contains a list of available languages. After user
      has selected a new language the function turns that language active.
      @param language              Returns the selected language.
      @param originalLanguage      Language used in the original application. If empty original language is not shown.
      @param originalLanguageName  Name of the original language. If empty the default name is used.
      @param languageName          Language name kind.
      @param dialogOptions         Dialog options.
      @param languageOptions       Language change options.
      @param position              Initial position of the form.
      @return @true on success, @false if cancelled by user. }
    class function Select(
      var language: String;
      const originalLanguage: String;
      const originalLanguageName: String = '';
      languageName: TNtLanguageName = lnNative;
      dialogOptions: TNtLanguageDialogOptions = [];
      languageOptions: TNtResourceOptions = [];
      position: TPosition = poMainFormCenter): Boolean; overload;

    { As @link(TNtLanguageDialog.Select) but no language parameter.}
    class function Select(
      const originalLanguage: String;
      const originalLanguageName: String = '';
      languageName: TNtLanguageName = lnNative;
      dialogOptions: TNtLanguageDialogOptions = [];
      languageOptions: TNtResourceOptions = [];
      position: TPosition = poMainFormCenter): Boolean; overload;

    { As @link(TNtLanguageDialog.Select) but does not turn on the selected language active.
      This function just shows a dialog that let you choose a new language. }
    class function Choose(
      var language: String;
      const originalLanguage: String = '';
      const originalLanguageName: String = '';
      languageName: TNtLanguageName = lnNative;
      options: TNtLanguageDialogOptions = [];
      position: TPosition = poMainFormCenter): Boolean;

    { If @true the dialog shows only those languages that are compatible to the current version of the application.
      If @false all languages are shown. }
    property CheckVersion: Boolean read FCheckVersion write FCheckVersion;

    { If @true the dialog shows only those languages that are compatible to the system.
      If @false all languages are shown. Not used in Delphi 2009 and later. }
    property Compatible: Boolean read FCompatible write FCompatible;

    { The amount of available languages. }
    property LanguageCount: Integer read GetLanguageCount;

    { Array of available languages. First language index is 0. }
    property Languages[i: Integer]: String read GetLanguage;

    { Original language of the application. }
    property OriginalLanguage: String read FOriginalLanguage write FOriginalLanguage;

    { Display name of the oOriginal language of the application. }
    property OriginalLanguageName: String read FOriginalLanguageName write FOriginalLanguageName;

    { Language that is currently selected in the dialog. }
    property SelectedLanguage: String read GetSelectedLanguage write SetSelectedLanguage;

    { Specifies how language names are shown. }
    property LanguageName: TNtLanguageName read FLanguageName write FLanguageName;
  end;

implementation

{$R *.dfm}

uses
  SysUtils,
  NtTranslator;

type
  TLanguageCode = class(TObject)
  private
    FValue: String;

  public
    constructor Create(const value: String);

    property Value: String read FValue;
  end;

constructor TLanguageCode.Create(const value: String);
begin
  inherited Create;
  FValue := value;
end;


// TNtLanguageDialog

function TNtLanguageDialog.GetLanguageCount: Integer;
begin
  Result := LanguageList.Items.Count;
end;

function TNtLanguageDialog.GetLanguage(i: Integer): String;
begin
  Result := TLanguageCode(LanguageList.Items.Objects[i]).Value;
end;

function TNtLanguageDialog.GetSelectedLanguage: String;
begin
  if LanguageList.ItemIndex >= 0 then
    Result := Languages[LanguageList.ItemIndex]
  else
    Result := '';
end;

procedure TNtLanguageDialog.SetSelectedLanguage(const value: String);
var
  i: Integer;
begin
  for i := 0 to LanguageCount - 1 do
    if Languages[i] = value then
    begin
      LanguageList.ItemIndex := i;
      Break;
    end;
end;

class function TNtLanguageDialog.Select(
  var language: String;
  const originalLanguage: String;
  const originalLanguageName: String;
  languageName: TNtLanguageName;
  dialogOptions: TNtLanguageDialogOptions;
  languageOptions: TNtResourceOptions;
  position: TPosition): Boolean;
begin
  Result := Choose(language, originalLanguage, originalLanguageName, languageName, dialogOptions, position);

  if Result then
  begin
    if language = originalLanguage then
      language := '';

    Result := TNtTranslator.SetNew(language, languageOptions, originalLanguage);
  end;
end;

class function TNtLanguageDialog.Select(
  const originalLanguage: String;
  const originalLanguageName: String;
  languageName: TNtLanguageName;
  dialogOptions: TNtLanguageDialogOptions;
  languageOptions: TNtResourceOptions;
  position: TPosition): Boolean;
var
  language: String;
begin
  Result := Select(
    language,
    originalLanguage,
    originalLanguageName,
    languageName,
    dialogOptions,
    languageOptions,
    position);
end;

class function TNtLanguageDialog.Select(
  languageName: TNtLanguageName;
  dialogOptions: TNtLanguageDialogOptions;
  languageOptions: TNtResourceOptions;
  position: TPosition): Boolean;
var
  language: String;
begin
  Result := Select(
    language,
    DefaultLocale,
    '',
    languageName,
    dialogOptions,
    languageOptions,
    position);
end;

class function TNtLanguageDialog.Choose(
  var language: String;
  const originalLanguage: String;
  const originalLanguageName: String;
  languageName: TNtLanguageName;
  options: TNtLanguageDialogOptions;
  position: TPosition): Boolean;
var
  dialog: TNtLanguageDialog;
begin
  if ldShowErrorIfNoDll in options then
    TNtBase.CheckThatDllsExist;

  dialog := TNtLanguageDialog.Create(nil);
  try
    dialog.CheckVersion := ldCheckVersion in options;
    dialog.Compatible := ldCompatible in options;
    dialog.CheckVersion := ldCheckVersion in options;
    dialog.OriginalLanguage := originalLanguage;
    dialog.OriginalLanguageName := originalLanguageName;
    dialog.Position := position;
    dialog.LanguageName := languageName;

    if dialog.ShowModal = mrOk then
    begin
      language := dialog.SelectedLanguage;
      Result := True;
    end
    else
    begin
      language := '';
      Result := False;
    end;
  finally
    dialog.Free;
  end;
end;

procedure TNtLanguageDialog.FormCreate(Sender: TObject);
begin
  FCheckVersion := False;
  FCompatible := False;
  FOriginalLanguage := '';
  FLanguageName := lnNative;

  if roFlipChildren in ResourceOptions then
    TNtTranslator.InitializeForm(Self);
end;

procedure TNtLanguageDialog.FormShow(Sender: TObject);

  procedure Add(language: TNtLanguage);
  begin
    if not CheckVersion or TNtResource.DoesLocaleVersionMatch(language.Code) then
      LanguageList.Items.AddObject(
        language.Names[FLanguageName],
        TLanguageCode.Create(language.Code));
  end;

var
  i: Integer;
  availableLanguages: TNtLanguages;
begin
  availableLanguages := TNtLanguages.Create;
  try
    if OriginalLanguage <> '' then
    begin
      if OriginalLanguageName <> '' then
        LanguageList.Items.AddObject(
          OriginalLanguageName,
          TLanguageCode.Create(OriginalLanguage))
      else
        availableLanguages.Add(OriginalLanguage);
    end;

    TNtBase.GetAvailable(availableLanguages, '', FCompatible, FCheckVersion);

    for i := 0 to availableLanguages.Count - 1 do
      Add(availableLanguages[i]);
  finally
    availableLanguages.Free;
  end;

  if TNtBase.GetActiveLocale <> '' then
  begin
    for i := 0 to LanguageCount - 1 do
    begin
      if SameText(Languages[i], TNtBase.GetActiveLocale) then
      begin
        LanguageList.ItemIndex := i;
        Exit;
      end;
    end;
  end
  else
  begin
    for i := 0 to LanguageCount - 1 do
    begin
      if SameText(Languages[i], OriginalLanguage) then
      begin
        LanguageList.ItemIndex := i;
        Exit;
      end;
    end;
  end;

  if (LanguageList.ItemIndex = -1) and (LanguageList.Items.Count > 0) then
    LanguageList.ItemIndex := 0;
end;

procedure TNtLanguageDialog.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to LanguageList.Items.Count - 1 do
    TLanguageCode(LanguageList.Items.Objects[i]).Free;
end;

procedure TNtLanguageDialog.LanguageListDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;


end.
