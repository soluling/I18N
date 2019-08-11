{
  @abstract Implements @link(TNtLanguageDialog) dialog that lets the user to choose a new language.

  FMX only.
}

unit FMX.NtLanguageDlg;

{$I NtVer.inc}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.ListBox,
  FMX.StdCtrls, FMX.NtLocalization, FMX.Controls.Presentation, NtBase;

type
  // Specifies options for @link(TNtLanguageDialog).
  TNtLanguageDialogOption =
  (
    ldShowErrorIfNoResource  //< Show an error if no language resource exists
  );

  TNtLanguageDialogOptions = set of TNtLanguageDialogOption;

  { @abstract Dialog that shows the languages of the available resource files.
    Dialog shows available languages and lets you choose a new language.
    @seealso(FMX.NtLanguageDlg) }
  TNtLanguageDialog = class(TForm)
    LanguageList: TListBox;
    OkButton: TButton;
    CancelButton: TButton;
    Resources1: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure LanguageListDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private
    FOriginalLanguage: String;
    FLanguageName: TNtLanguageName;
    FOptions: TNtLanguageDialogOptions;

    function GetLanguage(i: Integer): String;
    function GetLanguageCount: Integer;
    function GetSelectedLanguage: String;

    procedure SetSelectedLanguage(const value: String);

  public
    class function Select(
      var language: String;
      originalLanguage: String = '';
      languageName: TNtLanguageName = lnNative;
      dialogOptions: TNtLanguageDialogOptions = [];
      languageOptions: TNtResourceOptions = [];
      position: TFormPosition = TFormPosition.MainFormCenter): Boolean; overload;

    class function Select(
      originalLanguage: String = '';
      languageName: TNtLanguageName = lnNative;
      dialogOptions: TNtLanguageDialogOptions = [];
      languageOptions: TNtResourceOptions = [];
      position: TFormPosition = TFormPosition.MainFormCenter): Boolean; overload;

    class procedure Select(
      done: TProc<String>;
      originalLanguage: String = '';
      languageName: TNtLanguageName = lnNative;
      dialogOptions: TNtLanguageDialogOptions = [];
      languageOptions: TNtResourceOptions = [];
      position: TFormPosition = TFormPosition.MainFormCenter); overload;

    class procedure Select(
      done: TProc;
      originalLanguage: String = '';
      languageName: TNtLanguageName = lnNative;
      dialogOptions: TNtLanguageDialogOptions = [];
      languageOptions: TNtResourceOptions = [];
      position: TFormPosition = TFormPosition.MainFormCenter); overload;

    class function Choose(
      var language: String;
      originalLanguage: String = '';
      languageName: TNtLanguageName = lnNative;
      options: TNtLanguageDialogOptions = [];
      position: TFormPosition = TFormPosition.MainFormCenter): Boolean; overload;

    class procedure Choose(
      done: TProc<String>;
      originalLanguage: String = '';
      languageName: TNtLanguageName = lnNative;
      options: TNtLanguageDialogOptions = [];
      position: TFormPosition = TFormPosition.MainFormCenter); overload;

    property LanguageCount: Integer read GetLanguageCount;
    property Languages[i: Integer]: String read GetLanguage;
    property OriginalLanguage: String read FOriginalLanguage write FOriginalLanguage;
    property SelectedLanguage: String read GetSelectedLanguage write SetSelectedLanguage;
    property LanguageName: TNtLanguageName read FLanguageName write FLanguageName;
  end;

implementation

{$R *.fmx}

uses
  FMX.Graphics,
  NtResource,
  FMX.NtTranslator;

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
  originalLanguage: String;
  languageName: TNtLanguageName;
  dialogOptions: TNtLanguageDialogOptions;
  languageOptions: TNtResourceOptions;
  position: TFormPosition): Boolean;
begin
  Result := Choose(language, originalLanguage, languageName, dialogOptions, position);

  if Result then
  begin
    if language = originalLanguage then
      language := '';

    Result := TNtTranslator.SetNew(language, languageOptions, originalLanguage);
  end;
end;

class procedure TNtLanguageDialog.Select(
  done: TProc<String>;
  originalLanguage: String;
  languageName: TNtLanguageName;
  dialogOptions: TNtLanguageDialogOptions;
  languageOptions: TNtResourceOptions;
  position: TFormPosition);
begin
  Choose(
    procedure(language: String)
    begin
      if language = originalLanguage then
        language := '';

      TNtTranslator.SetNew(language, languageOptions, originalLanguage);
      done(language);
    end,
    originalLanguage,
    languageName,
    dialogOptions,
    position);
end;

class procedure TNtLanguageDialog.Select(
  done: TProc;
  originalLanguage: String;
  languageName: TNtLanguageName;
  dialogOptions: TNtLanguageDialogOptions;
  languageOptions: TNtResourceOptions;
  position: TFormPosition);
begin
  Choose(
    procedure(language: String)
    begin
      if language = originalLanguage then
        language := '';

      TNtTranslator.SetNew(language, languageOptions, originalLanguage);
      done;
    end,
    originalLanguage,
    languageName,
    dialogOptions,
    position);
end;

class function TNtLanguageDialog.Select(
  originalLanguage: String;
  languageName: TNtLanguageName;
  dialogOptions: TNtLanguageDialogOptions;
  languageOptions: TNtResourceOptions;
  position: TFormPosition): Boolean;
var
  language: String;
begin
  Result := Select(
    language,
    originalLanguage,
    languageName,
    dialogOptions,
    languageOptions,
    position);
end;

class function TNtLanguageDialog.Choose(
  var language: String;
  originalLanguage: String;
  languageName: TNtLanguageName;
  options: TNtLanguageDialogOptions;
  position: TFormPosition): Boolean;
var
  dialog: TNtLanguageDialog;
begin
  if ldShowErrorIfNoResource in options then
    TNtBase.CheckThatDllsExist;

  if originalLanguage = '' then
    originalLanguage := NtBase.OriginalLanguage;

  dialog := TNtLanguageDialog.Create(nil);
  dialog.OriginalLanguage := originalLanguage;
  dialog.Position := position;
  dialog.FLanguageName := languageName;
  dialog.FOptions := options;

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
end;

class procedure TNtLanguageDialog.Choose(
  done: TProc<String>;
  originalLanguage: String;
  languageName: TNtLanguageName;
  options: TNtLanguageDialogOptions;
  position: TFormPosition);
var
  dialog: TNtLanguageDialog;
begin
  if ldShowErrorIfNoResource in options then
    TNtBase.CheckThatDllsExist;

  if originalLanguage = '' then
    originalLanguage := NtBase.OriginalLanguage;

  dialog := TNtLanguageDialog.Create(nil);
  dialog.OriginalLanguage := originalLanguage;
  dialog.Position := position;
  dialog.FLanguageName := languageName;
  dialog.FOptions := options;

  dialog.ShowModal(procedure(modalResult: TModalResult)
    begin
      if modalResult = mrOk then
        done(dialog.SelectedLanguage);
    end);
end;

procedure TNtLanguageDialog.FormCreate(Sender: TObject);
begin
  FOriginalLanguage := '';
  FLanguageName := lnNative;
  _T(Self);
end;

procedure TNtLanguageDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure LoadBitmap(bitmap: TBitmap; const resName: String);
var
  stream: TResourceStream;
begin
  if resName = '' then
    Exit;

  stream := TResourceStream.Create(HInstance, resName, RT_RCDATA);
  try
    bitmap.LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TNtLanguageDialog.FormActivate(Sender: TObject);

  procedure Add(language: TNtLanguage);
  var
    item: TListBoxItem;
  begin
    item := TListBoxItem.Create(nil);
    item.Parent := LanguageList;
    item.StyleLookup := 'CustomItem';
    item.Text := language.Names[LanguageName];
    item.Data := TLanguageCode.Create(language.Code);
    LoadBitmap(item.ItemData.Bitmap, NtResources.LanguageImages[language.Code]);

    if item.ItemData.Bitmap.Height > 0 then
      item.Height := item.ItemData.Bitmap.Height;

    item.Height := item.Height + 2
  end;

var
  i: Integer;
  availableLanguages: TNtLanguages;
begin
  if LanguageList.Count > 0 then
    Exit;

  LanguageList.BeginUpdate;
  try
    availableLanguages := TNtLanguages.Create;
    try
      if OriginalLanguage <> '' then
        availableLanguages.Add(OriginalLanguage);

      TNtBase.GetAvailable(availableLanguages, '');

      for i := 0 to availableLanguages.Count - 1 do
        Add(availableLanguages[i]);
    finally
      availableLanguages.Free;
    end;

    if LoadedResourceLocale <> '' then
    begin
      for i := 0 to LanguageCount - 1 do
      begin
        if SameText(Languages[i], LoadedResourceLocale) then
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
  finally
    LanguageList.EndUpdate;
  end;
end;

procedure TNtLanguageDialog.LanguageListDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.
