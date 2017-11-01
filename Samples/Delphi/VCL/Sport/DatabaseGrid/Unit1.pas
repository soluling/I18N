{
  This sample shows how not only to localize user interface but database too.

  The sample also shows how to show only those rows that belong to the active
  language.
}

unit Unit1;

interface

uses
  Classes, Controls, Menus, Grids, DB, DBGrids, ADODB, Forms;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    DatabaseMenu: TMenuItem;
    OpenMenu: TMenuItem;
    CloseMenu: TMenuItem;
    N1: TMenuItem;
    LanguageMenu: TMenuItem;
    N2: TMenuItem;
    ExitMenu: TMenuItem;
    HelpMenu: TMenuItem;
    AboutMenu: TMenuItem;
    DBGrid1: TDBGrid;
    Connection1: TADOConnection;
    Query1: TADOQuery;
    DataSource1: TDataSource;
    procedure FormShow(Sender: TObject);
    procedure DatabaseMenuClick(Sender: TObject);
    procedure OpenMenuClick(Sender: TObject);
    procedure CloseMenuClick(Sender: TObject);
    procedure LanguageMenuClick(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure AboutMenuClick(Sender: TObject);

  private
    procedure UpdateItems;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses
  SysUtils, Dialogs, NtBase, NtDatabaseUtils, NtLanguageDlg, NtLocalization,
  NtBaseTranslator, NtTranslator;

procedure TMainForm.UpdateItems;

  function GetLanguageCode: String;
  begin
    if LoadedResourceLocale = '' then
      Result := 'en'
    else
      Result := TNtLocale.LocaleToIso639(TNtLocale.ExtensionToLocale(LoadedResourceLocale));
  end;

resourcestring
  SName = 'Name';
  SFieldplayers = 'Fieldplayers';
  SGoalie = 'Goalie';
  SOrigin = 'Origin';
  SDescription = 'Description';
begin
  Connection1.Connected := True;

  Query1.Close;
  Query1.SQL.Text := Format('SELECT * FROM Sport WHERE Lang=''%s''', [GetLanguageCode]);
  Query1.Open;

  TNtDatabaseUtils.HideField(Query1, 'Id');
  TNtDatabaseUtils.HideField(Query1, 'Lang');
  TNtDatabaseUtils.ShowField(Query1, 'Name', SName, 15);
  TNtDatabaseUtils.ShowField(Query1, 'Fieldplayers', SFieldplayers);
  TNtDatabaseUtils.ShowField(Query1, 'Goalie', SGoalie);
  TNtDatabaseUtils.ShowField(Query1, 'Origin', SOrigin, 15);
  TNtDatabaseUtils.ShowField(Query1, 'Description', SDescription, 100);

  DataSource1.DataSet := Query1;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  OpenMenuClick(Self);
end;

procedure TMainForm.DatabaseMenuClick(Sender: TObject);
begin
  OpenMenu.Enabled := not Query1.Active;
  CloseMenu.Enabled := Query1.Active;
end;

procedure TMainForm.OpenMenuClick(Sender: TObject);
begin
  UpdateItems;
end;

procedure TMainForm.CloseMenuClick(Sender: TObject);
begin
  Query1.Close;
end;

procedure TMainForm.LanguageMenuClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select('en', lnNative, [ldShowErrorIfNoDll], [roSaveLocale]) then
    UpdateItems;
end;

procedure TMainForm.ExitMenuClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AboutMenuClick(Sender: TObject);
resourcestring
  SAboutMessage = 'This application shows how to localize database content.';
begin
  ShowMessage(SAboutMessage);
end;

initialization
  NtEnabledProperties := STRING_TYPES;
end.
