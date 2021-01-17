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
    QueryId: TIntegerField;
    QueryName: TWideStringField;
    Query1FieldPlayers: TIntegerField;
    Query1Goalie: TBooleanField;
    Query1Origin: TWideStringField;
    Query1Description: TWideStringField;
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
  SysUtils,
  Dialogs,
  NtBase,
  NtLanguageDlg,
  NtLocalization,
  NtBaseTranslator,
  NtTranslator;

procedure TMainForm.UpdateItems;
begin
  Query1.Active := True;
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
  if TNtLanguageDialog.Select('en') then
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
