unit MainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Data.DB, Data.Win.ADODB, ppPrnabl, ppClass, ppCtrls, ppBands,
  ppCache, ppDB, ppDesignLayer, ppParameter, ppProd, ppReport, ppComm,
  ppRelatv, ppDBPipe, ppVar, ppRichTx, ppStrtch, ppMemo;

type
  TMainForm = class(TForm)
    Connection1: TADOConnection;
    DataSource1: TDataSource;
    Pipeline1: TppDBPipeline;
    Report1: TppReport;
    ppParameterList1: TppParameterList;
    ppDesignLayers1: TppDesignLayers;
    ppDesignLayer1: TppDesignLayer;
    ppHeaderBand1: TppHeaderBand;
    ppDetailBand1: TppDetailBand;
    ppFooterBand1: TppFooterBand;
    ppDBText1: TppDBText;
    PreviewButton: TButton;
    LanguageButton: TButton;
    Query1: TADOQuery;
    ppDBText3: TppDBText;
    procedure FormCreate(Sender: TObject);
    procedure PreviewButtonClick(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

  private
    procedure UpdateItems;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  NtBase, NtLocalization, NtLanguageDlg;

procedure TMainForm.UpdateItems;

  function GetLanguageCode: String;
  begin
    if LoadedResourceLocale = '' then
      Result := 'en'
    else
      Result := TNtLocale.LocaleToIso639(TNtLocale.ExtensionToLocale(LoadedResourceLocale));
  end;

begin
  Connection1.Connected := True;

  Query1.Close;
  Query1.SQL.Text := Format('SELECT * FROM Sport WHERE Lang=''%s''', [GetLanguageCode]);
  Query1.Open;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  UpdateItems;
end;

procedure TMainForm.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select('en', '', lnNative, [ldShowErrorIfNoDll], [roSaveLocale]) then
    UpdateItems;
end;

procedure TMainForm.PreviewButtonClick(Sender: TObject);
begin
  Report1.Print;
end;

end.
