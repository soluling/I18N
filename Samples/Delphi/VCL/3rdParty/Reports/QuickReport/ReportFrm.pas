unit ReportFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Data.DB, Data.Win.ADODB, QRCtrls, QuickRpt,
  QRDMBarcode;

type
  TReportForm = class(TForm)
    Connection1: TADOConnection;
    QuickRep1: TQuickRep;
    DetailBand1: TQRBand;
    DescriptionText: TQRDBText;
    NameText: TQRDBText;
    Query1: TADOQuery;
    procedure FormCreate(Sender: TObject);

  public
    procedure UpdateItems;
  end;

var
  ReportForm: TReportForm;

implementation

{$R *.dfm}

uses
  NtBase, NtLocalization;

procedure TReportForm.UpdateItems;

  function GetLanguageCode: String;
  begin
    if LoadedResourceLocale = '' then
      Result := 'en'
    else
      Result := TNtLocale.LocaleToIso639(TNtLocale.ExtensionToLocale(LoadedResourceLocale));
  end;

begin
  ReportForm.Connection1.Connected := True;

  ReportForm.Query1.Close;
  ReportForm.Query1.SQL.Text := Format('SELECT * FROM Sport WHERE Lang=''%s''', [GetLanguageCode]);
  ReportForm.Query1.Open;
end;

procedure TReportForm.FormCreate(Sender: TObject);
begin
  UpdateItems;
end;

end.
