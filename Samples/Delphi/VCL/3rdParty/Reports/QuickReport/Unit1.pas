unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, QRCtrls, QuickRpt, QRXLSXFilt,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    QuickRep1: TQuickRep;
    QRPreviewController1: TQRPreviewController;
    QuickAbstractRep1: TQuickAbstractRep;
    QRSubDetail1: TQRSubDetail;
    ColumnHeaderBand1: TQRBand;
    DetailBand1: TQRBand;
    PageFooterBand1: TQRBand;
    PageHeaderBand1: TQRBand;
    SummaryBand1: TQRBand;
    TitleBand1: TQRBand;
    QRSubDetail2: TQRSubDetail;
    QRLoopBand1: TQRLoopBand;
    QRStringsBand1: TQRStringsBand;
    QRBand1: TQRBand;
    QRWildBand1: TQRWildBand;
    QRChildBand1: TQRChildBand;
    QRGroup1: TQRGroup;
    QRLabel1: TQRLabel;
    QRDBText1: TQRDBText;
    QRExpr1: TQRExpr;
    QRSysData1: TQRSysData;
    QRXMLSSFilter1: TQRXMLSSFilter;
    QRCompositeReport1: TQRCompositeReport;
    QRDBImage1: TQRDBImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
