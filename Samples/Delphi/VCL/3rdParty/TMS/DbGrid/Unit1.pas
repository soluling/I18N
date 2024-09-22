unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvUtil, Vcl.Grids, AdvObj, DBAdvGrid,
  BaseGrid, AdvGrid, Data.DB, Data.Win.ADODB;

type
  TForm1 = class(TForm)
    DBAdvGrid1: TDBAdvGrid;
    Connection1: TADOConnection;
    Query1: TADOQuery;
    DataSource1: TDataSource;
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
