unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons, Data.DB, Data.Win.ADODB, SportFrm;

type
  TForm1 = class(TSportForm)
    Connection1: TADOConnection;
    Query1: TADOQuery;

  protected
    procedure LoadSports; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.LoadSports;
begin
  // Load sport data from a database
  Connection1.Connected := True;

  Query1.Close;
  Query1.SQL.Text := Format('SELECT * FROM Sport WHERE Lang=''%s''', [ActiveLanguage]);
  Query1.Open;

  FSports.LoadDatabase(Query1);
end;

end.
