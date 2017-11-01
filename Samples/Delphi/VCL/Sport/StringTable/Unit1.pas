unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons, SportFrm;

type
  TForm1 = class(TSportForm)
  protected
    procedure LoadSports; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Sport;

procedure TForm1.LoadSports;
begin
  // Load sport data from a string table resource
  FSports.LoadStringTable;
end;

end.
