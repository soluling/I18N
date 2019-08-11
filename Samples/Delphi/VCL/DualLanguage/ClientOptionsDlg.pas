unit ClientOptionsDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseDlg, Vcl.StdCtrls;

type
  TClientOptionsDialog = class(TBaseDialog)
    CheckBox1: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ClientOptionsDialog: TClientOptionsDialog;

implementation

{$R *.dfm}

end.
