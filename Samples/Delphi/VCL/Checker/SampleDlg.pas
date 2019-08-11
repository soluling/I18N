unit SampleDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, BaseFrm;

type
  TSampleDialog = class(TBaseForm)
    ComboBox1: TComboBox;
    ListBox1: TListBox;
    Memo1: TMemo;
    OkButton: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SampleDialog: TSampleDialog;

implementation

{$R *.dfm}

end.
