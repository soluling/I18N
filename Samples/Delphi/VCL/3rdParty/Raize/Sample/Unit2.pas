unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids,
  RzDBGrid, Vcl.DBCtrls, RzDBCmbo, Vcl.StdCtrls, RzCmboBx, RzLstBox, RzDBList,
  Vcl.ComCtrls, RzDTP, RzDBDTP, RzTrkBar, RzDBTrak, RzSpnEdt, RzDBSpin,
  RzDBEdit, RzDBBnEd, Vcl.Mask, RzEdit, RzDBRGrp, Vcl.ExtCtrls, RzPanel,
  RzRadGrp;

type
  TForm2 = class(TForm)
    RzDBRadioGroup1: TRzDBRadioGroup;
    RzDBCheckBoxGroup1: TRzDBCheckBoxGroup;
    RzDBEdit1: TRzDBEdit;
    RzDBButtonEdit1: TRzDBButtonEdit;
    RzDBDateTimeEdit1: TRzDBDateTimeEdit;
    RzDBNumericEdit3: TRzDBNumericEdit;
    RzDBSpinEdit1: TRzDBSpinEdit;
    RzDBExpandEdit1: TRzDBExpandEdit;
    RzDBMemo1: TRzDBMemo;
    RzDBRichEdit1: TRzDBRichEdit;
    RzDBSpinner1: TRzDBSpinner;
    RzDBTrackBar1: TRzDBTrackBar;
    RzDBDateTimePicker1: TRzDBDateTimePicker;
    RzDBListBox1: TRzDBListBox;
    RzDBComboBox1: TRzDBComboBox;
    RzDBComboBox2: TRzDBComboBox;
    RzDBLookupComboBox1: TRzDBLookupComboBox;
    RzDBGrid1: TRzDBGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

end.
