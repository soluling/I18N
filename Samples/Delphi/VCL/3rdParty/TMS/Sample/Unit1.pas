unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvGlowButton, AdvOfficeButtons,
  AdvOfficePager, Vcl.StdCtrls, AdvEdit, AdvLabel, AdvPageControl, Vcl.ComCtrls,
  DBAdvEd, Vcl.ExtCtrls, AdvPanel, TreeList, Vcl.Mask, AdvMEdBtn, EditBtn,
  AdvEdBtn, AdvMemo, AdvUtil, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, PlannerCal,
  Planner, PlannerMonthView, DBPlannerMonthView, AdvDBDateTimePicker,
  AdvDateTimePicker, datelbl, AdvGlassButton, AdvDBLookupComboBox, AdvCardList,
  AdvBadge;

type
  TForm1 = class(TForm)
    AdvPageControl1: TAdvPageControl;
    AdvTabSheet1: TAdvTabSheet;
    AdvTabSheet2: TAdvTabSheet;
    Sample: TAdvLabel;
    AdvEdit1: TAdvEdit;
    AdvOfficePager1: TAdvOfficePager;
    AdvOfficePager11: TAdvOfficePage;
    AdvOfficePager12: TAdvOfficePage;
    AdvOfficePager13: TAdvOfficePage;
    AdvOfficeCheckBox1: TAdvOfficeCheckBox;
    AdvGlowButton1: TAdvGlowButton;
    AdvLabel1: TAdvLabel;
    AdvPanel1: TAdvPanel;
    DBAdvEdit1: TDBAdvEdit;
    UnitAdvEditBtn1: TUnitAdvEditBtn;
    UnitEditBtn1: TUnitEditBtn;
    UniTAdvMaskEditBtn1: TUniTAdvMaskEditBtn;
    AdvMemo1: TAdvMemo;
    TreeView1: TTreeView;
    TreeList1: TTreeList;
    Memo1: TMemo;
    ListView1: TListView;
    AdvStringGrid1: TAdvStringGrid;
    PlannerCalendar1: TPlannerCalendar;
    MonthCalendar1: TMonthCalendar;
    DateLabel1: TDateLabel;
    AdvDateTimePicker1: TAdvDateTimePicker;
    AdvDBDateTimePicker1: TAdvDBDateTimePicker;
    DBPlannerMonthView1: TDBPlannerMonthView;
    AdvGlassButton1: TAdvGlassButton;
    AdvGlassButton2: TAdvGlassButton;
    AdvDBLookupComboBox1: TAdvDBLookupComboBox;
    AdvBadgeLabel1: TAdvBadgeLabel;
    AdvCardList1: TAdvCardList;
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
