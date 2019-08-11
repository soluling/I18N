unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, RzTreeVw, RzListVw,
  RzGroupBar, RzSplit, RzTabs, RzPanel, Vcl.ExtCtrls, RzButton,
  System.ImageList, Vcl.ImgList, RzChkLst, RzLstBox, Vcl.StdCtrls, RzDTP,
  RzTrkBar, RzSpnEdt, RzEdit, RzBtnEdt, Vcl.Mask, RzStatus;

type
  TForm1 = class(TForm)
    RzCheckTree1: TRzCheckTree;
    RzTreeView1: TRzTreeView;
    RzListView1: TRzListView;
    RzPanel1: TRzPanel;
    RzGroupBar1: TRzGroupBar;
    RzToolbar1: TRzToolbar;
    RzPageControl1: TRzPageControl;
    TabSheet1: TRzTabSheet;
    TabSheet2: TRzTabSheet;
    RzTabControl1: TRzTabControl;
    RzSplitter1: TRzSplitter;
    RzGroup1: TRzGroup;
    RzGroup2: TRzGroup;
    ImageList1: TImageList;
    RzToolButton2: TRzToolButton;
    RzStatusBar1: TRzStatusBar;
    RzStatusPane1: TRzStatusPane;
    RzGlyphStatus1: TRzGlyphStatus;
    RzEdit1: TRzEdit;
    RzMaskEdit1: TRzMaskEdit;
    RzButtonEdit1: TRzButtonEdit;
    RzDateTimeEdit1: TRzDateTimeEdit;
    RzNumericEdit1: TRzNumericEdit;
    RzSpinEdit1: TRzSpinEdit;
    RzColorEdit1: TRzColorEdit;
    RzExpandEdit1: TRzExpandEdit;
    RzHotKeyEdit1: TRzHotKeyEdit;
    RzMemo1: TRzMemo;
    RzRichEdit1: TRzRichEdit;
    RzSpinner1: TRzSpinner;
    RzTrackBar1: TRzTrackBar;
    RzDateTimePicker1: TRzDateTimePicker;
    RzListBox1: TRzListBox;
    RzRankListBox1: TRzRankListBox;
    RzTabbedListBox1: TRzTabbedListBox;
    RzCheckList1: TRzCheckList;
    RzVersionInfoStatus1: TRzVersionInfoStatus;
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
