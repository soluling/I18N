unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TreeView, FMX.Layouts, FMX.ListView.Types, FMX.ListView,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    ListView1: TListView;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  NtResource,
  FMX.NtTranslator;

procedure TForm1.FormCreate(Sender: TObject);

  procedure Add(const str: String);
  var
    item: TListViewItem;
  begin
    item := ListView1.Items.Add;
    item.Text := str;
  end;

begin
  _T(Self);

  Add(_T('One', 'SOne'));
  Add(_T('Two', 'STwo'));
  Add(_T('Three', 'SThree'));
end;

end.
