unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.ListBox,
  FMX.TreeView, FMX.StdCtrls, FMX.Controls.Presentation, FMX.TabControl;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    ComboBox1: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    TreeView1: TTreeView;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    Label1: TLabel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Label2: TLabel;
    CheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);

  private
    procedure UpdateStrings;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  NtBase,
  NtBaseTranslator,
  NtResource,
  NtResourceString,
  FMX.NtLanguageDlg,
  FMX.NtTranslator;

procedure TForm1.UpdateStrings;
resourcestring
  SHello = 'Hello world';
begin
  Label1.Text := SHello;
end;

procedure TForm1.FormCreate(Sender: TObject);
resourcestring
  SEnglish = 'English';
  SFinnish = 'Finnish';
  SGerman = 'German';
  SFrench = 'French';
  SJapanese = 'Japanese';
begin
  NtResources.Add('English', 'English', SEnglish, 'en');
  NtResources.Add('Finnish', 'suomi', SFinnish, 'fi');
  NtResources.Add('German', 'Deutsch', SGerman, 'de');
  NtResources.Add('French', 'français', SFrench, 'fr');
  NtResources.Add('Japanese', '日本語', SJapanese, 'ja');

  _T(Self);
  UpdateStrings;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if TNtLanguageDialog.Select('en', lnBoth) then
    UpdateStrings;
end;

initialization
  NtEnabledProperties := STRING_TYPES;
end.
