{
  A sample that shows how to perform a runtime language switch.
  The sample also show how to use extensions to enable language switch
  for untrivial data types such as image, tree views, and list view.
}

unit Unit1;

interface

uses
  Classes, Graphics, StdCtrls, Controls, ComCtrls, ExtCtrls, Forms, pngimage;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    TreeView1: TTreeView;
    Label2: TLabel;
    ListView1: TListView;
    ListBox1: TListBox;
    Image1: TImage;
    LanguageButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

  private
    procedure UpdateItems;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtBase,
  NtPictureTranslator,   // Enables runtime language switch of TImage.Picture
  NtListViewTranslator,  // Enables runtime language switch of TListView
  NtTreeViewTranslator,  // Enables runtime language switch of TTreeView
  NtLanguageDlg;

// This procedure initializes the properties that are set on run time
procedure TForm1.UpdateItems;
resourcestring
  SSample = 'This is another sample';
begin
  Label2.Caption := SSample;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // Set the properties for first time
  UpdateItems;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  // Show a language select dialog and turn on the selected language
  if TNtLanguageDialog.Select('en', '', lnBoth) then
  begin
    // Language has been changed.
    // Properties that were set on run time must be reset.
    UpdateItems;
  end;
end;

end.
