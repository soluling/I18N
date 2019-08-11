{
  You can get free TVirtualStringTree from http://www.jam-software.com/virtual-treeview/
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VirtualTrees;

type
  TForm1 = class(TForm)
    VirtualStringTree1: TVirtualStringTree;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtVirtualTreeTranslator,
  NtLanguageDlg;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TNtLanguageDialog.Select('en');
end;

end.
