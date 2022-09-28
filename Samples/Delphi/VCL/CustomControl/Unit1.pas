unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MyLabel;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    MyLabel1: TMyLabel;
    Memo1: TMemo;
    MyMemo1: TMyMemo;
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
