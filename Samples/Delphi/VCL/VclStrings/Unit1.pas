unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    ClickButton: TButton;
    procedure ClickButtonClick(Sender: TObject);
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
  UITypes;

procedure TForm1.ClickButtonClick(Sender: TObject);
resourcestring
  SStr = 'This is a sample';
begin
  // This function uses four VCL strings: Information, Yes, No, and Cancel
  MessageDlg(SStr, mtInformation, [mbOk, mbNo, mbCancel], 0);
end;

end.
