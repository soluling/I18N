unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    PlainLabel: TLabel;
    HtmlLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
resourcestring
  SPlain = 'Hello World';
  SHtml = '<p>This is a <b>small</b> sample.</p>';
begin
  PlainLabel.Caption := SPlain;
  HtmlLabel.Caption := SHtml;
end;

end.
