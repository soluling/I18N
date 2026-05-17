unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
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

uses
  Unit2;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Label1.Caption := 'This is a sample';
  Label2.Caption := 'John'; // Do not localize
  Label3.Caption := 'First line' + sLineBreak + 'Second line';

  Label4.Caption := GetValue;
  Label5.Caption := GetValue + ' World';
end;

end.
