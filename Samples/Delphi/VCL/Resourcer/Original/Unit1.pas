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
    function Process1(const value: String; default: String = 'Default'): String;
    function Process2(const value: String; const default: String = 'Default'): String;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Unit2;

function TForm1.Process1(const value: String; default: String): String;
begin
  Result := 'One' + sLineBreak + 'Two';
  Result := value + sLineBreak + 'Two';
  Result := value + sLineBreak + default;
end;

function TForm1.Process2(const value: String; const default: String): String;
begin
  Result := '';

  for var c in value do
  begin
    if c < #32 then
      Result := Result + Format('#%d', [Ord(c)])
  end;
end;

procedure Process3(const default: String = 'Another');
begin
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Label1.Caption := 'This is a sample';
  Label2.Caption := 'John'; // Do not localize
  Label3.Caption := 'First line' + sLineBreak + 'Second line';

  Label4.Caption := GetValue;
  Label5.Caption := GetValue + ' World';

  Process1('');
  Process2('');
  Process3('');
end;

end.
