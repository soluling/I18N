unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DefaultTranslator;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

uses
  Unit2;

resourcestring
  SHello = 'Hello World';
  SSample = 'This is a %s sample.';

procedure TForm1.FormCreate(Sender: TObject);
begin
  Label2.Caption := SHello;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage(Format(SSample, ['Lazarus']));
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  form: TForm2;
begin
  form := TForm2.Create(nil);
  form.Show;
end;

end.

