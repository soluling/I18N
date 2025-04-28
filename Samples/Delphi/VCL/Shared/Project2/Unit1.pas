unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    ShowButton: TButton;
    Button1: TButton;
    procedure ShowButtonClick(Sender: TObject);
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
  SharedFrm,
  SharedInheritedFrm;

procedure TForm1.ShowButtonClick(Sender: TObject);
begin
  var form := TSharedForm.Create(nil);
  form.Show;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  var form := TSharedInheritedForm.Create(nil);
  form.Show;
end;

end.
