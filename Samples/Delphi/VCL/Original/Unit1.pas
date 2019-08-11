unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
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
  NtLanguageDlg;

procedure TForm1.FormCreate(Sender: TObject);
resourcestring
  STwo = 'Two';
begin
  Label2.Caption := STwo;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if TNtLanguageDialog.Select('eno', 'Original English') then
    FormCreate(Self);
end;

end.
