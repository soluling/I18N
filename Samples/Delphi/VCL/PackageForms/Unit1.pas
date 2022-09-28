unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    BaseButton: TButton;
    PackageDerivedButton: TButton;
    Label1: TLabel;
    ApplicationDerivedButton: TButton;
    procedure BaseButtonClick(Sender: TObject);
    procedure PackageDerivedButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ApplicationDerivedButtonClick(Sender: TObject);
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
  BaseFor,
  PackageDerivedFor,
  ApplicationDerivedFor;

procedure TForm1.BaseButtonClick(Sender: TObject);
begin
  var form := TBaseForm.Create(nil);
  form.Show;
end;

procedure TForm1.PackageDerivedButtonClick(Sender: TObject);
begin
  var form := PackageDerivedFor.TDerivedForm.Create(nil);
  form.Show;
end;

procedure TForm1.ApplicationDerivedButtonClick(Sender: TObject);
begin
  var form := ApplicationDerivedFor.TDerivedForm.Create(nil);
  form.Show;
end;

procedure TForm1.FormCreate(Sender: TObject);
resourcestring
  SStr = 'This is a sample';
begin
  Label1.Caption := SStr;
end;

end.
