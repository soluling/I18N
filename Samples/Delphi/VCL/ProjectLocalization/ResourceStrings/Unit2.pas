unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  NtTranslatorEx;

procedure TForm2.FormCreate(Sender: TObject);
resourcestring
  SOne = 'One';
  STwo = 'Two';
begin
  _T(Self);

  Label2.Caption := SOne;
  Label3.Caption := STwo;
end;

end.
