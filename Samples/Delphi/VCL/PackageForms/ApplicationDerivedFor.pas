unit ApplicationDerivedFor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, BaseFor;

type
  TDerivedForm = class(TBaseForm)
    Button1: TButton;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TDerivedForm.FormCreate(Sender: TObject);
resourcestring
  SStr = 'Derived application string';
begin
  inherited;
  Label4.Caption := SStr;
end;

end.
