unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    RedLabel: TLabel;
    RedEdit: TEdit;
    CalculateButton: TButton;
    ResetButton: TButton;
    GreenLabel: TLabel;
    GreenEdit: TEdit;
    BlueLabel: TLabel;
    BlueEdit: TEdit;
    ValueLabel: TLabel;
    CloseButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure CalculateButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);

  private
    procedure UpdateValues;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.UpdateValues;
resourcestring
  SValues = 'Values: %s';
  SNotAvailable = 'N/A';
var
  str: String;
begin
  if (RedEdit.Text <> '') or (GreenEdit.Text <> '') or (BlueEdit.Text <> '') then
    str := RedEdit.Text + ':' + GreenEdit.Text + ':' + BlueEdit.Text
  else
    str := SNotAvailable;

  ValueLabel.Caption := Format(SValues, [str])
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateValues;
end;

procedure TForm1.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.EditChange(Sender: TObject);
begin
  UpdateValues;
end;

procedure TForm1.CalculateButtonClick(Sender: TObject);
begin
  UpdateValues;
end;

procedure TForm1.ResetButtonClick(Sender: TObject);
begin
  RedEdit.Text := '';
  GreenEdit.Text := '';
  BlueEdit.Text := '';
end;

end.
