unit AirplaneFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Imaging.pngimage, ImageFrm;

type
  TAirplaneForm = class(TImageForm)
    ComboBox1: TComboBox;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AirplaneForm: TAirplaneForm;

implementation

{$R *.dfm}

procedure TAirplaneForm.FormCreate(Sender: TObject);
resourcestring
  SText = 'Airplane';
  SDescription = 'This is am airplane. It is very fast way to travel.';
begin
  TextLabel.Caption := SText;
  DescriptionLabel.Caption := SDescription;
end;

end.
