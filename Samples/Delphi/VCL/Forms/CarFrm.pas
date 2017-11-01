unit CarFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls, ImageFrm;

type
  TCarForm = class(TImageForm)
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CarForm: TCarForm;

implementation

{$R *.dfm}

procedure TCarForm.FormCreate(Sender: TObject);
resourcestring
  SText = 'Car';  //loc Can title
  SDescription = 'This is a car. It is the most common way to move.'; // Car description. This wraps to multiple lines
begin
  TextLabel.Caption := SText;
  DescriptionLabel.Caption := SDescription;
end;

end.
