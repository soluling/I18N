unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ImgList, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    Image1: TImage;
    NextButton: TButton;
    Label1: TLabel;
    procedure NextButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    FCurrentImage: Integer;

    procedure UpdateImage;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.UpdateImage;
resourcestring
  SBook = 'Book';
  SSign = 'Sign';
  SPlane = 'Plane';
var
  bitmap: TBitmap;
begin
  // Update text
  case FCurrentImage of
    0: Label1.Caption := SBook;
    1: Label1.Caption := SSign;
    2: Label1.Caption := SPlane;
  else
    Label1.Caption := '';
  end;

  // Update image
  bitmap := TBitmap.Create;
  try
    ImageList1.GetBitmap(FCurrentImage, bitmap);
    Image1.Picture.Graphic := bitmap;
  finally
    bitmap.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateImage;
end;

procedure TForm1.NextButtonClick(Sender: TObject);
begin
  FCurrentImage := (FCurrentImage + 1) mod ImageList1.Count;
  UpdateImage;
end;

end.
