unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Image2: TImage;
    Label2: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);

  private
    procedure UpdateItems;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtBase,
  NtGraphic,
  NtLanguageDlg;

procedure TForm1.UpdateItems;
var
  png: TPngImage;
begin
  // Set the PNG image and assign it to Image2
  png := TPngImage.Create;
  try
    // A helper from NtGraphics.pas. Load image from custom resource (added through Images.rc file)
    png.LoadResource('IMAGE', 'Car');
    Image2.Picture.Graphic := png;
  finally
    png.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateItems;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Show a language select dialog and turn on the selected language
  if TNtLanguageDialog.Select('en', '', lnBoth) then
  begin
    // Language has been changed.
    // Properties that were set on run time must be reset.
    UpdateItems;
  end;
end;

end.
