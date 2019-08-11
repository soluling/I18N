{
  This sample shows how to store images and text data into a custom resource,
  how to use them and how to localize them.

  Resources are added by adding them into Custom.rc file and adding the rc file
  into the project.
}
unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Data;

type
  TForm1 = class(TForm)
    ImageLabel: TLabel;
    Image: TImage;
    ImagePreviousButton: TButton;
    ImageNextButton: TButton;
    TextLabel: TLabel;
    TextMemo: TMemo;
    TextPreviousButton: TButton;
    TextNextButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ImagePreviousButtonClick(Sender: TObject);
    procedure ImageNextButtonClick(Sender: TObject);
    procedure TextPreviousButtonClick(Sender: TObject);
    procedure TextNextButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private
    FImages: TResourceItems;
    FTexts: TResourceItems;

    procedure UpdateImage;
    procedure UpdateText;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Vcl.Imaging.pngimage,
  NtGraphics;

procedure TForm1.UpdateImage;
resourcestring
  SImageCaption = '%0:d/%1:d image resource: %2:s-%3:s';  // loc Image resource label. \n %0:d is the index of the current image. \n %1:d is the total image count. \n %2:s is the resource type. \n %3:s is the resource name.
var
  png: TPngImage;
  resource: TResourceItem;
begin
  resource := FImages.Current;
  ImageLabel.Caption := Format(SImageCaption, [FImages.Index + 1, FImages.Count, resource.ResourceType, resource.ResourceName]);

  png := TPngImage.Create;
  try
    png.LoadResource(resource.ResourceType, resource.ResourceName);
    Image.Picture.Graphic := png;
  finally
    png.Free;
  end;
end;

procedure TForm1.UpdateText;
resourcestring
  STextCaption = '%0:d/%1:d text resource: %2:s-%3:s';  // loc Text resource label. \n %0:d is the index of the current text. \n %1:d is the total text count. \n %2:s is the resource type. \n %3:s is the resource name.
var
  stream: TResourceStream;
  resource: TResourceItem;
begin
  resource := FTexts.Current;
  TextLabel.Caption := Format(STextCaption, [FTexts.Index + 1, FTexts.Count, resource.ResourceType, resource.ResourceName]);

  stream := resource.GetStream;
  try
    TextMemo.Lines.LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  SPORT = 'SPORT';
begin
  FImages := TResourceItems.Create;
  FTexts := TResourceItems.Create;

  // Populate image resource list
  FImages.Add(SPORT, 'Image');
  FImages.Add('IMAGE', 'Sport');

  // Populate text resource list
  FTexts.Add(SPORT, 'Text');
  FTexts.Add(SPORT, 'JSON');
  FTexts.Add(SPORT, 'XML');
  FTexts.Add(SPORT, 'Ini');
  FTexts.Add(SPORT, 'SVG');
  FTexts.Add(SPORT, 'HTML');
  FTexts.Add(SPORT, 'Script');
  FTexts.Add('XML', 'Sport');
  FTexts.Add('TEXT', 'Sport');

  UpdateImage;
  UpdateText;
end;

procedure TForm1.FormResize(Sender: TObject);
const
  GAP = 8;
begin
  ImagePreviousButton.Left := Image.Left + (Image.Width - ImagePreviousButton.Width - ImageNextButton.Width - GAP) div 2;
  ImageNextButton.Left := ImagePreviousButton.Left + ImagePreviousButton.Width + GAP;

  TextPreviousButton.Left := TextMemo.Left + (TextMemo.Width - TextPreviousButton.Width - TextNextButton.Width - GAP) div 2;
  TextNextButton.Left := TextPreviousButton.Left + TextPreviousButton.Width + GAP;
end;

procedure TForm1.ImagePreviousButtonClick(Sender: TObject);
begin
  FImages.Previous;
  UpdateImage;
end;

procedure TForm1.ImageNextButtonClick(Sender: TObject);
begin
  FImages.Next;
  UpdateImage;
end;

procedure TForm1.TextPreviousButtonClick(Sender: TObject);
begin
  FTexts.Previous;
  UpdateText;
end;

procedure TForm1.TextNextButtonClick(Sender: TObject);
begin
  FTexts.Next;
  UpdateText;
end;

end.
