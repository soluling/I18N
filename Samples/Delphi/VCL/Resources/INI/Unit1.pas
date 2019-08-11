{
  This sample shows how to store INI data into a custom resource,
  how to use them and how to localize them.

  INI resources are added by adding them into Custom.rc file and adding the rc file
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
    TextLabel: TLabel;
    TextMemo: TMemo;
    TextPreviousButton: TButton;
    TextNextButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TextPreviousButtonClick(Sender: TObject);
    procedure TextNextButtonClick(Sender: TObject);

  private
    FTexts: TResourceItems;

    procedure UpdateText;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

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
  NAME = 'INI';
begin
  FTexts := TResourceItems.Create;

  // Populate resource list
  FTexts.Add(NAME, 'Ansi');
  FTexts.Add(NAME, 'Utf8');
  FTexts.Add(NAME, 'Utf16');
  FTexts.Add(NAME, 'Utf16Be');

  UpdateText;
end;

procedure TForm1.FormResize(Sender: TObject);
const
  GAP = 8;
begin
  TextPreviousButton.Left := TextMemo.Left + (TextMemo.Width - TextPreviousButton.Width - TextNextButton.Width - GAP) div 2;
  TextNextButton.Left := TextPreviousButton.Left + TextPreviousButton.Width + GAP;
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
