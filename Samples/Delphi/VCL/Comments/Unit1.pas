unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
const
  NAME_C = 'Bill';
  PLACE_C = 'Helsinki';
resourcestring
  SHello = 'Hello World'; //loc This is a comment
  SCharacters = 'Characters'; //loc MaxChars=20 This is characters comment
  SPixels = 'Pixels'; //loc MaxPixels=100 This is pixels comment
  SFormatNoComment = 'Hello %s, welcome to %s';
  SFormatComment = 'Hello %s, welcome to %s'; //loc This is comment but no placeholder descriptions
  SFormatPlaceholderComment = 'Hello %s, welcome to %s'; //loc 0: Name of the person, 1: Name of the place
  SRegExComment = 'EXPRESSION_100'; //loc RegEx="[A-Z0-9_]*" This can only contains A to Z, numbers and underscore
  SIgnore = 'Ignore this'; //noloc
begin
  Label1.Caption := SHello;
  Label2.Caption := SCharacters;
  Label3.Caption := SPixels;
  Label4.Caption := Format(SFormatNoComment, [NAME_C, PLACE_C]);
  Label5.Caption := Format(SFormatComment, [NAME_C, PLACE_C]);
  Label6.Caption := Format(SFormatPlaceholderComment, [NAME_C, PLACE_C]);
  Label7.Caption := SRegExComment;
  Label8.Caption := SIgnore;
end;

end.
