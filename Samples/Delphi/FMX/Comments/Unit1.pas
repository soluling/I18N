unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.StdCtrls, FMX.Dialogs, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  NtBase,
  NtResource,
  NtResourceString,
  FMX.NtTranslator;

procedure TForm1.FormCreate(Sender: TObject);
const
  NAME_C = 'Bill';
  PLACE_C = 'Helsinki';
resourcestring
  SHelloWorld = 'Hello World';  //loc This is a comment
  SCharacters = 'Characters';  //loc MaxChars=20 This is characters comment
  SPixels = 'Pixels';  //loc MaxPixels=100 This is pixels comment

  // Same pattern string three times.
  // First without a comment, then with comment but without placeholder description.
  // Finally with proper descriptions.
  SHello1 = 'Hello %s, welcome to %s';
  SHello2 = 'Hello %s, welcome to %s';  //loc This is comment but no placeholder descriptions
  SHello3 = 'Hello %s, welcome to %s';  //loc 0: Name of the person, 1: Name of the place
begin
  _T(Self);

  Label1.Text := SHelloWorld;
  Label2.Text := SCharacters;
  Label3.Text := SPixels;

  Label4.Text := Format(SHello1, [NAME_C, PLACE_C]);
  Label5.Text := Format(SHello2, [NAME_C, PLACE_C]);
  Label6.Text := Format(SHello3, [NAME_C, PLACE_C]);
end;

initialization
  //DefaultLocale := 'fi';
end.
