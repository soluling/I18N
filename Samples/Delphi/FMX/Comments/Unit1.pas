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
  NtResource;

procedure TForm1.FormCreate(Sender: TObject);
const
  NAME_C = 'Bill';
  PLACE_C = 'Helsinki';
begin
  Label1.Text := _T('Hello World');  //loc This is a comment
  Label2.Text := _T('Characters');   //loc MaxChars=20 This is characters comment
  Label3.Text := _T('Pixels');       //loc MaxPixels=100 This is pixels comment

  // Same pattern string three times.
  // First without a comment, then with comment but without placeholder description.
  // Finally with proper descriptions.
  Label4.Text := Format(_T('Hello %s, welcome to %s'), [NAME_C, PLACE_C]);
  Label5.Text := Format(_T('Hello %s, welcome to %s'), [NAME_C, PLACE_C]);  //loc This is comment but no placeholder descriptions
  Label6.Text := Format(_T('Hello %s, welcome to %s'), [NAME_C, PLACE_C]);  //loc 0: Name of the person, 1: Name of the place
end;

end.
