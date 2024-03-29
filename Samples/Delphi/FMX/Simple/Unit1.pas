unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
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
resourcestring
  SSample = 'This is a sample';
begin
  // Translate form
  _T(Self);

  // Set property on runtime
  Label2.Text := SSample;  //loc This is a comment
end;

initialization
  //DefaultLocale := 'fi';
end.
