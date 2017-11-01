{
  This application shows how to localize package files (.bpl).

  The main application Project1.exe uses two packages:
  - a custom package SamplePackage.bpl
  - a standard VCL package vcl250.bpl (Delphi 10.2 Tokyo)

  SamplePacakge.bpl contains a form (TSampleForm), a frame (TSampleFrame) and
  a resource string (SampleConsts.SSampleString).

  Main form contaisn four labels:
  1) Label1 contains a desing time string
  2) Label2 is set on run time to hold a local resource string value (comes from Project1.exe or Project1.?? resource DLL)
  3) Label3 is set on run time to hold a resource string value of SamplePackage package (comes from SamplePackage.bpl or SamplePacakge.?? resource DLL)
  4) Label4 is set on run time to hold a VCL resource string value (comes from vcl230.bpl or vcl230.?? resource DLL)

  GroupBox1 contains a package frame.

  If you click Button1 a package form will be shown.
}

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    GroupBox1: TGroupBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    FFrame: TFrame;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Consts,
  SampleConsts,
  SampleFra,
  SampleFor;

procedure TForm1.FormCreate(Sender: TObject);
resourcestring
  SMsg = 'Resource string';
begin
  Label2.Caption := SMsg;                        // Local resource string
  Label3.Caption := SampleConsts.SSampleString;  // Custom package resource string
  Label4.Caption := Consts.SMsgDlgInformation;   // VCL package resource string

  FFrame := TSampleFrame.Create(nil);
  FFrame.Parent := GroupBox1;
  FFrame.Left := 8;
  FFrame.Top := 16;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  form: TSampleForm;
begin
  form := TSampleForm.Create(nil);
  try
    form.ShowModal;
  finally
    form.Free;
  end;
end;

end.
