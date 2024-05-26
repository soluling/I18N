unit SampleFor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, SampleFra;

type
  TSampleForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SampleFrame1: TSampleFrame;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SampleForm: TSampleForm;

implementation

{$R *.dfm}

uses
  Consts,
  SampleConsts;

procedure TSampleForm.FormCreate(Sender: TObject);
resourcestring
  SMsg = 'Resource string';
begin
  // See comments in Unit1.pas
  Label2.Caption := SMsg;
  Label3.Caption := SampleConsts.SSampleString;
  Label4.Caption := Consts.SMsgDlgInformation;
end;

end.
