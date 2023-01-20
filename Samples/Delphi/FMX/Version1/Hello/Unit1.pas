unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    RuntimeLabel: TLabel;
    procedure Button1Click(Sender: TObject);
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
{$R *.iPhone.fmx IOS}
{$R *.SmXhdpiPh.fmx ANDROID}

uses
  NtBase,
  NtResource,
  FMX.NtLocalization,
  FMX.NtTranslator;

procedure TForm1.FormCreate(Sender: TObject);
begin
  _T(Self);
  RuntimeLabel.Text := _T('This is a string', 'ThisIsString') + ' ' + TNtLocale.GetDefaultLanguage;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage(_T('Hello World', 'HelloWorld'));
end;

initialization
//  DefaultLocale := 'ja';
end.
