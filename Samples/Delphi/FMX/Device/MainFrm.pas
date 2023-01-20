unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, BaseFra,
  FMX.Controls.Presentation, FMX.StdCtrls, InheritedFra;

type
  TMainForm = class(TForm)
    Label1: TLabel;
    BaseFrame1: TBaseFrame;
    InheritedFrame1: TInheritedFrame;
    Button1: TButton;
    CheckBox1: TCheckBox;
    ShowButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ShowButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

uses
  NtBase,
  NtResource,
  NtResourceString,
  FMX.NtTranslator,
  InheritedFrm;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  InheritedForm.Show;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  _T(Self);
end;

procedure TMainForm.ShowButtonClick(Sender: TObject);
begin
  InheritedForm.Show;
end;

initialization
  //DefaultLocale := 'fi';
end.
