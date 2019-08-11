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

end.
