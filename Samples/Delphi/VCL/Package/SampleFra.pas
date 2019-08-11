unit SampleFra;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TSampleFrame = class(TFrame)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Label2: TLabel;

  public
    constructor Create(owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  SampleConsts;

constructor TSampleFrame.Create(owner: TComponent);
begin
  inherited;
  // See comments in Unit1.pas
  Label2.Caption := SampleConsts.SSampleString;
end;

end.
