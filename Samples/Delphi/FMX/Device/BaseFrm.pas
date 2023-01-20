unit BaseFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TBaseForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BaseForm: TBaseForm;

implementation

{$R *.fmx}

uses
  FMX.NtTranslator;

procedure TBaseForm.FormCreate(Sender: TObject);
begin
  // When you call _T in the base form you do not have to call it in the inherited forms
  _T(Self);
end;

end.
