unit InheritedFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  BaseFrm, FMX.Edit, FMX.Controls.Presentation;

type
  TInheritedForm = class(TBaseForm)
    CheckBox1: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  InheritedForm: TInheritedForm;

implementation

{$R *.fmx}

end.
