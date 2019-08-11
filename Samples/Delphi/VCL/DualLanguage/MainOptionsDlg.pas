unit MainOptionsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BaseDlg, Vcl.ExtCtrls;

type
  TMainOptionsDialog = class(TBaseDialog)
    RadioGroup1: TRadioGroup;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainOptionsDialog: TMainOptionsDialog;

implementation

{$R *.dfm}

end.
