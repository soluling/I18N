program Device;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  BaseFrm in 'BaseFrm.pas' {BaseForm},
  InheritedFrm in 'InheritedFrm.pas' {InheritedForm},
  BaseFra in 'BaseFra.pas' {BaseFrame: TFrame},
  InheritedFra in 'InheritedFra.pas' {InheritedFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TBaseForm, BaseForm);
  Application.CreateForm(TInheritedForm, InheritedForm);
  Application.Run;
end.
