unit InheritedFra;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  BaseFra, FMX.Controls.Presentation, FMX.ListBox;

type
  TInheritedFrame = class(TBaseFrame)
    TrackBar1: TTrackBar;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  InheritedFrame: TInheritedFrame;

implementation

{$R *.fmx}

end.
