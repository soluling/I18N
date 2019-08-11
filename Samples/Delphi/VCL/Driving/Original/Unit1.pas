unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ActnList, Menus, pngimage, System.Actions;

type
  TForm1 = class(TForm)
    DistanceLabel: TLabel;
    DistanceEdit: TEdit;
    SpeedLabel: TLabel;
    SpeedEdit: TEdit;
    CalculateButton: TButton;
    CarImage: TImage;
    TimeLabel: TLabel;
    HoursLabel: TLabel;
    MinutesLabel: TLabel;
    FlagImage: TImage;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Calculate1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ActionList1: TActionList;
    CalculateAction: TAction;
    procedure FormCreate(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure CalculateActionExecute(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);

  private
    function GetDistance: Double;
    function GetSpeed: Double;

  public
    property Distance: Double read GetDistance;
    property Speed: Double read GetSpeed;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.GetDistance: Double;
begin
  Result := StrToFloatDef(DistanceEdit.Text, 0);
end;

function TForm1.GetSpeed: Double;
begin
  Result := StrToFloatDef(SpeedEdit.Text, 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EditChange(Self);
end;

procedure TForm1.EditChange(Sender: TObject);
begin
  CalculateAction.Enabled := (Distance > 0) and (Speed > 0);
end;

procedure TForm1.CalculateActionExecute(Sender: TObject);
var
  time: Double;
  hours, minutes: Integer;
begin
  if Speed = 0 then
    Exit;

  time := Distance/Speed;
  hours := Trunc(time);
  minutes := Round(60*(time - hours));

  HoursLabel.Caption := IntToStr(hours) + ' hours';
  MinutesLabel.Caption := IntToStr(minutes) + ' minutes';
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
  ShowMessage('Driving time calculator');
end;

end.
