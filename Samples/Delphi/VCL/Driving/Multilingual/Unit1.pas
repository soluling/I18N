unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ActnList, Menus, pngimage, Actions;

type
  TForm1 = class(TForm)
    DistanceLabel: TLabel;
    DistanceEdit: TEdit;
    SpeedLabel: TLabel;
    SpeedEdit: TEdit;
    CalculateButton: TButton;
    CarImage: TImage;
    FlagImage: TImage;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Calculate1: TMenuItem;
    Language1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ActionList1: TActionList;
    CalculateAction: TAction;
    ResultLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure CalculateActionExecute(Sender: TObject);
    procedure Language1Click(Sender: TObject);
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

uses
  NtBase, NtPattern, NtBaseTranslator, NtLanguageDlg, NtPictureTranslator;

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
resourcestring
  // This multi-plular pattern has two parameters (hours and minutes).
  // It uses standard plural rules of English (1 = singular, all other are plurals) plus special zero case in hours.
  SResultPlural = 'Driving time{plural, zero { } one { %d hour } other { %d hours }}{plural, one {%d minute} other {%d minutes}}.';
var
  time: Double;
  hours, minutes: Integer;
begin
  if Speed = 0 then
    Exit;

  time := Distance/Speed;
  hours := Trunc(time);
  minutes := Round(60*(time - hours));

  ResultLabel.Caption := TMultiPattern.Format(SResultPlural, [hours, minutes]);
end;

procedure TForm1.Language1Click(Sender: TObject);
begin
  // Show a language select dialog and turn on the selected language
  if TNtLanguageDialog.Select('en', 'English') then
  begin
    // Language has been changed.
    // Properties that were set on run time must be reset.
    CalculateActionExecute(Self);
  end;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.About1Click(Sender: TObject);
resourcestring
  SAbout = 'Driving time calculator';
begin
  ShowMessage(SAbout);
end;

initialization
  //NtEnabledProperties := STRING_TYPES;
end.
