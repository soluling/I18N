unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Edit,
  FMX.Menus, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    DistanceLabel: TLabel;
    DistanceEdit: TEdit;
    SpeedLabel: TLabel;
    SpeedEdit: TEdit;
    CalculateButton: TButton;
    CarImage: TImage;
    ResultLabel: TLabel;
    FlagImage: TImage;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Calculate1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Language1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Language1Click(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure CalculateButtonClick(Sender: TObject);
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

{$R *.fmx}

uses
  NtPattern,
  NtResource,
  FMX.NtImageTranslator,
  FMX.NtLanguageDlg,
  FMX.NtTranslator;

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
  NtResources._T('English', 'en');
  NtResources._T('Finnish', 'fi');
  NtResources._T('German', 'de');
  NtResources._T('French', 'fr');
  NtResources._T('Spanish', 'es');
  NtResources._T('Japanese', 'ja');

  _T(Self);

  EditChange(Self);
end;

procedure TForm1.EditChange(Sender: TObject);
begin
  CalculateButton.Enabled := (Distance > 0) and (Speed > 0);
  Calculate1.Enabled := CalculateButton.Enabled;
end;

procedure TForm1.CalculateButtonClick(Sender: TObject);
var
  time: Double;
  hours, minutes: Integer;
begin
  if Speed = 0 then
    Exit;

  time := Distance/Speed;
  hours := Trunc(time);
  minutes := Round(60*(time - hours));

  // This multi-plural pattern has two parameters (hours and minutes).
  // It uses standard plural rules of English (1 = singular, all other are plurals) plus special zero case in hours.
  ResultLabel.Text := TMultiPattern.Format(_T('Driving time{plural, zero { } one { %d hour } other { %d hours }}{plural, one {%d minute} other {%d minutes}}.', 'ResultPlural'), [hours, minutes]);
end;

procedure TForm1.Language1Click(Sender: TObject);
begin
  TNtLanguageDialog.Select(procedure
    begin
      CalculateButtonClick(Self);
    end);
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
  ShowMessage(_T('Driving time calculator'));
end;

end.
