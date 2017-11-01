unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    SkiLabel: TLabel;
    SkiCombo: TComboBox;
    BicycleLabel: TLabel;
    BicycleCombo: TComboBox;
    LanguageButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ComboChange(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

  private
    function GetSkiAmount: Integer;
    function GetBicycleAmount: Integer;

    procedure UpdateStrings;

  public
    property SkiAmount: Integer read GetSkiAmount;
    property BicycleAmount: Integer read GetBicycleAmount;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  NtPattern,
  NtResource,
  FMX.NtLanguageDlg,
  FMX.NtTranslator;

function TForm1.GetSkiAmount: Integer;
begin
  if SkiCombo.ItemIndex >= 0 then
    Result := StrToInt(SkiCombo.Items[SkiCombo.ItemIndex])
  else
    Result := 0;
end;

function TForm1.GetBicycleAmount: Integer;
begin
  if BicycleCombo.ItemIndex >= 0 then
    Result := StrToInt(BicycleCombo.Items[BicycleCombo.ItemIndex])
  else
    Result := 0;
end;

procedure TForm1.UpdateStrings;
begin
  Label1.Text := TMultiPattern.Format(_T('I have %s;zero;no skis %1:s;one;one ski %1:s;other;%d skis %s;next;zero;and no bicycles;one;and one bicycle;other;and %d bicycles', 'MessagePlural'), [SkiAmount, BicycleAmount]);
end;

procedure TForm1.FormCreate(Sender: TObject);

  procedure Add(value: Integer);
  begin
    SkiCombo.Items.Add(IntToStr(value));
    BicycleCombo.Items.Add(IntToStr(value));
  end;

begin
  NtResources._T('English', 'en');
  NtResources._T('Finnish', 'fi');
  NtResources._T('German', 'de');
  NtResources._T('French', 'fr');
  NtResources._T('Japanese', 'ja');

  _T(Self);

  Add(0);
  Add(1);
  Add(2);
  Add(3);
  Add(4);
  Add(5);
  Add(11);
  Add(21);
  Add(101);
  Add(111);

  SkiCombo.ItemIndex := 1;
  BicycleCombo.ItemIndex := 2;

  ComboChange(Self);
end;

procedure TForm1.ComboChange(Sender: TObject);
begin
  UpdateStrings;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select then
    UpdateStrings;
end;

end.
