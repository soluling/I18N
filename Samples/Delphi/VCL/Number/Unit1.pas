unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, NtNumber;

type
  TForm1 = class(TForm)
    Abbreviated1: TLabel;
    Abbreviated2: TLabel;
    Abbreviated3: TLabel;
    Abbreviated4: TLabel;
    Abbreviated5: TLabel;
    Abbreviated6: TLabel;
    Abbreviated7: TLabel;
    Abbreviated8: TLabel;
    Abbreviated9: TLabel;
    Abbreviated10: TLabel;
    Abbreviated11: TLabel;
    Abbreviated12: TLabel;
    Abbreviated13: TLabel;
    Abbreviated14: TLabel;
    Original1: TLabel;
    Original2: TLabel;
    Original3: TLabel;
    Original4: TLabel;
    Original5: TLabel;
    Original6: TLabel;
    Original7: TLabel;
    Original8: TLabel;
    Original9: TLabel;
    Original10: TLabel;
    Original11: TLabel;
    Original12: TLabel;
    Original13: TLabel;
    Original14: TLabel;
    FormGroup: TRadioGroup;
    PrecisionLabel: TLabel;
    PrecisionEdit: TEdit;
    PrecisionUpDown: TUpDown;
    LanguageButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormGroupClick(Sender: TObject);
    procedure PrecisionEditChange(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

  private
    function GetPrecision: Integer;
    function GetNumberForm: TAbbreviatedNumberForm;

    procedure UpdateValues;

  public
    property Precision: Integer read GetPrecision;
    property NumberForm: TAbbreviatedNumberForm read GetNumberForm;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtLanguageDlg;

function TForm1.GetPrecision: Integer;
begin
  Result := PrecisionUpDown.Position;
end;

function TForm1.GetNumberForm: TAbbreviatedNumberForm;
begin
  Result := TAbbreviatedNumberForm(FormGroup.ItemIndex);
end;

procedure TForm1.UpdateValues;

  procedure Process(count: Double; abbreviatedLabel, originalLabel: TLabel);
  resourcestring
    SNumberSample = 'There are %s users.';  //loc 0: Abbreviated number such as "1K" or "12 million"
    SCurrencySample = 'I have %s in my account.';  //loc 0: Abbreviated currency amount such as "$1K" or "12M €"
  var
    template: String;
  begin
    if NumberForm = anCurrency then
      template := SCurrencySample
    else
      template := SNumberSample;

    abbreviatedLabel.Caption := Format(template, [TAbbreviatedNumber.Format(NumberForm, count, Precision)]);
    originalLabel.Caption := Format(template, [IntToStr(Round(count))]);
  end;

begin
  Process(1, Abbreviated1, Original1);
  Process(2.4, Abbreviated2, Original2);
  Process(121, Abbreviated3, Original3);
  Process(1000, Abbreviated4, Original4);
  Process(1650, Abbreviated5, Original5);
  Process(27450, Abbreviated6, Original6);
  Process(190394, Abbreviated7, Original7);
  Process(3400000, Abbreviated8, Original8);
  Process(54000000, Abbreviated9, Original9);
  Process(670000000, Abbreviated10, Original10);
  Process(1200000000, Abbreviated11, Original11);
  Process(20200000000, Abbreviated12, Original12);
  Process(410000000000, Abbreviated13, Original13);
  Process(9520000000000, Abbreviated14, Original14);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FormGroup.ItemIndex := 0;
  UpdateValues;
end;

procedure TForm1.FormGroupClick(Sender: TObject);
begin
  UpdateValues;
end;

procedure TForm1.PrecisionEditChange(Sender: TObject);
begin
  UpdateValues;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select then
    UpdateValues;
end;

end.
