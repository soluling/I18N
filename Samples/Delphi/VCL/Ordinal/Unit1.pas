unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, NtOrdinal, NtPattern;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label20: TLabel;
    Label101: TLabel;
    LanguageButton: TButton;
    FormGroup: TRadioGroup;
    PluralGroup: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure RadioClick(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

  private
    function GetOrdinalForm: TOrdinalForm;
    function GetPlural: TPlural;

    procedure UpdateValues;

  public
    property OrdinalForm: TOrdinalForm read GetOrdinalForm;
    property Plural: TPlural read GetPlural;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtLanguageDlg;

function TForm1.GetOrdinalForm: TOrdinalForm;
begin
  if FormGroup.ItemIndex = 0 then
    Result := ofShort
  else
    Result := ofLong
end;

function TForm1.GetPlural: TPlural;
begin
  if PluralGroup.ItemIndex = 0 then
    Result := pfOne
  else
    Result := pfOther
end;

procedure TForm1.UpdateValues;

  procedure Process(count: Integer; messageLabel: TLabel);
  resourcestring
    SSingularSample = 'This is the %s attempt.';  //loc 0: Singular ordinal number such as "1st" or "first"
    SPluralSample = 'These are the %s attempts.';  //loc 0: Plural ordinal number such as "1st" or "first"
  begin
    if Plural = pfOne then
      messageLabel.Caption := Format(SSingularSample, [TNtOrdinal.Format(OrdinalForm, count, pfOne)])
    else
      messageLabel.Caption := Format(SPluralSample, [TNtOrdinal.Format(OrdinalForm, count, pfOther)]);
  end;

begin
  Process(1, Label1);
  Process(2, Label2);
  Process(3, Label3);
  Process(4, Label4);
  Process(5, Label5);
  Process(10, Label10);
  Process(11, Label11);
  Process(21, Label20);
  Process(101, Label101);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FormGroup.ItemIndex := 0;
  PluralGroup.ItemIndex := 0;
  UpdateValues;
end;

procedure TForm1.RadioClick(Sender: TObject);
begin
  UpdateValues;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select then
    UpdateValues;
end;

end.
