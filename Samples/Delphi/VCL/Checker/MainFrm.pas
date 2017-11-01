unit MainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, BaseFrm, SampleFra;

type
  TMainForm = class(TBaseForm)
    TruncationGroup: TGroupBox;
    TruncationLabel1: TLabel;
    TruncationLabel2: TLabel;
    OverlappingGroup: TGroupBox;
    OverlappingLabel1: TLabel;
    OverlappingLabel2: TLabel;
    OverlappingLabel3: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Button2: TButton;
    LanguageButton: TButton;
    Label1: TLabel;
    TruncationLabel3: TLabel;
    Frame1: TFrame1;
    Panel1: TPanel;
    CheckBox3: TCheckBox;
    ShowButton: TButton;
    Button1: TButton;
    CheckBox4: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    ComboBox1: TComboBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    SimpleButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);
    procedure ShowButtonClick(Sender: TObject);
    procedure SimpleButtonClick(Sender: TObject);

  private
    procedure InitializeValues;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
{$IFDEF CHECKUI}
  NtChecker,
{$ENDIF}
  NtLanguageDlg,
  SimpleFrm,
  SampleDlg;

procedure TMainForm.InitializeValues;
resourcestring
  STruncation = 'This is another sample label text.';
  SOverlapping = 'Another strings.';
  SCheckBox = 'This is another sample';
  SButton = 'Click here to start';
begin
  TruncationLabel2.Caption := STruncation;
  TruncationLabel3.Caption := STruncation;
  Label1.Caption := STruncation;

  OverlappingLabel2.Caption := SOverlapping;
  CheckBox2.Caption := SCheckBox;
  Button2.Caption := SButton;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  InitializeValues;
end;

procedure TMainForm.ShowButtonClick(Sender: TObject);
var
  dialog: TSampleDialog;
begin
  dialog := TSampleDialog.Create(nil);
  try
    dialog.ShowModal;
  finally
    dialog.Free;
  end;
end;

procedure TMainForm.SimpleButtonClick(Sender: TObject);
var
  form: TSimpleForm;
begin
  inherited;

  form := TSimpleForm.Create(nil);
  try
    form.ShowModal;
  finally
    form.Free;
  end;
end;

procedure TMainForm.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select('en') then
  begin
    InitializeValues;
{$IFDEF CHECKUI}
    NtFormChecker.LanguageChanged;
    NtFormChecker.Check(Self);
{$ENDIF}
  end;
end;

end.
