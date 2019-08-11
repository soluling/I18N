unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    FirstNameLabel: TLabel;
    FirstNameEdit: TEdit;
    SecondNameLabel: TLabel;
    SecondNameEdit: TEdit;
    CountLabel: TLabel;
    CountEdit: TEdit;
    CountUpDown: TUpDown;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LanguageButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

  private
    function GetFirstName: String;
    function GetSecondName: String;
    function GetCount: Integer;

    procedure UpdateItems;

  public
    property FirstName: String read GetFirstName;
    property SecondName: String read GetSecondName;
    property Count: Integer read GetCount;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtBase,
  NtLanguageDlg;

function TForm1.GetFirstName: String;
begin
  Result := FirstNameEdit.Text;
end;

function TForm1.GetSecondName: String;
begin
  Result := SecondNameEdit.Text;
end;

function TForm1.GetCount: Integer;
begin
  Result := CountUpDown.Position;
end;

procedure TForm1.UpdateItems;
resourcestring
  SHello = 'Hello %s!';  //loc: 0: Name of a person
  SHello2 = 'Hello %s and %s!';  //loc: 0: Name of the first person, 1: Name of the second person
  SCount = '%s has %d cars';  //loc: 0: Name of a person, 1: Number of cars
  SCount2 = '%d cars will pick up %s and %s';  //loc: 0: Number of cars, 1: Name of the first person, 2: Name of the second person
  SDouble = '%0:s swims and %0:s skis'; //loc: 0: Name of a person. This parameter occurs twice.
begin
  label1.Caption := Format(SHello, [FirstName]);
  label2.Caption := Format(SHello2, [FirstName, SecondName]);
  label3.Caption := Format(SCount, [FirstName, Count]);
  label4.Caption := Format(SCount2, [Count, FirstName, SecondName]);
  label5.Caption := Format(SDouble, [FirstName]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FirstNameEdit.Text := 'John';
  SecondNameEdit.Text := 'Jill';
  CountUpDown.Position := 2;
  UpdateItems;
end;

procedure TForm1.EditChange(Sender: TObject);
begin
  inherited;
  UpdateItems;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  // Show a language select dialog and turn on the selected language
  if TNtLanguageDialog.Select('en', '', lnBoth) then
  begin
    // Language has been changed.
    // Properties that were set on run time must be set again.
    UpdateItems;
  end;
end;

end.
