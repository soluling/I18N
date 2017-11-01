unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, RzTabs, RzGroupBar;

type
  TForm3 = class(TForm)
    RzPageControl1: TRzPageControl;
    TabSheet1: TRzTabSheet;
    CheckBox1: TCheckBox;
    TabSheet2: TRzTabSheet;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    RzGroupBar1: TRzGroupBar;
    procedure FormActivate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

uses
  NtBaseTranslator,
  NtRaizeChecker,
  NtChecker;

procedure TForm3.FormActivate(Sender: TObject);
begin
  NtFormChecker.Check(Self);
end;

procedure TForm3.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  RzPageControl1.ActivePage := TabSheet1;
end;

end.
