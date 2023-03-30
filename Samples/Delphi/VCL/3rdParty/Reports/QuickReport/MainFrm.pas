unit MainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    LanguageButton: TButton;
    PreviewButton: TButton;
    PrintButton: TButton;
    procedure PreviewButtonClick(Sender: TObject);
    procedure PrintButtonClick(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  NtBase, NtLanguageDlg, ReportFrm;

procedure TMainForm.PreviewButtonClick(Sender: TObject);
begin
  ReportForm.QuickRep1.Preview;
end;

procedure TMainForm.PrintButtonClick(Sender: TObject);
begin
  ReportForm.QuickRep1.PrinterSetup;

  if ReportForm.QuickRep1.Tag = 0 then
    ReportForm.QuickRep1.Print;
end;

procedure TMainForm.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select('en', '', lnNative, [ldShowErrorIfNoDll], [roSaveLocale]) then
    ReportForm.UpdateItems;
end;

end.
