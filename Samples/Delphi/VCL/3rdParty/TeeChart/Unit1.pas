unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.TeEngine, VCLTee.Series,
  Vcl.ExtCtrls, VCLTee.TeeProcs, VCLTee.Chart, Vcl.StdCtrls, VCLTee.TeeEdit;

type
  TForm1 = class(TForm)
    Chart1: TChart;
    Series1: TLineSeries;
    ChartPreviewer1: TChartPreviewer;
    PreviewButton: TButton;
    LanguageButton: TButton;
    ChartEditor1: TChartEditor;
    EditButton: TButton;
    procedure LanguageButtonClick(Sender: TObject);
    procedure PreviewButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);

  private
    procedure UpdateItems;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtBase,
  NtLanguageDlg;

procedure TForm1.UpdateItems;
begin
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  // Show a language select dialog and turn on the selected language
  if TNtLanguageDialog.Select('en', '', lnBoth, [], [], poMainFormCenter, lcUpper) then
  begin
    // Language has been changed.
    // Properties that were set on run time must be reset.
    UpdateItems;
  end;
end;

procedure TForm1.PreviewButtonClick(Sender: TObject);
begin
  ChartPreviewer1.Execute;
end;

procedure TForm1.EditButtonClick(Sender: TObject);
begin
  ChartEditor1.Execute;
end;

end.
