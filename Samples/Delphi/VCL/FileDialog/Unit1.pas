unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    OpenButton1: TButton;
    OpenLabel1: TLabel;
    OpenButton2: TButton;
    OpenLabel2: TLabel;
    OpenButton3: TButton;
    OpenLabel3: TLabel;
    OpenButton4: TButton;
    OpenLabel4: TLabel;
    SaveButton1: TButton;
    SaveLabel1: TLabel;
    SaveButton2: TButton;
    SaveLabel2: TLabel;
    procedure OpenButton1Click(Sender: TObject);
    procedure OpenButton2Click(Sender: TObject);
    procedure OpenButton3Click(Sender: TObject);
    procedure OpenButton4Click(Sender: TObject);
    procedure SaveButton1Click(Sender: TObject);
    procedure SaveButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtDialog;

resourcestring
  SOpenTitle = 'Open File';
  SSaveTitle = 'Save File';
  SSaveFile = 'SampleFile.xml';

  SAllFiles = 'All files'; // loc Part of file dialog filter: All files (*.*)
  SAllSupportedFiles = 'All supported files'; // loc Part of file dialog filter: All supported files (...)
  SXmlFiles = 'XML files'; // loc Part of file dialog filter: XML files (*.xml)
  SIniFiles = 'Ini files'; // loc Part of file dialog filter: Ini files (*.ini)

procedure TForm1.OpenButton1Click(Sender: TObject);
resourcestring
  // This string is hard to translate and very error prone for syntax mistakes
  SOpenFilter = 'All supported files (*.xml;*.ini)|*.xml;*.ini|XML files (*.xml)|*.xml|Ini files (*.ini)|*.ini|All files (*.*)|*.*';
var
  dialog: TOpenDialog;
begin
  dialog := TOpenDialog.Create(nil);
  try
    dialog.Title := SOpenTitle;
    dialog.Filter := SOpenFilter;

    if dialog.Execute then
      ShowMessage(dialog.FileName);
  finally
    dialog.Free;
  end;
end;

procedure TForm1.OpenButton2Click(Sender: TObject);
var
  dialog: TOpenDialog;
  filter: TNtDialogFilter;
begin
  // The result of this is the same as above but instead of having one difficult and error prone string, SOpenFilter, we will have
  // four simple and easy to translate strings: SAllFiles, SAllSupportedFiles, SXmlFiles and SIniFiles
  dialog := TOpenDialog.Create(nil);
  try
    dialog.Title := SOpenTitle;

    filter := TNtDialogFilter.Create;
    try
      filter.Supported(SAllSupportedFiles);  // All supported files is first
      filter.Add(SXmlFiles, 'xml');          // XML files
      filter.Add(SIniFiles, 'ini');          // Ini files
      filter.All(SAllFiles);                 // All files is last

      dialog.Filter := filter.Value;
    finally
      filter.Free;
    end;

    if dialog.Execute then
      ShowMessage(dialog.FileName);
  finally
    dialog.Free;
  end;
end;

procedure TForm1.OpenButton3Click(Sender: TObject);
var
  dialog: TOpenDialog;
  filter: TNtDialogFilter;
begin
  // As above but without All supported files and All files.
  dialog := TOpenDialog.Create(nil);
  try
    dialog.Title := SOpenTitle;

    filter := TNtDialogFilter.Create;
    try
      filter.Add(SXmlFiles, 'xml');  // XML files
      filter.Add(SIniFiles, 'ini');  // Ini files

      dialog.Filter := filter.Value;
    finally
      filter.Free;
    end;

    if dialog.Execute then
      ShowMessage(dialog.FileName);
  finally
    dialog.Free;
  end;
end;

procedure TForm1.OpenButton4Click(Sender: TObject);
var
  dialog: TOpenDialog;
  filter: TNtDialogFilter;
begin
  dialog := TOpenDialog.Create(nil);
  try
    dialog.Title := SOpenTitle;

    filter := TNtDialogFilter.Create;
    try
      filter.Add(SXmlFiles, 'xml');          // XML files
      filter.Supported(SAllSupportedFiles);  // All supported files is second
      filter.Add(SIniFiles, 'ini');          // Ini files
      filter.All(SAllFiles);                 // All files is last

      dialog.Filter := filter.Value;
    finally
      filter.Free;
    end;

    if dialog.Execute then
      ShowMessage(dialog.FileName);
  finally
    dialog.Free;
  end;
end;

procedure TForm1.SaveButton1Click(Sender: TObject);
resourcestring
  // This string is hard to translate and very error prone for syntax mistakes
  SSaveFilter = 'XML files (*.xml)|*.xml|Ini files (*.ini)|*.ini';
var
  dialog: TSaveDialog;
begin
  dialog := TSaveDialog.Create(nil);
  try
    dialog.Title := SSaveTitle;
    dialog.FileName := SSaveFile;
    dialog.Filter := SSaveFilter;

    if dialog.Execute then
      ShowMessage(dialog.FileName);
  finally
    dialog.Free;
  end;
end;

procedure TForm1.SaveButton2Click(Sender: TObject);
var
  dialog: TSaveDialog;
  filter: TNtDialogFilter;
begin
  // The result of this is the same as above but instead of having one difficult and error prone string, SSaveFilter, we will have
  // two simple and easy to translate strings: SXmlFiles and SIniFiles
  dialog := TSaveDialog.Create(nil);
  try
    dialog.Title := SSaveTitle;
    dialog.FileName := SSaveFile;

    filter := TNtDialogFilter.Create;
    try
      filter.Add(SXmlFiles, 'xml');  // XML files
      filter.Add(SIniFiles, 'ini');  // Ini files

      dialog.Filter := filter.Value;
    finally
      filter.Free;
    end;

    if dialog.Execute then
      ShowMessage(dialog.FileName);
  finally
    dialog.Free;
  end;
end;

end.
