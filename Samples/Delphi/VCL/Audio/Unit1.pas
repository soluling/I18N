unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Imaging.pngimage;

type
  TForm1 = class(TForm)
    Image1: TImage;
    PlayButton: TButton;
    SampleLabel: TLabel;
    procedure PlayButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  MMSystem;

function GetTempFile(
  const prefix: String;
  const ext: String;
  dir: String = ''): String;
var
  path: array[0..MAX_PATH] of Char;
  buffer: array[0..MAX_PATH] of Char;
begin
  if dir = '' then
  begin
    GetTempPath(Sizeof(path) div Sizeof(Char), path);
    dir := path;
  end;

  GetTempFileName(PChar(dir), PChar(prefix), 0, buffer);
  Result := buffer;

  if ext <> '' then
  begin
    DeleteFile(Result);
    Result := ChangeFileExt(Result, ext);
  end;
end;

procedure StreamToFile(stream: TStream; const fileName: String);
var
  position: Integer;
  data: TBytes;
  fileStream: TFileStream;
begin
  position := stream.Position;
  fileStream := TFileStream.Create(fileName, fmCreate);
  try
    stream.Seek(0, soFromBeginning);
    SetLength(data, stream.Size);
    stream.Read(data[0], Length(data));
    fileStream.Write(data[0], Length(data));
  finally
    fileStream.Free;
    stream.Position := position;
  end;
end;

function PlayWaiting(const fileName: String): Boolean; overload;
var
  error, deviceID: Longint;
  openParm: TMCI_Open_Parms;
  playParm: TMCI_Play_Parms;
begin
  Result := False;

  FillChar(openParm, SizeOf(TMCI_Open_Parms), 0);
  openParm.dwCallback := 0;
  openParm.lpstrElementName := PChar(fileName);

  error := mciSendCommand(0, mci_Open, MCI_OPEN_ELEMENT or MCI_WAIT, Longint(@openParm));

  if (error <> 0) and (error <> MCIERR_DEVICE_OPEN) then
    Exit;

  deviceID := openParm.wDeviceID;

  try
    FillChar(playParm, SizeOf(TMCI_Play_Parms), 0);
    Result := mciSendCommand(deviceID, MCI_PLAY, MCI_WAIT, Longint(@playParm)) = 0;
  finally
    mciSendCommand(deviceID, MCI_CLOSE, 0, Longint(@openParm));
  end;
end;

function PlayWaiting(stream: TStream): Boolean; overload;
var
  fileName: String;
begin
  fileName := GetTempFile('Sample', '.mp3');
  StreamToFile(stream, fileName);
  try
    Result := PlayWaiting(fileName)
  finally
    DeleteFile(fileName);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SampleLabel.Hide;
end;

procedure TForm1.PlayButtonClick(Sender: TObject);
resourcestring
  SSample = 'How are you?';
var
  stream: TResourceStream;
begin
  SampleLabel.Caption := SSample;
  SampleLabel.Show;
  Update;

  stream := TResourceStream.Create(HInstance, 'Sample', 'Audio');
  try
    PlayWaiting(stream);
  finally
    stream.Free;
  end;
end;

end.
