unit SystemModified;

interface

//JS
type
  TResourceStringEvent = function(ResStringRec: PResStringRec; var value: String): Boolean;

var
  ResourceStringOverride: TResourceStringEvent;
//JS

implementation

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

{$IFDEF MSWINDOWS}
function LoadResString(ResStringRec: PResStringRec): string;
var
  Buffer: array [0..4095] of Char;
begin
  if ResStringRec = nil then Exit;
//JS
  if Assigned(ResourceStringOverride) and ResourceStringOverride(ResStringRec, Result) then
    Exit;
//JS
  if ResStringRec.Identifier < 64*1024 then
    SetString(Result, Buffer,
      LoadString(FindResourceHInstance(ResStringRec.Module^),
        ResStringRec.Identifier, Buffer, Length(Buffer)))
  else
    Result := PChar(ResStringRec.Identifier);
end;
{$ENDIF}

{$IF defined(EXTERNALLINKER)}
function LoadResString(ResStringRec: PResStringRec): string;
type
  TWCharArray = array[0..$FFFF] of WideChar;
  PResStringResource = ^TResStringResource;
  TResStringResource = packed record
    Len: Word;
    case Integer of
    0: ( CharData: TWCharArray; );
    1: ( LongLen: UInt32;
         LongCharData: TWCharArray; );
  end;
var
  Handle: NativeUInt;
  P: PResStringResource;
  L: Integer;
begin
  Result := '';
//JS
  if Assigned(ResourceStringOverride) and ResourceStringOverride(ResStringRec, Result) then
    Exit;
//JS
  if ResStringRec <> nil then
  begin
    Handle := RTLD_DEFAULT;
    dlerror;   // clear error state;  dlsym doesn't
    P := dlsym(Handle, ResStringRec^.Key);
    if (P <> nil) and (dlerror = nil) then
    begin
      L := P^.Len;
      if L <> $FFFF then
        SetString(Result, P^.CharData, L)
      else
        SetString(Result, P^.LongCharData, P^.LongLen);
    end;
  end;
end;
{$ENDIF}

end.
