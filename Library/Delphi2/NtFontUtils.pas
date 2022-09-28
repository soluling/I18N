{
  @abstract Implements font and character set routines.
}
unit NtFontUtils;

{$I NtVer.inc}

interface

uses
  Vcl.Graphics;

type
  { @abstract Record that stores information about charset. }
  TNtCharsetInfo = record
    CharSet: TFontCharset;
    CodePage: Integer;
  end;

  { @abstract Static class that contains font routines. }
  TNtFontUtils = class
  public
    { Get charset information.
      @param langId Language or locale id.
      @return Charset information. }
    class function GetCharSetInfo(langId: Integer): TNtCharsetInfo;

    { Get the active font charset.
      @return Font charset. }
    class function GetActiveFontCharset: TFontCharset;
  end;

implementation

uses
  Windows, NtBase, NtLocalization;

type
  TLocaleFontSignature = packed record
    fsUsb: array[0..3] of DWord;
    fsCsbDefault: array[0..1] of DWord;
    fsCsbSupported: array[0..1] of DWord;
  end;

class function TNtFontUtils.GetCharSetInfo(langId: Integer): TNtCharsetInfo;
const
  CHARSETSETS: array[0..31] of TNtCharsetInfo =
  (
    (charSet: ANSI_CHARSET; codePage: 1252),
    (charSet: EASTEUROPE_CHARSET; codePage: 1250),
    (charSet: RUSSIAN_CHARSET; codePage: 1251),
    (charSet: GREEK_CHARSET; codePage: 1253),
    (charSet: TURKISH_CHARSET; codePage: 1254),
    (charSet: HEBREW_CHARSET; codePage: 1255),
    (charSet: ARABIC_CHARSET; codePage: 1256),
    (charSet: BALTIC_CHARSET; codePage: 1257),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0),
    (charSet: THAI_CHARSET; codePage: 874),
    (charSet: SHIFTJIS_CHARSET; codePage: 932),
    (charSet: GB2312_CHARSET; codePage: 936),
    (charSet: HANGEUL_CHARSET; codePage: 949),
    (charSet: CHINESEBIG5_CHARSET; codePage: 950),
    (charSet: JOHAB_CHARSET; codePage: 1361),
    (charSet: VIETNAMESE_CHARSET; codePage: 1258),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0),
    (charSet: 0; codePage: 0)
  );
var
  f: DWord;
  i: Integer;
  lfs: TLocaleFontSignature;
begin
  if GetLocaleInfo(langId, LOCALE_FONTSIGNATURE, PChar(@lfs), Sizeof(lfs)) <> 0 then
  begin
    f := 1;

    for i := Low(CHARSETSETS) to High(CHARSETSETS) do
    begin
      if (f and lfs.fsCsbDefault[0]) <> 0 then
      begin
        Result := CHARSETSETS[i];
        Exit;
      end;

      f := f shl 1;
    end;
  end;

  Result := CHARSETSETS[0];
end;

class function TNtFontUtils.GetActiveFontCharset: TFontCharset;
begin
  Result := GetCharSetInfo(TNtLocale.ExtensionToLocale(LoadedResourceLocale)).charSet;
end;

end.
