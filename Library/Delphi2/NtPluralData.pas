unit NtPluralData;  //FI:ignore
// Generated from CLDR data. Do not edit.

interface

uses
  NtPattern;

{ Get a plural form for Bamanankan, Tibetan, Dzongkha, Indonesian, Igbo, Yi, in, Japanese, jbo, Javanese, jw, Makonde, Kabuverdianu, Khmer, Korean, Lakota, Lao, Malay, Burmese, N'ko, root, Sakha, Koyraboro Senni, Sango, Thai, Tongan, Vietnamese, Wolof, Yoruba, yue, Chinese, Simplified.
  @param n  Absolute value of the source number (integer and decimals) (e.g. 9.870 => 9.87).
  @param i  Integer digits of n (e.g. 9.870 => 9).
  @param v  Number of visible fraction digits in n, with trailing zeros (e.g. 9.870 => 3).
  @param w  Number of visible fraction digits in n, without trailing zeros (e.g. 9.870 => 2).
  @param f  Visible fractional digits in n, with trailing zeros (e.g. 9.870 => 870).
  @param t  Visible fractional digits in n, without trailing zeros (e.g. 9.870 => 87).
  @return Plural form. }
function GetSinglePlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;

implementation

// Bamanankan, Tibetan, Dzongkha, Indonesian, Igbo, Yi, in, Japanese, jbo, Javanese, jw, Makonde, Kabuverdianu, Khmer, Korean, Lakota, Lao, Malay, Burmese, N'ko, root, Sakha, Koyraboro Senni, Sango, Thai, Tongan, Vietnamese, Wolof, Yoruba, yue, Chinese, Simplified
function GetSinglePlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // other: @integer 0~15, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  Result := pfOther
end;

// Amharic, Assamese, Bangla, Persian, Gujarati, Hindi, Kannada, Marathi, isiZulu
function GetAmPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: i = 0 or n = 1 @integer 0, 1 @decimal 0.0~1.0, 0.00~0.04
  // other: @integer 2~17, 100, 1000, 10000, 100000, 1000000, … @decimal 1.1~2.6, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (i = 0) or (n = 1) then
    Result := pfOne
  else
    Result := pfOther
end;

// Fulah, French, Armenian, Kabyle
function GetFfPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: i = 0,1 @integer 0, 1 @decimal 0.0~1.5
  // other: @integer 2~17, 100, 1000, 10000, 100000, 1000000, … @decimal 2.0~3.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (i in [0,1]) then
    Result := pfOne
  else
    Result := pfOther
end;

// Asturian, Catalan, German, English, Estonian, Finnish, Western Frisian, Galician, Italian, ji, Dutch, Swedish, Kiswahili, Urdu, Yiddish
function GetAstPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: i = 1 and v = 0 @integer 1
  // other: @integer 0, 2~16, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (i = 1) and (v = 0) then
    Result := pfOne
  else
    Result := pfOther
end;

// Sinhala
function GetSinhalaPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: n = 0,1 or i = 0 and f = 1 @integer 0, 1 @decimal 0.0, 0.1, 1.0, 0.00, 0.01, 1.00, 0.000, 0.001, 1.000, 0.0000, 0.0001, 1.0000
  // other: @integer 2~17, 100, 1000, 10000, 100000, 1000000, … @decimal 0.2~0.9, 1.1~1.8, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if ((n = 0) or (n = 1)) or (i = 0) and (f = 1) then
    Result := pfOne
  else
    Result := pfOther
end;

// Akan, bh, guw, Lingala, Malagasy, Sesotho sa Leboa, Punjabi, Tigrinya, wa
function GetAkPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: n = 0..1 @integer 0, 1 @decimal 0.0, 1.0, 0.00, 1.00, 0.000, 1.000, 0.0000, 1.0000
  // other: @integer 2~17, 100, 1000, 10000, 100000, 1000000, … @decimal 0.1~0.9, 1.1~1.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (n >= 0) and (n <= 1) then
    Result := pfOne
  else
    Result := pfOther
end;

// Central Atlas Tamazight
function GetCentralAtlasTamazightPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: n = 0..1 or n = 11..99 @integer 0, 1, 11~24 @decimal 0.0, 1.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0, 22.0, 23.0, 24.0
  // other: @integer 2~10, 100~106, 1000, 10000, 100000, 1000000, … @decimal 0.1~0.9, 1.1~1.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (n >= 0) and (n <= 1) or (n >= 11) and (n <= 99) then
    Result := pfOne
  else
    Result := pfOther
end;

// Portuguese
function GetPortuguesePlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: n = 0..2 and n != 2 @integer 0, 1 @decimal 0.0, 1.0, 0.00, 1.00, 0.000, 1.000, 0.0000, 1.0000
  // other: @integer 2~17, 100, 1000, 10000, 100000, 1000000, … @decimal 0.1~0.9, 1.1~1.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (n >= 0) and (n <= 1) then
    Result := pfOne
  else
    Result := pfOther
end;

// Afrikaans, Asu, Azerbaijani, Latin, Bemba, Bena, Bulgarian, Bodo, Chechen, Chiga, Cherokee, ckb, Divehi, Ewe, Greek, Esperanto, Spanish, Basque, Faroese, Friulian, Swiss German, Hausa, Hawaiian, Hungarian, Ngomba, Machame, Georgian, kaj, kcg, Kazakh, Kako, Greenlandic, Kashmiri, Shambala, Central Kurdish, Kyrgyz, Luxembourgish, Ganda, Masai, Meta', Malayalam, Mongolian, Cyrillic, nah, Norwegian Bokmål, North Ndebele, Nepali, Norwegian Nynorsk, Ngiemboon, Norwegian, South Ndebele, ny, Nyankole, Oromo, Odia, Ossetic, Papiamento, Pashto, Romansh, Rombo, Rwa, Samburu, sdh, Sena, Shona, Somali, Albanian, siSwati, Saho, Sesotho, Syriac, Tamil, Telugu, Teso, Tigre, Turkmen, Setswana, Turkish, Tsonga, Uyghur, Uzbek, Latin, Venda, Volapük, Vunjo, Walser, isiXhosa, Soga
function GetAfPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: n = 1 @integer 1 @decimal 1.0, 1.00, 1.000, 1.0000
  // other: @integer 0, 2~16, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~0.9, 1.1~1.6, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (n = 1) then
    Result := pfOne
  else
    Result := pfOther
end;

// Portuguese (Portugal)
function GetPortuguesePortugalPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: n = 1 and v = 0 @integer 1
  // other: @integer 0, 2~16, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (n = 1) and (v = 0) then
    Result := pfOne
  else
    Result := pfOther
end;

// Danish
function GetDanishPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: n = 1 or t != 0 and i = 0,1 @integer 1 @decimal 0.1~1.6
  // other: @integer 0, 2~16, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 2.0~3.4, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (n = 1) or (t <> 0) and (i in [0,1]) then
    Result := pfOne
  else
    Result := pfOther
end;

// Icelandic
function GetIcelandicPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: t = 0 and i % 10 = 1 and i % 100 != 11 or t != 0 @integer 1, 21, 31, 41, 51, 61, 71, 81, 101, 1001, … @decimal 0.1~1.6, 10.1, 100.1, 1000.1, …
  // other: @integer 0, 2~16, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (t = 0) and (i mod 10 = 1) and (i mod 100 <> 11) or (t <> 0) then
    Result := pfOne
  else
    Result := pfOther
end;

// Macedonian
function GetMacedonianPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: v = 0 and i % 10 = 1 or f % 10 = 1 @integer 1, 11, 21, 31, 41, 51, 61, 71, 101, 1001, … @decimal 0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 10.1, 100.1, 1000.1, …
  // other: @integer 0, 2~10, 12~17, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 0.2~1.0, 1.2~1.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (v = 0) and (i mod 10 = 1) or (f mod 10 = 1) then
    Result := pfOne
  else
    Result := pfOther
end;

// Filipino, tl
function GetFilipinoPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: v = 0 and i = 1,2,3 or v = 0 and i % 10 != 4,6,9 or v != 0 and f % 10 != 4,6,9 @integer 0~3, 5, 7, 8, 10~13, 15, 17, 18, 20, 21, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~0.3, 0.5, 0.7, 0.8, 1.0~1.3, 1.5, 1.7, 1.8, 2.0, 2.1, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  // other: @integer 4, 6, 9, 14, 16, 19, 24, 26, 104, 1004, … @decimal 0.4, 0.6, 0.9, 1.4, 1.6, 1.9, 2.4, 2.6, 10.4, 100.4, 1000.4, …
  if (v = 0) and (i in [1,2,3]) or (v = 0) and not (i mod 10 in [4,6,9]) or (v <> 0) and not (f mod 10 in [4,6,9]) then
    Result := pfOne
  else
    Result := pfOther
end;

// Latvian, Prussian
function GetLatvianPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // zero: n % 10 = 0 or n % 100 = 11..19 or v = 2 and f % 100 = 11..19 @integer 0, 10~20, 30, 40, 50, 60, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  // one: n % 10 = 1 and n % 100 != 11 or v = 2 and f % 10 = 1 and f % 100 != 11 or v != 2 and f % 10 = 1 @integer 1, 21, 31, 41, 51, 61, 71, 81, 101, 1001, … @decimal 0.1, 1.0, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 10.1, 100.1, 1000.1, …
  // other: @integer 2~9, 22~29, 102, 1002, … @decimal 0.2~0.9, 1.2~1.9, 10.2, 100.2, 1000.2, …
  if (Trunc(n) mod 10 = 0) or (Trunc(n) mod 100 in [11..19]) or (v = 2) and (f mod 100 in [11..19]) then
    Result := pfZero
  else if (Trunc(n) mod 10 = 1) and (Trunc(n) mod 100 <> 11) or (v = 2) and (f mod 10 = 1) and (f mod 100 <> 11) or (v <> 2) and (f mod 10 = 1) then
    Result := pfOne
  else
    Result := pfOther
end;

// Langi
function GetLangiPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // zero: n = 0 @integer 0 @decimal 0.0, 0.00, 0.000, 0.0000
  // one: i = 0,1 and n != 0 @integer 1 @decimal 0.1~1.6
  // other: @integer 2~17, 100, 1000, 10000, 100000, 1000000, … @decimal 2.0~3.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (n = 0) then
    Result := pfZero
  else if (i in [0,1]) and (n <> 0) then
    Result := pfOne
  else
    Result := pfOther
end;

// Colognian
function GetColognianPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // zero: n = 0 @integer 0 @decimal 0.0, 0.00, 0.000, 0.0000
  // one: n = 1 @integer 1 @decimal 1.0, 1.00, 1.000, 1.0000
  // other: @integer 2~17, 100, 1000, 10000, 100000, 1000000, … @decimal 0.1~0.9, 1.1~1.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (n = 0) then
    Result := pfZero
  else if (n = 1) then
    Result := pfOne
  else
    Result := pfOther
end;

// Inuktitut, Latin, Cornish, Nama, Northern Sami, Sami, Southern, smi, Sami, Lule, Sami, Inari, Sami, Skolt
function GetIuPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: n = 1 @integer 1 @decimal 1.0, 1.00, 1.000, 1.0000
  // two: n = 2 @integer 2 @decimal 2.0, 2.00, 2.000, 2.0000
  // other: @integer 0, 3~17, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~0.9, 1.1~1.6, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (n = 1) then
    Result := pfOne
  else if (n = 2) then
    Result := pfTwo
  else
    Result := pfOther
end;

// Tachelhit
function GetTachelhitPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: i = 0 or n = 1 @integer 0, 1 @decimal 0.0~1.0, 0.00~0.04
  // few: n = 2..10 @integer 2~10 @decimal 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 2.00, 3.00, 4.00, 5.00, 6.00, 7.00, 8.00
  // other: @integer 11~26, 100, 1000, 10000, 100000, 1000000, … @decimal 1.1~1.9, 2.1~2.7, 10.1, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (i = 0) or (n = 1) then
    Result := pfOne
  else if (n >= 2) and (n <= 10) then
    Result := pfFew
  else
    Result := pfOther
end;

// mo, Romanian
function GetRomanianPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: i = 1 and v = 0 @integer 1
  // few: v != 0 or n = 0 or n != 1 and n % 100 = 1..19 @integer 0, 2~16, 101, 1001, … @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  // other: @integer 20~35, 100, 1000, 10000, 100000, 1000000, …
  if (i = 1) and (v = 0) then
    Result := pfOne
  else if (v <> 0) or (n = 0) or (n <> 1) and (Trunc(n) mod 100 in [1..19]) then
    Result := pfFew
  else
    Result := pfOther
end;

// Bosnian, Latin, Croatian, sh, Serbian, Cyrillic
function GetBsPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: v = 0 and i % 10 = 1 and i % 100 != 11 or f % 10 = 1 and f % 100 != 11 @integer 1, 21, 31, 41, 51, 61, 71, 81, 101, 1001, … @decimal 0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 10.1, 100.1, 1000.1, …
  // few: v = 0 and i % 10 = 2..4 and i % 100 != 12..14 or f % 10 = 2..4 and f % 100 != 12..14 @integer 2~4, 22~24, 32~34, 42~44, 52~54, 62, 102, 1002, … @decimal 0.2~0.4, 1.2~1.4, 2.2~2.4, 3.2~3.4, 4.2~4.4, 5.2, 10.2, 100.2, 1000.2, …
  // other: @integer 0, 5~19, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 0.5~1.0, 1.5~2.0, 2.5~2.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (v = 0) and (i mod 10 = 1) and (i mod 100 <> 11) or (f mod 10 = 1) and (f mod 100 <> 11) then
    Result := pfOne
  else if (v = 0) and (i mod 10 in [2..4]) and not (i mod 100 in [12..14]) or (f mod 10 in [2..4]) and not (f mod 100 in [12..14]) then
    Result := pfFew
  else
    Result := pfOther
end;

// Scottish Gaelic
function GetScottishGaelicPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: n = 1,11 @integer 1, 11 @decimal 1.0, 11.0, 1.00, 11.00, 1.000, 11.000, 1.0000
  // two: n = 2,12 @integer 2, 12 @decimal 2.0, 12.0, 2.00, 12.00, 2.000, 12.000, 2.0000
  // few: n = 3..10,13..19 @integer 3~10, 13~19 @decimal 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 3.00
  // other: @integer 0, 20~34, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~0.9, 1.1~1.6, 10.1, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if ((n = 1) or (n = 11)) then
    Result := pfOne
  else if ((n = 2) or (n = 12)) then
    Result := pfTwo
  else if ((n >= 3) and (n >= 13) and (n <= 19)) then
    Result := pfFew
  else
    Result := pfOther
end;

// Slovenian
function GetSlovenianPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: v = 0 and i % 100 = 1 @integer 1, 101, 201, 301, 401, 501, 601, 701, 1001, …
  // two: v = 0 and i % 100 = 2 @integer 2, 102, 202, 302, 402, 502, 602, 702, 1002, …
  // few: v = 0 and i % 100 = 3..4 or v != 0 @integer 3, 4, 103, 104, 203, 204, 303, 304, 403, 404, 503, 504, 603, 604, 703, 704, 1003, … @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  // other: @integer 0, 5~19, 100, 1000, 10000, 100000, 1000000, …
  if (v = 0) and (i mod 100 = 1) then
    Result := pfOne
  else if (v = 0) and (i mod 100 = 2) then
    Result := pfTwo
  else if (v = 0) and (i mod 100 in [3..4]) or (v <> 0) then
    Result := pfFew
  else
    Result := pfOther
end;

// Lower Sorbian, Upper Sorbian
function GetLowerSorbianPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: v = 0 and i % 100 = 1 or f % 100 = 1 @integer 1, 101, 201, 301, 401, 501, 601, 701, 1001, … @decimal 0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 10.1, 100.1, 1000.1, …
  // two: v = 0 and i % 100 = 2 or f % 100 = 2 @integer 2, 102, 202, 302, 402, 502, 602, 702, 1002, … @decimal 0.2, 1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2, 10.2, 100.2, 1000.2, …
  // few: v = 0 and i % 100 = 3..4 or f % 100 = 3..4 @integer 3, 4, 103, 104, 203, 204, 303, 304, 403, 404, 503, 504, 603, 604, 703, 704, 1003, … @decimal 0.3, 0.4, 1.3, 1.4, 2.3, 2.4, 3.3, 3.4, 4.3, 4.4, 5.3, 5.4, 6.3, 6.4, 7.3, 7.4, 10.3, 100.3, 1000.3, …
  // other: @integer 0, 5~19, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 0.5~1.0, 1.5~2.0, 2.5~2.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (v = 0) and (i mod 100 = 1) or (f mod 100 = 1) then
    Result := pfOne
  else if (v = 0) and (i mod 100 = 2) or (f mod 100 = 2) then
    Result := pfTwo
  else if (v = 0) and (i mod 100 in [3..4]) or (f mod 100 in [3..4]) then
    Result := pfFew
  else
    Result := pfOther
end;

// Hebrew, iw
function GetHebrewPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: i = 1 and v = 0 @integer 1
  // two: i = 2 and v = 0 @integer 2
  // many: v = 0 and n != 0..10 and n % 10 = 0 @integer 20, 30, 40, 50, 60, 70, 80, 90, 100, 1000, 10000, 100000, 1000000, …
  // other: @integer 0, 3~17, 101, 1001, … @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (i = 1) and (v = 0) then
    Result := pfOne
  else if (i = 2) and (v = 0) then
    Result := pfTwo
  else if (v = 0) and (n >= 0) and (n <= 10) and (Trunc(n) mod 10 = 0) then
    Result := pfMany
  else
    Result := pfOther
end;

// Czech, Slovak
function GetCzechPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: i = 1 and v = 0 @integer 1
  // few: i = 2..4 and v = 0 @integer 2~4
  // many: v != 0   @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  // other: @integer 0, 5~19, 100, 1000, 10000, 100000, 1000000, …
  if (i = 1) and (v = 0) then
    Result := pfOne
  else if (i in [2..4]) and (v = 0) then
    Result := pfFew
  else if (v <> 0) then
    Result := pfMany
  else
    Result := pfOther
end;

// Polish
function GetPolishPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: i = 1 and v = 0 @integer 1
  // few: v = 0 and i % 10 = 2..4 and i % 100 != 12..14 @integer 2~4, 22~24, 32~34, 42~44, 52~54, 62, 102, 1002, …
  // many: v = 0 and i != 1 and i % 10 = 0..1 or v = 0 and i % 10 = 5..9 or v = 0 and i % 100 = 12..14 @integer 0, 5~19, 100, 1000, 10000, 100000, 1000000, …
  // other: @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (i = 1) and (v = 0) then
    Result := pfOne
  else if (v = 0) and (i mod 10 in [2..4]) and not (i mod 100 in [12..14]) then
    Result := pfFew
  else if (v = 0) and (i <> 1) and (i mod 10 in [0..1]) or (v = 0) and (i mod 10 in [5..9]) or (v = 0) and (i mod 100 in [12..14]) then
    Result := pfMany
  else
    Result := pfOther
end;

// Belarusian
function GetBelarusianPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: n % 10 = 1 and n % 100 != 11 @integer 1, 21, 31, 41, 51, 61, 71, 81, 101, 1001, … @decimal 1.0, 21.0, 31.0, 41.0, 51.0, 61.0, 71.0, 81.0, 101.0, 1001.0, …
  // few: n % 10 = 2..4 and n % 100 != 12..14 @integer 2~4, 22~24, 32~34, 42~44, 52~54, 62, 102, 1002, … @decimal 2.0, 3.0, 4.0, 22.0, 23.0, 24.0, 32.0, 33.0, 102.0, 1002.0, …
  // many: n % 10 = 0 or n % 10 = 5..9 or n % 100 = 11..14 @integer 0, 5~19, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  // other: @decimal 0.1~0.9, 1.1~1.7, 10.1, 100.1, 1000.1, …
  if (Trunc(n) mod 10 = 1) and (Trunc(n) mod 100 <> 11) then
    Result := pfOne
  else if (Trunc(n) mod 10 in [2..4]) and not (Trunc(n) mod 100 in [12..14]) then
    Result := pfFew
  else if (Trunc(n) mod 10 = 0) or (Trunc(n) mod 10 in [5..9]) or (Trunc(n) mod 100 in [11..14]) then
    Result := pfMany
  else
    Result := pfOther
end;

// Lithuanian
function GetLithuanianPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: n % 10 = 1 and n % 100 != 11..19 @integer 1, 21, 31, 41, 51, 61, 71, 81, 101, 1001, … @decimal 1.0, 21.0, 31.0, 41.0, 51.0, 61.0, 71.0, 81.0, 101.0, 1001.0, …
  // few: n % 10 = 2..9 and n % 100 != 11..19 @integer 2~9, 22~29, 102, 1002, … @decimal 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 22.0, 102.0, 1002.0, …
  // many: f != 0   @decimal 0.1~0.9, 1.1~1.7, 10.1, 100.1, 1000.1, …
  // other: @integer 0, 10~20, 30, 40, 50, 60, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (Trunc(n) mod 10 = 1) and not (Trunc(n) mod 100 in [11..19]) then
    Result := pfOne
  else if (Trunc(n) mod 10 in [2..9]) and not (Trunc(n) mod 100 in [11..19]) then
    Result := pfFew
  else if (f <> 0) then
    Result := pfMany
  else
    Result := pfOther
end;

// Maltese
function GetMaltesePlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: n = 1 @integer 1 @decimal 1.0, 1.00, 1.000, 1.0000
  // few: n = 0 or n % 100 = 2..10 @integer 0, 2~10, 102~107, 1002, … @decimal 0.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 10.0, 102.0, 1002.0, …
  // many: n % 100 = 11..19 @integer 11~19, 111~117, 1011, … @decimal 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 111.0, 1011.0, …
  // other: @integer 20~35, 100, 1000, 10000, 100000, 1000000, … @decimal 0.1~0.9, 1.1~1.7, 10.1, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (n = 1) then
    Result := pfOne
  else if (n = 0) or (Trunc(n) mod 100 in [2..10]) then
    Result := pfFew
  else if (Trunc(n) mod 100 in [11..19]) then
    Result := pfMany
  else
    Result := pfOther
end;

// Russian, Ukrainian
function GetRussianPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: v = 0 and i % 10 = 1 and i % 100 != 11 @integer 1, 21, 31, 41, 51, 61, 71, 81, 101, 1001, …
  // few: v = 0 and i % 10 = 2..4 and i % 100 != 12..14 @integer 2~4, 22~24, 32~34, 42~44, 52~54, 62, 102, 1002, …
  // many: v = 0 and i % 10 = 0 or v = 0 and i % 10 = 5..9 or v = 0 and i % 100 = 11..14 @integer 0, 5~19, 100, 1000, 10000, 100000, 1000000, …
  // other: @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (v = 0) and (i mod 10 = 1) and (i mod 100 <> 11) then
    Result := pfOne
  else if (v = 0) and (i mod 10 in [2..4]) and not (i mod 100 in [12..14]) then
    Result := pfFew
  else if (v = 0) and (i mod 10 = 0) or (v = 0) and (i mod 10 in [5..9]) or (v = 0) and (i mod 100 in [11..14]) then
    Result := pfMany
  else
    Result := pfOther
end;

// Breton
function GetBretonPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: n % 10 = 1 and n % 100 != 11,71,91 @integer 1, 21, 31, 41, 51, 61, 81, 101, 1001, … @decimal 1.0, 21.0, 31.0, 41.0, 51.0, 61.0, 81.0, 101.0, 1001.0, …
  // two: n % 10 = 2 and n % 100 != 12,72,92 @integer 2, 22, 32, 42, 52, 62, 82, 102, 1002, … @decimal 2.0, 22.0, 32.0, 42.0, 52.0, 62.0, 82.0, 102.0, 1002.0, …
  // few: n % 10 = 3..4,9 and n % 100 != 10..19,70..79,90..99 @integer 3, 4, 9, 23, 24, 29, 33, 34, 39, 43, 44, 49, 103, 1003, … @decimal 3.0, 4.0, 9.0, 23.0, 24.0, 29.0, 33.0, 34.0, 103.0, 1003.0, …
  // many: n != 0 and n % 1000000 = 0 @integer 1000000, … @decimal 1000000.0, 1000000.00, 1000000.000, …
  // other: @integer 0, 5~8, 10~20, 100, 1000, 10000, 100000, … @decimal 0.0~0.9, 1.1~1.6, 10.0, 100.0, 1000.0, 10000.0, 100000.0, …
  if (Trunc(n) mod 10 = 1) and not (Trunc(n) mod 100 in [11,71,91]) then
    Result := pfOne
  else if (Trunc(n) mod 10 = 2) and not (Trunc(n) mod 100 in [12,72,92]) then
    Result := pfTwo
  else if (Trunc(n) mod 10 in [3..4,9]) and not (Trunc(n) mod 100 in [10..19,70..79,90..99]) then
    Result := pfFew
  else if (n <> 0) and (Trunc(n) mod 1000000 = 0) then
    Result := pfMany
  else
    Result := pfOther
end;

// Irish
function GetIrishPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: n = 1 @integer 1 @decimal 1.0, 1.00, 1.000, 1.0000
  // two: n = 2 @integer 2 @decimal 2.0, 2.00, 2.000, 2.0000
  // few: n = 3..6 @integer 3~6 @decimal 3.0, 4.0, 5.0, 6.0, 3.00, 4.00, 5.00, 6.00, 3.000, 4.000, 5.000, 6.000, 3.0000, 4.0000, 5.0000, 6.0000
  // many: n = 7..10 @integer 7~10 @decimal 7.0, 8.0, 9.0, 10.0, 7.00, 8.00, 9.00, 10.00, 7.000, 8.000, 9.000, 10.000, 7.0000, 8.0000, 9.0000, 10.0000
  // other: @integer 0, 11~25, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~0.9, 1.1~1.6, 10.1, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (n = 1) then
    Result := pfOne
  else if (n = 2) then
    Result := pfTwo
  else if (n >= 3) and (n <= 6) then
    Result := pfFew
  else if (n >= 7) and (n <= 10) then
    Result := pfMany
  else
    Result := pfOther
end;

// Manx
function GetManxPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // one: v = 0 and i % 10 = 1 @integer 1, 11, 21, 31, 41, 51, 61, 71, 101, 1001, …
  // two: v = 0 and i % 10 = 2 @integer 2, 12, 22, 32, 42, 52, 62, 72, 102, 1002, …
  // few: v = 0 and i % 100 = 0,20,40,60,80 @integer 0, 20, 40, 60, 80, 100, 120, 140, 1000, 10000, 100000, 1000000, …
  // many: v != 0   @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  // other: @integer 3~10, 13~19, 23, 103, 1003, …
  if (v = 0) and (i mod 10 = 1) then
    Result := pfOne
  else if (v = 0) and (i mod 10 = 2) then
    Result := pfTwo
  else if (v = 0) and (i mod 100 in [0,20,40,60,80]) then
    Result := pfFew
  else if (v <> 0) then
    Result := pfMany
  else
    Result := pfOther
end;

// Arabic, ars
function GetArabicPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // zero: n = 0 @integer 0 @decimal 0.0, 0.00, 0.000, 0.0000
  // one: n = 1 @integer 1 @decimal 1.0, 1.00, 1.000, 1.0000
  // two: n = 2 @integer 2 @decimal 2.0, 2.00, 2.000, 2.0000
  // few: n % 100 = 3..10 @integer 3~10, 103~110, 1003, … @decimal 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 103.0, 1003.0, …
  // many: n % 100 = 11..99 @integer 11~26, 111, 1011, … @decimal 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 111.0, 1011.0, …
  // other: @integer 100~102, 200~202, 300~302, 400~402, 500~502, 600, 1000, 10000, 100000, 1000000, … @decimal 0.1~0.9, 1.1~1.7, 10.1, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (n = 0) then
    Result := pfZero
  else if (n = 1) then
    Result := pfOne
  else if (n = 2) then
    Result := pfTwo
  else if (Trunc(n) mod 100 in [3..10]) then
    Result := pfFew
  else if (Trunc(n) mod 100 in [11..99]) then
    Result := pfMany
  else
    Result := pfOther
end;

// Welsh
function GetWelshPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;
begin
  // zero: n = 0 @integer 0 @decimal 0.0, 0.00, 0.000, 0.0000
  // one: n = 1 @integer 1 @decimal 1.0, 1.00, 1.000, 1.0000
  // two: n = 2 @integer 2 @decimal 2.0, 2.00, 2.000, 2.0000
  // few: n = 3 @integer 3 @decimal 3.0, 3.00, 3.000, 3.0000
  // many: n = 6 @integer 6 @decimal 6.0, 6.00, 6.000, 6.0000
  // other: @integer 4, 5, 7~20, 100, 1000, 10000, 100000, 1000000, … @decimal 0.1~0.9, 1.1~1.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
  if (n = 0) then
    Result := pfZero
  else if (n = 1) then
    Result := pfOne
  else if (n = 2) then
    Result := pfTwo
  else if (n = 3) then
    Result := pfFew
  else if (n = 6) then
    Result := pfMany
  else
    Result := pfOther
end;

initialization
  TMultiPattern.Register('bm', GetSinglePlural); // Bamanankan
  TMultiPattern.Register('bo', GetSinglePlural); // Tibetan
  TMultiPattern.Register('dz', GetSinglePlural); // Dzongkha
  TMultiPattern.Register('id', GetSinglePlural); // Indonesian
  TMultiPattern.Register('ig', GetSinglePlural); // Igbo
  TMultiPattern.Register('ii', GetSinglePlural); // Yi
  TMultiPattern.Register('in', GetSinglePlural); // in
  TMultiPattern.Register('ja', GetSinglePlural); // Japanese
  TMultiPattern.Register('jbo', GetSinglePlural); // jbo
  TMultiPattern.Register('jv', GetSinglePlural); // Javanese
  TMultiPattern.Register('jw', GetSinglePlural); // jw
  TMultiPattern.Register('kde', GetSinglePlural); // Makonde
  TMultiPattern.Register('kea', GetSinglePlural); // Kabuverdianu
  TMultiPattern.Register('km', GetSinglePlural); // Khmer
  TMultiPattern.Register('ko', GetSinglePlural); // Korean
  TMultiPattern.Register('lkt', GetSinglePlural); // Lakota
  TMultiPattern.Register('lo', GetSinglePlural); // Lao
  TMultiPattern.Register('ms', GetSinglePlural); // Malay
  TMultiPattern.Register('my', GetSinglePlural); // Burmese
  TMultiPattern.Register('nqo', GetSinglePlural); // N'ko
  TMultiPattern.Register('root', GetSinglePlural); // root
  TMultiPattern.Register('sah', GetSinglePlural); // Sakha
  TMultiPattern.Register('ses', GetSinglePlural); // Koyraboro Senni
  TMultiPattern.Register('sg', GetSinglePlural); // Sango
  TMultiPattern.Register('th', GetSinglePlural); // Thai
  TMultiPattern.Register('to', GetSinglePlural); // Tongan
  TMultiPattern.Register('vi', GetSinglePlural); // Vietnamese
  TMultiPattern.Register('wo', GetSinglePlural); // Wolof
  TMultiPattern.Register('yo', GetSinglePlural); // Yoruba
  TMultiPattern.Register('yue', GetSinglePlural); // yue
  TMultiPattern.Register('zh', GetSinglePlural); // Chinese, Simplified
  TMultiPattern.Register('am', GetAmPlural); // Amharic
  TMultiPattern.Register('as', GetAmPlural); // Assamese
  TMultiPattern.Register('bn', GetAmPlural); // Bangla
  TMultiPattern.Register('fa', GetAmPlural); // Persian
  TMultiPattern.Register('gu', GetAmPlural); // Gujarati
  TMultiPattern.Register('hi', GetAmPlural); // Hindi
  TMultiPattern.Register('kn', GetAmPlural); // Kannada
  TMultiPattern.Register('mr', GetAmPlural); // Marathi
  TMultiPattern.Register('zu', GetAmPlural); // isiZulu
  TMultiPattern.Register('ff', GetFfPlural); // Fulah
  TMultiPattern.Register('fr', GetFfPlural); // French
  TMultiPattern.Register('hy', GetFfPlural); // Armenian
  TMultiPattern.Register('kab', GetFfPlural); // Kabyle
  TMultiPattern.Register('ast', GetAstPlural); // Asturian
  TMultiPattern.Register('ca', GetAstPlural); // Catalan
  TMultiPattern.Register('de', GetAstPlural); // German
  TMultiPattern.Register('en', GetAstPlural); // English
  TMultiPattern.Register('et', GetAstPlural); // Estonian
  TMultiPattern.Register('fi', GetAstPlural); // Finnish
  TMultiPattern.Register('fy', GetAstPlural); // Western Frisian
  TMultiPattern.Register('gl', GetAstPlural); // Galician
  TMultiPattern.Register('it', GetAstPlural); // Italian
  TMultiPattern.Register('ji', GetAstPlural); // ji
  TMultiPattern.Register('nl', GetAstPlural); // Dutch
  TMultiPattern.Register('sv', GetAstPlural); // Swedish
  TMultiPattern.Register('sw', GetAstPlural); // Kiswahili
  TMultiPattern.Register('ur', GetAstPlural); // Urdu
  TMultiPattern.Register('yi', GetAstPlural); // Yiddish
  TMultiPattern.Register('si', GetSinhalaPlural); // Sinhala
  TMultiPattern.Register('ak', GetAkPlural); // Akan
  TMultiPattern.Register('bh', GetAkPlural); // bh
  TMultiPattern.Register('guw', GetAkPlural); // guw
  TMultiPattern.Register('ln', GetAkPlural); // Lingala
  TMultiPattern.Register('mg', GetAkPlural); // Malagasy
  TMultiPattern.Register('nso', GetAkPlural); // Sesotho sa Leboa
  TMultiPattern.Register('pa', GetAkPlural); // Punjabi
  TMultiPattern.Register('ti', GetAkPlural); // Tigrinya
  TMultiPattern.Register('wa', GetAkPlural); // wa
  TMultiPattern.Register('tzm', GetCentralAtlasTamazightPlural); // Central Atlas Tamazight
  TMultiPattern.Register('pt', GetPortuguesePlural); // Portuguese
  TMultiPattern.Register('af', GetAfPlural); // Afrikaans
  TMultiPattern.Register('asa', GetAfPlural); // Asu
  TMultiPattern.Register('az', GetAfPlural); // Azerbaijani, Latin
  TMultiPattern.Register('bem', GetAfPlural); // Bemba
  TMultiPattern.Register('bez', GetAfPlural); // Bena
  TMultiPattern.Register('bg', GetAfPlural); // Bulgarian
  TMultiPattern.Register('brx', GetAfPlural); // Bodo
  TMultiPattern.Register('ce', GetAfPlural); // Chechen
  TMultiPattern.Register('cgg', GetAfPlural); // Chiga
  TMultiPattern.Register('chr', GetAfPlural); // Cherokee
  TMultiPattern.Register('ckb', GetAfPlural); // ckb
  TMultiPattern.Register('dv', GetAfPlural); // Divehi
  TMultiPattern.Register('ee', GetAfPlural); // Ewe
  TMultiPattern.Register('el', GetAfPlural); // Greek
  TMultiPattern.Register('eo', GetAfPlural); // Esperanto
  TMultiPattern.Register('es', GetAfPlural); // Spanish
  TMultiPattern.Register('eu', GetAfPlural); // Basque
  TMultiPattern.Register('fo', GetAfPlural); // Faroese
  TMultiPattern.Register('fur', GetAfPlural); // Friulian
  TMultiPattern.Register('gsw', GetAfPlural); // Swiss German
  TMultiPattern.Register('ha', GetAfPlural); // Hausa
  TMultiPattern.Register('haw', GetAfPlural); // Hawaiian
  TMultiPattern.Register('hu', GetAfPlural); // Hungarian
  TMultiPattern.Register('jgo', GetAfPlural); // Ngomba
  TMultiPattern.Register('jmc', GetAfPlural); // Machame
  TMultiPattern.Register('ka', GetAfPlural); // Georgian
  TMultiPattern.Register('kaj', GetAfPlural); // kaj
  TMultiPattern.Register('kcg', GetAfPlural); // kcg
  TMultiPattern.Register('kk', GetAfPlural); // Kazakh
  TMultiPattern.Register('kkj', GetAfPlural); // Kako
  TMultiPattern.Register('kl', GetAfPlural); // Greenlandic
  TMultiPattern.Register('ks', GetAfPlural); // Kashmiri
  TMultiPattern.Register('ksb', GetAfPlural); // Shambala
  TMultiPattern.Register('ku', GetAfPlural); // Central Kurdish
  TMultiPattern.Register('ky', GetAfPlural); // Kyrgyz
  TMultiPattern.Register('lb', GetAfPlural); // Luxembourgish
  TMultiPattern.Register('lg', GetAfPlural); // Ganda
  TMultiPattern.Register('mas', GetAfPlural); // Masai
  TMultiPattern.Register('mgo', GetAfPlural); // Meta'
  TMultiPattern.Register('ml', GetAfPlural); // Malayalam
  TMultiPattern.Register('mn', GetAfPlural); // Mongolian, Cyrillic
  TMultiPattern.Register('nah', GetAfPlural); // nah
  TMultiPattern.Register('nb', GetAfPlural); // Norwegian Bokmål
  TMultiPattern.Register('nd', GetAfPlural); // North Ndebele
  TMultiPattern.Register('ne', GetAfPlural); // Nepali
  TMultiPattern.Register('nn', GetAfPlural); // Norwegian Nynorsk
  TMultiPattern.Register('nnh', GetAfPlural); // Ngiemboon
  TMultiPattern.Register('no', GetAfPlural); // Norwegian
  TMultiPattern.Register('nr', GetAfPlural); // South Ndebele
  TMultiPattern.Register('ny', GetAfPlural); // ny
  TMultiPattern.Register('nyn', GetAfPlural); // Nyankole
  TMultiPattern.Register('om', GetAfPlural); // Oromo
  TMultiPattern.Register('or', GetAfPlural); // Odia
  TMultiPattern.Register('os', GetAfPlural); // Ossetic
  TMultiPattern.Register('pap', GetAfPlural); // Papiamento
  TMultiPattern.Register('ps', GetAfPlural); // Pashto
  TMultiPattern.Register('rm', GetAfPlural); // Romansh
  TMultiPattern.Register('rof', GetAfPlural); // Rombo
  TMultiPattern.Register('rwk', GetAfPlural); // Rwa
  TMultiPattern.Register('saq', GetAfPlural); // Samburu
  TMultiPattern.Register('sdh', GetAfPlural); // sdh
  TMultiPattern.Register('seh', GetAfPlural); // Sena
  TMultiPattern.Register('sn', GetAfPlural); // Shona
  TMultiPattern.Register('so', GetAfPlural); // Somali
  TMultiPattern.Register('sq', GetAfPlural); // Albanian
  TMultiPattern.Register('ss', GetAfPlural); // siSwati
  TMultiPattern.Register('ssy', GetAfPlural); // Saho
  TMultiPattern.Register('st', GetAfPlural); // Sesotho
  TMultiPattern.Register('syr', GetAfPlural); // Syriac
  TMultiPattern.Register('ta', GetAfPlural); // Tamil
  TMultiPattern.Register('te', GetAfPlural); // Telugu
  TMultiPattern.Register('teo', GetAfPlural); // Teso
  TMultiPattern.Register('tig', GetAfPlural); // Tigre
  TMultiPattern.Register('tk', GetAfPlural); // Turkmen
  TMultiPattern.Register('tn', GetAfPlural); // Setswana
  TMultiPattern.Register('tr', GetAfPlural); // Turkish
  TMultiPattern.Register('ts', GetAfPlural); // Tsonga
  TMultiPattern.Register('ug', GetAfPlural); // Uyghur
  TMultiPattern.Register('uz', GetAfPlural); // Uzbek, Latin
  TMultiPattern.Register('ve', GetAfPlural); // Venda
  TMultiPattern.Register('vo', GetAfPlural); // Volapük
  TMultiPattern.Register('vun', GetAfPlural); // Vunjo
  TMultiPattern.Register('wae', GetAfPlural); // Walser
  TMultiPattern.Register('xh', GetAfPlural); // isiXhosa
  TMultiPattern.Register('xog', GetAfPlural); // Soga
  TMultiPattern.Register('pt-PT', GetPortuguesePortugalPlural); // Portuguese (Portugal)
  TMultiPattern.Register('da', GetDanishPlural); // Danish
  TMultiPattern.Register('is', GetIcelandicPlural); // Icelandic
  TMultiPattern.Register('mk', GetMacedonianPlural); // Macedonian
  TMultiPattern.Register('fil', GetFilipinoPlural); // Filipino
  TMultiPattern.Register('tl', GetFilipinoPlural); // tl
  TMultiPattern.Register('lv', GetLatvianPlural); // Latvian
  TMultiPattern.Register('prg', GetLatvianPlural); // Prussian
  TMultiPattern.Register('lag', GetLangiPlural); // Langi
  TMultiPattern.Register('ksh', GetColognianPlural); // Colognian
  TMultiPattern.Register('iu', GetIuPlural); // Inuktitut, Latin
  TMultiPattern.Register('kw', GetIuPlural); // Cornish
  TMultiPattern.Register('naq', GetIuPlural); // Nama
  TMultiPattern.Register('se', GetIuPlural); // Northern Sami
  TMultiPattern.Register('sma', GetIuPlural); // Sami, Southern
  TMultiPattern.Register('smi', GetIuPlural); // smi
  TMultiPattern.Register('smj', GetIuPlural); // Sami, Lule
  TMultiPattern.Register('smn', GetIuPlural); // Sami, Inari
  TMultiPattern.Register('sms', GetIuPlural); // Sami, Skolt
  TMultiPattern.Register('shi', GetTachelhitPlural); // Tachelhit
  TMultiPattern.Register('mo', GetRomanianPlural); // mo
  TMultiPattern.Register('ro', GetRomanianPlural); // Romanian
  TMultiPattern.Register('bs', GetBsPlural); // Bosnian, Latin
  TMultiPattern.Register('hr', GetBsPlural); // Croatian
  TMultiPattern.Register('sh', GetBsPlural); // sh
  TMultiPattern.Register('sr', GetBsPlural); // Serbian, Cyrillic
  TMultiPattern.Register('gd', GetScottishGaelicPlural); // Scottish Gaelic
  TMultiPattern.Register('sl', GetSlovenianPlural); // Slovenian
  TMultiPattern.Register('dsb', GetLowerSorbianPlural); // Lower Sorbian
  TMultiPattern.Register('hsb', GetLowerSorbianPlural); // Upper Sorbian
  TMultiPattern.Register('he', GetHebrewPlural); // Hebrew
  TMultiPattern.Register('iw', GetHebrewPlural); // iw
  TMultiPattern.Register('cs', GetCzechPlural); // Czech
  TMultiPattern.Register('sk', GetCzechPlural); // Slovak
  TMultiPattern.Register('pl', GetPolishPlural); // Polish
  TMultiPattern.Register('be', GetBelarusianPlural); // Belarusian
  TMultiPattern.Register('lt', GetLithuanianPlural); // Lithuanian
  TMultiPattern.Register('mt', GetMaltesePlural); // Maltese
  TMultiPattern.Register('ru', GetRussianPlural); // Russian
  TMultiPattern.Register('uk', GetRussianPlural); // Ukrainian
  TMultiPattern.Register('br', GetBretonPlural); // Breton
  TMultiPattern.Register('ga', GetIrishPlural); // Irish
  TMultiPattern.Register('gv', GetManxPlural); // Manx
  TMultiPattern.Register('ar', GetArabicPlural); // Arabic
  TMultiPattern.Register('ars', GetArabicPlural); // ars
  TMultiPattern.Register('cy', GetWelshPlural); // Welsh
end.
