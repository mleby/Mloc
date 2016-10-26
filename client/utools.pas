unit uTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils;

function removeDiacritics(aStr: string): string;
function NormalizeTerm(const aSearchTerm: string): string;
function StripNonAlphaNumeric(const aValue: string): string;
function CopyAndSplitCammelCaseString(const aValue: string): string;
function GlobCheck(const aGlob, aValue: string): boolean;
function GlobCheckAll(const aGlobList, aValue: string): boolean;

implementation

uses
  character;

function NormalizeTerm(const aSearchTerm: string): string;
var
  lSearchTerm: string;
begin
  lSearchTerm := removeDiacritics(aSearchTerm);
  lSearchTerm := LowerCase(lSearchTerm);
  lSearchTerm := Trim(lSearchTerm);
  lSearchTerm := StripNonAlphaNumeric(lSearchTerm);
  lSearchTerm := DelSpace1(lSearchTerm);
  lSearchTerm := Trim(lSearchTerm);
  Result := lSearchTerm;
end;

function removeDiacritics(aStr: string): string;
begin
  Result := aStr;

  //Čeština: á, é, í, ó, ú, ý, č, ď, ě, ň, ř, š, ť, ž, ů
  Result := StringReplace(Result, 'á', 'a', [rfReplaceAll]);
  Result := StringReplace(Result, 'é', 'e', [rfReplaceAll]);
  Result := StringReplace(Result, 'í', 'i', [rfReplaceAll]);
  Result := StringReplace(Result, 'ó', 'o', [rfReplaceAll]);
  Result := StringReplace(Result, 'ú', 'u', [rfReplaceAll]);
  Result := StringReplace(Result, 'ý', 'y', [rfReplaceAll]);
  Result := StringReplace(Result, 'č', 'c', [rfReplaceAll]);
  Result := StringReplace(Result, 'ď', 'd', [rfReplaceAll]);
  Result := StringReplace(Result, 'ě', 'e', [rfReplaceAll]);
  Result := StringReplace(Result, 'ň', 'n', [rfReplaceAll]);
  Result := StringReplace(Result, 'ř', 'r', [rfReplaceAll]);
  Result := StringReplace(Result, 'š', 's', [rfReplaceAll]);
  Result := StringReplace(Result, 'ť', 't', [rfReplaceAll]);
  Result := StringReplace(Result, 'ž', 'z', [rfReplaceAll]);
  Result := StringReplace(Result, 'ů', 'u', [rfReplaceAll]);
  Result := StringReplace(Result, 'Á', 'A', [rfReplaceAll]);
  Result := StringReplace(Result, 'É', 'E', [rfReplaceAll]);
  Result := StringReplace(Result, 'Í', 'I', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ó', 'O', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ú', 'U', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ý', 'Y', [rfReplaceAll]);
  Result := StringReplace(Result, 'Č', 'C', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ď', 'D', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ě', 'E', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ň', 'N', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ř', 'R', [rfReplaceAll]);
  Result := StringReplace(Result, 'Š', 'S', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ť', 'T', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ž', 'Z', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ů', 'U', [rfReplaceAll]);

  //Dánština a norština: å, ø
  //Esperanto: ĉ, ĝ, ĥ, ĵ, ŝ, ŭ
  //Estonština: š, ž, õ, ä, ö, ü
  //Francouzština: à, â, ç, é, è, ê, ë, î, ï, ô, ù, û, ü, ÿ
  //Chorvatština a bosenština: ž, š, č, ć, đ
  //Irština: á, é, í, ó, ú
  //Lotyština: ā, ē, ī, ū, č, š, ž, ļ, ķ, ņ, ģ
  //Maďarština: á, é, í, ó, ú, ö, ü, ő, ű
  //Němčina: ä, ö, ü
  //Nizozemština ë, ï
  //Polština: ą, ć, ę, ł, ń, ó, ś, ź, ż
  //Rumunština: ă, â, î, ș, ț
  //Slovenština: á, ä, č, ď, é, í, ĺ, ľ, ň, ó, ô, ŕ, š, ť, ú, ý, ž
  //Španělština: ñ
  //Švédština: å, ä, ö
  //Turečtina: ç, ş, ğ
  //Vietnamština: ă, â, đ, ê, ô, ơ, ư
end;

function StripNonAlphaNumeric(const aValue: string): string;
var
  C: char;
begin
  Result := '';
  for C in aValue do
    if IsLetterOrDigit(C) then
      Result := Result + C
    else
      Result := Result + ' ';
end;

function CopyAndSplitCammelCaseString(const aValue: string): string;
const
  ALPHA_CHARS = ['a'..'z', 'A'..'Z', '0'..'9'];
var
  C, O: char;
  lUpraveno: string;
begin
  O := ' ';
  Result := aValue;
  for C in aValue do
  begin
    if (C = UpperCase(C)) and (O in ALPHA_CHARS) then
      lUpraveno := lUpraveno + ' ';
    lUpraveno := lUpraveno + C;
    O := C;
  end;

  if Result <> lUpraveno then
    Result := DelSpace1(Result + ' ' + lUpraveno);
end;

Function GlobCheck(Const aGlob, aValue: string): boolean;
Var
  lFirst, lLast: Char;
  lStrip: String;
Begin
  lFirst := aGlob[1];
  lLast := aGlob[Length(aGlob)];

  if (lFirst = '*') and (lLast = '*') then
  begin
    lStrip := Copy(aGlob, 2, Length(aGlob) - 2);
    Result := AnsiContainsText(aValue, lStrip);
  //else if
  End
  else if (lFirst = '*') and (lLast <> '*') then
  begin
    lStrip := Copy(aGlob, 2, Length(aGlob));
    Result := AnsiEndsText(lStrip, aValue);
  End
  else if (lFirst <> '*') and (lLast = '*') then
  begin
    lStrip := Copy(aGlob, 1, Length(aGlob) - 1);
    Result := AnsiStartsText(lStrip, aValue);
  End
  else if (lFirst <> '*') and (lLast <> '*') then
  begin
    Result := SameText(aGlob, aValue);
  End;
end;

Function GlobCheckAll(Const aGlobList, aValue: string): boolean;
var
  sl: TStringList;
  i: Integer;
Begin
  sl := TStringList.Create;
  try
    //sl.StrictDelimiter := true;
    sl.Delimiter := ':';
    sl.DelimitedText := aGlobList;

    for i := 0 to sl.Count - 1 do
    begin
      if GlobCheck(sl[i], aValue) then
      begin
        Result := true;
        break;
      End;
    End;

  finally
    sl.Free;
  end;
end;

end.

