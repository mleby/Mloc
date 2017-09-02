unit uIndexMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function IndexMenu(const aPath: string): longint;
function LoadMenuFromLines(Const aLines: TStringList; const aPath: string): longint;

implementation

uses
  uAppContext, FileUtil, uIndexCmd, uMenuItem, uTools, process, StreamIO, strutils;

function IndexMenu(const aPath: string): longint;
var
  sl: TStringList;
begin
  Result := 0;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(aPath);
    Result := LoadMenuFromLines(sl, aPath);
  finally
    FreeAndNil(sl);
  end;
end;

function LoadMenuFromLines(Const aLines: TStringList; const aPath: string): longint;
var
  lLine, lName: string;
  i: integer;
  lMenuItemParser: TMenuItemParser;
begin
  Result := 0;

  // insert into menu
  for i := 0 to aLines.Count - 1 do
  begin
    lLine := Trim(aLines[i]);
    lLine := DelSpace1(lLine);
    if (lLine <> '') and not AnsiStartsStr('#', lLine) then
    begin
      lMenuItemParser := TMenuItemParser.Create(lLine);
      try
        if (lMenuItemParser.itemType in [MITprog, MITrunonce]) and (lMenuItemParser.cmd <> '') then
        begin
          if lMenuItemParser.Name <> '' then
            lName := lMenuItemParser.Name
          else
            lName := lMenuItemParser.cmd;

          insertCmd('#' + aPath + '#/' + lMenuItemParser.cmd, lName, lMenuItemParser.cmd, false);
          Inc(Result);
        End;
      finally
        FreeAndNil(lMenuItemParser);
      end;
    end;
  end;
end;

end.

