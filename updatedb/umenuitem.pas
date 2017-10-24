unit uMenuItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMenuItemParser }
  TMenuItemType = (MITprog, MITmenu, MITrunonce, MITmenufile, MITmenuprog, MITmenuprogreload, MITseparator, MITEndMenu, MITNone, MITMenusearch);

  TMenuItemParser = class(TObject)
  private
    FCmd: string;
    FIcon: string;
    FInputLine: string;
    FItemType: TMenuItemType;
    FMenuId: integer;
    FName: string;
    FShortCut: string;
    FSubMenuCmd: string;
    FSubMenuId: integer;
    FSubMenuPath: string;
    FSubMenuReloadInterval: integer;


    function GetSearch: string;
    function GetSubMenuChar: string;
    Procedure QuoteTrim(Var lName: String); // remove leading and ending quotes
    Procedure setNameAndShotrCutKey(Const aName: String);

    function SplitMenuLine(const aLine: string): TStringList;

    procedure startNewMenu(const aLine: string);
    procedure endMenu;

    procedure prepareProg(const aLine: string);
    procedure prepareProgreload(const aLine: string);
    procedure prepareSeparator(const aLine: string);
    procedure includeItems(const aLine: string);
  public

    constructor Create(const aLine: string);
    destructor Destroy; override;

    property Name: string read FName;
    property icon: string read FIcon;
    property cmd: string read FCmd;
    property menuId: integer read FMenuId;
    property itemType: TMenuItemType read FItemType;
    property search: string read GetSearch;
    property shortcut: string read FShortCut;
    property subMenuId: integer read FSubMenuId;
    property subMenuPath: string read FSubMenuPath;
    property subMenuCmd: string read FSubMenuCmd;
    property subMenuReloadInterval: integer read FSubMenuReloadInterval;
    property subMenuChar: string read GetSubMenuChar;
  end;

function MitToStr(const aMenuType: TMenuItemType): string;
function strToMit(const aMenuTypeStr: string): TMenuItemType;


implementation

uses strutils;

function MitToStr(const aMenuType: TMenuItemType): string;
begin
  WriteStr(Result, aMenuType);
end;

function strToMit(const aMenuTypeStr: string): TMenuItemType;
begin
  ReadStr(aMenuTypeStr, Result);
end;

{ TMenuItemParser }

procedure TMenuItemParser.prepareProg(const aLine: string);
var
  lSl: TStringList;
  i: integer;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITprog;

    setNameAndShotrCutKey(lSl[1]);

    FIcon := lSl[2];
    QuoteTrim(FIcon);

    for i := 3 to lsl.Count - 1 do
      FCmd := FCmd + ' ' + lsl[i];

    fCmd := Trim(FCmd);
  finally
    FreeAndNil(lSl);
  end;
end;

procedure TMenuItemParser.prepareProgreload(const aLine: string);
var
  lSl: TStringList;
  i: integer;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITmenuprogreload;

    setNameAndShotrCutKey(lSl[1]);
    FIcon := lSl[2];
    FSubMenuReloadInterval := StrToInt(lSl[3]);

    for i := 4 to lsl.Count - 1 do
      FSubMenuCmd := FSubMenuCmd + ' ' + lsl[i];

    FSubMenuCmd := Trim(FSubMenuCmd);
  finally
    FreeAndNil(lSl);
  end;
end;

procedure TMenuItemParser.prepareSeparator(const aLine: string);
var
  lSl: TStringList;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITseparator;

    if lSl.Count >= 2 then
      FName := lSl[1];

  finally
    FreeAndNil(lSl);
  end;
end;

procedure TMenuItemParser.includeItems(const aLine: string);
var
  lSl: TStringList;
  lFileName, lFileNameCfg: string;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITNone;

    lFileName := lSl[1];
    lFileNameCfg := GetEnvironmentVariable('HOME') + '/.icewm/' + lFileName;

    {TODO -oLebeda -cNone: dořešit include}
    if FileExists(lFileName) then
      //MainForm.LoadMenuFromFile(lFileName)
    else if FileExists(lFileNameCfg) then
      //MainForm.LoadMenuFromFile(lFileNameCfg);

  finally
    FreeAndNil(lSl);
  end;
end;

function TMenuItemParser.SplitMenuLine(const aLine: string): TStringList;
Var
  lLine: String;
begin
  Result := TStringList.Create;
  Result.Delimiter := ' ';
  lLine := StringReplace(aLine, '"', '"""', [rfReplaceAll]);
  Result.DelimitedText := lLine;
end;

procedure TMenuItemParser.startNewMenu(const aLine: string);
var
  lSl: TStringList;
begin
  //lSl := SplitMenuLine(aLine);
  //try
  //  FItemType := MITmenu;
  //
  //  setNameAndShotrCutKey(lSl[1]);
  //  FIcon := lSl[2];
  //  FSubMenuId := MainForm.AddMenu(FName, FMenuId);
  //finally
  //  FreeAndNil(lSl);
  //end;

end;

procedure TMenuItemParser.endMenu;
var
  lUpMenuId: integer;
begin
  //lUpMenuId := MainForm.SQLMenu.FieldByName('upMenuId').AsInteger;
  //FItemType := MITEndMenu;
  //MainForm.setActiveMenu(lUpMenuId);
end;

function TMenuItemParser.GetSearch: string;
begin
  Result := FName;
end;

function TMenuItemParser.GetSubMenuChar: string;
begin
  if FItemType in [MITmenu, MITmenufile, MITmenuprog, MITmenuprogreload] then
    Result := '>'
  else
    Result := '';
end;

Procedure TMenuItemParser.QuoteTrim(Var lName: String);
Begin
  If lName[1] = '"' Then
    Delete(lName, 1, 1);
  If lName[Length(lName)] = '"' Then
    Delete(lName, Length(lName), 1);
End;

procedure TMenuItemParser.setNameAndShotrCutKey(const aName: String);
Var
  i: Integer;
  lName: String;
Begin
  // identify explicit shortcut
  for i := 1 to Length(aName) do
  begin
    if (aName[i] = '_') and (FShortCut = '') and (aName[i + 1] <> '_') then
      FShortCut := LowerCase(aName[i + 1])
    else
      lName := lName + aName[i];
  End;

  QuoteTrim(lName);

  // generate implicit shortcut
  if FShortCut = '' then {TODO -oLebeda -cNone: generate shortcut more inteligent - use 1 unused letter}
    FShortCut := LowerCase(lName[1]);

  FName := lName;
End;

constructor TMenuItemParser.Create(const aLine: string);
begin
  FInputLine := aLine;
  //FMenuId := MainForm.SQLMenu.FieldByName('id').AsInteger;

  if AnsiStartsText('prog ', aLine) then
    prepareProg(aLine)
  else if AnsiStartsText('separator', aLine) then
    prepareSeparator(aLine)
  else if AnsiStartsText('menu ', aLine) then
    startNewMenu(aLine)
  else if AnsiStartsText('}', aLine) then
    endMenu
  else if AnsiStartsText('include ', aLine) then
    includeItems(aLine)
  else if AnsiStartsText('menuprogreload ', aLine) then
    prepareProgreload(aLine)
  else if AnsiStartsText('menusearch ', aLine) then
    prepareProgreload(aLine) {TODO -oLebeda -cNone: menusearch}
  // menufile
  // menuprog
  else
  begin
    FItemType := MITNone;
    WriteLn('!!!!!' + aLine);
  end;
end;

destructor TMenuItemParser.Destroy;
begin
  inherited Destroy;
end;

end.
