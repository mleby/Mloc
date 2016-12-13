unit uContentResolver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { IContentResolver }

  IContentResolver = interface
    ['{A426E92E-5F53-4C46-8969-AA047F30B81A}']
    function GetPath: string;
    function GetName: string;
    function GetDescription: string;
  end;

  { TBaseResolver }

  TBaseResolver = class(TInterfacedObject, IContentResolver)
  private
    FFileName: string;
  public
    constructor Create(const aFileName: string); virtual;
    function GetPath: string; virtual;
    function GetName: string; virtual;
    function GetDescription: string; virtual;
  end;

  { TDesktopResolver }

  TDesktopResolver = class(TInterfacedObject, IContentResolver)
  private
    FFileName: string;
    FName: string;
    FNameCz: string;
    FComment: string;
    FCommentCz: string;
    FKeywords: string;
    procedure GetValue(const lLine, aPrefix: string; var aVar: string);
  public
    constructor Create(const aFileName: string);
    function GetPath: string;
    function GetName: string;
    function GetDescription: string;
  end;

  { TMhtResolver }

  TMhtResolver = class(TBaseResolver, IContentResolver)
  private
  public
    function GetName: string; override;
  end;

function NewContentResolver(const aFileName: string): IContentResolver;

implementation

uses strutils, FileUtil, LazFileUtils;

function NewContentResolver(const aFileName: string): IContentResolver;
begin
  if AnsiEndsText('.desktop', aFileName) and FileIsReadable(aFileName) then
    Result := TDesktopResolver.Create(aFileName)
  else if AnsiEndsText('.mht', aFileName) then
    Result := TMhtResolver.Create(aFileName)
  else
    Result := TBaseResolver.Create(aFileName);
end;

{ TMhtResolver }

Function TMhtResolver.GetName: string;
Var
  lName: String;
Begin
  lName := Inherited;
  Result := StringReplace(lName, '_', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '.mht', ' ', [rfReplaceAll, rfIgnoreCase]);
End;

{ TBaseResolver }

constructor TBaseResolver.Create(const aFileName: string);
begin
  FFileName := aFileName;
end;

function TBaseResolver.GetPath: string;
begin
  Result := FFileName;
end;

function TBaseResolver.GetName: string;
begin
  Result := ExtractFileName(FFileName);
end;

function TBaseResolver.GetDescription: string;
begin
  Result := '';
end;

{ TDesktopResolver }

procedure TDesktopResolver.GetValue(const lLine, aPrefix: string; var aVar: string);
begin
  if AnsiStartsStr(aPrefix, lLine) then
  begin
    aVar := Copy(lLine, Pos('=', lLine) + 1, Length(lLine) - Pos('=', lLine));
  end;
end;

constructor TDesktopResolver.Create(const aFileName: string);
var
  F: Text;
  lLine: string;
begin
  FFileName := aFileName;

  AssignFile(F, FFileName);
  try
    Reset(F);
    while not EOF(F) do
    begin
      ReadLn(F, lLine);
      GetValue(lLine, 'Name=', FName);
      GetValue(lLine, 'Name[cz]=', FNameCz);
      GetValue(lLine, 'Comment=', FComment);
      GetValue(lLine, 'Comment[cz]=', FCommentCz);
      GetValue(lLine, 'Keywords=', FKeywords);
    end;
  finally
    Close(F);
  end;
end;

function TDesktopResolver.GetPath: string;
begin
  Result := FFileName;
end;

function TDesktopResolver.GetName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := ExtractFileName(FFileName);

  //WriteLn(Result);
end;

function TDesktopResolver.GetDescription: string;
begin
  Result := FName + ' ' + FNameCz + ' ' + FComment + ' ' + FCommentCz + ' ' + FKeywords + ' ' + ExtractFileName(FFileName);
  //WriteLn(Result);
end;

end.

