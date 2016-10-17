unit uConsoleOutput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TLogger }

  TLogger = class(TObject)
  private
    FDebug: boolean;
    FVerbose: boolean;
    function IsDebug: boolean;
    function IsInfo: boolean;
  public
    constructor Create(const aDebug, aVerbose: boolean);

    procedure Err(const aVallue: string);
    procedure Warn(const aVallue: string);
    procedure Info(const aVallue: string);
    procedure Debug(const aVallue: string);
  end;

implementation

{ TLogger }

function TLogger.IsDebug: boolean;
begin
  Result := FDebug;
end;

function TLogger.IsInfo: boolean;
begin
  Result := IsDebug or FVerbose;
end;

constructor TLogger.Create(const aDebug, aVerbose: boolean);
begin
  FDebug := aDebug;
  FVerbose := aVerbose;
end;

procedure TLogger.Err(const aVallue: string);
begin
  WriteLn(aVallue);
end;

procedure TLogger.Warn(const aVallue: string);
begin
  WriteLn(aVallue);
end;

procedure TLogger.Info(const aVallue: string);
begin
  if IsInfo then
    WriteLn(aVallue);
end;

procedure TLogger.Debug(const aVallue: string);
begin
  if IsDebug then
    WriteLn(aVallue);
end;

end.
