Unit uConsoleOutput;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Type

  { TLogger }

  TLogger = class(TObject)
  private
    FDebug: Boolean;
    FVerbose: Boolean;
    Function IsDebug: Boolean;
    Function IsInfo: Boolean;
  public
    Constructor Create(const aDebug, aVerbose: Boolean);

    procedure Err(const aVallue: String);
    procedure Warn(const aVallue: String);
    procedure Info(const aVallue: String);
    procedure Debug(const aVallue: String);
  end;

Implementation

{ TLogger }

Function TLogger.IsDebug: Boolean;
Begin
  Result := FDebug;
End;

Function TLogger.IsInfo: Boolean;
Begin
  Result := IsDebug or FVerbose;
End;

Constructor TLogger.Create(Const aDebug, aVerbose: Boolean);
Begin
  FDebug := aDebug;
  FVerbose := aVerbose;
End;

Procedure TLogger.Err(Const aVallue: String);
Begin

End;

Procedure TLogger.Warn(Const aVallue: String);
Begin

End;

Procedure TLogger.Info(Const aVallue: String);
Begin

End;

Procedure TLogger.Debug(Const aVallue: String);
Begin

End;

End.

