Unit uRunUtils;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, AsyncProcess;

Type

  { TRunUtils }

  TRunUtils = Class(TDataModule)
    runAsyncProcess: TAsyncProcess;
  Private
    { private declarations }
  Public
    procedure RunAsync(const aCmd, aParams, aDir, aPath, aName: String);
  End;

Var
  RunUtils: TRunUtils;

Implementation

{$R *.lfm}

{ TRunUtils }

Procedure TRunUtils.RunAsync(Const aCmd, aParams, aDir, aPath, aName: String);

Begin
  // TODO - replace macros

  // TODO - aParams to TStrings

  // execute process
  runAsyncProcess.Executable := 'exo-open';
  runAsyncProcess.Parameters.AddText(aDir);
  runAsyncProcess.Execute;
End;

End.

