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
var
  sl :TStringList;
  lParams: String;
Begin
  // replace macros
  if aDir <> '' then
     lParams := StringReplace(aParams, '%d', aDir, [rfReplaceAll]);

  if aPath <> '' then
     lParams := StringReplace(lParams, '%p', aPath, [rfReplaceAll]);

  if aName <> '' then
     lParams := StringReplace(lParams, '%f', aName, [rfReplaceAll]);

  sl := tstringlist.create;
  try
    //sl.StrictDelimiter := true;
    sl.Delimiter := ' ';
    sl.DelimitedText := lParams;

    // execute process
    if aDir <> '' then
       runAsyncProcess.CurrentDirectory := aDir;

    runAsyncProcess.Executable := aCmd;
    runAsyncProcess.Parameters.Clear;
    runAsyncProcess.Parameters.AddStrings(sl);
    runAsyncProcess.Execute;

  Finally
    sl.Free;
  End;
End;

End.

