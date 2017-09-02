unit uRunUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, FileUtil, AsyncProcess;

type

  { TRunUtils }

  TRunUtils = class(TDataModule) // TODO - odstranit třídu
    runSyncProcess: TProcess;
    runAsyncProcess: TAsyncProcess; // TODO - na lokální proměnnou
  private
    { private declarations }
  public
    procedure RunAsync(const aCmd, aParams, aDir, aPath, aName: string); // TODO - na běžnou proceduru
    procedure RunSync(const aCmd, aParams, aDir, aPath, aName: string);
  end;

var
  RunUtils: TRunUtils;

implementation

Const
  BEGIN_SL = 0;

{$R *.lfm}

{ TRunUtils }

Procedure TRunUtils.RunAsync(Const aCmd, aParams, aDir, aPath, aName: string);
var
  sl, slCmd: TStringList;
  lParams, lCmd: string;
begin
  // replace macros
  if aDir <> '' then
    lParams := StringReplace(aParams, '%d', '"' + aDir + '"', [rfReplaceAll]);

  if aPath <> '' then
    lParams := StringReplace(lParams, '%p', '"' + aPath + '"', [rfReplaceAll]);

  if aName <> '' then
    lParams := StringReplace(lParams, '%f', '"' + aName + '"', [rfReplaceAll]);

  sl := TStringList.Create;
  try
    slCmd := tStringList.Create;
    slCmd.Delimiter := ' ';
    slCmd.DelimitedText := aCmd;
    lCmd := slCmd[BEGIN_SL];
    slCmd.Delete(BEGIN_SL);

    //sl.StrictDelimiter := true;
    sl.Delimiter := ' ';
    sl.DelimitedText := lParams;
    slCmd.AddStrings(sl);

    // execute process
    if aDir <> '' then
      runAsyncProcess.CurrentDirectory := aDir;

    runAsyncProcess.Executable := lCmd;
    runAsyncProcess.Parameters.Clear;
    runAsyncProcess.Parameters.AddStrings(slCmd);
    runAsyncProcess.Execute;

  finally
    FreeAndNil(slCmd);
    sl.Free;
  end;
end;

Procedure TRunUtils.RunSync(Const aCmd, aParams, aDir, aPath, aName: string);
var
  sl: TStringList;
  lParams: string;
begin
  // replace macros
  if aDir <> '' then
    lParams := StringReplace(aParams, '%d', '''' + aDir + '''', [rfReplaceAll]);

  if aPath <> '' then
    lParams := StringReplace(lParams, '%p', '''' + aPath + '''', [rfReplaceAll]);

  if aPath <> '' then
    lParams := StringReplace(lParams, '%f', '''' + ExtractFileName(aPath) + '''', [rfReplaceAll]);

  if aName <> '' then
      lParams := StringReplace(lParams, '%n', '''' + aName + '''', [rfReplaceAll]);

  sl := TStringList.Create;
  try
    //sl.StrictDelimiter := true;
    sl.Delimiter := ' ';
    sl.DelimitedText := lParams;

    // execute process
    if aDir <> '' then
      runSyncProcess.CurrentDirectory := aDir;

    runSyncProcess.Executable := aCmd;
    runSyncProcess.Parameters.Clear;
    runSyncProcess.Parameters.AddStrings(sl);
    runSyncProcess.Execute;

    WriteLn(runSyncProcess.ExitCode);
    //sl.Clear;
    //sl.LoadFromStream(runSyncProcess.Output);
    //for i := 0 to sl.Count - 1 do
    //  WriteLn(sl[i]);
  finally
    sl.Free;
  end;
End;

end.

