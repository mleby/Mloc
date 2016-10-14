Unit uIndexPath;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Function IndexPath(const aPath:String): Integer;

Implementation

Uses
  uAppContext, FileUtil;

Function IndexPath(Const aPath: String): Integer;
Var
  lFiles, lDirs: TStringList;
  i: Integer;
Begin
  Result := 0;
  // files
  lFiles := FindAllFiles(aPath, '', False);
  try
    for i := 0 to lFiles.Count - 1 do
    Begin
      {TODO -oLebeda -cNone: include/exclude param}
      //App.Log.Debug(lFiles[i]);
      Inc(Result); {TODO -oLebeda -cNone: write file to DB}
    End;
  Finally
    lFiles.Free;
  End;

  lDirs := FindAllDirectories(aPath, False);
  try
    for i := 0 to lDirs.Count - 1 do
    Begin
      {TODO -oLebeda -cNone: include/exclude param}
      //App.Log.Debug('Dir: ' + lDirs[i]);
      Result := Result + IndexPath(lDirs[i]);
    End;
  Finally
    lDirs.Free;
  End;

  {TODO -oLebeda -cNone: symlinky - annex}
end;

End.

