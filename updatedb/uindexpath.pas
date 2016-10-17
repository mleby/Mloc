unit uIndexPath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function IndexPath(const aPath: string; aAnnex: Boolean): longint;

implementation

uses
  uAppContext, FileUtil, uIndexCmd, process, StreamIO, strutils;

function IndexPath(const aPath: string; aAnnex: Boolean): longint;
var
  lFiles, lDirs: TStringList;
  i: integer;
  lProcess: TProcess;
  F:Text;
  lLine, lAvfsDirName: String;
begin
  Result := 0;

  if aAnnex or DirectoryExists(IncludeTrailingPathDelimiter(aPath) + '.git' + PathDelim + 'annex') then
    aAnnex := true;

  if (App.Git <> '') and FileIsExecutable(App.Git) and DirectoryExists(IncludeTrailingPathDelimiter(aPath) + '.git') then
  begin
    App.Log.Debug('Using git for ' + aPath);
    lProcess := TProcess.Create(nil);
    try
      lProcess.Executable := App.Git;
      lProcess.Parameters.Add('ls-files');
      lProcess.CurrentDirectory := aPath;
      lProcess.Options := [poNoConsole, poUsePipes];
      lProcess.Execute;

      AssignStream(F, lProcess.Output);
      Reset(F);
      while not Eof(F) do
      begin
         Readln(F, lLine);
         insertFile(IncludeTrailingPathDelimiter(aPath) + lLine, aAnnex);
         Inc(Result);
      End;
      CloseFile(F);
    finally
      lProcess.Free;
    end;
    Exit; // using git terminate walk through tree
  end;

  // files
  lFiles := FindAllFiles(aPath, '', False);
  try
    for i := 0 to lFiles.Count - 1 do
    begin
      {TODO -oLebeda -cNone: include/exclude param}

      insertFile(lFiles[i], aAnnex);
      Inc(Result);

      if (App.Avfs <> '') and (
          AnsiEndsText('.zip', lFiles[i]) or
          AnsiEndsText('.rar', lFiles[i]) or
          AnsiEndsText('.tgz', lFiles[i]) or
          AnsiEndsText('.tag.gz', lFiles[i]) or
          AnsiEndsText('.tgz', lFiles[i]) or
          AnsiEndsText('.tar.bz2', lFiles[i]) or
          AnsiEndsText('.7z', lFiles[i]) or
          AnsiEndsText('.jar', lFiles[i])
        ) then
      begin
        lAvfsDirName := IncludeTrailingPathDelimiter(App.Avfs) + lFiles[i] + '#';

        {TODO -oLebeda -cNone: better avfs support - use date for update}
        //long changed = f.lastModified()
        //long indexed = (Long) (sql.firstRow("select min(updated) as indexed from sources WHERE path like ${avfsPath + "%"}").indexed ?: 0)
        //if (changed > indexed) {
        //    println("index from avfs: ${avfsPath}")
        //    cnt += processCommandLineInternal("$CMD_DELREC ${avfsPath}")
        //    cnt += processCommandLineInternal("$CMD_PATH ${avfsPath}")
        //} else {
        //    println("index from cache: ${avfsPath}")
        //    sql.execute("update sources set trash = null where path like ${avfsPath + "%"}")
        //}

        IndexPath(lAvfsDirName, false)
      end;
    end
  finally
    lFiles.Free;
  end;

  lDirs := FindAllDirectories(aPath, False);
  try
    for i := 0 to lDirs.Count - 1 do
    begin
      {TODO -oLebeda -cNone: include/exclude param}
      //App.Log.Debug('Dir: ' + lDirs[i]);
      Result := Result + IndexPath(lDirs[i], aAnnex);
    end;
  finally
    lDirs.Free;
  end;

  {TODO -oLebeda -cNone: symlinky - annex}
end;

end.

