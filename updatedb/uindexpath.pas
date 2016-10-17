unit uIndexPath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function IndexPath(const aPath: string): longint;

implementation

uses
  uAppContext, FileUtil, uIndexCmd, process, StreamIO;

function IndexPath(const aPath: string): longint;
var
  lFiles, lDirs: TStringList;
  i: integer;
  lProcess: TProcess;
  F:Text;
  lLine: String;
begin
  Result := 0;

  {TODO -oLebeda -cNone: git support}
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
         insertFile(IncludeTrailingPathDelimiter(aPath) + lLine);
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

      {TODO -oLebeda -cNone: support for file filters}
      //  // TODO podpora pro .desktop soubory
      //  if (file.name.endsWith(".desktop")) {

      //      def lines = file.readLines()

      //      String name = lines.grep(~/^Name=.*/)?.join("")?.replaceFirst(/^Name=/, '')
      //      String nameCz = lines.grep(~/^Name\[cz\]=.*/)?.join("")?.replaceFirst(/^Name.*?=/, '')
      //      String comment = lines.grep(~/^Comment=.*/)?.join("")?.replaceFirst(/^Comment=/, '')
      //      String commentCz = lines.grep(~/^Comment\[cz\]=.*/)?.join("")?.replaceFirst(/^Comment.*?=/, '')
      //      String keywords = lines.grep(~/^Keywords=.*/)?.join("")?.replaceFirst(/^Keywords=/, '')

      //      String description = [name, nameCz, comment, commentCz, keywords, file.name].join(" ").trim()
      //      addValueToDB(file.absolutePath, name, cmdname, description)
      //  } else { // obecné zařazení souboru
      //      addValueToDB(file.absolutePath, file.name, cmdname)
      //  }

      //App.Log.Debug(lFiles[i]);
      {TODO -oLebeda -cNone: avfs support}
      insertFile(lFiles[i]);
      Inc(Result);
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
      Result := Result + IndexPath(lDirs[i]);
    end;
  finally
    lDirs.Free;
  end;

  {TODO -oLebeda -cNone: symlinky - annex}
end;

end.

