unit uAppContext;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, uMainDataModule, CustApp, uConsoleOutput, uIndexPath;

type

  { TUpdateDb }

  TUpdateDb = class(TCustomApplication)
  private
    FAvfs: string;
    FDebug: boolean;
    FGit: string;
    FTag: string;
    FCmd: string;
    FVerbose: boolean;
    FPriority: integer;
    FLog: TLogger;

    function GetOptionValueDef(const aShort: char; const aLong, aDefault: string): string;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

    property Debug: boolean read FDebug;
    property Verbose: boolean read FVerbose;
    property Git: string read FGit;
    property Avfs: string read FAvfs;
    property Tag: string read FTag;
    property Cmd: string read FCmd;
    property Priority: integer read FPriority;
    property Log: TLogger read FLog;
  end;

var
  App: TUpdateDb; // application context

implementation

uses uIndexCmd;

{ TUpdateDb }

function TUpdateDb.GetOptionValueDef(const aShort: char; const aLong, aDefault: string): string;
begin
  if (aShort <> '_') and HasOption(aShort, aLong) then
    Result := GetOptionValue(aShort, aLong)
  else if (aShort = '_') and HasOption(aLong) then
    Result := GetOptionValue(aShort, aLong)
  else
    Result := aDefault;
end;

procedure TUpdateDb.DoRun;
//Var
//ErrorMsg: String;
var
  lPaths: TStringList;
  i: integer;
  lOptPath: string;
  lCnt: longint;
  lStartTime: TDateTime;
begin
  lCnt := 0;

  // quick check parameters
  //ErrorMsg := CheckOptions('h', 'help');
  //If ErrorMsg <> '' Then Begin
  //  ShowException(Exception.Create(ErrorMsg));
  //  Terminate;
  //  Exit;
  //End;

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  // set application properties
  FTag := GetOptionValueDef('t', 'tag', '');
  FPriority := StrToInt(GetOptionValueDef('i', 'priority', '50'));
  FGit := GetOptionValueDef('_', 'git', '');
  FAvfs := GetOptionValueDef('_', 'avfs', '');
  FVerbose := HasOption('v', 'verbose');
  FDebug := HasOption('d', 'debug');
  FCmd := GetOptionValueDef('c', 'cmd-open', 'xdg-open');

  FLog := TLogger.Create(FDebug, FVerbose);
  lStartTime := now;

  // include/exclude
  {TODO -oLebeda -cNone: x exclude}
  {TODO -oLebeda -cNone: include }

  DM.DBPath := GetOptionValueDef('l', 'localdb', IncludeTrailingPathDelimiter(GetUserDir) + '.mlocate.db');
  Log.Info('use DB: ' + DM.DBPath);

  //cli.s(longOpt: 'from-source', 'Update index from source files in arg')

  if HasOption('p', 'path') then
  begin
    lPaths := TStringList.Create();
    try
      lPaths.Delimiter := ':';
      lOptPath := GetOptionValue('p', 'path');
      lPaths.DelimitedText := lOptPath;

      for i := 0 to lPaths.Count - 1 do
      begin
        Log.Info('indexing path: ' + lPaths[i]);
        markPathAsTrash(lPaths[i]);
        lCnt := lCnt + IndexPath(lPaths[i], false);
        DM.SQLite3Connection1.Transaction.Commit;
      end;

    finally
      lPaths.Free;
    end;
  end;

  //cli._(longOpt: 'noreindex', 'ignore fulltext reindexation (for use in batch update)')
  if HasOption('noreindex') then
  begin
    Log.Info('Refresh of indexation was skipped, only clean trash.');
    clearTrash;
  End
  else
  begin
    Log.Info('Refreshing fulltext index and maitaining database');
    clearTrash;
    refreshFtIndex;
  end;

  Log.Info('done: ' + IntToStr(lCnt) + ' items in ' + TimeToStr(now - lStartTime));

  // stop program loop
  Terminate;
end;

constructor TUpdateDb.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TUpdateDb.Destroy;
begin
  DM.Free;
  FLog.Free;
  inherited Destroy;
end;

procedure TUpdateDb.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' -h');
  {TODO -oLebeda -cNone: Vypsání helpu}
  //cli.i(longOpt: 'priority', 'Priority for scanned entries in results', args: 1, argName: 'priority')
  //cli._(longOpt: 'git', 'if directory contain .git use "git ls-files" instead recursive direct listing')
  //cli._(longOpt: 'avfs', 'Path to avfs mount', args: 1, argName: 'avfsPath')
  //cli.c(longOpt: 'cmd', 'Command for open scanned entries', args: 1, argName: 'cmdname')
  //cli.p(longOpt: "path", "Simple recursive add paths in arg to index")
  // noreindex
end;

end.
