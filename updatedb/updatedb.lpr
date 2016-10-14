Program updatedb;

{$mode objfpc}{$H+}

Uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, uTools, uMainDataModule, CustApp;

Type

  { TUpdateDb }

  TUpdateDb = Class(TCustomApplication)
  Private
    FAvfs: String;
    FDebug: Boolean;
    FGit: Boolean;
    FTag: String;
    FVerbose: Boolean;
    FPriority: Integer;

    Function GetOptionValueDef(Const aShort: char; Const aLong, aDefault: String): String;
  protected
    Procedure DoRun; override;
  Public
    Constructor Create(TheOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure WriteHelp; virtual;

    Property Debug: Boolean Read FDebug;
    Property Verbose: Boolean Read FVerbose;
    Property Git: Boolean Read FGit;
    Property Avfs: String Read FAvfs;
    Property Tag: String Read FTag;
    Property Priority:Integer Read FPriority;
  End;

{ TUpdateDb }

Function TUpdateDb.GetOptionValueDef(Const aShort: char; Const aLong, aDefault: String): String;
Begin
  if HasOption(aShort, aLong) then
    Result := GetOptionValue(aShort, aLong)
  else if (aShort = '_') and HasOption(aLong) then
    Result := GetOptionValue(aLong)
  else
    Result := aDefault;
End;

Procedure TUpdateDb.DoRun;
//Var
  //ErrorMsg: String;
Begin
  // quick check parameters
  //ErrorMsg := CheckOptions('h', 'help');
  //If ErrorMsg <> '' Then Begin
  //  ShowException(Exception.Create(ErrorMsg));
  //  Terminate;
  //  Exit;
  //End;

  // parse parameters
  If HasOption('h', 'help') Then Begin
    WriteHelp;
    Terminate;
    Exit;
  End;

  // set application properties
  FTag := GetOptionValueDef('t', 'tag', '');
  FPriority := StrToInt(GetOptionValueDef('i', 'priority', '50'));
  FGit := HasOption('git');
  FAvfs := GetOptionValueDef('_', 'avfs', '');
  FVerbose := HasOption('v', 'verbose');
  FDebug := HasOption('d', 'debug');

  DM.DBPath := GetOptionValueDef('l', 'localdb', IncludeTrailingPathDelimiter(GetUserDir) + '.mlocate.db');

    //cli.u(longOpt: 'update', 'Update index from source files in arg')

  //if HasOption('p', 'path') then
    //MainSearchForm.Path := GetOptionValue('p', 'path');
    {TODO -oLebeda -cNone: simple write path}
    {TODO -oLebeda -cNone: create simple unit for commands}
    {TODO -oLebeda -cNone: create simple unit for output}


    //cli.c(longOpt: 'cmd', 'Command for open scanned entries', args: 1, argName: 'cmdname')
    //cli.p(longOpt: "path", "Simple recursive add paths in arg to index")

    //cli._(longOpt: 'noreindex', 'ignore fulltext reindexation (for use in batch update)')

  // TODO - add your program here
  Writeln('todo');

  // stop program loop
  Terminate;
End;

Constructor TUpdateDb.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);
  StopOnException := True;
End;

Destructor TUpdateDb.Destroy;
Begin
  DM.Free;
  Inherited Destroy;
End;

Procedure TUpdateDb.WriteHelp;
Begin
  writeln('Usage: ', ExeName, ' -h');
  {TODO -oLebeda -cNone: Vypsání helpu}
  //cli.i(longOpt: 'priority', 'Priority for scanned entries in results', args: 1, argName: 'priority')
  //cli._(longOpt: 'git', 'if directory contain .git use "git ls-files" instead recursive direct listing')
  //cli._(longOpt: 'avfs', 'Path to avfs mount', args: 1, argName: 'avfsPath')
End;

Var
  Application: TUpdateDb;
Begin
  Application := TUpdateDb.Create(Nil);
  Application.Title := 'Update DB';

  // initialize singeltons
  DM := TDM.Create(Application);

  Application.Run;
  Application.Free;
End.

