Program locate;

{$mode objfpc}{$H+}

Uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, uMainDataModule, uTools, CustApp
  { you can add units after this };

Type

  { locate }

  { TLocate }

  TLocate = Class(TCustomApplication)
  private
    Procedure writeStdResult;
  protected
    Procedure DoRun; override;
  Public
    Constructor Create(TheOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure WriteHelp; virtual;
  End;

{ TLocate }

Procedure TLocate.writeStdResult;
Begin
  DM.SQLQueryResult.First;
  while not DM.SQLQueryResult.EOF do
  begin
    WriteLn(DM.getPath);
    DM.SQLQueryResult.Next;
  End;
End;

Procedure TLocate.DoRun;
Var
  ErrorMsg, lTag, lSearch, lPath: String;
  lNormalize, lIceMenu: Boolean;
Begin
  // quick check parameters
  //ErrorMsg := CheckOptions('h', 'help');
  //If ErrorMsg <> '' Then Begin
  //  ShowException(Exception.Create(ErrorMsg));
  //  Terminate;
  //  Exit;
  //End;
//
//  // parse parameters
//  If HasOption('h', 'help') Then Begin
//    WriteHelp;
//    Terminate;
//    Exit;
//  End;

  { main program }
  if HasOption('l', 'localdb') then
    DM.DBPath := GetOptionValue('l', 'localdb')
  else
    DM.DBPath := IncludeTrailingPathDelimiter(GetUserDir) + '.mlocate.db';

  lNormalize := HasOption('n', 'normalize');

  if HasOption('t', 'tag') then
    lTag := GetOptionValue('t', 'tag');

  if HasOption('p', 'path') then
    lPath := GetOptionValue('p', 'path');

  if HasOption('s', 'search') then
    lSearch := GetOptionValue('s', 'search');

  if lNormalize then
  begin
    lSearch := NormalizeTerm(lSearch);
    lSearch := StringReplace(lSearch, ' ', '* ', [rfReplaceAll, rfIgnoreCase]);
    lSearch := Trim(lSearch + '*');
  end;

  lIceMenu := HasOption('m', 'icemenu');

  DM.DBSearch(lSearch, lPath, lTag);

  if not lIceMenu then
    writeStdResult;


  // stop program loop
  Terminate;
End;

Constructor TLocate.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);
  StopOnException := True;
End;

Destructor TLocate.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TLocate.WriteHelp;
Begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
End;

Var
  Application: TLocate;
Begin
  Application := TLocate.Create(Nil);

  DM := TDM.Create(Application);

  Application.Run;
  Application.Free;
End.

