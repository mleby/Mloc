unit uIndexCmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure markPathAsTrash(const aPath: string);
procedure deleteTag;
procedure insertFile(const aFileName: string; Const aAnnex: Boolean);
procedure insertCmd(const aPath, aName, aCommand: string; const aAnnex:Boolean; const aDescription: string = '');
Procedure refreshFtIndex;
Procedure clearTrash;

implementation

uses uMainDataModule, uTools, uAppContext, uContentResolver;

Procedure markPathAsTrash(Const aPath: string);
begin
  DM.deleteByPathSQLQuery.SQL.Text := 'update sources set trash = 1 where path = :value and tag=:tag';
  //App.Log.Debug(DM.deleteByPathSQLQuery.SQL.Text);
  DM.deleteByPathSQLQuery.ParamByName('value').AsString := ExcludeTrailingPathDelimiter(aPath);
  DM.deleteByPathSQLQuery.ParamByName('tag').AsString := App.Tag;
  DM.deleteByPathSQLQuery.ExecSQL;

  DM.deleteByPathSQLQuery.SQL.Text := 'update sources set trash = 1 where path like :value and tag=:tag';
  //App.Log.Debug(DM.deleteByPathSQLQuery.SQL.Text);
  DM.deleteByPathSQLQuery.ParamByName('value').AsString := IncludeTrailingPathDelimiter(aPath) + '%';
  DM.deleteByPathSQLQuery.ParamByName('tag').AsString := App.Tag;
  DM.deleteByPathSQLQuery.ExecSQL;
end;

Procedure deleteTag;
begin
  DM.deleteByTagSQLQuery.ParamByName('tagname').AsString := App.Tag;
  DM.deleteByTagSQLQuery.ExecSQL;
end;

Procedure insertFile(Const aFileName: string; Const aAnnex: Boolean);
Var
  lContentResolver: IContentResolver;
begin
  lContentResolver := NewContentResolver(aFileName);
  insertCmd(lContentResolver.GetPath, lContentResolver.GetName, App.Cmd, aAnnex, lContentResolver.GetDescription);
end;

Procedure insertCmd(Const aPath, aName, aCommand: string; Const aAnnex: Boolean; Const aDescription: string);
var
  lGuid: TGUID;
  lSearch: string;
begin
  CreateGUID(lGuid);
  lSearch := Trim(App.Tag + ' '
      + NormalizeTerm(CopyAndSplitCammelCaseString(aName)) + ' '
      + NormalizeTerm(aDescription) + ' '
      + NormalizeTerm(CopyAndSplitCammelCaseString(ExtractFileName(aPath))));

  DM.insertSQLQuery.ParamByName('id').AsString := GUIDToString(lGuid);
  DM.insertSQLQuery.ParamByName('path').AsString := aPath;
  DM.insertSQLQuery.ParamByName('name').AsString := aName;
  DM.insertSQLQuery.ParamByName('search').AsString := lSearch;
  DM.insertSQLQuery.ParamByName('command').AsString := aCommand;
  DM.insertSQLQuery.ParamByName('updated').AsDateTime := Now;
  if App.Tag <> '' then
    DM.insertSQLQuery.ParamByName('tag').AsString := App.Tag
  else
    DM.insertSQLQuery.ParamByName('tag').Value := ''; // not null because need unique indexed
  DM.insertSQLQuery.ParamByName('priority').AsFloat := App.Priority;
  DM.insertSQLQuery.ParamByName('trash').AsBoolean := false;
  DM.insertSQLQuery.ParamByName('annex').AsBoolean := aAnnex;
  DM.insertSQLQuery.ParamByName('description').AsString := aDescription;
  DM.insertSQLQuery.ExecSQL;
end;

Procedure refreshFtIndex;
Begin
  DM.SQLite3Connection1.ExecuteDirect('DELETE FROM sourcesSearch WHERE id NOT IN (SELECT id FROM sources)');
  DM.SQLite3Connection1.Transaction.Commit;

  DM.SQLite3Connection1.ExecuteDirect('End Transaction');  // End the transaction started by SQLdb
  DM.SQLite3Connection1.ExecuteDirect('VACUUM');
  DM.SQLite3Connection1.ExecuteDirect('Begin Transaction'); //Start a transaction for SQLdb to use
end;

Procedure clearTrash;
Begin
  DM.SQLite3Connection1.ExecuteDirect('DELETE FROM sources WHERE trash = 1');
  DM.SQLite3Connection1.Transaction.Commit;
end;

end.
