unit uIndexCmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure deletePath(const aPath: string);
procedure deleteTag;
procedure insertFile(const aFileName: string; Const aAnnex: Boolean);
procedure insertCmd(const path, Name, command: string; const aAnnex:Boolean; const description: string = '');
Procedure refreshFtIndex;

{TODO -oLebeda -cNone: refreshFtIndex}

implementation

uses uMainDataModule, uTools, uAppContext, uContentResolver;

Procedure deletePath(Const aPath: string);
var
  lTagWhere: string;
begin
  if App.Tag <> '' then
    lTagWhere := ' and tag=''' + App.Tag + ''''
  else
    lTagWhere := ' and tag is null';

  DM.deleteByPathSQLQuery.SQL.Text := 'delete from sources where path = :value and trash is null' + lTagWhere;
  //App.Log.Debug(DM.deleteByPathSQLQuery.SQL.Text);
  DM.deleteByPathSQLQuery.ParamByName('value').AsString := ExcludeTrailingPathDelimiter(aPath);
  DM.deleteByPathSQLQuery.ExecSQL;

  DM.deleteByPathSQLQuery.SQL.Text := 'delete from sources where path like :value and trash is null' + lTagWhere;
  //App.Log.Debug(DM.deleteByPathSQLQuery.SQL.Text);
  DM.deleteByPathSQLQuery.ParamByName('value').AsString := IncludeTrailingPathDelimiter(aPath) + '%';
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

Procedure insertCmd(Const path, Name, command: string; Const aAnnex: Boolean; Const description: string);
var
  lGuid: TGUID;
  lSearch: string;
begin
  {TODO -oLebeda -cNone: run as batch}
  CreateGUID(lGuid);
  lSearch := Trim(App.Tag + ' ' + NormalizeTerm(Name) + ' ' + NormalizeTerm(description));

  DM.insertSQLQuery.ParamByName('id').AsString := GUIDToString(lGuid);
  DM.insertSQLQuery.ParamByName('path').AsString := path;
  DM.insertSQLQuery.ParamByName('name').AsString := Name;
  DM.insertSQLQuery.ParamByName('search').AsString := lSearch;
  DM.insertSQLQuery.ParamByName('command').AsString := command;
  DM.insertSQLQuery.ParamByName('updated').AsDateTime := Now;
  if App.Tag <> '' then
    DM.insertSQLQuery.ParamByName('tag').AsString := App.Tag
  else
    DM.insertSQLQuery.ParamByName('tag').Value := null;
  DM.insertSQLQuery.ParamByName('priority').AsFloat := App.Priority;
  DM.insertSQLQuery.ParamByName('trash').Value := null;
  DM.insertSQLQuery.ParamByName('annex').AsBoolean := aAnnex;
  DM.insertSQLQuery.ParamByName('description').AsString := description;
  DM.insertSQLQuery.ExecSQL;
end;

Procedure refreshFtIndex;
Begin
  DM.SQLite3Connection1.ExecuteDirect('DELETE FROM sources WHERE trash = 1');
  DM.SQLite3Connection1.Transaction.Commit;

  DM.SQLite3Connection1.ExecuteDirect('DELETE FROM sourcesSearch WHERE id NOT IN (SELECT id FROM sources)');
  DM.SQLite3Connection1.ExecuteDirect('INSERT INTO sourcesSearch (id, search) SELECT id, search FROM sources WHERE id NOT IN (SELECT id FROM sourcesSearch)');
  DM.SQLite3Connection1.Transaction.Commit;

  DM.SQLite3Connection1.ExecuteDirect('End Transaction');  // End the transaction started by SQLdb
  DM.SQLite3Connection1.ExecuteDirect('VACUUM');
  DM.SQLite3Connection1.ExecuteDirect('Begin Transaction'); //Start a transaction for SQLdb to use
end;

end.
