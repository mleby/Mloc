unit uIndexCmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure deletePath(const aPath: string);
procedure deleteTag;
procedure insertFile(const aFileName: string);
procedure insertCmd(const path, Name, command: string; const description: string = '');
Procedure refreshFtIndex;

{TODO -oLebeda -cNone: refreshFtIndex}

implementation

uses uMainDataModule, uTools, uAppContext;

procedure deletePath(const aPath: string);
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

procedure deleteTag;
begin
  DM.deleteByTagSQLQuery.ParamByName('tagname').AsString := App.Tag;
  DM.deleteByTagSQLQuery.ExecSQL;
end;

procedure insertFile(const aFileName: string);
begin
  insertCmd(aFileName, ExtractFileName(aFileName), App.Cmd);
end;

procedure insertCmd(const path, Name, command: string; const description: string);
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

  //DM.SQLite3Connection1.Transaction.EndTransaction;
  DM.SQLite3Connection1.ExecuteDirect('End Transaction');  // End the transaction started by SQLdb
  DM.SQLite3Connection1.ExecuteDirect('VACUUM');
  DM.SQLite3Connection1.ExecuteDirect('Begin Transaction'); //Start a transaction for SQLdb to use
  //DM.SQLite3Connection1.Transaction.StartTransaction;
end;

//def refreshFtIndex() {
//    sql.execute("DELETE FROM sources WHERE trash = 1")

//    if (reindex) {
//        sql.withTransaction {
//            sql.execute("DELETE FROM sourcesSearch WHERE id NOT IN (SELECT id FROM sources)")
//            sql.execute("INSERT INTO sourcesSearch (id, search) SELECT id, search FROM sources WHERE id NOT IN (SELECT id FROM sourcesSearch)")
//        }
//        println("Reindexation completed")
//    } else {
//        println("Reindexation skipped")
//    }
//}

end.
