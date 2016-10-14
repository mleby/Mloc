Unit uMainDataModule;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, sqlite3conn, sqldb, db;

Type

  { TDM }

  // TODO - nastavení DB souboru

  TDM = Class(TDataModule)
    DataSource1: TDataSource;
    SQLite3Connection1: TSQLite3Connection;
    updateSQLQuery: TSQLQuery;
    SQLQueryCount: TSQLQuery;
    SQLQueryResult: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    tableExistsSQLQuery: TSQLQuery;
  Private
    FDBPath: String;
    Function getDirectory(aPath: String): String;
    Procedure SetDBPath(AValue: String);
    Function TableExists(aTableName: String): Boolean;
  Public
    Function getPath: String;
    Function getDir: String;
    Function getCommand: String;
    Procedure DBSearch(Const aSearchTerm, aPath, aTag: string);

    Property DBPath: String Read FDBPath Write SetDBPath;
  End;

Var
  DM: TDM;

Implementation

{$R *.lfm}

{ TDM }

Function TDM.getPath: String;
Begin
  Result := SQLQueryResult.FieldByName('path').AsString
End;

Function TDM.getDir: String;
Begin
  Result := getDirectory(SQLQueryResult.FieldByName('path').AsString);
End;

 Function TDM.getDirectory(aPath: String): String;
 var
   isDir: Boolean;
 Begin
   Result  := aPath;
   isDir := DirectoryExists(Result);

   If Not isDir Then
   Begin
     Result  := ExtractFilePath(Result);
     isDir := DirectoryExists(Result);
   End;

   If Not isDir Then
      Result := '';
 End;

Procedure TDM.SetDBPath(AValue: String);
Begin
  If FDBPath = AValue Then Exit;
  FDBPath := AValue;

  SQLite3Connection1.Close();
  SQLite3Connection1.DatabaseName := FDBPath;
  SQLite3Connection1.Open;


  // create database structure if not exists
  SQLite3Connection1.Transaction.Active := true;
  SQLite3Connection1.ExecuteDirect('CREATE TABLE IF NOT EXISTS sources (id PRIMARY KEY, path, name, search, command, updated, tag, priority, trash, description)');
  SQLite3Connection1.ExecuteDirect('CREATE INDEX IF NOT EXISTS tagIndex ON sources (tag)');

  if not TableExists('sourcesSearch') then
    SQLite3Connection1.ExecuteDirect('CREATE VIRTUAL TABLE sourcesSearch USING FTS4(id, search)');

  SQLite3Connection1.Transaction.Commit;

  //SQLite3Connection1.;
////sql.setAutoCommit(false)
// TODO - indexes - on sources tag, path
{TODO -oLebeda -cNone: work table for update}


End;

Function TDM.TableExists(aTableName: String): Boolean;
Begin
  tableExistsSQLQuery.Close;
  tableExistsSQLQuery.ParamByName('tableName').Value := aTableName;
  tableExistsSQLQuery.Open;
  Result := tableExistsSQLQuery.FieldByName('cnt').AsInteger > 0;
End;

//boolean isTableExists(String tableName) {
//    sql.firstRow("SELECT COUNT(*) as cnt FROM sqlite_master WHERE type = 'table' AND name = ${tableName}").cnt > 0;
//}

{TODO -oLebeda -cNone: inteligent diferential reindex FT table}
{TODO -oLebeda -cNone: vacuum DB after reindex}

Function TDM.getCommand: String;
Begin
    Result := SQLQueryResult.FieldByName('command').AsString;
End;

Procedure TDM.DBSearch(Const aSearchTerm, aPath, aTag: string);
Var
  lSelect: string;
  lWhere: string;
Begin
  lWhere := ' 1 = 1 ';
  If aSearchTerm <> '' Then
    lWhere := lWhere + ' and id in (select id from sourcesSearch where search MATCH ''' + aSearchTerm + ''') ';

  If aPath <> '' Then
    lWhere := lWhere + ' and path like ''' + aPath + '%'' ';

  If aTag <> '' Then
    lWhere := lWhere + ' and tag = ''' + aTag + ''' ';

  lWhere := lWhere + ' order by priority, name, path';

  lSelect := 'select * from sources where ' + lWhere;

  DM.SQLQueryResult.Close;
  DM.SQLQueryResult.SQL.Text := lSelect;
  DM.SQLQueryResult.Open;

  DM.SQLQueryCount.Close;
  DM.SQLQueryCount.SQL.Text := 'select count(*) as cnt from sources where ' + lWhere;
  DM.SQLQueryCount.Open;
End;

End.

