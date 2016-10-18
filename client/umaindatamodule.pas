unit uMainDataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, DB;

type

  { TDM }

  // TODO - nastavení DB souboru

  TDM = class(TDataModule)
    DataSource1: TDataSource;
    SQLite3Connection1: TSQLite3Connection;
    updateSQLQuery: TSQLQuery;
    SQLQueryCount: TSQLQuery;
    SQLQueryResult: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    tableExistsSQLQuery: TSQLQuery;
    insertSQLQuery: TSQLQuery;
    deleteByPathSQLQuery: TSQLQuery;
    deleteByTagSQLQuery: TSQLQuery;
  private
    FDBPath: string;
    function getDirectory(aPath: string): string;
    procedure SetDBPath(AValue: string);
    function TableExists(aTableName: string): boolean;
  public
    function getPath: string;
    function getDir: string;
    function getCommand: string;
    function isAnnex: boolean;
    procedure DBSearch(const aSearchTerm, aPath, aTag: string);
    property DBPath: string read FDBPath write SetDBPath;
  end;

var
  DM: TDM;

implementation

{$R *.lfm}

{ TDM }

Function TDM.getPath: string;
begin
  Result := SQLQueryResult.FieldByName('path').AsString;
end;

Function TDM.getDir: string;
begin
  Result := getDirectory(SQLQueryResult.FieldByName('path').AsString);
end;

Function TDM.getDirectory(aPath: string): string;
var
  isDir: boolean;
begin
  Result := aPath;
  isDir := DirectoryExists(Result);

  if not isDir then
  begin
    Result := ExtractFilePath(Result);
    isDir := DirectoryExists(Result);
  end;

  if not isDir then
    Result := '';
end;

Procedure TDM.SetDBPath(AValue: string);
begin
  if FDBPath = AValue then
    Exit;
  FDBPath := AValue;

  SQLite3Connection1.Close();
  SQLite3Connection1.DatabaseName := FDBPath;
  SQLite3Connection1.Open;


  // create database structure if not exists
  SQLite3Connection1.Transaction.Active := True;
  SQLite3Connection1.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS sources (id PRIMARY KEY, path, name, search, command, updated, tag, priority, trash, annex, description)');
  SQLite3Connection1.ExecuteDirect(
    'CREATE INDEX IF NOT EXISTS tagIndex ON sources (tag)');

  if not TableExists('sourcesSearch') then
    SQLite3Connection1.ExecuteDirect(
      'CREATE VIRTUAL TABLE sourcesSearch USING FTS4(id, search)');

  SQLite3Connection1.Transaction.Commit;

  //SQLite3Connection1.;
  ////sql.setAutoCommit(false)
  // TODO - indexes - on sources tag, path
  {TODO -oLebeda -cNone: work table for update}

end;

Function TDM.TableExists(aTableName: string): boolean;
begin
  tableExistsSQLQuery.Close;
  tableExistsSQLQuery.ParamByName('tableName').Value := aTableName;
  tableExistsSQLQuery.Open;
  Result := tableExistsSQLQuery.FieldByName('cnt').AsInteger > 0;
end;

//boolean isTableExists(String tableName) {
//    sql.firstRow("SELECT COUNT(*) as cnt FROM sqlite_master WHERE type = 'table' AND name = ${tableName}").cnt > 0;
//}

{TODO -oLebeda -cNone: inteligent diferential reindex FT table}
{TODO -oLebeda -cNone: vacuum DB after reindex}

Function TDM.getCommand: string;
begin
  Result := SQLQueryResult.FieldByName('command').AsString;
end;

Function TDM.isAnnex: boolean;
Begin
  Result := SQLQueryResult.FieldByName('annex').AsBoolean;
End;

Procedure TDM.DBSearch(Const aSearchTerm, aPath, aTag: string);
var
  lSelect: string;
  lWhere: string;
begin
  lWhere := ' 1 = 1 ';
  if aSearchTerm <> '' then
    lWhere := lWhere + ' and id in (select id from sourcesSearch where search MATCH ''' + aSearchTerm + ''') ';


  if aPath <> '' then
    lWhere := lWhere + ' and path like ''' + aPath + '%'' ';

  if aTag <> '' then
    lWhere := lWhere + ' and tag = ''' + aTag + ''' ';

  lWhere := lWhere + ' order by priority, name, path';

  lSelect := 'select * from sources where ' + lWhere;

  DM.SQLQueryResult.Close;
  DM.SQLQueryResult.SQL.Text := lSelect;
  DM.SQLQueryResult.Open;

  DM.SQLQueryCount.Close;
  DM.SQLQueryCount.SQL.Text := 'select count(*) as cnt from sources where ' + lWhere;
  DM.SQLQueryCount.Open;
end;

end.
