unit uMainDataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, DB, strutils;

type

  { TDM }

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
    Constructor Create(AOwner: TComponent); override;
    function getPath: string;
    function getDir: string;
    function getCommand: string;
    function getIcon: String;
    function getItemName: string;
    function isAnnex: boolean;
    procedure DBSearch(const aSearchTerm, aPath, aTag: string);
    property DBPath: string read FDBPath write SetDBPath;
  end;

var
  DM: TDM;

implementation

Uses sqlite3;

{$R *.lfm}

{ TDM }

Function TDM.getPath: string;
var
  lPath: String;
begin
  lPath := SQLQueryResult.FieldByName('path').AsString;
  if lPath[1] <> '#' then
    Result := lPath
  else
    Result := '';
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
  SQLite3Connection1.ExecuteDirect('CREATE TABLE IF NOT EXISTS sources (id PRIMARY KEY, path NOT NULL, name NOT NULL, search NOT NULL, command, updated, tag NOT NULL, priority NOT NULL, trash, annex NOT NULL, description, icon)');
  SQLite3Connection1.ExecuteDirect('CREATE INDEX IF NOT EXISTS tagIndex ON sources (tag)');
  SQLite3Connection1.ExecuteDirect('CREATE INDEX IF NOT EXISTS trashIndex ON sources (trash)');

  if not TableExists('sourcesSearch') then
    SQLite3Connection1.ExecuteDirect('CREATE VIRTUAL TABLE sourcesSearch USING FTS4(id, search)');

  SQLite3Connection1.ExecuteDirect('CREATE TRIGGER IF NOT EXISTS insert_sources AFTER INSERT ON sources'
    + ' BEGIN'
    + '   INSERT INTO sourcesSearch (id, search) values (new.id, new.search);'
    + ' END;');

  SQLite3Connection1.ExecuteDirect('create trigger IF NOT EXISTS check_unique_source before insert on sources'
    + ' begin'
    + '  update or Ignore sources set trash = 0 where path = new.path and tag = new.tag;'
    + '  select RAISE(ignore) from sources where path = new.path and tag = new.tag;'
    + ' end');

  SQLite3Connection1.ExecuteDirect('create unique index if not exists sourcesUniq on sources (path, tag)');
  SQLite3Connection1.ExecuteDirect('create unique index if not exists sourcesUniq2 on sources (id, trash, tag)');
  SQLite3Connection1.ExecuteDirect('create index if not exists sourcesTrashTag on sources (trash, tag)');

  SQLite3Connection1.Transaction.Commit;

  DM.SQLite3Connection1.ExecuteDirect('End Transaction');  // End the transaction started by SQLdb
  SQLite3Connection1.ExecuteDirect('PRAGMA synchronous=OFF');
  // SQLite3Connection1.ExecuteDirect('PRAGMA cache_size = -' + IntToStr(1024*1024*2)); // 2GB
  // SQLite3Connection1.ExecuteDirect('PRAGMA journal_mode=MEMORY');
  // SQLite3Connection1.ExecuteDirect('PRAGMA temp_store=2');
  // SQLite3Connection1.ExecuteDirect('PRAGMA PAGE_SIZE=4096');
  DM.SQLite3Connection1.ExecuteDirect('Begin Transaction'); //Start a transaction for SQLdb to use
end;

Function TDM.TableExists(aTableName: string): boolean;
begin
  tableExistsSQLQuery.Close;
  tableExistsSQLQuery.ParamByName('tableName').Value := aTableName;
  tableExistsSQLQuery.Open;
  Result := tableExistsSQLQuery.FieldByName('cnt').AsInteger > 0;
end;

procedure SqlReverse(ctx: psqlite3_context; N: LongInt; V: ppsqlite3_value); cdecl;
var S: String;
begin
  SetString(S, sqlite3_value_text(V[0]), sqlite3_value_bytes(V[0]));
  S := ReverseString(S);
  sqlite3_result_text(ctx, PAnsiChar(S), Length(S), sqlite3_destructor_type(SQLITE_TRANSIENT));
End;

Constructor TDM.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);

  sqlite3_create_function(SQLite3Connection1.Handle, 'reverse', 1, SQLITE_UTF8, nil, @SqlReverse, nil, nil);
End;

Function TDM.getCommand: string;
begin
  Result := SQLQueryResult.FieldByName('command').AsString;
end;

Function TDM.getIcon: String;
Var
  lTag, lIconName: String;
Begin
  Result := SQLQueryResult.FieldByName('icon').AsString;

  if Result = '' then
  begin
    lTag := SQLQueryResult.FieldByName('tag').AsString;
    if lTag <> '' then
    begin
      lIconName := GetEnvironmentVariable('HOME') + '/.mlocate.icons/' + lTag + '.png';

      if FileExists(lIconName) then
        Result := lIconName;
    End;
  End;

  if Result = '' then Result := 'NOICON';
End;

Function TDM.getItemName: string;
Begin
  Result := SQLQueryResult.FieldByName('name').AsString;
End;

Function TDM.isAnnex: boolean;
Begin
  Result := SQLQueryResult.FieldByName('annex').AsBoolean;
End;

Procedure TDM.DBSearch(Const aSearchTerm, aPath, aTag: string);
var
  lSelect: string;
  lWhere: string;
begin
  lWhere := ' trash = 0 ';
  if aSearchTerm <> '' then
    lWhere := lWhere + ' and id in (select id from sourcesSearch where search MATCH ''' + aSearchTerm + ''') ';

  if aPath <> '' then
    lWhere := lWhere + ' and path like ''' + aPath + '%'' ';

  if aTag <> '' then
    lWhere := lWhere + ' and tag = ''' + aTag + ''' ';

  //lWhere := lWhere + ' and trash = 0 ';

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
