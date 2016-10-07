unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, DB, FileUtil, LConvEncoding, frmSelectProps, Forms, Controls,
  Graphics, Dialogs, StdCtrls, DBGrids, ActnList, AsyncProcess,
  ComCtrls, ExtCtrls, Menus, Clipbrd, Buttons, LCLProc, uSettingsForm;

type

  { TMainSearchForm }

  TMainSearchForm = class(TForm)
    acSearchEditFocus: TAction;
    acDownToListing: TAction;
    acAppEnd: TAction;
    acOpenDirectory: TAction;
    acCommander: TAction;
    acEdit: TAction;
    acTerminal: TAction;
    acCopyPath: TAction;
    acHelp: TAction;
    acShowAdvanced: TAction;
    acSettings: TAction;
    Button1: TButton;
    btSettings: TButton;
    Edit1: TEdit;
    HeaderPanel: TPanel;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    NormalizeChb: TCheckBox;
    acRun: TAction;
    ActionList: TActionList;
    SpeedButton1: TSpeedButton;
    AdvancedPanel: TPanel;
    ResultPopUpMenu: TPopupMenu;
    runAsyncProcess: TAsyncProcess;
    DataSource1: TDataSource;
    ResultDBGrid: TDBGrid;
    SearchEdit: TEdit;
    SQLite3Connection1: TSQLite3Connection;
    SQLQueryCount: TSQLQuery;
    SQLQueryResult: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    StatusBar: TStatusBar;
    Timer1: TTimer;
    procedure acAppEndExecute(Sender: TObject);
    Procedure acCopyPathExecute(Sender: TObject);
    Procedure acCopyPathUpdate(Sender: TObject);
    Procedure acCommanderExecute(Sender: TObject);
    Procedure acCommanderUpdate(Sender: TObject);
    procedure acDownToListingUpdate(Sender: TObject);
    Procedure acEditExecute(Sender: TObject);
    Procedure acEditUpdate(Sender: TObject);
    Procedure acHelpExecute(Sender: TObject);
    Procedure acOpenDirectoryExecute(Sender: TObject);
    Procedure acOpenDirectoryUpdate(Sender: TObject);
    procedure acRunExecute(Sender: TObject);
    procedure acSearchEditFocusExecute(Sender: TObject);
    procedure acDownToListingExecute(Sender: TObject);
    Procedure acSettingsExecute(Sender: TObject);
    Procedure acShowAdvancedExecute(Sender: TObject);
    Procedure acTerminalExecute(Sender: TObject);
    Procedure acTerminalUpdate(Sender: TObject);
    Procedure IdleTimer1Timer(Sender: TObject);
    procedure ResultDBGridDblClick(Sender: TObject);
    procedure acRunUpdate(Sender: TObject);
    procedure ResultDBGridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    Procedure runAsyncProcessReadData(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
  private
    FPath: string;
    FTag: string;
    Function getDirectory(aPath: String): String;
    Function removeDiacritics(aStr: String): String;
    procedure SetPath(aPath: string);
    procedure SetTag(aTag: string);
    procedure Search;
    function GetSpecialShortCutName(ShortCut: TShortCut): string;
    { private declarations }
  public
    { public declarations }
    property Path: string read FPath write SetPath;
    property Tag: string read FTag write SetTag;
  end;

var
  MainSearchForm: TMainSearchForm;

implementation

uses shortcutHelpForm;

{$R *.lfm}

{ TMainSearchForm }

Procedure TMainSearchForm.SearchEditChange(Sender: TObject);
begin
  Timer1.Enabled := false;
  Timer1.Enabled := true;
end;

Procedure TMainSearchForm.SetPath(aPath: string);
begin
  FPath := aPath;
  StatusBar.Panels[2].Text := FPath;
end;

Function TMainSearchForm.getDirectory(aPath: String): String;
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

Function TMainSearchForm.removeDiacritics(aStr: String): String;
begin
  Result := aStr;

  //Čeština: á, é, í, ó, ú, ý, č, ď, ě, ň, ř, š, ť, ž, ů
  Result := StringReplace(Result, 'á', 'a', [rfReplaceAll]);
  Result := StringReplace(Result, 'é', 'e', [rfReplaceAll]);
  Result := StringReplace(Result, 'í', 'i', [rfReplaceAll]);
  Result := StringReplace(Result, 'ó', 'o', [rfReplaceAll]);
  Result := StringReplace(Result, 'ú', 'u', [rfReplaceAll]);
  Result := StringReplace(Result, 'ý', 'y', [rfReplaceAll]);
  Result := StringReplace(Result, 'č', 'c', [rfReplaceAll]);
  Result := StringReplace(Result, 'ď', 'd', [rfReplaceAll]);
  Result := StringReplace(Result, 'ě', 'e', [rfReplaceAll]);
  Result := StringReplace(Result, 'ň', 'n', [rfReplaceAll]);
  Result := StringReplace(Result, 'ř', 'r', [rfReplaceAll]);
  Result := StringReplace(Result, 'š', 's', [rfReplaceAll]);
  Result := StringReplace(Result, 'ť', 't', [rfReplaceAll]);
  Result := StringReplace(Result, 'ž', 'z', [rfReplaceAll]);
  Result := StringReplace(Result, 'ů', 'u', [rfReplaceAll]);
  Result := StringReplace(Result, 'Á', 'A', [rfReplaceAll]);
  Result := StringReplace(Result, 'É', 'E', [rfReplaceAll]);
  Result := StringReplace(Result, 'Í', 'I', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ó', 'O', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ú', 'U', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ý', 'Y', [rfReplaceAll]);
  Result := StringReplace(Result, 'Č', 'C', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ď', 'D', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ě', 'E', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ň', 'N', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ř', 'R', [rfReplaceAll]);
  Result := StringReplace(Result, 'Š', 'S', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ť', 'T', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ž', 'Z', [rfReplaceAll]);
  Result := StringReplace(Result, 'Ů', 'U', [rfReplaceAll]);

  //Dánština a norština: å, ø
  //Esperanto: ĉ, ĝ, ĥ, ĵ, ŝ, ŭ
  //Estonština: š, ž, õ, ä, ö, ü
  //Francouzština: à, â, ç, é, è, ê, ë, î, ï, ô, ù, û, ü, ÿ
  //Chorvatština a bosenština: ž, š, č, ć, đ
  //Irština: á, é, í, ó, ú
  //Lotyština: ā, ē, ī, ū, č, š, ž, ļ, ķ, ņ, ģ
  //Maďarština: á, é, í, ó, ú, ö, ü, ő, ű
  //Němčina: ä, ö, ü
  //Nizozemština ë, ï
  //Polština: ą, ć, ę, ł, ń, ó, ś, ź, ż
  //Rumunština: ă, â, î, ș, ț
  //Slovenština: á, ä, č, ď, é, í, ĺ, ľ, ň, ó, ô, ŕ, š, ť, ú, ý, ž
  //Španělština: ñ
  //Švédština: å, ä, ö
  //Turečtina: ç, ş, ğ
  //Vietnamština: ă, â, đ, ê, ô, ơ, ư
end;

Procedure TMainSearchForm.SetTag(aTag: string);
Begin
  FTag := aTag;
  StatusBar.Panels[3].Text := 'tag: ' + FTag;
End;

Procedure TMainSearchForm.Search;
var
  lSearchTerm, lWhere, lSelect: string;
Begin
  StatusBar.Panels[0].Text := '';
    StatusBar.Panels[1].Text := '';

    if (Length(SearchEdit.Text) > 2) or (FTag <> '') then
    begin
      if SearchEdit.Text <> '' then
      begin
        lSearchTerm := SearchEdit.Text;
        if NormalizeChb.Checked then
        begin
          // TODO - odstranit accenty
          lSearchTerm := removeDiacritics(lSearchTerm);
          lSearchTerm := LowerCase(lSearchTerm);
          lSearchTerm := Trim(lSearchTerm);
          lSearchTerm := StringReplace(lSearchTerm, '  ', ' ', [rfReplaceAll, rfIgnoreCase]);
          lSearchTerm := StringReplace(lSearchTerm, ' ', '* *',
            [rfReplaceAll, rfIgnoreCase]);
          lSearchTerm := '*' + lSearchTerm + '*';
        end;
      End;

      lWhere := ' 1 = 1 ';
      if lSearchTerm <> '' then
      begin
          lWhere := lWhere + ' and id in (select id from sourcesSearch where search MATCH ''' +
            lSearchTerm + ''') ';
      End;
      if FPath <> '' then
      begin
        lWhere := lWhere + ' and path like ''' + FPath + '%'' ';
        StatusBar.Panels[2].Text := FPath;
      end;
      if FTag <> '' then
      begin
        lWhere := lWhere + ' and tag = ''' + FTag + ''' ';
        StatusBar.Panels[3].Text := FTag;
      End;
      lWhere := lWhere + ' order by priority, name, path';

      lSelect := 'select * from sources where ' + lWhere;

      SQLQueryResult.Close;
      SQLQueryResult.SQL.Text := lSelect;
      SQLQueryResult.Open;

      SQLQueryCount.Close;
      SQLQueryCount.SQL.Text := 'select count(*) as cnt from sources where ' + lWhere;
      SQLQueryCount.Open;

      StatusBar.Panels[0].Text := lSearchTerm;
      StatusBar.Panels[1].Text := 'Found: ' + SQLQueryCount.FieldByName('cnt').AsString;
    end
    else
      SQLQueryResult.Close;
End;

Procedure TMainSearchForm.ResultDBGridDblClick(Sender: TObject);
begin
  acRun.Execute;
end;

Procedure TMainSearchForm.acRunUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (SQLQueryResult.RecordCount > 0);
  // and (ResultDBGrid.SelectedIndex > 0);
end;

Procedure TMainSearchForm.ResultDBGridKeyDown(Sender: TObject; Var Key: word; Shift: TShiftState);
begin
  if Key = 13 then
    acRun.Execute;
end;

Procedure TMainSearchForm.runAsyncProcessReadData(Sender: TObject);
Begin

end;

Procedure TMainSearchForm.acRunExecute(Sender: TObject);
var
  lCommand: string;
begin
  //ShowMessage('run');
  lCommand := SQLQueryResult.FieldByName('command').AsString +  // TODO - const na sloupce v DB
    ' ' + '''' + SQLQueryResult.FieldByName('path').AsString + '''';  // TODO - const na sloupce v DB
  runAsyncProcess.CommandLine := lCommand;
  runAsyncProcess.Execute;
  MainSearchForm.Close;
end;

Procedure TMainSearchForm.acDownToListingUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SearchEdit.Focused and (SQLQueryResult.RecordCount > 0);
end;

Procedure TMainSearchForm.acEditExecute(Sender: TObject);
var
  lFile, lCommand, lDir: String;
Begin
  lFile := SQLQueryResult.FieldByName('path').AsString; // TODO - const na sloupce v DB
  lDir := getDirectory(SQLQueryResult.FieldByName('path').AsString); // TODO - const na sloupce v DB

  if lFile <> '' then
  Begin
    // TODO - konfigurace
    lCommand := 'gvim "' + lFile + '"';
    runAsyncProcess.CommandLine := lCommand;
    runAsyncProcess.CurrentDirectory := lDir;
    runAsyncProcess.Execute;
  End;
end;

Procedure TMainSearchForm.acEditUpdate(Sender: TObject);
Begin
  (Sender as TAction).Enabled := (SQLQueryResult.RecordCount > 0)
        and (SQLQueryResult.FieldByName('path').AsString <> '');
  // TODO - kontrolovat nastavení ext. příkazu
end;

Procedure TMainSearchForm.acHelpExecute(Sender: TObject);
var
  i: Integer;
  lShortcutHelpFrm: TshortcutHelpFrm;
  lHint, lName, lShortCut: String;
Begin
  lShortcutHelpFrm := TshortcutHelpFrm.Create(self);
  try
    lShortcutHelpFrm.TextMemo.Lines.Clear;

    for i := 0 to ActionList.ActionCount - 1 do
    begin
      lHint := (ActionList.Actions[i] as TAction).Hint;
      lName := (ActionList.Actions[i] as TAction).Name;
      lShortCut := ShortCutToText((ActionList.Actions[i] as TAction).ShortCut);

      if lShortCut <> '' then
         lShortcutHelpFrm.TextMemo.Lines.Append(lShortCut + ' : ' {+ lName + ' ' } + lHint);
    end;

    lShortcutHelpFrm.ShowModal;
  Finally
    lShortcutHelpFrm.Free;
  End;
end;

// TODO - smazat
function TMainSearchForm.GetSpecialShortCutName(ShortCut: TShortCut): string;
var
  Key: Byte;
begin
  Key := ShortCut and $FF;
  case Key of
    $6B,$BB: Result := '+';
    $6D,$BD: Result := '-';
  else
    Result := '';
  end;
  // ToDo
  //Result := '';
end;

Procedure TMainSearchForm.acOpenDirectoryExecute(Sender: TObject);
var
  lDir, lCommand : String;
Begin
  lDir := getDirectory(SQLQueryResult.FieldByName('path').AsString); // TODO - const na sloupce v DB

  if lDir <> '' then
  Begin
    // TODO - zajistit náhradu v opencmd
    lCommand := settingsForm.openCmd + ' "' + lDir + '"';
    runAsyncProcess.CommandLine := lCommand;
    runAsyncProcess.Execute;
  End;
end;

Procedure TMainSearchForm.acOpenDirectoryUpdate(Sender: TObject);
Begin
  (Sender as TAction).Enabled := (SQLQueryResult.RecordCount > 0)
        and (SQLQueryResult.FieldByName('path').AsString <> '');
end;

Procedure TMainSearchForm.acAppEndExecute(Sender: TObject);
begin
  Self.Close;
end;

Procedure TMainSearchForm.acCopyPathExecute(Sender: TObject);
Begin
  Clipboard.AsText := SQLQueryResult.FieldByName('path').AsString;
end;

Procedure TMainSearchForm.acCopyPathUpdate(Sender: TObject);
Begin
  (Sender as TAction).Enabled := (SQLQueryResult.RecordCount > 0)
        and (SQLQueryResult.FieldByName('path').AsString <> '');
end;

Procedure TMainSearchForm.acCommanderExecute(Sender: TObject);
var
  lDir, lCommand: String;
Begin
  lDir := getDirectory(SQLQueryResult.FieldByName('path').AsString); // TODO - const na sloupce v DB

  if lDir <> '' then
  Begin
    // TODO - konfigurace
    lCommand := 'doublecmd -T "' + lDir + '"';
    runAsyncProcess.CommandLine := lCommand;
    runAsyncProcess.Execute;
  End;
end;

Procedure TMainSearchForm.acCommanderUpdate(Sender: TObject);
Begin
  (Sender as TAction).Enabled := (SQLQueryResult.RecordCount > 0)
        and (SQLQueryResult.FieldByName('path').AsString <> '');
  // TODO - kontrolovat nastavení ext. příkazu
end;

Procedure TMainSearchForm.acSearchEditFocusExecute(Sender: TObject);
begin
  SearchEdit.SetFocus;
end;

Procedure TMainSearchForm.acDownToListingExecute(Sender: TObject);
begin
  ResultDBGrid.SetFocus;
  SQLQueryResult.First;
end;

Procedure TMainSearchForm.acSettingsExecute(Sender: TObject);
Begin
  settingsForm.ShowModal;
end;

Procedure TMainSearchForm.acShowAdvancedExecute(Sender: TObject);
Begin
  AdvancedPanel.Visible := not AdvancedPanel.Visible;
end;

Procedure TMainSearchForm.acTerminalExecute(Sender: TObject);
var
  lDir, lCommand: String;
Begin
  lDir := getDirectory(SQLQueryResult.FieldByName('path').AsString); // TODO - const na sloupce v DB

  if lDir <> '' then
  Begin
    // TODO - konfigurace
    //lCommand := 'xfce4-terminal --working-directory="' + lDir + '"';
    //runAsyncProcess.CommandLine := lCommand;

    runAsyncProcess.Executable := 'xfce4-terminal';
    runAsyncProcess.CurrentDirectory := lDir;
    //runAsyncProcess.Parameters.Add('--working-directory="' + lDir + '"');
    runAsyncProcess.Execute;
  End;
end;

Procedure TMainSearchForm.acTerminalUpdate(Sender: TObject);
Begin
  (Sender as TAction).Enabled := (SQLQueryResult.RecordCount > 0)
        and (SQLQueryResult.FieldByName('path').AsString <> '');
  // TODO - kontrolovat nastavení ext. příkazu
end;

Procedure TMainSearchForm.IdleTimer1Timer(Sender: TObject);
Begin
  Timer1.Enabled := false;
  Search;
end;

end.
