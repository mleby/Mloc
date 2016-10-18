unit uSettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazUTF8, IDEWindowIntf, Forms, Controls, Graphics, Dialogs, IniPropStorage, ValEdit, StdCtrls,
  XMLPropStorage;

type

  { TsettingsForm }

  TsettingsForm = class(TForm)
    Button1: TButton;
    edFilemanagerCmd: TEdit;
    edCommanderCmd: TEdit;
    edEditorCmd: TEdit;
    edTerminalCmd: TEdit;
    edFilemanagerParams: TEdit;
    edCommanderParams: TEdit;
    edEditorParams: TEdit;
    edAnnexCmd: TEdit;
    edTerminalParams: TEdit;
    edAnnexParams: TEdit;
    IniPropStorageCmd: TIniPropStorage;
    lblFilemanager: TLabel;
    lblCommander: TLabel;
    lblEditor: TLabel;
    lblTerminal: TLabel;
    lblAnnex: TLabel;
    procedure Button1Click(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
  private
    function getAnnexCmd: string;
    function getAnnexParams: string;
    function getFilemanagerParams: string;
    function getCommanderCmd: string;
    function getCommanderParams: string;
    function getEditorCmd: string;
    function getEditorParams: string;
    function getFilemanagerCmd: string;
    function getTerminalCmd: string;
    function getTerminalParams: string;
    procedure LoadSettings;
    procedure SaveSettings;
    { private declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    // properties for config values
    property FilemanagerCmd: string read getFilemanagerCmd;
    property FilemanagerParams: string read getFilemanagerParams;

    property CommanderCmd: string read getCommanderCmd;
    property CommanderParams: string read getCommanderParams;

    property EditorCmd: string read getEditorCmd;
    property EditorParams: string read getEditorParams;

    property TerminalCmd: string read getTerminalCmd;
    property TerminalParams: string read getTerminalParams;

    property AnnexCmd: string read getAnnexCmd;
    property AnnexParams: string read getAnnexParams;
  end;

var
  settingsForm: TsettingsForm;

implementation

{$R *.lfm}

{ TsettingsForm }

function TsettingsForm.getFilemanagerCmd: string;
begin
  Result := IniPropStorageCmd.StoredValue['FilemanagerCmd'];
end;

procedure TsettingsForm.Button1Click(Sender: TObject);
begin
  SaveSettings;
  Close;
end;

Procedure TsettingsForm.FormActivate(Sender: TObject);
Begin
  LoadSettings;
end;

function TsettingsForm.getAnnexCmd: string;
begin
  Result := IniPropStorageCmd.StoredValue['AnnexCmd'];
end;

function TsettingsForm.getAnnexParams: string;
begin
  Result := IniPropStorageCmd.StoredValue['AnnexParams'];
end;

function TsettingsForm.getFilemanagerParams: string;
begin
  Result := IniPropStorageCmd.StoredValue['FilemanagerParams'];
end;

function TsettingsForm.getCommanderCmd: string;
begin
  Result := IniPropStorageCmd.StoredValue['CommanderCmd'];
end;

function TsettingsForm.getCommanderParams: string;
begin
  Result := IniPropStorageCmd.StoredValue['CommanderParams'];
end;

function TsettingsForm.getEditorCmd: string;
begin
  Result := IniPropStorageCmd.StoredValue['EditorCmd'];
end;

function TsettingsForm.getEditorParams: string;
begin
  Result := IniPropStorageCmd.StoredValue['EditorParams'];
end;

function TsettingsForm.getTerminalCmd: string;
begin
  Result := IniPropStorageCmd.StoredValue['TerminalCmd'];
end;

function TsettingsForm.getTerminalParams: string;
begin
  Result := IniPropStorageCmd.StoredValue['TerminalParams'];
end;

procedure TsettingsForm.LoadSettings;
begin
  edFilemanagerCmd.Text := FilemanagerCmd;
  edFilemanagerParams.Text := FilemanagerParams;

  edCommanderCmd.Text := CommanderCmd;
  edCommanderParams.Text := CommanderParams;

  edEditorCmd.Text := EditorCmd;
  edEditorParams.Text := EditorParams;

  edTerminalCmd.Text := TerminalCmd;
  edTerminalParams.Text := TerminalParams;

  edAnnexCmd.Text := AnnexCmd;
  edAnnexParams.Text := AnnexParams;
end;

procedure TsettingsForm.SaveSettings;
begin
  IniPropStorageCmd.StoredValue['FilemanagerCmd'] := edFilemanagerCmd.Text;
  IniPropStorageCmd.StoredValue['FilemanagerParams'] := edFilemanagerParams.Text;

  IniPropStorageCmd.StoredValue['CommanderCmd'] := edCommanderCmd.Text;
  IniPropStorageCmd.StoredValue['CommanderParams'] := edCommanderParams.Text;

  IniPropStorageCmd.StoredValue['EditorCmd'] := edEditorCmd.Text;
  IniPropStorageCmd.StoredValue['EditorParams'] := edEditorParams.Text;

  IniPropStorageCmd.StoredValue['TerminalCmd'] := edTerminalCmd.Text;
  IniPropStorageCmd.StoredValue['TerminalParams'] := edTerminalParams.Text;

  IniPropStorageCmd.StoredValue['AnnexCmd'] := edAnnexCmd.Text;
  IniPropStorageCmd.StoredValue['AnnexParams'] := edAnnexParams.Text;

  IniPropStorageCmd.Save;
end;

constructor TsettingsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IniPropStorageCmd.Active := False;
  IniPropStorageCmd.IniFileName := GetEnvironmentVariableUTF8('HOME') + '/.mloc.ini';
  IniPropStorageCmd.Active := True;
end;

destructor TsettingsForm.Destroy;
begin
  IniPropStorageCmd.Save;

  inherited Destroy;
end;

end.

