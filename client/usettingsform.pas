Unit uSettingsForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LazUTF8, IDEWindowIntf, Forms, Controls, Graphics, Dialogs, IniPropStorage, ValEdit, StdCtrls,
  XMLPropStorage;

Type

  { TsettingsForm }

  TsettingsForm = Class(TForm)
    Button1: TButton;
    edFilemanagerCmd: TEdit;
    edCommanderCmd: TEdit;
    edEditorCmd: TEdit;
    edTerminalCmd: TEdit;
    edFilemanagerParams: TEdit;
    edCommanderParams: TEdit;
    edEditorParams: TEdit;
    edTerminalParams: TEdit;
    IniPropStorageCmd: TIniPropStorage;
    lblFilemanager: TLabel;
    lblCommander: TLabel;
    lblEditor: TLabel;
    lblTerminal: TLabel;
    Procedure Button1Click(Sender: TObject);
  Private
    Function getFilemanagerParams: String;
    Function getCommanderCmd: String;
    Function getCommanderParams: String;
    Function getEditorCmd: String;
    Function getEditorParams: String;
    Function getFilemanagerCmd: String;
    Function getTerminalCmd: String;
    Function getTerminalParams: String;
    Procedure LoadSettings;
    Procedure SaveSettings;
    { private declarations }
  Public
    Constructor Create(TheOwner: TComponent); override;
    Destructor Destroy; override;

    // properties for config values
    Property FilemanagerCmd: String Read getFilemanagerCmd;
    Property FilemanagerParams: String Read getFilemanagerParams;

    Property CommanderCmd: String Read getCommanderCmd;
    Property CommanderParams: String Read getCommanderParams;

    Property EditorCmd: String Read getEditorCmd;
    Property EditorParams: String Read getEditorParams;

    Property TerminalCmd: String Read getTerminalCmd;
    Property TerminalParams: String Read getTerminalParams;
  End;

Var
  settingsForm: TsettingsForm;

Implementation

{$R *.lfm}

{ TsettingsForm }

Function TsettingsForm.getFilemanagerCmd: String;
Begin
  Result := IniPropStorageCmd.StoredValue['FilemanagerCmd'];
end;

Procedure TsettingsForm.Button1Click(Sender: TObject);
Begin
  SaveSettings;
  Close;
end;

Function TsettingsForm.getFilemanagerParams: String;
Begin
  Result := IniPropStorageCmd.StoredValue['FilemanagerParams'];
End;

Function TsettingsForm.getCommanderCmd: String;
Begin
  Result := IniPropStorageCmd.StoredValue['CommanderCmd'];
end;

Function TsettingsForm.getCommanderParams: String;
Begin
  Result := IniPropStorageCmd.StoredValue['CommanderParams'];
end;

Function TsettingsForm.getEditorCmd: String;
Begin
  Result := IniPropStorageCmd.StoredValue['EditorCmd'];
end;

Function TsettingsForm.getEditorParams: String;
Begin
  Result := IniPropStorageCmd.StoredValue['EditorParams'];
end;

Function TsettingsForm.getTerminalCmd: String;
Begin
  Result := IniPropStorageCmd.StoredValue['TerminalCmd'];
end;

Function TsettingsForm.getTerminalParams: String;
Begin
  Result := IniPropStorageCmd.StoredValue['TerminalParams'];
end;

Procedure TsettingsForm.LoadSettings;
Begin
  edFilemanagerCmd.Text := FilemanagerCmd;
  edFilemanagerParams.Text := FilemanagerParams;

  edCommanderCmd.Text := CommanderCmd;
  edCommanderParams.Text := CommanderParams;

  edEditorCmd.Text := EditorCmd;
  edEditorParams.Text := EditorParams;

  edTerminalCmd.Text := TerminalCmd;
  edTerminalParams.Text := TerminalParams;
End;

Procedure TsettingsForm.SaveSettings;
Begin
  IniPropStorageCmd.StoredValue['FilemanagerCmd']    := edFilemanagerCmd.Text;
  IniPropStorageCmd.StoredValue['FilemanagerParams'] := edFilemanagerParams.Text;

  IniPropStorageCmd.StoredValue['CommanderCmd']      := edCommanderCmd.Text;
  IniPropStorageCmd.StoredValue['CommanderParams']   := edCommanderParams.Text;

  IniPropStorageCmd.StoredValue['EditorCmd']         := edEditorCmd.Text;
  IniPropStorageCmd.StoredValue['EditorParams']      := edEditorParams.Text;

  IniPropStorageCmd.StoredValue['TerminalCmd']       := edTerminalCmd.Text;
  IniPropStorageCmd.StoredValue['TerminalParams']    := edTerminalParams.Text;

  IniPropStorageCmd.Save;
End;

Constructor TsettingsForm.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);
  IniPropStorageCmd.IniFileName := GetEnvironmentVariableUTF8('HOME')+'/.mloc.ini';
  LoadSettings;
End;

Destructor TsettingsForm.Destroy;
Begin
  IniPropStorageCmd.Save;

  Inherited Destroy;
End;

End.

