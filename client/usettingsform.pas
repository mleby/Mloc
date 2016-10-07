Unit uSettingsForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LazUTF8, IDEWindowIntf, Forms, Controls, Graphics, Dialogs, IniPropStorage, ValEdit, StdCtrls,
  XMLPropStorage;

Type

  { TsettingsForm }

  TsettingsForm = Class(TForm)
    edFilemanagerCmd: TEdit;
    edFilemanagerParams: TEdit;
    IniPropStorageCmd: TIniPropStorage;
    lblOpenCmd: TLabel;
    Procedure lblOpenCmdClick(Sender: TObject);
  Private
    Function getOpenCmd: String;
    { private declarations }
  Public
    Constructor Create(TheOwner: TComponent); override;
    Destructor Destroy; override;
    // TODO - properties for config values
    Property openCmd: String Read getOpenCmd;
  End;

Var
  settingsForm: TsettingsForm;

Implementation

{$R *.lfm}

{ TsettingsForm }

Procedure TsettingsForm.lblOpenCmdClick(Sender: TObject);
Begin

end;

Function TsettingsForm.getOpenCmd: String;
Begin
  Result := IniPropStorageCmd.StoredValue['openCmd'];
end;

Constructor TsettingsForm.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  // initialization for config
  IniPropStorageCmd.IniFileName := GetEnvironmentVariableUTF8('HOME')+'/.mloc.ini';
  IniPropStorageCmd.StoredValues.SaveValues;

  // TODO - Load to fields
  edFilemanagerParams.Text := openCmd;
End;

Destructor TsettingsForm.Destroy;
Begin
  IniPropStorageCmd.Save;

  Inherited Destroy;
End;

End.

