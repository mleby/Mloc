Program Mloc;

{$mode objfpc}{$H+}

Uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, shortcutHelpForm, uSettingsForm, uRunUtils, uTools, uMainDataModule, sysutils;

{$R *.res}

Begin
  if Application.HasOption('h', 'help') then begin
    // TODO - vypsání helpu
    Halt;
  end;

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainSearchForm, MainSearchForm);

  if Application.HasOption('a', 'auto') then
    MainSearchForm.AutoQuery := StrToInt(Application.GetOptionValue('a', 'auto'))
  else
    MainSearchForm.AutoQuery := 3;

  if Application.HasOption('p', 'path') then
    MainSearchForm.Path := Application.GetOptionValue('p', 'path');

  if Application.HasOption('t', 'tag') then
    MainSearchForm.Tag := Application.GetOptionValue('t', 'tag');

  if Application.HasOption('w', 'where') then
    MainSearchForm.Where := Application.GetOptionValue('w', 'where');

  if Application.HasOption('s', 'search') then
    MainSearchForm.SearchEdit.Text := Application.GetOptionValue('s', 'search');

  if Application.HasOption('q', 'query') then // q query
     MainSearchForm.Search(true);

  Application.CreateForm(TsettingsForm, settingsForm);
  Application.CreateForm(TRunUtils, RunUtils);
  Application.CreateForm(TDM, DM);

  Application.Run;
End.

