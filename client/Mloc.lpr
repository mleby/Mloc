Program Mloc;

{$mode objfpc}{$H+}

Uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, shortcutHelpForm, uSettingsForm, uRunUtils, uTools, uMainDataModule, sysutils, uRawDataSet;

{$R *.res}

Begin
  if Application.HasOption('h', 'help') then begin
    writeln('Usage: Mloc [options...]');
    writeln('    -h --help             show this help');
    writeln('    -l --localdb          path to database file, default: $HOME/.mlocate.db');
    writeln('    -t --tag              tag');
    writeln('    -p --path=X           paths for search'); 
    writeln('    -s --search=X         pattern for search'); 
    writeln('    -w --where=X          additional where part for search');
    writeln('    -q --query            automatic run search after start'); 
    writeln('    -d --delay=X          delay input for run automatic query in ms, default: 750');
    writeln('    -a --auto=X           count of character in input for run automatic query, default: 3');
    exit;
  end;

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainSearchForm, MainSearchForm);
  Application.CreateForm(TsettingsForm, settingsForm);
  Application.CreateForm(TRunUtils, RunUtils);
  Application.CreateForm(TDM, DM);

  if Application.HasOption('d', 'delay') then
    MainSearchForm.Delay := StrToInt(Application.GetOptionValue('d', 'delay'))
  else
    MainSearchForm.Delay := 750;

  if Application.HasOption('a', 'auto') then
    MainSearchForm.AutoQuery := StrToInt(Application.GetOptionValue('a', 'auto'))
  else
    MainSearchForm.AutoQuery := 3;

  if Application.HasOption('l', 'localdb') then
    DM.DBPath := Application.GetOptionValue('l', 'localdb')
  else
    DM.DBPath := IncludeTrailingPathDelimiter(GetUserDir) + '.mlocate.db';

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

  Application.Run;
End.

