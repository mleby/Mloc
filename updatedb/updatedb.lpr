program updatedb;

{$mode objfpc}{$H+}

uses
  uAppContext,
  uMainDataModule,
  uIndexCmd, uContentResolver;

var
  Application: TUpdateDb;
begin
  Application := TUpdateDb.Create(nil);
  App := Application;

  Application.Title := 'Update DB';

  // initialize singeltons
  DM := TDM.Create(Application);

  Application.Run;
  Application.Free;
end.
