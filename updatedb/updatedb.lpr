Program updatedb;

{$mode objfpc}{$H+}

Uses uAppContext, uMainDataModule;

Var
  Application: TUpdateDb;
Begin
  Application := TUpdateDb.Create(Nil);
  App := Application;

  Application.Title := 'Update DB';

  // initialize singeltons
  DM := TDM.Create(Application);

  Application.Run;
  Application.Free;
End.

