Unit uRawDataSet;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids, uMainDataModule;

Type

  { TRawDataSet }

  TRawDataSet = Class(TForm)
    DBGrid1: TDBGrid;
  Private
    { private declarations }
  Public
    { public declarations }
  End;

Var
  RawDataSet: TRawDataSet;

Implementation

{$R *.lfm}

End.

