Unit testcaseTools;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, uTools, fpcunit, testutils, testregistry;

Type

  TTestCaseTools = Class(TTestCase)
  protected
    Procedure SetUp; override;
    Procedure TearDown; override;
  Published
    Procedure TestHookUp;
  End;

Implementation

Procedure TTestCaseTools.TestHookUp;
Begin
  AssertEquals('ahoj nazdarek', NormalizeTerm('Ahój, Nazdárek'));
  AssertEquals('prilis zlutoucky kun upel dabelske ody', NormalizeTerm('Příliš  žluťoučký kůň - úpěl ďábelské ódy!'));
End;

Procedure TTestCaseTools.SetUp;
Begin

End;

Procedure TTestCaseTools.TearDown;
Begin

End;

Initialization

  RegisterTest(TTestCaseTools);
End.

