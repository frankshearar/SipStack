unit TestIdSipRegistrar;

interface

uses
  IdSipRegistrar, TestFramework;

type
  TestTIdSipRegistrar = class(TTestCase)
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipRegistrar unit tests');
  Result.AddTest(TestTIdSipRegistrar.Suite);
end;

initialization
  RegisterTest('Registrar', Suite);
end.
