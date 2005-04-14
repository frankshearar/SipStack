unit TestIdSipMessageLogger;

interface

uses
  IdSipMessageLogger, TestFramework;

type
  TestTIdSipMessageLogger = class(TTestCase)
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipMessageLogger unit tests');
  Result.AddTest(TestTIdSipMessageLogger.Suite);
end;

initialization
  RegisterTest('Message Logger', Suite);
end.
