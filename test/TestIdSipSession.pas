unit TestIdSipSession;

interface

uses
  IdSipSession, TestFramework;

type
  TestTIdSipSession = class(TTestCase)
  private
    Session: TIdSipSession;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('Foo unit tests');
  Result.AddTest(TestTIdSipSession.Suite);
end;

//******************************************************************************
//* TestTIdSipSession                                                          *
//******************************************************************************
//* TestTIdSipSession Public methods *******************************************

procedure TestTIdSipSession.SetUp;
begin
  inherited SetUp;

  Self.Session := TIdSipSession.Create;
end;

procedure TestTIdSipSession.TearDown;
begin
  Self.Session.Free;

  inherited TearDown;
end;

//* TestTIdSipSession Private methods ******************************************
//* TestTIdSipSession Published methods ****************************************

initialization
  RegisterTest('IdSipSession', Suite);
end.
