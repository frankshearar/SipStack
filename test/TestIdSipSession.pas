unit TestIdSipSession;

interface

uses
  IdSipMessage, IdSipSession, TestFramework;

type
  TestTIdSipSession = class(TTestCase)
  private
    InitialRequest: TIdSipRequest;
    Session:        TIdSipSession;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published

  end;

implementation

uses
  TestMessages;

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
var
  P: TIdSipParser;
begin
  inherited SetUp;

  P := TIdSipParser.Create;
  try
    Self.InitialRequest := P.ParseAndMakeRequest(LocalLoopRequest)
  finally
    P.Free;
  end;

  Self.Session := TIdSipSession.Create(Self.InitialRequest);
end;

procedure TestTIdSipSession.TearDown;
begin
  Self.Session.Free;
  Self.InitialRequest.Free;

  inherited TearDown;
end;

//* TestTIdSipSession Private methods ******************************************
//* TestTIdSipSession Published methods ****************************************

initialization
  RegisterTest('IdSipSession', Suite);
end.
