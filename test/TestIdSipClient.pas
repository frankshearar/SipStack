unit TestIdSipClient;

interface

uses
  IdSipClient, TestFramework;

type
  TestTIdSipClient = class(TTestCase)
  private
    Client: TIdSipClient;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  public
    procedure TestGetTransport;
    procedure TestInvite;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipClient unit tests');
  Result.AddTest(TestTIdSipClient.Suite);
end;

//******************************************************************************
//* TestTIdSipClient                                                           *
//******************************************************************************
//* TestTIdSipClient Public methods ********************************************

procedure TestTIdSipClient.SetUp;
begin
  Client := TIdSipClient.Create;
end;

procedure TestTIdSipClient.TearDown;
begin
  Self.Client.Free;
end;

//* TestTIdSipClient Private methods *******************************************
//* TestTIdSipClient Published methods *****************************************

procedure TestTIdSipClient.TestGetTransport;
begin
end;

procedure TestTIdSipClient.TestInvite;
begin
end;

initialization
  RegisterTest('TIdSipClient', Suite);
end.
