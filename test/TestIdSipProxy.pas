unit TestIdSipProxy;

interface

uses
  IdSipMessage, IdSipMockTransactionDispatcher, IdSipProxy, TestFramework;

type
  TestTIdSipProxy = class(TTestCase)
  private
    Dispatcher:  TIdSipMockTransactionDispatcher;
    Invite:      TIdSipRequest;
    Proxy:       TIdSipProxy;

    procedure RemoveBody(Msg: TIdSipMessage);
    procedure SimulateRemoteInvite;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRejectUnauthorizedRequest;
  end;

implementation

uses
  IdSipConsts, TestFrameworkSip, IdSipMockTransport;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipProxy unit tests');
  Result.AddTest(TestTIdSipProxy.Suite);
end;

//******************************************************************************
//* TestTIdSipProxy                                                            *
//******************************************************************************
//* TestTIdSipProxy Public methods *********************************************

procedure TestTIdSipProxy.SetUp;
begin
  inherited SetUp;

  Self.Dispatcher := TIdSipMockTransactionDispatcher.Create;
  Self.Dispatcher.Transport.LocalEchoMessages := false;
  Self.Dispatcher.Transport.TransportType := sttTCP;

  Self.Invite := TIdSipTestResources.CreateBasicRequest;
  Self.RemoveBody(Self.Invite);

  Self.Proxy := TIdSipProxy.Create;
  Self.Proxy.Dispatcher := Self.Dispatcher;
  Self.Dispatcher.AddUnhandledMessageListener(Self.Proxy);
end;

procedure TestTIdSipProxy.TearDown;
begin
  Self.Proxy.Free;
  Self.Invite.Free;
  Self.Dispatcher.Free;

  inherited TearDown;
end;

//* TestTIdSipProxy Private methods ********************************************

procedure TestTIdSipProxy.RemoveBody(Msg: TIdSipMessage);
begin
  Msg.RemoveAllHeadersNamed(ContentTypeHeaderFull);
  Msg.Body := '';
  Msg.ToHeader.Value := Msg.ToHeader.DisplayName
                               + ' <' + Msg.ToHeader.Address.URI + '>';
  Msg.RemoveAllHeadersNamed(ContentTypeHeaderFull);
  Msg.ContentLength := 0;
end;

procedure TestTIdSipProxy.SimulateRemoteInvite;
begin
  Self.Dispatcher.Transport.FireOnRequest(Self.Invite);
end;

//* TestTIdSipProxy Published methods ******************************************

procedure TestTIdSipProxy.TestRejectUnauthorizedRequest;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  Self.Proxy.RequireAuthentication := true;

  ResponseCount := Self.Dispatcher.Transport.SentResponseCount;
  Self.SimulateRemoteInvite;
  Check(ResponseCount < Self.Dispatcher.Transport.SentResponseCount,
        'No response sent');
  Response := Self.Dispatcher.Transport.LastResponse;
  CheckEquals(SIPUnauthorized,
              Response.StatusCode,
              'Unexpected response');
end;

initialization
  RegisterTest('Proxy', Suite);
end.
