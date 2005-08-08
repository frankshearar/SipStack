{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipProxy;

interface

uses
  IdSipAuthentication, IdSipCore, IdSipMessage, IdSipMockTransactionDispatcher,
  IdSipProxy, TestFramework;

type
  TestTIdSipProxy = class(TTestCase)
  private
    Authenticator:    TIdSipAuthenticator;
    Dispatcher:       TIdSipMockTransactionDispatcher;
    Invite:           TIdSipRequest;
    Proxy:            TIdSipProxy;

    function  CreateAuthorizedRequest(OriginalRequest: TIdSipRequest;
                                      Challenge: TIdSipResponse): TIdSipRequest;
    procedure RemoveBody(Msg: TIdSipMessage);
    procedure SimulateRemoteInvite;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAuthorization;
    procedure TestRejectUnauthorizedRequest;
  end;

implementation

uses
  IdSipConsts, TestFrameworkSip, IdSipMockTransport, SysUtils;

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

  Self.Authenticator := TIdSipAuthenticator.Create;

  Self.Dispatcher := TIdSipMockTransactionDispatcher.Create;
  Self.Dispatcher.AddTransactionDispatcherListener(Self.Proxy);
  Self.Dispatcher.TransportType := TcpTransport;

  Self.Invite := TIdSipTestResources.CreateBasicRequest;
  Self.RemoveBody(Self.Invite);

  Self.Proxy := TIdSipProxy.Create;
  Self.Proxy.Authenticator := Self.Authenticator;
  Self.Proxy.Dispatcher    := Self.Dispatcher;

  Self.Dispatcher.MockLocator.AddA(Self.Invite.LastHop.SentBy, '127.0.0.1');
end;

procedure TestTIdSipProxy.TearDown;
begin
  FreeAndNil(Self.Proxy);
  FreeAndNil(Self.Dispatcher);
  FreeAndNil(Self.Invite);
  FreeAndNil(Self.Authenticator);

  inherited TearDown;
end;

//* TestTIdSipProxy Private methods ********************************************

function TestTIdSipProxy.CreateAuthorizedRequest(OriginalRequest: TIdSipRequest;
                                                 Challenge: TIdSipResponse): TIdSipRequest;
begin
  Result := OriginalRequest.Copy as TIdSipRequest;
end;

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

procedure TestTIdSipProxy.TestAuthorization;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
  Retry:         TIdSipRequest;
begin
  Self.Proxy.RequireAuthentication := true;

  ResponseCount := Self.Dispatcher.Transport.SentResponseCount;
  Self.SimulateRemoteInvite;

  Check(ResponseCount < Self.Dispatcher.Transport.SentResponseCount,
        'No response sent');

  Response := Self.Dispatcher.Transport.LastResponse;

  Retry := Self.CreateAuthorizedRequest(Self.Dispatcher.Transport.LastRequest, Response);
  try
    ResponseCount := Self.Dispatcher.Transport.SentResponseCount;

    Self.Dispatcher.Transport.FireOnRequest(Retry);

    Check(ResponseCount < Self.Dispatcher.Transport.SentResponseCount,
          'No response sent after re-attempt');
  finally
    Retry.Free;
  end;
end;

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
  CheckEquals(SIPProxyAuthenticationRequired,
              Response.StatusCode,
              'Unexpected response');
  Check(Response.HasProxyAuthenticate,
        'No Proxy-Authenticate header');
end;

initialization
  RegisterTest('Proxy', Suite);
end.
