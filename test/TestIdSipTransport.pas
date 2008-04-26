{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipTransport;

interface

uses
  Classes, IdConnectionBindings, IdRoutingTable, IdSipLocation, IdSipMessage,
  IdSipMockTransport, IdSipTransport, IdSipTcpTransport, IdTcpConnection,
  IdTcpServer, IdTimerQueue, SyncObjs, SysUtils, TestFramework, TestFrameworkEx,
  TestFrameworkSip, TestFrameworkSipTransport;

type
  TIdSipTransportSubclass = class(TIdSipTcpTransport)
  public
    procedure NotifyOfReceivedRequest(Request: TIdSipRequest);
    procedure NotifyOfReceivedResponse(Response: TIdSipResponse);
    procedure NotifyOfSentRequest(Request: TIdSipRequest;
                                  Binding: TIdConnectionBindings);
    procedure NotifyOfSentResponse(Response: TIdSipResponse;
                                   Binding: TIdConnectionBindings);
  end;

  TestTIdSipTransportEventNotifications = class(TTestCaseSip,
                                                IIdSipTransportListener,
                                                IIdSipTransportSendingListener)
  private
    Binding:          TIdConnectionBindings;
    ReceivedRequest:  Boolean;
    ReceivedResponse: Boolean;
    Request:          TIdSipRequest;
    Response:         TIdSipResponse;
    SentRequest:      Boolean;
    SentResponse:     Boolean;
    Transport:        TIdSipTransportSubclass;

    procedure OnException(FailedMessage: TIdSipMessage;
                          E: Exception;
                          const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport;
                               Source: TIdConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdConnectionBindings);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String;
                                Source: TIdConnectionBindings);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Binding: TIdConnectionBindings);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Binding: TIdConnectionBindings);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddTransportListener;
    procedure TestAddTransportSendingListener;
    procedure TestAllListenersReceiveRequests;
    procedure TestAllListenersReceiveResponses;
    procedure TestAllListenersSendRequests;
    procedure TestAllListenersSendResponses;
    procedure TestRemoveTransportListener;
    procedure TestRemoveTransportSendingListener;
  end;

  TestTransportRegistry = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultPortFor;
    procedure TestInsecureTransports;
    procedure TestIsSecure;
    procedure TestNonstandardPort;
    procedure TestRegisterTransportType;
    procedure TestSecureTransports;
    procedure TestTransportTypeFor;
    procedure TestUriSchemeFor;
  end;

  TestTIdSipMockTransport = class(TestTIdSipTransport)
  private
    GatewayIP:        String;
    LanDestination:   String;
    LanIP:            String;
    LocalhostIP:      String;
    MockRoutingTable: TIdMockRoutingTable;
    MockTransport:    TIdSipMockTransport;
    RequestOne:       TIdSipRequest;
    RequestTwo:       TIdSipRequest;
    RequestThree:     TIdSipRequest;
    RequestFour:      TIdSipRequest;
    ResponseOne:      TIdSipResponse;
    ResponseTwo:      TIdSipResponse;
    ResponseThree:    TIdSipResponse;
    ResponseFour:     TIdSipResponse;
    VpnDestination:   String;
    VpnIP:            String;

    procedure CheckBinding(ExpectedIP: String;
                           ExpectedPort: Integer;
                           Destination,
                           Msg: String);
    function  CreateRequest(Method, Transport: String): TIdSipRequest;
    function  CreateResponse(StatusCode: Cardinal): TIdSipResponse;
    procedure InitialiseRoutingTable;
  protected
    procedure CheckServerOnPort(const Host: String;
                                Port: Cardinal;
                                const Msg: String); override;
    procedure SendMessage(Msg: String); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFindBindingLanOnly;
    procedure TestFindBindingLocalhostOnly;
    procedure TestFindBindingLocalhostAndLan;
    procedure TestFindBindingMappedRoute;
    procedure TestFindBindingNonStandardPort;
    procedure TestFindBindingTwoLan;
    procedure TestFindBindingTwoLanMultiplePort;
    procedure TestLastRequest;
    procedure TestLastResponse;
    procedure TestSecondLastRequest;
    procedure TestThirdLastRequest;
  end;

  TestTIdSipMockSctpTransport = class(TestTIdSipMockTransport)
  protected
    function TransportType: TIdSipTransportClass; override;
  end;

  TestTIdSipMockTcpTransport = class(TestTIdSipMockTransport)
  protected
    function TransportType: TIdSipTransportClass; override;
  end;

  TestTIdSipMockTlsTransport = class(TestTIdSipMockTransport)
  protected
    function TransportType: TIdSipTransportClass; override;
  end;

  TestTIdSipMockTlsOverSctpTransport = class(TestTIdSipMockTransport)
  protected
    function TransportType: TIdSipTransportClass; override;
  end;

  TestTIdSipMockUdpTransport = class(TestTIdSipMockTransport)
  protected
    function TransportType: TIdSipTransportClass; override;
  end;

  TTransportWaitTestCase = class(TTestCase)
  protected
    Transport: TIdSipMockTransport;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipMessageExceptionWait = class(TTransportWaitTestCase)
  private
    Wait: TIdSipMessageExceptionWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
    procedure TestTriggerOnNonExistentAction;
    procedure TestTriggerOnWrongTypeOfObject;
  end;

  TestTIdSipReceiveMessageWait = class(TTransportWaitTestCase)
  private
    Wait: TIdSipReceiveMessageWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
    procedure TestTriggerOnNonExistentAction;
    procedure TestTriggerOnWrongTypeOfObject;
  end;

  TestTIdSipTransports = class(TTestCase)
  private
    List: TIdSipTransports;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestClear;
    procedure TestGetTransport;
  end;

  TTransportMethodTestCase = class(TTestCase)
  protected
    Transport: TIdSipTransport;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipTransportExceptionMethod = class(TTransportMethodTestCase)
  private
    Exception:     Exception;
    FailedMessage: TIdSipMessage;
    Method:        TIdSipTransportExceptionMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransportReceiveRequestMethod = class(TTransportMethodTestCase)
  private
    Method:  TIdSipTransportReceiveRequestMethod;
    Request: TIdSipRequest;
    Source:  TIdConnectionBindings;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransportReceiveResponseMethod = class(TTransportMethodTestCase)
  private
    Method:   TIdSipTransportReceiveResponseMethod;
    Response: TIdSipResponse;
    Source:   TIdConnectionBindings;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransportRejectedMessageMethod = class(TTransportMethodTestCase)
  private
    Method: TIdSipTransportRejectedMessageMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TTransportSendingMethodTestCase = class(TTransportMethodTestCase)
  protected
    Binding: TIdConnectionBindings;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipTransportSendingRequestMethod = class(TTransportSendingMethodTestCase)
  private
    Method:  TIdSipTransportSendingRequestMethod;
    Request: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransportSendingResponseMethod = class(TTransportSendingMethodTestCase)
  private
    Method:   TIdSipTransportSendingResponseMethod;
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

implementation

uses
  IdException, IdGlobal, IdRegisteredObject, IdSimpleParser, IdSipSCTPTransport,
  IdSipTlsTransport, IdSipUdpTransport, IdSocketHandle, IdSSLOpenSSL, IdStack,
  IdSystem, IdTcpClient, IdUdpClient, IdUDPServer, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransport unit tests');
  Result.AddTest(TestTIdSipTransportEventNotifications.Suite);
  Result.AddTest(TestTransportRegistry.Suite);
  Result.AddTest(TestTIdSipMockSctpTransport.Suite);
  Result.AddTest(TestTIdSipMockTcpTransport.Suite);
  Result.AddTest(TestTIdSipMockTlsTransport.Suite);
  Result.AddTest(TestTIdSipMockTlsOverSctpTransport.Suite);
  Result.AddTest(TestTIdSipMockUdpTransport.Suite);
  Result.AddSuite(TestTIdSipMessageExceptionWait.Suite);
  Result.AddSuite(TestTIdSipReceiveMessageWait.Suite);
  Result.AddSuite(TestTIdSipTransports.Suite);
  Result.AddTest(TestTIdSipTransportExceptionMethod.Suite);
  Result.AddTest(TestTIdSipTransportReceiveRequestMethod.Suite);
  Result.AddTest(TestTIdSipTransportReceiveResponseMethod.Suite);
  Result.AddTest(TestTIdSipTransportRejectedMessageMethod.Suite);
  Result.AddTest(TestTIdSipTransportSendingRequestMethod.Suite);
  Result.AddTest(TestTIdSipTransportSendingResponseMethod.Suite);
end;

//******************************************************************************
//* TIdSipTransportSubclass                                                    *
//******************************************************************************
//* TIdSipTransportSubclass Public methods *************************************

procedure TIdSipTransportSubclass.NotifyOfReceivedRequest(Request: TIdSipRequest);
var
  FakeBinding: TIdConnectionBindings;
begin
  FakeBinding := TIdConnectionBindings.Create;
  try
    inherited NotifyOfReceivedRequest(Request, FakeBinding);
  finally
    FakeBinding.Free;
  end;
end;

procedure TIdSipTransportSubclass.NotifyOfReceivedResponse(Response: TIdSipResponse);
var
  FakeBinding: TIdConnectionBindings;
begin
  FakeBinding := TIdConnectionBindings.Create;
  try
    inherited NotifyOfReceivedResponse(Response, FakeBinding);
  finally
    FakeBinding.Free;
  end;
end;

procedure TIdSipTransportSubclass.NotifyOfSentRequest(Request: TIdSipRequest;
                                                      Binding: TIdConnectionBindings);
begin
  inherited NotifyOfSentRequest(Request, Binding);
end;

procedure TIdSipTransportSubclass.NotifyOfSentResponse(Response: TIdSipResponse;
                                                       Binding: TIdConnectionBindings);
begin
  inherited NotifyOfSentResponse(Response, Binding);
end;

//******************************************************************************
//* TestTIdSipTransportEventNotifications                                      *
//******************************************************************************
//* TestTIdSipTransportEventNotifications Public methods ***********************

procedure TestTIdSipTransportEventNotifications.SetUp;
begin
  inherited SetUp;

  Self.Binding := TIdConnectionBindings.Create;
  Self.Binding.Transport := 'TCP';
  Self.Binding.LocalIP   := '127.0.0.1';
  Self.Binding.LocalPort := 5060;
  Self.Binding.PeerIP   := '127.0.0.2';
  Self.Binding.PeerPort := 5061;

  Self.ReceivedRequest  := false;
  Self.ReceivedResponse := false;
  Self.Request          := TIdSipTestResources.CreateLocalLoopRequest;
  Self.Response         := TIdSipTestResources.CreateLocalLoopResponse;
  Self.Transport        := TIdSipTransportSubclass.Create;
end;

procedure TestTIdSipTransportEventNotifications.TearDown;
begin
  Self.Transport.Free;
  Self.Response.Free;
  Self.Request.Free;
  Self.Binding.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportEventNotifications Private methods **********************

procedure TestTIdSipTransportEventNotifications.OnException(FailedMessage: TIdSipMessage;
                                                            E: Exception;
                                                            const Reason: String);
begin
end;

procedure TestTIdSipTransportEventNotifications.OnReceiveRequest(Request: TIdSipRequest;
                                                                 Receiver: TIdSipTransport;
                                                                 Source: TIdConnectionBindings);
begin
  Self.ReceivedRequest := true;
  Check(Self.Request = Request,     'Request not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnReceiveResponse(Response: TIdSipResponse;
                                                                  Receiver: TIdSipTransport;
                                                                  Source: TIdConnectionBindings);
begin
  Self.ReceivedResponse := true;

  Check(Self.Response = Response,   'Response not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnRejectedMessage(const Msg: String;
                                                                  const Reason: String;
                                                                  Source: TIdConnectionBindings);
begin
end;

procedure TestTIdSipTransportEventNotifications.OnSendRequest(Request: TIdSipRequest;
                                                              Sender: TIdSipTransport;
                                                              Binding: TIdConnectionBindings);
begin
  Self.SentRequest := true;
  Check(Self.Request = Request,     'Request not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnSendResponse(Response: TIdSipResponse;
                                                               Sender: TIdSipTransport;
                                                               Binding: TIdConnectionBindings);
begin
  Self.SentResponse := true;

  Check(Self.Response = Response,   'Response not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

//* TestTIdSipTransportEventNotifications Published methods *******************************

procedure TestTIdSipTransportEventNotifications.TestAddTransportListener;
begin
  Self.Transport.AddTransportListener(Self);

  Self.Transport.NotifyOfReceivedRequest(Self.Request);

  Check(Self.ReceivedRequest, 'Listener wasn''t added');
end;

procedure TestTIdSipTransportEventNotifications.TestAddTransportSendingListener;
begin
  Self.Transport.AddTransportSendingListener(Self);

  Self.Transport.NotifyOfSentRequest(Self.Request, Self.Binding);

  Check(Self.SentRequest, 'Listener wasn''t added');
end;

procedure TestTIdSipTransportEventNotifications.TestAllListenersReceiveRequests;
var
  Listener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Transport.AddTransportListener(Self);
    Self.Transport.AddTransportListener(Listener);

    Self.Transport.NotifyOfReceivedRequest(Self.Request);

    Check(Self.ReceivedRequest and Listener.ReceivedRequest,
          'Not all Listeners received the request');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransportEventNotifications.TestAllListenersReceiveResponses;
var
  Listener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Transport.AddTransportListener(Self);
    Self.Transport.AddTransportListener(Listener);

    Self.Transport.NotifyOfReceivedResponse(Self.Response);

    Check(Self.ReceivedResponse and Listener.ReceivedResponse,
          'Not all Listeners received the Response');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransportEventNotifications.TestAllListenersSendRequests;
var
  Listener: TIdSipTestTransportSendingListener;
begin
  Listener := TIdSipTestTransportSendingListener.Create;
  try
    Self.Transport.AddTransportSendingListener(Self);
    Self.Transport.AddTransportSendingListener(Listener);

    Self.Transport.NotifyOfSentRequest(Self.Request,
                                       Self.Binding);

    Check(Self.SentRequest and Listener.SentRequest,
          'Not all Listeners Sent the request');

    CheckEquals(Self.Binding.LocalIP,   Listener.BindingParam.LocalIP,   'Binding param Local IP');
    CheckEquals(Self.Binding.LocalPort, Listener.BindingParam.LocalPort, 'Binding param Local Port');
    CheckEquals(Self.Binding.PeerIP,    Listener.BindingParam.PeerIP,    'Binding param peer IP');
    CheckEquals(Self.Binding.PeerPort,  Listener.BindingParam.PeerPort,  'Binding param peer Port');
    CheckEquals(Self.Binding.Transport, Listener.BindingParam.Transport, 'Binding param transport');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransportEventNotifications.TestAllListenersSendResponses;
var
  Listener: TIdSipTestTransportSendingListener;
begin
  Listener := TIdSipTestTransportSendingListener.Create;
  try
    Self.Transport.AddTransportSendingListener(Self);
    Self.Transport.AddTransportSendingListener(Listener);

    Self.Transport.NotifyOfSentResponse(Self.Response,
                                        Self.Binding);

    Check(Self.SentResponse and Listener.SentResponse,
          'Not all Listeners Sent the Response');

    CheckEquals(Self.Binding.LocalIP,   Listener.BindingParam.LocalIP,   'Binding param Local IP');
    CheckEquals(Self.Binding.LocalPort, Listener.BindingParam.LocalPort, 'Binding param Local Port');
    CheckEquals(Self.Binding.PeerIP,    Listener.BindingParam.PeerIP,    'Binding param peer IP');
    CheckEquals(Self.Binding.PeerPort,  Listener.BindingParam.PeerPort,  'Binding param peer Port');
    CheckEquals(Self.Binding.Transport, Listener.BindingParam.Transport, 'Binding param transport');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransportEventNotifications.TestRemoveTransportListener;
begin
  Self.Transport.AddTransportListener(Self);
  Self.Transport.RemoveTransportListener(Self);

  Self.Transport.NotifyOfReceivedRequest(Self.Request);

  Check(not Self.ReceivedRequest, 'Listener wasn''t removed');
end;

procedure TestTIdSipTransportEventNotifications.TestRemoveTransportSendingListener;
begin
  Self.Transport.AddTransportSendingListener(Self);
  Self.Transport.RemoveTransportSendingListener(Self);

  Self.Transport.NotifyOfSentRequest(Self.Request,
                                     Self.Binding);

  Check(not Self.SentRequest, 'Listener wasn''t removed');
end;

//******************************************************************************
//* TestTransportRegistry                                                      *
//******************************************************************************
//* TestTransportRegistry Public methods ***************************************

procedure TestTransportRegistry.SetUp;
begin
  inherited SetUp;

  TIdSipTransportRegistry.RegisterTransportType(UdpTransport, TIdSipUdpTransport);
  TIdSipTransportRegistry.RegisterTransportType(TlsTransport, TIdSipTlsTransport);
end;

procedure TestTransportRegistry.TearDown;
begin
  TIdSipTransportRegistry.UnregisterTransportType(TlsTransport);
  TIdSipTransportRegistry.UnregisterTransportType(UdpTransport);

  inherited TearDown;
end;

//* TestTransportRegistry Published methods ************************************

procedure TestTransportRegistry.TestDefaultPortFor;
begin
    CheckEquals(TIdSipUDPTransport.DefaultPort,
                TIdSipTransportRegistry.DefaultPortFor(UdpTransport),
                UdpTransport);

    CheckEquals(TIdSipTLSTransport.DefaultPort,
                TIdSipTransportRegistry.DefaultPortFor(TlsTransport),
                TlsTransport);

    CheckEquals(TIdSipTransport.DefaultPort,
                TIdSipTransportRegistry.DefaultPortFor('unknown transport'),
                'unknown transport');
end;

procedure TestTransportRegistry.TestInsecureTransports;
var
  Transports: TStrings;
begin
  Transports := TStringList.Create;
  try
    TIdSipTransportRegistry.InsecureTransports(Transports);

    CheckEquals(1, Transports.Count, 'Count');
    CheckEquals(UdpTransport, Transports[0], 'First transport');

    TIdSipTransportRegistry.RegisterTransportType(TcpTransport, TIdSipMockTcpTransport);
    Transports.Clear;

    TIdSipTransportRegistry.InsecureTransports(Transports);

    CheckEquals(2,
                Transports.Count,
                'Count after new register');
    CheckEquals(UdpTransport,
                Transports[0],
                'First transport after new register');
    CheckEquals(TcpTransport,
                Transports[1],
                'Second transport after new register');

    TIdSipTransportRegistry.UnregisterTransportType(TcpTransport);
    Transports.Clear;

    TIdSipTransportRegistry.InsecureTransports(Transports);

    CheckEquals(1,
                Transports.Count,
                'Count after unregister');
    CheckEquals(UdpTransport,
                Transports[0],
                'First transport after unregister');
  finally
    Transports.Free;
  end;
end;

procedure TestTransportRegistry.TestIsSecure;
begin
  Check(TIdSipUDPTransport.IsSecure
      = TIdSipTransportRegistry.IsSecure(TIdSipUDPTransport.GetTransportType),
        TIdSipUDPTransport.GetTransportType);

  Check(TIdSipTLSTransport.IsSecure
      = TIdSipTransportRegistry.IsSecure(TIdSipTLSTransport.GetTransportType),
        TIdSipTLSTransport.GetTransportType);
end;

procedure TestTransportRegistry.TestNonstandardPort;
const
  Transports: array[1..2] of TIdSipTransportClass = (TIdSipTLSTransport, TIdSipUDPTransport);
var
  I: Integer;
begin
  for I := Low(Transports) to High(Transports) do begin
    Check(not TIdSipTransportRegistry.NonstandardPort(Transports[I].GetTransportType,
                                                      Transports[I].DefaultPort),
          Transports[I].GetTransportType + ' standard port');
    Check(TIdSipTransportRegistry.NonstandardPort(Transports[I].GetTransportType,
                                                  Transports[I].DefaultPort + 1),
          Transports[I].GetTransportType + ' standard port + 1');
  end;
end;

procedure TestTransportRegistry.TestRegisterTransportType;
const
  Foo = 'foo';
  TransportType: TIdSipTransportClass = TIdSipUDPTransport;
begin
  try
    TIdSipTransportRegistry.TransportTypeFor(Foo);
    Fail('Didn''t blow up on an unknown transport ' + Foo);
  except
    on EUnknownTransport do;
  end;

  TIdSipTransportRegistry.RegisterTransportType(Foo, TransportType);
  try
    Check(TransportType = TIdSipTransportRegistry.TransportTypeFor(Foo),
          Foo + ' transport type not registered');
  finally
    TIdSipTransportRegistry.UnregisterTransportType(Foo);
  end;

  try
    TIdSipTransportRegistry.TransportTypeFor(Foo);
    Fail('Didn''t unregister transport ' + Foo);
  except
    on EUnknownTransport do;
  end;
end;

procedure TestTransportRegistry.TestSecureTransports;
var
  Transports: TStrings;
begin
  Transports := TStringList.Create;
  try
    TIdSipTransportRegistry.SecureTransports(Transports);

    CheckEquals(1,            Transports.Count, 'Count');
    CheckEquals(TlsTransport, Transports[0],    'First transport');

    TIdSipTransportRegistry.RegisterTransportType(TlsOverSctpTransport, TIdSipMockTlsOverSctpTransport);
    Transports.Clear;

    TIdSipTransportRegistry.SecureTransports(Transports);

    CheckEquals(2,
                Transports.Count,
                'Count after new register');
    CheckEquals(TlsTransport,
                Transports[0],
                'First transport after new register');
    CheckEquals(TlsOverSctpTransport,
                Transports[1],
                'Second transport after new register');

    TIdSipTransportRegistry.UnregisterTransportType(TlsOverSctpTransport);
    Transports.Clear;

    TIdSipTransportRegistry.SecureTransports(Transports);

    CheckEquals(1,
                Transports.Count,
                'Count after unregister');
    CheckEquals(TlsTransport,
                Transports[0],
                'First transport after unregister');
  finally
    Transports.Free;
  end;
end;

procedure TestTransportRegistry.TestTransportTypeFor;
const
  NewTransport = 'UNKNOWN-TRANSPORT';
begin
  TIdSipTransportRegistry.RegisterTransportType(NewTransport, TIdSipSCTPTransport);
  try
    CheckEquals(TIdSipSCTPTransport,
                TIdSipTransportRegistry.TransportTypeFor(NewTransport),
                NewTransport);
  finally
    TIdSipTransportRegistry.UnregisterTransportType(NewTransport);
  end;
end;

procedure TestTransportRegistry.TestUriSchemeFor;
begin
  CheckEquals(TIdSipUDPTransport.UriScheme,
              TIdSipTransportRegistry.UriSchemeFor(UdpTransport),
              UdpTransport);

  CheckEquals(TIdSipTLSTransport.UriScheme,
              TIdSipTransportRegistry.UriSchemeFor(TlsTransport),
              TlsTransport);

  CheckEquals(TIdSipTransport.UriScheme,
              TIdSipTransportRegistry.UriSchemeFor('unknown transport'),
              'unknown transport');
end;

//******************************************************************************
//* TestTIdSipMockTransport                                                    *
//******************************************************************************
//* TestTIdSipMockTransport Public methods *************************************

procedure TestTIdSipMockTransport.SetUp;
begin
  inherited SetUp;

  (Self.LowPortTransport  as TIdSipMockTransport).AutoDispatch := true;
  (Self.HighPortTransport as TIdSipMockTransport).AutoDispatch := true;

  Self.RequestOne   := Self.CreateRequest(MethodInvite, Self.LowPortTransport.GetTransportType);
  Self.RequestTwo   := Self.CreateRequest(MethodRegister, Self.LowPortTransport.GetTransportType);
  Self.RequestThree := Self.CreateRequest(MethodOptions, Self.LowPortTransport.GetTransportType);
  Self.RequestFour  := Self.CreateRequest(MethodSubscribe, Self.LowPortTransport.GetTransportType);

  Self.ResponseOne   := Self.CreateResponse(SIPTrying);
  Self.ResponseTwo   := Self.CreateResponse(SIPOK);
  Self.ResponseThree := Self.CreateResponse(SIPMultipleChoices);
  Self.ResponseFour  := Self.CreateResponse(SIPBadRequest);

  Self.MockTransport    := Self.LowPortTransport as TIdSipMockTransport;
  Self.MockRoutingTable := Self.MockTransport.RoutingTable as TIdMockRoutingTable;

  // There's nothing special about these addresses: they're just the current
  // configuration of the author's development machine.
  Self.GatewayIP   := '10.0.0.1';
  Self.LocalhostIP := '127.0.0.1';
  Self.LanIP       := '10.0.0.6';
  Self.VpnIP       := '192.168.1.100';

  Self.LanDestination := TIdIPAddressParser.IncIPAddress(Self.LanIP);
  Self.VpnDestination := TIdIPAddressParser.IncIPAddress(Self.VpnIP);

  Self.InitialiseRoutingTable;
end;

procedure TestTIdSipMockTransport.TearDown;
begin
  Self.ResponseFour.Free;
  Self.ResponseThree.Free;
  Self.ResponseTwo.Free;
  Self.ResponseOne.Free;
  Self.RequestFour.Free;
  Self.RequestThree.Free;
  Self.RequestTwo.Free;
  Self.RequestOne.Free;

  // MockTransport tests execute really fast. So fast, in fact, that the
  // TTransportTestTimerQueue blows up when you try free it. (You know this
  // happened when you see an access violation at $77f8f281 referencing memory
  // at $10.) Sleeping a bit first makes the AV go away!
  //
  // Please, this is a terrible fix. If you know of a better way, let me know!

  Sleep(100);
  inherited TearDown;
end;

//* TestTIdSipMockTransport Protected methods **********************************

procedure TestTIdSipMockTransport.CheckServerOnPort(const Host: String;
                                                    Port: Cardinal;
                                                    const Msg: String);
var
  Bindings: TIdSipLocations;
  I:        Integer;
  Running:  Boolean;
begin
  if not Self.MockTransport.IsRunning then Fail(Msg);

  Bindings := TIdSipLocations.Create;
  try
    Self.MockTransport.LocalBindings(Bindings);

    Running  := false;
    for I := 0 to Bindings.Count - 1 do begin
      if (Bindings[I].IPAddress = Host)
        and (Bindings[I].Port = Port) then begin
        Running := true;
        Break;
      end;
    end;
  finally
    Bindings.Free;
  end;

  if not Running then Fail(Msg);
end;

procedure TestTIdSipMockTransport.SendMessage(Msg: String);
var
  Binding: TIdConnectionBindings;
  M:       TIdSipMessage;
begin
  M := TIdSipMessage.ReadMessageFrom(Msg);
  try
    Binding := TIdConnectionBindings.Create;
    try
      Binding.LocalIP   := Self.HighPortLocation.IPAddress;
      Binding.LocalPort := Self.HighPortLocation.Port;
      Binding.PeerIP    := TIdIPAddressParser.IncIPAddress(Self.HighPortLocation.IPAddress);
      Binding.PeerPort  := 5060;
      Binding.Transport := Self.HighPortLocation.Transport;

      if M.IsRequest then
        (Self.HighPortTransport as TIdSipMockTransport).FireOnRequest(M as TIdSipRequest, Binding)
      else
        (Self.HighPortTransport as TIdSipMockTransport).FireOnResponse(M as TIdSipResponse, Binding)
    finally
      Binding.Free;
    end;
  finally
    M.Free;
  end;
end;

procedure TestTIdSipMockTransport.CheckBinding(ExpectedIP: String;
                                               ExpectedPort: Integer;
                                               Destination,
                                               Msg: String);
var
  Binding: TIdSocketHandle;
  Dest:    TIdConnectionBindings;
begin
  Dest := TIdConnectionBindings.Create;
  try
    Dest.Transport :=  Self.MockTransport.GetTransportType;
    Dest.PeerIP    := Destination;
    Dest.PeerPort  := 5060;
    Binding := Self.MockTransport.FindBinding(Dest);

    CheckEquals(ExpectedIP,   Binding.IP, Msg + ': IP address');
    CheckEquals(ExpectedPort, Binding.Port, Msg + ': port');
  finally
    Dest.Free;
  end;
end;

function TestTIdSipMockTransport.CreateRequest(Method, Transport: String): TIdSipRequest;
begin
  Result := TIdSipRequest.ReadRequestFrom(BasicRequest);

  Result.Method            := Method;
  Result.CSeq.Method       := Method;
  Result.LastHop.Transport := Transport;
end;

function TestTIdSipMockTransport.CreateResponse(StatusCode: Cardinal): TIdSipResponse;
begin
  Result := TIdSipResponse.ReadResponseFrom(BasicResponse);

  Result.StatusCode := StatusCode;
end;

procedure TestTIdSipMockTransport.InitialiseRoutingTable;
var
  RT: TIdMockRoutingTable;
begin
  RT := Self.MockTransport.RoutingTable as TIdMockRoutingTable;

  RT.AddOsRoute(TIdIPAddressParser.NetworkFor(Self.LocalhostIP, 24),
                TIdIPAddressParser.MaskToAddress(24, TIdIPAddressParser.IPVersion(Self.LocalhostIP)),
                Self.LocalhostIP,
                0,
                '1',
                Self.LocalhostIP);
  RT.AddOsRoute(TIdIPAddressParser.NetworkFor(Self.LanIP, 24),
                TIdIPAddressParser.MaskToAddress(24, TIdIPAddressParser.IPVersion(Self.LanIP)),
                Self.LanIP,
                0,
                '1',
                Self.LanIP);
  RT.AddOsRoute(TIdIPAddressParser.NetworkFor(Self.VpnIP, 24),
                TIdIPAddressParser.MaskToAddress(24, TIdIPAddressParser.IPVersion(Self.VpnIP)),
                Self.VpnIP,
                0,
                '1',
                Self.VpnIP);

  RT.AddOsRoute('0.0.0.0', '0.0.0.0', Self.GatewayIP, 0, '1', Self.LanIP);
end;

//* TestTIdSipMockTransport Published methods **********************************

procedure TestTIdSipMockTransport.TestFindBindingLanOnly;
begin
  Self.MockTransport.ClearBindings;
  Self.MockTransport.AddBinding(Self.LanIP, 5060);

  CheckBinding(Self.LanIP, 5060, Self.LocalhostIP,    'Localhost');
  CheckBinding(Self.LanIP, 5060, Self.LanDestination, 'LAN IP');
  CheckBinding(Self.LanIP, 5060, Self.VpnDestination, 'VPN IP');
end;

procedure TestTIdSipMockTransport.TestFindBindingLocalhostOnly;
begin
  Self.MockTransport.ClearBindings;
  Self.MockTransport.AddBinding(Self.LocalhostIP, 5060);

  CheckBinding(Self.LocalhostIP, 5060, Self.LocalhostIP,    'Localhost');
  CheckBinding(Self.LocalhostIP, 5060, Self.LanDestination, 'LAN IP');
  CheckBinding(Self.LocalhostIP, 5060, Self.VpnDestination, 'VPN IP');
end;

procedure TestTIdSipMockTransport.TestFindBindingLocalhostAndLan;
begin
  Self.MockTransport.ClearBindings;
  Self.MockTransport.AddBinding(Self.LocalhostIP, 5060);
  Self.MockTransport.AddBinding(Self.LanIP,       5060);

  CheckBinding(Self.LocalhostIP, 5060, Self.LocalhostIP,    'Localhost');
  CheckBinding(Self.LanIP,       5060, Self.LanDestination, 'LAN IP');
  CheckBinding(Self.LocalhostIP, 5060, Self.VpnDestination, 'VPN IP');
end;

procedure TestTIdSipMockTransport.TestFindBindingMappedRoute;
begin
  Self.MockRoutingTable.AddMappedRoute('0.0.0.0', '0.0.0.0', '1.2.3.4', 5060);
  Self.MockTransport.AddBinding(Self.LanIP, 5060);

  CheckBinding(Self.LanIP, 5060, '5.6.7.8', 'Internet IP');
end;

procedure TestTIdSipMockTransport.TestFindBindingNonStandardPort;
var
  NonstandardPort: Cardinal;
  StandardPort:    Cardinal;
begin
  StandardPort := TIdSipTransportRegistry.DefaultPortFor(Self.MockTransport.GetTransportType);
  NonstandardPort := StandardPort + 1000;

  Self.MockTransport.ClearBindings;
  Self.MockTransport.AddBinding(Self.LocalhostIP, StandardPort);
  Self.MockTransport.AddBinding(Self.LanIP,       NonstandardPort);

  CheckBinding(Self.LanIP, NonstandardPort, Self.LanDestination, 'LAN IP (nonstandard port)');

  Self.MockTransport.AddBinding(Self.LanIP, StandardPort);

  CheckBinding(Self.LanIP, StandardPort, Self.LanDestination, 'LAN IP (standard port)');
end;

procedure TestTIdSipMockTransport.TestFindBindingTwoLan;
begin
  Self.MockTransport.ClearBindings;
  Self.MockTransport.AddBinding(Self.LocalhostIP, 5060);
  Self.MockTransport.AddBinding(Self.LanIP,       5060);
  Self.MockTransport.AddBinding(Self.VpnIP,       5060);

  CheckBinding(Self.LocalhostIP, 5060, Self.LocalhostIP,    'Localhost');
  CheckBinding(Self.LanIP,       5060, Self.LanDestination, 'LAN IP');
  CheckBinding(Self.VpnIP,       5060, Self.VpnDestination, 'VPN IP');
end;

procedure TestTIdSipMockTransport.TestFindBindingTwoLanMultiplePort;
const
  DefaultPort    = 5060;
  NonDefaultPort = 15060;
begin
  Self.MockTransport.ClearBindings;
  Self.MockTransport.AddBinding(Self.LocalhostIP, 5060);
  Self.MockTransport.AddBinding(Self.LanIP,       NonDefaultPort);
  Self.MockTransport.AddBinding(Self.LanIP,       DefaultPort);
  Self.MockTransport.AddBinding(Self.VpnIP,       5060);

  CheckBinding(Self.LocalhostIP, 5060,        Self.LocalhostIP,    'Localhost');
  CheckBinding(Self.LanIP,       DefaultPort, Self.LanDestination, 'LAN IP');
  CheckBinding(Self.VpnIP,       5060,        Self.VpnDestination, 'VPN IP');
end;

procedure TestTIdSipMockTransport.TestLastRequest;
begin
  CheckNull(Self.MockTransport.LastRequest, 'Unexpected last request from a new transport');

  Self.MockTransport.Send(Self.RequestOne, Self.HighPortLocation);

  CheckNotNull(Self.MockTransport.LastRequest, '(Copy of) last request not stored');
  Check(Self.MockTransport.LastRequest.Equals(Self.RequestOne), 'Unknown request stored');
end;

procedure TestTIdSipMockTransport.TestLastResponse;
begin
  CheckNull(Self.MockTransport.LastResponse, 'Unexpected last response from a new transport');

  Self.MockTransport.Send(Self.ResponseOne, Self.HighPortLocation);

  CheckNotNull(Self.MockTransport.LastResponse, '(Copy of) last response not stored');
  Check(Self.MockTransport.LastResponse.Equals(Self.ResponseOne), 'Unknown response stored');
end;

procedure TestTIdSipMockTransport.TestSecondLastRequest;
begin
  CheckNull(Self.MockTransport.SecondLastRequest, 'Unexpected second-last request from a new transport');

  Self.MockTransport.Send(Self.RequestOne, Self.HighPortLocation);
  CheckNull(Self.MockTransport.SecondLastRequest, 'Unexpected second-last request from a nearly new transport');

  Self.MockTransport.Send(Self.RequestTwo, Self.HighPortLocation);
  CheckNotNull(Self.MockTransport.LastRequest, '(Copy of) second-last request not stored');
  Check(Self.MockTransport.SecondLastRequest.Equals(Self.RequestOne), 'Unknown request stored');
end;

procedure TestTIdSipMockTransport.TestThirdLastRequest;
begin
  CheckNull(Self.MockTransport.ThirdLastRequest, 'Unexpected third-last request from a new transport');

  Self.MockTransport.Send(Self.RequestOne, Self.HighPortLocation);
  CheckNull(Self.MockTransport.ThirdLastRequest, 'Unexpected third-last request from a nearly new transport');

  Self.MockTransport.Send(Self.RequestTwo, Self.HighPortLocation);
  CheckNull(Self.MockTransport.ThirdLastRequest, 'Unexpected third-last request from a not new transport');

  Self.MockTransport.Send(Self.RequestThree, Self.HighPortLocation);
  CheckNotNull(Self.MockTransport.LastRequest, '(Copy of) third-last request not stored');
  Check(Self.MockTransport.ThirdLastRequest.Equals(Self.RequestOne), 'Unknown request stored');

  Self.MockTransport.Send(Self.RequestFour, Self.HighPortLocation);
  CheckNotNull(Self.MockTransport.LastRequest, '(Copy of) third-last request not stored (again)');
  Check(Self.MockTransport.ThirdLastRequest.Equals(Self.RequestTwo), 'Unknown request stored (again)');
end;

//******************************************************************************
//* TestTIdSipMockSctpTransport                                                *
//******************************************************************************
//* TestTIdSipMockSctpTransport Protected methods ******************************

function TestTIdSipMockSctpTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipMockSctpTransport;
end;

//******************************************************************************
//* TestTIdSipMockTcpTransport                                                 *
//******************************************************************************
//* TestTIdSipMockTcpTransport Protected methods *******************************

function TestTIdSipMockTcpTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipMockTcpTransport;
end;

//******************************************************************************
//* TestTIdSipMockTlsTransport                                                 *
//******************************************************************************
//* TestTIdSipMockTlsTransport Protected methods *******************************

function TestTIdSipMockTlsTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipMockTlsTransport;
end;

//******************************************************************************
//* TestTIdSipMockTlsOverSctpTransport                                         *
//******************************************************************************
//* TestTIdSipMockTlsOverSctpTransport Protected methods ***********************

function TestTIdSipMockTlsOverSctpTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipMockTlsOverSctpTransport;
end;

//******************************************************************************
//* TestTIdSipMockUdpTransport                                                 *
//******************************************************************************
//* TestTIdSipMockUdpTransport Protected methods *******************************

function TestTIdSipMockUdpTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipMockUdpTransport;
end;

//******************************************************************************
//* TTransportWaitTestCase                                                     *
//******************************************************************************
//* TTransportWaitTestCase Public methods **************************************

procedure TTransportWaitTestCase.SetUp;
var
  TransportType: String;
begin
  inherited SetUp;

  TransportType := UdpTransport;

  TIdSipTransportRegistry.RegisterTransportType(TransportType, TIdSipMockUdpTransport);
  Self.Transport := TIdSipTransportRegistry.TransportTypeFor(TransportType).Create as TIdSipMockTransport;
end;

procedure TTransportWaitTestCase.TearDown;
begin
  Self.Transport.Free;

  TIdSipTransportRegistry.UnregisterTransportType(UdpTransport);

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipMessageExceptionWait                                             *
//******************************************************************************
//* TestTIdSipMessageExceptionWait Public methods ******************************

procedure TestTIdSipMessageExceptionWait.SetUp;
var
  Msg: TIdSipMessage;
begin
  inherited SetUp;

  Msg := TIdSipTestResources.CreateBasicRequest;
  try
    Self.Wait := TIdSipMessageExceptionWait.Create;
    Self.Wait.ExceptionType := EIdSipTransport;
    Self.Wait.ExceptionMessage := 'Connection timed out';
    Self.Wait.FailedMessage    := Msg.Copy;
    Self.Wait.Reason           := 'Too lazy to evaluate';
    Self.Wait.TransportID      := Self.Transport.ID;
  finally
    Msg.Free;
  end;
end;

procedure TestTIdSipMessageExceptionWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdSipMessageExceptionWait Published methods ***************************

procedure TestTIdSipMessageExceptionWait.TestTrigger;
var
  L: TIdSipTestTransportListener;
begin
  L := TIdSipTestTransportListener.Create;
  try
    Self.Transport.AddTransportListener(L);

    Self.Wait.Trigger;

    Check(L.Exception, 'Listener not notified: no exception occured, so no Wait triggered');
  finally
    Self.Transport.RemoveTransportListener(L);
    L.Free;
  end;
end;

procedure TestTIdSipMessageExceptionWait.TestTriggerOnNonExistentAction;
begin
  // Check that the Wait doesn't blow up when given the ID of a nonexistent
  // transport.
  Self.Wait.TransportID := 'fake ID';
  Self.Wait.Trigger;
end;

procedure TestTIdSipMessageExceptionWait.TestTriggerOnWrongTypeOfObject;
var
  R: TIdRegisteredObject;
begin
  // Check that the Wait doesn't blow up when given the ID of a non-transport
  // object.
  R := TIdRegisteredObject.Create;
  try
    Self.Wait.TransportID := R.ID;
    Self.Wait.Trigger;
  finally
    R.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipReceiveMessageWait                                               *
//******************************************************************************
//* TestTIdSipReceiveMessageWait Public methods ********************************

procedure TestTIdSipReceiveMessageWait.SetUp;
begin
  inherited SetUp;

  Self.Wait := TIdSipReceiveMessageWait.Create;
  Self.Wait.Message      := TIdSipTestResources.CreateBasicRequest;
  Self.Wait.TransportID  := Self.Transport.ID;

  Self.Wait.ReceivedFrom.LocalIP   := '127.0.0.1';
  Self.Wait.ReceivedFrom.LocalPort := 5060;
  Self.Wait.ReceivedFrom.PeerIP    := '127.0.0.1';
  Self.Wait.ReceivedFrom.PeerPort  := 5060;
  Self.Wait.ReceivedFrom.Transport := Self.Transport.GetTransportType;
end;

procedure TestTIdSipReceiveMessageWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

procedure TestTIdSipReceiveMessageWait.TestTrigger;
var
  L: TIdSipTestTransportListener;
begin
  L := TIdSipTestTransportListener.Create;
  try
    Self.Transport.AddTransportListener(L);

    Self.Wait.Trigger;

    Check(L.ReceivedRequest, 'No request received, so no Wait triggered');
  finally
    Self.Transport.RemoveTransportListener(L);
    L.Free;
  end;
end;

procedure TestTIdSipReceiveMessageWait.TestTriggerOnNonExistentAction;
begin
  // Check that the Wait doesn't blow up when given the ID of a nonexistent
  // transport.
  Self.Wait.TransportID := 'fake ID';
  Self.Wait.Trigger;
end;

procedure TestTIdSipReceiveMessageWait.TestTriggerOnWrongTypeOfObject;
var
  R: TIdRegisteredObject;
begin
  // Check that the Wait doesn't blow up when given the ID of a non-transport
  // object.
  R := TIdRegisteredObject.Create;
  try
    Self.Wait.TransportID := R.ID;
    Self.Wait.Trigger;
  finally
    R.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTransports                                                       *
//******************************************************************************
//* TestTIdSipTransports Public methods ****************************************

procedure TestTIdSipTransports.SetUp;
begin
  inherited SetUp;

  Self.List := TIdSipTransports.Create;
end;

procedure TestTIdSipTransports.TearDown;
begin
  Self.List.Free;

  inherited TearDown;
end;

//* TestTIdSipTransports Published methods *************************************

procedure TestTIdSipTransports.TestAddAndCount;
begin
  CheckEquals(0, Self.List.Count, 'Supposedly empty list');

  Self.List.Add(TIdSipMockTransport.Create);
  CheckEquals(1, Self.List.Count, 'One Add');

  Self.List.Add(TIdSipMockTransport.Create);
  CheckEquals(2, Self.List.Count, 'Two Adds');

  Self.List.Add(TIdSipMockTransport.Create);
  CheckEquals(3, Self.List.Count, 'Three Adds');
end;

procedure TestTIdSipTransports.TestClear;
begin
  Self.List.Add(TIdSipMockTransport.Create);
  Self.List.Add(TIdSipMockTransport.Create);
  Self.List.Add(TIdSipMockTransport.Create);

  Self.List.Clear;

  CheckEquals(0, Self.List.Count, 'After Clear the list should contain nothing');
end;

procedure TestTIdSipTransports.TestGetTransport;
begin
  Self.List.Add(TIdSipMockSctpTransport.Create);
  Self.List.Add(TIdSipMockTcpTransport.Create);
  Self.List.Add(TIdSipMockUdpTransport.Create);

  CheckEquals(TIdSipMockSctpTransport.ClassName,
              Self.List[0].ClassName,
              'First item');
  CheckEquals(TIdSipMockTcpTransport.ClassName,
              Self.List[1].ClassName,
              'Second item');
  CheckEquals(TIdSipMockUdpTransport.ClassName,
              Self.List[2].ClassName,
              'Third item');
end;

//******************************************************************************
//* TTransportMethodTestCase                                                   *
//******************************************************************************
//* TTransportMethodTestCase Public methods ************************************

procedure TTransportMethodTestCase.SetUp;
begin
  inherited SetUp;

  Self.Transport := TIdSipMockUdpTransport.Create;
end;

procedure TTransportMethodTestCase.TearDown;
begin
  Self.Transport.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipTransportExceptionMethod                                         *
//******************************************************************************
//* TestTIdSipTransportExceptionMethod Public methods **************************

procedure TestTIdSipTransportExceptionMethod.SetUp;
begin
  inherited SetUp;

  Self.Exception     := EUnknownTransport.Create('');
  Self.FailedMessage := TIdSipResponse.Create;

  Self.Method := TIdSipTransportExceptionMethod.Create;
  Self.Method.Exception := Self.Exception;
  Self.Method.Reason    := 'Bar';
end;

procedure TestTIdSipTransportExceptionMethod.TearDown;
begin
  Self.Method.Free;
  Self.FailedMessage.Free;
  Self.Exception.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportExceptionMethod Published methods ***********************

procedure TestTIdSipTransportExceptionMethod.TestRun;
var
  Listener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.Exception, 'Listener not notified');
    Check(Self.Method.Exception = Listener.ExceptionParam,
          'Exception param');
    Check(Self.Method.FailedMessage = Listener.FailedMessageParam,
          'FailedMessage param');
    CheckEquals(Self.Method.Reason,
                Listener.ReasonParam,
                'Reason param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTransportReceiveRequestMethod                                    *
//******************************************************************************
//* TestTIdSipTransportReceiveRequestMethod Public methods *********************

procedure TestTIdSipTransportReceiveRequestMethod.SetUp;
begin
  inherited SetUp;

  Self.Request := TIdSipRequest.Create;
  Self.Source := TIdConnectionBindings.Create;

  Self.Method := TIdSipTransportReceiveRequestMethod.Create;
  Self.Method.Receiver := Self.Transport;
  Self.Method.Request  := Self.Request;
  Self.Method.Source   := Self.Source;
end;

procedure TestTIdSipTransportReceiveRequestMethod.TearDown;
begin
  Self.Method.Free;
  Self.Source.Free;
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportReceiveRequestMethod Published methods ******************

procedure TestTIdSipTransportReceiveRequestMethod.TestRun;
var
  Listener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.ReceivedRequest, 'Listener not notified');
    Check(Self.Method.Receiver = Listener.ReceiverParam,
          'Receiver param');
    Check(Self.Method.Request = Listener.RequestParam,
          'Request param');
    Check(Self.Method.Source = Listener.SourceParam,
          'Source param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTransportReceiveResponseMethod                                   *
//******************************************************************************
//* TestTIdSipTransportReceiveResponseMethod Public methods ********************

procedure TestTIdSipTransportReceiveResponseMethod.SetUp;
begin
  inherited SetUp;

  Self.Response := TIdSipResponse.Create;
  Self.Source   := TIdConnectionBindings.Create;

  Self.Method := TIdSipTransportReceiveResponseMethod.Create;
  Self.Method.Receiver := Self.Transport;
  Self.Method.Response := Self.Response;
  Self.Method.Source   := Self.Source;
end;

procedure TestTIdSipTransportReceiveResponseMethod.TearDown;
begin
  Self.Method.Free;
  Self.Source.Free;
  Self.Response.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportReceiveResponseMethod Published methods *****************

procedure TestTIdSipTransportReceiveResponseMethod.TestRun;
var
  Listener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.ReceivedResponse, 'Listener not notified');
    Check(Self.Method.Receiver = Listener.ReceiverParam,
          'Receiver param');
    Check(Self.Method.Response = Listener.ResponseParam,
          'Response param');
    Check(Self.Method.Source = Listener.SourceParam,
          'Source param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTransportRejectedMessageMethod                                   *
//******************************************************************************
//* TestTIdSipTransportRejectedMessageMethod Public methods ********************

procedure TestTIdSipTransportRejectedMessageMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipTransportRejectedMessageMethod.Create;
  Self.Method.Msg    := 'Foo';
  Self.Method.Reason := 'Bar';
  Self.Method.Source := TIdConnectionBindings.Create;
end;

procedure TestTIdSipTransportRejectedMessageMethod.TearDown;
begin
  Self.Method.Source.Free;
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportRejectedMessageMethod Published methods *****************

procedure TestTIdSipTransportRejectedMessageMethod.TestRun;
var
  Listener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.RejectedMessage, 'Listener not notified');
    CheckEquals(Self.Method.Msg,
                Listener.MsgParam,
                'Msg param');
    CheckEquals(Self.Method.Reason,
                Listener.ReasonParam,
                'Reason param');
    Check(Self.Method.Source = Listener.SourceParam,
          'Source param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TTransportSendingMethodTestCase                                            *
//******************************************************************************
//* TTransportSendingMethodTestCase Public methods *****************************

procedure TTransportSendingMethodTestCase.SetUp;
begin
  inherited SetUp;

  Self.Binding := TIdConnectionBindings.Create;
  Self.Binding.Transport := 'TCP';
  Self.Binding.PeerIP    := '127.0.0.1';
  Self.Binding.PeerPort  := 5060;
end;

procedure TTransportSendingMethodTestCase.TearDown;
begin
  Self.Binding.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipTransportSendingRequestMethod                                    *
//******************************************************************************
//* TestTIdSipTransportSendingRequestMethod Public methods *********************

procedure TestTIdSipTransportSendingRequestMethod.SetUp;
begin
  inherited SetUp;

  Self.Request := TIdSipRequest.Create;

  Self.Method := TIdSipTransportSendingRequestMethod.Create;
  Self.Method.Binding := Self.Binding;
  Self.Method.Request := Self.Request;
  Self.Method.Sender  := Self.Transport;
end;

procedure TestTIdSipTransportSendingRequestMethod.TearDown;
begin
  Self.Method.Free;
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportSendingRequestMethod Published methods ******************

procedure TestTIdSipTransportSendingRequestMethod.TestRun;
var
  Listener: TIdSipTestTransportSendingListener;
begin
  Listener := TIdSipTestTransportSendingListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.SentRequest, 'Listener not notified');
    Check(Self.Method.Binding = Listener.BindingParam,
          'Binding param');
    Check(Self.Method.Sender = Listener.SenderParam,
          'Sender param');
    Check(Self.Method.Request.Equals(Listener.RequestParam),
          'Request param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTransportSendingResponseMethod                                   *
//******************************************************************************
//* TestTIdSipTransportSendingResponseMethod Public methods ********************

procedure TestTIdSipTransportSendingResponseMethod.SetUp;
begin
  inherited SetUp;

  Self.Response := TIdSipResponse.Create;

  Self.Method := TIdSipTransportSendingResponseMethod.Create;
  Self.Method.Binding  := Self.Binding;
  Self.Method.Response := Self.Response;
  Self.Method.Sender   := Self.Transport;
end;

procedure TestTIdSipTransportSendingResponseMethod.TearDown;
begin
  Self.Method.Free;
  Self.Response.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportSendingResponseMethod Published methods *****************

procedure TestTIdSipTransportSendingResponseMethod.TestRun;
var
  Listener: TIdSipTestTransportSendingListener;
begin
  Listener := TIdSipTestTransportSendingListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.SentResponse, 'Listener not notified');
    Check(Self.Method.Binding = Listener.BindingParam,
          'Binding param');
    Check(Self.Method.Sender = Listener.SenderParam,
          'Sender param');
    Check(Self.Method.Response.Equals(Listener.ResponseParam),
          'Response param');
  finally
    Listener.Free;
  end;
end;

initialization
  RegisterTest('Transport layer', Suite);
end.
