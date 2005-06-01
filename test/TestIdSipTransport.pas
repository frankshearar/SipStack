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
  Classes, IdSipLocator, IdSipMessage, IdSipTcpClient, IdSipTcpServer,
  IdSipTransport, IdSocketHandle, IdTcpConnection, IdTcpServer, IdTimerQueue,
  SyncObjs, SysUtils, TestFramework, TestFrameworkEx, TestFrameworkSip;

type
  TIdSipTransportSubclass = class(TIdSipTcpTransport)
  public
    procedure NotifyTransportListeners(const Request: TIdSipRequest); overload;
    procedure NotifyTransportListeners(const Response: TIdSipResponse); overload;
    procedure NotifyTransportSendingListeners(const Request: TIdSipRequest); overload;
    procedure NotifyTransportSendingListeners(const Response: TIdSipResponse); overload;
  end;

  TestTIdSipTransportEventNotifications = class(TTestCaseSip,
                                                IIdSipTransportListener,
                                                IIdSipTransportSendingListener)
  private
    ReceivedRequest:  Boolean;
    ReceivedResponse: Boolean;
    Request:          TIdSipRequest;
    Response:         TIdSipResponse;
    SentRequest:      Boolean;
    SentResponse:     Boolean;
    Transport:        TIdSipTransportSubclass;

    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport);
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
    procedure TestRegisterTransport;
    procedure TestSecureTransports;
    procedure TestTransportFor;
    procedure TestUriSchemeFor;
  end;

  TestTIdSipTransport = class;
  TTransportTestTimerQueue = class(TIdThreadedTimerQueue)
  private
    fHighPortTransport: TIdSipTransport;
    FinishedEvent:      TEvent;
    fLowPortTransport:  TIdSipTransport;

    procedure ConfigureTransport(Transport: TIdSipTransport;
                                 const HostName: String;
                                 const Address: String;
                                 Port: Cardinal;
                                 TestCase: TestTIdSipTransport);

  public
    constructor Create(TransportType: TIdSipTransportClass;
                       TestCase: TestTIdSipTransport;
                       FinishedEvent: TEvent); reintroduce;
    destructor  Destroy; override;

    property HighPortTransport: TIdSipTransport read fHighPortTransport;
    property LowPortTransport:  TIdSipTransport read fLowPortTransport;
  end;

  TestTIdSipTransport = class(TThreadingTestCase,
                              IIdSipTransportListener,
                              IIdSipTransportSendingListener)
  private
    LastSentResponse: TIdSipResponse;

    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport);
  protected
    CheckingRequestEvent:  TIdSipRequestEvent;
    CheckingResponseEvent: TIdSipResponseEvent;
    EmptyListEvent:        TEvent;
    FinishedTimer:         TEvent;
    HighPortLocation:      TIdSipLocation;
    LowPortLocation:       TIdSipLocation;
    Parser:                TIdSipParser;
    ReceivedRequest:       Boolean;
    ReceivedResponse:      Boolean;
    RecvdRequest:          TIdSipRequest;
    RejectedMessage:       Boolean;
    Request:               TIdSipRequest;
    Response:              TIdSipResponse;
    SendEvent:             TEvent;
    Timer:                 TTransportTestTimerQueue;
    WrongServer:           Boolean;

    procedure CheckCanReceiveRequest(Sender: TObject;
                                     R: TIdSipRequest;
                                     ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckCanReceiveResponse(Sender: TObject;
                                      R: TIdSipResponse;
                                      ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckDiscardResponseWithUnknownSentBy(Sender: TObject;
                                                    R: TIdSipResponse;
                                                    ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckForBadRequest(Sender: TObject;
                                 R: TIdSipResponse;
                                 ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckForSIPVersionNotSupported(Sender: TObject;
                                             R: TIdSipResponse);
    procedure CheckReceivedParamDifferentIPv4SentBy(Sender: TObject;
                                                    Request: TIdSipRequest;
                                                    ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckReceivedParamFQDNSentBy(Sender: TObject;
                                           Request: TIdSipRequest;
                                           ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckReceivedParamIPv4SentBy(Sender: TObject;
                                           Request: TIdSipRequest;
                                           ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckResponse(Response: TIdSipResponse;
                            ExpectedStatusCode: Cardinal);
    procedure CheckSendRequestFromNonStandardPort(Sender: TObject;
                                                  R: TIdSipRequest;
                                                  ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckSendRequestTopVia(Sender: TObject;
                                     R: TIdSipRequest;
                                     ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckSendResponseFromNonStandardPort(Sender: TObject;
                                                   R: TIdSipResponse;
                                                   ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckServerNotOnPort(const Host: String;
                                   Port: Cardinal;
                                   const Msg: String);
    procedure CheckServerOnPort(const Host: String;
                                Port: Cardinal;
                                const Msg: String); virtual; abstract;
    procedure CheckUseRport(Sender: TObject;
                            R: TIdSipRequest;
                            ReceivedFrom: TIdSipConnectionBindings);
    function  HighPortTransport: TIdSipTransport;
    function  LowPortTransport: TIdSipTransport;
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnEmpty(Sender: TIdTimerQueue);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
    procedure ReturnResponse(Sender: TObject;
                             R: TIdSipRequest;
                             ReceivedFrom: TIdSipConnectionBindings);
    procedure SendFromLowTransport(Msg: String); virtual;
    procedure SendMessage(Msg: String); virtual; abstract;
    procedure SendOkResponse(Transport: TIdSipTransport);
    procedure SetEvent(Sender: TObject;
                       R: TIdSipResponse;
                       ReceivedFrom: TIdSipConnectionBindings);
    function  TransportType: TIdSipTransportClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    function  DefaultPort: Cardinal; virtual;
  published
    procedure TestCanReceiveRequest;
    procedure TestCanReceiveResponse;
    procedure TestCanReceiveUnsolicitedResponse;
    procedure TestChangePort;
    procedure TestIsNull; virtual;
    procedure TestDiscardResponseWithUnknownSentBy;
    procedure TestDiscardMalformedMessage;
    procedure TestDontDiscardUnknownSipVersion;
    procedure TestReceivedParamDifferentIPv4SentBy;
    procedure TestReceivedParamFQDNSentBy;
    procedure TestReceivedParamIPv4SentBy;
    procedure TestSendRequest;
    procedure TestSendRequestFromNonStandardPort;
    procedure TestSendRequestTopVia;
    procedure TestSendRequestUsesDestinationLocation;
    procedure TestSendResponse;
    procedure TestSendResponseFromNonStandardPort;
    procedure TestSendResponseUsesDestinationLocation;
    procedure TestSendResponseWithReceivedParam;
    procedure TestTortureTest16;
    procedure TestTortureTest17;
    procedure TestTortureTest19;
    procedure TestTortureTest21;
    procedure TestTortureTest22;
    procedure TestTortureTest23;
    procedure TestTortureTest35;
    procedure TestTortureTest40;
    procedure TestUseRport;
  end;

  TestTIdSipTCPTransport = class(TestTIdSipTransport)
  private
    ClientReceivedResponse: Boolean;
    ServerReceivedResponse: Boolean;
    SipClient:              TIdSipTcpClient;

    procedure AcknowledgeEvent(Sender: TObject;
                               Response: TIdSipResponse;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure ClientOnResponse(Sender: TObject;
                               Response: TIdSipResponse;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure ClientOnResponseDownClosedConnection(Sender: TObject;
                                                   Response: TIdSipResponse;
                                                   ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckSendResponsesDownClosedConnection(Sender: TObject;
                                                     Response: TIdSipResponse;
                                                     ReceivedFrom: TIdSipConnectionBindings);
    procedure Send200OK(Sender: TObject;
                        Request: TIdSipRequest;
                        ReceivedFrom: TIdSipConnectionBindings);
  protected
    function  CreateClient: TIdSipTcpClient; virtual;
    procedure CheckServerOnPort(const Host: String;
                                Port: Cardinal;
                                const Msg: String); override;
    procedure DestroyClient(Client: TIdSipTcpClient); virtual;
    procedure SendMessage(Msg: String); override;
    function  TransportType: TIdSipTransportClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
    procedure TestSendResponsesClosedConnection;
    procedure TestSendResponsesClosedConnectionReceivedParam;
    procedure TestSendResponsesOpenConnection;
  end;

  TestTIdSipTLSTransport = class(TestTIdSipTransport)
  private
    procedure DoOnPassword(var Password: String);
    procedure SetUpTls(Transport: TIdSipTransport);
  protected
    procedure SendMessage(Msg: String); override;
    function  TransportType: TIdSipTransportClass; override;
  public
    procedure SetUp; override;

    function  DefaultPort: Cardinal; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
  end;

  TestTIdSipUDPTransport = class(TestTIdSipTransport)
  private
    RPort: Cardinal;

    procedure CheckMessageWithTrailingGarbage(Sender: TObject;
                                              R: TIdSipRequest;
                                              ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckRportParamFilledIn(Sender: TObject;
                                      R: TIdSipRequest;
                                      ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckLeaveNonRportRequestsUntouched(Sender: TObject;
                                                  R: TIdSipRequest;
                                                  ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckMissingContentLength(Sender: TObject;
                                        R: TIdSipRequest;
                                        ReceivedFrom: TIdSipConnectionBindings);
    procedure NoteSourcePort(Sender: TObject;
                             R: TIdSipRequest;
                             ReceivedFrom: TIdSipConnectionBindings);
  protected
    procedure CheckServerOnPort(const Host: String;
                                Port: Cardinal;
                                const Msg: String); override;
    procedure SendMessage(Msg: String); override;
    function  TransportType: TIdSipTransportClass; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
    procedure TestLeaveNonRportRequestsUntouched;
    procedure TestMessageWithTrailingGarbage;
    procedure TestMissingContentLength;
    procedure TestRportParamFilledIn;
    procedure TestRportListening;
  end;
{
  TestTIdSipSCTPTransport = class(TestTIdSipTransport)
  protected
    function  TransportType: TIdSipTransportClass; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
  end;
}

  TestTIdSipMockTransport = class(TestTIdSipTransport)
  protected
    function TransportType: TIdSipTransportClass; override;
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

  TestTIdSipConnectionTableEntry = class(TTestCase)
  published
    procedure TestCreate;
  end;

  TestTIdSipConnectionTable = class(TTestCaseSip)
  private
    Conn:    TIdTCPConnection;
    NewConn: TIdTCPConnection;
    NewReq:  TIdSipRequest;
    Req:     TIdSipRequest;
    Table:   TIdSipConnectionTable;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestConnectionFor;
    procedure TestConnectionForOneEntry;
    procedure TestConnectionForOnEmptyList;
    procedure TestConnectionForOnNoEntry;
    procedure TestConnectionForResponse;
    procedure TestRemove;
    procedure TestRemoveOnEmptyList;
    procedure TestRemoveOnNonEmptyList;
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
    Exception: Exception;
    Method:    TIdSipTransportExceptionMethod;
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

  TestTIdSipTransportSendingRequestMethod = class(TTransportMethodTestCase)
  private
    Method:  TIdSipTransportSendingRequestMethod;
    Request: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransportSendingResponseMethod = class(TTransportMethodTestCase)
  private
    Method:  TIdSipTransportSendingResponseMethod;
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

implementation

uses
  IdException, IdGlobal, IdSipConsts, IdSipMockTransport, IdSipUdpServer,
  IdSSLOpenSSL, IdStack, IdTcpClient, IdUdpClient, IdUDPServer,
  TestMessages;

var
  ServerThatInstantiatesGStack: TIdTcpServer;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransport unit tests');
  Result.AddTest(TestTIdSipTransportEventNotifications.Suite);
  Result.AddTest(TestTransportRegistry.Suite);
  Result.AddTest(TestTIdSipTCPTransport.Suite);
//  Result.AddTest(TestTIdSipTLSTransport.Suite);
  Result.AddTest(TestTIdSipUDPTransport.Suite);
//  Result.AddTest(TestTIdSipSCTPTransport.Suite);
//  Result.AddTest(TestTIdSipMockTransport.Suite);
  Result.AddSuite(TestTIdSipTransports.Suite);
  Result.AddSuite(TestTIdSipConnectionTableEntry.Suite);
  Result.AddSuite(TestTIdSipConnectionTable.Suite);
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

procedure TIdSipTransportSubclass.NotifyTransportListeners(const Request: TIdSipRequest);
begin
  inherited NotifyTransportListeners(Request);
end;

procedure TIdSipTransportSubclass.NotifyTransportListeners(const Response: TIdSipResponse);
begin
  inherited NotifyTransportListeners(Response);
end;

procedure TIdSipTransportSubclass.NotifyTransportSendingListeners(const Request: TIdSipRequest);
begin
  inherited NotifyTransportSendingListeners(Request);
end;

procedure TIdSipTransportSubclass.NotifyTransportSendingListeners(const Response: TIdSipResponse);
begin
  inherited NotifyTransportSendingListeners(Response);
end;

//******************************************************************************
//* TestTIdSipTransportEventNotifications                                      *
//******************************************************************************
//* TestTIdSipTransportEventNotifications Public methods ***********************

procedure TestTIdSipTransportEventNotifications.SetUp;
begin
  inherited SetUp;

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

  inherited TearDown;
end;

//* TestTIdSipTransportEventNotifications Private methods **********************

procedure TestTIdSipTransportEventNotifications.OnException(E: Exception;
                                                            const Reason: String);
begin
end;

procedure TestTIdSipTransportEventNotifications.OnReceiveRequest(Request: TIdSipRequest;
                                                                 Receiver: TIdSipTransport);
begin
  Self.ReceivedRequest := true;
  Check(Self.Request = Request,     'Request not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnReceiveResponse(Response: TIdSipResponse;
                                                                  Receiver: TIdSipTransport);
begin
  Self.ReceivedResponse := true;

  Check(Self.Response = Response,   'Response not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnRejectedMessage(const Msg: String;
                                                                  const Reason: String);
begin
end;

procedure TestTIdSipTransportEventNotifications.OnSendRequest(Request: TIdSipRequest;
                                                              Sender: TIdSipTransport);
begin
  Self.SentRequest := true;
  Check(Self.Request = Request,     'Request not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnSendResponse(Response: TIdSipResponse;
                                                               Sender: TIdSipTransport);
begin
  Self.SentResponse := true;

  Check(Self.Response = Response,   'Response not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

//* TestTIdSipTransportEventNotifications Published methods *******************************

procedure TestTIdSipTransportEventNotifications.TestAddTransportListener;
begin
  Self.Transport.AddTransportListener(Self);

  Self.Transport.NotifyTransportListeners(Self.Request);

  Check(Self.ReceivedRequest, 'Listener wasn''t added');
end;

procedure TestTIdSipTransportEventNotifications.TestAddTransportSendingListener;
begin
  Self.Transport.AddTransportSendingListener(Self);

  Self.Transport.NotifyTransportSendingListeners(Self.Request);

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

    Self.Transport.NotifyTransportListeners(Self.Request);

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

    Self.Transport.NotifyTransportListeners(Self.Response);

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

    Self.Transport.NotifyTransportSendingListeners(Self.Request);

    Check(Self.SentRequest and Listener.SentRequest,
          'Not all Listeners Sent the request');
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

    Self.Transport.NotifyTransportSendingListeners(Self.Response);

    Check(Self.SentResponse and Listener.SentResponse,
          'Not all Listeners Sent the Response');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransportEventNotifications.TestRemoveTransportListener;
begin
  Self.Transport.AddTransportListener(Self);
  Self.Transport.RemoveTransportListener(Self);

  Self.Transport.NotifyTransportListeners(Self.Request);

  Check(not Self.ReceivedRequest, 'Listener wasn''t removed');
end;

procedure TestTIdSipTransportEventNotifications.TestRemoveTransportSendingListener;
begin
  Self.Transport.AddTransportSendingListener(Self);
  Self.Transport.RemoveTransportSendingListener(Self);

  Self.Transport.NotifyTransportSendingListeners(Self.Request);

  Check(not Self.SentRequest, 'Listener wasn''t removed');
end;

//******************************************************************************
//* TestTransportRegistry                                                      *
//******************************************************************************
//* TestTransportRegistry Public methods ***************************************

procedure TestTransportRegistry.SetUp;
begin
  inherited SetUp;

  TIdSipTransportRegistry.RegisterTransport(UdpTransport, TIdSipUdpTransport);
  TIdSipTransportRegistry.RegisterTransport(TlsTransport, TIdSipTlsTransport);
end;

procedure TestTransportRegistry.TearDown;
begin
  TIdSipTransportRegistry.UnregisterTransport(TlsTransport);
  TIdSipTransportRegistry.UnregisterTransport(UdpTransport);

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

    TIdSipTransportRegistry.RegisterTransport(TcpTransport, TIdSipMockTcpTransport);
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

    TIdSipTransportRegistry.UnregisterTransport(TcpTransport);
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

procedure TestTransportRegistry.TestRegisterTransport;
const
  Foo = 'foo';
  TransportType: TIdSipTransportClass = TIdSipUDPTransport;
begin
  try
    TIdSipTransportRegistry.TransportFor(Foo);
    Fail('Didn''t blow up on an unknown transport ' + Foo);
  except
    on EUnknownTransport do;
  end;

  TIdSipTransportRegistry.RegisterTransport(Foo, TransportType);
  try
    Check(TransportType = TIdSipTransportRegistry.TransportFor(Foo),
          Foo + ' transport type not registered');
  finally
    TIdSipTransportRegistry.UnregisterTransport(Foo);
  end;

  try
    TIdSipTransportRegistry.TransportFor(Foo);
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

    TIdSipTransportRegistry.RegisterTransport(TlsOverSctpTransport, TIdSipMockTlsOverSctpTransport);
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

    TIdSipTransportRegistry.UnregisterTransport(TlsOverSctpTransport);
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

procedure TestTransportRegistry.TestTransportFor;
const
  NewTransport = 'UNKNOWN-TRANSPORT';
begin
  TIdSipTransportRegistry.RegisterTransport(NewTransport, TIdSipSCTPTransport);
  try
    CheckEquals(TIdSipSCTPTransport,
                TIdSipTransportRegistry.TransportFor(NewTransport),
                NewTransport);
  finally
    TIdSipTransportRegistry.UnregisterTransport(NewTransport);
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
//* TTransportTestTimerQueue                                                   *
//******************************************************************************
//* TTransportTestTimerQueue Public methods ************************************

constructor TTransportTestTimerQueue.Create(TransportType: TIdSipTransportClass;
                                            TestCase: TestTIdSipTransport;
                                            FinishedEvent: TEvent);
begin
  inherited Create(false);

  Self.FinishedEvent := FinishedEvent;

  Self.fHighPortTransport := TransportType.Create;
  Self.ConfigureTransport(Self.HighPortTransport,
                          'localhost',
                          '127.0.0.1',
                          TestCase.DefaultPort + 10000,
                          TestCase);

  Self.fLowPortTransport  := TransportType.Create;
  Self.ConfigureTransport(Self.LowPortTransport,
                          'localhost',
                          '127.0.0.1',
                          TestCase.DefaultPort,
                          TestCase);

  Self.HighPortTransport.Start;
  Self.LowPortTransport.Start;
end;

destructor TTransportTestTimerQueue.Destroy;
begin
  Self.LowPortTransport.Stop;
  Self.HighPortTransport.Stop;
  Self.LowPortTransport.Free;
  Self.HighPortTransport.Free;

  Self.FinishedEvent.SetEvent;

  inherited Destroy;
end;

//* TTransportTestTimerQueue Private methods ***********************************

procedure TTransportTestTimerQueue.ConfigureTransport(Transport: TIdSipTransport;
                                                      const HostName: String;
                                                      const Address: String;
                                                      Port: Cardinal;
                                                      TestCase: TestTIdSipTransport);
begin
  Transport.AddTransportListener(TestCase);
  Transport.AddTransportSendingListener(TestCase);
  Transport.Timeout  := TestCase.DefaultTimeout div 10;
  Transport.Timer    := Self;
  Transport.HostName := HostName;
  Transport.Address  := Address;
  Transport.Port     := Port;
end;

//******************************************************************************
//* TestTIdSipTransport                                                        *
//******************************************************************************
//* TestTIdSipTransport Public methods *****************************************

procedure TestTIdSipTransport.SetUp;
begin
  inherited SetUp;

  if not Assigned(GStack) then
    raise Exception.Create('GStack isn''t instantiated - you need something '
                         + 'that opens a socket');

  TIdSipTransportRegistry.RegisterTransport(Self.TransportType.GetTransportType,
                                            Self.TransportType);

  Self.ExceptionMessage := Self.TransportType.ClassName + ': ' + Self.ExceptionMessage;

  Self.EmptyListEvent := TSimpleEvent.Create;
  Self.FinishedTimer  := TSimpleEvent.Create;
  Self.SendEvent := TSimpleEvent.Create;
  Self.Timer := TTransportTestTimerQueue.Create(Self.TransportType, Self, Self.FinishedTimer);
  Self.Timer.OnEmpty := Self.OnEmpty;

  Self.LastSentResponse := TIdSipResponse.Create;

  Check(Self.HighPortTransport <> nil,
        'Something went wrong creating the TTransportTestTimerQueue');
  Self.HighPortLocation := TIdSipLocation.Create(Self.TransportType.GetTransportType,
                                                 Self.HighPortTransport.Address,
                                                 Self.HighPortTransport.Port);
  Self.LowPortLocation := TIdSipLocation.Create(Self.TransportType.GetTransportType,
                                                Self.LowPortTransport.Address,
                                                Self.LowPortTransport.Port);

  Self.Request  := TIdSipTestResources.CreateLocalLoopRequest;
  Self.Request.LastHop.SentBy    := Self.LowPortTransport.Address;
  Self.Request.LastHop.Transport := Self.LowPortTransport.GetTransportType;
  Self.Request.RequestUri.Host   := Self.HighPortTransport.HostName;
  Self.Request.RequestUri.Port   := Self.HighPortTransport.Port;

  Self.Response := TIdSipTestResources.CreateLocalLoopResponse;
  Self.Response.LastHop.Transport := Self.HighPortTransport.GetTransportType;
  Self.Response.LastHop.Port      := Self.HighPortTransport.Port;

  Self.RecvdRequest := TIdSipRequest.Create;

  Self.ReceivedRequest  := false;
  Self.ReceivedResponse := false;
  Self.RejectedMessage  := false;
  Self.WrongServer      := false;
end;

procedure TestTIdSipTransport.TearDown;
begin
  Self.DefaultTimeout := Self.DefaultTimeout * 3 div 2;

  Self.Timer.Terminate;
  Self.WaitForSignaled(Self.EmptyListEvent, 'Waiting for timer to finish processing its events');
  Self.WaitForSignaled(Self.FinishedTimer, 'Waiting for timer to finish destroying its transports');

  Self.RecvdRequest.Free;
  Self.Response.Free;
  Self.Request.Free;

  Self.LowPortLocation.Free;
  Self.HighPortLocation.Free;

  Self.LastSentResponse.Free;

  Self.SendEvent.Free;
  Self.FinishedTimer.Free;
  Self.EmptyListEvent.Free;

  TIdSipTransportRegistry.UnregisterTransport(Self.TransportType.GetTransportType);

  inherited TearDown;
end;

function TestTIdSipTransport.DefaultPort: Cardinal;
begin
  Result := IdPORT_SIP;
end;

//* TestTIdSipTransport Protected methods **************************************

procedure TestTIdSipTransport.CheckCanReceiveRequest(Sender: TObject;
                                                     R: TIdSipRequest;
                                                     ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    Self.ReceivedRequest := true;
    Self.SendOkResponse(Sender as TIdSipTransport);

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckCanReceiveResponse(Sender: TObject;
                                                      R: TIdSipResponse;
                                                      ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    Self.ReceivedResponse := true;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckDiscardResponseWithUnknownSentBy(Sender: TObject;
                                                                    R: TIdSipResponse;
                                                                    ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.ReceivedResponse := true;
end;

procedure TestTIdSipTransport.CheckForBadRequest(Sender: TObject;
                                                 R: TIdSipResponse;
                                                 ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.CheckResponse(R, SIPBadRequest);
end;

procedure TestTIdSipTransport.CheckForSIPVersionNotSupported(Sender: TObject;
                                                             R: TIdSipResponse);
begin
  Self.CheckResponse(R, SIPSIPVersionNotSupported);
end;

procedure TestTIdSipTransport.CheckReceivedParamDifferentIPv4SentBy(Sender: TObject;
                                                                    Request: TIdSipRequest;
                                                                    ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.CheckReceivedParamFQDNSentBy(Sender, Request, ReceivedFrom);
end;

procedure TestTIdSipTransport.CheckReceivedParamFQDNSentBy(Sender: TObject;
                                                           Request: TIdSipRequest;
                                                           ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    Check(Request.LastHop.HasReceived,
          Self.HighPortTransport.ClassName
        + ': Received param not appended by transport layer');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckReceivedParamIPv4SentBy(Sender: TObject;
                                                           Request: TIdSipRequest;
                                                           ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    Check(not Request.LastHop.HasReceived,
          Self.HighPortTransport.ClassName
        + ': Received param appended by transport layer');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckResponse(Response: TIdSipResponse;
                                            ExpectedStatusCode: Cardinal);
begin
  try
    CheckEquals(ExpectedStatusCode,
                Response.StatusCode,
                'Wrong Status-Code');
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckSendRequestFromNonStandardPort(Sender: TObject;
                                                                  R: TIdSipRequest;
                                                                  ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    CheckEquals(Request.LastHop.Port,
                Self.HighPortTransport.Port,
                Self.HighPortTransport.ClassName
              + ': Port number on top via');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckSendRequestTopVia(Sender: TObject;
                                                     R: TIdSipRequest;
                                                     ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    Check(Self.HighPortTransport.GetTransportType = R.LastHop.Transport,
          Self.HighPortTransport.ClassName
       + ': Incorrect transport specified');

    Check(R.LastHop.HasBranch,
          Self.HighPortTransport.ClassName + ': Branch parameter missing');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckSendResponseFromNonStandardPort(Sender: TObject;
                                                                   R: TIdSipResponse;
                                                                   ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    CheckEquals(Self.LowPortTransport.Port,
                R.LastHop.Port,
                Self.HighPortTransport.ClassName
              + ': Port number on top via');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckServerNotOnPort(const Host: String;
                                                      Port: Cardinal;
                                                      const Msg: String);
var
  ServerRunning: Boolean;
begin
  ServerRunning := true;
  try
    Self.CheckServerOnPort(Host, Port, Msg);
  except
    on ETestFailure do
      ServerRunning := false;
  end;

  if ServerRunning then
    Fail('Server running on ' + Host + ':' + IntToStr(Port) + '; ' + Msg);
end;

procedure TestTIdSipTransport.CheckUseRport(Sender: TObject;
                                            R: TIdSipRequest;
                                            ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    Check(R.LastHop.HasParam(RportParam),
          'No rport param');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

function TestTIdSipTransport.HighPortTransport: TIdSipTransport;
begin
  Result := Self.Timer.HighPortTransport;
end;

function TestTIdSipTransport.LowPortTransport: TIdSipTransport;
begin
  Result := Self.Timer.LowPortTransport;
end;

procedure TestTIdSipTransport.OnException(E: Exception;
                                          const Reason: String);
begin
  Self.ExceptionType := ExceptClass(E.ClassType);
  Self.ExceptionMessage := E.Message + ' caused by ''' + Reason + '''';
end;

procedure TestTIdSipTransport.OnEmpty(Sender: TIdTimerQueue);
begin
  Self.EmptyListEvent.SetEvent;
end;

procedure TestTIdSipTransport.OnReceiveRequest(Request: TIdSipRequest;
                                               Receiver: TIdSipTransport);
var
  Fake: TIdSipConnectionBindings;
begin
  Self.RecvdRequest.Assign(Request);

  if Assigned(Self.CheckingRequestEvent) then
    Self.CheckingRequestEvent(Receiver, Request, Fake);
end;

procedure TestTIdSipTransport.OnReceiveResponse(Response: TIdSipResponse;
                                                Receiver: TIdSipTransport);
var
  Fake: TIdSipConnectionBindings;
begin
  if Assigned(Self.CheckingResponseEvent) then
    Self.CheckingResponseEvent(Receiver, Response, Fake);
end;

procedure TestTIdSipTransport.OnRejectedMessage(const Msg: String;
                                                const Reason: String);
begin
  Self.RejectedMessage := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTransport.ReturnResponse(Sender: TObject;
                                             R: TIdSipRequest;
                                             ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    Self.SendOkResponse(Sender as TIdSipTransport);
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.SendFromLowTransport(Msg: String);
begin
  if (Pos('%s', Msg) > 0) then
    Msg := StringReplace(Msg,
                         '%s',
                         Self.HighPortTransport.Address
                       + ':'
                       + IntToStr(Self.HighPortTransport.Port), [rfReplaceAll]);

  Self.SendMessage(Msg);
end;

procedure TestTIdSipTransport.SendOkResponse(Transport: TIdSipTransport);
begin
  Self.Response.StatusCode := SIPOK;

  Transport.Send(Self.Response, Self.HighPortLocation);
end;

procedure TestTIdSipTransport.SetEvent(Sender: TObject;
                                       R: TIdSipResponse;
                                       ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.ThreadEvent.SetEvent;
end;

function TestTIdSipTransport.TransportType: TIdSipTransportClass;
begin
  raise Exception.Create('TestTIdSipTransport.TransportType: override for '
                       + Self.ClassName + '!');
  Result := nil;
end;

//* TestTIdSipTransport Private methods ****************************************

procedure TestTIdSipTransport.OnSendRequest(Request: TIdSipRequest;
                                            Sender: TIdSipTransport);
begin
end;

procedure TestTIdSipTransport.OnSendResponse(Response: TIdSipResponse;
                                             Sender: TIdSipTransport);
begin
  Self.LastSentResponse.Assign(Response);
  Self.SendEvent.SetEvent;
end;

//* TestTIdSipTransport Published methods **************************************

procedure TestTIdSipTransport.TestCanReceiveRequest;
begin
  // LowPortTransport sends an INVITE to HighPortTransport.
  // We check that HighPortTransport did actually get it.
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;

  Check(Self.ReceivedRequest,
        Self.HighPortTransport.ClassName + ': Request not received');
end;

procedure TestTIdSipTransport.TestCanReceiveResponse;
begin
  // LowPortTransport sends an INVITE to HighPortTransport.
  // HighPortTransport is set to immediately respond with a 200 OK.
  // We then check that LowPortTransport got the returned response.
  Self.CheckingRequestEvent  := Self.ReturnResponse;
  Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;

  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;

  Check(Self.ReceivedResponse,
        Self.HighPortTransport.ClassName + ': Response not received');
end;

procedure TestTIdSipTransport.TestCanReceiveUnsolicitedResponse;
begin
  Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;

  Self.HighPortTransport.Send(Self.Response, Self.LowPortLocation);

  Self.WaitForSignaled;

  Check(Self.ReceivedResponse,
        Self.HighPortTransport.ClassName + ': Response not received');
end;

procedure TestTIdSipTransport.TestChangePort;
var
  OriginalPort: Cardinal;
begin
  OriginalPort := Self.LowPortTransport.Port;
  Self.CheckServerOnPort(Self.LowPortTransport.Address,
                         OriginalPort,
                         'Sanity check');

  Self.LowPortTransport.Port := OriginalPort + 1;
  Self.CheckServerOnPort(Self.LowPortTransport.Address,
                         Self.LowPortTransport.Port,
                         'Port changed');
  Self.CheckServerNotOnPort(Self.LowPortTransport.Address,
                            OriginalPort,
                            'Port changed but still listening on old port');
end;

procedure TestTIdSipTransport.TestIsNull;
begin
  Check(not Self.HighPortTransport.IsNull,
        'non-null transport (' + Self.HighPortTransport.ClassName
      + ') marked as null');
end;

procedure TestTIdSipTransport.TestDiscardResponseWithUnknownSentBy;
begin
  Self.CheckingResponseEvent := Self.CheckDiscardResponseWithUnknownSentBy;

  // If we don't set the received param we won't be able to send
  // the message to the right SIP server. No, you'd never do this
  // in production, because it's wilfully wrong.
  Self.Response.LastHop.SentBy := 'unknown.host';
  Self.Response.LastHop.Received := Self.LowPortTransport.Address;
  Self.LowPortTransport.Send(Self.Response, Self.HighPortLocation);

  Self.WaitForSignaled;
  Check(not Self.ReceivedResponse,
        Self.HighPortTransport.ClassName
      + ': Response not silently discarded');
  Check(Self.RejectedMessage,
        Self.HighPortTransport.ClassName
      + ': Rejected message event didn''t fire');
end;

procedure TestTIdSipTransport.TestDiscardMalformedMessage;
var
  Listener:          TIdSipTestTransportListener;
  MangledSipVersion: String;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.HighPortTransport.AddTransportListener(Listener);
    MangledSipVersion := 'SIP/;2.0';
    Self.SendMessage('INVITE sip:wintermute@tessier-ashpool.co.luna ' + MangledSipVersion + #13#10
                   + 'Via: SIP/2.0/TCP %s;branch=z9hG4bK776asdhds'#13#10
                   + 'Max-Forwards: 70'#13#10
                   + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.luna>'#13#10
                   + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                   + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                   + 'CSeq: 314159 INVITE'#13#10
                   + 'Contact: <sip:wintermute@tessier-ashpool.co.luna>'#13#10
                   + 'Content-Type: text/plain'#13#10
                   + 'Content-Length: 29'#13#10
                   + #13#10
                   + 'I am a message. Hear me roar!');

    Self.WaitForSignaled(Self.SendEvent);

    Check(not Self.ReceivedRequest,
          Self.HighPortTransport.ClassName
        + ': Somehow we received a mangled message');
    Check(Self.RejectedMessage,
          Self.HighPortTransport.ClassName
        + ': Notification of message rejection not received');

    CheckNotEquals(0,
                   Self.LastSentResponse.StatusCode,
                   'We didn''t receive the "Bad Request" response');

    // Check that the transport sends the 400 Bad Request.
    CheckEquals(SIPBadRequest,
                Self.LastSentResponse.StatusCode,
                Self.HighPortTransport.ClassName
              + ': "Bad Request" response');

    // Check that the transport didn't send the malformed request up the stack
    Check(not Listener.ReceivedRequest,
          Self.HighPortTransport.ClassName
        + ': Transport passed malformed request up the stack');
  finally
    Self.HighPortTransport.RemoveTransportListener(Listener);
    Listener.Free;
  end;
end;

procedure TestTIdSipTransport.TestDontDiscardUnknownSipVersion;
begin
  // The Transaction-User level handles rejecting these messages.  
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.ExceptionMessage := 'Waiting for request to arrive';
  Self.SendMessage(TortureTest41);
  Self.WaitForSignaled(Self.ClassName
                    + ': Didn''t receive a message with an unknown SIP-Version (timeout)');
end;

procedure TestTIdSipTransport.TestReceivedParamDifferentIPv4SentBy;
begin
  Self.CheckingRequestEvent := Self.CheckReceivedParamDifferentIPv4SentBy;

  Self.Request.LastHop.SentBy := '127.0.0.3';
  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestReceivedParamFQDNSentBy;
begin
  Self.CheckingRequestEvent := Self.CheckReceivedParamFQDNSentBy;

  Self.Request.LastHop.SentBy := 'localhost';
  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestReceivedParamIPv4SentBy;
begin
  Self.CheckingRequestEvent := Self.CheckReceivedParamIPv4SentBy;
  // This is a bit of a hack. We want to make sure the sent-by's an IP.
  Self.HighPortTransport.HostName := Self.HighPortTransport.Address;
  Self.HighPortTransport.Send(Self.Request, Self.LowPortLocation);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestSendRequest;
begin
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;

  Check(Self.ReceivedRequest,
        Self.HighPortTransport.ClassName + ': Request not received');
end;

procedure TestTIdSipTransport.TestSendRequestFromNonStandardPort;
begin
  Self.Request.RequestUri.Host := Self.LowPortTransport.HostName;
  Self.Request.RequestUri.Port := Self.LowPortTransport.Port;
  Self.CheckingRequestEvent := Self.CheckSendRequestFromNonStandardPort;
  Self.HighPortTransport.Send(Self.Request, Self.LowPortLocation);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestSendRequestTopVia;
begin
  Self.CheckingRequestEvent := Self.CheckSendRequestTopVia;
  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestSendRequestUsesDestinationLocation;
begin
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.Request.RequestUri.Port := Self.Request.RequestUri.Port + 1;
  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestSendResponse;
begin
  Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;

  Self.LowPortTransport.Send(Self.Response, Self.HighPortLocation);

  Self.WaitForSignaled;

  Check(Self.ReceivedResponse,
        Self.HighPortTransport.ClassName + ': Response not received');
end;

procedure TestTIdSipTransport.TestSendResponseFromNonStandardPort;
begin
  Self.Response.LastHop.Port := Self.LowPortTransport.Port;
  Self.CheckingResponseEvent := Self.CheckSendResponseFromNonStandardPort;
  Self.HighPortTransport.Send(Self.Response, Self.LowPortLocation);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestSendResponseUsesDestinationLocation;
begin
  Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;

  Self.Response.LastHop.Port := Self.Response.LastHop.Port + 1;
  Self.LowPortTransport.Send(Self.Response, Self.HighPortLocation);

  Self.WaitForSignaled;

  Check(Self.ReceivedResponse,
        Self.HighPortTransport.ClassName + ': Response not received');
end;

procedure TestTIdSipTransport.TestSendResponseWithReceivedParam;
var
  HighPortListener: TIdSipTestTransportListener;
  LowPortListener:  TIdSipTestTransportListener;
begin
  // Send a response from HighPortTransport to LowPortTransport. Add listeners
  // to the two transports and check that only LowPortTransport received the
  // response.
  Self.CheckingResponseEvent := Self.SetEvent;

  HighPortListener := TIdSipTestTransportListener.Create;
  try
    Self.HighPortTransport.AddTransportListener(HighPortListener);
    try
      LowPortListener := TIdSipTestTransportListener.Create;
      try
        Self.LowPortTransport.AddTransportListener(LowPortListener);
        try
          // Ensure that we only set ThreadEvent after we know that
          // LowPortListener's finished with it
          Self.LowPortTransport.RemoveTransportListener(Self);
          Self.LowPortTransport.AddTransportListener(Self);

          Self.Response.LastHop.Received := Self.LowPortTransport.Address;
          Self.HighPortTransport.Send(Self.Response, Self.LowPortLocation);

          // It's not perfect, but anyway. We need to wait long enough for
          // LowPortTransport to get its response.
          Self.WaitForSignaled;

          Check(not Self.ReceivedRequest,
                Self.HighPortTransport.ClassName
              + ': Somehow we received a mangled message');

          Check(LowPortListener.ReceivedResponse,
                Self.HighPortTransport.ClassName
              + ': LowPortTransport didn''t get the message');
          Check(not HighPortListener.ReceivedResponse,
                Self.HighPortTransport.ClassName
              + ': Received param in top Via header ignored - '
              + 'wrong server got the message');
        finally
          Self.LowPortTransport.RemoveTransportListener(LowPortListener);
        end;
      finally
        LowPortListener.Free;
      end;
    finally
      Self.HighPortTransport.RemoveTransportListener(HighPortListener);
    end;
  finally
    HighPortListener.Free;
  end;
end;

procedure TestTIdSipTransport.TestTortureTest16;
begin
  // Content-Length much larger than message body

  Self.DefaultTimeout := Self.HighPortTransport.Timeout * 2;

  Self.CheckingResponseEvent := Self.CheckForBadRequest;
  Self.SendFromLowTransport(TortureTest16);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestTortureTest17;
begin
  // Negative Content-Length

  Self.CheckingResponseEvent := Self.CheckForBadRequest;
  Self.SendFromLowTransport(TortureTest17);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestTortureTest19;
begin
  // Unterminated quote in To

  Self.CheckingResponseEvent := Self.CheckForBadRequest;
  Self.SendFromLowTransport(TortureTest19);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestTortureTest21;
begin
  // Request-URI in <>

  Self.CheckingResponseEvent := Self.CheckForBadRequest;
  Self.SendFromLowTransport(TortureTest21);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestTortureTest22;
begin
  // Illegal LWS within the Request-URI.

  Self.CheckingResponseEvent := Self.CheckForBadRequest;
  Self.SendFromLowTransport(TortureTest22);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestTortureTest23;
begin
  // Illegal >1 SP between elements of the Request-Line.

  Self.CheckingResponseEvent := Self.CheckForBadRequest;
  Self.SendFromLowTransport(TortureTest23);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestTortureTest35;
var
  Destination: String;
begin
  // Badly mangled message: two mangled Expires, duplicated To, Call-ID,
  // Cseq headers.

  Destination := Self.HighPortTransport.Address + ':' + IntToStr(Self.HighPortTransport.Port);

  Self.CheckingResponseEvent := Self.CheckForBadRequest;
  Self.SendFromLowTransport(StringReplace(TortureTest35, '%s', Destination, [rfReplaceAll]));

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestTortureTest40;
begin
  // Illegal >1 SP between elements of the Request-Line.

  Self.CheckingResponseEvent := Self.CheckForBadRequest;
  Self.SendFromLowTransport(TortureTest40);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestUseRport;
begin
  Self.ExceptionMessage := 'Waiting for rport request';
  Self.LowPortTransport.UseRport := true;
  Self.CheckingRequestEvent := Self.CheckUseRport;
  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;
end;

//******************************************************************************
//* TestTIdSipTCPTransport                                                     *
//******************************************************************************
//* TestTIdSipTCPTransport Public methods **************************************

procedure TestTIdSipTCPTransport.SetUp;
begin
  inherited SetUp;

  Self.SipClient := Self.CreateClient;

  Self.ClientReceivedResponse := false;
  Self.ServerReceivedResponse := false;
end;

procedure TestTIdSipTCPTransport.TearDown;
begin
  Self.DestroyClient(Self.SipClient);

  inherited TearDown;
end;

//* TestTIdSipTCPTransport Protected methods ***********************************

function TestTIdSipTCPTransport.CreateClient: TIdSipTcpClient;
begin
  Result := TIdSipTcpClient.Create(nil);
end;

procedure TestTIdSipTCPTransport.CheckServerOnPort(const Host: String;
                                                   Port: Cardinal;
                                                   const Msg: String);
var
  Client: TIdTcpClient;
begin
  try
    Client := TIdTcpClient.Create(nil);
    try
      Client.Host := Host;
      Client.Port := Port;
      Client.Connect;
      try
        // Do nothing
      finally
        Client.Disconnect;
      end;
    finally
      Client.Free;
    end;
  except
    on EIdSocketError do
      Fail('No server running on ' + Host + ': ' + IntToStr(Port) + '; ' + Msg);
  end;
end;

procedure TestTIdSipTCPTransport.DestroyClient(Client: TIdSipTcpClient);
begin
  Client.Free;
end;

procedure TestTIdSipTCPTransport.SendMessage(Msg: String);
var
  Client: TIdTcpClient;
begin
  Client := TIdTcpClient.Create(nil);
  try
    Client.Host := Self.HighPortTransport.Address;
    Client.Port := Self.HighPortTransport.Port;
    Client.Connect(DefaultTimeout);
    try
      Client.Write(Msg);
    finally
      Client.DisconnectSocket;
    end;
  finally
    Client.Free;
  end;
end;

function TestTIdSipTCPTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipTcpTransport;
end;

//* TestTIdSipTCPTransport Private methods *************************************

procedure TestTIdSipTCPTransport.AcknowledgeEvent(Sender: TObject;
                                                  Response: TIdSipResponse;
                                                  ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTCPTransport.ClientOnResponse(Sender: TObject;
                                                  Response: TIdSipResponse;
                                                  ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    CheckEquals(SIPOK, Response.StatusCode, 'Status-Code');
    Self.ClientReceivedResponse := true;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTCPTransport.ClientOnResponseDownClosedConnection(Sender: TObject;
                                                                      Response: TIdSipResponse;
                                                                      ReceivedFrom: TIdSipConnectionBindings);
begin
  Fail('The connection is closed. The client should not receive a response');
end;

procedure TestTIdSipTCPTransport.CheckSendResponsesDownClosedConnection(Sender: TObject;
                                                                        Response: TIdSipResponse;
                                                                        ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    CheckEquals(SIPOK, Response.StatusCode, 'Status-Code');
    Self.ServerReceivedResponse := true;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTCPTransport.Send200OK(Sender: TObject;
                                           Request: TIdSipRequest;
                                           ReceivedFrom: TIdSipConnectionBindings);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipMessage.ReadResponseFrom(LocalLoopResponse);
  try
    Response.StatusCode := SIPOK;
    Self.LowPortTransport.Send(Response, Self.HighPortLocation);
  finally
    Response.Free;
  end;
end;

//* TestTIdSipTCPTransport Published methods ***********************************

procedure TestTIdSipTCPTransport.TestGetTransportType;
begin
  CheckEquals(TcpTransport,
              Self.HighPortTransport.GetTransportType,
              'Transport type');
end;

procedure TestTIdSipTCPTransport.TestIsReliable;
begin
  Check(Self.HighPortTransport.IsReliable,
        'TCP transport not marked as reliable');
end;

procedure TestTIdSipTCPTransport.TestIsSecure;
begin
  Check(not Self.HighPortTransport.IsSecure, 'TCP transport marked as secure');
end;

procedure TestTIdSipTCPTransport.TestSendResponsesClosedConnection;
var
  Request:  TIdSipRequest;
  Response: TIdSipResponse;
begin
  // We send an INVITE to the low port transport. The client then disconnects its
  // connection. We check that the transport sends the response down a new TCP
  // connection. In this case the transport will make a connection to itself
  // (since it runs on the default transport port).

  Self.CheckingResponseEvent := Self.CheckSendResponsesDownClosedConnection;

  Request := TIdSipMessage.ReadRequestFrom(LocalLoopRequest);
  try
    Self.SipClient.OnResponse  := Self.ClientOnResponseDownClosedConnection;
    Self.SipClient.Host        := Self.LowPortTransport.Address;
    Self.SipClient.Port        := Self.LowPortTransport.Port;
    Self.SipClient.ReadTimeout := 100;

    Self.SipClient.Connect;
    try
      Self.SipClient.Send(Request);
    finally
      Self.SipClient.Disconnect;
    end;

    // Why should we need to sleep here?
    // When SipClient disconnects, the server side of the connection should
    // invoke OnDisconnect which then unregisters the connection from the
    // ConnectionMap.
    Sleep(500);

    Response := TIdSipMessage.ReadResponseFrom(LocalLoopResponse);
    try
      Response.StatusCode := SIPOK;
      Self.LowPortTransport.Send(Response, Self.HighPortLocation);
    finally
      Response.Free;
    end;

    Self.WaitForSignaled;

    Check(Self.ServerReceivedResponse,
          'Response wasn''t sent down a new connection');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipTCPTransport.TestSendResponsesClosedConnectionReceivedParam;
var
  LowPortListener: TIdSipTestTransportListener;
  Request:         TIdSipRequest;
  Response:        TIdSipResponse;
begin
  Assert(Assigned(GStack) and (GStack.LocalAddress <> '127.0.0.1'),
         'This test cannot work on a machine with only one network interface. '
       + 'Please make sure it''s got a NIC and that NIC has an IP');

  LowPortListener := TIdSipTestTransportListener.Create;
  try
    Self.LowPortTransport.AddTransportListener(LowPortListener);
    try
      Request := TIdSipMessage.ReadRequestFrom(LocalLoopRequest);
      try
        Self.SipClient.OnResponse  := Self.ClientOnResponseDownClosedConnection;
        Self.SipClient.Host        := Self.HighPortTransport.Address;
        Self.SipClient.Port        := Self.HighPortTransport.Port;
        Self.SipClient.ReadTimeout := 100;

        Self.SipClient.Connect;
        try
          Self.SipClient.Send(Request);
        finally
          Self.SipClient.Disconnect;
        end;

        Check(not Self.SipClient.Connected,
              'Client still connected');

        Self.CheckingResponseEvent := Self.AcknowledgeEvent;
        Response := TIdSipMessage.ReadResponseFrom(LocalLoopResponse);
        try
          Response.LastHop.Received := Self.HighPortTransport.Address;
          Response.LastHop.Port     := Self.HighPortTransport.Port;
          Response.StatusCode       := SIPOK;
          Self.LowPortTransport.Send(Response, Self.HighPortLocation);
        finally
          Response.Free;
        end;

        Self.ExceptionMessage := 'High port server didn''t receive the response';
        Self.WaitForSignaled;
        Check(not LowPortListener.ReceivedResponse,
              'Low port server received response');
      finally
        Request.Free;
      end;
    finally
      Self.LowPortTransport.RemoveTransportListener(LowPortListener);
    end;
  finally
    LowPortListener.Free;
  end;
end;

procedure TestTIdSipTCPTransport.TestSendResponsesOpenConnection;
var
  Request:   TIdSipRequest;
  SipClient: TIdSipTcpClient;
begin
  Self.CheckingRequestEvent := Self.Send200OK;

  Request := TIdSipMessage.ReadRequestFrom(LocalLoopRequest);
  try
    SipClient := Self.CreateClient;
    try
      SipClient.OnResponse  := Self.ClientOnResponse;
      SipClient.Host        := Self.LowPortTransport.Address;
      SipClient.Port        := Self.LowPortTransport.Port;
      SipClient.ReadTimeout := 1000;

      SipClient.Connect;
      try
        SipClient.Send(Request);

        Self.WaitForSignaled;

        Check(Self.ClientReceivedResponse,
              'No response received on same connection');
      finally
        SipClient.Disconnect;
      end;
    finally
      Self.DestroyClient(SipClient);
    end;
  finally
    Request.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTLSTransport                                                     *
//******************************************************************************
//* TestTIdSipTLSTransport Public methods **************************************

procedure TestTIdSipTLSTransport.SetUp;
begin
  inherited SetUp;

  Self.SetUpTls(Self.HighPortTransport);
  Self.SetUpTls(Self.LowPortTransport);

  Self.Request.RequestUri.Scheme := SipsScheme;
  Self.Response.LastHop.Transport := Self.HighPortTransport.GetTransportType;
{
  Self.Response.LastHop.Value := StringReplace(Self.Response.LastHop.AsString,
                                               'TCP',
                                               'TLS',
                                               []);
}
end;

function TestTIdSipTLSTransport.DefaultPort: Cardinal;
begin
  Result := IdPORT_SIPS;
end;

//* TestTIdSipTLSTransport Protected methods ***********************************

procedure TestTIdSipTLSTransport.SendMessage(Msg: String);
var
  Client: TIdTcpClient;
begin
  // TODO: This won't work! You need to set up the certs & such!
  Client := TIdTcpClient.Create(nil);
  try
    Client.Host := Self.HighPortTransport.Address;
    Client.Port := Self.HighPortTransport.Port;
    Client.Connect(DefaultTimeout);
    try
      Client.Write(Msg);
    finally
      Client.DisconnectSocket;
    end;
  finally
    Client.Free;
  end;
end;

function TestTIdSipTLSTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipTlsTransport;
end;

//* TestTIdSipTLSTransport Private methods *************************************

procedure TestTIdSipTLSTransport.DoOnPassword(var Password: String);
begin
  Password := CertPasswd;
end;

procedure TestTIdSipTLSTransport.SetUpTls(Transport: TIdSipTransport);
var
  TLS: TIdSipTLSTransport;
begin
  CheckEquals(TIdSipTLSTransport.ClassName,
              Transport.ClassName,
              'TestTIdSipTLSTransport.SetUpTls');

  TLS := Transport as TIdSipTLSTransport;

  TLS.OnGetPassword     := Self.DoOnPassword;
  TLS.RootCertificate   := RootCert;
  TLS.ServerCertificate := ServerCert;
  TLS.ServerKey         := ServerKey;
end;

//* TestTIdSipTLSTransport Published methods ***********************************

procedure TestTIdSipTLSTransport.TestGetTransportType;
begin
  CheckEquals(TlsTransport,
              Self.HighPortTransport.GetTransportType,
              'Transport type');
end;

procedure TestTIdSipTLSTransport.TestIsReliable;
begin
  Check(Self.HighPortTransport.IsReliable,
        'TLS transport not marked as reliable');
end;

procedure TestTIdSipTLSTransport.TestIsSecure;
begin
  Check(Self.HighPortTransport.IsSecure,
        'TLS transport not marked as secure');
end;

//******************************************************************************
//* TestTIdSipUDPTransport                                                     *
//******************************************************************************
//* TestTIdSipUDPTransport Protected methods ***********************************

procedure TestTIdSipUDPTransport.CheckServerOnPort(const Host: String;
                                                   Port: Cardinal;
                                                   const Msg: String);
var
  Binding: TIdSocketHandle;
  Server:  TIdUdpServer;
begin
  try
    Server := TIdUdpServer.Create(nil);
    try
      Binding := Server.Bindings.Add;
      Binding.IP    := Host;
      Binding.Port  := Port;
      Server.Active := true;
      try
        // Do nothing
      finally
        Server.Active := false;
      end;
    finally
      Server.Free;
    end;
    Fail('No server running on ' + Host + ': ' + IntToStr(Port) + '; ' + Msg);
  except
    on EIdCouldNotBindSocket do;
  end;
end;

procedure TestTIdSipUDPTransport.SendMessage(Msg: String);
var
  Client: TIdUdpClient;
begin
  Client := TIdUdpClient.Create(nil);
  try
    Client.Host := Self.HighPortTransport.Address;
    Client.Port := Self.HighPortTransport.Port;

    Client.Send(Msg);
  finally
    Client.Free;
  end;
end;

function TestTIdSipUDPTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipUdpTransport;
end;

//* TestTIdSipUDPTransport Private methods *************************************

procedure TestTIdSipUDPTransport.CheckMessageWithTrailingGarbage(Sender: TObject;
                                                                 R: TIdSipRequest;
                                                                 ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.Request.Body := R.Body;
  
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUDPTransport.CheckRportParamFilledIn(Sender: TObject;
                                                         R: TIdSipRequest;
                                                         ReceivedFrom: TIdSipConnectionBindings);
begin
  CheckNotEquals('',
                 R.LastHop.Params[RPortParam],
                 'Transport didn''t fill in the rport param');

  Self.NoteSourcePort(Sender, R, ReceivedFrom);
end;

procedure TestTIdSipUDPTransport.CheckLeaveNonRportRequestsUntouched(Sender: TObject;
                                                                     R: TIdSipRequest;
                                                                     ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    Check(not R.LastHop.HasRport, 'rport param added by transport');
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUDPTransport.CheckMissingContentLength(Sender: TObject;
                                                           R: TIdSipRequest;
                                                           ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    Check(R.HasHeader(ContactHeaderFull),
          'Content-Length not added');
    CheckEquals('foofoo',
                R.Body,
                'Body');
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUDPTransport.NoteSourcePort(Sender: TObject;
                                                R: TIdSipRequest;
                                                ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    Self.RPort := R.LastHop.RPort;
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

//* TestTIdSipUDPTransport Published methods ***********************************

procedure TestTIdSipUDPTransport.TestGetTransportType;
begin
  CheckEquals(UdpTransport,
              Self.HighPortTransport.GetTransportType,
              'Transport type');
end;

procedure TestTIdSipUDPTransport.TestIsReliable;
begin
  Check(not Self.HighPortTransport.IsReliable,
        'UDP transport not marked as unreliable');
end;

procedure TestTIdSipUDPTransport.TestIsSecure;
begin
  Check(not Self.HighPortTransport.IsSecure,
        'UDP transport marked as secure');
end;

procedure TestTIdSipUDPTransport.TestLeaveNonRportRequestsUntouched;
begin
  Self.CheckingRequestEvent := Self.CheckLeaveNonRportRequestsUntouched;
  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;  

  CheckEquals(0, Self.RPort, 'rport value');
end;

procedure TestTIdSipUDPTransport.TestMessageWithTrailingGarbage;
var
  Body:   String;
  Client: TIdUdpClient;
begin
  Self.CheckingRequestEvent := Self.CheckMessageWithTrailingGarbage;
  Body := 'I am a message. Hear me roar!';

  Client := TIdUdpClient.Create(nil);
  try
    Client.Host := Self.HighPortTransport.Address;
    Client.Port := Self.HighPortTransport.Port;

    Client.Send('INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0'#13#10
              + 'Via: SIP/2.0/TCP %s;branch=z9hG4bK776asdhds'#13#10
              + 'Max-Forwards: 70'#13#10
              + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.luna>'#13#10
              + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
              + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
              + 'CSeq: 314159 INVITE'#13#10
              + 'Contact: <sip:wintermute@tessier-ashpool.co.luna>'#13#10
              + 'Content-Type: text/plain'#13#10
              + 'Content-Length: ' + IntToStr(Length(Body)) + #13#10
              + #13#10
              + Body + #0#0#0#0#0);

    Self.ExceptionMessage := Self.HighPortTransport.ClassName
        + ': We rejected a message with a Content-Length that had trailing '
        + 'octets';
    Self.WaitForSignaled;
    CheckEquals(Body,
                Self.Request.Body,
                'Body of message');
  finally
    Client.Free;
  end;
end;

procedure TestTIdSipUDPTransport.TestMissingContentLength;
begin
  Self.CheckingRequestEvent := Self.CheckMissingContentLength;

  Self.SendMessage('INVITE sip:foo SIP/2.0'#13#10
                 + 'Via: SIP/2.0/127.0.0.1;branch=' + BranchMagicCookie + 'f00L'#13#10
                 + 'Call-ID: foo'#13#10
                 + 'CSeq: 1 INVITE'#13#10
                 + 'From: sip:foo'#13#10
                 + 'To: sip:foo'#13#10
                 + #13#10
                 + 'foofoo');

  Self.WaitForSignaled;
end;

procedure TestTIdSipUDPTransport.TestRportParamFilledIn;
begin
  Self.CheckingRequestEvent := Self.CheckRportParamFilledIn;
  Self.Request.LastHop.Params[RportParam] := '';
  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;

  CheckNotEquals(0, Self.RPort, 'rport value');
end;

procedure TestTIdSipUDPTransport.TestRportListening;
var
  Binding: TIdSocketHandle;
  Server:  TIdUdpServer;
begin
  // We test here that when a request is sent from port x, that we listen on
  // port x for responses, as per RFC 3581
  Self.CheckingRequestEvent := Self.NoteSourcePort;
  Self.Request.LastHop.Params[RportParam] := '';
  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;

  Server := TIdUdpServer.Create(nil);
  try
    try
      Binding := Server.Bindings.Add;
      Binding.IP   := Self.LowPortTransport.Address;
      Binding.Port := Self.RPort;

      Server.Active := true;
      Fail('Server wasn''t listening on the port from which it sent an rport'
         + ' request');
    except
      on EIdCouldNotBindSocket do;
    end;
  finally
    Server.Free;
  end;
end;
{
//******************************************************************************
//* TestTIdSipSCTPTransport                                                    *
//******************************************************************************
//* TestTIdSipSCTPTransport Protected methods **********************************

function TestTIdSipSCTPTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipSCTPTransport;
end;

//* TestTIdSipSCTPTransport Published methods **********************************

procedure TestTIdSipSCTPTransport.TestGetTransportType;
begin
  Check(sttSCTP = Self.Transport.GetTransportType, 'Transport type');
end;

procedure TestTIdSipSCTPTransport.TestIsReliable;
begin
  Check(Self.Transport.IsReliable, 'SCTP transport not marked as reliable');
end;

procedure TestTIdSipTCPTransport.TestIsSecure;
begin
  Check(not Self.Transport.IsSecure, 'SCTP transport marked as secure');
end;
}
//******************************************************************************
//* TestTIdSipMockTransport                                                    *
//******************************************************************************
//* TestTIdSipMockTransport Protected methods **********************************

function TestTIdSipMockTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipMockTransport;
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
//* TestTIdSipConnectionTableEntry
//******************************************************************************
//* TestTIdSipConnectionTableEntry Published methods ***************************

procedure TestTIdSipConnectionTableEntry.TestCreate;
var
  Conn: TIdTCPConnection;
  E:    TIdSipConnectionTableEntry;
  Req:  TIdSipRequest;
begin
  Conn := TIdTCPConnection.Create(nil);
  try
    Req := TIdSipRequest.Create;
    try
      E := TIdSipConnectionTableEntry.Create(Conn, Req);
      try
        Check(Conn = E.Connection,   'Connection not set');
        Check(Req.Equals(E.Request), 'Request not set');
      finally
        E.Free;
      end;
    finally
      Req.Free;
    end;
  finally
    Conn.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipConnectionTable                                                  *
//******************************************************************************
//* TestTIdSipConnectionTable Public methods ***********************************

procedure TestTIdSipConnectionTable.SetUp;
begin
  inherited SetUp;

  Self.Conn    := TIdTCPConnection.Create(nil);
  Self.NewConn := TIdTCPConnection.Create(nil);
  Self.NewReq  := TIdSipRequest.Create;
  Self.Req     := TIdSipRequest.Create;
  Self.Table   := TIdSipConnectionTable.Create;

  Self.Req.RequestUri.URI    := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.NewReq.RequestUri.URI := 'sip:case@fried.neurons.org';
end;

procedure TestTIdSipConnectionTable.TearDown;
begin
  Self.Table.Free;
  Self.Req.Free;
  Self.NewReq.Free;
  Self.NewConn.Free;
  Self.Conn.Free;

  inherited TearDown;
end;

//* TestTIdSipConnectionTable Published methods ********************************

procedure TestTIdSipConnectionTable.TestAddAndCount;
var
  Count: Integer;
begin
  Count := Self.Table.Count;

  Self.Table.Add(Self.Conn, Self.Req);

  CheckEquals(Count + 1, Self.Table.Count, 'No entry added');
end;

procedure TestTIdSipConnectionTable.TestConnectionFor;
begin
  Self.Table.Add(Self.Conn,    Self.Req);
  Self.Table.Add(Self.NewConn, Self.NewReq);

  Check(Self.Table.ConnectionFor(Self.Req) = Self.Conn,
        'Wrong Connection 1');
  Check(Self.Table.ConnectionFor(Self.NewReq) = Self.NewConn,
        'Wrong Connection 2');
end;

procedure TestTIdSipConnectionTable.TestConnectionForOneEntry;
begin
  Self.Table.Add(Self.Conn, Self.Req);

  Check(Self.Table.ConnectionFor(Self.Req) = Self.Conn,
        'Wrong Connection');
end;

procedure TestTIdSipConnectionTable.TestConnectionForOnEmptyList;
begin
  Check(not Assigned(Self.Table.ConnectionFor(Self.Req)), 'non-nil result');
end;

procedure TestTIdSipConnectionTable.TestConnectionForOnNoEntry;
begin
  Self.Table.Add(Self.Conn, Self.Req);

  Check(not Assigned(Self.Table.ConnectionFor(Self.NewReq)), 'non-nil result');
end;

procedure TestTIdSipConnectionTable.TestConnectionForResponse;
var
  Response: TIdSipResponse;
begin
  Self.Req.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TCP localhost;' + BranchParam + '=' + BranchMagicCookie + 'f00';
  Self.Req.Method := MethodOptions;

  Self.Table.Add(Self.Conn, Self.Req);

  Response := TIdSipResponse.Create;
  try
    Response.AddHeader(Self.Req.LastHop);
    Response.CSeq.Method := MethodOptions;

    Check(Self.Conn = Self.Table.ConnectionFor(Response), 'Wrong connection');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipConnectionTable.TestRemove;
var
  Count: Integer;
begin
  Count := Self.Table.Count;

  Self.Table.Add(Conn, Req);
  Self.Table.Remove(Conn);
  CheckEquals(Count, Self.Table.Count, 'No entry removed');
end;

procedure TestTIdSipConnectionTable.TestRemoveOnEmptyList;
begin
  Self.Table.Remove(Self.Conn);
end;

procedure TestTIdSipConnectionTable.TestRemoveOnNonEmptyList;
var
  Count: Integer;
begin
  Self.Table.Add(Self.Conn, Self.Req);
  Self.Table.Add(NewConn, NewReq);

  Count := Self.Table.Count;

  Self.Table.Remove(NewConn);

  CheckEquals(Count - 1, Self.Table.Count, 'Nothing was removed');
  Check(Self.Table.ConnectionFor(Self.Req) = Self.Conn, 'Wrong entry removed (ConnectionFor)');
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

  Self.Exception := EUnknownTransport.Create('');

  Self.Method := TIdSipTransportExceptionMethod.Create;
  Self.Method.Exception := Self.Exception;
  Self.Method.Reason    := 'Bar';
end;

procedure TestTIdSipTransportExceptionMethod.TearDown;
begin
  Self.Method.Free;
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

  Self.Method := TIdSipTransportReceiveRequestMethod.Create;
  Self.Method.Receiver := Self.Transport;
  Self.Method.Request  := Self.Request;
end;

procedure TestTIdSipTransportReceiveRequestMethod.TearDown;
begin
  Self.Method.Free;
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

  Self.Method := TIdSipTransportReceiveResponseMethod.Create;
  Self.Method.Receiver := Self.Transport;
  Self.Method.Response := Self.Response;
end;

procedure TestTIdSipTransportReceiveResponseMethod.TearDown;
begin
  Self.Method.Free;
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
end;

procedure TestTIdSipTransportRejectedMessageMethod.TearDown;
begin
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
  finally
    Listener.Free;
  end;
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
    Check(Self.Method.Sender = Listener.SenderParam,
          'Sender param');
    Check(Self.Method.Request = Listener.RequestParam,
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
    Check(Self.Method.Sender = Listener.SenderParam,
          'Sender param');
    Check(Self.Method.Response = Listener.ResponseParam,
          'Response param');
  finally
    Listener.Free;
  end;
end;

initialization
  ServerThatInstantiatesGStack := TIdTCPServer.Create(nil);
  RegisterTest('IdSipTransport', Suite);
finalization
  ServerThatInstantiatesGStack.Free;
end.
