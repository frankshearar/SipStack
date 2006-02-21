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
  Classes, IdSipLocator, IdSipMessage, IdSipTransport, IdSipTcpTransport,
  IdSocketHandle, IdTcpConnection, IdTcpServer, IdTimerQueue, SyncObjs,
  SysUtils, TestFramework, TestFrameworkEx, TestFrameworkSip,
  TestFrameworkSipTransport;

type
  TIdSipTransportSubclass = class(TIdSipTcpTransport)
  public
    procedure NotifyOfReceivedRequest(Request: TIdSipRequest);
    procedure NotifyOfReceivedResponse(Response: TIdSipResponse);
    procedure NotifyOfSentRequest(Request: TIdSipRequest;
                                  Dest: TIdSipLocation);
    procedure NotifyOfSentResponse(Response: TIdSipResponse;
                                   Dest: TIdSipLocation); 
  end;

  TestTIdSipTransportEventNotifications = class(TTestCaseSip,
                                                IIdSipTransportListener,
                                                IIdSipTransportSendingListener)
  private
    Destination:      TIdSipLocation;
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
                               Receiver: TIdSipTransport;
                               Source: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdSipConnectionBindings);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Destination: TIdSipLocation);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Destination: TIdSipLocation);
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
    Source:  TIdSipConnectionBindings;
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
    Source:   TIdSipConnectionBindings;
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
    Destination: TIdSipLocation;
    Method:      TIdSipTransportSendingRequestMethod;
    Request:     TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransportSendingResponseMethod = class(TTransportMethodTestCase)
  private
    Destination: TIdSipLocation;
    Method:      TIdSipTransportSendingResponseMethod;
    Response:    TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

implementation

uses
  IdException, IdGlobal, IdSimpleParser, IdSipConsts, IdSipMockTransport,
  IdSipSCTPTransport, IdSipTlsTransport, IdSipUdpTransport, IdSSLOpenSSL,
  IdStack, IdSystem, IdTcpClient, IdUdpClient, IdUDPServer, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransport unit tests');
  Result.AddTest(TestTIdSipTransportEventNotifications.Suite);
  Result.AddTest(TestTransportRegistry.Suite);
//  Result.AddTest(TestTIdSipMockTransport.Suite);
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
  FakeBinding: TIdSipConnectionBindings;
begin
  FakeBinding := TIdSipConnectionBindings.Create;
  try
    inherited NotifyOfReceivedRequest(Request, FakeBinding);
  finally
    FakeBinding.Free;
  end;
end;

procedure TIdSipTransportSubclass.NotifyOfReceivedResponse(Response: TIdSipResponse);
var
  FakeBinding: TIdSipConnectionBindings;
begin
  FakeBinding := TIdSipConnectionBindings.Create;
  try
    inherited NotifyOfReceivedResponse(Response, FakeBinding);
  finally
    FakeBinding.Free;
  end;
end;

procedure TIdSipTransportSubclass.NotifyOfSentRequest(Request: TIdSipRequest;
                                                      Dest: TIdSipLocation);
begin
  inherited NotifyOfSentRequest(Request, Dest);
end;

procedure TIdSipTransportSubclass.NotifyOfSentResponse(Response: TIdSipResponse;
                                                       Dest: TIdSipLocation);
begin
  inherited NotifyOfSentResponse(Response, Dest);
end;

//******************************************************************************
//* TestTIdSipTransportEventNotifications                                      *
//******************************************************************************
//* TestTIdSipTransportEventNotifications Public methods ***********************

procedure TestTIdSipTransportEventNotifications.SetUp;
begin
  inherited SetUp;

  Self.Destination      := TIdSipLocation.Create('TCP', '127.0.0.1', 5060);
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
  Self.Destination.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportEventNotifications Private methods **********************

procedure TestTIdSipTransportEventNotifications.OnException(E: Exception;
                                                            const Reason: String);
begin
end;

procedure TestTIdSipTransportEventNotifications.OnReceiveRequest(Request: TIdSipRequest;
                                                                 Receiver: TIdSipTransport;
                                                                 Source: TIdSipConnectionBindings);
begin
  Self.ReceivedRequest := true;
  Check(Self.Request = Request,     'Request not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnReceiveResponse(Response: TIdSipResponse;
                                                                  Receiver: TIdSipTransport;
                                                                  Source: TIdSipConnectionBindings);
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
                                                              Sender: TIdSipTransport;
                                                              Destination: TIdSipLocation);
begin
  Self.SentRequest := true;
  Check(Self.Request = Request,     'Request not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnSendResponse(Response: TIdSipResponse;
                                                               Sender: TIdSipTransport;
                                                               Destination: TIdSipLocation);
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

  Self.Transport.NotifyOfSentRequest(Self.Request, Self.Destination);

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
                                       Self.Destination);

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

    Self.Transport.NotifyOfSentResponse(Self.Response,
                                        Self.Destination);

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

  Self.Transport.NotifyOfReceivedRequest(Self.Request);

  Check(not Self.ReceivedRequest, 'Listener wasn''t removed');
end;

procedure TestTIdSipTransportEventNotifications.TestRemoveTransportSendingListener;
begin
  Self.Transport.AddTransportSendingListener(Self);
  Self.Transport.RemoveTransportSendingListener(Self);

  Self.Transport.NotifyOfSentRequest(Self.Request,
                                     Self.Destination);

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
  Self.Source := TIdSipConnectionBindings.Create;

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
  Self.Source   := TIdSipConnectionBindings.Create;

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

  Self.Destination := TIdSipLocation.Create('TCP', '127.0.0.1', 5060);
  Self.Request := TIdSipRequest.Create;

  Self.Method := TIdSipTransportSendingRequestMethod.Create;
  Self.Method.Destination := Self.Destination;
  Self.Method.Request     := Self.Request;
  Self.Method.Sender      := Self.Transport;
end;

procedure TestTIdSipTransportSendingRequestMethod.TearDown;
begin
  Self.Method.Free;
  Self.Request.Free;
  Self.Destination.Free;

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
    Check(Self.Method.Destination = Listener.DestinationParam,
          'Destination param');
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

  Self.Destination := TIdSipLocation.Create('TCP', '127.0.0.1', 5060);
  Self.Response := TIdSipResponse.Create;

  Self.Method := TIdSipTransportSendingResponseMethod.Create;
  Self.Method.Destination := Self.Destination;
  Self.Method.Response    := Self.Response;
  Self.Method.Sender      := Self.Transport;
end;

procedure TestTIdSipTransportSendingResponseMethod.TearDown;
begin
  Self.Method.Free;
  Self.Response.Free;
  Self.Destination.Free;

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
    Check(Self.Method.Destination = Listener.DestinationParam,
          'Destination param');
    Check(Self.Method.Sender = Listener.SenderParam,
          'Sender param');
    Check(Self.Method.Response = Listener.ResponseParam,
          'Response param');
  finally
    Listener.Free;
  end;
end;

initialization
  RegisterTest('Transport layer', Suite);
end.
