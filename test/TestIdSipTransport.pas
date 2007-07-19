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
  Classes, IdSipLocation, IdSipMessage, IdSipMockTransport, IdSipTransport,
  IdSipTcpTransport, IdTcpConnection, IdTcpServer, IdTimerQueue, SyncObjs,
  SysUtils, TestFramework, TestFrameworkEx, TestFrameworkSip,
  TestFrameworkSipTransport;

type
  TIdSipTransportSubclass = class(TIdSipTcpTransport)
  public
    procedure NotifyOfReceivedRequest(Request: TIdSipRequest);
    procedure NotifyOfReceivedResponse(Response: TIdSipResponse);
    procedure NotifyOfSentRequest(Request: TIdSipRequest;
                                  Binding: TIdSipConnectionBindings);
    procedure NotifyOfSentResponse(Response: TIdSipResponse;
                                   Binding: TIdSipConnectionBindings);
  end;

  TestTIdSipTransportEventNotifications = class(TTestCaseSip,
                                                IIdSipTransportListener,
                                                IIdSipTransportSendingListener)
  private
    Binding:          TIdSipConnectionBindings;
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
                               Source: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdSipConnectionBindings);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String;
                                Source: TIdSipConnectionBindings);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Binding: TIdSipConnectionBindings);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Binding: TIdSipConnectionBindings);
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
    procedure TestTransportFor;
    procedure TestTransportTypeFor;
    procedure TestUnregisterTransport;
    procedure TestUriSchemeFor;
  end;

  TestTIdSipMockTransport = class(TestTIdSipTransport)
  private
    MockTransport: TIdSipMockTransport;
    RequestOne:    TIdSipRequest;
    RequestTwo:    TIdSipRequest;
    RequestThree:  TIdSipRequest;
    RequestFour:   TIdSipRequest;
    ResponseOne:   TIdSipResponse;
    ResponseTwo:   TIdSipResponse;
    ResponseThree: TIdSipResponse;
    ResponseFour:  TIdSipResponse;
  protected
    procedure CheckServerOnPort(const Host: String;
                                Port: Cardinal;
                                const Msg: String); override;
    procedure SendMessage(Msg: String); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
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

  TTransportSendingMethodTestCase = class(TTransportMethodTestCase)
  protected
    Binding: TIdSipConnectionBindings;
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
  IdException, IdGlobal, IdSimpleParser, IdSipSCTPTransport, IdSipTlsTransport,
  IdSipUdpTransport, IdSSLOpenSSL, IdStack, IdSystem, IdTcpClient, IdUdpClient,
  IdUDPServer, TestMessages;

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
                                                      Binding: TIdSipConnectionBindings);
begin
  inherited NotifyOfSentRequest(Request, Binding);
end;

procedure TIdSipTransportSubclass.NotifyOfSentResponse(Response: TIdSipResponse;
                                                       Binding: TIdSipConnectionBindings);
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

  Self.Binding := TIdSipConnectionBindings.Create;
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
                                                                  const Reason: String;
                                                                  Source: TIdSipConnectionBindings);
begin
end;

procedure TestTIdSipTransportEventNotifications.OnSendRequest(Request: TIdSipRequest;
                                                              Sender: TIdSipTransport;
                                                              Binding: TIdSipConnectionBindings);
begin
  Self.SentRequest := true;
  Check(Self.Request = Request,     'Request not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnSendResponse(Response: TIdSipResponse;
                                                               Sender: TIdSipTransport;
                                                               Binding: TIdSipConnectionBindings);
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

procedure TestTransportRegistry.TestTransportFor;
var
  ID1, ID2: String;
  T1, T2:   TIdSipTransport;
begin
  Check(nil = TIdSipTransportRegistry.TransportFor('non-existent transport ID'),
        'TransportFor returned a transport for a non-existent transport ID');

  T1 := TIdSipMockTcpTransport.Create;
  try
    T2 := TIdSipMockTcpTransport.Create;
    try
      ID1 := T1.ID;
      ID2 := T2.ID;

      Check(T1 = TIdSipTransportRegistry.TransportFor(ID1),
            'TransportFor returned an unexpected transport');
    finally
      T2.Free;
    end;
  finally
    T1.Free;
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

procedure TestTransportRegistry.TestUnregisterTransport;
var
  ID:            String;
  MockTransport: TIdSipTransport;
begin
  MockTransport := TIdSipMockTcpTransport.Create;
  try
    ID := MockTransport.ID;
    TIdSipTransportRegistry.UnregisterTransport(ID);
    Check(nil = TIdSipTransportRegistry.TransportFor(ID),
          'Registry didn''t unregister transport');

    try
      TIdSipTransportRegistry.UnregisterTransport(ID);
    except
      on E: Exception do
        Fail('Double-unregister blew up: ' + E.ClassName + ': ' + E.Message);
    end;
  finally
    MockTransport.Free;
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

  Self.RequestOne := TIdSipRequest.ReadRequestFrom(BasicRequest);
  Self.RequestOne.LastHop.Transport := Self.LowPortTransport.GetTransportType;

  Self.RequestTwo := TIdSipRequest.ReadRequestFrom(BasicRequest);
  Self.RequestTwo.Method      := MethodRegister;
  Self.RequestTwo.CSeq.Method := MethodRegister;
  Self.RequestTwo.LastHop.Transport := Self.LowPortTransport.GetTransportType;

  Self.RequestThree := TIdSipRequest.ReadRequestFrom(BasicRequest);
  Self.RequestThree.Method      := MethodOptions;
  Self.RequestThree.CSeq.Method := MethodOptions;
  Self.RequestThree.LastHop.Transport := Self.LowPortTransport.GetTransportType;

  Self.RequestFour := TIdSipRequest.ReadRequestFrom(BasicRequest);
  Self.RequestFour.Method      := MethodSubscribe;
  Self.RequestFour.CSeq.Method := MethodSubscribe;
  Self.RequestFour.LastHop.Transport := Self.LowPortTransport.GetTransportType;

  Self.ResponseOne := TIdSipResponse.ReadResponseFrom(BasicResponse);
  Self.ResponseOne.StatusCode := SIPTrying;

  Self.ResponseTwo := TIdSipResponse.ReadResponseFrom(BasicResponse);
  Self.ResponseTwo.StatusCode := SIPOK;

  Self.ResponseThree := TIdSipResponse.ReadResponseFrom(BasicResponse);
  Self.ResponseThree.StatusCode := SIPMultipleChoices;

  Self.ResponseFour := TIdSipResponse.ReadResponseFrom(BasicResponse);
  Self.ResponseFour.StatusCode := SIPBadRequest;

  Self.MockTransport := Self.LowPortTransport as TIdSipMockTransport;
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
  Binding: TIdSipConnectionBindings;
  M:       TIdSipMessage;
begin
  M := TIdSipMessage.ReadMessageFrom(Msg);
  try
    Binding := TIdSipConnectionBindings.Create;
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

//* TestTIdSipMockTransport Published methods **********************************

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
  Self.Method.Source := TIdSipConnectionBindings.Create;
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

  Self.Binding := TIdSipConnectionBindings.Create;
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
