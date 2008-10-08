{
  (c) 2006 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestFrameworkSipTransport;

interface

uses
  IdConnectionBindings, IdInterfacedObject, IdRoutingTable, IdSipLocation,
  IdSipMessage, IdSipTransport, IdTimerQueue, SyncObjs, SysUtils,
  TestFrameworkEx, TestFrameworkSip;

type
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
                                 TestCase: TestTIdSipTransport;
                                 LogName: String);

  public
    constructor Create(TransportType: TIdSipTransportClass;
                       TestCase: TestTIdSipTransport;
                       FinishedEvent: TEvent;
                       LogName: String); reintroduce;
    destructor  Destroy; override;

    property HighPortTransport: TIdSipTransport read fHighPortTransport;
    property LowPortTransport:  TIdSipTransport read fLowPortTransport;
  end;

  // I use two transports - LowPortTransport and HighPortTransport - to test
  // that the transport works correctly. LowPortTransport sends
  // HighPortTransport messages, and I listen to HighPortTransport. You may
  // test that the receipt of messages matches your expectations by assigning
  // a procedure to Checking(Request|Response)Event.
  TestTIdSipTransport = class(TThreadingTestCase,
                              IIdSipTransportListener,
                              IIdSipTransportSendingListener)
  private
    LastSentResponse: TIdSipResponse;
  protected
    CheckingRequestEvent:   TIdSipRequestEvent;
    CheckingResponseEvent:  TIdSipResponseEvent;
    EmptyListEvent:         TEvent;
    FinishedTimer:          TEvent;
    HighPortLocation:       TIdSipLocation;
    Lock:                   TCriticalSection;
    LogName:                String;
    LowPortLocation:        TIdSipLocation;
    ReceivedRequest:        Boolean;
    ReceivedResponse:       Boolean;
    ReceivingBinding:       TIdConnectionBindings;
    RecvdRequest:           TIdSipRequest;
    RejectedMessage:        Boolean;
    RejectedMessageEvent:   TEvent;
    RejectedMessageReason:  String;
    Request:                TIdSipRequest;
    Response:               TIdSipResponse;
    RequestSendingBinding:  TIdConnectionBindings;
    ResponseSendingBinding: TIdConnectionBindings;
    SendEvent:              TEvent;
    SentBy:                 String;
    TestRef:                Cardinal;
    Timer:                  TTransportTestTimerQueue;
    WrongServer:            Boolean;

    procedure CheckBinding(Received: TIdConnectionBindings);
    procedure CheckCanReceiveRequest(Sender: TObject;
                                     R: TIdSipRequest;
                                     ReceivedFrom: TIdConnectionBindings);
    procedure CheckCanReceiveResponse(Sender: TObject;
                                      R: TIdSipResponse;
                                      ReceivedFrom: TIdConnectionBindings);
    procedure CheckDiscardResponseWithUnknownSentBy(Sender: TObject;
                                                    R: TIdSipResponse;
                                                    ReceivedFrom: TIdConnectionBindings);
    procedure CheckForBadRequest(Sender: TObject;
                                 R: TIdSipResponse;
                                 ReceivedFrom: TIdConnectionBindings);
    procedure CheckForTrying(Sender: TObject;
                             R: TIdSipResponse;
                             ReceivedFrom: TIdConnectionBindings);
    procedure CheckForSIPVersionNotSupported(Sender: TObject;
                                             R: TIdSipResponse);
    procedure CheckHasPort(Transport: TIdSipTransport;
                           IPAddress: String;
                           Port: Cardinal;
                           Msg: String);
    procedure CheckReceivedParamDifferentIPv4SentBy(Sender: TObject;
                                                    Request: TIdSipRequest;
                                                    ReceivedFrom: TIdConnectionBindings);
    procedure CheckReceivedParamFQDNSentBy(Sender: TObject;
                                           Request: TIdSipRequest;
                                           ReceivedFrom: TIdConnectionBindings);
    procedure CheckReceivedParamIPv4SentBy(Sender: TObject;
                                           Request: TIdSipRequest;
                                           ReceivedFrom: TIdConnectionBindings);
    procedure CheckResponse(Response: TIdSipResponse;
                            ExpectedStatusCode: Cardinal);
    procedure CheckSendBindingSet(Binding: TIdConnectionBindings); virtual;
    procedure CheckSendRequestFromNonStandardPort(Sender: TObject;
                                                  R: TIdSipRequest;
                                                  ReceivedFrom: TIdConnectionBindings);
    procedure CheckSendRequestAutoRewriteVia(Sender: TObject;
                                     R: TIdSipRequest;
                                     ReceivedFrom: TIdConnectionBindings);
    procedure CheckSendRequestSpecifiedVia(Sender: TObject;
                                           R: TIdSipRequest;
                                           ReceivedFrom: TIdConnectionBindings);
    procedure CheckSendResponseFromNonStandardPort(Sender: TObject;
                                                   R: TIdSipResponse;
                                                   ReceivedFrom: TIdConnectionBindings);
    procedure CheckServerNotOnPort(const Host: String;
                                   Port: Cardinal;
                                   const Msg: String);
    procedure CheckServerOnPort(const Host: String;
                                Port: Cardinal;
                                const Msg: String); virtual;
    procedure CheckUseRport(Sender: TObject;
                            R: TIdSipRequest;
                            ReceivedFrom: TIdConnectionBindings);
    function  CopyFirstLocation(Transport: TIdSipTransport): TIdSipLocation;
    function  HighPortTransport: TIdSipTransport;
    procedure LogException(E: Exception; Method: String);
    function  LowPortTransport: TIdSipTransport;
    procedure OnException(FailedMessage: TIdSipMessage;
                          E: Exception;
                          const Reason: String);
    procedure OnEmpty(Sender: TIdTimerQueue);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport;
                               Source: TIdConnectionBindings); virtual;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdConnectionBindings); virtual;
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String;
                                Source: TIdConnectionBindings);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Binding: TIdConnectionBindings); virtual;
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Binding: TIdConnectionBindings); virtual;
    procedure ReturnResponse(Sender: TObject;
                             R: TIdSipRequest;
                             ReceivedFrom: TIdConnectionBindings);
    procedure SendFromLowTransport(Msg: String); virtual;
    procedure SendMessage(Msg: String); virtual;
    procedure SendOkResponse(Transport: TIdSipTransport);
    procedure SetEvent(Sender: TObject;
                       R: TIdSipResponse;
                       ReceivedFrom: TIdConnectionBindings);
    function  TransportType: TIdSipTransportClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    function  DefaultPort: Cardinal; virtual;
  published
    procedure TestAddBinding;
    procedure TestAddBindingDoesntStartStoppedTransport;
    procedure TestAddBindingRestartsStartedTransport;
    procedure TestBindingCount;
    procedure TestCanReceiveRequest;
    procedure TestCanReceiveResponse;
    procedure TestCanReceiveUnsolicitedResponse;
    procedure TestClearBindings;
    procedure TestClearBindingsDoesntStartStoppedTransport;
    procedure TestClearBindingRestartsStartedTransport;
    procedure TestDiscardMalformedMessage;
    procedure TestDiscardRequestWithInconsistentTransport;
    procedure TestDiscardResponseWithInconsistentTransport;
    procedure TestDiscardResponseWithUnknownSentBy;
    procedure TestDontDiscardUnknownSipVersion;
    procedure TestHasBinding;
    procedure TestInstantiationRegistersTransport;
    procedure TestIsNull; virtual;
    procedure TestIsRunning;
    procedure TestLocalBindingsDoesntClearParameter;
    procedure TestLocalBindingsMultipleBindings;
    procedure TestLocalBindingsSingleBinding;
    procedure TestMalformedCallID;
    procedure TestReceivedParamDifferentIPv4SentBy;
    procedure TestReceivedParamFQDNSentBy;
    procedure TestReceivedParamIPv4SentBy;
    procedure TestReceiveRequestShowsCorrectBinding;
    procedure TestReceiveResponseFromMappedRoute;
    procedure TestReceiveResponseOnLocallyInitiatedConnectionShowsCorrectBinding;
    procedure TestReceiveResponseShowsCorrectBinding;
    procedure TestRemoveBinding;
    procedure TestRemoveBindingDoesntStartStoppedTransport;
    procedure TestRemoveBindingNoSuchBinding;
    procedure TestRemoveBindingRestartsStartedTransport;
    procedure TestSendRequest;
    procedure TestSendRequestAutoRewriteVia;
    procedure TestSendRequestSpecifiedVia;
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
    procedure TestTortureTest41;
    procedure TestUseRport;
  end;

  TIdSipTestTransportListener = class(TIdSipTestActionListener,
                                      IIdSipTransportListener)
  private
    fException:          Boolean;
    fExceptionParam:     Exception;
    fFailedMessageParam: TIdSipMessage;
    fMsgParam:           String;
    fReasonParam:        String;
    fReceivedRequest:    Boolean;
    fReceivedResponse:   Boolean;
    fReceiverParam:      TIdSipTransport;
    fRejectedMessage:    Boolean;
    fRequestParam:       TIdSipRequest;
    fResponseParam:      TIdSipResponse;
    fSourceParam:        TIdConnectionBindings;

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
  public
    constructor Create; override;

    property Exception:          Boolean                  read fException;
    property ExceptionParam:     Exception                read fExceptionParam;
    property FailedMessageParam: TIdSipMessage            read fFailedMessageParam;
    property MsgParam:           String                   read fMsgParam;
    property ReasonParam:        String                   read fReasonParam;
    property ReceivedRequest:    Boolean                  read fReceivedRequest;
    property ReceivedResponse:   Boolean                  read fReceivedResponse;
    property ReceiverParam:      TIdSipTransport          read fReceiverParam;
    property RejectedMessage:    Boolean                  read fRejectedMessage;
    property RequestParam:       TIdSipRequest            read fRequestParam;
    property ResponseParam:      TIdSipResponse           read fResponseParam;
    property SourceParam:        TIdConnectionBindings read fSourceParam;
  end;

  TIdSipTestTransportSendingListener = class(TIdSipTestActionListener,
                                             IIdSipTransportSendingListener)
  private
    fBindingParam:  TIdConnectionBindings;
    fRequestParam:  TIdSipRequest;
    fResponseParam: TIdSipResponse;
    fSenderParam:   TIdSipTransport;
    fSentRequest:   Boolean;
    fSentResponse:  Boolean;

    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Binding: TIdConnectionBindings);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Binding: TIdConnectionBindings);
  public
    constructor Create; override;
    destructor  Destroy; override;

    property BindingParam:  TIdConnectionBindings  read fBindingParam;
    property RequestParam:  TIdSipRequest          read fRequestParam;
    property ResponseParam: TIdSipResponse         read fResponseParam;
    property SenderParam:   TIdSipTransport        read fSenderParam;
    property SentRequest:   Boolean                read fSentRequest;
    property SentResponse:  Boolean                read fSentResponse;
  end;

  TConnectionListener = class(TIdInterfacedObject, IIdSipConnectionListener)
  private
    fConnectionClosed: Boolean;
    fConnectionOpened: Boolean;
    fConnectionParam:  TIdConnectionBindings;
    fTransportParam:   TIdSipTransport;

    procedure OnConnection(Transport: TIdSipTransport;
                           Connection: TIdConnectionBindings);
    procedure OnDisconnection(Transport: TIdSipTransport;
                              Connection: TIdConnectionBindings);
  public
    constructor Create; override;
    destructor  Destroy; override;

    property ConnectionClosed: Boolean read fConnectionClosed;
    property ConnectionOpened: Boolean read fConnectionOpened;
    property ConnectionParam:  TIdConnectionBindings read fConnectionParam;
    property TransportParam:   TIdSipTransport read fTransportParam;
  end;

implementation

uses
  IdRegisteredObject, IdSimpleParser, IdStack, IdTcpServer, PluggableLogging,
  TestFramework, TestMessages;

var
  ServerThatInstantiatesGStack: TIdTcpServer;

//******************************************************************************
//* TTransportTestTimerQueue                                                   *
//******************************************************************************
//* TTransportTestTimerQueue Public methods ************************************

constructor TTransportTestTimerQueue.Create(TransportType: TIdSipTransportClass;
                                            TestCase: TestTIdSipTransport;
                                            FinishedEvent: TEvent;
                                            LogName: String);
begin
  inherited Create(false);

  Self.FinishedEvent := FinishedEvent;

  Self.fHighPortTransport := TransportType.Create;
  Self.ConfigureTransport(Self.HighPortTransport,
                          'localhost',
                          '127.0.0.1',
                          TestCase.DefaultPort + 10000,
                          TestCase,
                          LogName);

  Self.fLowPortTransport  := TransportType.Create;
  Self.ConfigureTransport(Self.LowPortTransport,
                          'localhost',
                          '127.0.0.1',
                          TestCase.DefaultPort,
                          TestCase,
                          LogName);

  Self.HighPortTransport.Start;
  Self.LowPortTransport.Start;
end;

destructor TTransportTestTimerQueue.Destroy;
begin
  Self.LowPortTransport.Stop;
  Self.HighPortTransport.Stop;
  Self.LowPortTransport.RoutingTable.Free;
  Self.LowPortTransport.Free;
  Self.HighPortTransport.RoutingTable.Free;
  Self.HighPortTransport.Free;

  Self.FinishedEvent.SetEvent;

  inherited Destroy;
end;

//* TTransportTestTimerQueue Private methods ***********************************

procedure TTransportTestTimerQueue.ConfigureTransport(Transport: TIdSipTransport;
                                                      const HostName: String;
                                                      const Address: String;
                                                      Port: Cardinal;
                                                      TestCase: TestTIdSipTransport;
                                                      LogName: String);
begin
  Transport.AddTransportListener(TestCase);
  Transport.AddTransportSendingListener(TestCase);

  Transport.RoutingTable := TIdMockRoutingTable.Create;
  Transport.Timeout      := TestCase.DefaultTimeout div 10;
  Transport.Timer        := Self;
  Transport.HostName     := HostName;

  Transport.SetFirstBinding(Address, Port);
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

  Self.ExceptionMessage       := Self.TransportType.ClassName + ': ' + Self.ExceptionMessage;
  Self.EmptyListEvent         := TSimpleEvent.Create;
  Self.Lock                   := TCriticalSection.Create;
  Self.LastSentResponse       := TIdSipResponse.Create;
  Self.FinishedTimer          := TSimpleEvent.Create;
  Self.ReceivingBinding       := TIdConnectionBindings.Create;
  Self.RecvdRequest           := TIdSipRequest.Create;
  Self.RejectedMessageEvent   := TSimpleEvent.Create;
  Self.Request                := TIdSipTestResources.CreateLocalLoopRequest;
  Self.RequestSendingBinding  := TIdConnectionBindings.Create;
  Self.Response               := TIdSipTestResources.CreateLocalLoopResponse;
  Self.ResponseSendingBinding := TIdConnectionBindings.Create;
  Self.SendEvent              := TSimpleEvent.Create;

  Self.LogName := Self.FTestName + 'Log';
  Self.TestRef := $decafbad;
  TIdObjectRegistry.SetLogger(Self.TestRef);

  TIdSipTransportRegistry.RegisterTransportType(Self.TransportType.GetTransportType,
                                                Self.TransportType);

  Self.Timer := TTransportTestTimerQueue.Create(Self.TransportType, Self, Self.FinishedTimer, LogName);
  Check(Self.Timer <> nil,
        'Timer didn''t instantiate: the previous test likely failed without '
      + 'killing the transports');
  Self.Timer.OnEmpty := Self.OnEmpty;

  Check(Self.HighPortTransport <> nil,
        'Something went wrong creating the TTransportTestTimerQueue');

  Self.HighPortLocation := Self.CopyFirstLocation(Self.HighPortTransport);
  Self.LowPortLocation  :=  Self.CopyFirstLocation(Self.LowPortTransport);

  Self.Request.LastHop.SentBy    := Self.LowPortLocation.IPAddress;
  Self.Request.LastHop.Transport := Self.LowPortLocation.Transport;
  Self.Request.RequestUri.Host   := Self.HighPortTransport.HostName;
  Self.Request.RequestUri.Port   := Self.HighPortLocation.Port;

  Self.Response.LastHop.Transport := Self.HighPortLocation.Transport;
  Self.Response.LastHop.Port      := Self.HighPortLocation.Port;

  Self.ReceivedRequest       := false;
  Self.ReceivedResponse      := false;
  Self.RejectedMessage       := false;
  Self.RejectedMessageReason := '';
  Self.SentBy                := '';
  Self.WrongServer           := false;
end;

procedure TestTIdSipTransport.TearDown;
begin
  Self.DefaultTimeout := Self.DefaultTimeout * 3 div 2;

  Self.Timer.Terminate;
  try
//    Self.WaitForSignaled(Self.EmptyListEvent,
//                         'Waiting for timer to finish processing its events (' + Self.ClassName + ')');
    Self.WaitForSignaled(Self.FinishedTimer,
                         'Waiting for timer to finish destroying its transports (' + Self.ClassName + ')');
  finally
    // We want to TRY wait for the Timer to finish. If it doesn't, though, we
    // must still try clean up the test, especially when we touch global
    // structures like TIdSipTransportRegistry.
    Self.Timer := nil;

    Self.Lock.Acquire;
    try
      FreeAndNil(Self.LastSentResponse);
      FreeAndNil(Self.SendEvent);
      FreeAndNil(Self.RejectedMessageEvent);
      FreeAndNil(Self.EmptyListEvent);
    finally
      Self.Lock.Release;
    end;
    Self.Lock.Free;

    Self.HighPortLocation.Free;
    Self.LowPortLocation.Free;

    Self.ResponseSendingBinding.Free;
    Self.Response.Free;
    Self.RequestSendingBinding.Free;
    Self.Request.Free;
    Self.RecvdRequest.Free;
    Self.ReceivingBinding.Free;
    Self.FinishedTimer.Free;

    TIdSipTransportRegistry.UnregisterTransportType(Self.TransportType.GetTransportType);

    inherited TearDown;
  end;
end;

function TestTIdSipTransport.DefaultPort: Cardinal;
begin
  Result := DefaultSipPort;
end;

//* TestTIdSipTransport Protected methods **************************************

procedure TestTIdSipTransport.CheckBinding(Received: TIdConnectionBindings);
begin
  CheckEquals(Self.HighPortLocation.IPAddress, Received.LocalIP,   'LocalIP incorrect');
  CheckEquals(Self.HighPortLocation.Port,      Received.LocalPort, 'LocalPort incorrect');
  CheckEquals(Self.HighPortLocation.Transport, Received.Transport, 'Transport incorrect');

  CheckNotEquals('', Received.PeerIP, 'PeerIP not filled in');
  CheckNotEquals(0,  Received.PeerPort, 'PeerPort not filled in');
end;

procedure TestTIdSipTransport.CheckCanReceiveRequest(Sender: TObject;
                                                     R: TIdSipRequest;
                                                     ReceivedFrom: TIdConnectionBindings);
begin
  try
    Self.ReceivedRequest := true;
    Self.ReceivingBinding.Assign(ReceivedFrom);
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
                                                      ReceivedFrom: TIdConnectionBindings);
begin
  try
    Self.ReceivedResponse := true;
    Self.ReceivingBinding.Assign(ReceivedFrom);

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
                                                                    ReceivedFrom: TIdConnectionBindings);
begin
  Self.ReceivedResponse := true;
end;

procedure TestTIdSipTransport.CheckForBadRequest(Sender: TObject;
                                                 R: TIdSipResponse;
                                                 ReceivedFrom: TIdConnectionBindings);
begin
  Self.CheckResponse(R, SIPBadRequest);
end;

procedure TestTIdSipTransport.CheckForTrying(Sender: TObject;
                                             R: TIdSipResponse;
                                             ReceivedFrom: TIdConnectionBindings);
begin
  Self.CheckResponse(R, SIPTrying);
end;

procedure TestTIdSipTransport.CheckForSIPVersionNotSupported(Sender: TObject;
                                                             R: TIdSipResponse);
begin
  Self.CheckResponse(R, SIPSIPVersionNotSupported);
end;

procedure TestTIdSipTransport.CheckHasPort(Transport: TIdSipTransport;
                                           IPAddress: String;
                                           Port: Cardinal;
                                           Msg: String);
var
  Bindings: TIdSipLocations;
  Found: Boolean;
  I:     Integer;
begin
  Found := false;

  Bindings := TIdSipLocations.Create;
  try
    Transport.LocalBindings(Bindings);

    for I := 0 to Bindings.Count - 1 do begin
      if (Bindings[I].IPAddress = IPAddress) and (Bindings[I].Port = Port) then begin
        Found := true;
        Break;
      end;
    end;
  finally
    Bindings.Free;
  end;

  Check(Found, Msg);
end;

procedure TestTIdSipTransport.CheckReceivedParamDifferentIPv4SentBy(Sender: TObject;
                                                                    Request: TIdSipRequest;
                                                                    ReceivedFrom: TIdConnectionBindings);
begin
  Self.CheckReceivedParamFQDNSentBy(Sender, Request, ReceivedFrom);
end;

procedure TestTIdSipTransport.CheckReceivedParamFQDNSentBy(Sender: TObject;
                                                           Request: TIdSipRequest;
                                                           ReceivedFrom: TIdConnectionBindings);
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
                                                           ReceivedFrom: TIdConnectionBindings);
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

procedure TestTIdSipTransport.CheckSendBindingSet(Binding: TIdConnectionBindings);
begin
  CheckEquals(Self.LowPortLocation.Transport,
              Binding.Transport,
              Self.HighPortTransport.ClassName
            + ': Transport of sending binding');
  CheckEquals(Self.LowPortLocation.IPAddress,
              Binding.LocalIP,
              Self.HighPortTransport.ClassName
            + ': LocalIP of sending binding');
  CheckEquals(Self.LowPortLocation.Port,
              Binding.LocalPort,
              Self.HighPortTransport.ClassName
            + ': LocalPort of sending binding');
  CheckEquals(Self.HighPortLocation.IPAddress,
              Binding.PeerIP,
              Self.HighPortTransport.ClassName
            + ': PeerIP of sending binding');
  CheckEquals(Self.HighPortLocation.Port,
              Binding.PeerPort,
              Self.HighPortTransport.ClassName
            + ': PeerPort of sending binding');
end;

procedure TestTIdSipTransport.CheckSendRequestFromNonStandardPort(Sender: TObject;
                                                                  R: TIdSipRequest;
                                                                  ReceivedFrom: TIdConnectionBindings);
begin
  try
    CheckEquals(Request.LastHop.Port,
                Self.HighPortLocation.Port,
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

procedure TestTIdSipTransport.CheckSendRequestAutoRewriteVia(Sender: TObject;
                                                     R: TIdSipRequest;
                                                     ReceivedFrom: TIdConnectionBindings);
begin
  try
    Check(Self.HighPortTransport.GetTransportType = R.LastHop.Transport,
          Self.HighPortTransport.ClassName
       + ': Incorrect transport specified');

    Check(R.LastHop.HasBranch,
          Self.HighPortTransport.ClassName + ': Branch parameter missing');
    CheckEquals(ReceivedFrom.PeerIP,
                R.LastHop.SentBy,
                Self.HighPortTransport.ClassName
             + ': Topmost Via header''s sent-by doesn''t match sending IP');
    CheckHasPort(Self.LowPortTransport, ReceivedFrom.PeerIP, R.LastHop.Port,
                Self.HighPortTransport.ClassName
             + ': Topmost Via header''s port must be one that can accept connections');
{
    CheckEquals(ReceivedFrom.PeerPort,
                R.LastHop.Port,
                Self.HighPortTransport.ClassName
             + ': Topmost Via header''s port mustn''t match sending port (RFC 3261, section 18.1.1)');
}
    CheckEquals(ReceivedFrom.Transport,
                R.LastHop.Transport,
                Self.HighPortTransport.ClassName
             + ': Topmost Via header''s transport doesn''t match sending transport');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckSendRequestSpecifiedVia(Sender: TObject;
                                                           R: TIdSipRequest;
                                                           ReceivedFrom: TIdConnectionBindings);
begin
  try
    Check(Self.HighPortTransport.GetTransportType = R.LastHop.Transport,
          Self.HighPortTransport.ClassName
       + ': Incorrect transport specified');

    Check(R.LastHop.HasBranch,
          Self.HighPortTransport.ClassName + ': Branch parameter missing');
    CheckEquals(Self.SentBy,
                R.LastHop.SentBy,
                Self.HighPortTransport.ClassName
             + ': (Set) topmost Via header''s sent-by altered by sending transport');
    CheckEquals(ReceivedFrom.Transport,
                R.LastHop.Transport,
                Self.HighPortTransport.ClassName
             + ': Topmost Via header''s transport doesn''t match sending transport');

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
                                                                   ReceivedFrom: TIdConnectionBindings);
begin
  try
    CheckEquals(Self.LowPortLocation.Port,
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

procedure TestTIdSipTransport.CheckServerOnPort(const Host: String;
                                                Port: Cardinal;
                                                const Msg: String);
begin
  Fail(Self.ClassName + ' must override CheckServerOnPort');
end;

procedure TestTIdSipTransport.CheckUseRport(Sender: TObject;
                                            R: TIdSipRequest;
                                            ReceivedFrom: TIdConnectionBindings);
begin
  try
    Check(R.LastHop.HasParameter(RportParam),
          'No rport param');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

function TestTIdSipTransport.CopyFirstLocation(Transport: TIdSipTransport): TIdSipLocation;
var
  Bindings: TIdSipLocations;
begin
  Bindings := TIdSipLocations.Create;
  try
    Transport.LocalBindings(Bindings);

    if Bindings.IsEmpty then
      Result := nil
    else
      Result := Bindings[0].Copy;
  finally
    Bindings.Free;
  end;
end;

function TestTIdSipTransport.HighPortTransport: TIdSipTransport;
begin
  Result := Self.Timer.HighPortTransport;
end;

procedure TestTIdSipTransport.LogException(E: Exception; Method: String);
const
  LogMsg = 'Exception %s occured in %s: %s';
begin
  LogEntry(Format(LogMsg, [E.ClassName, Method, E.Message]), Self.FTestName, slDebug, 0, '');
end;

function TestTIdSipTransport.LowPortTransport: TIdSipTransport;
begin
  Result := Self.Timer.LowPortTransport;
end;

procedure TestTIdSipTransport.OnException(FailedMessage: TIdSipMessage;
                                          E: Exception;
                                          const Reason: String);
begin
  Self.ExceptionType := ExceptClass(E.ClassType);
  Self.ExceptionMessage := E.Message + ' caused by ''' + Reason + '''';
end;

procedure TestTIdSipTransport.OnEmpty(Sender: TIdTimerQueue);
begin
  Self.Lock.Acquire;
  try
    if Assigned(Self.EmptyListEvent) then
      Self.EmptyListEvent.SetEvent;
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdSipTransport.OnReceiveRequest(Request: TIdSipRequest;
                                               Receiver: TIdSipTransport;
                                               Source: TIdConnectionBindings);
begin
  Self.RecvdRequest.Assign(Request);

  if Assigned(Self.CheckingRequestEvent) then
    Self.CheckingRequestEvent(Receiver, Request, Source);
end;

procedure TestTIdSipTransport.OnReceiveResponse(Response: TIdSipResponse;
                                                Receiver: TIdSipTransport;
                                                Source: TIdConnectionBindings);
begin
  if Assigned(Self.CheckingResponseEvent) then
    Self.CheckingResponseEvent(Receiver, Response, Source);
end;

procedure TestTIdSipTransport.OnRejectedMessage(const Msg: String;
                                                const Reason: String;
                                                Source: TIdConnectionBindings);
begin
  Self.RejectedMessage       := true;
  Self.RejectedMessageReason := Reason;
  Self.ExceptionMessage      := Reason;
  Self.ReceivingBinding.Assign(Source);

  Self.RejectedMessageEvent.SetEvent;
end;

procedure TestTIdSipTransport.OnSendRequest(Request: TIdSipRequest;
                                            Sender: TIdSipTransport;
                                            Binding: TIdConnectionBindings);
begin
  Self.Lock.Acquire;
  try
    Self.RequestSendingBinding.Assign(Binding);
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdSipTransport.OnSendResponse(Response: TIdSipResponse;
                                             Sender: TIdSipTransport;
                                             Binding: TIdConnectionBindings);
begin
  Self.Lock.Acquire;
  try
    try
      Self.ResponseSendingBinding.Assign(Binding);
      if Assigned(Self.LastSentResponse) then begin
        Self.LastSentResponse.Assign(Response);
        Self.SendEvent.SetEvent;
      end;
    except
      on E: Exception do
        Self.LogException(E, Self.ClassName + '.OnSendResponse');
    end;
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdSipTransport.ReturnResponse(Sender: TObject;
                                             R: TIdSipRequest;
                                             ReceivedFrom: TIdConnectionBindings);
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
                         Self.HighPortLocation.IPAddress
                       + ':'
                       + IntToStr(Self.HighPortLocation.Port), [rfReplaceAll]);

  Self.SendMessage(Msg);
end;

procedure TestTIdSipTransport.SendMessage(Msg: String);
begin
  Fail(Self.ClassName + ' must override SendMessage');
end;

procedure TestTIdSipTransport.SendOkResponse(Transport: TIdSipTransport);
var
  Dest: TIdSipLocation;
begin
  Self.Response.StatusCode := SIPOK;

  Dest := TIdSipLocation.Create;
  try
    Dest.IPAddress := Self.HighPortLocation.IPAddress;
    Dest.Port      := Self.HighPortLocation.Port;
    Dest.Transport := Self.HighPortLocation.Transport;

    Transport.Send(Self.Response, Dest);
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipTransport.SetEvent(Sender: TObject;
                                       R: TIdSipResponse;
                                       ReceivedFrom: TIdConnectionBindings);
begin
  Self.ThreadEvent.SetEvent;
end;

function TestTIdSipTransport.TransportType: TIdSipTransportClass;
begin
  raise Exception.Create('TestTIdSipTransport.TransportType: override for '
                       + Self.ClassName + '!');
  Result := nil;
end;

//* TestTIdSipTransport Published methods **************************************

procedure TestTIdSipTransport.TestAddBinding;
var
  NewPort:      Cardinal;
  OriginalPort: Cardinal;
begin
  OriginalPort := Self.LowPortLocation.Port;
  NewPort      := OriginalPort + 1;

  Self.CheckServerNotOnPort(Self.LowPortLocation.IPAddress,
                            NewPort,
                            'Sanity check: nothing should be running on port ' + IntToStr(NewPort));

  Self.LowPortTransport.AddBinding(Self.LowPortLocation.IPAddress, NewPort);
  Self.CheckServerOnPort(Self.LowPortLocation.IPAddress,
                         NewPort,
                         'Nothing running on port ' + IntToStr(NewPort) + '; binding not added');
end;

procedure TestTIdSipTransport.TestAddBindingDoesntStartStoppedTransport;
begin
  Self.LowPortTransport.Stop;
  Self.LowPortTransport.AddBinding(Self.LowPortLocation.IPAddress,
                                   Self.LowPortLocation.Port + 1);
  Check(not Self.LowPortTransport.IsRunning,
        'AddBinding started the server');
end;

procedure TestTIdSipTransport.TestAddBindingRestartsStartedTransport;
begin
  Self.LowPortTransport.Start;
  Self.LowPortTransport.AddBinding(Self.LowPortLocation.IPAddress,
                                   Self.LowPortLocation.Port + 1);
  Check(Self.LowPortTransport.IsRunning,
        'AddBinding stopped the server');
end;

procedure TestTIdSipTransport.TestBindingCount;
begin
  CheckEquals(1, Self.LowPortTransport.BindingCount, 'Initial count');

  Self.LowPortTransport.AddBinding(Self.LowPortLocation.IPAddress, Self.LowPortLocation.Port + 1);
  CheckEquals(2, Self.LowPortTransport.BindingCount, 'One AddBinding later');

  Self.LowPortTransport.AddBinding(Self.LowPortLocation.IPAddress, Self.LowPortLocation.Port + 2);
  CheckEquals(3, Self.LowPortTransport.BindingCount, 'Two AddBindings later');
end;

procedure TestTIdSipTransport.TestCanReceiveRequest;
begin
  // LowPortTransport sends an INVITE to HighPortTransport.
  // We check that HighPortTransport did actually get it.
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.SendMessage(Self.Request.AsString);

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

  Self.LowPortTransport.Send(Self.Response, Self.HighPortLocation);

  Self.WaitForSignaled;

  Check(Self.ReceivedResponse,
        Self.HighPortTransport.ClassName + ': Response not received');
end;

procedure TestTIdSipTransport.TestClearBindings;
var
  Address:   String;
  FirstPort: Integer;
  NewPort:   Integer;
begin
  // If we remove all the bindings, then the TIdTCPServer will re-add one as
  // soon as you restart the server (which happens implicitly in ClearBindings).
  // This binding will have a port of DefaultPort, hence our test bindings below
  // have values such that Port <> DefaultPort.

  Self.LowPortTransport.Stop;
  CheckServerNotOnPort(Self.LowPortLocation.IPAddress,
                       DefaultSipPort,
                       'Close down all SIP UAs before running this test.');

  Address   := Self.LowPortLocation.IPAddress;
  FirstPort := Self.LowPortLocation.Port + 1;

  NewPort := FirstPort + 1;

  Self.LowPortTransport.AddBinding(Address, NewPort);

  Self.LowPortTransport.ClearBindings;
  Self.LowPortTransport.Start;
  CheckServerNotOnPort(Address,
                       FirstPort,
                       'ClearBindings didn''t remove the first binding');
  CheckServerNotOnPort(Address,
                       NewPort,
                       'ClearBindings didn''t remove the second binding');
end;

procedure TestTIdSipTransport.TestClearBindingsDoesntStartStoppedTransport;
begin
  Self.LowPortTransport.Stop;
  Self.LowPortTransport.ClearBindings;

  Check(not Self.LowPortTransport.IsRunning,
        'ClearBindings started the transport');
end;

procedure TestTIdSipTransport.TestClearBindingRestartsStartedTransport;
begin
  Self.LowPortTransport.Stop;
  CheckServerNotOnPort(Self.LowPortLocation.IPAddress,
                       DefaultSipPort,
                       'Close down all SIP UAs before running this test.');

  Self.LowPortTransport.Start;
  Self.LowPortTransport.ClearBindings;

  Check(Self.LowPortTransport.IsRunning,
        'ClearBindings didn''t restart the transport');
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
                   + 'Via: SIP/2.0/' + Self.HighPortTransport.GetTransportType + ' proxy.tessier-ashpool.co.luna;branch=z9hG4bK776asdhds'#13#10
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

    // We wait for the SendEvent event, because we always notify of a rejected
    // message before we send the response. Thus, the test could fail because
    // we didn't have enough time to register the sending of the 400 Bad
    // Request.
    Self.WaitForSignaled(Self.SendEvent, 'Waiting to send message');

    Check(not Self.ReceivedRequest,
          Self.HighPortTransport.ClassName
        + ': Somehow we received a mangled message');
    Check(Self.RejectedMessage,
          Self.HighPortTransport.ClassName
        + ': Notification of message rejection not received');

    CheckEquals(Self.HighPortLocation.IPAddress,
                Self.ReceivingBinding.LocalIP,
                Self.HighPortTransport.ClassName
              + ': Receiving binding IP address not correctly recorded');
    CheckEquals(Self.HighPortLocation.Port,
                Self.ReceivingBinding.LocalPort,
                Self.HighPortTransport.ClassName
              + ': Receiving binding port not correctly recorded');
    CheckEquals(Self.HighPortLocation.Transport,
                Self.ReceivingBinding.Transport,
                Self.HighPortTransport.ClassName
              + ': Receiving binding transport not correctly recorded');

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

procedure TestTIdSipTransport.TestDiscardRequestWithInconsistentTransport;
var
  InconsistentTransport: String;
  Listener:              TIdSipTestTransportListener;
begin
  // InconsistentTransport will now not match the transport type of the socket
  // from which the Transport will receive the message. In other words, a UDP
  // socket will now "receive" a message with transport type UDP-Foo. We then
  // return a 400 Bad Request.
  InconsistentTransport := Self.HighPortTransport.GetTransportType + '-Foo';

  Listener := TIdSipTestTransportListener.Create;
  try
    Self.HighPortTransport.AddTransportListener(Listener);
    Self.SendMessage('INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0'#13#10
                   + 'Via: SIP/2.0/' + InconsistentTransport + ' proxy.tessier-ashpool.co.luna;branch=z9hG4bK776asdhds'#13#10
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

    // Self.SendEvent is set by OnSendResponse: that is, it is set after
    // HighPortTransport has received the request and rejected it.               
    Self.WaitForSignaled(Self.SendEvent, 'Waiting for send event');

    Check(not Self.ReceivedRequest,
          Self.HighPortTransport.ClassName
        + ': Somehow we received a "malformed" message');
    Check(Self.RejectedMessage,
          Self.HighPortTransport.ClassName
        + ': Notification of message rejection not received');
   CheckEquals(ViaTransportMismatch,
               Self.RejectedMessageReason,
                Self.HighPortTransport.ClassName
              + ': Rejection reason');

    CheckNotEquals(0,
                   Self.LastSentResponse.StatusCode,
                   'We didn''t receive the "Bad Request" response');

    // Check that the transport sends the 400 Bad Request.
    CheckEquals(SIPBadRequest,
                Self.LastSentResponse.StatusCode,
                Self.HighPortTransport.ClassName
              + ': "Bad Request" response');
    CheckEquals(ViaTransportMismatch,
                Self.LastSentResponse.StatusText,
                Self.HighPortTransport.ClassName
              + ': Unexpected Status-Text in the response');

    // Check that the transport didn't send the malformed request up the stack
    Check(not Listener.ReceivedRequest,
          Self.HighPortTransport.ClassName
        + ': Transport passed malformed request up the stack');
  finally
    Self.HighPortTransport.RemoveTransportListener(Listener);
    Listener.Free;
  end;
end;

procedure TestTIdSipTransport.TestDiscardResponseWithInconsistentTransport;
var
  InconsistentTransport: String;
  Listener:              TIdSipTestTransportListener;
begin
  // InconsistentTransport will now not match the transport type of the socket
  // from which the Transport will receive the message. In other words, a UDP
  // socket will now "receive" a message with transport type UDP-Foo. We then
  // return a 400 Bad Request.
  InconsistentTransport := Self.HighPortTransport.GetTransportType + '-Foo';

  Listener := TIdSipTestTransportListener.Create;
  try
    Self.HighPortTransport.AddTransportListener(Listener);
    Self.SendMessage('SIP/2.0 200 OK'#13#10
                   + 'Via: SIP/2.0/' + InconsistentTransport + ' ' + Self.HighPortTransport.HostName + ';branch=z9hG4bK776asdhds'#13#10
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

    Self.WaitForSignaled(Self.RejectedMessageEvent);

    Check(not Self.ReceivedResponse,
          Self.HighPortTransport.ClassName
        + ': Somehow we received a "malformed" message');
    Check(Self.RejectedMessage,
          Self.HighPortTransport.ClassName
        + ': Notification of message rejection not received');
   CheckEquals(ViaTransportMismatch,
               Self.RejectedMessageReason,
                Self.HighPortTransport.ClassName
              + ': Rejection reason');

    Check(not Listener.ReceivedResponse,
          Self.HighPortTransport.ClassName
        + ': Transport passed "malformed" request up the stack');
  finally
    Self.HighPortTransport.RemoveTransportListener(Listener);
    Listener.Free;
  end;
end;

procedure TestTIdSipTransport.TestDiscardResponseWithUnknownSentBy;
begin
  Self.CheckingResponseEvent := Self.CheckDiscardResponseWithUnknownSentBy;

  // If we don't set the received param we won't be able to send
  // the message to the right SIP server. No, you'd never do this
  // in production, because it's wilfully wrong.
  Self.Response.LastHop.SentBy := 'unknown.host';
  Self.Response.LastHop.Received := Self.LowPortLocation.IPAddress;
  Self.SendMessage(Self.Response.AsString);

  Self.WaitForSignaled(Self.RejectedMessageEvent);
  Check(not Self.ReceivedResponse,
        Self.HighPortTransport.ClassName
      + ': Response not silently discarded');
  Check(Self.RejectedMessage,
        Self.HighPortTransport.ClassName
      + ': Rejected message event didn''t fire');
end;

procedure TestTIdSipTransport.TestDontDiscardUnknownSipVersion;
var
  Destination:   String;
  TortureTest41: String;
begin
  // The Transaction-User level handles rejecting these messages.
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.ExceptionMessage := 'Waiting for request to arrive';

  Destination := Self.HighPortLocation.IPAddress + ':' + IntToStr(Self.HighPortLocation.Port);

  TortureTest41 := 'INVITE sip:t.watson@' + Destination + ' SIP/7.0'#13#10
                 + 'Via:     SIP/2.0/' + Self.HighPortTransport.GetTransportType + ' c.bell-tel.com;branch=z9hG4bKkdjuw'#13#10
                 + 'Max-Forwards:     70'#13#10
                 + 'From:    A. Bell <sip:a.g.bell@bell-tel.com>;tag=qweoiqpe'#13#10
                 + 'To:      T. Watson <sip:t.watson@' + Destination + '>'#13#10
                 + 'Call-ID: 31417@c.bell-tel.com'#13#10
                 + 'CSeq:    1 INVITE'#13#10
                 + #13#10;
  Self.SendMessage(TortureTest41);

  Self.WaitForSignaled(Self.ClassName
                    + ': Didn''t receive a message with an unknown SIP-Version (timeout)');
end;

procedure TestTIdSipTransport.TestHasBinding;
var
  Address: String;
  Port:    Cardinal;
begin
  Address := Self.LowPortLocation.IPAddress;
  Port    := Self.LowPortLocation.Port + 1;

  Check(not Self.LowPortTransport.HasBinding(Address, Port),
        Self.LowPortTransport.ClassName
      + ': The server has, but shouldn''t, a binding on '
      + Address + ':' + IntToStr(Port));

  Self.LowPortTransport.AddBinding(Address, Port);

  Check(Self.LowPortTransport.HasBinding(Address, Port),
        Self.LowPortTransport.ClassName
     +  ': The server doesn''t have, but should, a binding on '
     + Address + ':' + IntToStr(Port));
end;

procedure TestTIdSipTransport.TestInstantiationRegistersTransport;
begin
  CheckNotEquals('',
                 Self.HighPortTransport.ID,
                 'HighPortTransport not registered');
  CheckNotEquals('',
                 Self.LowPortTransport.ID,
                 'LowPortTransport not registered');
  CheckNotEquals(Self.HighPortTransport.ID,
                 Self.LowPortTransport.ID,
                 'HighPortTransport and LowPortTransport have same ID');
end;

procedure TestTIdSipTransport.TestIsNull;
begin
  Check(not Self.HighPortTransport.IsNull,
        'non-null transport (' + Self.HighPortTransport.ClassName
      + ') marked as null');
end;

procedure TestTIdSipTransport.TestIsRunning;
begin
  Self.LowPortTransport.Stop;
  Check(not Self.LowPortTransport.IsRunning,
        'Initial condition: stopped transport');

  Self.LowPortTransport.Start;
  Check(Self.LowPortTransport.IsRunning,
        'Transport didn''t start');

  Self.LowPortTransport.Stop;
  Check(not Self.LowPortTransport.IsRunning,
        'Transport didn''t stop');
end;

procedure TestTIdSipTransport.TestLocalBindingsDoesntClearParameter;
var
  LocalBindings: TIdSipLocations;
begin
  LocalBindings := TIdSipLocations.Create;
  try
    LocalBindings.AddLocation(Self.HighPortLocation);

    Self.LowPortTransport.LocalBindings(LocalBindings);
    CheckEquals(2, LocalBindings.Count, 'Incorrect binding count: parameter not first cleared');
    CheckEquals(Self.HighPortLocation.AsString, LocalBindings[0].AsString, 'First binding');
    CheckEquals(Self.LowPortLocation.AsString,  LocalBindings[1].AsString, 'Second binding');
  finally
    LocalBindings.Free;
  end;
end;

procedure TestTIdSipTransport.TestLocalBindingsMultipleBindings;
var
  LocalBindings: TIdSipLocations;
  SecondBinding: TIdSipLocation;
  ThirdBinding:  TIdSipLocation;
begin
  LocalBindings := TIdSipLocations.Create;
  try
    SecondBinding := TIdSipLocation.Create(Self.LowPortLocation.Transport,
                                           Self.LowPortLocation.IPAddress,
                                           Self.LowPortLocation.Port + 1);
    try
      ThirdBinding := TIdSipLocation.Create(SecondBinding.Transport,
                                            SecondBinding.IPAddress,
                                            SecondBinding.Port + 1);
      try
        Self.LowPortTransport.AddBinding(SecondBinding.IPAddress, SecondBinding.Port);
        Self.LowPortTransport.AddBinding(ThirdBinding.IPAddress,  ThirdBinding.Port);


        Self.LowPortTransport.LocalBindings(LocalBindings);
        CheckEquals(3, LocalBindings.Count, 'Incorrect number of bindings');
        CheckEquals(Self.LowPortLocation.AsString, LocalBindings[0].AsString, 'First binding');
        CheckEquals(SecondBinding.AsString, LocalBindings[1].AsString, 'Second binding');
        CheckEquals(ThirdBinding.AsString, LocalBindings[2].AsString, 'Third binding');
      finally
        ThirdBinding.Free;
      end;
    finally
      SecondBinding.Free;
    end;
  finally
    LocalBindings.Free;
  end;
end;

procedure TestTIdSipTransport.TestLocalBindingsSingleBinding;
var
  LocalBindings: TIdSipLocations;
begin
  LocalBindings := TIdSipLocations.Create;
  try
    Self.LowPortTransport.LocalBindings(LocalBindings);
    CheckEquals(1, LocalBindings.Count, 'Incorrect number of bindings');
    Check(LocalBindings[0].Equals(Self.LowPortLocation), 'First binding incorrect');
  finally
    LocalBindings.Free;
  end;
end;

procedure TestTIdSipTransport.TestMalformedCallID;
var
  MalformedCallID: String;
begin
  MalformedCallID := 'INVITE sip:lukec@%s:5060 SIP/2.0'#13#10
                   + 'Via: SIP/2.0/' + Self.HighPortTransport.GetTransportType + ' 192.168.102.34:5060;branch=z9hG4bK0f9df2b95'#13#10
                   + 'To: "lukec" <sip:lukec@%s>'#13#10
                   + 'From: "" <sip:@192.168.102.34:5061>;tag=-6649039992220922599'#13#10
                   + 'CSeq: 190744561 INVITE'#13#10
                   + 'Call-ID: 1158167609796[B@dd20f6@192.168.102.34'#13#10
                   + 'Content-Type: application/sdp'#13#10
                   + 'Contact: "PSTN In" <sip:192.168.102.34:5061>'#13#10
                   + 'Max-Forwards: 68'#13#10
                   + 'Content-Length: 115'#13#10
                   + #13#10
                   + 'v=0'#13#10
                   + 'o=none 0 0 IN IP4 0.0.0.0'#13#10
                   + 's=session'#13#10
                   + 'c=IN IP4 192.168.102.34'#13#10
                   + 'm=text 1308 RTP/AVP 98'#13#10
                   + 'a=rtpmap:98 t140/1000'#13#10;


  Self.SendFromLowTransport(StringReplace(MalformedCallID, '%s', Self.HighPortLocation.IPAddress, [rfReplaceAll]));

  Self.WaitForSignaled(Self.RejectedMessageEvent);
end;

procedure TestTIdSipTransport.TestReceivedParamDifferentIPv4SentBy;
begin
  Self.CheckingRequestEvent := Self.CheckReceivedParamDifferentIPv4SentBy;

  Self.Request.LastHop.SentBy := '127.0.0.3';
  Self.SendMessage(Self.Request.AsString);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestReceivedParamFQDNSentBy;
begin
  Self.CheckingRequestEvent := Self.CheckReceivedParamFQDNSentBy;

  Self.Request.LastHop.SentBy := 'localhost';
  Self.SendMessage(Self.Request.AsString);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestReceivedParamIPv4SentBy;
begin
  Self.CheckingRequestEvent := Self.CheckReceivedParamIPv4SentBy;
  // This is a bit of a hack. We want to make sure the sent-by's an IP.
  Self.LowPortTransport.HostName := Self.LowPortLocation.IPAddress;
  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestReceiveRequestShowsCorrectBinding;
begin
  // LowPortTransport sends an INVITE to HighPortTransport.
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.SendMessage(Self.Request.AsString);

  Self.WaitForSignaled;
  Self.CheckBinding(Self.ReceivingBinding);
end;

procedure TestTIdSipTransport.TestReceiveResponseFromMappedRoute;
const
  GatewayAddress = '1.2.3.4';
var
  MockRT: TIdMockRoutingTable;
begin
  // We have a NAT gateway to the Internet, with external IP of 1.2.3.4.
  MockRT := Self.HighPortTransport.RoutingTable as TIdMockRoutingTable;
  MockRT.AddMappedRoute('0.0.0.0', '0.0.0.0', GatewayAddress);

  Self.Response.LastHop.SentBy := GatewayAddress;

  // If we reach CheckingResponseEvent then the response has been accepted.
  Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;
  Self.SendMessage(Self.Response.AsString);

  Self.WaitForSignaled('Message from a mapped route not accepted');
end;

procedure TestTIdSipTransport.TestReceiveResponseOnLocallyInitiatedConnectionShowsCorrectBinding;
var
  Trying: TIdSipResponse;
begin
  // HighPortTransport  --   INVITE   --> LowPortTransport
  // HighPortTransport <-- 100 Trying --  LowPortTransport
  //
  // Does the receiving binding now hold the correct information?

  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;
  Self.HighPortTransport.Send(Self.Request, Self.LowPortLocation);
  Self.WaitForSignaled('No ' + Self.Request.Method + ' received');

  Self.CheckingRequestEvent := nil;
  Trying := TIdSipResponse.InResponseTo(Self.Request, SIPTrying);
  try
    Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;
    Self.LowPortTransport.Send(Trying, Self.HighPortLocation);
    Self.WaitForSignaled('No ' + Trying.Description + ' received');

    CheckEquals(Self.LowPortLocation.IPAddress,  Self.ReceivingBinding.PeerIP,   'PeerIP incorrect');
    CheckEquals(Self.LowPortLocation.Port,       Self.ReceivingBinding.PeerPort, 'PeerPort incorrect');
    CheckEquals(Self.HighPortLocation.Transport, Self.ReceivingBinding.Transport, 'Transport incorrect');

    // Remember, this local binding uses an ephemeral port.
    CheckNotEquals('', Self.ReceivingBinding.LocalIP,   'LocalIP not filled in');
    CheckNotEquals(0,  Self.ReceivingBinding.LocalPort, 'LocalPort not filled in');
  finally
    Trying.Free;
  end;
end;

procedure TestTIdSipTransport.TestReceiveResponseShowsCorrectBinding;
begin
  // LowPortTransport sends an INVITE to HighPortTransport.
  Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;

  Self.SendMessage(Self.Response.AsString);

  Self.WaitForSignaled;
  Self.CheckBinding(Self.ReceivingBinding);
end;

procedure TestTIdSipTransport.TestRemoveBinding;
var
  NewPort:      Cardinal;
  OriginalPort: Cardinal;
begin
  OriginalPort := Self.LowPortLocation.Port;
  NewPort      := OriginalPort + 1;

  Self.CheckServerNotOnPort(Self.LowPortLocation.IPAddress,
                            NewPort,
                            'Sanity check: nothing should be running on port ' + IntToStr(NewPort));

  Self.LowPortTransport.AddBinding(Self.LowPortLocation.IPAddress, NewPort);
  Self.LowPortTransport.RemoveBinding(Self.LowPortLocation.IPAddress, NewPort);

  Self.CheckServerNotOnPort(Self.LowPortLocation.IPAddress,
                            NewPort,
                            'Something running on port ' + IntToStr(NewPort)
                          + '; binding not removed');
end;

procedure TestTIdSipTransport.TestRemoveBindingDoesntStartStoppedTransport;
begin
  Self.LowPortTransport.Stop;
  Self.LowPortTransport.AddBinding(Self.LowPortLocation.IPAddress,
                                   Self.LowPortLocation.Port + 1);
  Self.LowPortTransport.RemoveBinding(Self.LowPortLocation.IPAddress,
                                      Self.LowPortLocation.Port + 1);

  Check(not Self.LowPortTransport.IsRunning,
        'RemoveBinding restarted the transport');
end;

procedure TestTIdSipTransport.TestRemoveBindingNoSuchBinding;
begin
  // Check that calling RemoveBinding for a non-existent doesn't blow up.

  Self.LowPortTransport.RemoveBinding(Self.LowPortLocation.IPAddress,
                                      Self.LowPortLocation.Port + 1);
end;

procedure TestTIdSipTransport.TestRemoveBindingRestartsStartedTransport;
begin
  Self.LowPortTransport.Start;

  Self.LowPortTransport.AddBinding(Self.LowPortLocation.IPAddress,
                                   Self.LowPortLocation.Port + 1);
  Self.LowPortTransport.RemoveBinding(Self.LowPortLocation.IPAddress,
                                      Self.LowPortLocation.Port + 1);

  Check(Self.LowPortTransport.IsRunning,
        'RemoveBinding stopped the transport');
end;

procedure TestTIdSipTransport.TestSendRequest;
begin
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;

  Check(Self.ReceivedRequest,
        Self.HighPortTransport.ClassName + ': Request not received: SendRequest '
      + 'didn''t use the DestinationLocation or something bad happened');

  CheckSendBindingSet(Self.RequestSendingBinding);
end;

procedure TestTIdSipTransport.TestSendRequestAutoRewriteVia;
begin
  Self.SentBy := 'talking-head.tessier-ashpool.co.luna';
  Self.Request.LastHop.SentBy := Self.SentBy;
  Self.Request.LastHop.IsUnset := true;
  Self.CheckingRequestEvent := Self.CheckSendRequestAutoRewriteVia;
  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestSendRequestSpecifiedVia;
begin
  Self.SentBy := 'talking-head.tessier-ashpool.co.luna';
  Self.Request.LastHop.SentBy := Self.SentBy;
  Self.Request.LastHop.IsUnset := false;
  Self.CheckingRequestEvent := Self.CheckSendRequestSpecifiedVia;
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

  CheckSendBindingSet(Self.ResponseSendingBinding);
end;

procedure TestTIdSipTransport.TestSendResponseFromNonStandardPort;
begin
  Self.Response.LastHop.Port := Self.LowPortLocation.Port;
  Self.CheckingResponseEvent := Self.CheckSendResponseFromNonStandardPort;
  Self.LowPortTransport.Send(Self.Response, Self.HighPortLocation);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestSendResponseUsesDestinationLocation;
begin
  // We mutate Self.Response to contain an unused port in the topmost Via. Then
  // we check that the transport uses the Location parameter for routing
  // information (ignoring the Via header). 
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
          Self.HighPortTransport.RemoveTransportListener(Self);
          Self.HighPortTransport.AddTransportListener(Self);

          Self.Response.LastHop.Received := Self.HighPortLocation.IPAddress;
          Self.LowPortTransport.Send(Self.Response, Self.HighPortLocation);

          // It's not perfect, but anyway. We need to wait long enough for
          // LowPortTransport to get its response.
          Self.WaitForSignaled;

          Check(not Self.ReceivedRequest,
                Self.HighPortTransport.ClassName
              + ': Somehow we received a mangled message');

          Check(HighPortListener.ReceivedResponse,
                Self.HighPortTransport.ClassName
              + ': HighPortTransport didn''t get the message');
          Check(not LowPortListener.ReceivedResponse,
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
var
  Destination:   String;
  TortureTest16: String;
begin
  //   This is a request message with a Content Length that is much larger
  //   than the length of the body.
  //
  //   When sent UDP, the server should respond with an error. With TCP,
  //   there's not much you can do but wait...

  Self.DefaultTimeout := Self.HighPortTransport.Timeout * 2;
  Self.CheckingResponseEvent := Self.CheckForBadRequest;

  Destination := Self.HighPortLocation.IPAddress + ':' + IntToStr(Self.HighPortLocation.Port);
  TortureTest16 := 'INVITE sip:user@%s SIP/2.0'#13#10
                 + 'Max-Forwards: 80'#13#10
                 + 'To: sip:j.user@%s'#13#10
                 + 'From: sip:caller@university.edu;tag=93942939o2'#13#10
                 + 'Call-ID: 0ha0isndaksdj@10.0.0.1'#13#10
                 + 'CSeq: 8 INVITE'#13#10
                 + 'Via: SIP/2.0/' + Self.HighPortTransport.GetTransportType + ' 135.180.130.133'#13#10
                 + 'Contact: sip:' + Self.LowPortLocation.IPAddress + ':' + IntToStr(Self.LowPortLocation.Port) +  #13#10
                 + 'Content-Type: application/sdp'#13#10
                 + 'Content-Length: 9999'#13#10
                 + #13#10
                 + 'v=0'#13#10
                 + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                 + 's=-'#13#10
                 + 'c=IN IP4 135.180.130.88'#13#10
                 + 't=0 0'#13#10
                 + 'm=audio 492170 RTP/AVP 0 12'#13#10
                 + 'm=video 3227 RTP/AVP 31'#13#10
                 + 'a=rtpmap:31 LPC';

  Self.SendFromLowTransport(StringReplace(TortureTest16, '%s', Destination, [rfReplaceAll]));

  Self.WaitForSignaled(Self.RejectedMessageEvent);
end;

procedure TestTIdSipTransport.TestTortureTest17;
var
  Destination:   String;
  TortureTest17: String;
begin
  //   This is a request message with a negative value for Content-Length.
  //
  //   The server should respond with an error.

  Destination := Self.HighPortLocation.IPAddress + ':' + IntToStr(Self.HighPortLocation.Port);

  TortureTest17 := 'INVITE sip:user@%s SIP/2.0'#13#10
                 + 'Max-Forwards: 254'#13#10
                 + 'To: sip:j.user@%s'#13#10
                 + 'From: sip:caller@university.edu;tag=3'#13#10
                 + 'Call-ID: 0ha0isndaksdj@10.0.0.1'#13#10
                 + 'CSeq: 8 INVITE'#13#10
                 + 'Via: SIP/2.0/' + Self.HighPortTransport.GetTransportType + ' 135.180.130.133;branch=z9hG4bKkdjuw'#13#10
                 + 'Content-Type: application/sdp'#13#10
                 + 'Content-Length: -999'#13#10
                 + #13#10
                 + 'v=0'#13#10
                 + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                 + 's=-'#13#10
                 + 'c=IN IP4 135.180.130.88'#13#10
                 + 't=0 0'#13#10;

  Self.CheckingResponseEvent := Self.CheckForBadRequest;

  Self.SendFromLowTransport(StringReplace(TortureTest17, '%s', Destination, [rfReplaceAll]));

  Self.WaitForSignaled(Self.RejectedMessageEvent);
end;

procedure TestTIdSipTransport.TestTortureTest19;
var
  Destination:   String;
  MalformedTo:   String;
  TortureTest19: String;
begin
  //   This is a request with an unterminated quote in the display name of
  //   the To field.
  //
  //   The server can either return an error, or proxy it if it is
  //   successful parsing without the terminating quote.

  Destination := Self.HighPortLocation.IPAddress + ':' + IntToStr(Self.HighPortLocation.Port);
  MalformedTo := '"Mr. J. User <sip:j.user@%s>';

  TortureTest19 := 'INVITE sip:user@%s SIP/2.0'#13#10
                 + 'To: ' + MalformedTo + #13#10
                 + 'From: sip:caller@university.edu;tag=93334'#13#10
                 + 'Max-Forwards: 10'#13#10
                 + 'Call-ID: 0ha0isndaksdj@10.0.0.1'#13#10
                 + 'CSeq: 8 INVITE'#13#10
                 + 'Via: SIP/2.0/' + Self.HighPortTransport.GetTransportType + ' 135.180.130.133:5050;branch=z9hG4bKkdjuw'#13#10
                 + 'Content-Type: application/sdp'#13#10
                 + 'Content-Length: 138'#13#10
                 + #13#10
                 + 'v=0'#13#10
                 + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                 + 's=-'#13#10
                 + 'c=IN IP4 135.180.130.88'#13#10
                 + 't=0 0'#13#10
                 + 'm=audio 492170 RTP/AVP 0 12'#13#10
                 + 'm=video 3227 RTP/AVP 31'#13#10
                 + 'a=rtpmap:31 LPC';

  Self.CheckingResponseEvent := Self.CheckForBadRequest;
  Self.SendFromLowTransport(StringReplace(TortureTest19, '%s', Destination, [rfReplaceAll]));

  Self.WaitForSignaled(Self.RejectedMessageEvent);
  CheckEquals(Format(MalformedToken, [ToHeaderFull, StringReplace(MalformedTo, '%s', Destination, [rfReplaceAll])]),
              Self.RejectedMessageReason,
              'Message rejected for the wrong reason');
end;

procedure TestTIdSipTransport.TestTortureTest21;
var
  Destination:   String;
  TortureTest21: String;
begin
  //   This INVITE is illegal because the Request-URI has been enclosed
  //   within in "<>".
  //   An intelligent server may be able to deal with this and fix up
  //   athe Request-URI if acting as a Proxy. If not it should respond 400
  //   with an appropriate reason phrase.

  Destination := Self.HighPortLocation.IPAddress + ':' + IntToStr(Self.HighPortLocation.Port);

  TortureTest21 := 'INVITE <sip:user@%s> SIP/2.0'#13#10
                 + 'To: sip:user@%s'#13#10
                 + 'From: sip:caller@university.edu;tag=39291'#13#10
                 + 'Max-Forwards: 23'#13#10
                 + 'Call-ID: 1@10.0.0.1'#13#10
                 + 'CSeq: 1 INVITE'#13#10
                 + 'Via: SIP/2.0/' + Self.HighPortTransport.GetTransportType + ' 135.180.130.133'#13#10
                 + 'Content-Type: application/sdp'#13#10
                 + 'Content-Length: 174'#13#10
                 + #13#10
                 + 'v=0'#13#10
                 + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                 + 's=-'#13#10
                 + 'c=IN IP4 135.180.130.88'#13#10
                 + 't=3149328700 0'#13#10
                 + 'm=audio 492170 RTP/AVP 0 12'#13#10
                 + 'm=video 3227 RTP/AVP 31'#13#10
                 + 'a=rtpmap:31 LPC';

  Self.CheckingResponseEvent := Self.CheckForBadRequest;
  Self.SendFromLowTransport(StringReplace(TortureTest21, '%s', Destination, [rfReplaceAll]));

  Self.WaitForSignaled(Self.RejectedMessageEvent);
end;

procedure TestTIdSipTransport.TestTortureTest22;
var
  Destination:   String;
  TortureTest22: String;
begin
  //   This INVITE has illegal LWS within the SIP URI.
  //
  //   An intelligent server may be able to deal with this and fix up
  //   the Request-URI if acting as a Proxy. If not it should respond 400
  //   with an appropriate reason phrase.

  Destination := Self.HighPortLocation.IPAddress + ':' + IntToStr(Self.HighPortLocation.Port);

  TortureTest22 := 'INVITE sip:user@%s; transport=udp SIP/2.0'#13#10
                 + 'To: sip:user@%s'#13#10
                 + 'From: sip:caller@university.edu;tag=231413434'#13#10
                 + 'Max-Forwards: 5'#13#10
                 + 'Call-ID: 2@10.0.0.1'#13#10
                 + 'CSeq: 1 INVITE'#13#10
                 + 'Via: SIP/2.0/' + Self.HighPortTransport.GetTransportType + ' 135.180.130.133:5060;branch=z9hG4bKkdjuw'#13#10
                 + 'Content-Type: application/sdp'#13#10
                 + 'Content-Length: 174'#13#10
                 + #13#10
                 + 'v=0'#13#10
                 + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                 + 's=-'#13#10
                 + 'c=IN IP4 135.180.130.88'#13#10
                 + 't=3149328700 0'#13#10
                 + 'm=audio 492170 RTP/AVP 0 12'#13#10
                 + 'm=video 3227 RTP/AVP 31'#13#10
                 + 'a=rtpmap:31 LPC';

  Self.CheckingResponseEvent := Self.CheckForBadRequest;
  Self.SendFromLowTransport(StringReplace(TortureTest22, '%s', Destination, [rfReplaceAll]));

  Self.WaitForSignaled(Self.RejectedMessageEvent);
end;

procedure TestTIdSipTransport.TestTortureTest23;
var
  Destination:   String;
  TortureTest23: String;
begin
  //   This INVITE has illegal >1 SP between elements of the Request-Line.
  //
  //   An intelligent server may be able to deal with this and fix up
  //   the Request-URI if acting as a Proxy. If not it should respond 400
  //   with an appropriate reason phrase.

  TortureTest23 := 'INVITE sip:user@%s  SIP/2.0'#13#10
                 + 'Max-Forwards: 8'#13#10
                 + 'To: sip:user@%s'#13#10
                 + 'From: sip:caller@university.edu;tag=8814'#13#10
                 + 'Call-ID: 3@10.0.0.1'#13#10
                 + 'CSeq: 1 INVITE'#13#10
                 + 'Via: SIP/2.0/' + Self.HighPortTransport.GetTransportType + ' 135.180.130.133:5060;branch=z9hG4bKkdjuw'#13#10
                 + 'Content-Type: application/sdp'#13#10
                 + 'Content-Length: 174'#13#10
                 + #13#10
                 + 'v=0'#13#10
                 + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                 + 's=-'#13#10
                 + 'c=IN IP4 135.180.130.88'#13#10
                 + 't=0 0'#13#10
                 + 'm=audio 492170 RTP/AVP 0 12'#13#10
                 + 'm=video 3227 RTP/AVP 31'#13#10
                 + 'a=rtpmap:31 LPC';

  Self.CheckingResponseEvent := Self.CheckForBadRequest;

  Destination := Self.HighPortLocation.IPAddress + ':' + IntToStr(Self.HighPortLocation.Port);
  Self.SendFromLowTransport(StringReplace(TortureTest23, '%s', Destination, [rfReplaceAll]));

  Self.WaitForSignaled(Self.RejectedMessageEvent);
end;

procedure TestTIdSipTransport.TestTortureTest35;
var
  Destination:   String;
  TortureTest35: String;
begin
  //   This is an illegal and badly mangled message.
  //
  //   A server should respond 400 with an appropriate reason phrase if it
  //   can. It may just drop this message.

  Destination := Self.HighPortLocation.IPAddress + ':' + IntToStr(Self.HighPortLocation.Port);

  TortureTest35 := 'OPTIONS sip:%s SIP/2.0'#13#10
                 + 'Via: SIP/2.0/' + Self.HighPortTransport.GetTransportType + ' %s'#13#10
                 + 'Max-Forwards: 70'#13#10
                 + 'From: sip:iuser@company.com;tag=74345345'#13#10
                 + 'To: sip:user@135.180.130.133'#13#10
                 + 'Call-ID: 1804928587@company.com'#13#10
                 + 'CSeq: 1 OPTIONS'#13#10
                 + 'Expires: 0 0l@company.com'#13#10               // mangled
                 + 'To: sip:user@135.180.130.133'#13#10            // 2nd To header
                 + 'Call-ID: 1804928587@company.com'#13#10         // 2nd Call-ID
                 + 'CSeq: 1 OPTIONS'#13#10                         // 2nd CSeq
                 + 'Contact: sip:host.company.com'#13#10
                 + 'Expires: 0xpires: 0sip:host.company.com'#13#10 // mangled
                 + 'Expires: 0'#13#10
                 + 'Contact: sip:host.company.com'#13#10
                 + #13#10;

  Self.CheckingResponseEvent := Self.CheckForBadRequest;
  Self.SendFromLowTransport(StringReplace(TortureTest35, '%s', Destination, [rfReplaceAll]));

  Self.WaitForSignaled(Self.RejectedMessageEvent);
end;

procedure TestTIdSipTransport.TestTortureTest40;
var
  Destination:   String;
  TortureTest40: String;
begin
  //   This is an illegal invite as the display names in the To and From
  //   headers contain non-token characters but are unquoted.
  //
  //   A server may be intelligent enough to cope with this but may also
  //   return a 400 response with an appropriate reason phrase.

  // We accept these messages since they're easy to safely fudge.

  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Destination := Self.HighPortLocation.IPAddress + ':' + IntToStr(Self.HighPortLocation.Port);
  TortureTest40 := 'INVITE sip:t.watson@%s SIP/2.0'#13#10
                + 'Via:     SIP/2.0/' + Self.HighPortTransport.GetTransportType + ' c.bell-tel.com:5060;branch=z9hG4bKkdjuw'#13#10
                + 'Max-Forwards:      70'#13#10
                + 'From:    Bell, Alexander <sip:a.g.bell@bell-tel.com>;tag=43'#13#10
                + 'To:      Watson, Thomas <sip:t.watson@%s>'#13#10
                + 'Call-ID: 31415@c.bell-tel.com'#13#10
                + 'CSeq:    1 INVITE'#13#10
                + #13#10;

  Self.SendFromLowTransport(StringReplace(TortureTest40, '%s', Destination, [rfReplaceAll]));

  Self.WaitForSignaled;

  Check(Self.ReceivedRequest,
        Self.HighPortTransport.ClassName + ': Request not received');
end;

procedure TestTIdSipTransport.TestTortureTest41;
var
  Destination:   String;
  TortureTest41: String;
begin
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Destination := Self.HighPortLocation.IPAddress + ':' + IntToStr(Self.HighPortLocation.Port);

  //   This is an illegal INVITE as the SIP Protocol version is unknown.
  //
  //   The server should respond to the request with a bad version error.

  // However, we let the transports pass up this message so that the
  // Transaction-User layer can reject this message in a centralised fashion.
  // The message is syntactically valid, after all, if semantically invalid.

  TortureTest41 := 'INVITE sip:t.watson@%s SIP/7.0'#13#10
                 + 'Via:     SIP/2.0/' + Self.HighPortTransport.GetTransportType + ' c.bell-tel.com;branch=z9hG4bKkdjuw'#13#10
                 + 'Max-Forwards:     70'#13#10
                 + 'From:    A. Bell <sip:a.g.bell@bell-tel.com>;tag=qweoiqpe'#13#10
                 + 'To:      T. Watson <sip:t.watson@%s>'#13#10
                 + 'Call-ID: 31417@c.bell-tel.com'#13#10
                 + 'CSeq:    1 INVITE'#13#10
                 + #13#10;

  Self.SendFromLowTransport(StringReplace(TortureTest41, '%s', Destination, [rfReplaceAll]));

  Self.WaitForSignaled;

  Check(Self.ReceivedRequest,
        Self.HighPortTransport.ClassName + ': Request not received');
end;

procedure TestTIdSipTransport.TestUseRport;
begin
  // The request does indeed come from localhost, but we want a FQDN here to
  // force the receiving UA to add an rport parameter.
  Self.Request.LastHop.SentBy := 'localhost';

  Self.ExceptionMessage := 'Waiting for rport request';
  Self.LowPortTransport.UseRport := true;
  Self.CheckingRequestEvent := Self.CheckUseRport;
  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;
end;

//******************************************************************************
//* TIdSipTestTransportListener                                                *
//******************************************************************************
//* TIdSipTestTransportListener Public methods *********************************

constructor TIdSipTestTransportListener.Create;
begin
  inherited Create;

  Self.fException        := false;
  Self.fReceivedRequest  := false;
  Self.fReceivedResponse := false;
  Self.fRejectedMessage  := false;
end;

//* TIdSipTestTransportListener Private methods ********************************

procedure TIdSipTestTransportListener.OnException(FailedMessage: TIdSipMessage;
                                                  E: Exception;
                                                  const Reason: String);
begin
  Self.fException          := true;
  Self.fExceptionParam     := E;
  Self.fFailedMessageParam := FailedMessage;
  Self.fReasonParam        := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnException');
end;

procedure TIdSipTestTransportListener.OnReceiveRequest(Request: TIdSipRequest;
                                                       Receiver: TIdSipTransport;
                                                       Source: TIdConnectionBindings);
begin
  Self.fReceiverParam   := Receiver;
  Self.fRequestParam    := Request;
  Self.fReceivedRequest := true;
  Self.fSourceParam      := Source;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveRequest');
end;

procedure TIdSipTestTransportListener.OnReceiveResponse(Response: TIdSipResponse;
                                                        Receiver: TIdSipTransport;
                                                        Source: TIdConnectionBindings);
begin
  Self.fReceiverParam    := Receiver;
  Self.fResponseParam    := Response;
  Self.fReceivedResponse := true;
  Self.fSourceParam      := Source;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveResponse');
end;

procedure TIdSipTestTransportListener.OnRejectedMessage(const Msg: String;
                                                        const Reason: String;
                                                        Source: TIdConnectionBindings);
begin
  Self.fMsgParam        := Msg;
  Self.fReasonParam     := Reason;
  Self.fRejectedMessage := true;
  Self.fSourceParam     := Source;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnRejectedMessage');

end;

//******************************************************************************
//* TIdSipTestTransportSendingListener                                         *
//******************************************************************************
//* TIdSipTestTransportSendingListener Public methods **************************

constructor TIdSipTestTransportSendingListener.Create;
begin
  inherited Create;

  Self.fResponseParam := TIdSipResponse.Create;
  Self.fRequestParam  := TIdSipRequest.Create;
  Self.fSentRequest   := false;
  Self.fSentResponse  := false;
end;

destructor TIdSipTestTransportSendingListener.Destroy;
begin
  Self.fRequestParam.Free;
  Self.fResponseParam.Free;

  inherited Destroy;
end;

//* TIdSipTestTransportSendingListener Private methods *************************


procedure TIdSipTestTransportSendingListener.OnSendRequest(Request: TIdSipRequest;
                                                           Sender: TIdSipTransport;
                                                           Binding: TIdConnectionBindings);
begin
  Self.fBindingParam := Binding;
  Self.fRequestParam.Assign(Request);
  Self.fSenderParam  := Sender;
  Self.fSentRequest  := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnSendRequest');
end;

procedure TIdSipTestTransportSendingListener.OnSendResponse(Response: TIdSipResponse;
                                                            Sender: TIdSipTransport;
                                                            Binding: TIdConnectionBindings);
begin
  Self.fBindingParam := Binding;
  Self.fResponseParam.Assign(Response);
  Self.fSenderParam  := Sender;
  Self.fSentResponse := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnSendResponse');
end;

//******************************************************************************
//* TConnectionListener                                                        *
//******************************************************************************
//* TConnectionListener Public methods *****************************************

constructor TConnectionListener.Create;
begin
  inherited Create;

  Self.fConnectionClosed := false;
  Self.fConnectionOpened := false;

  Self.fConnectionParam := TIdConnectionBindings.Create;
end;

destructor TConnectionListener.Destroy;
begin
  Self.fConnectionParam.Free;

  inherited Destroy;
end;

//* TConnectionListener Private methods ****************************************

procedure TConnectionListener.OnConnection(Transport: TIdSipTransport;
                                           Connection: TIdConnectionBindings);
begin
  Self.fConnectionOpened := true;
  Self.fConnectionParam.Assign(Connection);
  Self.fTransportParam    := Transport;
end;

procedure TConnectionListener.OnDisconnection(Transport: TIdSipTransport;
                                              Connection: TIdConnectionBindings);
begin
  Self.fConnectionClosed := true;
  Self.fConnectionParam.Assign(Connection);
  Self.fTransportParam    := Transport;
end;

initialization
  ServerThatInstantiatesGStack := TIdTCPServer.Create(nil);
finalization
  ServerThatInstantiatesGStack.Free;
end.
