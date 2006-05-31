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
  IdSipLocator, IdSipMessage, IdSipTransport, IdTimerQueue, SyncObjs,
  SysUtils, TestFrameworkEx;

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
                                 TestCase: TestTIdSipTransport);

  public
    constructor Create(TransportType: TIdSipTransportClass;
                       TestCase: TestTIdSipTransport;
                       FinishedEvent: TEvent); reintroduce;
    destructor  Destroy; override;

    procedure Terminate; override;

    property HighPortTransport: TIdSipTransport read fHighPortTransport;
    property LowPortTransport:  TIdSipTransport read fLowPortTransport;
  end;

  TestTIdSipTransport = class(TThreadingTestCase,
                              IIdSipTransportListener,
                              IIdSipTransportSendingListener)
  private
    LastSentResponse: TIdSipResponse;
  protected
    CheckingRequestEvent:  TIdSipRequestEvent;
    CheckingResponseEvent: TIdSipResponseEvent;
    EmptyListEvent:        TEvent;
    FinishedTimer:         TEvent;
    HighPortLocation:      TIdSipLocation;
    Lock:                  TCriticalSection;
    LowPortLocation:       TIdSipLocation;
    Parser:                TIdSipParser;
    ReceivedRequest:       Boolean;
    ReceivedResponse:      Boolean;
    RecvdRequest:          TIdSipRequest;
    RejectedMessage:       Boolean;
    RejectedMessageEvent:  TEvent;
    RejectedMessageReason: String;
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
    procedure CheckForTrying(Sender: TObject;
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
    procedure OnException(FailedMessage: TIdSipMessage;
                          E: Exception;
                          const Reason: String);
    procedure OnEmpty(Sender: TIdTimerQueue);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport;
                               Source: TIdSipConnectionBindings); virtual;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdSipConnectionBindings); virtual;
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Destination: TIdSipLocation); virtual;
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Destination: TIdSipLocation); virtual;
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
    procedure TestAddBinding;
    procedure TestAddBindingDoesntStartStoppedTransport;
    procedure TestAddBindingRestartsStartedTransport;
    procedure TestCanReceiveRequest;
    procedure TestCanReceiveResponse;
    procedure TestCanReceiveUnsolicitedResponse;
    procedure TestClearBindings;
    procedure TestClearBindingsDoesntStartStoppedTransport;
    procedure TestClearBindingLeavesOneBindingBehind;
    procedure TestClearBindingRestartsStartedTransport;
    procedure TestDestructionUnregistersTransport;
    procedure TestHasBinding;
    procedure TestInstantiationRegistersTransport;
    procedure TestIsNull; virtual;
    procedure TestIsRunning;
    procedure TestDiscardMalformedMessage;
    procedure TestDiscardRequestWithInconsistentTransport;
    procedure TestDiscardResponseWithInconsistentTransport;
    procedure TestDiscardResponseWithUnknownSentBy;
    procedure TestDontDiscardUnknownSipVersion;
    procedure TestReceivedParamDifferentIPv4SentBy;
    procedure TestReceivedParamFQDNSentBy;
    procedure TestReceivedParamIPv4SentBy;
    procedure TestRemoveBinding;
    procedure TestRemoveBindingDoesntStartStoppedTransport;
    procedure TestRemoveBindingNoSuchBinding;
    procedure TestRemoveBindingRestartsStartedTransport;
    procedure TestSendRequest;
    procedure TestSendRequestFromNonStandardPort;
    procedure TestSendRequestTopVia;
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

implementation

uses
  IdStack, IdSipConsts, IdTcpServer, TestFramework, TestFrameworkSip,
  TestMessages;

var
  ServerThatInstantiatesGStack: TIdTcpServer;

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

procedure TTransportTestTimerQueue.Terminate;
begin
  Self.LowPortTransport.Stop;
  Self.HighPortTransport.Stop;

  inherited Terminate;
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

  Transport.Bindings[0].IP   := Address;
  Transport.Bindings[0].Port := Port;
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

  TIdSipTransportRegistry.RegisterTransportType(Self.TransportType.GetTransportType,
                                              Self.TransportType);

  Self.ExceptionMessage := Self.TransportType.ClassName + ': ' + Self.ExceptionMessage;

  Self.EmptyListEvent       := TSimpleEvent.Create;
  Self.FinishedTimer        := TSimpleEvent.Create;
  Self.RejectedMessageEvent := TSimpleEvent.Create;
  Self.SendEvent            := TSimpleEvent.Create;

  Self.Timer := TTransportTestTimerQueue.Create(Self.TransportType, Self, Self.FinishedTimer);
  Check(Self.Timer <> nil,
        'Timer didn''t instantiate: the previous test likely failed without '
      + 'killing the transports');
  Self.Timer.OnEmpty := Self.OnEmpty;

  Self.Lock             := TCriticalSection.Create;
  Self.LastSentResponse := TIdSipResponse.Create;

  Check(Self.HighPortTransport <> nil,
        'Something went wrong creating the TTransportTestTimerQueue');
  Self.HighPortLocation := TIdSipLocation.Create(Self.TransportType.GetTransportType,
                                                 Self.HighPortTransport.Bindings[0].IP,
                                                 Self.HighPortTransport.Bindings[0].Port);
  Self.LowPortLocation := TIdSipLocation.Create(Self.TransportType.GetTransportType,
                                                Self.LowPortTransport.Bindings[0].IP,
                                                Self.LowPortTransport.Bindings[0].Port);

  Self.Request := TIdSipTestResources.CreateLocalLoopRequest;
  Self.Request.LastHop.SentBy    := Self.LowPortTransport.Bindings[0].IP;
  Self.Request.LastHop.Transport := Self.LowPortTransport.GetTransportType;
  Self.Request.RequestUri.Host   := Self.HighPortTransport.HostName;
  Self.Request.RequestUri.Port   := Self.HighPortTransport.Bindings[0].Port;

  Self.Response := TIdSipTestResources.CreateLocalLoopResponse;
  Self.Response.LastHop.Transport := Self.HighPortTransport.GetTransportType;
  Self.Response.LastHop.Port      := Self.HighPortTransport.Bindings[0].Port;

  Self.RecvdRequest := TIdSipRequest.Create;

  Self.ReceivedRequest  := false;
  Self.ReceivedResponse := false;
  Self.RejectedMessage  := false;
  RejectedMessageReason := '';
  Self.WrongServer      := false;
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

    Self.RecvdRequest.Free;
    Self.Response.Free;
    Self.Request.Free;

    Self.LowPortLocation.Free;
    Self.HighPortLocation.Free;

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

    Self.FinishedTimer.Free;

    TIdSipTransportRegistry.UnregisterTransportType(Self.TransportType.GetTransportType);

    inherited TearDown;
  end;
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

procedure TestTIdSipTransport.CheckForTrying(Sender: TObject;
                                             R: TIdSipResponse;
                                             ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.CheckResponse(R, SIPTrying);
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
                Self.HighPortTransport.Bindings[0].Port,
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
    CheckEquals(Self.LowPortTransport.Bindings[0].Port,
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
                                               Source: TIdSipConnectionBindings);
begin
  Self.RecvdRequest.Assign(Request);

  if Assigned(Self.CheckingRequestEvent) then
    Self.CheckingRequestEvent(Receiver, Request, Source);
end;

procedure TestTIdSipTransport.OnReceiveResponse(Response: TIdSipResponse;
                                                Receiver: TIdSipTransport;
                                                Source: TIdSipConnectionBindings);
begin
  if Assigned(Self.CheckingResponseEvent) then
    Self.CheckingResponseEvent(Receiver, Response, Source);
end;

procedure TestTIdSipTransport.OnRejectedMessage(const Msg: String;
                                                const Reason: String);
begin
  Self.RejectedMessage       := true;
  Self.RejectedMessageReason := Reason;
  Self.ExceptionMessage      := Reason;
  Self.RejectedMessageEvent.SetEvent;
end;

procedure TestTIdSipTransport.OnSendRequest(Request: TIdSipRequest;
                                            Sender: TIdSipTransport;
                                            Destination: TIdSipLocation);
begin
end;

procedure TestTIdSipTransport.OnSendResponse(Response: TIdSipResponse;
                                             Sender: TIdSipTransport;
                                             Destination: TIdSipLocation);
begin
  Self.Lock.Acquire;
  try
    if Assigned(Self.LastSentResponse) then begin
      Self.LastSentResponse.Assign(Response);
      Self.SendEvent.SetEvent;
    end;
  finally
    Self.Lock.Release;
  end;
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
                         Self.HighPortTransport.Bindings[0].IP
                       + ':'
                       + IntToStr(Self.HighPortTransport.Bindings[0].Port), [rfReplaceAll]);

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

//* TestTIdSipTransport Published methods **************************************

procedure TestTIdSipTransport.TestAddBinding;
var
  NewPort:      Cardinal;
  OriginalPort: Cardinal;
begin
  OriginalPort := Self.LowPortTransport.Bindings[0].Port;
  NewPort      := OriginalPort + 1;

  Self.CheckServerNotOnPort(Self.LowPortTransport.Bindings[0].IP,
                            NewPort,
                            'Sanity check: nothing should be running on port ' + IntToStr(NewPort));

  Self.LowPortTransport.AddBinding(Self.LowPortTransport.Bindings[0].IP, NewPort);
  Self.CheckServerOnPort(Self.LowPortTransport.Bindings[0].IP,
                         NewPort,
                         'Nothing running on port ' + IntToStr(NewPort) + '; binding not added');
end;

procedure TestTIdSipTransport.TestAddBindingDoesntStartStoppedTransport;
begin
  Self.LowPortTransport.Stop;
  Self.LowPortTransport.AddBinding(Self.LowPortTransport.Bindings[0].IP,
                                   Self.LowPortTransport.Bindings[0].Port + 1);
  Check(not Self.LowPortTransport.IsRunning,
        'AddBinding started the server');
end;

procedure TestTIdSipTransport.TestAddBindingRestartsStartedTransport;
begin
  Self.LowPortTransport.Start;
  Self.LowPortTransport.AddBinding(Self.LowPortTransport.Bindings[0].IP,
                                   Self.LowPortTransport.Bindings[0].Port + 1);
  Check(Self.LowPortTransport.IsRunning,
        'AddBinding stopped the server');
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

  Self.HighPortTransport.Send(Self.Response, Self.LowPortLocation);

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

  Address   := Self.LowPortTransport.Bindings[0].IP;
  FirstPort := Self.LowPortTransport.Bindings[0].Port + 1;

  NewPort := FirstPort + 1;
  Self.LowPortTransport.AddBinding(Address, NewPort);

  Self.LowPortTransport.ClearBindings;
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

procedure TestTIdSipTransport.TestClearBindingLeavesOneBindingBehind;
begin
  Self.LowPortTransport.AddBinding(Self.LowPortTransport.Bindings[0].IP,
                                   Self.LowPortTransport.Bindings[0].Port + 1);
  Self.LowPortTransport.AddBinding(Self.LowPortTransport.Bindings[0].IP,
                                   Self.LowPortTransport.Bindings[0].Port + 2);
  Self.LowPortTransport.ClearBindings;

  Check(Self.LowPortTransport.Bindings.Count > 0,
        'Indy''s behaviour changed: usually the server will recreate a default binding');
end;

procedure TestTIdSipTransport.TestClearBindingRestartsStartedTransport;
begin
  Self.LowPortTransport.Start;
  Self.LowPortTransport.ClearBindings;

  Check(Self.LowPortTransport.IsRunning,
        'ClearBindings didn''t restart the transport');
end;

procedure TestTIdSipTransport.TestDestructionUnregistersTransport;
var
  ID: String;
  T:  TIdSipTransport;
begin
  T := Self.TransportType.Create;
  try
    ID := T.ID;
  finally
    T.Free;
  end;

  Check(nil = TIdSipTransportRegistry.TransportFor(ID),
        Self.HighPortTransport.ClassName + ' didn''t unregister when Freed');
end;

procedure TestTIdSipTransport.TestHasBinding;
var
  Address: String;
  Port:    Cardinal;
begin
  Address := Self.LowPortTransport.Bindings[0].IP;
  Port    := Self.LowPortTransport.Bindings[0].Port + 1;

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
    Self.WaitForSignaled(Self.SendEvent);

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
  Self.Response.LastHop.Received := Self.LowPortTransport.Bindings[0].IP;
  Self.SendMessage(Self.Response.AsString);

  Self.WaitForSignaled;
  Check(not Self.ReceivedResponse,
        Self.HighPortTransport.ClassName
      + ': Response not silently discarded');
  Check(Self.RejectedMessage,
        Self.HighPortTransport.ClassName
      + ': Rejected message event didn''t fire');
end;

procedure TestTIdSipTransport.TestDontDiscardUnknownSipVersion;
var
  Destination: String;
begin
  // The Transaction-User level handles rejecting these messages.
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.ExceptionMessage := 'Waiting for request to arrive';

  Destination := Self.HighPortTransport.Bindings[0].IP + ':' + IntToStr(Self.HighPortTransport.Bindings[0].Port);
  Self.SendMessage(StringReplace(TortureTest41, '%s', Destination, [rfReplaceAll]));

  Self.WaitForSignaled(Self.ClassName
                    + ': Didn''t receive a message with an unknown SIP-Version (timeout)');
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
  Self.HighPortTransport.HostName := Self.HighPortTransport.Bindings[0].IP;
  Self.HighPortTransport.Send(Self.Request, Self.LowPortLocation);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestRemoveBinding;
var
  NewPort:      Cardinal;
  OriginalPort: Cardinal;
begin
  OriginalPort := Self.LowPortTransport.Bindings[0].Port;
  NewPort      := OriginalPort + 1;

  Self.CheckServerNotOnPort(Self.LowPortTransport.Bindings[0].IP,
                            NewPort,
                            'Sanity check: nothing should be running on port ' + IntToStr(NewPort));

  Self.LowPortTransport.AddBinding(Self.LowPortTransport.Bindings[0].IP, NewPort);
  Self.LowPortTransport.RemoveBinding(Self.LowPortTransport.Bindings[0].IP, NewPort);

  Self.CheckServerNotOnPort(Self.LowPortTransport.Bindings[0].IP,
                            NewPort,
                            'Something running on port ' + IntToStr(NewPort)
                          + '; binding not removed');
end;

procedure TestTIdSipTransport.TestRemoveBindingDoesntStartStoppedTransport;
begin
  Self.LowPortTransport.Stop;
  Self.LowPortTransport.AddBinding(Self.LowPortTransport.Bindings[0].IP,
                                   Self.LowPortTransport.Bindings[0].Port + 1);
  Self.LowPortTransport.RemoveBinding(Self.LowPortTransport.Bindings[0].IP,
                                      Self.LowPortTransport.Bindings[0].Port + 1);

  Check(not Self.LowPortTransport.IsRunning,
        'RemoveBinding restarted the transport');
end;

procedure TestTIdSipTransport.TestRemoveBindingNoSuchBinding;
begin
  // Check that calling RemoveBinding for a non-existent doesn't blow up.

  Self.LowPortTransport.RemoveBinding(Self.LowPortTransport.Bindings[0].IP,
                                      Self.LowPortTransport.Bindings[0].Port + 1);
end;

procedure TestTIdSipTransport.TestRemoveBindingRestartsStartedTransport;
begin
  Self.LowPortTransport.Start;

  Self.LowPortTransport.AddBinding(Self.LowPortTransport.Bindings[0].IP,
                                   Self.LowPortTransport.Bindings[0].Port + 1);
  Self.LowPortTransport.RemoveBinding(Self.LowPortTransport.Bindings[0].IP,
                                      Self.LowPortTransport.Bindings[0].Port + 1);

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
end;

procedure TestTIdSipTransport.TestSendRequestFromNonStandardPort;
begin
  Self.Request.RequestUri.Host := Self.LowPortTransport.HostName;
  Self.Request.RequestUri.Port := Self.LowPortTransport.Bindings[0].Port;
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
  Self.Response.LastHop.Port := Self.LowPortTransport.Bindings[0].Port;
  Self.CheckingResponseEvent := Self.CheckSendResponseFromNonStandardPort;
  Self.HighPortTransport.Send(Self.Response, Self.LowPortLocation);

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
          Self.LowPortTransport.RemoveTransportListener(Self);
          Self.LowPortTransport.AddTransportListener(Self);

          Self.Response.LastHop.Received := Self.LowPortTransport.Bindings[0].IP;
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

  Destination := Self.HighPortTransport.Bindings[0].IP + ':' + IntToStr(Self.HighPortTransport.Bindings[0].Port);

  Self.CheckingResponseEvent := Self.CheckForBadRequest;
  Self.SendFromLowTransport(StringReplace(TortureTest35, '%s', Destination, [rfReplaceAll]));

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestTortureTest40;
begin
  // Illegal >1 SP between elements of the Request-Line.
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.SendFromLowTransport(TortureTest40);

  Self.WaitForSignaled;

  Check(Self.ReceivedRequest,
        Self.HighPortTransport.ClassName + ': Request not received');
end;

procedure TestTIdSipTransport.TestUseRport;
begin
  Self.ExceptionMessage := 'Waiting for rport request';
  Self.LowPortTransport.UseRport := true;
  Self.CheckingRequestEvent := Self.CheckUseRport;
  Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

  Self.WaitForSignaled;
end;

initialization
  ServerThatInstantiatesGStack := TIdTCPServer.Create(nil);
finalization
  ServerThatInstantiatesGStack.Free;
end.
