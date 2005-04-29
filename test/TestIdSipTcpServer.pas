{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipTcpServer;

interface

uses
  IdSipLocator, IdSipMessage, IdSipTcpClient, IdSipTcpServer, IdTCPClient,
  IdTCPConnection, IdTCPServer, IdTimerQueue, SyncObjs, SysUtils, TestFramework,
  TestFrameworkSip;

type
  TIdTcpClientClass = class of TIdTcpClient;

  TIdSipRequestEvent = procedure(Sender: TObject;
                                 R: TIdSipRequest) of object;

  TestTIdSipTcpServer = class(TTestCaseSip, IIdSipMessageListener)
  private
    EmptyListEvent:           TEvent;
    NotifiedMalformedMessage: Boolean;

    procedure AcknowledgeEvent(Sender: TObject;
                               Request: TIdSipRequest); overload;
    procedure AcknowledgeEvent(Sender: TObject;
                               Response: TIdSipResponse;
                               ReceivedFrom: TIdSipConnectionBindings); overload;
    procedure CheckInternalServerError(Sender: TObject;
                                       Response: TIdSipResponse;
                                       ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckMultipleMessages(Sender: TObject;
                                    Request: TIdSipRequest);
    procedure CheckMethodEvent(Sender: TObject;
                               Request: TIdSipRequest);
    procedure OnEmpty(Sender: TIdTimerQueue);
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnMalformedMessage(const Msg: String;
                                 const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdSipConnectionBindings);
    procedure RaiseException(Sender: TObject;
                             Request: TIdSipRequest);
  protected
    CheckingRequestEvent:   TIdSipRequestEvent;
    CheckingResponseEvent:  TIdSipResponseEvent;
    Client:                 TIdTcpClient;
    ClientReceivedResponse: Boolean;
    HighPortLocation:       TIdSipLocation;
    HighPortServer:         TIdSipTcpServer;
    LowPortServer:          TIdSipTcpServer;
    MethodCallCount:        Cardinal;
    ServerReceivedResponse: Boolean;
    SipClient:              TIdSipTcpClient;
    Timer:                  TIdThreadedTimerQueue;

    function ServerType: TIdSipTcpServerClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddMessageListener;
    procedure TestInternalServerError;
    procedure TestLeadingEmptyLines;
    procedure TestListenerReceiveRequest;
    procedure TestListenerReceiveResponse;
    procedure TestMethodEvent;
    procedure TestMultipleMessages;
    procedure TestRemoveMessageListener;
  end;

const
  BasicRequest = 'INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0'#13#10
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
               + 'I am a message. Hear me roar!';
  ViaFQDN        = 'gw1.leo-ix.org';
  ViaIP          = '127.0.0.1';
  ViaDifferentIP = '196.25.1.1';
  DefaultTimeout = 1000;
  LocalHost      = '127.0.0.1';

implementation

uses
  Classes, IdGlobal, IdSocketHandle, IdSipConsts, IdSimpleParser, IdStack,
  TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTcpServer unit tests');
  Result.AddSuite(TestTIdSipTcpServer.Suite);
end;

//******************************************************************************
//* TestTIdSipTcpServer                                                        *
//******************************************************************************
//* TestTIdSipTcpServer Public methods *****************************************

procedure TestTIdSipTcpServer.SetUp;
var
  Binding: TIdSocketHandle;
begin
  inherited SetUp;

  Self.EmptyListEvent := TSimpleEvent.Create;
  Self.Timer := TIdThreadedTimerQueue.Create(false);
  Self.Timer.OnEmpty := Self.OnEmpty;

  Self.Client         := TIdTcpClient.Create(nil);
  Self.HighPortServer := Self.ServerType.Create(nil);
  Self.LowPortServer  := Self.ServerType.Create(nil);
  Self.SipClient      := Self.HighPortServer.CreateClient;

  Self.Client.Host := LocalHost;
  Self.Client.Port := LowPortServer.DefaultPort;

  Self.ClientReceivedResponse := false;
  Self.MethodCallCount        := 0;
  Self.ServerReceivedResponse := false;

  Self.LowPortServer.Timer := Self.Timer;
  Self.LowPortServer.Bindings.Clear;
  Binding := LowPortServer.Bindings.Add;
  Binding.IP   := LocalHost;
  Binding.Port := IdPORT_SIP;

  Self.HighPortServer.Timer := Self.Timer;
  Self.HighPortServer.Bindings.Clear;
  Binding := Self.HighPortServer.Bindings.Add;
  Binding.IP   := GStack.LocalAddress;
  Binding.Port := IdPORT_SIP + 10000;

  Self.HighPortLocation := TIdSipLocation.Create(TcpTransport,
                                                 Binding.IP,
                                                 Binding.Port);

  Self.LowPortServer.Active  := true;
  Self.HighPortServer.Active := true;

  Self.LowPortServer.AddMessageListener(Self);
  Self.HighPortServer.AddMessageListener(Self);

  Self.NotifiedMalformedMessage := false;
end;

procedure TestTIdSipTcpServer.TearDown;
var
  WaitTime: Cardinal;
begin
  // Wait for all scheduled events to execute
  WaitTime := Self.Timer.DefaultTimeout * 3 div 2;

  Self.Timer.Terminate;
  Self.EmptyListEvent.WaitFor(WaitTime);

  Self.HighPortServer.RemoveMessageListener(Self);
  Self.LowPortServer.RemoveMessageListener(Self);

  Self.HighPortServer.Active := false;
  Self.LowPortServer.Active := false;

  Self.HighPortServer.DestroyClient(Self.SipClient);
  Self.LowPortServer.Free;
  Self.HighPortLocation.Free;
  Self.HighPortServer.Free;
  Self.Client.Free;

  Self.EmptyListEvent.Free;

  inherited TearDown;
end;

//* TestTIdSipTcpServer Protected methods **************************************

function TestTIdSipTcpServer.ServerType: TIdSipTcpServerClass;
begin
  Result := TIdSipTcpServer;
end;

//* TestTIdSipTcpServer Private methods ****************************************

procedure TestTIdSipTcpServer.AcknowledgeEvent(Sender: TObject;
                                               Request: TIdSipRequest);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTcpServer.AcknowledgeEvent(Sender: TObject;
                                               Response: TIdSipResponse;
                                               ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTcpServer.CheckInternalServerError(Sender: TObject;
                                                       Response: TIdSipResponse;
                                                       ReceivedFrom: TIdSipConnectionBindings);
begin
  CheckEquals(SIPInternalServerError, Response.StatusCode, 'Status-Code');
end;

procedure TestTIdSipTcpServer.CheckMultipleMessages(Sender: TObject;
                                                    Request: TIdSipRequest);
begin
  try
    Inc(Self.MethodCallCount);

    // Otherwise we'll set the event the very first time we parse a message.
    // We expect this method to be called TWICE though.
    if (Self.MethodCallCount > 1) then
      Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpServer.CheckMethodEvent(Sender: TObject;
                                               Request: TIdSipRequest);
var
  Expected: TIdSipHeaders;
begin
  try
    CheckEquals('INVITE',                                 Request.Method,         'Method');
    CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Request.RequestUri.URI, 'RequestUri');
    CheckEquals('SIP/2.0',                                Request.SIPVersion,     'SipVersion');
    CheckEquals(29,                                       Request.ContentLength,  'ContentLength');
    CheckEquals('a84b4c76e66710@gw1.leo-ix.org',          Request.CallID,         'CallID');
    CheckEquals(70,                                       Request.MaxForwards,    'Max-Forwards');

    Expected := TIdSipHeaders.Create;
    try
      Expected.Add(ViaHeaderFull).Value           := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
      Expected.Add(MaxForwardsHeader).Value       := '70';
      Expected.Add(ToHeaderFull).Value            := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';
      Expected.Add(FromHeaderFull).Value          := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
      Expected.Add(CallIDHeaderFull).Value        := 'a84b4c76e66710@gw1.leo-ix.org';
      Expected.Add(CSeqHeader).Value              := '314159 INVITE';
      Expected.Add(ContactHeaderFull).Value       := 'sip:wintermute@tessier-ashpool.co.luna';
      Expected.Add(ContentTypeHeaderFull).Value   := 'text/plain';
      Expected.Add(ContentLengthHeaderFull).Value := '29';

      Check(Expected.Equals(Request.Headers), 'Headers');
    finally
      Expected.Free;
    end;
    CheckEquals('I am a message. Hear me roar!', Request.Body, 'message-body');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpServer.OnEmpty(Sender: TIdTimerQueue);
begin
  Self.EmptyListEvent.SetEvent;
end;

procedure TestTIdSipTcpServer.OnException(E: Exception;
                                          const Reason: String);
begin
  Self.ExceptionType    := ExceptClass(E.ClassType);
  Self.ExceptionMessage := E.Message + ' caused by ''' + Reason + '''';
end;

procedure TestTIdSipTcpServer.OnMalformedMessage(const Msg: String;
                                                 const Reason: String);
begin
  Self.NotifiedMalformedMessage := true;
end;

procedure TestTIdSipTcpServer.OnReceiveRequest(Request: TIdSipRequest;
                                               ReceivedFrom: TIdSipConnectionBindings);
begin
  if Assigned(Self.CheckingRequestEvent) then
    Self.CheckingRequestEvent(Self, Request);
end;

procedure TestTIdSipTcpServer.OnReceiveResponse(Response: TIdSipResponse;
                                                ReceivedFrom: TIdSipConnectionBindings);
begin
  if Assigned(Self.CheckingResponseEvent) then
    Self.CheckingResponseEvent(Self, Response, ReceivedFrom);
end;

procedure TestTIdSipTcpServer.RaiseException(Sender: TObject;
                                             Request: TIdSipRequest);
begin
  raise Exception.Create('RaiseException');
end;

//* TestTIdSipTcpServer Published methods **************************************

procedure TestTIdSipTcpServer.TestAddMessageListener;
begin
  // SetUp already adds Self as a listener to LowPortServer
  Self.CheckingRequestEvent := Self.AcknowledgeEvent;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(BasicRequest);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTcpServer.TestInternalServerError;
var
  Request: TIdSipRequest;
begin
  Self.CheckingRequestEvent := Self.RaiseException;

  Request := TIdSipMessage.ReadRequestFrom(LocalLoopRequest);
  try
    SipClient.OnResponse  := Self.CheckInternalServerError;
    SipClient.Host        := '127.0.0.1';
    SipClient.Port        := LowPortServer.DefaultPort;
    SipClient.ReadTimeout := 1000;

    SipClient.Connect;
    try
      SipClient.Send(Request);
    finally
      SipClient.Disconnect;
    end;
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipTcpServer.TestLeadingEmptyLines;
begin
  Self.CheckingRequestEvent := Self.CheckMethodEvent;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(#13#10#13#10#13#10
                  + Format(BasicRequest, [ViaFQDN]));

  Self.WaitForSignaled;
end;

procedure TestTIdSipTcpServer.TestListenerReceiveRequest;
var
  Listener: TIdSipTestMessageListener;
begin
  Self.LowPortServer.RemoveMessageListener(Self);
  Self.CheckingRequestEvent := Self.AcknowledgeEvent;

  Listener := TIdSipTestMessageListener.Create;
  try
    Self.LowPortServer.AddMessageListener(Listener);
    Self.LowPortServer.AddMessageListener(Self);

    Self.Client.Connect(DefaultTimeout);
    Self.Client.Write(BasicRequest);

    Self.WaitForSignaled;
    Check(Listener.ReceivedRequest, 'Not all listeners received the request');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTcpServer.TestListenerReceiveResponse;
var
  Listener: TIdSipTestMessageListener;
begin
  Self.CheckingResponseEvent := Self.AcknowledgeEvent;

  Listener := TIdSipTestMessageListener.Create;
  try
    Self.LowPortServer.AddMessageListener(Listener);
    Self.LowPortServer.AddMessageListener(Self);

    Self.Client.Connect(DefaultTimeout);
    Self.Client.Write(BasicResponse);

    Self.WaitForSignaled;
    Check(Listener.ReceivedResponse, 'Not all listeners received the Response');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTcpServer.TestMethodEvent;
begin
  Self.CheckingRequestEvent := Self.CheckMethodEvent;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(Format(BasicRequest, [ViaFQDN]));

  Self.WaitForSignaled;
end;

procedure TestTIdSipTcpServer.TestMultipleMessages;
begin
  Self.CheckingRequestEvent := Self.CheckMultipleMessages;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(Format(BasicRequest, [ViaFQDN])
                  + Format(BasicRequest, [ViaFQDN]));

  Self.WaitForSignaled;
  CheckEquals(2, Self.MethodCallCount, 'Method call count')
end;

procedure TestTIdSipTcpServer.TestRemoveMessageListener;
var
  Listener: TIdSipTestMessageListener;
begin
  Self.CheckingRequestEvent := Self.AcknowledgeEvent;

  Listener := TIdSipTestMessageListener.Create;
  try
    // This juggle ensures that the Listener gets the notification first.
    Self.HighPortServer.RemoveMessageListener(Self);
    Self.HighPortServer.AddMessageListener(Listener);
    Self.HighPortServer.RemoveMessageListener(Listener);
    Self.HighPortServer.AddMessageListener(Self);

    Self.Client.Connect(DefaultTimeout);
    Self.Client.Write(BasicRequest);

    Self.WaitForSignaled;
    Check(not Listener.ReceivedRequest, 'Listener not removed: ' + Self.ClassName);
  finally
    Listener.Free;
  end;
end;

initialization
  RegisterTest('SIP Server using TCP', Suite);
end.
