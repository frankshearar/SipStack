unit TestIdSipTcpServer;

interface

uses
  IdSipMessage, IdSipTcpClient, IdSipTcpServer, IdTCPClient, IdTCPConnection,
  IdTCPServer, SyncObjs, SysUtils, TestFramework, TestFrameworkEx;

type
  TIdTcpClientClass = class of TIdTcpClient;

  TestTIdSipConnectionTableEntry = class(TTestCase)
  published
    procedure TestCreate;
  end;

  TestTIdSipConnectionTable = class(TTestCase)
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

  TIdSipRequestEvent = procedure(Sender: TObject;
                                 R: TIdSipRequest) of object;

  TestTIdSipTcpServer = class(TThreadingTestCase, IIdSipMessageListener)
  private
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
    procedure CheckSendResponsesDownClosedConnection(Sender: TObject;
                                                     Response: TIdSipResponse;
                                                     ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckTortureTest17;
    procedure CheckTortureTest19;
    procedure CheckTortureTest21;
    procedure CheckTortureTest22;
    procedure CheckTortureTest23;
    procedure CheckTortureTest35;
    procedure CheckTortureTest40;
//    procedure CheckTortureTest41;
    procedure ClientOnResponse(Sender: TObject;
                               Response: TIdSipResponse;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure ClientOnResponseDownClosedConnection(Sender: TObject;
                                                   Response: TIdSipResponse;
                                                   ReceivedFrom: TIdSipConnectionBindings);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdSipConnectionBindings);
    procedure OnServerDisconnect(AThread: TIdPeerThread);
    procedure RaiseException(Sender: TObject;
                             Request: TIdSipRequest);
    function  ReadResponse: String;
    procedure Send200OK(Sender: TObject;
                        Request: TIdSipRequest);
  protected
    CheckingRequestEvent:   TIdSipRequestEvent;
    CheckingResponseEvent:  TIdSipResponseEvent;
    Client:                 TIdTcpClient;
    ClientReceivedResponse: Boolean;
    ConnectionDropped:      Boolean;
    HighPortServer:         TIdSipTcpServer;
    LowPortServer:          TIdSipTcpServer;
    MethodCallCount:        Cardinal;
    Parser:                 TIdSipParser;
    ServerReceivedResponse: Boolean;
    SipClient:              TIdSipTcpClient;

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
    procedure TestMalformedRequest;
    procedure TestMethodEvent;
    procedure TestMultipleMessages;
    procedure TestRemoveMessageListener;
    procedure TestSendResponsesClosedConnection;
    procedure TestSendResponsesClosedConnectionReceivedParam;
    procedure TestSendResponsesOpenConnection;
    procedure TestTortureTest16;
    procedure TestTortureTest17;
    procedure TestTortureTest19;
    procedure TestTortureTest21;
    procedure TestTortureTest22;
    procedure TestTortureTest23;
    procedure TestTortureTest35;
    procedure TestTortureTest40;
//    procedure TestTortureTest41;
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
  Classes, IdGlobal, IdSocketHandle, IdSipConsts, IdSipHeaders, IdSimpleParser,
  IdStack, TestFrameworkSip, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTcpServer unit tests');
  Result.AddSuite(TestTIdSipConnectionTableEntry.Suite);
  Result.AddSuite(TestTIdSipConnectionTable.Suite);
  Result.AddSuite(TestTIdSipTcpServer.Suite);
end;

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
        Check(Conn = E.Connection,      'Connection not set');
        Check(Req.IsEqualTo(E.Request), 'Request not set');
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
//* TestTIdSipConnectionTable
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
//* TestTIdSipTcpServer                                                        *
//******************************************************************************
//* TestTIdSipTcpServer Public methods *****************************************

procedure TestTIdSipTcpServer.SetUp;
var
  Binding: TIdSocketHandle;
begin
  inherited SetUp;

  Self.Client         := TIdTcpClient.Create(nil);
  Self.Parser         := TIdSipParser.Create;
  Self.HighPortServer := Self.ServerType.Create(nil);
  Self.LowPortServer  := Self.ServerType.Create(nil);
  Self.SipClient      := Self.HighPortServer.CreateClient;

  Self.Client.Host := LocalHost;
  Self.Client.Port := LowPortServer.DefaultPort;

  Self.ClientReceivedResponse := false;
  Self.ConnectionDropped      := false;
  Self.MethodCallCount        := 0;
  Self.ServerReceivedResponse := false;

  LowPortServer.Bindings.Clear;
  Binding := LowPortServer.Bindings.Add;
  Binding.IP   := LocalHost;
  Binding.Port := IdPORT_SIP;

  Self.HighPortServer.Bindings.Clear;
  Binding := Self.HighPortServer.Bindings.Add;
  Binding.IP   := GStack.LocalAddress;
  Binding.Port := IdPORT_SIP + 10000;

  Self.LowPortServer.Active  := true;
  Self.HighPortServer.Active := true;

  Self.LowPortServer.AddMessageListener(Self);
  Self.HighPortServer.AddMessageListener(Self);
end;

procedure TestTIdSipTcpServer.TearDown;
begin
  Self.HighPortServer.RemoveMessageListener(Self);
  Self.LowPortServer.RemoveMessageListener(Self);

  Self.HighPortServer.Active := false;
  Self.LowPortServer.Active := false;

  Self.HighPortServer.DestroyClient(Self.SipClient);
  Self.LowPortServer.Free;
  Self.HighPortServer.Free;
  Self.Parser.Free;
  Self.Client.Free;

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

      Check(Expected.IsEqualTo(Request.Headers), 'Headers');
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

procedure TestTIdSipTcpServer.CheckSendResponsesDownClosedConnection(Sender: TObject;
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

procedure TestTIdSipTcpServer.CheckTortureTest17;
var
  Response: TIdSipResponse;
begin
  Response := Self.Parser.ParseAndMakeResponse(Self.ReadResponse);
  try
    CheckEquals(SipVersion,    Response.SipVersion, 'SipVersion');
    CheckEquals(SIPBadRequest, Response.StatusCode, 'StatusCode');

    CheckEquals(Format(MalformedToken, [ContentLengthHeaderFull, 'Content-Length: -999']),
                Response.StatusText,
                'StatusText');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest19;
var
  Response: TIdSipResponse;
begin
  Response := Self.Parser.ParseAndMakeResponse(Self.ReadResponse);
  try
    CheckEquals(SipVersion,    Response.SipVersion, 'SipVersion');
    CheckEquals(SIPBadRequest, Response.StatusCode, 'StatusCode');

    CheckEquals(Format(MalformedToken, [ToHeaderFull, 'To: "Mr. J. User <sip:j.user@company.com>']),
                Response.StatusText,
                'StatusText');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest21;
var
  Response: TIdSipResponse;
begin
  Response := Self.Parser.ParseAndMakeResponse(Self.ReadResponse);
  try
    CheckEquals(SipVersion,                Response.SipVersion, 'SipVersion');
    CheckEquals(SIPBadRequest,             Response.StatusCode, 'StatusCode');
    CheckEquals(RequestUriNoAngleBrackets, Response.StatusText, 'StatusText');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest22;
var
  Response: TIdSipResponse;
begin
  Response := Self.Parser.ParseAndMakeResponse(Self.ReadResponse);
  try
    CheckEquals(SipVersion,         Response.SipVersion, 'SipVersion');
    CheckEquals(SIPBadRequest,      Response.StatusCode, 'StatusCode');
    CheckEquals(RequestUriNoSpaces, Response.StatusText, 'StatusText');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest23;
var
  Response: TIdSipResponse;
begin
  Response := Self.Parser.ParseAndMakeResponse(Self.ReadResponse);
  try
    CheckEquals(SipVersion,         Response.SipVersion, 'SipVersion');
    CheckEquals(SIPBadRequest,      Response.StatusCode, 'StatusCode');
    CheckEquals(RequestUriNoSpaces, Response.StatusText, 'StatusText');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest35;
var
  Response: TIdSipResponse;
begin
  Response := Self.Parser.ParseAndMakeResponse(Self.ReadResponse);
  try
    CheckEquals(SipVersion,    Response.SipVersion, 'SipVersion');
    CheckEquals(SIPBadRequest, Response.StatusCode, 'StatusCode');
    CheckEquals(Format(MalformedToken, [ExpiresHeader, 'Expires: 0 0l@company.com']),
                Response.StatusText,
                'StatusText');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest40;
var
  Response: TIdSipResponse;
begin
  Response := Self.Parser.ParseAndMakeResponse(Self.ReadResponse);
  try
    CheckEquals(SipVersion,    Response.SipVersion, 'SipVersion');
    CheckEquals(SIPBadRequest, Response.StatusCode, 'StatusCode');
    CheckEquals(Format(MalformedToken, [FromHeaderFull, 'From:    Bell, Alexander <sip:a.g.bell@bell-tel.com>;tag=43']),
                Response.StatusText,
                'StatusText');
  finally
    Response.Free;
  end;
end;
{
procedure TestTIdSipTcpServer.CheckTortureTest41;
var
  Response: TIdSipResponse;
  Str:      TStringStream;
begin
  Response := Self.Parser.ParseAndMakeResponse(Self.ReadResponse);
  try
    CheckEquals(SipVersion,                  Response.SipVersion, 'SipVersion');
    CheckEquals(SIPSIPVersionNotSupported,   Response.StatusCode, 'StatusCode');
    CheckEquals(RSSIPSIPVersionNotSupported, Response.StatusText, 'StatusText');
  finally
    Response.Free;
  end;
end;
}
procedure TestTIdSipTcpServer.ClientOnResponse(Sender: TObject;
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

procedure TestTIdSipTcpServer.ClientOnResponseDownClosedConnection(Sender: TObject;
                                                             Response: TIdSipResponse;
                                                             ReceivedFrom: TIdSipConnectionBindings);
begin
  Fail('The connection is closed. The client should not receive a response');
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

procedure TestTIdSipTcpServer.OnServerDisconnect(AThread: TIdPeerThread);
begin
  Self.ConnectionDropped := true;
end;

procedure TestTIdSipTcpServer.RaiseException(Sender: TObject;
                                             Request: TIdSipRequest);
begin
  raise Exception.Create('RaiseException');
end;

function TestTIdSipTcpServer.ReadResponse: String;
var
  Line: String;
begin
  Result := '';
  Line := Self.Client.ReadLn(#$A, DefaultTimeout);
  Result := Line;
  while Line <> '' do begin
    Line := Self.Client.ReadLn(#$A, DefaultTimeout);
    Result := Result + #13#10 + Line;
  end;
end;

procedure TestTIdSipTcpServer.Send200OK(Sender: TObject;
                                        Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.Parser.ParseAndMakeResponse(LocalLoopResponse);
  try
    Response.StatusCode := SIPOK;
    LowPortServer.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

//* TestTIdSipTcpServer Published methods **************************************

procedure TestTIdSipTcpServer.TestAddMessageListener;
begin
  // SetUp already adds Self as a listener to LowPortServer
  Self.CheckingRequestEvent := Self.AcknowledgeEvent;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(BasicRequest);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpServer.TestInternalServerError;
var
  Request: TIdSipRequest;
begin
  Self.CheckingRequestEvent := Self.RaiseException;

  Request := Self.Parser.ParseAndMakeRequest(LocalLoopRequest);
  try
    SipClient.OnResponse := Self.CheckInternalServerError;
    SipClient.Host       := '127.0.0.1';
    SipClient.Port       := LowPortServer.DefaultPort;
    SipClient.Timeout    := 1000;

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

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpServer.TestListenerReceiveRequest;
var
  Listener: TIdSipTestMessageListener;
begin
  Self.CheckingRequestEvent := Self.AcknowledgeEvent;

  Listener := TIdSipTestMessageListener.Create;
  try
    Self.LowPortServer.AddMessageListener(Listener);
    Self.LowPortServer.AddMessageListener(Self);

    Self.Client.Connect(DefaultTimeout);
    Self.Client.Write(BasicRequest);

    if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);
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

    if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);
    Check(Listener.ReceivedResponse, 'Not all listeners received the Response');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTcpServer.TestMalformedRequest;
var
  Response: TIdSipResponse;
  P:        TIdSipParser;
begin
  // For the weak of eyes - the SIP-Version is malformed. Spot the semicolon.
  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write('INVITE sip:wintermute@tessier-ashpool.co.luna SIP/;2.0'#13#10
                  + #13#10);

  P := TIdSipParser.Create;
  try
    Response := P.ParseAndMakeResponse(Self.Client.AllData);
    try
      Response.SipVersion := SipVersion;

      CheckEquals(SipVersion,
                  Response.SipVersion,
                  'SIP-Version');
      CheckEquals(SIPBadRequest,
                  Response.StatusCode,
                  'Status-Code');
      CheckEquals(Format(InvalidSipVersion, ['SIP/;2.0']),
                  Response.StatusText,
                  'Status-Text');
    finally
      Response.Free;
    end;
  finally
    P.Free;
  end;
end;

procedure TestTIdSipTcpServer.TestMethodEvent;
begin
  Self.CheckingRequestEvent := Self.CheckMethodEvent;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(Format(BasicRequest, [ViaFQDN]));

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpServer.TestMultipleMessages;
begin
  Self.CheckingRequestEvent := Self.CheckMultipleMessages;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(Format(BasicRequest, [ViaFQDN])
                  + Format(BasicRequest, [ViaFQDN]));

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);

  CheckEquals(2, Self.MethodCallCount, 'Method call count')
end;

procedure TestTIdSipTcpServer.TestRemoveMessageListener;
begin
//  Self.HighPortServer.AddMessageListener(Self);
  Self.HighPortServer.RemoveMessageListener(Self);

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(BasicRequest);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrTimeout) then
    Fail('Listener wasn''t removed');
end;

procedure TestTIdSipTcpServer.TestSendResponsesClosedConnection;
var
  Request:  TIdSipRequest;
  Response: TIdSipResponse;
begin
  Self.CheckingResponseEvent := Self.CheckSendResponsesDownClosedConnection;

  Request := Self.Parser.ParseAndMakeRequest(LocalLoopRequest);
  try
    Self.SipClient.OnResponse := Self.ClientOnResponseDownClosedConnection;
    Self.SipClient.Host       := '127.0.0.1';
    Self.SipClient.Port       := IdPORT_SIP;
    Self.SipClient.Timeout    := 100;

    Self.SipClient.Connect;
    try
      Self.SipClient.Send(Request);
    finally
      Self.SipClient.Disconnect;
    end;

    // I can't say WHY we need to pause here, but it seems to work...
    // Not exactly an ideal situation.
    IdGlobal.Sleep(500);

    Response := Self.Parser.ParseAndMakeResponse(LocalLoopResponse);
    try
      Response.StatusCode := SIPOK;
      Self.LowPortServer.SendResponse(Response);
    finally
      Response.Free;
    end;

    if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ServerReceivedResponse,
          'Response wasn''t sent down a new connection');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipTcpServer.TestSendResponsesClosedConnectionReceivedParam;
var
  HighPortListener: TIdSipTestMessageListener;
  LowPortListener:  TIdSipTestMessageListener;
  Request:          TIdSipRequest;
  Response:         TIdSipResponse;
begin
  Assert(Assigned(GStack) and (GStack.LocalAddress <> '127.0.0.1'),
         'This test cannot work on a machine with only one network interface. '
       + 'Please make sure it''s got a NIC and that NIC has an IP');

  HighPortListener := TIdSipTestMessageListener.Create;
  try
    LowPortListener := TIdSipTestMessageListener.Create;
    try
      Self.HighPortServer.AddMessageListener(HighPortListener);
      try
        Self.LowPortServer.AddMessageListener(LowPortListener);
        try
          Request := Self.Parser.ParseAndMakeRequest(LocalLoopRequest);
          try
            Self.SipClient.OnResponse := Self.ClientOnResponseDownClosedConnection;
            Self.SipClient.Host       := Self.HighPortServer.Bindings[0].IP;
            Self.SipClient.Port       := Self.HighPortServer.Bindings[0].Port;
            Self.SipClient.Timeout    := 100;

            Self.SipClient.Connect;
            try
              Self.SipClient.Send(Request);
            finally
              Self.SipClient.Disconnect;
            end;

            // I can't say WHY we need to pause here, but it seems to work...
            // Not exactly an ideal situation. We're waiting for the connection
            // to be completely torn down.
            IdGlobal.Sleep(500);

            Response := Self.Parser.ParseAndMakeResponse(LocalLoopResponse);
            try
              Response.LastHop.Received := Self.HighPortServer.Bindings[0].IP;
              Response.LastHop.Port     := Self.HighPortServer.Bindings[0].Port;
              Response.StatusCode       := SIPOK;
              Self.LowPortServer.SendResponse(Response);
            finally
              Response.Free;
            end;

            Check(HighPortListener.ReceivedResponse
                  and not LowPortListener.ReceivedResponse,
                  'Wrong server received response');
          finally
            Request.Free;
          end;
        finally
          Self.LowPortServer.RemoveMessageListener(LowPortListener);
        end;
      finally
        Self.HighPortServer.RemoveMessageListener(HighPortListener);
      end;
    finally
      LowPortListener.Free;
    end;
  finally
    HighPortListener.Free;
  end;
end;

procedure TestTIdSipTcpServer.TestSendResponsesOpenConnection;
var
  Request:   TIdSipRequest;
  SipClient: TIdSipTcpClient;
begin
  Self.CheckingRequestEvent := Self.Send200OK;

  Request := Self.Parser.ParseAndMakeRequest(LocalLoopRequest);
  try
    SipClient := Self.HighPortServer.CreateClient;
    try
      SipClient.OnResponse := Self.ClientOnResponse;
      SipClient.Host       := '127.0.0.1';
      SipClient.Port       := IdPORT_SIP;
      SipClient.Timeout    := 1000;

      SipClient.Connect;
      try
        SipClient.Send(Request);

        if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
          raise Self.ExceptionType.Create(Self.ExceptionMessage);

        Check(Self.ClientReceivedResponse,
              'No response received on same connection');
      finally
        SipClient.Disconnect;
      end;
    finally
      Self.HighPortServer.DestroyClient(SipClient);
    end;
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipTcpServer.TestTortureTest16;
begin
  Self.LowPortServer.OnDisconnect := Self.OnServerDisconnect;
  Self.LowPortServer.ReadBodyTimeout := 50;

  Self.Client.Connect(DefaultTimeout);

  Self.Client.Write(TortureTest16);
  IdGlobal.Sleep(100);
end;

procedure TestTIdSipTcpServer.TestTortureTest17;
begin
  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(TortureTest17);

  Self.CheckTortureTest17;
end;

procedure TestTIdSipTcpServer.TestTortureTest19;
begin
  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(TortureTest19);

  Self.CheckTortureTest19;
end;

procedure TestTIdSipTcpServer.TestTortureTest21;
begin
  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(TortureTest21);

  Self.CheckTortureTest21;
end;

procedure TestTIdSipTcpServer.TestTortureTest22;
begin
  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(TortureTest22);

  Self.CheckTortureTest22;
end;

procedure TestTIdSipTcpServer.TestTortureTest23;
begin
  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(TortureTest23);

  Self.CheckTortureTest23;
end;

procedure TestTIdSipTcpServer.TestTortureTest35;
begin
  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(TortureTest35);

  Self.CheckTortureTest35;
end;

procedure TestTIdSipTcpServer.TestTortureTest40;
begin
  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(TortureTest40);

  Self.CheckTortureTest40;
end;
{
procedure TestTIdSipTcpServer.TestTortureTest41;
begin
  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(TortureTest41);

  Self.CheckTortureTest41;
end;
}

initialization
  RegisterTest('SIP Server using TCP', Suite);
end.
