unit TestIdSipTcpServer;

interface

uses
  IdSipMessage, IdSipParser, IdSipTcpServer, IdTCPClient, IdTCPServer, SyncObjs,
  SysUtils, TestFrameworkEx;

type
  TestTIdSipTcpServer = class(TThreadingTestCase)
  private
    Client:            TIdTcpClient;
    ConnectionDropped: Boolean;
    MethodCallCount:   Cardinal;
    Parser:            TIdSipParser;
    Server:            TIdSipTcpServer;

    procedure CheckMultipleMessages(AThread: TIdPeerThread;
                                    AMessage: TIdSipMessage);
    procedure CheckMethodEvent(AThread: TIdPeerThread;
                               AMessage: TIdSipMessage);
    procedure CheckReceivedParamDifferentIPv4SentBy(AThread: TIdPeerThread;
                                                    AMessage: TIdSipMessage);
    procedure CheckReceivedParamFQDNSentBy(AThread: TIdPeerThread;
                                 AMessage: TIdSipMessage);
    procedure CheckReceivedParamIPv4SentBy(AThread: TIdPeerThread;
                                 AMessage: TIdSipMessage);
    procedure CheckTortureTest19;
    procedure CheckTortureTest21;
    procedure CheckTortureTest22;
    procedure CheckTortureTest23;
    procedure CheckTortureTest35;
    procedure CheckTortureTest40;
//    procedure CheckTortureTest41;
    procedure OnServerDisconnect(AThread: TIdPeerThread);
    function ReadResponse: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLeadingEmptyLines;
    procedure TestMalformedRequest;
    procedure TestMethodEvent;
    procedure TestMultipleMessages;
    procedure TestReceivedParamDifferentIPv4SentBy;
    procedure TestReceivedParamFQDNSentBy;
    procedure TestReceivedParamIPv4SentBy;
    procedure TestTortureTest16;
    procedure TestTortureTest19;
    procedure TestTortureTest21;
    procedure TestTortureTest22;
    procedure TestTortureTest23;
    procedure TestTortureTest35;
    procedure TestTortureTest40;
//    procedure TestTortureTest41;
  end;

const
  BasicRequest = 'INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
               + 'Via: SIP/2.0/TCP %s;branch=z9hG4bK776asdhds'#13#10
               + 'Max-Forwards: 70'#13#10
               + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
               + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
               + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
               + 'CSeq: 314159 INVITE'#13#10
               + 'Contact: <sip:wintermute@tessier-ashpool.co.lu>'#13#10
               + 'Content-Type: text/plain'#13#10
               + 'Content-Length: 29'#13#10
               + #13#10
               + 'I am a message. Hear me roar!';
  ViaFQDN        = 'gw1.leo-ix.org';
  ViaIP          = '127.0.0.1';
  ViaDifferentIP = '196.25.1.1';
  DefaultTimeout = 5000;

implementation

uses
  Classes, IdSimpleParser, TestFramework, TestMessages;

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
begin
  inherited SetUp;

  Self.Client := TIdTcpClient.Create(nil);
  Self.Server := TIdSipTcpServer.Create(nil);

  Self.Client.Host := '127.0.0.1';
  Self.Client.Port := Self.Server.DefaultPort;

  Self.MethodCallCount := 0;
  Self.Server.Active := true;

  Self.Parser := TIdSipParser.Create;

  Self.ConnectionDropped := false;
end;

procedure TestTIdSipTcpServer.TearDown;
begin
  Self.Parser.Free;
  Self.Server.Active := false;

  Client.Free;
  Server.Free;

  inherited TearDown;
end;

//* TestTIdSipTcpServer Private methods ****************************************

procedure TestTIdSipTcpServer.CheckMultipleMessages(AThread: TIdPeerThread;
                                                    AMessage: TIdSipMessage);
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

procedure TestTIdSipTcpServer.CheckMethodEvent(AThread: TIdPeerThread;
                                               AMessage: TIdSipMessage);
var
  Request: TIdSipRequest;
begin
  try
    Request := AMessage as TIdSipRequest;

    CheckEquals('INVITE',                               Request.Method,        'Method');
    CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Request.RequestUri,    'RequestUri');
    CheckEquals('SIP/2.0',                              Request.SIPVersion,    'SipVersion');
    CheckEquals(29,                                     Request.ContentLength, 'ContentLength');
    CheckEquals('a84b4c76e66710@gw1.leo-ix.org',        Request.CallID,        'CallID');
    CheckEquals(70,                                     Request.MaxForwards,   'Max-Forwards');

    CheckEquals('Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds;received=127.0.0.1',
                Request.Headers.Items[0].AsString,
                'Via');
    CheckEquals('Max-Forwards: 70',                                       Request.Headers.Items[1].AsString, 'Max-Forwards');
    CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',  Request.Headers.Items[2].AsString, 'To');
    CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774', Request.Headers.Items[3].AsString, 'From');
    CheckEquals('Call-ID: a84b4c76e66710@gw1.leo-ix.org',                 Request.Headers.Items[4].AsString, 'Call-ID');
    CheckEquals('CSeq: 314159 INVITE',                                    Request.Headers.Items[5].AsString, 'CSeq');
    CheckEquals('Contact: sip:wintermute@tessier-ashpool.co.lu',          Request.Headers.Items[6].AsString, 'Contact');
    CheckEquals('Content-Type: text/plain',                               Request.Headers.Items[7].AsString, 'Content-Length');
    CheckEquals('Content-Length: 29',                                     Request.Headers.Items[8].AsString, 'Content-Length');

    CheckEquals('I am a message. Hear me roar!', Request.Body, 'message-body');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpServer.CheckReceivedParamDifferentIPv4SentBy(AThread: TIdPeerThread;
                                                                    AMessage: TIdSipMessage);
begin
  Self.CheckReceivedParamFQDNSentBy(AThread, AMessage);
end;

procedure TestTIdSipTcpServer.CheckReceivedParamFQDNSentBy(AThread: TIdPeerThread;
                                                 AMessage: TIdSipMessage);
begin
  try
    CheckNotEquals('', AMessage.Path.LastHop.Received, 'Received param not appended by transport layer');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpServer.CheckReceivedParamIPv4SentBy(AThread: TIdPeerThread;
                                                           AMessage: TIdSipMessage);
begin
  try
    CheckEquals('', AMessage.Path.LastHop.Received, 'Received param appended by transport layer');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest19;
var
  Response: TIdSipResponse;
  Str:      TStringStream;
begin
  Str := TStringStream.Create(Self.ReadResponse);
  try
    Parser.Source := Str;

    Response := Parser.ParseAndMakeMessage as TIdSipResponse;
    try
      CheckEquals(SipVersion,    Response.SipVersion, 'SipVersion');
      CheckEquals(SIPBadRequest, Response.StatusCode, 'StatusCode');

      CheckEquals(Format(MalformedToken, [ToHeaderFull, 'To: "Mr. J. User <sip:j.user@company.com>']),
                  Response.StatusText,
                  'StatusText');
    finally
      Response.Free;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest21;
var
  Response: TIdSipResponse;
  Str:      TStringStream;
begin
  Str := TStringStream.Create(Self.ReadResponse);
  try
    Parser.Source := Str;

    Response := Parser.ParseAndMakeMessage as TIdSipResponse;
    try
      CheckEquals(SipVersion,                Response.SipVersion, 'SipVersion');
      CheckEquals(SIPBadRequest,             Response.StatusCode, 'StatusCode');
      CheckEquals(RequestUriNoAngleBrackets, Response.StatusText, 'StatusText');
    finally
      Response.Free;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest22;
var
  Response: TIdSipResponse;
  Str:      TStringStream;
begin
  Str := TStringStream.Create(Self.ReadResponse);
  try
    Parser.Source := Str;

    Response := Parser.ParseAndMakeMessage as TIdSipResponse;
    try
      CheckEquals(SipVersion,         Response.SipVersion, 'SipVersion');
      CheckEquals(SIPBadRequest,      Response.StatusCode, 'StatusCode');
      CheckEquals(RequestUriNoSpaces, Response.StatusText, 'StatusText');
    finally
      Response.Free;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest23;
var
  Response: TIdSipResponse;
  Str:      TStringStream;
begin
  Str := TStringStream.Create(Self.ReadResponse);
  try
    Parser.Source := Str;

    Response := Parser.ParseAndMakeMessage as TIdSipResponse;
    try
      CheckEquals(SipVersion,         Response.SipVersion, 'SipVersion');
      CheckEquals(SIPBadRequest,      Response.StatusCode, 'StatusCode');
      CheckEquals(RequestUriNoSpaces, Response.StatusText, 'StatusText');
    finally
      Response.Free;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest35;
var
  Response: TIdSipResponse;
  Str:      TStringStream;
begin
  Str := TStringStream.Create(Self.ReadResponse);
  try
    Parser.Source := Str;

    Response := Parser.ParseAndMakeMessage as TIdSipResponse;
    try
      CheckEquals(SipVersion,    Response.SipVersion, 'SipVersion');
      CheckEquals(SIPBadRequest, Response.StatusCode, 'StatusCode');
      CheckEquals(Format(MalformedToken, [ExpiresHeader, 'Expires: 0 0l@company.com']),
                  Response.StatusText,
                  'StatusText');
    finally
      Response.Free;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest40;
var
  Response: TIdSipResponse;
  Str:      TStringStream;
begin
  Str := TStringStream.Create(Self.ReadResponse);
  try
    Parser.Source := Str;

    Response := Parser.ParseAndMakeMessage as TIdSipResponse;
    try
      CheckEquals(SipVersion,    Response.SipVersion, 'SipVersion');
      CheckEquals(SIPBadRequest, Response.StatusCode, 'StatusCode');
      CheckEquals(Format(MalformedToken, [FromHeaderFull, 'From:    Bell, Alexander <sip:a.g.bell@bell-tel.com>;tag=43']),
                  Response.StatusText,
                  'StatusText');
    finally
      Response.Free;
    end;
  finally
    Str.Free;
  end;
end;
{
procedure TestTIdSipTcpServer.CheckTortureTest41;
var
  Response: TIdSipResponse;
  Str:      TStringStream;
begin
  Str := TStringStream.Create(Self.ReadResponse);
  try
    Parser.Source := Str;

    Response := Parser.ParseAndMakeMessage as TIdSipResponse;
    try
      CheckEquals(SipVersion,                  Response.SipVersion, 'SipVersion');
      CheckEquals(SIPSIPVersionNotSupported,   Response.StatusCode, 'StatusCode');
      CheckEquals(RSSIPSIPVersionNotSupported, Response.StatusText, 'StatusText');
    finally
      Response.Free;
    end;
  finally
    Str.Free;
  end;
end;
}
procedure TestTIdSipTcpServer.OnServerDisconnect(AThread: TIdPeerThread);
begin
  Self.ConnectionDropped := true;
end;

function TestTIdSipTcpServer.ReadResponse: String;
var
  Line: String;
begin
  Result := '';
  Line := Self.Client.ReadLn(#$A, DefaultTimeout);
  while Line <> '' do begin
    Result := Result + Line;
    Line := Self.Client.ReadLn(#$A, DefaultTimeout);
  end;
end;

//* TestTIdSipTcpServer Published methods **************************************

procedure TestTIdSipTcpServer.TestLeadingEmptyLines;
begin
  Server.OnMethod := Self.CheckMethodEvent;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(#13#10#13#10#13#10
                  + Format(BasicRequest, [ViaFQDN]));

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpServer.TestMalformedRequest;
var
  Expected: TStrings;
  Received: TStrings;
  Temp:     TIdSipMessage;
  P:        TIdSipParser;
begin
  // For the weak of eyes - the SIP-Version is malformed. Spot the semicolon.
  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/;2.0'#13#10
                  + #13#10);

  Expected := TStringList.Create;
  try
    P := TIdSipParser.Create;
    try
      Temp := P.MakeBadRequestResponse(Format(InvalidSipVersion, ['SIP/;2.0']));
      try
        Expected.Text := Temp.AsString;
      finally
        Temp.Free;
      end;
    finally
      P.Free;
    end;

    Received := TStringList.Create;
    try
      Received.Text := Self.Client.AllData;

      CheckEquals(Expected, Received, 'Malformed SIP-Version');
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipTcpServer.TestMethodEvent;
begin
  Server.OnMethod := Self.CheckMethodEvent;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(Format(BasicRequest, [ViaFQDN]));

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpServer.TestMultipleMessages;
begin
  Server.OnMethod := Self.CheckMultipleMessages;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(Format(BasicRequest, [ViaFQDN])
                  + Format(BasicRequest, [ViaFQDN]));

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);

  CheckEquals(2, Self.MethodCallCount, 'Method call count')
end;

procedure TestTIdSipTcpServer.TestReceivedParamDifferentIPv4SentBy;
begin
  Server.OnMethod := Self.CheckReceivedParamDifferentIPv4SentBy;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(Format(BasicRequest, [ViaDifferentIP]));

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpServer.TestReceivedParamFQDNSentBy;
begin
  Server.OnMethod := Self.CheckReceivedParamFQDNSentBy;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(Format(BasicRequest, [ViaFQDN]));

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpServer.TestReceivedParamIPv4SentBy;
begin
  Server.OnMethod := Self.CheckReceivedParamIPv4SentBy;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(Format(BasicRequest, [ViaIP]));

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpServer.TestTortureTest16;
begin
  Self.Server.OnDisconnect := Self.OnServerDisconnect;
  Self.Server.ReadBodyTimeout := 50;

  Self.Client.Connect(DefaultTimeout);

  Self.Client.Write(TortureTest16);
  Sleep(100);
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
