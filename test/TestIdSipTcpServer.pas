unit TestIdSipTcpServer;

interface

uses
  IdSipParser, IdSipTcpServer, IdTCPClient, IdTCPServer, SyncObjs, SysUtils,
  TestFrameworkEx;

type
  TestTIdSipTcpServer = class(TThreadingTestCase)
  private
    Client:          TIdTcpClient;
    MethodCallCount: Cardinal;
    Server:          TIdSipTcpServer;

    procedure CheckMultipleMessages(AThread: TIdPeerThread;
                                    AMessage: TIdSipMessage);
    procedure CheckMethodEvent(AThread: TIdPeerThread;
                               AMessage: TIdSipMessage);
    procedure CheckTortureTest19(AThread: TIdPeerThread;
                                AMessage: TIdSipMessage);
    procedure CheckTortureTest21(AThread: TIdPeerThread;
                                AMessage: TIdSipMessage);
    procedure CheckTortureTest22(AThread: TIdPeerThread;
                                AMessage: TIdSipMessage);
    procedure CheckTortureTest23(AThread: TIdPeerThread;
                                AMessage: TIdSipMessage);
    procedure CheckTortureTest35(AThread: TIdPeerThread;
                                AMessage: TIdSipMessage);
    procedure CheckTortureTest40(AThread: TIdPeerThread;
                                AMessage: TIdSipMessage);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLeadingEmptyLines;
    procedure TestMalformedRequest;
    procedure TestMethodEvent;
    procedure TestMultipleMessages;
    procedure TestTortureTest19;
    procedure TestTortureTest21;
    procedure TestTortureTest22;
    procedure TestTortureTest23;
    procedure TestTortureTest35;
    procedure TestTortureTest40;
  end;

const
  DefaultTimeout = 5000;

implementation

uses
  Classes, TestFramework, TortureTests;

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

  Self.Client           := TIdTcpClient.Create(nil);
  Self.Server           := TIdSipTcpServer.Create(nil);

  Self.Client.Host := '127.0.0.1';
  Self.Client.Port := Self.Server.DefaultPort;

  Self.MethodCallCount := 0;
  Self.Server.Active := true;
end;

procedure TestTIdSipTcpServer.TearDown;
begin
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
    CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Request.Request,       'Request');
    CheckEquals('SIP/2.0',                              Request.SIPVersion,    'SipVersion');
    CheckEquals(29,                                     Request.ContentLength, 'ContentLength');
    CheckEquals('a84b4c76e66710@gw1.leo_ix.org',        Request.CallID,        'CallID');
    CheckEquals(70,                                     Request.MaxForwards,   'Max-Forwards');

    CheckEquals('Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds', Request.Headers.Items[0].AsString, 'Via');
    CheckEquals('Max-Forwards: 70',                                        Request.Headers.Items[1].AsString, 'Max-Forwards');
    CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',   Request.Headers.Items[2].AsString, 'To');
    CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',  Request.Headers.Items[3].AsString, 'From');
    CheckEquals('Call-ID: a84b4c76e66710@gw1.leo_ix.org',                  Request.Headers.Items[4].AsString, 'Call-ID');
    CheckEquals('CSeq: 314159 INVITE',                                     Request.Headers.Items[5].AsString, 'CSeq');
    CheckEquals('Contact: sip:wintermute@tessier-ashpool.co.lu',           Request.Headers.Items[6].AsString, 'Contact');
    CheckEquals('Content-Length: 29',                                      Request.Headers.Items[7].AsString, 'Content-Length');
    CheckEquals(8, Request.Headers.Count, 'Header count');

    CheckEquals('I am a message. Hear me roar!', Request.Body, 'message-body');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest19(AThread: TIdPeerThread;
                                                 AMessage: TIdSipMessage);
var
  Response: TIdSipResponse;
begin
  try
    Response := AMessage as TIdSipResponse;

    CheckEquals(SipVersion, Response.SipVersion, 'SipVersion');
    CheckEquals(400,        Response.StatusCode, 'StatusCode');

    CheckEquals(Format(MalformedToken, [ToHeaderFull, '"Mr. J. User <sip:j.user@company.com>']),
                Response.StatusText,
                'StatusText');
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest21(AThread: TIdPeerThread;
                                                 AMessage: TIdSipMessage);
var
  Response: TIdSipResponse;
begin
  try
    Response := AMessage as TIdSipResponse;

    CheckEquals(SipVersion,                Response.SipVersion, 'SipVersion');
    CheckEquals(400,                       Response.StatusCode, 'StatusCode');
    CheckEquals(RequestUriNoAngleBrackets, Response.StatusText, 'StatusText');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest22(AThread: TIdPeerThread;
                                                 AMessage: TIdSipMessage);
var
  Response: TIdSipResponse;
begin
  try
    Response := AMessage as TIdSipResponse;

    CheckEquals(SipVersion,         Response.SipVersion, 'SipVersion');
    CheckEquals(400,                Response.StatusCode, 'StatusCode');
    CheckEquals(RequestUriNoSpaces, Response.StatusText, 'StatusText');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest23(AThread: TIdPeerThread;
                                                 AMessage: TIdSipMessage);
var
  Response: TIdSipResponse;
begin
  try
    Response := AMessage as TIdSipResponse;

    CheckEquals(SipVersion,         Response.SipVersion, 'SipVersion');
    CheckEquals(400,                Response.StatusCode, 'StatusCode');
    CheckEquals(RequestUriNoSpaces, Response.StatusText, 'StatusText');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest35(AThread: TIdPeerThread;
                                                 AMessage: TIdSipMessage);
var
  Response: TIdSipResponse;
begin
  try
    Response := AMessage as TIdSipResponse;

    CheckEquals(SipVersion,         Response.SipVersion, 'SipVersion');
    CheckEquals(350,                Response.StatusCode, 'StatusCode');
    CheckEquals(Format(MalformedToken, [ExpiresHeader, 'Expires: 0 0l@company.com']),
                Response.StatusText,
                'StatusText');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpServer.CheckTortureTest40(AThread: TIdPeerThread;
                                                 AMessage: TIdSipMessage);
var
  Response: TIdSipResponse;
begin
  try
    Response := AMessage as TIdSipResponse;

    CheckEquals(SipVersion,         Response.SipVersion, 'SipVersion');
    CheckEquals(400,                Response.StatusCode, 'StatusCode');
    CheckEquals(Format(MalformedToken, [FromHeaderFull, 'Bell, Alexander <sip:a.g.bell@bell-tel.com>;tag=43']),
                Response.StatusText,
                'StatusText');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

//* TestTIdSipTcpServer Published methods **************************************

procedure TestTIdSipTcpServer.TestLeadingEmptyLines;
begin
  Server.OnMethod := Self.CheckMethodEvent;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(#13#10#13#10#13#10
                  + 'INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                  + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10
                  + 'Max-Forwards: 70'#13#10
                  + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                  + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                  + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
                  + 'CSeq: 314159 INVITE'#13#10
                  + 'Contact: <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                  + 'Content-Length: 29'#13#10
                  + #13#10
                  + 'I am a message. Hear me roar!');

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

//  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
//    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpServer.TestMethodEvent;
begin
  Server.OnMethod := Self.CheckMethodEvent;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                  + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10
                  + 'Max-Forwards: 70'#13#10
                  + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                  + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                  + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
                  + 'CSeq: 314159 INVITE'#13#10
                  + 'Contact: <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                  + 'Content-Length: 29'#13#10
                  + #13#10
                  + 'I am a message. Hear me roar!');

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpServer.TestMultipleMessages;
begin
  Server.OnMethod := Self.CheckMultipleMessages;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                  + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10
                  + 'Max-Forwards: 70'#13#10
                  + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                  + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                  + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
                  + 'CSeq: 314159 INVITE'#13#10
                  + 'Contact: <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                  + 'Content-Length: 29'#13#10
                  + #13#10
                  + 'I am a message. Hear me roar!'
                  + 'INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                  + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10
                  + 'Max-Forwards: 70'#13#10
                  + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                  + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                  + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
                  + 'CSeq: 314159 INVITE'#13#10
                  + 'Contact: <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                  + 'Content-Length: 29'#13#10
                  + #13#10
                  + 'I am a message. Hear me roar!');

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);

  CheckEquals(2, Self.MethodCallCount, 'Method call count')
end;

procedure TestTIdSipTcpServer.TestTortureTest19;
begin
  Server.OnMethod := Self.CheckTortureTest19;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(TortureTest19);
end;

procedure TestTIdSipTcpServer.TestTortureTest21;
begin
  Server.OnMethod := Self.CheckTortureTest21;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(TortureTest21);
end;

procedure TestTIdSipTcpServer.TestTortureTest22;
begin
  Server.OnMethod := Self.CheckTortureTest22;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(TortureTest22);
end;

procedure TestTIdSipTcpServer.TestTortureTest23;
begin
  Server.OnMethod := Self.CheckTortureTest23;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(TortureTest23);
end;

procedure TestTIdSipTcpServer.TestTortureTest35;
begin
  Server.OnMethod := Self.CheckTortureTest35;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(TortureTest35);
end;

procedure TestTIdSipTcpServer.TestTortureTest40;
begin
  Server.OnMethod := Self.CheckTortureTest40;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(TortureTest40);
end;

initialization
  RegisterTest('SIP Server using TCP', Suite);
end.
