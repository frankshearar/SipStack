unit TestIdSipUdpServer;

interface

uses
  Classes, IdSipParser, IdSipUdpServer, IdUDPClient, SysUtils, TestFrameworkEx;

type
  TestTIdSipUdpServer = class(TThreadingTestCase)
  private
    Client: TIdUDPClient;
    Server: TIdSipUdpServer;

    procedure CheckRequest(Sender: TObject; const Request: TIdSipRequest);
    procedure CheckResponse(Sender: TObject; const Response: TIdSipResponse);
    procedure CheckTortureTest19(Sender: TObject; const Response: TIdSipResponse);
    procedure CheckTortureTest21(Sender: TObject; const Response: TIdSipResponse);
    procedure CheckTortureTest22(Sender: TObject; const Response: TIdSipResponse);
    procedure CheckTortureTest23(Sender: TObject; const Response: TIdSipResponse);
    procedure CheckTortureTest35(Sender: TObject; const Response: TIdSipResponse);
    procedure CheckTortureTest40(Sender: TObject; const Response: TIdSipResponse);
    procedure CheckTortureTest41(Sender: TObject; const Response: TIdSipResponse);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMalformedRequest;
    procedure TestMalformedResponse;
    procedure TestRequest;
    procedure TestResponse;
    procedure TestTortureTest19;
    procedure TestTortureTest21;
    procedure TestTortureTest22;
    procedure TestTortureTest23;
    procedure TestTortureTest35;
    procedure TestTortureTest40;
    procedure TestTortureTest41;
  end;

const
  DefaultTimeout = 5000;

implementation

uses
  SyncObjs, TestFramework, TortureTests;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipUdpServer unit tests');
  Result.AddTest(TestTIdSipUdpServer.Suite);
end;

//*******************************************************************************
//* TestTIdSipUdpServer                                                         *
//*******************************************************************************
//* TestTIdSipUdpServer Public methods ******************************************

procedure TestTIdSipUdpServer.SetUp;
begin
  inherited SetUp;

  Client := TIdUDPClient.Create(nil);
  Server := TIdSipUdpServer.Create(nil);

  Server.Active := true;
  Client.Host := '127.0.0.1';
  Client.Port := Server.DefaultPort;
end;

procedure TestTIdSipUdpServer.TearDown;
begin
  Server.Active := false;

  Server.Free;
  Client.Free;

  inherited TearDown;
end;

//* TestTIdSipUdpServer Private methods *****************************************

procedure TestTIdSipUdpServer.CheckRequest(Sender: TObject; const Request: TIdSipRequest);
begin
  try
    CheckEquals(MethodInvite, Request.Method,             'Method');
    CheckEquals('SIP/2.0',    Request.SipVersion,         'SipVersion');
    CheckEquals(29,           Request.ContentLength,      'ContentLength');
    CheckEquals(70,           Request.MaxForwards,        'Max-Forwards');

    CheckEquals('I am a message. Hear me roar!', Request.Body, 'Body');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUdpServer.CheckResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  try
    CheckEquals('SIP/2.0',                       Response.SipVersion,    'SipVersion');
    CheckEquals(486,                             Response.StatusCode,    'StatusCode');
    CheckEquals('Busy Here',                     Response.StatusText,    'StatusText');
    CheckEquals('a84b4c76e66710@gw1.leo_ix.org', Response.CallID,        'CallID');
    CheckEquals(29,                              Response.ContentLength, 'ContentLength');
    CheckEquals(70,                              Response.MaxForwards,   'MaxForwards');

  CheckEquals('Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds',
              Response.Headers.Items[0].AsString,
              'Headers.Items[0].AsString');
  CheckEquals('Max-Forwards: 70',
              Response.Headers.Items[1].AsString,
              'Headers.Items[1].AsString');
  CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',
              Response.Headers.Items[2].AsString,
              'Headers.Items[2].AsString');
  CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',
              Response.Headers.Items[3].AsString,
              'Headers.Items[3].AsString');
  CheckEquals('Call-ID: a84b4c76e66710@gw1.leo_ix.org',
              Response.Headers.Items[4].AsString,
              'Headers.Items[4].AsString');
  CheckEquals('CSeq: 314159 INVITE',
              Response.Headers.Items[5].AsString,
              'Headers.Items[5].AsString');
  CheckEquals('Contact: sip:wintermute@tessier-ashpool.co.lu',
              Response.Headers.Items[6].AsString,
              'Headers.Items[6].AsString');
  CheckEquals('Content-Length: 29',
              Response.Headers.Items[7].AsString,
              'Headers.Items[7].AsString');
  CheckEquals(8, Response.Headers.Count, 'OtherHeaders Count');

  CheckEquals('I am a message. Hear me roar!', Response.Body, 'Body');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUdpServer.CheckTortureTest19(Sender: TObject; const Response: TIdSipResponse);
begin
  try
    CheckEquals(SipVersion,                Response.SipVersion, 'SipVersion');
    CheckEquals(400,                       Response.StatusCode, 'StatusCode');

    CheckEquals(Format(MalformedToken, [ToHeaderFull, '"Mr. J. User <sip:j.user@company.com>']),
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

procedure TestTIdSipUdpServer.CheckTortureTest21(Sender: TObject; const Response: TIdSipResponse);
begin
  try
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

procedure TestTIdSipUdpServer.CheckTortureTest22(Sender: TObject; const Response: TIdSipResponse);
begin
  try
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

procedure TestTIdSipUdpServer.CheckTortureTest23(Sender: TObject; const Response: TIdSipResponse);
begin
  try
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

procedure TestTIdSipUdpServer.CheckTortureTest35(Sender: TObject; const Response: TIdSipResponse);
begin
  try
    CheckEquals(SipVersion,         Response.SipVersion, 'SipVersion');
    CheckEquals(350,                Response.StatusCode, 'StatusCode');
    CheckEquals(Format(MalformedToken, [FromHeaderFull, 'Expires: 0 0l@company.com']),
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

procedure TestTIdSipUdpServer.CheckTortureTest40(Sender: TObject; const Response: TIdSipResponse);
begin
  try
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

procedure TestTIdSipUdpServer.CheckTortureTest41(Sender: TObject; const Response: TIdSipResponse);
begin
  try
    CheckEquals(SipVersion,         Response.SipVersion, 'SipVersion');
    CheckEquals(400,                Response.StatusCode, 'StatusCode');
    CheckEquals(Format(InvalidSipVersion, ['SIP/7.0']),
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

//* TestTIdSipUdpServer Published methods ***************************************

procedure TestTIdSipUdpServer.TestMalformedRequest;
var
  Expected: TStrings;
  Received: TStrings;
  Msg:      TIdSipMessage;
  P:        TIdSipParser;
begin
  // note the semicolon in the SIP-version
  Client.Send('INVITE sip:tentacleface@rlyeh.org.au SIP/;2.0'#13#10
            + 'To: "Cthulhu" <tentacleface@rlyeh.org.au>'#13#10
            + 'From: "Great Old Ones" <greatoldones@outerdarkness.lu>'#13#10
            + 'CSeq: 0 INVITE'#13#10
            + 'Call-ID: 0'#13#10
            + 'Max-Forwards: 5'#13#10
            + 'Via: SIP/2.0/UDP 127.0.0.1:5060'#13#10
            + #13#10);

  Expected := TStringList.Create;
  try
    P := TIdSipParser.Create;
    try
      Msg := P.MakeBadRequestResponse(Format(InvalidSipVersion, ['SIP/;2.0']));
      try
        Expected.Text := Msg.AsString;
      finally
        Msg.Free;
      end;
    finally
      P.Free;
    end;

    Received := TStringList.Create;
    try
      Received.Text := Client.ReceiveString(DefaultTimeout);

      CheckEquals(Expected, Received, 'Malformed request');
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipUdpServer.TestMalformedResponse;
begin
  Client.Send('SIP/;2.0 200 OK'#13#10
            + #13#10);

  CheckEquals('', Client.ReceiveString(DefaultTimeout), 'Response to a malformed response');
end;

procedure TestTIdSipUdpServer.TestRequest;
begin
  Server.OnRequest := Self.CheckRequest;

  Client.Send('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
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

procedure TestTIdSipUdpServer.TestResponse;
begin
  Server.OnResponse := Self.CheckResponse;

  Client.Send('SIP/2.0 486 Busy Here'#13#10
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

procedure TestTIdSipUdpServer.TestTortureTest19;
begin
  Server.OnResponse := Self.CheckTortureTest19;

  Self.Client.Send(TortureTest19);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipUdpServer.TestTortureTest21;
begin
  Server.OnResponse := Self.CheckTortureTest21;

  Self.Client.Send(TortureTest21);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipUdpServer.TestTortureTest22;
begin
  Server.OnResponse := Self.CheckTortureTest22;

  Self.Client.Send(TortureTest22);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipUdpServer.TestTortureTest23;
begin
  Server.OnResponse := Self.CheckTortureTest23;

  Self.Client.Send(TortureTest23);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipUdpServer.TestTortureTest35;
begin
  Server.OnResponse := Self.CheckTortureTest35;

  Self.Client.Send(TortureTest35);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipUdpServer.TestTortureTest40;
begin
  Server.OnResponse := Self.CheckTortureTest40;

  Self.Client.Send(TortureTest40);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipUdpServer.TestTortureTest41;
begin
  Server.OnResponse := Self.CheckTortureTest41;

  Self.Client.Send(TortureTest41);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

initialization
  RegisterTest('SIP server using UDP', Suite);
end.
