unit TestIdSipTcpServer;

interface

uses
  IdSipParser, IdSipTcpServer, IdTCPClient, IdTCPServer, SyncObjs, SysUtils,
  TestFrameworkEx;

type
  TestTIdSipTcpServer = class(TThreadingTestCase)
  private
    Client:           TIdTcpClient;
    MethodCallCount:  Cardinal;
    Server:           TIdSipTcpServer;

    procedure CheckMultipleMessages(AThread: TIdPeerThread;
                                    AMessage: TIdSipMessage);
    procedure CheckMethodEvent(AThread: TIdPeerThread;
                               AMessage: TIdSipMessage);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLeadingEmptyLines;
    procedure TestMalformedRequest;
    procedure TestMethodEvent;
    procedure TestMultipleMessages;
  end;

implementation

uses
  Classes, TestFramework;

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

    CheckEquals(7, Request.OtherHeaders.Count, 'Header count');
    CheckEquals('Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds', Request.OtherHeaders[0], 'Via');
    CheckEquals('Max-Forwards: 70',                                        Request.OtherHeaders[1], 'Max-Forwards');
    CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',   Request.OtherHeaders[2], 'To');
    CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',  Request.OtherHeaders[3], 'From');
    CheckEquals('Call-ID: a84b4c76e66710@gw1.leo_ix.org',                  Request.OtherHeaders[4], 'Call-ID');
    CheckEquals('CSeq: 314159 INVITE',                                     Request.OtherHeaders[5], 'CSeq');
    CheckEquals('Contact: <sip:wintermute@tessier-ashpool.co.lu>',         Request.OtherHeaders[6], 'Contact');

    CheckEquals('I am a message. Hear me roar!', Request.Body, 'message-body');

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

  Self.Client.Connect(5000);
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

  if (Self.ThreadEvent.WaitFor(5000) <> wrSignaled) then
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
  Self.Client.Connect(5000);
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

//  if (Self.ThreadEvent.WaitFor(5000) <> wrSignaled) then
//    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpServer.TestMethodEvent;
begin
  Server.OnMethod := Self.CheckMethodEvent;

  Self.Client.Connect(5000);
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

  if (Self.ThreadEvent.WaitFor(5000) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpServer.TestMultipleMessages;
begin
  Server.OnMethod := Self.CheckMultipleMessages;

  Self.Client.Connect(5000);
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

  if (Self.ThreadEvent.WaitFor(5000) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);

  CheckEquals(2, Self.MethodCallCount, 'Method call count')
end;

initialization
  RegisterTest('SIP Server using TCP', Suite);
end.
