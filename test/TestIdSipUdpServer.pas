unit TestIdSipUdpServer;

interface

uses
  Classes, IdSipParser, IdSipUdpServer, IdUDPClient, SysUtils, TestFrameworkEx;

type
  TIdSipUdpServerThread = class(TThread)
  private
    fExceptionType:    ExceptClass;
    fExceptionMessage: String;
    fServer:           TIdSipUdpServer;
  protected
    procedure Execute; override;
  public
    constructor Create(Server: TIdSipUdpServer);

    property ExceptionType:    ExceptClass read fExceptionType;
    property ExceptionMessage: String      read fExceptionMessage;
  end;

  TestTIdSipUdpServer = class(TThreadingTestCase)
  private
    Client:           TIdUDPClient;
    Server:           TIdSipUdpServer;

    procedure CheckRequest(Sender: TObject; const Request: TIdSipRequest);
    procedure CheckResponse(Sender: TObject; const Response: TIdSipResponse);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRequest;
    procedure TestResponse;
  end;

implementation

uses
  SyncObjs, TestFramework;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipUdpServer unit tests');
  Result.AddTest(TestTIdSipUdpServer.Suite);
end;

//*******************************************************************************
//* TIdSipUdpServerThread                                                       *
//*******************************************************************************
//* TIdSipUdpServerThread Public methods ****************************************

constructor TIdSipUdpServerThread.Create(Server: TIdSipUdpServer);
begin
  inherited Create(true);
  fServer := Server;
end;

//* TIdSipUdpServerThread Protected methods *************************************

procedure TIdSipUdpServerThread.Execute;
begin
end;

//* TIdSipUdpServerThread Private methods ***************************************
//* TIdSipUdpServerThread Published methods *************************************

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
    CheckEquals(7,            Request.OtherHeaders.Count, 'OtherHeaders Count');
    CheckEquals(29,           Request.ContentLength,      'ContentLength');

  CheckEquals('Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds',
              Request.OtherHeaders[0],
              'OtherHeaders[0]');
  CheckEquals('Max-Forwards: 70',
              Request.OtherHeaders[1],
              'OtherHeaders[1]');
  CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',
              Request.OtherHeaders[2],
              'OtherHeaders[2]');
  CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',
              Request.OtherHeaders[3],
              'OtherHeaders[3]');
  CheckEquals('Call-ID: a84b4c76e66710@gw1.leo_ix.org',
              Request.OtherHeaders[4],
              'OtherHeaders[4]');
  CheckEquals('CSeq: 314159 INVITE',
              Request.OtherHeaders[5],
              'OtherHeaders[5]');
  CheckEquals('Contact: <sip:wintermute@tessier-ashpool.co.lu>',
              Request.OtherHeaders[6],
              'OtherHeaders[6]');

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
    CheckEquals('SIP/2.0',   Response.SipVersion,         'SipVersion');
    CheckEquals(486,         Response.StatusCode,         'StatusCode');
    CheckEquals('Busy Here', Response.StatusText,         'StatusText');
    CheckEquals(7,           Response.OtherHeaders.Count, 'OtherHeaders Count');
    CheckEquals(29,          Response.ContentLength,      'ContentLength');

  CheckEquals('Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds',
              Response.OtherHeaders[0],
              'OtherHeaders[0]');
  CheckEquals('Max-Forwards: 70',
              Response.OtherHeaders[1],
              'OtherHeaders[1]');
  CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',
              Response.OtherHeaders[2],
              'OtherHeaders[2]');
  CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',
              Response.OtherHeaders[3],
              'OtherHeaders[3]');
  CheckEquals('Call-ID: a84b4c76e66710@gw1.leo_ix.org',
              Response.OtherHeaders[4],
              'OtherHeaders[4]');
  CheckEquals('CSeq: 314159 INVITE',
              Response.OtherHeaders[5],
              'OtherHeaders[5]');
  CheckEquals('Contact: <sip:wintermute@tessier-ashpool.co.lu>',
              Response.OtherHeaders[6],
              'OtherHeaders[6]');

  CheckEquals('I am a message. Hear me roar!', Response.Body, 'Body');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

//* TestTIdSipUdpServer Published methods ***************************************

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

  if (Self.ThreadEvent.WaitFor(5000) <> wrSignaled) then
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

  if (Self.ThreadEvent.WaitFor(5000) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

initialization
  RegisterTest('SIP server using UDP', Suite);
end.
