unit TestIdSipUdpServer;

interface

uses
  Classes, IdSipMessage, IdSipUdpServer, IdUDPClient, SysUtils, TestFrameworkEx;

type
  TestTIdSipUdpServer = class(TThreadingTestCase, IIdSipMessageListener)
  private
    CheckReceivedRequest:  TIdSipRequestEvent;
    CheckReceivedResponse: TIdSipResponseEvent;
    Client:                TIdUDPClient;
    Parser:                TIdSipParser;
    Server:                TIdSipUdpServer;

    procedure AcknowledgeEvent(Sender: TObject; const Request: TIdSipRequest); overload;
    procedure AcknowledgeEvent(Sender: TObject; const Response: TIdSipResponse); overload;
    procedure CheckReceivedParamDifferentIPv4SentBy(Sender: TObject; const Request: TIdSipRequest);
    procedure CheckReceivedParamFQDNSentBy(Sender: TObject; const Request: TIdSipRequest);
    procedure CheckReceivedParamIPv4SentBy(Sender: TObject; const Request: TIdSipRequest);
    procedure CheckRequest(Sender: TObject; const Request: TIdSipRequest);
    procedure CheckTortureTest16;
    procedure CheckTortureTest17;
    procedure CheckTortureTest19;
    procedure CheckTortureTest21;
    procedure CheckTortureTest22;
    procedure CheckTortureTest23;
    procedure CheckTortureTest35;
    procedure CheckTortureTest40;
//    procedure CheckTortureTest41;
    procedure OnReceiveRequest(const Request: TIdSipRequest);
    procedure OnReceiveResponse(const Response: TIdSipResponse);
    function  ReadResponse: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddMessageListener;
    procedure TestListenerReceiveRequest;
    procedure TestListenerReceiveResponse;
    procedure TestMalformedRequest;
    procedure TestMalformedResponse;
    procedure TestReceivedParamDifferentIPv4SentBy;
    procedure TestReceivedParamFQDNSentBy;
    procedure TestReceivedParamIPv4SentBy;
    procedure TestRemoveMessageListener;
    procedure TestRequest;
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
  IdSipConsts, IdSimpleParser, IdSocketHandle, SyncObjs, TestFramework,
  TestFrameworkSip, TestMessages;

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
var
  Binding: TIdSocketHandle;
begin
  inherited SetUp;

  Self.Client := TIdUDPClient.Create(nil);
  Self.Server := TIdSipUdpServer.Create(nil);
  Self.Server.Bindings.Clear;
  Binding := Self.Server.Bindings.Add;
  Binding.IP := '127.0.0.1';
  Binding.Port := IdPORT_SIP;

  Self.Server.AddMessageListener(Self);
  Self.Server.Active := true;
  Self.Client.Host := '127.0.0.1';
  Self.Client.Port := Server.DefaultPort;

  Self.Parser := TIdSipParser.Create;
end;

procedure TestTIdSipUdpServer.TearDown;
begin
  Self.Parser.Free;

  Self.Server.Active := false;

  Self.Server.Free;
  Self.Client.Free;

  inherited TearDown;
end;

//* TestTIdSipUdpServer Private methods *****************************************

procedure TestTIdSipUdpServer.AcknowledgeEvent(Sender: TObject; const Request: TIdSipRequest);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUdpServer.AcknowledgeEvent(Sender: TObject; const Response: TIdSipResponse);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUdpServer.CheckReceivedParamDifferentIPv4SentBy(Sender: TObject; const Request: TIdSipRequest);
begin
  Self.CheckReceivedParamFQDNSentBy(Sender, Request);
end;

procedure TestTIdSipUdpServer.CheckReceivedParamFQDNSentBy(Sender: TObject; const Request: TIdSipRequest);
begin
  try
    CheckNotEquals('', Request.LastHop.Received, 'Received param not appended by transport layer');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUdpServer.CheckReceivedParamIPv4SentBy(Sender: TObject; const Request: TIdSipRequest);
begin
  try
    CheckEquals('', Request.LastHop.Received, 'Received param appended by transport layer');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

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

procedure TestTIdSipUdpServer.CheckTortureTest16;
var
  Response: TIdSipResponse;
begin
  Response := Self.Parser.ParseAndMakeResponse(Self.ReadResponse);
  try
    CheckEquals(SipVersion,    Response.SipVersion, 'SipVersion');
    CheckEquals(SIPBadRequest, Response.StatusCode, 'StatusCode');

    CheckEquals(Format(UnexpectedMessageLength, [154, 9999]),
                Response.StatusText,
                'StatusText');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUdpServer.CheckTortureTest17;
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

procedure TestTIdSipUdpServer.CheckTortureTest19;
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

procedure TestTIdSipUdpServer.CheckTortureTest21;
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

procedure TestTIdSipUdpServer.CheckTortureTest22;
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

procedure TestTIdSipUdpServer.CheckTortureTest23;
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

procedure TestTIdSipUdpServer.CheckTortureTest35;
var
  Response: TIdSipResponse;
begin
  Response := Self.Parser.ParseAndMakeResponse(Self.ReadResponse);
  try
    CheckEquals(SipVersion,         Response.SipVersion, 'SipVersion');
    CheckEquals(SIPBadRequest,      Response.StatusCode, 'StatusCode');
    CheckEquals(Format(MalformedToken, [ExpiresHeader, 'Expires: 0 0l@company.com']),
                Response.StatusText,
                'StatusText');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUdpServer.CheckTortureTest40;
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
procedure TestTIdSipUdpServer.CheckTortureTest41;
var
  Response: TIdSipResponse;
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

procedure TestTIdSipUdpServer.OnReceiveRequest(const Request: TIdSipRequest);
begin
  if Assigned(Self.CheckReceivedRequest) then
    Self.CheckReceivedRequest(Self, Request);
end;

procedure TestTIdSipUdpServer.OnReceiveResponse(const Response: TIdSipResponse);
begin
  if Assigned(Self.CheckReceivedResponse) then
    Self.CheckReceivedResponse(Self, Response);
end;

function TestTIdSipUdpServer.ReadResponse: String;
begin
  Result := Self.Client.ReceiveString(DefaultTimeout);
end;

//* TestTIdSipUdpServer Published methods ***************************************

procedure TestTIdSipUdpServer.TestAddMessageListener;
begin
  Self.CheckReceivedRequest := Self.AcknowledgeEvent;

  // We don't need to add the listener because that's done in the SetUp method

  Self.Client.Send(BasicRequest);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipUdpServer.TestListenerReceiveRequest;
var
  Listener: TIdSipTestMessageListener;
begin
  Self.Server.RemoveMessageListener(Self);
  Self.CheckReceivedRequest := Self.AcknowledgeEvent;

  Listener := TIdSipTestMessageListener.Create;
  try
    Self.Server.AddMessageListener(Listener);
    Self.Server.AddMessageListener(Self);

    Self.Client.Send(BasicRequest);

    if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);
    Check(Listener.ReceivedRequest, 'Not all listeners received the Request');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipUdpServer.TestListenerReceiveResponse;
var
  Listener: TIdSipTestMessageListener;
begin
  Self.Server.RemoveMessageListener(Self);
  Self.CheckReceivedResponse := Self.AcknowledgeEvent;

  Listener := TIdSipTestMessageListener.Create;
  try
    Self.Server.AddMessageListener(Listener);
    Self.Server.AddMessageListener(Self);

    Self.Client.Send(BasicResponse);

    if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);
    Check(Listener.ReceivedResponse, 'Not all listeners received the Response');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipUdpServer.TestMalformedRequest;
var
  Response: TIdSipResponse;
begin
  // note the semicolon in the SIP-version
  Self.Client.Send('INVITE sip:tentacleface@rlyeh.org.au SIP/;2.0'#13#10
                 + 'To: "Cthulhu" <tentacleface@rlyeh.org.au>'#13#10
                 + 'From: "Great Old Ones" <greatoldones@outerdarkness.lu>'#13#10
                 + 'CSeq: 0 INVITE'#13#10
                 + 'Call-ID: 0'#13#10
                 + 'Max-Forwards: 5'#13#10
                 + 'Via: SIP/2.0/UDP 127.0.0.1:5060'#13#10
                 + #13#10);

  Response := Self.Parser.ParseAndMakeResponse(Client.ReceiveString(DefaultTimeout));
  try
    CheckEquals(SipVersion,
                Response.SipVersion,
                'SIP-Version');
    CheckEquals(SIPBadRequest,
                Response.StatusCode,
                'Status-Code');
    CheckEquals(Format(InvalidSipVersion, ['SIP/;2.0']),
                Response.StatusText, 'Status-Text');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUdpServer.TestMalformedResponse;
var
  Method: String;
begin
  Method := 'SIP/;2.0';

  // Note the semicolon in the SIP-version. Note, too, how this response is
  // treated as a REQUEST.
  Self.Client.Send(Method + ' 200 OK'#13#10
                 + #13#10);

  CheckEquals('',
              Client.ReceiveString(DefaultTimeout),
              'Response not just dropped on the floor');
end;

procedure TestTIdSipUdpServer.TestReceivedParamDifferentIPv4SentBy;
begin
  Self.CheckReceivedRequest := Self.CheckReceivedParamDifferentIPv4SentBy;

  Self.Client.Send(Format(BasicRequest, [ViaDifferentIP]));

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipUdpServer.TestReceivedParamFQDNSentBy;
begin
  Self.CheckReceivedRequest := Self.CheckReceivedParamFQDNSentBy;

  Self.Client.Send(Format(BasicRequest, [ViaFQDN]));

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipUdpServer.TestReceivedParamIPv4SentBy;
begin
  Self.CheckReceivedRequest := Self.CheckReceivedParamIPv4SentBy;

  Self.Client.Send(Format(BasicRequest, [ViaIP]));

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipUdpServer.TestRemoveMessageListener;
begin
  Self.CheckReceivedRequest := Self.AcknowledgeEvent;
  Self.Server.RemoveMessageListener(Self);

  Self.Client.Send(BasicRequest);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrTimeout) then
    Fail('Listener wasn''t removed');
end;

procedure TestTIdSipUdpServer.TestRequest;
begin
  Self.CheckReceivedRequest := Self.CheckRequest;

  Self.Client.Send(Format(BasicRequest, [ViaFQDN]));

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipUdpServer.TestTortureTest16;
begin
  Self.Client.Send(TortureTest16);

  Self.CheckTortureTest16;
end;

procedure TestTIdSipUdpServer.TestTortureTest17;
begin
  Self.Client.Send(TortureTest17);

  Self.CheckTortureTest17;
end;

procedure TestTIdSipUdpServer.TestTortureTest19;
begin
  Self.Client.Send(TortureTest19);

  Self.CheckTortureTest19;
end;

procedure TestTIdSipUdpServer.TestTortureTest21;
begin
  Self.Client.Send(TortureTest21);

  Self.CheckTortureTest21;
end;

procedure TestTIdSipUdpServer.TestTortureTest22;
begin
  Self.Client.Send(TortureTest22);

  Self.CheckTortureTest22;
end;

procedure TestTIdSipUdpServer.TestTortureTest23;
begin
  Self.Client.Send(TortureTest23);

  Self.CheckTortureTest23;
end;

procedure TestTIdSipUdpServer.TestTortureTest35;
begin
  Self.Client.Send(TortureTest35);

  Self.CheckTortureTest35;
end;

procedure TestTIdSipUdpServer.TestTortureTest40;
begin
  Self.Client.Send(TortureTest40);

  Self.CheckTortureTest40;
end;
{
procedure TestTIdSipUdpServer.TestTortureTest41;
begin
  Self.Client.Send(TortureTest41);

  Self.CheckTortureTest41;
end;
}
initialization
  RegisterTest('SIP server using UDP', Suite);
end.
