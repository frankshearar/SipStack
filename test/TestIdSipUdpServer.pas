{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipUdpServer;

interface

uses
  Classes, IdSipMessage, IdSipTcpServer, IdSipUdpServer, IdUDPClient, SysUtils,
  TestFrameworkEx;

type
  TestTIdSipUdpServer = class(TThreadingTestCase,
                              IIdSipMessageListener)
  private
    CheckReceivedRequest:     TIdSipRequestEvent;
    CheckReceivedResponse:    TIdSipResponseEvent;
    Client:                   TIdUDPClient;
    NotifiedMalformedMessage: Boolean;
    Parser:                   TIdSipParser;
    ReceivedResponse:         Boolean;
    Server:                   TIdSipUdpServer;

    procedure AcknowledgeEvent(Sender: TObject;
                               Request: TIdSipRequest); overload;
    procedure AcknowledgeEvent(Sender: TObject;
                               Response: TIdSipResponse;
                               ReceivedFrom: TIdSipConnectionBindings); overload;
    procedure CheckRejectFragmentedRequestProperly(Sender: TObject;
                                                   Request: TIdSipRequest);
    procedure CheckRejectFragmentedResponseProperly(Sender: TObject;
                                                    Response: TIdSipResponse;
                                                    ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckRequest(Sender: TObject;
                           Request: TIdSipRequest);
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnMalformedMessage(const Msg: String;
                                 const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdSipConnectionBindings);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddMessageListener;
    procedure TestListenerReceiveRequest;
    procedure TestListenerReceiveResponse;
    procedure TestNotifyFragmentedRequestProperly;
    procedure TestRemoveMessageListener;
    procedure TestRequest;
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

  Self.NotifiedMalformedMessage := false;
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

procedure TestTIdSipUdpServer.AcknowledgeEvent(Sender: TObject;
                                               Request: TIdSipRequest);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUdpServer.AcknowledgeEvent(Sender: TObject;
                                               Response: TIdSipResponse;
                                               ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUdpServer.CheckRejectFragmentedRequestProperly(Sender: TObject;
                                                                   Request: TIdSipRequest);
begin
  try
    Check(Request.IsMalformed,
          'Fragment (that looks like a request) not marked as malformed');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUdpServer.CheckRejectFragmentedResponseProperly(Sender: TObject;
                                                                    Response: TIdSipResponse;
                                                                    ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    Check(Response.IsMalformed,
          'Fragment (that by default looks like a response) not marked as malformed');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUdpServer.CheckRequest(Sender: TObject;
                                           Request: TIdSipRequest);
begin
  try
    CheckEquals(MethodInvite, Request.Method,        'Method');
    CheckEquals('SIP/2.0',    Request.SipVersion,    'SipVersion');
    CheckEquals(29,           Request.ContentLength, 'ContentLength');
    CheckEquals(70,           Request.MaxForwards,   'Max-Forwards');

    CheckEquals('I am a message. Hear me roar!', Request.Body, 'Body');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUdpServer.OnException(E: Exception;
                                          const Reason: String);
begin
end;

procedure TestTIdSipUdpServer.OnMalformedMessage(const Msg: String;
                                                 const Reason: String);
begin
  Self.NotifiedMalformedMessage := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUdpServer.OnReceiveRequest(Request: TIdSipRequest;
                                               ReceivedFrom: TIdSipConnectionBindings);
begin
  if Assigned(Self.CheckReceivedRequest) then
    Self.CheckReceivedRequest(Self, Request);
end;

procedure TestTIdSipUdpServer.OnReceiveResponse(Response: TIdSipResponse;
                                                ReceivedFrom: TIdSipConnectionBindings);
begin
  if Assigned(Self.CheckReceivedResponse) then
    Self.CheckReceivedResponse(Self, Response, ReceivedFrom);
  Self.ReceivedResponse := true;
  Self.ThreadEvent.SetEvent;
end;

//* TestTIdSipUdpServer Published methods ***************************************

procedure TestTIdSipUdpServer.TestAddMessageListener;
begin
  Self.CheckReceivedRequest := Self.AcknowledgeEvent;

  // We don't need to add the listener because that's done in the SetUp method

  Self.Client.Send(BasicRequest);

  Self.WaitForSignaled;
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

    Self.WaitForSignaled;
    
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

    Self.WaitForSignaled;
    Check(Listener.ReceivedResponse, 'Not all listeners received the Response');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipUdpServer.TestNotifyFragmentedRequestProperly;
var
  Divider: Integer;
  Msg: String;
begin
  Self.CheckReceivedRequest := Self.CheckRejectFragmentedRequestProperly;
  Self.CheckReceivedResponse := Self.CheckRejectFragmentedResponseProperly;

  Msg := Format(BasicRequest, [Self.Server.Bindings[0].IP]);

  Divider := Length(Msg) div 2;
  Self.Client.Send(Copy(Msg, 1, Divider));

  Self.WaitForSignaled;
  Self.ThreadEvent.ResetEvent;

  Self.Client.Send(Copy(Msg, Divider + 1, Length(Msg)));
  Self.WaitForSignaled;
end;

procedure TestTIdSipUdpServer.TestRemoveMessageListener;
begin
  Self.CheckReceivedRequest := Self.AcknowledgeEvent;
  Self.Server.RemoveMessageListener(Self);

  Self.Client.Send(BasicRequest);

  Self.WaitForTimeout('Listener wasn''t removed');
end;

procedure TestTIdSipUdpServer.TestRequest;
begin
  Self.CheckReceivedRequest := Self.CheckRequest;

  Self.Client.Send(Format(BasicRequest, [ViaFQDN]));

  Self.WaitForSignaled;
end;

initialization
  RegisterTest('SIP server using UDP', Suite);
end.
