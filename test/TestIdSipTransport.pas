unit TestIdSipTransport;

interface

uses
  Classes, IdSipMessage, IdSipTcpClient, IdSipTcpServer, IdSipTransport,
  IdSocketHandle, IdTcpServer, SyncObjs, SysUtils, TestFramework,
  TestFrameworkEx;

type
  TIdSipTransportSubclass = class(TIdSipTcpTransport)
  public
    procedure NotifyTransportListeners(const Request: TIdSipRequest); overload;
    procedure NotifyTransportListeners(const Response: TIdSipResponse); overload;
    procedure NotifyTransportSendingListeners(const Request: TIdSipRequest); overload;
    procedure NotifyTransportSendingListeners(const Response: TIdSipResponse); overload;
  end;

  TestTIdSipTransportEventNotifications = class(TTestCase,
                                                IIdSipTransportListener,
                                                IIdSipTransportSendingListener)
  private
    ReceivedRequest:  Boolean;
    ReceivedResponse: Boolean;
    Request:          TIdSipRequest;
    Response:         TIdSipResponse;
    SentRequest:      Boolean;
    SentResponse:     Boolean;
    Transport:        TIdSipTransportSubclass;

    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transport: TIdSipTransport);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transport: TIdSipTransport);
    procedure OnSendRequest(const Request: TIdSipRequest;
                            const Transport: TIdSipTransport);
    procedure OnSendResponse(const Response: TIdSipResponse;
                             const Transport: TIdSipTransport);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddTransportListener;
    procedure TestAddTransportSendingListener;
    procedure TestAllListenersReceiveRequests;
    procedure TestAllListenersReceiveResponses;
    procedure TestAllListenersSendRequests;
    procedure TestAllListenersSendResponses;
    procedure TestRemoveTransportListener;
    procedure TestRemoveTransportSendingListener;
  end;

  TTestIdSipRequestEvent = procedure(Sender: TObject;
                                     const R: TIdSipRequest) of object;
  TTestIdSipResponseEvent = procedure(Sender: TObject;
                                      const R: TIdSipResponse) of object;

  TestTIdSipTransport = class(TThreadingTestCase,
                              IIdSipTransportListener)
  protected
    CheckingRequestEvent:  TTestIdSipRequestEvent;
    CheckingResponseEvent: TTestIdSipResponseEvent;
    HighPortTransport:     TIdSipTransport;
    LowPortTransport:      TIdSipTransport;
    ReceivedRequest:       Boolean;
    ReceivedResponse:      Boolean;
    Request:               TIdSipRequest;
    Response:              TIdSipResponse;
    WrongServer:           Boolean;

    procedure CheckCanReceiveRequest(Sender: TObject;
                                     const R: TIdSipRequest);
    procedure CheckCanReceiveResponse(Sender: TObject;
                                      const R: TIdSipResponse);
    procedure CheckDiscardResponseWithUnknownSentBy(Sender: TObject;
                                                    const R: TIdSipResponse);
    procedure CheckReceivedParamDifferentIPv4SentBy(Sender: TObject;
                                                    const Request: TIdSipRequest);
    procedure CheckReceivedParamFQDNSentBy(Sender: TObject;
                                           const Request: TIdSipRequest);
    procedure CheckReceivedParamIPv4SentBy(Sender: TObject;
                                           const Request: TIdSipRequest);
    procedure CheckSendRequestTopVia(Sender: TObject;
                                     const R: TIdSipRequest);
    function  DefaultPort: Cardinal; virtual;
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transport: TIdSipTransport);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transport: TIdSipTransport);
    procedure ReturnResponse(Sender: TObject;
                             const R: TIdSipRequest);
    procedure SendOkResponse(const Transport: TIdSipTransport);
    function  TransportType: TIdSipTransportClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanReceiveRequest;
    procedure TestCanReceiveResponse;
    procedure TestCanReceiveUnsolicitedResponse;
    procedure TestTransportFor;
    procedure TestDiscardResponseWithUnknownSentBy;
    procedure TestReceivedParamDifferentIPv4SentBy;
    procedure TestReceivedParamFQDNSentBy;
    procedure TestReceivedParamIPv4SentBy;
    procedure TestSendRequest;
    procedure TestSendRequestTopVia;
    procedure TestSendResponse;
    procedure TestSendResponseWithReceivedParam;
  end;

  TestTIdSipTCPTransport = class(TestTIdSipTransport)
  protected
    function  TransportType: TIdSipTransportClass; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
  end;

  TestTIdSipTLSTransport = class(TestTIdSipTransport)
  private
    procedure DoOnPassword(var Password: String);
    procedure SetUpTls(Transport: TIdSipTransport);
  protected
    function  DefaultPort: Cardinal; override;
    function  TransportType: TIdSipTransportClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
  end;

  TestTIdSipUDPTransport = class(TestTIdSipTransport,
                                 IIdSipMessageListener)
  private
    MaddrTransport: TIdSipTransport;
    RPort:          Cardinal;
    RPortEvent:     TEvent;

    procedure CheckRportParamFilledIn(Sender: TObject;
                                      const R: TIdSipRequest);
    procedure CheckLeaveNonRportRequestsUntouched(Sender: TObject;
                                                  const R: TIdSipRequest);
    procedure CheckMaddrUser(Sender: TObject;
                             const R: TIdSipResponse);
    procedure NoteSourcePort(Sender: TObject;
                             const R: TIdSipRequest);
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const ReceivedFrom: TIdSipConnectionBindings);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const ReceivedFrom: TIdSipConnectionBindings);
  protected
    function TransportType: TIdSipTransportClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
    procedure TestLeaveNonRportRequestsUntouched;
    procedure TestMaddrUsed;
    procedure TestRespectRport;
    procedure TestRportParamFilledIn;
    procedure TestRportListening;
  end;
{
  TestTIdSipSCTPTransport = class(TestTIdSipTransport)
  protected
    function  TransportType: TIdSipTransportClass; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
  end;
}

implementation

uses
  IdGlobal, IdSipHeaders, IdSipConsts, IdSipUdpServer, IdSSLOpenSSL, IdStack,
  IdTcpClient, IdUDPServer, TestMessages, TestFrameworkSip;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransport unit tests');
  Result.AddTest(TestTIdSipTransportEventNotifications.Suite);
  Result.AddTest(TestTIdSipTCPTransport.Suite);
//  Result.AddTest(TestTIdSipTLSTransport.Suite);
  Result.AddTest(TestTIdSipUDPTransport.Suite);
//  Result.AddTest(TestTIdSipSCTPTransport.Suite);
end;

//******************************************************************************
//* TIdSipTransportSubclass                                                    *
//******************************************************************************
//* TIdSipTransportSubclass Public methods *************************************

procedure TIdSipTransportSubclass.NotifyTransportListeners(const Request: TIdSipRequest);
begin
  inherited NotifyTransportListeners(Request);
end;

procedure TIdSipTransportSubclass.NotifyTransportListeners(const Response: TIdSipResponse);
begin
  inherited NotifyTransportListeners(Response);
end;

procedure TIdSipTransportSubclass.NotifyTransportSendingListeners(const Request: TIdSipRequest);
begin
  inherited NotifyTransportSendingListeners(Request);
end;

procedure TIdSipTransportSubclass.NotifyTransportSendingListeners(const Response: TIdSipResponse);
begin
  inherited NotifyTransportSendingListeners(Response);
end;

//******************************************************************************
//* TestTIdSipTransportEventNotifications                                      *
//******************************************************************************
//* TestTIdSipTransportEventNotifications Public methods ***********************

procedure TestTIdSipTransportEventNotifications.SetUp;
begin
  inherited SetUp;

  Self.ReceivedRequest  := false;
  Self.ReceivedResponse := false;
  Self.Request          := TIdSipRequest.Create;
  Self.Response         := TIdSipResponse.Create;
  Self.Transport        := TIdSipTransportSubclass.Create(0);
end;

procedure TestTIdSipTransportEventNotifications.TearDown;
begin
  Self.Transport.Free;
  Self.Response.Free;
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportEventNotifications Private methods **********************

procedure TestTIdSipTransportEventNotifications.OnReceiveRequest(const Request: TIdSipRequest;
                                                                 const Transport: TIdSipTransport);
begin
  Self.ReceivedRequest := true;
  Check(Self.Request = Request,     'Request not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnReceiveResponse(const Response: TIdSipResponse;
                                                                  const Transport: TIdSipTransport);
begin
  Self.ReceivedResponse := true;

  Check(Self.Response = Response,   'Response not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnSendRequest(const Request: TIdSipRequest;
                                                              const Transport: TIdSipTransport);
begin
  Self.SentRequest := true;
  Check(Self.Request = Request,     'Request not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnSendResponse(const Response: TIdSipResponse;
                                                               const Transport: TIdSipTransport);
begin
  Self.SentResponse := true;

  Check(Self.Response = Response,   'Response not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

//* TestTIdSipTransportEventNotifications Published methods *******************************

procedure TestTIdSipTransportEventNotifications.TestAddTransportListener;
begin
  Self.Transport.AddTransportListener(Self);

  Self.Transport.NotifyTransportListeners(Self.Request);

  Check(Self.ReceivedRequest, 'Listener wasn''t added');
end;

procedure TestTIdSipTransportEventNotifications.TestAddTransportSendingListener;
begin
  Self.Transport.AddTransportSendingListener(Self);

  Self.Transport.NotifyTransportSendingListeners(Self.Request);

  Check(Self.SentRequest, 'Listener wasn''t added');
end;

procedure TestTIdSipTransportEventNotifications.TestAllListenersReceiveRequests;
var
  Listener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Transport.AddTransportListener(Self);
    Self.Transport.AddTransportListener(Listener);

    Self.Transport.NotifyTransportListeners(Self.Request);

    Check(Self.ReceivedRequest and Listener.ReceivedRequest,
          'Not all Listeners received the request');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransportEventNotifications.TestAllListenersReceiveResponses;
var
  Listener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Transport.AddTransportListener(Self);
    Self.Transport.AddTransportListener(Listener);

    Self.Transport.NotifyTransportListeners(Self.Response);

    Check(Self.ReceivedResponse and Listener.ReceivedResponse,
          'Not all Listeners received the Response');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransportEventNotifications.TestAllListenersSendRequests;
var
  Listener: TIdSipTestTransportSendingListener;
begin
  Listener := TIdSipTestTransportSendingListener.Create;
  try
    Self.Transport.AddTransportSendingListener(Self);
    Self.Transport.AddTransportSendingListener(Listener);

    Self.Transport.NotifyTransportSendingListeners(Self.Request);

    Check(Self.SentRequest and Listener.SentRequest,
          'Not all Listeners Sent the request');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransportEventNotifications.TestAllListenersSendResponses;
var
  Listener: TIdSipTestTransportSendingListener;
begin
  Listener := TIdSipTestTransportSendingListener.Create;
  try
    Self.Transport.AddTransportSendingListener(Self);
    Self.Transport.AddTransportSendingListener(Listener);

    Self.Transport.NotifyTransportSendingListeners(Self.Response);

    Check(Self.SentResponse and Listener.SentResponse,
          'Not all Listeners Sent the Response');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransportEventNotifications.TestRemoveTransportListener;
begin
  Self.Transport.AddTransportListener(Self);
  Self.Transport.RemoveTransportListener(Self);

  Self.Transport.NotifyTransportListeners(Self.Request);

  Check(not Self.ReceivedRequest, 'Listener wasn''t removed');
end;

procedure TestTIdSipTransportEventNotifications.TestRemoveTransportSendingListener;
begin
  Self.Transport.AddTransportSendingListener(Self);
  Self.Transport.RemoveTransportSendingListener(Self);

  Self.Transport.NotifyTransportSendingListeners(Self.Request);

  Check(not Self.SentRequest, 'Listener wasn''t removed');
end;

//******************************************************************************
//* TestTIdSipTransport                                                        *
//******************************************************************************
//* TestTIdSipTransport Public methods *****************************************

procedure TestTIdSipTransport.SetUp;
var
  Binding: TIdSocketHandle;
  P:       TIdSipParser;
begin
  inherited SetUp;

  Self.ExceptionMessage := 'Response not received - event didn''t fire';

  Self.HighPortTransport := Self.TransportType.Create(Self.DefaultPort + 10000);
  Self.HighPortTransport.AddTransportListener(Self);
  Self.HighPortTransport.Timeout := 500;
  Self.HighPortTransport.HostName := IndyGetHostName;
  Self.HighPortTransport.Bindings.Clear;
  Binding := Self.HighPortTransport.Bindings.Add;
  Binding.IP := GStack.LocalAddress;
  Binding.Port := Self.DefaultPort + 10000;
  Self.HighPortTransport.Start;

  Self.LowPortTransport := Self.TransportType.Create(Self.DefaultPort);
  Self.LowPortTransport.AddTransportListener(Self);
  Self.LowPortTransport.Timeout := 500;
  Self.LowPortTransport.HostName := 'localhost';
  Self.LowPortTransport.Bindings.Clear;
  Binding := Self.LowPortTransport.Bindings.Add;
  Binding.IP := '127.0.0.1';
  Binding.Port := Self.DefaultPort;
  Self.LowPortTransport.Start;

  P := TIdSipParser.Create;
  try
    Self.Request  := P.ParseAndMakeRequest(LocalLoopRequest);
    Self.Response := P.ParseAndMakeResponse(LocalLoopResponse);
  finally
    P.Free;
  end;
  Self.Request.LastHop.SentBy     := Self.LowPortTransport.Bindings[0].IP;
  Self.Response.LastHop.Transport := Self.HighPortTransport.GetTransportType;

  Self.Request.RequestUri.Host := Self.HighPortTransport.HostName;
  Self.Request.RequestUri.Port := Self.HighPortTransport.Bindings[0].Port;

  Self.ReceivedRequest  := false;
  Self.ReceivedResponse := false;
  Self.WrongServer      := false;
end;

procedure TestTIdSipTransport.TearDown;
begin
  Self.Response.Free;
  Self.Request.Free;

  Self.LowPortTransport.Stop;
  Self.HighPortTransport.Stop;

  Self.LowPortTransport.Free;
  Self.HighPortTransport.Free;

  inherited TearDown;
end;

//* TestTIdSipTransport Protected methods **************************************

procedure TestTIdSipTransport.CheckCanReceiveRequest(Sender: TObject;
                                                     const R: TIdSipRequest);
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
                                                      const R: TIdSipResponse);
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
                                                                    const R: TIdSipResponse);
begin
  Self.ReceivedResponse := true;
end;

procedure TestTIdSipTransport.CheckReceivedParamDifferentIPv4SentBy(Sender: TObject;
                                                                    const Request: TIdSipRequest);
begin
  Self.CheckReceivedParamFQDNSentBy(Sender, Request);
end;

procedure TestTIdSipTransport.CheckReceivedParamFQDNSentBy(Sender: TObject;
                                                           const Request: TIdSipRequest);
begin
  try
    Check(Request.LastHop.HasReceived,
          'Received param not appended by transport layer');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckReceivedParamIPv4SentBy(Sender: TObject;
                                                           const Request: TIdSipRequest);
begin
  try
    Check(not Request.LastHop.HasReceived,
          'Received param appended by transport layer');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckSendRequestTopVia(Sender: TObject;
                                                     const R: TIdSipRequest);
begin
  try
    Check(Self.HighPortTransport.GetTransportType = R.LastHop.Transport,
          'Incorrect transport specified');

    Check(R.LastHop.HasBranch, 'Branch parameter missing');

//    CheckEquals(Self.LowPortTransport.HostName,
//                R.Las.Hop.SentBy,
//                'SentBy incorrect');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

function TestTIdSipTransport.DefaultPort: Cardinal;
begin
  Result := IdPORT_SIP;
end;

procedure TestTIdSipTransport.OnReceiveRequest(const Request: TIdSipRequest;
                                               const Transport: TIdSipTransport);
begin
  if Assigned(Self.CheckingRequestEvent) then
    Self.CheckingRequestEvent(Transport, Request);

//  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTransport.OnReceiveResponse(const Response: TIdSipResponse;
                                                const Transport: TIdSipTransport);
begin
  if Assigned(Self.CheckingResponseEvent) then
    Self.CheckingResponseEvent(Transport, Response);

//  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTransport.ReturnResponse(Sender: TObject;
                                             const R: TIdSipRequest);
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

procedure TestTIdSipTransport.SendOkResponse(const Transport: TIdSipTransport);
begin
  Self.Response.StatusCode := SIPOK;

  Transport.Send(Self.Response);
end;

function TestTIdSipTransport.TransportType: TIdSipTransportClass;
begin
  Result := nil;
end;

//* TestTIdSipTransport Published methods **************************************

procedure TestTIdSipTransport.TestCanReceiveRequest;
begin
  // LowPortTransport sends an INVITE to HighPortTransport.
  // We check that HighPortTransport did actually get it.
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.LowPortTransport.Send(Self.Request);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);

  Check(Self.ReceivedRequest, 'Request not received');
end;

procedure TestTIdSipTransport.TestCanReceiveResponse;
begin
  // LowPortTransport sends an INVITE to HighPortTransport.
  // HighPortTransport is set to immediately respond with a 200 OK.
  // We then check that LowPortTransport got the returned response.
  Self.CheckingRequestEvent  := Self.ReturnResponse;
  Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;

  Self.LowPortTransport.Send(Self.Request);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);

  Check(Self.ReceivedResponse, 'Response not received');
end;

procedure TestTIdSipTransport.TestCanReceiveUnsolicitedResponse;
begin
  Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;

  Self.HighPortTransport.Send(Self.Response);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);

  Check(Self.ReceivedResponse, 'Response not received');
end;

procedure TestTIdSipTransport.TestTransportFor;
begin
  CheckEquals(TIdSipTCPTransport,  TIdSipTransport.TransportFor(sttTCP),  'TCP');
  CheckEquals(TIdSipTLSTransport,  TIdSipTransport.TransportFor(sttTLS),  'TLS');
  CheckEquals(TIdSipUDPTransport,  TIdSipTransport.TransportFor(sttUDP),  'UDP');
  CheckEquals(TIdSipSCTPTransport, TIdSipTransport.TransportFor(sttSCTP), 'SCTP');
end;

procedure TestTIdSipTransport.TestDiscardResponseWithUnknownSentBy;
begin
  Self.CheckingResponseEvent := Self.CheckDiscardResponseWithUnknownSentBy;

  // If we don't set the received param we won't be able to send
  // the message to the right SIP server. No, you'd never do this
  // in production, because it's wilfully wrong.
  Self.Response.LastHop.SentBy := 'unknown.host';
  Self.Response.LastHop.Received := Self.LowPortTransport.Bindings[0].IP;
  Self.LowPortTransport.Send(Self.Response);

  Check(wrTimeout = Self.ThreadEvent.WaitFor(DefaultTimeout),
        'Response not silently discarded');
end;

procedure TestTIdSipTransport.TestReceivedParamDifferentIPv4SentBy;
begin
  Self.CheckingRequestEvent := Self.CheckReceivedParamDifferentIPv4SentBy;

  Self.Request.LastHop.SentBy := '127.0.0.3';
  Self.LowPortTransport.Send(Self.Request);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTransport.TestReceivedParamFQDNSentBy;
begin
  Self.CheckingRequestEvent := Self.CheckReceivedParamFQDNSentBy;

  Self.Request.LastHop.SentBy := 'localhost';
  Self.LowPortTransport.Send(Self.Request);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTransport.TestReceivedParamIPv4SentBy;
begin
  Self.CheckingRequestEvent := Self.CheckReceivedParamIPv4SentBy;
  // This is a bit of a hack. Messages from the loopback interface
  // to the NIC interface arrive with a source address of the NIC
  // interface's IP, not the loopback interface's IP.
  Self.LowPortTransport.HostName := Self.HighPortTransport.Bindings[0].IP;
  Self.LowPortTransport.Send(Self.Request);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTransport.TestSendRequest;
begin
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.LowPortTransport.Send(Self.Request);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);

  Check(Self.ReceivedRequest, 'Request not received');
end;

procedure TestTIdSipTransport.TestSendRequestTopVia;
begin
  Self.CheckingRequestEvent := Self.CheckSendRequestTopVia;
  Self.LowPortTransport.Send(Self.Request);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTransport.TestSendResponse;
begin
  Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;

  Self.LowPortTransport.Send(Self.Response);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(5000)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);

  Check(Self.ReceivedResponse, 'Response not received');
end;

procedure TestTIdSipTransport.TestSendResponseWithReceivedParam;
var
  HighPortListener: TIdSipTestTransportListener;
  LowPortListener:  TIdSipTestTransportListener;
begin
  HighPortListener := TIdSipTestTransportListener.Create;
  try
    Self.HighPortTransport.AddTransportListener(HighPortListener);
    try
      LowPortListener := TIdSipTestTransportListener.Create;
      try
        Self.LowPortTransport.AddTransportListener(LowPortListener);
        try
          Check(Self.LowPortTransport.Bindings.Count > 0,
                'Sanity check on LowPortTransport''s bindings');

          Self.Response.LastHop.Received := Self.LowPortTransport.Bindings[0].IP;
          Self.HighPortTransport.Send(Self.Response);

          // It's not perfect, but anyway. We need to wait long enough for
          // LowPortTransport to get its response.
          IdGlobal.Sleep(500);

          Check(LowPortListener.ReceivedResponse
                and not HighPortListener.ReceivedResponse,
                'Received param in top Via header ignored - '
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

//******************************************************************************
//* TestTIdSipTCPTransport                                                     *
//******************************************************************************
//* TestTIdSipTCPTransport Protected methods ***********************************

function TestTIdSipTCPTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipTcpTransport;
end;

//* TestTIdSipTCPTransport Published methods ***********************************

procedure TestTIdSipTCPTransport.TestGetTransportType;
begin
  Check(Self.HighPortTransport.GetTransportType = sttTCP, 'Transport type');
end;

procedure TestTIdSipTCPTransport.TestIsReliable;
begin
  Check(Self.HighPortTransport.IsReliable,
        'TCP transport not marked as reliable');
end;

procedure TestTIdSipTCPTransport.TestIsSecure;
begin
  Check(not Self.HighPortTransport.IsSecure, 'TCP transport marked as secure');
end;

//******************************************************************************
//* TestTIdSipTLSTransport                                                     *
//******************************************************************************
//* TestTIdSipTLSTransport Public methods **************************************

procedure TestTIdSipTLSTransport.SetUp;
begin
  inherited SetUp;

  Self.SetUpTls(Self.HighPortTransport);
  Self.SetUpTls(Self.LowPortTransport);

  Self.Request.RequestUri.Scheme := SipsScheme;
  Self.Response.LastHop.Transport := Self.HighPortTransport.GetTransportType;
{
  Self.Response.LastHop.Value := StringReplace(Self.Response.LastHop.AsString,
                                               'TCP',
                                               'TLS',
                                               []);
}
end;

//* TestTIdSipTLSTransport Protected methods ***********************************

function TestTIdSipTLSTransport.DefaultPort: Cardinal;
begin
  Result := IdPORT_SIPS;
end;

function TestTIdSipTLSTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipTlsTransport;
end;

//* TestTIdSipTLSTransport Private methods *************************************

procedure TestTIdSipTLSTransport.DoOnPassword(var Password: String);
begin
  Password := CertPasswd;
end;

procedure TestTIdSipTLSTransport.SetUpTls(Transport: TIdSipTransport);
var
  TLS: TIdSipTLSTransport;
begin
  CheckEquals(TIdSipTLSTransport.ClassName,
              Transport.ClassName,
              'TestTIdSipTLSTransport.SetUpTls');

  TLS := Transport as TIdSipTLSTransport;

  TLS.OnGetPassword     := Self.DoOnPassword;
  TLS.RootCertificate   := RootCert;
  TLS.ServerCertificate := ServerCert;
  TLS.ServerKey         := ServerKey;
end;

//* TestTIdSipTLSTransport Published methods ***********************************

procedure TestTIdSipTLSTransport.TestGetTransportType;
begin
  Check(sttTLS = Self.HighPortTransport.GetTransportType, 'Transport type');
end;

procedure TestTIdSipTLSTransport.TestIsReliable;
begin
  Check(Self.HighPortTransport.IsReliable,
        'TLS transport not marked as reliable');
end;

procedure TestTIdSipTLSTransport.TestIsSecure;
begin
  Check(Self.HighPortTransport.IsSecure,
        'TLS transport not marked as secure');
end;

//******************************************************************************
//* TestTIdSipUDPTransport                                                     *
//******************************************************************************
//* TestTIdSipUDPTransport Public methods **************************************

procedure TestTIdSipUDPTransport.SetUp;
var
  Binding: TIdSocketHandle;
begin
  inherited SetUp;

  (Self.LowPortTransport  as TIdSipUDPTransport).CleanerThreadPollTime := 10;
  (Self.HighPortTransport as TIdSipUDPTransport).CleanerThreadPollTime := 10;

  Self.MaddrTransport := Self.TransportType.Create(IdPORT_SIP);
  (Self.MaddrTransport as TIdSipUDPTransport).CleanerThreadPollTime := 10;
  Self.MaddrTransport.AddTransportListener(Self);
  Self.MaddrTransport.HostName := 'localhost';
  Binding := Self.MaddrTransport.Bindings.Add;
  Binding.IP := '127.0.0.2';
  Binding.Port := IdPORT_SIP;

  Self.RPortEvent := TSimpleEvent.Create;
end;

procedure TestTIdSipUDPTransport.TearDown;
begin
  Self.MaddrTransport.Free;
  Self.RPortEvent.Free;

  inherited TearDown;
end;

//* TestTIdSipUDPTransport Protected methods ***********************************

function TestTIdSipUDPTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipUdpTransport;
end;

//* TestTIdSipUDPTransport Published methods ***********************************

procedure TestTIdSipUDPTransport.CheckRportParamFilledIn(Sender: TObject;
                                                         const R: TIdSipRequest);
begin
  CheckNotEquals('',
                 R.LastHop.Params[RPortParam],
                 'Transport didn''t fill in the rport param');

  Self.NoteSourcePort(Sender, R);
end;

procedure TestTIdSipUDPTransport.CheckLeaveNonRportRequestsUntouched(Sender: TObject;
                                                                     const R: TIdSipRequest);
begin
  try
    Check(not R.LastHop.HasRport, 'rport param added by transport');
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUDPTransport.CheckMaddrUser(Sender: TObject;
                                                const R: TIdSipResponse);
begin
  try
    Check(Sender = Self.MaddrTransport,
          'An unexpected transport received the response');
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUDPTransport.NoteSourcePort(Sender: TObject;
                                                const R: TIdSipRequest);
begin
  try
    Self.RPort := R.LastHop.RPort;
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUDPTransport.OnReceiveRequest(const Request: TIdSipRequest;
                                                  const ReceivedFrom: TIdSipConnectionBindings);
begin
end;

procedure TestTIdSipUDPTransport.OnReceiveResponse(const Response: TIdSipResponse;
                                                   const ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    Self.RPort := ReceivedFrom.PeerPort;
    Self.RPortEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

//* TestTIdSipUDPTransport Published methods ***********************************

procedure TestTIdSipUDPTransport.TestGetTransportType;
begin
  Check(Self.HighPortTransport.GetTransportType = sttUDP, 'Transport type');
end;

procedure TestTIdSipUDPTransport.TestIsReliable;
begin
  Check(not Self.HighPortTransport.IsReliable,
        'UDP transport not marked as unreliable');
end;

procedure TestTIdSipUDPTransport.TestIsSecure;
begin
  Check(not Self.HighPortTransport.IsSecure,
        'UDP transport marked as secure');
end;

procedure TestTIdSipUDPTransport.TestLeaveNonRportRequestsUntouched;
begin
  Self.CheckingRequestEvent := Self.CheckLeaveNonRportRequestsUntouched;
  Self.LowPortTransport.Send(Self.Request);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);

  CheckEquals(0, Self.RPort, 'rport value');
end;

procedure TestTIdSipUDPTransport.TestMaddrUsed;
begin
  Self.CheckingResponseEvent := Self.CheckMaddrUser;
  Self.MaddrTransport.Start;
  try
    Self.Response.LastHop.Maddr  := Self.MaddrTransport.Bindings[0].IP;
    Self.Response.LastHop.SentBy := Self.MaddrTransport.Bindings[0].IP;

    Self.LowPortTransport.Send(Self.Response);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);
  finally
    Self.MaddrTransport.Stop;
  end;
end;

procedure TestTIdSipUDPTransport.TestRespectRport;
var
  Server: TIdSipUdpServer;
begin
  // If we get a request with an rport param (which MUST be empty!) then we
  // should send our responses back to the server at received:rport.

  // What should happen is that:
  // 1. Server sends a request to Self.HighPortTransport
  // 2. HighPortTransport (because of Self.ReturnResponse) sends back a 200 OK
  // 3. Server receives this response and sets Self.RPortEvent

  Self.CheckingRequestEvent := Self.ReturnResponse;

  Self.Request.LastHop.Params[RPortParam] := '';

  Server := TIdSipUdpServer.Create(nil);
  try
    Server.AddMessageListener(Self);
    Server.DefaultPort := 5000; // some arbitrary value
    Server.Active      := true;

    Self.Response.LastHop.Received := Self.HighPortTransport.Bindings[0].IP;
    Self.Response.LastHop.Rport    := Server.DefaultPort;

    Server.Send(Self.HighPortTransport.Bindings[0].IP,
                Self.HighPortTransport.Bindings[0].Port,
                Self.Request.AsString);

    Self.WaitForSignaled(Self.RPortEvent);
  finally
    Server.Free;
  end;
end;

procedure TestTIdSipUDPTransport.TestRportParamFilledIn;
begin
  Self.CheckingRequestEvent := Self.CheckRportParamFilledIn;
  Self.Request.LastHop.Params[RportParam] := '';
  Self.LowPortTransport.Send(Self.Request);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);

  CheckNotEquals(0, Self.RPort, 'rport value');
end;

procedure TestTIdSipUDPTransport.TestRportListening;
var
  Binding: TIdSocketHandle;
  Server:  TIdUdpServer;
begin
  // We test here that when a request is sent from port x, that we listen on
  // port x for responses, as per RFC 3581
  Self.CheckingRequestEvent := Self.NoteSourcePort;
  Self.Request.LastHop.Params[RportParam] := '';
  Self.LowPortTransport.Send(Self.Request);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);

  Server := TIdUdpServer.Create(nil);
  try
    try
      Binding := Server.Bindings.Add;
      Binding.IP   := Self.LowPortTransport.Bindings[0].IP;
      Binding.Port := Self.RPort;

      Server.Active := true;
      Fail('Server wasn''t listening on the port from which it sent an rport'
         + ' request');
    except
      on EIdCouldNotBindSocket do;
    end;
  finally
    Server.Free;
  end;
end;
{
//******************************************************************************
//* TestTIdSipSCTPTransport                                                    *
//******************************************************************************
//* TestTIdSipSCTPTransport Protected methods **********************************

function TestTIdSipSCTPTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipSCTPTransport;
end;

//* TestTIdSipSCTPTransport Published methods **********************************

procedure TestTIdSipSCTPTransport.TestGetTransportType;
begin
  Check(sttSCTP = Self.Transport.GetTransportType, 'Transport type');
end;

procedure TestTIdSipSCTPTransport.TestIsReliable;
begin
  Check(Self.Transport.IsReliable, 'SCTP transport not marked as reliable');
end;

procedure TestTIdSipTCPTransport.TestIsSecure;
begin
  Check(not Self.Transport.IsSecure, 'SCTP transport marked as secure');
end;
}
initialization
  RegisterTest('IdSipTransport', Suite);
end.
