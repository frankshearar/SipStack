unit TestIdSipTransport;

interface

uses
  Classes, IdSipMessage, IdSipTransport, IdSocketHandle, IdTcpServer, SyncObjs,
  SysUtils, TestFramework, TestFrameworkEx;

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

  TestTIdSipTransport = class(TThreadingTestCase, IIdSipTransportListener)
  protected
    CheckingRequestEvent:  TIdSipRequestEvent;
    CheckingResponseEvent: TIdSipResponseEvent;
    LocalLoopTransport:    TIdSipTransport;
    ReceivedRequest:       Boolean;
    ReceivedResponse:      Boolean;
    Request:               TIdSipRequest;
    Response:              TIdSipResponse;
    Transport:             TIdSipTransport;
    WrongServer:           Boolean;

    procedure CheckCanReceiveRequest(Sender: TObject;
                                     const R: TIdSipRequest);
    procedure CheckCanReceiveResponse(Sender: TObject;
                                      const R: TIdSipResponse);
    procedure CheckDiscardResponseWithUnknownSentBy(Sender: TObject;
                                                    const R: TIdSipResponse);
    procedure CheckSendRequestTopVia(Sender: TObject;
                                     const R: TIdSipRequest);
    function  DefaultPort: Cardinal; virtual;
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transport: TIdSipTransport);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transport: TIdSipTransport);
    procedure ReturnResponse(Sender: TObject; const R: TIdSipRequest);
    procedure SendOkResponse;
    function  TransportType: TIdSipTransportClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanReceiveRequest;
    procedure TestCanReceiveResponse;
    procedure TestTransportFor;
    procedure TestDiscardResponseWithUnknownSentBy;
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

  TestTIdSipUDPTransport = class(TestTIdSipTransport)
  protected
    function TransportType: TIdSipTransportClass; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
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

const
  DefaultTimeout       = 1000;
  RootCert             = '..\etc\cacert.pem';
  ServerCert           = '..\etc\newcert.pem';
  ServerKey            = '..\etc\newkey.pem';
  TestServerPortOffset = 10000;

implementation

uses
  IdSipHeaders, IdSipConsts, IdSSLOpenSSL, IdStack, IdTcpClient, IdUDPServer,
  TestMessages, TestFrameworkSip;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransport unit tests');
  Result.AddTest(TestTIdSipTransportEventNotifications.Suite);
  Result.AddTest(TestTIdSipTCPTransport.Suite);
  Result.AddTest(TestTIdSipTLSTransport.Suite);
  Result.AddTest(TestTIdSipUDPTransport.Suite);
//  Result.AddTest(TestTIdSipSCTPTransport.Suite);
end;

//******************************************************************************
//* TIdSipTransportSubclass                                             *
//******************************************************************************
//* TIdSipTransportSubclass Public methods ******************************

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

  Self.Transport := Self.TransportType.Create(Self.DefaultPort);
  Self.Transport.AddTransportListener(Self);
  Self.Transport.Timeout := 500;
  Self.Transport.HostName := 'wsfrank';
  Self.Transport.Bindings.Clear;
  Binding := Self.Transport.Bindings.Add;
  Binding.IP := GStack.LocalAddress;
  Binding.Port := Self.DefaultPort;

  Self.LocalLoopTransport := Self.TransportType.Create(Self.DefaultPort);
  Self.LocalLoopTransport.AddTransportListener(Self);
  Self.LocalLoopTransport.Timeout := 500;
  Self.LocalLoopTransport.HostName := 'localhost';
  Self.LocalLoopTransport.Bindings.Clear;
  Binding := Self.LocalLoopTransport.Bindings.Add;
  Binding.IP := '127.0.0.1';
  Binding.Port := Self.DefaultPort;

  P := TIdSipParser.Create;
  try
    Self.Request  := P.ParseAndMakeRequest(LocalLoopRequest);
    Self.Response := P.ParseAndMakeResponse(LocalLoopResponse);
  finally
    P.Free;
  end;
  Self.Response.LastHop.Transport := Self.Transport.GetTransportType;
  Self.Request.RequestUri.Port := Self.Transport.DefaultPort;

  Self.ReceivedRequest  := false;
  Self.ReceivedResponse := false;
  Self.WrongServer      := false;
end;

procedure TestTIdSipTransport.TearDown;
begin
  Self.Response.Free;
  Self.Request.Free;

  Self.LocalLoopTransport.Free;
  Self.Transport.Free;

  inherited TearDown;
end;

//* TestTIdSipTransport Protected methods **************************************

procedure TestTIdSipTransport.CheckCanReceiveRequest(Sender: TObject;
                                                       const R:      TIdSipRequest);
begin
  try
    Self.ReceivedRequest := true;
    Self.SendOkResponse;

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

procedure TestTIdSipTransport.CheckSendRequestTopVia(Sender: TObject;
                                                     const R: TIdSipRequest);
begin
  try
    Check(Self.Transport.GetTransportType = R.LastHop.Transport,
          'Incorrect transport specified');

    Check(R.LastHop.HasBranch, 'Branch parameter missing');

    CheckEquals(Self.LocalLoopTransport.HostName, R.LastHop.SentBy, 'SentBy incorrect');

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
    Self.CheckingRequestEvent(Self, Request);

  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTransport.OnReceiveResponse(const Response: TIdSipResponse;
                                                const Transport: TIdSipTransport);
begin
  if Assigned(Self.CheckingResponseEvent) then
    Self.CheckingResponseEvent(Self, Response);

  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTransport.ReturnResponse(Sender: TObject;
                                               const R:      TIdSipRequest);
begin
  try
    Self.SendOkResponse;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.SendOkResponse;
begin
  Self.Response.StatusCode := SIPOK;

  Self.Transport.Send(Self.Response);
end;

function TestTIdSipTransport.TransportType: TIdSipTransportClass;
begin
  Result := nil;
end;

//* TestTIdSipTransport Published methods **************************************

procedure TestTIdSipTransport.TestCanReceiveRequest;
begin
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.LocalLoopTransport.Start;
  try
    Self.LocalLoopTransport.Send(Self.Request);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedRequest, 'Request not received');
  finally
    Self.LocalLoopTransport.Stop;
  end;
end;

procedure TestTIdSipTransport.TestCanReceiveResponse;
begin
  Self.CheckingRequestEvent  := Self.ReturnResponse;
  Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;

  Self.LocalLoopTransport.Start;
  try
    Self.LocalLoopTransport.Send(Self.Request);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedResponse, 'Response not received');
  finally
    Self.LocalLoopTransport.Stop;
  end;
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

  Self.LocalLoopTransport.Start;
  try
    // If we don't set the received param we won't be able to send
    // the message to the right SIP server. No, you'd never do this
    // in production, because it's wilfully wrong.
    Self.Response.LastHop.SentBy := 'unknown.host';
    Self.Response.LastHop.Received := '127.0.0.1';
    Self.LocalLoopTransport.Send(Self.Response);

    Check(wrTimeout = Self.ThreadEvent.WaitFor(DefaultTimeout),
          'Response not silently discarded');
  finally
    Self.LocalLoopTransport.Stop;
  end;
end;

procedure TestTIdSipTransport.TestSendRequest;
begin
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.LocalLoopTransport.Start;
  try
    Self.LocalLoopTransport.Send(Self.Request);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedRequest, 'Request not received');
  finally
    Self.LocalLoopTransport.Stop;
  end;
end;

procedure TestTIdSipTransport.TestSendRequestTopVia;
begin
  Self.CheckingRequestEvent := Self.CheckSendRequestTopVia;

  Self.LocalLoopTransport.Start;
  try
    Self.LocalLoopTransport.Send(Self.Request);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);
  finally
    Self.LocalLoopTransport.Stop;
  end;
end;

procedure TestTIdSipTransport.TestSendResponse;
begin
  Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;

  Self.LocalLoopTransport.Start;
  try
    Self.LocalLoopTransport.Send(Self.Response);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(5000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedResponse, 'Response not received');
  finally
    Self.LocalLoopTransport.Stop;
  end;
end;

procedure TestTIdSipTransport.TestSendResponseWithReceivedParam;
var
  Listener:      TIdSipTestTransportListener;
  LocalListener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Transport.AddTransportListener(Listener);
    try
      LocalListener := TIdSipTestTransportListener.Create;
      try
        Self.LocalLoopTransport.AddTransportListener(LocalListener);
        try
          Self.Transport.Start;
          try
            Self.LocalLoopTransport.Start;
            try
              Check(Self.LocalLoopTransport.Bindings.Count > 0,
                    'Sanity check on LocalLoop''s bindings');

              Self.Response.LastHop.Received := Self.LocalLoopTransport.Bindings[0].IP;
              Self.Transport.Send(Self.Response);

              // It's not perfect, but anyway. We need to wait long enough for
              // LocalLoopTransport to get its response.
              Sleep(500);

              Check(LocalListener.ReceivedResponse and not Listener.ReceivedResponse,
                    'Received param in top Via header ignored - '
                  + 'wrong server got the message');

            finally
              Self.LocalLoopTransport.Stop;
            end;
          finally
            Self.Transport.Stop;
          end;
        finally
          Self.LocalLoopTransport.RemoveTransportListener(LocalListener);
        end;
      finally
        LocalListener.Free;
      end;
    finally
      Self.Transport.RemoveTransportListener(Listener);
    end;
  finally
    Listener.Free;
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
  Check(Self.Transport.GetTransportType = sttTCP, 'Transport type');
end;

procedure TestTIdSipTCPTransport.TestIsReliable;
begin
  Check(Self.Transport.IsReliable, 'TCP transport not marked as reliable');
end;

procedure TestTIdSipTCPTransport.TestIsSecure;
begin
  Check(not Self.Transport.IsSecure, 'TCP transport marked as secure');
end;

//******************************************************************************
//* TestTIdSipTLSTransport                                                     *
//******************************************************************************
//* TestTIdSipTLSTransport Public methods **************************************

procedure TestTIdSipTLSTransport.SetUp;
begin
  inherited SetUp;

  Self.SetUpTls(Self.Transport);
  Self.SetUpTls(Self.LocalLoopTransport);

  Self.Request.RequestUri.Scheme := SipsScheme;
  Self.Response.LastHop.Value := StringReplace(Self.Response.LastHop.AsString, 'TCP', 'TLS', []);
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
  Password := 'test';
end;

procedure TestTIdSipTLSTransport.SetUpTls(Transport: TIdSipTransport);
var
  TLS: TIdSipTLSTransport;
begin
  CheckEquals(TIdSipTLSTransport,
              Transport.ClassType,
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
  Check(sttTLS = Self.Transport.GetTransportType, 'Transport type');
end;

procedure TestTIdSipTLSTransport.TestIsReliable;
begin
  Check(Self.Transport.IsReliable, 'TLS transport not marked as reliable');
end;

procedure TestTIdSipTLSTransport.TestIsSecure;
begin
  Check(Self.Transport.IsSecure, 'TLS transport not marked as secure');
end;

//******************************************************************************
//* TestTIdSipUDPTransport                                                     *
//******************************************************************************
//* TestTIdSipUDPTransport Protected methods ***********************************

function TestTIdSipUDPTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipUdpTransport;
end;

//* TestTIdSipUDPTransport Published methods ***********************************

procedure TestTIdSipUDPTransport.TestGetTransportType;
begin
  Check(Self.Transport.GetTransportType = sttUDP, 'Transport type');
end;

procedure TestTIdSipUDPTransport.TestIsReliable;
begin
  Check(not Self.Transport.IsReliable, 'UDP transport not marked as unreliable');
end;

procedure TestTIdSipUDPTransport.TestIsSecure;
begin
  Check(not Self.Transport.IsSecure, 'UDP transport marked as secure');
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
