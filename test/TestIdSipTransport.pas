unit TestIdSipTransport;

interface

uses
  Classes, IdSipMessage, IdSipTransport, IdSocketHandle, IdTcpServer, SyncObjs,
  SysUtils, TestFramework, TestFrameworkEx;

type
  TIdSipTransportSubjectSubclass = class(TIdSipTcpTransport)
  public
    procedure NotifyTransportListeners(const Request: TIdSipRequest); overload;
    procedure NotifyTransportListeners(const Response: TIdSipResponse); overload;
  end;

  TestTIdSipTransportSubject = class(TTestCase, IIdSipTransportListener)
  private
    ReceivedRequest:  Boolean;
    ReceivedResponse: Boolean;
    Request:          TIdSipRequest;
    Response:         TIdSipResponse;
    Transport:        TIdSipTransportSubjectSubclass;

    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transport: TIdSipTransport);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transport: TIdSipTransport);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddTransportListener;
    procedure TestAllListenersReceiveRequests;
    procedure TestAllListenersReceiveResponses;
    procedure TestRemoveTransportListener;
  end;

  TestTIdSipTransport = class(TThreadingTestCase, IIdSipMessageListener)
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

    procedure CheckCanReceiveRequest(Sender: TObject; const R: TIdSipRequest);
    procedure CheckCanReceiveResponse(Sender: TObject; const R: TIdSipResponse);
    procedure CheckDiscardResponseWithUnknownSentBy(Sender: TObject; const R: TIdSipResponse);
    procedure CheckSendRequestTopVia(Sender: TObject; const R: TIdSipRequest);
    function  DefaultPort: Cardinal; virtual;
    procedure OnReceiveRequest(const Request: TIdSipRequest);
    procedure OnReceiveResponse(const Response: TIdSipResponse);
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
  Result.AddTest(TestTIdSipTransportSubject.Suite);
  Result.AddTest(TestTIdSipTCPTransport.Suite);
  Result.AddTest(TestTIdSipTLSTransport.Suite);
  Result.AddTest(TestTIdSipUDPTransport.Suite);
//  Result.AddTest(TestTIdSipSCTPTransport.Suite);
end;

//******************************************************************************
//* TIdSipTransportSubjectSubclass                                             *
//******************************************************************************
//* TIdSipTransportSubjectSubclass Public methods ******************************

procedure TIdSipTransportSubjectSubclass.NotifyTransportListeners(const Request: TIdSipRequest);
begin
  inherited NotifyTransportListeners(Request);
end;

procedure TIdSipTransportSubjectSubclass.NotifyTransportListeners(const Response: TIdSipResponse);
begin
  inherited NotifyTransportListeners(Response);
end;

//******************************************************************************
//* TestTIdSipTransportSubject                                                 *
//******************************************************************************
//* TestTIdSipTransportSubject Public methods **********************************

procedure TestTIdSipTransportSubject.SetUp;
begin
  inherited SetUp;

  Self.ReceivedRequest  := false;
  Self.ReceivedResponse := false;
  Self.Request          := TIdSipRequest.Create;
  Self.Response         := TIdSipResponse.Create;
  Self.Transport        := TIdSipTransportSubjectSubclass.Create(0);
end;

procedure TestTIdSipTransportSubject.TearDown;
begin
  Self.Transport.Free;
  Self.Response.Free;
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportSubject Private methods *********************************

procedure TestTIdSipTransportSubject.OnReceiveRequest(const Request: TIdSipRequest;
                                                      const Transport: TIdSipTransport);
begin
  Self.ReceivedRequest := true;
  Check(Self.Request = Request,     'Request not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportSubject.OnReceiveResponse(const Response: TIdSipResponse;
                                                       const Transport: TIdSipTransport);
begin
  Self.ReceivedResponse := true;

  Check(Self.Response = Response,   'Response not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

//* TestTIdSipTransportSubject Published methods *******************************

procedure TestTIdSipTransportSubject.TestAddTransportListener;
begin
  Self.Transport.AddTransportListener(Self);

  Self.Transport.NotifyTransportListeners(Self.Request);

  Check(Self.ReceivedRequest, 'Listener wasn''t added');
end;

procedure TestTIdSipTransportSubject.TestAllListenersReceiveRequests;
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

procedure TestTIdSipTransportSubject.TestAllListenersReceiveResponses;
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

procedure TestTIdSipTransportSubject.TestRemoveTransportListener;
begin
  Self.Transport.AddTransportListener(Self);
  Self.Transport.RemoveTransportListener(Self);

  Self.Transport.NotifyTransportListeners(Self.Request);

  Check(not Self.ReceivedRequest, 'Listener wasn''t removed');
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
  Self.Transport.AddMessageListener(Self);
  Self.Transport.Timeout := 500;
  Self.Transport.HostName := 'wsfrank';
  Self.Transport.Bindings.Clear;
  Binding := Self.Transport.Bindings.Add;
  Binding.IP := GStack.LocalAddress;
  Binding.Port := Self.DefaultPort;

  Self.LocalLoopTransport := Self.TransportType.Create(Self.DefaultPort);
  Self.LocalLoopTransport.AddMessageListener(Self);
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
                                                        const R:      TIdSipResponse);
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
                                                                      const R:      TIdSipResponse);
begin
  Self.ReceivedResponse := true;
end;

procedure TestTIdSipTransport.CheckSendRequestTopVia(Sender: TObject;
                                                       const R:      TIdSipRequest);
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

procedure TestTIdSipTransport.OnReceiveRequest(const Request: TIdSipRequest);
begin
  if Assigned(Self.CheckingRequestEvent) then
    Self.CheckingRequestEvent(Self, Request);

  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTransport.OnReceiveResponse(const Response: TIdSipResponse);
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
  Listener:      TIdSipTestMessageListener;
  LocalListener: TIdSipTestMessageListener;
begin
  Listener := TIdSipTestMessageListener.Create;
  try
    Self.Transport.AddMessageListener(Listener);
    try
      LocalListener := TIdSipTestMessageListener.Create;
      try
        Self.LocalLoopTransport.AddMessageListener(LocalListener);
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
          Self.LocalLoopTransport.RemoveMessageListener(LocalListener);
        end;
      finally
        LocalListener.Free;
      end;
    finally
      Self.Transport.RemoveMessageListener(Listener);
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

  Self.Request.RequestUri.Protocol := SipsScheme;
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
  Check(Self.Transport.IsReliable, 'TCP transport not marked as reliable');
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
