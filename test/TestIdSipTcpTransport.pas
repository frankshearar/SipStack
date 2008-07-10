{
  (c) 2006 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipTcpTransport;

interface

uses
  IdConnectionBindings, IdSipLocation, IdSipMessage, IdSipMockTransport,
  IdSipTcpTransport, IdSipTransport, IdTimerQueue, IdTCPClient, IdTCPConnection,
  IdTCPServer, SyncObjs, SysUtils, TestFramework, TestFrameworkSip,
  TestFrameworkSipTransport;

type
  TestTIdSipTCPTransport = class(TestTIdSipTransport)
  private
    ClientReceivedResponse: Boolean;
    MockTransport:          TIdSipMockTcpTransport;
    RequestConnection:      TIdConnectionBindings;
    ResponseConnection:     TIdConnectionBindings;
    ServerReceivedResponse: Boolean;
    SipClient:              TIdSipTcpClient;

    procedure AcknowledgeEvent(Sender: TObject;
                               Response: TIdSipResponse;
                               ReceivedFrom: TIdConnectionBindings);
    procedure CheckSendResponsesOpenConnection(Sender: TObject;
                                               Response: TIdSipResponse;
                                               ReceivedFrom: TIdConnectionBindings);
    procedure CheckSendResponsesDownClosedConnection(Sender: TObject;
                                                     Response: TIdSipResponse;
                                                     ReceivedFrom: TIdConnectionBindings);
    procedure Send200OK(Sender: TObject;
                        Request: TIdSipRequest;
                        ReceivedFrom: TIdConnectionBindings);
  protected
    procedure CheckSendBindingSet(Binding: TIdConnectionBindings); override;
    function  CreateClient: TIdSipTcpClient; virtual;
    procedure CheckServerOnPort(const Host: String;
                                Port: Cardinal;
                                const Msg: String); override;
    procedure DestroyClient(Client: TIdSipTcpClient); virtual;
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport;
                               Source: TIdConnectionBindings); override;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdConnectionBindings); override;
    procedure SendMessage(Msg: String); override;
    function  TransportType: TIdSipTransportClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddConnection;
    procedure TestConserveConnectionsOutbound;
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
    procedure TestNormalReadTimeoutDoesntNotify;
    procedure TestNotifyOfConnectionInbound;
    procedure TestNotifyOfConnectionOutbound;
    procedure TestNotifyOfDisconnection;
    procedure TestSendRequestOverExistingConnection;
    procedure TestSendResponsesClosedConnection;
    procedure TestSendResponsesClosedConnectionReceivedParam;
    procedure TestSendResponsesOpenConnection;
    procedure TestTransportExceptionSchedulesWait;
  end;

  TIdSipRequestEvent = procedure(Sender: TObject;
                                 R: TIdSipRequest) of object;
  TIdSipResponseEvent = procedure(Sender: TObject;
                                  R: TIdSipResponse) of object;

  TIdTcpClientClass = class of TIdTcpClient;

  TestTIdSipTcpServer = class(TTestCaseSip,
                              IIdSipTransportListener)
  private
    EmptyListEvent:           TEvent;
    NotifiedMalformedMessage: Boolean;
    Transport:                TIdSipMockTransport;

    procedure AcknowledgeEvent(Sender: TObject;
                               Request: TIdSipRequest); overload;
    procedure AcknowledgeEvent(Sender: TObject;
                               Response: TIdSipResponse); overload;
    procedure CheckInternalServerError(Sender: TObject;
                                       Response: TIdSipResponse);
    procedure CheckMultipleMessages(Sender: TObject;
                                    Request: TIdSipRequest);
    procedure CheckMethodEvent(Sender: TObject;
                               Request: TIdSipRequest);
    procedure OnEmpty(Sender: TIdTimerQueue);
    procedure OnException(FailedMessage: TIdSipMessage;
                          E: Exception;
                          const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport;
                               Source: TIdConnectionBindings); overload;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdConnectionBindings); overload;
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String;
                                Source: TIdConnectionBindings);
    procedure RaiseException(Sender: TObject;
                             Request: TIdSipRequest);
  protected
    CheckingRequestEvent:   TIdSipRequestEvent;
    CheckingResponseEvent:  TIdSipResponseEvent;
    Client:                 TIdTcpClient;
    ClientReceivedResponse: Boolean;
    HighPortLocation:       TIdSipLocation;
    HighPortServer:         TIdSipTcpServer;
    LowPortServer:          TIdSipTcpServer;
    MethodCallCount:        Cardinal;
    ServerReceivedResponse: Boolean;
    SipClient:              TIdSipTcpClient;
    Timer:                  TIdThreadedTimerQueue;

    function ServerType: TIdSipTcpServerClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInternalServerError;
    procedure TestLeadingEmptyLines;
    procedure TestMethodEvent;
    procedure TestMultipleMessages;
    procedure TestReceiveRequest;
    procedure TestReceiveResponse;
  end;

  TestTIdSipTcpClient = class(TTestCaseSip,
                              IIdSipTransportListener)
  private
    CheckingRequestEvent:  TIdSipRequestEvent;
    CheckingResponseEvent: TIdSipResponseEvent;
    Client:                TIdSipTcpClient;
    ClientEvent:           TEvent;
    EmptyListEvent:        TEvent;
    Finished:              Boolean;
    Invite:                TIdSipRequest;
    InviteCount:           Cardinal;
    ReceivedRequestMethod: String;
    ReceivedResponseCount: Cardinal;
    Server:                TIdSipTcpServer;
    Timer:                 TIdThreadedTimerQueue;
    Transport:             TIdSipMockTransport;

    procedure CheckReceiveOkResponse(Sender: TObject;
                                     Response: TIdSipResponse);
    procedure CheckReceiveOptions(Sender: TObject;
                                  Request: TIdSipRequest);
    procedure CheckReceiveProvisionalAndOkResponse(Sender: TObject;
                                                   Response: TIdSipResponse);
    procedure CheckSendInvite(Sender: TObject;
                              Request: TIdSipRequest);
    procedure CheckSendTwoInvites(Sender: TObject;
                                  Request: TIdSipRequest);
    procedure ClientReceivedRequest(Sender: TObject;
                                    R: TIdSipRequest);
    procedure CutConnection(Sender: TObject;
                            R: TIdSipRequest);
    procedure OnEmpty(Sender: TIdTimerQueue);
    procedure OnException(FailedMessage: TIdSipMessage;
                          E: Exception;
                          const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport;
                               Source: TIdConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdConnectionBindings);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String;
                                Source: TIdConnectionBindings);
    procedure PauseAndSendOkResponse(Sender: TObject;
                                     Request: TIdSipRequest);
    procedure ReceiveOptions;
    procedure SendOkResponse(Sender: TObject;
                             Request: TIdSipRequest);
    procedure SendProvisionalAndOkResponse(Sender: TObject;
                                           Request: TIdSipRequest);
    procedure SendResponseReceiveOptions(Sender: TObject;
                                         Response: TIdSipResponse);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanReceiveRequest;
    procedure TestConnectAndDisconnect;
    procedure TestTerminatedWithServerDisconnect;
    procedure TestReceiveOkResponse;
    procedure TestReceiveOkResponseWithPause;
    procedure TestReceiveProvisionalAndOkResponse;
    procedure TestSendInvite;
    procedure TestSendResponseReceiveOptions;
    procedure TestSendTwoInvites;
    procedure TestSendWithServerDisconnect;
    procedure TestSetConserveConnectionsAfterConnect;
    procedure TestSetConserveConnectionsThenConnect;
  end;

  TestTIdSipConnectionTableEntry = class(TTestCase)
  private
    Connection: TIdTCPConnection;
    Request:    TIdSipRequest;
    Server:     TIdTCPServer;

    procedure DoOnExecute(Thread: TIdPeerThread);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
  end;

  TestTIdSipConnectionTable = class(TTestCaseSip)
  private
    Conn:    TIdTCPConnection;
    NewConn: TIdTCPConnection;
    NewReq:  TIdSipRequest;
    Req:     TIdSipRequest;
    Table:   TIdSipConnectionTable;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAddPreventsDuplicates;
    procedure TestConnectionFor;
    procedure TestConnectionForOneEntry;
    procedure TestConnectionForOnEmptyList;
    procedure TestConnectionForOnNoEntry;
    procedure TestConnectionForResponse;
    procedure TestRemove;
    procedure TestRemoveOnEmptyList;
    procedure TestRemoveOnNonEmptyList;
    procedure TestRemoveWithMultipleRequests;
  end;

  TTcpTransportWaitTestCase = class(TTestCase)
  protected
    Conn:      TIdTCPConnection;
    Invite:    TIdSipRequest;
    Transport: TIdSipTcpTransport;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipTcpConnectionOpenWait = class(TTcpTransportWaitTestCase)
  private
    Wait: TIdSipTcpConnectionOpenWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
    procedure TestTriggerOnNonExistentAction;
    procedure TestTriggerOnWrongTypeOfObject;
  end;

  TestTIdSipTcpConnectionCloseWait = class(TTcpTransportWaitTestCase)
  private
    Wait: TIdSipTcpConnectionCloseWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
    procedure TestTriggerOnNonExistentAction;
    procedure TestTriggerOnWrongTypeOfObject;
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
  OneSecond      = 1000;
  ViaFQDN        = 'gw1.leo-ix.org';
  ViaIP          = '127.0.0.1';
  ViaDifferentIP = '196.25.1.1';
  LocalHost      = '127.0.0.1';

implementation

uses
  Classes, IdException, IdGlobal, IdRegisteredObject, IdSimpleParser,
  IdSocketHandle, IdStack, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTcpTransport unit tests');
  Result.AddTest(TestTIdSipTCPTransport.Suite);
  Result.AddSuite(TestTIdSipTcpServer.Suite);
  Result.AddTest(TestTIdSipTcpClient.Suite);
  Result.AddSuite(TestTIdSipConnectionTableEntry.Suite);
  Result.AddSuite(TestTIdSipConnectionTable.Suite);
  Result.AddSuite(TestTIdSipTcpConnectionOpenWait.Suite);
  Result.AddSuite(TestTIdSipTcpConnectionCloseWait.Suite);
end;

//******************************************************************************
//* TestTIdSipTCPTransport                                                     *
//******************************************************************************
//* TestTIdSipTCPTransport Public methods **************************************

procedure TestTIdSipTCPTransport.SetUp;
begin
  inherited SetUp;

  Self.MockTransport := TIdSipMockTcpTransport.Create;
  Self.MockTransport.AddTransportListener(Self);

  Self.SipClient := Self.CreateClient;

  Self.RequestConnection  := TIdConnectionBindings.Create;
  Self.ResponseConnection := TIdConnectionBindings.Create;

  Self.ClientReceivedResponse := false;
  Self.ServerReceivedResponse := false;
end;

procedure TestTIdSipTCPTransport.TearDown;
begin
  // Usually we'd clean up our stuff before invoking the superclass's TearDown.
  // We don't here because TearDown protects us from destroying our resources
  // before Self.Timer has finished doing its thing (causing access violations
  // in the process).

  inherited TearDown;

  Self.DestroyClient(Self.SipClient);
  Self.MockTransport.Free;
  Self.ResponseConnection.Free;
  Self.RequestConnection.Free;
end;

//* TestTIdSipTCPTransport Protected methods ***********************************

procedure TestTIdSipTCPTransport.CheckSendBindingSet(Binding: TIdConnectionBindings);
begin
  CheckEquals(Self.LowPortLocation.Transport,
              Binding.Transport,
              Self.HighPortTransport.ClassName
            + ': Transport of sending binding');
  CheckEquals(Self.LowPortLocation.IPAddress,
              Binding.LocalIP,
              Self.HighPortTransport.ClassName
            + ': LocalIP of sending binding');
  // Initiating a TCP connection will result in an ephemeral local port. We don't
  // know what the port will be, but we know it won't be zero.
  CheckNotEquals(0,
                 Binding.LocalPort,
                 Self.HighPortTransport.ClassName
               + ': LocalPort of sending binding');
  CheckEquals(Self.HighPortLocation.IPAddress,
              Binding.PeerIP,
              Self.HighPortTransport.ClassName
            + ': PeerIP of sending binding');
  CheckEquals(Self.HighPortLocation.Port,
              Binding.PeerPort,
              Self.HighPortTransport.ClassName
            + ': PeerPort of sending binding');
end;

function TestTIdSipTCPTransport.CreateClient: TIdSipTcpClient;
begin
  Result := TIdSipTcpClient.Create(nil);
  Result.ConserveConnections := false;
  Result.TransportID := Self.MockTransport.ID;
end;

procedure TestTIdSipTCPTransport.CheckServerOnPort(const Host: String;
                                                   Port: Cardinal;
                                                   const Msg: String);
var
  Client: TIdTcpClient;
begin
  try
    Client := TIdTcpClient.Create(nil);
    try
      Client.Host := Host;
      Client.Port := Port;
      Client.Connect;
      try
        // Do nothing
      finally
        Client.Disconnect;
      end;
    finally
      Client.Free;
    end;
  except
    on EIdSocketError do
      Fail('No server running on ' + Host + ': ' + IntToStr(Port) + '; ' + Msg);
  end;
end;

procedure TestTIdSipTCPTransport.DestroyClient(Client: TIdSipTcpClient);
begin
  Client.Free;
end;

procedure TestTIdSipTCPTransport.OnReceiveRequest(Request: TIdSipRequest;
                                                  Receiver: TIdSipTransport;
                                                  Source: TIdConnectionBindings);
begin
  Self.RequestConnection.Assign(Source);

  inherited OnReceiveRequest(Request, Receiver, Source);
end;

procedure TestTIdSipTCPTransport.OnReceiveResponse(Response: TIdSipResponse;
                                                   Receiver: TIdSipTransport;
                                                   Source: TIdConnectionBindings);
begin
  Self.ResponseConnection.Assign(Source);

  inherited OnReceiveResponse(Response, Receiver, Source);
end;

procedure TestTIdSipTCPTransport.SendMessage(Msg: String);
var
  Client: TIdTcpClient;
begin
  Client := TIdTcpClient.Create(nil);
  try
    Client.Host := Self.HighPortLocation.IPAddress;
    Client.Port := Self.HighPortLocation.Port;
    Client.Connect(DefaultTimeout);
    try
      Client.Write(Msg);
    finally
      Client.DisconnectSocket;
    end;
  finally
    Client.Free;
  end;
end;

function TestTIdSipTCPTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipTcpTransport;
end;

//* TestTIdSipTCPTransport Private methods *************************************

procedure TestTIdSipTCPTransport.AcknowledgeEvent(Sender: TObject;
                                                  Response: TIdSipResponse;
                                                  ReceivedFrom: TIdConnectionBindings);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTCPTransport.CheckSendResponsesOpenConnection(Sender: TObject;
                                                                  Response: TIdSipResponse;
                                                                  ReceivedFrom: TIdConnectionBindings);
begin
  try
    Self.ClientReceivedResponse := true;

    // These tests check that the connection on which the UAS received the
    // request is the connection on which the UAC received the response.
    CheckEquals(Self.RequestConnection.LocalIP,
                Self.ResponseConnection.PeerIP,
                'RequestConnection.LocalIP/ResponseConnection.PeerIP');
    CheckEquals(Self.RequestConnection.PeerIP,
                Self.ResponseConnection.LocalIP,
                'RequestConnection.PeerIP/ResponseConnection.LocalIP');
    CheckEquals(Self.RequestConnection.LocalPort,
                Self.ResponseConnection.PeerPort,
                'RequestConnection.LocalPort/ResponseConnection.PeerPort');
    CheckEquals(Self.RequestConnection.PeerPort,
                Self.ResponseConnection.LocalPort,
                'RequestConnection.PeerPort/ResponseConnection.LocalPort');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTCPTransport.CheckSendResponsesDownClosedConnection(Sender: TObject;
                                                                        Response: TIdSipResponse;
                                                                        ReceivedFrom: TIdConnectionBindings);
begin
  try
    CheckEquals(SIPOK, Response.StatusCode, 'Status-Code');
    Self.ServerReceivedResponse := true;
    CheckNotEquals(Self.RequestConnection.AsString,
                   Self.ResponseConnection.AsString,
                   'RequestConnection matches ResponseConnection');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTCPTransport.Send200OK(Sender: TObject;
                                           Request: TIdSipRequest;
                                           ReceivedFrom: TIdConnectionBindings);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipMessage.ReadResponseFrom(LocalLoopResponse);
  try
    Response.StatusCode := SIPOK;
    Self.LowPortTransport.Send(Response, Self.HighPortLocation);
  finally
    Response.Free;
  end;
end;

//* TestTIdSipTCPTransport Published methods ***********************************

procedure TestTIdSipTCPTransport.TestAddConnection;
var
  L:        TConnectionListener;
  TcpTrans: TIdSipTcpTransport;
begin
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;
  TcpTrans := Self.HighPortTransport as TIdSipTcpTransport;

  L := TConnectionListener.Create;
  try
    Self.HighPortTransport.AddConnectionListener(L);

    Self.SipClient.Host := Self.HighPortLocation.IPAddress;
    Self.SipClient.Port := Self.HighPortLocation.Port;

    Self.SipClient.Connect(DefaultTimeout);
    try
      TcpTrans.AddConnection(Self.SipClient, Self.RecvdRequest);

      Check(L.ConnectionOpened, 'Listeners not notified');
      Check(TcpTrans = L.TransportParam, 'Transport param');

      // We reverse the meaning of peer and local in the below because when
      // TcpTrans adds its INBOUND connection, this OUTBOUND connection's
      // meanings will be transposed. That is, SipClient's local ip/port is
      // the TcpTrans.AddConnection's peer ip/port.
      CheckEquals(Self.HighPortLocation.Transport,    L.ConnectionParam.Transport, 'Connection param; transport');
      CheckEquals(Self.SipClient.Socket.Binding.IP,   L.ConnectionParam.LocalIP,    'Connection param; peer IP');
      CheckEquals(Self.SipClient.Socket.Binding.Port, L.ConnectionParam.LocalPort,  'Connection param; peer port');
      CheckEquals(Self.HighPortLocation.IPAddress,    L.ConnectionParam.PeerIP,   'Connection param; local IP');
      CheckEquals(Self.HighPortLocation.Port,         L.ConnectionParam.PeerPort, 'Connection param; local port');
    finally
      Self.SipClient.Disconnect;
    end;
  finally
    Self.HighPortTransport.RemoveConnectionListener(L);
    L.Free;
  end;
end;

procedure TestTIdSipTCPTransport.TestConserveConnectionsOutbound;
  function ShortAsString(B: TIdConnectionBindings): String;
  const
    Form = '%s:%d/%s';
  begin
    Result := Format(Form, [B.LocalIP, B.LocalPort, B.Transport])
            + '-->'
            + Format(Form, [B.PeerIP, B.PeerPort, B.Transport])
  end;
var
  FirstBinding: TIdConnectionBindings;
begin
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  FirstBinding := TIdConnectionBindings.Create;
  try
    (Self.LowPortTransport as TIdSipTcpTransport).ConserveConnections := true;
    (Self.HighPortTransport as TIdSipTcpTransport).ConserveConnections := true;
    Self.LowPortTransport.Send(Request, Self.HighPortLocation);
    Self.WaitForSignaled;
    FirstBinding.Assign(Self.ReceivingBinding);

    // Let the connection time out on a read.
    Sleep(2*Self.LowPortTransport.Timeout);

    Self.ReceivedRequest := false;
    Self.LowPortTransport.Send(Request, Self.HighPortLocation);
    Self.WaitForSignaled;

    CheckEquals(ShortAsString(FirstBinding),
                ShortAsString(Self.ReceivingBinding),
                'Same binding (hence connection) not used');
  finally
    FirstBinding.Free;
  end;
end;

procedure TestTIdSipTCPTransport.TestGetTransportType;
begin
  CheckEquals(TcpTransport,
              Self.HighPortTransport.GetTransportType,
              'Transport type');
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

procedure TestTIdSipTCPTransport.TestNormalReadTimeoutDoesntNotify;
var
  L: TIdSipTestTransportListener;
begin
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  L := TIdSipTestTransportListener.Create;
  try
    Self.LowPortTransport.AddTransportListener(L);
    // Set up the TCP connection.
    Self.LowPortTransport.ConserveConnections := true;
    Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);

    Self.WaitForSignaled;

    // Time out waiting for a message to arrive.
    Sleep(2*Self.DefaultTimeout);

    // Show that there's no spurious "rejection" of a malformed message.
    Check(not L.RejectedMessage, 'Transport "rejected" a message that actually never arrived');
  finally
    Self.LowPortTransport.RemoveTransportListener(L);
    L.Free;
  end;
end;

procedure TestTIdSipTCPTransport.TestNotifyOfConnectionInbound;
var
  L: TConnectionListener;
begin
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  L := TConnectionListener.Create;
  try
    Self.HighPortTransport.AddConnectionListener(L);
    Self.LowPortTransport.Send(Self.Request, Self.HighPortLocation);
    Self.WaitForSignaled;

    Check(L.ConnectionOpened, 'Listeners not notified');
    Check(Self.HighPortTransport = L.TransportParam, 'Transport param');

    CheckEquals(Self.LowPortLocation.Transport,  L.ConnectionParam.Transport, 'Connection param; transport');
    CheckEquals(Self.HighPortLocation.IPAddress, L.ConnectionParam.LocalIP,   'Connection param; local IP');
    CheckEquals(Self.HighPortLocation.Port,      L.ConnectionParam.LocalPort, 'Connection param; local port');
    CheckEquals(Self.LowPortLocation.IPAddress,  L.ConnectionParam.PeerIP,    'Connection param; peer IP');
    CheckNotEquals(0,                            L.ConnectionParam.PeerPort,  'Connection param; peer port');    
  finally
    Self.HighPortTransport.RemoveConnectionListener(L);
    L.Free;
  end;
end;

procedure TestTIdSipTCPTransport.TestNotifyOfConnectionOutbound;
var
  L: TConnectionListener;
begin
  L := TConnectionListener.Create;
  try
    Self.HighPortTransport.AddConnectionListener(L);
    Self.HighPortTransport.Send(Self.Request, Self.LowPortLocation);

    Check(L.ConnectionOpened, 'Listeners not notified');
    Check(Self.HighPortTransport = L.TransportParam, 'Transport param');

    CheckEquals(Self.LowPortLocation.Transport,  L.ConnectionParam.Transport, 'Connection param; transport');
    CheckEquals(Self.LowPortLocation.IPAddress,  L.ConnectionParam.PeerIP,    'Connection param; peer IP');
    CheckEquals(Self.LowPortLocation.Port,       L.ConnectionParam.PeerPort,  'Connection param; peer port');
    CheckEquals(Self.HighPortLocation.IPAddress, L.ConnectionParam.LocalIP,   'Connection param; local IP');
    CheckNotEquals(0,                            L.ConnectionParam.LocalPort, 'Connection param; local port');
  finally
    Self.HighPortTransport.RemoveConnectionListener(L);
    L.Free;
  end;
end;

procedure TestTIdSipTCPTransport.TestNotifyOfDisconnection;
var
  L:         TConnectionListener;
  LocalIP:   String;
  LocalPort: Integer;
begin
  Self.SipClient.Host := Self.HighPortLocation.IPAddress;
  Self.SipClient.Port := Self.HighPortLocation.Port;

  L := TConnectionListener.Create;
  try
    Self.HighPortTransport.AddConnectionListener(L);

    Self.SipClient.Connect(DefaultTimeout);
    try
      LocalIP   := Self.SipClient.Socket.Binding.IP;
      LocalPort := Self.SipClient.Socket.Binding.Port;
      Self.SipClient.Write(Self.Request.AsString);
    finally
      Self.SipClient.Disconnect;
    end;

    Check(L.ConnectionClosed, 'Listeners not notified');
    Check(Self.HighPortTransport = L.TransportParam, 'Transport param');

    CheckEquals(Self.LowPortLocation.Transport,  L.ConnectionParam.Transport, 'Connection param; transport');
    CheckEquals(LocalIP,                         L.ConnectionParam.PeerIP,    'Connection param; peer IP');
    CheckEquals(LocalPort,                       L.ConnectionParam.PeerPort,  'Connection param; peer port');
    CheckEquals(Self.HighPortLocation.IPAddress, L.ConnectionParam.LocalIP,   'Connection param; local IP');
    CheckEquals(Self.HighPortLocation.Port,      L.ConnectionParam.LocalPort, 'Connection param; local port');
  finally
    Self.HighPortTransport.RemoveConnectionListener(L);
    L.Free;
  end;
end;

procedure TestTIdSipTCPTransport.TestSendRequestOverExistingConnection;
var
  FirstBinding: TIdConnectionBindings;
  Request:      TIdSipRequest;
begin
  // If a TCP connection exists to a remote party, we should reuse that
  // connection.

  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Request := TIdSipMessage.ReadRequestFrom(LocalLoopRequest);
  try
    FirstBinding := TIdConnectionBindings.Create;
    try
      Self.LowPortTransport.Send(Request, Self.HighPortLocation);
      Self.WaitForSignaled;
      FirstBinding.Assign(Self.ReceivingBinding);

      Self.ReceivedRequest := false;

      Self.LowPortTransport.Send(Request, Self.HighPortLocation);
      Self.WaitForSignaled;
      Check(Self.ReceivedRequest, '2nd request not received');
      CheckEquals(FirstBinding.AsString,
                  Self.ReceivingBinding.AsString,
                  'Same binding (hence connection) not used');
    finally
      FirstBinding.Free;
    end;
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipTCPTransport.TestSendResponsesClosedConnection;
var
  Request:  TIdSipRequest;
  Response: TIdSipResponse;
begin
  // We send an INVITE to the low port transport. The client then disconnects its
  // connection. We check that the transport sends the response down a new TCP
  // connection. In this case the transport will make a connection to itself
  // (since it runs on the default transport port).

  Self.CheckingResponseEvent := Self.CheckSendResponsesDownClosedConnection;

  Request := TIdSipMessage.ReadRequestFrom(LocalLoopRequest);
  try
    Self.SipClient.Host        := Self.LowPortLocation.IPAddress;
    Self.SipClient.Port        := Self.LowPortLocation.Port;
    Self.SipClient.ReadTimeout := 100;

    Self.SipClient.Connect(OneSecond);
    try
      Self.SipClient.Send(Request);
    finally
      Self.SipClient.Disconnect;
    end;

    // Why should we need to sleep here?
    // When SipClient disconnects, the server side of the connection should
    // invoke OnDisconnect which then unregisters the connection from the
    // ConnectionMap.
    Sleep(500);

    // Then the LowPortTransport sends the response. This should result in a
    // new TCP connection.
    Response := TIdSipMessage.ReadResponseFrom(LocalLoopResponse);
    try
      Response.StatusCode := SIPOK;
      Self.LowPortTransport.Send(Response, Self.HighPortLocation);
    finally
      Response.Free;
    end;

    Self.WaitForSignaled;

    Check(Self.ServerReceivedResponse,
          'Response wasn''t sent down a new connection');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipTCPTransport.TestSendResponsesClosedConnectionReceivedParam;
var
  LowPortListener: TIdSipTestTransportListener;
  Request:         TIdSipRequest;
  Response:        TIdSipResponse;
begin
  Assert(Assigned(GStack) and (GStack.LocalAddress <> '127.0.0.1'),
         'This test cannot work on a machine with only one network interface. '
       + 'Please make sure it''s got a NIC and that NIC has an IP');

  LowPortListener := TIdSipTestTransportListener.Create;
  try
    Self.LowPortTransport.AddTransportListener(LowPortListener);
    try
      Request := TIdSipMessage.ReadRequestFrom(LocalLoopRequest);
      try
        Self.SipClient.Host             := Self.HighPortLocation.IPAddress;
        Self.SipClient.Port             := Self.HighPortLocation.Port;
        Self.SipClient.ReadTimeout      := 100;

        Self.SipClient.Connect(OneSecond);
        try
          Self.SipClient.Send(Request);
        finally
          Self.SipClient.Disconnect;
        end;

        Check(not Self.SipClient.Connected,
              'Client still connected');

        Self.CheckingResponseEvent := Self.AcknowledgeEvent;
        Response := TIdSipMessage.ReadResponseFrom(LocalLoopResponse);
        try
          Response.LastHop.Received := Self.HighPortLocation.IPAddress;
          Response.LastHop.Port     := Self.HighPortLocation.Port;
          Response.StatusCode       := SIPOK;
          Self.LowPortTransport.Send(Response, Self.HighPortLocation);
        finally
          Response.Free;
        end;

        Self.ExceptionMessage := 'High port server didn''t receive the response';
        Self.WaitForSignaled;
        Check(not LowPortListener.ReceivedResponse,
              'Low port server received response');
      finally
        Request.Free;
      end;
    finally
      Self.LowPortTransport.RemoveTransportListener(LowPortListener);
    end;
  finally
    LowPortListener.Free;
  end;
end;

procedure TestTIdSipTCPTransport.TestSendResponsesOpenConnection;
var
  Request:   TIdSipRequest;
  SipClient: TIdSipTcpClient;
begin
  Self.LowPortTransport.Timeout := 1000;
  Self.HighPortTransport.Timeout := 1000;
  Self.CheckingRequestEvent := Self.Send200OK;
  Self.CheckingResponseEvent := Self.CheckSendResponsesOpenConnection;

  Request := TIdSipMessage.ReadRequestFrom(LocalLoopRequest);
  try
    Self.MockTransport.SetFirstBinding(Request.LastHop.SentBy, Request.LastHop.Port);

    SipClient := Self.CreateClient;
    try
      SipClient.Host        := Self.LowPortLocation.IPAddress;
      SipClient.Port        := Self.LowPortLocation.Port;
      SipClient.ReadTimeout := 1000;
      SipClient.Timer       := Self.Timer;

      SipClient.Connect(OneSecond);
      try
        SipClient.Send(Request);
        SipClient.ReceiveMessages;

        Self.WaitForSignaled;

        Check(Self.ClientReceivedResponse,
              'No response received on same connection');
      finally
        SipClient.Disconnect;
      end;
    finally
      Self.DestroyClient(SipClient);
    end;
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipTCPTransport.TestTransportExceptionSchedulesWait;
var
  ExceptionWait: TIdSipMessageExceptionWait;
  NoServer:      TIdSipLocation;
  DebugTimer:    TIdDebugTimerQueue;
  OldTimer:      TIdTimerQueue;
begin
  // This test demonstrates that should a transport-level exception occur in
  // the sending of a message (there's no target server, in this case), that the
  // notification of failure is scheduled as a Wait object, and not just
  // propogated up the stack.

  NoServer := TIdSipLocation.Create(Self.HighPortLocation.Transport,
                                    Self.HighPortLocation.IPAddress,
                                    Self.HighPortLocation.Port + 1);
  try
    CheckServerNotOnPort(NoServer.IPAddress, NoServer.Port, 'This test cannot work with something running on ' + NoServer.AsString);

    DebugTimer := TIdDebugTimerQueue.Create(false);
    try
      OldTimer := Self.LowPortTransport.Timer;
      Self.LowPortTransport.Timer := DebugTimer;
      try
        Self.LowPortTransport.Send(Self.Request, NoServer);

        ExceptionWait := DebugTimer.LastEventScheduled(TIdSipMessageExceptionWait) as TIdSipMessageExceptionWait;
        Check(ExceptionWait <> nil, 'No Wait scheduled');
        Check(Self.Request.Equals(ExceptionWait.FailedMessage), 'Wait has incorrect message');
      finally
        Self.LowPortTransport.Timer := OldTimer;
      end;
    finally
      DebugTimer.Free;
    end;
  finally
    NoServer.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTcpServer                                                        *
//******************************************************************************
//* TestTIdSipTcpServer Public methods *****************************************

procedure TestTIdSipTcpServer.SetUp;
var
  Binding: TIdSocketHandle;
begin
  inherited SetUp;

  Self.EmptyListEvent := TSimpleEvent.Create;
  Self.Timer := TIdThreadedTimerQueue.Create(false);
  Self.Timer.OnEmpty := Self.OnEmpty;

  Self.Transport := TIdSipMockTcpTransport.Create;
  Self.Transport.ClearBindings;
  Self.Transport.AddBinding('127.0.0.1', 5060);
  Self.Transport.AddTransportListener(Self);

  Self.Client         := TIdTcpClient.Create(nil);
  Self.HighPortServer := Self.ServerType.Create(nil);
  Self.LowPortServer  := Self.ServerType.Create(nil);
  Self.SipClient      := Self.HighPortServer.CreateClient;
  Self.SipClient.TransportID := Self.Transport.ID;

  Self.Client.Host := LocalHost;
  Self.Client.Port := LowPortServer.DefaultPort;

  Self.ClientReceivedResponse := false;
  Self.MethodCallCount        := 0;
  Self.ServerReceivedResponse := false;

  Self.LowPortServer.Timer := Self.Timer;
  Self.LowPortServer.TransportID := Self.Transport.ID;
  Self.LowPortServer.Bindings.Clear;
  Binding := LowPortServer.Bindings.Add;
  Binding.IP   := LocalHost;
  Binding.Port := DefaultSipPort;

  Self.HighPortServer.Timer := Self.Timer;
  Self.HighPortServer.TransportID := Self.Transport.ID;
  Self.HighPortServer.Bindings.Clear;
  Binding := Self.HighPortServer.Bindings.Add;
  Binding.IP   := GStack.LocalAddress;
  Binding.Port := DefaultSipPort + 10000;

  Self.HighPortLocation := TIdSipLocation.Create(TcpTransport,
                                                 Binding.IP,
                                                 Binding.Port);

  Self.LowPortServer.Active  := true;
  Self.HighPortServer.Active := true;

  Self.NotifiedMalformedMessage := false;

  CheckNotEquals('',
                 Self.Transport.FirstIPBound,
                 'Transport.FirstIPBound returned the empty string: add a binding');
end;

procedure TestTIdSipTcpServer.TearDown;
var
  WaitTime: Cardinal;
begin
  // Wait for all scheduled events to execute
  WaitTime := Self.Timer.DefaultTimeout * 3 div 2;

  Self.Timer.Terminate;
  Self.EmptyListEvent.WaitFor(WaitTime);

  Self.HighPortServer.Active := false;
  Self.LowPortServer.Active := false;

  Self.HighPortServer.DestroyClient(Self.SipClient);
  Self.LowPortServer.Free;
  Self.HighPortLocation.Free;
  Self.HighPortServer.Free;
  Self.Client.Free;

  Self.Transport.Free;
  Self.EmptyListEvent.Free;

  inherited TearDown;
end;

//* TestTIdSipTcpServer Protected methods **************************************

function TestTIdSipTcpServer.ServerType: TIdSipTcpServerClass;
begin
  Result := TIdSipTcpServer;
end;

//* TestTIdSipTcpServer Private methods ****************************************

procedure TestTIdSipTcpServer.AcknowledgeEvent(Sender: TObject;
                                               Request: TIdSipRequest);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTcpServer.AcknowledgeEvent(Sender: TObject;
                                               Response: TIdSipResponse);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTcpServer.CheckInternalServerError(Sender: TObject;
                                                       Response: TIdSipResponse);
begin
  CheckEquals(SIPInternalServerError, Response.StatusCode, 'Status-Code');
end;

procedure TestTIdSipTcpServer.CheckMultipleMessages(Sender: TObject;
                                                    Request: TIdSipRequest);
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

procedure TestTIdSipTcpServer.CheckMethodEvent(Sender: TObject;
                                               Request: TIdSipRequest);
var
  Expected: TIdSipHeaders;
begin
  try
    CheckEquals('INVITE',                                 Request.Method,         'Method');
    CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Request.RequestUri.URI, 'RequestUri');
    CheckEquals('SIP/2.0',                                Request.SIPVersion,     'SipVersion');
    CheckEquals(29,                                       Request.ContentLength,  'ContentLength');
    CheckEquals('a84b4c76e66710@gw1.leo-ix.org',          Request.CallID,         'CallID');
    CheckEquals(70,                                       Request.MaxForwards,    'Max-Forwards');

    Expected := TIdSipHeaders.Create;
    try
      Expected.Add(ViaHeaderFull).Value           := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
      Expected.Add(MaxForwardsHeader).Value       := '70';
      Expected.Add(ToHeaderFull).Value            := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';
      Expected.Add(FromHeaderFull).Value          := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
      Expected.Add(CallIDHeaderFull).Value        := 'a84b4c76e66710@gw1.leo-ix.org';
      Expected.Add(CSeqHeader).Value              := '314159 INVITE';
      Expected.Add(ContactHeaderFull).Value       := 'sip:wintermute@tessier-ashpool.co.luna';
      Expected.Add(ContentTypeHeaderFull).Value   := 'text/plain';
      Expected.Add(ContentLengthHeaderFull).Value := '29';

      // The transport will add a received tag to the Via:
      Expected[ViaHeaderFull].Params[ReceivedParam] := Request.LastHop.Received;

      Check(Expected.Equals(Request.Headers), 'Headers');
    finally
      Expected.Free;
    end;
    CheckEquals('I am a message. Hear me roar!', Request.Body, 'message-body');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpServer.OnEmpty(Sender: TIdTimerQueue);
begin
  Self.EmptyListEvent.SetEvent;
end;

procedure TestTIdSipTcpServer.OnException(FailedMessage: TIdSipMessage;
                                          E: Exception;
                                          const Reason: String);
begin
  Self.ExceptionType    := ExceptClass(E.ClassType);
  Self.ExceptionMessage := E.Message + ' caused by ''' + Reason + '''';
end;

procedure TestTIdSipTcpServer.OnReceiveRequest(Request: TIdSipRequest;
                                               Receiver: TIdSipTransport;
                                               Source: TIdConnectionBindings);
begin
  if Assigned(Self.CheckingRequestEvent) then
    Self.CheckingRequestEvent(Self, Request);
end;

procedure TestTIdSipTcpServer.OnReceiveResponse(Response: TIdSipResponse;
                                                Receiver: TIdSipTransport;
                                                Source: TIdConnectionBindings);
begin
  if Assigned(Self.CheckingResponseEvent) then
    Self.CheckingResponseEvent(Self, Response);
end;

procedure TestTIdSipTcpServer.OnRejectedMessage(const Msg: String;
                                                const Reason: String;
                                                Source: TIdConnectionBindings);
begin
  Self.NotifiedMalformedMessage := true;
end;

procedure TestTIdSipTcpServer.RaiseException(Sender: TObject;
                                             Request: TIdSipRequest);
begin
  raise Exception.Create('RaiseException');
end;

//* TestTIdSipTcpServer Published methods **************************************

procedure TestTIdSipTcpServer.TestInternalServerError;
var
  Request: TIdSipRequest;
begin
  Self.CheckingRequestEvent  := Self.RaiseException;
  Self.CheckingResponseEvent := Self.CheckInternalServerError;

  Request := TIdSipMessage.ReadRequestFrom(LocalLoopRequest);
  try
    SipClient.Host        := '127.0.0.1';
    SipClient.Port        := LowPortServer.DefaultPort;
    SipClient.ReadTimeout := 1000;

    SipClient.Connect(OneSecond);
    try
      SipClient.Send(Request);
    finally
      SipClient.Disconnect;
    end;
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipTcpServer.TestLeadingEmptyLines;
begin
  Self.CheckingRequestEvent := Self.CheckMethodEvent;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(#13#10#13#10#13#10
                  + Format(BasicRequest, [ViaFQDN]));

  Self.WaitForSignaled;
end;

procedure TestTIdSipTcpServer.TestMethodEvent;
begin
  Self.CheckingRequestEvent := Self.CheckMethodEvent;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(Format(BasicRequest, [ViaFQDN]));

  Self.WaitForSignaled;
end;

procedure TestTIdSipTcpServer.TestMultipleMessages;
begin
  Self.CheckingRequestEvent := Self.CheckMultipleMessages;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(Format(BasicRequest, [ViaFQDN])
                  + Format(BasicRequest, [ViaFQDN]));

  Self.WaitForSignaled;
  CheckEquals(2, Self.MethodCallCount, 'Method call count')
end;

procedure TestTIdSipTcpServer.TestReceiveRequest;
begin
  Self.CheckingRequestEvent := Self.AcknowledgeEvent;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Write(Format(BasicRequest, [ViaFQDN]));

  Self.WaitForSignaled;
  Check(nil <> Self.Transport.LastRequest,
        'Transport didn''t receive request');
end;

procedure TestTIdSipTcpServer.TestReceiveResponse;
var
  OK: TIdSipResponse;
begin
  Self.CheckingResponseEvent := Self.AcknowledgeEvent;

  Self.Client.Connect(DefaultTimeout);

  OK := TIdSipResponse.ReadResponseFrom(Format(BasicResponse, [ViaFQDN]));
  try
    OK.LastHop.SentBy := Self.Transport.FirstIPBound;

    Self.Client.Write(OK.AsString);
  finally
    OK.Free;
  end;

  Self.WaitForSignaled;
  Check(nil <> Self.Transport.LastResponse,
          'Transport didn''t receive Response');
end;

//******************************************************************************
//* TestTIdSipTcpClient                                                        *
//******************************************************************************
//* TestTIdSipTcpClient Public methods *****************************************

procedure TestTIdSipTcpClient.SetUp;
begin
  inherited SetUp;

  Self.DefaultTimeout := OneSecond;
  
  Self.EmptyListEvent := TSimpleEvent.Create;
  Self.ClientEvent := TSimpleEvent.Create;
  Self.Timer := TIdThreadedTimerQueue.Create(false);
  Self.Timer.OnEmpty := Self.OnEmpty;

  Self.Transport := TIdSipMockTcpTransport.Create;
  Self.Transport.ClearBindings;
  Self.Transport.AddBinding('127.0.0.1', 5060);
  Self.Transport.AddTransportListener(Self);

  Self.Client := TIdSipTcpClient.Create(nil);
  Self.Client.TransportID := Self.Transport.ID;
  Self.Server := TIdSipTcpServer.Create(nil);
  Self.Server.Timer := Self.Timer;
  Self.Server.TransportID := Self.Transport.ID;

  Self.Client.Host        := '127.0.0.1';
  Self.Client.Port        := Self.Server.DefaultPort;
  Self.Client.ReadTimeout := 1000;
  Self.Client.Timer       := Self.Timer;

  Self.Invite := TIdSipTestResources.CreateLocalLoopRequest;
  Self.Invite.LastHop.SentBy := Self.Transport.FirstIPBound;

  CheckNotEquals('',
                 Self.Transport.FirstIPBound,
                 'Transport.FirstIPBound returned the empty string: add a binding');

  Self.Finished              := false;
  Self.InviteCount           := 0;
  Self.ReceivedRequestMethod := '';
  Self.ReceivedResponseCount := 0;
  Self.Server.Active         := true;
end;

procedure TestTIdSipTcpClient.TearDown;
var
  WaitTime: Cardinal;
begin
  // Wait for all scheduled events to execute
  WaitTime := Self.Timer.DefaultTimeout * 3 div 2;
  Self.Timer.Terminate;
  Self.EmptyListEvent.WaitFor(WaitTime);

  Self.Server.Active := false;
  Self.Invite.Free;
  Self.Server.Free;
  Self.Client.Free;

  Self.Transport.Free;
  Self.ClientEvent.Free;
  Self.EmptyListEvent.Free;

  inherited TearDown;
end;

//* TestTIdSipTcpClient Private methods ****************************************

procedure TestTIdSipTcpClient.CheckReceiveOkResponse(Sender: TObject;
                                                     Response: TIdSipResponse);
begin
  try
    CheckEquals(SIPOK, Response.StatusCode, 'Unexpected response');
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.CheckReceiveOptions(Sender: TObject;
                                                  Request: TIdSipRequest);
begin
  try
    CheckEquals(MethodOptions, Request.Method, 'Unexpected request');
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.CheckReceiveProvisionalAndOkResponse(Sender: TObject;
                                                                   Response: TIdSipResponse);
begin
  try
    Inc(Self.ReceivedResponseCount);

    case Self.ReceivedResponseCount of
      1: CheckEquals(SIPTrying, Response.StatusCode, '1st response');
      2: begin
           CheckEquals(SIPOK,   Response.StatusCode, '2nd response');
           Self.ClientEvent.SetEvent;
         end;
    else
      Self.ExceptionMessage := 'Too many responses received';
    end;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.CheckSendInvite(Sender: TObject;
                                              Request: TIdSipRequest);
begin
  try
    CheckEquals(MethodInvite, Request.Method, 'Incorrect method');

    Self.SendOkResponse(Sender, Request);

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.CheckSendTwoInvites(Sender: TObject;
                                                  Request: TIdSipRequest);
begin
  try
    Inc(Self.InviteCount);

    Self.SendOkResponse(Sender, Request);

    if (Self.InviteCount > 1) then
      Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.ClientReceivedRequest(Sender: TObject;
                                                    R: TIdSipRequest);
begin
  Self.ReceivedRequestMethod := R.Method;
  Self.ThreadEvent.SetEvent;

  Self.CutConnection(Sender, R);
end;

procedure TestTIdSipTcpClient.CutConnection(Sender: TObject;
                                            R: TIdSipRequest);
var
  Threads: TList;
begin
  try
    Threads := Self.Server.Threads.LockList;
    try
      if (Threads.Count = 0) then
        raise Exception.Create('TCP connection disappeared: CutConnection');

      (TObject(Threads[0]) as TIdPeerThread).Connection.DisconnectSocket;
    finally
      Self.Server.Threads.UnlockList;
    end;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.OnEmpty(Sender: TIdTimerQueue);
begin
  Self.EmptyListEvent.SetEvent;
end;

procedure TestTIdSipTcpClient.OnException(FailedMessage: TIdSipMessage;
                                          E: Exception;
                                          const Reason: String);
begin
  Self.ExceptionType    := ExceptClass(E.ClassType);
  Self.ExceptionMessage := E.Message + ' caused by ''' + Reason + '''';
end;

procedure TestTIdSipTcpClient.OnReceiveRequest(Request: TIdSipRequest;
                                               Receiver: TIdSipTransport;
                                               Source: TIdConnectionBindings);
begin
  if Assigned(Self.CheckingRequestEvent) then
    Self.CheckingRequestEvent(Self, Request);

  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTcpClient.OnReceiveResponse(Response: TIdSipResponse;
                                                Receiver: TIdSipTransport;
                                                Source: TIdConnectionBindings);
begin
  if Assigned(Self.CheckingResponseEvent) then
    Self.CheckingResponseEvent(Self, Response);

  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTcpClient.OnRejectedMessage(const Msg: String;
                                                const Reason: String;
                                                Source: TIdConnectionBindings);
begin
  Self.ExceptionType    := ETestFailure;
  Self.ExceptionMessage := 'Message rejected: ' + Reason;
end;

procedure TestTIdSipTcpClient.PauseAndSendOkResponse(Sender: TObject;
                                                     Request: TIdSipRequest);
begin
  Sleep(Self.DefaultTimeout);
  Self.SendOkResponse(Sender, Request);
end;

procedure TestTIdSipTcpClient.ReceiveOptions;
var
  Connection: TIdTCPConnection;
  S:          String;
  Threads:    TList;
begin
  S := StringReplace(LocalLoopRequest, MethodInvite, MethodOptions, [rfReplaceAll]);

  Threads := Self.Server.Threads.LockList;
  try
    if (Threads.Count = 0) then
      raise Exception.Create('TCP connection disappeared: SendOptionsRequest');

    Connection := (TObject(Threads[0]) as TIdPeerThread).Connection;
    if not Connection.Connected then
      raise Exception.Create('TCP connection closed');

    Connection.Write(S);
  finally
    Self.Server.Threads.UnlockList;
  end;
end;

procedure TestTIdSipTcpClient.SendOkResponse(Sender: TObject;
                                             Request: TIdSipRequest);
var
  S:       String;
  Threads: TList;
begin
  S := StringReplace(LocalLoopResponse, '486 Busy Here', '200 OK', []);

  Threads := Self.Server.Threads.LockList;
  try
    if (Threads.Count = 0) then
      raise Exception.Create('TCP connection disappeared: SendOkResponse');

    (TObject(Threads[0]) as TIdPeerThread).Connection.Write(S);
  finally
    Self.Server.Threads.UnlockList;
  end;

  Self.CutConnection(Sender, Request);
end;

procedure TestTIdSipTcpClient.SendProvisionalAndOkResponse(Sender: TObject;
                                                           Request: TIdSipRequest);
var
  OK:      TIdSipResponse;
  Threads: TList;
  Trying:  TIdSipResponse;
begin
  Trying := TIdSipResponse.InResponseTo(Self.Invite, SIPTrying);
  try
    Ok := TIdSipResponse.InResponseTo(Self.Invite, SIPOK);
    try
      Threads := Self.Server.Threads.LockList;
      try
        if (Threads.Count = 0) then
          raise Exception.Create('TCP connection disappeared: SendProvisionalAndOkResponse');

        (TObject(Threads[0]) as TIdPeerThread).Connection.Write(Trying.AsString);
        Sleep(2*Self.DefaultTimeout);
        (TObject(Threads[0]) as TIdPeerThread).Connection.Write(OK.AsString);
      finally
        Self.Server.Threads.UnlockList;
      end;
    finally
      OK.Free;
    end;
  finally
    Trying.Free;
  end;

  Self.CutConnection(Sender, Request);
end;

procedure TestTIdSipTcpClient.SendResponseReceiveOptions(Sender: TObject;
                                                         Response: TIdSipResponse);
var
  Options: TIdSipRequest;
  Threads: TList;
begin
  Options := Self.Invite.Copy as TIdSipRequest;
  try
    Options.Method := MethodOptions;
    Options.CSeq.Method := Options.Method;

    Threads := Self.Server.Threads.LockList;
    try
      if (Threads.Count = 0) then
        raise Exception.Create('TCP connection disappeared: SendProvisionalAndOkResponse');

      (TObject(Threads[0]) as TIdPeerThread).Connection.Write(Options.AsString);
    finally
      Self.Server.Threads.UnlockList;
    end;
  finally
    Options.Free;
  end;
end;

//* TestTIdSipTcpClient Published methods **************************************

procedure TestTIdSipTcpClient.TestCanReceiveRequest;
begin
  Self.CheckingRequestEvent := Self.ClientReceivedRequest;

  Self.Client.Connect(Self.DefaultTimeout);
  Self.ReceiveOptions;
  Self.Client.ReceiveMessages;

  CheckEquals(MethodOptions,
             Self.ReceivedRequestMethod,
             'Unexpected received request');
end;

procedure TestTIdSipTcpClient.TestConnectAndDisconnect;
begin
  Self.Client.Host := '127.0.0.1';
  Self.Client.Port := DefaultSipPort;
  Self.Client.Connect(Self.DefaultTimeout);
  try
    Check(Self.Client.Connected, 'Client didn''t connect');
  finally
    Self.Client.Disconnect;
  end;
end;

procedure TestTIdSipTcpClient.TestTerminatedWithServerDisconnect;
begin
  Self.CheckingRequestEvent := Self.CutConnection;

  Self.Client.Connect(Self.DefaultTimeout);
  Self.Client.Send(Self.Invite);
  Self.Client.ReceiveMessages;

  Check(Self.Client.Terminated, 'After connection unexpectedly cut');
end;

procedure TestTIdSipTcpClient.TestReceiveOkResponse;
begin
  Self.CheckingRequestEvent  := Self.SendOkResponse;
  Self.CheckingResponseEvent := Self.CheckReceiveOkResponse;

  Self.Client.Connect(Self.DefaultTimeout);
  Self.Client.Send(Self.Invite);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTcpClient.TestReceiveOkResponseWithPause;
var
  DebugTimer: TIdDebugTimerQueue;
  W:          TIdSipReceiveMessageWait;
begin
  DebugTimer := TIdDebugTimerQueue.Create(false);
  try
    Self.Client.Timer := DebugTimer;

    Self.CheckingRequestEvent  := Self.PauseAndSendOkResponse;
    Self.CheckingResponseEvent := Self.CheckReceiveOkResponse;

    Self.Client.Connect(Self.DefaultTimeout);
    Self.Client.Send(Self.Invite);

    Self.Client.ReceiveMessages;

    W := DebugTimer.LastEventScheduled(TIdSipReceiveMessageWait) as TIdSipReceiveMessageWait;
    Check(nil <> W, 'No responses received');
    Check(W.Message.IsResponse, 'Received something other than a response');
    CheckEquals(SIPOK, (W.Message as TIdSipResponse).StatusCode, 'Unexpected response');
  finally
    Self.Client.Timer := Self.Timer;
    DebugTimer.Free;
  end;
end;

procedure TestTIdSipTcpClient.TestReceiveProvisionalAndOkResponse;
var
  DebugTimer: TIdDebugTimerQueue;
begin
  DebugTimer := TIdDebugTimerQueue.Create(false);
  try
    Self.Client.Timer := DebugTimer;
    Self.CheckingRequestEvent  := Self.SendProvisionalAndOkResponse;
    Self.CheckingResponseEvent := Self.CheckReceiveProvisionalAndOkResponse;

    Self.Client.Connect(Self.DefaultTimeout);
    Self.Client.Send(Self.Invite);
    Self.Client.ReceiveMessages;

    CheckEquals(2, DebugTimer.EventCountFor(TIdSipReceiveMessageWait), 'Received response count');
  finally
    Self.Client.Timer := Self.Timer;
    DebugTimer.Free;
  end;
end;

procedure TestTIdSipTcpClient.TestSendInvite;
begin
  Self.CheckingRequestEvent := Self.CheckSendInvite;

  Self.Client.Connect(Self.DefaultTimeout);
  Self.Client.Send(Self.Invite);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTcpClient.TestSendResponseReceiveOptions;
var
  OK: TIdSipResponse;
begin
  Self.CheckingResponseEvent := Self.SendResponseReceiveOptions;
  Self.CheckingRequestEvent  := Self.CheckReceiveOptions;

  OK := TIdSipResponse.InResponseTo(Self.Invite, SIPOK);
  try
    Self.Client.Connect(Self.DefaultTimeout);
    Self.Client.Send(OK);
  finally
    OK.Free;
  end;
end;

procedure TestTIdSipTcpClient.TestSendTwoInvites;
begin
  Self.CheckingRequestEvent := Self.CheckSendTwoInvites;

  Self.Client.Connect(Self.DefaultTimeout);
  Self.Client.Send(Self.Invite);

  Self.Client.Send(Self.Invite);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTcpClient.TestSendWithServerDisconnect;
begin
  Self.CheckingRequestEvent := Self.CutConnection;

  Self.Client.Connect(Self.DefaultTimeout);
  Self.Client.Send(Self.Invite);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTcpClient.TestSetConserveConnectionsAfterConnect;
begin
  Self.CheckingRequestEvent := Self.CheckSendInvite;

  Self.Client.Connect(Self.DefaultTimeout);
  Self.Client.ConserveConnections := true;

  Self.Client.Send(Self.Invite);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTcpClient.TestSetConserveConnectionsThenConnect;
begin
  Self.CheckingRequestEvent := Self.CheckSendInvite;

  Self.Client.ConserveConnections := true;
  Self.Client.Connect(Self.DefaultTimeout);

  Self.Client.Send(Self.Invite);

  Self.WaitForSignaled;
end;

//******************************************************************************
//* TestTIdSipConnectionTableEntry                                             *
//******************************************************************************
//* TestTIdSipConnectionTableEntry Private methods *****************************

procedure TestTIdSipConnectionTableEntry.SetUp;
var
  Binding: TIdSocketHandle;
  Client:  TIdTcpClient;
begin
  inherited SetUp;

  Client := TIdTcpClient.Create(nil);
  Self.Connection := Client;

  Self.Request := TIdSipRequest.Create;

  Self.Server := TIdTcpServer.Create(nil);
  Self.Server.OnExecute := Self.DoOnExecute;
  Binding := Self.Server.Bindings.Add;
  Binding.IP   := '127.0.0.1';
  Binding.Port := 5060;

  Self.Server.Active := true;

  Client.Host := Binding.IP;
  Client.Port := Binding.Port;
  Client.Connect(DefaultTimeout);
end;

procedure TestTIdSipConnectionTableEntry.TearDown;
begin
  Self.Server.Free;
  Self.Request.Free;
  Self.Connection.Free;

  inherited TearDown;
end;

//* TestTIdSipConnectionTableEntry Private methods *****************************

procedure TestTIdSipConnectionTableEntry.DoOnExecute(Thread: TIdPeerThread);
begin
  // Do nothing.
end;

//* TestTIdSipConnectionTableEntry Published methods ***************************

procedure TestTIdSipConnectionTableEntry.TestCreate;
var
  E: TIdSipConnectionTableEntry;
begin
  E := TIdSipConnectionTableEntry.Create(Self.Connection, Self.Request);
  try
    Check(Self.Connection = E.Connection,   'Connection not set');
    Check(Self.Request.Equals(E.Request), 'Request not set');

    CheckEquals(Self.Connection.Socket.Binding.IP,       E.Binding.LocalIP,   'Binding LocalIP');
    CheckEquals(Self.Connection.Socket.Binding.Port,     E.Binding.LocalPort, 'Binding LocalPort');
    CheckEquals(Self.Connection.Socket.Binding.PeerIP,   E.Binding.PeerIP,    'Binding PeerIP');
    CheckEquals(Self.Connection.Socket.Binding.PeerPort, E.Binding.PeerPort,  'Binding PeerPort');
    CheckEquals(TcpTransport,                            E.Binding.Transport, 'Binding Transport');
  finally
    E.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipConnectionTable                                                  *
//******************************************************************************
//* TestTIdSipConnectionTable Public methods ***********************************

procedure TestTIdSipConnectionTable.SetUp;
begin
  inherited SetUp;

  Self.Conn    := TIdTCPConnection.Create(nil);
  Self.NewConn := TIdTCPConnection.Create(nil);
  Self.NewReq  := TIdSipRequest.Create;
  Self.Req     := TIdSipRequest.Create;
  Self.Table   := TIdSipConnectionTable.Create;

  Self.Req.RequestUri.URI    := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.NewReq.RequestUri.URI := 'sip:case@fried.neurons.org';
end;

procedure TestTIdSipConnectionTable.TearDown;
begin
  Self.Table.Free;
  Self.Req.Free;
  Self.NewReq.Free;
  Self.NewConn.Free;
  Self.Conn.Free;

  inherited TearDown;
end;

//* TestTIdSipConnectionTable Published methods ********************************

procedure TestTIdSipConnectionTable.TestAddAndCount;
var
  Count: Integer;
begin
  Count := Self.Table.Count;

  Self.Table.Add(Self.Conn, Self.Req);

  CheckEquals(Count + 1, Self.Table.Count, 'No entry added');
end;

procedure TestTIdSipConnectionTable.TestAddPreventsDuplicates;
var
  Count: Integer;
begin
  Count := Self.Table.Count;

  Self.Table.Add(Self.Conn, Self.Req);
  Self.Table.Add(Self.Conn, Self.Req);

  CheckEquals(Count + 1, Self.Table.Count, 'Duplicate entry added');
end;

procedure TestTIdSipConnectionTable.TestConnectionFor;
begin
  Self.Table.Add(Self.Conn,    Self.Req);
  Self.Table.Add(Self.NewConn, Self.NewReq);

  Check(Self.Table.ConnectionFor(Self.Req) = Self.Conn,
        'Wrong Connection 1');
  Check(Self.Table.ConnectionFor(Self.NewReq) = Self.NewConn,
        'Wrong Connection 2');
end;

procedure TestTIdSipConnectionTable.TestConnectionForOneEntry;
begin
  Self.Table.Add(Self.Conn, Self.Req);

  Check(Self.Table.ConnectionFor(Self.Req) = Self.Conn,
        'Wrong Connection');
end;

procedure TestTIdSipConnectionTable.TestConnectionForOnEmptyList;
begin
  Check(not Assigned(Self.Table.ConnectionFor(Self.Req)), 'non-nil result');
end;

procedure TestTIdSipConnectionTable.TestConnectionForOnNoEntry;
begin
  Self.Table.Add(Self.Conn, Self.Req);

  Check(not Assigned(Self.Table.ConnectionFor(Self.NewReq)), 'non-nil result');
end;

procedure TestTIdSipConnectionTable.TestConnectionForResponse;
var
  Response: TIdSipResponse;
begin
  Self.Req.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TCP localhost;' + BranchParam + '=' + BranchMagicCookie + 'f00';
  Self.Req.Method := MethodOptions;

  Self.Table.Add(Self.Conn, Self.Req);

  Response := TIdSipResponse.Create;
  try
    Response.AddHeader(Self.Req.LastHop);
    Response.CSeq.Method := MethodOptions;

    Check(Self.Conn = Self.Table.ConnectionFor(Response), 'Wrong connection');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipConnectionTable.TestRemove;
var
  Count: Integer;
begin
  Count := Self.Table.Count;

  Self.Table.Add(Self.Conn, Self.Req);
  Self.Table.Remove(Self.Conn);
  CheckEquals(Count, Self.Table.Count, 'No entry removed');
end;

procedure TestTIdSipConnectionTable.TestRemoveOnEmptyList;
begin
  Self.Table.Remove(Self.Conn);
end;

procedure TestTIdSipConnectionTable.TestRemoveOnNonEmptyList;
var
  Count: Integer;
begin
  Self.Table.Add(Self.Conn, Self.Req);
  Self.Table.Add(Self.NewConn, NewReq);

  Count := Self.Table.Count;

  Self.Table.Remove(Self.NewConn);

  CheckEquals(Count - 1, Self.Table.Count, 'Nothing was removed');
  Check(Self.Table.ConnectionFor(Self.Req) = Self.Conn, 'Wrong entry removed (ConnectionFor)');
end;

procedure TestTIdSipConnectionTable.TestRemoveWithMultipleRequests;
var
  OldCount: Integer;
begin
  OldCount := Self.Table.Count;

  Self.Table.Add(Self.Conn, Self.Req);
  Self.Table.Add(Self.Conn, Self.NewReq);

  Self.Table.Remove(Self.Conn);
  CheckEquals(OldCount, Self.Table.Count, 'Not all request-connection associations removed');
end;

//******************************************************************************
//* TTcpTransportWaitTestCase                                                  *
//******************************************************************************
//* TTcpTransportWaitTestCase Public methods ***********************************

procedure TTcpTransportWaitTestCase.SetUp;
begin
  inherited SetUp;

  Self.Conn      := TIdTCPConnection.Create(nil);
  Self.Invite    := TIdSipRequest.Create;
  Self.Transport := TIdSipTCPTransport.Create;
end;

procedure TTcpTransportWaitTestCase.TearDown;
begin
  Self.Transport.Free;
  Self.Invite.Free;
  Self.Conn.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipTcpConnectionOpenWait                                            *
//******************************************************************************
//* TestTIdSipTcpConnectionOpenWait Public methods *****************************

procedure TestTIdSipTcpConnectionOpenWait.SetUp;
begin
  inherited SetUp;

  Self.Wait := TIdSipTcpConnectionOpenWait.Create;
  Self.Wait.Binding        := Self.Conn;
  Self.Wait.OpeningRequest := Self.Invite;
  Self.Wait.TransportID    := Self.Transport.ID;
end;

procedure TestTIdSipTcpConnectionOpenWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdSipTcpConnectionOpenWait Published methods **************************

procedure TestTIdSipTcpConnectionOpenWait.TestTrigger;
var
  L: TConnectionListener;
begin
  L := TConnectionListener.Create;
  try
    Self.Transport.AddConnectionListener(L);

    Self.Wait.Trigger;

    Check(L.ConnectionOpened, 'Listener not notified: Wait didn''t Trigger');
  finally
    Self.Transport.RemoveConnectionListener(L);
    L.Free;
  end;
end;

procedure TestTIdSipTcpConnectionOpenWait.TestTriggerOnNonExistentAction;
begin
  // Check that the Wait doesn't blow up when given the ID of a nonexistent
  // transport.
  Self.Wait.TransportID := 'fake ID';
  Self.Wait.Trigger;
end;

procedure TestTIdSipTcpConnectionOpenWait.TestTriggerOnWrongTypeOfObject;
var
  R: TIdRegisteredObject;
begin
  // Check that the Wait doesn't blow up when given the ID of a non-transport
  // object.
  R := TIdRegisteredObject.Create;
  try
    Self.Wait.TransportID := R.ID;
    Self.Wait.Trigger;
  finally
    R.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTcpConnectionCloseWait                                           *
//******************************************************************************
//* TestTIdSipTcpConnectionCloseWait Public methods ****************************

procedure TestTIdSipTcpConnectionCloseWait.SetUp;
begin
  inherited SetUp;

  Self.Wait := TIdSipTcpConnectionCloseWait.Create;
  Self.Wait.Binding     := Self.Conn;
  Self.Wait.TransportID := Self.Transport.ID;
end;

procedure TestTIdSipTcpConnectionCloseWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdSipTcpConnectionCloseWait Published methods **************************

procedure TestTIdSipTcpConnectionCloseWait.TestTrigger;
var
  L: TConnectionListener;
begin
  L := TConnectionListener.Create;
  try
    Self.Transport.AddConnectionListener(L);

    Self.Transport.AddConnection(Self.Conn, Self.Invite);
    Self.Wait.Trigger;

    Check(L.ConnectionClosed, 'Listener not notified: Wait didn''t Trigger');
  finally
    Self.Transport.RemoveConnectionListener(L);
    L.Free;
  end;
end;

procedure TestTIdSipTcpConnectionCloseWait.TestTriggerOnNonExistentAction;
begin
  // Check that the Wait doesn't blow up when given the ID of a nonexistent
  // transport.
  Self.Wait.TransportID := 'fake ID';
  Self.Wait.Trigger;
end;

procedure TestTIdSipTcpConnectionCloseWait.TestTriggerOnWrongTypeOfObject;
var
  R: TIdRegisteredObject;
begin
  // Check that the Wait doesn't blow up when given the ID of a non-transport
  // object.
  R := TIdRegisteredObject.Create;
  try
    Self.Wait.TransportID := R.ID;
    Self.Wait.Trigger;
  finally
    R.Free;
  end;
end;

initialization
  RegisterTest('TCP transport', Suite);
end.
