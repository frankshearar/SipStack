unit IdSipTransport;

interface

uses
  Classes, Contnrs, IdException, IdSipHeaders, IdInterfacedObject,
  IdSipMessage, IdSipTcpClient, IdSipTcpServer, IdSipTlsServer, IdSipUdpClient,
  IdSipUdpServer, IdSocketHandle, IdSSLOpenSSL, IdTCPClient, IdTCPServer,
  IdThread, SyncObjs, SysUtils;

type
  TIdSipTransport = class;
  TIdSipTransportClass = class of TIdSipTransport;

  // I listen for incoming messages.
  IIdSipTransportListener = interface
    ['{D3F0A0D5-A4E9-42BD-B337-D5B3C652F340}']
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transport: TIdSipTransport);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transport: TIdSipTransport);
  end;

  // I listen for when messages are sent, rather than received. I'm most useful
  // as a logger/debugging tool.
  IIdSipTransportSendingListener = interface
    ['{2E451F5D-5053-4A2C-BE5F-BB68E5CB3A6D}']
    procedure OnSendRequest(const Request: TIdSipRequest;
                            const Transport: TIdSipTransport);
    procedure OnSendResponse(const Response: TIdSipResponse;
                             const Transport: TIdSipTransport);
  end;

  TIdSipTransport = class(TIdInterfacedObject,
                          IIdSipMessageVisitor,
                          IIdSipMessageListener)
  private
    fHostName:                    String;
    fTimeout:                     Cardinal;
    TransportListenerLock:        TCriticalSection;
    TransportListeners:           TList;
    TransportSendingListenerLock: TCriticalSection;
    TransportSendingListeners:    TList;

    procedure RewriteOwnVia(Msg: TIdSipMessage);
  protected
    function  GetBindings: TIdSocketHandles; virtual; abstract;
    function  GetPort: Cardinal; virtual; abstract;
    procedure NotifyTransportListeners(const Request: TIdSipRequest); overload;
    procedure NotifyTransportListeners(const Response: TIdSipResponse); overload;
    procedure NotifyTransportSendingListeners(const Request: TIdSipRequest); overload;
    procedure NotifyTransportSendingListeners(const Response: TIdSipResponse); overload;
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const ReceivedFrom: TIdSipConnectionBindings); virtual;
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const ReceivedFrom: TIdSipConnectionBindings);
    procedure SendRequest(const R: TIdSipRequest); virtual;
    procedure SendResponse(const R: TIdSipResponse); virtual;
    function  SentByIsRecognised(const Via: TIdSipViaHeader): Boolean; virtual;
    procedure SetPort(const Value: Cardinal); virtual; abstract;
  public
    class function TransportFor(const TT: TIdSipTransportType): TIdSipTransportClass;
    constructor Create(const Port: Cardinal); virtual;
    destructor  Destroy; override;

    procedure AddTransportListener(const Listener: IIdSipTransportListener);
    procedure AddTransportSendingListener(const Listener: IIdSipTransportSendingListener);
    function  DefaultPort: Cardinal; virtual;
    function  DefaultTimeout: Cardinal; virtual;
    function  GetTransportType: TIdSipTransportType; virtual; abstract;
    function  IsReliable: Boolean; virtual;
    function  IsSecure: Boolean; virtual;
    procedure RemoveTransportListener(const Listener: IIdSipTransportListener);
    procedure RemoveTransportSendingListener(const Listener: IIdSipTransportSendingListener);
    procedure Send(const Msg: TIdSipMessage);
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure VisitRequest(const Request: TIdSipRequest);
    procedure VisitResponse(const Response: TIdSipResponse);

    property Bindings: TIdSocketHandles read GetBindings;
    property HostName: String           read fHostName write fHostName;
    property Port:     Cardinal         read GetPort write SetPort;
    property Timeout:  Cardinal         read fTimeout write fTimeout;
  end;

  TIdSipTCPTransport = class(TIdSipTransport)
  private
    Clients:    TObjectList;
    ClientLock: TCriticalSection;

    function  AddClient: TIdSipTcpClient;
    procedure DoOnClientFinished(Sender: TObject);
    procedure DoOnTcpResponse(Sender: TObject;
                              const Response: TIdSipResponse;
                              const ReceivedFrom: TIdSipConnectionBindings);
    procedure RemoveClient(Client: TIdSipTcpClient);
  protected
    Transport: TIdSipTcpServer;

    function  CreateClient: TIdSipTcpClient; virtual;
    procedure DestroyClient(Client: TIdSipTcpClient); virtual;
    function  GetBindings: TIdSocketHandles; override;
    function  GetPort: Cardinal; override;
    procedure SendRequest(const R: TIdSipRequest); override;
    procedure SendResponse(const R: TIdSipResponse); override;
    function  ServerType: TIdSipTcpServerClass; virtual;
    procedure SetPort(const Value: Cardinal); override;
  public
    constructor Create(const Port: Cardinal); override;
    destructor  Destroy; override;

    function  GetTransportType: TIdSipTransportType; override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TIdSipTLSTransport = class(TIdSipTCPTransport)
  private
    function  GetOnGetPassword: TPasswordEvent;
    function  GetRootCertificate: TFileName;
    function  GetServerCertificate: TFileName;
    function  GetServerKey: TFileName;
    function  TLS: TIdSipTlsServer;
    procedure SetOnGetPassword(const Value: TPasswordEvent);
    procedure SetRootCertificate(const Value: TFileName);
    procedure SetServerCertificate(const Value: TFileName);
    procedure SetServerKey(const Value: TFileName);
  protected
    function  CreateClient: TIdSipTcpClient; override;
    procedure DestroyClient(Client: TIdSipTcpClient); override;
    function  ServerType: TIdSipTcpServerClass; override;
  public
    function  DefaultPort: Cardinal; override;
    function  GetTransportType: TIdSipTransportType; override;
    function  IsSecure: Boolean; override;

    property OnGetPassword:     TPasswordEvent read GetOnGetPassword write SetOnGetPassword;
    property RootCertificate:   TFileName read GetRootCertificate write SetRootCertificate;
    property ServerCertificate: TFileName read GetServerCertificate write SetServerCertificate;
    property ServerKey:         TFileName read GetServerKey write SetServerKey;
  end;

  TIdSipSCTPTransport = class(TIdSipTransport)
  public
    function GetTransportType: TIdSipTransportType; override;
  end;

  TIdSipUDPTransport = class;

  TIdServerCleanupThread = class(TIdThread)
  private
    fOwner:            TIdSipUDPTransport;
    fPollTime:         Cardinal;
    TerminatedServers: TObjectList;
    ListLock:          TCriticalSection;

    procedure CleanOutTerminatedClients;
  public
    constructor Create(Owner: TIdSipUDPTransport); reintroduce;
    destructor  Destroy; override;

    function  DefaultPollTime: Cardinal;
    procedure TerminateClient(Server: TIdSipUdpClient);
    procedure Run; override;

    property PollTime: Cardinal read fPollTime write fPollTime;
  end;

  TIdSipUDPTransport = class(TIdSipTransport)
  private
    ClientCleaner: TIdServerCleanupThread;
    Clients:       TObjectList;
    ClientLock:    TCriticalSection;
    Transport:     TIdSipUdpServer;

    function  AddClient: TIdSipUdpClient;
    procedure DoOnClientFinished(Sender: TObject);
    function  GetCleanerThreadPollTime: Cardinal;
    procedure RemoveClient(Client: TIdSipUdpClient);
    procedure SendRequestNotExpectingResponses(const R: TIdSipRequest);
    procedure SetCleanerThreadPollTime(const Value: Cardinal);
  protected
    function  GetBindings: TIdSocketHandles; override;
    function  GetPort: Cardinal; override;
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const ReceivedFrom: TIdSipConnectionBindings); override;
    procedure SendRequest(const R: TIdSipRequest); override;
    procedure SendResponse(const R: TIdSipResponse); override;
    procedure SetPort(const Value: Cardinal); override;
  public
    constructor Create(const Port: Cardinal); override;
    destructor  Destroy; override;

    function  GetTransportType: TIdSipTransportType; override;
    function  IsReliable: Boolean; override;
    procedure Start; override;
    procedure Stop; override;

    property CleanerThreadPollTime: Cardinal read GetCleanerThreadPollTime write SetCleanerThreadPollTime;
  end;

  EUnknownTransport = class(EIdException);

implementation

uses
  IdGlobal, IdSipConsts, IdUdpClient;

//******************************************************************************
//* TIdSipTransport                                                            *
//******************************************************************************
//* TIdSipTransport Public methods *********************************************

class function TIdSipTransport.TransportFor(const TT: TIdSipTransportType): TIdSipTransportClass;
begin
  case TT of
    sttSCTP: Result := TIdSipSCTPTransport;
    sttTCP:  Result := TIdSipTCPTransport;
    sttTLS:  Result := TIdSipTLSTransport;
    sttUDP:  Result := TIdSipUDPTransport;
  else
    raise EUnknownTransport.Create('TIdSipTransport.TransportFor');
  end;
end;

constructor TIdSipTransport.Create(const Port: Cardinal);
begin
  inherited Create;

  Self.TransportListenerLock := TCriticalSection.Create;
  Self.TransportListeners    := TList.Create;

  Self.TransportSendingListenerLock := TCriticalSection.Create;
  Self.TransportSendingListeners    := TList.Create;

  Self.Timeout := Self.DefaultTimeout;
end;

destructor TIdSipTransport.Destroy;
begin
  Self.TransportSendingListeners.Free;
  Self.TransportSendingListenerLock.Free;

  Self.TransportListeners.Free;
  Self.TransportListenerLock.Free;

  inherited Destroy;
end;

procedure TIdSipTransport.AddTransportListener(const Listener: IIdSipTransportListener);
begin
  Self.TransportListenerLock.Acquire;
  try
    Self.TransportListeners.Add(Pointer(Listener))
  finally
    Self.TransportListenerLock.Release;
  end;
end;

procedure TIdSipTransport.AddTransportSendingListener(const Listener: IIdSipTransportSendingListener);
begin
  Self.TransportSendingListenerLock.Acquire;
  try
    Self.TransportSendingListeners.Add(Pointer(Listener))
  finally
    Self.TransportSendingListenerLock.Release;
  end;
end;

function TIdSipTransport.DefaultPort: Cardinal;
begin
  Result := IdPORT_SIP;
end;

function TIdSipTransport.DefaultTimeout: Cardinal;
begin
  Result := 5000;
end;

function TIdSipTransport.IsReliable: Boolean;
begin
  Result := true;
end;

function TIdSipTransport.IsSecure: Boolean;
begin
  Result := false;
end;

procedure TIdSipTransport.RemoveTransportListener(const Listener: IIdSipTransportListener);
begin
  Self.TransportListenerLock.Acquire;
  try
    Self.TransportListeners.Remove(Pointer(Listener))
  finally
    Self.TransportListenerLock.Release;
  end;
end;

procedure TIdSipTransport.RemoveTransportSendingListener(const Listener: IIdSipTransportSendingListener);
begin
  Self.TransportSendingListenerLock.Acquire;
  try
    Self.TransportSendingListeners.Remove(Pointer(Listener))
  finally
    Self.TransportSendingListenerLock.Release;
  end;
end;

procedure TIdSipTransport.Send(const Msg: TIdSipMessage);
begin
  Msg.Accept(Self);
end;

procedure TIdSipTransport.Start;
begin
end;

procedure TIdSipTransport.Stop;
begin
end;

procedure TIdSipTransport.VisitRequest(const Request: TIdSipRequest);
begin
  Self.SendRequest(Request);
end;

procedure TIdSipTransport.VisitResponse(const Response: TIdSipResponse);
begin
  Self.SendResponse(Response);
end;

//* TIdSipTransport Protected methods ******************************************

procedure TIdSipTransport.NotifyTransportListeners(const Request: TIdSipRequest);
var
  I: Integer;
begin
  Self.TransportListenerLock.Acquire;
  try
    for I := 0 to Self.TransportListeners.Count - 1 do
      IIdSipTransportListener(Self.TransportListeners[I]).OnReceiveRequest(Request, Self);
  finally
    Self.TransportListenerLock.Release;
  end;
end;

procedure TIdSipTransport.NotifyTransportListeners(const Response: TIdSipResponse);
var
  I: Integer;
begin
  Self.TransportListenerLock.Acquire;
  try
    for I := 0 to Self.TransportListeners.Count - 1 do
      IIdSipTransportListener(Self.TransportListeners[I]).OnReceiveResponse(Response, Self);
  finally
    Self.TransportListenerLock.Release;
  end;
end;

procedure TIdSipTransport.NotifyTransportSendingListeners(const Request: TIdSipRequest);
var
  I: Integer;
begin
  Self.TransportSendingListenerLock.Acquire;
  try
    for I := 0 to Self.TransportSendingListeners.Count - 1 do
      IIdSipTransportSendingListener(Self.TransportSendingListeners[I]).OnSendRequest(Request, Self);
  finally
    Self.TransportSendingListenerLock.Release;
  end;
end;

procedure TIdSipTransport.NotifyTransportSendingListeners(const Response: TIdSipResponse);
var
  I: Integer;
begin
  Self.TransportSendingListenerLock.Acquire;
  try
    for I := 0 to Self.TransportSendingListeners.Count - 1 do
      IIdSipTransportSendingListener(Self.TransportSendingListeners[I]).OnSendResponse(Response, Self);
  finally
    Self.TransportSendingListenerLock.Release;
  end;
end;

procedure TIdSipTransport.OnReceiveRequest(const Request: TIdSipRequest;
                                           const ReceivedFrom: TIdSipConnectionBindings);
begin
  // cf. RFC 3261 section 18.2.1
  if TIdSipParser.IsFQDN(Request.LastHop.SentBy)
    or (Request.LastHop.SentBy <> ReceivedFrom.PeerIP) then
    Request.LastHop.Received := ReceivedFrom.PeerIP;

  Self.NotifyTransportListeners(Request);
end;

procedure TIdSipTransport.OnReceiveResponse(const Response: TIdSipResponse;
                                            const ReceivedFrom: TIdSipConnectionBindings);
begin
  // cf. RFC 3261 section 18.1.2

  if Self.SentByIsRecognised(Response.LastHop) then begin
    Self.NotifyTransportListeners(Response);
  end;
end;

procedure TIdSipTransport.SendRequest(const R: TIdSipRequest);
begin
  Self.RewriteOwnVia(R);
  Self.NotifyTransportSendingListeners(R);
end;

procedure TIdSipTransport.SendResponse(const R: TIdSipResponse);
begin
//  Self.RewriteOwnVia(R);
  Self.NotifyTransportSendingListeners(R);
end;

function TIdSipTransport.SentByIsRecognised(const Via: TIdSipViaHeader): Boolean;
var
  I: Integer;
begin
  Result := IsEqual(Via.SentBy, Self.HostName);

  I := 0;

  if not Result then begin
    while (I < Self.Bindings.Count) and not Result do begin
      Result := Result or (Self.Bindings[I].IP = Via.SentBy);

      Inc(I);
    end;
  end;
end;

//* TIdSipTransport Private methods ********************************************

procedure TIdSipTransport.RewriteOwnVia(Msg: TIdSipMessage);
begin
  Assert(Msg.Path.Length > 0,
         'An outbound message must always have at least one Via, '
       + 'namely, this stack.');

  Msg.LastHop.Transport := Self.GetTransportType;
  Msg.LastHop.SentBy := Self.HostName;
end;

//******************************************************************************
//* TIdSipTCPTransport                                                         *
//******************************************************************************
//* TIdSipTCPTransport Public methods ******************************************

constructor TIdSipTCPTransport.Create(const Port: Cardinal);
begin
  inherited Create(Port);

  Self.Clients    := TObjectList.Create(true);
  Self.ClientLock := TCriticalSection.Create;
  Self.Transport  := Self.ServerType.Create(nil);
  Self.SetPort(Port);
  Self.Transport.AddMessageListener(Self);
end;

destructor TIdSipTCPTransport.Destroy;
begin
  Self.Transport.Free;
  Self.ClientLock.Free;
  Self.Clients.Free;

  inherited Destroy;
end;

function TIdSipTCPTransport.GetTransportType: TIdSipTransportType;
begin
  Result := sttTCP;
end;

procedure TIdSipTCPTransport.Start;
begin
  Self.Transport.Active := true;
end;

procedure TIdSipTCPTransport.Stop;
begin
  Self.Transport.Active := false;
end;

//* TIdSipTCPTransport Protected methods ***************************************

function TIdSipTCPTransport.CreateClient: TIdSipTcpClient;
begin
  // Precondition: Self.ClientLock has been acquired.
  Result := TIdSipTcpClient.Create(nil);
  Result.OnFinished := Self.DoOnClientFinished;
  Result.OnResponse := Self.DoOnTcpResponse;
  Result.Timeout    := Self.Timeout;
end;

function TIdSipTCPTransport.GetPort: Cardinal;
begin
  Result := Self.Transport.DefaultPort;
end;

procedure TIdSipTCPTransport.SendRequest(const R: TIdSipRequest);
var
  Client: TIdSipTcpClient;
begin
  inherited SendRequest(R);

  Client := Self.AddClient;

  Client.Host    := R.RequestUri.Host;
  Client.Port    := R.RequestUri.Port;
  Client.Timeout := Self.Timeout;

  Client.Connect(Self.Timeout);
  Client.Send(R);
end;

procedure TIdSipTCPTransport.SendResponse(const R: TIdSipResponse);
begin
  inherited SendResponse(R);

  Self.Transport.SendResponse(R);
end;

function TIdSipTCPTransport.ServerType: TIdSipTcpServerClass;
begin
  Result := TIdSipTcpServer;
end;

procedure TIdSipTCPTransport.DestroyClient(Client: TIdSipTcpClient);
begin
  // Precondition: Self.ClientLock has been acquired.
  if Client.Connected then
    Client.Disconnect;

  Self.Clients.Remove(Client);
end;

function TIdSipTCPTransport.GetBindings: TIdSocketHandles;
begin
  Result := Self.Transport.Bindings;
end;

procedure TIdSipTCPTransport.SetPort(const Value: Cardinal);
var
  I: Integer;
begin
  Self.Transport.DefaultPort := Value;

  for I := 0 to Self.Bindings.Count - 1 do
    Self.Bindings[I].Port := Value;
end;

//* TIdSipTCPTransport Private methods *****************************************

function TIdSipTCPTransport.AddClient: TIdSipTcpClient;
begin
  Self.ClientLock.Acquire;
  try
    Result := Self.CreateClient;
    try
      Self.Clients.Add(Result);
    except
      // Remember, Self.Clients owns the object and will free it.
      Self.Clients.Remove(Result);
      Result := nil;

      raise;
    end;
  finally
    Self.ClientLock.Release;
  end;
end;

procedure TIdSipTCPTransport.DoOnClientFinished(Sender: TObject);
begin
  Self.RemoveClient(Sender as TIdSipTcpClient);
end;

procedure TIdSipTCPTransport.DoOnTcpResponse(Sender: TObject;
                                             const Response: TIdSipResponse;
                                             const ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.OnReceiveResponse(Response, ReceivedFrom);
end;

procedure TIdSipTCPTransport.RemoveClient(Client: TIdSipTcpClient);
begin
  Self.ClientLock.Acquire;
  try
    Self.DestroyClient(Client);
  finally
    Self.ClientLock.Release;
  end;
end;

//******************************************************************************
//* TIdSipTLSTransport                                                         *
//******************************************************************************
//* TIdSipTLSTransport Public methods ******************************************

function TIdSipTLSTransport.DefaultPort: Cardinal;
begin
  Result := IdPORT_SIPS;
end;

function TIdSipTLSTransport.GetTransportType: TIdSipTransportType;
begin
  Result := sttTLS;
end;

function TIdSipTLSTransport.IsSecure: Boolean;
begin
  Result := true;
end;

//* TIdSipTLSTransport Protected methods ***************************************

function TIdSipTLSTransport.CreateClient: TIdSipTcpClient;
begin
  Result := inherited CreateClient;

  Result.IOHandler := TIdSSLIOHandlerSocket.Create(nil);
end;

procedure TIdSipTLSTransport.DestroyClient(Client: TIdSipTcpClient);
begin
  Client.IOHandler.Free;

  inherited DestroyClient(Client);
end;

function TIdSipTLSTransport.ServerType: TIdSipTcpServerClass;
begin
  Result := TIdSipTlsServer;
end;

//* TIdSipTLSTransport Private methods *****************************************

function TIdSipTLSTransport.GetOnGetPassword: TPasswordEvent;
begin
  Result := Self.TLS.OnGetPassword;
end;

function TIdSipTLSTransport.GetRootCertificate: TFileName;
begin
  Result := Self.TLS.RootCertificate;
end;

function TIdSipTLSTransport.GetServerCertificate: TFileName;
begin
  Result := Self.TLS.ServerCertificate;
end;

function TIdSipTLSTransport.GetServerKey: TFileName;
begin
  Result := Self.TLS.ServerKey;
end;

function TIdSipTLSTransport.TLS: TIdSipTlsServer;
begin
  Result := Self.Transport as TIdSipTlsServer;
end;

procedure TIdSipTLSTransport.SetOnGetPassword(const Value: TPasswordEvent);
begin
  Self.TLS.OnGetPassword := Value;
end;

procedure TIdSipTLSTransport.SetRootCertificate(const Value: TFileName);
begin
  Self.TLS.RootCertificate := Value;
end;

procedure TIdSipTLSTransport.SetServerCertificate(const Value: TFileName);
begin
  Self.TLS.ServerCertificate := Value;
end;

procedure TIdSipTLSTransport.SetServerKey(const Value: TFileName);
begin
  Self.TLS.ServerKey := Value;
end;

//******************************************************************************
//* TIdSipSCTPTransport                                                        *
//******************************************************************************
//* TIdSipSCTPTransport Public methods *****************************************

function TIdSipSCTPTransport.GetTransportType: TIdSipTransportType;
begin
  Result := sttSCTP;
end;

//******************************************************************************
//* TIdServerCleanupThread                                                     *
//******************************************************************************
//* TIdServerCleanupThread Public methods **************************************

constructor TIdServerCleanupThread.Create(Owner: TIdSipUDPTransport);
begin
  inherited Create(true);

  Self.ListLock := TCriticalSection.Create;

  Self.fOwner := Owner;
  Self.PollTime := Self.DefaultPollTime;

  Self.TerminatedServers := TObjectList.Create(false);
end;

destructor TIdServerCleanupThread.Destroy;
begin
  Self.TerminatedServers.Free;
  Self.ListLock.Free;

  inherited Destroy;
end;

function TIdServerCleanupThread.DefaultPollTime: Cardinal;
begin
  Result := 1000;
end;

procedure TIdServerCleanupThread.TerminateClient(Server: TIdSipUdpClient);
begin
  Self.ListLock.Acquire;
  try
    Self.TerminatedServers.Add(Server);
  finally
    Self.ListLock.Release;
  end;
end;

procedure TIdServerCleanupThread.Run;
begin
  while not Terminated do begin
    IdGlobal.Sleep(Self.PollTime);

    Self.CleanOutTerminatedClients;
  end;
end;

//* TIdServerCleanupThread Private methods *************************************

procedure TIdServerCleanupThread.CleanOutTerminatedClients;
var
  I: Integer;
begin
  Self.ListLock.Acquire;
  try
    for I := 0 to Self.TerminatedServers.Count - 1 do
      Self.fOwner.RemoveClient(Self.TerminatedServers[I] as TIdSipUdpClient);

    Self.TerminatedServers.Clear;
  finally
    Self.ListLock.Release;
  end;
end;

//******************************************************************************
//* TIdSipUDPTransport                                                         *
//******************************************************************************
//* TIdSipUDPTransport Public methods ******************************************

constructor TIdSipUDPTransport.Create(const Port: Cardinal);
begin
  inherited Create(Port);

  Self.ClientCleaner := TIdServerCleanupThread.Create(Self);
  Self.Clients       := TObjectList.Create(true);
  Self.ClientLock    := TCriticalSection.Create;

  Self.Transport := TIdSipUdpServer.Create(nil);
  Self.SetPort(Port);
  Self.Transport.AddMessageListener(Self);
  Self.Transport.ThreadedEvent := true;
end;

destructor TIdSipUDPTransport.Destroy;
begin
  Self.ClientCleaner.TerminateAndWaitFor;

  Self.Transport.Free;
  Self.ClientLock.Free;
  Self.Clients.Free;
  Self.ClientCleaner.Free;

  inherited Destroy;
end;

function TIdSipUDPTransport.GetTransportType: TIdSipTransportType;
begin
  Result := sttUDP;
end;

function TIdSipUDPTransport.IsReliable: Boolean;
begin
  Result := false;
end;

procedure TIdSipUDPTransport.Start;
begin
  Self.Transport.Active := true;
  Self.ClientCleaner.Start;
end;

procedure TIdSipUDPTransport.Stop;
begin
  Self.Transport.Active := false;
  Self.ClientCleaner.Stop;
end;

//* TIdSipUDPTransport Protected methods ***************************************

function TIdSipUDPTransport.GetBindings: TIdSocketHandles;
begin
  Result := Self.Transport.Bindings;
end;

function TIdSipUDPTransport.GetPort: Cardinal;
begin
  Result := Self.Transport.DefaultPort;
end;

procedure TIdSipUDPTransport.OnReceiveRequest(const Request: TIdSipRequest;
                                              const ReceivedFrom: TIdSipConnectionBindings);
begin
  // RFC 3581 section 4
  if Request.LastHop.HasRPort then begin
    if not Request.LastHop.HasReceived then
      Request.LastHop.Received := ReceivedFrom.PeerIP;

    Request.LastHop.RPort := ReceivedFrom.PeerPort;
  end;

  inherited OnReceiveRequest(Request, ReceivedFrom);
end;

procedure TIdSipUDPTransport.SendRequest(const R: TIdSipRequest);
var
  Client: TIdSipUdpServer;
begin
  inherited SendRequest(R);

  if not R.RequiresResponse then
    Self.SendRequestNotExpectingResponses(R)
  else begin
    // Really, this we expect this behaviour from an RFC 3581 compliant agent.
    // X-Lite behaves in a braindead fashion though, and assumes blithely that
    // anything that calls it implements RFC 3581.
    Client := Self.AddClient;

    Client.Active := true;
    Client.Send(R.RequestUri.Host,
                R.RequestUri.Port,
                R.AsString);
  end;
end;

procedure TIdSipUDPTransport.SendResponse(const R: TIdSipResponse);
var
  Host: String;
  Port: Cardinal;
begin
  inherited SendResponse(R);

  if R.LastHop.HasMaddr then begin
    Host := R.LastHop.Maddr;
    Port := R.LastHop.Port;
  end
  else if R.LastHop.HasRport then begin
    // cf RFC 3581 section 4.
    // TODO: this isn't quite right. We have to send the response
    // from the ip/port that the request was received on.
    Host := R.LastHop.Received;
    Port := R.LastHop.RPort;
  end
  else if R.LastHop.HasReceived then begin
    Host := R.LastHop.Received;
    Port := R.LastHop.Port;
  end
  else begin
    Host := R.LastHop.SentBy;
    Port := R.LastHop.Port;
  end;
{
  if R.LastHop.HasMaddr then begin
    Host := R.LastHop.Maddr;
    Port := R.LastHop.Port;
  end
  else if R.LastHop.HasReceived then begin
    Host := R.LastHop.Received;

    // cf RFC 3581 section 4.
    // TODO: this isn't quite right. We have to send the response
    // from the ip/port that the request was received on.
    if R.LastHop.HasRport then
      Port := R.LastHop.RPort
    else
      Port := R.LastHop.Port;
  end
  else begin
    Host := R.LastHop.SentBy;
    Port := R.LastHop.Port;
  end;
}
  Self.Transport.Send(Host, Port, R.AsString);
end;

procedure TIdSipUDPTransport.SetPort(const Value: Cardinal);
var
  I: Integer;
begin
  Self.Transport.DefaultPort := Value;

  for I := 0 to Self.Bindings.Count - 1 do
    Self.Bindings[I].Port := Value;
end;

//* TIdSipUDPTransport Private methods *****************************************

function TIdSipUDPTransport.AddClient: TIdSipUdpClient;
begin
  Self.ClientLock.Acquire;
  try
    if not Self.ClientCleaner.Stopped then
      Self.ClientCleaner.Start;

    Result := TIdSipUdpClient.Create(nil);
    try
      Self.Clients.Add(Result);
      Result.AddMessageListener(Self);
      Result.DefaultPort := 0;
      Result.OnFinished := Self.DoOnClientFinished;
    except
      // Remember, Self.Clients owns the object and will free it.
      Self.Clients.Remove(Result);
      Result := nil;

      raise;
    end;
  finally
    Self.ClientLock.Release;
  end;
end;

procedure TIdSipUDPTransport.DoOnClientFinished(Sender: TObject);
begin
  Self.ClientCleaner.TerminateClient(Sender as TIdSipUdpClient);
end;

function TIdSipUDPTransport.GetCleanerThreadPollTime: Cardinal;
begin
  Result := Self.ClientCleaner.PollTime;
end;

procedure TIdSipUDPTransport.RemoveClient(Client: TIdSipUdpClient);
begin
  Self.ClientLock.Acquire;
  try
    Self.Clients.Remove(Client);

    if (Self.Clients.Count = 0) then
      Self.ClientCleaner.Stop;
  finally
    Self.ClientLock.Release;
  end;
end;

procedure TIdSipUDPTransport.SendRequestNotExpectingResponses(const R: TIdSipRequest);
var
  Client: TIdUdpClient;
begin
  Client := TIdUdpClient.Create(nil);
  try
    Client.Host := R.RequestUri.Host;
    Client.Port := R.RequestUri.Port;
    Client.Send(R.AsString);
  finally
    Client.Free;
  end;
end;

procedure TIdSipUDPTransport.SetCleanerThreadPollTime(const Value: Cardinal);
begin
  Self.ClientCleaner.PollTime := Value;
end;

end.
