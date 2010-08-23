{
  (c) 2006 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipTcpTransport;

interface

uses
  Classes, Contnrs, IdBaseThread, IdConnectionBindings, IdSipMessage,
  IdRegisteredObject, IdSimpleParser, IdSipLocation, IdSipTransport,
  IdSocketHandle, IdTCPConnection, IdTCPServer, IdThreadableTcpClient,
  IdTimerQueue, Messages, SyncObjs, SysUtils, Winsock;

const
  CM_SOCKETMESSAGE = WM_USER + $0001;

type
  // Copied verbatim from ScktComp
  TCMSocketMessage = record
    Msg: Cardinal;
    Socket: TSocket;
    SelectEvent: Word;
    SelectError: Word;
    Result: Longint;
  end;

  TIdSipConnectionTableLock = class;
  TIdSipTcpServer = class;

  // I implement the Transmission Control Protocol (RFC 793) connections for the
  // SIP stack.
  TIdSipTCPTransport = class(TIdSipTransport)
  private
    // Keep a reference of created clients so that we can free them at shutdown
    // time.
    fConnectionTimeout: Cardinal;

    Active:       Boolean;
    fListenQueue: Integer;
    ConnectedSockets: TObjectList;
    Server: TIdSipTcpServer;

    procedure ScheduleExceptionNotification(E: Exception;
                                            Msg: TIdSipMessage;
                                            Dest: TIdConnectionBindings);
    procedure StartWinsock;
  protected
    procedure DestroyServer; override;
    procedure InstantiateServer; override;
    procedure SendMessage(Msg: TIdSipMessage;
                          Dest: TIdConnectionBindings); override;
    procedure SetConserveConnections(Value: Boolean); override;
    procedure SetTimeout(Value: Cardinal); override;
    procedure SetTimer(Value: TIdTimerQueue); override;
  public
    class function GetTransportType: String; override;
    class function SrvPrefix: String; override;

    constructor Create; override;
    destructor  Destroy; override;

    function  IsRunning: Boolean; override;
    procedure Start; override;
    procedure Stop; override;

    property ConnectionTimeout: Cardinal read fConnectionTimeout write fConnectionTimeout;
    property ListenQueue:       Integer  read fListenQueue write fListenQueue;
  end;

  // Given a TCP connection, I read messages off the connection and dispatch
  // them to a TimerQueue until I die or something severs the connection.
  TIdSipTcpMessageReader = class(TObject)
  private
    fReadTimeout:        Integer;
    fTimer:              TIdTimerQueue;
    fTransportID:        TRegisteredObjectID;
    fTransportType:      String;

    procedure AddConnection(Connection: TIdTCPConnection;
                            Request: TIdSipRequest);
    procedure NotifyOfConnectionInTimerContext(Connection: TIdTCPConnection;
                                               Request: TIdSipRequest);
    procedure NotifyOfDisconnectionInTimerContext(Connection: TIdTCPConnection);
    procedure ReadBodyInto(Connection: TIdTCPConnection;
                           Msg: TIdSipMessage;
                           Dest: TStringStream);
    procedure ReadHeaders(Connection: TIdTCPConnection;
                          Dest: TStringStream);
    function  ReadMessage(Connection: TIdTCPConnection;
                          ConserveConnections: Boolean;
                          var AbnormalTermination: Boolean): TIdSipMessage;
    procedure ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                           Binding: TIdConnectionBindings);
    procedure ReturnInternalServerError(Connection: TIdTCPConnection;
                                        const Reason: String);
  public
    procedure NotifyOfException(ExceptionType: ExceptClass;
                                const Reason: String);
    procedure ReadOneMessage(Connection: TIdTCPConnection;
                             ConserveConnections: Boolean);
    procedure ReadMessages(Connection: TIdTCPConnection;
                           ConserveConnections: Boolean);

    property ReadTimeout:   Integer             read fReadTimeout write fReadTimeout;
    property Timer:         TIdTimerQueue       read fTimer write fTimer;
    property TransportID:   TRegisteredObjectID read fTransportID write fTransportID;
    property TransportType: String              read fTransportType write fTransportType;
  end;

  TIdSipConnection = class(TObject)
  private
    fBinding:       TIdConnectionBindings;
    fKeepAlive:     Boolean; // Set this BEFORE you Connect() or Bind()!
    InternalBuffer: String;
    MessageQueue:   THandle;
    SendBuffer:     String;

    procedure Capture(Dest: TStringStream; Delimiter: String);
    procedure CMSocketMessage(var Message: TCMSocketMessage); message CM_SOCKETMESSAGE;
    function  FirstN(ByteCount: Integer): String;
    function  InternalBufferLength: Integer;
    procedure PopulateSockAddr(Location: TIdSipLocation; var Addr: TSockAddrIn);
    function  ProtocolType(Address: String): Integer;
    procedure ReadBodyInto(Msg: TIdSipMessage;
                           Dest: TStringStream);
    procedure ReadHeaders(Dest: TStringStream);
    function  ReadLn: String;
    procedure ReadStream(Dest: TStringStream; ByteCount: Integer);
    function  Recv: String;
    function  SendBufferLength: Integer;
    procedure Select;
    procedure WndProc(var Message: TMessage);
  protected
    fConnected: Boolean;
    fHandle:    TSocket;

    procedure OnAccept; virtual;
    procedure OnConnect; virtual;
    procedure OnClose; virtual;
    procedure OnDisconnect; virtual;
    procedure OnOOB; virtual;
    procedure OnRead; virtual;
    procedure OnWrite; virtual;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Close; virtual;
    function  Connected: Boolean; virtual;
    function  IsOutbound: Boolean; virtual;
    function  PartialSend: Boolean;
    function  Receive(var AbnormalTermination: Boolean): TIdSipMessage;
    procedure Send(Msg: TIdSipMessage);

    property Binding:   TIdConnectionBindings read fBinding;
    property Handle:    TSocket               read fHandle;
    property KeepAlive: Boolean               read fKeepAlive write fKeepAlive;
  end;

  // I am a socket that connects to another machine.
  TIdSipClientConnection = class(TIdSipConnection)
  private
    procedure SetLocalBinding(Addr: TSockAddr; Binding: TIdConnectionBindings);
    procedure SetPeerBinding(Addr: TSockAddr; Binding: TIdConnectionBindings);
  public
    procedure Connect(PeerIP: String; PeerPort: Cardinal); overload;
    procedure Connect(Location: TIdSipLocation); overload;
    function  IsOutbound: Boolean; override;
  end;

  // I am a socket on the server side of a connection.
  TIdSipServerConnection = class(TIdSipConnection)
  public
    constructor Create(Handle: TSocket; Binding: TIdConnectionBindings);
  end;

  // I am a socket that listens on a port. When I Accept()
  // a connection, I spawn a TIdSipServerConnection.
  TIdSipListeningConnection = class(TIdSipConnection)
  private
    ServerLocation: TIdSipLocation;

    function  IPVersion(Protocol: Integer): TIdIPVersion;
  protected
    procedure OnAccept; override;
  public
    destructor Destroy; override;

    function  Accept: TIdSipServerConnection;
    procedure Bind(IP: String; Port: Cardinal); overload;
    procedure Bind(Location: TIdSipLocation); overload;
    procedure Listen(QueueLength: Cardinal);
  end;

  // I relate a request with a TCP connection. I store a COPY of a request
  // while storing a REFERENCE to a connection. Transports construct requests
  // and so bear responsibility for destroying them, and I need to remember
  // these requests.
  // I represent a (possibly) deferred handling of an exception by using a
  // TNotifyEvent.
  //
  // Binding contains the binding details of Connection. Connection already has
  // all this information, but if Connection has to disconnect, we would not
  // know the binding information anymore. Thus, Binding caches this
  // information. This has several consequences: Connection must be connected
  // before you create me, and should Connection disconnect and reconnect, my
  // binding information will be out of date. (The Connection has an
  // OnDisconnect notification, but no OnConnection notification.) The latter
  // consequence shouldn't matter, since the TCP transport will remove me from
  // my table when Connection disconnects.
  //
  // Further note: should a client connect and then disconnect very quickly,
  // we might receive a well-formed message but not know where that message came
  // from, because the Connection's no longer connected. Thus, I provide an
  // additional constructor that explicitly allows to store the connection's
  // binding details. See TIdSipTcpConnectionOpenWait and
  // TIdSipTCPTransport.AddConnection(Closure: TIdSipTcpConnectionOpenWait).
  TIdSipConnectionTableEntry = class(TObject)
  private
    fConnection: TIdSipConnection;
    fRequest:    TIdSipRequest;
  public
    constructor Create(Connection: TIdSipConnection;
                       Request: TIdSipRequest); overload;
    destructor  Destroy; override;

    property Connection: TIdSipConnection read fConnection;
    property Request:    TIdSipRequest    read fRequest;
  end;

  // I represent a table containing ordered pairs of (TCP connection, Request).
  // If a transport wishes to send a request or response I match the outbound
  // message against my table of tuples, and return the appropriate TCP
  // connection. My users bear responsibility for informing me when TCP
  // connections appear and disappear.
  TIdSipConnectionTable = class(TObject)
  private
    List: TObjectList;

    function EntryAt(Index: Integer): TIdSipConnectionTableEntry;
    function FindEntry(Connection: TIdSipConnection;
                       Request: TIdSipRequest): TIdSipConnectionTableEntry;
  public
    constructor Create;
    destructor  Destroy; override;

    function  Add(Connection: TIdSipConnection;
                  Request:  TIdSipRequest): TIdSipConnectionTableEntry;
    function  AnyConnectionTo(Destination: TIdSipLocation): TIdSipConnection;
    function  ConnectionEntry(Connection: TIdSipConnection): TIdSipConnectionTableEntry;
    function  ConnectionFor(Msg: TIdSipMessage): TIdSipConnection; overload;
    function  ConnectionFor(S: TSocket): TIdSipConnection; overload;
    function  ConnectionFor(Destination: TIdConnectionBindings): TIdSipConnection; overload;
    function  Count: Integer;
    procedure Remove(Connection: TIdSipConnection);

    property Connection[Index: Integer]: TIdSipConnectionTableEntry read EntryAt; default;
  end;

  TIdSipConnectionTableLock = class(TObject)
  private
    Table: TIdSipConnectionTable;
    Lock:  TCriticalSection;
  public
    constructor Create;
    destructor  Destroy; override;

    function  LockList: TIdSipConnectionTable;
    procedure UnlockList;
  end;

  // I expose the internal state of a TIdSipTcpServer. TIdSipTcpServerWait
  // instances use me.
  IIdSipTcpServerInternalState = interface(IInterface)
  ['{218ED566-703C-42A1-80AF-0901B2F0FCF3}']
    procedure SendMessage(Msg: TIdSipMessage; Dest: TIdSipLocation);
    procedure SetListenQueue(NewValue: Integer);
    procedure StartListening;
  end;

  // My subclasses manipulate the state of my Server through the asynchronous
  // mechanism of a TIdWait. Because my subclasses and I require access to
  // parts of my Server that we do not, in general, wish to expose in the
  // public interface, my subclasses and I access the PRIVATE state of my Server
  // through a IIdSipTcpServerInternalState reference.
  TIdSipTcpServerWait = class(TIdWait)
  private
    // DO NOT make this point to ANYTHING other than the server that will
    // execute this Wait!
    fServer: IIdSipTcpServerInternalState;
  public
    property Server: IIdSipTcpServerInternalState read fServer write fServer;
  end;

  TIdSipWriteToConnectionWait = class(TIdSipTcpServerWait)
  private
    fDestination: TIdSipLocation;
    fMsg:         TIdSipMessage;

    procedure SetDestination(Value: TIdSipLocation);
    procedure SetMsg(Value: TIdSipMessage);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Trigger; override;

    property Destination: TIdSipLocation  read fDestination write SetDestination;
    property Msg:         TIdSipMessage   read fMsg write SetMsg;
  end;

  // A TIdSipTcpServer uses me to start itself in order to bind to sockets in
  // its own execution context.
  TIdSipTcpServerStartWait = class(TIdSipTcpServerWait)
  public
    procedure Trigger; override;
  end;

  // Use me to set the listen queue lengths for any new server sockets.
  TIdSipTcpServerSetListenQueueWait = class(TIdSipTcpServerWait)
  private
    fListenQueue: Integer;
  public
    procedure Trigger; override;

    property ListenQueue: Integer read fListenQueue write fListenQueue;
  end;

  // I represent a TimerQueue specialised to handle both inbound and outbound
  // TCP connections. Use TIdSipTcpServerWait subclasses to manipulate my
  // state.
  TIdSipTcpServer = class(TIdThreadedTimerQueue,
                          IIdSipTcpServerInternalState)
  private
    Bindings:         TIdSipLocations;
    Clients:          TObjectList;
    ConnectionMap:    TIdSipConnectionTable;
    ListeningSockets: TObjectList;
    ListenQueue:      Integer;

    // IInterface support: NOT REFERENCE COUNTED
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    procedure Accept(Connection: TIdSipConnection);
    function  BindingCount: Integer;
    procedure Disconnect(Connection: TIdSipConnection);
    function  FirstServerPortFor(IPAddress: String): Cardinal;
    procedure NotifyOfReceivedMessage(M: TIdSipMessage;
                                      Connection: TIdConnectionBindings);
    procedure NotifyOfSentMessage(M: TIdSipMessage;
                                  Connection: TIdConnectionBindings);
    procedure ReadMessageFrom(Conn: TIdSipConnection);
    procedure SendMessage(Msg: TIdSipMessage; Dest: TIdSipLocation);
    procedure SendMessageTo(Msg: TIdSipMessage;
                            Dest: TIdSipLocation);
    procedure SetListenQueue(NewValue: Integer);
    procedure StartListening;
    procedure WriteMessageTo(Msg: TIdSipMessage;
                             Connection: TIdSipConnection); overload;
  public
    constructor Create(Bindings: TIdSipLocations); reintroduce;
    destructor  Destroy; override;

    procedure Resume; override;
  end;

  // Note that the Timeout property determines the maximum length of time to
  // wait for the next line of data to arrive.
  // Through my MessageReader, I read messages from the network and feed them to
  // a TimerQueue. That, and I send messages to the network.
  TIdSipTcpClient = class(TIdThreadableTcpClient)
  private
    MessageReader: TIdSipTcpMessageReader;

    function  GetTransportID: TRegisteredObjectID;
    function  GetTransportType: String;
    procedure SetTransportID(const Value: TRegisteredObjectID);
    procedure SetTransportType(Value: String);
  protected
    function  GetTimer: TIdTimerQueue; override;
    procedure SetTimer(Value: TIdTimerQueue); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  Protocol: String; override;
    procedure ReceiveMessages; override;
    procedure Send(Msg: TIdSipMessage);

    property TransportID:   TRegisteredObjectID read GetTransportID write SetTransportID;
    property TransportType: String              read GetTransportType write SetTransportType;
  end;

  EIdSocketException = class(Exception)
  public
    constructor Create(FunctionName: String; WSError: Integer);
  end;

// Winsock extensions:
const
  AF_INET6 = 28;
  PF_INET6 = AF_INET6;

implementation

uses
  IdException, IdIndyUtils, IdSipDns, IdTcpClient, Math, MessageQueues, Windows;

//******************************************************************************
//* Unit Private functions/procedures                                          *
//******************************************************************************

function CreateWindowHandle(WndProc: TWndMethod): HWND;
begin
  Result := MessageQueues.AllocateHWnd(WndProc)
end;

procedure DestroyWindowHandle(H: HWND);
begin
  MessageQueues.DeallocateHWnd(H);
end;

procedure SetSocketOption(S: TSocket; Option: Cardinal; NewValue: Boolean);
var
  RC:    Integer;
  Value: Byte;
begin
  // Note: Boolean is, under the hood, a 32 bit value. It's possible to construct
  // a B: Boolean such that Cardinal(B) <> 1 and Cardinal(B) <> 0 (for instance,
  // the uninitialised result of a Boolean function).

  if NewValue then
    Value := 1
  else
    Value := 0;

  RC := setsockopt(S, SOL_SOCKET, Option, @Value, Sizeof(Value));

  if (RC <> 0) then
    raise EIdSocketException.Create('setsockopt', WSAGetLastError);
end;

procedure SetSocketKeepAlive(S: TSocket; KeepAlive: Boolean);
begin
  SetSocketOption(S, SO_KEEPALIVE, KeepAlive);
end;

procedure SetSocketReuseAddr(S: TSocket; ReuseAddress: Boolean);
begin
  SetSocketOption(S, SO_REUSEADDR, ReuseAddress);
end;

function TranslateError(Err: Integer): String;
begin
  case Err of
    WSANOTINITIALISED:  Result := 'A successful WSAStartup call must occur before using this function.';
    WSAEACCES:          Result := 'An attempt to bind a datagram socket to the '
                                + 'broadcast address failed because the '
                                + 'setsockopt option SO_BROADCAST is not enabled.';
    WSAEADDRINUSE:      Result := 'A process on the computer is already bound '
                                + 'to the same fully-qualified address and the '
                                + 'socket has not been marked to allow address '
                                + 'reuse with SO_REUSEADDR. For example, the IP '
                                + 'address and port specified in the name '
                                + 'parameter are already bound to another '
                                + 'socket being used by another application. '
                                + 'For more information, see the SO_REUSEADDR '
                                + 'socket option in the SOL_SOCKET Socket '
                                + 'Options reference, Using SO_REUSEADDR and '
                                + 'SO_EXCLUSIVEADDRUSE, and SO_EXCLUSIVEADDRUSE.';
    WSAEADDRNOTAVAIL:   Result := 'The specified address pointed to by the name '
                                + 'parameter is not a valid local IP address on this computer.';
    WSAEAFNOSUPPORT:    Result := 'The specified address family is not '
                                + 'supported. For example, an application tried '
                                + 'to create a socket for the AF_IRDA address '
                                + 'family but an infrared adapter and device '
                                + 'driver is not installed on the local computer.';
    WSAECONNABORTED:    Result := 'The virtual circuit was terminated due to a '
                                + 'time-out or other failure. The application '
                                + 'should close the socket as it is no longer '
                                + 'usable.';
    WSAECONNREFUSED:    Result := 'The attempt to connect was rejected.';
    WSAECONNRESET:      Result := 'An incoming connection was indicated, but was '
                                + 'subsequently terminated by the remote peer '
                                + 'prior to accepting the call.';
    WSAEFAULT:          Result := 'The name or namelen parameter is not a valid '
                                + 'part of the user address space, the namelen '
                                + 'parameter is too small, the name parameter '
                                + 'contains an incorrect address format for the '
                                + 'associated address family, or the first two '
                                + 'bytes of the memory block specified by name '
                                + 'does not match the address family associated '
                                + 'with the socket descriptor s.';
    WSAEHOSTUNREACH:    Result := 'The remote host cannot be reached from this host at this time.';
    WSAEINPROGRESS:     Result := 'A blocking Windows Sockets 1.1 call is in '
                                + 'progress, or the service provider is still '
                                + 'processing a callback function.';
    WSAEINTR:           Result := 'A blocking Windows Sockets 1.1 call was canceled through WSACancelBlockingCall.';
    WSAEINVAL:          Result := 'An invalid argument was supplied. This error '
                                + 'is returned if the af parameter is set to '
                                + 'AF_UNSPEC and the type and protocol parameter '
                                + 'are unspecified.';
    WSAEISCONN:         Result := 'The socket is already connected.';
    WSAEMFILE:          Result := 'No more socket descriptors are available.';
    WSAEMSGSIZE:        Result := 'The socket is message oriented, and the message is larger than the maximum supported by the underlying transport.';
    WSAENETDOWN:        Result := 'The network subsystem or the associated '
                                + 'service provider has failed.';
    WSAENETRESET:       Result := 'The connection has timed out when SO_KEEPALIVE is set.';
    WSAENETUNREACH:     Result := 'The network cannot be reached from this host at this time.';
    WSAENOBUFS:         Result := 'No buffer space is available. The socket cannot be created.';
    WSAENOPROTOOPT:     Result := 'The option is unknown or unsupported for the '
                                + 'specified provider or socket (see SO_GROUP_PRIORITY limitations).';
    WSAENOTCONN:        Result := 'The connection has been reset when SO_KEEPALIVE is set.';
    WSAENOTSOCK:        Result := 'The descriptor in the s parameter is not a socket.';
    WSAEOPNOTSUPP:      Result := 'The referenced socket is not of a type that supports this operation.';
    WSAEPROTONOSUPPORT: Result := 'The specified protocol is not supported.';
    WSAEPROTOTYPE:      Result := 'The specified protocol is the wrong type for this socket.';
    WSAESHUTDOWN:       Result := 'The socket has been shut down; it is not '
                                + 'possible to send on a socket after shutdown '
                                + 'has been invoked with how set to SD_SEND or '
                                + 'SD_BOTH.';
    WSAESOCKTNOSUPPORT: Result := 'The specified socket type is not supported in this address family.';
    WSAETIMEDOUT:       Result := 'The connection has been dropped, because of '
                                + 'a network failure or because the system on '
                                + 'the other end went down without notice.';
    WSAEWOULDBLOCK:     Result := 'The socket is marked as nonblocking and no connections are present to be accepted.';
  else
    Result := 'Unknown error';
  end;
end;

function TranslateEvent(Event: Integer): String;
begin
  case Event of
    FD_READ:    Result := 'FD_READ';
    FD_WRITE:   Result := 'FD_WRITE';
    FD_OOB:     Result := 'FD_OOB';
    FD_ACCEPT:  Result := 'FD_ACCEPT';
    FD_CONNECT: Result := 'FD_CONNECT';
    FD_CLOSE:   Result := 'FD_CLOSE';
  else
    Result := Format('Unknown event (%d)', [Event]);
  end;
end;

//******************************************************************************
//* TIdSipTCPTransport                                                         *
//******************************************************************************
//* TIdSipTCPTransport Public methods ******************************************

class function TIdSipTCPTransport.GetTransportType: String;
begin
  Result := TcpTransport;
end;

class function TIdSipTCPTransport.SrvPrefix: String;
begin
  Result := SrvTcpPrefix;
end;

constructor TIdSipTCPTransport.Create;
begin
  Self.ConnectedSockets := TObjectList.Create(true);

  inherited Create;

  Self.StartWinsock;

  Self.ConnectionTimeout := FiveSeconds;

  // Some sensible defaults
  Self.ListenQueue := 5;
end;

destructor TIdSipTCPTransport.Destroy;
begin
  Self.ConnectedSockets.Free;

  inherited Destroy;
end;

function TIdSipTCPTransport.IsRunning: Boolean;
begin
  Result := Self.Active;
end;

procedure TIdSipTCPTransport.Start;
begin
  inherited Start;

  if Self.Active then Exit;

  // For every binding in Binding, start listening on a socket, or start none.

  Self.Server := TIdSipTcpServer.Create(Self.Bindings);
  Self.Active := true;
end;

procedure TIdSipTCPTransport.Stop;
const
  FiveSeconds = 5000; // milliseconds
var
  E:              TEvent;
  FailedShutdown: Boolean;
begin
  if not Self.Active then Exit;

  E := TSimpleEvent.Create;
  try
    Self.Server.TerminateAndWaitFor(E);
    FailedShutdown :=  wrSignaled <> E.WaitFor(FiveSeconds);

    if FailedShutdown then
      raise EIdSipFailedTransportShutdown.Create(Self);
  finally
    Self.Active := false;

    // Note that if FailedShutdown is true, Self.Server might well (if not hung)
    // raise an exception when it finally tries to notify that it's terminated.
    // Since Self.Server's finished at this point, the exception's harmless.
    E.Free;
  end;
end;

//* TIdSipTCPTransport Protected methods ***************************************

procedure TIdSipTCPTransport.DestroyServer;
begin
  // Starting and stopping creates/destroys the server.
end;

procedure TIdSipTCPTransport.InstantiateServer;
begin
  // Starting and stopping creates/destroys the server.
end;

procedure TIdSipTCPTransport.SendMessage(Msg: TIdSipMessage;
                                         Dest: TIdConnectionBindings);
var
  L:    TIdSipLocation;
  Wait: TIdSipWriteToConnectionWait;
begin
  Wait := TIdSipWriteToConnectionWait.Create;
  Wait.Msg    := Msg;
  Wait.Server := Self.Server;

  L := TIdSipLocation.CreatePeerLocation(Dest);
  try
    Wait.Destination := L;
  finally
    L.Free;
  end;

  Self.Server.AddEvent(TriggerImmediately, Wait);
end;

procedure TIdSipTCPTransport.SetConserveConnections(Value: Boolean);
{
var
  Connections: TIdSipConnectionTable;
  I:           Integer;
}
begin
  inherited SetConserveConnections(Value);
{
  Self.Transport.ConserveConnections := Value;

  Connections := Self.ConnectionMap.LockList;
  try
    for I := 0 to Connections.Count - 1 do
      SetSocketKeepAlive(Connections[I].Socket, Value);
  finally
    Self.ConnectionMap.UnlockList;
  end;
}
end;

procedure TIdSipTCPTransport.SetTimeout(Value: Cardinal);
{
var
  Connections: TIdSipConnectionTable;
  I:           Integer;
}
begin
  inherited SetTimeout(Value);
{
  Self.Transport.ReadTimeout       := Value;
  Self.Transport.ConnectionTimeout := Value;

  Connections := Self.ConnectionMap.LockList;
  try
    for I := 0 to Connections.Count - 1 do
      Connections[I].Connection.ReadTimeout := Value;
  finally
    Self.ConnectionMap.UnlockList;
  end;
}
end;

procedure TIdSipTCPTransport.SetTimer(Value: TIdTimerQueue);
begin
  inherited SetTimer(Value);

//  Self.Transport.Timer := Value;
end;

//* TIdSipTCPTransport Protected methods ***************************************

procedure TIdSipTCPTransport.ScheduleExceptionNotification(E: Exception;
                                                           Msg: TIdSipMessage;
                                                           Dest: TIdConnectionBindings);
var
  Wait: TIdSipMessageExceptionWait;
begin
  Wait := TIdSipMessageExceptionWait.Create;
  Wait.ExceptionType    := ExceptClass(E.ClassType);
  Wait.ExceptionMessage := E.Message;
  Wait.FailedMessage    := Msg.Copy;
  Wait.Reason           := Format('Something went wrong connecting to %s:%d/%s', [Dest.PeerIP, Dest.PeerPort, Dest.Transport]);
  Wait.TransportID      := Self.ID;

  Self.Timer.AddEvent(TriggerImmediately, Wait);
end;

procedure TIdSipTCPTransport.StartWinsock;
begin
  // For the moment, do _nothing_, and let Indy take care of starting/cleaning
  // up Winsock.
{
var
  Info: TWSAData;
  RC:   Integer;
begin

  RC := WSAStartUp($0202, Info);

  if (RC <> 0) then begin
    raise Exception.Create(Format('Fatal: failed to start Winsock 2.2: %s (%d)', [SysErrorMessage(RC), RC]));
  end;
}
end;

//******************************************************************************
//* TIdSipTcpMessageReader                                                     *
//******************************************************************************
//* TIdSipTcpMessageReader Public methods **************************************

procedure TIdSipTcpMessageReader.NotifyOfException(ExceptionType: ExceptClass;
                                                   const Reason: String);
var
  Wait: TIdSipMessageExceptionWait;
begin
  Wait := TIdSipMessageExceptionWait.Create;
  Wait.ExceptionType := ExceptionType;
  Wait.ExceptionMessage := Reason;
  Wait.Reason           := Reason;

  Self.Timer.AddEvent(TriggerImmediately, Wait);
end;

procedure TIdSipTcpMessageReader.ReadOneMessage(Connection: TIdTCPConnection;
                                                ConserveConnections: Boolean);
var
  AbnormalTermination: Boolean;
  Msg:                 TIdSipMessage;
  ReceivedFrom:        TIdConnectionBindings;
begin
  AbnormalTermination := false;

  Connection.ReadTimeout := Self.ReadTimeout;

  ReceivedFrom := TIdConnectionBindings.Create;
  try
    if Connection.Connected then begin
      try
        ReceivedFrom.LocalIP   := Connection.Socket.Binding.IP;
        ReceivedFrom.LocalPort := Connection.Socket.Binding.Port;
        ReceivedFrom.PeerIP    := Connection.Socket.Binding.PeerIP;
        ReceivedFrom.PeerPort  := Connection.Socket.Binding.PeerPort;
        ReceivedFrom.Transport := Self.TransportType;
      except
        on E: Exception do begin
          Self.NotifyOfException(ExceptClass(E.ClassType), Format('%s while copying binding information for a SIP message off a TCP connection', [E.Message]));
          AbnormalTermination := true;
        end;
      end;
    end;

    Msg := Self.ReadMessage(Connection, ConserveConnections, AbnormalTermination);
    try
      try
        if Assigned(Msg) then begin
          // We've read something off the network. If it's a request, and if
          // we're still connected, the TCP transport will want to know of this
          // new connection.
          if Msg.IsRequest and Connection.Connected then
            Self.AddConnection(Connection, Msg as TIdSipRequest);

          // Always tell the TCP transport of the received message, even if
          // malformed (say, because Content-Length has a positive value and we
          // timed out reading the message's body).
          Self.ReceiveMessageInTimerContext(Msg, ReceivedFrom);
        end;
      except
        // Catch-all: ReadMessage should catch any exceptions it's interested
        // in, so any other exceptions indicate Something Bad. Just notify the
        // transport, and close the connection (if not already closed).
        on E: Exception do begin
          Self.NotifyOfException(ExceptClass(E.ClassType), E.Message);

          if Connection.Connected then
            Connection.DisconnectSocket;
        end;
      end;
    finally
      Msg.Free;
    end;
  finally
    ReceivedFrom.Free;
  end;

  Self.NotifyOfDisconnectionInTimerContext(Connection);
end;

procedure TIdSipTcpMessageReader.ReadMessages(Connection: TIdTCPConnection;
                                              ConserveConnections: Boolean);
var
  AbnormalTermination: Boolean;
  Msg:                 TIdSipMessage;
  ReceivedFrom:        TIdConnectionBindings;
begin
  AbnormalTermination := false;

  Connection.ReadTimeout := Self.ReadTimeout;

  ReceivedFrom := TIdConnectionBindings.Create;
  try
    if Connection.Connected then begin
      try
        ReceivedFrom.LocalIP   := Connection.Socket.Binding.IP;
        ReceivedFrom.LocalPort := Connection.Socket.Binding.Port;
        ReceivedFrom.PeerIP    := Connection.Socket.Binding.PeerIP;
        ReceivedFrom.PeerPort  := Connection.Socket.Binding.PeerPort;
        ReceivedFrom.Transport := Self.TransportType;
      except
        on E: Exception do begin
          Self.NotifyOfException(ExceptClass(E.ClassType), Format('%s while copying binding information for a SIP message off a TCP connection', [E.Message]));
          AbnormalTermination := true;
        end;
      end;
    end;

    while Connection.Connected and not AbnormalTermination do begin
      Msg := Self.ReadMessage(Connection, ConserveConnections, AbnormalTermination);
      try
        try
          if Assigned(Msg) then begin
            // We've read something off the network. If it's a request, and if
            // we're still connected, the TCP transport will want to know of this
            // new connection.
            if Msg.IsRequest and Connection.Connected then
              Self.AddConnection(Connection, Msg as TIdSipRequest);

            // Always tell the TCP transport of the received message, even if
            // malformed (say, because Content-Length has a positive value and we
            // timed out reading the message's body).
            Self.ReceiveMessageInTimerContext(Msg, ReceivedFrom);
          end;
        except
          // Catch-all: ReadMessage should catch any exceptions it's interested
          // in, so any other exceptions indicate Something Bad. Just notify the
          // transport, and close the connection (if not already closed).
          on E: Exception do begin
            Self.NotifyOfException(ExceptClass(E.ClassType), E.Message);

            if Connection.Connected then
              Connection.DisconnectSocket;
          end;
        end;
      finally
        Msg.Free;
      end;
    end;
  finally
    ReceivedFrom.Free;
  end;

  Self.NotifyOfDisconnectionInTimerContext(Connection);
end;

//* TIdSipTcpMessageReader Private methods *************************************

procedure TIdSipTcpMessageReader.AddConnection(Connection: TIdTCPConnection;
                                               Request: TIdSipRequest);
begin
  Self.NotifyOfConnectionInTimerContext(Connection, Request);
end;

procedure TIdSipTcpMessageReader.NotifyOfConnectionInTimerContext(Connection: TIdTCPConnection;
                                                                  Request: TIdSipRequest);
begin
end;

procedure TIdSipTcpMessageReader.NotifyOfDisconnectionInTimerContext(Connection: TIdTCPConnection);
begin
end;

procedure TIdSipTcpMessageReader.ReadBodyInto(Connection: TIdTCPConnection;
                                              Msg: TIdSipMessage;
                                              Dest: TStringStream);
begin
  Connection.ReadStream(Dest, Msg.ContentLength);

  // Roll back the stream to just before the message body!
  Dest.Seek(-Msg.ContentLength, soFromCurrent);
end;

procedure TIdSipTcpMessageReader.ReadHeaders(Connection: TIdTCPConnection;
                                             Dest: TStringStream);
begin
  // We skip any leading CRLFs, and read up to (and including) the first blank
  // line.
  while (Dest.DataString = '') do
    Connection.Capture(Dest, '');

  // Capture() returns up to the blank line, but eats it: we add it back in
  // manually.
  Dest.Write(CrLf, Length(CrLf));
  Dest.Seek(0, soFromBeginning);
end;

function TIdSipTcpMessageReader.ReadMessage(Connection: TIdTCPConnection;
                                            ConserveConnections: Boolean;
                                            var AbnormalTermination: Boolean): TIdSipMessage;
var
  S: TStringStream;
begin
  // Read in a SIP message from Connection, instantiate a TIdSipMessage
  // subclass, and return the reified message. THE CALLER FREES THE MESSAGE.
  //
  // In the event of an unimportant timeout ("unimportant" means "not in the
  // middle of reading a message" - a timeout between reading the headers of a
  // message and reading the body), return nil.
  //
  // As a side effect, set AbnormalTermination to true if necessary.

  AbnormalTermination := false;
  Result := nil;

  S := TStringStream.Create('');
  try
    try
      Self.ReadHeaders(Connection, S);
    except
      on EIdReadTimeout do
        AbnormalTermination := not ConserveConnections;
      on EIdConnClosedGracefully do;
      on EIdClosedSocket do;
    end;

    if not AbnormalTermination and (S.DataString <> '') then begin
      Result := TIdSipMessage.ReadMessageFrom(S);

      try
        Self.ReadBodyInto(Connection, Result, S);
        Result.ReadBody(S);
      except
        on EIdReadTimeout do
          AbnormalTermination := true;
        on EIdConnClosedGracefully do;
        on EIdClosedSocket do;
      end;
    end;
  finally
    S.Free;
  end;
end;

procedure TIdSipTcpMessageReader.ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                                              Binding: TIdConnectionBindings);
var
  RecvWait: TIdSipReceiveMessageWait;
begin
  RecvWait := TIdSipReceiveMessageWait.Create;
  RecvWait.Message      := Msg.Copy;
  RecvWait.ReceivedFrom := Binding.Copy;
  RecvWait.TransportID  := Self.TransportID;

  Self.Timer.AddEvent(TriggerImmediately, RecvWait);
end;

procedure TIdSipTcpMessageReader.ReturnInternalServerError(Connection: TIdTCPConnection;
                                                           const Reason: String);
var
  Res: TIdSipResponse;
begin
  Res := TIdSipResponse.Create;
  try
    // We really can't do much more than this.
    Res.StatusCode := SIPInternalServerError;
    Res.StatusText := Reason;
    Res.SipVersion := SipVersion;

    Connection.Write(Res.AsString);
  finally
    Res.Free;
  end;
end;

//******************************************************************************
//* TIdSipConnection                                                           *
//******************************************************************************
//* TIdSipConnection Public methods ********************************************

constructor TIdSipConnection.Create;
begin
  inherited Create;

  Self.fBinding     := TIdConnectionBindings.Create;
  Self.fConnected   := false;
  Self.fHandle      := INVALID_SOCKET;
  Self.MessageQueue := CreateWindowHandle(Self.WndProc);
end;

destructor TIdSipConnection.Destroy;
begin
  Self.Close;

  DestroyWindowHandle(Self.MessageQueue);
  Self.fBinding.Free;

  inherited Destroy;
end;

procedure TIdSipConnection.Close;
begin
  if (Self.Handle = INVALID_SOCKET) then Exit;

  // We don't care about the error:
  WinSock.closesocket(Self.Handle);

  Self.fConnected := false;
end;

function TIdSipConnection.Connected: Boolean;
begin
  Result := Self.fConnected;
end;

function TIdSipConnection.IsOutbound: Boolean;
begin
  Result := false;
end;

function TIdSipConnection.PartialSend: Boolean;
var
  BytesWritten: Integer;
  Error:        Integer;
begin
  // Try send data. Return true iff our send buffer's emptied.

  if (Self.SendBuffer = '') then begin
    Result := true;
    Exit;
  end;

  // WinSock has just told us we may now send more data.
  repeat
    BytesWritten := WinSock.send(Self.Handle, Self.SendBuffer[1], Self.SendBufferLength, 0);
    Delete(Self.SendBuffer, 1, BytesWritten)
  until (BytesWritten = 0) or (BytesWritten = SOCKET_ERROR) or (Self.SendBuffer = '');

  Result := Self.SendBuffer = '';

  if (BytesWritten = SOCKET_ERROR) then begin
    Error := WSAGetLastError;
    case Error of
      WSAENOBUFS:;
    else
      raise EIdSocketException.Create('send', Error);
    end;
  end;
end;

function TIdSipConnection.Receive(var AbnormalTermination: Boolean): TIdSipMessage;
var
  S: TStringStream;
begin
  // Return a fully formed message if possible.
  // In the event of a timeout, remember what we've read so far, and return nil.
  // In the event of an error, raise an exception.

  // Read in a SIP message from Connection, instantiate a TIdSipMessage
  // subclass, and return the reified message. THE CALLER FREES THE MESSAGE.
  //
  // In the event of an unimportant timeout ("unimportant" means "not in the
  // middle of reading a message" - a timeout between reading the headers of a
  // message and reading the body), return nil.
  //
  // As a side effect, set AbnormalTermination to true if necessary.

  AbnormalTermination := false;
  Result := nil;

  S := TStringStream.Create('');
  try
    try
      Self.ReadHeaders(S);
    except
      on EIdReadTimeout do
        AbnormalTermination := not Self.KeepAlive;
      on EIdConnClosedGracefully do;
      on EIdClosedSocket do;
    end;

    if not AbnormalTermination and (S.DataString <> '') then begin
      Result := TIdSipMessage.ReadMessageFrom(S);

      try
        Self.ReadBodyInto(Result, S);
        Result.ReadBody(S);
      except
        on EIdReadTimeout do
          AbnormalTermination := true;
        on EIdConnClosedGracefully do;
        on EIdClosedSocket do;
      end;
    end;
  finally
    S.Free;
  end;
end;

procedure TIdSipConnection.Send(Msg: TIdSipMessage);
begin
  if not Self.Connected then Exit;

  Self.SendBuffer := Self.SendBuffer + Msg.AsString;
  Self.PartialSend;
end;

//* TIdSipConnection Protected methods *****************************************

procedure TIdSipConnection.OnAccept;
begin
  // By default, do nothing.
end;

procedure TIdSipConnection.OnConnect;
begin
  // By default, do nothing.
end;

procedure TIdSipConnection.OnClose;
begin
  // By default, do nothing.
end;

procedure TIdSipConnection.OnDisconnect;
begin
  // By default, do nothing.
end;

procedure TIdSipConnection.OnOOB;
begin
  // By default, do nothing.
end;

procedure TIdSipConnection.OnRead;
begin
  Self.Recv;
end;

procedure TIdSipConnection.OnWrite;
begin
  Self.PartialSend;
end;

//* TIdSipConnection Private methods *******************************************

procedure TIdSipConnection.Capture(Dest: TStringStream; Delimiter: String);
var
  S: String;
begin
  // Repeatedly read lines until we hit a line matching Delimiter.
  // Return all data read, INCLUDING the delimiter.

  repeat
    S := Self.ReadLn;
    Dest.WriteString(S);
  until (S = Delimiter + CrLf);
end;

procedure TIdSipConnection.CMSocketMessage(var Message: TCMSocketMessage);
var
  ErrorCode: Integer;
begin
  ErrorCode := Message.SelectError;

  if (ErrorCode <> 0) then begin
//    Self.NotifyOfError(Self.Connection.Binding, Message.SelectEvent, Message.SelectError
  end;

  case Message.SelectEvent of
    FD_ACCEPT:  Self.OnAccept;
    FD_CONNECT: Self.OnConnect;
    FD_CLOSE:   Self.OnDisconnect;
    FD_OOB:     Self.OnOOB;
    FD_READ:    Self.OnRead;
    FD_WRITE:   Self.OnWrite;
    // Unsupported by Delphi 7's WinSock.pas:
//    FD_GROUP_QOS: Self.OnGroupQos;
//    FD_QOS:     Self.OnQos;
//    FD_ADDRESS_LIST_CHANGE: Self.OnAddressListChange;
//    FD_ROUTING_INTERFACE_CHANGE: Self.OnRoutingInterfaceChange;
  end;
end;

function TIdSipConnection.FirstN(ByteCount: Integer): String;
begin
  // Read and remove ByteCount bytes from the internal buffer.
  Result := Copy(Self.InternalBuffer, 1, ByteCount);
  Delete(Self.InternalBuffer, 1, ByteCount);
end;

function TIdSipConnection.InternalBufferLength: Integer;
begin
  Result := Length(Self.InternalBuffer);
end;

procedure TIdSipConnection.PopulateSockAddr(Location: TIdSipLocation; var Addr: TSockAddrIn);
begin
  Addr.sin_family := Self.ProtocolType(Location.IPAddress);
  Addr.sin_addr.S_addr := htonl(TIdIPAddressParser.InetAddr(Location.IPAddress));
  Addr.sin_port := htons(u_short(Location.Port and $ffff)); // TODO CLEANUP: bitmask shouldn't be necessary! TPortNum->u_short!
end;

function TIdSipConnection.ProtocolType(Address: String): Integer;
begin
  case TIdIPAddressParser.IPVersion(Address) of
    Id_IPv4: Result := PF_INET;
    Id_IPv6: Result := PF_INET6;
  else
    raise Exception.Create(Format('Cannot determine protocol family of address ''%s''', [Address]));
  end;
end;

procedure TIdSipConnection.ReadBodyInto(Msg: TIdSipMessage;
                                        Dest: TStringStream);
begin
  Self.ReadStream(Dest, Msg.ContentLength);

  // Roll back the stream to just before the message body!
  Dest.Seek(-Msg.ContentLength , soFromCurrent);
end;

procedure TIdSipConnection.ReadHeaders(Dest: TStringStream);
begin
  // We skip any leading CRLFs, and read up to (and including) the first blank
  // line.
  while (Dest.DataString = '') do
    Self.Capture(Dest, '');

  // Capture() returns up to the blank line, but eats it: we add it back in
  // manually.
  Dest.Seek(0, soFromBeginning);
end;

function TIdSipConnection.ReadLn: String;
const
  CrLf = #$D#$A;
var
  LinePos: Integer;
begin
  Result := '';

  // TODO:
  // This algorithm's pretty naive. If we receive data very slowly - such that
  // every call to Self.Recv only adds a single character to the internal
  // buffer - we will repeatedly iterate through the entire internal buffer.
  repeat
    LinePos := Pos(Crlf, Self.InternalBuffer);
    if (LinePos > 0) then begin
      // Return the entire line, including line end.
      Result := Self.FirstN(LinePos + Length(Crlf) - 1);
    end else begin
      Self.Recv;
    end;
  until (LinePos > 0);
end;

procedure TIdSipConnection.ReadStream(Dest: TStringStream; ByteCount: Integer);
var
  BytesRead:      Integer;
  BytesRemaining: Integer;
begin
  // Read ByteCount number of bytes from the socket/internal buffer and write
  // them to Dest.

  if (Self.InternalBufferLength >= ByteCount) then begin
    // We have enough data in the internal buffer to not need to read from the
    // socket.
    Dest.WriteString(Self.FirstN(ByteCount));
  end else begin
    BytesRemaining := ByteCount;
    repeat
      // We need to read more data.
      Self.Recv;

      BytesRead := Min(BytesRemaining, Self.InternalBufferLength);

      Dest.WriteString(Self.FirstN(BytesRead));
      Dec(BytesRemaining, BytesRead);
    until (BytesRemaining <= 0);
  end;
end;

function TIdSipConnection.Recv: String;
const
  BufLen = 1024;
var
  Buf:       array[0..BufLen - 1] of Char;
  BytesRead: Integer;
begin
  // Read all we can off the socket. Call this S. Store S in the internal
  // buffer, and return S.
  Result := '';

  BytesRead := WinSock.recv(Self.Handle, Buf, BufLen, 0);
  if (BytesRead = SOCKET_ERROR) then begin
    raise EIdSocketException.Create('recv', WSAGetLastError);
  end;

  Result := Copy(Buf, 1, BytesRead);

  Self.InternalBuffer := Self.InternalBuffer + Result;
end;

function TIdSipConnection.SendBufferLength: Integer;
begin
  Result := Length(Self.SendBuffer);
end;

procedure TIdSipConnection.Select;
var
  RC: Integer;
begin
  RC := WSAAsyncSelect(Self.Handle, Self.MessageQueue, CM_SOCKETMESSAGE, FD_ACCEPT or FD_CLOSE or FD_READ or FD_WRITE);
  if (RC = SOCKET_ERROR) then begin
      raise EIdSocketException.Create('WSAAsyncSelect', WSAGetLastError);
  end;
end;

procedure TIdSipConnection.WndProc(var Message: TMessage);
begin
  // Copied verbatim from ScktComp.TCustomWinSocket.WndProc
  try
    Self.Dispatch(Message);
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
  end;
end;

//******************************************************************************
//* TIdSipClientConnection                                                     *
//******************************************************************************
//* TIdSipClientConnection Public methods **************************************

procedure TIdSipClientConnection.Connect(PeerIP: String; PeerPort: Cardinal);
var
  L: TIdSipLocation;
begin
  L := TIdSipLocation.Create(TcpTransport, PeerIP, PeerPort);
  try
    Self.Connect(L);
  finally
    L.Free;
  end;
end;

procedure TIdSipClientConnection.Connect(Location: TIdSipLocation);
var
  LocalAddr:    TSockAddr;
  LocalAddrLen: Integer;
  RemoteAddr:   TSockAddr;
  RC:           Integer;
begin
  Self.PopulateSockAddr(Location, RemoteAddr);

  Self.fHandle := WinSock.socket(RemoteAddr.sin_family, SOCK_STREAM, IPPROTO_TCP);
  if (Self.fHandle = INVALID_SOCKET) then
    raise EIdSocketException.Create('socket', WSAGetLastError);

  SetSocketKeepAlive(Self.Handle, Self.KeepAlive);

  RC := WinSock.connect(Self.Handle, RemoteAddr, SizeOf(RemoteAddr));
  if (RC <> 0) then
    EIdSocketException.Create('connect', WSAGetLastError);

  RC := WinSock.getsockname(Self.Handle, LocalAddr, LocalAddrLen);
  if (RC <> 0) then
    EIdSocketException.Create('getsockname', WSAGetLastError);

  Self.SetLocalBinding(LocalAddr, Self.Binding);
  Self.SetPeerBinding(RemoteAddr, Self.Binding);
  Self.Binding.Transport := TcpTransport;

  Self.Select;

  Self.fConnected := true;
end;

function TIdSipClientConnection.IsOutbound: Boolean;
begin
  Result := true;
end;

//* TIdSipClientConnection Private methods *************************************

procedure TIdSipClientConnection.SetLocalBinding(Addr: TSockAddr; Binding: TIdConnectionBindings);
begin
  // TODO: Support IPv6!
  Binding.LocalIP   := TIdIPAddressParser.IPv4AddressToStr(ntohl(Addr.sin_addr.S_addr));
  Binding.LocalPort := ntohs(Addr.sin_port);
end;

procedure TIdSipClientConnection.SetPeerBinding(Addr: TSockAddr; Binding: TIdConnectionBindings);
begin
  // TODO: Support IPv6!
  Binding.PeerIP   := TIdIPAddressParser.IPv4AddressToStr(ntohl(Addr.sin_addr.S_addr));
  Binding.PeerPort := ntohs(Addr.sin_port);
end;

//******************************************************************************
//* TIdSipServerConnection                                                     *
//******************************************************************************
//* TIdSipServerConnection Public methods **************************************

constructor TIdSipServerConnection.Create(Handle: TSocket; Binding: TIdConnectionBindings);
begin
  inherited Create;

  Self.fHandle := Handle;
  Self.Binding.Assign(Binding);
end;

//******************************************************************************
//* TIdSipListeningConnection                                                  *
//******************************************************************************
//* TIdSipListeningConnection Public methods ***********************************

destructor TIdSipListeningConnection.Destroy;
begin
  Self.ServerLocation.Free;

  inherited Destroy;
end;

function TIdSipListeningConnection.Accept: TIdSipServerConnection;
var
  Binding:       TIdConnectionBindings;
  NewConnection: TSocket;
  PeerAddr:      TSockAddr;
  PeerAddrLen:   Integer;
begin
  Result := nil;

  if (Self.Handle = INVALID_SOCKET) then
    raise Exception.Create('Call Bind() before Accept()');

  NewConnection := WinSock.accept(Self.Handle, @PeerAddr, @PeerAddrLen);
  if (NewConnection = INVALID_SOCKET) then
    raise EIdSocketException.Create('socket', WSAGetLastError);

  Binding := TIdConnectionBindings.Create;
  try
    Binding.LocalIP   := Self.ServerLocation.IPAddress;
    Binding.LocalPort := Self.ServerLocation.Port;

    if (Self.IPVersion(PeerAddr.sin_family) = Id_IPv4) then begin
      Binding.PeerIP := TIdIPAddressParser.IPv4AddressToStr(PeerAddr.sin_addr.S_addr);
    end
    else begin
      raise Exception.Create('TIdSipListeningConnection.Accept doesn''t yet support IPv6');
    end;
    Binding.PeerPort := Cardinal(PeerAddr.sin_port);
    Binding.Transport := TcpTransport;

    Result := TIdSipServerConnection.Create(NewConnection, Binding);
  finally
    Binding.Free;
  end;
end;

procedure TIdSipListeningConnection.Bind(IP: String; Port: Cardinal);
var
  L: TIdSipLocation;
begin
  L := TIdSipLocation.Create(TcpTransport, IP, Port);
  try
    Self.Bind(L);
  finally
    L.Free;
  end;
end;

procedure TIdSipListeningConnection.Bind(Location: TIdSipLocation);
var
  A:  TSockAddrIn;
  RC: Integer;
begin
  Self.ServerLocation := Location.Copy;

  Self.PopulateSockAddr(Location, A);

  Self.fHandle := socket(A.sin_family, SOCK_STREAM, IPPROTO_TCP);
  if (Self.fHandle = INVALID_SOCKET) then
    raise EIdSocketException.Create('socket', WSAGetLastError);

  SetSocketReuseAddr(Self.Handle, true);
  SetSocketKeepAlive(Self.Handle, Self.KeepAlive);

  RC := WinSock.bind(Self.Handle, A, SizeOf(A));
  if (RC <> 0) then
    raise EIdSocketException.Create('bind', WSAGetLastError);
end;

procedure TIdSipListeningConnection.Listen(QueueLength: Cardinal);
var
  RC: Integer;
begin
  if (Self.Handle = INVALID_SOCKET) then
    raise Exception.Create('Call Bind() before Accept()');

  RC := WinSock.listen(Self.Handle, QueueLength);
  if (RC <> 0) then begin
    raise EIdSocketException.Create(Self.ServerLocation.AsCompactString + ': listen', WSAGetLastError);
  end;
end;

//* TIdSipListeningConnection Private methods **********************************

function TIdSipListeningConnection.IPVersion(Protocol: Integer): TIdIPVersion;
begin
  case Protocol of
    PF_INET: Result := Id_IPv4;
    PF_INET6: Result := Id_IPv6;
  else
    raise Exception.Create(Format('Unknown protocol family ''%d''', [Protocol]));
  end;
end;

//* TIdSipListeningConnection Protected methods ********************************

procedure TIdSipListeningConnection.OnAccept;
begin
  // Notify the owner.
end;

//******************************************************************************
//* TIdSipConnectionTableEntry                                                 *
//******************************************************************************
//* TIdSipConnectionTableEntry Public methods **********************************

constructor TIdSipConnectionTableEntry.Create(Connection: TIdSipConnection;
                                              Request: TIdSipRequest);
begin
  inherited Create;

  Self.fConnection := Connection;

  Self.fRequest := Request.Copy as TIdSipRequest;
end;

destructor TIdSipConnectionTableEntry.Destroy;
begin
  Self.fRequest.Free;

  inherited Destroy;
end;

//******************************************************************************
//* TIdSipConnectionTable                                                      *
//******************************************************************************
//* TIdSipConnectionTable Public methods ***************************************

constructor TIdSipConnectionTable.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdSipConnectionTable.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

function TIdSipConnectionTable.Add(Connection: TIdSipConnection;
                                   Request:  TIdSipRequest): TIdSipConnectionTableEntry;
begin
  Result := Self.FindEntry(Connection, Request);

  if not Assigned(Result) then begin
    Result := TIdSipConnectionTableEntry.Create(Connection, Request);
    Self.List.Add(Result);
  end;
end;

function TIdSipConnectionTable.AnyConnectionTo(Destination: TIdSipLocation): TIdSipConnection;
var
  B: TIdConnectionBindings;
  I: Integer;
begin
  // Return ANY connection to this destination.
  Result := nil;
  for I := 0 to Self.Count - 1 do begin
    B := Self.EntryAt(I).Connection.Binding;
    if (B.PeerIP    = Destination.IPAddress) and
       (B.PeerPort  = Destination.Port) and
       (B.Transport = Destination.Transport) then
      Result := Self.EntryAt(I).Connection;
      Break;
  end;
end;

function TIdSipConnectionTable.ConnectionEntry(Connection: TIdSipConnection): TIdSipConnectionTableEntry;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.Count - 1 do begin
    if (Self.EntryAt(I).Connection = Connection) then begin
      Result := Self.EntryAt(I);
      Break;
    end;
  end;
end;

function TIdSipConnectionTable.ConnectionFor(Msg: TIdSipMessage): TIdSipConnection;
var
  Count: Integer;
  I:     Integer;
  Found: Boolean;
begin
  Result := nil;

  I := 0;
  Count := Self.List.Count;
  Found := false;
  while (I < Count) and not Found do begin
    if Msg.IsRequest then
      Found := Self.EntryAt(I).Request.Equals(Msg)
    else
      Found := Self.EntryAt(I).Request.Match(Msg);

    if not Found then
      Inc(I);
  end;

  if (I < Count) then
    Result := Self.EntryAt(I).Connection;
end;

function TIdSipConnectionTable.ConnectionFor(S: TSocket): TIdSipConnection;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.Count - 1 do begin
    if (Self.EntryAt(I).Connection.Handle = S) then begin
      Result := Self.EntryAt(I).Connection;
      Break;
    end;
  end;
end;

function TIdSipConnectionTable.ConnectionFor(Destination: TIdConnectionBindings): TIdSipConnection;
var
  I: Integer;
  C: TIdSipConnectionTableEntry;
begin
  Result := nil;

  for I := 0 to Self.List.Count - 1 do begin
    C := Self.EntryAt(I);
    if C.Connection.Binding.Equals(Destination) then begin
      Result := C.Connection;
      Break;
    end;
  end;
end;

function TIdSipConnectionTable.Count: Integer;
begin
  Result := Self.List.Count;
end;

procedure TIdSipConnectionTable.Remove(Connection: TIdSipConnection);
var
  I: Integer;
begin
  I := 0;
  while (I < Self.List.Count) do begin
    if (Self.EntryAt(I).Connection = Connection) then
      Self.List.Delete(I)
    else Inc(I);
  end;
end;

//* TIdSipConnectionTable Private methods **************************************

function TIdSipConnectionTable.EntryAt(Index: Integer): TIdSipConnectionTableEntry;
begin
  Result := Self.List[Index] as TIdSipConnectionTableEntry;
end;

function TIdSipConnectionTable.FindEntry(Connection: TIdSipConnection;
                                         Request: TIdSipRequest): TIdSipConnectionTableEntry;
var
  E: TIdSipConnectionTableEntry;
  I: Integer;
begin
  Result := nil;

  I := 0;
  while (Result = nil) and (I < Self.List.Count) do begin
    E := Self.EntryAt(I);
    if E.Request.Equals(Request) and (E.Connection = Connection) then
      Result := E;

    Inc(I);
  end;
end;

//******************************************************************************
//* TIdSipConnectionTableLock                                                  *
//******************************************************************************
//* TIdSipConnectionTableLock Public methods ***********************************

constructor TIdSipConnectionTableLock.Create;
begin
  inherited Create;

  Self.Lock  := TCriticalSection.Create;
  Self.Table := TIdSipConnectionTable.Create;
end;

destructor TIdSipConnectionTableLock.Destroy;
begin
  Self.Lock.Acquire;
  try
    Self.Table.Free;
  finally
    Self.Lock.Release;
  end;
  Self.Lock.Free;

  inherited Destroy;
end;

function TIdSipConnectionTableLock.LockList: TIdSipConnectionTable;
begin
  Self.Lock.Acquire;
  Result := Self.Table;
end;

procedure TIdSipConnectionTableLock.UnlockList;
begin
  Self.Lock.Release;
end;

//******************************************************************************
//* TIdSipWriteToConnectionWait                                                *
//******************************************************************************
//* TIdSipWriteToConnectionWait Public methods *********************************

constructor TIdSipWriteToConnectionWait.Create;
begin
  inherited Create;

  Self.fDestination := TIdSipLocation.Create;
  Self.fMsg         := TIdSipMessage.Create;
end;

destructor TIdSipWriteToConnectionWait.Destroy;
begin
  Self.fMsg.Free;
  Self.fDestination.Free;

  inherited Destroy;
end;

procedure TIdSipWriteToConnectionWait.Trigger;
begin
  Self.Server.SendMessage(Self.Msg, Self.Destination);
end;

//* TIdSipWriteToConnectionWait Private methods ********************************

procedure TIdSipWriteToConnectionWait.SetDestination(Value: TIdSipLocation);
begin
  Self.fDestination.Assign(Value);
end;

procedure TIdSipWriteToConnectionWait.SetMsg(Value: TIdSipMessage);
begin
  Self.fMsg.Free;
  Self.fMsg := Value.Copy;
end;

//******************************************************************************
//* TIdSipTcpServerStartWait                                                   *
//******************************************************************************
//* TIdSipTcpServerStartWait Public methods ************************************

procedure TIdSipTcpServerStartWait.Trigger;
begin
  Self.Server.StartListening;
end;

//******************************************************************************
//* TIdSipTcpServerSetListenQueueWait                                          *
//******************************************************************************
//* TIdSipTcpServerSetListenQueueWait Public methods ***************************

procedure TIdSipTcpServerSetListenQueueWait.Trigger;
begin
  Self.Server.SetListenQueue(Self.ListenQueue);
end;

//******************************************************************************
//* TIdSipTcpServer                                                            *
//******************************************************************************
//* TIdSipTcpServer Public methods *********************************************

constructor TIdSipTcpServer.Create(Bindings: TIdSipLocations);
begin
  inherited Create(true);

  Self.Bindings         := TIdSipLocations.Create;
  Self.Clients          := TObjectList.Create(true);
  Self.ConnectionMap    := TIdSipConnectionTable.Create;
  Self.ListeningSockets := TObjectList.Create(true);

  Self.Bindings.AddLocations(Bindings);

  // Set some sensible defaults
  Self.ListenQueue := 5;

  Self.Resume;
end;

destructor TIdSipTcpServer.Destroy;
begin
  Self.ListeningSockets.Free;
  Self.ConnectionMap.Free;
  Self.Clients.Free;
  Self.Bindings.Free;

  inherited Destroy;
end;

procedure TIdSipTcpServer.Resume;
var
  Wait: TIdSipTcpServerStartWait;
begin
  inherited Resume;

  Wait := TIdSipTcpServerStartWait.Create;
  Wait.Server := Self;
  Self.AddEvent(TriggerImmediately, Wait);
end;

//* TIdSipTcpServer Private methods ********************************************

function TIdSipTcpServer.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TIdSipTcpServer._AddRef: Integer;
begin
  Result := -1;
end;

function TIdSipTcpServer._Release: Integer;
begin
  Result := -1;
end;

procedure TIdSipTcpServer.Accept(Connection: TIdSipConnection);
var
  NewConnection: TIdSipServerConnection;
begin
  if Connection.IsOutbound then Exit;

  NewConnection := (Connection as TIdSipListeningConnection).Accept;
  Self.Clients.Add(NewConnection);
end;

function TIdSipTcpServer.BindingCount: Integer;
begin
  Result := Self.Bindings.Count;
end;

procedure TIdSipTcpServer.Disconnect(Connection: TIdSipConnection);
begin
  Self.ConnectionMap.Remove(Connection);
  Self.Clients.Remove(Connection);
end;

function TIdSipTcpServer.FirstServerPortFor(IPAddress: String): Cardinal;
var
  I: Integer;
begin
  Result := TIdSipTransportRegistry.DefaultPortFor(Self.Bindings[0].Transport);

  for I := 0 to Self.BindingCount - 1 do begin
    if (Self.Bindings[I].IPAddress = IPAddress) then begin
      Result := Self.Bindings[I].Port;
      Break;
    end;
  end;
end;

procedure TIdSipTcpServer.NotifyOfReceivedMessage(M: TIdSipMessage;
                                                  Connection: TIdConnectionBindings);
begin
  // TODO: Communicate back to the main TimerQueue!
end;

procedure TIdSipTcpServer.NotifyOfSentMessage(M: TIdSipMessage;
                                              Connection: TIdConnectionBindings);
begin
  // TODO: Communicate back to the main TimerQueue!
end;

procedure TIdSipTcpServer.ReadMessageFrom(Conn: TIdSipConnection);
var
  AbnormalTermination: Boolean;
  M:                   TIdSipMessage;
begin
  M := Conn.Receive(AbnormalTermination);
  try
    if Assigned(M) then begin
      Self.NotifyOfReceivedMessage(M, Conn.Binding);
    end;

    if AbnormalTermination then;
      // TODO Fill in the blank.
  finally
    M.Free;
  end;
end;

procedure TIdSipTcpServer.SendMessage(Msg: TIdSipMessage; Dest: TIdSipLocation);
var
  Connection:  TIdSipConnection;
  LocBinding:  TIdConnectionBindings;
begin
  LocBinding := TIdConnectionBindings.Create;
  try
    // Try send the response down the same connection on which we received the
    // request.
    Connection := Self.ConnectionMap.ConnectionFor(Msg);

    // Otherwise, try find an existing connection to Dest.
    if not Assigned(Connection) then
      Connection := Self.ConnectionMap.AnyConnectionTo(Dest);

    if Assigned(Connection) and Connection.Connected then begin
      LocBinding.Assign(Connection.Binding);

      // Outbound connections have an ephemeral local port, and we MUST be
      // prepared to accept connections at the port listed in a Via header
      // we generate (cf. RFC 3261, section 18.1.1). We don't really care which
      // binding's port we use, as long as the port's on the same IP address.
      //
      // We use a special local variable - LocBinding - for storing this info
      // because Dest contains the real binding info that the superclass uses
      // to log the message send.
      if Connection.IsOutbound then
        LocBinding.LocalPort := Self.FirstServerPortFor(Dest.IPAddress)
      else
        LocBinding.LocalPort := Connection.Binding.LocalPort;

      if Msg.LastHop.IsUnset then
        Msg.RewriteLocationHeaders(LocBinding);

      Self.WriteMessageTo(Msg, Connection)
    end
    else begin
      // Last resort: make a new connection to Dest.
      Self.SendMessageTo(Msg, Dest);
    end;
  finally
    LocBinding.Free;
  end;
end;

procedure TIdSipTcpServer.SendMessageTo(Msg: TIdSipMessage;
                                        Dest: TIdSipLocation);
var
  FakeRequest:   TIdSipRequest;
  LocBinding:    TIdConnectionBindings;
  NewConnection: TIdSipClientConnection;
begin
  LocBinding := TIdConnectionBindings.Create;
  try
    NewConnection := TIdSipClientConnection.Create;
    Self.Clients.Add(NewConnection);
    NewConnection.Connect(Dest.IPAddress, Dest.Port);

    // If Msg is a response, what then? The connection will get used just for
    // that one response, and not again.
    if Msg.IsRequest then
      Self.ConnectionMap.Add(NewConnection, Msg as TIdSipRequest)
    else begin
      // This is really crap! What a hack!
      FakeRequest := TIdSipRequest.Create;
      try
        // Allow us to disambiguate the ConnectionEnty objects so we get the
        // right result from ConnectionFor.
        FakeRequest.AddHeader(ViaHeaderFull);
        FakeRequest.RewriteLocationHeaders(Dest);
        Self.ConnectionMap.Add(NewConnection, FakeRequest)
      finally
        FakeRequest.Free;
      end;
    end;

    if Msg.LastHop.IsUnset then
      Msg.RewriteLocationHeaders(Dest);

    NewConnection.Send(Msg);

    Self.NotifyOfSentMessage(Msg, LocBinding);
  finally
    LocBinding.Free;
  end;
end;

procedure TIdSipTcpServer.SetListenQueue(NewValue: Integer);
begin
  Self.ListenQueue := NewValue;
end;

procedure TIdSipTcpServer.StartListening;
var
  B: TIdSipLocation;
  S: TIdSipListeningConnection;
  I: Integer;
begin
  try
    for I := 0 to Self.Bindings.Count - 1 do begin
      B := Self.Bindings[I];

      S := TIdSipListeningConnection.Create;
      Self.ListeningSockets.Add(S);
      S.Bind(B);
      S.Listen(Self.ListenQueue);
    end;
  except
    for I := 0 to Self.ListeningSockets.Count - 1 do
      TIdSipListeningConnection(Self.ListeningSockets[I]).Close;
    raise;
  end;
end;

procedure TIdSipTcpServer.WriteMessageTo(Msg: TIdSipMessage;
                                         Connection: TIdSipConnection);
begin
  Connection.Send(Msg);

  Self.NotifyOfSentMessage(Msg, Connection.Binding);
end;

//******************************************************************************
//* TIdSipTcpClient                                                            *
//******************************************************************************
//* TIdSipTcpClient Public methods *********************************************

constructor TIdSipTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.MessageReader := TIdSipTcpMessageReader.Create;
end;

destructor TIdSipTcpClient.Destroy;
begin
  Self.MessageReader.Free;

  inherited Destroy;
end;

function TIdSipTcpClient.Protocol: String;
begin
  Result := Self.MessageReader.TransportType;
end;

procedure TIdSipTcpClient.ReceiveMessages;
begin
  Self.MessageReader.ReadTimeout := Self.ReadTimeout;
  Self.MessageReader.ReadMessages(Self, Self.ConserveConnections);
end;

procedure TIdSipTcpClient.Send(Msg: TIdSipMessage);
begin
  Self.Write(Msg.AsString);
end;

//* TIdSipTcpClient Private methods ********************************************

function TIdSipTcpClient.GetTimer: TIdTimerQueue;
begin
  Result := Self.MessageReader.Timer;
end;

function TIdSipTcpClient.GetTransportID: TRegisteredObjectID;
begin
  Result := Self.MessageReader.TransportID;
end;

function TIdSipTcpClient.GetTransportType: String;
begin
  Result := Self.MessageReader.TransportType;
end;

procedure TIdSipTcpClient.SetTimer(Value: TIdTimerQueue);
begin
  Self.MessageReader.Timer := Value;
end;

procedure TIdSipTcpClient.SetTransportID(const Value: TRegisteredObjectID);
begin
  Self.MessageReader.TransportID := Value;
end;

procedure TIdSipTcpClient.SetTransportType(Value: String);
begin
  Self.MessageReader.TransportType := Value;
end;

//******************************************************************************
//* EIdSocketException                                                         *
//******************************************************************************
//* EIdSocketException Public methods ******************************************

constructor EIdSocketException.Create(FunctionName: String; WSError: Integer);
begin
  inherited Create(Format('%s() returned %d (%s)', [FunctionName, WSError, TranslateError(WSError)]));
end;

end.
