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
  Classes, Contnrs, IdBaseThread, IdConnectionBindings, IdNotification,
  IdSipMessage, IdRegisteredObject, IdSimpleParser, IdSipLocation,
  IdSipTransport, IdSocketHandle, IdTCPConnection, IdTCPServer,
  IdThreadableTcpClient, IdTimerQueue, Messages, SyncObjs, SysUtils, Windows,
  Winsock;

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

    procedure StartWinsock;
  protected
    procedure DestroyServer; override;
    procedure InstantiateServer; override;
    procedure SendMessage(Msg: TIdSipMessage;
                          Dest: TIdConnectionBindings); override;
    procedure SetConserveConnections(Value: Boolean); override;
    procedure SetTimeout(Value: Cardinal); override;
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

  TIdConnectionHandle = TSocket;

  // I represent a network (IP) non-blocking socket. I notify a TimerQueue
  // (identified by ServerID) of events by scheduling instances
  // TIdSipTcpServerWait subclasses.
  //
  // You may freely send messages immediately after accepting an inbound
  // connection or after attempting to connect to a remote party: I queue up
  // unsent messages and will automatically start sending data as soon as I am
  // fully established (as long as Configured = true (*). When I finish sending a
  // complete SIP message, I notify you through the OnSentMsg method.
  //
  // (*) Sometimes, external parties may need to perform additional
  // configuration after I've established a connection but before I send any
  // messages. In this case, set my Configured property to false after
  // instantiation. When I signal that I've established the connection, set
  // Configured to true.
  //
  // I automatically (and always) use keep-alive.
  TIdSipConnection = class(TObject)
  private
    fCurrentMsg:    TIdSipMessage;
    fConnected:     Boolean;
    fConfigured:    Boolean; // Allows external parties to set properties before messages are sent.
    fBinding:       TIdConnectionBindings;
    fKeepAlive:     Boolean; // Set this BEFORE you Connect() or Bind()!
    fReadTimeout:   Integer;
    fServerID:      TRegisteredObjectID;
    InternalBuffer: String;
    MessageQueue:   THandle;

    procedure Capture(Dest: TStringStream; Delimiter: String);
    procedure CMSocketMessage(var Message: TCMSocketMessage); message CM_SOCKETMESSAGE;
    function  FirstN(ByteCount: Integer): String;
    function  GetTransport: String;
    function  InternalBufferLength: Integer;
    procedure OnConnect(ErrorCode: Integer);
    procedure OnDisconnect(ErrorCode: Integer);
    procedure OnMessageSent(Msg: TIdSipMessage);
    procedure OnOOB(ErrorCode: Integer);
    procedure OnRead(ErrorCode: Integer);
    procedure OnWrite(ErrorCode: Integer);
    procedure PopulateSockAddr(Location: TIdSipLocation; var Addr: TSockAddrIn);
    function  ProcessMessage(var Msg: TMsg): Boolean;
    function  ProtocolType(Address: String): Integer;
    procedure RaiseExceptionExceptWhen(FunctionName: String; AcceptableError: Integer); overload;
    procedure RaiseExceptionExceptWhen(FunctionName: String; AcceptableErrors: array of Integer); overload;
    procedure ReadBodyInto(Msg: TIdSipMessage;
                           Dest: TStringStream);
    procedure ReadHeaders(Dest: TStringStream);
    function  ReadLn: String;
    procedure ReadStream(Dest: TStringStream; ByteCount: Integer);
    function  Recv: String;
    procedure Select;
    procedure SetCurrentMsg(Msg: TIdSipMessage);
    procedure SetTransport(Value: String);
    procedure WndProc(var Message: TMessage);
  protected
    fHandle: TSocket;

    procedure BeforeConnect; virtual;
    function  GetAdvertisedPort: TPortNum; virtual;
    procedure OnAccept(ErrorCode: Integer); virtual;
    procedure RewriteLocationHeaders(Msg: TIdSipMessage);
    procedure SetLocalBinding(Addr: TSockAddr; Binding: TIdConnectionBindings);
    procedure SetPeerBinding(Addr: TSockAddr; Binding: TIdConnectionBindings);
  public
    constructor Create; overload; virtual;
    destructor  Destroy; override;

    procedure Close; virtual;
    function  Connected: Boolean;
    function  IsOutbound: Boolean; virtual;
    procedure ProcessMessages;
    function  Receive(var AbnormalTermination: Boolean): TIdSipMessage;

    property AdvertisedPort: TPortNum              read GetAdvertisedPort;
    property Binding:        TIdConnectionBindings read fBinding;
    property Configured:     Boolean               read fConfigured write fConfigured;
    property CurrentMsg:     TIdSipMessage         read fCurrentMsg;
    property Handle:         TIdConnectionHandle   read fHandle;
    property KeepAlive:      Boolean               read fKeepAlive write fKeepAlive;
    property ReadTimeout:    Integer               read fReadTimeout write fReadTimeout;
    property ServerID:       TRegisteredObjectID   read fServerID write fServerID;
    property Transport:      String                read GetTransport write SetTransport;
  end;

  TIdSipSendingConnection = class(TIdSipConnection)
  private
    SendBuffer:     String;
    SendQueue:      TObjectList;

    function  CheckConnected: Boolean;
    function  GetNextMessage: TIdSipMessage;
    function  SendBufferLength: Integer;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function  PartialSend: Boolean; virtual;
    procedure Queue(Msg: TIdSipMessage);
  end;

  TIdSipConnectionClass = class of TIdSipConnection;

  // I am a socket that connects to another machine.
  TIdSipClientConnection = class(TIdSipSendingConnection)
  private
    fAdvertisedPort: TPortNum;

    procedure ScheduleConnectCancellation(Timeout: Cardinal);
  protected
    procedure BeforeConnect; override;
    function  GetAdvertisedPort: TPortNum; override;
  public
    procedure Connect(PeerIP: String; PeerPort: TPortNum; Timeout: Cardinal); overload;
    procedure Connect(Location: TIdSipLocation; Timeout: Cardinal); overload;
    function  IsOutbound: Boolean; override;
    procedure SetAdvertisedPort(Value: TPortNum);
  end;

  // I am a socket on the server side of a connection.
  TIdSipServerConnection = class(TIdSipSendingConnection)
  public
    constructor Create(Handle: TSocket; Binding: TIdConnectionBindings); overload;
  end;

  // I am a socket that listens on a port. When I Accept()
  // a connection, I spawn a TIdSipServerConnection.
  TIdSipListeningConnection = class(TIdSipConnection)
  private
    ServerLocation: TIdSipLocation;

    procedure Bind(Location: TIdSipLocation);
  protected
    procedure OnAccept(ErrorCode: Integer); override;
  public
    destructor Destroy; override;

    function  Accept: TIdSipServerConnection;
    procedure Listen(IP: String; Port: TPortNum; QueueLength: Cardinal); overload;
    procedure Listen(Location: TIdSipLocation; QueueLength: Cardinal); overload;
  end;

  // I relate a request with a TCP connection. I store a COPY of a request
  // while storing a REFERENCE to a connection. Transports construct requests
  // and so bear responsibility for destroying them, and I need to remember
  // these requests.
  TIdSipConnectionTableEntry = class(TObject)
  private
    fConnection: TIdSipSendingConnection;
    fRequest:    TIdSipRequest;
  public
    constructor Create(Connection: TIdSipSendingConnection;
                       Request: TIdSipRequest); overload;
    destructor  Destroy; override;

    property Connection: TIdSipSendingConnection read fConnection;
    property Request:    TIdSipRequest           read fRequest;
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

    function  Add(Connection: TIdSipSendingConnection;
                  Request:  TIdSipRequest): TIdSipConnectionTableEntry;
    function  AnyConnectionTo(Destination: TIdSipLocation): TIdSipSendingConnection;
    function  ConnectionEntry(Connection: TIdSipSendingConnection): TIdSipConnectionTableEntry;
    function  ConnectionFor(Msg: TIdSipMessage): TIdSipSendingConnection; overload;
    function  ConnectionFor(S: TSocket): TIdSipSendingConnection; overload;
    function  ConnectionFor(Destination: TIdConnectionBindings): TIdSipSendingConnection; overload;
    function  Count: Integer;
    procedure Remove(Connection: TIdSipSendingConnection);

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

  // My subclasses manipulate the state of my Server through the asynchronous
  // mechanism of a TIdWait.
  TIdSipTcpServerWait = class(TIdWait)
  private
    fErrorCode: Integer;
    fHandle:    TIdConnectionHandle;
    fServerID:  TRegisteredObjectID;

    procedure Run(O: TObject);
  protected
    procedure RunWait(S: TIdSipTcpServer); virtual;
  public
    procedure Trigger; override;

    property ErrorCode: Integer             read fErrorCode write fErrorCode;
    property Handle:    TIdConnectionHandle read fHandle write fHandle;
    property ServerID:  TRegisteredObjectID read fServerID write fServerID;
  end;

  TIdSipTcpServerAcceptWait = class(TIdSipTcpServerWait)
  protected
    procedure RunWait(S: TIdSipTcpServer); override;
  end;

  TIdSipTcpConnectionCancelWait = class(TIdSipTcpServerWait)
  protected
    procedure RunWait(S: TIdSipTcpServer); override;
  end;

  TIdSipTcpServerConnectionSentMsgWait = class(TIdSipTcpServerWait)
  private
    fMsg: TIdSipMessage;

    procedure SetMsg(Value: TIdSipMessage);
  protected
    procedure RunWait(S: TIdSipTcpServer); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    property Msg: TIdSipMessage read fMsg write SetMsg;
  end;

  TIdSipTcpServerConnectWait = class(TIdSipTcpServerWait)
  private
    fErrorCode: Integer;
  protected
    procedure RunWait(S: TIdSipTcpServer); override;

    property ErrorCode: Integer read fErrorCode write fErrorCode;
  end;

  TIdSipTcpServerDisconnectWait = class(TIdSipTcpServerWait)
  protected
    procedure RunWait(S: TIdSipTcpServer); override;
  end;

  TIdSipTcpServerOOBWait = class(TIdSipTcpServerWait)
  protected
    procedure RunWait(S: TIdSipTcpServer); override;
  end;

  TIdSipTcpServerReadWait = class(TIdSipTcpServerWait)
  protected
    procedure RunWait(S: TIdSipTcpServer); override;
  end;

  TIdSipTcpServerWriteWait = class(TIdSipTcpServerWait)
  protected
    procedure RunWait(S: TIdSipTcpServer); override;
  end;

  TIdSipTcpServerSendMessageWait = class(TIdSipTcpServerWait)
  private
    fDestination: TIdSipLocation;
    fMsg:         TIdSipMessage;

    procedure SetDestination(Value: TIdSipLocation);
    procedure SetMsg(Value: TIdSipMessage);
  protected
    procedure RunWait(S: TIdSipTcpServer); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    property Destination: TIdSipLocation  read fDestination write SetDestination;
    property Msg:         TIdSipMessage   read fMsg write SetMsg;
  end;

  // A TIdSipTcpServer uses me to start itself in order to bind to sockets in
  // its own execution context.
  TIdSipTcpServerStartWait = class(TIdSipTcpServerWait)
  protected
    procedure RunWait(S: TIdSipTcpServer); override;
  end;

  // Use me to set the listen queue lengths for any new server sockets.
  TIdSipTcpServerSetListenQueueWait = class(TIdSipTcpServerWait)
  private
    fListenQueue: Integer;
  protected
    procedure RunWait(S: TIdSipTcpServer); override;
  public
    property ListenQueue: Integer read fListenQueue write fListenQueue;
  end;

  TIdSipProcessSocketEventsWait = class(TIdSipTcpServerWait)
  public
    procedure RunWait(S: TIdSipTcpServer); override;
  end;

  // I represent a TimerQueue specialised to handle both inbound and outbound
  // TCP connections. Use TIdSipTcpServerWait subclasses to manipulate my
  // state.
  TIdSipTcpServer = class(TIdThreadedTimerQueue)
  private
    Bindings:         TIdSipLocations;
    Clients:          TObjectList;
    ConnectionMap:    TIdSipConnectionTable;
    ConnectTimeout:   Cardinal; // milliseconds
    ListeningSockets: TObjectList;
    ListenQueue:      Integer;
    RemoteTQ:         TRegisteredObjectID;
    TransportID:      TRegisteredObjectID;

    procedure Accept(ListeningSocket: TIdConnectionHandle; ErrorCode: Integer);
    function  AddSocket(L: TObjectList; SocketType: TIdSipConnectionClass): TIdSipConnection;
    function  BindingCount: Integer;
    procedure CancelConnect(Handle: TIdConnectionHandle);
    function  ClientFor(Handle: TIdConnectionHandle): TIdSipSendingConnection; overload;
    procedure Connect(Handle: TIdConnectionHandle; ErrorCode: Integer);
    procedure Disconnect(Handle: TIdConnectionHandle; ErrorCode: Integer);
    function  FirstServerPortFor(IPAddress: String): TPortNum;
    function  ListeningSocket(Handle: TIdConnectionHandle): TIdSipListeningConnection;
    procedure MessageSent(Msg: TIdSipMessage;
                          Handle: TIdConnectionHandle);
    procedure NotifyOfReceivedMessage(Msg: TIdSipMessage;
                                      Connection: TIdConnectionBindings);
    procedure NotifyOfSentMessage(Msg: TIdSipMessage;
                                  Connection: TIdConnectionBindings);
    procedure OOB(Handle: TIdConnectionHandle; ErrorCode: Integer);
    procedure Read(Handle: TIdConnectionHandle; ErrorCode: Integer);
    procedure RecordConnection(Socket: TIdSipSendingConnection;
                               Msg: TIdSipMessage;
                               Dest: TIdSipLocation); overload;
    procedure RecordConnection(Socket: TIdSipSendingConnection;
                               Msg: TIdSipMessage); overload;
    procedure ProcessSocketEvents;
    procedure ScheduleProcessingSocketEvents;
    procedure SendMessage(Msg: TIdSipMessage; Dest: TIdSipLocation);
    procedure SendMessageTo(Msg: TIdSipMessage;
                            Dest: TIdSipLocation);
    procedure SetListenQueue(NewValue: Integer);
    procedure SignalFailedConnect(Connection: TIdSipConnection; ErrorCode: Integer);
    procedure StartListening;
    procedure Write(Handle: TIdConnectionHandle; ErrorCode: Integer);
  public
    constructor Create(Bindings: TIdSipLocations;
                       TransportID: TRegisteredObjectID;
                       RemoteTQ: TRegisteredObjectID); reintroduce;
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
  IdException, IdIndyUtils, IdSipDns, IdTcpClient, Math, MessageQueues,
  DateUtils;

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

function GetLastError(S: TSocket): Integer;
{
var
  RC:       Integer;
  ValueLen: Integer;
}
begin
  Result := WSAGetLastError;
{
  // This should work, but doesn't. WSAEFAULT in WSAGetLastError!
  ValueLen := SizeOf(Result);
  RC := WinSock.getsockopt(S, SOL_SOCKET, SO_ERROR, @Result, ValueLen);
  if (RC = SOCKET_ERROR) then begin
    raise EIdSocketException.Create('getsockopt', WSAGetLastError);
  end;
}
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

  if (RC = SOCKET_ERROR) then begin
    raise EIdSocketException.Create('setsockopt', WSAGetLastError);
  end;
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

  // Server is self-starting.
  Self.Server := TIdSipTcpServer.Create(Self.Bindings, Self.ID, Self.Timer.ID);
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
  Wait: TIdSipTcpServerSendMessageWait;
begin
  Wait := TIdSipTcpServerSendMessageWait.Create;
  Wait.Msg      := Msg;
  Wait.ServerID := Self.Server.ID;

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

//* TIdSipTCPTransport Protected methods ***************************************

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
  Self.fConfigured  := true;
  Self.fCurrentMsg  := nil;
  Self.fHandle      := INVALID_SOCKET;
  Self.MessageQueue := CreateWindowHandle(Self.WndProc);

  // A sensible default value. Another value might be TlsTransport.
  Self.ReadTimeout := 500; // milliseconds
  Self.Transport   := TcpTransport;
end;

destructor TIdSipConnection.Destroy;
begin
  Self.Close;

  FreeAndNil(Self.fCurrentMsg);

  DestroyWindowHandle(Self.MessageQueue);
  Self.fBinding.Free;

  inherited Destroy;
end;

procedure TIdSipConnection.Close;
begin
  if (Self.Handle = INVALID_SOCKET) then Exit;

  // We don't care about the error:
  WinSock.closesocket(Self.Handle);
end;

function TIdSipConnection.Connected: Boolean;
begin
  Result := Self.fConnected;
end;

function TIdSipConnection.IsOutbound: Boolean;
begin
  Result := false;
end;

procedure TIdSipConnection.ProcessMessages;
var
  Msg: TMsg;
begin
  // We use async notifications. When something interesting happens to the
  // socket, WinSock signals that event through a Message. However, that doesn't
  // process the message. In some circumstances - like testing - it's useful to
  // force the processing of these messages.

  while ProcessMessage(Msg) do {loop};
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

//* TIdSipConnection Protected methods *****************************************

procedure TIdSipConnection.BeforeConnect;
begin
  // By default, do nothing.
end;

function TIdSipConnection.GetAdvertisedPort: TPortNum;
begin
  Result := Self.Binding.LocalPort;
end;

procedure TIdSipConnection.OnAccept(ErrorCode: Integer);
begin
  // By default do nothing.
end;

procedure TIdSipConnection.RewriteLocationHeaders(Msg: TIdSipMessage);
var
  Binding: TIdConnectionBindings;
begin
  // Outbound connections have an ephemeral local port, and we MUST be
  // prepared to accept connections at the port listed in a Via header
  // we generate (cf. RFC 3261, section 18.1.1).
  // For outbound connections, we rely on an external party (typically a
  // TIdSipTcpServer) to set AdvertisedPort; for inbound connections we already
  // have such a port, namely, Self.Binding.LocalPort.

  if Msg.LastHop.IsUnset then begin
    Binding := Self.Binding.Copy;
    try
      if Self.IsOutbound then begin
        Binding.LocalPort := Self.AdvertisedPort;
      end;

      Msg.RewriteLocationHeaders(Binding);
    finally
      Binding.Free;
    end;
  end;
end;

procedure TIdSipConnection.SetLocalBinding(Addr: TSockAddr; Binding: TIdConnectionBindings);
begin
  // TODO: Support IPv6!
  Binding.LocalIP   := TIdIPAddressParser.IPv4AddressToStr(ntohl(Addr.sin_addr.S_addr));
  Binding.LocalPort := ntohs(Addr.sin_port);
end;

procedure TIdSipConnection.SetPeerBinding(Addr: TSockAddr; Binding: TIdConnectionBindings);
begin
  // TODO: Support IPv6!
  Binding.PeerIP   := TIdIPAddressParser.IPv4AddressToStr(ntohl(Addr.sin_addr.S_addr));
  Binding.PeerPort := ntohs(Addr.sin_port);
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

  case Message.SelectEvent of
    FD_ACCEPT:  Self.OnAccept(ErrorCode);
    FD_CONNECT: Self.OnConnect(ErrorCode);
    FD_CLOSE:   Self.OnDisconnect(ErrorCode);
    FD_OOB:     Self.OnOOB(ErrorCode);
    FD_READ:    Self.OnRead(ErrorCode);
    FD_WRITE:   Self.OnWrite(ErrorCode);
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

function TIdSipConnection.GetTransport: String;
begin
  Result := Self.Binding.Transport;
end;

function TIdSipConnection.InternalBufferLength: Integer;
begin
  Result := Length(Self.InternalBuffer);
end;

procedure TIdSipConnection.OnConnect(ErrorCode: Integer);
var
  Wait: TIdSipTcpServerConnectWait;
begin
  Self.fConnected := true;

  Self.BeforeConnect;

  Wait := TIdSipTcpServerConnectWait.Create;
  Wait.Handle     := Self.Handle;
  Wait.ErrorCode  := ErrorCode;
  Wait.ServerID   := Self.ServerID;

  TIdTimerQueue.DispatchEvent(Self.ServerID, TriggerImmediately, Wait);
end;

procedure TIdSipConnection.OnDisconnect(ErrorCode: Integer);
var
  Wait: TIdSipTcpServerDisconnectWait;
begin
  Self.fConnected := false;

  Wait := TIdSipTcpServerDisconnectWait.Create;
  Wait.ErrorCode  := ErrorCode;
  Wait.Handle     := Self.Handle;
  Wait.ServerID   := Self.ServerID;
  TIdTimerQueue.DispatchEvent(Self.ServerID, TriggerImmediately, Wait);
end;                                           

procedure TIdSipConnection.OnMessageSent(Msg: TIdSipMessage);
var
  Wait: TIdSipTcpServerConnectionSentMsgWait;
begin
  Wait := TIdSipTcpServerConnectionSentMsgWait.Create;
  Wait.Handle     := Self.Handle;
  Wait.Msg        := Msg;
  Wait.ServerID   := Self.ServerID;
  TIdTimerQueue.DispatchEvent(Self.ServerID, TriggerImmediately, Wait);
end;

procedure TIdSipConnection.OnOOB(ErrorCode: Integer);
var
  Wait: TIdSipTcpServerOOBWait;
begin
  Wait := TIdSipTcpServerOOBWait.Create;
  Wait.ErrorCode  := ErrorCode;
  Wait.Handle     := Self.Handle;
  Wait.ServerID   := Self.ServerID;
  TIdTimerQueue.DispatchEvent(Self.ServerID, TriggerImmediately, Wait);
end;

procedure TIdSipConnection.OnRead(ErrorCode: Integer);
var
  Wait: TIdSipTcpServerReadWait;
begin
  Wait := TIdSipTcpServerReadWait.Create;
  Wait.ErrorCode  := ErrorCode;
  Wait.Handle     := Self.Handle;
  Wait.ServerID   := Self.ServerID;
  TIdTimerQueue.DispatchEvent(Self.ServerID, TriggerImmediately, Wait);
end;

procedure TIdSipConnection.OnWrite(ErrorCode: Integer);
var
  Wait: TIdSipTcpServerWriteWait;
begin
  // The socket's signalled that it's now ready to send data.

  Wait := TIdSipTcpServerWriteWait.Create;
  Wait.ErrorCode  := ErrorCode;
  Wait.Handle     := Self.Handle;
  Wait.ServerID   := Self.ServerID;
  TIdTimerQueue.DispatchEvent(Self.ServerID, TriggerImmediately, Wait);
end;

procedure TIdSipConnection.PopulateSockAddr(Location: TIdSipLocation; var Addr: TSockAddrIn);
begin
  Addr.sin_family := Self.ProtocolType(Location.IPAddress);
  Addr.sin_addr.S_addr := htonl(TIdIPAddressParser.InetAddr(Location.IPAddress));
  Addr.sin_port := htons(u_short(Location.Port and $ffff)); // TODO CLEANUP: bitmask shouldn't be necessary! TPortNum->u_short!
end;

function TIdSipConnection.ProcessMessage(var Msg: TMsg): Boolean;
begin
  // Liberally adapted from TApplication.ProcessMessage.
  Result := false;
  if PeekMessage(Msg, 0, CM_SOCKETMESSAGE, CM_SOCKETMESSAGE, PM_REMOVE) then
  begin
    Result := true;
    DispatchMessage(Msg);
  end;
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

procedure TIdSipConnection.RaiseExceptionExceptWhen(FunctionName: String; AcceptableError: Integer);
begin
  Self.RaiseExceptionExceptWhen(FunctionName, [AcceptableError]);
end;

procedure TIdSipConnection.RaiseExceptionExceptWhen(FunctionName: String; AcceptableErrors: array of Integer);
var
  Acceptable: Boolean;
  Error:      Integer;
  I:          Integer;
begin
  Error := GetLastError(Self.Handle);

  Acceptable := false;
  for I := Low(AcceptableErrors) to High(AcceptableErrors) do begin
    if (Error = AcceptableErrors[I]) then begin
      Acceptable := true;
      Break;
    end;
  end;

  if (not Acceptable) then begin
{
    case Error of
      WSAETIMEDOUT: raise EIdReadTimeout.Create('Read timeout on socket ' + Self.Binding.AsString);
      WSAECONNRESET:
      WSAECONNABORTED:
      WSAESHUTDOWN:
      on EIdConnClosedGracefully do;
      on EIdClosedSocket do;
    else
      raise EIdSocketException.Create('send', Error);
    end;
}
    raise EIdSocketException.Create('send', Error);
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
  StartTime:      TDateTime;
begin
  // Read ByteCount number of bytes from the socket/internal buffer and write
  // them to Dest.

  StartTime := Now;

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

      if (MilliSecondOf(Now - StartTime) > Self.ReadTimeout) then
        raise EIdReadTimeout.Create('Read timeout on socket ' + Self.Binding.AsString);
    until (BytesRemaining <= 0);
  end;
end;

function TIdSipConnection.Recv: String;
  function Store(Buf: array of Char; Count: Integer): String;
  begin
    Result := Copy(Buf, 1, Count);

    Self.InternalBuffer := Self.InternalBuffer + Result;
  end;
const
  BufLen = 1024;
var
  Buf:       array[0..BufLen - 1] of Char;
  BytesRead: Integer;
  ErrorCode: Integer;
begin
  // Read all we can off the socket. Call this S. Store S in the internal
  // buffer, and return S.
  Result := '';

  BytesRead := WinSock.recv(Self.Handle, Buf, BufLen, 0);
  if (BytesRead = SOCKET_ERROR) then begin
    ErrorCode := GetLastError(Self.Handle);
    case ErrorCode of
      WSAEMSGSIZE: begin
        // There's still unread data!
        Result := Store(Buf, BytesRead) + Self.Recv;
      end;
      WSAEWOULDBLOCK:;
      WSAETIMEDOUT: raise EIdReadTimeout.Create('Read timeout on socket ' + Self.Binding.AsString);
    else
      raise EIdSocketException.Create('recv', Error);
    end;
  end else begin
    Result := Store(Buf, BytesRead);
  end;
end;

procedure TIdSipConnection.Select;
var
  RC: Integer;
begin
  RC := WSAAsyncSelect(Self.Handle, Self.MessageQueue, CM_SOCKETMESSAGE, FD_ACCEPT or FD_CLOSE or FD_CONNECT or FD_READ or FD_WRITE);
  if (RC = SOCKET_ERROR) then begin
      raise EIdSocketException.Create('WSAAsyncSelect', GetLastError(Self.Handle));
  end;
end;

procedure TIdSipConnection.SetCurrentMsg(Msg: TIdSipMessage);
begin
  FreeAndNil(Self.fCurrentMsg);
  Self.fCurrentMsg := Msg;
end;

procedure TIdSipConnection.SetTransport(Value: String);
begin
  Self.Binding.Transport := Value;
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
//* TIdSipSendingConnection                                                    *
//******************************************************************************
//* TIdSipSendingConnection Public methods *************************************

constructor TIdSipSendingConnection.Create;
begin
  inherited Create;

  Self.SendQueue := TObjectList.Create(true);
end;

destructor TIdSipSendingConnection.Destroy;
begin
  Self.SendQueue.Free;

  inherited Destroy;
end;

function TIdSipSendingConnection.PartialSend: Boolean;
var
  BytesWritten: Integer;
  CompletedMsg: Boolean;
begin
  // Return true iff we have nothing at all to send.
  // Notify when we finish sending a completed message.

  Result := false;

  if (not Self.CheckConnected) then Exit;

  // Our "owner" has signaled that it needs to configure some of our properties
  // after we've connected. We have connected, but they haven't finished
  // configuring us.
  if (not Self.Configured) then begin
    Exit;
  end;

  // Nothing to do?
  if (Self.SendBuffer = '') and not Assigned(Self.CurrentMsg) then begin
    Result := true;
    Exit;
  end;

  if not Assigned(Self.CurrentMsg) then begin
    Self.SetCurrentMsg(Self.GetNextMessage);
  end;

  Self.RewriteLocationHeaders(Self.CurrentMsg);
  Self.SendBuffer := Self.CurrentMsg.AsString;

  Assert(Length(Self.SendBuffer) > 0, 'No data to send');
  repeat
    BytesWritten := WinSock.send(Self.Handle, Self.SendBuffer[1], Self.SendBufferLength, 0);
    Delete(Self.SendBuffer, 1, BytesWritten)
  until (BytesWritten = 0) or (BytesWritten = SOCKET_ERROR) or (Self.SendBuffer = '');

  CompletedMsg := Self.SendBuffer = '';
  Result := CompletedMsg and (Self.SendQueue.Count = 0);

  if (BytesWritten = SOCKET_ERROR) then begin
    Self.RaiseExceptionExceptWhen('send', [WSAENOTCONN, WSAENOBUFS]);
  end;

  if CompletedMsg then begin
    Self.OnMessageSent(Self.CurrentMsg);
  end;
end;

procedure TIdSipSendingConnection.Queue(Msg: TIdSipMessage);
begin
  // Queue up the message for sending, but nothing more. That way a caller can
  // happily say
  // Connect;
  // Send(Msg1);
  // Send(Msg2);
  // Send(Msg3);
  // and still adjust the connection as needed in the OnConnect notification
  // such that this connection may still rewrite the location headers of Msg1,
  // Msg2, Msg3 just before the messages are serialised to the socket.

  Self.SendQueue.Add(Msg.Copy);

  // If we still have to connect, we need the "current message" to be
  // the message that has still yet to be sent.
    Self.SetCurrentMsg(Self.GetNextMessage);

  Self.PartialSend;
end;

//* TIdSipSendingConnection Private methods *************************************

function TIdSipSendingConnection.CheckConnected: Boolean;
begin
  Result := true;

  if (not Self.Connected) then begin
    // Maybe someone's trying to send data straight after connecting. If so, the
    // FD_CONNECT may not yet have been processed.
    Self.ProcessMessages;

    if (not Self.Connected) then begin
      // No FD_CONNECT. This socket's not connected.
      //
      // We return FALSE because there might be data buffered, but we're not yet
      // in a position to send the data.
      Result := false;
    end;
  end;
end;

function TIdSipSendingConnection.GetNextMessage: TIdSipMessage;
begin
  Result := TIdSipMessage(Self.SendQueue[0]).Copy;
  Self.SendQueue.Delete(0);
end;

function TIdSipSendingConnection.SendBufferLength: Integer;
begin
  Result := Length(Self.SendBuffer);
end;

//******************************************************************************
//* TIdSipClientConnection                                                     *
//******************************************************************************
//* TIdSipClientConnection Public methods **************************************

procedure TIdSipClientConnection.Connect(PeerIP: String; PeerPort: TPortNum; Timeout: Cardinal);
var
  L: TIdSipLocation;
begin
  L := TIdSipLocation.Create(TcpTransport, PeerIP, PeerPort);
  try
    Self.Connect(L, Timeout);
  finally
    L.Free;
  end;
end;

procedure TIdSipClientConnection.Connect(Location: TIdSipLocation; Timeout: Cardinal);
var
  RemoteAddr: TSockAddr;
  RC:         Integer;
begin
  Self.Binding.Transport := Self.Transport;
  Self.PopulateSockAddr(Location, RemoteAddr);
  Self.SetPeerBinding(RemoteAddr, Self.Binding);

  Self.fHandle := WinSock.socket(RemoteAddr.sin_family, SOCK_STREAM, IPPROTO_TCP);
  if (Self.fHandle = INVALID_SOCKET) then
    raise EIdSocketException.Create('socket', GetLastError(Self.Handle));

  SetSocketKeepAlive(Self.Handle, Self.KeepAlive);
  Self.Select;

  RC := WinSock.connect(Self.Handle, RemoteAddr, SizeOf(RemoteAddr));
  if (RC <> 0) then begin
    Self.RaiseExceptionExceptWhen('connect', WSAEWOULDBLOCK);
  end;
  Self.ScheduleConnectCancellation(Timeout);
end;

function TIdSipClientConnection.IsOutbound: Boolean;
begin
  Result := true;
end;

procedure TIdSipClientConnection.SetAdvertisedPort(Value: TPortNum);
begin
  Self.fAdvertisedPort := Value;
end;

//* TIdSipClientConnection Protected methods ***********************************

procedure TIdSipClientConnection.BeforeConnect;
var
  LocalAddr:    TSockAddr;
  LocalAddrLen: Integer;
  RC:           Integer;
begin
  inherited BeforeConnect;

  RC := WinSock.getsockname(Self.Handle, LocalAddr, LocalAddrLen);
  if (RC <> 0) then
    EIdSocketException.Create('getsockname', GetLastError(Self.Handle));

  Self.SetLocalBinding(LocalAddr, Self.Binding);
  Self.Binding.Transport := TcpTransport;
end;

function TIdSipClientConnection.GetAdvertisedPort: TPortNum;
begin
  Result := Self.fAdvertisedPort;
end;

//* TIdSipClientConnection Private methods *************************************

procedure TIdSipClientConnection.ScheduleConnectCancellation(Timeout: Cardinal);
var
  Wait: TIdSipTcpConnectionCancelWait;
begin
  Wait := TIdSipTcpConnectionCancelWait.Create;
  Wait.ServerID := Self.ServerID;
  Wait.Handle   := Self.Handle;

  TIdTimerQueue.DispatchEvent(Self.ServerID, Timeout, Wait);
end;

//******************************************************************************
//* TIdSipServerConnection                                                     *
//******************************************************************************
//* TIdSipServerConnection Public methods **************************************

constructor TIdSipServerConnection.Create(Handle: TSocket; Binding: TIdConnectionBindings);
begin
  inherited Create;

  // We just accept()ed a connection. We're definitely Connected. (And if the
  // remote end rapidly disconnects, we'll pick up an FD_DISCONNECT and adjust
  // accordingly.)
  Self.fConnected := true;

  Self.fHandle := Handle;
  Self.Binding.Assign(Binding);

  Self.Select;
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
  ErrorCode:     Integer;
  NewConnection: TSocket;
  PeerAddr:      TSockAddr;
  PeerAddrLen:   Integer;
begin
  Result := nil;

  if (Self.Handle = INVALID_SOCKET) then
    raise Exception.Create('Call Bind() before Accept()');

  PeerAddrLen := SizeOf(PeerAddr);
  // TODO: Does this support IPv6?
  NewConnection := WinSock.accept(Self.Handle, @PeerAddr, @PeerAddrLen);
  if (NewConnection = INVALID_SOCKET) then begin
    ErrorCode := GetLastError(Self.Handle);

    if (ErrorCode = WSAEWOULDBLOCK) then begin
      // Somebody called Accept without first receiving an FD_ACCEPT: nothing
      // to do here, move along.
      Exit;
    end
    else begin
      // Possibly the connection disappeared after we received the FD_ACCEPT
      // but before we called accept().
      raise EIdSocketException.Create('accept', ErrorCode);
    end;
  end
  else begin
    Binding := TIdConnectionBindings.Create;
    try
      Binding.Transport := Self.Binding.Transport;
      Binding.LocalIP   := Self.ServerLocation.IPAddress;
      Binding.LocalPort := Self.ServerLocation.Port;

      Self.SetPeerBinding(PeerAddr, Binding);

      Result := TIdSipServerConnection.Create(NewConnection, Binding);
    finally
      Binding.Free;
    end;
  end;
end;

procedure TIdSipListeningConnection.Listen(IP: String; Port: TPortNum; QueueLength: Cardinal);
var
  L: TIdSipLocation;
begin
  L := TIdSipLocation.Create(TcpTransport, IP, Port);
  try
    Self.Listen(L, QueueLength);
  finally
    L.Free;
  end;
end;

procedure TIdSipListeningConnection.Listen(Location: TIdSipLocation; QueueLength: Cardinal);
var
  RC: Integer;
begin
  Self.Bind(Location);

  RC := WinSock.listen(Self.Handle, QueueLength);
  if (RC <> 0) then begin
    raise EIdSocketException.Create(Self.ServerLocation.AsCompactString + ': listen', GetLastError(Self.Handle));
  end;
  Self.Select;
end;

//* TIdSipListeningConnection Protected methods ********************************

procedure TIdSipListeningConnection.OnAccept(ErrorCode: Integer);
var
  Wait: TIdSipTcpServerAcceptWait;
begin
  Wait := TIdSipTcpServerAcceptWait.Create;
  Wait.ErrorCode  := ErrorCode;
  Wait.Handle     := Self.Handle;
  Wait.ServerID   := Self.ServerID;

  TIdTimerQueue.DispatchEvent(Self.ServerID, TriggerImmediately, Wait);
end;

//* TIdSipListeningConnection Private methods **********************************

procedure TIdSipListeningConnection.Bind(Location: TIdSipLocation);
var
  A:  TSockAddrIn;
  RC: Integer;
begin
  Self.ServerLocation := Location.Copy;

  Self.PopulateSockAddr(Location, A);

  Self.fHandle := WinSock.socket(A.sin_family, SOCK_STREAM, IPPROTO_TCP);
  if (Self.fHandle = INVALID_SOCKET) then
    raise EIdSocketException.Create('socket', GetLastError(Self.Handle));

  SetSocketReuseAddr(Self.Handle, true);
  SetSocketKeepAlive(Self.Handle, Self.KeepAlive);

  RC := WinSock.bind(Self.Handle, A, SizeOf(A));
  if (RC <> 0) then
    raise EIdSocketException.Create('bind', GetLastError(Self.Handle));

  Self.SetLocalBinding(A, Self.Binding);
end;

//******************************************************************************
//* TIdSipConnectionTableEntry                                                 *
//******************************************************************************
//* TIdSipConnectionTableEntry Public methods **********************************

constructor TIdSipConnectionTableEntry.Create(Connection: TIdSipSendingConnection;
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

function TIdSipConnectionTable.Add(Connection: TIdSipSendingConnection;
                                   Request:  TIdSipRequest): TIdSipConnectionTableEntry;
begin
  Result := Self.FindEntry(Connection, Request);

  if not Assigned(Result) then begin
    Result := TIdSipConnectionTableEntry.Create(Connection, Request);
    Self.List.Add(Result);
  end;
end;

function TIdSipConnectionTable.AnyConnectionTo(Destination: TIdSipLocation): TIdSipSendingConnection;
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

function TIdSipConnectionTable.ConnectionEntry(Connection: TIdSipSendingConnection): TIdSipConnectionTableEntry;
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

function TIdSipConnectionTable.ConnectionFor(Msg: TIdSipMessage): TIdSipSendingConnection;
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

function TIdSipConnectionTable.ConnectionFor(S: TSocket): TIdSipSendingConnection;
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

function TIdSipConnectionTable.ConnectionFor(Destination: TIdConnectionBindings): TIdSipSendingConnection;
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

procedure TIdSipConnectionTable.Remove(Connection: TIdSipSendingConnection);
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
//* TIdSipTcpServerWait                                                        *
//******************************************************************************
//* TIdSipTcpServerWait Public methods *****************************************

procedure TIdSipTcpServerWait.Trigger;
begin
  TIdObjectRegistry.Singleton.WithExtantObjectDo(Self.ServerID, Self.Run);
end;

//* TIdSipTcpServerWait Private methods ****************************************

procedure TIdSipTcpServerWait.Run(O: TObject);
begin
  if not (O is TIdSipTcpServer) then Exit;

  Self.RunWait(O as TIdSipTcpServer);
end;

//* TIdSipTcpServerWait Protected methods **************************************

procedure TIdSipTcpServerWait.RunWait(S: TIdSipTcpServer);
begin
  // By default, do nothing.
end;

//******************************************************************************
//* TIdSipTcpServerAcceptWait                                                  *
//******************************************************************************
//* TIdSipTcpServerAcceptWait Protected methods ********************************

procedure TIdSipTcpServerAcceptWait.RunWait(S: TIdSipTcpServer);
begin
  S.Accept(Self.Handle, Self.ErrorCode);
end;

//******************************************************************************
//* TIdSipTcpConnectionCancelWait                                              *
//******************************************************************************
//* TIdSipTcpConnectionCancelWait Public methods *******************************

procedure TIdSipTcpConnectionCancelWait.RunWait(S: TIdSipTcpServer);
begin
  S.CancelConnect(Self.Handle);
end;

//******************************************************************************
//* TIdSipTcpServerConnectionSentMsgWait                                       *
//******************************************************************************
//* TIdSipTcpServerConnectionSentMsgWait Public methods ************************

constructor TIdSipTcpServerConnectionSentMsgWait.Create;
begin
  inherited Create;

  // We will never use this instance; it's there so other objects can always
  // safely call the Msg property.
  Self.fMsg := TIdSipMessage.Create;
end;

destructor TIdSipTcpServerConnectionSentMsgWait.Destroy;
begin
  Self.fMsg.Free;

  inherited Destroy;
end;

//* TIdSipTcpServerConnectionSentMsgWait Protected methods *********************

procedure TIdSipTcpServerConnectionSentMsgWait.RunWait(S: TIdSipTcpServer);
begin
  S.MessageSent(Self.Msg, Self.Handle);
end;

//* TIdSipTcpServerConnectionSentMsgWait Private methods ***********************

procedure TIdSipTcpServerConnectionSentMsgWait.SetMsg(Value: TIdSipMessage);
begin
  Self.fMsg.Free;
  Self.fMsg := Value.Copy;
end;

//******************************************************************************
//* TIdSipTcpServerConnectWait                                                 *
//******************************************************************************
//* TIdSipTcpServerConnectWait Protected methods *******************************

procedure TIdSipTcpServerConnectWait.RunWait(S: TIdSipTcpServer);
begin
  S.Connect(Self.Handle, Self.ErrorCode);
end;

//******************************************************************************
//* TIdSipTcpServerDisconnectWait                                              *
//******************************************************************************
//* TIdSipTcpServerDisconnectWait Protected methods ****************************

procedure TIdSipTcpServerDisconnectWait.RunWait(S: TIdSipTcpServer);
begin
  S.Disconnect(Self.Handle, Self.ErrorCode);
end;

//******************************************************************************
//* TIdSipTcpServerOOBWait                                                     *
//******************************************************************************
//* TIdSipTcpServerOOBWait Protected methods ***********************************

procedure TIdSipTcpServerOOBWait.RunWait(S: TIdSipTcpServer);
begin
  S.OOB(Self.Handle, Self.ErrorCode);
end;

//******************************************************************************
//* TIdSipTcpServerReadWait                                                    *
//******************************************************************************
//* TIdSipTcpServerReadWait Protected methods **********************************

procedure TIdSipTcpServerReadWait.RunWait(S: TIdSipTcpServer);
begin
  S.Read(Self.Handle, Self.ErrorCode);
end;

//******************************************************************************
//* TIdSipTcpServerWriteWait                                                   *
//******************************************************************************
//* TIdSipTcpServerWriteWait Protected methods *********************************

procedure TIdSipTcpServerWriteWait.RunWait(S: TIdSipTcpServer);
begin
  S.Write(Self.Handle, Self.ErrorCode);
end;

//******************************************************************************
//* TIdSipTcpServerSendMessageWait                                             *
//******************************************************************************
//* TIdSipTcpServerSendMessageWait Public methods ******************************

constructor TIdSipTcpServerSendMessageWait.Create;
begin
  inherited Create;

  Self.fDestination := TIdSipLocation.Create;
  Self.fMsg         := TIdSipMessage.Create;
end;

destructor TIdSipTcpServerSendMessageWait.Destroy;
begin
  Self.fMsg.Free;
  Self.fDestination.Free;

  inherited Destroy;
end;

//* TIdSipTcpServerSendMessageWait Protected methods ***************************

procedure TIdSipTcpServerSendMessageWait.RunWait(S: TIdSipTcpServer);
begin
  S.SendMessage(Self.Msg, Self.Destination);
end;

//* TIdSipTcpServerSendMessageWait Private methods *****************************

procedure TIdSipTcpServerSendMessageWait.SetDestination(Value: TIdSipLocation);
begin
  Self.fDestination.Assign(Value);
end;

procedure TIdSipTcpServerSendMessageWait.SetMsg(Value: TIdSipMessage);
begin
  Self.fMsg.Free;

  Self.fMsg := Value.Copy;
end;

//******************************************************************************
//* TIdSipTcpServerStartWait                                                   *
//******************************************************************************
//* TIdSipTcpServerStartWait Protected methods *********************************

procedure TIdSipTcpServerStartWait.RunWait(S: TIdSipTcpServer);
begin
  S.StartListening;
end;

//******************************************************************************
//* TIdSipTcpServerSetListenQueueWait                                          *
//******************************************************************************
//* TIdSipTcpServerSetListenQueueWait Protected methods ************************

procedure TIdSipTcpServerSetListenQueueWait.RunWait(S: TIdSipTcpServer);
begin
  S.SetListenQueue(Self.ListenQueue);
end;

//******************************************************************************
//* TIdSipProcessSocketEventsWait                                              *
//******************************************************************************
//* TIdSipProcessSocketEventsWait Protected methods ****************************

procedure TIdSipProcessSocketEventsWait.RunWait(S: TIdSipTcpServer);
begin
  S.ProcessSocketEvents;
end;

//******************************************************************************
//* TIdSipTcpServer                                                            *
//******************************************************************************
//* TIdSipTcpServer Public methods *********************************************

constructor TIdSipTcpServer.Create(Bindings: TIdSipLocations;
                                   TransportID: TRegisteredObjectID;
                                   RemoteTQ: TRegisteredObjectID);
begin
  inherited Create(true);

  Self.Bindings         := TIdSipLocations.Create;
  Self.Clients          := TObjectList.Create(true);
  Self.ConnectionMap    := TIdSipConnectionTable.Create;
  Self.ConnectTimeout   := 5000;
  Self.ListeningSockets := TObjectList.Create(true);

  Self.Bindings.AddLocations(Bindings);

  Self.ListenQueue := 5;
  Self.RemoteTQ    := RemoteTQ;
  Self.TransportID := TransportID;

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
  Wait.ServerID := Self.ID;
  Self.AddEvent(TriggerImmediately, Wait);
end;

//* TIdSipTcpServer Private methods ********************************************

procedure TIdSipTcpServer.Accept(ListeningSocket: TIdConnectionHandle; ErrorCode: Integer);
var
  Connection:    TIdSipListeningConnection;
  NewConnection: TIdSipServerConnection;
begin
  Connection := Self.ListeningSocket(ListeningSocket);

  if not Assigned(Connection) then Exit; // No such socket? Something whacked is going on.

  NewConnection := Connection.Accept;
  NewConnection.ServerID := Self.ID;
  Self.Clients.Add(NewConnection);
end;

function TIdSipTcpServer.AddSocket(L: TObjectList; SocketType: TIdSipConnectionClass): TIdSipConnection;
begin
  Result := SocketType.Create;
  Result.ServerID := Self.ID;
  L.Add(Result);
end;

function TIdSipTcpServer.BindingCount: Integer;
begin
  Result := Self.Bindings.Count;
end;

procedure TIdSipTcpServer.CancelConnect(Handle: TIdConnectionHandle);
var
  Connection: TIdSipConnection;
begin
  Connection := Self.ClientFor(Handle);

  if not Assigned(Connection) then Exit;

  if not Connection.Connected then begin
    Connection.Close;
    Self.SignalFailedConnect(Connection, WSAETIMEDOUT);
  end;
end;

function TIdSipTcpServer.ClientFor(Handle: TIdConnectionHandle): TIdSipSendingConnection;
var
  I: Integer;
  S: TIdSipSendingConnection;
begin
  Result := nil;

  for I := 0 to Self.Clients.Count - 1 do begin
    S := TIdSipSendingConnection(Self.Clients[I]);
    if (S.Handle = Handle) then begin
      Result := S;
      Break;
    end;
  end;
end;

procedure TIdSipTcpServer.Connect(Handle: TIdConnectionHandle; ErrorCode: Integer);
var
  Connection: TIdSipSendingConnection;
begin
  // The Connection described by SocketDescription has just connected.

  Connection := Self.ClientFor(Handle);

  if not Assigned(Connection) then begin
    // We shouldn't ever reach here - it means that a socket we never
    // created has reported to us!
    Exit;
  end;

  if (ErrorCode = 0) then begin
    // Outbound connections have an ephemeral local port, and we MUST be
    // prepared to accept connections at the port listed in a Via header
    // we generate (cf. RFC 3261, section 18.1.1). We don't really care which
    // binding's port we use, as long as the port's on the same IP address.
    if Connection.IsOutbound then
      (Connection as TIdSipClientConnection).SetAdvertisedPort(Self.FirstServerPortFor(Connection.Binding.LocalIP));

    // Yes, we violate encapsulation here. That's because Self is the thread in
    // which we're executing, and it's only at this point that we can synchronise
    // state safely.
    Connection.Configured := true;

    // We may already have data queued up for sending.
    Connection.PartialSend;
  end
  else begin
    // The connection attempt failed!
    Self.SignalFailedConnect(Connection, ErrorCode);
  end;
end;

procedure TIdSipTcpServer.Disconnect(Handle: TIdConnectionHandle; ErrorCode: Integer);
var
  Connection: TIdSipSendingConnection;
begin
  Connection := Self.ConnectionMap.ConnectionFor(Handle);

  Self.ConnectionMap.Remove(Connection);
  Self.Clients.Remove(Connection);
end;

function TIdSipTcpServer.FirstServerPortFor(IPAddress: String): TPortNum;
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

function TIdSipTcpServer.ListeningSocket(Handle: TIdConnectionHandle): TIdSipListeningConnection;
var
  I: Integer;
  L: TIdSipListeningConnection;
begin
  Result := nil;

  for I := 0 to Self.ListeningSockets.Count - 1 do begin
    L := TIdSipListeningConnection(Self.ListeningSockets[I]);
    if (L.Handle = Handle) then begin
      Result := L;
      Break;
    end;
  end;
end;

procedure TIdSipTcpServer.MessageSent(Msg: TIdSipMessage;
                                      Handle: TIdConnectionHandle);
var
  Connection: TIdSipSendingConnection;
begin
  // We can only associate an outbound connection with a message now. We use an
  // asynchronous connect, so we only know the "location" data - Contact, etc. -
  // when we actually send the message.
  //
  // That's when this method's called.
  //
  // We will have already associated a message with an inbound connection
  // because, assuming the remote party hasn't reset its connection, Accept will
  // return a valid socket handle. We immediately read a message, and store the
  // association in our ConnectionMap.

  Connection := Self.ClientFor(Handle);

  if not Assigned(Connection) then Exit;

  Self.RecordConnection(Connection, Msg);
  Self.NotifyOfSentMessage(Msg, Connection.Binding);
end;

procedure TIdSipTcpServer.NotifyOfReceivedMessage(Msg: TIdSipMessage;
                                                  Connection: TIdConnectionBindings);
var
  Wait: TIdSipReceiveMessageWait;
begin
  Wait := TIdSipReceiveMessageWait.Create;
  Wait.Message      := Msg.Copy;
  Wait.ReceivedFrom := Connection;
  Wait.TransportID  := Self.TransportID;

  Self.AddRemoteEvent(Self.RemoteTQ, TriggerImmediately, Wait);
end;

procedure TIdSipTcpServer.NotifyOfSentMessage(Msg: TIdSipMessage;
                                              Connection: TIdConnectionBindings);
var
  Wait: TIdSipSentMessageWait;
begin
  Wait := TIdSipSentMessageWait.Create;
  Wait.Message     := Msg.Copy;
  Wait.Destination := Connection;
  Wait.TransportID := Self.TransportID;

  Self.DispatchEvent(Self.RemoteTQ, TriggerImmediately, Wait);
end;

procedure TIdSipTcpServer.OOB(Handle: TIdConnectionHandle; ErrorCode: Integer);
begin
  // TODO: For now, do nothing. But otherwise?
end;

procedure TIdSipTcpServer.Read(Handle: TIdConnectionHandle; ErrorCode: Integer);
var
  AbnormalTermination: Boolean;
  Connection:          TIdSipSendingConnection;
  Msg:                 TIdSipMessage;
begin
  Connection := Self.ClientFor(Handle);

  if Assigned(Connection) then begin
    Msg := Connection.Receive(AbnormalTermination);
    if Assigned(Msg) then begin
        Self.RecordConnection(Connection, Msg);

      // Always tell the TCP transport of the received message, even if
      // malformed (say, because Content-Length has a positive value and we
      // timed out reading the message's body).
      Self.NotifyOfReceivedMessage(Msg, Connection.Binding);
    end;
    // TODO: if AbnormalTermination then ???
  end;
end;

procedure TIdSipTcpServer.RecordConnection(Socket: TIdSipSendingConnection;
                                           Msg: TIdSipMessage;
                                           Dest: TIdSipLocation);
var
  FakeRequest: TIdSipRequest;
begin
  // We have read a message Msg off Socket. If Msg is a request, fine: record
  // the (Msg, Socket) pair in our connection map.
  //
  // If Msg is a response, either we're sending one down a new connection, or
  // have received a response on a connection that we've just accepted.
  //
  // In the former case, we want to be able to send responses to the same
  // request down the same socket, so we add a fake request to simulate having
  // received a request on Socket.
  //
  // In the latter case, er, I don't know. Fake it.

  // If Msg is a response, what then? The connection will get used just for
  // that one response, and not again.
  if Msg.IsRequest then
    Self.ConnectionMap.Add(Socket, Msg as TIdSipRequest)
  else begin
    // This is really crap! What a hack!
    FakeRequest := TIdSipRequest.Create;
    try
      // Allow us to disambiguate the ConnectionEntry objects so we get the
      // right result from ConnectionFor.
      FakeRequest.AddHeader(ViaHeaderFull);
      FakeRequest.RewriteLocationHeaders(Dest);
      Self.ConnectionMap.Add(Socket, FakeRequest)
    finally
      FakeRequest.Free;
    end;
  end;
end;

procedure TIdSipTcpServer.RecordConnection(Socket: TIdSipSendingConnection;
                                           Msg: TIdSipMessage);
var
  Peer: TIdSipLocation;
begin
  Peer := TIdSipLocation.CreatePeerLocation(Socket.Binding);
  try
    Self.RecordConnection(Socket, Msg, Peer);
  finally
    Peer.Free;
  end;
end;

procedure TIdSipTcpServer.ProcessSocketEvents;
  procedure ProcessSequentially(L: TObjectList);
  var
    I: Integer;
  begin
    for I := 0 to L.Count - 1 do begin
      TIdSipConnection(L[I]).ProcessMessages;
    end;
  end;
begin
  // For each socket, process every queued message before proceeding to the next
  // socket.
  ProcessSequentially(Self.ListeningSockets);
  ProcessSequentially(Self.Clients);

  Self.ScheduleProcessingSocketEvents;
end;

procedure TIdSipTcpServer.ScheduleProcessingSocketEvents;
var
  Wait: TIdSipProcessSocketEventsWait;
begin
  Wait := TIdSipProcessSocketEventsWait.Create;
  Wait.ServerID := Self.ID;
  Self.AddEvent(500, Wait);
end;

procedure TIdSipTcpServer.SendMessage(Msg: TIdSipMessage; Dest: TIdSipLocation);
var
  Connection:  TIdSipSendingConnection;
begin
  // Try send the response down the same connection on which we received the
  // request.
  Connection := Self.ConnectionMap.ConnectionFor(Msg);

  // Otherwise, try find an existing connection to Dest.
  if not Assigned(Connection) then
    Connection := Self.ConnectionMap.AnyConnectionTo(Dest);

  if Assigned(Connection) and Connection.Connected then begin
    Connection.Queue(Msg);
  end
  else begin
    // Last resort: make a new connection to Dest.
    Self.SendMessageTo(Msg, Dest);
  end;
end;

procedure TIdSipTcpServer.SendMessageTo(Msg: TIdSipMessage;
                                        Dest: TIdSipLocation);
var
  NewConnection: TIdSipClientConnection;
begin
  NewConnection := Self.AddSocket(Self.Clients, TIdSipClientConnection) as TIdSipClientConnection;
  NewConnection.Configured := false;
  NewConnection.Connect(Dest.IPAddress, Dest.Port, Self.ConnectTimeout);

  NewConnection.Queue(Msg);
end;

procedure TIdSipTcpServer.SetListenQueue(NewValue: Integer);
begin
  Self.ListenQueue := NewValue;
end;

procedure TIdSipTcpServer.SignalFailedConnect(Connection: TIdSipConnection; ErrorCode: Integer);
var
  Wait: TIdSipMessageExceptionWait;
begin
  Wait := TIdSipMessageExceptionWait.Create;
  Wait.ExceptionMessage := TranslateError(ErrorCode);
  Wait.ExceptionType    := EIdSocketException;
  Wait.FailedMessage    := Connection.CurrentMsg;
  Wait.Reason           := TranslateError(ErrorCode);
  Wait.TransportID      := Self.TransportID;

  Self.DispatchEvent(Self.RemoteTQ, TriggerImmediately, Wait);
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

      S := Self.AddSocket(Self.ListeningSockets, TIdSipListeningConnection) as TIdSipListeningConnection;
      S.Listen(B, Self.ListenQueue);
    end;

    Self.ScheduleProcessingSocketEvents;
  except
    for I := 0 to Self.ListeningSockets.Count - 1 do
      TIdSipListeningConnection(Self.ListeningSockets[I]).Close;
    raise;
  end;
end;

procedure TIdSipTcpServer.Write(Handle: TIdConnectionHandle; ErrorCode: Integer);
var
  Connection: TIdSipSendingConnection;
begin
  Connection := Self.ConnectionMap.ConnectionFor(Handle);

  if not Assigned(Connection) then Exit;

  Connection.PartialSend;
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
