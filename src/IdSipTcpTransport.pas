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
  Classes, Contnrs, IdBaseThread, IdConnectionBindings, IdSipLocation,
  IdSipMessage, IdSipTransport, IdSocketHandle, IdTCPConnection, IdTCPClient,
  IdTCPServer, IdThreadableTcpClient, IdTimerQueue, SyncObjs, SysUtils;

type
  TIdSipConnectionTableLock = class;
  TIdSipTcpServer = class;
  TIdSipTcpServerClass = class of TIdSipTcpServer;

  TIdSipTcpClient = class;
  TIdSipTcpClientClass = class of TIdSipTcpClient;

  // I implement the Transmission Control Protocol (RFC 793) connections for the
  // SIP stack.
  TIdSipTCPTransport = class(TIdSipTransport)
  private
    fConnectionTimeout: Cardinal;
    RunningClients:     TThreadList;

    function  FirstServerPortFor(IPAddress: String): Cardinal;
    function  OutboundConnection(Connection: TIdTCPConnection): Boolean;
    procedure ScheduleExceptionNotification(E: Exception;
                                            Msg: TIdSipMessage;
                                            Dest: TIdConnectionBindings);
    procedure SendMessageTo(Msg: TIdSipMessage;
                            Dest: TIdConnectionBindings);
    procedure StopAllClientConnections;
    procedure WriteMessageTo(Msg: TIdSipMessage;
                             Connection: TIdTCPConnection);
  protected
    ConnectionMap: TIdSipConnectionTableLock;
    Transport:     TIdSipTcpServer;

    procedure ConnectionDisconnected(Sender: TObject);
    procedure DestroyServer; override;
    function  GetBindings: TIdSocketHandles; override;
    procedure InstantiateServer; override;
    procedure SendMessage(Msg: TIdSipMessage;
                          Dest: TIdConnectionBindings); override;
    function  ServerType: TIdSipTcpServerClass; virtual;
    procedure SetConserveConnections(Value: Boolean); override;
    procedure SetTimeout(Value: Cardinal); override;
    procedure SetTimer(Value: TIdTimerQueue); override;
  public
    class function GetTransportType: String; override;
    class function SrvPrefix: String; override;

    constructor Create; override;
    destructor  Destroy; override;

    procedure AddConnection(Connection: TIdTCPConnection;
                            Request: TIdSipRequest);
    function  ClientType: TIdSipTcpClientClass; virtual;
    function  IsRunning: Boolean; override;
    procedure RemoveClient(ClientThread: TObject);
    procedure RemoveConnection(Connection: TIdTCPConnection);
    procedure Start; override;
    procedure Stop; override;

    property ConnectionTimeout: Cardinal read fConnectionTimeout write fConnectionTimeout;
  end;

  // I allow the sending of TCP requests to happen asynchronously: in my
  // context, I instantiate a TCP client, send a request, and receive
  // messages, usually responses. I schedule Waits for each of these messages so
  // that my Transport handles the response in the context of its Timer.
  TIdSipTcpClientThread = class(TIdThreadedTcpClient)
  private
    FirstMsg:  TIdSipMessage;
    Transport: TIdSipTCPTransport;

    function SipClient: TIdSipTcpClient;
  protected
    procedure AfterRun; override;
    function  ClientType: TIdSipTcpClientClass; virtual;
    function  GetTimer: TIdTimerQueue; override;
    procedure NotifyOfException(E: Exception); override;
    procedure Run; override;
    procedure SetTimer(Value: TIdTimerQueue); override;
  public
    constructor Create(Connection: TIdSipTCPClient;
                       FirstMsg: TIdSipMessage;
                       Transport: TIdSipTCPTransport); reintroduce;
    destructor Destroy; override;
  end;

  // Given a TCP connection, I read messages off the connection and dispatch
  // them to a TimerQueue until I die or something severs the connection.
  TIdSipTcpMessageReader = class(TObject)
  private
    fReadTimeout:        Integer;
    fTimer:              TIdTimerQueue;
    fTransportID:        String;
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
    procedure ReadMessages(Connection: TIdTCPConnection;
                           ConserveConnections: Boolean);

    property ReadTimeout:   Integer       read fReadTimeout write fReadTimeout;
    property Timer:         TIdTimerQueue read fTimer write fTimer;
    property TransportID:   String        read fTransportID write fTransportID;
    property TransportType: String        read fTransportType write fTransportType;
  end;

  // ReadTimeout = -1 implies that we never timeout the body wait. We do not
  // recommend this. ReadTimeout = n implies we wait n milliseconds for
  // the body to be received. If we haven't read Content-Length bytes by the
  // time the timeout occurs, we sever the connection.
  TIdSipTcpServer = class(TIdTCPServer)
  private
    fConnectionTimeout:   Integer;
    fConserveConnections: Boolean;
    MessageReader:        TIdSipTcpMessageReader;

    procedure DoOnException(Thread: TIdPeerThread;
                            Exception: Exception); overload;
    function  GetReadTimeout: Integer;
    function  GetTimer: TIdTimerQueue;
    function  GetTransportID: String;
    procedure SetReadTimeout(Value: Integer);
    procedure SetTimer(Value: TIdTimerQueue);
    procedure SetTransportID(const Value: String);
  protected
    procedure DoOnExecute(Thread: TIdPeerThread);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  CreateClient: TIdSipTcpClient; virtual;
    function  DefaultTimeout: Cardinal; virtual;
    procedure DestroyClient(Client: TIdSipTcpClient); virtual;
  published
    property ConnectionTimeout:   Integer                     read fConnectionTimeout write fConnectionTimeout;
    property ConserveConnections: Boolean                     read fConserveConnections write fConserveConnections;
    property ReadTimeout:         Integer                     read GetReadTimeout write SetReadTimeout;
    property Timer:               TIdTimerQueue               read GetTimer write SetTimer;
    property TransportID:         String                      read GetTransportID write SetTransportID;
  end;

  // Note that the Timeout property determines the maximum length of time to
  // wait for the next line of data to arrive.
  // Through my MessageReader, I read messages from the network and feed them to
  // a TimerQueue. That, and I send messages to the network.
  TIdSipTcpClient = class(TIdThreadableTcpClient)
  private
    MessageReader: TIdSipTcpMessageReader;

    function  GetTransportID: String;
    procedure SetTransportID(const Value: String);
  protected
    function  GetTimer: TIdTimerQueue; override;
    procedure SetTimer(Value: TIdTimerQueue); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure ReceiveMessages; override;
    procedure Send(Msg: TIdSipMessage);

    property TransportID: String  read GetTransportID write SetTransportID;
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
  TIdSipConnectionTableEntry = class(TObject)
  private
    fBinding:    TIdConnectionBindings;
    fConnection: TIdTCPConnection;
    fRequest:    TIdSipRequest;

    function CreateBinding(Connection: TIdTCPConnection): TIdConnectionBindings;
  public
    constructor Create(Connection:    TIdTCPConnection;
                       CopyOfRequest: TIdSipRequest);
    destructor  Destroy; override;

    property Binding:    TIdConnectionBindings read fBinding;
    property Connection: TIdTCPConnection      read fConnection;
    property Request:    TIdSipRequest         read fRequest;
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
    function FindEntry(Connection: TIdTCPConnection;
                       Request:    TIdSipRequest): TIdSipConnectionTableEntry;
  public
    constructor Create;
    destructor  Destroy; override;

    function  Add(Connection: TIdTCPConnection;
                  Request:    TIdSipRequest): TIdSipConnectionTableEntry;
    function  ConnectionEntry(Connection: TIdTCPConnection): TIdSipConnectionTableEntry;
    function  ConnectionFor(Msg: TIdSipMessage): TIdTCPConnection; overload;
    function  ConnectionFor(Destination: TIdConnectionBindings): TIdTCPConnection; overload;
    function  Count: Integer;
    procedure Remove(Connection: TIdTCPConnection);
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

  // I store a REFERENCE to a TIdTCPConnection.
  TIdSipTcpConnectionWait = class(TIdSipTransportWait)
  private
    fBinding: TIdTCPConnection;
  public
    property Binding: TIdTCPConnection read fBinding write fBinding;
  end;

  TIdSipTcpConnectionOpenWait = class(TIdSipTcpConnectionWait)
  private
    fOpeningRequest: TIdSipRequest;

    procedure SetOpeningRequest(Value: TIdSipRequest);
  protected
    procedure TriggerOn(Transport: TIdSipTransport); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    property OpeningRequest: TIdSipRequest read fOpeningRequest write SetOpeningRequest;
  end;

  TIdSipTcpConnectionCloseWait = class(TIdSipTcpConnectionWait)
  protected
    procedure TriggerOn(Transport: TIdSipTransport); override;
  end;

implementation

uses
  IdException, IdIndyUtils, IdRegisteredObject, IdSipDns, IdStackConsts;

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
  // The superclass sets Timeout, which uses RunningClients.
  Self.RunningClients := TThreadList.Create;

  inherited Create;

  Self.ConnectionMap := TIdSipConnectionTableLock.Create;

  Self.ConnectionTimeout := FiveSeconds;

  Self.Bindings.Add;
end;

destructor TIdSipTCPTransport.Destroy;
begin
  Self.RunningClients.Free;
  Self.ConnectionMap.Free;

  inherited Destroy;
end;

procedure TIdSipTCPTransport.AddConnection(Connection: TIdTCPConnection;
                                           Request: TIdSipRequest);
var
  Entry: TIdSipConnectionTableEntry;
  Table: TIdSipConnectionTable;
begin
  Table := Self.ConnectionMap.LockList;
  try
    Entry := Table.Add(Connection, Request);
    Connection.OnDisconnected := Self.ConnectionDisconnected;

    Self.NotifyOfConnection(Entry.Binding);
  finally
    Self.ConnectionMap.UnlockList;
  end;
end;

procedure TIdSipTCPTransport.RemoveConnection(Connection: TIdTCPConnection);
var
  Entry: TIdSipConnectionTableEntry;
  Table: TIdSipConnectionTable;
begin
  Table := Self.ConnectionMap.LockList;
  try
    Entry := Table.ConnectionEntry(Connection);

    // This should ALWAYS be non-nil, but let's just be certain.
    if Assigned(Entry) then
      Self.NotifyOfDisconnection(Entry.Binding);

    Table.Remove(Connection);
  finally
    Self.ConnectionMap.UnlockList;
  end;
end;

function TIdSipTCPTransport.ClientType: TIdSipTcpClientClass;
begin
  Result := TIdSipTcpClient;
end;

function TIdSipTCPTransport.IsRunning: Boolean;
begin
  Result := Self.Transport.Active;
end;

procedure TIdSipTCPTransport.RemoveClient(ClientThread: TObject);
begin
  Self.RunningClients.Remove(ClientThread);
end;

procedure TIdSipTCPTransport.Start;
begin
  inherited Start;

  try
    Self.Transport.Active := true;
  except
    on E: EIdException do
      RaiseSocketError(E, Self.Transport.Bindings);
  end;
end;

procedure TIdSipTCPTransport.Stop;
begin
  Self.Transport.Active := false;

  Self.StopAllClientConnections;
end;

//* TIdSipTCPTransport Protected methods ***************************************

procedure TIdSipTCPTransport.ConnectionDisconnected(Sender: TObject);
begin
  Self.RemoveConnection(Sender as TIdTCPConnection);
end;

procedure TIdSipTCPTransport.DestroyServer;
begin
  Self.Transport.Free;
end;

function TIdSipTCPTransport.GetBindings: TIdSocketHandles;
begin
  Result := Self.Transport.Bindings;
end;

procedure TIdSipTCPTransport.InstantiateServer;
begin
  Self.Transport := Self.ServerType.Create(nil);
  Self.Transport.TransportID := Self.ID;
end;

procedure TIdSipTCPTransport.SendMessage(Msg: TIdSipMessage;
                                         Dest: TIdConnectionBindings);
var
  Connection:  TIdTCPConnection;
  LocBinding:  TIdConnectionBindings;
  Table:       TIdSipConnectionTable;
begin
  LocBinding := Dest.Copy;
  try
    Table := Self.ConnectionMap.LockList;
    try
      // Try send the response down the same connection on which we received the
      // request.
      Connection := Table.ConnectionFor(Msg);

      // Otherwise, try find an existing connection to Dest.
      if not Assigned(Connection) then
        Connection := Table.ConnectionFor(Dest);

      if Assigned(Connection) and Connection.Connected then begin
        Dest.LocalIP   := Connection.Socket.Binding.IP;
        Dest.LocalPort := Connection.Socket.Binding.Port;

        LocBinding.LocalIP := Connection.Socket.Binding.IP;

        // Outbound connections have an ephemeral local port, and we MUST be
        // prepared to accept connections at the port listed in a Via header
        // we generate (cf. RFC 3261, section 18.1.1). We don't really care which
        // binding's port we use, as long as the port's on the same IP address.
        //
        // We use a special local variable - LocBinding - for storing this info
        // because Dest contains the real binding info that the superclass uses
        // to log the message send.
        if Self.OutboundConnection(Connection) then
          LocBinding.LocalPort := Self.FirstServerPortFor(Dest.LocalIP)
        else
          LocBinding.LocalPort := Connection.Socket.Binding.Port;

        if Msg.LastHop.IsUnset then
          Msg.RewriteLocationHeaders(LocBinding);

        Self.WriteMessageTo(Msg, Connection)
      end
      else begin
        // Last resort: make a new connection to Dest.
        Self.SendMessageTo(Msg, Dest);
      end;
    finally
      Self.ConnectionMap.UnlockList;
    end;
  finally
    LocBinding.Free;
  end;
end;

function TIdSipTCPTransport.ServerType: TIdSipTcpServerClass;
begin
  Result := TIdSipTcpServer;
end;

procedure TIdSipTCPTransport.SetConserveConnections(Value: Boolean);
var
  Clients: TList;
  I:       Integer;
begin
  inherited SetConserveConnections(Value);

  Self.Transport.ConserveConnections := Value;

  Clients := Self.RunningClients.LockList;
  try
    for I := 0 to Clients.Count - 1 do
      TIdSipTcpClient(TIdSipTcpClientThread(Clients[I]).Client).ConserveConnections := Value;
  finally
    Self.RunningClients.UnlockList;
  end;
end;

procedure TIdSipTCPTransport.SetTimeout(Value: Cardinal);
var
  Clients: TList;
  I:       Integer;
begin
  inherited SetTimeout(Value);

  Self.Transport.ReadTimeout       := Value;
  Self.Transport.ConnectionTimeout := Value;

  Clients := Self.RunningClients.LockList;
  try
    for I := 0 to Clients.Count - 1 do
      TIdSipTcpClientThread(Clients[I]).Client.ReadTimeout := Value;
  finally
    Self.RunningClients.UnlockList;
  end;
end;

procedure TIdSipTCPTransport.SetTimer(Value: TIdTimerQueue);
begin
  inherited SetTimer(Value);

  Self.Transport.Timer := Value;
end;

//* TIdSipTCPTransport Protected methods ***************************************

function TIdSipTCPTransport.FirstServerPortFor(IPAddress: String): Cardinal;
var
  I: Integer;
begin
  Result := Self.DefaultPort;

  for I := 0 to Self.BindingCount - 1 do begin
    if (Self.Bindings[I].IP = IPAddress) then begin
      Result := Self.Bindings[I].Port;
      Break;
    end;
  end;
end;

function TIdSipTCPTransport.OutboundConnection(Connection: TIdTCPConnection): Boolean;
var
  Clients: TList;
  I:       Integer;
begin
  Result := false;

  Clients := Self.RunningClients.LockList;
  try
    for I := 0 to Clients.Count - 1 do begin
      if (TIdSipTcpClientThread(Clients[I]).Client = Connection) then begin
        Result := true;
        Break;
      end;
    end;
  finally
    Self.RunningClients.UnlockList;
  end;
end;

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

procedure TIdSipTCPTransport.SendMessageTo(Msg: TIdSipMessage;
                                           Dest: TIdConnectionBindings);
var
  NewConnection: TIdSipTcpClient;
begin
  NewConnection := Self.ClientType.Create(nil);
  NewConnection.ConserveConnections := Self.ConserveConnections;
  NewConnection.Host                := Dest.PeerIP;
  NewConnection.Port                := Dest.PeerPort;
  NewConnection.ReadTimeout         := Self.Timeout;
  NewConnection.Timer               := Self.Timer;
  NewConnection.TransportID         := Self.ID;

  try
    NewConnection.Connect(Self.ConnectionTimeout);

    if Msg.IsRequest then
      Self.AddConnection(NewConnection, Msg as TIdSipRequest);

    // Outbound connections have an ephemeral local port, and we MUST be
    // prepared to accept connections at the port listed in a Via header
    // we generate (cf. RFC 3261, section 18.1.1). We don't really care which
    // binding's port we use, as long as the port's on the same IP address.
    Dest.LocalIP   := NewConnection.Socket.Binding.IP;
    Dest.LocalPort := Self.FirstServerPortFor(Dest.LocalIP);

    if Msg.LastHop.IsUnset then
      Msg.RewriteLocationHeaders(Dest);

    Self.RunningClients.Add(TIdSipTcpClientThread.Create(NewConnection, Msg, Self));
  except
    on E: EIdConnectException do
      Self.ScheduleExceptionNotification(E, Msg, Dest);
    on E: EIdException do
      Self.ScheduleExceptionNotification(E, Msg, Dest);
  end;
end;

procedure TIdSipTCPTransport.StopAllClientConnections;
var
  Clients: TList;
  I:       Integer;
begin
  Clients := Self.RunningClients.LockList;
  try
    for I := 0 to Clients.Count - 1 do
      TIdSipTcpClientThread(Clients[I]).Terminate;
  finally
    Self.RunningClients.UnlockList;
  end;
end;

procedure TIdSipTCPTransport.WriteMessageTo(Msg: TIdSipMessage;
                                            Connection: TIdTCPConnection);
begin
  Connection.Write(Msg.AsString);
end;

//******************************************************************************
//* TIdSipTcpClientThread                                                      *
//******************************************************************************
//* TIdSipTcpClientThread Public methods ***************************************

constructor TIdSipTcpClientThread.Create(Connection: TIdSipTCPClient;
                                         FirstMsg: TIdSipMessage;
                                         Transport: TIdSipTCPTransport);
begin
  inherited Create(Connection);

  Self.FirstMsg  := FirstMsg.Copy;
  Self.Transport := Transport;
end;

destructor TIdSipTcpClientThread.Destroy;
begin
  Self.FirstMsg.Free;
  Self.Client.Free;

  inherited Destroy;
end;

//* TIdSipTcpClientThread Protected methods ************************************

procedure TIdSipTcpClientThread.AfterRun;
begin
  Self.Transport.RemoveClient(Self);
end;

function TIdSipTcpClientThread.ClientType: TIdSipTcpClientClass;
begin
  Result := TIdSipTcpClient;
end;

function TIdSipTcpClientThread.GetTimer: TIdTimerQueue;
begin
  Result := Self.Transport.Timer;
end;

procedure TIdSipTcpClientThread.NotifyOfException(E: Exception);
var
  Wait: TIdSipMessageExceptionWait;
begin
  if Self.Terminated then Exit;

  Wait := TIdSipMessageExceptionWait.Create;
  Wait.ExceptionType    := ExceptClass(E.ClassType);
  Wait.ExceptionMessage := E.Message;
  Wait.FailedMessage    := Self.FirstMsg.Copy;
  Wait.Reason           := ExceptionDuringTcpClientRequestSend;
  Wait.TransportID      := Self.Transport.ID;

  Self.Timer.AddEvent(TriggerImmediately, Wait);
end;

procedure TIdSipTcpClientThread.Run;
begin
  try
    try
      Self.SipClient.Send(Self.FirstMsg);
      Self.SipClient.ReceiveMessages;
    finally
      Self.SipClient.Disconnect;
    end;
  except
    on EIdConnClosedGracefully do;
    on EIdConnectTimeout do;
    on E: Exception do
      Self.NotifyOfException(E);
  end;
end;

procedure TIdSipTcpClientThread.SetTimer(Value: TIdTimerQueue);
begin
  // Ignore attempts to change the Timer.
end;

//* TIdSipTcpClientThread Private methods **************************************

function TIdSipTcpClientThread.SipClient: TIdSipTcpClient;
begin
  Result := Self.Client as TIdSipTcpClient;
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
var
  Wait: TIdSipTcpConnectionOpenWait;
begin
  Wait := TIdSipTcpConnectionOpenWait.Create;
  Wait.Binding        := Connection;
  Wait.OpeningRequest := Request;
  Wait.TransportID    := Self.TransportID;
  Self.Timer.AddEvent(TriggerImmediately, Wait);
end;

procedure TIdSipTcpMessageReader.NotifyOfDisconnectionInTimerContext(Connection: TIdTCPConnection);
var
  Wait: TIdSipTcpConnectionCloseWait;
begin
  Wait := TIdSipTcpConnectionCloseWait.Create;
  Wait.Binding     := Connection;
  Wait.TransportID := Self.TransportID;
  Self.Timer.AddEvent(TriggerImmediately, Wait);
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
const
  CrLf = #$D#$A;
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
//* TIdSipTcpServer                                                            *
//******************************************************************************
//* TIdSipTcpServer Public methods *********************************************

constructor TIdSipTcpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.ConnectionTimeout := Self.DefaultTimeout;
  Self.DefaultPort       := TIdSipTransportRegistry.DefaultPortFor(TcpTransport);
  Self.OnExecute         := Self.DoOnExecute;
  Self.OnException       := Self.DoOnException;

  Self.MessageReader := TIdSipTcpMessageReader.Create;
  Self.ReadTimeout   := Self.DefaultTimeout;
end;

destructor TIdSipTcpServer.Destroy;
begin
  Self.Active := false;
  Self.MessageReader.Free;

  inherited Destroy;
end;

function TIdSipTcpServer.CreateClient: TIdSipTcpClient;
begin
  Result := TIdSipTcpClient.Create(nil);
end;

function TIdSipTcpServer.DefaultTimeout: Cardinal;
begin
  Result := 60000; // One minute
end;

procedure TIdSipTcpServer.DestroyClient(Client: TIdSipTcpClient);
begin
  Client.Free;
end;

//* TIdSipTcpServer Protected methods ******************************************

procedure TIdSipTcpServer.DoOnExecute(Thread: TIdPeerThread);
begin
  KeepAliveSocket(Thread.Connection, Self.ConserveConnections);

  Self.MessageReader.ReadTimeout := Self.DefaultTimeout;
  Self.MessageReader.ReadMessages(Thread.Connection, Self.ConserveConnections);
end;

//* TIdSipTcpServer Private methods ********************************************

procedure TIdSipTcpServer.DoOnException(Thread: TIdPeerThread;
                                        Exception: Exception);
begin
  Self.MessageReader.NotifyOfException(ExceptClass(Exception.ClassType),
                                       Exception.Message);
end;

function TIdSipTcpServer.GetReadTimeout: Integer;
begin
  Result := Self.MessageReader.ReadTimeout;
end;

function TIdSipTcpServer.GetTimer: TIdTimerQueue;
begin
  Result := Self.MessageReader.Timer;
end;

function TIdSipTcpServer.GetTransportID: String;
begin
  Result := Self.MessageReader.TransportID;
end;

procedure TIdSipTcpServer.SetReadTimeout(Value: Integer);
begin
  Self.MessageReader.ReadTimeout := Value;
end;

procedure TIdSipTcpServer.SetTimer(Value: TIdTimerQueue);
begin
  Self.MessageReader.Timer := Value;
end;

procedure TIdSipTcpServer.SetTransportID(const Value: String);
var
  Transport: TObject;
begin
  Self.MessageReader.TransportID := Value;

  Transport := TIdObjectRegistry.Singleton.FindObject(Value);

  if Assigned(Transport) and (Transport is TIdSipTransport) then
    Self.MessageReader.TransportType := (Transport as TIdSipTransport).GetTransportType;
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

function TIdSipTcpClient.GetTransportID: String;
begin
  Result := Self.MessageReader.TransportID;
end;

procedure TIdSipTcpClient.SetTimer(Value: TIdTimerQueue);
begin
  Self.MessageReader.Timer := Value;
end;

procedure TIdSipTcpClient.SetTransportID(const Value: String);
var
  Transport: TObject;
begin
  Self.MessageReader.TransportID := Value;

  Transport := TIdObjectRegistry.Singleton.FindObject(Value);

  if Assigned(Transport) and (Transport is TIdSipTransport) then
    Self.MessageReader.TransportType := (Transport as TIdSipTransport).GetTransportType;
end;

//******************************************************************************
//* TIdSipConnectionTableEntry                                                 *
//******************************************************************************
//* TIdSipConnectionTableEntry Public methods **********************************

constructor TIdSipConnectionTableEntry.Create(Connection:    TIdTCPConnection;
                                              CopyOfRequest: TIdSipRequest);
begin
  inherited Create;

  Self.fBinding    := Self.CreateBinding(Connection);
  Self.fConnection := Connection;
  Self.fRequest := TIdSipRequest.Create;
  Self.fRequest.Assign(CopyOfRequest);
end;

destructor TIdSipConnectionTableEntry.Destroy;
begin
  Self.fRequest.Free;
  Self.fBinding.Free;

  inherited Destroy;
end;

//* TIdSipConnectionTableEntry Private methods *********************************

function TIdSipConnectionTableEntry.CreateBinding(Connection: TIdTCPConnection): TIdConnectionBindings;
begin
  if Connection.Connected then
    Result := TIdConnectionBindings.Create(Connection.Socket.Binding.IP,
                                           Connection.Socket.Binding.Port,
                                           Connection.Socket.Binding.PeerIP,
                                           Connection.Socket.Binding.PeerPort,
                                           TcpTransport)
  else begin
    // We should never enter this clause, because disconnected Connections don't
    // belong in a TIdSipConnectionTable. This clause is only here as a paranoid
    // protection.
    Result := TIdConnectionBindings.Create('', 0, '', 0, TcpTransport);
  end;
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

function TIdSipConnectionTable.Add(Connection: TIdTCPConnection;
                                   Request:    TIdSipRequest): TIdSipConnectionTableEntry;
begin
  Result := Self.FindEntry(Connection, Request);

  if not Assigned(Result) then begin
    Result := TIdSipConnectionTableEntry.Create(Connection, Request);
    Self.List.Add(Result);
  end;
end;

function TIdSipConnectionTable.ConnectionEntry(Connection: TIdTCPConnection): TIdSipConnectionTableEntry;
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

function TIdSipConnectionTable.ConnectionFor(Msg: TIdSipMessage): TIdTCPConnection;
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

function TIdSipConnectionTable.ConnectionFor(Destination: TIdConnectionBindings): TIdTCPConnection;
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
    if Self.EntryAt(I).Connection.Connected then
      Found := (Destination.Transport = TcpTransport)
           and (Destination.PeerIP = Self.EntryAt(I).Connection.Socket.Binding.PeerIP)
           and (Integer(Destination.PeerPort) = Self.EntryAt(I).Connection.Socket.Binding.PeerPort);

    if not Found then
      Inc(I);
  end;

  if (I < Count) then
    Result := Self.EntryAt(I).Connection;
end;

function TIdSipConnectionTable.Count: Integer;
begin
  Result := Self.List.Count;
end;

procedure TIdSipConnectionTable.Remove(Connection: TIdTCPConnection);
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

function TIdSipConnectionTable.FindEntry(Connection: TIdTCPConnection;
                                         Request:    TIdSipRequest): TIdSipConnectionTableEntry;
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
//* TIdSipTcpConnectionOpenWait                                                *
//******************************************************************************
//* TIdSipTcpConnectionOpenWait Public methods *********************************

constructor TIdSipTcpConnectionOpenWait.Create;
begin
  inherited Create;

  Self.fOpeningRequest := TIdSipRequest.Create;
end;

destructor TIdSipTcpConnectionOpenWait.Destroy;
begin
  Self.fOpeningRequest.Free;

  inherited Destroy;
end;

//* TIdSipTcpConnectionOpenWait Protected methods ******************************

procedure TIdSipTcpConnectionOpenWait.TriggerOn(Transport: TIdSipTransport);
begin
  if not Transport.IsMock then
    (Transport as TIdSipTcpTransport).AddConnection(Self.Binding, Self.OpeningRequest);
end;

//* TIdSipTcpConnectionOpenWait Private methods ********************************

procedure TIdSipTcpConnectionOpenWait.SetOpeningRequest(Value: TIdSipRequest);
begin
  Self.fOpeningRequest.Assign(Value);
end;

//******************************************************************************
//* TIdSipTcpConnectionCloseWait                                               *
//******************************************************************************
//* TIdSipTcpConnectionCloseWait Protected methods *****************************

procedure TIdSipTcpConnectionCloseWait.TriggerOn(Transport: TIdSipTransport);
begin
  if not Transport.IsMock then
    (Transport as TIdSipTcpTransport).RemoveConnection(Self.Binding);
end;

end.
