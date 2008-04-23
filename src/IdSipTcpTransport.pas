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
    procedure DoOnAddConnection(Connection: TIdTCPConnection;
                                Request: TIdSipRequest);
    procedure DoOnRemoveConnection(Connection: TIdTCPConnection);
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

    function  ClientType: TIdSipTcpClientClass; virtual;
    function  IsRunning: Boolean; override;
    procedure RemoveClient(ClientThread: TObject);
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

  TIdSipAddConnectionEvent = procedure(Connection: TIdTCPConnection;
                                       Request: TIdSipRequest) of object;
  TIdSipRemoveConnectionEvent = procedure(Connection: TIdTCPConnection) of object;

  // Given a TCP connection, I read messages off the connection and dispatch
  // them to a TimerQueue until I die or something severs the connection.
  TIdSipTcpMessageReader = class(TObject)
  private
    fOnAddConnection:    TIdSipAddConnectionEvent;
    fOnRemoveConnection: TIdSipRemoveConnectionEvent;
    fReadTimeout:        Integer;
    fTimer:              TIdTimerQueue;
    fTransportID:        String;
    fTransportType:      String;

    procedure AddConnection(Connection: TIdTCPConnection;
                            Request: TIdSipRequest);
    procedure ReadBodyInto(Connection: TIdTCPConnection;
                           Msg: TIdSipMessage;
                           Dest: TStringStream);
    procedure ReadHeaders(Connection: TIdTCPConnection;
                          Dest: TStringStream);
    function  ReadMessage(Connection: TIdTCPConnection;
                          ConserveConnections: Boolean;
                          var ConnClosedOrTimedOut: Boolean): TIdSipMessage;
    procedure ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                           Binding: TIdConnectionBindings);
    procedure ReturnInternalServerError(Connection: TIdTCPConnection;
                                        const Reason: String);
  public
    procedure NotifyOfException(ExceptionType: ExceptClass;
                                const Reason: String);
    procedure ReadMessages(Connection: TIdTCPConnection;
                           ConserveConnections: Boolean);

    property OnAddConnection:    TIdSipAddConnectionEvent    read fOnAddConnection write fOnAddConnection;
    property OnRemoveConnection: TIdSipRemoveConnectionEvent read fOnRemoveConnection write fOnRemoveConnection;
    property ReadTimeout:        Integer                     read fReadTimeout write fReadTimeout;
    property Timer:              TIdTimerQueue               read fTimer write fTimer;
    property TransportID:        String                      read fTransportID write fTransportID;
    property TransportType:      String                      read fTransportType write fTransportType;
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
    function  GetOnAddConnection: TIdSipAddConnectionEvent;
    function  GetOnRemoveConnection: TIdSipRemoveConnectionEvent;
    function  GetReadTimeout: Integer;
    function  GetTimer: TIdTimerQueue;
    function  GetTransportID: String;
    procedure SetOnAddConnection(Value: TIdSipAddConnectionEvent);
    procedure SetOnRemoveConnection(Value: TIdSipRemoveConnectionEvent);
    procedure SetReadTimeout(Value: Integer);
    procedure SetTimer(Value: TIdTimerQueue);
    procedure SetTransportID(const Value: String);
  protected
    procedure DoDisconnect(Thread: TIdPeerThread); override;
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
    property OnAddConnection:     TIdSipAddConnectionEvent    read GetOnAddConnection write SetOnAddConnection;
    property OnRemoveConnection:  TIdSipRemoveConnectionEvent read GetOnRemoveConnection write SetOnRemoveConnection;
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
    fConserveConnections: Boolean;
    MessageReader:        TIdSipTcpMessageReader;

    function  BoolToSockOpt(B: Boolean): Integer;
    procedure SetConserveConnections(Value: Boolean);
    function  GetTransportID: String;
    procedure SetTransportID(const Value: String);
    procedure SetKeepAlive(UseKeepAlive: Boolean);
  protected
    function  GetTimer: TIdTimerQueue; override;
    procedure SetTimer(Value: TIdTimerQueue); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Connect(const Timeout: Integer); override;
    procedure ReceiveMessages; override;
    procedure Send(Msg: TIdSipMessage);

    property ConserveConnections: Boolean read fConserveConnections write SetConserveConnections;
    property TransportID:         String  read GetTransportID write SetTransportID;
  end;

  // I relate a request with a TCP connection. I store a COPY of a request
  // while storing a REFERENCE to a connection. Transports construct requests
  // and so bear responsibility for destroying them, and I need to remember
  // these requests.
  // I represent a (possibly) deferred handling of an exception by using a
  // TNotifyEvent.
  TIdSipConnectionTableEntry = class(TObject)
  private
    fConnection: TIdTCPConnection;
    fRequest:    TIdSipRequest;
  public
    constructor Create(Connection:    TIdTCPConnection;
                       CopyOfRequest: TIdSipRequest);
    destructor  Destroy; override;

    property Connection: TIdTCPConnection read fConnection;
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
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(Connection: TIdTCPConnection;
                  Request:    TIdSipRequest);
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

  Self.ConnectionMap  := TIdSipConnectionTableLock.Create;

  Self.ConnectionTimeout := FiveSeconds;

  Self.Bindings.Add;
end;

destructor TIdSipTCPTransport.Destroy;
begin
  Self.RunningClients.Free;
  Self.ConnectionMap.Free;

  inherited Destroy;
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
var
  Clients: TList;
begin
  Clients := Self.RunningClients.LockList;
  try
    Clients.Remove(ClientThread);
  finally
    Self.RunningClients.UnlockList;
  end;
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
  Self.DoOnRemoveConnection(Sender as TIdTCPConnection);
end;

procedure TIdSipTCPTransport.DestroyServer;
begin
  Self.Transport.Free;
end;

procedure TIdSipTCPTransport.DoOnAddConnection(Connection: TIdTCPConnection;
                                               Request: TIdSipRequest);
var
  Table: TIdSipConnectionTable;
begin
  Table := Self.ConnectionMap.LockList;
  try
    Table.Add(Connection, Request);
    Connection.OnDisconnected := Self.ConnectionDisconnected;
  finally
    Self.ConnectionMap.UnlockList;
  end;
end;

procedure TIdSipTCPTransport.DoOnRemoveConnection(Connection: TIdTCPConnection);
var
  Table: TIdSipConnectionTable;
begin
  Table := Self.ConnectionMap.LockList;
  try
    Table.Remove(Connection);
  finally
    Self.ConnectionMap.UnlockList;
  end;
end;

function TIdSipTCPTransport.GetBindings: TIdSocketHandles;
begin
  Result := Self.Transport.Bindings;
end;

procedure TIdSipTCPTransport.InstantiateServer;
begin
  Self.Transport := Self.ServerType.Create(nil);
  Self.Transport.TransportID := Self.ID;

  Self.Transport.OnAddConnection    := Self.DoOnAddConnection;
  Self.Transport.OnRemoveConnection := Self.DoOnRemoveConnection;
end;

procedure TIdSipTCPTransport.SendMessage(Msg: TIdSipMessage;
                                         Dest: TIdConnectionBindings);
var
  Connection:  TIdTCPConnection;
  Table:       TIdSipConnectionTable;
begin
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

      if Msg.LastHop.IsUnset then
        Msg.RewriteLocationHeaders(Dest);

      Self.WriteMessageTo(Msg, Connection)
    end
    else begin
      // Last resort: make a new connection to Dest.
      Self.SendMessageTo(Msg, Dest);
    end;
  finally
    Self.ConnectionMap.UnlockList;
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
      Self.DoOnAddConnection(NewConnection, Msg as TIdSipRequest);

    Dest.LocalIP   := NewConnection.Socket.Binding.IP;
    Dest.LocalPort := NewConnection.Socket.Binding.Port;

    if Msg.LastHop.IsUnset then
      Msg.RewriteLocationHeaders(Dest);

    Self.RunningClients.Add(TIdSipTcpClientThread.Create(NewConnection, Msg, Self));
  except
    on E: EIdException do
      Self.NotifyOfException(Msg, E, E.Message);
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
  ConnClosedOrTimedOut: Boolean;
  Msg:                  TIdSipMessage;
  ReceivedFrom:         TIdConnectionBindings;
begin
  ConnClosedOrTimedOut := false;

  Connection.ReadTimeout := Self.ReadTimeout;
  while Connection.Connected and not ConnClosedOrTimedOut do begin
    ReceivedFrom := TIdConnectionBindings.Create;
    try
      ReceivedFrom.LocalIP   := Connection.Socket.Binding.IP;
      ReceivedFrom.LocalPort := Connection.Socket.Binding.Port;
      ReceivedFrom.PeerIP    := Connection.Socket.Binding.PeerIP;
      ReceivedFrom.PeerPort  := Connection.Socket.Binding.PeerPort;
      ReceivedFrom.Transport := Self.TransportType;

      Msg := Self.ReadMessage(Connection, ConserveConnections, ConnClosedOrTimedOut);
      try
        // ReadMessage returns nil only if the connection broke before the
        // message could be fully read. We can't do anything with half a
        // message, so we abort.
        if not Assigned(Msg) then Exit;

        try
          // If Self.ReadBody closes the connection, we don't want to AddConnection!
          if Msg.IsRequest and not ConnClosedOrTimedOut then
            Self.AddConnection(Connection, Msg as TIdSipRequest);

          Self.ReceiveMessageInTimerContext(Msg, ReceivedFrom);
        except
          on E: Exception do begin
            // This results in returning a 500 Internal Server Error to a response!
            if Connection.Connected then begin
              Self.ReturnInternalServerError(Connection, E.Message);
              Connection.DisconnectSocket;
            end;

            Self.NotifyOfException(ExceptClass(E.ClassType),
                                               E.Message);
          end;
        end;
      finally
        Msg.Free;
      end;
    finally
      ReceivedFrom.Free;
    end;
  end;
end;

//* TIdSipTcpMessageReader Private methods *************************************

procedure TIdSipTcpMessageReader.AddConnection(Connection: TIdTCPConnection;
                                               Request: TIdSipRequest);
begin
  if Assigned(Self.OnAddConnection) then
    Self.OnAddConnection(Connection, Request);
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
                                            var ConnClosedOrTimedOut: Boolean): TIdSipMessage;
var
  S: TStringStream;
begin
  // Read in a SIP message from Connection, instantiate a TIdSipMessage
  // subclass, and return the reified message. THE CALLER FREES THE MESSAGE.
  //
  // As a side effect, set ConnClosedOrTimedOut to true if necessary.

  Result := nil;
  S := TStringStream.Create('');
  try
    try
      Self.ReadHeaders(Connection, S);
    except
      on EIdReadTimeout do
        ConnClosedOrTimedOut := not ConserveConnections;
      on EIdConnClosedGracefully do
        ConnClosedOrTimedOut := true;
      on EIdClosedSocket do
        ConnClosedOrTimedOut := true;
    end;

    if not ConnClosedOrTimedOut then begin
      Result := TIdSipMessage.ReadMessageFrom(S);

      try
        Self.ReadBodyInto(Connection, Result, S);
        Result.ReadBody(S);
      except
        on EIdReadTimeout do
          ConnClosedOrTimedOut := true;
        on EIdConnClosedGracefully do
          ConnClosedOrTimedOut := true;
        on EIdClosedSocket do
          ConnClosedOrTimedOut := true;
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

procedure TIdSipTcpServer.DoDisconnect(Thread: TIdPeerThread);
begin
  if Assigned(Self.OnRemoveConnection) then
    Self.OnRemoveConnection(Thread.Connection);

  inherited DoDisconnect(Thread);
end;

procedure TIdSipTcpServer.DoOnExecute(Thread: TIdPeerThread);
begin
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

function TIdSipTcpServer.GetOnAddConnection: TIdSipAddConnectionEvent;
begin
  Result := Self.MessageReader.OnAddConnection;
end;

function TIdSipTcpServer.GetOnRemoveConnection: TIdSipRemoveConnectionEvent;
begin
  Result := Self.MessageReader.OnRemoveConnection;
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

procedure TIdSipTcpServer.SetOnAddConnection(Value: TIdSipAddConnectionEvent);
begin
  Self.MessageReader.OnAddConnection := Value;
end;

procedure TIdSipTcpServer.SetOnRemoveConnection(Value: TIdSipRemoveConnectionEvent);
begin
  Self.MessageReader.OnRemoveConnection := Value;
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

  Transport := TIdObjectRegistry.FindObject(Value);

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

procedure TIdSipTcpClient.Connect(const Timeout: Integer);
begin
  inherited Connect(Timeout);

  Self.SetKeepAlive(Self.ConserveConnections);
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

function TIdSipTcpClient.BoolToSockOpt(B: Boolean): Integer;
begin
  if B then
    Result := Id_SO_True
  else
    Result := Id_SO_False;
end;

procedure TIdSipTcpClient.SetConserveConnections(Value: Boolean);
begin
  Self.fConserveConnections := Value;

  Self.SetKeepAlive(Value);
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

  Transport := TIdObjectRegistry.FindObject(Value);

  if Assigned(Transport) and (Transport is TIdSipTransport) then
    Self.MessageReader.TransportType := (Transport as TIdSipTransport).GetTransportType;
end;

procedure TIdSipTcpClient.SetKeepAlive(UseKeepAlive: Boolean);
var
  SetKeepalive: Integer;
begin
  if Self.Connected then begin
    SetKeepalive := BoolToSockOpt(UseKeepAlive);
    Self.Socket.Binding.SetSockOpt(Id_SOL_SOCKET, Id_SO_KEEPALIVE, @SetKeepalive, Sizeof(SetKeepalive));
  end;
end;

//******************************************************************************
//* TIdSipConnectionTableEntry                                                 *
//******************************************************************************
//* TIdSipConnectionTableEntry Public methods **********************************

constructor TIdSipConnectionTableEntry.Create(Connection:    TIdTCPConnection;
                                              CopyOfRequest: TIdSipRequest);
begin
  inherited Create;

  Self.fConnection := Connection;
  Self.fRequest := TIdSipRequest.Create;
  Self.fRequest.Assign(CopyOfRequest);
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

procedure TIdSipConnectionTable.Add(Connection: TIdTCPConnection;
                                    Request:    TIdSipRequest);
begin
  Self.List.Add(TIdSipConnectionTableEntry.Create(Connection, Request));
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
  Count: Integer;
  I: Integer;
begin
  I := 0;
  Count := Self.List.Count;
  while (I < Count)
    and (Self.EntryAt(I).Connection <> Connection) do
    Inc(I);

  if (I < Count) then
    Self.List.Delete(I);
end;

//* TIdSipConnectionTable Private methods **************************************

function TIdSipConnectionTable.EntryAt(Index: Integer): TIdSipConnectionTableEntry;
begin
  Result := Self.List[Index] as TIdSipConnectionTableEntry;
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

end.
