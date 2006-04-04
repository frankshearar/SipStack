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
  Classes, Contnrs, IdBaseThread, IdSipLocator, IdSipMessage, IdSipTransport,
  IdSocketHandle, IdTCPConnection, IdTCPClient, IdTCPServer, IdTimerQueue,
  SyncObjs, SysUtils;

type
  TIdSipConnectionTableLock = class;
  TIdSipTcpServer = class;
  TIdSipTcpServerClass = class of TIdSipTcpServer;

  // I implement the Transmission Control Protocol (RFC 793) connections for the
  // SIP stack.
  TIdSipTCPTransport = class(TIdSipTransport)
  private
    RunningClients: TThreadList;

    procedure SendMessageTo(Msg: TIdSipMessage;
                            Dest: TIdSipLocation);
    procedure SendMessage(Msg: TIdSipMessage;
                          Dest: TIdSipLocation);
    procedure StopAllClientConnections;
  protected
    ConnectionMap: TIdSipConnectionTableLock;
    Transport:     TIdSipTcpServer;

    procedure DestroyServer; override;
    procedure DoOnAddConnection(Connection: TIdTCPConnection;
                                Request: TIdSipRequest);
    function  GetBindings: TIdSocketHandles; override;
    procedure InstantiateServer; override;
    procedure SendRequest(R: TIdSipRequest;
                          Dest: TIdSipLocation); override;
    procedure SendResponse(R: TIdSipResponse;
                           Dest: TIdSipLocation); override;
    function  ServerType: TIdSipTcpServerClass; virtual;
    procedure SetTimeout(Value: Cardinal); override;
    procedure SetTimer(Value: TIdTimerQueue); override;
  public
    class function GetTransportType: String; override;
    class function SrvPrefix: String; override;

    constructor Create; override;
    destructor  Destroy; override;

    function  IsRunning: Boolean; override;
    procedure RemoveClient(ClientThread: TObject);
    procedure Start; override;
    procedure Stop; override;
    procedure WriteMessageTo(Msg: TIdSipMessage;
                             Connection: TIdTCPConnection);
  end;

  TIdSipTcpClient = class;
  TIdSipTcpClientClass = class of TIdSipTcpClient;

  // I allow the sending of TCP requests to happen asynchronously: in my
  // context, I instantiate a TCP client, send a request, and receive
  // messages, usually responses. I schedule Waits for each of these messages so
  // that my Transport handles the response in the context of its Timer.
  TIdSipTcpClientThread = class(TIdBaseThread)
  private
    Client:    TIdSipTcpClient;
    FirstMsg:  TIdSipMessage;
    Transport: TIdSipTCPTransport;

    procedure AddConnection(Connection: TIdTCPConnection;
                            Request: TIdSipRequest);
    procedure NotifyOfException(E: Exception);
  protected
    function  ClientType: TIdSipTcpClientClass; virtual;
    procedure Run; override;
  public
    constructor Create(const Host: String;
                       Port: Cardinal;
                       Msg: TIdSipMessage;
                       Transport: TIdSipTCPTransport); reintroduce;
    destructor Destroy; override;

    procedure Terminate; override;
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

    procedure AddConnection(Connection: TIdTCPConnection;
                            Request: TIdSipRequest);
    procedure ReadBodyInto(Connection: TIdTCPConnection;
                           Msg: TIdSipMessage;
                           Dest: TStringStream);
    procedure ReadMessage(Connection: TIdTCPConnection;
                          Dest: TStringStream);
    procedure ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                           Binding: TIdSipConnectionBindings);
    procedure ReturnInternalServerError(Connection: TIdTCPConnection;
                                        const Reason: String);
  public
    procedure NotifyOfException(ExceptionType: ExceptClass;
                                const Reason: String);
    procedure ReadMessages(Connection: TIdTCPConnection);

    property OnAddConnection:    TIdSipAddConnectionEvent    read fOnAddConnection write fOnAddConnection;
    property OnRemoveConnection: TIdSipRemoveConnectionEvent read fOnRemoveConnection write fOnRemoveConnection;
    property ReadTimeout:        Integer                     read fReadTimeout write fReadTimeout;
    property Timer:              TIdTimerQueue               read fTimer write fTimer;
    property TransportID:        String                      read fTransportID write fTransportID;
  end;

  // ReadTimeout = -1 implies that we never timeout the body wait. We do not
  // recommend this. ReadTimeout = n implies we wait n milliseconds for
  // the body to be received. If we haven't read Content-Length bytes by the
  // time the timeout occurs, we sever the connection.
  TIdSipTcpServer = class(TIdTCPServer)
  private
    fConnectionTimeout: Integer;
    MessageReader:      TIdSipTcpMessageReader;

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
    property ConnectionTimeout:  Integer                     read fConnectionTimeout write fConnectionTimeout;
    property OnAddConnection:    TIdSipAddConnectionEvent    read GetOnAddConnection write SetOnAddConnection;
    property OnRemoveConnection: TIdSipRemoveConnectionEvent read GetOnRemoveConnection write SetOnRemoveConnection;
    property ReadTimeout:        Integer                     read GetReadTimeout write SetReadTimeout;
    property Timer:              TIdTimerQueue               read GetTimer write SetTimer;
    property TransportID:        String                      read GetTransportID write SetTransportID;
  end;

  // Note that the Timeout property determines the maximum length of time to
  // wait for the next line of data to arrive.
  // Through my MessageReader, I read messages from the network and feed them to
  // a TimerQueue. That, and I send messages to the network.
  TIdSipTcpClient = class(TIdTCPClient)
  private
    fTerminated:   Boolean;
    MessageReader: TIdSipTcpMessageReader;

    function  GetTimer: TIdTimerQueue;
    function  GetTransportID: String;
    procedure MarkAsTerminated(Sender: TObject);
    procedure SetTerminated(Value: Boolean);
    procedure SetTimer(Value: TIdTimerQueue);
    procedure SetTransportID(const Value: String);
  protected
    function DefaultTimeout: Cardinal; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure ReceiveMessages;
    procedure Send(Msg: TIdSipMessage);

    property Terminated:  Boolean       read fTerminated write SetTerminated;
    property Timer:       TIdTimerQueue read GetTimer write SetTimer;
    property TransportID: String        read GetTransportID write SetTransportID;
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

    procedure ConnectionDisconnected(Sender: TObject);
    function  EntryAt(Index: Integer): TIdSipConnectionTableEntry;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(Connection: TIdTCPConnection;
                  Request:    TIdSipRequest);
    function  ConnectionFor(Msg: TIdSipMessage): TIdTCPConnection; overload;
    function  ConnectionFor(Destination: TIdSipLocation): TIdTCPConnection; overload;
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
  IdException, IdSipConsts, IdSipDns;

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
  inherited Create;

  Self.ConnectionMap  := TIdSipConnectionTableLock.Create;
  Self.RunningClients := TThreadList.Create;

  Self.Bindings.Add;
end;

destructor TIdSipTCPTransport.Destroy;
begin
  Self.RunningClients.Free;
  Self.ConnectionMap.Free;

  inherited Destroy;
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

  Self.Transport.Active := true;
end;

procedure TIdSipTCPTransport.Stop;
begin
  Self.Transport.Active := false;

  Self.StopAllClientConnections;
end;

procedure TIdSipTCPTransport.WriteMessageTo(Msg: TIdSipMessage;
                                            Connection: TIdTCPConnection);
begin
  if Msg.IsRequest then
    Msg.LastHop.SentBy := Connection.Socket.Binding.IP;

  Connection.Write(Msg.AsString);
end;

//* TIdSipTCPTransport Protected methods ***************************************

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

  Self.Transport.OnAddConnection := Self.DoOnAddConnection;
end;

procedure TIdSipTCPTransport.SendRequest(R: TIdSipRequest;
                                         Dest: TIdSipLocation);
begin
  inherited SendRequest(R, Dest);

  Self.SendMessage(R, Dest);
end;

procedure TIdSipTCPTransport.SendResponse(R: TIdSipResponse;
                                          Dest: TIdSipLocation);
begin
  inherited SendResponse(R, Dest);

  Self.SendMessage(R, Dest);
end;

function TIdSipTCPTransport.ServerType: TIdSipTcpServerClass;
begin
  Result := TIdSipTcpServer;
end;

procedure TIdSipTCPTransport.SetTimeout(Value: Cardinal);
begin
  inherited SetTimeout(Value);

  Self.Transport.ReadTimeout       := Value;
  Self.Transport.ConnectionTimeout := Value;
end;

procedure TIdSipTCPTransport.SetTimer(Value: TIdTimerQueue);
begin
  inherited SetTimer(Value);

  Self.Transport.Timer := Value;
end;

//* TIdSipTCPTransport Protected methods ***************************************

procedure TIdSipTCPTransport.SendMessageTo(Msg: TIdSipMessage;
                                           Dest: TIdSipLocation);
begin
  Self.RunningClients.Add(TIdSipTcpClientThread.Create(Dest.IPAddress, Dest.Port, Msg, Self));
end;

procedure TIdSipTCPTransport.SendMessage(Msg: TIdSipMessage;
                                         Dest: TIdSipLocation);
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

    if Assigned(Connection) and Connection.Connected then
      Self.WriteMessageTo(Msg, Connection)
    else begin
      // Last resort: make a new connection to Dest.
      Self.SendMessageTo(Msg, Dest);
    end;
  finally
    Self.ConnectionMap.UnlockList;
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

//******************************************************************************
//* TIdSipTcpClientThread                                                      *
//******************************************************************************
//* TIdSipTcpClientThread Public methods ***************************************

constructor TIdSipTcpClientThread.Create(const Host: String;
                                         Port: Cardinal;
                                         Msg: TIdSipMessage;
                                         Transport: TIdSipTCPTransport);
begin
  Self.FreeOnTerminate := true;

  Self.Client := Self.ClientType.Create(nil);
  Self.Client.Host        := Host;
  Self.Client.Port        := Port;
  Self.Client.Timer       := Transport.Timer;
  Self.Client.TransportID := Transport.ID;

  Self.FirstMsg  := Msg.Copy;
  Self.Transport := Transport;

  inherited Create(false);
end;

destructor TIdSipTcpClientThread.Destroy;
begin
  Self.FirstMsg.Free;
  Self.Client.Free;

  inherited Destroy;
end;

procedure TIdSipTcpClientThread.Terminate;
begin
  Self.Client.Terminated := true;

  inherited Terminate;
end;

//* TIdSipTcpClientThread Protected methods ************************************

function TIdSipTcpClientThread.ClientType: TIdSipTcpClientClass;
begin
  Result := TIdSipTcpClient;
end;

procedure TIdSipTcpClientThread.Run;
begin
  try
    Self.Client.Connect(Self.Client.ReadTimeout);
    try
      if Self.FirstMsg.IsRequest then
        Self.AddConnection(Self.Client, Self.FirstMsg as TIdSipRequest);

      Self.Client.Send(Self.FirstMsg);
      Self.Client.ReceiveMessages;
    finally
      Self.Client.Disconnect;
    end;
  except
    on EIdConnClosedGracefully do;
    on EIdConnectTimeout do;
    on E: Exception do
      Self.NotifyOfException(E);
  end;

  Self.Transport.RemoveClient(Self);
end;

//* TIdSipTcpClientThread Private methods **************************************

procedure TIdSipTcpClientThread.AddConnection(Connection: TIdTCPConnection;
                                              Request: TIdSipRequest);
begin
  // TOD: This is accessing the PROTECTED method of the transport!
  Self.Transport.DoOnAddConnection(Connection, Request);
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

  Self.Transport.Timer.AddEvent(TriggerImmediately, Wait);
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

procedure TIdSipTcpMessageReader.ReadMessages(Connection: TIdTCPConnection);
var
  ConnClosedOrTimedOut: Boolean;
  Msg:                  TIdSipMessage;
  ReceivedFrom:         TIdSipConnectionBindings;
  S:                    TStringStream;
begin
  ConnClosedOrTimedOut := false;

  Connection.ReadTimeout := Self.ReadTimeout;
  while Connection.Connected and not ConnClosedOrTimedOut do begin
    ReceivedFrom := TIdSipConnectionBindings.Create;
    try
      ReceivedFrom.LocalIP   := Connection.Socket.Binding.IP;
      ReceivedFrom.LocalPort := Connection.Socket.Binding.Port;
      ReceivedFrom.PeerIP    := Connection.Socket.Binding.PeerIP;
      ReceivedFrom.PeerPort  := Connection.Socket.Binding.PeerPort;

      S := TStringStream.Create('');
      try
        try
          Self.ReadMessage(Connection, S);
        except
          on EIdReadTimeout do
            ConnClosedOrTimedOut := true;
          on EIdConnClosedGracefully do
            ConnClosedOrTimedOut := true;
          on EIdClosedSocket do
            ConnClosedOrTimedOut := true;
        end;

        if not ConnClosedOrTimedOut then begin
          Msg := TIdSipMessage.ReadMessageFrom(S);
          try
            try
              try
                Self.ReadBodyInto(Connection, Msg, S);
                Msg.ReadBody(S);
              except
                on EIdReadTimeout do
                  ConnClosedOrTimedOut := true;
                on EIdConnClosedGracefully do
                  ConnClosedOrTimedOut := true;
                on EIdClosedSocket do
                  ConnClosedOrTimedOut := true;
              end;

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
        end;
      finally
        S.Free;
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

procedure TIdSipTcpMessageReader.ReadMessage(Connection: TIdTCPConnection;
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

procedure TIdSipTcpMessageReader.ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                                              Binding: TIdSipConnectionBindings);
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
  Self.MessageReader.ReadMessages(Thread.Connection);
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
begin
  Self.MessageReader.TransportID := Value;
end;

//******************************************************************************
//* TIdSipTcpClient                                                            *
//******************************************************************************
//* TIdSipTcpClient Public methods *********************************************

constructor TIdSipTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.OnDisconnected := Self.MarkAsTerminated;
  Self.ReadTimeout    := Self.DefaultTimeout;

  Self.MessageReader := TIdSipTcpMessageReader.Create;

  Self.Terminated  := false;
end;

destructor TIdSipTcpClient.Destroy;
begin
  Self.MessageReader.Free;

  inherited Destroy;
end;

procedure TIdSipTcpClient.ReceiveMessages;
begin
  Self.MessageReader.ReadTimeout := Self.ReadTimeout;
  Self.MessageReader.ReadMessages(Self);
end;

procedure TIdSipTcpClient.Send(Msg: TIdSipMessage);
begin
  Self.Write(Msg.AsString);
end;

//* TIdSipTcpClient Protected methods ******************************************

function TIdSipTcpClient.DefaultTimeout: Cardinal;
begin
  Result := 5000;
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

procedure TIdSipTcpClient.MarkAsTerminated(Sender: TObject);
begin
  Self.Terminated := true;
end;

procedure TIdSipTcpClient.SetTerminated(Value: Boolean);
begin
  Self.fTerminated := Value;

  if Self.fTerminated and Self.Connected then
    Self.Disconnect;
end;

procedure TIdSipTcpClient.SetTimer(Value: TIdTimerQueue);
begin
  Self.MessageReader.Timer := Value;
end;

procedure TIdSipTcpClient.SetTransportID(const Value: String);
begin
  Self.MessageReader.TransportID := Value;
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
  Connection.OnDisconnected := Self.ConnectionDisconnected;
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

function TIdSipConnectionTable.ConnectionFor(Destination: TIdSipLocation): TIdTCPConnection;
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
           and (Destination.IPAddress = Self.EntryAt(I).Connection.Socket.Binding.PeerIP)
           and (Integer(Destination.Port) = Self.EntryAt(I).Connection.Socket.Binding.PeerPort);

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

procedure TIdSipConnectionTable.ConnectionDisconnected(Sender: TObject);
begin
  Self.Remove(Sender as TIdTCPConnection);
end;

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
