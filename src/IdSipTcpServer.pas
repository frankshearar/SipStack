unit IdSipTcpServer;

interface

uses
  Classes, Contnrs, IdSipConsts, IdSipMessage, IdSipTcpClient, IdSipTimer,
  IdTCPConnection, IdTCPServer, SyncObjs, SysUtils;

type
  IIdSipMessageListener = interface
    ['{941E4681-89F9-4491-825C-F6458F7E663C}']
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdSipConnectionBindings);
  end;

  TIdSipTcpConnectionCutter = class(TIdSipTimer)
  private
    fConnection: TIdTCPConnection;
  public
    property Connection: TIdTCPConnection read fConnection write fConnection;
  end;

  // I relate a request with a TCP connection. I store a COPY of a request
  // while storing a REFERENCE to a connection. Transports construct requests
  // and so are responsible for destroying them, and I need to remember these
  // requests.
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

  TIdSipConnectionTable = class(TObject)
  private
    List: TObjectList;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(Connection: TIdTCPConnection;
                  Request:    TIdSipRequest);
    function  ConnectionFor(Request: TIdSipRequest): TIdTCPConnection; overload;
    function  ConnectionFor(Response: TIdSipResponse): TIdTCPConnection; overload;
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

    function LockList: TIdSipConnectionTable;
    procedure UnlockList;
  end;

  // ReadBodyTimeout = 0 implies that we never timeout the body wait. We do not
  // recommend this. ReadBodyTimeout = n implies we wait n milliseconds for
  // the body to be received. If we haven't read Content-Length bytes by the
  // time the timeout occurs, we sever the connection.
  TIdSipTcpServer = class(TIdTCPServer)
  private
    ConnectionMap:      TIdSipConnectionTableLock;
    fConnectionTimeout: Cardinal;
    fReadBodyTimeout:   Cardinal;
    ListenerLock:       TCriticalSection;
    Listeners:          TList;

    procedure AddConnection(Connection: TIdTCPConnection;
                            Request: TIdSipRequest);
    procedure NotifyListeners(Request: TIdSipRequest;
                              ReceivedFrom: TIdSipConnectionBindings); overload;
    procedure NotifyListeners(Response: TIdSipResponse;
                              ReceivedFrom: TIdSipConnectionBindings); overload;
    procedure OnReadBodyTimeout(Sender: TObject);
    function  ReadBody(Connection: TIdTCPConnection;
                       Message: TIdSipMessage): String;
    function  ReadMessage(Connection: TIdTCPConnection): TStream;
    procedure ReturnBadRequest(Connection: TIdTCPConnection;
                               Reason: String;
                               Parser: TIdSipParser);
    procedure ReturnInternalServerError(Connection: TIdTCPConnection;
                                        Reason: String;
                                        Parser: TIdSipParser);
    procedure SendResponseTo(Response: TIdSipResponse;
                             Dest: TIdSipConnectionBindings);
    procedure WriteMessage(Connection: TIdTCPConnection;
                           AMessage: TIdSipMessage);
  protected
    procedure DoDisconnect(AThread: TIdPeerThread); override;
    function  DoExecute(AThread: TIdPeerThread): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure AddMessageListener(const Listener: IIdSipMessageListener);
    function  CreateClient: TIdSipTcpClient; virtual;
    function  DefaultTimeout: Cardinal; virtual;
    procedure DestroyClient(Client: TIdSipTcpClient); virtual;
    procedure RemoveMessageListener(const Listener: IIdSipMessageListener);
    procedure SendResponse(Response: TIdSipResponse);
  published
    property ConnectionTimeout: Cardinal read fConnectionTimeout write fConnectionTimeout;
    property DefaultPort default IdPORT_SIP;
    property ReadBodyTimeout:   Cardinal read fReadBodyTimeout write fReadBodyTimeout;
  end;

  TIdSipTcpServerClass = class of TIdSipTcpServer;

implementation

uses
  IdSipTransaction, IdSipHeaders;

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

function TIdSipConnectionTable.ConnectionFor(Request: TIdSipRequest): TIdTCPConnection;
var
  Count: Integer;
  I:     Integer;
begin
  Result := nil;

  I := 0;
  Count := Self.List.Count;
  while (I < Count)
    and not (Self.List[I] as TIdSipConnectionTableEntry).Request.IsEqualTo(Request) do
    Inc(I);

  if (I < Count) then
    Result := (Self.List[I] as TIdSipConnectionTableEntry).Connection;
end;

function TIdSipConnectionTable.ConnectionFor(Response: TIdSipResponse): TIdTCPConnection;
var
  Count: Integer;
  I:     Integer;
begin
  Result := nil;

  I := 0;
  Count := Self.List.Count;
  while (I < Count)
    and not (Self.List[I] as TIdSipConnectionTableEntry).Request.Match(Response) do
    Inc(I);

  if (I < Count) then
    Result := (Self.List[I] as TIdSipConnectionTableEntry).Connection;
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
    and ((Self.List[I] as TIdSipConnectionTableEntry).Connection <> Connection) do
    Inc(I);

  if (I < Count) then
    Self.List.Delete(I);
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
  Self.Lock.Free;
  Self.Table.Free;

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
//* TIdSipTcpServer                                                            *
//******************************************************************************
//* TIdSipTcpServer Public methods *********************************************

constructor TIdSipTcpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.ConnectionMap     := TIdSipConnectionTableLock.Create;
  Self.ConnectionTimeout := Self.DefaultTimeout;
  Self.DefaultPort       := IdPORT_SIP;
  Self.ListenerLock      := TCriticalSection.Create;
  Self.Listeners         := TList.Create;
end;

destructor TIdSipTcpServer.Destroy;
begin
  Self.Listeners.Free;
  Self.ListenerLock.Free;
  Self.ConnectionMap.Free;

  inherited Destroy;
end;

procedure TIdSipTcpServer.AddMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.ListenerLock.Acquire;
  try
    Self.Listeners.Add(Pointer(Listener));
  finally
    Self.ListenerLock.Release;
  end;
end;

function TIdSipTcpServer.CreateClient: TIdSipTcpClient;
begin
  Result := TIdSipTcpClient.Create(nil);
end;

function TIdSipTcpServer.DefaultTimeout: Cardinal;
begin
  Result := 5000;
end;

procedure TIdSipTcpServer.DestroyClient(Client: TIdSipTcpClient);
begin
  Client.Free;
end;

procedure TIdSipTcpServer.RemoveMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.ListenerLock.Acquire;
  try
    Self.Listeners.Remove(Pointer(Listener));
  finally
    Self.ListenerLock.Release;
  end;
end;

procedure TIdSipTcpServer.SendResponse(Response: TIdSipResponse);
var
  Connection:  TIdTCPConnection;
  Table:       TIdSipConnectionTable;
  Destination: TIdSipConnectionBindings;
begin
  Table := Self.ConnectionMap.LockList;
  try
    Connection := Table.ConnectionFor(Response);

    if Assigned(Connection) and Connection.Connected then
      Connection.Write(Response.AsString)
    else begin
      if Response.LastHop.HasReceived then begin
        Destination.PeerIP   := Response.LastHop.Received;
        Destination.PeerPort := Response.LastHop.Port;
      end
      else begin
        Destination.PeerIP   := Response.LastHop.SentBy;
        Destination.PeerPort := Response.LastHop.Port;
      end;

      Self.SendResponseTo(Response, Destination);
    end;
  finally
    Self.ConnectionMap.UnlockList;
  end;
end;

//* TIdSipTcpServer Protected methods ******************************************

procedure TIdSipTcpServer.DoDisconnect(AThread: TIdPeerThread);
var
  Table: TIdSipConnectionTable;
begin
  Table := Self.ConnectionMap.LockList;
  try
    inherited DoDisconnect(AThread);

    Table.Remove(AThread.Connection);
  finally
    Self.ConnectionMap.UnlockList;
  end;
end;

function TIdSipTcpServer.DoExecute(AThread: TIdPeerThread): Boolean;
var
  Msg:          TIdSipMessage;
  Parser:       TIdSipParser;
  ReceivedFrom: TIdSipConnectionBindings;
  S:            TStream;
begin
  Result := true;

  ReceivedFrom.PeerIP   := AThread.Connection.Socket.Binding.PeerIP;
  ReceivedFrom.PeerPort := AThread.Connection.Socket.Binding.PeerPort;

  while AThread.Connection.Connected do begin
    S := Self.ReadMessage(AThread.Connection);
    try
      Parser := TIdSipParser.Create;
      try
        Parser.Source := S;

        try
          Msg := Parser.ParseAndMakeMessage;
          try
            if not Msg.Headers.HasHeader(ContentLengthHeaderFull) then
              Msg.ContentLength := 0;
            Msg.Body := Self.ReadBody(AThread.Connection, Msg);

            if Msg.IsRequest then begin
              Self.AddConnection(AThread.Connection, Msg as TIdSipRequest);
              Self.NotifyListeners(Msg as TIdSipRequest, ReceivedFrom);
            end
            else
              Self.NotifyListeners(Msg as TIdSipResponse, ReceivedFrom);
          finally
            Msg.Free;
          end;
        except
          on E: EBadRequest do begin
            Self.ReturnBadRequest(AThread.Connection, E.Message, Parser);
            AThread.Connection.DisconnectSocket;
          end;
          on E: EBadResponse do begin
            // drop it on the floor
            AThread.Connection.DisconnectSocket;
          end;
          on E: Exception do begin
            Self.ReturnInternalServerError(AThread.Connection, E.Message, Parser);
            AThread.Connection.DisconnectSocket;
          end;
        end;
      finally
        Parser.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

//* TIdSipTcpServer Private methods ********************************************

procedure TIdSipTcpServer.AddConnection(Connection: TIdTCPConnection;
                                        Request: TIdSipRequest);
var
  Table:  TIdSipConnectionTable;
begin
  Table := Self.ConnectionMap.LockList;
  try
    Table.Add(Connection, Request);
  finally
    Self.ConnectionMap.UnlockList;
  end;
end;

procedure TIdSipTcpServer.NotifyListeners(Request: TIdSipRequest;
                                          ReceivedFrom: TIdSipConnectionBindings);
var
  I: Integer;
begin
  Self.ListenerLock.Acquire;
  try
    for I := 0 to Self.Listeners.Count - 1 do
      IIdSipMessageListener(Self.Listeners[I]).OnReceiveRequest(Request,
                                                                ReceivedFrom);
  finally
    Self.ListenerLock.Release;
  end;
end;

procedure TIdSipTcpServer.NotifyListeners(Response: TIdSipResponse;
                                          ReceivedFrom: TIdSipConnectionBindings);
var
  I: Integer;
begin
  Self.ListenerLock.Acquire;
  try
    for I := 0 to Self.Listeners.Count - 1 do
      IIdSipMessageListener(Self.Listeners[I]).OnReceiveResponse(Response,
                                                                 ReceivedFrom);
  finally
    Self.ListenerLock.Release;
  end;
end;

procedure TIdSipTcpServer.OnReadBodyTimeout(Sender: TObject);
begin
  (Sender as TIdSipTcpConnectionCutter).Connection.DisconnectSocket;
end;

function TIdSipTcpServer.ReadBody(Connection: TIdTCPConnection;
                                  Message: TIdSipMessage): String;
var
  Timer: TIdSipTcpConnectionCutter;
begin
  if (Self.ReadBodyTimeout > 0) then begin
    Timer := TIdSipTcpConnectionCutter.Create(true);
    try
      Timer.FreeOnTerminate := true;
      Timer.OnTimer := Self.OnReadBodyTimeout;
      Timer.Start;
    except
      Timer.Free;
    end;
  end;

  Result := Connection.ReadString(Message.ContentLength);
end;

function TIdSipTcpServer.ReadMessage(Connection: TIdTCPConnection): TStream;
var
  Str: TStringStream;
begin
  Result := TStringStream.Create('');
  try
    Str := Result as TStringStream;

    // we skip any leading CRLFs
    while (Str.DataString = '') do
      Connection.Capture(Str, '');
    Str.Seek(0, soFromBeginning);
  except
    Result.Free;

    raise;
  end;
end;

procedure TIdSipTcpServer.ReturnBadRequest(Connection: TIdTCPConnection;
                                           Reason: String;
                                           Parser: TIdSipParser);
var
  OwnVia: TIdSipViaHeader;
  Res:    TIdSipResponse;
begin
  Res := TIdSipResponse.Create;
  try
    // We really can't do much more than this. The message was unparseable,
    // so what else can we do?
    Res.StatusCode := SIPBadRequest;
    Res.StatusText := Reason;
    Res.SipVersion := SipVersion;

    OwnVia := Res.Headers.Add(ViaHeaderFull) as TIdSipViaHeader;
    OwnVia.Port       := Connection.Socket.Binding.Port;
    OwnVia.SentBy     := Connection.Socket.Binding.IP;
    OwnVia.SipVersion := SIPVersion;
    OwnVia.Transport  := sttUDP;

    // We need From, To, CSeq, Call-ID headers!

    Self.WriteMessage(Connection, Res);
  finally
    Res.Free;
  end;
end;

procedure TIdSipTcpServer.ReturnInternalServerError(Connection: TIdTCPConnection;
                                                    Reason: String;
                                                    Parser: TIdSipParser);
var
  Res: TIdSipResponse;
begin
  Res := TIdSipResponse.Create;
  try
    // We really can't do much more than this.
    Res.StatusCode := SIPInternalServerError;
    Res.StatusText := Reason;
    Res.SipVersion := SipVersion;

    Self.WriteMessage(Connection, Res);
  finally
    Res.Free;
  end;
end;

procedure TIdSipTcpServer.SendResponseTo(Response: TIdSipResponse;
                                         Dest: TIdSipConnectionBindings);
var
  Client: TIdSipTcpClient;
begin
  Client := Self.CreateClient;
  try
    Client.Host := Dest.PeerIP;
    Client.Port := Dest.PeerPort;

    Client.Connect(Self.ConnectionTimeout);
    try
      Client.Send(Response);
    finally
      Client.Disconnect;
    end;
  finally
    Self.DestroyClient(Client);
  end;
end;

procedure TIdSipTcpServer.WriteMessage(Connection: TIdTCPConnection;
                                       AMessage: TIdSipMessage);
begin
  Connection.Write(AMessage.AsString);
end;

end.
