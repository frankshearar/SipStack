{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipTcpServer;

interface

uses
  Classes, Contnrs, IdNotification, IdSipConsts, IdSipMessage,
  IdSipServerNotifier, IdSipTcpClient, IdTCPConnection, IdTCPServer, SyncObjs,
  SysUtils;

type
  // I relate a request with a TCP connection. I store a COPY of a request
  // while storing a REFERENCE to a connection. Transports construct requests
  // and so bear responsibility for destroying them, and I need to remember
  //these requests.
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

    function  LockList: TIdSipConnectionTable;
    procedure UnlockList;
  end;

  // ReadTimeout = -1 implies that we never timeout the body wait. We do not
  // recommend this. ReadTimeout = n implies we wait n milliseconds for
  // the body to be received. If we haven't read Content-Length bytes by the
  // time the timeout occurs, we sever the connection.
  TIdSipTcpServer = class(TIdTCPServer)
  private
    ConnectionMap:      TIdSipConnectionTableLock;
    fConnectionTimeout: Integer;
    fReadTimeout:   Integer;
    Notifier:           TIdSipServerNotifier;

    procedure AddConnection(Connection: TIdTCPConnection;
                            Request: TIdSipRequest);
    function  ReadBody(Connection: TIdTCPConnection;
                       Message: TIdSipMessage): String;
    function  ReadMessage(Connection: TIdTCPConnection): TStream;
    procedure ReturnInternalServerError(Connection: TIdTCPConnection;
                                        Reason: String);
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
    property ConnectionTimeout: Integer read fConnectionTimeout write fConnectionTimeout;
    property ReadTimeout:       Integer read fReadTimeout write fReadTimeout;
  end;

  TIdSipTcpServerClass = class of TIdSipTcpServer;

implementation

uses
  IdException, IdSipTransport;

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
    and not (Self.List[I] as TIdSipConnectionTableEntry).Request.Equals(Request) do
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
  Self.DefaultPort       := TIdSipTransport.TransportFor(TcpTransport).DefaultPort;
  Self.Notifier          := TIdSipServerNotifier.Create;
  Self.ReadTimeout   := Self.DefaultTimeout;
end;

destructor TIdSipTcpServer.Destroy;
begin
  Self.Notifier.Free;
  Self.ConnectionMap.Free;

  inherited Destroy;
end;

procedure TIdSipTcpServer.AddMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.Notifier.AddMessageListener(Listener);
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
  Self.Notifier.RemoveMessageListener(Listener);
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
  ReceivedFrom: TIdSipConnectionBindings;
  S:            TStream;
  ConnTimedOut: Boolean;
begin
  ConnTimedOut := false;
  Result       := true;

  ReceivedFrom.PeerIP   := AThread.Connection.Socket.Binding.PeerIP;
  ReceivedFrom.PeerPort := AThread.Connection.Socket.Binding.PeerPort;

  while AThread.Connection.Connected do begin
    AThread.Connection.ReadTimeout := Self.ReadTimeout;

    S := Self.ReadMessage(AThread.Connection);
    try
      Msg := TIdSipMessage.ReadMessageFrom(S);
      try
        try
          try
            Msg.Body := Self.ReadBody(AThread.Connection, Msg);
          except
            on EIdReadTimeout do
              ConnTimedOut := true;
            on EIdConnClosedGracefully do
              ConnTimedOut := true;
          end;

          if Msg.IsRequest then begin
            // If Self.ReadBody closes the connection, we don't want to AddConnection!
            if not ConnTimedOut then
              Self.AddConnection(AThread.Connection, Msg as TIdSipRequest);

            Self.Notifier.NotifyListenersOfRequest(Msg as TIdSipRequest,
                                                   ReceivedFrom);
          end
          else
            Self.Notifier.NotifyListenersOfResponse(Msg as TIdSipResponse,
                                                    ReceivedFrom);
        except
          on E: Exception do begin
            // This results in returning a 500 Internal Server Error to a response!
            Self.ReturnInternalServerError(AThread.Connection, E.Message);
            AThread.Connection.DisconnectSocket;
            Self.Notifier.NotifyListenersOfException(E,
                                                     'TCP Server: ' + E.Message);
          end;
        end;
      finally
        Msg.Free;
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

function TIdSipTcpServer.ReadBody(Connection: TIdTCPConnection;
                                  Message: TIdSipMessage): String;
begin
  Result := Connection.ReadString(Message.ContentLength);
end;

function TIdSipTcpServer.ReadMessage(Connection: TIdTCPConnection): TStream;
var
  Str: TStringStream;
begin
  Result := TStringStream.Create('');
  try
    Str := Result as TStringStream;

    // We skip any leading CRLFs, and read up to the first blank line.
    while (Str.DataString = '') do
      Connection.Capture(Str, '');
      
    Str.Seek(0, soFromBeginning);
  except
    Result.Free;

    raise;
  end;
end;

procedure TIdSipTcpServer.ReturnInternalServerError(Connection: TIdTCPConnection;
                                                    Reason: String);
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
