unit IdSipTcpServer;

interface

uses
  Classes, Contnrs, IdSipConsts, IdSipMessage, IdSipTcpClient, IdSipTimer,
  IdSipUdpServer, IdTCPConnection, IdTCPServer, SyncObjs, SysUtils;

type
  TIdSipTcpConnectionCutter = class(TIdSipTimer)
  private
    fConnection: TIdTCPConnection;
  public
    property Connection: TIdTCPConnection read fConnection write fConnection;
  end;

  TIdSipConnectionTableEntry = class(TObject)
  private
    fConnection: TIdTCPConnection;
    fRequest:    TIdSipRequest;
  public
    constructor Create(const Connection: TIdTCPConnection;
                       const Request:    TIdSipRequest);
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

    procedure Add(const Connection: TIdTCPConnection;
                  const Request:    TIdSipRequest);
    function  ConnectionFor(const Request: TIdSipRequest): TIdTCPConnection; overload;
    function  ConnectionFor(const Response: TIdSipResponse): TIdTCPConnection; overload;
    function  Count: Integer;
    procedure Remove(const Connection: TIdTCPConnection);
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

  // ReadBodyTimeout = 0 implies that we never timeout the body wait. This is
  // not recommended. ReadBodyTimeout = n implies we wait n milliseconds for
  // the body to be received. If we haven't read Content-Length bytes by the
  // time the timeout occurs, we sever the connection.
  TIdSipTcpServer = class(TIdTCPServer, IIdSipMessageVisitor)
  private
    ConnectionMap:    TIdSipConnectionTableLock;
    fOnRequest:       TIdSipRequestEvent;
    fOnResponse:      TIdSipResponseEvent;
    fReadBodyTimeout: Cardinal;

    procedure AddConnection(const Connection: TIdTCPConnection; const Request: TIdSipRequest);
    procedure DoOnRequest(Sender: TObject; const Request: TIdSipRequest);
    procedure DoOnResponse(Sender: TObject; const Response: TIdSipResponse);
    procedure OnReadBodyTimeout(Sender: TObject);
    function  ReadBody(Connection: TIdTCPConnection; Message: TIdSipMessage): String;
    function  ReadMessage(Connection: TIdTCPConnection): TStream;
    procedure ReturnBadRequest(Connection: TIdTCPConnection; Reason: String; Parser: TIdSipParser);
    procedure ReturnInternalServerError(Connection: TIdTCPConnection; Reason: String; Parser: TIdSipParser);
    procedure SendResponseTo(const Response: TIdSipResponse; Dest: TIdSipIPTarget);
    procedure WriteMessage(Connection: TIdTCPConnection; AMessage: TIdSipMessage);
  protected
    procedure DoDisconnect(AThread: TIdPeerThread); override;
    function  DoExecute(AThread: TIdPeerThread): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  CreateClient: TIdSipTcpClient; virtual;
    procedure DestroyClient(Client: TIdSipTcpClient); virtual;

    procedure SendResponse(const Response: TIdSipResponse);
    procedure VisitRequest(const Request: TIdSipRequest);
    procedure VisitResponse(const Response: TIdSipResponse);
  published
    property DefaultPort default IdPORT_SIP;
    property OnRequest:       TIdSipRequestEvent  read fOnRequest write fOnRequest;
    property OnResponse:      TIdSipResponseEvent read fOnResponse write fOnResponse;
    property ReadBodyTimeout: Cardinal            read fReadBodyTimeout write fReadBodyTimeout;
  end;

  TIdSipTcpServerClass = class of TIdSipTcpServer;

const
  DefaultTimeout = 5000;

implementation

uses
  IdSipTransaction, IdSipHeaders;

//******************************************************************************
//* TIdSipConnectionTableEntry                                                 *
//******************************************************************************
//* TIdSipConnectionTableEntry Public methods **********************************

constructor TIdSipConnectionTableEntry.Create(const Connection: TIdTCPConnection;
                                              const Request:    TIdSipRequest);
begin
  inherited Create;

  Self.fConnection := Connection;
  Self.fRequest := TIdSipRequest.Create;
  Self.fRequest.Assign(Request);
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

procedure TIdSipConnectionTable.Add(const Connection: TIdTCPConnection;
                                    const Request:    TIdSipRequest);
begin
  Self.List.Add(TIdSipConnectionTableEntry.Create(Connection, Request));
end;

function TIdSipConnectionTable.ConnectionFor(const Request: TIdSipRequest): TIdTCPConnection;
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

function TIdSipConnectionTable.ConnectionFor(const Response: TIdSipResponse): TIdTCPConnection;
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

procedure TIdSipConnectionTable.Remove(const Connection: TIdTCPConnection);
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

  Self.ConnectionMap := TIdSipConnectionTableLock.Create;
  Self.DefaultPort   := IdPORT_SIP;
end;

destructor TIdSipTcpServer.Destroy;
begin
  Self.ConnectionMap.Free;

  inherited Destroy;
end;

function TIdSipTcpServer.CreateClient: TIdSipTcpClient;
begin
  Result := TIdSipTcpClient.Create(nil);
end;

procedure TIdSipTcpServer.DestroyClient(Client: TIdSipTcpClient);
begin
  Client.Free;
end;

procedure TIdSipTcpServer.SendResponse(const Response: TIdSipResponse);
var
  Connection:  TIdTCPConnection;
  Table:       TIdSipConnectionTable;
  Destination: TIdSipIPTarget;
begin
  Table := Self.ConnectionMap.LockList;
  try
    Connection := Table.ConnectionFor(Response);

    if Assigned(Connection) and Connection.Connected then
      Connection.Write(Response.AsString)
    else begin
      if Response.LastHop.HasReceived then begin
        Destination.IP   := Response.LastHop.Received;
        Destination.Port := Response.LastHop.Port;
      end
      else begin
        Destination.IP   := Response.LastHop.SentBy;
        Destination.Port := Response.LastHop.Port;
      end;

      Self.SendResponseTo(Response, Destination);
    end;
  finally
    Self.ConnectionMap.UnlockList;
  end;
end;

procedure TIdSipTcpServer.VisitRequest(const Request: TIdSipRequest);
begin
  Self.DoOnRequest(Self, Request);
end;

procedure TIdSipTcpServer.VisitResponse(const Response: TIdSipResponse);
begin
  Self.DoOnResponse(Self, Response);
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
  Msg:    TIdSipMessage;
  Parser: TIdSipParser;
  S:      TStream;
begin
  Result := true;

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

            if TIdSipParser.IsFQDN(Msg.LastHop.SentBy)
              or (Msg.LastHop.SentBy <> AThread.Connection.Socket.Binding.IP) then
              Msg.LastHop.Received := AThread.Connection.Socket.Binding.IP;

            if Msg.IsRequest then
              Self.AddConnection(AThread.Connection, Msg as TIdSipRequest);

            Msg.Accept(Self);
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

procedure TIdSipTcpServer.AddConnection(const Connection: TIdTCPConnection; const Request: TIdSipRequest);
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

procedure TIdSipTcpServer.DoOnRequest(Sender: TObject; const Request: TIdSipRequest);
begin
  if Assigned(Self.OnRequest) then
    Self.OnRequest(Self, Request);
end;

procedure TIdSipTcpServer.DoOnResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  if Assigned(Self.OnResponse) then
    Self.OnResponse(Self, Response);
end;

procedure TIdSipTcpServer.OnReadBodyTimeout(Sender: TObject);
begin
  (Sender as TIdSipTcpConnectionCutter).Connection.DisconnectSocket;
end;

function TIdSipTcpServer.ReadBody(Connection: TIdTCPConnection; Message: TIdSipMessage): String;
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

procedure TIdSipTcpServer.ReturnBadRequest(Connection: TIdTCPConnection; Reason: String; Parser: TIdSipParser);
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
    OwnVia.SipVersion := 'SIP/2.0';
    OwnVia.Transport  := sttUDP;

    Self.WriteMessage(Connection, Res);
  finally
    Res.Free;
  end;
end;

procedure TIdSipTcpServer.ReturnInternalServerError(Connection: TIdTCPConnection; Reason: String; Parser: TIdSipParser);
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

procedure TIdSipTcpServer.SendResponseTo(const Response: TIdSipResponse;
                                               Dest: TIdSipIPTarget);
var
  Client: TIdSipTcpClient;
begin
  Client := Self.CreateClient;
  try
    Client.Host := Dest.IP;
    Client.Port := Dest.Port;

    Client.Connect(DefaultTimeout);
    try
      Client.Send(Response);
    finally
      Client.Disconnect;
    end;
  finally
    Self.DestroyClient(Client);
  end;
end;

procedure TIdSipTcpServer.WriteMessage(Connection: TIdTCPConnection; AMessage: TIdSipMessage);
begin
  Connection.Write(AMessage.AsString);
end;

end.
