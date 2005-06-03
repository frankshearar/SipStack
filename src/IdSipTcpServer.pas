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
  Classes, Contnrs, IdNotification, IdSipConsts, IdSipLocator, IdSipMessage,
  IdSipServerNotifier, IdSipTcpClient, IdTCPConnection, IdTCPServer,
  IdTimerQueue, SyncObjs, SysUtils;

type
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
    procedure SetOnAddConnection(Value: TIdSipAddConnectionEvent);
    procedure SetOnRemoveConnection(Value: TIdSipRemoveConnectionEvent);
    procedure SetReadTimeout(Value: Integer);
    procedure SetTimer(Value: TIdTimerQueue);
  protected
    procedure DoDisconnect(Thread: TIdPeerThread); override;
    procedure DoOnExecute(Thread: TIdPeerThread);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure AddMessageListener(const Listener: IIdSipMessageListener);
    function  CreateClient: TIdSipTcpClient; virtual;
    function  DefaultTimeout: Cardinal; virtual;
    procedure DestroyClient(Client: TIdSipTcpClient); virtual;
    procedure RemoveMessageListener(const Listener: IIdSipMessageListener);
  published
    property ConnectionTimeout:  Integer                     read fConnectionTimeout write fConnectionTimeout;
    property OnAddConnection:    TIdSipAddConnectionEvent    read GetOnAddConnection write SetOnAddConnection;
    property OnRemoveConnection: TIdSipRemoveConnectionEvent read GetOnRemoveConnection write SetOnRemoveConnection;
    property ReadTimeout:        Integer                     read GetReadTimeout write SetReadTimeout;
    property Timer:              TIdTimerQueue               read GetTimer write SetTimer;
  end;

  TIdSipTcpServerClass = class of TIdSipTcpServer;

implementation

uses
  IdException, IdSipTransport;

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

procedure TIdSipTcpServer.AddMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.MessageReader.Notifier.AddMessageListener(Listener);
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

procedure TIdSipTcpServer.RemoveMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.MessageReader.Notifier.RemoveMessageListener(Listener);
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

end.
