unit IdSipTransport;

interface

uses
  Classes, Contnrs, IdException, IdSipHeaders, IdSipInterfacedObject,
  IdSipMessage, IdSipTcpClient, IdSipTcpServer, IdSipTlsServer, IdSipUdpServer,
  IdSocketHandle, IdSSLOpenSSL, IdTCPClient, IdTCPServer, SyncObjs, SysUtils;

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

  TIdSipTransport = class(TIdSipInterfacedObject,
                          IIdSipMessageVisitor,
                          IIdSipMessageListener)
  private
    fHostName:                    String;
    fTimeout:                     Cardinal;
    TransportListenerLock:        TCriticalSection;
    TransportListeners:           TList;
    TransportSendingListenerLock: TCriticalSection;
    TransportSendingListeners:    TList;

    procedure OnReceiveRequest(const Request: TIdSipRequest);
    procedure OnReceiveResponse(const Response: TIdSipResponse);
    procedure RewriteOwnVia(Msg: TIdSipMessage);
  protected
    function  GetBindings: TIdSocketHandles; virtual; abstract;
    function  GetPort: Cardinal; virtual; abstract;
    procedure NotifyTransportListeners(const Request: TIdSipRequest); overload;
    procedure NotifyTransportListeners(const Response: TIdSipResponse); overload;
    procedure NotifyTransportSendingListeners(const Request: TIdSipRequest); overload;
    procedure NotifyTransportSendingListeners(const Response: TIdSipResponse); overload;
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

    property Bindings:   TIdSocketHandles    read GetBindings;
    property HostName:   String              read fHostName write fHostName;
    property Port:       Cardinal            read GetPort write SetPort;
    property Timeout:    Cardinal            read fTimeout write fTimeout;
  end;

  TIdSipTCPTransport = class(TIdSipTransport)
  private
    Clients:    TObjectList;
    ClientLock: TCriticalSection;

    function  AddClient: TIdSipTcpClient;
    procedure DoOnClientFinished(Sender: TIdSipTcpClient);
    procedure DoOnTcpResponse(Sender: TObject; const Response: TIdSipResponse);
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

  TIdSipUDPTransport = class(TIdSipTransport)
  private
    Transport: TIdSipUdpServer;

    procedure DoOnReceiveResponse(Sender: TObject; const Response: TIdSipResponse);
  protected
    function  GetBindings: TIdSocketHandles; override;
    function  GetPort: Cardinal; override;
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
  end;

  EUnknownTransport = class(EIdException);

implementation

uses
  IdSipConsts, IdUDPClient;

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

procedure TIdSipTransport.OnReceiveRequest(const Request: TIdSipRequest);
begin
  Self.NotifyTransportListeners(Request);
end;

procedure TIdSipTransport.OnReceiveResponse(const Response: TIdSipResponse);
begin
  // cf. RFC 3261 section 18.1.2

  if Self.SentByIsRecognised(Response.LastHop) then begin
    Self.NotifyTransportListeners(Response);
  end;
end;

procedure TIdSipTransport.RewriteOwnVia(Msg: TIdSipMessage);
begin
  Msg.LastHop.Transport := Self.GetTransportType;
  Msg.LastHop.SentBy    := Self.HostName;
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

  Client.LocalHostName := Self.HostName;
  Client.Host          := R.RequestUri.Host;
  Client.Port          := R.RequestUri.Port;
  Client.Timeout       := Self.Timeout;

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

procedure TIdSipTCPTransport.DoOnClientFinished(Sender: TIdSipTcpClient);
begin
  Self.RemoveClient(Sender);
end;

procedure TIdSipTCPTransport.DoOnTcpResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  Self.NotifyTransportListeners(Response);
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
//* TIdSipUDPTransport                                                         *
//******************************************************************************
//* TIdSipUDPTransport Public methods ******************************************

constructor TIdSipUDPTransport.Create(const Port: Cardinal);
begin
  inherited Create(Port);

  Self.Transport := TIdSipUdpServer.Create(nil);
  Self.SetPort(Port);
  Self.Transport.AddMessageListener(Self);
end;

destructor TIdSipUDPTransport.Destroy;
begin
  Self.Transport.Free;

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
end;

procedure TIdSipUDPTransport.Stop;
begin
  Self.Transport.Active := false;
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

procedure TIdSipUDPTransport.SendRequest(const R: TIdSipRequest);
var
  Client:   TIdUdpClient;
  P:        TIdSipParser;
  Response: TIdSipResponse;
  Reply:    String;
begin
  inherited SendRequest(R);

  Client := TIdUdpClient.Create(nil);
  try
    Client.Host := R.RequestUri.Host;
    Client.Port := R.RequestUri.Port;

    Client.Send(R.AsString);

    P := TIdSipParser.Create;
    try
      Reply := Client.ReceiveString(Self.Timeout);
      while (Reply <> '') do begin
        Response := P.ParseAndMakeResponse(Reply);
        try
          Self.DoOnReceiveResponse(Self, Response);
        finally
          Response.Free;
        end;

        Reply := Client.ReceiveString(Self.Timeout);
      end;
    finally
      P.Free;
    end;
  finally
    Client.Free;
  end;
end;

procedure TIdSipUDPTransport.SendResponse(const R: TIdSipResponse);
var
  Client: TIdUdpClient;
begin
  inherited SendResponse(R);

  Client := TIdUdpClient.Create(nil);
  try
    if R.LastHop.HasReceived then begin
      Client.Host := R.LastHop.Received;
      Client.Port := R.LastHop.Port;
    end
    else begin
      Client.Host := R.LastHop.SentBy;
      Client.Port := R.LastHop.Port;
    end;

    Client.Send(R.AsString);
  finally
    Client.Free;
  end;
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

procedure TIdSipUDPTransport.DoOnReceiveResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  Self.NotifyTransportListeners(Response);
end;

end.
