unit IdSipTransport;

interface

uses
  Contnrs, IdException, IdSipHeaders, IdSipMessage, IdSipTcpClient,
  IdSipTcpServer, IdSipUdpServer, IdSocketHandle, IdSSLOpenSSL, IdTCPClient,
  IdTCPServer, SyncObjs, SysUtils;

const
  DefaultTimeout = 5000;

type
  TIdSipAbstractTransport = class;
  TIdSipAbstractTransportClass = class of TIdSipAbstractTransport;

  TIdSipAbstractTransport = class(TObject, IIdSipMessageVisitor)
  private
    fHostName:   String;
    fOnRequest:  TIdSipRequestEvent;
    fOnResponse: TIdSipResponseEvent;
    fTimeout:    Cardinal;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;

    procedure RewriteOwnVia(Message: TIdSipMessage);
  protected
    procedure DoOnRequest(const R: TIdSipRequest);
    procedure DoOnResponse(const R: TIdSipResponse);
    function  GetBindings: TIdSocketHandles; virtual; abstract;
    function  GetPort: Cardinal; virtual; abstract;
    procedure SendRequest(const R: TIdSipRequest); virtual;
    procedure SendResponse(const R: TIdSipResponse); virtual;
    procedure SetPort(const Value: Cardinal); virtual; abstract;
  public
    class function TransportFor(const TT: TIdSipTransportType): TIdSipAbstractTransportClass;
    constructor Create(const Port: Cardinal); virtual;

    function DefaultPort: Cardinal; virtual;
    function GetTransportType: TIdSipTransportType; virtual; abstract;
    function IsReliable: Boolean; virtual;
    function IsSecure: Boolean; virtual;
    // This should raise an appropriate exception if a transport or suchlike
    // error occurs.
    procedure Send(const Msg: TIdSipMessage);
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure VisitRequest(const Request: TIdSipRequest);
    procedure VisitResponse(const Response: TIdSipResponse);

    property Bindings:   TIdSocketHandles    read GetBindings;
    property OnRequest:  TIdSipRequestEvent  read fOnRequest write fOnRequest;
    property OnResponse: TIdSipResponseEvent read fOnResponse write fOnResponse;
    property HostName:   String              read fHostName write fHostName;
    property Port:       Cardinal            read GetPort write SetPort;
    property Timeout:    Cardinal            read fTimeout write fTimeout;
  end;

  TIdSipTransportClass = class of TIdSipAbstractTransport;

  TIdSipTCPTransport = class(TIdSipAbstractTransport)
  private
    Clients:    TObjectList;
    ClientLock: TCriticalSection;

    function  AddClient: TIdSipTcpClient;
    procedure DoOnClientFinished(Sender: TObject);
    procedure DoOnTcpRequest(Sender: TObject; const Request: TIdSipRequest);
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
    fOnGetPassword: TPasswordEvent;
    TLS:            TIdServerIOHandlerSSL;

    procedure DoOnGetPassword(var Password: String);
    function  GetRootCertificate: TFileName;
    function  GetServerCertificate: TFileName;
    function  GetServerKey: TFileName;
    procedure SetRootCertificate(const Value: TFileName);
    procedure SetServerCertificate(const Value: TFileName);
    procedure SetServerKey(const Value: TFileName);
  protected
    function  CreateClient: TIdSipTcpClient; override;
    procedure DestroyClient(Client: TIdSipTcpClient); override;
    function  ServerType: TIdSipTcpServerClass; override;
  public
    constructor Create(const Port: Cardinal); override;
    destructor  Destroy; override;

    function  DefaultPort: Cardinal; override;
    function  GetTransportType: TIdSipTransportType; override;
    function  IsSecure: Boolean; override;

    property OnGetPassword:     TPasswordEvent read fOnGetPassword write fOnGetPassword;
    property RootCertificate:   TFileName read GetRootCertificate write SetRootCertificate;
    property ServerCertificate: TFileName read GetServerCertificate write SetServerCertificate;
    property ServerKey:         TFileName read GetServerKey write SetServerKey;
  end;

  TIdSipSCTPTransport = class(TIdSipAbstractTransport)
  public
    function GetTransportType: TIdSipTransportType; override;
  end;

  TIdSipUDPTransport = class(TIdSipAbstractTransport)
  private
    Transport: TIdSipUdpServer;

    procedure DoOnReceiveRequest(Sender: TObject; const Request: TIdSipRequest);
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

  TIdSipMockTransport = class(TIdSipAbstractTransport)
  private
    fACKCount:          Cardinal;
    fLastACK:           TIdSipRequest;
    fLastRequest:       TIdSipRequest;
    fLastResponse:      TIdSipResponse;
    fFailWith:          ExceptClass;
    fSentRequestCount:  Cardinal;
    fSentResponseCount: Cardinal;
    fTransportType:     TIdSipTransportType;
  protected
    function  GetBindings: TIdSocketHandles; override;
    procedure SendRequest(const R: TIdSipRequest); override;
    procedure SendResponse(const R: TIdSipResponse); override;
  public
    constructor Create(const Port: Cardinal); override;
    destructor  Destroy; override;

    procedure FireOnRequest(const R: TIdSipRequest);
    procedure FireOnResponse(const R: TIdSipResponse);
    function  GetTransportType: TIdSipTransportType; override;
    function  IsReliable: Boolean; override;
    function  IsSecure: Boolean; override;
    procedure RaiseException(const E: ExceptClass);
    procedure ResetACKCount;
    procedure ResetSentRequestCount;
    procedure ResetSentResponseCount;
    procedure Start; override;
    procedure Stop; override;

    property ACKCount:          Cardinal            read fACKCount;
    property FailWith:          ExceptClass         read fFailWith write fFailWith;
    property LastACK:           TIdSipRequest       read fLastACK;
    property LastRequest:       TIdSipRequest       read fLastRequest;
    property LastResponse:      TIdSipResponse      read fLastResponse;
    property SentRequestCount:  Cardinal            read fSentRequestCount;
    property SentResponseCount: Cardinal            read fSentResponseCount;
    property TransportType:     TIdSipTransportType read fTransportType write fTransportType;
  end;

  EUnknownTransport = class(EIdException);

implementation

uses
  IdSipConsts, IdSipTlsServer, IdUDPClient;

//******************************************************************************
//* TIdSipAbstractTransport                                                    *
//******************************************************************************
//* TIdSipAbstractTransport Public methods *************************************

class function TIdSipAbstractTransport.TransportFor(const TT: TIdSipTransportType): TIdSipAbstractTransportClass;
begin
  case TT of
    sttSCTP: Result := TIdSipSCTPTransport;
    sttTCP:  Result := TIdSipTCPTransport;
    sttTLS:  Result := TIdSipTLSTransport;
    sttUDP:  Result := TIdSipUDPTransport;
  else
    raise EUnknownTransport.Create('TIdSipAbstractTransport.TransportFor');
  end;
end;

constructor TIdSipAbstractTransport.Create(const Port: Cardinal);
begin
  inherited Create;
end;

function TIdSipAbstractTransport.DefaultPort: Cardinal;
begin
  Result := IdPORT_SIP;
end;

function TIdSipAbstractTransport.IsReliable: Boolean;
begin
  Result := true;
end;

function TIdSipAbstractTransport.IsSecure: Boolean;
begin
  Result := false;
end;

procedure TIdSipAbstractTransport.Send(const Msg: TIdSipMessage);
begin
  Msg.Accept(Self);
end;

procedure TIdSipAbstractTransport.Start;
begin
end;

procedure TIdSipAbstractTransport.Stop;
begin
end;

procedure TIdSipAbstractTransport.VisitRequest(const Request: TIdSipRequest);
begin
  Self.SendRequest(Request);
end;

procedure TIdSipAbstractTransport.VisitResponse(const Response: TIdSipResponse);
begin
  Self.SendResponse(Response);
end;

//* TIdSipAbstractTransport Protected methods **********************************

procedure TIdSipAbstractTransport.DoOnRequest(const R: TIdSipRequest);
begin
  if Assigned(Self.OnRequest) then
    Self.OnRequest(Self, R);
end;

procedure TIdSipAbstractTransport.DoOnResponse(const R: TIdSipResponse);
begin
  // look for top Via header. If sent-by <> Self.Hostname then discard the request
  // cf. RFC 3261 section 18.1.2

  if Assigned(Self.OnResponse) then
    Self.OnResponse(Self, R);
end;

procedure TIdSipAbstractTransport.SendRequest(const R: TIdSipRequest);
begin
  Self.RewriteOwnVia(R);
end;

procedure TIdSipAbstractTransport.SendResponse(const R: TIdSipResponse);
begin
end;

//* TIdSipAbstractTransport Private methods ************************************

function TIdSipAbstractTransport._AddRef: Integer;
begin
  Result := -1;
end;

function TIdSipAbstractTransport._Release: Integer;
begin
  Result := -1;
end;

function TIdSipAbstractTransport.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := 0;
end;

procedure TIdSipAbstractTransport.RewriteOwnVia(Message: TIdSipMessage);
begin
  Message.LastHop.Transport := Self.GetTransportType;
  Message.LastHop.SentBy    := Self.HostName;
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
  Self.Transport.OnRequest  := Self.DoOnTcpRequest;
  Self.Transport.OnResponse := Self.DoOnTcpResponse;
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

  Client.Host    := R.RequestUri.Host;
  Client.Port    := StrToIntDef(R.RequestUri.Port, Self.DefaultPort);
  Client.Timeout := Self.Timeout;

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
begin
  Self.Transport.DefaultPort := Value;
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

procedure TIdSipTCPTransport.DoOnClientFinished(Sender: TObject);
begin
  Self.RemoveClient(Sender as TIdSipTcpClient);
end;

procedure TIdSipTCPTransport.DoOnTcpRequest(Sender: TObject; const Request: TIdSipRequest);
begin
  Self.DoOnRequest(Request);
end;

procedure TIdSipTCPTransport.DoOnTcpResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  Self.DoOnResponse(Response);
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

constructor TIdSipTLSTransport.Create(const Port: Cardinal);
begin
  inherited Create(Port);

  Self.TLS := TIdServerIOHandlerSSL.Create(nil);
  Self.TLS.OnGetPassword := Self.DoOnGetPassword;

  Self.Transport.IOHandler := Self.TLS;
end;

destructor TIdSipTLSTransport.Destroy;
begin
  Self.Transport.IOHandler := nil;
  Self.TLS.Free;

  inherited Destroy;
end;

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

procedure TIdSipTLSTransport.DoOnGetPassword(var Password: String);
begin
  if Assigned(Self.OnGetPassword) then
    Self.OnGetPassword(Password);
end;

function TIdSipTLSTransport.GetRootCertificate: TFileName;
begin
  Result := Self.TLS.SSLOptions.RootCertFile;
end;

function TIdSipTLSTransport.GetServerCertificate: TFileName;
begin
  Result := Self.TLS.SSLOptions.CertFile;
end;

function TIdSipTLSTransport.GetServerKey: TFileName;
begin
  Result := Self.TLS.SSLOptions.KeyFile;
end;

procedure TIdSipTLSTransport.SetRootCertificate(const Value: TFileName);
begin
  Self.TLS.SSLOptions.RootCertFile := Value;
end;

procedure TIdSipTLSTransport.SetServerCertificate(const Value: TFileName);
begin
  Self.TLS.SSLOptions.CertFile := Value;
end;

procedure TIdSipTLSTransport.SetServerKey(const Value: TFileName);
begin
  Self.TLS.SSLOptions.KeyFile := Value;
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
  Self.Transport.OnRequest  := Self.DoOnReceiveRequest;
  Self.Transport.OnResponse := Self.DoOnReceiveResponse;
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
    Client.Port := StrToIntDef(R.RequestUri.Port, IdPORT_SIP);

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
begin
  Self.Transport.DefaultPort := Value;
end;

//* TIdSipUDPTransport Private methods *****************************************

procedure TIdSipUDPTransport.DoOnReceiveRequest(Sender: TObject; const Request: TIdSipRequest);
begin
  Self.DoOnRequest(Request);
end;

procedure TIdSipUDPTransport.DoOnReceiveResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  Self.DoOnResponse(Response);
end;

//******************************************************************************
//* TIdSipMockTransport                                                        *
//******************************************************************************
//* TIdSipMockTransport Public methods *****************************************

constructor TIdSipMockTransport.Create(const Port: Cardinal);
begin
  inherited Create(Port);

  Self.ResetSentRequestCount;
  Self.fLastACK := TIdSipRequest.Create;
  Self.fLastRequest := TIdSipRequest.Create;
  Self.fLastResponse := TIdSipResponse.Create;
end;

destructor TIdSipMockTransport.Destroy;
begin
  Self.LastResponse.Free;
  Self.LastRequest.Free;
  Self.LastACK.Free;

  inherited Destroy;
end;

procedure TIdSipMockTransport.FireOnRequest(const R: TIdSipRequest);
begin
  Self.DoOnRequest(R);
end;

procedure TIdSipMockTransport.FireOnResponse(const R: TIdSipResponse);
begin
  Self.DoOnResponse(R);
end;

function TIdSipMockTransport.GetTransportType: TIdSipTransportType;
begin
  Result := Self.TransportType;
end;

function TIdSipMockTransport.IsReliable: Boolean;
begin
  Result := Self.TransportType <> sttUDP;
end;

function TIdSipMockTransport.IsSecure: Boolean;
begin
  Result := Self.TransportType = sttTLS;
end;

procedure TIdSipMockTransport.RaiseException(const E: ExceptClass);
begin
  raise E.Create('TIdSipMockTransport');
end;

procedure TIdSipMockTransport.ResetACKCount;
begin
  Self.fACKCount := 0;
end;

procedure TIdSipMockTransport.ResetSentRequestCount;
begin
  Self.fSentRequestCount := 0;
end;

procedure TIdSipMockTransport.ResetSentResponseCount;
begin
  Self.fSentResponseCount := 0;
end;

procedure TIdSipMockTransport.Start;
begin
end;

procedure TIdSipMockTransport.Stop;
begin
end;

//* TIdSipMockTransport Protected methods **************************************

function TIdSipMockTransport.GetBindings: TIdSocketHandles;
begin
  Result := nil;
end;

procedure TIdSipMockTransport.SendRequest(const R: TIdSipRequest);
begin
  inherited SendRequest(R);

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipMockTransport.SendRequest');

  if R.IsAck then begin
    Self.LastACK.Assign(R);
    Inc(Self.fACKCount)
  end
  else
    Inc(Self.fSentRequestCount);

  Self.LastRequest.Assign(R);

  Self.DoOnRequest(R);
end;

procedure TIdSipMockTransport.SendResponse(const R: TIdSipResponse);
begin
  inherited SendResponse(R);

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipMockTransport.SendResponse');

  Self.LastResponse.Assign(R);

  Inc(Self.fSentResponseCount);

  Self.DoOnResponse(R);
end;

end.
