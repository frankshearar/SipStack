unit IdSipTransport;

interface

uses
  Classes, Contnrs, IdException, IdSipConsts, IdSipHeaders, IdSipMessage,
  IdSSLOpenSSL, IdSipTcpClient, IdSipTcpServer, IdSipUdpServer, IdTCPClient,
  IdTCPServer, IdUDPServer, SyncObjs, SysUtils;

const
  DefaultTimeout = 5000;

type
  TIdSipAbstractTransport = class;
  TIdSipAbstractTransportClass = class of TIdSipAbstractTransport;

  TIdSipAbstractTransport = class(TObject)
  private
    fHostName:   String;
    fOnRequest:  TIdSipRequestEvent;
    fOnResponse: TIdSipResponseEvent;

    procedure RewriteOwnVia(Message: TIdSipMessage);
  protected
    procedure DoOnRequest(Sender: TObject; const R: TIdSipRequest);
    procedure DoOnResponse(Sender: TObject; const R: TIdSipResponse);
    function  GetPort: Cardinal; virtual; abstract;
    procedure SetPort(const Value: Cardinal); virtual; abstract;
  public
    class function TransportFor(const TT: TIdSipTransportType): TIdSipAbstractTransportClass;
    constructor Create(const Port: Cardinal); virtual;

    function  DefaultPort: Cardinal; virtual;
    function GetTransportType: TIdSipTransportType; virtual; abstract;
    function IsReliable: Boolean; virtual; abstract;
    // This should raise an appropriate exception if a transport or suchlike
    // error occurs.
    procedure SendRequest(const R: TIdSipRequest;
                          const Timeout: Cardinal = DefaultTimeout); virtual;
    procedure SendResponse(const R: TIdSipResponse); virtual;
    procedure Start; virtual;
    procedure Stop; virtual;

    property OnRequest:  TIdSipRequestEvent  read fOnRequest write fOnRequest;
    property OnResponse: TIdSipResponseEvent read fOnResponse write fOnResponse;
    property HostName:   String              read fHostName write fHostName;
    property Port:       Cardinal            read GetPort write SetPort;
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
    Server: TIdSipTcpServer;

    function  CreateClient: TIdSipTcpClient; virtual;
    procedure DestroyClient(Client: TIdSipTcpClient); virtual;
    function  GetPort: Cardinal; override;
    function  ServerType: TIdSipTcpServerClass; virtual;
    procedure SetPort(const Value: Cardinal); override;
  public
    constructor Create(const Port: Cardinal); override;
    destructor  Destroy; override;

    function  GetTransportType: TIdSipTransportType; override;
    function  IsReliable: Boolean; override;
    procedure SendRequest(const R: TIdSipRequest;
                          const Timeout: Cardinal = DefaultTimeout); override;
    procedure SendResponse(const R: TIdSipResponse); override;
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

    property OnGetPassword:     TPasswordEvent read fOnGetPassword write fOnGetPassword;
    property RootCertificate:   TFileName read GetRootCertificate write SetRootCertificate;
    property ServerCertificate: TFileName read GetServerCertificate write SetServerCertificate;
    property ServerKey:         TFileName read GetServerKey write SetServerKey;
  end;

  TIdSipSCTPTransport = class(TIdSipAbstractTransport)
  public
    function GetTransportType: TIdSipTransportType; override;
    function IsReliable: Boolean; override;
  end;

  TIdSipUDPTransport = class(TIdSipAbstractTransport)
  private
    Transport: TIdSipUdpServer;

    procedure DoOnReceiveRequest(Sender: TObject; const Request: TIdSipRequest);
    procedure DoOnReceiveResponse(Sender: TObject; const Response: TIdSipResponse);
  protected
    function  GetPort: Cardinal; override;
    procedure SetPort(const Value: Cardinal); override;
  public
    constructor Create(const Port: Cardinal); override;
    destructor  Destroy; override;

    function  GetTransportType: TIdSipTransportType; override;
    function  IsReliable: Boolean; override;
    procedure SendRequest(const R: TIdSipRequest;
                          const Timeout: Cardinal = DefaultTimeout); override;
    procedure SendResponse(const R: TIdSipResponse); override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TIdSipMockTransport = class(TIdSipAbstractTransport)
  private
    fACKCount:          Cardinal;
    fIsReliable:        Boolean;
    fLastACK:           TIdSipRequest;
    fLastRequest:       TIdSipRequest;
    fLastResponse:      TIdSipResponse;
    fFailWith:          ExceptClass;
    fSentRequestCount:  Cardinal;
    fSentResponseCount: Cardinal;
    fTransportType:     TIdSipTransportType;
  public
    constructor Create(const Port: Cardinal); override;
    destructor  Destroy; override;

    procedure FireOnRequest(const R: TIdSipRequest);
    procedure FireOnResponse(const R: TIdSipResponse);
    function  GetTransportType: TIdSipTransportType; override;
    function  IsReliable: Boolean; override;
    procedure RaiseException(const E: ExceptClass);
    procedure ResetACKCount;
    procedure ResetSentRequestCount;
    procedure ResetSentResponseCount;
    procedure SendRequest(const R: TIdSipRequest;
                          const Timeout: Cardinal = DefaultTimeout); override;
    procedure SendResponse(const R: TIdSipResponse); override;
    procedure SetReliable(const Reliable: Boolean);
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
  IdSipTlsServer, IdUDPClient, IdURI;

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

procedure TIdSipAbstractTransport.SendRequest(const R: TIdSipRequest;
                                              const Timeout: Cardinal = DefaultTimeout);
begin
  Self.RewriteOwnVia(R);
end;

procedure TIdSipAbstractTransport.SendResponse(const R: TIdSipResponse);
begin
end;

procedure TIdSipAbstractTransport.Start;
begin
end;

procedure TIdSipAbstractTransport.Stop;
begin
end;

//* TIdSipAbstractTransport Protected methods **********************************

procedure TIdSipAbstractTransport.DoOnRequest(Sender: TObject; const R: TIdSipRequest);
begin
  if Assigned(Self.OnRequest) then
    Self.OnRequest(Sender, R);
end;

procedure TIdSipAbstractTransport.DoOnResponse(Sender: TObject; const R: TIdSipResponse);
begin
  // look for top Via header. If sent-by <> Self.Hostname then discard the request
  // cf. RFC 3261 section 18.1.2

  if Assigned(Self.OnResponse) then
    Self.OnResponse(Sender, R);
end;

//* TIdSipAbstractTransport Private methods ************************************

procedure TIdSipAbstractTransport.RewriteOwnVia(Message: TIdSipMessage);
begin
  Message.LastHop.Transport := Self.GetTransportType;
  Message.LastHop.SentBy    := Self.HostName;
end;

//******************************************************************************
//* TIdSipTcpTransport                                                         *
//******************************************************************************
//* TIdSipTcpTransport Public methods ******************************************

constructor TIdSipTcpTransport.Create(const Port: Cardinal);
begin
  inherited Create(Port);

  Self.Clients    := TObjectList.Create(true);
  Self.ClientLock := TCriticalSection.Create;
  Self.Server     := Self.ServerType.Create(nil);
  Self.SetPort(Port);
  Self.Server.OnRequest  := Self.DoOnTcpRequest;
  Self.Server.OnResponse := Self.DoOnTcpResponse;
end;

destructor TIdSipTcpTransport.Destroy;
begin
  Self.Server.Free;
  Self.ClientLock.Free;
  Self.Clients.Free;

  inherited Destroy;
end;

function TIdSipTcpTransport.GetTransportType: TIdSipTransportType;
begin
  Result := sttTCP;
end;

function TIdSipTcpTransport.IsReliable: Boolean;
begin
  Result := true;
end;

procedure TIdSipTcpTransport.SendRequest(const R: TIdSipRequest;
                                         const Timeout: Cardinal = DefaultTimeout);
var
  Client: TIdSipTcpClient;
begin
  inherited SendRequest(R, Timeout);

  Client := Self.AddClient;

  Client.Host    := R.RequestUri.Host;
  Client.Port    := StrToIntDef(R.RequestUri.Port, Self.DefaultPort);
  Client.Timeout := Timeout;

  Client.Connect(Timeout);
  Client.Send(R);
end;

procedure TIdSipTcpTransport.SendResponse(const R: TIdSipResponse);
begin
       inherited SendResponse(R);

  Self.Server.SendResponse(R);
end;

procedure TIdSipTcpTransport.Start;
begin
  Self.Server.Active := true;
end;

procedure TIdSipTcpTransport.Stop;
begin
  Self.Server.Active := false;
end;

//* TIdSipTcpTransport Protected methods ***************************************

function TIdSipTcpTransport.CreateClient: TIdSipTcpClient;
begin
  // Precondition: Self.ClientLock has been acquired.
  Result := TIdSipTcpClient.Create(nil);
  Result.OnFinished := Self.DoOnClientFinished;
  Result.OnResponse := Self.DoOnTcpResponse;
end;

function TIdSipTcpTransport.GetPort: Cardinal;
begin
  Result := Self.Server.DefaultPort;
end;

function TIdSipTcpTransport.ServerType: TIdSipTcpServerClass;
begin
  Result := TIdSipTcpServer;
end;

procedure TIdSipTcpTransport.DestroyClient(Client: TIdSipTcpClient);
begin
  // Precondition: Self.ClientLock has been acquired.
  if Client.Connected then
    Client.Disconnect;

  Self.Clients.Remove(Client);
end;

procedure TIdSipTcpTransport.SetPort(const Value: Cardinal);
begin
  Self.Server.DefaultPort := Value;
end;

//* TIdSipTcpTransport Private methods *****************************************

function TIdSipTcpTransport.AddClient: TIdSipTcpClient;
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

procedure TIdSipTcpTransport.DoOnClientFinished(Sender: TObject);
begin
  Self.RemoveClient(Sender as TIdSipTcpClient);
end;

procedure TIdSipTcpTransport.DoOnTcpRequest(Sender: TObject; const Request: TIdSipRequest);
begin
  Self.DoOnRequest(Sender, Request);
end;

procedure TIdSipTcpTransport.DoOnTcpResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  Self.DoOnResponse(Sender, Response);
end;

procedure TIdSipTcpTransport.RemoveClient(Client: TIdSipTcpClient);
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

  Self.Server.IOHandler := Self.TLS;
end;

destructor TIdSipTLSTransport.Destroy;
begin
  Self.Server.IOHandler := nil;
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

function TIdSipSCTPTransport.IsReliable: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TIdSipUdpTransport                                                         *
//******************************************************************************
//* TIdSipUdpTransport Public methods ******************************************

constructor TIdSipUdpTransport.Create(const Port: Cardinal);
begin
  inherited Create(Port);

  Self.Transport := TIdSipUdpServer.Create(nil);
  Self.SetPort(Port);
  Self.Transport.OnRequest  := Self.DoOnReceiveRequest;
  Self.Transport.OnResponse := Self.DoOnReceiveResponse;
end;

destructor TIdSipUdpTransport.Destroy;
begin
  Self.Transport.Free;

  inherited Destroy;
end;

function TIdSipUdpTransport.GetTransportType: TIdSipTransportType;
begin
  Result := sttUDP;
end;

function TIdSipUdpTransport.IsReliable: Boolean;
begin
  Result := false;
end;

procedure TIdSipUdpTransport.SendRequest(const R: TIdSipRequest;
                                         const Timeout: Cardinal = DefaultTimeout);
var
  Client:   TIdUdpClient;
  P:        TIdSipParser;
  Response: TIdSipResponse;
  Reply:    String;
begin
  inherited SendRequest(R, Timeout);

  Client := TIdUdpClient.Create(nil);
  try
    Client.Host := R.RequestUri.Host;
    Client.Port := StrToIntDef(R.RequestUri.Port, IdPORT_SIP);

    Client.Send(R.AsString);

    P := TIdSipParser.Create;
    try
      Reply := Client.ReceiveString(DefaultTimeout);
      while (Reply <> '') do begin
        Response := P.ParseAndMakeResponse(Reply);
        try
          Self.DoOnReceiveResponse(Self, Response);
        finally
          Response.Free;
        end;

        Reply := Client.ReceiveString(DefaultTimeout);
      end;
    finally
      P.Free;
    end;
  finally
    Client.Free;
  end;
end;

procedure TIdSipUdpTransport.SendResponse(const R: TIdSipResponse);
var
  Client: TIdUdpClient;
begin
  inherited SendResponse(R);

  Client := TIdUdpClient.Create(nil);
  try
    Client.Host := R.LastHop.SentBy;
    Client.Port := R.LastHop.Port;

    Client.Send(R.AsString);
  finally
    Client.Free;
  end;
end;

procedure TIdSipUdpTransport.Start;
begin
  Self.Transport.Active := true;
end;

procedure TIdSipUdpTransport.Stop;
begin
  Self.Transport.Active := false;
end;

//* TIdSipTcpTransport Protected methods ***************************************

function TIdSipUdpTransport.GetPort: Cardinal;
begin
  Result := Self.Transport.DefaultPort;
end;

procedure TIdSipUdpTransport.SetPort(const Value: Cardinal);
begin
  Self.Transport.DefaultPort := Value;
end;

//* TIdSipTcpTransport Private methods *****************************************

procedure TIdSipUdpTransport.DoOnReceiveRequest(Sender: TObject; const Request: TIdSipRequest);
begin
  Self.DoOnRequest(Sender, Request);
end;

procedure TIdSipUdpTransport.DoOnReceiveResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  Self.DoOnResponse(Sender, Response);
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
  Self.DoOnRequest(Self, R);
end;

procedure TIdSipMockTransport.FireOnResponse(const R: TIdSipResponse);
begin
  Self.DoOnResponse(Self, R);
end;

function TIdSipMockTransport.GetTransportType: TIdSipTransportType;
begin
  Result := Self.TransportType;
end;

function TIdSipMockTransport.IsReliable: Boolean;
begin
  Result := Self.fIsReliable;
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

procedure TIdSipMockTransport.SendRequest(const R: TIdSipRequest;
                                          const Timeout: Cardinal = DefaultTimeout);
begin
  inherited SendRequest(R, Timeout);

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipMockTransport.SendRequest');

  if R.IsAck then begin
    Self.LastACK.Assign(R);
    Inc(Self.fACKCount)
  end
  else
    Inc(Self.fSentRequestCount);

  Self.LastRequest.Assign(R);

  Self.DoOnRequest(Self, R);
end;

procedure TIdSipMockTransport.SendResponse(const R: TIdSipResponse);
begin
  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipMockTransport.SendResponse');

  Self.LastResponse.Assign(R);

  Inc(Self.fSentResponseCount);

  Self.DoOnResponse(Self, R);
end;

procedure TIdSipMockTransport.SetReliable(const Reliable: Boolean);
begin
  Self.fIsReliable := Reliable;
end;

procedure TIdSipMockTransport.Start;
begin
end;

procedure TIdSipMockTransport.Stop;
begin
end;

end.
