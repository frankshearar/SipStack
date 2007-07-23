{
  (c) 2006 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipTlsTransport;

interface

uses
  Classes, IdSipTcpTransport, IdSipTransport, IdSSLOpenSSL, SysUtils;

type
  TIdSipTlsServer = class;

  // I implement the Transport Layer Security (RFC 2249) connections for the SIP
  // stack.
  TIdSipTLSTransport = class(TIdSipTCPTransport)
  private
    function  GetOnGetPassword: TPasswordEvent;
    function  GetRootCertificate: TFileName;
    function  GetServerCertificate: TFileName;
    function  GetServerKey: TFileName;
    function  TLS: TIdSipTlsServer;
    procedure SetOnGetPassword(Value: TPasswordEvent);
    procedure SetRootCertificate(Value: TFileName);
    procedure SetServerCertificate(Value: TFileName);
    procedure SetServerKey(Value: TFileName);
  protected
    function  ServerType: TIdSipTcpServerClass; override;
  public
    class function DefaultPort: Cardinal; override;
    class function GetTransportType: String; override;
    class function IsSecure: Boolean; override;
    class function SrvPrefix: String; override;

    function  ClientType: TIdSipTcpClientClass; override;

    property OnGetPassword:     TPasswordEvent read GetOnGetPassword write SetOnGetPassword;
    property RootCertificate:   TFileName      read GetRootCertificate write SetRootCertificate;
    property ServerCertificate: TFileName      read GetServerCertificate write SetServerCertificate;
    property ServerKey:         TFileName      read GetServerKey write SetServerKey;
  end;

  TIdSipTlsServer = class(TIdSipTcpServer)
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  CreateClient: TIdSipTcpClient; override;
    procedure DestroyClient(Client: TIdSipTcpClient); override;

    property OnGetPassword:     TPasswordEvent read fOnGetPassword write fOnGetPassword;
    property RootCertificate:   TFileName read GetRootCertificate write SetRootCertificate;
    property ServerCertificate: TFileName read GetServerCertificate write SetServerCertificate;
    property ServerKey:         TFileName read GetServerKey write SetServerKey;
  end;

  TIdSipTlsClientThread = class(TIdSipTcpClientThread)
  protected
    function ClientType: TIdSipTcpClientClass; override;
  end;

  TIdSipTlsClient = class(TIdSipTcpClient)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

implementation

uses
  IdSipDns, IdSipMessage;

//******************************************************************************
//* TIdSipTLSTransport                                                         *
//******************************************************************************
//* TIdSipTLSTransport Public methods ******************************************

class function TIdSipTLSTransport.DefaultPort: Cardinal;
begin
  Result := DefaultSipsPort;
end;

class function TIdSipTLSTransport.GetTransportType: String;
begin
  Result := TlsTransport;
end;

class function TIdSipTLSTransport.IsSecure: Boolean;
begin
  Result := true;
end;

class function TIdSipTLSTransport.SrvPrefix: String;
begin
  Result := SrvTlsPrefix;
end;

function TIdSipTLSTransport.ClientType: TIdSipTcpClientClass; 
begin
  Result := TIdSipTlsClient;
end;

//* TIdSipTLSTransport Protected methods ***************************************

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

procedure TIdSipTLSTransport.SetOnGetPassword(Value: TPasswordEvent);
begin
  Self.TLS.OnGetPassword := Value;
end;

procedure TIdSipTLSTransport.SetRootCertificate(Value: TFileName);
begin
  Self.TLS.RootCertificate := Value;
end;

procedure TIdSipTLSTransport.SetServerCertificate(Value: TFileName);
begin
  Self.TLS.ServerCertificate := Value;
end;

procedure TIdSipTLSTransport.SetServerKey(Value: TFileName);
begin
  Self.TLS.ServerKey := Value;
end;

//******************************************************************************
//* TIdSipTlsServer                                                            *
//******************************************************************************
//* TIdSipTlsServer Public methods *********************************************

constructor TIdSipTlsServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.TLS := TIdServerIOHandlerSSL.Create(nil);
  Self.TLS.OnGetPassword := Self.DoOnGetPassword;

  Self.IOHandler := Self.TLS;
end;

destructor TIdSipTlsServer.Destroy;
begin
  Self.IOHandler := nil;
  Self.TLS.Free;

  inherited Destroy;
end;

function TIdSipTlsServer.CreateClient: TIdSipTcpClient;
begin
  Result := inherited CreateClient;

  Result.IOHandler := TIdSSLIOHandlerSocket.Create(nil);
end;

procedure TIdSipTlsServer.DestroyClient(Client: TIdSipTcpClient);
begin
  Client.IOHandler.Free;

  inherited DestroyClient(Client);
end;

//* TIdSipTlsServer Private methods ********************************************

procedure TIdSipTlsServer.DoOnGetPassword(var Password: String);
begin
  if Assigned(Self.OnGetPassword) then
    Self.OnGetPassword(Password);
end;

function TIdSipTlsServer.GetRootCertificate: TFileName;
begin
  Result := Self.TLS.SSLOptions.RootCertFile;
end;

function TIdSipTlsServer.GetServerCertificate: TFileName;
begin
  Result := Self.TLS.SSLOptions.CertFile;
end;

function TIdSipTlsServer.GetServerKey: TFileName;
begin
  Result := Self.TLS.SSLOptions.KeyFile;
end;

procedure TIdSipTlsServer.SetRootCertificate(const Value: TFileName);
begin
  Self.TLS.SSLOptions.RootCertFile := Value;
end;

procedure TIdSipTlsServer.SetServerCertificate(const Value: TFileName);
begin
  Self.TLS.SSLOptions.CertFile := Value;
end;

procedure TIdSipTlsServer.SetServerKey(const Value: TFileName);
begin
  Self.TLS.SSLOptions.KeyFile := Value;
end;

//******************************************************************************
//* TIdSipTlsClientThread                                                      *
//******************************************************************************
//* TIdSipTlsClientThread Protected methods ************************************

function TIdSipTlsClientThread.ClientType: TIdSipTcpClientClass;
begin
  Result := TIdSipTlsClient;
end;

//******************************************************************************
//* TIdSipTlsClient                                                            *
//******************************************************************************
//* TIdSipTlsClient Public methods *********************************************

constructor TIdSipTlsClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.IOHandler := TIdSSLIOHandlerSocket.Create(nil);
end;

destructor TIdSipTlsClient.Destroy;
begin
  Self.IOHandler.Free;

  inherited Destroy;
end;

end.
