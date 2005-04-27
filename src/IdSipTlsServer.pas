{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipTlsServer;

interface

uses
  Classes, IdSSLOpenSSL, IdSipTcpClient, IdSipTcpServer, SysUtils;

type
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

implementation

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

end.
