unit TestIdSipTlsServer;

interface

uses
  IdSipTcpClient, IdSipTcpServer, IdSipTlsServer, TestIdSipTcpServer;

type
  TestTIdSipTlsServer = class(TestTIdSipTcpServer)
  private
    procedure DoOnGetPassword(var Password: String);
  protected
    function ServerType: TIdSipTcpServerClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  IdSSLOpenSSL, TestFramework, TestFrameworkSip;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTlsServer unit tests');
  Result.AddTest(TestTIdSipTlsServer.Suite);
end;

//******************************************************************************
//* TestTIdSipTlsServer                                                        *
//******************************************************************************
//* TestTIdSipTlsServer Public methods *****************************************

procedure TestTIdSipTlsServer.SetUp;
var
  TLS: TIdSipTlsServer;
begin
  inherited SetUp;

  TLS := Self.LowPortServer as TIdSipTlsServer;

  TLS.OnGetPassword     := Self.DoOnGetPassword;
  TLS.RootCertificate   := RootCert;
  TLS.ServerCertificate := ServerCert;
  TLS.ServerKey         := ServerKey;

  TLS := Self.HighPortServer as TIdSipTlsServer;

  TLS.OnGetPassword     := Self.DoOnGetPassword;
  TLS.RootCertificate   := RootCert;
  TLS.ServerCertificate := ServerCert;
  TLS.ServerKey         := ServerKey;

  Self.Client.IOHandler := TIdSSLIOHandlerSocket.Create(nil);
end;

procedure TestTIdSipTlsServer.TearDown;
begin
  Self.Client.IOHandler.Free;

  inherited TearDown;
end;

//* TestTIdSipTlsServer Protected methods **************************************

function TestTIdSipTlsServer.ServerType: TIdSipTcpServerClass;
begin
  Result := TIdSipTlsServer;
end;

//* TestTIdSipTlsServer Private methods ****************************************

procedure TestTIdSipTlsServer.DoOnGetPassword(var Password: String);
begin
  Password := CertPasswd;
end;

initialization
  RegisterTest('IdSipTlsServer', Suite);
end.
