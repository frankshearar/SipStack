{
  (c) 2006 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipTlsTransport;

interface

uses
  IdSipTransport, TestFrameworkSipTransport;

type
  TestTIdSipTLSTransport = class(TestTIdSipTransport)
  private
    procedure DoOnPassword(var Password: String);
    procedure SetUpTls(Transport: TIdSipTransport);
  protected
    procedure SendMessage(Msg: String); override;
    function  TransportType: TIdSipTransportClass; override;
  public
    procedure SetUp; override;

    function  DefaultPort: Cardinal; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
  end;

implementation

uses
  IdSipMessage, IdTcpClient, IdSipTlsTransport, TestFramework, TestFrameworkSip;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTlsTransport unit tests');
//  Result.AddTest(TestTIdSipTLSTransport.Suite);
end;

//******************************************************************************
//* TestTIdSipTLSTransport                                                     *
//******************************************************************************
//* TestTIdSipTLSTransport Public methods **************************************

procedure TestTIdSipTLSTransport.SetUp;
begin
  inherited SetUp;

  Self.SetUpTls(Self.HighPortTransport);
  Self.SetUpTls(Self.LowPortTransport);

  Self.Request.RequestUri.Scheme := SipsScheme;
  Self.Response.LastHop.Transport := Self.HighPortTransport.GetTransportType;
{
  Self.Response.LastHop.Value := StringReplace(Self.Response.LastHop.AsString,
                                               'TCP',
                                               'TLS',
                                               []);
}
end;

function TestTIdSipTLSTransport.DefaultPort: Cardinal;
begin
  Result := DefaultSipsPort;
end;

//* TestTIdSipTLSTransport Protected methods ***********************************

procedure TestTIdSipTLSTransport.SendMessage(Msg: String);
var
  Client: TIdTcpClient;
begin
  // TODO: This won't work! You need to set up the certs & such!
  Client := TIdTcpClient.Create(nil);
  try
    Client.Host := Self.HighPortLocation.IPAddress;
    Client.Port := Self.HighPortLocation.Port;
    Client.Connect(DefaultTimeout);
    try
      Client.Write(Msg);
    finally
      Client.DisconnectSocket;
    end;
  finally
    Client.Free;
  end;
end;

function TestTIdSipTLSTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipTlsTransport;
end;

//* TestTIdSipTLSTransport Private methods *************************************

procedure TestTIdSipTLSTransport.DoOnPassword(var Password: String);
begin
  Password := CertPasswd;
end;

procedure TestTIdSipTLSTransport.SetUpTls(Transport: TIdSipTransport);
var
  TLS: TIdSipTLSTransport;
begin
  CheckEquals(TIdSipTLSTransport.ClassName,
              Transport.ClassName,
              'TestTIdSipTLSTransport.SetUpTls');

  TLS := Transport as TIdSipTLSTransport;

  TLS.OnGetPassword     := Self.DoOnPassword;
  TLS.RootCertificate   := RootCert;
  TLS.ServerCertificate := ServerCert;
  TLS.ServerKey         := ServerKey;
end;

//* TestTIdSipTLSTransport Published methods ***********************************

procedure TestTIdSipTLSTransport.TestGetTransportType;
begin
  CheckEquals(TlsTransport,
              Self.HighPortTransport.GetTransportType,
              'Transport type');
end;

procedure TestTIdSipTLSTransport.TestIsReliable;
begin
  Check(Self.HighPortTransport.IsReliable,
        'TLS transport not marked as reliable');
end;

procedure TestTIdSipTLSTransport.TestIsSecure;
begin
  Check(Self.HighPortTransport.IsSecure,
        'TLS transport not marked as secure');
end;

initialization
  RegisterTest('TLS transport', Suite);
end.
