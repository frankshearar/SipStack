unit TestIdSipTransport;

interface

uses
  IdSipMessage, IdSipTransport, IdTcpServer, SyncObjs, SysUtils, TestFramework,
  TestFrameworkEx;

type
  TestTIdSipAbstractTransport = class(TThreadingTestCase)
  protected
    ReceivedRequest:  Boolean;
    ReceivedResponse: Boolean;
    Request:          TIdSipRequest;
    Response:         TIdSipResponse;
    Transport:        TIdSipAbstractTransport;

    procedure CheckCanReceiveRequest(Sender: TObject; const R: TIdSipRequest);
    procedure CheckCanReceiveResponse(Sender: TObject; const R: TIdSipResponse);
    procedure CheckDiscardResponseWithUnknownSentBy(Sender: TObject; const R: TIdSipResponse);
    procedure CheckSendRequestTopVia(Sender: TObject; const R: TIdSipRequest);
    function  DefaultPort: Cardinal; virtual;
    procedure ReturnResponse(Sender: TObject; const R: TIdSipRequest);
    procedure SendOkResponse;
    function  TransportType: TIdSipTransportClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanReceiveRequest;
    procedure TestCanReceiveResponse;
    procedure TestTransportFor;
    procedure TestDiscardResponseWithUnknownSentBy;
    procedure TestSendRequest;
    procedure TestSendRequestTopVia;
    procedure TestSendResponse;
  end;

  TestTIdSipTCPTransport = class(TestTIdSipAbstractTransport)
  protected
    function  TransportType: TIdSipTransportClass; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
  end;

  TestTIdSipTLSTransport = class(TestTIdSipAbstractTransport)
  private
    procedure DoOnPassword(var Password: String);
  protected
    function  DefaultPort: Cardinal; override;
    function  TransportType: TIdSipTransportClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestCanReceiveRequestOne;
    procedure TestGetTransportType;
    procedure TestIsReliable;
  end;

  TestTIdSipUDPTransport = class(TestTIdSipAbstractTransport)
  protected
    function  TransportType: TIdSipTransportClass; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
  end;
{
  TestTIdSipSCTPTransport = class(TestTIdSipAbstractTransport)
  protected
    function  TransportType: TIdSipTransportClass; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
  end;
}

const
  RootCert             = '..\etc\cacert.pem';
  ServerCert           = '..\etc\newcert.pem';
  ServerKey            = '..\etc\newkey.pem';
  TestServerPortOffset = 10000;

implementation

uses
  IdSipHeaders, IdSocketHandle, IdSipConsts, IdSSLOpenSSL, IdTcpClient,
  IdUdpClient, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransport unit tests');
  Result.AddTest(TestTIdSipTCPTransport.Suite);
  Result.AddTest(TestTIdSipTLSTransport.Suite);
  Result.AddTest(TestTIdSipUDPTransport.Suite);
//  Result.AddTest(TestTIdSipSCTPTransport.Suite);
end;

//******************************************************************************
//* TestTIdSipAbstractTransport                                                *
//******************************************************************************
//* TestTIdSipAbstractTransport Public methods *********************************

procedure TestTIdSipAbstractTransport.SetUp;
var
  P: TIdSipParser;
begin
  inherited SetUp;

  Self.ExceptionMessage    := 'Response not received - event didn''t fire';

  Self.Transport := Self.TransportType.Create(Self.DefaultPort);
  Self.Transport.HostName := 'wsfrank';

  P := TIdSipParser.Create;
  try
    Self.Request  := P.ParseAndMakeRequest(LocalLoopRequest);

    Self.Response := P.ParseAndMakeResponse(LocalLoopResponse);
  finally
    P.Free;
  end;

  Self.ReceivedRequest  := false;
  Self.ReceivedResponse := false;
end;

procedure TestTIdSipAbstractTransport.TearDown;
begin
  Self.Response.Free;
  Self.Request.Free;

  Self.Transport.Free;

  inherited TearDown;
end;

//* TestTIdSipAbstractTransport Protected methods ******************************

procedure TestTIdSipAbstractTransport.CheckCanReceiveRequest(Sender: TObject; const R: TIdSipRequest);
begin
  try
    Self.ReceivedRequest := true;
    Self.SendOkResponse;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipAbstractTransport.CheckCanReceiveResponse(Sender: TObject; const R: TIdSipResponse);
begin
  try
    Self.ReceivedResponse := true;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipAbstractTransport.CheckDiscardResponseWithUnknownSentBy(Sender: TObject; const R: TIdSipResponse);
begin
  Self.ReceivedResponse := true;
end;

procedure TestTIdSipAbstractTransport.CheckSendRequestTopVia(Sender: TObject; const R: TIdSipRequest);
begin
  try
    Check(Self.Transport.GetTransportType = R.LastHop.Transport,
          'Incorrect transport specified');

    Check(R.LastHop.HasBranch, 'Branch parameter missing');

    CheckEquals(Self.Transport.HostName, R.LastHop.SentBy, 'SentBy incorrect');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

function TestTIdSipAbstractTransport.DefaultPort: Cardinal;
begin
  Result := IdPORT_SIP;
end;

procedure TestTIdSipAbstractTransport.ReturnResponse(Sender: TObject; const R: TIdSipRequest);
begin
  try
    Self.SendOkResponse;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipAbstractTransport.SendOkResponse;
begin
  Self.Response.StatusCode := SIPOK;

  Self.Transport.SendResponse(Self.Response);
end;

function TestTIdSipAbstractTransport.TransportType: TIdSipTransportClass;
begin
  Result := nil;
end;

//* TestTIdSipAbstractTransport Published methods ******************************

procedure TestTIdSipAbstractTransport.TestCanReceiveRequest;
begin
  Self.Transport.OnRequest := Self.CheckCanReceiveRequest;
  Self.Transport.Start;
  try
    Self.Transport.SendRequest(Self.Request);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(1000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedRequest, 'Request not received');
  finally
    Self.Transport.Stop;
  end;
end;

procedure TestTIdSipAbstractTransport.TestCanReceiveResponse;
begin
  Self.Transport.OnRequest := Self.ReturnResponse;
  Self.Transport.OnResponse := Self.CheckCanReceiveResponse;

  Self.Transport.Start;
  try
    Self.Transport.SendRequest(Self.Request);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(1000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedResponse, 'Response not received');
  finally
    Self.Transport.Stop;
  end;
end;

procedure TestTIdSipAbstractTransport.TestTransportFor;
begin
  CheckEquals(TIdSipTCPTransport,  TIdSipAbstractTransport.TransportFor(sttTCP),  'TCP');
  CheckEquals(TIdSipTLSTransport,  TIdSipAbstractTransport.TransportFor(sttTLS),  'TLS');
  CheckEquals(TIdSipUDPTransport,  TIdSipAbstractTransport.TransportFor(sttUDP),  'UDP');
  CheckEquals(TIdSipSCTPTransport, TIdSipAbstractTransport.TransportFor(sttSCTP), 'SCTP');
end;

procedure TestTIdSipAbstractTransport.TestDiscardResponseWithUnknownSentBy;
begin
  Self.Transport.OnResponse := Self.CheckDiscardResponseWithUnknownSentBy;

  Self.Transport.Start;
  try
    Self.Response.LastHop.SentBy := 'localhost';
    Self.Transport.SendResponse(Self.Response);

    Check(wrTimeout = Self.ThreadEvent.WaitFor(1000),
          'Response not silently discarded');
  finally
    Self.Transport.Stop;
  end;
end;

procedure TestTIdSipAbstractTransport.TestSendRequest;
begin
  Self.Transport.OnRequest := Self.CheckCanReceiveRequest;

  Self.Transport.Start;
  try
    Self.Transport.SendRequest(Self.Request, 500);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(1000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedRequest, 'Request not received');
  finally
    Self.Transport.Stop;
  end;
end;

procedure TestTIdSipAbstractTransport.TestSendRequestTopVia;
begin
  Self.Transport.OnRequest := Self.CheckSendRequestTopVia;

  Self.Transport.Start;
  try
    Self.Transport.SendRequest(Self.Request, 500);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(1000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);
  finally
    Self.Transport.Stop;
  end;
end;

procedure TestTIdSipAbstractTransport.TestSendResponse;
begin
  Self.Transport.OnResponse := Self.CheckCanReceiveResponse;

  Self.Transport.Start;
  try
    Self.Transport.SendResponse(Self.Response);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(5000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedResponse, 'Response not received');
  finally
    Self.Transport.Stop;
  end;
end;

//******************************************************************************
//* TestTIdSipTCPTransport                                                     *
//******************************************************************************
//* TestTIdSipTCPTransport Protected methods ***********************************

function TestTIdSipTCPTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipTcpTransport;
end;

//* TestTIdSipTCPTransport Published methods ***********************************

procedure TestTIdSipTCPTransport.TestGetTransportType;
begin
  Check(Self.Transport.GetTransportType = sttTCP, 'Transport type');
end;

procedure TestTIdSipTCPTransport.TestIsReliable;
begin
  Check(Self.Transport.IsReliable, 'TCP transport not marked as reliable');
end;

//******************************************************************************
//* TestTIdSipTLSTransport                                                     *
//******************************************************************************
//* TestTIdSipTLSTransport Public methods **************************************

procedure TestTIdSipTLSTransport.SetUp;
var
  TLS: TIdSipTLSTransport;
begin
  inherited SetUp;

  TLS := Self.Transport as TIdSipTLSTransport;

  TLS.OnGetPassword     := Self.DoOnPassword;
  TLS.RootCertificate   := RootCert;
  TLS.ServerCertificate := ServerCert;
  TLS.ServerKey         := ServerKey;

  Self.Request.RequestUri.Protocol := SipsScheme;
  Self.Response.LastHop.Value := StringReplace(Self.Response.LastHop.AsString, 'TCP', 'TLS', []);
end;

//* TestTIdSipTLSTransport Protected methods ***********************************

function TestTIdSipTLSTransport.DefaultPort: Cardinal;
begin
  Result := IdPORT_SIPS;
end;

function TestTIdSipTLSTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipTlsTransport;
end;

//* TestTIdSipTLSTransport Private methods *************************************

procedure TestTIdSipTLSTransport.DoOnPassword(var Password: String);
begin
  Password := 'test';
end;

//* TestTIdSipTLSTransport Published methods ***********************************

procedure TestTIdSipTLSTransport.TestCanReceiveRequestOne;
var
  Client: TIdTcpClient;
  TLS:    TIdSSLIOHandlerSocket;
begin
  Self.Transport.OnRequest := Self.CheckCanReceiveRequest;
  Self.Transport.Start;
  try
    TLS := TIdSSLIOHandlerSocket.Create(nil);
    try
      Client := TIdTcpClient.Create(nil);
      try
        Client.Host      := '127.0.0.1';
        Client.IOHandler := TLS;
        Client.Port      := Self.DefaultPort;
        Client.Connect(1000);
        Client.Write(BasicRequest);
      finally
        Client.Free;
      end;
    finally
      TLS.Free;
    end;

    if (wrSignaled <> Self.ThreadEvent.WaitFor(1000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedRequest, 'Request not received');
  finally
    Self.Transport.Stop;
  end;
end;

procedure TestTIdSipTLSTransport.TestGetTransportType;
begin
  Check(sttTLS = Self.Transport.GetTransportType, 'Transport type');
end;

procedure TestTIdSipTLSTransport.TestIsReliable;
begin
  Check(Self.Transport.IsReliable, 'TCP transport not marked as reliable');
end;

//******************************************************************************
//* TestTIdSipUDPTransport                                                     *
//******************************************************************************
//* TestTIdSipUDPTransport Protected methods ***********************************

function TestTIdSipUDPTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipUdpTransport;
end;

//* TestTIdSipUDPTransport Published methods ***********************************

procedure TestTIdSipUDPTransport.TestGetTransportType;
begin
  Check(Self.Transport.GetTransportType = sttUDP, 'Transport type');
end;

procedure TestTIdSipUDPTransport.TestIsReliable;
begin
  Check(not Self.Transport.IsReliable, 'UDP transport not marked as unreliable');
end;

{
//******************************************************************************
//* TestTIdSipSCTPTransport                                                    *
//******************************************************************************
//* TestTIdSipSCTPTransport Protected methods **********************************

function TestTIdSipSCTPTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipSCTPTransport;
end;

//* TestTIdSipSCTPTransport Published methods **********************************

procedure TestTIdSipSCTPTransport.TestGetTransportType;
begin
  Check(sttSCTP = Self.Transport.GetTransportType, 'Transport type');
end;

procedure TestTIdSipSCTPTransport.TestIsReliable;
begin
  Check(Self.Transport.IsReliable, 'SCTP transport not marked as reliable');
end;
}
initialization
  RegisterTest('IdSipTransport', Suite);
end.
