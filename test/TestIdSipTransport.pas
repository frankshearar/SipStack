unit TestIdSipTransport;

interface

uses
  Classes, IdSipMessage, IdSipTransport, IdSocketHandle, IdTcpServer, SyncObjs,
  SysUtils, TestFramework, TestFrameworkEx;

type
  TestTIdSipAbstractTransport = class(TThreadingTestCase)
  protected
    LocalLoopTransport: TIdSipAbstractTransport;
    ReceivedRequest:    Boolean;
    ReceivedResponse:   Boolean;
    Request:            TIdSipRequest;
    Response:           TIdSipResponse;
    Transport:          TIdSipAbstractTransport;
    WrongServer:        Boolean;

    procedure CheckCanReceiveRequest(Sender: TObject; const R: TIdSipRequest);
    procedure CheckCanReceiveResponse(Sender: TObject; const R: TIdSipResponse);
    procedure CheckDiscardResponseWithUnknownSentBy(Sender: TObject; const R: TIdSipResponse);
    procedure CheckSendRequestTopVia(Sender: TObject; const R: TIdSipRequest);
    procedure CheckSendResponseWithReceivedParam(Sender: TObject; const R: TIdSipResponse);
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
    procedure TestSendResponseWithReceivedParam;
  end;

  TestTIdSipTCPTransport = class(TestTIdSipAbstractTransport)
  protected
    function  TransportType: TIdSipTransportClass; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
  end;

  TestTIdSipTLSTransport = class(TestTIdSipAbstractTransport)
  private
    procedure DoOnPassword(var Password: String);
    procedure SetUpTls(Transport: TIdSipAbstractTransport);
  protected
    function  DefaultPort: Cardinal; override;
    function  TransportType: TIdSipTransportClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
  end;

  TestTIdSipUDPTransport = class(TestTIdSipAbstractTransport)
  protected
    function TransportType: TIdSipTransportClass; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
  end;
{
  TestTIdSipSCTPTransport = class(TestTIdSipAbstractTransport)
  protected
    function  TransportType: TIdSipTransportClass; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
  end;
}

const
  DefaultTimeout       = 1000;
  RootCert             = '..\etc\cacert.pem';
  ServerCert           = '..\etc\newcert.pem';
  ServerKey            = '..\etc\newkey.pem';
  TestServerPortOffset = 10000;

implementation

uses
  IdSipHeaders, IdSipConsts, IdSSLOpenSSL, IdStack, IdTcpClient, IdUDPServer,
  TestMessages;

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
  Binding: TIdSocketHandle;
  P:       TIdSipParser;
begin
  inherited SetUp;

  Self.ExceptionMessage    := 'Response not received - event didn''t fire';

  Self.Transport := Self.TransportType.Create(Self.DefaultPort);
  Self.Transport.Timeout := 500;
  Self.Transport.HostName := 'wsfrank';
  Self.Transport.Bindings.Clear;
  Binding := Self.Transport.Bindings.Add;
  Binding.IP := GStack.LocalAddress;
  Binding.Port := Self.DefaultPort;

  Self.LocalLoopTransport := Self.TransportType.Create(Self.DefaultPort);
  Self.LocalLoopTransport.Timeout := 500;
  Self.LocalLoopTransport.HostName := 'localhost';
  Self.LocalLoopTransport.Bindings.Clear;
  Binding := Self.LocalLoopTransport.Bindings.Add;
  Binding.IP := '127.0.0.1';
  Binding.Port := Self.DefaultPort;

  P := TIdSipParser.Create;
  try
    Self.Request  := P.ParseAndMakeRequest(LocalLoopRequest);

    Self.Response := P.ParseAndMakeResponse(LocalLoopResponse);
  finally
    P.Free;
  end;
  Self.Response.LastHop.Transport := Self.Transport.GetTransportType;


  Self.ReceivedRequest  := false;
  Self.ReceivedResponse := false;
  Self.WrongServer      := false;
end;

procedure TestTIdSipAbstractTransport.TearDown;
begin
  Self.Response.Free;
  Self.Request.Free;

  Self.LocalLoopTransport.Free;
  Self.Transport.Free;

  inherited TearDown;
end;

//* TestTIdSipAbstractTransport Protected methods ******************************

procedure TestTIdSipAbstractTransport.CheckCanReceiveRequest(Sender: TObject;
                                                       const R:      TIdSipRequest);
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

procedure TestTIdSipAbstractTransport.CheckCanReceiveResponse(Sender: TObject;
                                                        const R:      TIdSipResponse);
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

procedure TestTIdSipAbstractTransport.CheckDiscardResponseWithUnknownSentBy(Sender: TObject;
                                                                      const R:      TIdSipResponse);
begin
  Self.ReceivedResponse := true;
end;

procedure TestTIdSipAbstractTransport.CheckSendRequestTopVia(Sender: TObject;
                                                       const R:      TIdSipRequest);
begin
  try
    Check(Self.Transport.GetTransportType = R.LastHop.Transport,
          'Incorrect transport specified');

    Check(R.LastHop.HasBranch, 'Branch parameter missing');

    CheckEquals(Self.LocalLoopTransport.HostName, R.LastHop.SentBy, 'SentBy incorrect');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipAbstractTransport.CheckSendResponseWithReceivedParam(Sender: TObject;
                                                                   const R:      TIdSipResponse);
begin
  try
    Self.WrongServer := Sender <> Self.LocalLoopTransport;

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

procedure TestTIdSipAbstractTransport.ReturnResponse(Sender: TObject;
                                               const R:      TIdSipRequest);
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

  Self.Transport.Send(Self.Response);
end;

function TestTIdSipAbstractTransport.TransportType: TIdSipTransportClass;
begin
  Result := nil;
end;

//* TestTIdSipAbstractTransport Published methods ******************************

procedure TestTIdSipAbstractTransport.TestCanReceiveRequest;
begin
  Self.LocalLoopTransport.OnRequest := Self.CheckCanReceiveRequest;
  Self.LocalLoopTransport.Start;
  try
    Self.LocalLoopTransport.Send(Self.Request);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedRequest, 'Request not received');
  finally
    Self.LocalLoopTransport.Stop;
  end;
end;

procedure TestTIdSipAbstractTransport.TestCanReceiveResponse;
begin
  Self.LocalLoopTransport.OnRequest := Self.ReturnResponse;
  Self.LocalLoopTransport.OnResponse := Self.CheckCanReceiveResponse;

  Self.LocalLoopTransport.Start;
  try
    Self.LocalLoopTransport.Send(Self.Request);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedResponse, 'Response not received');
  finally
    Self.LocalLoopTransport.Stop;
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
  Self.LocalLoopTransport.OnResponse := Self.CheckDiscardResponseWithUnknownSentBy;

  Self.LocalLoopTransport.Start;
  try
    // If we don't set the received param we won't be able to send
    // the message to the right SIP server. No, you'd never do this
    // in production, because it's wilfully wrong.
    Self.Response.LastHop.SentBy := 'unknown.host';
    Self.Response.LastHop.Received := '127.0.0.1';
    Self.LocalLoopTransport.Send(Self.Response);

    Check(wrTimeout = Self.ThreadEvent.WaitFor(DefaultTimeout),
          'Response not silently discarded');
  finally
    Self.LocalLoopTransport.Stop;
  end;
end;

procedure TestTIdSipAbstractTransport.TestSendRequest;
begin
  Self.LocalLoopTransport.OnRequest := Self.CheckCanReceiveRequest;

  Self.LocalLoopTransport.Start;
  try
    Self.LocalLoopTransport.Send(Self.Request);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedRequest, 'Request not received');
  finally
    Self.LocalLoopTransport.Stop;
  end;
end;

procedure TestTIdSipAbstractTransport.TestSendRequestTopVia;
begin
  Self.LocalLoopTransport.OnRequest := Self.CheckSendRequestTopVia;

  Self.LocalLoopTransport.Start;
  try
    Self.LocalLoopTransport.Send(Self.Request);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);
  finally
    Self.LocalLoopTransport.Stop;
  end;
end;

procedure TestTIdSipAbstractTransport.TestSendResponse;
begin
  Self.LocalLoopTransport.OnResponse := Self.CheckCanReceiveResponse;

  Self.LocalLoopTransport.Start;
  try
    Self.LocalLoopTransport.Send(Self.Response);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(5000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedResponse, 'Response not received');
  finally
    Self.LocalLoopTransport.Stop;
  end;
end;

procedure TestTIdSipAbstractTransport.TestSendResponseWithReceivedParam;
begin
  Self.Transport.OnResponse          := Self.CheckSendResponseWithReceivedParam;
  Self.LocalLoopTransport.OnResponse := Self.CheckSendResponseWithReceivedParam;

  Self.Transport.Start;
  try
    Self.LocalLoopTransport.Start;
    try
      Self.Response.LastHop.Received := Self.LocalLoopTransport.Bindings[0].IP;
      Self.Transport.Send(Self.Response);

      if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
        raise Self.ExceptionType.Create('No response received');

      Check(not Self.WrongServer,
            'Received param in top Via header ignored - '
          + 'wrong server got the message');
    finally
      Self.LocalLoopTransport.Stop;
    end;
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

procedure TestTIdSipTCPTransport.TestIsSecure;
begin
  Check(not Self.Transport.IsSecure, 'TCP transport marked as secure');
end;

//******************************************************************************
//* TestTIdSipTLSTransport                                                     *
//******************************************************************************
//* TestTIdSipTLSTransport Public methods **************************************

procedure TestTIdSipTLSTransport.SetUp;
begin
  inherited SetUp;

  Self.SetUpTls(Self.Transport);
  Self.SetUpTls(Self.LocalLoopTransport);

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

procedure TestTIdSipTLSTransport.SetUpTls(Transport: TIdSipAbstractTransport);
var
  TLS: TIdSipTLSTransport;
begin
  CheckEquals(TIdSipTLSTransport,
              Transport.ClassType,
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
  Check(sttTLS = Self.Transport.GetTransportType, 'Transport type');
end;

procedure TestTIdSipTLSTransport.TestIsReliable;
begin
  Check(Self.Transport.IsReliable, 'TCP transport not marked as reliable');
end;

procedure TestTIdSipTLSTransport.TestIsSecure;
begin
  Check(Self.Transport.IsSecure, 'TLS transport not marked as secure');
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

procedure TestTIdSipUDPTransport.TestIsSecure;
begin
  Check(not Self.Transport.IsSecure, 'UDP transport marked as secure');
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

procedure TestTIdSipTCPTransport.TestIsSecure;
begin
  Check(not Self.Transport.IsSecure, 'SCTP transport marked as secure');
end;
}
initialization
  RegisterTest('IdSipTransport', Suite);
end.
