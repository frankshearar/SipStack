unit TestIdSipTransport;

interface

uses
  IdSipMessage, IdSipTransport, SyncObjs, SysUtils, TestFramework, TestFrameworkEx;

type
  TestTIdSipTransport = class(TTestCase)
  protected
    Trans: TIdSipAbstractTransport;
    function TransportType: TIdSipTransportClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure TestWillUseReliableTransport; virtual;
  end;

  TestTIdSipTcpTransport = class(TThreadingTestCase)
  private
    ReceivedMessage: Boolean;
    Request:         TIdSipRequest;
    Response:        TIdSipResponse;
    Transport:       TIdSipTcpTransport;

    procedure CheckCanReceiveRequest(Sender: TObject; const R: TIdSipRequest);
    procedure CheckCanReceiveResponse(Sender: TObject; const R: TIdSipResponse);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanReceiveRequest;
    procedure TestCanReceiveResponse;
    procedure TestSendRequest;
  end;

  TestTIdSipMockTransport = class(TestTIdSipTransport)
  protected
    function TransportType: TIdSipTransportClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWillUseReliableTransport; override;
  end;

implementation

uses
  IdSipParser, IdTcpClient, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransport unit tests');
  Result.AddTest(TestTIdSipTransport.Suite);
  Result.AddTest(TestTIdSipTcpTransport.Suite);
  Result.AddTest(TestTIdSipMockTransport.Suite);
end;

//******************************************************************************
//* TestTIdSipTransport                                                        *
//******************************************************************************
//* TestTIdSipTransport Public methods *****************************************

procedure TestTIdSipTransport.SetUp;
begin
  inherited SetUp;

  Self.Trans := Self.TransportType.Create;
end;

procedure TestTIdSipTransport.TearDown;
begin
  Self.Trans.Free;

  inherited TearDown;
end;

procedure TestTIdSipTransport.TestWillUseReliableTransport;
var
  R: TIdSipRequest;
begin
  R := TIdSipRequest.Create;
  try
    R.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    R.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/UDP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    R.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/SCTP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    R.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/TLS gw1.leo-ix.org;branch=z9hG4bK776asdhds';

    Check(Self.Trans.WillUseReliableTranport(R), 'TCP');

    R.Path.LastHop.Value := 'SIP/2.0/TLS gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    Check(Self.Trans.WillUseReliableTranport(R), 'TLS');

    R.Path.LastHop.Value := 'SIP/2.0/UDP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    Check(not Self.Trans.WillUseReliableTranport(R), 'TCP');

    R.Path.LastHop.Value := 'SIP/2.0/SCTP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    Check(Self.Trans.WillUseReliableTranport(R), 'SCTP');

  finally
    R.Free;
  end;
end;

//* TestTIdSipTransport Protected methods **************************************

function TestTIdSipTransport.TransportType: TIdSipTransportClass;
begin
  Result := nil;
end;

//******************************************************************************
//* TestTIdSipTcpTransport                                                     *
//******************************************************************************
//* TestTIdSipTcpTransport Public methods **************************************

procedure TestTIdSipTcpTransport.SetUp;
var
  P: TIdSipParser;
begin
  inherited SetUp;

  Self.ReceivedMessage := false;
  Self.Transport       := TIdSipTcpTransport.Create;

  P := TIdSipParser.Create;
  try
    Self.Request  := P.ParseAndMakeMessage(BasicRequest) as TIdSipRequest;
    Self.Response := P.ParseAndMakeMessage(BasicResponse) as TIdSipResponse;
  finally
    P.Free;
  end;
end;

procedure TestTIdSipTcpTransport.TearDown;
begin
  Self.Response.Free;
  Self.Request.Free;
  Self.Transport.Free;

  inherited TearDown;
end;

//* TestTIdSipTcpTransport Private methods *************************************

procedure TestTIdSipTcpTransport.CheckCanReceiveRequest(Sender: TObject; const R: TIdSipRequest);
begin
  try
    Self.ReceivedMessage := true;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpTransport.CheckCanReceiveResponse(Sender: TObject; const R: TIdSipResponse);
begin
  try
    Self.ReceivedMessage := true;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;


//* TestTIdSipTcpTransport Published methods ***********************************

procedure TestTIdSipTcpTransport.TestCanReceiveRequest;
var
  Client: TIdTcpClient;
begin
  Self.ExceptionMessage := 'Response not received - event didn''t fire';
  Self.Transport.OnRequest := Self.CheckCanReceiveRequest;
  Self.Transport.Start;
  try
    Client := TIdTcpClient.Create(nil);
    try
      Client.Host := '127.0.0.1';
      Client.Port := IdPORT_SIP;
      Client.Connect(1000);
      Client.Write(BasicRequest);
    finally
      Client.Free;
    end;

    if (wrSignaled <> Self.ThreadEvent.WaitFor(1000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedMessage, 'Request not received');
  finally
    Self.Transport.Stop;
  end;
end;

procedure TestTIdSipTcpTransport.TestCanReceiveResponse;
var
  Client: TIdTcpClient;
begin
  Self.ExceptionMessage := 'Response not received - event didn''t fire';
  Self.Transport.OnResponse := Self.CheckCanReceiveResponse;
  Self.Transport.Start;
  try
    Client := TIdTcpClient.Create(nil);
    try
      Client.Host := '127.0.0.1';
      Client.Port := IdPORT_SIP;
      Client.Connect(1000);
      Client.Write(BasicResponse);
    finally
      Client.Free;
    end;

    if (wrSignaled <> Self.ThreadEvent.WaitFor(1000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedMessage, 'Response not received');
  finally
    Self.Transport.Stop;
  end;
end;

procedure TestTIdSipTcpTransport.TestSendRequest;
var
  T: TIdSipTcpTransport;
begin
  Self.ExceptionMessage := 'Response not received - event didn''t fire';
  Self.Transport.OnRequest := Self.CheckCanReceiveRequest;

  Self.Transport.Start;
  try
    T := TIdSipTcpTransport.Create(IdPORT_SIP + 10000);
    try
      T.Start;
      Self.Request.RequestUri := 'sip:127.0.0.1';
      T.SendRequest(Self.Request);
    finally
      T.Free;
    end;

    if (wrSignaled <> Self.ThreadEvent.WaitFor(1000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedMessage, 'Response not received');
  finally
    Self.Transport.Stop;
  end;
end;

//******************************************************************************
//* TestTIdSipMockTransport                                                    *
//******************************************************************************
//* TestTIdSipMockTransport Public methods *************************************

procedure TestTIdSipMockTransport.SetUp;
begin
  inherited SetUp;
end;

procedure TestTIdSipMockTransport.TearDown;
begin
  inherited TearDown;
end;

//* TestTIdSipMockTransport Protected methods **********************************

function TestTIdSipMockTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipMockTransport;
end;

//* TestTIdSipMockTransport Published methods **********************************

procedure TestTIdSipMockTransport.TestWillUseReliableTransport;
begin
  inherited TestWillUseReliableTransport;
end;

initialization
  RegisterTest('IdSipTransport', Suite);
end.
