unit TestIdSipTransport;

interface

uses
  IdSipMessage, IdSipTransport, IdTcpServer, SyncObjs, SysUtils, TestFramework,
  TestFrameworkEx;

type
  TestTIdSipTransport = class(TThreadingTestCase)
  protected
    Transport: TIdSipAbstractTransport;

    function TransportType: TIdSipTransportClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipTcpTransport = class(TestTIdSipTransport)
  private
    ReceivedMessage: Boolean;
    Request:         TIdSipRequest;
    Response:        TIdSipResponse;

    procedure ReturnResponse(AThread: TIdPeerThread);
    procedure CheckCanReceiveRequest(Sender: TObject; const R: TIdSipRequest);
    procedure CheckCanReceiveResponse(Sender: TObject; const R: TIdSipResponse);
  protected
    function TransportType: TIdSipTransportClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanReceiveRequest;
    procedure TestCanReceiveResponse;
    procedure TestSendRequest;
  end;

  TestTIdSipUdpTransport = class(TestTIdSipTransport)
  private
    ReceivedMessage: Boolean;
    Request:         TIdSipRequest;
    Response:        TIdSipResponse;

    procedure CheckCanReceiveRequest(Sender: TObject; const R: TIdSipRequest);
    procedure CheckCanReceiveResponse(Sender: TObject; const R: TIdSipResponse);
  protected
    function TransportType: TIdSipTransportClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanReceiveRequest;
    procedure TestCanReceiveResponse;
    procedure TestSendRequest;
  end;

implementation

uses
  IdSipConsts, IdTcpClient, IdUdpClient, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransport unit tests');
  Result.AddTest(TestTIdSipTransport.Suite);
  Result.AddTest(TestTIdSipTcpTransport.Suite);
  Result.AddTest(TestTIdSipUdpTransport.Suite);
end;

//******************************************************************************
//* TestTIdSipTransport                                                        *
//******************************************************************************
//* TestTIdSipTransport Public methods *****************************************

procedure TestTIdSipTransport.SetUp;
begin
  inherited SetUp;

  Self.Transport := Self.TransportType.Create;
end;

procedure TestTIdSipTransport.TearDown;
begin
  Self.Transport.Free;

  inherited TearDown;
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

  inherited TearDown;
end;

//* TestTIdSipTcpTransport Protected methods ***********************************

function TestTIdSipTcpTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipTcpTransport;
end;

//* TestTIdSipTcpTransport Private methods *************************************

procedure TestTIdSipTcpTransport.ReturnResponse(AThread: TIdPeerThread);
var
  OK: TIdSipResponse;
  P:  TIdSipParser;
begin
  P := TIdSipParser.Create;
  try
    OK := P.ParseAndMakeResponse(BasicResponse);
    try
      AThread.Connection.Write(OK.AsString);
    finally
      OK.Free;
    end;
  finally
    P.Free;
  end;
end;

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
  Self.ExceptionMessage    := 'Response not received - event didn''t fire';
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
  Self.ExceptionMessage     := 'Response not received - event didn''t fire';
  Self.Transport.OnResponse := Self.CheckCanReceiveResponse;
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

    Check(Self.ReceivedMessage, 'Response not received');
  finally
    Self.Transport.Stop;
  end;
end;

procedure TestTIdSipTcpTransport.TestSendRequest;
var
  T: TIdSipTcpTransport;
begin
  Self.ExceptionMessage    := 'Response not received - event didn''t fire';
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
//* TestTIdSipUdpTransport                                                     *
//******************************************************************************
//* TestTIdSipUdpTransport Public methods **************************************

procedure TestTIdSipUdpTransport.SetUp;
var
  P: TIdSipParser;
begin
  inherited SetUp;

  Self.ReceivedMessage := false;

  P := TIdSipParser.Create;
  try
    Self.Request  := P.ParseAndMakeMessage(BasicRequest) as TIdSipRequest;
    Self.Response := P.ParseAndMakeMessage(BasicResponse) as TIdSipResponse;
  finally
    P.Free;
  end;
end;

procedure TestTIdSipUdpTransport.TearDown;
begin
  Self.Response.Free;
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipUdpTransport Protected methods ***********************************

function TestTIdSipUdpTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipUdpTransport;
end;

//* TestTIdSipUdpTransport Private methods *************************************

procedure TestTIdSipUdpTransport.CheckCanReceiveRequest(Sender: TObject; const R: TIdSipRequest);
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

procedure TestTIdSipUdpTransport.CheckCanReceiveResponse(Sender: TObject; const R: TIdSipResponse);
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


//* TestTIdSipUdpTransport Published methods ***********************************

procedure TestTIdSipUdpTransport.TestCanReceiveRequest;
var
  Client: TIdUdpClient;
begin
  Self.ExceptionMessage    := 'Response not received - event didn''t fire';
  Self.Transport.OnRequest := Self.CheckCanReceiveRequest;
  Self.Transport.Start;
  try
    Client := TIdUdpClient.Create(nil);
    try
      Client.Host := '127.0.0.1';
      Client.Port := IdPORT_SIP;
      Client.Send(BasicRequest);
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

procedure TestTIdSipUdpTransport.TestCanReceiveResponse;
var
  Client: TIdUdpClient;
begin
  Self.ExceptionMessage     := 'Response not received - event didn''t fire';
  Self.Transport.OnResponse := Self.CheckCanReceiveResponse;
  Self.Transport.Start;
  try
    Client := TIdUdpClient.Create(nil);
    try
      Client.Host := '127.0.0.1';
      Client.Port := IdPORT_SIP;
      Client.Send(BasicRequest);
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

procedure TestTIdSipUdpTransport.TestSendRequest;
var
  T: TIdSipUdpTransport;
begin
  Self.ExceptionMessage    := 'Response not received - event didn''t fire';
  Self.Transport.OnRequest := Self.CheckCanReceiveRequest;

  Self.Transport.Start;
  try
    T := TIdSipUdpTransport.Create(IdPORT_SIP + 10000);
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

initialization
  RegisterTest('IdSipTransport', Suite);
end.
