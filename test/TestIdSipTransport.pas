unit TestIdSipTransport;

interface

uses
  IdSipMessage, IdSipTransport, IdTcpServer, SyncObjs, SysUtils, TestFramework,
  TestFrameworkEx;

type
  TestTIdSipTransport = class(TThreadingTestCase)
  protected
    HostName:         String;
    ReceivedRequest:  Boolean;
    ReceivedResponse: Boolean;
    Request:          TIdSipRequest;
    Response:         TIdSipResponse;
    Transport:        TIdSipAbstractTransport;

    procedure CheckSendRequestTopVia(Sender: TObject; const R: TIdSipRequest);
    function  TransportType: TIdSipTransportClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipTcpTransport = class(TestTIdSipTransport)
  private
    procedure CheckCanReceiveRequest(Sender: TObject; const R: TIdSipRequest);
    procedure CheckCanReceiveResponse(Sender: TObject; const R: TIdSipResponse);
    procedure ReturnResponse(Sender: TObject; const R: TIdSipRequest); overload;
    procedure ReturnResponse(AThread: TIdPeerThread); overload;
    procedure SendOkResponse;
  protected
    function TransportType: TIdSipTransportClass; override;
  published
    procedure TestCanReceiveRequest;
    procedure TestCanReceiveResponse;
    procedure TestGetTransportType;
    procedure TestSendRequest;
    procedure TestSendRequestTopVia;
  end;

  TestTIdSipUdpTransport = class(TestTIdSipTransport)
  private
    procedure CheckCanReceiveRequest(Sender: TObject; const R: TIdSipRequest);
    procedure CheckCanReceiveResponse(Sender: TObject; const R: TIdSipResponse);
    procedure ReturnResponse(Sender: TObject; const R: TIdSipRequest);
  protected
    function TransportType: TIdSipTransportClass; override;
  published
    procedure TestCanReceiveRequest;
    procedure TestCanReceiveResponse;
    procedure TestGetTransportType;
    procedure TestSendRequest;
    procedure TestSendRequestTopVia;
  end;

implementation

uses
  IdSipHeaders, IdSocketHandle, IdSipConsts, IdTcpClient, IdUdpClient,
  TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransport unit tests');
  Result.AddTest(TestTIdSipTcpTransport.Suite);
  Result.AddTest(TestTIdSipUdpTransport.Suite);
end;

//******************************************************************************
//* TestTIdSipTransport                                                        *
//******************************************************************************
//* TestTIdSipTransport Public methods *****************************************

procedure TestTIdSipTransport.SetUp;
var
  P: TIdSipParser;
begin
  inherited SetUp;

  Self.ExceptionMessage    := 'Response not received - event didn''t fire';

  Self.Transport := Self.TransportType.Create;

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

procedure TestTIdSipTransport.TearDown;
begin
  Self.Response.Free;
  Self.Request.Free;

  Self.Transport.Free;

  inherited TearDown;
end;

//* TestTIdSipTransport Protected methods **************************************

procedure TestTIdSipTransport.CheckSendRequestTopVia(Sender: TObject; const R: TIdSipRequest);
begin
  try
    Check(Self.Transport.GetTransportType = R.LastHop.Transport,
          'Incorrect transport specified');

    Check(R.LastHop.HasBranch, 'Branch parameter missing');
    
    CheckEquals(Self.HostName, R.LastHop.SentBy, 'SentBy incorrect');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

function TestTIdSipTransport.TransportType: TIdSipTransportClass;
begin
  Result := nil;
end;

//******************************************************************************
//* TestTIdSipTcpTransport                                                     *
//******************************************************************************
//* TestTIdSipTcpTransport Protected methods ***********************************

function TestTIdSipTcpTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipTcpTransport;
end;

//* TestTIdSipTcpTransport Private methods *************************************

procedure TestTIdSipTcpTransport.CheckCanReceiveRequest(Sender: TObject; const R: TIdSipRequest);
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

procedure TestTIdSipTcpTransport.CheckCanReceiveResponse(Sender: TObject; const R: TIdSipResponse);
begin
  try
    Self.ReceivedResponse := true;
    Self.SendOkResponse;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpTransport.ReturnResponse(Sender: TObject; const R: TIdSipRequest);
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

procedure TestTIdSipTcpTransport.ReturnResponse(AThread: TIdPeerThread);
var
  OK: TIdSipResponse;
  P:  TIdSipParser;
begin
  P := TIdSipParser.Create;
  try
    OK := P.ParseAndMakeResponse(LocalLoopResponse);
    try
      AThread.Connection.Write(OK.AsString);
    finally
      OK.Free;
    end;
  finally
    P.Free;
  end;
end;

procedure TestTIdSipTcpTransport.SendOkResponse;
var
  Client: TIdTcpClient;
  OK:     TIdSipResponse;
  P:      TIdSipParser;
begin
  P := TIdSipParser.Create;
  try
    OK := P.ParseAndMakeResponse(LocalLoopResponse);
    try
      Client := TIdTcpClient.Create(nil);
      try
        Client.Host := '127.0.0.1';
        Client.Port := IdPORT_SIP;

        Client.Connect(DefaultTimeout);
        try
          Client.Write(Self.Response.AsString);
        finally
          Client.Disconnect;
        end;
      finally
        Client.Free;
      end;
    finally
      OK.Free;
    end;
  finally
    P.Free;
  end;
end;

//* TestTIdSipTcpTransport Published methods ***********************************

procedure TestTIdSipTcpTransport.TestCanReceiveRequest;
var
  Client: TIdTcpClient;
begin
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

    Check(Self.ReceivedRequest, 'Request not received');
  finally
    Self.Transport.Stop;
  end;
end;

procedure TestTIdSipTcpTransport.TestCanReceiveResponse;
var
  T: TIdTcpServer;
begin
  Self.Transport.OnResponse := Self.CheckCanReceiveResponse;

  T := TIdTcpServer.Create(nil);
  try
    T.DefaultPort := IdPORT_SIP;
    T.OnExecute := Self.ReturnResponse;

    T.Active := true;
    try
      Self.Transport.SendRequest(Self.Request);

      if (wrSignaled <> Self.ThreadEvent.WaitFor(1000)) then
        raise Self.ExceptionType.Create(Self.ExceptionMessage);

      Check(Self.ReceivedResponse, 'Response not received');
    finally
      T.Active := false;
    end;
  finally
    T.Free;
  end;
end;

procedure TestTIdSipTcpTransport.TestGetTransportType;
begin
  Check(Self.Transport.GetTransportType = sttTCP, 'Transport type');
end;

procedure TestTIdSipTcpTransport.TestSendRequest;
var
  T: TIdSipTcpTransport;
begin
  Self.Transport.OnRequest := Self.CheckCanReceiveRequest;

  Self.Transport.Start;
  try
    T := TIdSipTcpTransport.Create(IdPORT_SIP + 10000);
    try
      T.SendRequest(Self.Request, 500);
    finally
      T.Free;
    end;

    if (wrSignaled <> Self.ThreadEvent.WaitFor(1000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedRequest, 'Request not received');
  finally
    Self.Transport.Stop;
  end;
end;

procedure TestTIdSipTcpTransport.TestSendRequestTopVia;
var
  T: TIdSipTcpTransport;
begin
  Self.Transport.OnRequest := Self.CheckSendRequestTopVia;

  Self.Transport.Start;
  try
    T := TIdSipTcpTransport.Create(IdPORT_SIP + 10000);
    try
      Self.HostName := 'obviously.fake.org';
      T.HostName := Self.HostName;
      T.SendRequest(Self.Request, 500);
    finally
      T.Free;
    end;

    if (wrSignaled <> Self.ThreadEvent.WaitFor(1000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);
  finally
    Self.Transport.Stop;
  end;
end;

//******************************************************************************
//* TestTIdSipUdpTransport                                                     *
//******************************************************************************
//* TestTIdSipUdpTransport Protected methods ***********************************

function TestTIdSipUdpTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipUdpTransport;
end;

//* TestTIdSipUdpTransport Private methods *************************************

procedure TestTIdSipUdpTransport.CheckCanReceiveRequest(Sender: TObject; const R: TIdSipRequest);
begin
  try
    Self.ReceivedRequest := true;

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
    Self.ReceivedResponse := true;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUdpTransport.ReturnResponse(Sender: TObject; const R: TIdSipRequest);
var
  Client: TIdUdpClient;
begin
  Self.Response.StatusCode := SIPOK;

  Client := TIdUdpClient.Create(nil);
  try
    Client.Host := Self.Response.ToHeader.Address.Host;
    Client.Port := StrToIntDef(Self.Response.ToHeader.Address.Port, IdPORT_SIP);
    Client.Send(Self.Response.AsString);
  finally
    Client.Free;
  end;
end;

//* TestTIdSipUdpTransport Published methods ***********************************

procedure TestTIdSipUdpTransport.TestCanReceiveRequest;
var
  Client: TIdUdpClient;
begin
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

    Check(Self.ReceivedRequest, 'Request not received');
  finally
    Self.Transport.Stop;
  end;
end;

procedure TestTIdSipUdpTransport.TestCanReceiveResponse;
var
  Client: TIdUdpClient;
begin
  Self.Transport.OnRequest  := Self.ReturnResponse;
  Self.Transport.OnResponse := Self.CheckCanReceiveResponse;

  Self.Transport.Start;
  try
    Client := TIdUdpClient.Create(nil);
    try
      Client.Host := '127.0.0.1';
      Client.Port := IdPORT_SIP;
      Client.Send(Self.Request.AsString);
    finally
      Client.Free;
    end;

    if (wrSignaled <> Self.ThreadEvent.WaitFor(1000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedResponse, 'Response not received');
  finally
    Self.Transport.Stop;
  end;
end;

procedure TestTIdSipUdpTransport.TestGetTransportType;
begin
  Check(Self.Transport.GetTransportType = sttUDP, 'Transport type');
end;

procedure TestTIdSipUdpTransport.TestSendRequest;
var
  T: TIdSipUdpTransport;
begin
  Self.Transport.OnRequest := Self.CheckCanReceiveRequest;

  Self.Transport.Start;
  try
    T := TIdSipUdpTransport.Create(IdPORT_SIP + 10000);
    try
      T.OnRequest := Self.ReturnResponse;

      T.Start;
      try
        T.SendRequest(Self.Request, 500);
      finally
        T.Stop;
      end;
    finally
      T.Free;
    end;

    if (wrSignaled <> Self.ThreadEvent.WaitFor(1000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);

    Check(Self.ReceivedRequest, 'Request not received');
  finally
    Self.Transport.Stop;
  end;
end;

procedure TestTIdSipUdpTransport.TestSendRequestTopVia;
var
  T: TIdSipUdpTransport;
begin
  Self.Transport.OnRequest := Self.CheckSendRequestTopVia;

  Self.Transport.Start;
  try
    T := TIdSipUdpTransport.Create(IdPORT_SIP + 10000);
    try
      Self.HostName := 'obviously.fake.org';
      T.HostName := Self.HostName;
      T.SendRequest(Self.Request, 500);
    finally
      T.Free;
    end;

    if (wrSignaled <> Self.ThreadEvent.WaitFor(1000)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);
  finally
    Self.Transport.Stop;
  end;
end;

initialization
  RegisterTest('IdSipTransport', Suite);
end.
