unit TestIdSipTcpClient;

interface

uses
  IdSipMessage, IdSipTcpClient, IdSipTcpServer, IdTCPServer, TestFrameworkEx;

type
  TestTIdSipTcpClient = class(TThreadingTestCase)
  private
    Client:                TIdSipTcpClient;
    Invite:                TIdSipRequest;
    InviteCount:           Cardinal;
    ReceivedResponseCount: Cardinal;
    Server:                TIdSipTcpServer;

    procedure CheckReceiveOkResponse(Sender: TObject; const Response: TIdSipResponse);
    procedure CheckReceiveProvisionalAndOkResponse(Sender: TObject; const Response: TIdSipResponse);
    procedure CheckSendInvite(AThread: TIdPeerThread; const Request: TIdSipRequest);
    procedure CheckSendTwoInvites(AThread: TIdPeerThread; const Request: TIdSipRequest);
    procedure CutConnection(AThread: TIdPeerThread; const R: TIdSipRequest);
    procedure SendOkResponse(AThread: TIdPeerThread; const Request: TIdSipRequest);
    procedure SendProvisionalAndOkResponse(AThread: TIdPeerThread; const Request: TIdSipRequest);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnectAndDisconnect;
    procedure TestReceiveOkResponse;
    procedure TestReceiveProvisionalAndOkResponse;
    procedure TestSendInvite;
    procedure TestSendTwoInvites;
    procedure TestSendWithServerDisconnect;
  end;

const
  DefaultTimeout = 2000;

implementation

uses
  Classes, IdSipConsts, SyncObjs, SysUtils, TestFramework, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTcpClient unit tests');
  Result.AddTest(TestTIdSipTcpClient.Suite);
end;

//******************************************************************************
//* TestTIdSipTcpClient                                                        *
//******************************************************************************
//* TestTIdSipTcpClient Public methods *****************************************

procedure TestTIdSipTcpClient.SetUp;
var
  P: TIdSipParser;
  S: TStringStream;
begin
  inherited SetUp;

  Self.Client := TIdSipTcpClient.Create(nil);
  Self.Server := TIdSipTcpServer.Create(nil);

  Self.Client.Host := '127.0.0.1';
  Self.Client.Port := Self.Server.DefaultPort;

  S := TStringStream.Create(BasicRequest);
  try
    P := TIdSipParser.Create;
    try
      P.Source := S;
      Self.Invite := P.ParseAndMakeMessage as TIdSipRequest;
      Self.Invite.Body := BasicBody;
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;

  Self.InviteCount           := 0;
  Self.ReceivedResponseCount := 0;
  Self.Server.Active         := true;
end;

procedure TestTIdSipTcpClient.TearDown;
begin
  Self.Server.Active := false;

  Self.Invite.Free;
  Self.Server.Free;
  Self.Client.Free;

  inherited TearDown;
end;

//* TestTIdSipTcpClient Private methods ****************************************

procedure TestTIdSipTcpClient.CheckReceiveOkResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  try
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.CheckReceiveProvisionalAndOkResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  try
    Inc(Self.ReceivedResponseCount);

    case Self.ReceivedResponseCount of
      1: CheckEquals(SIPTrying,   Response.StatusCode, '1st response');
      2: CheckEquals(SIPBusyHere, Response.StatusCode, '2nd response');
    else
      Self.ExceptionMessage := 'Too many responses received';
    end;

    if (Self.ReceivedResponseCount > 1) then
      Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.CheckSendInvite(AThread: TIdPeerThread; const Request: TIdSipRequest);
begin
  try
    CheckEquals(MethodInvite, Request.Method, 'Incorrect method');

    AThread.Connection.Write(BasicResponse);

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.CheckSendTwoInvites(AThread: TIdPeerThread; const Request: TIdSipRequest);
begin
  try
    Inc(Self.InviteCount);

    AThread.Connection.Write(BasicResponse);

    if (Self.InviteCount > 1) then
      Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.CutConnection(AThread: TIdPeerThread; const R: TIdSipRequest);
begin
  try
    AThread.Connection.DisconnectSocket;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.SendOkResponse(AThread: TIdPeerThread; const Request: TIdSipRequest);
begin
  AThread.Connection.Write(BasicResponse);
end;

procedure TestTIdSipTcpClient.SendProvisionalAndOkResponse(AThread: TIdPeerThread; const Request: TIdSipRequest);
begin
  AThread.Connection.Write(StringReplace(BasicResponse, '486 Busy Here', '100 Trying', []));
  Sleep(500);
  AThread.Connection.Write(BasicResponse);
end;

//* TestTIdSipTcpClient Published methods **************************************

procedure TestTIdSipTcpClient.TestConnectAndDisconnect;
begin
  Self.Client.Host := '127.0.0.1';
  Self.Client.Port := IdPORT_SIP;
  Self.Client.Connect(1000);
  try
    Check(Self.Client.Connected, 'Client didn''t connect');
  finally
    Self.Client.Disconnect;
  end;
end;

procedure TestTIdSipTcpClient.TestReceiveOkResponse;
begin
  Self.Server.OnRequest  := Self.SendOkResponse;
  Self.Client.OnResponse := Self.CheckReceiveOkResponse;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Send(Self.Invite);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpClient.TestReceiveProvisionalAndOkResponse;
begin
  Self.Server.OnRequest  := Self.SendProvisionalAndOkResponse;
  Self.Client.OnResponse := Self.CheckReceiveProvisionalAndOkResponse;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Send(Self.Invite);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);

  CheckEquals(2, Self.ReceivedResponseCount, 'Received response count');
end;

procedure TestTIdSipTcpClient.TestSendInvite;
begin
  Self.Server.OnRequest := Self.CheckSendInvite;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Send(Self.Invite);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpClient.TestSendTwoInvites;
begin
  Self.Server.OnRequest := Self.CheckSendTwoInvites;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Send(Self.Invite);

  Self.Client.Send(Self.Invite);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdSipTcpClient.TestSendWithServerDisconnect;
begin
  Self.Server.OnRequest := Self.CutConnection;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Send(Self.Invite);

  if (Self.ThreadEvent.WaitFor(DefaultTimeout) <> wrSignaled) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;


initialization
  RegisterTest('IdSipTcpClient', Suite);
end.
