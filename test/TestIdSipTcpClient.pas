{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipTcpClient;

interface

uses
  IdSipMessage, IdSipTcpClient, IdSipTcpServer, IdTimerQueue, IdTCPServer,
  SyncObjs, SysUtils, TestFrameworkSip;

type
  TIdSipRequestEvent = procedure(Sender: TObject;
                                 R: TIdSipRequest) of object;
  TIdSipResponseEvent = procedure(Sender: TObject;
                                  R: TIdSipResponse) of object;

  TestTIdSipTcpClient = class(TTestCaseSip, IIdSipMessageListener)
  private
    CheckingRequestEvent:  TIdSipRequestEvent;
    CheckingResponseEvent: TIdSipResponseEvent;
    Client:                TIdSipTcpClient;
    ClientEvent:           TEvent;
    EmptyListEvent:        TEvent;
    Finished:              Boolean;
    Invite:                TIdSipRequest;
    InviteCount:           Cardinal;
    ReceivedRequestMethod: String;
    ReceivedResponseCount: Cardinal;
    Server:                TIdSipTcpServer;
    Timer:                 TIdThreadedTimerQueue;

    procedure CheckReceiveOkResponse(Sender: TObject;
                                     Response: TIdSipResponse;
                                     ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckReceiveOptions(Sender: TObject;
                                  Request: TIdSipRequest;
                                  ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckReceiveProvisionalAndOkResponse(Sender: TObject;
                                                   Response: TIdSipResponse;
                                                   ReceivedFrom: TIdSipConnectionBindings);
    procedure CheckSendInvite(Sender: TObject;
                              Request: TIdSipRequest);
    procedure CheckSendTwoInvites(Sender: TObject;
                                  Request: TIdSipRequest);
    procedure ClientReceivedRequest(Sender: TObject;
                                    R: TIdSipRequest;
                                    ReceivedFrom: TIdSipConnectionBindings);
    procedure CutConnection(Sender: TObject;
                            R: TIdSipRequest);
    procedure OnEmpty(Sender: TIdTimerQueue);
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnMalformedMessage(const Msg: String;
                                 const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdSipConnectionBindings);
    procedure PauseAndSendOkResponse(Sender: TObject;
                                     Request: TIdSipRequest);
    procedure SendOkResponse(Sender: TObject;
                             Request: TIdSipRequest);
    procedure SendOptionsRequest(Sender: TObject;
                                 Request: TIdSipRequest);
    procedure SendProvisionalAndOkResponse(Sender: TObject;
                                           Request: TIdSipRequest);
    procedure SendResponseReceiveOptions(Sender: TObject;
                                         Response: TIdSipResponse);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanReceiveRequest;
    procedure TestConnectAndDisconnect;
    procedure TestTerminated;
    procedure TestTerminatedWithServerDisconnect;
    procedure TestReceiveOkResponse;
    procedure TestReceiveOkResponseWithPause;
    procedure TestReceiveProvisionalAndOkResponse;
    procedure TestSendInvite;
    procedure TestSendResponseReceiveOptions;
    procedure TestSendTwoInvites;
    procedure TestSendWithServerDisconnect;
  end;

const
  DefaultTimeout = 2000;

implementation

uses
  Classes, IdGlobal, IdSipConsts, IdStack, IdTCPConnection, TestFramework,
  TestMessages;

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
begin
  inherited SetUp;

  Self.EmptyListEvent := TSimpleEvent.Create;
  Self.ClientEvent := TSimpleEvent.Create;
  Self.Timer := TIdThreadedTimerQueue.Create(false);
  Self.Timer.OnEmpty := Self.OnEmpty;

  Self.Client := TIdSipTcpClient.Create(nil);
  Self.Server := TIdSipTcpServer.Create(nil);
  Self.Server.AddMessageListener(Self);
  Self.Server.Timer := Self.Timer;

  Self.Client.Host        := '127.0.0.1';
  Self.Client.Port        := Self.Server.DefaultPort;
  Self.Client.ReadTimeout := 1000;

  Self.Invite := TIdSipTestResources.CreateLocalLoopRequest;

  Self.Finished              := false;
  Self.InviteCount           := 0;
  Self.ReceivedRequestMethod := '';
  Self.ReceivedResponseCount := 0;
  Self.Server.Active         := true;
end;

procedure TestTIdSipTcpClient.TearDown;
var
  WaitTime: Cardinal;
begin
  // Wait for all scheduled events to execute
  WaitTime := Self.Timer.DefaultTimeout * 3 div 2;
  Self.Timer.Terminate;
  Self.EmptyListEvent.WaitFor(WaitTime);

  Self.Server.Active := false;
  Self.Invite.Free;
  Self.Server.Free;
  Self.Client.Free;

  Self.ClientEvent.Free;
  Self.EmptyListEvent.Free;

  inherited TearDown;
end;

//* TestTIdSipTcpClient Private methods ****************************************

procedure TestTIdSipTcpClient.CheckReceiveOkResponse(Sender: TObject;
                                                     Response: TIdSipResponse;
                                                     ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    CheckEquals(SIPOK, Response.StatusCode, 'Unexpected response');
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.CheckReceiveOptions(Sender: TObject;
                                                  Request: TIdSipRequest;
                                                  ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    CheckEquals(MethodOptions, Request.Method, 'Unexpected request');
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.CheckReceiveProvisionalAndOkResponse(Sender: TObject;
                                                                   Response: TIdSipResponse;
                                                                   ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    Inc(Self.ReceivedResponseCount);

    case Self.ReceivedResponseCount of
      1: CheckEquals(SIPTrying, Response.StatusCode, '1st response');
      2: CheckEquals(SIPOK,     Response.StatusCode, '2nd response');
    else
      Self.ExceptionMessage := 'Too many responses received';
    end;

    if (Self.ReceivedResponseCount > 1) then
      Self.ClientEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.CheckSendInvite(Sender: TObject;
                                              Request: TIdSipRequest);
begin
  try
    CheckEquals(MethodInvite, Request.Method, 'Incorrect method');

    Self.SendOkResponse(Sender, Request);

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.CheckSendTwoInvites(Sender: TObject;
                                                  Request: TIdSipRequest);
begin
  try
    Inc(Self.InviteCount);

    Self.SendOkResponse(Sender, Request);

    if (Self.InviteCount > 1) then
      Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.ClientReceivedRequest(Sender: TObject;
                                                    R: TIdSipRequest;
                                                    ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.ReceivedRequestMethod := R.Method;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTcpClient.CutConnection(Sender: TObject;
                                            R: TIdSipRequest);
var
  Threads: TList;
begin
  try
    Threads := Self.Server.Threads.LockList;
    try
      if (Threads.Count = 0) then
        raise Exception.Create('TCP connection disappeared: CutConnection');

      (TObject(Threads[0]) as TIdPeerThread).Connection.DisconnectSocket;
    finally
      Self.Server.Threads.UnlockList;
    end;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTcpClient.OnEmpty(Sender: TIdTimerQueue);
begin
  Self.EmptyListEvent.SetEvent;
end;

procedure TestTIdSipTcpClient.OnException(E: Exception;
                                          const Reason: String);
begin
  Self.ExceptionType    := ExceptClass(E.ClassType);
  Self.ExceptionMessage := E.Message + ' caused by ''' + Reason + '''';
end;

procedure TestTIdSipTcpClient.OnMalformedMessage(const Msg: String;
                                                 const Reason: String);
begin
end;

procedure TestTIdSipTcpClient.OnReceiveRequest(Request: TIdSipRequest;
                                               ReceivedFrom: TIdSipConnectionBindings);
begin
  if Assigned(Self.CheckingRequestEvent) then
    Self.CheckingRequestEvent(Self, Request);

  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTcpClient.OnReceiveResponse(Response: TIdSipResponse;
                                                ReceivedFrom: TIdSipConnectionBindings);
begin
  if Assigned(Self.CheckingResponseEvent) then
    Self.CheckingResponseEvent(Self, Response);

  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTcpClient.PauseAndSendOkResponse(Sender: TObject;
                                                     Request: TIdSipRequest);
begin
  IdGlobal.Sleep(200);
  Self.SendOkResponse(Sender, Request);
end;

procedure TestTIdSipTcpClient.SendOkResponse(Sender: TObject;
                                             Request: TIdSipRequest);
var
  S:       String;
  Threads: TList;
begin
  S := StringReplace(LocalLoopResponse, '486 Busy Here', '200 OK', []);

  Threads := Self.Server.Threads.LockList;
  try
    if (Threads.Count = 0) then
      raise Exception.Create('TCP connection disappeared: SendOkResponse');

    (TObject(Threads[0]) as TIdPeerThread).Connection.Write(S);
  finally
    Self.Server.Threads.UnlockList;
  end;
end;

procedure TestTIdSipTcpClient.SendOptionsRequest(Sender: TObject;
                                                 Request: TIdSipRequest);
var
  Connection: TIdTCPConnection;
  S:          String;
  Threads:    TList;
begin
  S := StringReplace(LocalLoopRequest, MethodInvite, MethodOptions, []);

  Threads := Self.Server.Threads.LockList;
  try
    if (Threads.Count = 0) then
      raise Exception.Create('TCP connection disappeared: SendOptionsRequest');

    Connection := (TObject(Threads[0]) as TIdPeerThread).Connection;
    if not Connection.Connected then
      raise Exception.Create('TCP connection closed');

    Connection.Write(S);
  finally
    Self.Server.Threads.UnlockList;
  end;
end;

procedure TestTIdSipTcpClient.SendProvisionalAndOkResponse(Sender: TObject;
                                                           Request: TIdSipRequest);
var
  OK:      String;
  Threads: TList;
  Trying:  String;
begin
  Trying := StringReplace(LocalLoopResponse, '486 Busy Here', '100 Trying', []);
  OK     := StringReplace(LocalLoopResponse, '486 Busy Here', '200 OK', []);

  Threads := Self.Server.Threads.LockList;
  try
    if (Threads.Count = 0) then
      raise Exception.Create('TCP connection disappeared: SendProvisionalAndOkResponse');

    (TObject(Threads[0]) as TIdPeerThread).Connection.Write(Trying);
    IdGlobal.Sleep(500);
    (TObject(Threads[0]) as TIdPeerThread).Connection.Write(OK);
  finally
    Self.Server.Threads.UnlockList;
  end;
end;

procedure TestTIdSipTcpClient.SendResponseReceiveOptions(Sender: TObject;
                                                         Response: TIdSipResponse);
var
  Options: TIdSipRequest;
  Threads: TList;
begin
  Options := Self.Invite.Copy as TIdSipRequest;
  try
    Options.Method := MethodOptions;
    Options.CSeq.Method := Options.Method;

    Threads := Self.Server.Threads.LockList;
    try
      if (Threads.Count = 0) then
        raise Exception.Create('TCP connection disappeared: SendProvisionalAndOkResponse');

      (TObject(Threads[0]) as TIdPeerThread).Connection.Write(Options.AsString);
    finally
      Self.Server.Threads.UnlockList;
    end;
  finally
    Options.Free;
  end;
end;

//* TestTIdSipTcpClient Published methods **************************************

procedure TestTIdSipTcpClient.TestCanReceiveRequest;
begin
  Self.CheckingRequestEvent := Self.SendOptionsRequest;
  Self.Client.OnRequest     := Self.ClientReceivedRequest;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Send(Self.Invite);
  Self.Client.ReceiveMessages;

  Self.WaitForSignaled;
  CheckEquals(MethodOptions,
             Self.ReceivedRequestMethod,
             'Unexpected received request');
end;

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

procedure TestTIdSipTcpClient.TestTerminated;
begin
  Self.CheckingRequestEvent := Self.SendOkResponse;

  Check(not Self.Client.Terminated, 'Before connect');

  Self.Client.Connect(DefaultTimeout);
  Check(not Self.Client.Terminated, 'Connection established');

  Self.Client.Send(Self.Invite);
  Self.Client.ReceiveMessages;

  Self.WaitForSignaled;
  Check(Self.Client.Terminated, 'After final response received');
end;

procedure TestTIdSipTcpClient.TestTerminatedWithServerDisconnect;
begin
  Self.CheckingRequestEvent := Self.CutConnection;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Send(Self.Invite);
  Self.Client.ReceiveMessages;

  Self.WaitForSignaled;
  Check(Self.Client.Terminated, 'After connection unexpectedly cut');
end;

procedure TestTIdSipTcpClient.TestReceiveOkResponse;
begin
  Self.CheckingRequestEvent := Self.SendOkResponse;
  Self.Client.OnResponse    := Self.CheckReceiveOkResponse;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Send(Self.Invite);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTcpClient.TestReceiveOkResponseWithPause;
begin
  Self.CheckingRequestEvent := Self.PauseAndSendOkResponse;
  Self.Client.OnResponse    := Self.CheckReceiveOkResponse;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Send(Self.Invite);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTcpClient.TestReceiveProvisionalAndOkResponse;
begin
  Self.CheckingRequestEvent := Self.SendProvisionalAndOkResponse;
  Self.Client.OnResponse    := Self.CheckReceiveProvisionalAndOkResponse;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Send(Self.Invite);
  Self.Client.ReceiveMessages;

  Self.WaitForSignaled(Self.ClientEvent);

  CheckEquals(2, Self.ReceivedResponseCount, 'Received response count');
end;

procedure TestTIdSipTcpClient.TestSendInvite;
begin
  Self.CheckingRequestEvent := Self.CheckSendInvite;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Send(Self.Invite);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTcpClient.TestSendResponseReceiveOptions;
var
  OK: TIdSipResponse;
begin
  Self.CheckingResponseEvent := Self.SendResponseReceiveOptions;
  Self.Client.OnRequest      := Self.CheckReceiveOptions;

  OK := TIdSipResponse.InResponseTo(Self.Invite, SIPOK);
  try
    Self.Client.Connect(DefaultTimeout);
    Self.Client.Send(OK);
  finally
    OK.Free;
  end;
end;

procedure TestTIdSipTcpClient.TestSendTwoInvites;
begin
  Self.CheckingRequestEvent := Self.CheckSendTwoInvites;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Send(Self.Invite);

  Self.Client.Send(Self.Invite);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTcpClient.TestSendWithServerDisconnect;
begin
  Self.CheckingRequestEvent := Self.CutConnection;

  Self.Client.Connect(DefaultTimeout);
  Self.Client.Send(Self.Invite);

  Self.WaitForSignaled;
end;

initialization
  RegisterTest('IdSipTcpClient', Suite);
end.
