{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipTransport;

interface

uses
  Classes, IdSipMessage, IdSipTcpClient, IdSipTcpServer, IdSipTransport,
  IdSocketHandle, IdTcpServer, SyncObjs, SysUtils, TestFramework,
  TestFrameworkEx;

type
  TIdSipTransportSubclass = class(TIdSipTcpTransport)
  public
    procedure NotifyTransportListeners(const Request: TIdSipRequest); overload;
    procedure NotifyTransportListeners(const Response: TIdSipResponse); overload;
    procedure NotifyTransportSendingListeners(const Request: TIdSipRequest); overload;
    procedure NotifyTransportSendingListeners(const Response: TIdSipResponse); overload;
  end;

  TestTIdSipTransportEventNotifications = class(TTestCase,
                                                IIdSipTransportListener,
                                                IIdSipTransportSendingListener)
  private
    ReceivedRequest:  Boolean;
    ReceivedResponse: Boolean;
    Request:          TIdSipRequest;
    Response:         TIdSipResponse;
    SentRequest:      Boolean;
    SentResponse:     Boolean;
    Transport:        TIdSipTransportSubclass;

    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddTransportListener;
    procedure TestAddTransportSendingListener;
    procedure TestAllListenersReceiveRequests;
    procedure TestAllListenersReceiveResponses;
    procedure TestAllListenersSendRequests;
    procedure TestAllListenersSendResponses;
    procedure TestRemoveTransportListener;
    procedure TestRemoveTransportSendingListener;
  end;

  TTestIdSipRequestEvent = procedure(Sender: TObject;
                                     R: TIdSipRequest) of object;
  TTestIdSipResponseEvent = procedure(Sender: TObject;
                                      R: TIdSipResponse) of object;

  TestTIdSipTransport = class(TThreadingTestCase,
                              IIdSipTransportListener)
  protected
    CheckingRequestEvent:  TTestIdSipRequestEvent;
    CheckingResponseEvent: TTestIdSipResponseEvent;
    HighPortTransport:     TIdSipTransport;
    LowPortTransport:      TIdSipTransport;
    ReceivedRequest:       Boolean;
    ReceivedResponse:      Boolean;
    RecvdRequest:          TIdSipRequest;
    RejectedMessage:       Boolean;
    Request:               TIdSipRequest;
    Response:              TIdSipResponse;
    WrongServer:           Boolean;

    procedure CheckCanReceiveRequest(Sender: TObject;
                                     R: TIdSipRequest);
    procedure CheckCanReceiveResponse(Sender: TObject;
                                      R: TIdSipResponse);
    procedure CheckDiscardResponseWithUnknownSentBy(Sender: TObject;
                                                    R: TIdSipResponse);
    procedure CheckReceivedParamDifferentIPv4SentBy(Sender: TObject;
                                                    Request: TIdSipRequest);
    procedure CheckReceivedParamFQDNSentBy(Sender: TObject;
                                           Request: TIdSipRequest);
    procedure CheckReceivedParamIPv4SentBy(Sender: TObject;
                                           Request: TIdSipRequest);
    procedure CheckSendRequestFromNonStandardPort(Sender: TObject;
                                                  R: TIdSipRequest);
    procedure CheckSendRequestTopVia(Sender: TObject;
                                     R: TIdSipRequest);
    procedure CheckSendResponseFromNonStandardPort(Sender: TObject;
                                                   R: TIdSipResponse);
    procedure CheckUseRport(Sender: TObject;
                            R: TIdSipRequest);
    function  DefaultPort: Cardinal; virtual;
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
    procedure ReturnResponse(Sender: TObject;
                             R: TIdSipRequest);
    procedure SendMessage(Msg: String); virtual; abstract;
    procedure SendOkResponse(Transport: TIdSipTransport);
    function  TransportType: TIdSipTransportClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanReceiveRequest;
    procedure TestCanReceiveResponse;
    procedure TestCanReceiveUnsolicitedResponse;
    procedure TestIsNull; virtual;
    procedure TestTransportFor;
    procedure TestDiscardResponseWithUnknownSentBy;
    procedure TestDiscardMalformedMessage;
    procedure TestDiscardUnknownSipVersion;
    procedure TestReceivedParamDifferentIPv4SentBy;
    procedure TestReceivedParamFQDNSentBy;
    procedure TestReceivedParamIPv4SentBy;
    procedure TestSendRequest;
    procedure TestSendRequestFromNonStandardPort;
    procedure TestSendRequestTopVia;
    procedure TestSendResponse;
    procedure TestSendResponseFromNonStandardPort;
    procedure TestSendResponseWithReceivedParam;
    procedure TestUseRport;
  end;

  TestTIdSipTCPTransport = class(TestTIdSipTransport)
  protected
    procedure SendMessage(Msg: String); override;
    function  TransportType: TIdSipTransportClass; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
  end;

  TestTIdSipTLSTransport = class(TestTIdSipTransport)
  private
    procedure DoOnPassword(var Password: String);
    procedure SetUpTls(Transport: TIdSipTransport);
  protected
    function  DefaultPort: Cardinal; override;
    procedure SendMessage(Msg: String); override;
    function  TransportType: TIdSipTransportClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
  end;

  TestTIdSipUDPTransport = class(TestTIdSipTransport,
                                 IIdSipMessageListener)
  private
    MaddrTransport:  TIdSipTransport;
    RPort:           Cardinal;
    RPortEvent:      TEvent;

    procedure CheckMessageWithTrailingGarbage(Sender: TObject;
                                              R: TIdSipRequest);
    procedure CheckRportParamFilledIn(Sender: TObject;
                                      R: TIdSipRequest);
    procedure CheckLeaveNonRportRequestsUntouched(Sender: TObject;
                                                  R: TIdSipRequest);
    procedure CheckMaddrUser(Sender: TObject;
                             R: TIdSipResponse);
    procedure NoteSourcePort(Sender: TObject;
                             R: TIdSipRequest);
    procedure OnMalformedMessage(const Msg: String;
                                 const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdSipConnectionBindings);
  protected
    procedure SendMessage(Msg: String); override;
    function  TransportType: TIdSipTransportClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMessageWithTrailingGarbage;
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
    procedure TestLeaveNonRportRequestsUntouched;
    procedure TestMaddrUsed;
    procedure TestRespectRport;
    procedure TestRportParamFilledIn;
    procedure TestRportListening;
  end;
{
  TestTIdSipSCTPTransport = class(TestTIdSipTransport)
  protected
    function  TransportType: TIdSipTransportClass; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
  end;
}

  TestTIdSipNullTransport = class(TestTIdSipTransport)
  protected
    function  TransportType: TIdSipTransportClass; override;
  published
    procedure TestIsNull; override;
  end;

  TTransportMethodTestCase = class(TTestCase)
  protected
    Transport: TIdSipTransport;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipTransportExceptionMethod = class(TTransportMethodTestCase)
  private
    Exception: Exception;
    Method:    TIdSipTransportExceptionMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransportReceiveRequestMethod = class(TTransportMethodTestCase)
  private
    Method:  TIdSipTransportReceiveRequestMethod;
    Request: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransportReceiveResponseMethod = class(TTransportMethodTestCase)
  private
    Method:   TIdSipTransportReceiveResponseMethod;
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransportRejectedMessageMethod = class(TTransportMethodTestCase)
  private
    Method: TIdSipTransportRejectedMessageMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransportSendingRequestMethod = class(TTransportMethodTestCase)
  private
    Method:  TIdSipTransportSendingRequestMethod;
    Request: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransportSendingResponseMethod = class(TTransportMethodTestCase)
  private
    Method:  TIdSipTransportSendingResponseMethod;
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

implementation

uses
  IdGlobal, IdSipConsts, IdSipUdpServer, IdSSLOpenSSL, IdStack, IdTcpClient,
  IdUdpClient, IdUDPServer, TestMessages, TestFrameworkSip;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransport unit tests');
  Result.AddTest(TestTIdSipTransportEventNotifications.Suite);
  Result.AddTest(TestTIdSipTCPTransport.Suite);
//  Result.AddTest(TestTIdSipTLSTransport.Suite);
  Result.AddTest(TestTIdSipUDPTransport.Suite);
//  Result.AddTest(TestTIdSipSCTPTransport.Suite);
  Result.AddTest(TestTIdSipTransportExceptionMethod.Suite);
  Result.AddTest(TestTIdSipTransportReceiveRequestMethod.Suite);
  Result.AddTest(TestTIdSipTransportReceiveResponseMethod.Suite);
  Result.AddTest(TestTIdSipTransportRejectedMessageMethod.Suite);
  Result.AddTest(TestTIdSipTransportSendingRequestMethod.Suite);
  Result.AddTest(TestTIdSipTransportSendingResponseMethod.Suite);
end;

//******************************************************************************
//* TIdSipTransportSubclass                                                    *
//******************************************************************************
//* TIdSipTransportSubclass Public methods *************************************

procedure TIdSipTransportSubclass.NotifyTransportListeners(const Request: TIdSipRequest);
begin
  inherited NotifyTransportListeners(Request);
end;

procedure TIdSipTransportSubclass.NotifyTransportListeners(const Response: TIdSipResponse);
begin
  inherited NotifyTransportListeners(Response);
end;

procedure TIdSipTransportSubclass.NotifyTransportSendingListeners(const Request: TIdSipRequest);
begin
  inherited NotifyTransportSendingListeners(Request);
end;

procedure TIdSipTransportSubclass.NotifyTransportSendingListeners(const Response: TIdSipResponse);
begin
  inherited NotifyTransportSendingListeners(Response);
end;

//******************************************************************************
//* TestTIdSipTransportEventNotifications                                      *
//******************************************************************************
//* TestTIdSipTransportEventNotifications Public methods ***********************

procedure TestTIdSipTransportEventNotifications.SetUp;
begin
  inherited SetUp;

  Self.ReceivedRequest  := false;
  Self.ReceivedResponse := false;
  Self.Request          := TIdSipRequest.Create;
  Self.Response         := TIdSipResponse.Create;
  Self.Transport        := TIdSipTransportSubclass.Create(0);
end;

procedure TestTIdSipTransportEventNotifications.TearDown;
begin
  Self.Transport.Free;
  Self.Response.Free;
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportEventNotifications Private methods **********************

procedure TestTIdSipTransportEventNotifications.OnException(E: Exception;
                                                            const Reason: String);
begin
end;

procedure TestTIdSipTransportEventNotifications.OnReceiveRequest(Request: TIdSipRequest;
                                                                 Receiver: TIdSipTransport);
begin
  Self.ReceivedRequest := true;
  Check(Self.Request = Request,     'Request not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnReceiveResponse(Response: TIdSipResponse;
                                                                  Receiver: TIdSipTransport);
begin
  Self.ReceivedResponse := true;

  Check(Self.Response = Response,   'Response not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnRejectedMessage(const Msg: String;
                                                                  const Reason: String);
begin
end;

procedure TestTIdSipTransportEventNotifications.OnSendRequest(Request: TIdSipRequest;
                                                              Sender: TIdSipTransport);
begin
  Self.SentRequest := true;
  Check(Self.Request = Request,     'Request not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

procedure TestTIdSipTransportEventNotifications.OnSendResponse(Response: TIdSipResponse;
                                                               Sender: TIdSipTransport);
begin
  Self.SentResponse := true;

  Check(Self.Response = Response,   'Response not correct');
  Check(Self.Transport = Transport, 'Transport not correct');
end;

//* TestTIdSipTransportEventNotifications Published methods *******************************

procedure TestTIdSipTransportEventNotifications.TestAddTransportListener;
begin
  Self.Transport.AddTransportListener(Self);

  Self.Transport.NotifyTransportListeners(Self.Request);

  Check(Self.ReceivedRequest, 'Listener wasn''t added');
end;

procedure TestTIdSipTransportEventNotifications.TestAddTransportSendingListener;
begin
  Self.Transport.AddTransportSendingListener(Self);

  Self.Transport.NotifyTransportSendingListeners(Self.Request);

  Check(Self.SentRequest, 'Listener wasn''t added');
end;

procedure TestTIdSipTransportEventNotifications.TestAllListenersReceiveRequests;
var
  Listener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Transport.AddTransportListener(Self);
    Self.Transport.AddTransportListener(Listener);

    Self.Transport.NotifyTransportListeners(Self.Request);

    Check(Self.ReceivedRequest and Listener.ReceivedRequest,
          'Not all Listeners received the request');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransportEventNotifications.TestAllListenersReceiveResponses;
var
  Listener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Transport.AddTransportListener(Self);
    Self.Transport.AddTransportListener(Listener);

    Self.Transport.NotifyTransportListeners(Self.Response);

    Check(Self.ReceivedResponse and Listener.ReceivedResponse,
          'Not all Listeners received the Response');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransportEventNotifications.TestAllListenersSendRequests;
var
  Listener: TIdSipTestTransportSendingListener;
begin
  Listener := TIdSipTestTransportSendingListener.Create;
  try
    Self.Transport.AddTransportSendingListener(Self);
    Self.Transport.AddTransportSendingListener(Listener);

    Self.Transport.NotifyTransportSendingListeners(Self.Request);

    Check(Self.SentRequest and Listener.SentRequest,
          'Not all Listeners Sent the request');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransportEventNotifications.TestAllListenersSendResponses;
var
  Listener: TIdSipTestTransportSendingListener;
begin
  Listener := TIdSipTestTransportSendingListener.Create;
  try
    Self.Transport.AddTransportSendingListener(Self);
    Self.Transport.AddTransportSendingListener(Listener);

    Self.Transport.NotifyTransportSendingListeners(Self.Response);

    Check(Self.SentResponse and Listener.SentResponse,
          'Not all Listeners Sent the Response');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransportEventNotifications.TestRemoveTransportListener;
begin
  Self.Transport.AddTransportListener(Self);
  Self.Transport.RemoveTransportListener(Self);

  Self.Transport.NotifyTransportListeners(Self.Request);

  Check(not Self.ReceivedRequest, 'Listener wasn''t removed');
end;

procedure TestTIdSipTransportEventNotifications.TestRemoveTransportSendingListener;
begin
  Self.Transport.AddTransportSendingListener(Self);
  Self.Transport.RemoveTransportSendingListener(Self);

  Self.Transport.NotifyTransportSendingListeners(Self.Request);

  Check(not Self.SentRequest, 'Listener wasn''t removed');
end;

//******************************************************************************
//* TestTIdSipTransport                                                        *
//******************************************************************************
//* TestTIdSipTransport Public methods *****************************************

procedure TestTIdSipTransport.SetUp;
var
  Binding: TIdSocketHandle;
begin
  inherited SetUp;

  Self.ExceptionMessage := 'Response not received - event didn''t fire';

  Self.HighPortTransport := Self.TransportType.Create(Self.DefaultPort + 10000);
  Self.HighPortTransport.AddTransportListener(Self);
  Self.HighPortTransport.Timeout := 500;
  Self.HighPortTransport.HostName := IndyGetHostName;
  Self.HighPortTransport.Bindings.Clear;
  Binding := Self.HighPortTransport.Bindings.Add;
  Binding.IP := GStack.LocalAddress;
  Binding.Port := Self.DefaultPort + 10000;
  Self.HighPortTransport.Start;

  Self.LowPortTransport := Self.TransportType.Create(Self.DefaultPort);
  Self.LowPortTransport.AddTransportListener(Self);
  Self.LowPortTransport.Timeout := 500;
  Self.LowPortTransport.HostName := 'localhost';
  Self.LowPortTransport.Bindings.Clear;
  Binding := Self.LowPortTransport.Bindings.Add;
  Binding.IP := '127.0.0.1';
  Binding.Port := Self.DefaultPort;
  Self.LowPortTransport.Start;

  Self.Request  := TIdSipTestResources.CreateLocalLoopRequest;
  Self.Request.LastHop.SentBy     := Self.LowPortTransport.Bindings[0].IP;
  Self.Request.RequestUri.Host := Self.HighPortTransport.HostName;
  Self.Request.RequestUri.Port := Self.HighPortTransport.Bindings[0].Port;

  Self.Response := TIdSipTestResources.CreateLocalLoopResponse;
  Self.Response.LastHop.Transport := Self.HighPortTransport.GetTransportType;

  Self.RecvdRequest := TIdSipRequest.Create;

  Self.ReceivedRequest  := false;
  Self.ReceivedResponse := false;
  Self.RejectedMessage  := false;
  Self.WrongServer      := false;
end;

procedure TestTIdSipTransport.TearDown;
begin
  Self.RecvdRequest.Free;
  Self.Response.Free;
  Self.Request.Free;

  Self.LowPortTransport.Stop;
  Self.HighPortTransport.Stop;

  Self.LowPortTransport.Free;
  Self.HighPortTransport.Free;

  inherited TearDown;
end;

//* TestTIdSipTransport Protected methods **************************************

procedure TestTIdSipTransport.CheckCanReceiveRequest(Sender: TObject;
                                                     R: TIdSipRequest);
begin
  try
    Self.ReceivedRequest := true;
    Self.SendOkResponse(Sender as TIdSipTransport);

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckCanReceiveResponse(Sender: TObject;
                                                      R: TIdSipResponse);
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

procedure TestTIdSipTransport.CheckDiscardResponseWithUnknownSentBy(Sender: TObject;
                                                                    R: TIdSipResponse);
begin
  Self.ReceivedResponse := true;
end;

procedure TestTIdSipTransport.CheckReceivedParamDifferentIPv4SentBy(Sender: TObject;
                                                                    Request: TIdSipRequest);
begin
  Self.CheckReceivedParamFQDNSentBy(Sender, Request);
end;

procedure TestTIdSipTransport.CheckReceivedParamFQDNSentBy(Sender: TObject;
                                                           Request: TIdSipRequest);
begin
  try
    Check(Request.LastHop.HasReceived,
          Self.HighPortTransport.ClassName
        + ': Received param not appended by transport layer');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckReceivedParamIPv4SentBy(Sender: TObject;
                                                           Request: TIdSipRequest);
begin
  try
    Check(not Request.LastHop.HasReceived,
          Self.HighPortTransport.ClassName
        + ': Received param appended by transport layer');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckSendRequestFromNonStandardPort(Sender: TObject;
                                                                  R: TIdSipRequest);
begin
  try
    CheckEquals(Request.LastHop.Port,
                Self.HighPortTransport.Bindings[0].Port,
                Self.HighPortTransport.ClassName
              + ': Port number on top via');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckSendRequestTopVia(Sender: TObject;
                                                     R: TIdSipRequest);
begin
  try
    Check(Self.HighPortTransport.GetTransportType = R.LastHop.Transport,
          Self.HighPortTransport.ClassName
       + ': Incorrect transport specified');

    Check(R.LastHop.HasBranch,
          Self.HighPortTransport.ClassName + ': Branch parameter missing');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckSendResponseFromNonStandardPort(Sender: TObject;
                                                                   R: TIdSipResponse);
begin
  try
    CheckEquals(Self.LowPortTransport.Bindings[0].Port,
                R.LastHop.Port,
                Self.HighPortTransport.ClassName
              + ': Port number on top via');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.CheckUseRport(Sender: TObject;
                                            R: TIdSipRequest);
begin
  try
    Check(R.LastHop.HasParam(RportParam),
          'No rport param');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;          
end;

function TestTIdSipTransport.DefaultPort: Cardinal;
begin
  Result := IdPORT_SIP;
end;

procedure TestTIdSipTransport.OnException(E: Exception;
                                          const Reason: String);
begin
  Self.ExceptionType := ExceptClass(E.ClassType);
  Self.ExceptionMessage := E.Message + ' caused by ''' + Reason + '''';
end;

procedure TestTIdSipTransport.OnReceiveRequest(Request: TIdSipRequest;
                                               Receiver: TIdSipTransport);
begin
  Self.RecvdRequest.Assign(Request);

  if Assigned(Self.CheckingRequestEvent) then
    Self.CheckingRequestEvent(Receiver, Request);
end;

procedure TestTIdSipTransport.OnReceiveResponse(Response: TIdSipResponse;
                                                Receiver: TIdSipTransport);
begin
  if Assigned(Self.CheckingResponseEvent) then
    Self.CheckingResponseEvent(Receiver, Response);
end;

procedure TestTIdSipTransport.OnRejectedMessage(const Msg: String;
                                                const Reason: String);
begin
  Self.RejectedMessage := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTransport.ReturnResponse(Sender: TObject;
                                             R: TIdSipRequest);
begin
  try
    Self.SendOkResponse(Sender as TIdSipTransport);
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipTransport.SendOkResponse(Transport: TIdSipTransport);
begin
  Self.Response.StatusCode := SIPOK;

  Transport.Send(Self.Response);
end;

function TestTIdSipTransport.TransportType: TIdSipTransportClass;
begin
  Result := nil;
end;

//* TestTIdSipTransport Published methods **************************************

procedure TestTIdSipTransport.TestCanReceiveRequest;
begin
  // LowPortTransport sends an INVITE to HighPortTransport.
  // We check that HighPortTransport did actually get it.
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.LowPortTransport.Send(Self.Request);

  Self.WaitForSignaled;

  Check(Self.ReceivedRequest,
        Self.HighPortTransport.ClassName + ': Request not received');
end;

procedure TestTIdSipTransport.TestCanReceiveResponse;
begin
  // LowPortTransport sends an INVITE to HighPortTransport.
  // HighPortTransport is set to immediately respond with a 200 OK.
  // We then check that LowPortTransport got the returned response.
  Self.CheckingRequestEvent  := Self.ReturnResponse;
  Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;

  Self.LowPortTransport.Send(Self.Request);

  Self.WaitForSignaled;

  Check(Self.ReceivedResponse,
        Self.HighPortTransport.ClassName + ': Response not received');
end;

procedure TestTIdSipTransport.TestCanReceiveUnsolicitedResponse;
begin
  Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;

  Self.HighPortTransport.Send(Self.Response);

  Self.WaitForSignaled;

  Check(Self.ReceivedResponse,
        Self.HighPortTransport.ClassName + ': Response not received');
end;

procedure TestTIdSipTransport.TestIsNull;
begin
  Check(not Self.HighPortTransport.IsNull,
        'non-null transport (' + Self.HighPortTransport.ClassName
      + ') marked as null');
end;

procedure TestTIdSipTransport.TestTransportFor;
begin
  CheckEquals(TIdSipTCPTransport,  TIdSipTransport.TransportFor(sttTCP),  'TCP');
  CheckEquals(TIdSipTLSTransport,  TIdSipTransport.TransportFor(sttTLS),  'TLS');
  CheckEquals(TIdSipUDPTransport,  TIdSipTransport.TransportFor(sttUDP),  'UDP');
  CheckEquals(TIdSipSCTPTransport, TIdSipTransport.TransportFor(sttSCTP), 'SCTP');
end;

procedure TestTIdSipTransport.TestDiscardResponseWithUnknownSentBy;
begin
  Self.CheckingResponseEvent := Self.CheckDiscardResponseWithUnknownSentBy;

  // If we don't set the received param we won't be able to send
  // the message to the right SIP server. No, you'd never do this
  // in production, because it's wilfully wrong.
  Self.Response.LastHop.SentBy := 'unknown.host';
  Self.Response.LastHop.Received := Self.LowPortTransport.Bindings[0].IP;
  Self.LowPortTransport.Send(Self.Response);

  Self.WaitForSignaled;
  Check(not Self.ReceivedResponse,
        Self.HighPortTransport.ClassName
      + ': Response not silently discarded');
  Check(Self.RejectedMessage,
        Self.HighPortTransport.ClassName
      + ': Rejected message event didn''t fire');
end;

procedure TestTIdSipTransport.TestDiscardMalformedMessage;
var
  MangledSipVersion: String;
begin
  MangledSipVersion := 'SIP/;2.0';
  Self.SendMessage('INVITE sip:wintermute@tessier-ashpool.co.luna ' + MangledSipVersion + #13#10
                 + 'Via: SIP/2.0/TCP %s;branch=z9hG4bK776asdhds'#13#10
                 + 'Max-Forwards: 70'#13#10
                 + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.luna>'#13#10
                 + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                 + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                 + 'CSeq: 314159 INVITE'#13#10
                 + 'Contact: <sip:wintermute@tessier-ashpool.co.luna>'#13#10
                 + 'Content-Type: text/plain'#13#10
                 + 'Content-Length: 29'#13#10
                 + #13#10
                 + 'I am a message. Hear me roar!');

  Self.WaitForSignaled;
  Check(not Self.ReceivedRequest,
        Self.HighPortTransport.ClassName
      + ': Somehow we received a mangled message');
  Check(Self.RejectedMessage,
        Self.HighPortTransport.ClassName
      + ': Notification of message rejection not received');
end;

procedure TestTIdSipTransport.TestDiscardUnknownSipVersion;
begin
  Self.ExceptionMessage := 'Waiting for request to arrive';
  Self.SendMessage(TortureTest41);
  Self.WaitForTimeout(Self.ClassName
                    + ': Received a message with an unknown SIP-Version');

  Check(not Self.ReceivedRequest,
        Self.ClassName
      + ': Received a message with an unknown SIP-Version');
end;

procedure TestTIdSipTransport.TestReceivedParamDifferentIPv4SentBy;
begin
  Self.CheckingRequestEvent := Self.CheckReceivedParamDifferentIPv4SentBy;

  Self.Request.LastHop.SentBy := '127.0.0.3';
  Self.LowPortTransport.Send(Self.Request);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestReceivedParamFQDNSentBy;
begin
  Self.CheckingRequestEvent := Self.CheckReceivedParamFQDNSentBy;

  Self.Request.LastHop.SentBy := 'localhost';
  Self.LowPortTransport.Send(Self.Request);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestReceivedParamIPv4SentBy;
begin
  Self.CheckingRequestEvent := Self.CheckReceivedParamIPv4SentBy;
  // This is a bit of a hack. We want to make sure the sent-by's an IP.
  Self.HighPortTransport.HostName := Self.HighPortTransport.Bindings[0].IP;
  Self.HighPortTransport.Send(Self.Request);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestSendRequest;
begin
  Self.CheckingRequestEvent := Self.CheckCanReceiveRequest;

  Self.LowPortTransport.Send(Self.Request);

  Self.WaitForSignaled;

  Check(Self.ReceivedRequest,
        Self.HighPortTransport.ClassName + ': Request not received');
end;

procedure TestTIdSipTransport.TestSendRequestFromNonStandardPort;
begin
  Self.Request.RequestUri.Host := Self.LowPortTransport.HostName;
  Self.Request.RequestUri.Port := Self.LowPortTransport.Bindings[0].Port;
  Self.CheckingRequestEvent := Self.CheckSendRequestFromNonStandardPort;
  Self.HighPortTransport.Send(Self.Request);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestSendRequestTopVia;
begin
  Self.CheckingRequestEvent := Self.CheckSendRequestTopVia;
  Self.LowPortTransport.Send(Self.Request);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestSendResponse;
begin
  Self.CheckingResponseEvent := Self.CheckCanReceiveResponse;

  Self.LowPortTransport.Send(Self.Response);

  Self.WaitForSignaled;

  Check(Self.ReceivedResponse,
        Self.HighPortTransport.ClassName + ': Response not received');
end;

procedure TestTIdSipTransport.TestSendResponseFromNonStandardPort;
begin
  Self.Response.LastHop.Port := Self.LowPortTransport.Bindings[0].Port;
  Self.CheckingResponseEvent := Self.CheckSendResponseFromNonStandardPort;
  Self.HighPortTransport.Send(Self.Response);

  Self.WaitForSignaled;
end;

procedure TestTIdSipTransport.TestSendResponseWithReceivedParam;
var
  HighPortListener: TIdSipTestTransportListener;
  LowPortListener:  TIdSipTestTransportListener;
begin
  HighPortListener := TIdSipTestTransportListener.Create;
  try
    Self.HighPortTransport.AddTransportListener(HighPortListener);
    try
      LowPortListener := TIdSipTestTransportListener.Create;
      try
        Self.LowPortTransport.AddTransportListener(LowPortListener);
        try
          Check(Self.LowPortTransport.Bindings.Count > 0,
                Self.HighPortTransport.ClassName
              + ': Sanity check on LowPortTransport''s bindings');

          Self.Response.LastHop.Received := Self.LowPortTransport.Bindings[0].IP;
          Self.HighPortTransport.Send(Self.Response);

          // It's not perfect, but anyway. We need to wait long enough for
          // LowPortTransport to get its response.
          IdGlobal.Sleep(500);

          Check(LowPortListener.ReceivedResponse
                and not HighPortListener.ReceivedResponse,
                Self.HighPortTransport.ClassName
              + ': Received param in top Via header ignored - '
              + 'wrong server got the message');
        finally
          Self.LowPortTransport.RemoveTransportListener(LowPortListener);
        end;
      finally
        LowPortListener.Free;
      end;
    finally
      Self.HighPortTransport.RemoveTransportListener(HighPortListener);
    end;
  finally
    HighPortListener.Free;
  end;
end;

procedure TestTIdSipTransport.TestUseRport;
begin
  Self.ExceptionMessage := 'Waiting for rport request';
  Self.LowPortTransport.UseRport := true;
  Self.CheckingRequestEvent := Self.CheckUseRport;
  Self.LowPortTransport.Send(Self.Request);

  Self.WaitForSignaled;

end;

//******************************************************************************
//* TestTIdSipTCPTransport                                                     *
//******************************************************************************
//* TestTIdSipTCPTransport Protected methods ***********************************

procedure TestTIdSipTCPTransport.SendMessage(Msg: String);
var
  Client: TIdTcpClient;
begin
  Client := TIdTcpClient.Create(nil);
  try
    Client.Host := Self.HighPortTransport.Bindings[0].IP;
    Client.Port := Self.HighPortTransport.Bindings[0].Port;
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

function TestTIdSipTCPTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipTcpTransport;
end;

//* TestTIdSipTCPTransport Published methods ***********************************

procedure TestTIdSipTCPTransport.TestGetTransportType;
begin
  Check(Self.HighPortTransport.GetTransportType = sttTCP, 'Transport type');
end;

procedure TestTIdSipTCPTransport.TestIsReliable;
begin
  Check(Self.HighPortTransport.IsReliable,
        'TCP transport not marked as reliable');
end;

procedure TestTIdSipTCPTransport.TestIsSecure;
begin
  Check(not Self.HighPortTransport.IsSecure, 'TCP transport marked as secure');
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

//* TestTIdSipTLSTransport Protected methods ***********************************

function TestTIdSipTLSTransport.DefaultPort: Cardinal;
begin
  Result := IdPORT_SIPS;
end;

procedure TestTIdSipTLSTransport.SendMessage(Msg: String);
var
  Client: TIdTcpClient;
begin
  // TODO: This won't work! You need to set up the certs & such!
  Client := TIdTcpClient.Create(nil);
  try
    Client.Host := Self.HighPortTransport.Bindings[0].IP;
    Client.Port := Self.HighPortTransport.Bindings[0].Port;
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
  Check(sttTLS = Self.HighPortTransport.GetTransportType, 'Transport type');
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

//******************************************************************************
//* TestTIdSipUDPTransport                                                     *
//******************************************************************************
//* TestTIdSipUDPTransport Public methods **************************************

procedure TestTIdSipUDPTransport.SetUp;
var
  Binding: TIdSocketHandle;
begin
  inherited SetUp;

  Self.MaddrTransport := Self.TransportType.Create(IdPORT_SIP);
  Self.MaddrTransport.AddTransportListener(Self);
  Self.MaddrTransport.HostName := 'localhost';
  Binding := Self.MaddrTransport.Bindings.Add;
  Binding.IP := '127.0.0.2';
  Binding.Port := IdPORT_SIP;

  Self.RPortEvent := TSimpleEvent.Create;
end;

procedure TestTIdSipUDPTransport.TearDown;
begin
  Self.RPortEvent.Free;
  Self.MaddrTransport.Free;

  inherited TearDown;
end;

//* TestTIdSipUDPTransport Protected methods ***********************************

procedure TestTIdSipUDPTransport.SendMessage(Msg: String);
var
  Client: TIdUdpClient;
begin
  Client := TIdUdpClient.Create(nil);
  try
    Client.Host := Self.HighPortTransport.Bindings[0].IP;
    Client.Port := Self.HighPortTransport.Bindings[0].Port;

    Client.Send(Msg);
  finally
    Client.Free;
  end;
end;

function TestTIdSipUDPTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipUdpTransport;
end;

//* TestTIdSipUDPTransport Private methods *************************************

procedure TestTIdSipUDPTransport.CheckMessageWithTrailingGarbage(Sender: TObject;
                                                                 R: TIdSipRequest);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUDPTransport.CheckRportParamFilledIn(Sender: TObject;
                                                         R: TIdSipRequest);
begin
  CheckNotEquals('',
                 R.LastHop.Params[RPortParam],
                 'Transport didn''t fill in the rport param');

  Self.NoteSourcePort(Sender, R);
end;

procedure TestTIdSipUDPTransport.CheckLeaveNonRportRequestsUntouched(Sender: TObject;
                                                                     R: TIdSipRequest);
begin
  try
    Check(not R.LastHop.HasRport, 'rport param added by transport');
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUDPTransport.CheckMaddrUser(Sender: TObject;
                                                R: TIdSipResponse);
begin
  try
    Check(Sender = Self.MaddrTransport,
          'An unexpected transport received the response');
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUDPTransport.NoteSourcePort(Sender: TObject;
                                                R: TIdSipRequest);
begin
  try
    Self.RPort := R.LastHop.RPort;
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdSipUDPTransport.OnMalformedMessage(const Msg: String;
                                                    const Reason: String);
begin
end;

procedure TestTIdSipUDPTransport.OnReceiveRequest(Request: TIdSipRequest;
                                                  ReceivedFrom: TIdSipConnectionBindings);
begin
end;

procedure TestTIdSipUDPTransport.OnReceiveResponse(Response: TIdSipResponse;
                                                   ReceivedFrom: TIdSipConnectionBindings);
begin
  try
    Self.RPort := ReceivedFrom.PeerPort;
    Self.RPortEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

//* TestTIdSipUDPTransport Published methods ***********************************

procedure TestTIdSipUDPTransport.TestMessageWithTrailingGarbage;
var
  Body:   String;
  Client: TIdUdpClient;
begin
  Self.CheckingRequestEvent := Self.CheckMessageWithTrailingGarbage;
  Body := 'I am a message. Hear me roar!';

  Client := TIdUdpClient.Create(nil);
  try
    Client.Host := Self.HighPortTransport.Bindings[0].IP;
    Client.Port := Self.HighPortTransport.Bindings[0].Port;

    Client.Send('INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0'#13#10
              + 'Via: SIP/2.0/TCP %s;branch=z9hG4bK776asdhds'#13#10
              + 'Max-Forwards: 70'#13#10
              + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.luna>'#13#10
              + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
              + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
              + 'CSeq: 314159 INVITE'#13#10
              + 'Contact: <sip:wintermute@tessier-ashpool.co.luna>'#13#10
              + 'Content-Type: text/plain'#13#10
              + 'Content-Length: ' + IntToStr(Length(Body)) + #13#10
              + #13#10
              + Body + #0#0#0#0#0);

    Self.ExceptionMessage := Self.HighPortTransport.ClassName
        + ': We rejected a message with a Content-Length that had trailing '
        + 'octets';
    Self.WaitForSignaled;
    CheckEquals(Body,
                Self.Request.Body,
                'Body of message');
  finally
    Client.Free;
  end;
end;

procedure TestTIdSipUDPTransport.TestGetTransportType;
begin
  Check(Self.HighPortTransport.GetTransportType = sttUDP, 'Transport type');
end;

procedure TestTIdSipUDPTransport.TestIsReliable;
begin
  Check(not Self.HighPortTransport.IsReliable,
        'UDP transport not marked as unreliable');
end;

procedure TestTIdSipUDPTransport.TestIsSecure;
begin
  Check(not Self.HighPortTransport.IsSecure,
        'UDP transport marked as secure');
end;

procedure TestTIdSipUDPTransport.TestLeaveNonRportRequestsUntouched;
begin
  Self.CheckingRequestEvent := Self.CheckLeaveNonRportRequestsUntouched;
  Self.LowPortTransport.Send(Self.Request);

  Self.WaitForSignaled;  

  CheckEquals(0, Self.RPort, 'rport value');
end;

procedure TestTIdSipUDPTransport.TestMaddrUsed;
begin
  Self.CheckingResponseEvent := Self.CheckMaddrUser;
  Self.MaddrTransport.Start;
  try
    Self.Response.LastHop.Maddr  := Self.MaddrTransport.Bindings[0].IP;
    Self.Response.LastHop.SentBy := Self.MaddrTransport.Bindings[0].IP;

    Self.LowPortTransport.Send(Self.Response);

    Self.WaitForSignaled;
  finally
    Self.MaddrTransport.Stop;
  end;
end;

procedure TestTIdSipUDPTransport.TestRespectRport;
var
  Server: TIdSipUdpServer;
begin
  // If we get a request with an rport param (which MUST be empty!) then we
  // should send our responses back to the server at received:rport.

  // What should happen is that:
  // 1. Server sends a request to Self.HighPortTransport
  // 2. HighPortTransport (because of Self.ReturnResponse) sends back a 200 OK
  // 3. Server receives this response and sets Self.RPortEvent

  Self.CheckingRequestEvent := Self.ReturnResponse;

  Self.Request.LastHop.Params[RPortParam] := '';

  Server := TIdSipUdpServer.Create(nil);
  try
    Server.AddMessageListener(Self);
    Server.DefaultPort := 5000; // some arbitrary value
    Server.Active      := true;

    Self.Response.LastHop.Received := Self.HighPortTransport.Bindings[0].IP;
    Self.Response.LastHop.Rport    := Server.DefaultPort;

    Server.Send(Self.HighPortTransport.Bindings[0].IP,
                Self.HighPortTransport.Bindings[0].Port,
                Self.Request.AsString);

    Self.WaitForSignaled(Self.RPortEvent);
  finally
    Server.Free;
  end;
end;

procedure TestTIdSipUDPTransport.TestRportParamFilledIn;
begin
  Self.CheckingRequestEvent := Self.CheckRportParamFilledIn;
  Self.Request.LastHop.Params[RportParam] := '';
  Self.LowPortTransport.Send(Self.Request);

  Self.WaitForSignaled;

  CheckNotEquals(0, Self.RPort, 'rport value');
end;

procedure TestTIdSipUDPTransport.TestRportListening;
var
  Binding: TIdSocketHandle;
  Server:  TIdUdpServer;
begin
  // We test here that when a request is sent from port x, that we listen on
  // port x for responses, as per RFC 3581
  Self.CheckingRequestEvent := Self.NoteSourcePort;
  Self.Request.LastHop.Params[RportParam] := '';
  Self.LowPortTransport.Send(Self.Request);

  Self.WaitForSignaled;

  Server := TIdUdpServer.Create(nil);
  try
    try
      Binding := Server.Bindings.Add;
      Binding.IP   := Self.LowPortTransport.Bindings[0].IP;
      Binding.Port := Self.RPort;

      Server.Active := true;
      Fail('Server wasn''t listening on the port from which it sent an rport'
         + ' request');
    except
      on EIdCouldNotBindSocket do;
    end;
  finally
    Server.Free;
  end;
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

//******************************************************************************
//* TestTIdSipNullTransport                                                    *
//******************************************************************************
//* TestTIdSipNullTransport Protected methods **********************************

function TestTIdSipNullTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipNullTransport;
end;

//* TestTIdSipNullTransport Published methods **********************************

procedure TestTIdSipNullTransport.TestIsNull;
begin
  Check(Self.HighPortTransport.IsNull, 'Null transport marked as non-null');
end;

//******************************************************************************
//* TTransportMethodTestCase                                                   *
//******************************************************************************
//* TTransportMethodTestCase Public methods ************************************

procedure TTransportMethodTestCase.SetUp;
begin
  inherited SetUp;

  Self.Transport := TIdSipNullTransport.Create(IdPORT_SIP);
end;

procedure TTransportMethodTestCase.TearDown;
begin
  Self.Transport.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipTransportExceptionMethod                                         *
//******************************************************************************
//* TestTIdSipTransportExceptionMethod Public methods **************************

procedure TestTIdSipTransportExceptionMethod.SetUp;
begin
  inherited SetUp;

  Self.Exception := EUnknownTransport.Create('');

  Self.Method := TIdSipTransportExceptionMethod.Create;
  Self.Method.Exception := Self.Exception;
  Self.Method.Reason    := 'Bar';
end;

procedure TestTIdSipTransportExceptionMethod.TearDown;
begin
  Self.Method.Free;
  Self.Exception.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportExceptionMethod Published methods ***********************

procedure TestTIdSipTransportExceptionMethod.TestRun;
var
  Listener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.Exception, 'Listener not notified');
    Check(Self.Method.Exception = Listener.ExceptionParam,
          'Exception param');
    CheckEquals(Self.Method.Reason,
                Listener.ReasonParam,
                'Reason param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTransportReceiveRequestMethod                                    *
//******************************************************************************
//* TestTIdSipTransportReceiveRequestMethod Public methods *********************

procedure TestTIdSipTransportReceiveRequestMethod.SetUp;
begin
  inherited SetUp;

  Self.Request := TIdSipRequest.Create;

  Self.Method := TIdSipTransportReceiveRequestMethod.Create;
  Self.Method.Receiver := Self.Transport;
  Self.Method.Request  := Self.Request;
end;

procedure TestTIdSipTransportReceiveRequestMethod.TearDown;
begin
  Self.Method.Free;
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportReceiveRequestMethod Published methods ******************

procedure TestTIdSipTransportReceiveRequestMethod.TestRun;
var
  Listener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.ReceivedRequest, 'Listener not notified');
    Check(Self.Method.Receiver = Listener.ReceiverParam,
          'Receiver param');
    Check(Self.Method.Request = Listener.RequestParam,
          'Request param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTransportReceiveResponseMethod                                   *
//******************************************************************************
//* TestTIdSipTransportReceiveResponseMethod Public methods ********************

procedure TestTIdSipTransportReceiveResponseMethod.SetUp;
begin
  inherited SetUp;

  Self.Response := TIdSipResponse.Create;

  Self.Method := TIdSipTransportReceiveResponseMethod.Create;
  Self.Method.Receiver := Self.Transport;
  Self.Method.Response := Self.Response;
end;

procedure TestTIdSipTransportReceiveResponseMethod.TearDown;
begin
  Self.Method.Free;
  Self.Response.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportReceiveResponseMethod Published methods *****************

procedure TestTIdSipTransportReceiveResponseMethod.TestRun;
var
  Listener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.ReceivedResponse, 'Listener not notified');
    Check(Self.Method.Receiver = Listener.ReceiverParam,
          'Receiver param');
    Check(Self.Method.Response = Listener.ResponseParam,
          'Response param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTransportRejectedMessageMethod                                   *
//******************************************************************************
//* TestTIdSipTransportRejectedMessageMethod Public methods ********************

procedure TestTIdSipTransportRejectedMessageMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipTransportRejectedMessageMethod.Create;
  Self.Method.Msg    := 'Foo';
  Self.Method.Reason := 'Bar';
end;

procedure TestTIdSipTransportRejectedMessageMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportRejectedMessageMethod Published methods *****************

procedure TestTIdSipTransportRejectedMessageMethod.TestRun;
var
  Listener: TIdSipTestTransportListener;
begin
  Listener := TIdSipTestTransportListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.RejectedMessage, 'Listener not notified');
    CheckEquals(Self.Method.Msg,
                Listener.MsgParam,
                'Msg param');
    CheckEquals(Self.Method.Reason,
                Listener.ReasonParam,
                'Reason param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTransportSendingRequestMethod                                    *
//******************************************************************************
//* TestTIdSipTransportSendingRequestMethod Public methods *********************

procedure TestTIdSipTransportSendingRequestMethod.SetUp;
begin
  inherited SetUp;

  Self.Request := TIdSipRequest.Create;

  Self.Method := TIdSipTransportSendingRequestMethod.Create;
  Self.Method.Request := Self.Request;
  Self.Method.Sender  := Self.Transport;
end;

procedure TestTIdSipTransportSendingRequestMethod.TearDown;
begin
  Self.Method.Free;
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportSendingRequestMethod Published methods ******************

procedure TestTIdSipTransportSendingRequestMethod.TestRun;
var
  Listener: TIdSipTestTransportSendingListener;
begin
  Listener := TIdSipTestTransportSendingListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.SentRequest, 'Listener not notified');
    Check(Self.Method.Sender = Listener.SenderParam,
          'Sender param');
    Check(Self.Method.Request = Listener.RequestParam,
          'Request param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTransportSendingResponseMethod                                   *
//******************************************************************************
//* TestTIdSipTransportSendingResponseMethod Public methods ********************

procedure TestTIdSipTransportSendingResponseMethod.SetUp;
begin
  inherited SetUp;

  Self.Response := TIdSipResponse.Create;

  Self.Method := TIdSipTransportSendingResponseMethod.Create;
  Self.Method.Response := Self.Response;
  Self.Method.Sender   := Self.Transport;
end;

procedure TestTIdSipTransportSendingResponseMethod.TearDown;
begin
  Self.Method.Free;
  Self.Response.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportSendingResponseMethod Published methods *****************

procedure TestTIdSipTransportSendingResponseMethod.TestRun;
var
  Listener: TIdSipTestTransportSendingListener;
begin
  Listener := TIdSipTestTransportSendingListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.SentResponse, 'Listener not notified');
    Check(Self.Method.Sender = Listener.SenderParam,
          'Sender param');
    Check(Self.Method.Response = Listener.ResponseParam,
          'Response param');
  finally
    Listener.Free;
  end;
end;

initialization
  RegisterTest('IdSipTransport', Suite);
end.
