{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipStackInterface;

interface

uses
  Classes, Contnrs, Forms, IdSipDialog, IdSipInviteModule, IdSipMessage,
  IdSipMockTransport, IdSipStackInterface, IdSipSubscribeModule, IdSipUserAgent,
  IdTimerQueue, Messages, TestFramework, TestFrameworkEx, TestFrameworkSip;

type
  // The testing of the StackInterface is not completely simple. The UI (or
  // this test case) and the stack-running thread communicate using a Windows
  // message queue. TTestCases don't have a window handle, so we create a
  // TStackWindow which does.
  //
  // As an example, let's look at how TestInboundCall works. The test case
  // sends a SIP message through the (local loopback) network to the stack. The
  // stack sees it's an inbound INVITE, does what it needs to do, and posts the
  // CM_CALL_REQUEST_NOTIFY message to the UI's message queue. The UI picks this
  // up, fires the TestCase's OnEvent. Now we set TestCase.CheckDataProc to
  // point to CheckInboundCallData, so we can check the data.



  // That window does nothing but do stuff to the test
  // cases. That means that you have to keep the message handlers in the
  // TStackWindow in sync with those defined in the StackInterface.

  TDataCheckProc = procedure(Stack: TIdSipStackInterface;
                             Event: Cardinal;
                             Data: TIdEventData) of object;

  TestTIdSipStackInterfaceCreation = class(TTestCase)
  published
    procedure TestCreateStackWithNoSubscribeSupport;
  end;

  // When writing tests for the stack interface, remember that the stack runs in
  // a separate thread. All the methods (that don't create Actions) of the
  // StackInterface use TIdWaits to schedule events within the stack thread.
  // Thus, when you invoke these methods (like Send, AnswerCall, RejectCall,
  // etc.), you have to trigger the newly-scheduled events by, for instance,
  //
  //    Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  //
  // The same applies for notifications: the StackWindow sends us notifications
  // like CM_CALL_REQUEST_NOTIFY, and you have to process these notifications
  // (by invoking Application.ProcessMessages) before you can inspect what the
  // stack does with these notification, or how it presents them. This means
  // that if you're establishing a call and you receive a 200 OK, you must call
  // Application.ProcessMessages before the test can know about the response.
  TestTIdSipStackInterface = class(TThreadingTestCase,
                                   IIdSipInviteModuleListener)
  private
    fIntf:             TIdSipStackInterface;
    DataList:          TObjectList; // Holds all the data received from the stack
    Destination:       TIdSipToHeader;
    LocalAddress:      String;
    LocalMimeType:     String;
    LocalOffer:        String;
    LocalPort:         Cardinal;
    MockTransport:     TIdSipMockTransport;
    Registrar:         TIdSipUri;
    RemoteMimeType:    String;
    RemoteOffer:       String;
    RemoteSession:     TIdSipInboundSession;
    RemoteUA:          TIdSipUserAgent;
    Requests:          TIdSipRequestList;
    Responses:         TIdSipResponseList;
    SentRequestCount:  Cardinal;
    SentResponseCount: Cardinal;
    TargetAddress:     String;
    TargetPort:        Cardinal;
    TimerQueue:        TIdDebugTimerQueue;
    UI:                TCustomForm;

    procedure AddSubscribeSupport(Stack: TIdSipStackInterface);
    procedure CheckNotificationReceived(EventType: TIdEventDataClass; Msg: String);
    procedure CheckRequestSent(Msg: String);
    procedure CheckResponseSent(Msg: String);
{
    function  CreateBindings: TIdSipContacts;
    function  CreateRemoteBye(LocalDialog: TIdSipDialog): TIdSipRequest;
}
    function  CreateRemoteInvite: TIdSipRequest;
    function  CreateRemoteNotify(RemoteDialog: TIdSipDialog; Subscribe: TIdSipRequest): TIdSipRequest;
    function  CreateRemoteOk(Request: TIdSipRequest): TIdSipResponse;
    function  EstablishCall: TIdSipHandle;
    function  EventAt(Index: Integer): TIdEventData;
    function  LastEventOfType(EventType: TIdEventDataClass): TIdEventData;
    function  LastSentRequest: TIdSipRequest;
    function  LastSentResponse: TIdSipResponse;
    procedure MarkSentRequestCount;
    procedure MarkSentResponseCount;
    procedure OnInboundCall(UserAgent: TIdSipInviteModule;
                            Session: TIdSipInboundSession);
{
    procedure LogSentMessage(Msg: TIdSipMessage);
}
    procedure ReceiveAck;
    procedure ReceiveBusyHereFromRegistrar(Register: TIdSipRequest);
{
    procedure ReceiveBye(LocalDialog: TIdSipDialog);
    procedure ReceiveByeForOutboundCall;
}
    procedure ReceiveIntervalTooBrief(Register: TIdSipRequest);
    procedure ReceiveInvite;
    procedure ReceiveInviteWithOffer(const Offer: String;
                                     const MimeType: String);
    procedure ReceiveNotify(RemoteDialog: TIdSipDialog; Subscribe: TIdSipRequest);
    procedure ReceiveTerminatingNotify(RemoteDialog: TIdSipDialog; Subscribe: TIdSipRequest; Reason: String);
    procedure ReceiveOk(Request: TIdSipRequest; Offer: String = ''; MimeType: String = '');
{
    procedure ReceiveOkWithContacts(Register: TIdSipRequest;
                                    Contacts: TIdSipContacts);
    procedure ReceiveOkWithOffer(Invite: TIdSipRequest;
                                 const Offer: String;
                                 const MimeType: String);
}
    procedure ReceiveRequest(Request: TIdSipRequest);
    procedure ReceiveResponse(Response: TIdSipResponse);
    procedure ReceiveSubscribe(EventPackage: String);
    function  SecondLastEventData: TIdEventData;
    procedure SetUpPackageSupport(EventPackage: TIdSipEventPackageClass);
    procedure TearDownPackageSupport(EventPackage: TIdSipEventPackageClass);
    function  ThirdLastEventData: TIdEventData;
{
    procedure ReceiveReInvite;
}
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure OnEvent(Stack: TIdSipStackInterface;
                      Event: Cardinal;
                      Data:  TIdEventData);

    property Intf: TIdSipStackInterface read fIntf write fIntf;
  published
    procedure TestAcceptCall;
    procedure TestAcceptCallWithInvalidHandle;
    procedure TestAcceptCallWithNoExistentHandle;
    procedure TestEndedSession;
    procedure TestEstablishedSessionInboundCall;
    procedure TestEstablishedSessionOutboundCall;
    procedure TestHangUp;
    procedure TestHangUpWithInvalidHandle;
    procedure TestHangUpWithNonExistentHandle;
    procedure TestInboundCall;
    procedure TestMakeCall;
    procedure TestMakeCallMalformedAddress;
    procedure TestMakeRegistration;
    procedure TestMakeSubscription;
    procedure TestMakeSubscriptionMalformedTarget;
    procedure TestMakeSubscriptionNoSubscribeSupport;
{
    procedure TestModifyCall;
}
    procedure TestModifyCallWithInvalidHandle;
    procedure TestModifyCallWithNonExistentHandle;
    procedure TestNetworkFailure;
    procedure TestOutboundCall;
    procedure TestRedirectCall;
    procedure TestRedirectCallWithInvalidHandle;
    procedure TestRedirectCallWithNonExistentHandle;
    procedure TestRejectCall;
    procedure TestRejectCallWithInvalidHandle;
    procedure TestRejectCallWithNonExistentHandle;
    procedure TestRegistrationFails;
    procedure TestRegistrationFailsWithRetry;
    procedure TestResubscription;
    procedure TestSendNonExistentHandle;
//    procedure TestSessionModifiedByRemoteSide;
    procedure TestStackListensToSubscribeModule;
    procedure TestStackListensToSubscribeModuleAfterReconfigure;
  end;

  TestTIdSipStackInterfaceRegistry = class(TTestCase)
  private
    Configuration: TStrings;
    Timer:         TIdTimerQueue;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStackInterfacesAddToRegistryAutomatically;
    procedure TestStackInterfacesGetUniqueIDs;
    procedure TestStackInterfacesAutomaticallyUnregister;
  end;

  TestTIdEventData = class(TTestCase)
  private
    Data: TIdEventData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdInformationalData = class(TTestCase)
  private
    Data: TIdInformationalData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdAuthenticationChallengeData = class(TTestCase)
  private
    Data: TIdAuthenticationChallengeData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdDebugData = class(TTestCase)
  private
    Data: TIdDebugData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdDebugMessageData = class(TTestCase)
  private
    Data: TIdDebugMessageData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestAsString;
  end;

  TestTIdDebugDroppedMessageData = class(TTestCase)
  private
    Data: TIdDebugDroppedMessageData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestAsString;
  end;

  TestTIdDebugReceiveMessageData = class(TTestCase)
  private
    Data: TIdDebugReceiveMessageData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestAsString;
  end;

  TestTIdDebugSendMessageData = class(TTestCase)
  private
    Data: TIdDebugSendMessageData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdDebugTransportExceptionData = class(TTestCase)
  private
    Data: TIdDebugTransportExceptionData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdDebugTransportRejectedMessageData = class(TTestCase)
  private
    Data: TIdDebugTransportRejectedMessageData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdFailData = class(TTestCase)
  private
    Data: TIdFailData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdCallEndedData = class(TTestCase)
  private
    Data: TIdCallEndedData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdRegistrationData = class(TTestCase)
  private
    Data: TIdRegistrationData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestSetContacts;
  end;

  TestTIdFailedRegistrationData = class(TTestCase)
  private
    Data: TIdFailedRegistrationData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdSessionData = class(TTestCase)
  private
    Data: TIdSessionData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestSetRemotePartyStripsTagParam;
  end;

  TestTIdSessionProgressData = class(TTestCase)
  private
    Data: TIdSessionProgressData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdSubscriptionRequestData = class(TTestCase)
  private
    Data: TIdSubscriptionRequestData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdResubscriptionData = class(TTestCase)
  private
    Data: TIdSubscriptionData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdSessionReferralData = class(TTestCase)
  private
    Data: TIdSessionReferralData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdSubscriptionNotifyData = class(TTestCase)
  private
    Data: TIdSubscriptionNotifyData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdFailedSubscriptionData = class(TTestCase)
  private
    Data:         TIdFailedSubscriptionData;
    FailResponse: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCopy;
  end;

  TestTIdSipStackReconfigureStackInterfaceWait = class(TTestCase)
  private
    Conf:       TStrings;
    Stack:      TIdSipStackInterface;
    TimerQueue: TIdTimerQueue;
    Wait:       TIdSipStackReconfigureStackInterfaceWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckUdpServerOnPort(const Host: String;
                                   Port: Cardinal;
                                   const Msg: String);
  published
    procedure TestSetConfiguration;
    procedure TestTriggerStartsTransports;
  end;

  // I provide a message queue to which a StackInterface can send messages. Then
  // I route them back to a test case.
  TStackWindow = class(TCustomForm)
  private
    fTestCase: TestTIdSipStackInterface;

    function  IsStackMessage(Msg: TMessage): Boolean;
    procedure NotifyTestCase(Msg: TIdSipEventMessage);
  public
    constructor CreateNew(AOwner: TComponent; TestCase: TestTIdSipStackInterface); reintroduce;

    procedure DefaultHandler(var Message); override;

    property TestCase: TestTIdSipStackInterface read fTestCase;
  end;

const
  DummySdp = 'v=0'#13#10
           + 'o=sc 1105373135 1105373135 IN IP4 %s'#13#10
           + 's=Dummy on hold SDP'#13#10
           + 'c=IN IP4 0.0.0.0'#13#10
           + 'm=audio 65534 RTP/AVP 0'#13#10
           + 'a=rtpmap:0 PCMU/8000'#13#10
           + 'a=recvonly'#13#10;


implementation

uses
  IdRandom, IdSimpleParser, IdSipCore, IdSipLocation, IdSipTransport,
  IdSipUdpTransport, IdSocketHandle, IdUdpServer, SysUtils;

type
  TIdSipTestPackage = class(TIdSipEventPackage)
  public
    class function EventPackage: String; override;
  end;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipStackInterface unit tests');
  Result.AddTest(TestTIdSipStackInterfaceCreation.Suite);
  Result.AddTest(TestTIdSipStackInterface.Suite);
  Result.AddTest(TestTIdSipStackInterfaceRegistry.Suite);
  Result.AddTest(TestTIdEventData.Suite);
  Result.AddTest(TestTIdInformationalData.Suite);
  Result.AddTest(TestTIdAuthenticationChallengeData.Suite);
  Result.AddTest(TestTIdDebugData.Suite);
  Result.AddTest(TestTIdDebugMessageData.Suite);
  Result.AddTest(TestTIdDebugDroppedMessageData.Suite);
  Result.AddTest(TestTIdDebugReceiveMessageData.Suite);
  Result.AddTest(TestTIdDebugSendMessageData.Suite);
  Result.AddTest(TestTIdDebugTransportExceptionData.Suite);
  Result.AddTest(TestTIdDebugTransportRejectedMessageData.Suite);
  Result.AddTest(TestTIdFailData.Suite);
  Result.AddTest(TestTIdCallEndedData.Suite);
  Result.AddTest(TestTIdRegistrationData.Suite);
  Result.AddTest(TestTIdFailedRegistrationData.Suite);
  Result.AddTest(TestTIdSessionProgressData.Suite);
  Result.AddTest(TestTIdSessionData.Suite);
  Result.AddTest(TestTIdSubscriptionRequestData.Suite);
  Result.AddTest(TestTIdResubscriptionData.Suite);
  Result.AddTest(TestTIdSessionReferralData.Suite);
  Result.AddTest(TestTIdSubscriptionNotifyData.Suite);
  Result.AddTest(TestTIdFailedSubscriptionData.Suite);
  Result.AddTest(TestTIdSipStackReconfigureStackInterfaceWait.Suite);
end;

//******************************************************************************
//* TIdSipTestPackage                                                          *
//******************************************************************************
//* TIdSipTestPackage Public methods *******************************************

class function TIdSipTestPackage.EventPackage: String;
begin
  Result := 'foo';
end;

//******************************************************************************
//* TestTIdSipStackInterfaceCreation                                           *
//******************************************************************************
//* TestTIdSipStackInterfaceCreation Public methods ****************************

procedure TestTIdSipStackInterfaceCreation.TestCreateStackWithNoSubscribeSupport;
var
  EmptyConf: TStrings;
  Stack:     TIdSipStackInterface;
begin
  EmptyConf := TStringList.Create;
  try
    Stack := TIdSipStackInterface.Create(0, TIdDebugTimerQueue.Create(true), EmptyConf);
    try
      // This test tries catches a (now squashed) bug: when no subscribe module
      // was attached to the stack we'd get an Invalid Cast exception.
    finally
      Stack.Free;
    end;
  finally
    EmptyConf.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipStackInterface                                                   *
//******************************************************************************
//* TestTIdSipStackInterface Public methods ************************************

procedure TestTIdSipStackInterface.SetUp;
var
  BasicConf: TStrings;
  Conf:      TIdSipStackConfigurator;
begin
  inherited SetUp;

  TIdSipTransportRegistry.RegisterTransportType(UdpTransport, TIdSipMockUDPTransport);
  TIdSipEventPackageRegistry.RegisterEvent(TIdSipTestPackage);

  Self.DataList    := TObjectList.Create(true);
  Self.Destination := TIdSipToHeader.Create;
  Self.Requests    := TIdSipRequestList.Create;
  Self.Responses   := TIdSipResponseList.Create;

  Self.TimerQueue := TIdDebugTimerQueue.Create(true);

  Self.TargetAddress := '10.0.0.8';
  Self.TargetPort    := 5060;
  BasicConf := TStringList.Create;
  try
    BasicConf.Add('Listen: UDP ' + Self.TargetAddress + ':' + IntToStr(Self.TargetPort));
    BasicConf.Add('NameServer: MOCK');
    BasicConf.Add('Contact: sip:' + Self.TargetAddress + ':' + IntToStr(Self.TargetPort));
    BasicConf.Add('From: sip:' + Self.TargetAddress + ':' + IntToStr(Self.TargetPort));
    BasicConf.Add('SupportEvent: ' + TIdSipTestPackage.EventPackage);

    Conf := TIdSipStackConfigurator.Create;
    try
      Self.RemoteUA := Conf.CreateUserAgent(BasicConf, Self.TimerQueue);
      Self.RemoteUA.InviteModule.AddListener(Self);
    finally
      Conf.Free;
    end;
  finally
    BasicConf.Free;
  end;

  Self.UI := TStackWindow.CreateNew(nil, Self);

  Self.LocalAddress := '10.0.0.6';
  Self.LocalPort    := 5060;
  BasicConf := TStringList.Create;
  try
    BasicConf.Add('Listen: UDP ' + Self.LocalAddress + ':' + IntToStr(Self.LocalPort));
    BasicConf.Add('NameServer: MOCK;ReturnOnlySpecifiedRecords');
    BasicConf.Add('Contact: sip:foo@' + Self.LocalAddress + ':' + IntToStr(Self.LocalPort));
    BasicConf.Add('From: sip:foo@' + Self.LocalAddress + ':' + IntToStr(Self.LocalPort));

    Self.Intf := TIdSipStackInterface.Create(Self.UI.Handle, Self.TimerQueue, BasicConf);
  finally
    BasicConf.Free;
  end;
  Self.Intf.Resume;
  // Clear the "pending" CM_DEBUG_STACK_STARTED
  Application.ProcessMessages;

  Self.MockTransport := TIdSipDebugTransportRegistry.TransportAt(TIdSipDebugTransportRegistry.TransportCount - 1) as TIdSipMockTransport;

  // The registrar URI MUST NOT be that of RemoteUA, because RemoteUA will not
  // process REGISTER messages.
  Self.Registrar := TIdSipUri.Create;
  Self.Registrar.Uri := Self.RemoteUA.From.Address.AsString;
  Self.Registrar.Host := TIdIPAddressParser.IncIPAddress(Self.Registrar.Host);

  Self.Destination.Value := Self.RemoteUA.From.Value;
  Self.LocalOffer        := Format(DummySdp, [Self.LocalAddress]);
  Self.LocalMimeType     := 'application/sdp';
  Self.RemoteOffer       := Format(DummySdp, [Self.TargetAddress]);
  Self.RemoteMimeType    := 'application/sdp';
end;

procedure TestTIdSipStackInterface.TearDown;
begin
  // Process any outstanding notifications from the StackWindow
  Application.ProcessMessages;
  Self.UI.Release;
  Self.Registrar.Free;
  Self.Intf.Free;
  Self.TimerQueue.Terminate;
  Self.Responses.Free;
  Self.Requests.Free;
  Self.RemoteUA.Free;
  Self.Destination.Free;
  Self.DataList.Free;

  TIdSipEventPackageRegistry.UnregisterEvent(TIdSipTestPackage);
  TIdSipTransportRegistry.UnregisterTransportType(UdpTransport);

  inherited TearDown;
end;

procedure TestTIdSipStackInterface.OnEvent(Stack: TIdSipStackInterface;
                                           Event: Cardinal;
                                           Data:  TIdEventData);
begin
  Self.DataList.Add(Data);
end;

//* TestTIdSipStackInterface Private methods ***********************************

procedure TestTIdSipStackInterface.AddSubscribeSupport(Stack: TIdSipStackInterface);
var
  NewConf: TStrings;
begin
  NewConf := TStringList.Create;
  try
    NewConf.Add('SupportEvent: ' + TIdSipTestPackage.EventPackage);
    Stack.ReconfigureStack(NewConf);
    Self.TimerQueue.TriggerAllEventsOfType(TIdSipStackReconfigureStackInterfaceWait);
  finally
    NewConf.Free;
  end;
end;

procedure TestTIdSipStackInterface.CheckNotificationReceived(EventType: TIdEventDataClass; Msg: String);
var
  Found: Boolean;
  I: Integer;
begin
  Found := false;
  I     := 0;
  while (I < Self.DataList.Count) and not Found do begin
    Found := Self.EventAt(I) is EventType;
    Inc(I);
  end;

  if not Found then Fail(Msg);
end;

procedure TestTIdSipStackInterface.CheckRequestSent(Msg: String);
begin
  Check(Self.SentRequestCount < Self.MockTransport.SentRequestCount, Msg);
end;

procedure TestTIdSipStackInterface.CheckResponseSent(Msg: String);
begin
  Check(Self.SentResponseCount < Self.MockTransport.SentResponseCount, Msg);
end;
{
function TestTIdSipStackInterface.CreateBindings: TIdSipContacts;
begin
  Result := TIdSipContacts.Create;

  Result.Add(ContactHeaderFull).Value := 'sip:case@fried.neurons.org';
  Result.Add(ContactHeaderFull).Value := 'sip:wintermute@tessier-ashpool.co.luna';
end;

function TestTIdSipStackInterface.CreateRemoteBye(LocalDialog: TIdSipDialog): TIdSipRequest;
begin
  Result := Self.RemoteUA.CreateBye(LocalDialog);
  try
    Result.ToHeader.Tag := LocalDialog.ID.LocalTag;
    Result.From.Tag     := LocalDialog.ID.RemoteTag;
  except
    FreeAndNil(Result);

    raise;
  end;
end;
}
function TestTIdSipStackInterface.CreateRemoteInvite: TIdSipRequest;
begin
  Result := Self.RemoteUA.InviteModule.CreateInvite(Self.Destination, '', '');
end;

function TestTIdSipStackInterface.CreateRemoteNotify(RemoteDialog: TIdSipDialog; Subscribe: TIdSipRequest): TIdSipRequest;
var
  SubMod: TIdSipSubscribeModule;
begin
  SubMod := Self.RemoteUA.ModuleFor(TIdSipSubscribeModule) as TIdSipSubscribeModule;

  Result := SubMod.CreateNotify(RemoteDialog, Subscribe, SubscriptionSubstateActive);
end;

function TestTIdSipStackInterface.CreateRemoteOk(Request: TIdSipRequest): TIdSipResponse;
begin
  Result := TIdSipResponse.InResponseTo(Request, SIPOK);
  try
    Result.FirstContact.Value := 'sip:' + Self.TargetAddress + ':' + IntToStr(Self.TargetPort);
    Result.ToHeader.Tag := GRandomNumber.NextSipUserAgentTag;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TestTIdSipStackInterface.EstablishCall: TIdSipHandle;
begin
  Result := Self.Intf.MakeCall(Self.Destination,
                               Self.LocalOffer,
                               Self.LocalMimeType);

  Self.MarkSentRequestCount;
  Self.Intf.Send(Result);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  CheckRequestSent('No INVITE sent in EstablishCall');

  Self.ReceiveOk(Self.LastSentRequest, Self.RemoteOffer, Self.RemoteMimeType);
  Application.ProcessMessages;
end;

function TestTIdSipStackInterface.EventAt(Index: Integer): TIdEventData;
begin
  Result := Self.DataList[Index] as TIdEventData;
end;

function TestTIdSipStackInterface.LastEventOfType(EventType: TIdEventDataClass): TIdEventData;
var
  Found: Boolean;
  I:     Integer;
begin
  Result := nil;

  Found := false;
  I     := Self.DataList.Count - 1;
  while (I > 0) and not Found do begin
    Found := Self.EventAt(I) is EventType;

    if not Found then Dec(I);
  end;

  if Found then
    Result := Self.EventAt(I)
  else
    Fail('No event of type ' + EventType.ClassName + ' found');
end;

function TestTIdSipStackInterface.LastSentRequest: TIdSipRequest;
begin
  Result := Self.MockTransport.LastRequest;
end;

function TestTIdSipStackInterface.LastSentResponse: TIdSipResponse;
begin
  Result := Self.MockTransport.LastResponse;
end;

procedure TestTIdSipStackInterface.MarkSentRequestCount;
begin
  Self.SentRequestCount := Self.MockTransport.SentRequestCount;
end;

procedure TestTIdSipStackInterface.MarkSentResponseCount;
begin
  Self.SentResponseCount := Self.MockTransport.SentResponseCount;
end;

procedure TestTIdSipStackInterface.OnInboundCall(UserAgent: TIdSipInviteModule;
                                                 Session: TIdSipInboundSession);
begin
  Self.RemoteSession := Session;
end;
{
procedure TestTIdSipStackInterface.LogSentMessage(Msg: TIdSipMessage);
begin
  if Msg.IsRequest then begin
    Self.Requests.AddCopy(Msg as TIdSipRequest);
  end
  else begin
    Self.Responses.AddCopy(Msg as TIdSipResponse);
  end;
end;
}
procedure TestTIdSipStackInterface.ReceiveAck;
var
  Ack: TIdSipRequest;
begin
  Ack := Self.MockTransport.LastRequest.AckFor(Self.LastSentResponse);
  try
    Self.ReceiveRequest(Ack);
  finally
    Ack.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveBusyHereFromRegistrar(Register: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Register);
  try
    Response.StatusCode := SIPBusyHere;
    Response.Contacts.Clear;

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;
{
procedure TestTIdSipStackInterface.ReceiveBye(LocalDialog: TIdSipDialog);
var
  Bye: TIdSipRequest;
begin
  Bye := Self.CreateRemoteBye(LocalDialog);
  try
    Self.ReceiveRequest(Bye);
  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveByeForOutboundCall;
var
  LocalDlg: TIdSipDialog;
begin
  LocalDlg := TIdSipDialog.CreateOutboundDialog(Self.LastSentRequest,
                                                Self.LastResponse,
                                                false);
  try
    Self.ReceiveBye(LocalDlg);
  finally
    LocalDlg.Free;
  end;
end;
}
procedure TestTIdSipStackInterface.ReceiveIntervalTooBrief(Register: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Register);
  try
    Response.StatusCode := SIPIntervalTooBrief;
    Response.AddHeader(MinExpiresHeader).Value := '1000';
    Response.Contacts.Clear;
    Response.Contacts.Add(Register.Contacts);

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveInvite;
begin
  Self.ReceiveInviteWithOffer('', '');
end;

procedure TestTIdSipStackInterface.ReceiveInviteWithOffer(const Offer: String;
                                                          const MimeType: String);
var
  Invite: TIdSipRequest;
begin
  Invite := Self.CreateRemoteInvite;
  try
    Invite.Body          := Offer;
    Invite.ContentLength := Length(Invite.Body);
    Invite.ContentType   := MimeType;

    Self.ReceiveRequest(Invite);
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveNotify(RemoteDialog: TIdSipDialog; Subscribe: TIdSipRequest);
var
  Notify: TIdSipRequest;
begin
  Notify := Self.CreateRemoteNotify(RemoteDialog, Subscribe);
  try
    Self.ReceiveRequest(Notify);
  finally
    Notify.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveTerminatingNotify(RemoteDialog: TIdSipDialog; Subscribe: TIdSipRequest; Reason: String);
var
  Notify: TIdSipRequest;
begin
  Notify := Self.CreateRemoteNotify(RemoteDialog, Subscribe);
  try
    Notify.SubscriptionState.SubState := SubscriptionSubstateTerminated;
    Notify.SubscriptionState.Reason   := Reason;

    Self.ReceiveRequest(Notify);
  finally
    Notify.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveOk(Request: TIdSipRequest; Offer: String = ''; MimeType: String = '');
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Request);
  try
    if (Offer <> '') then begin
      Response.Body := Offer;
      Response.ContentLength := Length(Offer);
    end;
    
    if (MimeType <> '') then
      Response.ContentType := MimeType;

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;
{
procedure TestTIdSipStackInterface.ReceiveOkWithContacts(Register: TIdSipRequest;
                                                         Contacts: TIdSipContacts);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Register);
  try
    Response.Contacts.Clear;
    Response.Contacts.Add(Contacts);
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveOkWithOffer(Invite: TIdSipRequest;
                                                      const Offer: String;
                                                      const MimeType: String);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Invite);
  try
    Response.Body          := Offer;
    Response.ContentLength := Length(Response.Body);
    Response.ContentType   := MimeType;
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;
}
procedure TestTIdSipStackInterface.ReceiveRequest(Request: TIdSipRequest);
var
  Target: TIdSipConnectionBindings;
begin
  Self.Requests.AddCopy(Request);

  Target := TIdSipConnectionBindings.Create;
  try
    Target.LocalIP   := Self.MockTransport.Bindings[0].IP;
    Target.LocalPort := Self.MockTransport.Bindings[0].Port;
    Target.PeerIP    := Self.TargetAddress;
    Target.PeerPort  := Self.TargetPort;
    Target.Transport := Self.MockTransport.GetTransportType;

    Self.MockTransport.ReceiveRequest(Request, Target);
  finally
    Target.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveResponse(Response: TIdSipResponse);
var
  Target: TIdSipConnectionBindings;
begin
  Self.Responses.AddCopy(Response);

  Target := TIdSipConnectionBindings.Create;
  try
    Target.LocalIP   := Self.MockTransport.Bindings[0].IP;
    Target.LocalPort := Self.MockTransport.Bindings[0].Port;
    Target.PeerIP    := Self.TargetAddress;
    Target.PeerPort  := Self.TargetPort;
    Target.Transport := Self.MockTransport.GetTransportType;

    Self.MockTransport.ReceiveResponse(Response, Target);
  finally
    Target.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveSubscribe(EventPackage: String);
var
  LocalFrom: TIdSipAddressHeader;
  Subscribe: TIdSipRequest;
  SubMod:    TIdSipSubscribeModule;
begin
  Check(Self.RemoteUA.UsesModule(TIdSipSubscribeModule),
        'RemoteUA must support SUBSCRIBE requests for this test');

  SubMod := Self.RemoteUA.ModuleFor(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  Check(Assigned(SubMod.Package(EventPackage)),
        'RemoteUA must support the ' + EventPackage + ' event package');

  LocalFrom := TIdSipFromHeader.Create;
  try
    LocalFrom.Value := 'sip:' + Self.LocalAddress + ':' + IntToStr(Self.LocalPort);

    Subscribe := SubMod.CreateSubscribe(LocalFrom, EventPackage);
    try
      Self.ReceiveRequest(Subscribe);
    finally
      Subscribe.Free;
    end;
  finally
    LocalFrom.Free;
  end;
end;

function TestTIdSipStackInterface.SecondLastEventData: TIdEventData;
begin
  Result := Self.DataList[Self.DataList.Count - 2] as TIdEventData;
end;

procedure TestTIdSipStackInterface.SetUpPackageSupport(EventPackage: TIdSipEventPackageClass);
var
  SubMod: TIdSipSubscribeModule;
begin
  TIdSipEventPackageRegistry.RegisterEvent(EventPackage);

  Self.RemoteUA.AddModule(TIdSipSubscribeModule);
  SubMod := Self.RemoteUA.ModuleFor(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  SubMod.AddPackage(EventPackage);
end;

procedure TestTIdSipStackInterface.TearDownPackageSupport(EventPackage: TIdSipEventPackageClass);
begin
  TIdSipEventPackageRegistry.UnregisterEvent(EventPackage);
end;

function TestTIdSipStackInterface.ThirdLastEventData: TIdEventData;
begin
  Result := Self.DataList[Self.DataList.Count - 3] as TIdEventData;
end;
{
procedure TestTIdSipStackInterface.ReceiveReInvite;
var
  ReInvite: TIdSipRequest;
  Temp:     String;
begin
  // Precondition: A full call has been established, so LastSentRequest and
  // LastResponse both point to meaningful messages.

  ReInvite := TIdSipRequest.Create;
  try
    ReInvite.Assign(Self.LastSentRequest);

    ReInvite.CSeq.Increment;
    ReInvite.LastHop.Branch := ReInvite.LastHop.Branch + '1';
    ReInvite.ToHeader.Tag := Self.LastResponse.ToHeader.Tag;

    // This message comes FROM the network so its From/To tags are the reverse
    // of the outbound INVITE's
    Temp                  := ReInvite.From.Tag;
    ReInvite.From.Tag     := ReInvite.ToHeader.Tag;
    ReInvite.ToHeader.Tag := Temp;

    Self.ReceiveRequest(ReInvite);
  finally
    ReInvite.Free;
  end;
end;
}
//* TestTIdSipStackInterface Published methods *********************************

procedure TestTIdSipStackInterface.TestAcceptCall;
begin
  Self.ReceiveInviteWithOffer(Self.RemoteOffer, Self.RemoteMimeType);
  Application.ProcessMessages;
  CheckNotificationReceived(TIdInboundCallData, 'No inbound call notification received');

  Self.MarkSentResponseCount;

  // Second last data because a CM_DEBUG_RECV_MSG always follows.
  Self.Intf.AnswerCall(Self.LastEventOfType(TIdInboundCallData).Handle, Self.LocalOffer, Self.LocalMimeType);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipSessionAcceptWait);
  CheckResponseSent('No response sent');

  CheckEquals(SIPOK, Self.LastSentResponse.StatusCode, 'Unexpected response sent');
  CheckEquals(Self.LocalOffer, Self.LastSentResponse.Body, 'Local offer not used');
  Check(Self.LastSentResponse.HasHeader(ContentTypeHeaderFull), 'Local MIME type not present');
  CheckEquals(Self.LocalMimeType, Self.LastSentResponse.ContentType, 'Local MIME type not used');
end;

procedure TestTIdSipStackInterface.TestAcceptCallWithInvalidHandle;
var
  H: TIdSipHandle;
begin
  H := Self.Intf.MakeCall(Self.Destination, '', '');

  try
    // Of course, you can't answer an outbound call.
    Self.Intf.AnswerCall(H, '', '');

    Fail('No exception raised for an invalid handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestAcceptCallWithNoExistentHandle;
const
  ArbValue = 42;
begin
  try
    Self.Intf.AnswerCall(ArbValue, '', '');
    Fail('No exception raised for a non-existent handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestEndedSession;
var
  Data: TIdCallEndedData;
  H:    TIdSipHandle;
begin
  H := Self.EstablishCall;

  Self.MarkSentRequestCount;
  Self.Intf.HangUp(H);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionTerminateWait);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  Application.ProcessMessages;
  CheckRequestSent('No BYE sent');
  CheckEquals(MethodBye, Self.MockTransport.LastRequest.Method, 'Unexpected request sent');

  CheckNotificationReceived(TIdCallEndedData, 'Call hangup notification not sent');

  Data := Self.LastEventOfType(TIdCallEndedData) as TIdCallEndedData;
  CheckEquals(IntToHex(H, 8), IntToHex(Data.Handle, 8), 'Handle');
  CheckEquals(0, Data.ErrorCode, 'ErrorCode');
end;

procedure TestTIdSipStackInterface.TestEstablishedSessionInboundCall;
var
  InboundCallData: TIdInboundCallData;
begin
  Self.ReceiveInvite;
  Application.ProcessMessages;
  CheckNotificationReceived(TIdInboundCallData, 'No inbound call notification received?');
  InboundCallData := Self.LastEventOfType(TIdInboundCallData) as TIdInboundCallData;

  Self.Intf.AnswerCall(InboundCallData.Handle, Self.LocalOffer, Self.LocalMimeType);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipSessionAcceptWait);

  Self.ReceiveAck;
  Application.ProcessMessages;

  CheckNotificationReceived(TIdEstablishedSessionData, 'No established session notification received');
end;

procedure TestTIdSipStackInterface.TestEstablishedSessionOutboundCall;
var
  Call: TIdSipHandle;
begin
  Call := Self.Intf.MakeCall(Self.Destination, Self.LocalOffer, Self.LocalMimeType);
  Self.Intf.Send(Call);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  // Receive the CM_DEBUG_SEND_MSG for the INVITE
  Application.ProcessMessages;

  Self.ReceiveOk(Self.LastSentRequest);
  Application.ProcessMessages;
  CheckNotificationReceived(TIdEstablishedSessionData, 'No established session notification received');
  CheckEquals(TIdEstablishedSessionData.ClassName,
              Self.ThirdLastEventData.ClassName,
              'Unexpected notification');
end;

procedure TestTIdSipStackInterface.TestHangUp;
var
  Call: TIdSipHandle;
begin
  Call := Self.EstablishCall;

  Self.MarkSentRequestCount;
  Self.Intf.HangUp(Call);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionTerminateWait);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  Application.ProcessMessages;
  CheckNotificationReceived(TIdCallEndedData, 'No notification of end of call');
  CheckRequestSent('No BYE sent');
  CheckEquals(MethodBye, Self.LastSentRequest.Method, 'Unexpected request sent');
end;

procedure TestTIdSipStackInterface.TestHangUpWithInvalidHandle;
var
  R: TIdSipHandle;
begin
  R := Self.Intf.MakeRegistration(Self.Destination.Address);

  try
    // You can't, obviously, "hang up" a registration attempt.
    Self.Intf.HangUp(R);

    Fail('No exception raised for an invalid handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestHangUpWithNonExistentHandle;
const
  ArbValue = 42;
begin
  try
    Self.Intf.HangUp(ArbValue);
    Fail('No exception raised for a non-existent handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestInboundCall;
var
  Data: TIdInboundCallData;
begin
  Self.ReceiveInviteWithOffer(Self.RemoteOffer, Self.RemoteMimeType);
  Application.ProcessMessages;
  CheckNotificationReceived(TIdInboundCallData, 'No inbound call notification received');

  Data := Self.LastEventOfType(TIdInboundCallData) as TIdInboundCallData;
  Check(Data.Handle > 0, 'Invalid Action handle');
  CheckEquals(Self.RemoteOffer,            Data.RemoteSessionDescription, 'RemoteSessionDescription');
  CheckEquals(Self.RemoteMimeType,         Data.RemoteMimeType,           'RemoteMimeType');
  CheckEquals(Self.RemoteUA.Contact.Value, Data.RemoteContact.Value,      'RemoteContact');
  CheckEquals(Self.RemoteUA.From.Value,    Data.RemoteParty.Value,        'RemoteParty');
end;

procedure TestTIdSipStackInterface.TestMakeCall;
var
  Handle: TIdSipHandle;
begin
  Handle := Self.Intf.MakeCall(Self.Destination, '', '');

  Self.MarkSentRequestCount;
  Self.Intf.Send(Handle);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  CheckRequestSent('No request sent');
  CheckEquals(MethodInvite, Self.MockTransport.LastRequest.Method, 'Unexpected request sent');
end;

procedure TestTIdSipStackInterface.TestMakeCallMalformedAddress;
var
  Handle:           TIdSipHandle;
  MalformedAddress: TIdSipToHeader;
begin
  MalformedAddress := TIdSipToHeader.Create;
  try
    MalformedAddress.Address.Uri := 'sip:::1';
    Check(MalformedAddress.IsMalformed, 'Sanity check: the URI must be malformed');

    Handle := Self.Intf.MakeCall(MalformedAddress, '', '');
    CheckEquals(InvalidHandle, Handle, 'MakeCall didn''t return the invalid handle');
  finally
    MalformedAddress.Free;
  end;
end;

procedure TestTIdSipStackInterface.TestMakeRegistration;
var
  Handle: TIdSipHandle;
begin
  Handle := Self.Intf.MakeRegistration(Self.Registrar);

  Self.MarkSentRequestCount;
  Self.Intf.Send(Handle);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  CheckRequestSent('No request sent');
  CheckEquals(MethodRegister, Self.LastSentRequest.Method, 'Unexpected request sent');
end;

procedure TestTIdSipStackInterface.TestMakeSubscription;
var
  Handle: TIdSipHandle;
begin
  Self.AddSubscribeSupport(Self.Intf);

  // We try subscribe to something that isn't going to immediately reply.
  Self.Destination.Address.Host := TIdIPAddressParser.IncIPAddress(Self.TargetAddress);

  Handle := Self.Intf.MakeSubscription(Self.Destination, TIdSipTestPackage.EventPackage);

  Self.MarkSentRequestCount;
  Self.Intf.Send(Handle);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  CheckRequestSent('No request sent');
  CheckEquals(MethodSubscribe, Self.LastSentRequest.Method, 'Unexpected request sent');
end;

procedure TestTIdSipStackInterface.TestMakeSubscriptionMalformedTarget;
var
  Handle: TIdSipHandle;
begin
  Self.Destination.Address.Uri := 'sip:foo@@bar';
  Check(Self.Destination.IsMalformed, 'Destination not malformed');

  Handle := Self.Intf.MakeSubscription(Self.Destination, 'foo');
  CheckEquals(IntToHex(InvalidHandle, 8),
              IntToHex(Handle, 8),
              'Malformed Target should result in an InvalidHandle');
end;

procedure TestTIdSipStackInterface.TestMakeSubscriptionNoSubscribeSupport;
var
  Handle: TIdSipHandle;
begin
  Handle := Self.Intf.MakeSubscription(Self.Destination, TIdSipTestPackage.EventPackage);

  CheckEquals(IntToHex(InvalidHandle, 8),
              IntToHex(Handle, 8),
              'No SUBSCRIBE support should result in an InvalidHandle');
end;
{
procedure TestTIdSipStackInterface.TestModifyCall;
begin
  //  ---   INVITE   --->
  // <---   200 OK   ---
  //  ---    ACK     --->
  //  --- (re)INVITE --->
  // <---   200 OK   ---
  //  ---    ACK     --->

  Self.LocalMimeType := 'text/plain';
  Self.LocalOffer    := 'empty';
end;
}
procedure TestTIdSipStackInterface.TestModifyCallWithInvalidHandle;
var
  R: TIdSipHandle;
begin
  R := Self.Intf.MakeRegistration(Self.Destination.Address);

  try
    // You can't, obviously, "modify" a registration attempt.
    Self.Intf.ModifyCall(R, Self.LocalOffer, Self.LocalMimeType);

    Fail('No exception raised for an invalid handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestModifyCallWithNonExistentHandle;
const
  ArbValue = 42;
begin
  try
    Self.Intf.ModifyCall(ArbValue, '', '');
    Fail('No exception raised for a non-existent handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestNetworkFailure;
var
  Call:     TIdSipHandle;
  FailData: TIdNetworkFailureData;
begin
  Self.Destination.Address.Host := 'does.not.exist.com';
  Call := Self.Intf.MakeCall(Self.Destination, '', '');
  Check(Call > 0, 'Invalid handle');
  Self.Intf.Send(Call);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  Application.ProcessMessages;

  CheckNotificationReceived(TIdNetworkFailureData, 'Network failure notification not received');

  FailData := Self.LastEventOfType(TIdNetworkFailureData) as TIdNetworkFailureData;
//  CheckEquals(Format(OutboundActionFailed, [MethodInvite, Format(RSNoLocationSucceeded, [Self.Destination.Address.AsString])]), FailData.Reason, 'Reason');
  CheckEquals(NoLocationFound, FailData.ErrorCode, 'ErrorCode');
  CheckEquals(IntToHex(Call, 8), IntToHex(FailData.Handle, 8), 'Handle');
end;

procedure TestTIdSipStackInterface.TestOutboundCall;
var
  H:           TIdSipHandle;
  SessionData: TIdEstablishedSessionData;
begin
  H := Self.Intf.MakeCall(Self.Destination,
                          Self.LocalOffer,
                          Self.LocalMimeType);
  Self.Intf.Send(H);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  Application.ProcessMessages;

  Check(Assigned(Self.RemoteSession), 'RemoteSession never assigned: RemoteUA didn''t receive INVITE?');
  Self.RemoteSession.AcceptCall(Self.RemoteOffer, Self.RemoteMimeType);
  Application.ProcessMessages;

  CheckNotificationReceived(TIdEstablishedSessionData, 'No established session notification');

  SessionData := Self.LastEventOfType(TIdEstablishedSessionData) as TIdEstablishedSessionData;

  CheckEquals(Self.LocalOffer,
              SessionData.LocalSessionDescription,
              'LocalSessionDescription');
  CheckEquals(Self.LocalMimeType,
              SessionData.LocalMimeType,
              'LocalMimeType');
  CheckEquals(Self.RemoteOffer,
              SessionData.RemoteSessionDescription,
              'RemoteSessionDescription');
  CheckEquals(Self.RemoteMimeType,
              SessionData.RemoteMimeType,
              'RemoteMimeType');
end;

procedure TestTIdSipStackInterface.TestRedirectCall;
var
  NewTarget: TIdSipAddressHeader;
begin
  Self.ReceiveInviteWithOffer(Self.RemoteOffer, Self.RemoteMimeType);
  Application.ProcessMessages;
  CheckNotificationReceived(TIdInboundCallData, 'No inbound call notification received');

  Check(Self.LastEventOfType(TIdInboundCallData).Handle > 0, 'Invalid Action handle');

  NewTarget := TIdSipContactHeader.Create;
  try
    NewTarget.Value := 'sip:' + TIdIPAddressParser.IncIPAddress(Self.TargetAddress);

    Self.MarkSentResponseCount;
    Self.Intf.RedirectCall(Self.SecondLastEventData.Handle, NewTarget);
    Self.TimerQueue.TriggerAllEventsOfType(TIdSipSessionRedirectWait);
    CheckResponseSent('No response sent');
    CheckEquals(SIPMovedTemporarily, Self.LastSentResponse.StatusCode, 'Unexpected response sent');
    CheckEquals(NewTarget.AsString,
                Self.LastSentResponse.FirstContact.AsString,
                'Unexpected redirection target');
  finally
    NewTarget.Free;
  end;
end;

procedure TestTIdSipStackInterface.TestRedirectCallWithInvalidHandle;
var
  R: TIdSipHandle;
begin
  R := Self.Intf.MakeRegistration(Self.Destination.Address);

  try
    // You can't, obviously, "Redirect" a registration attempt.
    Self.Intf.RedirectCall(R, Self.Destination);

    Fail('No exception raised for an invalid handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestRedirectCallWithNonExistentHandle;
const
  ArbValue = 42;
begin
  try
    Self.Intf.RedirectCall(ArbValue, Self.Destination);
    Fail('No exception raised for a non-existent handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestRejectCall;
begin
  Self.ReceiveInviteWithOffer(Self.RemoteOffer, Self.RemoteMimeType);
  Application.ProcessMessages;
  CheckNotificationReceived(TIdInboundCallData, 'No inbound call notification received');

  Check(Self.LastEventOfType(TIdInboundCallData).Handle > 0, 'Invalid Action handle');

  Self.MarkSentResponseCount;
  Self.Intf.RejectCall(Self.SecondLastEventData.Handle);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipSessionRejectWait);
  CheckResponseSent('No response sent');
  CheckEquals(SIPBusyHere, Self.LastSentResponse.StatusCode, 'Unexpected response sent');
end;

procedure TestTIdSipStackInterface.TestRejectCallWithInvalidHandle;
var
  R: TIdSipHandle;
begin
  R := Self.Intf.MakeRegistration(Self.Destination.Address);

  try
    // You can't, obviously, "reject" a registration attempt.
    Self.Intf.RejectCall(R);

    Fail('No exception raised for an invalid handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestRejectCallWithNonExistentHandle;
const
  ArbValue = 42;
begin
  try
    Self.Intf.RejectCall(ArbValue);
    Fail('No exception raised for a non-existent handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestRegistrationFails;
var
  FailData: TIdFailData;
  Reg:      TIdSipHandle;
begin
  //  ---    REGISTER   --->
  // <--- 486 Busy Here ---
  Reg := Self.Intf.MakeRegistration(Self.Registrar);
  Self.Intf.Send(Reg);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);

  Self.ReceiveBusyHereFromRegistrar(Self.LastSentRequest);
  Application.ProcessMessages;
  CheckNotificationReceived(TIdFailData, 'No fail notification received');

  FailData := Self.LastEventOfType(TIdFailData) as TIdFailData;
  CheckEquals(IntToHex(Reg, 8), IntToHex(FailData.Handle, 8), 'Incorrect handle');
  CheckEquals(Self.LastSentResponse.StatusCode, FailData.ErrorCode, 'Incorrect ErrorCode');
  CheckEquals(Self.LastSentResponse.StatusText, FailData.Reason, 'Incorrect Reason');
end;

procedure TestTIdSipStackInterface.TestRegistrationFailsWithRetry;
var
  Reg: TIdSipHandle;
begin
  //  ---        REGISTER        --->
  // <--- 423 Interval Too Brief ---
  //  ---        REGISTER        --->
  // <---         200 OK         ---

  Reg := Self.Intf.MakeRegistration(Self.Registrar);
  Self.Intf.Send(Reg);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);

  Self.MarkSentRequestCount;
  Self.ReceiveIntervalTooBrief(Self.LastSentRequest);
  CheckRequestSent('No REGISTER resent');
  CheckEquals(MethodRegister, Self.LastSentRequest.Method, 'Unexpected request sent');

  Self.ReceiveOk(Self.LastSentRequest);
  Application.ProcessMessages;

  CheckNotificationReceived(TIdRegistrationData, 'No registration success notification received');
end;

procedure TestTIdSipStackInterface.TestResubscription;
var
  OK:           TIdSipResponse;
  RemoteDialog: TIdSipDialog;
  Sub:          TIdSipHandle;
  Subscribe:    TIdSipRequest;
begin
  //  ---              SUBSCRIBE              --->
  // <---               200 OK                ---
  // <---               NOTIFY                ---
  //  ---               200 OK                --->
  // <--- NOTIFY (terminated; reason=timeout) ---
  //  ---               200 OK                --->
  //  ---              SUBSCRIBE              --->
  // <---               200 OK                ---

  Self.AddSubscribeSupport(Self.Intf);

  Self.Destination.Address.Host := TIdIPAddressParser.IncIPAddress(Self.Destination.Address.Host);
  Sub := Self.Intf.MakeSubscription(Self.Destination, TIdSipTestPackage.EventPackage);
  Self.Intf.Send(Sub);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);

  Subscribe := TIdSipRequest.Create;
  try
    Subscribe.Assign(Self.LastSentRequest);

    OK := TIdSipResponse.InResponseTo(Subscribe, SIPOK);
    try
      RemoteDialog := TIdSipDialog.CreateInboundDialog(Subscribe, OK, false);
      try
        RemoteDialog.ReceiveRequest(Subscribe);

        Self.ReceiveNotify(RemoteDialog, Subscribe);

        Self.ReceiveTerminatingNotify(RemoteDialog, Subscribe, EventReasonTimeout);

        Application.ProcessMessages;
        CheckNotificationReceived(TIdResubscriptionData, 'No resubscription notification received');
      finally
        RemoteDialog.Free;
      end;
    finally
      OK.Free;
    end;
  finally
    Subscribe.Free;
  end;
end;

procedure TestTIdSipStackInterface.TestSendNonExistentHandle;
const
  ArbValue = 42;
begin
  try
    Self.Intf.Send(ArbValue);
    Fail('No exception raised for a non-existent handle');
  except
    on EInvalidHandle do;
  end;
end;
{
procedure TestTIdSipStackInterface.TestSessionModifiedByRemoteSide;
begin
  //  ---   INVITE   --->
  // <---   200 OK   ---
  //  ---    ACK     --->
  // <--- (re)INVITE ---
  //  ---   200 OK   --->
  // <---    ACK     ---
end;
}
procedure TestTIdSipStackInterface.TestStackListensToSubscribeModule;
var
  Conf:    TStrings;
  Package: TIdSipEventPackageClass;
  Stack:   TIdSipStackInterface;
begin
  Package := TIdSipTargetDialogPackage;

  Self.SetUpPackageSupport(Package);
  try
    Conf := TStringList.Create;
    try
      Conf.Add('Listen: UDP ' + Self.LocalAddress + ':' + IntToStr(Self.LocalPort));
      Conf.Add('NameServer: MOCK;ReturnOnlySpecifiedRecords');
      Conf.Add('Contact: sip:foo@' + Self.LocalAddress + ':' + IntToStr(Self.LocalPort));
      Conf.Add('From: sip:foo@' + Self.LocalAddress + ':' + IntToStr(Self.LocalPort));
      Conf.Add('SupportEvent: ' + Package.EventPackage);

      Stack := TIdSipStackInterface.Create(Self.UI.Handle, Self.TimerQueue, Conf);
      try
        // This is expedient, but evil: it works because Self.MockTransport will
        // be reset in SetUp when the next test runs.
        Self.MockTransport := TIdSipDebugTransportRegistry.TransportAt(TIdSipDebugTransportRegistry.TransportCount - 1) as TIdSipMockUdpTransport;

        Self.ReceiveSubscribe(Package.EventPackage);
        Application.ProcessMessages;
        CheckNotificationReceived(TIdSubscriptionRequestData, 'No subscription request notification received');

      finally
        Stack.Free;
      end;
    finally
      Conf.Free;
    end;
  finally
    Self.TearDownPackageSupport(Package);
  end;
end;

procedure TestTIdSipStackInterface.TestStackListensToSubscribeModuleAfterReconfigure;
var
  NewConf: TStrings;
  Package: TIdSipEventPackageClass;
begin
  Package := TIdSipTargetDialogPackage;

  Self.SetUpPackageSupport(Package);
  try
    NewConf := TStringList.Create;
    try
      NewConf.Add('SupportEvent: ' + Package.EventPackage);
      Self.Intf.ReconfigureStack(NewConf);
      Self.TimerQueue.TriggerAllEventsOfType(TIdSipStackReconfigureStackInterfaceWait);

      Self.ReceiveSubscribe(Package.EventPackage);
      Application.ProcessMessages;
      CheckNotificationReceived(TIdSubscriptionRequestData, 'No subscription request notification received');
    finally
      NewConf.Free;
    end;
  finally
    Self.TearDownPackageSupport(Package);
  end;
end;

//******************************************************************************
//* TestTIdSipStackInterfaceRegistry                                           *
//******************************************************************************
//* TestTIdSipStackInterfaceRegistry Public methods ****************************

procedure TestTIdSipStackInterfaceRegistry.SetUp;
begin
  inherited SetUp;

  Self.Configuration := TStringList.Create;
  Self.Timer         := TIdDebugTimerQueue.Create(false);
end;

procedure TestTIdSipStackInterfaceRegistry.TearDown;
begin
  Self.Timer.Terminate;
  Self.Configuration.Free;

  inherited TearDown;
end;

//* TestTIdSipStackInterfaceRegistry Published methods *************************

procedure TestTIdSipStackInterfaceRegistry.TestStackInterfacesAddToRegistryAutomatically;
var
  UA: TIdSipStackInterface;
begin
  UA := TIdSipStackInterface.Create(0, Self.Timer, Self.Configuration);
  try
    CheckNotEquals('', UA.ID, 'StackInterface has no ID');
    Check(nil <> TIdSipStackInterfaceRegistry.FindStackInterface(UA.ID),
          'StackInterface not added to registry');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackInterfaceRegistry.TestStackInterfacesGetUniqueIDs;
var
  UA1: TIdSipStackInterface;
  UA2: TIdSipStackInterface;
begin
  // This test isn't exactly thorough: it's not possible to write a test that
  // proves the registry will never duplicate an existing StackInterface's ID,
  // but this at least demonstrates that the registry won't return the same
  // ID twice in a row.

  UA1 := TIdSipStackInterface.Create(0, Self.Timer, Self.Configuration);
  try
    UA2 := TIdSipStackInterface.Create(0, Self.Timer, Self.Configuration);
    try
      CheckNotEquals(UA1.ID,
                     UA2.ID,
                     'The registry gave two StackInterfaces the same ID');
    finally
      UA2.Free;
    end;
  finally
    UA1.Free;
  end;
end;

procedure TestTIdSipStackInterfaceRegistry.TestStackInterfacesAutomaticallyUnregister;
var
  UA:               TIdSipStackInterface;
  StackInterfaceID: String;
begin
  UA := TIdSipStackInterface.Create(0, Self.Timer, Self.Configuration);
  try
    StackInterfaceID := UA.ID;
  finally
    UA.Free;
  end;

  Check(nil = TIdSipStackInterfaceRegistry.FindStackInterface(StackInterfaceID),
        'StackInterface not removed from registry');
end;

//******************************************************************************
//* TestTIdEventData                                                           *
//******************************************************************************
//* TestTIdEventData Public methods ********************************************

procedure TestTIdEventData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdEventData.Create;
  Self.Data.Handle := $decafbad;
end;

procedure TestTIdEventData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdEventData Published methods *****************************************

procedure TestTIdEventData.TestCopy;
var
  Copy: TIdEventData;
begin
  Copy := Self.Data.Copy;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdInformationalData                                                   *
//******************************************************************************
//* TestTIdInformationalData Public methods ************************************

procedure TestTIdInformationalData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdInformationalData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Reason := 'Arbitrary';
end;

procedure TestTIdInformationalData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdInformationalData Published methods ******************************************

procedure TestTIdInformationalData.TestCopy;
var
  Copy: TIdInformationalData;
begin
  Copy := Self.Data.Copy as TIdInformationalData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.Reason,
                Copy.Reason,
                'Reason');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdAuthenticationChallengeData                                         *
//******************************************************************************
//* TestTIdAuthenticationChallengeData Public methods **************************

procedure TestTIdAuthenticationChallengeData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdAuthenticationChallengeData.Create;
  Self.Data.ChallengedRequest.Method := MethodInvite;
  Self.Data.Challenge.StatusCode     := SIPOK;
  Self.Data.Handle                   := $decafbad;
end;

procedure TestTIdAuthenticationChallengeData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdAuthenticationChallengeData Published methods ***********************

procedure TestTIdAuthenticationChallengeData.TestCopy;
var
  Copy: TIdAuthenticationChallengeData;
begin
  Copy := Self.Data.Copy as TIdAuthenticationChallengeData;
  try
    Check(Self.Data.ChallengedRequest.Equals(Copy.ChallengedRequest),
          'ChallengedRequest');
    Check(Self.Data.Challenge.Equals(Copy.Challenge),
          'Challenge');
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdDebugData                                                           *
//******************************************************************************
//* TestTIdDebugData Public methods ********************************************

procedure TestTIdDebugData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdDebugData.Create;
  Self.Data.Event  := CM_DEBUG_STACK_STARTED;
  Self.Data.Handle := $decafbad;
end;

procedure TestTIdDebugData.TearDown;
begin
  inherited TearDown;

  Self.Data.Free;
end;

//* TestTIdDebugData Published methods *****************************************

procedure TestTIdDebugData.TestCopy;
var
  Copy: TIdDebugData;
begin
  Copy := Self.Data.Copy as TIdDebugData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(IntToHex(Self.Data.Event, 8),
                IntToHex(Copy.Event, 8),
                'Event');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdDebugMessageData                                                    *
//******************************************************************************
//* TestTIdDebugMessageData Public methods *************************************

procedure TestTIdDebugMessageData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdDebugMessageData.Create;
  Self.Data.Message := TIdSipResponse.Create;
end;

procedure TestTIdDebugMessageData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdDebugMessageData Published methods **********************************

procedure TestTIdDebugMessageData.TestCopy;
var
  Copy: TIdDebugMessageData;
begin
  Copy := Self.Data.Copy as TIdDebugMessageData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    Check(Copy.Message.Equals(Self.Data.Message),
          'The copy''s message doesn''t contain the original message');
    Check(Copy.Message <> Self.Data.Message,
          'The copy contains a reference to the original message, not a copy');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdDebugMessageData.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Expected.Text := Self.Data.Message.AsString;
      Expected.Insert(0, ''); // Timestamp + Handle
      Expected.Insert(0, ''); // Event name

      Received.Text := Self.Data.AsString;

      // We ignore the first two line of the debug data (timestamp & handle,
      // and event name)
      Received[0] := '';
      Received[1] := '';

      CheckEquals(Expected.Text,
                  Received.Text,
                  'Unexpected debug data');
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

//******************************************************************************
//* TestTIdDebugDroppedMessageData                                             *
//******************************************************************************
//* TestTIdDebugDroppedMessageData Public methods ******************************

procedure TestTIdDebugDroppedMessageData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdDebugDroppedMessageData.Create;

  Self.Data.Binding := TIdSipConnectionBindings.Create;
  Self.Data.Message := TIdSipResponse.Create;
end;

procedure TestTIdDebugDroppedMessageData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

procedure TestTIdDebugDroppedMessageData.TestCopy;
var
  Copy: TIdDebugDroppedMessageData;
begin
  Copy := Self.Data.Copy as TIdDebugDroppedMessageData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    Check(Copy.Binding.Equals(Self.Data.Binding),
          'The copy''s binding doesn''t contain the original binding');
    Check(Copy.Binding <> Self.Data.Binding,
          'The copy contains a reference to the original binding, not a copy');
    Check(Copy.Message.Equals(Self.Data.Message),
          'The copy''s message doesn''t contain the original message');
    Check(Copy.Message <> Self.Data.Message,
          'The copy contains a reference to the original message, not a copy');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdDebugDroppedMessageData.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Expected.Text := Self.Data.Message.AsString;
      Expected.Insert(0, '');
      Expected.Insert(1, EventNames(CM_DEBUG_DROPPED_MSG));
      Expected.Insert(2, Self.Data.Binding.AsString);
      Received.Text := Self.Data.AsString;

      // We ignore the first line of the debug data (it's a timestamp & a
      // handle)
      Received[0] := '';

      CheckEquals(Expected.Text,
                  Received.Text,
                  'Unexpected debug data');
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

//******************************************************************************
//* TestTIdDebugReceiveMessageData                                             *
//******************************************************************************
//* TestTIdDebugReceiveMessageData Public methods ******************************

procedure TestTIdDebugReceiveMessageData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdDebugReceiveMessageData.Create;
  Self.Data.Binding := TIdSipConnectionBindings.Create;
  Self.Data.Message := TIdSipRequest.Create;
end;

procedure TestTIdDebugReceiveMessageData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdDebugReceiveMessageData Published methods ***************************

procedure TestTIdDebugReceiveMessageData.TestCopy;
var
  Copy: TIdDebugReceiveMessageData;
begin
  Copy := Self.Data.Copy as TIdDebugReceiveMessageData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    Check(Copy.Binding.Equals(Self.Data.Binding),
          'The copy''s binding doesn''t contain the original binding');
    Check(Copy.Binding <> Self.Data.Binding,
          'The copy contains a reference to the original binding, not a copy');
    Check(Copy.Message.Equals(Self.Data.Message),
          'The copy''s message doesn''t contain the original message');
    Check(Copy.Message <> Self.Data.Message,
          'The copy contains a reference to the original message, not a copy');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdDebugReceiveMessageData.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Expected.Text := Self.Data.Message.AsString;
      Expected.Insert(0, '');
      Expected.Insert(1, EventNames(CM_DEBUG_RECV_MSG));
      Expected.Insert(2, Self.Data.Binding.AsString);
      Received.Text := Self.Data.AsString;

      // We ignore the first line of the debug data (it's a timestamp & a
      // handle)
      Received[0] := '';

      CheckEquals(Expected.Text,
                  Received.Text,
                  'Unexpected debug data');
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

//******************************************************************************
//* TestTIdDebugSendMessageData                                                *
//******************************************************************************
//* TestTIdDebugSendMessageData Public methods *********************************

procedure TestTIdDebugSendMessageData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdDebugSendMessageData.Create;
  Self.Data.Destination := TIdSipLocation.Create('TCP', '127.0.0.1', 5060);
  Self.Data.Handle      := $decafbad;
  Self.Data.Message     := TIdSipRequest.Create;
end;

procedure TestTIdDebugSendMessageData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdDebugSendMessageData Published methods ******************************

procedure TestTIdDebugSendMessageData.TestCopy;
var
  Copy: TIdDebugSendMessageData;
begin
  Copy := Self.Data.Copy as TIdDebugSendMessageData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.Destination.AsString,
                Copy.Destination.AsString,
               'The copy''s Destination doesn''t contain the original Destination');
    Check(Copy.Destination <> Self.Data.Destination,
          'The copy contains a reference to the original Destination, not a copy');
    Check(Copy.Message.Equals(Self.Data.Message),
          'The copy''s message doesn''t contain the original message');
    Check(Copy.Message <> Self.Data.Message,
          'The copy contains a reference to the original message, not a copy');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdDebugTransportExceptionData                                         *
//******************************************************************************
//* TestTIdDebugTransportExceptionData Public methods **************************

procedure TestTIdDebugTransportExceptionData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdDebugTransportExceptionData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Error  := 'No Error';
  Self.Data.Reason := 'Some Arb Reason';
end;

procedure TestTIdDebugTransportExceptionData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdDebugTransportExceptionData Published methods ***********************

procedure TestTIdDebugTransportExceptionData.TestCopy;
var
  Copy: TIdDebugTransportExceptionData;
begin
  Copy := Self.Data.Copy as TIdDebugTransportExceptionData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.Error,
                Copy.Error,
                'Error');
    CheckEquals(Self.Data.Reason,
                Copy.Reason,
                'Reason');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdDebugTransportRejectedMessageData                                   *
//******************************************************************************
//* TestTIdDebugTransportRejectedMessageData Public methods ********************

procedure TestTIdDebugTransportRejectedMessageData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdDebugTransportRejectedMessageData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Msg    := 'This contains a (malformed) SIP message';
  Self.Data.Reason := 'Here''s why it''s malformed';
end;

procedure TestTIdDebugTransportRejectedMessageData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdDebugTransportRejectedMessageData Published methods *****************

procedure TestTIdDebugTransportRejectedMessageData.TestCopy;
var
  Copy: TIdDebugTransportRejectedMessageData;
begin
  Copy := Self.Data.Copy as TIdDebugTransportRejectedMessageData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.Msg,
                Copy.Msg,
                'Msg');
    CheckEquals(Self.Data.Reason,
                Copy.Reason,
                'Reason');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdFailData                                                            *
//******************************************************************************
//* TestTIdFailData Public methods *********************************************

procedure TestTIdFailData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdFailData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Reason := 'Arbitrary';
end;

procedure TestTIdFailData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdFailData Published methods ******************************************

procedure TestTIdFailData.TestCopy;
var
  Copy: TIdFailData;
begin
  Copy := Self.Data.Copy as TIdFailData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.Reason,
                Copy.Reason,
                'Reason');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdCallEndedData                                                       *
//******************************************************************************
//* TestTIdCallEndedData Public methods ****************************************

procedure TestTIdCallEndedData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdCallEndedData.Create;
  Self.Data.ErrorCode := 42;
  Self.Data.Handle    := $decafbad;
  Self.Data.Reason    := 'Arbitrary';
end;

procedure TestTIdCallEndedData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdCallEndedData Published methods ******************************************

procedure TestTIdCallEndedData.TestCopy;
var
  Copy: TIdCallEndedData;
begin
  Copy := Self.Data.Copy as TIdCallEndedData;
  try
    CheckEquals(Self.Data.ErrorCode,
                Copy.ErrorCode,
                'ErrorCode');
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.Reason,
                Copy.Reason,
                'Reason');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdRegistrationData                                                    *
//******************************************************************************
//* TestTIdRegistrationData Public methods *************************************

procedure TestTIdRegistrationData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdRegistrationData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Contacts.Add(ContactHeaderFull).Value := 'sip:case@fried.neurons.org';
  Self.Data.Contacts.Add(ContactHeaderFull).Value := 'sip:wintermute@tessier-ashpool.co.luna';
end;

procedure TestTIdRegistrationData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdRegistrationData Published methods **********************************

procedure TestTIdRegistrationData.TestCopy;
var
  Copy: TIdRegistrationData;
begin
  Copy := Self.Data.Copy as TIdRegistrationData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    Check(Self.Data.Contacts.Equals(Copy.Contacts),
          'Contacts');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdRegistrationData.TestSetContacts;
var
  Bindings: TIdSipContacts;
begin
  Bindings := TIdSipContacts.Create;
  try
    Bindings.Add(ContactHeaderFull).Value := 'sip:cthulhu@rlyeh.org';
    Bindings.Add(ContactHeaderFull).Value := 'sip:azathoth@centre-of-all-infinity.org';

    Self.Data.Contacts := Bindings;

    Check(Bindings.Equals(Self.Data.Contacts),
          'Setter didn''t set Contacts');
  finally
    Bindings.Free;
  end;
end;

//******************************************************************************
//* TestTIdFailedRegistrationData                                              *
//******************************************************************************
//* TestTIdFailedRegistrationData Public methods *******************************

procedure TestTIdFailedRegistrationData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdFailedRegistrationData.Create;
  Self.Data.ErrorCode := 42;
  Self.Data.Reason    := 'For no good reason';
end;

procedure TestTIdFailedRegistrationData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdFailedRegistrationData Private methods ******************************

procedure TestTIdFailedRegistrationData.TestCopy;
var
  Copy: TIdFailedRegistrationData;
begin
  Copy := Self.Data.Copy as TIdFailedRegistrationData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.ErrorCode,
                Copy.ErrorCode,
                'ErrorCode');
    CheckEquals(Self.Data.Reason,
                Copy.Reason,
                'Reason');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdSessionData                                                         *
//******************************************************************************
//* TestTIdSessionData Public methods ******************************************

procedure TestTIdSessionData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdSessionData.Create;
  Self.Data.LocalMimeType            := '2';
  Self.Data.LocalSessionDescription  := '1';
  Self.Data.RemoteContact.Value      := 'sip:wintermute@terminalhead.tessier-ashpool.co.luna;transport=sctp';
  Self.Data.RemoteMimeType           := '4';
  Self.Data.RemoteParty.Value        := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.Data.RemoteSessionDescription := '3';
end;

procedure TestTIdSessionData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdSessionData Published methods ***************************************

procedure TestTIdSessionData.TestCopy;
var
  Copy: TIdSessionData;
begin
  Copy := Self.Data.Copy as TIdSessionData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.LocalMimeType,
                Copy.LocalMimeType,
                'LocalMimeType');
    CheckEquals(Self.Data.LocalSessionDescription,
                Copy.LocalSessionDescription,
                'LocalSessionDescription');
    CheckEquals(Self.Data.RemoteContact.FullValue,
                Copy.RemoteContact.FullValue,
                'RemoteContact');
    CheckEquals(Self.Data.RemoteMimeType,
                Copy.RemoteMimeType,
                'RemoteMimeType');
    CheckEquals(Self.Data.RemoteParty.FullValue,
                Copy.RemoteParty.FullValue,
                'RemotePartyp');
    CheckEquals(Self.Data.RemoteSessionDescription,
                Copy.RemoteSessionDescription,
                'RemoteSessionDescription');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdSessionData.TestSetRemotePartyStripsTagParam;
var
  Copy: TIdSessionData;
begin
  Self.Data.RemoteParty.Params[TagParam] := 'foofoo';

  Copy := TIdSessionData.Create;
  try
    Copy.RemoteParty := Self.Data.RemoteParty;
    Check(not Copy.RemoteParty.HasParameter(TagParam), 'Tag param not removed');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdSessionProgressData                                                 *
//******************************************************************************
//* TestTIdSessionProgressData Public methods **********************************

procedure TestTIdSessionProgressData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdSessionProgressData.Create;

  Self.Data.Banner                   := 'Fake TextDirect banner';
  Self.Data.Handle                   := $decafbad;
  Self.Data.LocalMimeType            := 'text/html';
  Self.Data.LocalSessionDescription  := '<html />';
  Self.Data.ProgressCode             := SIPSessionProgress;
  Self.Data.RemoteMimeType           := 'text/plain';
  Self.Data.RemoteSessionDescription := 'random data';
end;

procedure TestTIdSessionProgressData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdSessionProgressData Published methods *******************************

procedure TestTIdSessionProgressData.TestCopy;
var
  Copy: TIdSessionProgressData;
begin
  Copy := Self.Data.Copy as TIdSessionProgressData;
  try
    CheckEquals(Self.Data.Banner,
                Copy.Banner,
                'Banner');
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.LocalMimeType,
                Copy.LocalMimeType,
                'LocalMimeType');
    CheckEquals(Self.Data.LocalSessionDescription,
                Copy.LocalSessionDescription,
                'LocalSessionDescription');
    CheckEquals(Self.Data.ProgressCode,
                Copy.ProgressCode,
                'ProgressCode');
    CheckEquals(Self.Data.RemoteMimeType,
                Copy.RemoteMimeType,
                'RemoteMimeType');
    CheckEquals(Self.Data.RemoteSessionDescription,
                Copy.RemoteSessionDescription,
                'RemoteSessionDescription');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdSubscriptionRequestData                                             *
//******************************************************************************
//* TestTIdSubscriptionRequestData Public methods ******************************

procedure TestTIdSubscriptionRequestData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdSubscriptionRequestData.Create;
  Self.Data.EventPackage        := PackageRefer;
  Self.Data.From.Value          := 'Case <sip:case@fried-neurons.org>';
  Self.Data.Handle              := $decafbad;
  Self.Data.ReferTo.Value       := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna';
  Self.Data.RemoteContact.Value := 'sip:machine-1@internet-cafe.org>';
  Self.Data.Target.Uri          := 'sip:case@fried-neurons.org;grid="foo"';
end;

procedure TestTIdSubscriptionRequestData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdSubscriptionRequestData Published methods ***************************

procedure TestTIdSubscriptionRequestData.TestCopy;
var
  Copy: TIdSubscriptionRequestData;
begin
  Copy := Self.Data.Copy as TIdSubscriptionRequestData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.EventPackage,
                Copy.EventPackage,
                'EventPackage');
    CheckEquals(Self.Data.From.FullValue,
                Copy.From.FullValue,
                'From');
    CheckEquals(Self.Data.ReferTo.FullValue,
                Copy.ReferTo.FullValue,
                'ReferTo');
    CheckEquals(Self.Data.RemoteContact.FullValue,
                Copy.RemoteContact.FullValue,
                'RemoteContact');
    CheckEquals(Self.Data.Target.Uri,
                Copy.Target.Uri,
                'Target');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdResubscriptionData                                                  *
//******************************************************************************
//* TestTIdResubscriptionData Public methods ***********************************

procedure TestTIdResubscriptionData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdSubscriptionData.Create;
  Self.Data.EventPackage := 'foo';
  Self.Data.Handle       := $decafbad;
end;

procedure TestTIdResubscriptionData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdResubscriptionData Published methods ********************************

procedure TestTIdResubscriptionData.TestCopy;
var
  Copy: TIdSubscriptionData;
begin
  Copy := Self.Data.Copy as TIdSubscriptionData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.EventPackage,
                Copy.EventPackage,
                'EventPackage');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdSessionReferralData                                                 *
//******************************************************************************
//* TestTIdSessionReferralData Public methods **********************************

procedure TestTIdSessionReferralData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdSessionReferralData.Create;
  Self.Data.EventPackage        := PackageRefer;
  Self.Data.From.Value          := 'Case <sip:case@fried-neurons.org>';
  Self.Data.ReferTo.Value       := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna';
  Self.Data.ReferAction         := $decafbad;
  Self.Data.RemoteContact.Value := 'sip:machine-1@internet-cafe.org>';
  Self.Data.Target.Uri          := 'sip:case@fried-neurons.org;grid="foo"';
end;

procedure TestTIdSessionReferralData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdSessionReferralData Published methods *******************************

procedure TestTIdSessionReferralData.TestCopy;
var
  Copy: TIdSessionReferralData;
begin
  Copy := Self.Data.Copy as TIdSessionReferralData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.EventPackage,
                Copy.EventPackage,
                'EventPackage');
    CheckEquals(Self.Data.From.FullValue,
                Copy.From.FullValue,
                'From');
    CheckEquals(Self.Data.ReferTo.FullValue,
                Copy.ReferTo.FullValue,
                'ReferTo');
    CheckEquals(IntToHex(Self.Data.ReferAction, 8),
                IntToHex(Copy.ReferAction, 8),
                'ReferAction');
    CheckEquals(Self.Data.RemoteContact.FullValue,
                Copy.RemoteContact.FullValue,
                'RemoteContact');
    CheckEquals(Self.Data.Target.Uri,
                Copy.Target.Uri,
                'Target');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdSubscriptionNotifyData                                              *
//******************************************************************************
//* TestTIdSubscriptionNotifyData Public methods *******************************

procedure TestTIdSubscriptionNotifyData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdSubscriptionNotifyData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Notify.RequestUri.Uri := 'sip:case@fried-neurons.org';
end;

procedure TestTIdSubscriptionNotifyData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdSubscriptionNotifyData Published methods ****************************

procedure TestTIdSubscriptionNotifyData.TestCopy;
var
  Copy: TIdSubscriptionNotifyData;
begin
  Copy := Self.Data.Copy as TIdSubscriptionNotifyData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    Check(Copy.Notify.Equals(Self.Data.Notify),
          'NOTIFY messages don''t match');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdFailedSubscriptionData                                              *
//******************************************************************************
//* TestTIdFailedSubscriptionData Public methods *******************************

procedure TestTIdFailedSubscriptionData.SetUp;
begin
  inherited SetUp;

  Self.FailResponse := TIdSipResponse.Create;
  Self.FailResponse.StatusCode := SIPCallLegOrTransactionDoesNotExist;

  Self.Data := TIdFailedSubscriptionData.Create;
  Self.Data.Handle    := $decafbad;
  Self.Data.ErrorCode := SIPCallLegOrTransactionDoesNotExist;
  Self.Data.Reason    := RSSIPCallLegOrTransactionDoesNotExist;
  Self.Data.Response  := Self.FailResponse;
end;

procedure TestTIdFailedSubscriptionData.TearDown;
begin
  Self.Data.Free;
  Self.FailResponse.Free;

  inherited TearDown;
end;

//* TestTIdFailedSubscriptionData Published methods ****************************

procedure TestTIdFailedSubscriptionData.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Expected.Text := Self.Data.Response.AsString;
      Expected.Insert(0, '');
      Expected.Insert(1, EventNames(CM_FAIL) + ' Subscription');
      Received.Text := Self.Data.AsString;

      // We ignore the first line of the debug data (it's a timestamp & a
      // handle)
      Received[0] := '';

      CheckEquals(Expected.Text,
                  Received.Text,
                  'Unexpected debug data');
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdFailedSubscriptionData.TestCopy;
var
  Copy: TIdFailedSubscriptionData;
begin
  Copy := Self.Data.Copy as TIdFailedSubscriptionData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.ErrorCode,
                Copy.ErrorCode,
                'ErrorCode');
    CheckEquals(Self.Data.Reason,
                Copy.Reason,
                'Reason');
    Check(Copy.Response.Equals(Self.Data.Response),
          'Response messages don''t match');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipStackReconfigureStackInterfaceWait                               *
//******************************************************************************
//* TestTIdSipStackReconfigureStackInterfaceWait Public methods ****************

procedure TestTIdSipStackReconfigureStackInterfaceWait.SetUp;
const
  OneSecond = 1000;
begin
  inherited SetUp;

  TIdSipTransportRegistry.RegisterTransportType(UdpTransport, TIdSipUdpTransport);

  Self.Conf := TStringList.Create;
  Self.Conf.Add('Listen: UDP 127.0.0.1:5060');

  Self.TimerQueue := TIdDebugTimerQueue.Create(true);

  Self.Stack := TIdSipStackInterface.Create(0, Self.TimerQueue, Self.Conf);
  Self.Stack.Resume;
  Self.Wait := TIdSipStackReconfigureStackInterfaceWait.Create;
  Self.Wait.StackID := Self.Stack.ID;
end;

procedure TestTIdSipStackReconfigureStackInterfaceWait.TearDown;
begin
  Self.Stack.Free;
  Self.TimerQueue.Terminate;
  Self.Wait.Free;

  inherited TearDown;
end;

procedure TestTIdSipStackReconfigureStackInterfaceWait.CheckUdpServerOnPort(const Host: String;
                                                                            Port: Cardinal;
                                                                            const Msg: String);
var
  Binding: TIdSocketHandle;
  Server:  TIdUdpServer;
begin
  try
    Server := TIdUdpServer.Create(nil);
    try
      Binding := Server.Bindings.Add;
      Binding.IP    := Host;
      Binding.Port  := Port;
      Server.Active := true;
      try
        // Do nothing
      finally
        Server.Active := false;
      end;
    finally
      Server.Free;
    end;
    Fail('No UDP server running on ' + Host + ': ' + IntToStr(Port) + '; ' + Msg);
  except
    on EIdCouldNotBindSocket do;
  end;
end;

//* TestTIdSipStackReconfigureStackInterfaceWait Published methods *************

procedure TestTIdSipStackReconfigureStackInterfaceWait.TestSetConfiguration;
var
  Expected: TStrings;
begin
  Expected := TStringList.Create;
  try
    Expected.Add('Listen: UDP 127.0.0.1:5060');

    Self.Wait.Configuration := Expected;

    CheckEquals(Expected.Count, Self.Wait.Configuration.Count, 'Configuration not set');
    CheckEquals(Expected[0], Self.Wait.Configuration[0], 'Configuration set incorrectly');
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipStackReconfigureStackInterfaceWait.TestTriggerStartsTransports;
const
  Address = '127.0.0.1';
  Port    = 15060;
var
  Conf: TStrings;
begin
  Conf := TStringList.Create;
  try
    Conf.Add('Listen: UDP ' + Address + ':' + IntToStr(Port));
    Self.Wait.Configuration := Conf;

    Self.Wait.Trigger;

    CheckUdpServerOnPort(Address, Port, 'Stack not reconfigured or transports not started');
  finally
    Conf.Free;
  end;
end;

//******************************************************************************
//* TStackWindow                                                               *
//******************************************************************************
//* TStackWindow Public methods ************************************************

constructor TStackWindow.CreateNew(AOwner: TComponent; TestCase: TestTIdSipStackInterface);
begin
  inherited CreateNew(AOwner, 0);

  Self.fTestCase := TestCase;
end;

procedure TStackWindow.DefaultHandler(var Message);
begin
  inherited DefaultHandler(Message);

  if Self.IsStackMessage(TMessage(Message)) then
    Self.NotifyTestCase(TIdSipEventMessage(Message));
end;

//* TStackWindow Private methods ***********************************************

function TStackWindow.IsStackMessage(Msg: TMessage): Boolean;
begin
  Result := ((Msg.Msg >= CM_BASE) and (Msg.Msg <= CM_LAST));
end;

procedure TStackWindow.NotifyTestCase(Msg: TIdSipEventMessage);
begin
  try
    Self.TestCase.OnEvent(Msg.Data.Stack, Msg.Data.Event, Msg.Data.Data);
  finally
    Msg.Data.Free;
  end;
end;

initialization
  RegisterTest('SIP stack interface tests', Suite);
end.
