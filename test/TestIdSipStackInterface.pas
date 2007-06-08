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
  Classes, IdSipDialog, IdSipInviteModule, IdSipMessage, IdSipMockTransport,
  IdSipStackInterface, IdSipSubscribeModule, IdSipUserAgent, IdTimerQueue,
  Messages, TestFramework, TestFrameworkSip, TestFrameworkStackInterface,
  TestFrameworkTimerQueue;

type
  // The testing of the StackInterface is not completely simple. The UI (or
  // this test case) and the stack-running thread communicate using a Windows
  // message queue. TTestCases don't have a window handle, so we create a
  // TIdSipStackWindow which does.
  //
  // As an example, let's look at how TestInboundCall works. The test case
  // sends a SIP message through the (local loopback) network to the stack. The
  // stack sees it's an inbound INVITE, does what it needs to do, and posts the
  // CM_CALL_REQUEST_NOTIFY message to the UI's message queue. The UI picks this
  // up, fires the TestCase's OnEvent. Now we set TestCase.CheckDataProc to
  // point to CheckInboundCallData, so we can check the data.



  // That window does nothing but do stuff to the test
  // cases. That means that you have to keep the message handlers in the
  // TIdSipStackWindow in sync with those defined in the StackInterface.

  TDataCheckProc = procedure(Stack: TIdSipStackInterface;
                             Event: Cardinal;
                             Data: TIdEventData) of object;

  TestTIdSipStackInterfaceCreation = class(TTestCase)
  published
    procedure TestCreateStackWithNoSubscribeSupport;
  end;

  TestTIdSipStackInterface = class(TStackInterfaceTestCase,
                                   IIdSipInviteModuleListener)
  private
    fIntf:               TIdSipStackInterface;
    Destination:         TIdSipToHeader;
    From:                TIdSipFromHeader;
    LocalAddress:        String;
    LocalMimeType:       String;
    LocalOffer:          String;
    LocalPort:           Cardinal;
    LocalUsername:       String;
    Registrar:           TIdSipUri;
    RemoteMimeType:      String;
    RemoteMockTransport: TIdSipMockTransport;
    RemoteOffer:         String;
    RemoteSession:       TIdSipInboundSession;
    RemoteUA:            TIdSipUserAgent;
    Requests:            TIdSipRequestList;
    Responses:           TIdSipResponseList;
    TargetAddress:       String;
    TargetPort:          Cardinal;

    procedure AddSubscribeSupport(Stack: TIdSipStackInterface; EventPackage: String);
    procedure CheckRedirectCall(Temporary: Boolean);
    procedure ClearPendingStackStartedNotification;
{
    function  CreateBindings: TIdSipContacts;
    function  CreateRemoteBye(LocalDialog: TIdSipDialog): TIdSipRequest;
}
    function  CreateRemoteInvite: TIdSipRequest;
    function  CreateRemoteNotify(RemoteDialog: TIdSipDialog; Subscribe: TIdSipRequest): TIdSipRequest;
    function  CreateRemoteOk(Request: TIdSipRequest): TIdSipResponse;
    function  EstablishCall: TIdSipHandle;
    procedure OnInboundCall(UserAgent: TIdSipInviteModule;
                            Session: TIdSipInboundSession);
{
    procedure LogSentMessage(Msg: TIdSipMessage);
}
    procedure ProcessAllPendingTerminationActions;
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
    procedure SetUpPackageSupport(EventPackage: TIdSipEventPackageClass);
    procedure TearDownPackageSupport(EventPackage: TIdSipEventPackageClass);
{
    procedure ReceiveReInvite;
}
  public
    procedure SetUp; override;
    procedure TearDown; override;

    property Intf: TIdSipStackInterface read fIntf write fIntf;
  published
    procedure TestAcceptCall;
    procedure TestAcceptCallWithInvalidHandle;
    procedure TestAcceptCallWithNoExistentHandle;
    procedure TestAttachExtension;
    procedure TestCreateNotifiesOfReconfiguration;
    procedure TestEndedSession;
    procedure TestEstablishedSessionInboundCall;
    procedure TestEstablishedSessionOutboundCall;
    procedure TestHangUp;
    procedure TestHangUpWithInvalidHandle;
    procedure TestHangUpWithNonExistentHandle;
    procedure TestInboundCall;
    procedure TestMakeCall;
    procedure TestMakeCallMalformedAddress;
    procedure TestMakeCallMalformedFrom;
    procedure TestMakeOptionsQuery;
    procedure TestMakeOptionsQueryMalformedAddress;
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
    procedure TestOptionsQuery;
    procedure TestReconfigureAddsStackAsTransportListener;
    procedure TestReconfigureSendsNotify;
    procedure TestRedirectCall;
    procedure TestRedirectCallPermanently;
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
    procedure TestStackReceivesExceptionNotifications;
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

  TStackInterfaceExtensionTestCase = class(TStackInterfaceTestCase)
  protected
    Configuration: TStrings;
    Iface:         TIdSipStackInterface;

    function CreateStackInterface: TIdSipStackInterface; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipColocatedRegistrarExtension = class(TStackInterfaceExtensionTestCase)
  private
    Contact:  String;
    Contacts: TIdSipContacts;
    Reg:      TIdSipColocatedRegistrarExtension;
    Target:   TIdSipUri;

    procedure ReceiveRegister(FromUri: TIdSipUri; Contact: String);
  protected
    function CreateStackInterface: TIdSipStackInterface; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTargetsForOneTargetUri;
    procedure TestTargetsForUnknownUri;
    procedure TestTargetsForWithDBFailure;
  end;

  TestTIdSipNameServerExtension = class(TStackInterfaceExtensionTestCase)
  private
    LocalAddress: String;
    NS: TIdSipNameServerExtension;

    procedure ReconfigureStack(Intf: TIdSipStackInterface);
  protected
    function CreateStackInterface: TIdSipStackInterface; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLocalAddressForEmptyString;
    procedure TestLocalAddressForFQDN;
    procedure TestLocalAddressForNonFQDN;
    procedure TestResolveNamesFor;
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

  TestTIdQueryOptionsData = class(TTestCase)
  private
    Data: TIdQueryOptionsData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCopy;
  end;

  TestTIdDebugExceptionData = class(TTestCase)
  private
    Data: TIdDebugExceptionData;
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

  TestTIdDebugWaitExceptionData = class(TTestCase)
  private
    Data: TIdDebugWaitExceptionData;
    Wait: TIdSipReconfigureStackWait;

    procedure ExpectedAsString(Expected: TStrings);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestAsStringWithNoWait;
    procedure TestCopy;
    procedure TestCopyFromNewObject;
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
    procedure TestSetLocalPartyStripsTagParam;
    procedure TestSetRemotePartyStripsTagParam;
  end;

  TestTIdInboundCallData = class(TTestCase)
  private
    Data: TIdInboundCallData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
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
    Data:      TIdSubscriptionRequestData;
    Subscribe: TIdSipRequest;
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

  TestTIdStackReconfiguredData = class(TTestCase)
  private
    Data: TIdStackReconfiguredData;
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
  IdRandom, IdSimpleParser, IdSipCore, IdSipDns, IdSipLocation,
  IdSipMockBindingDatabase, IdSipRegistration, IdSipTransport,
  IdSipUdpTransport, IdSocketHandle, IdUdpServer, SysUtils, TestMessages;

type
  TIdSipStackInterfaceNullExtension = class(TIdSipStackInterfaceExtension)
  public
    function UA: TIdSipUserAgent;
  end;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipStackInterface unit tests');
  Result.AddTest(TestTIdSipStackInterfaceCreation.Suite);
  Result.AddTest(TestTIdSipStackInterface.Suite);
  Result.AddTest(TestTIdSipStackInterfaceRegistry.Suite);
  Result.AddTest(TestTIdSipColocatedRegistrarExtension.Suite);
  Result.AddTest(TestTIdSipNameServerExtension.Suite);
  Result.AddTest(TestTIdEventData.Suite);
  Result.AddTest(TestTIdInformationalData.Suite);
  Result.AddTest(TestTIdAuthenticationChallengeData.Suite);
  Result.AddTest(TestTIdDebugData.Suite);
  Result.AddTest(TestTIdDebugMessageData.Suite);
  Result.AddTest(TestTIdDebugDroppedMessageData.Suite);
  Result.AddTest(TestTIdDebugReceiveMessageData.Suite);
  Result.AddTest(TestTIdDebugSendMessageData.Suite);
  Result.AddTest(TestTIdQueryOptionsData.Suite);
  Result.AddTest(TestTIdDebugTransportExceptionData.Suite);
  Result.AddTest(TestTIdDebugWaitExceptionData.Suite);
  Result.AddTest(TestTIdDebugTransportRejectedMessageData.Suite);
  Result.AddTest(TestTIdFailData.Suite);
  Result.AddTest(TestTIdCallEndedData.Suite);
  Result.AddTest(TestTIdRegistrationData.Suite);
  Result.AddTest(TestTIdFailedRegistrationData.Suite);
  Result.AddTest(TestTIdSessionProgressData.Suite);
  Result.AddTest(TestTIdSessionData.Suite);
  Result.AddTest(TestTIdInboundCallData.Suite);
  Result.AddTest(TestTIdSubscriptionRequestData.Suite);
  Result.AddTest(TestTIdResubscriptionData.Suite);
  Result.AddTest(TestTIdSessionReferralData.Suite);
  Result.AddTest(TestTIdSubscriptionNotifyData.Suite);
  Result.AddTest(TestTIdFailedSubscriptionData.Suite);
  Result.AddTest(TestTIdStackReconfiguredData.Suite);
  Result.AddTest(TestTIdSipStackReconfigureStackInterfaceWait.Suite);
end;

//******************************************************************************
//* TIdSipStackInterfaceNullExtension                                          *
//******************************************************************************
//* TIdSipStackInterfaceNullExtension Public methods ***************************

function TIdSipStackInterfaceNullExtension.UA: TIdSipUserAgent;
begin
  Result := Self.UserAgent;
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

  Self.Destination := TIdSipToHeader.Create;
  Self.From        := TIdSipFromHeader.Create;
  Self.Requests    := TIdSipRequestList.Create;
  Self.Responses   := TIdSipResponseList.Create;

  Self.TargetAddress := '10.0.0.8';
  Self.TargetPort    := 5060;
  BasicConf := TStringList.Create;
  try
    BasicConf.Add('Listen: UDP ' + Self.TargetAddress + ':' + IntToStr(Self.TargetPort));
    BasicConf.Add('NameServer: MOCK');
    BasicConf.Add('Contact: sip:' + Self.TargetAddress + ':' + IntToStr(Self.TargetPort));
    BasicConf.Add('From: sip:' + Self.TargetAddress + ':' + IntToStr(Self.TargetPort));

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

  Self.RemoteMockTransport := TIdSipDebugTransportRegistry.LastTransport as TIdSipMockTransport;

  Self.LocalAddress  := '10.0.0.6';
  Self.LocalPort     := 5060;
  Self.LocalUsername := 'foo';
  Self.From.Value    := 'sip:' + Self.LocalUsername + '@' + Self.LocalAddress + ':' + IntToStr(Self.LocalPort);
  BasicConf := TStringList.Create;
  try
    BasicConf.Add('Listen: UDP ' + Self.LocalAddress + ':' + IntToStr(Self.LocalPort));
    BasicConf.Add('NameServer: MOCK;ReturnOnlySpecifiedRecords');
    BasicConf.Add('Contact: sip:foo@' + Self.LocalAddress + ':' + IntToStr(Self.LocalPort));
    BasicConf.Add(Self.From.AsString);

    Self.Intf := TIdSipStackInterface.Create(Self.UI.Handle, Self.TimerQueue, BasicConf);
  finally
    BasicConf.Free;
  end;
  Self.Intf.Resume;

  Self.MockTransport := TIdSipDebugTransportRegistry.LastTransport as TIdSipMockTransport;

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

  Self.ClearPendingStackStartedNotification;  
end;

procedure TestTIdSipStackInterface.TearDown;
begin
  Self.Registrar.Free;
  Self.Intf.Free;
  Self.Responses.Free;
  Self.Requests.Free;
  Self.RemoteUA.Free;
  Self.From.Free;
  Self.Destination.Free;

  TIdSipTransportRegistry.UnregisterTransportType(UdpTransport);

  inherited TearDown;
end;

//* TestTIdSipStackInterface Private methods ***********************************

procedure TestTIdSipStackInterface.AddSubscribeSupport(Stack: TIdSipStackInterface; EventPackage: String);
var
  NewConf: TStrings;
begin
  NewConf := TStringList.Create;
  try
    NewConf.Add('SupportEvent: ' + EventPackage);
    Stack.ReconfigureStack(NewConf);
    Self.TimerQueue.TriggerAllEventsOfType(TIdSipStackReconfigureStackInterfaceWait);
  finally
    NewConf.Free;
  end;
end;

procedure TestTIdSipStackInterface.CheckRedirectCall(Temporary: Boolean);
var
  NewTarget: TIdSipAddressHeader;
begin
  Self.ReceiveInviteWithOffer(Self.RemoteOffer, Self.RemoteMimeType);
  Self.ProcessAllPendingNotifications;
  CheckNotificationReceived(TIdInboundCallData, 'No inbound call notification received');

  Check(Self.LastEventOfType(TIdInboundCallData).Handle > 0, 'Invalid Action handle');

  NewTarget := TIdSipContactHeader.Create;
  try
    NewTarget.Value := 'sip:' + TIdIPAddressParser.IncIPAddress(Self.TargetAddress);

    Self.MarkSentResponseCount;
    Self.Intf.RedirectCall(Self.SecondLastEventData.Handle, NewTarget, Temporary);
    Self.TimerQueue.TriggerAllEventsOfType(TIdSipSessionRedirectWait);
    CheckResponseSent('No response sent');
    CheckEquals(TIdSipInboundInvite.RedirectStatusCode(Temporary),
                Self.LastSentResponse.StatusCode,
                'Unexpected response sent');
    CheckEquals(NewTarget.AsString,
                Self.LastSentResponse.FirstContact.AsString,
                'Unexpected redirection target');
  finally
    NewTarget.Free;
  end;
end;

procedure TestTIdSipStackInterface.ClearPendingStackStartedNotification;
begin
  Self.ProcessAllPendingNotifications;
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
  Result := Self.RemoteUA.InviteModule.CreateInvite(Self.RemoteUA.From, Self.Destination, '', '');
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
  Result := Self.Intf.MakeCall(Self.From,
                               Self.Destination,
                               Self.LocalOffer,
                               Self.LocalMimeType);

  Self.MarkSentRequestCount;
  Self.Intf.Send(Result);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  CheckRequestSent('No INVITE sent in EstablishCall');

  Self.ReceiveOk(Self.LastSentRequest, Self.RemoteOffer, Self.RemoteMimeType);
  Self.ProcessAllPendingNotifications;
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
procedure TestTIdSipStackInterface.ProcessAllPendingTerminationActions;
begin
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionTerminateWait);
end;

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
    Target.LocalIP   := Self.LocalAddress;
    Target.LocalPort := Self.LocalPort;
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
    Target.LocalIP   := Self.LocalAddress;
    Target.LocalPort := Self.LocalPort;
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

    Subscribe := SubMod.CreateSubscribe(Self.RemoteUA.From, LocalFrom, EventPackage);
    try
      Self.ReceiveRequest(Subscribe);
    finally
      Subscribe.Free;
    end;
  finally
    LocalFrom.Free;
  end;
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
  Self.ProcessAllPendingNotifications;
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
  H := Self.Intf.MakeCall(Self.From, Self.Destination, '', '');

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

procedure TestTIdSipStackInterface.TestAttachExtension;
var
  Null: TIdSipStackInterfaceExtension;
  E:    TIdSipStackInterfaceNullExtension;
begin
  Null := Self.Intf.AttachExtension(TIdSipStackInterfaceNullExtension);
  CheckNotNull(Null, 'AttachExtension didn''t return a value');
  CheckEquals(TIdSipStackInterfaceNullExtension, Null.ClassType, 'AttachExtension returned the wrong type object');

  E := Null as TIdSipStackInterfaceNullExtension;
  CheckEquals(Self.MockTransport.ID,
              E.UA.Dispatcher.Transports[0].ID,
              'Extension''s UserAgent isn''t using the same transport as the '
            + 'StackInterface, and thus the UserAgent property is not properly set');
end;

procedure TestTIdSipStackInterface.TestCreateNotifiesOfReconfiguration;
begin
  CheckNotificationReceived(TIdStackReconfiguredData, 'No reconfigure notification received');
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
  Self.ProcessAllPendingNotifications;
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
  Invite:          TIdSipRequest;
  Data:            TIdEstablishedSessionData;
  LocalParty:      TIdSipAddressHeader;
  RemoteParty:     TIdSipAddressHeader;
begin
  Invite := TIdSipRequest.Create;
  try
    Self.ReceiveInviteWithOffer(Self.RemoteOffer, Self.RemoteMimeType);
    Invite.Assign(Self.MockTransport.LastRequest);

    Self.ProcessAllPendingNotifications;
    CheckNotificationReceived(TIdInboundCallData, 'No inbound call notification received?');
    InboundCallData := Self.LastEventOfType(TIdInboundCallData) as TIdInboundCallData;

    Self.Intf.AnswerCall(InboundCallData.Handle, Self.LocalOffer, Self.LocalMimeType);
    Self.TimerQueue.TriggerAllEventsOfType(TIdSipSessionAcceptWait);
    Self.ProcessAllPendingNotifications;

    Self.ReceiveAck;
    Self.ProcessAllPendingNotifications;

    CheckNotificationReceived(TIdEstablishedSessionData, 'No established session notification received');

    Data := Self.LastEventOfType(TIdEstablishedSessionData) as TIdEstablishedSessionData;

    LocalParty := TIdSipFromHeader.Create;
    try
      RemoteParty := TIdSipFromHeader.Create;
      try
        LocalParty.Assign(Self.LastSentResponse.ToHeader);
        LocalParty.RemoveParameter(TagParam);
        RemoteParty.Assign(Invite.From);
        RemoteParty.RemoveParameter(TagParam);

        CheckEquals(Self.LastSentResponse.FirstContact.FullValue,
                    Data.LocalContact.FullValue,
                    'LocalContact');
        CheckEquals(Self.LocalMimeType, Data.LocalMimeType, 'LocalMimeType');
        CheckEquals(LocalParty.FullValue,
                    Data.LocalParty.FullValue,
                    'LocalParty');
        CheckEquals(Self.LocalOffer, Data.LocalSessionDescription, 'LocalSessionDescription');

        CheckEquals(Invite.FirstContact.FullValue,
                    Data.RemoteContact.FullValue,
                    'RemoteContact');
        CheckEquals(Self.RemoteMimeType, Data.RemoteMimeType, 'RemoteMimeType');
        CheckEquals(RemoteParty.FullValue,
                    Data.RemoteParty.FullValue,
                    'RemoteParty');
        CheckEquals(Self.RemoteOffer, Data.RemoteSessionDescription, 'RemoteSessionDescription');
      finally
        RemoteParty.Free;
      end;
    finally
      LocalParty.Free;
    end;
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipStackInterface.TestEstablishedSessionOutboundCall;
var
  Call: TIdSipHandle;
begin
  Call := Self.Intf.MakeCall(Self.From, Self.Destination, Self.LocalOffer, Self.LocalMimeType);
  Self.Intf.Send(Call);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  Self.ProcessAllPendingNotifications;

  Self.ReceiveOk(Self.LastSentRequest);
  Self.ProcessAllPendingNotifications;
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
  Self.ProcessAllPendingNotifications;
  CheckNotificationReceived(TIdCallEndedData, 'No notification of end of call');
  CheckRequestSent('No BYE sent');
  CheckEquals(MethodBye, Self.LastSentRequest.Method, 'Unexpected request sent');
end;

procedure TestTIdSipStackInterface.TestHangUpWithInvalidHandle;
var
  H: TIdSipHandle;
begin
  H := Self.Intf.MakeOptionsQuery(Self.Destination);

  try
    // You can't, obviously, "hang up" an options query attempt.
    Self.Intf.HangUp(H);

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
  Data:          TIdInboundCallData;
  DatasInvite:   TIdSipRequest;
  RemoteContact: TIdSipContactHeader;
begin
  Self.ReceiveInviteWithOffer(Self.RemoteOffer, Self.RemoteMimeType);
  Self.ProcessAllPendingNotifications;
  CheckNotificationReceived(TIdInboundCallData, 'No inbound call notification received');

  Data := Self.LastEventOfType(TIdInboundCallData) as TIdInboundCallData;
  Check(Data.Handle > 0, 'Invalid Action handle');
  CheckEquals('',                                 Data.LocalSessionDescription,  'LocalSessionDescription');
  CheckEquals('',                                 Data.LocalMimeType,            'LocalMimeType');
  CheckEquals(Self.MockTransport.LastRequest.ToHeader.Value,
              Data.LocalParty.Value,
              'LocalParty');
  CheckEquals('sip:' + Self.LocalAddress + ':' + IntToStr(Self.LocalPort),
              Data.LocalContact.Value,
              'LocalContact');
  CheckEquals(Self.RemoteOffer,                   Data.RemoteSessionDescription, 'RemoteSessionDescription');
  CheckEquals(Self.RemoteMimeType,                Data.RemoteMimeType,           'RemoteMimeType');
  CheckEquals(Self.RemoteUA.From.Value,           Data.RemoteParty.Value,        'RemoteParty');

  RemoteContact := TIdSipContactHeader.Create;
  try
    RemoteContact.Assign(Self.RemoteUA.From);
    RemoteContact.Address.Host := Self.TargetAddress;
    RemoteContact.Address.Port := Self.TargetPort;
    CheckEquals(RemoteContact.AsString, Data.RemoteContact.AsString, 'RemoteContact');
  finally
    RemoteContact.Free;
  end;

  DatasInvite := Data.Invite.Copy as TIdSipRequest;
  try
    // The transport layer adds a "received" parameter, which doesn't show in
    // the copy of the INVITE stored by the mock transport.
    DatasInvite.LastHop.RemoveParameter(ReceivedParam);
    DatasInvite.ToHeader.RemoveParameter(TagParam);

    Check(Self.MockTransport.LastRequest.Equals(DatasInvite), 'Invite');
  finally
    DatasInvite.Free;
  end;
end;

procedure TestTIdSipStackInterface.TestMakeCall;
var
  ActualFrom: TIdSipFromHeader;
  Handle:     TIdSipHandle;
begin
  Handle := Self.Intf.MakeCall(Self.From, Self.Destination, '', '');

  Self.MarkSentRequestCount;
  Self.Intf.Send(Handle);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  CheckRequestSent('No request sent');
  CheckEquals(MethodInvite, Self.MockTransport.LastRequest.Method, 'Unexpected request sent');

  ActualFrom := TIdSipFromHeader.Create;
  try
    ActualFrom.Assign(Self.MockTransport.LastRequest.From);
    ActualFrom.RemoveParameter(TagParam);

    CheckEquals(Self.From.AsString, ActualFrom.AsString, 'From');
  finally
    ActualFrom.Free;
  end;
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

    Handle := Self.Intf.MakeCall(Self.From, MalformedAddress, '', '');
    CheckEquals(InvalidHandle, Handle, 'MakeCall didn''t return the invalid handle');
  finally
    MalformedAddress.Free;
  end;
end;

procedure TestTIdSipStackInterface.TestMakeCallMalformedFrom;
var
  Handle:           TIdSipHandle;
  MalformedAddress: TIdSipFromHeader;
begin
  MalformedAddress := TIdSipFromHeader.Create;
  try
    MalformedAddress.Address.Uri := 'sip:::1';
    Check(MalformedAddress.IsMalformed, 'Sanity check: the URI must be malformed');

    Handle := Self.Intf.MakeCall(MalformedAddress, Self.Destination, '', '');
    CheckEquals(InvalidHandle, Handle, 'MakeCall didn''t return the invalid handle');
  finally
    MalformedAddress.Free;
  end;
end;

procedure TestTIdSipStackInterface.TestMakeOptionsQuery;
var
  Handle: TIdSipHandle;
begin
  Handle := Self.Intf.MakeOptionsQuery(Self.Destination);

  Self.MarkSentRequestCount;
  Self.Intf.Send(Handle);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  CheckRequestSent('No request sent');
  CheckEquals(MethodOptions, Self.LastSentRequest.Method, 'Unexpected request sent');
end;

procedure TestTIdSipStackInterface.TestMakeOptionsQueryMalformedAddress;
var
  Handle:           TIdSipHandle;
  MalformedAddress: TIdSipToHeader;
begin
  MalformedAddress := TIdSipToHeader.Create;
  try
    MalformedAddress.Address.Uri := 'sip:::1';
    Check(MalformedAddress.IsMalformed, 'Sanity check: the URI must be malformed');

    Handle := Self.Intf.MakeOptionsQuery(MalformedAddress);
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
  Package: TIdSipEventPackageClass;
  Handle:  TIdSipHandle;
begin
  Package := TIdSipTargetDialogPackage;

  Self.SetUpPackageSupport(Package);
  try
    Self.AddSubscribeSupport(Self.Intf, Package.EventPackage);

    // We try subscribe to something that isn't going to immediately reply.
    Self.Destination.Address.Host := TIdIPAddressParser.IncIPAddress(Self.TargetAddress);

    Handle := Self.Intf.MakeSubscription(Self.Destination, Package.EventPackage);

    Self.MarkSentRequestCount;
    Self.Intf.Send(Handle);
    Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
    CheckRequestSent('No request sent');
    CheckEquals(MethodSubscribe, Self.LastSentRequest.Method, 'Unexpected request sent');
  finally
    Self.TearDownPackageSupport(Package);
  end;
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
  Handle:  TIdSipHandle;
  Package: TIdSipEventPackageClass;
begin
  Package := TIdSipTargetDialogPackage;

  Self.SetUpPackageSupport(Package);
  try
    Handle := Self.Intf.MakeSubscription(Self.Destination, Package.EventPackage);

    CheckEquals(IntToHex(InvalidHandle, 8),
                IntToHex(Handle, 8),
                'No SUBSCRIBE support should result in an InvalidHandle');
  finally
    Self.TearDownPackageSupport(Package);
  end;
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
  H: TIdSipHandle;
begin
  H := Self.Intf.MakeOptionsQuery(Self.Destination);

  try
    // You can't, obviously, "modify" an options query attempt.
    Self.Intf.ModifyCall(H, Self.LocalOffer, Self.LocalMimeType);

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
  Call := Self.Intf.MakeCall(Self.From, Self.Destination, '', '');
  Check(Call > 0, 'Invalid handle');
  Self.Intf.Send(Call);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  Self.ProcessAllPendingNotifications;

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
  H := Self.Intf.MakeCall(Self.From,
                          Self.Destination,
                          Self.LocalOffer,
                          Self.LocalMimeType);
  // Send the INVITE
  Self.Intf.Send(H);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  Self.RemoteMockTransport.FireOnRequest(Self.MockTransport.LastRequest);

  // Receive the 100 Trying and 180 Ringing from the RemoteUA
  Self.MockTransport.FireOnResponse(Self.RemoteMockTransport.SecondLastResponse);
  Self.MockTransport.FireOnResponse(Self.RemoteMockTransport.LastResponse);
  Self.ProcessAllPendingNotifications;

  Check(Assigned(Self.RemoteSession), 'RemoteSession never assigned: RemoteUA didn''t receive INVITE?');
  Self.RemoteSession.AcceptCall(Self.RemoteOffer, Self.RemoteMimeType);
  Self.MockTransport.FireOnResponse(Self.RemoteMockTransport.LastResponse);
  Self.ProcessAllPendingNotifications;

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

procedure TestTIdSipStackInterface.TestOptionsQuery;
var
  Data: TIdQueryOptionsData;
  H: TIdSipHandle;
begin
  H := Self.Intf.MakeOptionsQuery(Self.Destination);

  Self.MarkSentRequestCount;
  Self.Intf.Send(H);
  // Send the OPTIONS
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  Self.RemoteMockTransport.FireOnRequest(Self.MockTransport.LastRequest);

  // Receive the response
  Self.MockTransport.FireOnResponse(Self.RemoteMockTransport.LastResponse);
  Self.ProcessAllPendingNotifications;

  CheckNotificationReceived(TIdQueryOptionsData, 'No options query response notification received');

  Data := Self.LastEventOfType(TIdQueryOptionsData) as TIdQueryOptionsData;

  CheckEquals(IntToHex(H, 8), IntToHex(Data.Handle, 8), 'Invalid Action handle');
  Check(Data.Response.Equals(Self.MockTransport.LastResponse), 'Unexpected response');
end;

procedure TestTIdSipStackInterface.TestReconfigureAddsStackAsTransportListener;
var
  Conf: TStrings;
begin
  Conf := TStringList.Create;
  try
    Conf.Add(ListenDirective + ': UDP 127.0.0.1:5060');

    Self.Intf.ReconfigureStack(Conf);
    Self.TimerQueue.TriggerAllEventsOfType(TIdSipStackReconfigureStackInterfaceWait);
    Self.MockTransport := TIdSipDebugTransportRegistry.LastTransport as TIdSipMockTransport;
  finally
    Conf.Free;
  end;

  Self.ReceiveInvite;
  Self.ProcessAllPendingNotifications;
  CheckNotificationReceived(TIdDebugReceiveMessageData, 'Stack not listening to UA''s transports');
end;

procedure TestTIdSipStackInterface.TestReconfigureSendsNotify;
var
  E:       TIdEventData;
  NewConf: TStrings;
  Recon:   TIdStackReconfiguredData;
begin
  NewConf := TStringList.Create;
  try
    NewConf.Add('Listen: UDP ' + Self.TargetAddress + ':' + IntToStr(Self.TargetPort));
    NewConf.Add('NameServer: MOCK');
    NewConf.Add('Contact: sip:' + Self.TargetAddress + ':' + IntToStr(Self.TargetPort));
    NewConf.Add('From: sip:' + Self.TargetAddress + ':' + IntToStr(Self.TargetPort));

    Self.Intf.ReconfigureStack(NewConf);
    Self.TimerQueue.TriggerAllEventsUpToFirst(TIdSipStackReconfigureStackInterfaceWait);
    Self.ProcessAllPendingNotifications;
    E := Self.LastEventOfType(TIdStackReconfiguredData);
    Check(Assigned(E), 'No TIdStackReconfiguredData found (1)');

    Recon := E as TIdStackReconfiguredData;
    Check(not Recon.ActsAsRegistrar, 'Stack thinks it should act as a registrar');

    NewConf.Add(ActAsRegistrarDirective + ': true');
    NewConf.Add(RegistrarDatabaseDirective + ': ' + MockKeyword);

    Self.Intf.ReconfigureStack(NewConf);
    Self.TimerQueue.TriggerAllEventsUpToFirst(TIdSipStackReconfigureStackInterfaceWait);
    Self.ProcessAllPendingNotifications;
    E := Self.LastEventOfType(TIdStackReconfiguredData);
    Check(Assigned(E), 'No TIdStackReconfiguredData found (2)');
    Recon := E as TIdStackReconfiguredData;
    Check(Recon.ActsAsRegistrar, 'Stack doesn''t think it should act as a registrar');
  finally
    NewConf.Free;
  end;
end;

procedure TestTIdSipStackInterface.TestRedirectCall;
begin
  CheckRedirectCall(true);
end;

procedure TestTIdSipStackInterface.TestRedirectCallPermanently;
begin
  CheckRedirectCall(false);
end;

procedure TestTIdSipStackInterface.TestRedirectCallWithInvalidHandle;
var
  H: TIdSipHandle;
begin
  H := Self.Intf.MakeOptionsQuery(Self.Destination);

  try
    // You can't, obviously, "Redirect" an options query attempt.
    Self.Intf.RedirectCall(H, Self.Destination);

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
const
  StatusCode = SIPBusyHere;
  StatusText = 'Call again later';
begin
  Self.ReceiveInviteWithOffer(Self.RemoteOffer, Self.RemoteMimeType);
  Self.ProcessAllPendingNotifications;
  CheckNotificationReceived(TIdInboundCallData, 'No inbound call notification received');

  Check(Self.LastEventOfType(TIdInboundCallData).Handle > 0, 'Invalid Action handle');

  Self.MarkSentResponseCount;
  Self.Intf.RejectCall(Self.SecondLastEventData.Handle, StatusCode, StatusText);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipSessionRejectWait);
  CheckResponseSent('No response sent');
  CheckEquals(StatusCode, Self.LastSentResponse.StatusCode, 'Unexpected Status-Code sent');
  CheckEquals(StatusText, Self.LastSentResponse.StatusText, 'Unexpected Status-Text sent');
end;

procedure TestTIdSipStackInterface.TestRejectCallWithInvalidHandle;
var
  H: TIdSipHandle;
begin
  H := Self.Intf.MakeOptionsQuery(Self.Destination);

  try
    // You can't, obviously, "reject" an options query attempt.
    Self.Intf.RejectCall(H, SIPBusyHere);

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
    Self.Intf.RejectCall(ArbValue, SIPBusyHere);
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
  Self.ProcessAllPendingNotifications;
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
  Self.ProcessAllPendingNotifications;

  CheckNotificationReceived(TIdRegistrationData, 'No registration success notification received');
end;

procedure TestTIdSipStackInterface.TestResubscription;
var
  OK:           TIdSipResponse;
  Package:      TIdSipEventPackageClass;
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

  Package := TIdSipTargetDialogPackage;

  Self.SetUpPackageSupport(Package);
  try
    Self.AddSubscribeSupport(Self.Intf, Package.EventPackage);

    Self.Destination.Address.Host := TIdIPAddressParser.IncIPAddress(Self.Destination.Address.Host);
    Sub := Self.Intf.MakeSubscription(Self.Destination, Package.EventPackage);
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

          Self.ProcessAllPendingNotifications;
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
  finally
    Self.TearDownPackageSupport(Package);
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
        Self.MockTransport := TIdSipDebugTransportRegistry.LastTransport as TIdSipMockUdpTransport;

        Self.ReceiveSubscribe(Package.EventPackage);
        Self.ProcessAllPendingNotifications;
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
      Self.ProcessAllPendingNotifications;
      CheckNotificationReceived(TIdSubscriptionRequestData, 'No subscription request notification received');
    finally
      NewConf.Free;
    end;
  finally
    Self.TearDownPackageSupport(Package);
  end;
end;

procedure TestTIdSipStackInterface.TestStackReceivesExceptionNotifications;
var
  E: TExceptionRaisingWait;
begin
  E := TExceptionRaisingWait.Create;
  Self.TimerQueue.AddEvent(TriggerImmediately, E);

  Self.TimerQueue.TriggerEarliestEvent;
  Self.ProcessAllPendingNotifications;

  CheckNotificationReceived(TIdDebugWaitExceptionData, 'No exception notification received');
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
//* TStackInterfaceExtensionTestCase                                           *
//******************************************************************************
//* TStackInterfaceExtensionTestCase Public methods ****************************

procedure TStackInterfaceExtensionTestCase.SetUp;
begin
  inherited SetUp;

  TIdSipTransportRegistry.RegisterTransportType(UdpTransport, TIdSipMockUdpTransport);

  Self.Configuration := TStringList.Create;

  Self.Iface := Self.CreateStackInterface;
end;

procedure TStackInterfaceExtensionTestCase.TearDown;
begin
  Self.Configuration.Free;

  TIdSipTransportRegistry.UnregisterTransportType(UdpTransport);

  inherited TearDown;
end;

//* TStackInterfaceExtensionTestCase Protected methods *************************

function TStackInterfaceExtensionTestCase.CreateStackInterface: TIdSipStackInterface;
begin
  Result := TIdSipStackInterface.Create(Self.UI.Handle, Self.TimerQueue, Self.Configuration);
end;

//******************************************************************************
//* TestTIdSipColocatedRegistrarExtension                                      *
//******************************************************************************
//* TestTIdSipColocatedRegistrarExtension Public methods ***********************

procedure TestTIdSipColocatedRegistrarExtension.SetUp;
begin
  inherited SetUp;

  Self.Contacts := TIdSipContacts.Create;

  Self.MockTransport := TIdSipDebugTransportRegistry.LastTransport as TIdSipMockTransport;

  Self.Reg    := Self.Iface.AttachExtension(TIdSipColocatedRegistrarExtension) as TIdSipColocatedRegistrarExtension;
  Self.Target := TIdSipUri.Create('sip:case@fried-neurons.org');

  Self.Contact := 'sip:case@tmp.node';
end;

procedure TestTIdSipColocatedRegistrarExtension.TearDown;
begin
  Self.Target.Free;
  Self.Iface.Free;
  Self.Contacts.Free;

  inherited TearDown;
end;

//* TestTIdSipColocatedRegistrarExtension Protected methods ********************

function TestTIdSipColocatedRegistrarExtension.CreateStackInterface: TIdSipStackInterface;
begin
  Self.Configuration.Add(ListenDirective            + ': UDP 127.0.0.1:5060');
  Self.Configuration.Add(NameServerDirective        + ': ' + MockKeyword);
  Self.Configuration.Add(ActAsRegistrarDirective    + ': yes');
  Self.Configuration.Add(RegistrarDatabaseDirective + ': ' + MockKeyword);

  Result := inherited CreateStackInterface;
end;

//* TestTIdSipColocatedRegistrarExtension Private methods **********************

procedure TestTIdSipColocatedRegistrarExtension.ReceiveRegister(FromUri: TIdSipUri; Contact: String);
var
  Reg: TIdSipRequest;
begin
  Self.MarkSentResponseCount;

  Reg := TIdSipTestResources.CreateRegister(FromUri, Contact);
  try
    Self.MockTransport.FireOnRequest(Reg);
  finally
    Reg.Free;
  end;

  // Trigger the sending of a 200 OK to the REGISTER
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipMessageWait);

  CheckResponseSent('No response sent to REGISTER');
  CheckEquals(SIPOK, Self.LastSentResponse.StatusCode, 'Unexpected response sent: ' + Self.LastSentResponse.StatusText);
  Check(Self.LastSentResponse.HasHeader(ContactHeaderFull), 'Registrar didn''t store bindings');
end;

//* TestTIdSipColocatedRegistrarExtension Published methods ********************

procedure TestTIdSipColocatedRegistrarExtension.TestTargetsForOneTargetUri;
begin
  Self.ReceiveRegister(Self.Target, Self.Contact);

  Self.Reg.TargetsFor(Self.Target, Self.Contacts);

  Check(not Self.Contacts.IsEmpty, 'TargetsFor returned no targets');
  CheckEquals(1, Self.Contacts.Count, 'Number of contacts');

  Self.Contacts.First;
  CheckEquals(Self.Contact, Self.Contacts.CurrentContact.Address.AsString, 'Wrong contact');
end;

procedure TestTIdSipColocatedRegistrarExtension.TestTargetsForUnknownUri;
begin
  Self.Reg.TargetsFor(Self.Target, Self.Contacts);

  Check(Self.Contacts.IsEmpty, 'TargetsFor returned targets for an unknown URI');
end;

procedure TestTIdSipColocatedRegistrarExtension.TestTargetsForWithDBFailure;
var
  DB: TIdSipMockBindingDatabase;
  N:  TIdSipStackInterfaceNullExtension;
begin
  N := Self.Iface.AttachExtension(TIdSipStackInterfaceNullExtension) as TIdSipStackInterfaceNullExtension;
  Check(N.UA.UsesModule(TIdSipRegisterModule), 'UA doesn''t support REGISTER method');
  DB := (N.UA.ModuleFor(TIdSipRegisterModule) as TIdSipRegisterModule).BindingDB as TIdSipMockBindingDatabase;

  Self.ReceiveRegister(Self.Target, Self.Contact);

  DB.FailBindingsFor := true;

  Self.Reg.TargetsFor(Self.Target, Self.Contacts);
  Check(Self.Contacts.IsEmpty, 'TargetsFor returned targets when the DB failed');
end;

//******************************************************************************
//* TestTIdSipNameServerExtension                                              *
//******************************************************************************
//* TestTIdSipNameServerExtension Public methods *******************************

procedure TestTIdSipNameServerExtension.SetUp;
begin
  inherited SetUp;

  Self.NS := Self.Iface.AttachExtension(TIdSipNameServerExtension) as TIdSipNameServerExtension;

  Self.Configuration.Clear;
end;

procedure TestTIdSipNameServerExtension.TearDown;
begin
  Self.NS.Free;

  inherited TearDown;
end;

//* TestTIdSipNameServerExtension Protected methods ****************************

function TestTIdSipNameServerExtension.CreateStackInterface: TIdSipStackInterface;
begin
  Self.LocalAddress := '172.1.2.3';

  Self.Configuration.Add(ListenDirective           + ': UDP 127.0.0.1:5060');
  Self.Configuration.Add(NameServerDirective       + ': ' + MockKeyword);
  Self.Configuration.Add(RoutingTableDirective     + ': ' + MockKeyword);
  Self.Configuration.Add(MockLocalAddressDirective + ': ' + Self.LocalAddress);

  Result := inherited CreateStackInterface;
end;

//* TestTIdSipNameServerExtension Private methods ******************************

procedure TestTIdSipNameServerExtension.ReconfigureStack(Intf: TIdSipStackInterface);
begin
  Self.Iface.ReconfigureStack(Self.Configuration);
  Self.TimerQueue.TriggerAllEventsUpToFirst(TIdSipStackReconfigureStackInterfaceWait);
end;

//* TestTIdSipNameServerExtension Published methods ****************************

procedure TestTIdSipNameServerExtension.TestLocalAddressForEmptyString;
begin
  Self.ExpectedException := EBadParameter;
  Self.NS.LocalAddressFor('');
end;

procedure TestTIdSipNameServerExtension.TestLocalAddressForFQDN;
const
  InternetHost = 'tessier-ashpool.co.luna';
  InternetIP   = '1.2.3.4';
  LocalAddress = '10.0.0.6';
  VpnAddress   = '192.168.1.42';
  VpnHost      = 'giftshop.local';
  VpnIP        = '192.168.1.22';
begin
  Self.Configuration.Clear;
  Self.Configuration.Add(RoutingTableDirective + ': ' + MockKeyword);
  Self.Configuration.Add(Format('MockDns: A %s %s', [InternetHost, InternetIP]));
  Self.Configuration.Add(Format('MockDns: A %s %s', [VpnHost, VpnIP]));
  Self.Configuration.Add(MockRouteDirective + ': 0.0.0.0/0 10.0.0.1 1 1 ' + LocalAddress);
  Self.Configuration.Add(MockRouteDirective + ': 10.0.0.0/24 10.0.0.1 1 1 ' + LocalAddress);
  Self.Configuration.Add(MockRouteDirective + ': 192.168.1.0/24 192.168.1.1 1 1 ' + VpnAddress);
  Self.ReconfigureStack(Self.Iface);

  CheckEquals(LocalAddress, Self.NS.LocalAddressFor(InternetHost), 'Internet address');
  CheckEquals(VpnAddress,   Self.NS.LocalAddressFor(VpnHost),      'VPN address');
end;

procedure TestTIdSipNameServerExtension.TestLocalAddressForNonFQDN;
begin
  Self.ExpectedException := EBadParameter;
  Self.NS.LocalAddressFor('::G');
end;

procedure TestTIdSipNameServerExtension.TestResolveNamesFor;
var
  Names: TIdDomainNameRecords;
begin
  Self.Configuration.Clear;
  Self.Configuration.Add(MockDnsDirective + ': A tessier-ashpool.co.luna 1.2.3.4');
  Self.Configuration.Add(MockDnsDirective + ': AAAA tessier-ashpool.co.luna 12::34');
  Self.ReconfigureStack(Self.Iface);

  Names := TIdDomainNameRecords.Create;
  try
    Self.NS.ResolveNamesFor('tessier-ashpool.co.luna', Names);

    CheckEquals(2, Names.Count, 'Unexpected number of name records');
    CheckEquals('1.2.3.4', Names[0].IPAddress, 'First name record');
    CheckEquals('12::34',  Names[1].IPAddress, 'Second name record');
  finally
    Names.Free;
  end;
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
//* TestTIdQueryOptionsData                                                    *
//******************************************************************************
//* TestTIdQueryOptionsData Public methods *************************************

procedure TestTIdQueryOptionsData.SetUp;
var
  R: TIdSipResponse;
begin
  inherited SetUp;

  Self.Data := TIdQueryOptionsData.Create;
  Self.Data.Handle := $decafbad;

  R := TIdSipResponse.ReadResponseFrom(BasicResponse);
  try
    Self.Data.Response := R;
  finally
    R.Free;
  end;
end;

procedure TestTIdQueryOptionsData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdQueryOptionsData Published methods **********************************

procedure TestTIdQueryOptionsData.TestAsString;
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
      Expected.Insert(1, EventNames(CM_QUERY_OPTIONS_RESPONSE));
      Expected.Insert(2, 'Response:');
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

procedure TestTIdQueryOptionsData.TestCopy;
var
  Copy: TIdQueryOptionsData;
begin
  Copy := Self.Data.Copy as TIdQueryOptionsData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    Check(Self.Data.Response.Equals(Copy.Response), 'Response');            
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdDebugExceptionData                                                  *
//******************************************************************************
//* TestTIdDebugExceptionData Public methods ***********************************

procedure TestTIdDebugExceptionData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdDebugExceptionData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Error  := 'No Error';
  Self.Data.Reason := 'Some Arb Reason';
end;

procedure TestTIdDebugExceptionData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdDebugExceptionData Published methods ********************************

procedure TestTIdDebugExceptionData.TestCopy;
var
  Copy: TIdDebugExceptionData;
begin
  Copy := Self.Data.Copy as TIdDebugExceptionData;
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
//* TestTIdDebugWaitExceptionData                                              *
//******************************************************************************
//* TestTIdDebugWaitExceptionData Public methods *******************************

procedure TestTIdDebugWaitExceptionData.SetUp;
begin
  inherited SetUp;

  Self.Wait := TIdSipReconfigureStackWait.Create;
  Self.Wait.Configuration.Add('First line');

  Self.Data := TIdDebugWaitExceptionData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Error  := 'EAccessViolation';
  Self.Data.Reason := 'Dangling Pointer';
  Self.Data.Wait   := Self.Wait;
end;

procedure TestTIdDebugWaitExceptionData.TearDown;
begin
  Self.Data.Free;
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdDebugWaitExceptionData Private methods ******************************

procedure TestTIdDebugWaitExceptionData.ExpectedAsString(Expected: TStrings);
begin
    Expected.Add(Self.Data.Error + ': ' + Self.Data.Reason);
    Expected.Add('Wait type: ' + Self.Data.Wait.ClassName);
    Expected.Insert(0, '');
    Expected.Insert(1, EventNames(CM_DEBUG_WAIT_EXCEPTION));
end;

//* TestTIdDebugWaitExceptionData Published methods ****************************

procedure TestTIdDebugWaitExceptionData.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Self.ExpectedAsString(Expected);
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

procedure TestTIdDebugWaitExceptionData.TestAsStringWithNoWait;
var
  Expected: TStrings;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Self.ExpectedAsString(Expected);
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

procedure TestTIdDebugWaitExceptionData.TestCopy;
var
  Copy: TIdDebugWaitExceptionData;
begin
  Copy := Self.Data.Copy as TIdDebugWaitExceptionData;
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
    CheckEquals(Self.Data.Wait.ClassType,
                Copy.Wait.ClassType,
                'Wait type');
    CheckEquals(TIdSipReconfigureStackWait(Self.Data.Wait).Configuration.Text,
                TIdSipReconfigureStackWait(Copy.Wait).Configuration.Text,
                'Wait contents');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdDebugWaitExceptionData.TestCopyFromNewObject;
var
  NewObject: TIdDebugWaitExceptionData;
begin
  NewObject := TIdDebugWaitExceptionData.Create;
  try
    Self.Data.Assign(NewObject);

    CheckNull(Self.Data.Wait, 'Wait object not nilled');
  finally
    NewObject.Free;
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
  Self.Data.LocalContact.Value       := 'sip:giftshop.hilton.tr;transport=tcp';
  Self.Data.LocalMimeType            := '2';
  Self.Data.LocalParty.Value         := 'sip:case@fried-neurons.org';
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
    CheckEquals(Self.Data.LocalContact.FullValue,
                Copy.LocalContact.FullValue,
                'LocalContact');
    CheckEquals(Self.Data.LocalMimeType,
                Copy.LocalMimeType,
                'LocalMimeType');
    CheckEquals(Self.Data.LocalParty.FullValue,
                Copy.LocalParty.FullValue,
                'LocalParty');
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
                'RemoteParty');
    CheckEquals(Self.Data.RemoteSessionDescription,
                Copy.RemoteSessionDescription,
                'RemoteSessionDescription');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdSessionData.TestSetLocalPartyStripsTagParam;
var
  Copy: TIdSessionData;
begin
  Self.Data.LocalParty.Params[TagParam] := 'foofoo';

  Copy := TIdSessionData.Create;
  try
    Copy.LocalParty := Self.Data.LocalParty;
    Check(not Copy.LocalParty.HasParameter(TagParam), 'Tag param not removed');
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
//* TestTIdInboundCallData                                                     *
//******************************************************************************
//* TestTIdInboundCallData Public methods **************************************

procedure TestTIdInboundCallData.SetUp;
var
  Inv: TIdSipRequest;
begin
  inherited SetUp;

  Self.Data := TIdInboundCallData.Create;
  Self.Data.Handle := $decafbad;

  Inv := TIdSipRequest.ReadRequestFrom(BasicRequest);
  try
    Self.Data.Invite                   := Inv;
    Self.Data.LocalMimeType            := Inv.ContentType;
    Self.Data.LocalSessionDescription  := '1';
    Self.Data.RemoteContact            := Inv.FirstContact;
    Self.Data.RemoteMimeType           := Inv.ContentType;
    Self.Data.RemoteParty              := Inv.From;
    Self.Data.RemoteSessionDescription := Inv.Body;
  finally
    Inv.Free;
  end;
end;

procedure TestTIdInboundCallData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdInboundCallData Published methods ***********************************

procedure TestTIdInboundCallData.TestCopy;
var
  Copy: TIdInboundCallData;
begin
  Copy := Self.Data.Copy as TIdInboundCallData;
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
                'RemoteParty');
    CheckEquals(Self.Data.RemoteSessionDescription,
                Copy.RemoteSessionDescription,
                'RemoteSessionDescription');
    Check(Self.Data.Invite.Equals(Copy.Invite),
                'Invite');
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

  Self.Subscribe := TIdSipTestResources.CreateBasicRequest;
  Self.Subscribe.Method             := MethodSubscribe;
  Self.Subscribe.CSeq.Method        := Self.Subscribe.Method;
  Self.Subscribe.FirstContact.Value := 'sip:machine-1@internet-cafe.org>';
  Self.Subscribe.From.Value         := 'Case <sip:case@fried-neurons.org>';
  Self.Subscribe.ReferTo.Value      := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna';
  Self.Subscribe.RequestUri.Uri     := 'sip:case@fried-neurons.org;grid="foo"';

  Self.Data := TIdSubscriptionRequestData.Create;
  Self.Data.EventPackage        := PackageRefer;
  Self.Data.Handle              := $decafbad;
  Self.Data.Request             := Self.Subscribe;
end;

procedure TestTIdSubscriptionRequestData.TearDown;
begin
  Self.Data.Free;
  Self.Subscribe.Free;

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
    Check(Self.Data.Request.Equals(Copy.Request),
                'Request');            
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
//* TestTIdStackReconfiguredData
//******************************************************************************
//* TestTIdStackReconfiguredData Public methods ********************************

procedure TestTIdStackReconfiguredData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdStackReconfiguredData.Create;
  Self.Data.ActsAsRegistrar := true;
  Self.Data.RoutingTableType := 'TIdWindowsRoutingTable';
end;

procedure TestTIdStackReconfiguredData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdStackReconfiguredData Published methods *****************************

procedure TestTIdStackReconfiguredData.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Expected.Add('ActsAsRegistrar: True');
      Expected.Add('RoutingTableType: ' + Self.Data.RoutingTableType);
      Expected.Insert(0, '');
      Expected.Insert(1, EventNames(CM_STACK_RECONFIGURED));
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

procedure TestTIdStackReconfiguredData.TestCopy;
var
  Copy: TIdStackReconfiguredData;
begin
  Copy := Self.Data.Copy as TIdStackReconfiguredData;
  try
    CheckEquals(Self.Data.ActsAsRegistrar,  Copy.ActsAsRegistrar,  'ActsAsRegistrar');
    CheckEquals(Self.Data.RoutingTableType, Copy.RoutingTableType, 'RoutingTableType');
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

initialization
  RegisterTest('SIP stack interface tests', Suite);
end.
