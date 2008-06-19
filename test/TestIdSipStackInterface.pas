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
    procedure CheckAuthentication(AuthType: TIdSipAuthorizationHeaderClass);
    procedure CheckRedirectCall(Temporary: Boolean);
    procedure ClearPendingStackStartedNotification;
    procedure ConfigureToUseRegistrar(Intf: TIdSipStackInterface; RegistrarUri: String);
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
    procedure ReceiveChallenge(Register: TIdSipRequest);
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
    procedure TestAuthenticateSchedulesWait;
    procedure TestAuthenticateWithProxySchedulesWait;
    procedure TestAutoregistrationNotifiesOfAuthenticationChallenge;
    procedure TestCreateNotifiesOfReconfiguration;
    procedure TestEndedSession;
    procedure TestEstablishedSessionInboundCall;
    procedure TestEstablishedSessionOutboundCall;
    procedure TestHangUp;
    procedure TestHangUpWithInvalidHandle;
    procedure TestHangUpWithNonExistentHandle;
    procedure TestInboundCall;
    procedure TestInternallyGeneratedActionsHaveHandles;
    procedure TestInviteChallengeDoesntNotifyTwice;
    procedure TestMakeCall;
    procedure TestMakeCallMalformedAddress;
    procedure TestMakeCallMalformedFrom;
    procedure TestMakeOptionsQuery;
    procedure TestMakeOptionsQueryMalformedAddress;
    procedure TestMakeRegistration;
    procedure TestMakeRegistrationMultiple;
    procedure TestMakeRegistrationMultipleMalformedContact;
    procedure TestMakeSubscription;
    procedure TestMakeSubscriptionMalformedTarget;
    procedure TestMakeSubscriptionNoSubscribeSupport;
{
    procedure TestModifyCall;
}
    procedure TestModifyCallWithInvalidHandle;
    procedure TestModifyCallWithNonExistentHandle;
    procedure TestNetworkFailure;
    procedure TestNotifyOfAsyncMessageResult;
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
    procedure TestSendProvisional;
    procedure TestSendProvisionalWithInvalidHandle;
    procedure TestSendProvisionalWithNonExistentHandle;
//    procedure TestSessionModifiedByRemoteSide;
    procedure TestStackListensToSubscribeModule;
    procedure TestStackListensToSubscribeModuleAfterReconfigure;
    procedure TestStackReceivesExceptionNotifications;
    procedure TestTerminateOccursInStackThread;
    procedure TestTerminateAction;
    procedure TestTerminateActionWithNonExistentHandle;
  end;

  TStackInterfaceExtensionTestCase = class(TStackInterfaceTestCase)
  protected
    Configuration: TStrings;
    Iface:         TIdSipStackInterface;
    LocalAddress:  String;
    LocalPort:     Integer;

    procedure GetConfiguration(Conf: TStrings); virtual;
    function  CreateStackInterface: TIdSipStackInterface;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipColocatedRegistrarExtension = class(TStackInterfaceExtensionTestCase)
  private
    Contact:      String;
    Contacts:     TIdSipContacts;
    Reg:          TIdSipColocatedRegistrarExtension;
    Target:       TIdSipUri;

    procedure ReceiveRegister(FromUri: TIdSipUri; Contact: String);
  protected
    procedure GetConfiguration(Conf: TStrings); override;
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
    NS: TIdSipNameServerExtension;

    procedure ReconfigureStack(Intf: TIdSipStackInterface);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLocalAddressForEmptyString;
    procedure TestLocalAddressForFQDN;
    procedure TestLocalAddressForNonFQDN;
    procedure TestLocalOrMappedAddressForEmptyString;
    procedure TestLocalOrMappedAddressForFQDN;
    procedure TestLocalOrMappedAddressForNonFQDN;
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
    procedure TestNotAnError;
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
    procedure TestAsString;
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

  TSessionDataTestCase = class(TTestCase)
  protected
    procedure SetTestData(Data: TIdSessionData); virtual;
  end;

  TestTIdSessionData = class(TSessionDataTestCase)
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

  TestTIdEstablishedSessionData = class(TSessionDataTestCase)
  private
    Data: TIdEstablishedSessionData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCopy;
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

  TestTIdAsynchronousMessageResultData = class(TTestCase)
  private
    Data: TIdAsynchronousMessageResultData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCopy;
  end;

  TestTIdBooleanResultData = class(TTestCase)
  private
    Data: TIdBooleanResultData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCopy;
  end;

  TestTIdDomainNameRecordsResultData = class(TTestCase)
  private
    Data: TIdDomainNameRecordsResultData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCopy;
  end;

  TestTIdGetBindingsData = class(TTestCase)
  private
    Data: TIdGetBindingsData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCopy;
  end;

  TestTIdStringResultData = class(TTestCase)
  private
    Data: TIdStringResultData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCopy;
  end;

  TestTIdStringDictionaryResultData = class(TTestCase)
  private
    Data: TIdStringDictionaryResultData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCopy;
  end;

  TStackWaitTestCase = class(TStackInterfaceTestCase)
  protected
    BindingIP:   String;
    BindingPort: Cardinal;
    Conf:        TStrings;
    Stack:       TIdSipStackInterface;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckNoUdpServerOnPort(Host: String;
                                     Port: Cardinal;
                                     Msg: String);
    procedure CheckUdpServerOnPort(Host: String;
                                   Port: Cardinal;
                                   Msg: String);
 end;

  TestTIdStackShutdownWait = class(TStackWaitTestCase)
  private
    Wait:  TIdStackShutdownWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
  end;

  TestTIdSipStackReconfigureStackInterfaceWait = class(TStackWaitTestCase)
  private
    Wait: TIdSipStackReconfigureStackInterfaceWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetConfiguration;
    procedure TestTriggerStartsTransports;
  end;

  TestTIdGetBindingsWait = class(TStackWaitTestCase)
  private
    Wait: TIdGetBindingsWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
  end;

  TestTIdIsSourceOfWait = class(TStackWaitTestCase)
  private
    Dest: TIdSipAddressHeader;
    From: TIdSipAddressHeader;
    Wait: TIdIsSourceOfWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTriggerIsSource;
    procedure TestTriggerIsNotSource;
  end;

  TIdNetworkingMessageWaitTestCase = class(TStackWaitTestCase)
  protected
    LocalAddress: String;
    NatAddress:   String;
    VpnAddress:   String;

    procedure ConfigureNat;
  public
    procedure SetUp; override;
  end;

  TestTIdLocalAddressForWait = class(TIdNetworkingMessageWaitTestCase)
  private
    Wait: TIdLocalAddressForWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTriggerWithNat;
    procedure TestTriggerWithNoNat;
  end;

  TestTIdLocalOrMappedAddressForWait = class(TIdNetworkingMessageWaitTestCase)
  private
    Wait: TIdLocalOrMappedAddressForWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTriggerWithNat;
    procedure TestTriggerWithNoNat;
  end;

  TestTIdResolveNamesForWait = class(TIdNetworkingMessageWaitTestCase)
  private
    Host:        String;
    IPv4Address: String;
    IPv6Address: String;
    Wait:        TIdResolveNamesForWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
  end;

  TestTIdCollectStatisticsWait = class(TStackWaitTestCase)
  private
    Wait: TIdCollectStatisticsWait;

    procedure SendOptions;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
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
  IdConnectionBindings, IdRandom, IdSimpleParser, IdSipCore, IdSipDns,
  IdSipLocation, IdSipMockBindingDatabase, IdSipRegistration, IdSipTransport,
  IdSipUdpTransport, SysUtils, TestMessages;

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
  Result.AddTest(TestTIdEstablishedSessionData.Suite);
  Result.AddTest(TestTIdInboundCallData.Suite);
  Result.AddTest(TestTIdSubscriptionRequestData.Suite);
  Result.AddTest(TestTIdResubscriptionData.Suite);
  Result.AddTest(TestTIdSessionReferralData.Suite);
  Result.AddTest(TestTIdSubscriptionNotifyData.Suite);
  Result.AddTest(TestTIdFailedSubscriptionData.Suite);
  Result.AddTest(TestTIdStackReconfiguredData.Suite);
  Result.AddTest(TestTIdAsynchronousMessageResultData.Suite);
  Result.AddTest(TestTIdBooleanResultData.Suite);
  Result.AddTest(TestTIdDomainNameRecordsResultData.Suite);
  Result.AddTest(TestTIdGetBindingsData.Suite);
  Result.AddTest(TestTIdStringResultData.Suite);
  Result.AddTest(TestTIdStringDictionaryResultData.Suite);
  Result.AddTest(TestTIdStackShutdownWait.Suite);
  Result.AddTest(TestTIdSipStackReconfigureStackInterfaceWait.Suite);
  Result.AddTest(TestTIdGetBindingsWait.Suite);
  Result.AddTest(TestTIdIsSourceOfWait.Suite);
  Result.AddTest(TestTIdLocalAddressForWait.Suite);
  Result.AddTest(TestTIdLocalOrMappedAddressForWait.Suite);
  Result.AddTest(TestTIdResolveNamesForWait.Suite);
  Result.AddTest(TestTIdCollectStatisticsWait.Suite);
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
  Timer:     TIdDebugTimerQueue;
begin
  EmptyConf := TStringList.Create;
  try
    Timer := TIdDebugTimerQueue.Create(true);
    try
      Stack := TIdSipStackInterface.Create(0, Timer, EmptyConf);
      try
        // This test tries catches a (now squashed) bug: when no subscribe module
        // was attached to the stack we'd get an Invalid Cast exception.
      finally
        Stack.Free;
      end;
    finally
      Timer.Terminate;
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
  T:         TIdSipTransport;
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

  T := TIdSipDebugTransportRegistry.TransportRunningOn(Self.TargetAddress, Self.TargetPort);
  Check(T is TIdSipMockTransport, 'Unexpected transport type (' + T.ClassName + ') running on ' + Self.TargetAddress + ':' + IntToStr(Self.TargetPort));
  Self.RemoteMockTransport := T as TIdSipMockTransport;
  CheckEquals(Self.TargetAddress, Self.RemoteMockTransport.FirstIPBound, 'DebugTransportRegistry messed up finding RemoteMockTransport');

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

  T := TIdSipDebugTransportRegistry.TransportRunningOn(Self.LocalAddress, Self.LocalPort);
  Check(T is TIdSipMockTransport, 'Unexpected transport type (' + T.ClassName + ') running on ' + Self.LocalAddress + ':' + IntToStr(Self.LocalPort));
  Self.MockTransport := T as TIdSipMockTransport;
  CheckEquals(Self.LocalAddress, Self.MockTransport.FirstIPBound, 'DebugTransportRegistry messed up finding MockTransport');

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

procedure TestTIdSipStackInterface.CheckAuthentication(AuthType: TIdSipAuthorizationHeaderClass);
var
  Creds: TIdSipAuthorizationHeader;
  H:     TIdSipHandle;
begin
  // Send an INVITE. Get a challenge. Make sure that the resent INVITE+creds is
  // actually sent in the context of the stack thread.
  H := Self.Intf.MakeCall(Self.From, Self.Destination, '', '', 70);
  Self.MarkSentRequestCount;
  Self.Intf.Send(H);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  CheckRequestSent('No request sent');

  // Really, this should EITHER be a 401 OR a 407, but we're testing the sending
  // of a request; we don't particularly care WHICH kind of challenge we've
  // received.
  Self.ReceiveChallenge(Self.LastSentRequest);
  Self.ProcessAllPendingNotifications;

  Creds := AuthType.Create;
  try
    Self.MarkSentRequestCount;
    Self.Intf.Authenticate(H, Creds);
    CheckNoRequestSent('Request sent, hence sending did NOT occur in context of stack thread');

    Check(nil <> Self.TimerQueue.LastEventScheduled(TIdSipActionAuthenticateWait),
          'No authentication wait scheduled');
    Self.TimerQueue.TriggerAllEventsUpToFirst(TIdSipActionAuthenticateWait);
    Check(Self.LastSentRequest.HasHeader(Creds.Name),
          Format('No %s header', [Creds.Name]));
  finally
    Creds.Free;
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

procedure TestTIdSipStackInterface.ConfigureToUseRegistrar(Intf: TIdSipStackInterface; RegistrarUri: String);
var
  Conf: TStrings;
begin
  Conf := TStringList.Create;
  try
    Conf.Add(RegisterDirective + ': ' + RegistrarUri);

    Intf.ReconfigureStack(Conf);
    Self.TimerQueue.TriggerAllEventsUpToFirst(TIdSipStackReconfigureStackInterfaceWait);
  finally
    Conf.Free;
  end;
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
  Result := Self.RemoteUA.InviteModule.CreateInvite(Self.RemoteUA.From, Self.Destination, '', '', TIdSipRequest.DefaultMaxForwards);
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
                               Self.LocalMimeType,
                               TIdSipRequest.DefaultMaxForwards);

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

procedure TestTIdSipStackInterface.ReceiveChallenge(Register: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Register);
  try
    Response.StatusCode := SIPUnauthorized;
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
  Target: TIdConnectionBindings;
begin
  Self.Requests.AddCopy(Request);

  Target := TIdConnectionBindings.Create;
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
  Target: TIdConnectionBindings;
begin
  Self.Responses.AddCopy(Response);

  Target := TIdConnectionBindings.Create;
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

    Subscribe := SubMod.CreateSubscribe(Self.RemoteUA.From, LocalFrom, EventPackage, TIdSipRequest.DefaultMaxForwards);
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
  H := Self.Intf.MakeCall(Self.From, Self.Destination, '', '', TIdSipRequest.DefaultMaxForwards);

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

procedure TestTIdSipStackInterface.TestAuthenticateSchedulesWait;
begin
  Self.CheckAuthentication(TIdSipAuthorizationHeader);
end;

procedure TestTIdSipStackInterface.TestAuthenticateWithProxySchedulesWait;
begin
  Self.CheckAuthentication(TIdSipProxyAuthorizationHeader);
end;

procedure TestTIdSipStackInterface.TestAutoregistrationNotifiesOfAuthenticationChallenge;
var
  Reg: TStrings;
begin
  // The StackInterface doesn't create autoregistration actions (actions
  // generated as a result of the Register directive), but when these
  // REGISTERs are challenged, the user still needs to know of the
  // challenge. This test shows that the user will receive a
  // CM_AUTHENTICATION_CHALLENGE.

  Reg := TStringList.Create;
  try
    Reg.Add(RegisterDirective + ': ' + Self.Registrar.AsString);
    Self.Intf.ReconfigureStack(Reg);

    // Fire off the REGISTER
    Self.TimerQueue.TriggerAllEventsUpToFirst(TIdSipReregisterWait);
    Self.ReceiveChallenge(Self.LastSentRequest);
    Self.ProcessAllPendingNotifications;

    CheckNotificationReceived(TIdAuthenticationChallengeData, 'Authentication challenge notification not received');
  finally
    Reg.Free;
  end;
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
  Call := Self.Intf.MakeCall(Self.From, Self.Destination, Self.LocalOffer, Self.LocalMimeType, TIdSipRequest.DefaultMaxForwards);
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

procedure TestTIdSipStackInterface.TestInternallyGeneratedActionsHaveHandles;
const
  RegistrarUri = 'sip:10.0.0.1';//gw1.leo-ix.net';
var
  Data: TIdAuthenticationChallengeData;
begin
  Self.ConfigureToUseRegistrar(Self.Intf, RegistrarUri);

  Self.MarkSentRequestCount;
  Self.TimerQueue.TriggerAllEventsUpToFirst(TIdSipReregisterWait);
  CheckRequestSent('No request sent - Wait didn''t fire?');

  Self.ReceiveChallenge(Self.LastSentRequest);
  Self.ProcessAllPendingNotifications;

  CheckNotificationReceived(TIdAuthenticationChallengeData, 'No challenge notification received');
  Data := Self.LastEventOfType(TIdAuthenticationChallengeData) as TIdAuthenticationChallengeData;
  CheckNotEquals(IntToHex(InvalidHandle, 8), IntToHex(Data.Handle, 8), 'Internally-generated action has no handle');
end;

procedure TestTIdSipStackInterface.TestInviteChallengeDoesntNotifyTwice;
var
  Handle: TIdSipHandle;
begin
  // Make a call. Receive a 401 Unauthorized. Show that the stack interface
  // receives only one challenge notification.

  Handle := Self.Intf.MakeCall(Self.From, Self.Destination, '', '', TIdSipRequest.DefaultMaxForwards);
  Self.MarkSentRequestCount;
  Self.Intf.Send(Handle);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  CheckRequestSent('No request sent');

  Self.ReceiveChallenge(Self.LastSentRequest);
  Self.ProcessAllPendingNotifications;

  CheckNotificationsReceived(TIdAuthenticationChallengeData, 1,
                             'Authentication challenge notification received for owned and owning action');
end;

procedure TestTIdSipStackInterface.TestMakeCall;
const
  MaxForwards = 42;
var
  ActualFrom: TIdSipFromHeader;
  Handle:     TIdSipHandle;
begin
  Handle := Self.Intf.MakeCall(Self.From, Self.Destination, '', '', MaxForwards);

  Self.MarkSentRequestCount;
  Self.Intf.Send(Handle);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  CheckRequestSent('No request sent');
  CheckEquals(MethodInvite, Self.MockTransport.LastRequest.Method, 'Unexpected request sent');
  CheckEquals(MaxForwards, Self.MockTransport.LastRequest.MaxForwards, 'Max-Forwards');

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

    Handle := Self.Intf.MakeCall(Self.From, MalformedAddress, '', '', TIdSipRequest.DefaultMaxForwards);
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

    Handle := Self.Intf.MakeCall(MalformedAddress, Self.Destination, '', '', TIdSipRequest.DefaultMaxForwards);
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

procedure TestTIdSipStackInterface.TestMakeRegistrationMultiple;
const
  FirstContact  = 'sip:case@1.2.3.4';
  SecondContact = 'sip:anon@jakes-cafe.net';
var
  Contacts:     TIdSipContacts;
  Handle:       TIdSipHandle;
  SentContacts: TIdSipContacts;
begin
  Contacts := TIdSipContacts.Create;
  try
    Contacts.Add(ContactHeaderFull).Value := FirstContact;
    Contacts.Add(ContactHeaderFull).Value := SecondContact;

    Handle := Self.Intf.MakeRegistration(Self.Registrar, Contacts);

    Self.MarkSentRequestCount;
    Self.Intf.Send(Handle);
    Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
    CheckRequestSent('No request sent');
    CheckEquals(MethodRegister, Self.LastSentRequest.Method, 'Unexpected request sent');

    SentContacts := Self.LastSentRequest.Contacts;
    CheckEquals(Contacts.Count, SentContacts.Count, 'Incorrect number of contacts sent');
    Contacts.First;
    SentContacts.First;
    while Contacts.HasNext do begin
      CheckEquals(Contacts.CurrentContact.FullValue,
                  SentContacts.CurrentContact.FullValue,
                  'Unexpected Contact');

      Contacts.Next;
      SentContacts.Next;
    end;
  finally
    Contacts.Free;
  end;
end;

procedure TestTIdSipStackInterface.TestMakeRegistrationMultipleMalformedContact;
var
  Contacts: TIdSipContacts;
begin
  Contacts := TIdSipContacts.Create;
  try
    Contacts.Add(ContactHeaderFull).Value := 'sip::';

    CheckEquals(IntToHex(InvalidHandle, 8),
                IntToHex(Self.Intf.MakeRegistration(Self.Registrar, Contacts), 8),
                'Valid handle returned when trying to register with a malformed Contact');
  finally
    Contacts.Free;
  end;
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
  Call := Self.Intf.MakeCall(Self.From, Self.Destination, '', '', TIdSipRequest.DefaultMaxForwards);
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

procedure TestTIdSipStackInterface.TestNotifyOfAsyncMessageResult;
var
  FakeResult:   TIdAsynchronousMessageResultData;
  Notification: TIdAsynchronousMessageResultData;
begin
  FakeResult := TIdAsynchronousMessageResultData.Create;
  try
    FakeResult.ReferenceID := 'fake ID';

    Self.Intf.NotifyOfAsyncMessageResult(FakeResult);

    Self.ProcessAllPendingNotifications;

    CheckNotificationReceived(TIdAsynchronousMessageResultData, 'No async message result notification received');

    Notification := Self.LastEventOfType(TIdAsynchronousMessageResultData) as TIdAsynchronousMessageResultData;
    CheckEquals(FakeResult.ReferenceID, Notification.ReferenceID, 'Wrong result ID');
  finally
    FakeResult.Free;
  end;
end;

procedure TestTIdSipStackInterface.TestOutboundCall;
var
  H:           TIdSipHandle;
  SessionData: TIdEstablishedSessionData;
begin
  H := Self.Intf.MakeCall(Self.From,
                          Self.Destination,
                          Self.LocalOffer,
                          Self.LocalMimeType,
                          TIdSipRequest.DefaultMaxForwards);
  // Send the INVITE
  Self.Intf.Send(H);
  Self.MarkSentRequestCount;
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  CheckRequestSent('No INVITE sent');
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
const
  Address = '127.0.0.1';
  Port    = 5060;
var
  Conf: TStrings;
begin
  Conf := TStringList.Create;
  try
    Conf.Add(ListenDirective + ': UDP ' + Address + ':' + IntToStr(Port));

    Self.Intf.ReconfigureStack(Conf);
    Self.TimerQueue.TriggerAllEventsOfType(TIdSipStackReconfigureStackInterfaceWait);
    Self.MockTransport := TIdSipDebugTransportRegistry.TransportRunningOn(Address, Port) as TIdSipMockTransport;
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
    CheckEquals(NewConf.Count, Recon.RawConfiguration.Count, 'RawConfiguration data missing');
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

procedure TestTIdSipStackInterface.TestSendProvisional;
const
  ReasonPhrase = 'Some progress';
  StatusCode   = SIPQueued;
begin
  Self.ReceiveInviteWithOffer(Self.RemoteOffer, Self.RemoteMimeType);
  Self.ProcessAllPendingNotifications;
  CheckNotificationReceived(TIdInboundCallData, 'No inbound call notification received');

  Self.MarkSentResponseCount;
  Self.Intf.SendProvisional(Self.LastEventOfType(TIdInboundCallData).Handle, StatusCode, ReasonPhrase);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipSendProvisionalWait);
  CheckResponseSent('No response sent');

  CheckEquals(StatusCode,   Self.LastSentResponse.StatusCode, 'Status-Code ignored');
  CheckEquals(ReasonPhrase, Self.LastSentResponse.StatusText, 'Reason-Phrase ignored');
end;

procedure TestTIdSipStackInterface.TestSendProvisionalWithInvalidHandle;
var
  H: TIdSipHandle;
begin
  H := Self.Intf.MakeCall(Self.From, Self.Destination, '', '', TIdSipRequest.DefaultMaxForwards);

  try
    // Of course, you can't send a progress response for an outbound call.
    Self.Intf.SendProvisional(H);

    Fail('No exception raised for an invalid handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestSendProvisionalWithNonExistentHandle;
const
  ArbValue = 42;
begin
  try
    Self.Intf.SendProvisional(ArbValue);
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
  Conf:      TStrings;
  LocalPort: Cardinal;
  Package:   TIdSipEventPackageClass;
  Stack:     TIdSipStackInterface;
begin
  Package := TIdSipTargetDialogPackage;

  Self.SetUpPackageSupport(Package);
  try
    Conf := TStringList.Create;
    try
      LocalPort := Self.LocalPort + 1;
      Conf.Add('Listen: UDP ' + Self.LocalAddress + ':' + IntToStr(LocalPort));
      Conf.Add('NameServer: MOCK;ReturnOnlySpecifiedRecords');
      Conf.Add('Contact: sip:foo@' + Self.LocalAddress + ':' + IntToStr(LocalPort));
      Conf.Add('From: sip:foo@' + Self.LocalAddress + ':' + IntToStr(LocalPort));
      Conf.Add('SupportEvent: ' + Package.EventPackage);

      Stack := TIdSipStackInterface.Create(Self.UI.Handle, Self.TimerQueue, Conf);
      try
        // This is expedient, but evil: it works because Self.MockTransport will
        // be reset in SetUp when the next test runs.
        Self.MockTransport := TIdSipDebugTransportRegistry.TransportRunningOn(Self.LocalAddress, LocalPort) as TIdSipMockUdpTransport;

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

procedure TestTIdSipStackInterface.TestTerminateOccursInStackThread;
var
  NoConf: TStrings;
  S:      TIdSipStackInterface;
  W:      TIdWait;
begin
  NoConf := TStringList.Create;
  try
    S := TIdSipStackInterface.Create(Self.UI.Handle, Self.TimerQueue, NoConf);
    try
      S.Terminate;
      W := Self.TimerQueue.LastEventScheduled(TIdStackShutdownWait);
      Check(W <> nil, 'No shutdown Wait scheduled');

      // Remove the shutdown event so we can free S ourselves.
      Self.TimerQueue.RemoveAllEvents;
    finally
      S.Free;
    end;
  finally
    NoConf.Free;
  end;
end;

procedure TestTIdSipStackInterface.TestTerminateAction;
var
  Handle:  TIdSipHandle;
  Notify:  TIdSipRequest;
  Package: TIdSipEventPackageClass;
begin
  Package := TIdSipTargetDialogPackage;

  Self.SetUpPackageSupport(Package);
  try
    Self.AddSubscribeSupport(Self.Intf, Package.EventPackage);

    Self.ReceiveSubscribe(Package.EventPackage);
    Self.ProcessAllPendingNotifications;
    CheckNotificationReceived(TIdSubscriptionRequestData, 'No subscription request notification received');
    Handle := Self.LastEventOfType(TIdSubscriptionRequestData).Handle;

    Self.MarkSentRequestCount;
    Self.Intf.Terminate(Handle);
    Self.TimerQueue.TriggerAllEventsUpToFirst(TIdSipActionTerminateWait);
    CheckRequestSent('No request sent');

    Notify := Self.LastSentRequest;
    CheckEquals(MethodNotify, Notify.Method, 'Unexpected request sent');
    Check(Notify.HasHeader(SubscriptionStateHeader),
          'NOTIFY lacks a Subscription-State header');
    CheckEquals(SubscriptionSubstateTerminated,
                Notify.SubscriptionState.SubState,
                'NOTIFY didn''t terminate');
  finally
    Self.TearDownPackageSupport(Package);
  end;
end;

procedure TestTIdSipStackInterface.TestTerminateActionWithNonExistentHandle;
const
  ArbValue = 42;
begin
  try
    Self.Intf.Terminate(ArbValue);
    Fail('No exception raised for a non-existent handle');
  except
    on EInvalidHandle do;
  end;
end;

//******************************************************************************
//* TStackInterfaceExtensionTestCase                                           *
//******************************************************************************
//* TStackInterfaceExtensionTestCase Public methods ****************************

procedure TStackInterfaceExtensionTestCase.SetUp;
var
  T: TIdSipTransport;
begin
  inherited SetUp;

  TIdSipTransportRegistry.RegisterTransportType(UdpTransport, TIdSipMockUdpTransport);

  Self.LocalAddress := '127.0.0.1';
  Self.LocalPort    := 5060;

  Self.Configuration := TStringList.Create;
  Self.Iface         := Self.CreateStackInterface;

  T := TIdSipDebugTransportRegistry.TransportRunningOn(Self.LocalAddress, Self.LocalPort);
  Check(T is TIdSipMockTransport, 'Unexpected transport type (' + T.ClassName + ') running on ' + Self.LocalAddress + ':' + IntToStr(Self.LocalPort));
  Self.MockTransport := T as TIdSipMockTransport;
end;

procedure TStackInterfaceExtensionTestCase.TearDown;
begin
  Self.Iface.Free;
  Self.Configuration.Free;

  TIdSipTransportRegistry.UnregisterTransportType(UdpTransport);

  inherited TearDown;
end;

//* TStackInterfaceExtensionTestCase Protected methods *************************

procedure TStackInterfaceExtensionTestCase.GetConfiguration(Conf: TStrings);
begin
  Self.Configuration.Add(ListenDirective           + ': UDP ' + Self.LocalAddress + ':' + IntToStr(Self.LocalPort));
  Self.Configuration.Add(NameServerDirective       + ': ' + MockKeyword);
  Self.Configuration.Add(RoutingTableDirective     + ': ' + MockKeyword);
  Self.Configuration.Add(MockLocalAddressDirective + ': ' + Self.LocalAddress);
end;

function TStackInterfaceExtensionTestCase.CreateStackInterface: TIdSipStackInterface;
begin
  Self.GetConfiguration(Self.Configuration);

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

  Self.Reg    := Self.Iface.AttachExtension(TIdSipColocatedRegistrarExtension) as TIdSipColocatedRegistrarExtension;
  Self.Target := TIdSipUri.Create('sip:case@fried-neurons.org');

  Self.Contact := 'sip:case@tmp.node';
end;

procedure TestTIdSipColocatedRegistrarExtension.TearDown;
begin
  Self.Target.Free;
  Self.Contacts.Free;

  inherited TearDown;
end;

//* TestTIdSipColocatedRegistrarExtension Protected methods ********************

procedure TestTIdSipColocatedRegistrarExtension.GetConfiguration(Conf: TStrings);
begin
  inherited GetConfiguration(Conf);

  Self.Configuration.Add(ActAsRegistrarDirective    + ': yes');
  Self.Configuration.Add(RegistrarDatabaseDirective + ': ' + MockKeyword);
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
  try
    Check(N.UA.UsesModule(TIdSipRegisterModule), 'UA doesn''t support REGISTER method');
    DB := (N.UA.ModuleFor(TIdSipRegisterModule) as TIdSipRegisterModule).BindingDB as TIdSipMockBindingDatabase;

    Self.ReceiveRegister(Self.Target, Self.Contact);

    DB.FailBindingsFor := true;

    Self.Reg.TargetsFor(Self.Target, Self.Contacts);
    Check(Self.Contacts.IsEmpty, 'TargetsFor returned targets when the DB failed');
  finally
    N.Free;
  end;
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
  LanAddress   = '10.0.0.1';
  LocalAddress = '10.0.0.6';
  NatAddress   = '1.2.3.4';
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
  Self.Configuration.Add(MappedRouteDirective + ': 0.0.0.0/0 ' + NatAddress);
  Self.ReconfigureStack(Self.Iface);

  CheckEquals(LocalAddress, Self.NS.LocalOrMappedAddressFor(LanAddress), 'LAN address');
  CheckEquals(LocalAddress, Self.NS.LocalAddressFor(InternetHost),       'Internet host');
  CheckEquals(VpnAddress,   Self.NS.LocalAddressFor(VpnHost),            'VPN host');
end;

procedure TestTIdSipNameServerExtension.TestLocalAddressForNonFQDN;
begin
  Self.ExpectedException := EBadParameter;
  Self.NS.LocalAddressFor('::G');
end;

procedure TestTIdSipNameServerExtension.TestLocalOrMappedAddressForEmptyString;
begin
  Self.ExpectedException := EBadParameter;
  Self.NS.LocalOrMappedAddressFor('');
end;

procedure TestTIdSipNameServerExtension.TestLocalOrMappedAddressForFQDN;
const
  InternetHost = 'tessier-ashpool.co.luna';
  InternetIP   = '1.2.3.4';
  LanAddress   = '10.0.0.1';
  LocalAddress = '10.0.0.6';
  NatAddress   = '1.2.3.4';
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
  Self.Configuration.Add(MappedRouteDirective + ': 0.0.0.0/0 ' + NatAddress);
  Self.ReconfigureStack(Self.Iface);

  CheckEquals(LocalAddress, Self.NS.LocalOrMappedAddressFor(LanAddress),   'LAN address');
  CheckEquals(NatAddress,   Self.NS.LocalOrMappedAddressFor(InternetHost), 'Internet host');
  CheckEquals(VpnAddress,   Self.NS.LocalOrMappedAddressFor(VpnHost),      'VPN host');
end;

procedure TestTIdSipNameServerExtension.TestLocalOrMappedAddressForNonFQDN;
begin
  Self.ExpectedException := EBadParameter;
  Self.NS.LocalOrMappedAddressFor('::G');
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

procedure TestTIdInformationalData.TestNotAnError;
begin
  Self.Data.ErrorCode := 0;
  Check(Self.Data.NotAnError, 'Data thinks it''s an error');

  Self.Data.ErrorCode := 1;
  Check(not Self.Data.NotAnError, 'Data doesn''t think it''s an error');

  Self.Data.ErrorCode := $ffffffff;
  Check(not Self.Data.NotAnError, 'Data doesn''t think it''s an error');
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

  Self.Data.Binding := TIdConnectionBindings.Create;
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
  Self.Data.Binding := TIdConnectionBindings.Create;
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
  Self.Data.Binding := TIdConnectionBindings.Create;
  Self.Data.Binding.LocalIP   := '127.0.0.1';
  Self.Data.Binding.LocalPort := 5060;
  Self.Data.Binding.PeerIP    := '127.0.0.2';
  Self.Data.Binding.PeerPort  := 5061;
  Self.Data.Binding.Transport := 'UDP';

  Self.Data.Handle  := $decafbad;
  Self.Data.Message := TIdSipRequest.Create;
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
    CheckEquals(Self.Data.Binding.AsString,
                Copy.Binding.AsString,
               'The copy''s Binding doesn''t contain the original Binding');
    Check(Copy.Binding <> Self.Data.Binding,
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
  Self.Data.Handle  := $decafbad;
  Self.Data.Binding := TIdConnectionBindings.Create;
  Self.Data.Msg     := 'This contains a (malformed) SIP message';
  Self.Data.Reason  := 'Here''s why it''s malformed';

  Self.Data.Binding.Transport := SctpTransport;
  Self.Data.Binding.LocalIP   := '127.0.0.1';
  Self.Data.Binding.LocalPort := 5060;
  Self.Data.Binding.PeerIP    := '127.0.0.2';
  Self.Data.Binding.PeerPort  := 15060;
end;

procedure TestTIdDebugTransportRejectedMessageData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdDebugTransportRejectedMessageData Published methods *****************

procedure TestTIdDebugTransportRejectedMessageData.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Expected.Add(Self.Data.Binding.AsString);
      Expected.Add(Self.Data.Reason);
      Expected.Add(Self.Data.Msg);

      Expected.Insert(0, '');
      Expected.Insert(1, EventNames(CM_DEBUG_TRANSPORT_REJECTED_MSG));
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

procedure TestTIdDebugTransportRejectedMessageData.TestCopy;
var
  Copy: TIdDebugTransportRejectedMessageData;
begin
  Copy := Self.Data.Copy as TIdDebugTransportRejectedMessageData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    Check(Copy.Binding.Equals(Self.Data.Binding),
          'The copy''s binding doesn''t contain the original binding');
    Check(Copy.Binding <> Self.Data.Binding,
          'The copy contains a reference to the original binding, not a copy');
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

//* TSessionDataTestCase Protected methods *************************************

procedure TSessionDataTestCase.SetTestData(Data: TIdSessionData);
begin
  Data.LocalContact.Value       := 'sip:giftshop.hilton.tr;transport=tcp';
  Data.LocalMimeType            := '2';
  Data.LocalParty.Value         := 'sip:case@fried-neurons.org';
  Data.LocalSessionDescription  := '1';
  Data.RemoteContact.Value      := 'sip:wintermute@terminalhead.tessier-ashpool.co.luna;transport=sctp';
  Data.RemoteMimeType           := '4';
  Data.RemoteParty.Value        := 'sip:wintermute@tessier-ashpool.co.luna';
  Data.RemoteSessionDescription := '3';
end;

//******************************************************************************
//* TestTIdSessionData                                                         *
//******************************************************************************
//* TestTIdSessionData Public methods ******************************************

procedure TestTIdSessionData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdSessionData.Create;
  Self.SetTestData(Self.Data);
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
//* TestTIdEstablishedSessionData                                              *
//******************************************************************************
//* TestTIdEstablishedSessionData Public methods *******************************

procedure TestTIdEstablishedSessionData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdEstablishedSessionData.Create;
  Self.SetTestData(Self.Data);
  Self.Data.ID.CallID    := '1';
  Self.Data.ID.LocalTag  := '2';
  Self.Data.ID.RemoteTag := '3';
end;

procedure TestTIdEstablishedSessionData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdEstablishedSessionData Published methods ****************************

procedure TestTIdEstablishedSessionData.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Expected.Add('Local party: ' + Self.Data.LocalParty.FullValue);
      Expected.Add('Local contact: ' + Self.Data.LocalContact.FullValue);
      Expected.Add('Remote party: ' + Self.Data.RemoteParty.FullValue);
      Expected.Add('Remote contact: ' + Self.Data.RemoteContact.FullValue);
      Expected.Add('Local session description (' + Self.Data.LocalMimeType + '):');
      Expected.Add(Self.Data.LocalSessionDescription);
      Expected.Add('Remote session description (' + Self.Data.RemoteMimeType + '):');
      Expected.Add(Self.Data.RemoteSessionDescription);
      Expected.Add('Dialog ID: ' + Self.Data.ID.AsString);
      Expected.Insert(0, '');
      Expected.Insert(1, EventNames(CM_CALL_ESTABLISHED));
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

procedure TestTIdEstablishedSessionData.TestCopy;
var
  Copy: TIdEstablishedSessionData;
begin
  Copy := Self.Data.Copy as TIdEstablishedSessionData;
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
    CheckEquals(Self.Data.ID.AsString,
                Copy.ID.AsString,
                'ID');
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
var
  Binding: TIdSipLocation;
begin
  inherited SetUp;

  Self.Data := TIdStackReconfiguredData.Create;
  Self.Data.ActsAsRegistrar := true;
  Self.Data.RoutingTableType := 'TIdWindowsRoutingTable';
  Self.Data.RawConfiguration.Add('Raw config goes here');
  Self.Data.RawConfiguration.Add('In many lines');

  Binding := TIdSipLocation.Create;
  try
    Binding.IPAddress := '127.0.0.1';
    Binding.Port      := 5060;
    Binding.Transport := 'UDP';

    Self.Data.Bindings.AddLocation(Binding);
  finally
    Binding.Free;
  end;
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
      Expected.Add('RawConfiguration:');
      Expected.Add(Self.Data.RawConfiguration.Text);
      Expected.Add('ActsAsRegistrar: True');
      Expected.Add(Self.Data.Bindings.AsStringWithPrefix('Binding: '));
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
  I:    Integer;
begin
  Copy := Self.Data.Copy as TIdStackReconfiguredData;
  try
    CheckEquals(Self.Data.ActsAsRegistrar,   Copy.ActsAsRegistrar,   'ActsAsRegistrar');
    CheckEquals(Self.Data.Bindings.AsString, Copy.Bindings.AsString, 'Bindings');
    CheckEquals(Self.Data.RoutingTableType,  Copy.RoutingTableType,  'RoutingTableType');

    CheckEquals(Self.Data.RawConfiguration.Count, Copy.RawConfiguration.Count, 'RawConfiguration line count');
    for I := 0 to Self.Data.RawConfiguration.Count - 1 do
      CheckEquals(Self.Data.RawConfiguration[I], Copy.RawConfiguration[I], Format('RawConfiguration line %d', [I]));
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdAsynchronousMessageResultData                                       *
//******************************************************************************
//* TestTIdAsynchronousMessageResultData Public methods ************************

procedure TestTIdAsynchronousMessageResultData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdAsynchronousMessageResultData.Create;
  Self.Data.Handle      := $decafbad;
  Self.Data.ReferenceID := 'fake ID';
end;

procedure TestTIdAsynchronousMessageResultData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

procedure TestTIdAsynchronousMessageResultData.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Expected.Add('ReferenceID: ' + Self.Data.ReferenceID);
      Expected.Insert(0, '');
      Expected.Insert(1, EventNames(CM_ASYNC_MSG_RESULT));
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

procedure TestTIdAsynchronousMessageResultData.TestCopy;
var
  Copy: TIdAsynchronousMessageResultData;
begin
  Copy := Self.Data.Copy as TIdAsynchronousMessageResultData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8), IntToHex(Copy.Handle, 8), 'Handle');
    CheckEquals(Self.Data.ReferenceID,         Copy.ReferenceID,         'ReferenceID');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdBooleanResultData                                                   *
//******************************************************************************
//* TestTIdBooleanResultData Public methods ************************************

procedure TestTIdBooleanResultData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdBooleanResultData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Result := true;
  Self.Data.ReferenceID := 'badf00d';
end;

procedure TestTIdBooleanResultData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdBooleanResultData Published methods *********************************

procedure TestTIdBooleanResultData.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Expected.Add(''); // Timestamp + Handle
      Expected.Add(''); // Event name
      Expected.Add('ReferenceID: ' + Self.Data.ReferenceID);
      Expected.Add('Result: ' + BoolToStr(Self.Data.Result));

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

procedure TestTIdBooleanResultData.TestCopy;
var
  Copy: TIdBooleanResultData;
begin
  Copy := Self.Data.Copy as TIdBooleanResultData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.Result, Copy.Result, 'Result');
    CheckEquals(Self.Data.ReferenceID, Copy.ReferenceID, 'ReferenceID');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdDomainNameRecordsResultData                                         *
//******************************************************************************
//* TestTIdDomainNameRecordsResultData Public methods **************************

procedure TestTIdDomainNameRecordsResultData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdDomainNameRecordsResultData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.ReferenceID := 'badf00d';

  Self.Data.IPAddresses.Add('A',    'foo.bar', '1.2.3.4');
  Self.Data.IPAddresses.Add('AAAA', 'foo.bar', '2002:deca:fbad::1');
end;

procedure TestTIdDomainNameRecordsResultData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdDomainNameRecordsResultData Published methods ***********************

procedure TestTIdDomainNameRecordsResultData.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Expected.Add(''); // Timestamp + Handle
      Expected.Add(''); // Event name
      Expected.Add('ReferenceID: ' + Self.Data.ReferenceID);
      Expected.Add('IPAddress0: ' + Self.Data.IPAddresses[0].AsString);
      Expected.Add('IPAddress1: ' + Self.Data.IPAddresses[1].AsString);

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

procedure TestTIdDomainNameRecordsResultData.TestCopy;
var
  Copy: TIdDomainNameRecordsResultData;
  I:    Integer;
begin
  Copy := Self.Data.Copy as TIdDomainNameRecordsResultData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');

    CheckEquals(Self.Data.ReferenceID, Copy.ReferenceID, 'ReferenceID');

    for I := 0 to Self.Data.IPAddresses.Count - 1 do
      CheckEquals(Self.Data.IPAddresses[I].AsString,
                  Copy.IPAddresses[I].AsString,
                  'IP address #' + IntToStr(I + 1));
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdGetBindingsData                                                     *
//******************************************************************************
//* TestTIdGetBindingsData Public methods **************************************

procedure TestTIdGetBindingsData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdGetBindingsData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.ReferenceID := 'fake ID';
  Self.Data.Bindings.AddLocation('TCP', '127.0.0.1', 5060);
  Self.Data.Bindings.AddLocation('UDP', '127.0.0.1', 5060);
end;

procedure TestTIdGetBindingsData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdGetBindingsData Published methods ***********************************

procedure TestTIdGetBindingsData.TestAsString;
var
  Expected: TStrings;
  I:        Integer;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Expected.Add('ReferenceID: ' + Self.Data.ReferenceID);
      for I := 0 to Self.Data.Bindings.Count - 1 do
        Expected.Add('Binding' + IntToStr(I) + ': ' + Self.Data.Bindings[I].AsString);
      Expected.Insert(0, '');
      Expected.Insert(1, EventNames(CM_ASYNC_MSG_RESULT));
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

procedure TestTIdGetBindingsData.TestCopy;
var
  Copy: TIdGetBindingsData;
  I:    Integer;
begin
  Copy := Self.Data.Copy as TIdGetBindingsData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8), IntToHex(Copy.Handle, 8), 'Handle');
    CheckEquals(Self.Data.ReferenceID, Copy.ReferenceID, 'ReferenceID');
    CheckEquals(Self.Data.Bindings.Count, Copy.Bindings.Count, 'Bindings size');

    for I := 0 to Self.Data.Bindings.Count - 1 do
      CheckEquals(Self.Data.Bindings[I].AsString, Copy.Bindings[I].AsString, 'Binding #' + IntToStr(I));
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdStringResultData                                                    *
//******************************************************************************
//* TestTIdStringResultData Public methods *************************************

procedure TestTIdStringResultData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdStringResultData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Result := 'Your Answer';
  Self.Data.ReferenceID := 'badf00d';
end;

procedure TestTIdStringResultData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdStringResultData Published methods **********************************

procedure TestTIdStringResultData.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Expected.Add(''); // Timestamp + Handle
      Expected.Add(''); // Event name
      Expected.Add('ReferenceID: ' + Self.Data.ReferenceID);
      Expected.Add('Result: ' + Self.Data.Result);

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

procedure TestTIdStringResultData.TestCopy;
var
  Copy: TIdStringResultData;
begin
  Copy := Self.Data.Copy as TIdStringResultData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.Result, Copy.Result, 'Result');
    CheckEquals(Self.Data.ReferenceID, Copy.ReferenceID, 'ReferenceID');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdStringDictionaryResultData                                          *
//******************************************************************************
//* TestTIdStringDictionaryResultData Public methods ***************************

procedure TestTIdStringDictionaryResultData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdStringDictionaryResultData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Result.Add('question', 'answer');
  Self.Data.ReferenceID := 'badf00d';
end;

procedure TestTIdStringDictionaryResultData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdStringDictionaryResultData Published methods ************************

procedure TestTIdStringDictionaryResultData.TestAsString;
var
  Expected: TStrings;
  I:        Integer;
  KeyNames: TStrings;
  Received: TStrings;
begin
  Expected := TStringList.Create;
  try
    Received := TStringList.Create;
    try
      Expected.Add(''); // Timestamp + Handle
      Expected.Add(''); // Event name
      Expected.Add('ReferenceID: ' + Self.Data.ReferenceID);
      Expected.Add('Result:');

      KeyNames := TStringList.Create;
      try
        Self.Data.Result.CollectKeys(KeyNames);
        for I := 0 to KeyNames.Count - 1 do
          Expected.Add(Format('  "%s": "%s"', [KeyNames[I], Self.Data.Result.Find(KeyNames[I])]));
      finally
        KeyNames.Free;
      end;

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

procedure TestTIdStringDictionaryResultData.TestCopy;
var
  Copy:     TIdStringDictionaryResultData;
  KeyNames: TStrings;
  I:        Integer;
begin
  Copy := Self.Data.Copy as TIdStringDictionaryResultData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8), IntToHex(Copy.Handle, 8), 'Handle');
    CheckEquals(Self.Data.ReferenceID, Copy.ReferenceID, 'ReferenceID');
    CheckEquals(Self.Data.Result.Count, Copy.Result.Count, 'Result size');

    KeyNames := TStringList.Create;
    try
      Self.Data.Result.CollectKeys(KeyNames);

      for I := 0 to KeyNames.Count - 1 do begin
        Check(Copy.Result.HasKey(KeyNames[I]), 'Key ' + KeyNames[I] + ' not copied');
        CheckEquals(Self.Data.Result.Find(KeyNames[I]),
                    Copy.Result.Find(KeyNames[I]),
                    'Key ' + KeyNames[I] + ' not copied correctly');
      end;
    finally
      KeyNames.Free;
    end;
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TStackWaitTestCase                                                         *
//******************************************************************************
//* TStackWaitTestCase Public methods ******************************************

procedure TStackWaitTestCase.SetUp;
var
  T: TIdSipTransport;
begin
  inherited SetUp;

  Self.BindingIP   := '127.0.0.1';
  Self.BindingPort := 5060;

  TIdSipTransportRegistry.RegisterTransportType(UdpTransport, TIdSipMockUdpTransport);

  Self.Conf := TStringList.Create;
  Self.Conf.Add('Listen: UDP ' + Self.BindingIP + ':' + IntToStr(Self.BindingPort));
  Self.Conf.Add('NameServer: MOCK');

  Self.Stack := TIdSipStackInterface.Create(Self.UI.Handle, Self.TimerQueue, Self.Conf);
  Self.Stack.Resume;

  T := TIdSipDebugTransportRegistry.TransportRunningOn(Self.BindingIP, Self.BindingPort);
  Check(nil <> T, 'No transport running on ' + Self.BindingIP + ':' + IntToStr(Self.BindingPort));
  Self.MockTransport := T as TIdSipMockTransport;
end;

procedure TStackWaitTestCase.TearDown;
begin
  Self.Stack.Free;

  TIdSipTransportRegistry.UnregisterTransportType(UdpTransport);

  inherited TearDown;
end;

procedure TStackWaitTestCase.CheckNoUdpServerOnPort(Host: String;
                                                    Port: Cardinal;
                                                    Msg: String);
begin
  Check(nil = TIdSipDebugTransportRegistry.TransportRunningOn(Host, Port),
        'UDP server running on ' + Host + ':' + IntToStr(Port) + '; Msg');
end;

procedure TStackWaitTestCase.CheckUdpServerOnPort(Host: String;
                                                  Port: Cardinal;
                                                  Msg: String);
begin
  Check(nil <> TIdSipDebugTransportRegistry.TransportRunningOn(Host, Port),
        'No UDP server running on ' + Host + ':' + IntToStr(Port) + '; Msg');
end;

//******************************************************************************
//* TestTIdStackShutdownWait                                                   *
//******************************************************************************
//* TestTIdStackShutdownWait Public methods ************************************

procedure TestTIdStackShutdownWait.SetUp;
begin
  inherited SetUp;

  Self.Wait := TIdStackShutdownWait.Create;
  Self.Wait.StackID := Self.Stack.ID;
end;

procedure TestTIdStackShutdownWait.TearDown;
begin
  Self.Wait.Free;

  // Remember, this test shows that Stack is freed; setting Self.Stack to nil
  // means that inherited TearDown can call Self.Stack.Free without blowing up.
  Self.Stack := nil;

  inherited TearDown;
end;

//* TestTIdStackShutdownWait Published methods *********************************

procedure TestTIdStackShutdownWait.TestTrigger;
begin
  CheckUdpServerOnPort(Self.BindingIP, Self.BindingPort, 'Stack not configured correctly');

  Self.Wait.Trigger;

  CheckNoUdpServerOnPort(Self.BindingIP, Self.BindingPort, 'Stack not shut down');
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

  Self.Wait := TIdSipStackReconfigureStackInterfaceWait.Create;
  Self.Wait.StackID := Self.Stack.ID;
end;

procedure TestTIdSipStackReconfigureStackInterfaceWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
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
//* TestTIdGetBindingsWait                                                     *
//******************************************************************************
//* TestTIdGetBindingsWait Public methods **************************************

procedure TestTIdGetBindingsWait.SetUp;
begin
  inherited SetUp;

  Self.Wait := TIdGetBindingsWait.Create;
  Self.Wait.StackID := Self.Stack.ID;
end;

procedure TestTIdGetBindingsWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdGetBindingsWait Published methods ***********************************

procedure TestTIdGetBindingsWait.TestTrigger;
var
  E:        TIdSipNetworkExtension;
  Expected: TIdSipLocations;
  I:        Integer;
  Received: TIdGetBindingsData;
begin
  Self.Wait.Trigger;
  Self.ProcessAllPendingNotifications;
  CheckNotificationReceived(TIdGetBindingsData, 'No notification about GetBindings received');

  E := Self.Stack.AttachExtension(TIdSipNetworkExtension) as TIdSipNetworkExtension;
  try
    Expected := TIdSipLocations.Create;
    try
      E.GetBindings(Expected);

      Received := Self.LastEventOfType(TIdGetBindingsData) as TIdGetBindingsData;

      CheckEquals(Expected.Count, Received.Bindings.Count, 'Unexpected number of bindings');
      for I := 0 to Expected.Count - 1 do
        CheckEquals(Expected[I].AsString, Received.Bindings[I].AsString, 'Binding #' + IntToStr(I));

      CheckEquals(Self.Wait.ID, Received.ReferenceID, 'ReferenceID not set');
    finally
      Expected.Free;
    end;
  finally
    E.Free;
  end;
end;

//******************************************************************************
//* TestTIdIsSourceOfWait                                                      *
//******************************************************************************
//* TestTIdIsSourceOfWait Public methods ***************************************

procedure TestTIdIsSourceOfWait.SetUp;
begin
  inherited SetUp;

  Self.Dest := TIdSipToHeader.Create;
  Self.From := TIdSipFromHeader.Create;

  Self.Dest.Value := 'sip:case@fried-neurons.org';
  Self.From.Value := 'sip:wintermute@tessier-ashpool.co.luna';

  Self.Wait := TIdIsSourceOfWait.Create;
  Self.Wait.StackID := Self.Stack.ID;
end;

procedure TestTIdIsSourceOfWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdIsSourceOfWait Published methods ************************************

procedure TestTIdIsSourceOfWait.TestTriggerIsSource;
var
  Received: TIdBooleanResultData;
begin
  Self.MarkSentRequestCount;
  Self.Stack.Send(Self.Stack.MakeCall(Self.From, Self.Dest, '', '', TIdSipRequest.DefaultMaxForwards));
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  CheckRequestSent('No INVITE sent');
  Self.Wait.Request := Self.LastSentRequest;

  Self.Wait.Trigger;
  Self.ProcessAllPendingNotifications;
  CheckNotificationReceived(TIdBooleanResultData, 'No notification about IsSourceOf received');

  Received := Self.LastEventOfType(TIdBooleanResultData) as TIdBooleanResultData;
  Check(Received.Result, 'Stack claims to NOT be source of request');
  CheckEquals(Self.Wait.ID, Received.ReferenceID, 'ReferenceID not set');
end;

procedure TestTIdIsSourceOfWait.TestTriggerIsNotSource;
var
  ArbitraryRequest: TIdSipRequest;
  Received:         TIdBooleanResultData;
begin
  ArbitraryRequest := TIdSipTestResources.CreateBasicRequest;
  try
    Self.Wait.Request := ArbitraryRequest;

    Self.Wait.Trigger;
    Self.ProcessAllPendingNotifications;
    CheckNotificationReceived(TIdBooleanResultData, 'No notification about IsSourceOf received');

    Received := Self.LastEventOfType(TIdBooleanResultData) as TIdBooleanResultData;
    Check(not Received.Result, 'Stack claims to be source of request');
    CheckEquals(Self.Wait.ID, Received.ReferenceID, 'ReferenceID not set');
  finally
    ArbitraryRequest.Free;
  end;
end;

//******************************************************************************
//* TIdNetworkingMessageWaitTestCase                                           *
//******************************************************************************
//* TIdNetworkingMessageWaitTestCase Public methods ****************************

procedure TIdNetworkingMessageWaitTestCase.SetUp;
begin
  inherited SetUp;

  Self.LocalAddress := '10.0.0.6';
  Self.NatAddress   := '1.2.3.4';
  Self.VpnAddress   := '192.168.0.2';

  Self.Conf.Clear;
  Self.Conf.Add('MockLocalAddress: 127.0.0.1');
  Self.Conf.Add('MockLocalAddress: ' + Self.LocalAddress);
  Self.Conf.Add('MockLocalAddress: ' + Self.VpnAddress);
  Self.Conf.Add('RoutingTable: MOCK');
  Self.Conf.Add('MockRoute: 10.0.0.0/8 10.0.0.1 1 xl0 ' + Self.LocalAddress);
  Self.Conf.Add('MockRoute: 0.0.0.0/0 10.0.0.1 1 xl0 ' + Self.LocalAddress);
  Self.Conf.Add('MockRoute: 192.168.0.0/16 192.168.0.1 1 tun1 ' + Self.VpnAddress);
  Self.Stack.ReconfigureStack(Self.Conf);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipStackReconfigureStackInterfaceWait);
end;

//* TIdNetworkingMessageWaitTestCase Protected methods *************************

procedure TIdNetworkingMessageWaitTestCase.ConfigureNat;
begin
  Self.Conf.Clear;
  Self.Conf.Add('MappedRoute: 0.0.0.0/0 ' + Self.NatAddress);
  Self.Stack.ReconfigureStack(Self.Conf);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipStackReconfigureStackInterfaceWait);
end;

//******************************************************************************
//* TestTIdLocalAddressForWait                                                 *
//******************************************************************************
//* TestTIdLocalAddressForWait Public methods **********************************

procedure TestTIdLocalAddressForWait.SetUp;
begin
  inherited SetUp;

  Self.Wait := TIdLocalAddressForWait.Create;

  Self.Wait.DestinationAddress := '196.25.1.1';
  Self.Wait.StackID            := Self.Stack.ID;
end;

procedure TestTIdLocalAddressForWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdLocalAddressForWait Published methods *******************************

procedure TestTIdLocalAddressForWait.TestTriggerWithNat;
var
  Received: TIdStringResultData;
begin
  Self.ConfigureNat;

  Self.Wait.Trigger;
  Self.ProcessAllPendingNotifications;

  CheckNotificationReceived(TIdStringResultData, 'No notification about LocalAddressFor received');

  Received := Self.LastEventOfType(TIdStringResultData) as TIdStringResultData;
  CheckEquals(Self.LocalAddress, Received.Result, 'Unexpected local address');
  CheckEquals(Self.Wait.ID, Received.ReferenceID, 'ReferenceID not set');
end;

procedure TestTIdLocalAddressForWait.TestTriggerWithNoNat;
var
  Received: TIdStringResultData;
begin
  Self.Wait.Trigger;
  Self.ProcessAllPendingNotifications;

  CheckNotificationReceived(TIdStringResultData, 'No notification about LocalAddressFor received');

  Received := Self.LastEventOfType(TIdStringResultData) as TIdStringResultData;
  CheckEquals(Self.LocalAddress, Received.Result, 'Unexpected local address');
  CheckEquals(Self.Wait.ID, Received.ReferenceID, 'ReferenceID not set');
end;

//******************************************************************************
//* TestTIdLocalOrMappedAddressForWait                                         *
//******************************************************************************
//* TestTIdLocalOrMappedAddressForWait Public methods **************************

procedure TestTIdLocalOrMappedAddressForWait.SetUp;
begin
  inherited SetUp;

  Self.Wait := TIdLocalOrMappedAddressForWait.Create;

  Self.Wait.DestinationAddress := '196.25.1.1';
  Self.Wait.StackID            := Self.Stack.ID;
end;

procedure TestTIdLocalOrMappedAddressForWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdLocalOrMappedAddressForWait Published methods ***********************

procedure TestTIdLocalOrMappedAddressForWait.TestTriggerWithNat;
var
  Received: TIdStringResultData;
begin
  Self.ConfigureNat;

  Self.Wait.Trigger;
  Self.ProcessAllPendingNotifications;

  CheckNotificationReceived(TIdStringResultData, 'No notification about LocalOrMappedAddressFor received');

  Received := Self.LastEventOfType(TIdStringResultData) as TIdStringResultData;
  CheckEquals(Self.NatAddress, Received.Result, 'Unexpected local address');
  CheckEquals(Self.Wait.ID, Received.ReferenceID, 'ReferenceID not set');
end;

procedure TestTIdLocalOrMappedAddressForWait.TestTriggerWithNoNat;
var
  Received: TIdStringResultData;
begin
  Self.Wait.Trigger;
  Self.ProcessAllPendingNotifications;

  CheckNotificationReceived(TIdStringResultData, 'No notification about LocalOrMappedAddressFor received');

  Received := Self.LastEventOfType(TIdStringResultData) as TIdStringResultData;
  CheckEquals(Self.LocalAddress, Received.Result, 'Unexpected local address');
  CheckEquals(Self.Wait.ID, Received.ReferenceID, 'ReferenceID not set');
end;

//******************************************************************************
//* TestTIdResolveNamesForWait                                                 *
//******************************************************************************
//* TestTIdResolveNamesForWait Public methods **********************************

procedure TestTIdResolveNamesForWait.SetUp;
begin
  inherited SetUp;

  Self.Host        := 'foo.bar';
  Self.IPv4Address := '1.2.3.4';
  Self.IPv6Address := '2002:dead:beef::1';

  Self.Conf.Clear;
  Self.Conf.Add('NameServer: MOCK;ReturnOnlySpecifiedRecords');
  Self.Conf.Add('MockDns: A ' + Self.Host + ' ' + Self.IPv4Address);
  Self.Conf.Add('MockDns: AAAA ' + Self.Host + ' ' + Self.IPv6Address);
  Self.Stack.ReconfigureStack(Self.Conf);
  Self.TimerQueue.TriggerAllEventsOfType(TIdSipStackReconfigureStackInterfaceWait);

  Self.Wait := TIdResolveNamesForWait.Create;
  Self.Wait.HostName := Self.Host;
  Self.Wait.StackID  := Self.Stack.ID;
end;

procedure TestTIdResolveNamesForWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

procedure TestTIdResolveNamesForWait.TestTrigger;
var
  Received: TIdDomainNameRecordsResultData;
begin
  Self.Wait.Trigger;
  Self.ProcessAllPendingNotifications;

  CheckNotificationReceived(TIdDomainNameRecordsResultData, 'No notification about ResolveNamesFor received');

  Received := Self.LastEventOfType(TIdDomainNameRecordsResultData) as TIdDomainNameRecordsResultData;

  CheckEquals(Self.Wait.ID, Received.ReferenceID, 'ReferenceID not set');

  CheckEquals(2, Received.IPAddresses.Count, 'Wrong number of resource records');
  CheckEquals(Self.IPv4Address, Received.IPAddresses[0].IPAddress, 'First RR');
  CheckEquals(Self.IPv6Address, Received.IPAddresses[1].IPAddress, 'Second RR');
end;

//******************************************************************************
//* TestTIdCollectStatisticsWait                                               *
//******************************************************************************
//* TestTIdCollectStatisticsWait Public methods ********************************

procedure TestTIdCollectStatisticsWait.SetUp;
begin
  inherited SetUp;

  Self.Wait := TIdCollectStatisticsWait.Create;
  Self.Wait.StackID := Self.Stack.ID;
end;

procedure TestTIdCollectStatisticsWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdCollectStatisticsWait Private methods *******************************

procedure TestTIdCollectStatisticsWait.SendOptions;
var
  O:      TIdSipHandle;
  Target: TIdSipToHeader;
begin
  Target := TIdSipToHeader.Create;
  try
    Target.Value := 'sip:127.0.0.1';
    O := Self.Stack.MakeOptionsQuery(Target);
    Self.Stack.Send(O);
    Self.TimerQueue.TriggerAllEventsUpToFirst(TIdSipActionSendWait);
  finally
    Target.Free;
  end;
end;

//* TestTIdCollectStatisticsWait Published methods *****************************

procedure TestTIdCollectStatisticsWait.TestTrigger;
var
  Received: TIdStringDictionaryResultData;
begin
  // This ensures that the stack produces at least some data.
  Self.SendOptions;

  Self.Wait.Trigger;
  Self.ProcessAllPendingNotifications;

  CheckNotificationReceived(TIdStringDictionaryResultData, 'No notification about CollectStatistics received');

  Received := Self.LastEventOfType(TIdStringDictionaryResultData) as TIdStringDictionaryResultData;

  CheckEquals(Self.Wait.ID, Received.ReferenceID, 'ReferenceID not set');

  Check(not Received.Result.IsEmpty, 'No data received, so stack not actually queried');
end;

initialization
  RegisterTest('SIP stack interface tests', Suite);
end.
