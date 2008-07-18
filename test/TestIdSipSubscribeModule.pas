{
  (c) 2005 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipSubscribeModule;

interface
{ TEMPORARY HOLDING SPACE FOR DELETED-FOR-NOW CODE
  TestTIdSipBlindTransferral = class(TestTIdSipOutboundReferral,
                                     IIdSipMessageModuleListener,
                                     IIdSipInviteModuleListener,
                                     IIdSipSubscribeModuleListener,
                                     IIdSipTransactionUserListener,
                                     IIdSipUserAgentListener)
  private
    Session: TIdSipInboundSession;
    Subscription:   TIdSipInboundSubscription;
    Transferee:     TIdSipUserAgent;
    Transferor:     TIdSipUserAgent;
    TransferTarget: TIdSipUserAgent;
    ReceivingUA:    TIdSipAbstractCore;

    function  EstablishCallBetween(Caller: TIdSipUserAgent;
                                   Callee: TIdSipUserAgent): TIdSipSession;
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                        Message: TIdSipMessage;
                                        Receiver: TIdSipTransport);
    procedure OnInboundCall(UserAgent: TIdSipAbstractCore;
                            Session: TIdSipInboundSession);
  protected
    function  CreateReferral: TIdSipOutboundReferral; override;
    function  CreateUserAgent(Timer: TIdTimerQueue;
                              const Address: String): TIdSipUserAgent; override;
    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipOutboundSubscription); override;
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipInboundSubscription); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFullTransfer;
  end;

//******************************************************************************
//* TestTIdSipBlindTransferral                                                 *
//******************************************************************************
//* TestTIdSipBlindTransferral Public methods **********************************

procedure TestTIdSipBlindTransferral.SetUp;
begin
  inherited SetUp;

  Self.Transferor := Self.Core;
  Self.Transferee     := Self.CreateUserAgent(Self.DebugTimer, 'sip:127.0.0.2');
  Self.TransferTarget := Self.CreateUserAgent(Self.DebugTimer, 'sip:127.0.0.3');
end;

procedure TestTIdSipBlindTransferral.TearDown;
begin
  Self.TransferTarget.Free;
  Self.Transferee.Free;

  inherited TearDown;
end;

//* TestTIdSipBlindTransferral Protected methods *******************************

function TestTIdSipBlindTransferral.CreateReferral: TIdSipOutboundReferral;
var
  NewCall: TIdSipSession;
begin
  Self.Module.AddPackage(TIdSipReferPackage);

  NewCall := Self.Core.InviteModule.Call(Self.Destination, '', '');
  NewCall.Send;
  Self.ReceiveOk(NewCall.InitialRequest);

  Result := Self.Module.BlindTransfer(NewCall, Self.Destination);
  Result.AddListener(Self);
  Result.Send;
end;

function TestTIdSipBlindTransferral.CreateUserAgent(Timer: TIdTimerQueue;
                                                    const Address: String): TIdSipUserAgent;
var
  SubModule: TIdSipSubscribeModule;
begin
  Result := inherited CreateUserAgent(Timer, Address);

  Result.AddUserAgentListener(Self);

  SubModule := Result.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  SubModule.AddListener(Self);
end;

procedure TestTIdSipBlindTransferral.OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                                           Subscription: TIdSipOutboundSubscription);
begin
  // Do nothing.
end;

procedure TestTIdSipBlindTransferral.OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                                           Subscription: TIdSipInboundSubscription);
begin
  Self.ReceivingUA  := UserAgent;
  Self.Subscription := Subscription;
end;

//* TestTIdSipBlindTransferral Private methods *********************************

function TestTIdSipBlindTransferral.EstablishCallBetween(Caller: TIdSipUserAgent;
                                                         Callee: TIdSipUserAgent): TIdSipSession;
begin
  Result := Caller.InviteModule.Call(Callee.Contact, '', '');
  Result.Send;

  Check(Assigned(Self.Session), 'Callee didn''t receive an INVITE');
  Self.Session.AcceptCall('', '');
end;

procedure TestTIdSipBlindTransferral.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                                               Message: TIdSipMessage;
                                                               Receiver: TIdSipTransport);
begin
  Fail('Dropped unmatched message: ' + Message.AsString);
end;

procedure TestTIdSipBlindTransferral.OnInboundCall(UserAgent: TIdSipAbstractCore;
                                                   Session: TIdSipInboundSession);
begin
  Self.Session := Session;
end;

//* TestTIdSipBlindTransferral Published methods *******************************

procedure TestTIdSipBlindTransferral.TestFullTransfer;
var
  Call:      TIdSipSession;
  InvitesOk: TIdSipResponse;
  Transfer:  TIdSipBlindTransferral;
begin
// cf. draft-ietf-sipping-cc-transfer-04, section 5.1
//
//     Transferor           Transferee             Transfer
//          |                    |                  Target
//          |          INVITE    |                    |
//          |<-------------------|                    |
//          |          200 OK    |                    |
//          |------------------->|                    |
//          |            ACK     |                    |
//          |<-------------------|                    |
//          |  REFER             |                    |
//          |------------------->|                    |
//          |  202 Accepted      |                    |
//          |<-------------------|                    |
//          | NOTIFY (100 Trying)                     |
//          |<-------------------|                    |
//          |            200 OK  |                    |
//          |------------------->|                    |
//          |                    |  INVITE            |
//          |                    |------------------->|
//          |                    |  200 OK            |
//          |                    |<-------------------|
//          |                    |  ACK               |
//          |                    |------------------->|
//          |  NOTIFY (200 OK)   |                    |
//          |<-------------------|                    |
//          |            200 OK  |                    |
//          |------------------->|                    |
//          |  BYE               |                    |
//          |------------------->|                    |
//          |  200 OK            |                    |
//          |<-------------------|                    |

  Call := Self.EstablishCallBetween(Self.Transferee, Self.Transferor);

  Transfer := Self.Module.BlindTransfer(Call, Self.TransferTarget.From);

  Self.MarkSentRequestCount;
  Transfer.Send;
  CheckRequestSent('No request sent');
  CheckEquals(MethodRefer,
              Self.LastSentRequest.Method,
              'Unexpected request sent for the referral');

  Check(Assigned(Self.Subscription),
        'Transferee didn''t get the REFER');
  Check(Self.Transferee = Self.ReceivingUA,
        'The REFER didn''t end up at the transferee but at '
      + Self.ReceivingUA.From.Address.AsString);

  // This isn't _quite_ correct: the OK should be in response to the INVITE
  // between the transferee and transfer target. Still, this doesn't matter:
  // the OK will send the right NOTIFY to the transferor.
  InvitesOk := TIdSipResponse.InResponseTo(Call.InitialRequest, SIPOK);
  try
    Self.ReceiveNotify(Transfer.InitialRequest,
                       InvitesOk,
                       SubscriptionSubstateTerminated,
                       EventReasonNoResource);
  finally
    InvitesOk.Free;
  end;

  Check(Call.IsTerminated,
        'Blind transfer didn''t kill the original session');
end;
}

uses
  IdConnectionBindings, IdSipAuthentication, IdSipCore, IdSipDialog,
  IdSipInviteModule, IdSipMessage, IdSipSubscribeModule, IdSipTransport,
  IdSipUserAgent, IdTimerQueue, TestFramework, TestFrameworkSip,
  TestFrameworkSipTU;

type
  TSubscribeTestCase = class(TTestCaseTU,
                             IIdSipSubscribeModuleListener)
  private
  protected
    ExpiryTime:                 Cardinal;
    Module:                     TIdSipSubscribeModule;
    OnRenewedSubscriptionFired: Boolean;
    OnSubscriptionRequestFired: Boolean;
    Package:                    TIdSipEventPackage;
    Subscription:               TIdSipSubscription;
    UserAgentParam:             TIdSipAbstractCore;

    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipOutboundSubscription); virtual;
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipInboundSubscription); virtual;
    procedure ReceiveRefer(Target: TIdSipAddressHeader);
    procedure ReceiveSubscribe(const EventPackage: String;
                               ExpiryTime: Cardinal = 0); virtual;
  public
    procedure SetUp; override;
  end;

  TestTIdSipSubscribeModule = class(TSubscribeTestCase,
                                    IIdSipMessageModuleListener,
                                    IIdSipInviteModuleListener,
                                    IIdSipTransactionUserListener)
  private
    InboundCall: TIdSipInboundSession;

    procedure CheckBadEventResponseSent(const UnknownEvent: String);
    procedure CheckNoPackageFound(PackageType: TIdSipEventPackageClass);
    procedure CheckPackageFound(PackageType: TIdSipEventPackageClass);
    procedure OnAddAction(UserAgent: TIdSipAbstractCore;
                          Action: TIdSipAction);
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                        Message: TIdSipMessage;
                                        Binding: TIdConnectionBindings);
    procedure OnInboundCall(UserAgent: TIdSipInviteModule;
                            Session: TIdSipInboundSession);
    procedure OnRemoveAction(UserAgent: TIdSipAbstractCore;
                             Action: TIdSipAction);
    procedure ReceiveNotify(const EventPackage: String);
    procedure ReceiveReferWithNoReferToHeader;
    procedure ReceiveSubscribeWithNoEventHeader;
  public
    procedure SetUp; override;
  published
    procedure TestAcceptsMethodsWithReferPackage;
    procedure TestAddListener;
    procedure TestAddPackage;
    procedure TestAddPackageTwice;
    procedure TestAddReferPackageTwice;
    procedure TestCreateRefer;
    procedure TestCreateSubscribe;
    procedure TestPackage;
    procedure TestPackageFor;
    procedure TestReceiveReferWithNoReferTo;
    procedure TestReceiveReferNotifiesListeners;
    procedure TestRefer;
    procedure TestRejectNewSubscribeForReferPackage;
    procedure TestRejectSubscribeWithNoEventHeader;
    procedure TestRejectUnknownEventSubscriptionRequest;
    procedure TestRejectUnmatchedNotify;
    procedure TestRemoveListener;
    procedure TestSubscribe;
    procedure TestSubscriptionRequest;
    procedure TestTransfer;
    procedure TestUAAllowedContentTypes;
  end;

  TestTIdSipUserAgentWithSubscribeModule = class(TTestCaseTU)
  private
    Module: TIdSipSubscribeModule;

    procedure ReceiveOptions;
  public
    procedure SetUp; override;
  published
    procedure TestReceiveNotifyForUnmatchedDialog;
    procedure TestReceiveOptions;
    procedure TestSendInvite;
  end;

  TestCallFlows = class(TSubscribeTestCase)
  private
    function  CreateAndEstablishInboundCall: TIdSipOutboundSession;
  public
    procedure SetUp; override;
  published
    procedure TestCallTransferred;
  end;

  TestTIdSipEventPackageRegistry = class(TSubscribeTestCase)
  published
    procedure TestRegisterUnregisterPackage;
    procedure TestUnregisterUnregisteredPackage;
  end;

  TestTIdSipEventPackage = class(TSubscribeTestCase)
  published
    procedure TestProbationRetryTime;
  end;

  TestTIdSipSubscribe = class(TestTIdSipAction)
  protected
    Module: TIdSipSubscribeModule;
  public
    procedure SetUp; override;
  end;

  TestTIdSipOutboundNotifyBase = class(TestTIdSipSubscribe,
                                       IIdSipNotifyListener)
  protected
    Body:              String;
    Dialog:            TIdSipDialog;
    MimeType:          String;
    Notify:            TIdSipOutboundNotifyBase;
    Subscribe:         TIdSipRequest;
    SubscriptionState: String;

    procedure ConfigureNotify(Action: TIdSipOutboundNotifyBase); virtual;
    procedure OnFailure(NotifyAgent: TIdSipOutboundNotify;
                        Response: TIdSipResponse);
    procedure OnSuccess(NotifyAgent: TIdSipOutboundNotify;
                        Response: TIdSipResponse);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsOwned; override;
    procedure TestMethod;
    procedure TestSend; virtual;
  end;

  TestTIdSipOutboundNotify = class(TestTIdSipOutboundNotifyBase)
  private
    Expires: Cardinal;

  protected
    procedure ConfigureNotify(Action: TIdSipOutboundNotifyBase); override;
    function  CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
  published
    procedure TestAddListener;
    procedure TestRemoveListener;
    procedure TestSend; override;
  end;

  TestTIdSipOutboundTerminatingNotify = class(TestTIdSipOutboundNotifyBase)
  private
    Reason: String;
  protected
    procedure ConfigureNotify(Action: TIdSipOutboundNotifyBase); override;
    function  CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
  published
    procedure TestSend; override;
  end;

  TestTIdSipOutboundSubscribe = class(TestTIdSipSubscribe,
                                      IIdSipOwnedActionListener)
  private
    EventID:      String;
    EventPackage: String;
    Failed:       Boolean;
    Succeeded:    Boolean;

    procedure OnFailure(Action: TIdSipAction;
                        Response: TIdSipResponse;
                        const Reason: String);
    procedure OnRedirect(Action: TIdSipAction;
                         Redirect: TIdSipResponse);
    procedure OnSuccess(Action: TIdSipAction;
                        Msg: TIdSipMessage);
  protected
    procedure ConfigureAction(Action: TIdSipAction); virtual;
    function  CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
  published
    procedure TestIsOwned; override;
    procedure TestMatchNotify;
    procedure TestMatchResponse;
    procedure TestMethod; virtual;
    procedure TestReceive2xx;
    procedure TestReceiveFailure;
    procedure TestSend; virtual;
  end;

  TestTIdSipOutboundRefreshSubscribe = class(TestTIdSipOutboundSubscribe)
  private
    Dialog:       TIdSipDialog;
    ExpiresValue: Cardinal;

    function CreateRefreshSubscribe: TIdSipOutboundRefreshSubscribe;
  protected
    procedure ConfigureAction(Action: TIdSipAction); override;
    function  CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReceiveIntervalTooBrief;
    procedure TestSend; override;
  end;

  TestTIdSipOutboundUnsubscribe = class(TestTIdSipOutboundSubscribe)
  private
    CallID:  String;
    FromTag: String;

    function CreateUnsubscribe: TIdSipOutboundUnsubscribe;
  protected
    procedure ConfigureAction(Action: TIdSipAction); override;
    function  CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
  published
    procedure TestSend; override;
  end;

  TestTIdSipOutboundRefer = class(TestTIdSipOutboundSubscribe)
  private
    ReferTo: TIdSipAddressHeader;

    function CreateRefer: TIdSipOutboundRefer;
  protected
    procedure ConfigureAction(Action: TIdSipAction); override;
    function  CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMethod; override;
    procedure TestSend; override;
    procedure TestSendAlwaysUsesReferEvent;
    procedure TestSendDoesntSendTwoRequests;
  end;

  TSubscribeModuleActionTestCase = class(TestTIdSipAction,
                                         IIdSipSubscribeModuleListener)
  protected
    Module:      TIdSipSubscribeModule;
    Package:     TIdSipEventPackage;
    RemoteParty: TIdSipFromHeader;

    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipOutboundSubscription); virtual;
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipInboundSubscription); virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipInboundSubscriptionBase = class(TSubscribeModuleActionTestCase)
  protected
    Action:        TIdSipInboundSubscription;
    ActionRequest: TIdSipRequest;

    procedure CheckNotify(Notify: TIdSipRequest;
                          const Body: String;
                          const MimeType: String);
    procedure CheckSendNotify(Sub: TIdSipInboundSubscription;
                              const SubscriptionState: String);
    function  CreateRefresh(Sub: TIdSipInboundSubscription;
                            Response: TIdSipResponse;
                            ExpiryTime: Cardinal): TIdSipRequest;
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipInboundSubscription); override;
    procedure ReceiveRefreshingSubscribe(Sub: TIdSipInboundSubscription;
                                         Response: TIdSipResponse;
                                         ExpiryTime: Cardinal);
    procedure ReceiveSubscribeRequest; virtual;
    procedure ReceiveSubscribeRequestWithGruu; virtual;
  public
    procedure SetUp; override;
  published
    procedure TestAccept;
    procedure TestDontMatchInDialogInvite;
    procedure TestIsInbound; override;
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsOwned; override;
    procedure TestIsOutbound; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
    procedure TestNotify; virtual;
    procedure TestNotifyWithGruu; virtual;
    procedure TestReceiveRefreshingSubscribe;
    procedure TestReceiveRequestSendsAccepted;
    procedure TestReceiveRequestSendsAcceptedWithGruu;
    procedure TestReceiveRequestSendsNotify;
    procedure TestReceiveRequestWithGruu;
    procedure TestReceiveSubscribeWithZeroExpires;
    procedure TestTerminateSignalled; override;
  end;

  TestTIdSipInboundSubscription = class(TestTIdSipInboundSubscriptionBase)
  private
    EventID: String;

    procedure CheckExpiresScheduled(ExpectedExpires: Cardinal;
                                    const Msg: String);
//    procedure ReceiveAuthChallengeWithRetryAfter(Sub: TIdSipRequest;
//                                                 RetryAfter: Cardinal);
    procedure ReceiveSubscribe(const EventPackage: String;
                               ExpiryTime: Cardinal = 0);
    procedure ReceiveSubscribeWithoutExpires(const EventPackage: String);
    procedure ReceiveSubscribeWithExpiresInContact(Duration: Cardinal);
//    procedure RemoveExpiresWait(Timer: TIdDebugTimerQueue);
  protected
    procedure ReceiveSubscribeRequest; override;
    procedure ReceiveSubscribeRequestWithGruu; override;
  public
    procedure SetUp; override;
  published
    procedure TestExpire;
    procedure TestExpiryTimeInSeconds;
    procedure TestMatchInDialogSubscribe;
    procedure TestMatchResponse;
    procedure TestNotify; override;
    procedure TestNotifyWithGruu; override;
    procedure TestReceiveExpiresInContactHeader;
    procedure TestReceiveExpiresTooShort;
//    procedure TestReceiveOutOfOrderRefresh;
    procedure TestReceiveNoExpires;
    procedure TestReceiveRefreshingSubscribeIntervalTooBrief;
    procedure TestReceiveSubscribe;
    procedure TestReceiveSubscribeReturnsAccepted;
    procedure TestReceiveSubscribeSendsNotify;
    procedure TestSendNotify;
    procedure TestSendNotifyAffectsState;
    procedure TestSendNotifyNetworkFailure;
    procedure TestSendNotifyReceiveFail;
    // TODO: Uncomment and implement once authentication's asynchronous.
//    procedure TestSendNotifyReceiveFailWithRetryAfter;
  end;

  TestTIdSipOutboundSubscriptionBase = class(TSubscribeModuleActionTestCase,
                                             IIdSipSubscriptionListener)
  protected
    ArbExpiresValue:         Cardinal;
    ArbRetryAfterValue:      Cardinal;
    ChallengeResponse:       TIdSipResponse;
    Password:                String;
    ReceivedNotify:          TIdSipRequest;
    RemoteRealmInfo:         TIdRealmInfo;
    RenewSubscriptionFired:  Boolean;
    Subscription:            TIdSipOutboundSubscription;
    SubscriptionEstablished: Boolean;
    SubscriptionExpired:     Boolean;
    SubscriptionFailed:      Boolean;
    SubscriptionNotified:    Boolean;
    UnknownReason:           String;

    procedure CheckExpires(ExpectedRefreshTime: Cardinal);
    procedure CheckNoRetryScheduled(const MsgPrefix: String);
    procedure CheckReceiveFailureResponse(StatusCode: Cardinal);
    procedure CheckRetryScheduled(const MsgPrefix: String);
    procedure CheckTerminatedSubscription(Subscription: TIdSipSubscription;
                                          const MsgPrefix: String);
    procedure CheckTerminatedSubscriptionWithResubscribe(const Reason: String);
    procedure CheckTerminatedSubscriptionWithNoResubscribe(const Reason: String);
    function  CreateAction: TIdSipAction; override;
    function  CreateChallengeResponse: TIdSipResponse;
    function  CreateNotify(Subscribe: TIdSipRequest;
                           Response: TIdSipResponse;
                           const State: String): TIdSipRequest;
    function  CreateSubscription: TIdSipOutboundSubscription; virtual;
    function  CreateSubscriptionWithoutSend: TIdSipOutboundSubscription;
    function  EstablishSubscription: TIdSipOutboundSubscription; overload;
    procedure EstablishSubscription(Sub: TIdSipOutboundSubscription); overload;
    procedure OnEstablishedSubscription(Subscription: TIdSipOutboundSubscription;
                                        Notify: TIdSipRequest);
    procedure OnExpiredSubscription(Subscription: TIdSipOutboundSubscription;
                                    Notify: TIdSipRequest);
    procedure OnFailure(Subscription: TIdSipOutboundSubscription;
                        Response: TIdSipResponse);
    procedure OnNotify(Subscription: TIdSipOutboundSubscription;
                       Notify: TIdSipRequest);
    procedure ReceiveBadEvent(Subscribe: TIdSipRequest);
    procedure ReceiveNotify(Subscribe: TIdSipRequest;
                            Response: TIdSipResponse;
                            const State: String;
                            const Reason: String = '';
                            RetryAfter: Cardinal = 0;
                            Expires: Cardinal = 0);
    procedure ReceiveNotifyTerminated(Sub: TIdSipOutboundSubscription);
    procedure ReceiveOkFor(Sub: TIdSipOutboundSubscription;
                           Expires: Cardinal); overload;
    procedure ReceiveOkFor(Subscribe: TIdSipRequest;
                           Expires: Cardinal); overload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAbandonAuthentication; override;
    procedure TestAddListener;
    procedure TestCircularRedirect;
    procedure TestDoubleRedirect;
    procedure TestMatchForkedNotify;
    procedure TestMatchNotify;
    procedure TestMethod; virtual;
    procedure TestReceive2xxWithNoExpires;
    procedure TestReceiveActiveNotify;
    procedure TestReceiveFailureResponse;
    procedure TestReceiveGlobalFailureResponse;
    procedure TestReceiveServerFailureResponse;
    procedure TestReceiveNotify;
    procedure TestReceiveNotifyBeforeSubscribesResponse;
    procedure TestReceiveTerminatingNotifyDeactivated; virtual;
    procedure TestReceiveTerminatingNotifyDeactivatedWithRetryAfter; virtual;
    procedure TestReceiveTerminatingNotifyGiveUp; virtual;
    procedure TestReceiveTerminatingNotifyGiveUpWithRetryAfter; virtual;
    procedure TestReceiveTerminatingNotifyWithNoReason; virtual;
    procedure TestReceiveTerminatingNotifyWithNoReasonAndRetryAfter; virtual;
    procedure TestReceiveTerminatingNotifyNoResource;
    procedure TestReceiveTerminatingNotifyNoResourceWithRetryAfter;
    procedure TestReceiveTerminatingNotifyProbation; virtual;
    procedure TestReceiveTerminatingNotifyProbationWithRetryAfter; virtual;
    procedure TestReceiveTerminatingNotifyRejected;
    procedure TestReceiveTerminatingNotifyRejectedWithRetryAfter;
    procedure TestReceiveTerminatingNotifyTimeout; virtual;
    procedure TestReceiveTerminatingNotifyTimeoutWithRetryAfter; virtual;
    procedure TestReceiveTerminatingNotifyWithUnknownReason; virtual;
    procedure TestReceiveTerminatingNotifyWithUnknownReasonAndRetryAfter; virtual;
    procedure TestRedirectWithMultipleContacts;
    procedure TestRedirectWithMaxForwards;
    procedure TestRefresh;
    procedure TestRefreshReceives423;
    procedure TestRefreshReceives481;
    procedure TestRefreshReceives4xx;
    procedure TestRefreshUpdatesExpiryTime;
    procedure TestRemoveListener;
    procedure TestSendWithGruu;
    procedure TestSendWithMaxForwards;
    procedure TestTerminate;
    procedure TestTerminateBeforeEstablished;
    procedure TestTerminateSignalled; override;
    procedure TestUnsubscribe;
  end;

  TestTIdSipOutboundSubscription = class(TestTIdSipOutboundSubscriptionBase,
                                         IIdSipSubscriptionListener)
  private
{
    procedure ReceiveNotifyNoAuth(Subscribe: TIdSipRequest;
                                  Response: TIdSipResponse;
                                  const State: String);
    procedure ReceiveNotifyWrongAuth(Subscribe: TIdSipRequest;
                                     Response: TIdSipResponse;
                                     const State: String);
}
  protected
    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipOutboundSubscription); override;
  published
    procedure TestLongRunningSubscription;
    procedure TestMethod; override;
    procedure TestReceive2xx;
    procedure TestReceiveActiveNotifyWithExpires;
//    procedure TestReceiveNotifyNoAuthorization;
//    procedure TestReceiveNotifyWrongAuthorization;
    procedure TestReceivePendingNotifyWithExpires;
    procedure TestSetEventPackage;
    procedure TestSubscribe;
  end;

  TestTIdSipInboundReferral = class(TestTIdSipInboundSubscriptionBase)
  private
    Refer: TIdSipInboundReferral;

    procedure ReceiveRefer(Target: TIdSipAddressHeader);
  protected
    procedure ReceiveSubscribeRequest; override;
    procedure ReceiveSubscribeRequestWithGruu; override;
  public
    procedure SetUp; override;
  published
    procedure TestNotify; override;
    procedure TestNotifyWithGruu; override;
    procedure TestNotifyWithInappropriateBody;
    procedure TestNotifyWithNoBody;
    procedure TestReceiveRefer;
    procedure TestReferenceDenied;
    procedure TestReferenceFailed;
    procedure TestReferenceFailedWithResponse;
    procedure TestReferenceSucceeded;
    procedure TestReferenceTrying;
    procedure TestRejectUnsupportedReferToUri;
    procedure TestRenotifySendsCorrectState;
  end;

  TestTIdSipOutboundReferral = class(TestTIdSipOutboundSubscriptionBase)
  protected
    function CreateReferral: TIdSipOutboundReferral; virtual;
    function CreateSubscription: TIdSipOutboundSubscription; override;
  published
    procedure TestEventPackage;
    procedure TestMethod; override;
    procedure TestReceiveSecondFinalResponse;
    procedure TestReceiveTerminatingNotifyDeactivated; override;
    procedure TestReceiveTerminatingNotifyGiveUp; override;
    procedure TestReceiveTerminatingNotifyGiveUpWithRetryAfter; override;
    procedure TestReceiveTerminatingNotifyWithNoReason; override;
    procedure TestReceiveTerminatingNotifyWithNoReasonAndRetryAfter; override;
    procedure TestReceiveTerminatingNotifyProbation; override;
    procedure TestReceiveTerminatingNotifyProbationWithRetryAfter; override;
    procedure TestReceiveTerminatingNotifyTimeout; override;
    procedure TestReceiveTerminatingNotifyTimeoutWithRetryAfter; override;
    procedure TestReceiveTerminatingNotifyWithUnknownReason; override;
    procedure TestReceiveTerminatingNotifyWithUnknownReasonAndRetryAfter; override;
    procedure TestSendWithTargetDialog;
    procedure TestTargetDialog;
  end;

  TestTIdSipSubscriptionExpires = class(TSubscribeTestCase)
  private
    InSubscription:  TIdSipInboundSubscription;
    OutSubscription: TIdSipOutboundSubscription;
    Block:           TIdSipSubscriptionExpires;

  protected
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipInboundSubscription); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTriggerInboundSubscription;
    procedure TestTriggerOutboundSubscription;
  end;

  TestTIdSipSubscriptionRenotify = class(TSubscribeTestCase)
  private
    Block:        TIdSipSubscriptionRenotify;
    Subscription: TIdSipInboundSubscription;
  protected
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipInboundSubscription); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
    procedure TestTriggerOnWrongTypeOfAction;
  end;

  TestTIdSipOutboundSubscriptionRefreshWait = class(TSubscribeTestCase)
  private
    NewDuration:  Cardinal;
    Subscription: TIdSipOutboundSubscription;
    Wait:         TIdSipOutboundSubscriptionRefreshWait;

    procedure CheckTriggerDoesNothing(Wait: TIdWait;
                                      Msg: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
    procedure TestTriggerWithIDOfNonexistentObject;
    procedure TestTriggerWithIDOfWrongTypeOfObject;
  end;

  TIdSipInboundSubscriptionTestCase = class(TSubscribeTestCase)
  private
    Wait: TIdSipActionWait;
  protected
    procedure AddRequiredPackage; virtual;
    procedure CheckTriggerDoesNothing(Wait: TIdWait;
                                      Msg: String);
    procedure ReceiveSubscribeRequest; virtual;
    function  WaitType: TIdSipActionWaitClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTriggerWithIDOfNonexistentObject;
    procedure TestTriggerWithIDOfWrongTypeOfObject;
  end;

  TestTIdSipInboundSubscriptionNotifyWait = class(TIdSipInboundSubscriptionTestCase)
  private
    MimeType:     String;
    Notification: String;
  protected
    function WaitType: TIdSipActionWaitClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestTrigger;
  end;

  TIdSipInboundReferralWaitClass = class of TIdSipInboundReferralWait;

  TestTIdSipInboundReferralWait = class(TIdSipInboundSubscriptionTestCase)
  protected
    procedure AddRequiredPackage; override;
    procedure CheckReferralResponse(Msg: String); virtual;
    procedure ReceiveSubscribeRequest; override;
  published
    procedure TestHasResponse;
    procedure TestTrigger;
  end;

  TestTIdSipNotifyReferralDeniedWait = class(TestTIdSipInboundReferralWait)
  protected
    procedure CheckReferralResponse(Msg: String); override;
    function  WaitType: TIdSipActionWaitClass; override;
  end;

  TestTIdSipNotifyReferralFailedWait = class(TestTIdSipInboundReferralWait)
  protected
    procedure CheckReferralResponse(Msg: String); override;
    function  WaitType: TIdSipActionWaitClass; override;
  end;

  TestTIdSipNotifyReferralSucceededWait = class(TestTIdSipInboundReferralWait)
  protected
    procedure CheckReferralResponse(Msg: String); override;
    function  WaitType: TIdSipActionWaitClass; override;
  end;

  TestTIdSipNotifyReferralTryingWait = class(TestTIdSipInboundReferralWait)
  protected
    procedure CheckReferralResponse(Msg: String); override;
    function  WaitType: TIdSipActionWaitClass; override;
  end;

  TestTIdSipSubscriptionRetryWait = class(TSubscribeTestCase)
  private
    Wait: TIdSipSubscriptionRetryWait;

    procedure CheckTriggerDoesNothing(Wait: TIdWait;
                                      Msg: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
    procedure TestTriggerWithIDOfNonexistentObject;
    procedure TestTriggerWithIDOfWrongTypeOfObject;
  end;

  TSubscriptionActionMethodTestCase = class(TActionMethodTestCase)
  protected
    Module: TIdSipSubscribeModule;
  public
    procedure SetUp; override;
  end;

  TTestNotifyMethod = class(TSubscriptionActionMethodTestCase)
  protected
    Listener: TIdSipTestNotifyListener;
    Response: TIdSipResponse;
    Notify:   TIdSipOutboundNotify;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipNotifyFailedMethod = class(TTestNotifyMethod)
  private
    Method: TIdSipNotifyFailedMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipNotifySucceededMethod = class(TTestNotifyMethod)
  private
    Method: TIdSipNotifySucceededMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipOutboundSubscriptionMethod = class(TSubscriptionActionMethodTestCase)
  protected
    Listener:     TIdSipTestSubscriptionListener;
    Subscription: TIdSipOutboundSubscription;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipEstablishedSubscriptionMethod = class(TestTIdSipOutboundSubscriptionMethod)
  private
    Method: TIdSipEstablishedSubscriptionMethod;
    Notify: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipExpiredSubscriptionMethod = class(TestTIdSipOutboundSubscriptionMethod)
  private
    Method: TIdSipExpiredSubscriptionMethod;
    Notify: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipFailedSubscriptionMethod = class(TestTIdSipOutboundSubscriptionMethod)
  private
    Method:   TIdSipFailedSubscriptionMethod;
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipOutboundSubscriptionNotifyMethod = class(TestTIdSipOutboundSubscriptionMethod)
  private
    Method: TIdSipSubscriptionNotifyMethod;
    Notify: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TSubscribeModuleTestCase = class(TSubscriptionActionMethodTestCase)
  protected
    Listener: TIdSipTestSubscribeModuleListener;
    Request:  TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipRenewedSubscriptionMethod = class(TSubscribeModuleTestCase)
  private
    Method:       TIdSipRenewedSubscriptionMethod;
    Subscription: TIdSipOutboundSubscription;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipSubscriptionRequestMethod = class(TSubscribeModuleTestCase)
  private
    Method:       TIdSipSubscriptionRequestMethod;
    Subscription: TIdSipInboundSubscription;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

implementation

uses
  IdException, IdRegisteredObject, IdSipDialogID, IdSipOptionsModule,
  IdSipTransaction, SysUtils;

type
  TIdSipTestPackage = class(TIdSipEventPackage)
  public
    class function EventPackage: String; override;
  end;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipSubscribeModule unit tests');
  Result.AddTest(TestTIdSipSubscribeModule.Suite);
  Result.AddTest(TestTIdSipUserAgentWithSubscribeModule.Suite);
  Result.AddTest(TestCallFlows.Suite);
  Result.AddTest(TestTIdSipEventPackageRegistry.Suite);
  Result.AddTest(TestTIdSipEventPackage.Suite);
  Result.AddTest(TestTIdSipOutboundNotify.Suite);
  Result.AddTest(TestTIdSipOutboundTerminatingNotify.Suite);
  Result.AddTest(TestTIdSipOutboundSubscribe.Suite);
  Result.AddTest(TestTIdSipOutboundRefreshSubscribe.Suite);
  Result.AddTest(TestTIdSipOutboundUnsubscribe.Suite);
  Result.AddTest(TestTIdSipOutboundRefer.Suite);
  Result.AddTest(TestTIdSipInboundSubscription.Suite);
  Result.AddTest(TestTIdSipOutboundSubscription.Suite);
  Result.AddTest(TestTIdSipInboundReferral.Suite);
  Result.AddTest(TestTIdSipOutboundReferral.Suite);
  Result.AddTest(TestTIdSipSubscriptionExpires.Suite);
  Result.AddTest(TestTIdSipSubscriptionRenotify.Suite);
  Result.AddTest(TestTIdSipSubscriptionRetryWait.Suite);
  Result.AddTest(TestTIdSipOutboundSubscriptionRefreshWait.Suite);
  Result.AddTest(TestTIdSipInboundSubscriptionNotifyWait.Suite);
  Result.AddTest(TestTIdSipNotifyReferralDeniedWait.Suite);
  Result.AddTest(TestTIdSipNotifyReferralFailedWait.Suite);
  Result.AddTest(TestTIdSipNotifyReferralSucceededWait.Suite);
  Result.AddTest(TestTIdSipNotifyReferralTryingWait.Suite);
  Result.AddTest(TestTIdSipNotifyFailedMethod.Suite);
  Result.AddTest(TestTIdSipNotifySucceededMethod.Suite);
  Result.AddTest(TestTIdSipEstablishedSubscriptionMethod.Suite);
  Result.AddTest(TestTIdSipExpiredSubscriptionMethod.Suite);
  Result.AddTest(TestTIdSipFailedSubscriptionMethod.Suite);
  Result.AddTest(TestTIdSipRenewedSubscriptionMethod.Suite);
  Result.AddTest(TestTIdSipOutboundSubscriptionNotifyMethod.Suite);
  Result.AddTest(TestTIdSipSubscriptionRequestMethod.Suite);
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
//* TSubscribeTestCase                                                         *
//******************************************************************************
//* TSubscribeTestCase Public methods ******************************************

procedure TSubscribeTestCase.SetUp;
begin
  inherited SetUp;

  Self.ExpiryTime := 1000;

  Self.Module := Self.Core.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  Self.Module.AddListener(Self);
  Self.Module.AddPackage(TIdSipTestPackage);
  Self.Package := Self.Module.Package(TIdSipTestPackage.EventPackage);

  Self.OnRenewedSubscriptionFired := false;
  Self.OnSubscriptionRequestFired := false;
end;

//* TSubscribeTestCase Protected methods ***************************************

procedure TSubscribeTestCase.OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                                   Subscription: TIdSipOutboundSubscription);
begin
  Self.OnRenewedSubscriptionFired := true;
  Self.UserAgentParam             := UserAgent;
end;

procedure TSubscribeTestCase.OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                                   Subscription: TIdSipInboundSubscription);
begin
  Self.OnSubscriptionRequestFired := true;
  Self.Subscription               := Subscription;
  Self.UserAgentParam             := UserAgent;
end;

procedure TSubscribeTestCase.ReceiveRefer(Target: TIdSipAddressHeader);
var
  Refer: TIdSipRequest;
begin
  Refer := Self.Module.CreateRefer(Self.Core.From, Self.Destination, Target, TIdSipRequest.DefaultMaxForwards);
  try
    // See the comment in TSubscribeTestCase.ReceiveSubscribe.
    Refer.FirstContact.Address := Self.Destination.Address;
    Self.ReceiveRequest(Refer);
  finally
    Refer.Free;
  end;
end;

procedure TSubscribeTestCase.ReceiveSubscribe(const EventPackage: String;
                                              ExpiryTime: Cardinal = 0);
var
  Sub: TIdSipRequest;
begin
  Sub := Self.Core.CreateRequest(MethodSubscribe, Self.Core.From, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    // Self.Core will use "localhost" here, and as a result of the mock
    // infrastructure this will access violate - the stack assumes that
    // processing the received request will complete before the response, which
    // arrives at the same stack, is processed.

    // In hideous detail, the SUBSCRIBE/REFER arrives at the stack. The core
    // creates an InboundSubscription/Referral, which sends a NOTIFY. That
    // NOTIFY arrives at the same stack. The stack tries to match that NOTIFY
    // against an OutboundSubscription/Referral. It can't, and sends a 481
    // response. That response arrives at the same stack again, is matched
    // against the outbound NOTIFY action, and terminates it. That
    // OutboundNotify notifies its OutboundSubscription/Referral of its failure,
    // and the OutboundSubscription/Referral terminates. Thus, the outbound
    // action terminates before it's even added to the stack's Actions list!
    // Remember, this can only happen when we allow processing of a response
    // halfway through the processing of a (hairpinned) request, and in a real
    // stack (i.e., one that doesn't use a MockTransport) that can't happen.

    Sub.FirstContact.Address := Self.Destination.Address;
    Sub.Event.EventPackage   := EventPackage;

    if (ExpiryTime > 0) then
      Sub.Expires.NumericValue := ExpiryTime
    else
      Sub.Expires.NumericValue := Self.Module.DefaultSubscriptionDuration;

    Self.ReceiveRequest(Sub);
  finally
    Sub.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipSubscribeModule                                                  *
//******************************************************************************
//* TestTIdSipSubscribeModule Public methods ***********************************

procedure TestTIdSipSubscribeModule.SetUp;
begin
  inherited SetUp;

  Self.Core.AddListener(Self);
  Self.Core.InviteModule.AddListener(Self);
end;

//* TestTIdSipSubscribeModule Private methods **********************************

procedure TestTIdSipSubscribeModule.CheckBadEventResponseSent(const UnknownEvent: String);
var
  MsgPrefix: String;
begin
  MsgPrefix := '"' + UnknownEvent + '": ';

  CheckResponseSent(MsgPrefix + 'No response sent');
  CheckEquals(SIPBadEvent,
              Self.LastSentResponse.StatusCode,
              MsgPrefix + 'Unexpected response');
  CheckHasHeader(Self.LastSentResponse,
                 AllowEventsHeaderFull,
                 MsgPrefix);
  CheckEquals(Self.Module.AllowedEvents,
              Self.LastSentResponse.FirstHeader(AllowEventsHeaderFull).Value,
              MsgPrefix + 'Wrong Allow-Events value');
end;


procedure TestTIdSipSubscribeModule.CheckNoPackageFound(PackageType: TIdSipEventPackageClass);
begin
  Check(not Assigned(Self.Module.Package(PackageType.EventPackage)),
        PackageType.EventPackage + ' found');
end;

procedure TestTIdSipSubscribeModule.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                                              Message: TIdSipMessage;
                                                              Binding: TIdConnectionBindings);
begin
  // Do nothing.
end;

procedure TestTIdSipSubscribeModule.CheckPackageFound(PackageType: TIdSipEventPackageClass);
begin
  Check(Assigned(Self.Module.Package(PackageType.EventPackage)),
        PackageType.EventPackage + ' package not found');
  CheckEquals(PackageType.ClassName,
              Self.Module.Package(PackageType.EventPackage).ClassName,
              'Wrong package found for package ' + PackageType.EventPackage);
end;

procedure TestTIdSipSubscribeModule.OnAddAction(UserAgent: TIdSipAbstractCore;
                                                Action: TIdSipAction);
begin
  // Do nothing.
end;

procedure TestTIdSipSubscribeModule.OnInboundCall(UserAgent: TIdSipInviteModule;
                                                  Session: TIdSipInboundSession);
begin
  Self.InboundCall := Session;
end;

procedure TestTIdSipSubscribeModule.OnRemoveAction(UserAgent: TIdSipAbstractCore;
                                                   Action: TIdSipAction);
begin
  // Do nothing.
end;

procedure TestTIdSipSubscribeModule.ReceiveNotify(const EventPackage: String);
var
  RemoteDialog: TIdSipDialog;
  Notify:       TIdSipRequest;
  Ok:           TIdSipResponse;
  Sub:          TIdSipRequest;
begin
  Sub := Self.Module.CreateSubscribe(Self.Core.From, Self.Destination, EventPackage, TIdSipRequest.DefaultMaxForwards);
  try
    Ok := TIdSipResponse.InResponseTo(Sub, SIPOK, Sub.FirstContact);
    try
      RemoteDialog := TIdSipDialog.CreateOutboundDialog(Sub, Ok, false);
      try
        Notify := Self.Module.CreateNotify(RemoteDialog,
                                           Sub,
                                           SubscriptionSubstateActive);
        try
          Self.ReceiveRequest(Notify);
        finally
          Notify.Free;
        end;
      finally
        RemoteDialog.Free;
      end;
    finally
      Ok.Free;
    end;
  finally
    Sub.Free;
  end;
end;

procedure TestTIdSipSubscribeModule.ReceiveReferWithNoReferToHeader;
var
  Refer: TIdSipRequest;
begin
  Refer := Self.Module.CreateRefer(Self.Core.From, Self.Destination, Self.Core.From, TIdSipRequest.DefaultMaxForwards);
  try
    // See the comment in TSubscribeTestCase.ReceiveSubscribe.
    Refer.FirstContact.Address := Self.Destination.Address;
    Refer.RemoveAllHeadersNamed(ReferToHeaderFull);
    Self.ReceiveRequest(Refer);
  finally
    Refer.Free;
  end;
end;

procedure TestTIdSipSubscribeModule.ReceiveSubscribeWithNoEventHeader;
var
  MalformedSub: TIdSipRequest;
begin
  MalformedSub := Self.Module.CreateSubscribe(Self.Core.From,
                                              Self.Destination,
                                              TIdSipTestPackage.EventPackage, TIdSipRequest.DefaultMaxForwards);
  try
    MalformedSub.RemoveAllHeadersNamed(EventHeaderFull);

    Self.ReceiveRequest(MalformedSub);
  finally
    MalformedSub.Free;
  end;
end;

//* TestTIdSipSubscribeModule Published methods ********************************

procedure TestTIdSipSubscribeModule.TestAcceptsMethodsWithReferPackage;
begin
  Check(Pos(MethodRefer, Self.Module.AcceptsMethods) = 0,
        'REFER method supported when the package isn''t');

  Self.Module.AddPackage(TIdSipReferPackage);

  Check(Pos(MethodRefer, Self.Module.AcceptsMethods) > 0,
        'REFER method not supported when the package is');
end;

procedure TestTIdSipSubscribeModule.TestAddListener;
var
  L: TIdSipTestSubscribeModuleListener;
begin
  L := TIdSipTestSubscribeModuleListener.Create;
  try
    Self.Module.AddListener(L);

    Self.ReceiveSubscribe(TIdSipTestPackage.EventPackage);

    Check(L.SubscriptionRequest,
          'Listener not notified of subscription request');

    // Remember, as soon as we send a 2xx, we must send a NOTIFY. Thus the
    // LastRequest is a NOTIFY and SecondLastRequest's a SUBSCRIBE.
    //
    // Further, this altering of the transport request looks downright wrong,
    // but remember that we have to alter the subscription's InitialRequest -
    // adding a To header - because we establish a dialog off the InitialRequest
    // and the sent 202 Accepted.
    //
    // We also use .Match() instead of .Equals() because the transport layer can
    // add a received param to the topmost Via.
    Self.SecondLastSentRequest.ToHeader.Tag := Self.LastSentResponse.ToHeader.Tag;
    Check(Self.SecondLastSentRequest.Match(L.SubscriptionParam.InitialRequest),
          'Subscription param');
    Check(L.UserAgentParam = Self.Core,
          'UserAgent param');
  finally
    L.Free;
  end;
end;

procedure TestTIdSipSubscribeModule.TestAddPackage;
begin
  Self.Module.RemoveAllPackages;

  CheckEquals('',
              Self.Module.AllowedEvents,
              'Initially, allow no events');

  Self.Module.AddPackage(TIdSipReferPackage);
  CheckEquals(TIdSipReferPackage.EventPackage,
              Self.Module.AllowedEvents,
              'After adding ' + TIdSipReferPackage.EventPackage + ' package');

  Self.Module.AddPackage(TIdSipTestPackage);
  CheckEquals(TIdSipReferPackage.EventPackage + ', '
            + TIdSipTestPackage.EventPackage,
              Self.Module.AllowedEvents,
              'After adding ' + TIdSipTestPackage.EventPackage + ' package');
end;

procedure TestTIdSipSubscribeModule.TestAddPackageTwice;
var
  OldAllowedEvents: String;
begin
  Self.Module.AddPackage(TIdSipTestPackage);
  OldAllowedEvents := Self.Module.AllowedEvents;

  Self.Module.AddPackage(TIdSipTestPackage);
  CheckEquals(OldAllowedEvents, Self.Module.AllowedEvents, 'Event package duplicate added'); 
end;

procedure TestTIdSipSubscribeModule.TestAddReferPackageTwice;
var
  OldAllowedMethods: String;
begin
  Self.Module.RemoveAllPackages;

  Self.Module.AddPackage(TIdSipReferPackage);

  OldAllowedMethods := Self.Module.AcceptsMethods;

  Self.Module.AddPackage(TIdSipReferPackage);
  CheckEquals(OldAllowedMethods,
              Self.Module.AcceptsMethods,
              'Re-adding the "refer" package messed up AcceptsMethods');
end;

procedure TestTIdSipSubscribeModule.TestCreateRefer;
const
  MaxForwards = 42;
var
  Refer: TIdSipRequest;
begin
  Refer := Self.Module.CreateRefer(Self.Core.From, Self.Destination, Self.Destination, MaxForwards);
  try
    Check(Refer.HasHeader(ReferToHeaderFull), 'No Refer-To header');

    CheckEquals(Self.Core.From.DisplayName,
                Refer.From.DisplayName,
                'From.DisplayName');
    CheckEquals(Self.Core.From.Address,
                Refer.From.Address,
                'From.Address');
    Check(Refer.From.HasTag,
          'Requests MUST have a From tag; cf. RFC 3261 section 8.1.1.3');

    CheckEquals(MaxForwards,                Refer.MaxForwards,        'Max-Forwards');
    CheckEquals(Self.Destination.FullValue, Refer.ReferTo.FullValue,  'Refer-To');
    CheckEquals(Self.Destination.FullValue, Refer.ToHeader.FullValue, 'To');
  finally
    Refer.Free;
  end;
end;

procedure TestTIdSipSubscribeModule.TestCreateSubscribe;
const
  MaxForwards = 42;
var
  Subscribe: TIdSipRequest;
begin
  Subscribe := Self.Module.CreateSubscribe(Self.Core.From, Self.Destination, TIdSipTestPackage.EventPackage, MaxForwards);
  try
    Check(Subscribe.HasHeader(EventHeaderFull), 'No Event header');

    CheckEquals(TIdSipTestPackage.EventPackage, Subscribe.Event.EventPackage, 'Event');

    CheckEquals(Self.Core.From.DisplayName,
                Subscribe.From.DisplayName,
                'From.DisplayName');
    CheckEquals(Self.Core.From.Address,
                Subscribe.From.Address,
                'From.Address');
    Check(Subscribe.From.HasTag,
          'Requests MUST have a From tag; cf. RFC 3261 section 8.1.1.3');

    CheckEquals(MaxForwards,                    Subscribe.MaxForwards,        'Max-Forwards');
    CheckEquals(Self.Destination.FullValue,     Subscribe.ToHeader.FullValue, 'To');
  finally
    Subscribe.Free;
  end;
end;

procedure TestTIdSipSubscribeModule.TestPackage;
begin
  Self.CheckPackageFound(TIdSipTestPackage);

  Self.CheckNoPackageFound(TIdSipReferPackage);

  Self.Module.AddPackage(TIdSipReferPackage);

  Self.CheckPackageFound(TIdSipReferPackage);
end;

procedure TestTIdSipSubscribeModule.TestPackageFor;
var
  ReferPkg: TIdSipEventPackage;
  Request:  TIdSipRequest;
begin
  Self.Module.AddPackage(TIdSipReferPackage);
  ReferPkg := Self.Module.Package(PackageRefer);

  Request := TIdSipRequest.Create;
  try
    Check(nil = Self.Module.PackageFor(Request),
          'No method');

    Request.Method := MethodSubscribe;
    Check(nil = Self.Module.PackageFor(Request),
          'SUBSCRIBE with no Event header');

    Request.AddHeader(EventHeaderFull);
    Check(nil = Self.Module.PackageFor(Request),
          'Blank Event header');

    Request.Event.EventPackage := 'x-' + Self.Package.EventPackage;
    Check(nil = Self.Module.PackageFor(Request),
          'Event header with unknown event package');

    Request.Event.EventPackage := Self.Package.EventPackage;
    Check(Self.Package = Self.Module.PackageFor(Request),
          'Event header with known event package');

    Request.Event.EventPackage := TIdSipReferPackage.EventPackage;
    Check(nil <> Self.Module.PackageFor(Request),
          'SUBSCRIBE with refer Event header');

    Request.Method := MethodRefer;
    Check(ReferPkg = Self.Module.PackageFor(Request),
          'REFER with Event header');

    Request.RemoveAllHeadersNamed(EventHeaderFull);
    Check(ReferPkg = Self.Module.PackageFor(Request),
          'Well-formed REFER');

    Self.Module.RemoveAllPackages;
    Check(nil = Self.Module.PackageFor(Request),
          'Well-formed REFER but Module doesn''t support refer package');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipSubscribeModule.TestReceiveReferWithNoReferTo;
begin
  Self.Module.AddPackage(TIdSipReferPackage);

  Self.MarkSentResponseCount;
  Self.ReceiveReferWithNoReferToHeader;
  CheckResponseSent('No response sent');
  CheckEquals(SIPBadRequest,
              Self.LastSentResponse.StatusCode,
              'Unexpected response');
end;

procedure TestTIdSipSubscribeModule.TestReceiveReferNotifiesListeners;
begin
  Self.Module.AddPackage(TIdSipReferPackage);

  Self.ReceiveRefer(Self.Core.From);

  Check(Self.OnSubscriptionRequestFired, 'OnSubscriptionRequest didn''t fire');
  Check(Self.Core = Self.UserAgentParam,
        'UserAgent param of Subscribe''s SubscriptionRequest notification wrong');
  CheckEquals(TIdSipInboundReferral.ClassName,
              Self.Subscription.ClassName,
              'Module didn''t create the correct (inbound) subscription)');
end;

procedure TestTIdSipSubscribeModule.TestRefer;
var
  Refer: TIdSipOutboundReferral;
begin
  Self.Module.AddPackage(TIdSipReferPackage);

  Refer := Self.Module.Refer(Self.Destination, Self.Core.From);
  Check(Assigned(Refer),
        'Result not assigned');

  Self.MarkSentRequestCount;
  Refer.Send;
  CheckRequestSent('No request sent');
  CheckEquals(MethodRefer,
              Self.LastSentRequest.Method,
              'Unexpected request sent');
end;

procedure TestTIdSipSubscribeModule.TestRejectNewSubscribeForReferPackage;
var
  Response: TIdSipResponse;
begin
  Self.Module.AddPackage(TIdSipReferPackage);

  Self.MarkSentResponseCount;
  Self.ReceiveSubscribe(PackageRefer);

  CheckResponseSent('No response sent');
  Response := Self.LastSentResponse;
  CheckEquals(SIPForbidden,
              Response.StatusCode,
              'RFC 3515 section 2.4.4: reject a SUBSCRIBE with Event: refer if '
            + 'it doesn''t match an existing subscription');
end;

procedure TestTIdSipSubscribeModule.TestRejectSubscribeWithNoEventHeader;
begin
  Self.MarkSentResponseCount;
  Self.ReceiveSubscribeWithNoEventHeader;

  CheckBadEventResponseSent('');

  CheckResponseSent('No response sent');
  CheckEquals(SIPBadEvent,
              Self.LastSentResponse.StatusCode,
              'Unexpected response');
end;

procedure TestTIdSipSubscribeModule.TestRejectUnknownEventSubscriptionRequest;
const
  UnknownEvent = 'Foo.bar';
begin
  Self.MarkSentResponseCount;

  Self.ReceiveSubscribe(UnknownEvent);

  CheckBadEventResponseSent(UnknownEvent);
end;

procedure TestTIdSipSubscribeModule.TestRejectUnmatchedNotify;
begin
  Self.MarkSentResponseCount;

  Self.ReceiveNotify(TIdSipTestPackage.EventPackage);

  CheckResponseSent('No response sent');
  CheckEquals(SIPCallLegOrTransactionDoesNotExist,
              Self.LastSentResponse.StatusCode,
              'Unexpected response sent');
end;

procedure TestTIdSipSubscribeModule.TestRemoveListener;
var
  L: TIdSipTestSubscribeModuleListener;
begin
  L := TIdSipTestSubscribeModuleListener.Create;
  try
    Self.Module.AddListener(L);
    Self.Module.RemoveListener(L);

    Self.ReceiveSubscribe(TIdSipTestPackage.EventPackage);

    Check(not L.SubscriptionRequest,
          'Listener notified of subscription request, thus not removed');
  finally
    L.Free;
  end;
end;

procedure TestTIdSipSubscribeModule.TestSubscribe;
var
  Sub: TIdSipOutboundSubscription;
begin
  Sub := Self.Module.Subscribe(Self.Destination, TIdSipTestPackage.EventPackage);

  Self.MarkSentRequestCount;
  Sub.Send;
  CheckRequestSent('No request sent');
  CheckEquals(MethodSubscribe,
              Self.LastSentRequest.Method,
              'Unexpected response sent');

  CheckEquals(TIdSipTestPackage.EventPackage,
              Self.LastSentRequest.Event.EventPackage,
              'Event header');
end;

procedure TestTIdSipSubscribeModule.TestSubscriptionRequest;
begin
  Self.Module.AddPackage(TIdSipTestPackage);

  Self.ReceiveSubscribe(TIdSipTestPackage.EventPackage);

  Check(Self.OnSubscriptionRequestFired, 'OnSubscriptionRequest didn''t fire');
  Check(Self.Core = Self.UserAgentParam,
        'UserAgent param of Subscribe''s SubscriptionRequest notification wrong');
end;

procedure TestTIdSipSubscribeModule.TestTransfer;
var
  Session:      TIdSipSession;
  TargetDialog: TIdSipDialogID;
begin
  // Make sure we support the "refer" package.
  Self.Module.AddPackage(TIdSipReferPackage);

  // Establish a session
  Session := Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '');
  Session.Send;
  Self.ReceiveOk(Session.InitialRequest);

  // Now transfer the call to someone else
  TargetDialog := Session.Dialog.ID.GetRemoteID;
  try
    Self.MarkSentRequestCount;
    Self.Module.Transfer(Self.Destination, Self.Destination, TargetDialog).Send;
    CheckRequestSent('No request sent');

    CheckEquals(MethodRefer,
                Self.LastSentRequest.Method,
                'Unexpected request sent');
    Check(Self.LastSentRequest.HasHeader(TargetDialogHeader),
          'REFER''s missing a Target-Dialog header');
    CheckEquals(Session.Dialog.ID.CallID,
                Self.LastSentRequest.TargetDialog.CallID,
                'Target-Dialog call-id');
    CheckEquals(Session.Dialog.ID.LocalTag,
                Self.LastSentRequest.TargetDialog.RemoteTag,
                'Target-Dialog remote-tag');
    CheckEquals(Session.Dialog.ID.RemoteTag,
                Self.LastSentRequest.TargetDialog.LocalTag,
                'Target-Dialog local-tag');
  finally
    TargetDialog.Free;
  end;
end;

procedure TestTIdSipSubscribeModule.TestUAAllowedContentTypes;
begin
  Check(Pos(SipFragmentMimeType, Self.Core.AllowedContentTypes) > 0,
        'UA with SubscribeModule doesn''t know how to handle '
      + SipFragmentMimeType + ' MIME type');
end;

//******************************************************************************
//* TestTIdSipUserAgentWithSubscribeModule                                     *
//******************************************************************************
//* TestTIdSipUserAgentWithSubscribeModule Public methods **********************

procedure TestTIdSipUserAgentWithSubscribeModule.SetUp;
begin
  inherited SetUp;

  Self.Module := Self.Core.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
end;

//* TestTIdSipUserAgentWithSubscribeModule Private methods *********************

procedure TestTIdSipUserAgentWithSubscribeModule.ReceiveOptions;
var
  Module:  TIdSipOptionsModule;
  Options: TIdSipRequest;
  Temp:    String;
begin
  Module := Self.Core.ModuleFor(MethodOptions) as TIdSipOptionsModule;

  Options := Module.CreateOptions(Self.Core.From, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    // Swop To & From because this comes from the network
    Temp := Options.From.FullValue;
    Options.From.Value := Options.ToHeader.FullValue;
    Options.ToHeader.Value := Temp;

    Self.ReceiveRequest(Options);
  finally
    Options.Free;
  end;
end;

//* TestTIdSipUserAgentWithSubscribeModule Published methods *******************

procedure TestTIdSipUserAgentWithSubscribeModule.TestReceiveNotifyForUnmatchedDialog;
var
  Notify:   TIdSipRequest;
  Response: TIdSipResponse;
begin
  Notify := Self.Core.CreateRequest(MethodInvite, Self.Core.From, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    Notify.Method          := MethodNotify;
    Notify.CSeq.SequenceNo := $deadbeef;
    Notify.CSeq.Method     := Notify.Method;
    Notify.AddHeader(EventHeaderFull).Value         := 'UnsupportedEvent';
    Notify.AddHeader(SubscriptionStateHeader).Value := 'Foo';

    Self.MarkSentResponseCount;

    Self.ReceiveRequest(Notify);

    CheckResponseSent('No response sent');
    Response := Self.LastSentResponse;
    CheckEquals(SIPCallLegOrTransactionDoesNotExist,
                Response.StatusCode,
                'Response Status-Code')

  finally
    Notify.Free;
  end;
end;

procedure TestTIdSipUserAgentWithSubscribeModule.TestReceiveOptions;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;

  Self.ReceiveOptions;

  CheckResponseSent('No response sent');
  Response := Self.LastSentResponse;

  Check(Response.HasHeader(AllowEventsHeaderFull),
        'No Allow-Events header');
  CheckEquals(Self.Module.AllowedEvents,
              Response.FirstHeader(AllowEventsHeaderFull).Value,
              'Allow-Events value');
end;

procedure TestTIdSipUserAgentWithSubscribeModule.TestSendInvite;
var
  Invite:    TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '').Send;

  CheckRequestSent('No request sent');

  Invite := Self.LastSentRequest;
  Check(Invite.HasHeader(AllowEventsHeaderFull),
        'No Allow-Events header');
  CheckEquals(Self.Module.AllowedEvents,
              Invite.FirstHeader(AllowEventsHeaderFull).Value,
              'Allow-Events value');
end;

//******************************************************************************
//* TestCallFlows                                                              *
//******************************************************************************
//* TestCallFlows Public methods ***********************************************

procedure TestCallFlows.SetUp;
begin
  inherited SetUp;

  Self.Module.AddPackage(TIdSipReferPackage);
end;

//* TestCallFlows Private methods **********************************************

function TestCallFlows.CreateAndEstablishInboundCall: TIdSipOutboundSession;
begin
  Result := Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '');
  Result.Send;

  Self.MarkSentAckCount;
  Self.ReceiveOk(Result.InitialRequest);
  CheckAckSent('No ACK sent for call setup');
end;

//* TestCallFlows Published methods ********************************************

procedure TestCallFlows.TestCallTransferred;
var
  Refer: TIdSipInboundReferral;
begin
  //  ---        INVITE       --->
  // <---        200 OK       ---
  //  ---         ACK         --->
  //        <media streams>
  // <---        REFER        ---
  //  ---     202 Accepted    --->
  //  --- NOTIFY (100 Trying) --->
  // <---        200 OK       ---
  //    < follow the Refer-To>
  //  --- NOTIFY (200 OK)     --->
  // <---        200 OK       ---
  // ?????????????????????????????

  Self.CreateAndEstablishInboundCall;

  Self.ReceiveRefer(Self.LastSentRequest.FirstContact);
  Check(Assigned(Self.Subscription), 'No REFER/SUBSCRIBE received');
  CheckEquals(TIdSipInboundReferral.ClassName,
              Self.Subscription.ClassName,
              'Wrong kind of subscription');
  Refer := Self.Subscription as TIdSipInboundReferral;
  Refer.Accept;
  Refer.ReferenceSucceeded;
end;

//******************************************************************************
//* TestTIdSipEventPackageRegistry                                             *
//******************************************************************************
//* TestTIdSipEventPackageRegistry Published methods ***************************

procedure TestTIdSipEventPackageRegistry.TestRegisterUnregisterPackage;
var
  PackageType: TIdSipEventPackageClass;
begin
  PackageType := TIdSipTestPackage;

  TIdSipEventPackageRegistry.RegisterEvent(PackageType);
  try
    CheckEquals(PackageType.ClassName,
                TIdSipEventPackageRegistry.EventFor(PackageType.EventPackage).ClassName,
                'Incorrect registration/mapping');
    Check(TIdSipEventPackageRegistry.IsRegistered(PackageType.EventPackage),
          'Event package not registered');
  finally
    TIdSipEventPackageRegistry.UnregisterEvent(PackageType);
  end;

  Check(not TIdSipEventPackageRegistry.IsRegistered(PackageType.EventPackage),
        'Event package not unregistered');
end;

procedure TestTIdSipEventPackageRegistry.TestUnregisterUnregisteredPackage;
var
  PackageType: TIdSipEventPackageClass;
begin
  PackageType := TIdSipTestPackage;

  Check(not TIdSipEventPackageRegistry.IsRegistered(PackageType.EventPackage),
        'Event package registered in a supposedly empty register');

  TIdSipEventPackageRegistry.UnregisterEvent(PackageType);

  Check(not TIdSipEventPackageRegistry.IsRegistered(PackageType.EventPackage),
        'Event package not unregistered');
end;

//******************************************************************************
//* TestTIdSipEventPackage                                                     *
//******************************************************************************
//* TestTIdSipEventPackage Published methods ***********************************

procedure TestTIdSipEventPackage.TestProbationRetryTime;
const
  ArbProbationRetryTime = 1000;
begin
  CheckEquals(Self.Package.DefaultProbationRetryTime,
              Self.Package.ProbationRetryTime,
              'Default ProbationRetryTime');

  Self.Package.ProbationRetryTime := ArbProbationRetryTime;
  CheckEquals(ArbProbationRetryTime,
              Self.Package.ProbationRetryTime,
              'ProbationRetryTime');
end;

//******************************************************************************
//* TestTIdSipSubscribe                                                        *
//******************************************************************************
//* TestTIdSipSubscribe Public methods *****************************************

procedure TestTIdSipSubscribe.SetUp;
begin
  inherited SetUp;

  Self.Module := Self.Core.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  Self.Module.AddPackage(TIdSipTestPackage);
end;

//******************************************************************************
//* TestTIdSipOutboundNotifyBase                                               *
//******************************************************************************
//* TestTIdSipOutboundNotifyBase Public methods ********************************

procedure TestTIdSipOutboundNotifyBase.SetUp;
var
  Ok:          TIdSipResponse;
  RemoteParty: TIdSipFromHeader;
begin
  inherited SetUp;

  Self.Body              := 'random data';
  Self.MimeType          := 'text/plain';
  Self.SubscriptionState := SubscriptionSubstateActive;

  // Self.Subscribe contains a SUBSCRIBE we receive from sip:case@remotehost.
  // CreateSubscribe creates a SUBSCRIBE that WE SEND, so we alter the request
  // by hand to make it look like it comes from the network.
  RemoteParty := TIdSipFromHeader.Create;
  try
    RemoteParty.Assign(Self.Destination);
    Self.Subscribe := Self.Module.CreateSubscribe(RemoteParty,
                                                  Self.Destination,
                                                  TIdSipTestPackage.EventPackage,
                                                  TIdSipRequest.DefaultMaxForwards);
  finally
    RemoteParty.Free;
  end;

  Self.Subscribe.RequestUri           := Self.Core.From.Address;
  Self.Subscribe.From.Address         := Self.Destination.Address;
  Self.Subscribe.ToHeader.Address     := Self.Core.From.Address;
  Self.Subscribe.FirstContact.Address := Self.Destination.Address;
  Self.Subscribe.LastHop.SentBy       := Self.Destination.Address.Host;

  Ok := Self.Core.CreateResponse(Self.Subscribe, SIPOK);
  try
    Self.Dialog := TIdSipDialog.CreateInboundDialog(Self.Subscribe, Ok, false);
  finally
    Ok.Free;
  end;

  Self.Notify := Self.CreateAction as TIdSipOutboundNotifyBase;
end;

procedure TestTIdSipOutboundNotifyBase.TearDown;
begin
  Self.Dialog.Free;
  Self.Subscribe.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundNotifyBase Protected methods *****************************

procedure TestTIdSipOutboundNotifyBase.ConfigureNotify(Action: TIdSipOutboundNotifyBase);
begin
  Action.Body      := Self.Body;
  Action.Dialog    := Self.Dialog;
  Action.MimeType  := Self.MimeType;
  Action.Subscribe := Self.Subscribe;
  Action.LocalGruu := Self.Subscribe.FirstContact;

  Action.AddActionListener(Self);
  Action.AddNotifyListener(Self);
end;

procedure TestTIdSipOutboundNotifyBase.OnFailure(NotifyAgent: TIdSipOutboundNotify;
                                                 Response: TIdSipResponse);
begin
end;

procedure TestTIdSipOutboundNotifyBase.OnSuccess(NotifyAgent: TIdSipOutboundNotify;
                                                 Response: TIdSipResponse);
begin
end;

//* TestTIdSipOutboundNotifyBase Published methods *****************************

procedure TestTIdSipOutboundNotifyBase.TestIsOwned;
begin
  Check(Self.Notify.IsOwned,
        Self.Notify.ClassName + ' isn''t Owned');
end;

procedure TestTIdSipOutboundNotifyBase.TestMethod;
begin
  CheckEquals(MethodNotify,
              Self.Notify.Method,
              Self.Notify.ClassName + '; Method');
end;

procedure TestTIdSipOutboundNotifyBase.TestSend;
var
  Notify: TIdSipRequest;
begin
  Check(Self.LastSentRequest <> nil,
        Self.ClassName + ': No request sent');

  Notify := Self.LastSentRequest;
  CheckEquals(MethodNotify,
              Notify.Method,
              Self.ClassName + ': Unexpected request sent');

  Check(Notify.HasHeader(EventHeaderFull),
        Self.ClassName + ': No Event header');
  CheckEquals(Self.Subscribe.Event.EventPackage,
              Notify.Event.EventPackage,
              Self.ClassName + ': Event value');
  CheckEquals(Self.Subscribe.Event.ID,
              Notify.Event.ID,
              Self.ClassName + ': Event id parameter');

  Check(Notify.HasHeader(SubscriptionStateHeader),
        Self.ClassName + ': No Subscription-State header');
  CheckEquals(Self.SubscriptionState,
              Notify.SubscriptionState.SubState,
              Self.ClassName + ': Unexpected substate');
end;

//******************************************************************************
//* TestTIdSipOutboundNotify                                                   *
//******************************************************************************
//* TestTIdSipOutboundNotify Public methods ************************************

procedure TestTIdSipOutboundNotify.SetUp;
begin
  Self.Expires := OneHour;

  inherited SetUp;
end;

//* TestTIdSipOutboundNotify Protected methods *********************************

procedure TestTIdSipOutboundNotify.ConfigureNotify(Action: TIdSipOutboundNotifyBase);
var
  Notify: TIdSipOutboundNotify;
begin
  inherited ConfigureNotify(Action);

  Notify := Action as TIdSipOutboundNotify;

  Notify.Expires           := Self.Expires;
  Notify.SubscriptionState := Self.SubscriptionState;
end;

function TestTIdSipOutboundNotify.CreateAction: TIdSipAction;
var
  Sub: TIdSipOutboundNotify;
begin
  Sub := Self.Core.AddOutboundAction(TIdSipOutboundNotify) as TIdSipOutboundNotify;
  Self.ConfigureNotify(Sub);
  Sub.Send;

  Result := Sub;
end;

//* TestTIdSipOutboundNotify Published methods *********************************

procedure TestTIdSipOutboundNotify.TestAddListener;
var
  L:      TIdSipTestNotifyListener;
  Notify: TIdSipOutboundNotify;
begin
  L := TIdSipTestNotifyListener.Create;
  try
    Notify := Self.Core.AddOutboundAction(TIdSipOutboundNotify) as TIdSipOutboundNotify;
    Self.ConfigureNotify(Notify);
    Notify.AddNotifyListener(L);
    Notify.Send;
    Self.ReceiveServiceUnavailable(Self.LastSentRequest);

    Check(L.Failed,
          'Notify didn''t notify listener of failure: Listener not added');
  finally
    L.Free;
  end;
end;

procedure TestTIdSipOutboundNotify.TestRemoveListener;
var
  L:      TIdSipTestNotifyListener;
  Notify: TIdSipOutboundNotify;
begin
  L := TIdSipTestNotifyListener.Create;
  try
    Notify := Self.Core.AddOutboundAction(TIdSipOutboundNotify) as TIdSipOutboundNotify;
    Self.ConfigureNotify(Notify);
    Notify.AddNotifyListener(L);
    Notify.RemoveNotifyListener(L);
    Notify.Send;
    Self.ReceiveServiceUnavailable(Self.LastSentRequest);

    Check(not L.Failed,
          'Notify notify listener of failure: Listener not removed');
  finally
    L.Free;
  end;
end;

procedure TestTIdSipOutboundNotify.TestSend;
var
  Notify: TIdSipRequest;
begin
  inherited TestSend;

  Notify := Self.LastSentRequest;

  CheckEquals(Self.Expires,
              Notify.SubscriptionState.Expires,
              Self.ClassName + ': Subscription-State expire param');

  CheckEquals(Body,
              Notify.Body,
              Self.ClassName + ': Notify body');
  Check(Notify.HasHeader(ContentLengthHeaderFull),
        Self.ClassName + ': Notify has no Content-Length header');
  CheckEquals(Notify.ContentLength,
              Notify.ContentLength,
              Self.ClassName + ': Notify Content-Length');
  Check(Notify.HasHeader(ContentTypeHeaderFull),
        Self.ClassName + ': Notify has no Content-Type header');
  CheckEquals(Notify.ContentType,
              Notify.ContentType,
              Self.ClassName + ': Notify Content-Type');
end;

//******************************************************************************
//* TestTIdSipOutboundTerminatingNotify                                        *
//******************************************************************************
//* TestTIdSipOutboundTerminatingNotify Public methods *************************

procedure TestTIdSipOutboundTerminatingNotify.SetUp;
begin
  Self.Reason := EventReasonNoResource;

  inherited SetUp;

  Self.SubscriptionState := SubscriptionSubstateTerminated;
end;

//* TestTIdSipOutboundTerminatingNotify Protected methods **********************

procedure TestTIdSipOutboundTerminatingNotify.ConfigureNotify(Action: TIdSipOutboundNotifyBase);
var
  Term: TIdSipOutboundTerminatingNotify;
begin
  inherited ConfigureNotify(Action);

  Term := Action as TIdSipOutboundTerminatingNotify;
  Term.Reason := Self.Reason;
end;

function TestTIdSipOutboundTerminatingNotify.CreateAction: TIdSipAction;
var
  Sub: TIdSipOutboundTerminatingNotify;
begin
  Sub := Self.Core.AddOutboundAction(TIdSipOutboundTerminatingNotify) as TIdSipOutboundTerminatingNotify;
  Self.ConfigureNotify(Sub);
  Sub.Send;

  Result := Sub;
end;

//* TestTIdSipOutboundTerminatingNotify Published methods **********************

procedure TestTIdSipOutboundTerminatingNotify.TestSend;
var
  Notify: TIdSipRequest;
begin
  inherited TestSend;

  Notify := Self.LastSentRequest;
  CheckEquals(Self.Reason,
              Notify.SubscriptionState.Reason,
              Self.ClassName + ': Subscription-State''s reason');
end;

//******************************************************************************
//* TestTIdSipOutboundSubscribe                                                *
//******************************************************************************
//* TestTIdSipOutboundSubscribe Public methods *********************************

procedure TestTIdSipOutboundSubscribe.SetUp;
begin
  inherited SetUp;

  Self.EventID      := 'id1';
  Self.EventPackage := TIdSipTestPackage.EventPackage;
  Self.Failed       := false;
  Self.Succeeded    := false;
end;

//* TestTIdSipOutboundSubscribe Protected methods ******************************

procedure TestTIdSipOutboundSubscribe.ConfigureAction(Action: TIdSipAction);
var
  Sub: TIdSipOutboundSubscribe;
begin
  Sub := Action as TIdSipOutboundSubscribe;

  Sub.Target       := Self.Destination;
  Sub.EventPackage := Self.EventPackage;
  Sub.EventID      := Self.EventID;
  Sub.AddActionListener(Self);
  Sub.AddOwnedActionListener(Self);
end;

function TestTIdSipOutboundSubscribe.CreateAction: TIdSipAction;
var
  Sub: TIdSipOutboundSubscribe;
begin
  Sub := Self.Core.AddOutboundAction(TIdSipOutboundSubscribe) as TIdSipOutboundSubscribe;
  Self.ConfigureAction(Sub);
  Sub.Send;

  Result := Sub;
end;

//* TestTIdSipOutboundSubscribe Private methods ********************************

procedure TestTIdSipOutboundSubscribe.OnFailure(Action: TIdSipAction;
                                                Response: TIdSipResponse;
                                                const Reason: String);
begin
  Self.Failed := true;
end;

procedure TestTIdSipOutboundSubscribe.OnRedirect(Action: TIdSipAction;
                                                 Redirect: TIdSipResponse);
begin
end;

procedure TestTIdSipOutboundSubscribe.OnSuccess(Action: TIdSipAction;
                                                Msg: TIdSipMessage);
begin
  Self.Succeeded := true;
end;

//* TestTIdSipOutboundSubscribe Published methods ******************************

procedure TestTIdSipOutboundSubscribe.TestIsOwned; 
var
  Sub: TIdSipAction;
begin
  Sub := Self.CreateAction;

  Check(Sub.IsOwned,
        Sub.ClassName + ' not marked as being owned');
end;

procedure TestTIdSipOutboundSubscribe.TestMatchNotify;
var
  Notify: TIdSipRequest;
  Sub:    TIdSipAction;
begin
  Sub := Self.CreateAction;

  Notify := TIdSipRequest.Create;
  try
    Notify.Method := MethodNotify;
    Notify.CallID := Sub.InitialRequest.CallID;
    Notify.AddHeader(Sub.InitialRequest.Event);
    Notify.ToHeader.Tag := Sub.InitialRequest.From.Tag;

    Check(not Sub.Match(Notify),
          'Matching request method, Call-ID, Event, From-tag-and-To-tag: Subscription must match this!');
  finally
    Notify.Free;
  end;
end;

procedure TestTIdSipOutboundSubscribe.TestMatchResponse;
var
  OK:  TIdSipResponse;
  Sub: TIdSipAction;
begin
  Sub := Self.CreateAction;

  OK := TIdSipResponse.Create;
  try
    OK.CSeq.Value := Sub.InitialRequest.CSeq.Value;
    Check(not Sub.Match(OK), 'Only matching CSeq');

    OK.CallID := Sub.InitialRequest.CallID;
    Check(not Sub.Match(OK), 'Only matching CSeq, Call-ID');

    OK.From.Tag := Sub.InitialRequest.From.Tag;
    Check(Sub.Match(OK), 'Only matching CSeq, Call-ID, From tag');
  finally
    OK.Free;
  end;
end;

procedure TestTIdSipOutboundSubscribe.TestMethod;
var
  Action: TIdSipAction;
begin
  Action := Self.CreateAction;

  CheckEquals(MethodSubscribe,
              Action.Method,
              Action.ClassName + '; Method');
end;

procedure TestTIdSipOutboundSubscribe.TestReceive2xx;
begin
  Self.CreateAction;
  Self.ReceiveOk(Self.LastSentRequest);

  Check(Self.Succeeded, 'Subscription didn''t succeed');
end;

procedure TestTIdSipOutboundSubscribe.TestReceiveFailure;
begin
  Self.CreateAction;
  Self.ReceiveResponse(SIPNotImplemented);

  Check(Self.Failed, 'Subscription didn''t fail');
end;

procedure TestTIdSipOutboundSubscribe.TestSend;
var
  Events: TIdSipHeadersFilter;
  Sub:    TIdSipAction;
begin
  Sub := Self.CreateAction;
  CheckEquals(Sub.Method,
              Sub.InitialRequest.Method,
              Self.ClassName + ': Method of request');
  Check(Sub.InitialRequest.HasHeader(ExpiresHeader),
        Self.ClassName + ': SHOULD have Expires header');
  Check(Sub.InitialRequest.HasHeader(EventHeaderFull),
        Self.ClassName + ': MUST have Event header');
  CheckEquals(Self.EventPackage,
              Sub.InitialRequest.Event.Value,
              Self.ClassName + ': Wrong Event header');
  CheckEquals(Self.EventID,
              Sub.InitialRequest.Event.ID,
              Self.ClassName + ': ID param of Event header');

  Events := TIdSipHeadersFilter.Create(Sub.InitialRequest.Headers, EventHeaderFull);
  try
    CheckEquals(1,
                Events.Count,
                Self.ClassName + ': Wrong number of Event headers');
  finally
    Events.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipOutboundRefreshSubscribe                                         *
//******************************************************************************
//* TestTIdSipOutboundRefreshSubscribe Public methods **************************

procedure TestTIdSipOutboundRefreshSubscribe.SetUp;
var
  Subscribe: TIdSipRequest;
  OK:        TIdSipResponse;
begin
  inherited SetUp;

  Self.ExpiresValue := 1000;

  Subscribe := Self.Module.CreateSubscribe(Self.Core.From, Self.Destination, TIdSipTestPackage.EventPackage, TIdSipRequest.DefaultMaxForwards);
  try
    OK := TIdSipResponse.InResponseTo(Subscribe, SIPOK);
    try
      // We need a Contact for the dialog's remote-target.
      OK.AddHeader(ContactHeaderFull).Value := 'sip:wintermute@remotehost';
      Self.Dialog := TIdSipDialog.CreateOutboundDialog(Subscribe,
                                                       OK,
                                                       false);
    finally
      OK.Free;
    end;
  finally
    Subscribe.Free;
  end;
end;

procedure TestTIdSipOutboundRefreshSubscribe.TearDown;
begin
  Self.Dialog.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundRefreshSubscribe Protected methods ***********************

procedure TestTIdSipOutboundRefreshSubscribe.ConfigureAction(Action: TIdSipAction);
var
  Refresh: TIdSipOutboundRefreshSubscribe;
begin
  inherited ConfigureAction(Action);

  Refresh := Action as TIdSipOutboundRefreshSubscribe;

  Refresh.Dialog    := Self.Dialog;
  Refresh.Duration  := Self.ExpiresValue;
end;


function TestTIdSipOutboundRefreshSubscribe.CreateAction: TIdSipAction;
begin
  Result := Self.CreateRefreshSubscribe;
end;

//* TestTIdSipOutboundRefreshSubscribe Private methods *************************

function TestTIdSipOutboundRefreshSubscribe.CreateRefreshSubscribe: TIdSipOutboundRefreshSubscribe;
begin
  Result := Self.Core.AddOutboundAction(TIdSipOutboundRefreshSubscribe) as TIdSipOutboundRefreshSubscribe;
  Self.ConfigureAction(Result);
  Result.Send;
end;

//* TestTIdSipOutboundRefreshSubscribe Published methods ***********************

procedure TestTIdSipOutboundRefreshSubscribe.TestReceiveIntervalTooBrief;
var
  MinExpires: Cardinal;
  Refresh:    TIdSipAction;
  SubCount:   Cardinal;
begin
  Refresh := Self.CreateAction;
  SubCount := Self.Core.CountOf(Refresh.Method);

  MinExpires := 2*Self.ExpiresValue;
  Self.MarkSentRequestCount;
  Self.ReceiveIntervalTooBrief(Refresh.InitialRequest, MinExpires);
  CheckRequestSent(Self.ClassName + ': No new attempt sent');
  CheckEquals(MethodSubscribe,
              Self.LastSentRequest.Method,
              'Unexpected request sent');
  Check(Self.LastSentRequest.HasHeader(ExpiresHeader),
        'Refreshing SUBSCRIBE missing an Expires header');
  CheckEquals(MinExpires,
              Self.LastSentRequest.Expires.NumericValue,
              'Wrong Expires value sent; Min-Expires value not honoured');
  CheckEquals(SubCount,
              Self.Core.CountOf(MethodSubscribe),
              'Action was erroneously terminated');
end;

procedure TestTIdSipOutboundRefreshSubscribe.TestSend;
begin
  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent(Self.ClassName + ': No request sent');
  CheckEquals(Self.ExpiresValue,
              Self.LastSentRequest.Expires.NumericValue,
              Self.ClassName + ': Wrong Expires value');
  CheckEquals(Self.Dialog.ID.CallID,
              Self.LastSentRequest.CallID,
              Self.ClassName + ': Call-ID');
  CheckEquals(Self.Dialog.ID.LocalTag,
              Self.LastSentRequest.From.Tag,
              Self.ClassName + ': From tag');
end;

//******************************************************************************
//* TestTIdSipOutboundUnsubscribe                                              *
//******************************************************************************
//* TestTIdSipOutboundUnsubscribe Public methods *******************************

procedure TestTIdSipOutboundUnsubscribe.SetUp;
begin
  inherited SetUp;

  Self.CallID  := 'random-callid@localhost';
  Self.FromTag := BranchMagicCookie + 'randomtoken';
end;

//* TestTIdSipOutboundUnsubscribe Protected methods ****************************

procedure TestTIdSipOutboundUnsubscribe.ConfigureAction(Action: TIdSipAction);
var
  Unsub: TIdSipOutboundUnsubscribe;
begin
  inherited ConfigureAction(Action);

  Unsub := Action as TIdSipOutboundUnsubscribe;

  Unsub.CallID  := Self.CallID;
  Unsub.FromTag := Self.FromTag;
end;

function TestTIdSipOutboundUnsubscribe.CreateAction: TIdSipAction;
begin
  Result := Self.CreateUnsubscribe;
end;

//* TestTIdSipOutboundUnsubscribe Private methods ******************************

function TestTIdSipOutboundUnsubscribe.CreateUnsubscribe: TIdSipOutboundUnsubscribe;
begin
  Result := Self.Core.AddOutboundAction(TIdSipOutboundUnsubscribe) as TIdSipOutboundUnsubscribe;
  Self.ConfigureAction(Result);
  Result.Send;
end;

//* TestTIdSipOutboundUnsubscribe Published methods ****************************

procedure TestTIdSipOutboundUnsubscribe.TestSend;
begin
  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent(Self.ClassName + ': No request sent');
  CheckEquals(0,
              Self.LastSentRequest.Expires.NumericValue,
              Self.ClassName + ': Wrong Expires value');
end;

//******************************************************************************
//* TestTIdSipOutboundRefer                                                    *
//******************************************************************************
//* TestTIdSipOutboundRefer Public methods *************************************

procedure TestTIdSipOutboundRefer.SetUp;
begin
  inherited SetUp;

  Self.Module.AddPackage(TIdSipReferPackage);

  Self.ReferTo := TIdSipToHeader.Create;
  Self.ReferTo.Assign(Self.Core.From);
end;

procedure TestTIdSipOutboundRefer.TearDown;
begin
  Self.ReferTo.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundRefer Protected methods **********************************

procedure TestTIdSipOutboundRefer.ConfigureAction(Action: TIdSipAction);
var
  Refer: TIdSipOutboundRefer;
begin
  inherited ConfigureAction(Action);

  Refer := Action as TIdSipOutboundRefer;

  Refer.ReferTo := Self.ReferTo;
end;

function TestTIdSipOutboundRefer.CreateAction: TIdSipAction;
begin
  Result := Self.CreateRefer;
end;

//* TestTIdSipOutboundRefer Private methods ************************************

function TestTIdSipOutboundRefer.CreateRefer: TIdSipOutboundRefer;
begin
  Result := Self.Core.AddOutboundAction(TIdSipOutboundRefer) as TIdSipOutboundRefer;
  Self.ConfigureAction(Result);
  Result.Send;
end;

//* TestTIdSipOutboundRefer Published methods **********************************

procedure TestTIdSipOutboundRefer.TestMethod;
var
  Action: TIdSipAction;
begin
  Action := Self.CreateAction;

  CheckEquals(MethodRefer,
              Action.Method,
              Action.ClassName + '; Method');
end;

procedure TestTIdSipOutboundRefer.TestSend;
var
  Events:   TIdSipHeadersFilter;
  Refer:    TIdSipRequest;
  ReferTos: TIdSipHeadersFilter;
begin
  Self.MarkSentRequestCount;
  Self.CreateAction;

  CheckRequestSent(Self.ClassName + ': No request sent');

  Refer := Self.LastSentRequest;
  CheckEquals(MethodRefer,
              Refer.Method,
              Self.ClassName + ': Method');
  Check(Refer.HasHeader(ReferToHeaderFull),
        Self.ClassName + ': No Refer-To header');
  CheckEquals(Self.ReferTo.Value,
              Refer.FirstHeader(ReferToHeaderFull).Value,
              Self.ClassName + ': Wrong Refer-To value');
  Check(Refer.HasHeader(EventHeaderFull),
        Self.ClassName + ': No Event header');
  CheckEquals(TIdSipReferPackage.EventPackage,
              Refer.Event.Value,
              Self.ClassName + ': Wrong Event value');

  ReferTos := TIdSipHeadersFilter.Create(Refer.Headers, ReferToHeaderFull);
  try
    CheckEquals(1,
                ReferTos.Count,
                Self.ClassName + ': Only one Refer-To allowed: '
              + 'RFC 3515, section 2.4.1');
  finally
    ReferTos.Free;
  end;

  Events := TIdSipHeadersFilter.Create(Refer.Headers, EventHeaderFull);
  try
    CheckEquals(1,
                Events.Count,
                Self.ClassName + ': Only one Event allowed: '
              + 'RFC 3515, section 2.4.1');
  finally
    Events.Free;
  end;
end;

procedure TestTIdSipOutboundRefer.TestSendAlwaysUsesReferEvent;
var
  Refer: TIdSipOutboundRefer;
begin
  Refer := Self.Core.AddOutboundAction(TIdSipOutboundRefer) as TIdSipOutboundRefer;
  Self.ConfigureAction(Refer);
  Refer.EventPackage := 'foo';
  Refer.Send;

  CheckRequestSent(Self.ClassName + ': No request sent');

  Check(Self.LastSentRequest.HasHeader(EventHeaderFull),
        'No Event header');
  CheckEquals(TIdSipReferPackage.EventPackage,
              Self.LastSentRequest.Event.Value,
              'Wrong Event package used');
end;

procedure TestTIdSipOutboundRefer.TestSendDoesntSendTwoRequests;
var
  OldSentRequestCount: Integer;
begin
  OldSentRequestCount := Self.SentRequestCount;
  Self.CreateAction;
  CheckEquals(OldSentRequestCount + 1,
              Self.SentRequestCount,
              'Number of messages sent');
end;

//******************************************************************************
//* TSubscribeModuleActionTestCase                                             *
//******************************************************************************
//* TSubscribeModuleActionTestCase Public methods ******************************

procedure TSubscribeModuleActionTestCase.SetUp;
begin
  inherited SetUp;

  Self.Module := Self.Core.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  Self.Module.AddListener(Self);

  Self.Module.AddPackage(TIdSipTestPackage);
  Self.Package := Self.Module.Package(TIdSipTestPackage.EventPackage);

  Self.RemoteParty := TIdSipFromHeader.Create;
  Self.RemoteParty.Assign(Self.Destination);
end;

procedure TSubscribeModuleActionTestCase.TearDown;
begin
  Self.RemoteParty.Free;

  inherited TearDown;
end;

//* TSubscribeModuleActionTestCase Protected methods ***************************

procedure TSubscribeModuleActionTestCase.OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                                               Subscription: TIdSipOutboundSubscription);
begin
end;

procedure TSubscribeModuleActionTestCase.OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                                               Subscription: TIdSipInboundSubscription);
begin
end;

//******************************************************************************
//* TestTIdSipInboundSubscriptionBase                                          *
//******************************************************************************
//* TestTIdSipInboundSubscriptionBase Public methods ***************************

procedure TestTIdSipInboundSubscriptionBase.SetUp;
begin
  inherited SetUp;

  Self.ReceiveSubscribeRequest;
  Check(Assigned(Self.Action),
        'No subscribing request received');

  Self.ActionRequest := Self.Action.InitialRequest;
end;

//* TestTIdSipInboundSubscriptionBase Protected methods ************************

procedure TestTIdSipInboundSubscriptionBase.CheckNotify(Notify: TIdSipRequest;
                                                        const Body: String;
                                                        const MimeType: String);
begin
  CheckEquals(MethodNotify,
              Notify.Method,
              'Unexpected request sent');
  CheckEquals(Body,
              Notify.Body,
              'Notify body');
  Check(Notify.HasHeader(ContentLengthHeaderFull),
        'Notify has no Content-Length header');
  CheckEquals(Notify.ContentLength,
              Notify.ContentLength,
              'Notify Content-Length');
  Check(Notify.HasHeader(ContentTypeHeaderFull),
        'Notify has no Content-Type header');
  CheckEquals(Notify.ContentType,
              Notify.ContentType,
              'Notify Content-Type');
end;

procedure TestTIdSipInboundSubscriptionBase.CheckSendNotify(Sub: TIdSipInboundSubscription;
                                                            const SubscriptionState: String);
var
  Notify: TIdSipRequest;
begin
  CheckRequestSent('No request sent');

  CheckEquals(SubscriptionState,
              Sub.SubscriptionState,
              'Subscription state');

  Notify := Self.LastSentRequest;
  CheckEquals(MethodNotify,
              Notify.Method,
              'Unexpected request sent');
  CheckEquals(Sub.SubscriptionState,
              Notify.SubscriptionState.SubState,
              'Notify state <> subscription''s state');
end;

function TestTIdSipInboundSubscriptionBase.CreateRefresh(Sub: TIdSipInboundSubscription;
                                                         Response: TIdSipResponse;
                                                         ExpiryTime: Cardinal): TIdSipRequest;
var
  RemoteDialog: TIdSipDialog;
begin
  // Create an OUTbound dialog because the remote send SENDS the request.
  RemoteDialog := TIdSipDialog.CreateOutboundDialog(Sub.InitialRequest,
                                                   Response,
                                                   false);
  try
    Result := Self.Module.CreateSubscribe(RemoteDialog, Sub.EventPackage);
  finally
    RemoteDialog.Free;
  end;
end;

procedure TestTIdSipInboundSubscriptionBase.OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                                                  Subscription: TIdSipInboundSubscription);
begin
  Self.Action := Subscription;
end;

procedure TestTIdSipInboundSubscriptionBase.ReceiveRefreshingSubscribe(Sub: TIdSipInboundSubscription;
                                                                       Response: TIdSipResponse;
                                                                       ExpiryTime: Cardinal);
var
  Refresh: TIdSipRequest;
begin
  Refresh := Self.CreateRefresh(Sub, Response, ExpiryTime);
  try
    Refresh.Expires.NumericValue := ExpiryTime;

    Self.ReceiveRequest(Refresh);
  finally
    Refresh.Free;
  end;
end;

procedure TestTIdSipInboundSubscriptionBase.ReceiveSubscribeRequest;
begin
end;

procedure TestTIdSipInboundSubscriptionBase.ReceiveSubscribeRequestWithGruu;
begin
end;

//* TestTIdSipInboundSubscriptionBase Published methods ************************

procedure TestTIdSipInboundSubscriptionBase.TestAccept;
begin
  Self.MarkSentRequestCount;
  Self.MarkSentResponseCount;

  Self.Action.Accept;

  Self.CheckSendNotify(Self.Action, SubscriptionSubstateActive);
end;

procedure TestTIdSipInboundSubscriptionBase.TestDontMatchInDialogInvite;
var
  Invite:       TIdSipRequest;
  RemoteDialog: TIdSipDialog;
begin
  RemoteDialog := TIdSipDialog.CreateInboundDialog(Self.Action.InitialRequest,
                                                   Self.LastSentResponse,
                                                   false);
  try
    Invite := Self.Core.InviteModule.CreateReInvite(RemoteDialog, '', '');
    try
      Check(not Self.Action.Match(Invite),
            'Matched an in-dialog INVITE');
    finally
      Invite.Free;
    end;
  finally
    RemoteDialog.Free;
  end;
end;

procedure TestTIdSipInboundSubscriptionBase.TestIsInbound;
begin
  Check(Self.Action.IsInbound,
        Self.Action.ClassName + ' not marked as inbound');
end;

procedure TestTIdSipInboundSubscriptionBase.TestIsInvite;
begin
  Check(not Self.Action.IsInvite,
        Self.Action.ClassName + ' marked as an Invite');
end;

procedure TestTIdSipInboundSubscriptionBase.TestIsOptions;
begin
  Check(not Self.Action.IsOptions,
        Self.Action.ClassName + ' marked as an Options');
end;

procedure TestTIdSipInboundSubscriptionBase.TestIsOwned;
begin
  Check(not Self.Action.IsOwned,
        Self.Action.ClassName + ' marked as being owned');
end;

procedure TestTIdSipInboundSubscriptionBase.TestIsOutbound;
begin
  Check(not Self.Action.IsOutbound,
        Self.Action.ClassName + ' marked as outbound');
end;

procedure TestTIdSipInboundSubscriptionBase.TestIsRegistration;
begin
  Check(not Self.Action.IsRegistration,
        Self.Action.ClassName + ' marked as a Registration');
end;

procedure TestTIdSipInboundSubscriptionBase.TestIsSession;
begin
  Check(not Self.Action.IsSession,
        Self.Action.ClassName + ' marked as a Session');
end;

procedure TestTIdSipInboundSubscriptionBase.TestNotify;
begin
  Fail('Implement ' + Self.ClassName + '.TestNotify');
end;

procedure TestTIdSipInboundSubscriptionBase.TestNotifyWithGruu;
begin
  Fail('Implement ' + Self.ClassName + '.TestNotifyWithGruu');
end;

procedure TestTIdSipInboundSubscriptionBase.TestReceiveRefreshingSubscribe;
var
  ArbExpiresValue: Integer;
  Method:          String;
  SubCount:        Integer;
begin
  Method := Self.Action.Method;
  ArbExpiresValue := Self.Package.MinimumExpiryTime + 1;

  Self.Action.Accept;

  SubCount := Self.Core.CountOf(MethodSubscribe);
  Self.MarkSentRequestCount;
  Self.MarkSentResponseCount;
  Self.ReceiveRefreshingSubscribe(Self.Action,
                                  Self.LastSentResponse,
                                  ArbExpiresValue);

  CheckResponseSent('No response sent');
  CheckNotEquals(SIPAccepted,
                 Self.LastSentResponse.StatusCode,
                 Method + ' started a new subscription: it didn''t match the existing one');
  CheckEquals(SIPOK,
              Self.LastSentResponse.StatusCode,
              'Unexpected response sent to refreshing subscription');

  CheckRequestSent('No request sent');
  CheckEquals(MethodNotify,
              Self.LastSentRequest.Method,
              'Unexpected request sent after refreshing subscription');

  // There should be two expiry events scheduled. We trigger the first one,
  // demonstrating that the second still exists.
  Self.DebugTimer.TriggerEarliestEvent;
  Check(SubCount >= Self.Core.CountOf(MethodSubscribe),
        'The Subscription terminated prematurely: check the logic around '
      + 'OutstandingExpires');
end;

procedure TestTIdSipInboundSubscriptionBase.TestReceiveRequestSendsAccepted;
begin
  Self.MarkSentResponseCount;
  Self.ReceiveSubscribeRequestWithGruu;
  CheckResponseSent('No response sent');

  CheckEquals(SIPAccepted,
              Self.LastSentResponse.StatusCode,
              'Unexpected response sent');
end;

procedure TestTIdSipInboundSubscriptionBase.TestReceiveRequestSendsAcceptedWithGruu;
begin
  Self.UseGruu;

  Self.MarkSentResponseCount;
  Self.ReceiveSubscribeRequestWithGruu;
  CheckResponseSent('No response sent');

  Check(Assigned(Self.Action),
        'No subscribing request received');

  Check(Self.LastSentResponse.HasHeader(SupportedHeaderFull),
        'Response missing Supported header');
  Check(Self.LastSentResponse.SupportsExtension(ExtensionGruu),
        'Supported header fails to indicate support of "gruu" extension');
  CheckEquals(Self.Action.LocalGruu.AsString,
              Self.LastSentResponse.FirstContact.AsString,
              'Response didn''t use GRUU');
  Check(Self.LastSentResponse.FirstContact.Address.HasGrid,
        'Response''s GRUU doesn''t have a "grid" parameter');
end;

procedure TestTIdSipInboundSubscriptionBase.TestReceiveRequestSendsNotify;
begin
  Self.MarkSentRequestCount;
  Self.ReceiveSubscribeRequest;

  Self.CheckSendNotify(Self.Action, SubscriptionSubstatePending);
end;

procedure TestTIdSipInboundSubscriptionBase.TestReceiveRequestWithGruu;
begin
  Self.UseGruu;

  Self.MarkSentResponseCount;
  Self.ReceiveSubscribeRequestWithGruu;
  Check(Assigned(Self.Action),
        'No subscribing request with GRUU received');

  CheckResponseSent(Self.Action.ClassName + ': No response to the '
                  + Self.Action.Method + ' sent');

  Check(Self.LastSentResponse.FirstContact.Address.HasGrid,
        Self.Action.ClassName + ': 202 Accepted''s Contact address has no "grid" parameter');
  CheckEquals(Self.LastSentResponse.FirstContact.AsString,
              Self.Action.LocalGruu.AsString,
              Self.Action.ClassName + '''s LocalGruu doesn''t match Contact in 202 Accepted');
end;

procedure TestTIdSipInboundSubscriptionBase.TestReceiveSubscribeWithZeroExpires;
var
  Method:   String;
  SubCount: Integer;
begin
  //                     <existing subscription>
  // <---                 SUBSCRIBE (Expires: 0)                ---
  //  ---                        200 OK                         --->
  //  --- NOTIFY (Subscription-State: terminated;reason=timeout --->
  // <---                        200 OK                         ---

  Method   := Self.Action.Method;
  SubCount := Self.Core.CountOf(Method);
  Self.MarkSentRequestCount;
  Self.MarkSentResponseCount;
  Self.ReceiveRefreshingSubscribe(Self.Action,
                                  Self.LastSentResponse,
                                  0);
  CheckResponseSent('No response sent');
  CheckEquals(SIPOK,
              Self.LastSentResponse.StatusCode,
              'Unexpected response sent');
  CheckRequestSent('No request sent');
  CheckEquals(MethodNotify,
              Self.LastSentRequest.Method,
              'No NOTIFY sent');
  CheckEquals(SubscriptionSubstateTerminated,
              Self.LastSentRequest.SubscriptionState.SubState,
              'Incorrect subscription state');
  CheckEquals(EventReasonTimeout,
              Self.LastSentRequest.SubscriptionState.Reason,
              'Incorrect termination reason');

  // Receive the 200 OK for the terminating NOTIFY
  Self.ReceiveOk(Self.LastSentRequest);

  Check(Self.Core.CountOf(Method) < SubCount,
        'Subscription not terminated');
end;

procedure TestTIdSipInboundSubscriptionBase.TestTerminateSignalled;
var
  L: TIdSipTestActionListener;
begin
  L := TIdSipTestActionListener.Create;
  try
    Self.Action.AddActionListener(L);
    try
      Self.Action.Terminate;

      Check(L.Terminated, Self.ClassName + ': Listener not notified of termination');
    finally
      Self.Action.RemoveActionListener(L);
    end;
  finally
    L.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipInboundSubscription                                              *
//******************************************************************************
//* TestTIdSipInboundSubscription Public methods *******************************

procedure TestTIdSipInboundSubscription.SetUp;
begin
  Self.EventID := 'random-id';

  inherited SetUp;
end;

//* TestTIdSipInboundSubscription Private methods ******************************

procedure TestTIdSipInboundSubscription.CheckExpiresScheduled(ExpectedExpires: Cardinal;
                                                              const Msg: String);
var
  Wait: TIdWait;
begin
  Check(Self.DebugTimer.EventCount > 0,
        Msg + ': No events scheduled');

  Wait := Self.DebugTimer.LastEventScheduled(TIdSipActionsWait);
  CheckNotNull(Wait, Msg + ': No event scheduled');
  CheckEquals(ExpectedExpires*1000,
              Wait.DebugWaitTime,
              Msg + ': Expires wait time (in milliseconds)');

  Self.MarkSentRequestCount;
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionsWait);
  CheckRequestSent(Msg + ': No request sent for expired subscription');
  CheckEquals(MethodNotify,
              Self.LastSentRequest.Method,
              Msg + ': Unexpected message sent');
  CheckEquals(SubscriptionSubstateTerminated,
              Self.LastSentRequest.SubscriptionState.SubState,
              Msg + ': Subscription-State value');
end;
{
procedure TestTIdSipInboundSubscription.ReceiveAuthChallengeWithRetryAfter(Sub: TIdSipRequest;
                                                                           RetryAfter: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Sub, SIPUnauthorized);
  try
    Response.FirstWWWAuthenticate.Value := 'Digest realm="atlanta.com",'
                                         + 'domain="sip:boxesbybob.com", qop="auth",'
                                         + 'nonce="f84f1cec41e6cbe5aea9c8e88d359",'
                                         + 'opaque="", stale=FALSE, algorithm=MD5';
    Response.FirstRetryAfter.NumericValue := RetryAfter;
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;
}
procedure TestTIdSipInboundSubscription.ReceiveSubscribeWithoutExpires(const EventPackage: String);
var
  Subscribe: TIdSipRequest;
begin
  Subscribe := Self.Module.CreateSubscribe(RemoteParty, Self.Destination, EventPackage, TIdSipRequest.DefaultMaxForwards);
  try
    // See the comment in TSubscribeTestCase.ReceiveSubscribe.
    Subscribe.FirstContact.Address := Self.Destination.Address;
    Subscribe.RemoveAllHeadersNamed(ExpiresHeader);

    Self.ReceiveRequest(Subscribe);
  finally
    Subscribe.Free;
  end;
end;

procedure TestTIdSipInboundSubscription.ReceiveSubscribe(const EventPackage: String;
                                                         ExpiryTime: Cardinal = 0);
var
  Sub: TIdSipRequest;
begin
  Sub := Self.Module.CreateSubscribe(Self.RemoteParty, Self.Core.From, EventPackage, TIdSipRequest.DefaultMaxForwards);
  try
    Sub.From.Address         := Self.Destination.Address;
    Sub.FirstContact.Address := Self.Destination.Address;

    Sub.Event.ID := Self.EventID;
    if (ExpiryTime > 0) then
      Sub.Expires.NumericValue := ExpiryTime;

    Self.ReceiveRequest(Sub);
  finally
    Sub.Free;
  end;
end;

procedure TestTIdSipInboundSubscription.ReceiveSubscribeWithExpiresInContact(Duration: Cardinal);
var
  Subscribe: TIdSipRequest;
begin
  Subscribe := Self.Module.CreateSubscribe(RemoteParty, Self.Destination, TIdSipTestPackage.EventPackage, TIdSipRequest.DefaultMaxForwards);
  try
    // See the comment in TSubscribeTestCase.ReceiveSubscribe.
    Subscribe.FirstContact.Address := Self.Destination.Address;
    Subscribe.RemoveAllHeadersNamed(ExpiresHeader);
    Subscribe.FirstContact.Expires := Duration;

    Self.ReceiveRequest(Subscribe);
  finally
    Subscribe.Free;
  end;
end;
{
procedure TestTIdSipInboundSubscription.RemoveExpiresWait(Timer: TIdDebugTimerQueue);
begin
  Timer.RemoveEvent(Timer.LastEventScheduled);
end;
}
//* TestTIdSipInboundSubscription Protected methods ****************************

procedure TestTIdSipInboundSubscription.ReceiveSubscribeRequest;
const
  MinExpTime = 42;
begin
  // This is a bit hacky. The superclass' SetUp runs first, then this,
  // and then our SetUp runs. Usually we'd set this
  Self.Package.MinimumExpiryTime := MinExpTime;

  Self.ReceiveSubscribe(TIdSipTestPackage.EventPackage);
end;

procedure TestTIdSipInboundSubscription.ReceiveSubscribeRequestWithGruu;
const
  MinExpTime = 42;
var
  Sub: TIdSipRequest;
begin
  Self.Package.MinimumExpiryTime := MinExpTime;

  Sub := Self.Module.CreateSubscribe(RemoteParty, Self.Destination, TIdSipTestPackage.EventPackage, TIdSipRequest.DefaultMaxForwards);
  try
    // See the comment in TSubscribeTestCase.ReceiveSubscribe.
    Sub.FirstContact.Address := Self.Destination.Address;
    Sub.Event.ID := Self.EventID;

    Sub.Supported.Values.Add(ExtensionGruu);

    Self.ReceiveRequest(Sub);
  finally
    Sub.Free;
  end;
end;

//* TestTIdSipInboundSubscription Published methods ****************************

procedure TestTIdSipInboundSubscription.TestExpire;
var
  Notify:   TIdSipRequest;
  SubCount: Integer;
begin
  Self.Action.Accept;

  SubCount := Self.Core.CountOf(MethodSubscribe);
  Self.MarkSentRequestCount;
  Self.Action.Expire;

  CheckRequestSent('No request sent for expiry');
  Notify := Self.LastSentRequest;
  CheckEquals(MethodNotify,
              Notify.Method,
              'Unexpected request sent');
  Check(Notify.HasHeader(SubscriptionStateHeader),
        'No Subscription-State header');
  CheckEquals(SubscriptionSubStateTerminated,
              Notify.SubscriptionState.SubState,
              'Unexpected substate');
  CheckEquals(EventReasonTimeout,
              Notify.SubscriptionState.Reason,
              'Unexpected substate reason');
  CheckEquals(SubscriptionSubStateTerminated,
              Self.Action.SubscriptionState,
              'Subscription state');
  Check(SubCount > Self.Core.CountOf(MethodSubscribe),
        'Subscription not terminated');
end;

procedure TestTIdSipInboundSubscription.TestExpiryTimeInSeconds;
begin
  // WARNING! This test uses Now() and invokes a time-dependent function!
  // If you put breakpoints within the invoked code the test will likely fail!
  CheckEquals((Self.Action.ExpiryTime - Now),
              Self.Action.ExpiryTimeInSeconds * OneTDateTimeSecond,
              OneTDateTimeSecond,
              'ExpiryTimeInSeconds');
end;

procedure TestTIdSipInboundSubscription.TestMatchInDialogSubscribe;
var
  Refresh: TIdSipRequest;
begin
  Refresh := Self.CreateRefresh(Self.Action,
                                Self.LastSentResponse,
                                1000);
  try
    Check(Self.Action.Match(Refresh),
          'Didn''t match an in-dialog ' + Self.Action.Method);
  finally
    Refresh.Free;
  end;
end;

procedure TestTIdSipInboundSubscription.TestMatchResponse;
var
  OK: TIdSipResponse;
begin
  OK := TIdSipResponse.InResponseTo(Self.Action.InitialRequest, SIPOK);
  try
    Check(Self.Action.Match(OK),
          'Didn''t match the ' + Self.Action.Method + '''s response');
  finally
    OK.Free;
  end;
end;

procedure TestTIdSipInboundSubscription.TestNotify;
const
  Body     = 'random';
  MimeType = 'text/plain';
begin
  Self.Action.Accept;

  Self.MarkSentRequestCount;
  Self.Action.Notify(Body, MimeType);

  CheckRequestSent('No request sent');
  Self.CheckNotify(Self.LastSentRequest, Body, MimeType);
end;

procedure TestTIdSipInboundSubscription.TestNotifyWithGruu;
const
  Body     = 'random';
  MimeType = 'text/plain';
var
  Accepted: TIdSipResponse;
  Sub:      TIdSipRequest;
begin
  // Set us up to support GRUU
  Self.UseGruu;

  Sub := Self.Module.CreateSubscribe(Self.Core.From, Self.Destination, TIdSipTestPackage.EventPackage, TIdSipRequest.DefaultMaxForwards);
  try
    // See the comment in TSubscribeTestCase.ReceiveSubscribe.
    Sub.FirstContact.Address := Self.Destination.Address;
    Sub.Event.ID := Self.EventID;

    Self.ReceiveRequest(Sub);
    Check(Assigned(Self.Action),
          'No subscribing request received');

    Accepted := Self.LastSentResponse;
    Check(Accepted.FirstContact.Address.HasGrid,
          'Dialog-establishing response should have a "grid" parameter');

    Self.Action.Accept;

    Self.MarkSentRequestCount;
    Self.Action.Notify(Body, MimeType);

    CheckRequestSent('No request sent');
    Self.CheckNotify(Self.LastSentRequest,
                     Body,
                     MimeType);
    Check(Self.LastSentRequest.HasHeader(SupportedHeaderFull),
          'NOTIFY lacks a Supported header');
    Check(Self.LastSentRequest.SupportsExtension(ExtensionGruu),
          'Supported header lacks a "gruu" entry');
    CheckEquals(Self.Action.LocalGruu.AsString,
                Self.LastSentRequest.FirstContact.AsString,
                'NOTIFY didn''t use ' + Self.Action.Method + '''s GRUU');
    CheckEquals(Self.LastSentRequest.FirstContact.Address.Host,
                Accepted.FirstContact.Address.Host,
                'NOTIFY''s remote target should match that of the 202 Accepted');
  finally
    Sub.Free;
  end;
end;

procedure TestTIdSipInboundSubscription.TestReceiveExpiresInContactHeader;
var
  ContactExpiresTime: Cardinal;
  Response:           TIdSipResponse;
begin
  ContactExpiresTime := Self.Package.MinimumExpiryTime;

  Self.MarkSentResponseCount;
  Self.ReceiveSubscribeWithExpiresInContact(ContactExpiresTime);

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckNotEquals(ContactExpiresTime,
                 Response.Expires.NumericValue,
                 'We didn''t ignore the Contact expires parameter');
  CheckEquals(Self.Package.InboundSubscriptionDuration,
              Response.Expires.NumericValue,
              'We didn''t use the package''s default expires time');
end;

procedure TestTIdSipInboundSubscription.TestReceiveExpiresTooShort;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;
  Self.ReceiveSubscribe(TIdSipTestPackage.EventPackage, Self.Package.MinimumExpiryTime - 1);

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPIntervalTooBrief,
              Response.StatusCode,
              'Unexpected response sent');
  Check(Response.HasHeader(MinExpiresHeader),
        'No Min-Expires header');
  CheckEquals(Self.Package.MinimumExpiryTime,
              Response.MinExpires.NumericValue,
              'Min-Expires value');
end;

procedure TestTIdSipInboundSubscription.TestReceiveNoExpires;
var
  Response: TIdSipResponse;
begin
  Self.ReceiveSubscribeWithoutExpires(TIdSipTestPackage.EventPackage);

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPAccepted,
              Response.StatusCode,
              'Unexpected response sent');
  Check(Response.HasHeader(ExpiresHeader),
        'No Expires header');
  CheckEquals(TIdSipTestPackage.DefaultSubscriptionDuration,
              Response.Expires.NumericValue,
              'Wrong Expires value');
end;

procedure TestTIdSipInboundSubscription.TestReceiveRefreshingSubscribeIntervalTooBrief;
var
  Response: TIdSipResponse;
  SubCount: Integer;
begin
  Self.Action.Accept;

  SubCount := Self.Core.CountOf(MethodSubscribe);
  Self.MarkSentRequestCount;

  Self.ReceiveRefreshingSubscribe(Self.Action,
                                  Self.LastSentResponse,
                                  Self.Package.MinimumExpiryTime - 1);

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPIntervalTooBrief,
              Response.StatusCode,
              'Unexpected response sent');
  Check(Response.HasHeader(MinExpiresHeader),
        'No Min-Expires header');
  CheckEquals(Self.Package.MinimumExpiryTime,
              Response.MinExpires.NumericValue,
              'Min-Expires value');
  Check(SubCount >= Self.Core.CountOf(MethodSubscribe),
        'Rejecting a refreshing subscription shouldn''t kill the subscription');
end;

procedure TestTIdSipInboundSubscription.TestReceiveSubscribe;
begin
  CheckEquals(Self.ActionRequest.Event.EventPackage,
              Self.Action.EventPackage,
              'EventPackage');

  CheckEquals(Self.ActionRequest.Event.ID,
              Self.Action.EventID,
              'ID');

  CheckEquals(SubscriptionSubstatePending,
              Self.Action.SubscriptionState,
              'Action state');

  CheckExpiresScheduled(Self.ActionRequest.Expires.NumericValue,
                        'Subscription won''t expire');
end;

procedure TestTIdSipInboundSubscription.TestReceiveSubscribeReturnsAccepted;
begin
  Self.MarkSentResponseCount;

  // This SUBSCRIBE sets up a new subscription.
  Self.ReceiveSubscribe(TIdSipTestPackage.EventPackage);

  CheckResponseSent('No response sent');
  CheckEquals(SIPAccepted,
              Self.LastSentResponse.StatusCode,
              'Unexpected response sent');
  Check(Self.LastSentResponse.ContactCount > 0,
        'Response has no Contact header: RFC 3261, section 12.1.1');
  Check(Self.LastSentResponse.ToHeader.HasTag,
        'Response''s To header has no tag');
end;

procedure TestTIdSipInboundSubscription.TestReceiveSubscribeSendsNotify;
begin
  Self.MarkSentRequestCount;

  Self.ReceiveSubscribe(TIdSipTestPackage.EventPackage);

  CheckRequestSent('No request sent');
  CheckEquals(MethodNotify,
              Self.LastSentRequest.Method,
              'Unexpected request sent');
end;

procedure TestTIdSipInboundSubscription.TestSendNotify;
const
  Body     = 'random data';
  MimeType = 'text/plain';
var
  Notify: TIdSipRequest;
begin
  Self.Action.Accept;
  Self.MarkSentRequestCount;

  Self.Action.Notify(Body, MimeType);

  Self.CheckRequestSent('No request sent');

  Notify := Self.LastSentRequest;
  CheckEquals(MethodNotify,
              Notify.Method,
              'Unexpected request sent');
  CheckEquals(Body,
              Notify.Body,
              'Notify body');
  Check(Notify.HasHeader(ContentTypeHeaderFull),
        'Notify has no Content-Type header');
  CheckEquals(MimeType,
              Notify.ContentType,
              'Notify has incorrect Content-Type header');
end;

procedure TestTIdSipInboundSubscription.TestSendNotifyAffectsState;
const
  UndefinedState = 'undefined';
begin
  Self.Action.Accept;

  Self.Action.Notify('', '', UndefinedState);

  CheckEquals(UndefinedState,
              Self.Action.SubscriptionState,
              'NOTIFY didn''t alter action state');
end;

procedure TestTIdSipInboundSubscription.TestSendNotifyNetworkFailure;
var
  SubCount: Integer;
begin
  Self.Action.Accept;

  SubCount := Self.Core.CountOf(MethodSubscribe);

  Self.Action.Notify('', '');
  Self.Dispatcher.Transport.FireOnException(Self.LastSentRequest,
                                            EIdConnectException,
                                            '10061',
                                            'Connection refused');

  Check(SubCount > Self.Core.CountOf(MethodSubscribe),
        'Subscription not terminated');
end;

procedure TestTIdSipInboundSubscription.TestSendNotifyReceiveFail;
var
  SubCount: Integer;
begin
  Self.Action.Accept;

  SubCount := Self.Core.CountOf(MethodSubscribe);

  Self.Action.Notify('', '');
  Self.ReceiveResponse(Self.LastSentRequest, SIPDecline);

  Check(SubCount > Self.Core.CountOf(MethodSubscribe),
        'Subscription not terminated');
end;
{
procedure TestTIdSipInboundSubscription.TestSendNotifyReceiveFailWithRetryAfter;
const
  RetryAfterValue = 60;
var
  Notify:    TIdSipRequest;
  WaitCount: Integer;
begin
  Self.Action.Accept;
  Self.Action.Notify('', '');

  Self.RemoveExpiresWait(Self.DebugTimer);

  WaitCount := Self.DebugTimer.EventCount;
  Self.ReceiveAuthChallengeWithRetryAfter(Self.LastSentRequest, RetryAfterValue);

  Check(WaitCount < Self.DebugTimer.EventCount,
        'No RetryAfter wait scheduled');
  CheckEquals(RetryAfterValue*1000,
              Self.DebugTimer.LastEventScheduled.DebugWaitTime,
              'Wrong wait time');

  Self.MarkSentRequestCount;
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionsWait);
  CheckRequestSent('No request sent');

  Notify := Self.LastSentRequest;
  CheckEquals(MethodNotify,
              Notify.Method,
              'Unexpected request sent');
end;
}

//******************************************************************************
//* TestTIdSipOutboundSubscriptionBase                                         *
//******************************************************************************
//* TestTIdSipOutboundSubscriptionBase Public methods **************************

procedure TestTIdSipOutboundSubscriptionBase.SetUp;
begin
  inherited SetUp;

  Self.Password := 'password';
  Self.ChallengeResponse := Self.CreateChallengeResponse;
  Self.RemoteRealmInfo   := TIdRealmInfo.Create;
  Self.RemoteRealmInfo.Username := Self.Core.Username;
  Self.RemoteRealmInfo.Realm    := Self.Core.Realm;
  Self.Authenticator.AddUser(Self.Core.Username,
                             Self.Core.Realm,
                             '');

  Self.ReceivedNotify := TIdSipRequest.Create;

  // It looks odd that we set this value before the others. If we don't then the
  // Subscribe will refresh after 0 seconds, and the DebugTimer will fire Waits
  // with a zero wait time immediately, which isn't what we want.
  Self.ArbExpiresValue := 22;

  Self.Subscription := Self.EstablishSubscription;

  Self.ArbRetryAfterValue      := 42;
  Self.RenewSubscriptionFired  := false;
  Self.SubscriptionEstablished := false;
  Self.SubscriptionExpired     := false;
  Self.SubscriptionFailed      := false;
  Self.SubscriptionNotified    := false;
  Self.UnknownReason           := 'unknown-reason';
end;

procedure TestTIdSipOutboundSubscriptionBase.TearDown;
begin
  Self.ReceivedNotify.Free;
  Self.RemoteRealmInfo.Free;
  Self.ChallengeResponse.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundSubscriptionBase Protected methods ***********************

function TestTIdSipOutboundSubscriptionBase.CreateSubscription: TIdSipOutboundSubscription;
begin
  Result := Self.CreateSubscriptionWithoutSend;
  Result.Send;
end;

function TestTIdSipOutboundSubscriptionBase.CreateSubscriptionWithoutSend: TIdSipOutboundSubscription;
begin
  Result := Self.Module.Subscribe(Self.Destination,
                                  TIdSipTestPackage.EventPackage) as TIdSipOutboundSubscription;
  Result.AddActionListener(Self);
  Result.AddListener(Self);
end;

procedure TestTIdSipOutboundSubscriptionBase.CheckExpires(ExpectedRefreshTime: Cardinal);
var
  Wait: TIdWait;
begin
  Check(Self.DebugTimer.EventCount > 0,
        'No events scheduled at all');

  Wait := Self.DebugTimer.LastEventScheduled(TIdSipOutboundSubscriptionRefreshWait);
  CheckNotNull(Wait, 'No event scheduled');
  Check(Wait.DebugWaitTime < ExpectedRefreshTime*1000,
        'Refresh scheduled too late');

  Self.MarkSentRequestCount;
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionWait);
  Self.CheckRequestSent('No request sent, thus no refresh was scheduled');

  Check(not Self.Subscription.Terminating,
        'Subscription can''t terminating, since we''ve just refreshed');
  Check(not Self.Subscription.IsTerminated,
        'Subscription can''t be terminated, since we''ve not received a '
      + 'terminating NOTIFY');
end;

procedure TestTIdSipOutboundSubscriptionBase.CheckNoRetryScheduled(const MsgPrefix: String);
begin
  // Self.RenewSubscriptionFired might be true if the subscription automatically
  // renewed itself.
  Self.RenewSubscriptionFired := false;

  Self.DebugTimer.TriggerAllEventsOfType(TIdSipSubscriptionRetryWait);

  Check(not Self.RenewSubscriptionFired,
        Self.ClassName + ': OnRenewSubscription fired, so a '
       + TIdSipSubscriptionRetryWait.ClassName + ' was scheduled');
end;

procedure TestTIdSipOutboundSubscriptionBase.CheckReceiveFailureResponse(StatusCode: Cardinal);
var
  Sub: TIdSipOutboundSubscription;
begin
  Sub := Self.CreateSubscription;
  Self.ReceiveBadEvent(Sub.InitialRequest);

  Check(Sub.IsTerminated,
        Self.ClassName + ': Subscription didn''t terminate after failure '
     + '(Status-Code = ' + IntToStr(StatusCode) + ')');
  Check(not Self.SubscriptionExpired,
        Self.ClassName + ': The subscription mustn''t EXPIRE, it must FAIL '
     + '(Status-Code = ' + IntToStr(StatusCode) + ')');
  Check(Self.SubscriptionFailed,
        Self.ClassName + ': The subscription didn''t tell its listeners of its failure '
     + '(Status-Code = ' + IntToStr(StatusCode) + ')');
end;

procedure TestTIdSipOutboundSubscriptionBase.CheckRetryScheduled(const MsgPrefix: String);
begin
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipSubscriptionRetryWait);

  Check(Self.RenewSubscriptionFired,
        Self.ClassName + ': OnRenewSubscription didn''t fire, so no '
       + TIdSipSubscriptionRetryWait.ClassName + ' scheduled');
end;

procedure TestTIdSipOutboundSubscriptionBase.CheckTerminatedSubscription(Subscription: TIdSipSubscription;
                                                                         const MsgPrefix: String);
begin
  CheckRequestSent(Self.ClassName + ': ' + MsgPrefix + ': No request sent');
  CheckEquals(MethodSubscribe,
              Self.LastSentRequest.Method,
              Self.ClassName + ': ' + MsgPrefix + ': Unexpected request sent');
  CheckEquals(0,
              Self.LastSentRequest.Expires.NumericValue,
              Self.ClassName + ': ' + MsgPrefix + ': Wrong Expires value');
  Check(Subscription.Terminating,
        Self.ClassName + ': ' + MsgPrefix + ': Not marked as terminating');
end;

procedure TestTIdSipOutboundSubscriptionBase.CheckTerminatedSubscriptionWithResubscribe(const Reason: String);
begin
  Check(Self.SubscriptionNotified,
        Self.ClassName + ': ' + Reason
      + ': Subscription didn''t notify listeners of received NOTIFY');

  Check(Self.SubscriptionExpired,
        Self.ClassName + ': ' + Reason + ': Subscription didn''t expire');

  Check(Self.RenewSubscriptionFired,
        Self.ClassName + ': ' + Reason
      + ': Subscription didn''t notify of the new subscription');
end;

procedure TestTIdSipOutboundSubscriptionBase.CheckTerminatedSubscriptionWithNoResubscribe(const Reason: String);
begin
  Check(Self.SubscriptionNotified,
        Self.ClassName + ': ' + Reason
      + ': Subscription didn''t notify listeners of received NOTIFY');

  Check(Self.SubscriptionExpired,
        Self.ClassName + ': ' + Reason + ': Subscription didn''t expire');

  Check(not Self.RenewSubscriptionFired,
        Self.ClassName + ': ' + Reason + ': Subscription resubscribed');
end;

function TestTIdSipOutboundSubscriptionBase.CreateAction: TIdSipAction;
begin
  Result := Self.CreateSubscription;
end;

function TestTIdSipOutboundSubscriptionBase.CreateChallengeResponse: TIdSipResponse;
begin
  (Self.Core.Authenticator as TIdSipMockAuthenticator).AuthenticateAllRequests := false;
  try
    Self.MarkSentResponseCount;
    Self.ReceiveInvite;
    CheckResponseSent('No challenge response sent: '
                    + 'TestTIdSipOutboundSubscription.CreateChallengeResponse');

    Result := TIdSipResponse.Create;
    Result.Assign(Self.LastSentResponse);
  finally
    (Self.Core.Authenticator as TIdSipMockAuthenticator).AuthenticateAllRequests := true;
  end;
end;

function TestTIdSipOutboundSubscriptionBase.CreateNotify(Subscribe: TIdSipRequest;
                                                         Response: TIdSipResponse;
                                                         const State: String): TIdSipRequest;
var
  RemoteDialog: TIdSipDialog;
begin
  // Create an INbound dialog because the dialog, from the perspective of the
  // remote end, IS inbound.
  RemoteDialog := TIdSipDialog.CreateInboundDialog(Subscribe,
                                                   Response,
                                                   false);
  try
    Result := Self.Module.CreateNotify(RemoteDialog,
                                       Subscribe,
                                       State);
  finally
    RemoteDialog.Free;
  end;
end;

function TestTIdSipOutboundSubscriptionBase.EstablishSubscription: TIdSipOutboundSubscription;
begin
  Result := Self.CreateSubscription;
  Self.EstablishSubscription(Result);
end;

procedure TestTIdSipOutboundSubscriptionBase.EstablishSubscription(Sub: TIdSipOutboundSubscription);
begin
  Self.ReceiveOkFor(Sub, Self.ArbExpiresValue);
  Self.ReceiveNotify(Sub.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateActive);
end;

procedure TestTIdSipOutboundSubscriptionBase.OnEstablishedSubscription(Subscription: TIdSipOutboundSubscription;
                                                                       Notify: TIdSipRequest);
begin
  Self.SubscriptionEstablished := true;
end;

procedure TestTIdSipOutboundSubscriptionBase.OnExpiredSubscription(Subscription: TIdSipOutboundSubscription;
                                                                   Notify: TIdSipRequest);
begin
  Self.SubscriptionExpired := true;
end;

procedure TestTIdSipOutboundSubscriptionBase.OnFailure(Subscription: TIdSipOutboundSubscription;
                                                       Response: TIdSipResponse);
begin
  Self.SubscriptionFailed := true;
end;

procedure TestTIdSipOutboundSubscriptionBase.OnNotify(Subscription: TIdSipOutboundSubscription;
                                                      Notify: TIdSipRequest);
begin
  Self.SubscriptionNotified := true;
  Self.ReceivedNotify.Assign(Notify);
end;

procedure TestTIdSipOutboundSubscriptionBase.ReceiveBadEvent(Subscribe: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Subscribe, SIPBadEvent);
  try
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSubscriptionBase.ReceiveNotify(Subscribe: TIdSipRequest;
                                                           Response: TIdSipResponse;
                                                           const State: String;
                                                           const Reason: String = '';
                                                           RetryAfter: Cardinal = 0;
                                                           Expires: Cardinal = 0);
var
  AuthCreds: TIdSipAuthorizationHeader;
  Notify:    TIdSipRequest;
begin
  Notify := Self.CreateNotify(Subscribe,
                              Response,
                              State);
  try
    if (Reason <> '') then
      Notify.SubscriptionState.Reason := Reason;

    if (Expires > 0) then
      Notify.SubscriptionState.Expires := Expires;

    if (RetryAfter > 0) then
      Notify.SubscriptionState.RetryAfter := RetryAfter;

    AuthCreds := Self.RemoteRealmInfo.CreateAuthorization(Self.ChallengeResponse,
                                                          MethodNotify,
                                                          Notify.Body,
                                                          Self.Password);
    try
      Notify.AddHeader(AuthCreds);
    finally
      AuthCreds.Free;
    end;

    Self.ReceiveRequest(Notify);
  finally
    Notify.Free;
  end;
end;

procedure TestTIdSipOutboundSubscriptionBase.ReceiveNotifyTerminated(Sub: TIdSipOutboundSubscription);
begin
  Self.ReceiveNotify(Sub.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonRejected);
end;

procedure TestTIdSipOutboundSubscriptionBase.ReceiveOkFor(Sub: TIdSipOutboundSubscription;
                                                      Expires: Cardinal);
begin
  Self.ReceiveOkFor(Sub.InitialRequest, Expires);
end;

procedure TestTIdSipOutboundSubscriptionBase.ReceiveOkFor(Subscribe: TIdSipRequest;
                                                          Expires: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Subscribe, SIPOK);
  try
    Response.FirstContact.Assign(Subscribe.ToHeader);
    Response.Expires.NumericValue := Expires;

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

//* TestTIdSipOutboundSubscriptionBase Published methods ***********************

procedure TestTIdSipOutboundSubscriptionBase.TestAbandonAuthentication;
var
  Action: TIdSipOutboundSubscription;
begin
  // This test only makes sense for OUTbound actions.
  if Self.IsInboundTest then Exit;

  Action := Self.CreateAction as TIdSipOutboundSubscription;

  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');

  Action.Terminate;
  Check(Action.Terminating,
        Self.ClassName + ': Action not terminating');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestAddListener;
var
  Listener: TIdSipTestSubscriptionListener;
begin
  Listener := TIdSipTestSubscriptionListener.Create;
  try
    Self.Subscription.AddListener(Listener);
    Self.ReceiveNotifyTerminated(Self.Subscription);

    Check(Listener.ExpiredSubscription,
          Self.ClassName
        + ': Test case not notified of terminated subscription; '
        + 'thus, not added as listener');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipOutboundSubscriptionBase.TestCircularRedirect;
var
  Action:    TIdSipAction;
  ClassName: String;
begin
  //  ---   SUBSCRIBE (original)   --->
  // <---   302 Moved Temporarily  ---
  //  --- SUBSCRIBE (redirect #1)  --->
  // <---   302 Moved Temporarily  ---
  //  --- SUBSCRIBE (redirect #2)  --->
  // <---   302 Moved Temporarily  ---
  //  --- SUBSCRIBE (redirect #1)  ---> again!
  // <---   302 Moved Temporarily  ---

  Action := Self.CreateAction;
  ClassName := Action.ClassName;
  Self.ReceiveMovedTemporarily('sip:foo@bar.org');
  Self.ReceiveMovedTemporarily('sip:bar@bar.org');

  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily('sip:foo@bar.org');
  CheckNoRequestSent('The ' + ClassName + ' accepted the run-around');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestDoubleRedirect;
var
  Action: TIdSipAction;
  Method: String;
begin
  //  ---   SUBSCRIBE (original)   --->
  // <---   302 Moved Temporarily  ---
  //  --- SUBSCRIBE (redirect #1)  --->
  // <---   302 Moved Temporarily  ---
  //  --- SUBSCRIBE (redirect #2)  --->
  // <---   302 Moved Temporarily  ---

  Action := Self.CreateAction;
  Method := Action.Method;
  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily('sip:foo@bar.org');
  CheckRequestSent('No redirected ' + Method + ' #1 sent: ' + Self.FailReason);
  CheckEquals('sip:foo@bar.org',
              Self.LastSentRequest.RequestUri.Uri,
              'Request-URI of redirect #1');

  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily('sip:baz@quaax.org');
  CheckRequestSent('No redirected ' + Method + ' #2 sent: ' + Self.FailReason);
  CheckEquals('sip:baz@quaax.org',
              Self.LastSentRequest.RequestUri.Uri,
              'Request-URI of redirect #2');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestMatchForkedNotify;
var
  Notify: TIdSipRequest;
  Sub:    TIdSipOutboundSubscription;
begin
  // cf. RFC 3265, section 3.3.3.

  Sub := Self.CreateSubscription;

  Notify := TIdSipRequest.Create;
  try
    Notify.Method := MethodNotify;
    Notify.CallID := Sub.InitialRequest.CallID;
    Notify.AddHeader(Sub.InitialRequest.Event);
    Notify.ToHeader.Tag := Sub.InitialRequest.From.Tag;
    Notify.From.Tag     := Sub.InitialRequest.ToHeader.Tag + '1';

    Check(Sub.Match(Notify),
          Self.ClassName
        + ': Subscription MUST accept NOTIFYs from a forked subscription '
        + '(RFC 3265, section 3.3.3)');
  finally
    Notify.Free;
  end;
end;

procedure TestTIdSipOutboundSubscriptionBase.TestMatchNotify;
var
  Notify: TIdSipRequest;
  Sub:    TIdSipOutboundSubscription;
begin
  Sub := Self.CreateSubscription;

  Notify := TIdSipRequest.Create;
  try
    Notify.Method := MethodNotify;

    Check(not Sub.Match(Notify),
          Self.ClassName
        + ': Only matching request method');

    Notify.CallID := Sub.InitialRequest.CallID;
    Check(not Sub.Match(Notify),
          Self.ClassName
        + ': Only matching request method, Call-ID');

    Notify.AddHeader(Sub.InitialRequest.Event);
    Check(not Sub.Match(Notify),
          Self.ClassName
        + ': Only matching request method, Call-ID, Event');

    Notify.ToHeader.Tag := Sub.InitialRequest.From.Tag;
    Check(Sub.Match(Notify),
          Self.ClassName
        + ': Matching request method, Call-ID, Event, From-tag-and-To-tag');

    Notify.Method := MethodInvite;
    Check(not Sub.Match(Notify),
          Self.ClassName + ': Matches everything except method');
  finally
    Notify.Free;
  end;
end;

procedure TestTIdSipOutboundSubscriptionBase.TestMethod;
begin
  Fail(Self.ClassName + ' must override TestTIdSipOutboundSubscriptionBase.TestMethod');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceive2xxWithNoExpires;
var
  Sub: TIdSipOutboundSubscription;
begin
  Sub := Self.CreateSubscription;
  Self.ReceiveResponse(SIPAccepted);

  CheckExpires(Sub.Duration);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveActiveNotify;
var
  Sub: TIdSipOutboundSubscription;
begin
  Sub := Self.CreateSubscription;
  Self.ReceiveOk(Sub.InitialRequest);

  Check(not Self.SubscriptionEstablished,
        Self.ClassName
      + ': No subscription''s established until a NOTIFY says so');

  Self.ReceiveNotify(Sub.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateActive);

  Check(Self.SubscriptionEstablished,
        Self.ClassName
      + ': Subscription didn''t notify listeners of established subscription');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveFailureResponse;
begin
  Self.CheckReceiveFailureResponse(SIPBadRequest);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveGlobalFailureResponse;
begin
  Self.CheckReceiveFailureResponse(SIPBusyEverywhere);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveServerFailureResponse;
begin
  Self.CheckReceiveFailureResponse(SIPInternalServerError);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveNotify;
begin
  Self.MarkSentResponseCount;

  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateActive);

  Check(Self.SubscriptionNotified,
        Self.ClassName + ': Subscription didn''t notify listeners of received NOTIFY');

  // We would use .Equals() here, but remember that the transport could alter
  // the message, putting a received param on the topmost Via header.
  Check(Self.ReceivedNotify.Match(Self.LastSentRequest),
        Self.ClassName + ': Wrong NOTIFY in the notification');

  CheckResponseSent(Self.ClassName + ': No response to the NOTIFY sent');
  CheckEquals(SIPOK,
              Self.LastSentResponse.StatusCode,
              Self.ClassName + ': Unexpected response');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveNotifyBeforeSubscribesResponse;
const
  NotifyState = SubscriptionSubstateActive;
var
  OK:  TIdSipResponse;
  Sub: TIdSipOutboundSubscription;
begin
  // |       SUBSCRIBE      |
  // |--------------------->|
  // |        NOTIFY        |
  // |<---------------------|
  // |  200 OK for NOTIFY   |
  // |--------------------->|
  // | 200 OK for SUBSCRIBE |
  // |<---------------------|

  Sub := Self.CreateSubscription;
  OK := TIdSipResponse.InResponseTo(Sub.InitialRequest, SIPOK);
  try
    Self.MarkSentResponseCount;
    Self.ReceiveNotify(Sub.InitialRequest, OK, NotifyState);
    CheckResponseSent('No response sent for NOTIFY');
    CheckEquals(SIPOK, Self.LastSentResponse.StatusCode, 'Unexpected response sent');

    Check(Sub.DialogEstablished, 'NOTIFY didn''t establish a dialog');

    Self.ReceiveResponse(OK);

    CheckEquals(NotifyState, Sub.SubscriptionState, Self.ClassName + ': Subscription-State');
  finally
    OK.Free;
  end;
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyDeactivated;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonDeactivated);

  Self.CheckTerminatedSubscriptionWithResubscribe(EventReasonDeactivated);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyDeactivatedWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonDeactivated,
                     Self.ArbRetryAfterValue);

  Self.CheckNoRetryScheduled(EventReasonDeactivated);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyGiveUp;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonGiveUp);

  Self.CheckTerminatedSubscriptionWithResubscribe(EventReasonGiveUp);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyGiveUpWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonGiveUp);

  Self.CheckRetryScheduled(EventReasonGiveUp);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyWithNoReason;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated);

  Self.CheckTerminatedSubscriptionWithNoResubscribe('no reason');
  Self.CheckRetryScheduled('no reason');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyWithNoReasonAndRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     '',
                     Self.ArbRetryAfterValue);

  Self.CheckTerminatedSubscriptionWithNoResubscribe('no reason');
  Self.CheckRetryScheduled('no reason');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyNoResource;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonNoResource);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonNoResource);
  Self.CheckNoRetryScheduled(EventReasonNoResource);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyNoResourceWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonNoResource);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonNoResource);
  Self.CheckNoRetryScheduled(EventReasonNoResource);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyProbation;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonProbation);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonProbation);
  Self.CheckRetryScheduled(EventReasonProbation);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyProbationWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonProbation,
                     Self.ArbRetryAfterValue);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonProbation);
  Self.CheckRetryScheduled(EventReasonProbation);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyRejected;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonRejected);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonRejected);
  Self.CheckNoRetryScheduled(EventReasonRejected);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyRejectedWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonRejected);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonRejected);
  Self.CheckNoRetryScheduled(EventReasonRejected);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyTimeout;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonTimeout);

  Self.CheckTerminatedSubscriptionWithResubscribe(EventReasonTimeout);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyTimeoutWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonTimeout,
                     Self.ArbRetryAfterValue);

  Self.CheckTerminatedSubscriptionWithResubscribe(EventReasonTimeout);
  Self.CheckNoRetryScheduled(EventReasonTimeout);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyWithUnknownReason;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     Self.UnknownReason);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(Self.UnknownReason);
  Self.CheckRetryScheduled(Self.UnknownReason);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestReceiveTerminatingNotifyWithUnknownReasonAndRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     Self.UnknownReason,
                     Self.ArbRetryAfterValue);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(Self.UnknownReason);
  Self.CheckRetryScheduled(Self.UnknownReason);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestRedirectWithMultipleContacts;
var
  Action:    TIdSipAction;
  ClassName: String;
  Contacts:  array of String;
begin
  SetLength(Contacts, 2);
  Contacts[0] := 'sip:foo@bar.org';
  Contacts[1] := 'sip:bar@bar.org';

  Action := Self.CreateAction;
  ClassName := Action.ClassName;
  Self.MarkSentRequestCount;

  Self.ReceiveMovedTemporarily(Contacts);

  // ARG! Why do they make Length return an INTEGER? And WHY Abs() too?
  CheckEquals(Self.RequestCount + Cardinal(Length(Contacts)),
              Self.SentRequestCount,
              ClassName + ' didn''t attempt to contact all Contacts: ' + Self.FailReason);
end;

procedure TestTIdSipOutboundSubscriptionBase.TestRedirectWithMaxForwards;
const
  MaxForwards = 42;
var
  Subscribe: TIdSipOutboundSubscription;
begin
  Subscribe := Self.CreateSubscriptionWithoutSend;
  Subscribe.MaxForwards := MaxForwards;
  Subscribe.Send;

  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily('sip:foo');
  CheckRequestSent('No ' + Subscribe.Method + ' sent');

  CheckEquals(MaxForwards, Self.LastSentRequest.MaxForwards, 'Max-Forwards not overridden');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestRefresh;
const
  ExpiryTime = 1000;
begin
  //  --- SUBSCRIBE --->
  // <---  200 OK   ---
  //    <time passes>
  //  --- SUBSCRIBE --->
  // <---  200 OK   ---

  Self.MarkSentRequestCount;

  Self.Subscription.Refresh(ExpiryTime);

  CheckRequestSent(Self.ClassName + ': No request sent');
  CheckEquals(MethodSubscribe,
              Self.LastSentRequest.Method,
              Self.ClassName
            + ': Unexpected request sent');
  CheckEquals(Self.Subscription.EventPackage,
              Self.LastSentRequest.Event.Value,
              Self.ClassName
            + ': Wrong Event header');
  CheckEquals(Self.Subscription.EventID,
              Self.LastSentRequest.Event.ID,
              Self.ClassName
            + ': Wrong event ID');
  CheckEquals(Self.Subscription.InitialRequest.CallID,
              Self.LastSentRequest.CallID,
              Self.ClassName
            + ': Refresh must happen in the context of the original dialog: Call-ID');
  CheckEquals(Self.Subscription.InitialRequest.From.Tag,
              Self.LastSentRequest.From.Tag,
              Self.ClassName
            + ': Refresh must happen in the context of the original dialog: From tag');
  CheckEquals(Self.Subscription.InitialRequest.ToHeader.Tag,
              Self.LastSentRequest.ToHeader.Tag,
              Self.ClassName
            + ': Refresh must happen in the context of the original dialog: To tag');
  Check(Self.LastSentRequest.HasHeader(ExpiresHeader),
        Self.ClassName
      + ': Refresh has no Expires header');
  CheckEquals(ExpiryTime,
              Self.LastSentRequest.Expires.NumericValue,
              Self.ClassName
            + ': Refresh sent incorrect Expires time');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestRefreshReceives423;
const
  AttemptedExpiry = 1000;
  NewExpiry       = 2*AttemptedExpiry;
begin
  //  ---          SUBSCRIBE         --->
  // <---            200 OK          ---
  //             <time passes>
  //  ---          SUBSCRIBE         --->
  // <---   423 Interval Too Brief   ---
  //  --- SUBSCRIBE with new Expires --->

  Self.Subscription.Refresh(AttemptedExpiry);

  Self.MarkSentRequestCount;
  Self.ReceiveIntervalTooBrief(Self.LastSentRequest, NewExpiry);
  CheckRequestSent(Self.ClassName + ': No request sent, so no re-attempted SUBSCRIBE refresh');
  CheckEquals(MethodSubscribe,
              Self.LastSentRequest.Method,
              Self.ClassName
            + ': Wrong request sent');
  CheckEquals(NewExpiry,
              Self.LastSentRequest.Expires.NumericValue,
              Self.ClassName
            + ': Wrong expiry time attempted');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestRefreshReceives481;
begin
  //  ---                SUBSCRIBE                --->
  // <---                  200 OK                 ---
  //                   <time passes>
  //  ---                SUBSCRIBE                --->
  // <--- 481 Call Leg/Transaction Does Not Exist ---

  Self.Subscription.Refresh(1000);
  Self.ReceiveResponse(SIPCallLegOrTransactionDoesNotExist);

  Check(Self.SubscriptionFailed,
        Self.ClassName
      + ': Subscription didn''t fail (or didn''t notify us)');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestRefreshReceives4xx;
begin
  //  ---      SUBSCRIBE      --->
  // <---       200 OK        ---
  //          <time passes>
  //  ---      SUBSCRIBE      --->
  // <--- 408 Request Timeout --->

  Self.Subscription.Refresh(1000);
  Self.ReceiveResponse(SIPRequestTimeout);
  Check(not Self.SubscriptionExpired,
        Self.ClassName
      + ': Subscription mustn''t expire, but still exist until its Duration runs out');
  Check(not Self.SubscriptionExpired,
        Self.ClassName
      + ': mustn''t fail, but still exist until its Duration runs out');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestRefreshUpdatesExpiryTime;


var
  NewExpiryTime: Cardinal;
  OldExpiry:     TDateTime;
begin
  OldExpiry     := Self.Subscription.ExpiryTime;
  NewExpiryTime := TIdSipTestPackage.DefaultSubscriptionDuration * 2;

  Self.Subscription.Refresh(NewExpiryTime);
  Self.ReceiveOkFor(Self.LastSentRequest,
                    NewExpiryTime);

  // WARNING: this test can fail if you're debugging before this line.
  Check(OldExpiry < Self.Subscription.ExpiryTime,
        'Refresh didn''t reschedule expiration');

  Self.DebugTimer.TriggerEarliestEvent;

  Check(not Self.Subscription.IsTerminated,
        'Subscription terminated on the old expiry time');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestRemoveListener;
var
  Listener: TIdSipTestSubscriptionListener;
  Sub:      TIdSipOutboundSubscription;
begin
  Sub := Self.CreateSubscription;

  Listener := TIdSipTestSubscriptionListener.Create;
  try
    Sub.AddListener(Listener);
    Sub.RemoveListener(Listener);

    Self.ReceiveResponse(SIPNotImplemented);
    Check(not Listener.FailedSubscription,
          Self.ClassName
        + ': Test case notified of failure; thus, not removed as listener');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipOutboundSubscriptionBase.TestSendWithGruu;
var
  Sub: TIdSipRequest;
begin
  Self.UseGruu;

  Self.MarkSentRequestCount;
  Self.CreateSubscription;
  CheckRequestSent('No request sent');

  Sub := Self.LastSentRequest;

  Check(Sub.HasHeader(SupportedHeaderFull),
        'Missing Supported header');
  Check(Sub.SupportsExtension(ExtensionGruu),
        'Supported header fails to indicate "gruu" support');
  Check(Sub.FirstContact.IsGruu,
        Sub.Method + ' didn''t use GRUU');
  Check(Sub.FirstContact.Address.HasGrid,
        Sub.Method + ' doesn''t have a "grid" parameter');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestSendWithMaxForwards;
const
  MaxForwards = 42;
var
  Subscribe: TIdSipOutboundSubscription;
begin
  Subscribe := Self.CreateSubscriptionWithoutSend;
  Subscribe.MaxForwards := MaxForwards;

  Self.MarkSentRequestCount;
  Subscribe.Send;
  CheckRequestSent('No ' + Subscribe.Method + ' sent');
  CheckEquals(MaxForwards, Self.LastSentRequest.MaxForwards, 'Max-Forwards not overidden');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestTerminate;
begin
  Self.MarkSentRequestCount;
  Self.Subscription.Terminate;
  Self.CheckTerminatedSubscription(Self.Subscription, 'Terminate');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestTerminateBeforeEstablished;
var
  Sub: TIdSipOutboundSubscription;
begin
  Sub := Self.CreateSubscription;

  Self.MarkSentRequestCount;
  Sub.Terminate;
  Self.CheckTerminatedSubscription(Sub, 'Terminate before established');
end;

procedure TestTIdSipOutboundSubscriptionBase.TestTerminateSignalled;
var
  L: TIdSipTestActionListener;
begin
  L := TIdSipTestActionListener.Create;
  try
    Self.Subscription.AddActionListener(L);

    Self.Subscription.Terminate;
    Self.ReceiveNotifyTerminated(Self.Subscription);

    Check(L.Terminated, Self.ClassName + ': Listener not notified of termination');

    // No removal of listener necessary, since Self.Subscription is now freed.
  finally
    L.Free;
  end;
end; 

procedure TestTIdSipOutboundSubscriptionBase.TestUnsubscribe;
begin
  Self.MarkSentRequestCount;
  Self.Subscription.Unsubscribe;
  Self.CheckTerminatedSubscription(Self.Subscription, 'Unsubscribe');
end;

//******************************************************************************
//* TestTIdSipOutboundSubscription                                             *
//******************************************************************************
//* TestTIdSipOutboundSubscription Protected methods ***************************

procedure TestTIdSipOutboundSubscription.OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                                               Subscription: TIdSipOutboundSubscription);
begin
  inherited OnRenewedSubscription(UserAgent, Subscription);

  Self.RenewSubscriptionFired := true;
end;

//* TestTIdSipOutboundSubscription Private methods *****************************
{
procedure TestTIdSipOutboundSubscription.ReceiveNotifyNoAuth(Subscribe: TIdSipRequest;
                                                             Response: TIdSipResponse;
                                                             const State: String);
var
  Notify: TIdSipRequest;
begin
  Notify := Self.CreateNotify(Subscribe,
                              Response,
                              State);
  try
    Self.ReceiveRequest(Notify);
  finally
    Notify.Free;
  end;
end;

procedure TestTIdSipOutboundSubscription.ReceiveNotifyWrongAuth(Subscribe: TIdSipRequest;
                                                                Response: TIdSipResponse;
                                                                const State: String);
var
  AuthCreds: TIdSipAuthorizationHeader;
  Notify:    TIdSipRequest;
begin
  Notify := Self.CreateNotify(Subscribe,
                              Response,
                              State);
  try
    AuthCreds := Self.RemoteRealmInfo.CreateAuthorization(Self.ChallengeResponse,
                                                          MethodNotify,
                                                          Notify.Body,
                                                          'wrong ' + Self.Password);
    try
      Notify.AddHeader(AuthCreds);
    finally
      AuthCreds.Free;
    end;

    Self.ReceiveRequest(Notify);
  finally
    Notify.Free;
  end;
end;
}
//* TestTIdSipOutboundSubscription Published methods ***************************

procedure TestTIdSipOutboundSubscription.TestLongRunningSubscription;
begin
  // This test checks that multiple refreshes, over time, work as
  // expected: that the subscription send periodic SUBSCRIBEs refreshing
  // the remote subscription.

  //  --- SUBSCRIBE --->
  // <---  200 OK   ---
  // <---  NOTIFY   ---
  //  ---  200 OK   ---> (Our test precondition: EstablishSubscription)
  //  "time passes"
  //  --- SUBSCRIBE ---> (Refresh #1)
  // <---  200 OK   ---
  // <---  NOTIFY   ---
  //  ---  200 OK   --->
  //  more "time passes"
  //  --- SUBSCRIBE ---> (Refresh #2)
  // <---  200 OK   ---
  // <---  NOTIFY   ---
  //  ---  200 OK   --->

  Self.MarkSentRequestCount;
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionWait);
  Check(not Self.Subscription.IsTerminated,
        'Subscription terminated after the first re-SUBSCRIBE');
  CheckRequestSent('No request sent: no re-SUBSCRIBE');

  // Receive a 200 OK for the re-SUBSCRIBE
  Self.ReceiveOkFor(Self.LastSentRequest,
                    TIdSipTestPackage.DefaultSubscriptionDuration);

  Self.MarkSentRequestCount;
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionWait);
  Check(not Self.Subscription.IsTerminated,
        'Subscription terminated after the second re-SUBSCRIBE');
  CheckRequestSent('No request sent: no second re-SUBSCRIBE');
end;

procedure TestTIdSipOutboundSubscription.TestMethod;
var
  Action: TIdSipAction;
begin
  Action := Self.CreateAction;

  CheckEquals(MethodSubscribe,
              Action.Method,
              Action.ClassName + '; Method');
end;

procedure TestTIdSipOutboundSubscription.TestReceive2xx;
const
  ExpireTime = 1000;
var
  Sub: TIdSipOutboundSubscription;
begin
  Sub := Self.CreateSubscription;
  Self.ReceiveOkFor(Self.LastSentRequest, ExpireTime);

  Check(not Self.SubscriptionEstablished,
        'Subscriptions are only established when we receive a NOTIFY saying so');

  CheckExpires(ExpireTime);

  CheckEquals(ExpireTime,
              Sub.Duration,
              Self.ClassName + ': Subscription''s duration not updated');
end;

procedure TestTIdSipOutboundSubscription.TestReceiveActiveNotifyWithExpires;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateActive,
                     '',
                     0,
                     Self.ArbExpiresValue);

  Self.CheckExpires(Self.ArbExpiresValue);
end;
{
procedure TestTIdSipOutboundSubscription.TestReceiveNotifyNoAuthorization;
begin
  Fail('Authorization implementation deferred');
  Self.MarkSentResponseCount;

  Self.ReceiveNotifyNoAuth(Self.Subscription.InitialRequest,
                           Self.LastSentResponse,
                           SubscriptionSubstateActive);

  CheckResponseSent('No response to the NOTIFY sent');
  CheckEquals(SIPUnauthorized,
              Self.LastSentResponse.StatusCode,
              'Unexpected response: RFC 3265 section 3.2.4 says you SHOULD require authentication for NOTIFYs');
  Check(Self.LastSentResponse.HasWWWAuthenticate,
        'Challenge response lacks a WWW-Authenticate');
end;

procedure TestTIdSipOutboundSubscription.TestReceiveNotifyWrongAuthorization;
begin
  Fail('Authorization implementation deferred');
  Self.MarkSentResponseCount;

  Self.ReceiveNotifyWrongAuth(Self.Subscription.InitialRequest,
                             Self.LastSentResponse,
                             SubscriptionSubstateActive);

  CheckResponseSent('No response to the NOTIFY sent');
  CheckEquals(SIPUnauthorized,
              Self.LastSentResponse.StatusCode,
              'Unexpected response: bad Authorization credentials');
  Check(Self.LastSentResponse.HasWWWAuthenticate,
        'Challenge response lacks a WWW-Authenticate');
end;
}
procedure TestTIdSipOutboundSubscription.TestReceivePendingNotifyWithExpires;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstatePending,
                     '',
                     0,
                     Self.ArbExpiresValue);

  Self.CheckExpires(Self.ArbExpiresValue);
end;

procedure TestTIdSipOutboundSubscription.TestSetEventPackage;
var
  Sub: TIdSipOutboundSubscription;
begin
  Sub := Self.CreateSubscription;
  CheckEquals(TIdSipTestPackage.DefaultSubscriptionDuration,
              Sub.Duration,
              'Setting EventPackage didn''t set subscription duration');
end;

procedure TestTIdSipOutboundSubscription.TestSubscribe;
var
  Sub: TIdSipOutboundSubscription;
begin
  Self.MarkSentRequestCount;
  Sub := Self.CreateSubscription;
  CheckRequestSent('No request sent');
  CheckEquals(Sub.Method,
              Self.LastSentRequest.Method,
              'Unexpected request sent');
end;

//******************************************************************************
//* TestTIdSipInboundReferral                                                  *
//******************************************************************************
//* TestTIdSipInboundReferral Public methods ***********************************

procedure TestTIdSipInboundReferral.SetUp;
begin
  inherited SetUp;

  Self.Refer := Self.Action as TIdSipInboundReferral;
end;

//* TestTIdSipInboundReferral Protected methods ********************************

procedure TestTIdSipInboundReferral.ReceiveSubscribeRequest;
begin
  // cf. TestTIdSipInboundSubscription.ReceiveSubscribeRequest.
  Self.Module.AddPackage(TIdSipReferPackage);
  Self.ReceiveRefer(Self.RemoteParty);
end;

procedure TestTIdSipInboundReferral.ReceiveSubscribeRequestWithGruu;
const
  MinExpTime = 42;
var
  Refer: TIdSipRequest;
begin
  // cf. TestTIdSipInboundSubscription.ReceiveSubscribeRequestWithGruu.
  Self.Module.AddPackage(TIdSipReferPackage);

  Refer := Self.Module.CreateRefer(Self.Core.From, Self.Destination, Self.Core.From, TIdSipRequest.DefaultMaxForwards);
  try
    // See the comment in TSubscribeTestCase.ReceiveSubscribe.
    Refer.FirstContact.Address := Self.Destination.Address;
    Refer.Supported.Values.Add(ExtensionGruu);

    Self.ReceiveRequest(Refer);
  finally
    Refer.Free;
  end;
end;

//* TestTIdSipInboundReferral Private methods **********************************

procedure TestTIdSipInboundReferral.ReceiveRefer(Target: TIdSipAddressHeader);
var
  Refer: TIdSipRequest;
begin
  Refer := Self.Module.CreateRefer(Self.RemoteParty, Self.Core.From, Target, TIdSipRequest.DefaultMaxForwards);
  try
    Refer.From.Address         := Self.Destination.Address;
    Refer.FirstContact.Address := Self.Destination.Address;

    Self.ReceiveRequest(Refer);
  finally
    Refer.Free;
  end;
end;

//* TestTIdSipInboundReferral Published methods ********************************

procedure TestTIdSipInboundReferral.TestNotify;
const
  Body     = 'SIP/2.0 200 OK';
  MimeType = SipFragmentMimeType;
begin
  Self.Action.Accept;

  Self.MarkSentRequestCount;
  Self.Action.Notify(Body, MimeType);

  CheckRequestSent('No request sent');
  Self.CheckNotify(Self.LastSentRequest, Body, MimeType);
end;

procedure TestTIdSipInboundReferral.TestNotifyWithGruu;
var
  Accepted: TIdSipResponse;
  Refer:    TIdSipRequest;
  Referral: TIdSipInboundReferral;
begin
  // Set us up to support GRUU
  Self.UseGruu;

  Refer := Self.Module.CreateRefer(Self.RemoteParty, Self.Destination, Self.Core.From, TIdSipRequest.DefaultMaxForwards);
  try
    // See the comment in TSubscribeTestCase.ReceiveSubscribe.
    Refer.FirstContact.Address := Self.Destination.Address;

    Self.ReceiveRequest(Refer);
    Check(Assigned(Self.Action),
          'No subscribing request received');
    Referral := Self.Action as TIdSipInboundReferral;

    Accepted := Self.LastSentResponse;
    Check(Accepted.FirstContact.Address.HasGrid,
          'Dialog-establishing response should have a "grid" parameter');

    Referral.Accept;

    Self.MarkSentRequestCount;
    Referral.ReferenceTrying;

    CheckRequestSent('No request sent');
    Self.CheckNotify(Self.LastSentRequest,
                     TIdSipInboundReferral.ReferralTryingBody,
                     SipFragmentMimeType);
    Check(Self.LastSentRequest.HasHeader(SupportedHeaderFull),
          'NOTIFY lacks a Supported header');
    Check(Self.LastSentRequest.SupportsExtension(ExtensionGruu),
          'Supported header lacks a "gruu" entry');
    CheckEquals(Referral.LocalGruu.AsString,
                Self.LastSentRequest.FirstContact.AsString,
                'NOTIFY didn''t use ' + Referral.Method + '''s GRUU');
    CheckEquals(Self.LastSentRequest.FirstContact.Address.Host,
                Accepted.FirstContact.Address.Host,
                'NOTIFY''s remote target should match that of the 202 Accepted');
  finally
    Refer.Free;
  end;
end;

procedure TestTIdSipInboundReferral.TestNotifyWithInappropriateBody;
const
  MimeType = 'text/plain';
begin
  try
    Self.Action.Notify('random data', MimeType);

    Fail('Failed to bail out when sending a REFER NOTIFY with a ' + MimeType + ' body');
  except
    on EIdSipTransactionUser do;
  end;
end;

procedure TestTIdSipInboundReferral.TestNotifyWithNoBody;
begin
  try
    Self.Action.Notify('', SipFragmentMimeType);

    Fail('Failed to bail out when sending a REFER NOTIFY with a missing body');
  except
    on EIdSipTransactionUser do;
  end;
end;

procedure TestTIdSipInboundReferral.TestReceiveRefer;
var
  Notify: TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.ReceiveRefer(Self.Destination);

  CheckRequestSent('No request sent');
  CheckEquals(MethodNotify,
              Self.LastSentRequest.Method,
              'Unexpected request sent');

  Notify := Self.LastSentRequest;
  CheckNotEquals('',
                 Notify.Body,
                 'No body present in the NOTIFY');
  CheckEquals(SipFragmentMimeType,
              Notify.ContentType,
              'Incorrect Content-Type value');
  CheckEquals(TIdSipInboundReferral.ReferralTryingBody,
              Notify.Body,
              'Incorrect body');
end;

procedure TestTIdSipInboundReferral.TestReferenceDenied;
var
  Notify: TIdSipRequest;
begin
  Self.Refer.Accept;

  Self.MarkSentRequestCount;
  Self.Refer.ReferenceDenied;
  Self.CheckSendNotify(Self.Refer, SubscriptionSubstateTerminated);

  Notify := Self.LastSentRequest;
  CheckEquals(SipFragmentMimeType,
              Notify.ContentType,
              'NOTIFY Content-Type');
  CheckEquals(TIdSipInboundReferral.ReferralDeniedBody,
              Notify.Body,
              'NOTIFY body');
  CheckEquals(EventReasonNoResource,
              Notify.SubscriptionState.Reason,
              'Subscription-State reason');
  Check(Self.Refer.IsTerminated,
        'Referral didn''t terminate');
end;

procedure TestTIdSipInboundReferral.TestReferenceFailed;
var
  Notify: TIdSipRequest;
begin
  Self.Refer.Accept;

  Self.MarkSentRequestCount;
  Self.Refer.ReferenceFailed;
  Self.CheckSendNotify(Self.Refer, SubscriptionSubstateTerminated);

  Notify := Self.LastSentRequest;
  CheckEquals(SipFragmentMimeType,
              Notify.ContentType,
              'NOTIFY Content-Type');
  CheckEquals(TIdSipInboundReferral.ReferralFailedBody,
              Notify.Body,
              'NOTIFY body');
  CheckEquals(EventReasonNoResource,
              Notify.SubscriptionState.Reason,
              'Subscription-State reason');
  Check(Self.Refer.IsTerminated,
        'Referral didn''t terminate');
end;

procedure TestTIdSipInboundReferral.TestReferenceFailedWithResponse;
var
  Notify:              TIdSipRequest;
  ReferFailedResponse: TIdSipResponse;
begin
  ReferFailedResponse := TIdSipResponse.Create;
  try
    ReferFailedResponse.StatusCode := SIPBusyHere;
    ReferFailedResponse.ContentType := 'text/plain';
    ReferFailedResponse.Body := 'A body explaining the failure reason';

    Self.Refer.Accept;

    Self.MarkSentRequestCount;
    Self.Refer.ReferenceFailed(ReferFailedResponse);
    Self.CheckSendNotify(Self.Refer, SubscriptionSubstateTerminated);

    Notify := Self.LastSentRequest;
    CheckEquals(SipFragmentMimeType,
                Notify.ContentType,
                'NOTIFY Content-Type');
    CheckEquals(ReferFailedResponse.AsString,
                Notify.Body,
                'NOTIFY body');
    CheckEquals(EventReasonNoResource,
                Notify.SubscriptionState.Reason,
                'Subscription-State reason');
    Check(Self.Refer.IsTerminated,
          'Referral didn''t terminate');
  finally
    ReferFailedResponse.Free;
  end;
end;

procedure TestTIdSipInboundReferral.TestReferenceSucceeded;
var
  Notify: TIdSipRequest;
begin
  Self.Refer.Accept;

  Self.MarkSentRequestCount;
  Self.Refer.ReferenceSucceeded;
  Self.CheckSendNotify(Self.Refer, SubscriptionSubstateTerminated);

  Notify := Self.LastSentRequest;
  CheckEquals(SipFragmentMimeType,
              Notify.ContentType,
              'NOTIFY Content-Type');
  CheckEquals(TIdSipInboundReferral.ReferralSucceededBody,
              Notify.Body,
              'NOTIFY body');
  Check(Self.Refer.IsTerminated,
        'Referral didn''t terminate');
end;

procedure TestTIdSipInboundReferral.TestReferenceTrying;
var
  Notify: TIdSipRequest;
begin
  Self.Refer.Accept;

  Self.MarkSentRequestCount;
  Self.Refer.ReferenceTrying;
  Self.CheckSendNotify(Self.Refer, SubscriptionSubstateActive);

  Notify := Self.LastSentRequest;
  CheckEquals(SipFragmentMimeType,
              Notify.ContentType,
              'NOTIFY Content-Type');
  CheckEquals(TIdSipInboundReferral.ReferralTryingBody,
              Notify.Body,
              'NOTIFY body');
  Check(not Self.Refer.IsTerminated,
        'Referral terminated');
end;

procedure TestTIdSipInboundReferral.TestRejectUnsupportedReferToUri;
var
  HttpReferTo: TIdSipAddressHeader;
begin
  Fail('This test cannot work until TIdSipUriHeaders can properly support non-SIP URIs');
  HttpReferTo := TIdSipFromHeader.Create;
  try
    HttpReferTo.Address.Uri := 'http://www.example.com/';

    Self.MarkSentResponseCount;
    Self.ReceiveRefer(HttpReferTo);
    CheckResponseSent('No request sent');
    CheckEquals(SIPBadRequest,
                Self.LastSentResponse.StatusCode,
                'Unexpected response');
  finally
    HttpReferTo.Free;
  end;
end;

procedure TestTIdSipInboundReferral.TestRenotifySendsCorrectState;
var
  ExpectedBody: String;
begin
  // Make the notify fail. Since this response indicates that we can retry the
  // NOTIFY, we check that the resending of the NOTIFY uses the right body,
  // and indicates the correct subscription state.

  Self.Refer.Accept;

  Self.Refer.ReferenceTrying;
  ExpectedBody := Self.LastSentRequest.Body;

  Self.ReceiveUnauthorized(WWWAuthenticateHeader, QopAuth);

  Self.MarkSentRequestCount;
  Self.Refer.Renotify;
  CheckRequestSent('Renotify didn''t send a request');
  CheckEquals(ExpectedBody,
              Self.LastSentRequest.Body,
              'Unexpected body: the package''s state wasn''t updated');
end;

//******************************************************************************
//* TestTIdSipOutboundReferral                                                 *
//******************************************************************************
//* TestTIdSipOutboundReferral Public methods **********************************

procedure TestTIdSipOutboundReferral.TestEventPackage;
begin
  CheckEquals(PackageRefer,
              Self.LastSentRequest.Event.Value,
              'Event header of REFER MUST contain ONLY "refer"');
end;

procedure TestTIdSipOutboundReferral.TestMethod;
var
  Action: TIdSipAction;
begin
  Action := Self.CreateAction;

  CheckEquals(MethodRefer,
              Action.Method,
              Action.ClassName + '; Method');
end;

procedure TestTIdSipOutboundReferral.TestReceiveSecondFinalResponse;
var
  Action:    TIdSipAction;
  ClassName: String;
begin
  //  ---                 REFER                  --->
  // <---                 200 OK                 ---
  //          <transaction's Timer K fires>
  // <-- 481 Call leg/Transaction Does Not Exist --- (*)
  //
  // (*) No RFC 3261-compliant would send a 481 for a request after sending a
  // 200 OK to the same request. Nevertheless, the author's seen it happen.
  //
  // What we expect to happen is that the Action drops/ignores the second
  // response.
  Action := Self.CreateAction;
  ClassName := Action.ClassName;

  Self.ReceiveOk(Self.LastSentRequest);
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipClientNonInviteTransactionTimerKWait);
  Self.ReceiveServiceUnavailable(Self.LastSentRequest);
  Check(not Action.IsTerminated,
        ClassName + ' accepted second response, and now thinks it failed');
end;

procedure TestTIdSipOutboundReferral.TestReceiveTerminatingNotifyDeactivated;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonDeactivated);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonDeactivated);
  Self.CheckNoRetryScheduled(EventReasonGiveUp);
end;

procedure TestTIdSipOutboundReferral.TestReceiveTerminatingNotifyGiveUp;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonGiveUp);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonGiveUp);
  Self.CheckNoRetryScheduled(EventReasonGiveUp);
end;

procedure TestTIdSipOutboundReferral.TestReceiveTerminatingNotifyGiveUpWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonGiveUp);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonGiveUp);
  Self.CheckNoRetryScheduled(EventReasonGiveUp);
end;

procedure TestTIdSipOutboundReferral.TestReceiveTerminatingNotifyWithNoReason;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated);

  Self.CheckTerminatedSubscriptionWithNoResubscribe('no reason');
  Self.CheckNoRetryScheduled('no reason');
end;

procedure TestTIdSipOutboundReferral.TestReceiveTerminatingNotifyWithNoReasonAndRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     '',
                     Self.ArbRetryAfterValue);

  Self.CheckTerminatedSubscriptionWithNoResubscribe('no reason');
  Self.CheckNoRetryScheduled('no reason');
end;

procedure TestTIdSipOutboundReferral.TestReceiveTerminatingNotifyProbation;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonProbation);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonProbation);
  Self.CheckNoRetryScheduled(EventReasonProbation);
end;

procedure TestTIdSipOutboundReferral.TestReceiveTerminatingNotifyProbationWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonProbation,
                     Self.ArbRetryAfterValue);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonProbation);
  Self.CheckNoRetryScheduled(EventReasonProbation);
end;

procedure TestTIdSipOutboundReferral.TestReceiveTerminatingNotifyTimeout;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonTimeout);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonTimeout);
  Self.CheckNoRetryScheduled(EventReasonTimeout);
end;

procedure TestTIdSipOutboundReferral.TestReceiveTerminatingNotifyTimeoutWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonTimeout,
                     Self.ArbRetryAfterValue);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonTimeout);
  Self.CheckNoRetryScheduled(EventReasonTimeout);
end;

procedure TestTIdSipOutboundReferral.TestReceiveTerminatingNotifyWithUnknownReason;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     Self.UnknownReason);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(Self.UnknownReason);
  Self.CheckNoRetryScheduled(Self.UnknownReason);
end;

procedure TestTIdSipOutboundReferral.TestReceiveTerminatingNotifyWithUnknownReasonAndRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.LastSentResponse,
                     SubscriptionSubstateTerminated,
                     Self.UnknownReason,
                     Self.ArbRetryAfterValue);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(Self.UnknownReason);
  Self.CheckNoRetryScheduled(Self.UnknownReason);
end;

procedure TestTIdSipOutboundReferral.TestSendWithTargetDialog;
var
  ID:  TIdSipDialogID;
  Ref: TIdSipOutboundReferral;
begin
  ID := TIdSipDialogID.Create('call-id', 'local-tag', 'remote-tag');
  try
    Ref := TIdSipOutboundReferral.Create(Self.Core);
    try
      // Supply some necessary, though arbitrary, details.
      Ref.ReferredResource := Self.Destination;
      Ref.Target           := Self.Destination;

      Ref.TargetDialog := ID;

      Self.MarkSentRequestCount;
      Ref.Send;
      CheckRequestSent('No request sent');
      Check(Self.LastSentRequest.HasHeader(TargetDialogHeader),
            'Request missing a Target-Dialog header');
      CheckEquals(ID.CallID,
                  Self.LastSentRequest.TargetDialog.CallID,
                  'Target-Dialog''s call-id');
      CheckEquals(ID.LocalTag,
                  Self.LastSentRequest.TargetDialog.LocalTag,
                  'Target-Dialog''s local-tag');
      CheckEquals(ID.RemoteTag,
                  Self.LastSentRequest.TargetDialog.RemoteTag,
                  'Target-Dialog''s remote-tag');
    finally
      Ref.Free;
    end;
  finally
    ID.Free;
  end;
end;

procedure TestTIdSipOutboundReferral.TestTargetDialog;
var
  NewID: TIdSipDialogID;
  OldID: TIdSipDialogID;
  Ref:   TIdSipOutboundReferral;
begin
  NewID := TIdSipDialogID.Create('new-id', '1', '1');
  try
    OldID := TIdSipDialogID.Create('old-id', '1', '1');
    try
      Ref := TIdSipOutboundReferral.Create(Self.Core);
      try
        Ref.TargetDialog := OldID;
        Check(Ref.TargetDialog.Equals(OldID),
              'TargetDialog not assigned');

        Ref.TargetDialog := NewID;
        Check(Ref.TargetDialog.Equals(NewID),
              'TargetDialog not reassigned');
      finally
        Ref.Free;
      end;
    finally
      OldID.Free;
    end;
  finally
    NewID.Free;
  end;
end;

//* TestTIdSipOutboundReferral Protected methods *******************************

function TestTIdSipOutboundReferral.CreateReferral: TIdSipOutboundReferral;
begin
  Self.Module.AddPackage(TIdSipReferPackage);

  Result := Self.Module.Refer(Self.Destination, Self.Core.From);
  Result.AddActionListener(Self);
  Result.AddListener(Self);
  Result.Send;
end;

function TestTIdSipOutboundReferral.CreateSubscription: TIdSipOutboundSubscription;
begin
  Result := Self.CreateReferral;
end;

//******************************************************************************
//* TestTIdSipSubscriptionExpires                                              *
//******************************************************************************
//* TestTIdSipSubscriptionExpires Public methods *******************************

procedure TestTIdSipSubscriptionExpires.SetUp;
begin
  inherited SetUp;

  Self.OutSubscription := Self.Module.Subscribe(Self.Destination,
                                                TIdSipTestPackage.EventPackage);
  Self.OutSubscription.Send;                                              

  Self.Block := TIdSipSubscriptionExpires.Create;
end;

procedure TestTIdSipSubscriptionExpires.TearDown;
begin
  Self.Block.Free;

  inherited TearDown;
end;

//* TestTIdSipSubscriptionExpires Protected methods ****************************

procedure TestTIdSipSubscriptionExpires.OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                                              Subscription: TIdSipInboundSubscription);
begin
  inherited OnSubscriptionRequest(UserAgent, Subscription);

  Self.InSubscription := Subscription;
end;

//* TestTIdSipSubscriptionExpires Published methods ****************************

procedure TestTIdSipSubscriptionExpires.TestTriggerInboundSubscription;
begin
  Self.ReceiveSubscribe(TIdSipTestPackage.EventPackage);
  Self.Block.Execute(Self.InSubscription);

  Check(Self.InSubscription.IsTerminated,
        'Subscription didn''t terminate');
end;

procedure TestTIdSipSubscriptionExpires.TestTriggerOutboundSubscription;
begin
  Self.Block.Execute(Self.OutSubscription);

  Check(Self.OutSubscription.Terminating,
        'Subscription''s not terminating');
end;

//******************************************************************************
//* TestTIdSipSubscriptionRenotify                                             *
//******************************************************************************
//* TestTIdSipSubscriptionRenotify Public methods ******************************

procedure TestTIdSipSubscriptionRenotify.SetUp;
begin
  inherited SetUp;

  Self.Block := TIdSipSubscriptionRenotify.Create;

  Self.ReceiveSubscribe(TIdSipTestPackage.EventPackage);
end;

procedure TestTIdSipSubscriptionRenotify.TearDown;
begin
  Self.Block.Free;

  inherited TearDown;
end;

//* TestTIdSipSubscriptionRenotify Protected methods ***************************

procedure TestTIdSipSubscriptionRenotify.OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                                               Subscription: TIdSipInboundSubscription);
begin
  Self.Subscription := Subscription;
end;

//* TestTIdSipSubscriptionRenotify Publishedr methods **************************

procedure TestTIdSipSubscriptionRenotify.TestTrigger;
var
  OldPackageState: String;
begin
  Self.MarkSentRequestCount;
  Check(Assigned(Self.Subscription), 'OnSubscriptionRequest didn''t fire');
  Self.Subscription.Accept;
  CheckRequestSent('No NOTIFY sent');
  CheckEquals(MethodNotify, Self.LastSentRequest.Method, 'Unexpected request sent');

  OldPackageState := Self.LastSentRequest.Body;

  Self.MarkSentRequestCount;
  Self.Block.Execute(Self.Subscription);
  CheckRequestSent('No (re-)NOTIFY sent, so block didn''t execute properly');
  CheckEquals(MethodNotify, Self.LastSentRequest.Method, 'Unexpected request sent for renotify');

  CheckEquals(OldPackageState, Self.LastSentRequest.Body, 're-NOTIFY package state');
end;

procedure TestTIdSipSubscriptionRenotify.TestTriggerOnWrongTypeOfAction;
var
  Invite: TIdSipOutboundSession;
begin
  Invite := TIdSipOutboundSession.Create(Self.Core);
  try
    Self.MarkSentRequestCount;
    Self.Block.Execute(Invite);
    CheckNoRequestSent('Block didn''t abort execution');
  finally
    Invite.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipOutboundSubscriptionRefreshWait                                  *
//******************************************************************************
//* TestTIdSipOutboundSubscriptionRefreshWait Public methods *******************

procedure TestTIdSipOutboundSubscriptionRefreshWait.SetUp;
begin
  inherited SetUp;

  Self.NewDuration := 3600;

  Self.Subscription := Self.Module.Subscribe(Self.Destination, Self.Package.EventPackage);
  Self.Subscription.Send;

  Self.ReceiveOk(Self.LastSentRequest);
  Check(Self.Subscription.DialogEstablished, 'Subscription dialog not established');

  Self.Wait := TIdSipOutboundSubscriptionRefreshWait.Create;
  Self.Wait.ActionID    := Self.Subscription.ID;
  Self.Wait.NewDuration := Self.NewDuration;
end;

procedure TestTIdSipOutboundSubscriptionRefreshWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

procedure TestTIdSipOutboundSubscriptionRefreshWait.CheckTriggerDoesNothing(Wait: TIdWait;
                                                                            Msg: String);
begin
  Self.MarkSentRequestCount;
  Self.Wait.Trigger;
  CheckNoRequestSent(Msg);
end;

//* TestTIdSipOutboundSubscriptionRefreshWait Published methods ****************

procedure TestTIdSipOutboundSubscriptionRefreshWait.TestTrigger;
var
  Sub: TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.Wait.Trigger;
  CheckRequestSent('Wait didn''t trigger');

  Sub := Self.LastSentRequest;
  Check(Sub.HasHeader(ExpiresHeader),
        'Refreshing SUBSCRIBE missing Expires header');
  CheckEquals(Self.Wait.NewDuration,
              Sub.Expires.NumericValue,
              'Refreshing SUBSCRIBE has incorrect duration');

end;

procedure TestTIdSipOutboundSubscriptionRefreshWait.TestTriggerWithIDOfNonexistentObject;
begin
  // Check that the Wait does nothing if its ActionID doesn't point to a
  // registered object.

  Self.Wait.ActionID := 'fake ID';
  CheckTriggerDoesNothing(Self.Wait, 'Wait didn''t check object type before triggering');
end;

procedure TestTIdSipOutboundSubscriptionRefreshWait.TestTriggerWithIDOfWrongTypeOfObject;
var
  ArbitraryObject: TIdSipAction;
begin
  // This test checks two things:
  //   1. If you give the Wait the ID of an object that isn't a TIdSipOutboundSubscription,
  //      the Wait does nothing, and
  //   2. the Wait doesn't blow up.

  ArbitraryObject := TIdSipOutboundCancel.Create(Self.Core);
  try
    Self.Wait.ActionID := ArbitraryObject.ID;

    CheckTriggerDoesNothing(Self.Wait, 'Wait didn''t check object type before triggering');
  finally
    ArbitraryObject.Free;
  end;
end;

//******************************************************************************
//* TIdSipInboundSubscriptionTestCase                                          *
//******************************************************************************
//* TIdSipInboundSubscriptionTestCase Public methods ***************************

procedure TIdSipInboundSubscriptionTestCase.SetUp;
var
  L: TIdSipTestSubscribeModuleListener;
begin
  inherited SetUp;

  Self.Wait := Self.WaitType.Create;

  Self.AddRequiredPackage;
  L := TIdSipTestSubscribeModuleListener.Create;
  try
    Self.Module.AddListener(L);
    try
      Self.ReceiveSubscribeRequest;
      Check(L.SubscriptionParam <> nil, 'SUBSCRIBE rejected');
      Self.Wait.ActionID := L.SubscriptionParam.ID;
    finally
      Self.Module.RemoveListener(L);
    end;
  finally
    L.Free;
  end;
end;

procedure TIdSipInboundSubscriptionTestCase.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TIdSipInboundSubscriptionTestCase Protected methods ************************

procedure TIdSipInboundSubscriptionTestCase.AddRequiredPackage;
begin
  // Override this to add any packages the SubscribeModule needs to know about
  // for this test.
end;

procedure TIdSipInboundSubscriptionTestCase.CheckTriggerDoesNothing(Wait: TIdWait;
                                                                    Msg: String);
begin
  Self.MarkSentRequestCount;
  Self.Wait.Trigger;
  CheckNoRequestSent(Msg);
end;

procedure TIdSipInboundSubscriptionTestCase.ReceiveSubscribeRequest;
begin
  Self.ReceiveSubscribe(Self.Package.EventPackage);
end;

function TIdSipInboundSubscriptionTestCase.WaitType: TIdSipActionWaitClass;
begin
  Result := nil;

  Fail(Self.ClassName + ' must override WaitType');
end;

//* TIdSipInboundSubscriptionTestCase Published methods ************************

procedure TIdSipInboundSubscriptionTestCase.TestTriggerWithIDOfNonexistentObject;
begin
  // Check that the Wait does nothing if its SubscriptionID doesn't point to a
  // registered object.

  Self.Wait.ActionID := 'fake ID';
  CheckTriggerDoesNothing(Self.Wait, 'Wait didn''t check object type before triggering');
end;

procedure TIdSipInboundSubscriptionTestCase.TestTriggerWithIDOfWrongTypeOfObject;
var
  ArbitraryObject: TIdSipAction;
begin
  // This test checks two things:
  //   1. If you give the Wait the ID of an object that isn't a TIdSipInboundSubscription,
  //      the Wait does nothing, and
  //   2. the Wait doesn't blow up.

  ArbitraryObject := TIdSipOutboundCancel.Create(Self.Core);
  try
    Self.Wait.ActionID := ArbitraryObject.ID;

    CheckTriggerDoesNothing(Self.Wait, 'Wait didn''t check object type before triggering');
  finally
    ArbitraryObject.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipInboundSubscriptionNotifyWait                                    *
//******************************************************************************
//* TestTIdSipInboundSubscriptionNotifyWait Public methods *********************

procedure TestTIdSipInboundSubscriptionNotifyWait.SetUp;
var
  W: TIdSipInboundSubscriptionNotifyWait;
begin
  inherited SetUp;

  Self.MimeType     := SipFragmentMimeType;
  Self.Notification := TIdSipInboundReferral.ReferralSucceededBody;

  W := Self.Wait as TIdSipInboundSubscriptionNotifyWait;
  W.MimeType     := Self.MimeType;
  W.Notification := Self.Notification;
end;

//* TestTIdSipInboundSubscriptionNotifyWait Protected methods ******************

function TestTIdSipInboundSubscriptionNotifyWait.WaitType: TIdSipActionWaitClass;
begin
  Result := TIdSipInboundSubscriptionNotifyWait;
end;

//* TestTIdSipInboundSubscriptionNotifyWait Published methods ******************

procedure TestTIdSipInboundSubscriptionNotifyWait.TestTrigger;
begin
  Self.MarkSentRequestCount;
  Self.Wait.Trigger;
  CheckRequestSent('No NOTIFY sent');
  CheckEquals(MethodNotify, Self.LastSentRequest.Method, 'Unexpected request sent');
  CheckEquals(Self.MimeType, Self.LastSentRequest.ContentType, 'Content-Type');
  CheckEquals(Self.Notification, Self.LastSentRequest.Body, 'Message body');
end;

//******************************************************************************
//* TestTIdSipInboundReferralWait                                              *
//******************************************************************************
//* TestTIdSipInboundReferralWait Protected methods ****************************

procedure TestTIdSipInboundReferralWait.AddRequiredPackage;
begin
  Self.Module.AddPackage(TIdSipReferPackage);
end;

procedure TestTIdSipInboundReferralWait.CheckReferralResponse(Msg: String);
begin
  CheckRequestSent(Msg + ': no NOTIFY sent');
  CheckEquals(MethodNotify, Self.LastSentRequest.Method, Msg + ': Method');
end;

procedure TestTIdSipInboundReferralWait.ReceiveSubscribeRequest;
begin
  Self.ReceiveRefer(Self.Core.From);
end;

//* TestTIdSipInboundReferralWait Published methods ****************************

procedure TestTIdSipInboundReferralWait.TestHasResponse;
var
  Response: TIdSipResponse;
  W:        TIdSipInboundReferralWait;
begin
  W := Self.Wait as TIdSipInboundReferralWait;

  Response := TIdSipResponse.Create;
  try
    Check(not W.HasResponse, 'Initial state of HasResponse');

    W.Response := Response;
    Check(W.HasResponse, 'After assigning a response to Response');

    W.Response := nil;
    Check(not W.HasResponse, 'After assigning nil to Response');
  finally
    Response.Free
  end;
end;

procedure TestTIdSipInboundReferralWait.TestTrigger;
begin
  Self.MarkSentResponseCount;
  Self.MarkSentRequestCount;
  Self.Wait.Trigger;
  CheckReferralResponse('Unexpected NOTIFY sent');
end;

//******************************************************************************
//* TestTIdSipNotifyReferralDeniedWait                                         *
//******************************************************************************
//* TestTIdSipNotifyReferralDeniedWait Protected methods ***********************

procedure TestTIdSipNotifyReferralDeniedWait.CheckReferralResponse(Msg: String);
begin
  inherited CheckReferralResponse(Msg);

  CheckEquals(TIdSipInboundReferral.ReferralDeniedBody,
              Self.LastSentRequest.Body,
              Msg + ': body');
end;

function TestTIdSipNotifyReferralDeniedWait.WaitType: TIdSipActionWaitClass;
begin
  Result := TIdSipNotifyReferralDeniedWait;
end;

//******************************************************************************
//* TestTIdSipNotifyReferralFailedWait                                         *
//******************************************************************************
//* TestTIdSipNotifyReferralFailedWait Protected methods ***********************

procedure TestTIdSipNotifyReferralFailedWait.CheckReferralResponse(Msg: String);
begin
  inherited CheckReferralResponse(Msg);

  CheckEquals(TIdSipInboundReferral.ReferralFailedBody,
              Self.LastSentRequest.Body,
              Msg + ': body');
end;

function TestTIdSipNotifyReferralFailedWait.WaitType: TIdSipActionWaitClass;
begin
  Result := TIdSipNotifyReferralFailedWait;
end;

//******************************************************************************
//* TestTIdSipNotifyReferralSucceededWait                                      *
//******************************************************************************
//* TestTIdSipNotifyReferralSucceededWait Protected methods ********************

procedure TestTIdSipNotifyReferralSucceededWait.CheckReferralResponse(Msg: String);
begin
  inherited CheckReferralResponse(Msg);

  CheckEquals(TIdSipInboundReferral.ReferralSucceededBody,
              Self.LastSentRequest.Body,
              Msg + ': body');
end;

function TestTIdSipNotifyReferralSucceededWait.WaitType: TIdSipActionWaitClass;
begin
  Result := TIdSipNotifyReferralSucceededWait;
end;

//******************************************************************************
//* TestTIdSipNotifyReferralTryingWait                                         *
//******************************************************************************
//* TestTIdSipNotifyReferralTryingWait Protected methods ***********************

procedure TestTIdSipNotifyReferralTryingWait.CheckReferralResponse(Msg: String);
begin
  inherited CheckReferralResponse(Msg);

  CheckEquals(TIdSipInboundReferral.ReferralTryingBody,
              Self.LastSentRequest.Body,
              Msg + ': body');
end;

function TestTIdSipNotifyReferralTryingWait.WaitType: TIdSipActionWaitClass;
begin
  Result := TIdSipNotifyReferralTryingWait;
end;

//******************************************************************************
//* TestTIdSipSubscriptionRetryWait                                            *
//******************************************************************************
//* TestTIdSipSubscriptionRetryWait Public methods *****************************

procedure TestTIdSipSubscriptionRetryWait.SetUp;
begin
  inherited SetUp;

  Self.Wait := TIdSipSubscriptionRetryWait.Create;
  Self.Wait.EventPackage := TIdSipTestPackage.EventPackage;
  Self.Wait.Target.Value := 'sip:foo@bar';
  Self.Wait.ModuleID     := Self.Module.ID;
end;

procedure TestTIdSipSubscriptionRetryWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdSipSubscriptionRetryWait Private methods ****************************

procedure TestTIdSipSubscriptionRetryWait.CheckTriggerDoesNothing(Wait: TIdWait;
                                                                  Msg: String);
begin
  Self.Wait.Trigger;
  Check(not Self.OnRenewedSubscriptionFired, Msg);
end;

//* TestTIdSipSubscriptionRetryWait Published methods **************************

procedure TestTIdSipSubscriptionRetryWait.TestTrigger;
begin
  Self.Wait.Trigger;

  Check(Self.OnRenewedSubscriptionFired,
        'OnRenewedSubscription didn''t fire');
end;

procedure TestTIdSipSubscriptionRetryWait.TestTriggerWithIDOfNonexistentObject;
begin
  // Check that the Wait does nothing if its ModuleID doesn't point to a
  // registered object.

  Self.Wait.ModuleID := 'fake ID';
  CheckTriggerDoesNothing(Self.Wait, 'Wait didn''t check object type before triggering');
end;

procedure TestTIdSipSubscriptionRetryWait.TestTriggerWithIDOfWrongTypeOfObject;
var
  ArbitraryObject: TIdSipAction;
begin
  // This test checks two things:
  //   1. If you give the Wait the ID of an object that isn't a TIdSipSubscribeModule
  //      the Wait does nothing, and
  //   2. the Wait doesn't blow up.

  ArbitraryObject := TIdSipOutboundCancel.Create(Self.Core);
  try
    Self.Wait.ModuleID := ArbitraryObject.ID;

    CheckTriggerDoesNothing(Self.Wait, 'Wait didn''t check object type before triggering');
  finally
    ArbitraryObject.Free;
  end;
end;

//******************************************************************************
//* TSubscriptionActionMethodTestCase                                          *
//******************************************************************************
//* TSubscriptionActionMethodTestCase Public methods ***************************

procedure TSubscriptionActionMethodTestCase.SetUp;
begin
  inherited SetUp;

  Self.Module := Self.UA.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
end;

//******************************************************************************
//* TTestNotifyMethod                                                          *
//******************************************************************************
//* TTestNotifyMethod Public methods *******************************************

procedure TTestNotifyMethod.SetUp;
begin
  inherited SetUp;

  Self.Listener := TIdSipTestNotifyListener.Create;
  Self.Response := TIdSipResponse.Create;
  Self.Notify   := TIdSipOutboundNotify.Create(Self.UA);
end;

procedure TTestNotifyMethod.TearDown;
begin
  Self.Notify.Free;
  Self.Response.Free;
  Self.Listener.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipNotifyFailedMethod                                               *
//******************************************************************************
//* TestTIdSipNotifyFailedMethod Public methods ********************************

procedure TestTIdSipNotifyFailedMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipNotifyFailedMethod.Create;
  Self.Method.Notify   := Self.Notify;
  Self.Method.Response := Self.Response;
end;

procedure TestTIdSipNotifyFailedMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipNotifyFailedMethod Published methods *****************************

procedure TestTIdSipNotifyFailedMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.Failed, 'Listener not notified of failure');
  Check(Self.Notify = Self.Listener.NotifyAgentParam,
        'NotifyAgent param');
  Check(Self.Response = Self.Listener.ResponseParam,
        'Response param');
end;

//******************************************************************************
//* TestTIdSipNotifySucceededMethod                                            *
//******************************************************************************
//* TestTIdSipNotifySucceededMethod Public methods *****************************

procedure TestTIdSipNotifySucceededMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipNotifySucceededMethod.Create;
  Self.Method.Notify   := Self.Notify;
  Self.Method.Response := Self.Response;
end;

procedure TestTIdSipNotifySucceededMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipNotifySucceededMethod Published methods **************************

procedure TestTIdSipNotifySucceededMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.Succeeded, 'Listener not notified of Succeedure');
  Check(Self.Notify = Self.Listener.NotifyAgentParam,
        'NotifyAgent param');
  Check(Self.Response = Self.Listener.ResponseParam,
        'Response param');
end;

//******************************************************************************
//* TestTIdSipOutboundSubscriptionMethod                                       *
//******************************************************************************
//* TestTIdSipOutboundSubscriptionMethod Public methods ************************

procedure TestTIdSipOutboundSubscriptionMethod.SetUp;
begin
  inherited SetUp;

  Self.Listener     := TIdSipTestSubscriptionListener.Create;
  Self.Subscription := TIdSipOutboundSubscription.Create(Self.UA);
end;

procedure TestTIdSipOutboundSubscriptionMethod.TearDown;
begin
  Self.Subscription.Free;
  Self.Listener.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipEstablishedSubscriptionMethod                                    *
//******************************************************************************
//* TestTIdSipEstablishedSubscriptionMethod Public methods *********************

procedure TestTIdSipEstablishedSubscriptionMethod.SetUp;
begin
  inherited SetUp;

  Self.Notify := TIdSipRequest.Create;

  Self.Method := TIdSipEstablishedSubscriptionMethod.Create;
  Self.Method.Notify       := Self.Notify;
  Self.Method.Subscription := Self.Subscription;
end;

procedure TestTIdSipEstablishedSubscriptionMethod.TearDown;
begin
  Self.Method.Free;
  Self.Notify.Free;

  inherited TearDown;
end;

//* TestTIdSipEstablishedSubscriptionMethod Published methods ******************

procedure TestTIdSipEstablishedSubscriptionMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.EstablishedSubscription,
        'Listener not notified of established subscription');
  Check(Self.Notify = Self.Listener.NotifyParam,
        'Notify param');
  Check(Self.Subscription = Self.Listener.SubscriptionParam,
        'Subscription param');
end;

//******************************************************************************
//* TestTIdSipExpiredSubscriptionMethod                                        *
//******************************************************************************
//* TestTIdSipExpiredSubscriptionMethod Public methods *************************

procedure TestTIdSipExpiredSubscriptionMethod.SetUp;
begin
  inherited SetUp;

  Self.Notify := TIdSipRequest.Create;
  Self.Method := TIdSipExpiredSubscriptionMethod.Create;

  Self.Method.Notify       := Self.Notify;
  Self.Method.Subscription := Self.Subscription;
end;

procedure TestTIdSipExpiredSubscriptionMethod.TearDown;
begin
  Self.Method.Free;
  Self.Notify.Free;

  inherited TearDown;
end;

//* TestTIdSipExpiredSubscriptionMethod Published methods **********************

procedure TestTIdSipExpiredSubscriptionMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.ExpiredSubscription,
        'Listener not notified of expired subscription');
  Check(Self.Notify = Self.Listener.NotifyParam,
        'Notify param');
  Check(Self.Subscription = Self.Listener.SubscriptionParam,
        'Subscription param');
end;

//******************************************************************************
//* TestTIdSipFailedSubscriptionMethod                                         *
//******************************************************************************
//* TestTIdSipFailedSubscriptionMethod Public methods **************************

procedure TestTIdSipFailedSubscriptionMethod.SetUp;
begin
  inherited SetUp;

  Self.Response := TIdSipResponse.Create;
  Self.Method := TIdSipFailedSubscriptionMethod.Create;

  Self.Method.Response     := Self.Response;
  Self.Method.Subscription := Self.Subscription;
end;

procedure TestTIdSipFailedSubscriptionMethod.TearDown;
begin
  Self.Response.Free;
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipFailedSubscriptionMethod Published methods ***********************

procedure TestTIdSipFailedSubscriptionMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.FailedSubscription,
        'Listener not notified of failed subscription');
  Check(Self.Response = Self.Listener.ResponseParam,
        'Response param');
  Check(Self.Subscription = Self.Listener.SubscriptionParam,
        'Subscription param');
end;

//******************************************************************************
//* TestTIdSipOutboundSubscriptionNotifyMethod                                 *
//******************************************************************************
//* TestTIdSipOutboundSubscriptionNotifyMethod Public methods ******************

procedure TestTIdSipOutboundSubscriptionNotifyMethod.SetUp;
begin
  inherited SetUp;

  Self.Notify := TIdSipRequest.Create;
  Self.Method := TIdSipSubscriptionNotifyMethod.Create;

  Self.Method.Notify       := Self.Notify;
  Self.Method.Subscription := Self.Subscription;
end;

procedure TestTIdSipOutboundSubscriptionNotifyMethod.TearDown;
begin
  Self.Notify.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundSubscriptionNotifyMethod Published methods ***************

procedure TestTIdSipOutboundSubscriptionNotifyMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.Notify,
        'Listener not notified of inbound NOTIFY');
  Check(Self.Notify = Self.Listener.NotifyParam,
        'Notify param');
  Check(Self.Subscription = Self.Listener.SubscriptionParam,
        'Subscription param');
end;

//******************************************************************************
//* TSubscribeModuleTestCase                                                   *
//******************************************************************************
//* TSubscribeModuleTestCase Public methods ************************************

procedure TSubscribeModuleTestCase.SetUp;
begin
  inherited SetUp;

  Self.Listener := TIdSipTestSubscribeModuleListener.Create;

  Self.Request := TIdSipTestResources.CreateBasicRequest;
  Self.Request.Event.EventPackage := TIdSipTestPackage.EventPackage;

  Self.Module.AddPackage(TIdSipTestPackage);
end;

procedure TSubscribeModuleTestCase.TearDown;
begin
  Self.Request.Free;
  Self.Listener.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipRenewedSubscriptionMethod                                        *
//******************************************************************************
//* TestTIdSipRenewedSubscriptionMethod Public methods *************************

procedure TestTIdSipRenewedSubscriptionMethod.SetUp;
begin
  inherited SetUp;

  Self.Subscription := Self.Module.Subscribe(Self.UA.From,
                                             TIdSipTestPackage.EventPackage);

  Self.Method := TIdSipRenewedSubscriptionMethod.Create;
  Self.Method.Subscription := Self.Subscription;
end;

procedure TestTIdSipRenewedSubscriptionMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

procedure TestTIdSipRenewedSubscriptionMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.RenewedSubscription,
        'Listener not notified of renewed subscription');
  Check(Self.Subscription = Self.Listener.SubscriptionParam,
        'Subscription param');
end;

//******************************************************************************
//* TestTIdSipSubscriptionRequestMethod                                        *
//******************************************************************************
//* TestTIdSipSubscriptionRequestMethod Public methods *************************

procedure TestTIdSipSubscriptionRequestMethod.SetUp;
begin
  inherited SetUp;

  Self.Subscription := TIdSipInboundSubscription.CreateInbound(Self.UA, Self.Request, Self.Binding);
  Self.Method := TIdSipSubscriptionRequestMethod.Create;
  Self.Method.Subscription := Self.Subscription;
end;

procedure TestTIdSipSubscriptionRequestMethod.TearDown;
begin
  Self.Method.Free;
  Self.Subscription.Free;

  inherited TearDown;
end;

//* TestTIdSipSubscriptionRequestMethod Published methods **********************

procedure TestTIdSipSubscriptionRequestMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.SubscriptionRequest, 'Listener not notified');
  Check(Self.Method.Subscription = Self.Listener.SubscriptionParam,
        'Subscription param');
  Check(Self.Method.UserAgent = Self.Listener.UserAgentParam,
        'UserAgent param');
end;

initialization
  RegisterTest('Subscribe module', Suite);
end.
