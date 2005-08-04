{
  (c) 2005 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipSubscribeModule;

interface

uses
  IdSipAuthentication, IdSipCore, IdSipDialog, IdSipMessage,
  IdSipSubscribeModule, IdTimerQueue, TestFrameworkSip, TestFrameworkSipTU;

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
    UserAgentParam:             TIdSipAbstractUserAgent;

    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipOutboundSubscription); virtual;
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipInboundSubscription); virtual;
    procedure ReceiveSubscribe(const EventPackage: String;
                               ExpiryTime: Cardinal = 0); virtual;
  public
    procedure SetUp; override;
  end;

  TestTIdSipSubscribeModule = class(TSubscribeTestCase)
  private
    procedure CheckNoPackageFound(PackageType: TIdSipEventPackageClass);
    procedure CheckBadEventResponseSent(const UnknownEvent: String);
    procedure CheckPackageFound(PackageType: TIdSipEventPackageClass);
    procedure ReceiveNotify(const EventPackage: String);
    procedure ReceiveRefer;
    procedure ReceiveReferWithNoReferToHeader;
    procedure ReceiveSubscribeWithNoEventHeader;
  published
    procedure TestAcceptsMethodsWithReferPackage;
    procedure TestAddListener;
    procedure TestAddPackage;
    procedure TestPackage;
    procedure TestPackageFor;
    procedure TestReceiveReferWithNoReferTo;
    procedure TestReceiveReferNotifiesListeners;
    procedure TestRejectNewSubscribeForReferPackage;
    procedure TestRejectSubscribeWithNoEventHeader;
    procedure TestRejectUnknownEventSubscriptionRequest;
    procedure TestRejectUnmatchedNotify;
    procedure TestRemoveListener;
    procedure TestSubscribe;
    procedure TestSubscriptionRequest;
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

  TestTIdSipSubscribe = class(TestTIdSipAction)
  protected
    Module: TIdSipSubscribeModule;
  public
    procedure SetUp; override;
  end;

  TestTIdSipOutboundNotify = class(TestTIdSipSubscribe,
                                   IIdSipNotifyListener)
  private
    Body:              String;
    Dialog:            TIdSipDialog;
    Expires:           Cardinal;
    MimeType:          String;
    Notify:            TIdSipOutboundNotify;
    Subscribe:         TIdSipRequest;
    SubscriptionState: String;

    procedure OnFailure(NotifyAgent: TIdSipOutboundNotify;
                        Response: TIdSipResponse);
    procedure OnSuccess(NotifyAgent: TIdSipOutboundNotify;
                        Response: TIdSipResponse);
  protected
    procedure ConfigureNotify(Action: TIdSipOutboundNotify);
    function  CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddListener;
    procedure TestRemoveListener;
    procedure TestSend;
  end;

  TestTIdSipOutboundSubscribe = class(TestTIdSipSubscribe,
                                      IIdSipSubscribeListener)
  private
    EventPackage: String;
    Failed:       Boolean;
    ID:           String;
    Succeeded:    Boolean;

    procedure OnFailure(SubscribeAgent: TIdSipOutboundSubscribe;
                        Response: TIdSipResponse);
    procedure OnSuccess(SubscribeAgent: TIdSipOutboundSubscribe;
                        Response: TIdSipResponse);
  protected
    procedure ConfigureAction(Action: TIdSipAction); virtual;
    function  CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
  published
    procedure TestMatchNotify;
    procedure TestMatchResponse;
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
    procedure TestSend; override;
  end;

  TSubscribeModuleActionTestCase = class(TestTIdSipAction,
                                         IIdSipSubscribeModuleListener)
  protected
    Module:  TIdSipSubscribeModule;
    Package: TIdSipEventPackage;

    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipOutboundSubscription); virtual;
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipInboundSubscription); virtual;
  public
    procedure SetUp; override;
  end;

  TestTIdSipInboundSubscriptionBase = class(TSubscribeModuleActionTestCase)
  protected
    Action:        TIdSipInboundSubscription;
    ActionRequest: TIdSipRequest;

    procedure CheckSendNotify(Sub: TIdSipInboundSubscription;
                              const SubscriptionState: String);
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipInboundSubscription); override;
    procedure ReceiveSubscribeRequest; virtual;
  public
    procedure SetUp; override;
  published
    procedure TestAccept;
    procedure TestDontMatchInDialogInvite;
    procedure TestIsInbound; override;
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
    procedure TestReceiveRequestSendsNotify;
  end;

  TestTIdSipInboundSubscription = class(TestTIdSipInboundSubscriptionBase)
  private
    ID: String;

    procedure CheckExpiresScheduled(ExpectedExpires: Cardinal;
                                    const Msg: String);
    function  CreateRefresh(Sub: TIdSipInboundSubscription;
                            Response: TIdSipResponse;
                            ExpiryTime: Cardinal): TIdSipRequest;
//    procedure ReceiveAuthChallengeWithRetryAfter(Sub: TIdSipRequest;
//                                                 RetryAfter: Cardinal);
    procedure ReceiveRefreshingSubscribe(Sub: TIdSipInboundSubscription;
                                         Response: TIdSipResponse;
                                         ExpiryTime: Cardinal);
    procedure ReceiveSubscribe(const EventPackage: String;
                               ExpiryTime: Cardinal = 0);
    procedure ReceiveSubscribeWithoutExpires(const EventPackage: String);
    procedure ReceiveSubscribeWithExpiresInContact(Duration: Cardinal);
//    procedure RemoveExpiresWait(Timer: TIdDebugTimerQueue);
  protected
    procedure ReceiveSubscribeRequest; override;
  public
    procedure SetUp; override;
  published
    procedure TestExpire;
    procedure TestExpiryTimeInSeconds;
    procedure TestMatchInDialogSubscribe;
    procedure TestMatchResponse;
    procedure TestNotify;
    procedure TestReceiveExpiresInContactHeader;
    procedure TestReceiveExpiresTooShort;
//    procedure TestReceiveOutOfOrderRefresh;
    procedure TestReceiveNoExpires;
    procedure TestReceiveRefreshingSubscribe;
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

  TestTIdSipOutboundSubscription = class(TSubscribeModuleActionTestCase,
                                         IIdSipSubscriptionListener)
  private
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
    SubscriptionNotified:    Boolean;
    UnknownReason:           String;

    procedure CheckExpires(ExpectedRefreshTime: Cardinal);
    procedure CheckNoRetryScheduled(const MsgPrefix: String);
    procedure CheckRetryScheduled(const MsgPrefix: String);
    procedure CheckTerminatedSubscription(Subscription: TIdSipSubscription;
                                          const MsgPrefix: String);
    procedure CheckTerminatedSubscriptionWithNoResubscribe(const Reason: String);
    procedure CheckTerminatedSubscriptionWithResubscribe(const Reason: String);
    function  CreateChallengeResponse: TIdSipResponse;
    function  CreateNotify(Subscribe: TIdSipRequest;
                           Response: TIdSipResponse;
                           const State: String): TIdSipRequest;
    function  CreateSubscription: TIdSipOutboundSubscription;
    function  EstablishSubscription: TIdSipOutboundSubscription; overload;
    procedure EstablishSubscription(Sub: TIdSipOutboundSubscription); overload;

    procedure OnEstablishedSubscription(Subscription: TIdSipOutboundSubscription;
                                        Notify: TIdSipRequest);
    procedure OnExpiredSubscription(Subscription: TIdSipOutboundSubscription;
                                    Notify: TIdSipRequest);
    procedure OnNotify(Subscription: TIdSipOutboundSubscription;
                       Notify: TIdSipRequest);
    procedure ReceiveNotify(Subscribe: TIdSipRequest;
                            Response: TIdSipResponse;
                            const State: String;
                            const Reason: String = '';
                            RetryAfter: Cardinal = 0;
                            Expires: Cardinal = 0);
{
    procedure ReceiveNotifyNoAuth(Subscribe: TIdSipRequest;
                                  Response: TIdSipResponse;
                                  const State: String);
    procedure ReceiveNotifyWrongAuth(Subscribe: TIdSipRequest;
                                     Response: TIdSipResponse;
                                     const State: String);
}
    procedure ReceiveNotifyTerminated(Sub: TIdSipOutboundSubscription);
    procedure ReceiveOkFor(Sub: TIdSipOutboundSubscription;
                           Expires: Cardinal); overload;
    procedure ReceiveOkFor(Subscribe: TIdSipRequest;
                           Expires: Cardinal); overload;
  protected
    function  CreateAction: TIdSipAction; override;
    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipOutboundSubscription); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddListener;
    procedure TestLongRunningSubscription;
    procedure TestMatchNotify;
    procedure TestReceive2xx;
    procedure TestReceive2xxWithNoExpires;
    procedure TestReceiveActiveNotify;
    procedure TestReceiveActiveNotifyWithExpires;
    procedure TestReceiveNotify;
//    procedure TestReceiveNotifyNoAuthorization;
//    procedure TestReceiveNotifyWrongAuthorization;
    procedure TestReceivePendingNotifyWithExpires;
    procedure TestReceiveTerminatingNotifyDeactivated;
    procedure TestReceiveTerminatingNotifyDeactivatedWithRetryAfter;
    procedure TestReceiveTerminatingNotifyGiveUp;
    procedure TestReceiveTerminatingNotifyGiveUpWithRetryAfter;
    procedure TestReceiveTerminatingNotifyNoResource;
    procedure TestReceiveTerminatingNotifyNoResourceWithRetryAfter;
    procedure TestReceiveTerminatingNotifyProbation;
    procedure TestReceiveTerminatingNotifyProbationWithRetryAfter;
    procedure TestReceiveTerminatingNotifyRejected;
    procedure TestReceiveTerminatingNotifyRejectedWithRetryAfter;
    procedure TestReceiveTerminatingNotifyTimeout;
    procedure TestReceiveTerminatingNotifyTimeoutWithRetryAfter;
    procedure TestReceiveTerminatingNotifyWithNoReason;
    procedure TestReceiveTerminatingNotifyWithNoReasonAndRetryAfter;
    procedure TestReceiveTerminatingNotifyWithUnknownReason;
    procedure TestReceiveTerminatingNotifyWithUnknownReasonAndRetryAfter;
    procedure TestReceiveTimeoutNotify;
    procedure TestRemoveListener;
    procedure TestRefresh;
    procedure TestRefreshUpdatesExpiryTime;
    procedure TestRefreshReceives481;
    procedure TestRefreshReceives4xx;
    procedure TestSetEventPackage;
    procedure TestSubscribe;
    procedure TestTerminateBeforeEstablished;
    procedure TestTerminate;
    procedure TestUnsubscribe;
  end;

  TestTIdSipInboundReferral = class(TestTIdSipInboundSubscriptionBase)
  private
    procedure ReceiveRefer(Target: TIdSipAddressHeader);
  protected
    procedure ReceiveSubscribeRequest; override;
  published
    procedure TestRejectUnsupportedReferToUri;
  end;

  TestTIdSipSubscriptionExpires = class(TSubscribeTestCase)
  private
    InSubscription:  TIdSipInboundSubscription;
    OutSubscription: TIdSipOutboundSubscription;
    Block:           TIdSipSubscriptionExpires;

  protected
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipInboundSubscription); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTriggerInboundSubscription;
    procedure TestTriggerOutboundSubscription;
  end;

  TestTIdSipSubscriptionRetryWait = class(TSubscribeTestCase)
  private
    Wait: TIdSipSubscriptionRetryWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
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

  TTestSubscribeMethod = class(TSubscriptionActionMethodTestCase)
  protected
    Listener:  TIdSipTestSubscribeListener;
    Response:  TIdSipResponse;
    Subscribe: TIdSipOutboundSubscribe;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipOutboundSubscribeFailedMethod = class(TTestSubscribeMethod)
  private
    Method: TIdSipOutboundSubscribeFailedMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipOutboundSubscribeSucceededMethod = class(TTestSubscribeMethod)
  private
    Method: TIdSipOutboundSubscribeSucceededMethod;
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
  IdException, IdSipConsts, SysUtils, TestFramework;

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
  Result.AddTest(TestTIdSipOutboundNotify.Suite);
  Result.AddTest(TestTIdSipOutboundSubscribe.Suite);
  Result.AddTest(TestTIdSipOutboundRefreshSubscribe.Suite);
  Result.AddTest(TestTIdSipOutboundUnsubscribe.Suite);
  Result.AddTest(TestTIdSipOutboundRefer.Suite);
  Result.AddTest(TestTIdSipInboundSubscription.Suite);
  Result.AddTest(TestTIdSipOutboundSubscription.Suite);
  Result.AddTest(TestTIdSipInboundReferral.Suite);
  Result.AddTest(TestTIdSipSubscriptionExpires.Suite);
  Result.AddTest(TestTIdSipSubscriptionRetryWait.Suite);
  Result.AddTest(TestTIdSipNotifyFailedMethod.Suite);
  Result.AddTest(TestTIdSipNotifySucceededMethod.Suite);
  Result.AddTest(TestTIdSipOutboundSubscribeFailedMethod.Suite);
  Result.AddTest(TestTIdSipOutboundSubscribeSucceededMethod.Suite);
  Result.AddTest(TestTIdSipEstablishedSubscriptionMethod.Suite);
  Result.AddTest(TestTIdSipExpiredSubscriptionMethod.Suite);
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

procedure TSubscribeTestCase.OnRenewedSubscription(UserAgent: TIdSipAbstractUserAgent;
                                                   Subscription: TIdSipOutboundSubscription);
begin
  Self.OnRenewedSubscriptionFired := true;
  Self.UserAgentParam             := UserAgent;
end;

procedure TSubscribeTestCase.OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                                   Subscription: TIdSipInboundSubscription);
begin
  Self.OnSubscriptionRequestFired := true;
  Self.Subscription               := Subscription;
  Self.UserAgentParam             := UserAgent;
end;

procedure TSubscribeTestCase.ReceiveSubscribe(const EventPackage: String;
                                              ExpiryTime: Cardinal = 0);
var
  Sub: TIdSipRequest;
begin
  Sub := Self.Core.CreateRequest(MethodSubscribe, Self.Destination);
  try
    Sub.FirstEvent.EventPackage   := EventPackage;

    if (ExpiryTime > 0) then
      Sub.FirstExpires.NumericValue := ExpiryTime
    else
      Sub.FirstExpires.NumericValue := Self.Module.DefaultSubscriptionDuration;

    Self.ReceiveRequest(Sub);
  finally
    Sub.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipSubscribeModule                                                  *
//******************************************************************************

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

procedure TestTIdSipSubscribeModule.CheckPackageFound(PackageType: TIdSipEventPackageClass);
begin
  Check(Assigned(Self.Module.Package(PackageType.EventPackage)),
        PackageType.EventPackage + ' package not found');
  CheckEquals(PackageType.ClassName,
              Self.Module.Package(PackageType.EventPackage).ClassName,
              'Wrong package found for package ' + PackageType.EventPackage);
end;

procedure TestTIdSipSubscribeModule.ReceiveNotify(const EventPackage: String);
var
  RemoteDialog: TIdSipDialog;
  Notify:       TIdSipRequest;
  Ok:           TIdSipResponse;
  Sub:          TIdSipRequest;
begin
  Sub := Self.Module.CreateSubscribe(Self.Destination, EventPackage);
  try
    Ok := TIdSipResponse.InResponseTo(Sub, SIPOK, Self.Core.Contact);
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

procedure TestTIdSipSubscribeModule.ReceiveRefer;
var
  Refer: TIdSipRequest;
begin
  Refer := Self.Module.CreateRefer(Self.Destination, Self.Core.Contact);
  try
    Self.ReceiveRequest(Refer);
  finally
    Refer.Free;
  end;
end;

procedure TestTIdSipSubscribeModule.ReceiveReferWithNoReferToHeader;
var
  Refer: TIdSipRequest;
begin
  Refer := Self.Module.CreateRefer(Self.Destination, Self.Core.Contact);
  try
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
  MalformedSub := Self.Module.CreateSubscribe(Self.Destination,
                                              TIdSipTestPackage.EventPackage);
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
    Self.Dispatcher.Transport.SecondLastRequest.ToHeader.Tag := Self.Dispatcher.Transport.LastResponse.ToHeader.Tag;
    Check(Self.Dispatcher.Transport.SecondLastRequest.Equals(L.SubscriptionParam.InitialRequest),
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
              'After adding ' + TIdSipReferPackage.EventPackage + ' package');
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

    Request.FirstEvent.EventPackage := 'x-' + Self.Package.EventPackage;
    Check(nil = Self.Module.PackageFor(Request),
          'Event header with unknown event package');

    Request.FirstEvent.EventPackage := Self.Package.EventPackage;
    Check(Self.Package = Self.Module.PackageFor(Request),
          'Event header with known event package');

    Request.FirstEvent.EventPackage := TIdSipReferPackage.EventPackage;
    Check(nil <> Self.Module.PackageFor(Request),
          'SUBSCRIBE with refer Event header');

    Request.Method := MethodRefer;
    Check(nil = Self.Module.PackageFor(Request),
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

  Self.ReceiveRefer;

  Check(Self.OnSubscriptionRequestFired, 'OnSubscriptionRequest didn''t fire');
  Check(Self.Core = Self.UserAgentParam,
        'UserAgent param of Subscribe''s SubscriptionRequest notification wrong');
  CheckEquals(TIdSipInboundReferral.ClassName,
              Self.Subscription.ClassName,
              'Module didn''t create the correct (inbound) subscription)');
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
              Self.LastSentRequest.FirstEvent.EventPackage,
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
  Options: TIdSipRequest;
  Temp:    String;
begin
  Options := Self.Core.CreateOptions(Self.Destination);
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
  Notify := Self.Core.CreateRequest(MethodInvite, Self.Destination);
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
  Self.Core.InviteModule.Call(Self.Destination, '', '').Send;

  CheckRequestSent('No request sent');

  Invite := Self.LastSentRequest;
  Check(Invite.HasHeader(AllowEventsHeaderFull),
        'No Allow-Events header');
  CheckEquals(Self.Module.AllowedEvents,
              Invite.FirstHeader(AllowEventsHeaderFull).Value,
              'Allow-Events value');
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
//* TestTIdSipOutboundNotify                                                   *
//******************************************************************************

procedure TestTIdSipOutboundNotify.SetUp;
var
  Ok: TIdSipResponse;
begin
  inherited SetUp;

  Self.Expires           := OneHour;
  Self.Body              := 'random data';
  Self.MimeType          := 'text/plain';
  Self.SubscriptionState := SubscriptionSubstateActive;

  Self.Subscribe := Self.Module.CreateSubscribe(Self.Destination,
                                                TIdSipTestPackage.EventPackage);

  Ok := Self.Core.CreateResponse(Self.Subscribe, SIPOK);
  try
    Self.Dialog := TIdSipDialog.CreateInboundDialog(Self.Subscribe, Ok, false);
  finally
    Ok.Free;
  end;

  Self.Notify := Self.CreateAction as TIdSipOutboundNotify;
end;

procedure TestTIdSipOutboundNotify.TearDown;
begin
  Self.Dialog.Free;
  Self.Subscribe.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundNotify Protected methods *********************************

procedure TestTIdSipOutboundNotify.ConfigureNotify(Action: TIdSipOutboundNotify);
begin
  Action.Body              := Self.Body;
  Action.Dialog            := Self.Dialog;
  Action.Expires           := Self.Expires;
  Action.MimeType          := Self.MimeType;
  Action.Subscribe         := Self.Subscribe;
  Action.SubscriptionState := Self.SubscriptionState;
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

//* TestTIdSipOutboundNotify Private methods ***********************************

procedure TestTIdSipOutboundNotify.OnFailure(NotifyAgent: TIdSipOutboundNotify;
                                             Response: TIdSipResponse);
begin
end;

procedure TestTIdSipOutboundNotify.OnSuccess(NotifyAgent: TIdSipOutboundNotify;
                                             Response: TIdSipResponse);
begin
end;

//* TestTIdSipOutboundNotify Published methods *********************************

procedure TestTIdSipOutboundNotify.TestAddListener;
var
  L: TIdSipTestNotifyListener;
begin
  L := TIdSipTestNotifyListener.Create;
  try
    Self.Notify.AddListener(L);
    Self.Notify.Send;
    Self.ReceiveServiceUnavailable(Self.LastSentRequest);

    Check(L.Failed,
          'Notify didn''t notify listener of failure: Listener not added');
  finally
    L.Free;
  end;
end;

procedure TestTIdSipOutboundNotify.TestRemoveListener;
var
  L: TIdSipTestNotifyListener;
begin
  L := TIdSipTestNotifyListener.Create;
  try
    Self.Notify.AddListener(L);
    Self.Notify.RemoveListener(L);
    Self.Notify.Send;
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
  Self.MarkSentRequestCount;
  Self.Notify.Send;

  CheckRequestSent('No request sent');

  Notify := Self.LastSentRequest;
  CheckEquals(MethodNotify,
              Notify.Method,
              'Unexpected request sent');
  Check(Notify.HasHeader(EventHeaderFull),
        'No Event header');
  CheckEquals(Self.Subscribe.FirstEvent.EventPackage,
              Notify.FirstEvent.EventPackage,
              'Event value');
  CheckEquals(Self.Subscribe.FirstEvent.ID,
              Notify.FirstEvent.ID,
              'Event id parameter');
  Check(Notify.HasHeader(SubscriptionStateHeader),
        'No Subscription-State header');
  CheckEquals(SubscriptionSubStateActive,
              Notify.FirstSubscriptionState.SubState,
              'Unexpected substate');
  CheckEquals(Self.Expires,
              Notify.FirstSubscriptionState.Expires,
              'Subscription-State expire param');

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

//******************************************************************************
//* TestTIdSipOutboundSubscribe                                                *
//******************************************************************************
//* TestTIdSipOutboundSubscribe Public methods *********************************

procedure TestTIdSipOutboundSubscribe.SetUp;
begin
  inherited SetUp;

  Self.EventPackage := TIdSipTestPackage.EventPackage;
  Self.Failed       := false;
  Self.ID           := 'id1';
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
  Sub.ID           := Self.ID;
  Sub.AddListener(Self);
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

procedure TestTIdSipOutboundSubscribe.OnFailure(SubscribeAgent: TIdSipOutboundSubscribe;
                                                Response: TIdSipResponse);
begin
  Self.Failed := true;
end;

procedure TestTIdSipOutboundSubscribe.OnSuccess(SubscribeAgent: TIdSipOutboundSubscribe;
                                                Response: TIdSipResponse);
begin
  Self.Succeeded := true;
end;

//* TestTIdSipOutboundSubscribe Published methods ******************************

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
    Notify.AddHeader(EventHeaderFull).Value := Sub.InitialRequest.FirstEvent.FullValue;
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
              'Method of request');
  Check(Sub.InitialRequest.HasHeader(ExpiresHeader),
        'SHOULD have Expires header');
  Check(Sub.InitialRequest.HasHeader(EventHeaderFull),
        'MUST have Event header');
  CheckEquals(Self.EventPackage,
              Sub.InitialRequest.FirstEvent.Value,
              'Wrong Event header');
  CheckEquals(Self.ID,
              Sub.InitialRequest.FirstEvent.ID,
              'ID param of Event header');

  Events := TIdSipHeadersFilter.Create(Sub.InitialRequest.Headers, EventHeaderFull);
  try
    CheckEquals(1, Events.Count, 'Wrong number of Event headers');
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

  Subscribe := Self.Module.CreateSubscribe(Self.Destination, TIdSipTestPackage.EventPackage);
  try
    OK := TIdSipResponse.InResponseTo(Subscribe, SIPOK);
    try
      // We need a Contact for the dialog's remote-target.
      OK.AddHeader(ContactHeaderFull).Value := 'sip:wintermute@localhost';
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

  Refresh.Dialog   := Self.Dialog;
  Refresh.Duration := Self.ExpiresValue;
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

procedure TestTIdSipOutboundRefreshSubscribe.TestSend;
begin
  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No request sent');
  CheckEquals(Self.ExpiresValue,
              Self.LastSentRequest.FirstExpires.NumericValue,
              'Wrong Expires value');
  CheckEquals(Self.Dialog.ID.CallID,
              Self.LastSentRequest.CallID,
              'Call-ID');
  CheckEquals(Self.Dialog.ID.LocalTag,
              Self.LastSentRequest.From.Tag,
              'From tag');
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
  CheckRequestSent('No request sent');
  CheckEquals(0,
              Self.LastSentRequest.FirstExpires.NumericValue,
              'Wrong Expires value');
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
  Self.ReferTo.Assign(Self.Core.Contact);
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

procedure TestTIdSipOutboundRefer.TestSend;
var
  Refer:    TIdSipRequest;
  ReferTos: TIdSipHeadersFilter;
begin
  Self.MarkSentRequestCount;
  Self.CreateAction;

  CheckRequestSent('No request sent');

  Refer := Self.LastSentRequest;
  CheckEquals(MethodRefer,
              Refer.Method,
              'Method');
  Check(Refer.HasHeader(ReferToHeaderFull),
        'No Refer-To header');
  CheckEquals(Self.Core.Contact.Value,
              Refer.FirstHeader(ReferToHeaderFull).Value,
              'Wrong Refer-To value');

  ReferTos := TIdSipHeadersFilter.Create(Refer.Headers, ReferToHeaderFull);
  try
    CheckEquals(1,
                ReferTos.Count,
                'Only one Refer-To allowed: RFC 3515, section 2.4.1'); 
  finally
    ReferTos.Free;
  end;
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
end;

//* TSubscribeModuleActionTestCase Protected methods ***************************

procedure TSubscribeModuleActionTestCase.OnRenewedSubscription(UserAgent: TIdSipAbstractUserAgent;
                                                               Subscription: TIdSipOutboundSubscription);
begin
end;

procedure TSubscribeModuleActionTestCase.OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
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

procedure TestTIdSipInboundSubscriptionBase.CheckSendNotify(Sub: TIdSipInboundSubscription;
                                                            const SubscriptionState: String);
var
  Notify: TIdSipRequest;
begin
  CheckRequestSent('No request sent');

  CheckEquals(SubscriptionState,
              Sub.State,
              'Subscription state');

  Notify := Self.LastSentRequest;
  CheckEquals(MethodNotify,
              Notify.Method,
              'Unexpected request sent');
  CheckEquals(Sub.State,
              Notify.FirstSubscriptionState.SubState,
              'Notify state <> subscription''s state');
end;

procedure TestTIdSipInboundSubscriptionBase.OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                                                  Subscription: TIdSipInboundSubscription);
begin
  Self.Action := Subscription;
end;

procedure TestTIdSipInboundSubscriptionBase.ReceiveSubscribeRequest;
begin
end;

//* TestTIdSipInboundSubscriptionBase Published methods ************************

procedure TestTIdSipInboundSubscriptionBase.TestAccept;
var
  Notify: TIdSipRequest;
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
                                                   Self.Dispatcher.Transport.LastResponse,
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

procedure TestTIdSipInboundSubscriptionBase.TestReceiveRequestSendsNotify;
begin
  Self.MarkSentRequestCount;
  Self.ReceiveSubscribeRequest;

  Self.CheckSendNotify(Self.Action, SubscriptionSubstatePending);
end;

//******************************************************************************
//* TestTIdSipInboundSubscription                                              *
//******************************************************************************
//* TestTIdSipInboundSubscription Public methods *******************************

procedure TestTIdSipInboundSubscription.SetUp;
begin
  Self.ID := 'random-id';

  inherited SetUp;
end;

//* TestTIdSipInboundSubscription Private methods ******************************

procedure TestTIdSipInboundSubscription.CheckExpiresScheduled(ExpectedExpires: Cardinal;
                                                              const Msg: String);
var
  ActualExpires: Cardinal;
begin
  Check(Self.DebugTimer.EventCount > 0,
        Msg + ': No events scheduled');

  // DebugWaitTime's in milliseconds & ExpectedExpires's in seconds
  ActualExpires := Self.DebugTimer.LastEventScheduled.DebugWaitTime div 1000;
  CheckEquals(ExpectedExpires,
              ActualExpires,
              Msg + ': Expires wait time');

  Self.MarkSentRequestCount;
  Self.DebugTimer.TriggerEarliestEvent;
  CheckRequestSent(Msg + ': No request sent for expired subscription');
  CheckEquals(MethodNotify,
              Self.LastSentRequest.Method,
              Msg + ': Unexpected message sent');
  CheckEquals(SubscriptionSubstateTerminated,
              Self.LastSentRequest.FirstSubscriptionState.SubState,
              Msg + ': Subscription-State value');
end;

function TestTIdSipInboundSubscription.CreateRefresh(Sub: TIdSipInboundSubscription;
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
procedure TestTIdSipInboundSubscription.ReceiveRefreshingSubscribe(Sub: TIdSipInboundSubscription;
                                                                   Response: TIdSipResponse;
                                                                   ExpiryTime: Cardinal);
var
  Refresh: TIdSipRequest;
begin
  Refresh := Self.CreateRefresh(Sub, Response, ExpiryTime);
  try
    Refresh.FirstExpires.NumericValue := ExpiryTime;

    Self.ReceiveRequest(Refresh);
  finally
    Refresh.Free;
  end;
end;

procedure TestTIdSipInboundSubscription.ReceiveSubscribeWithoutExpires(const EventPackage: String);
var
  Subscribe: TIdSipRequest;
begin
  Subscribe := Self.Module.CreateSubscribe(Self.Destination, EventPackage);
  try
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
  Sub := Self.Module.CreateSubscribe(Self.Destination, EventPackage);
  try
    Sub.FirstEvent.ID := Self.ID;
    if (ExpiryTime > 0) then
      Sub.FirstExpires.NumericValue := ExpiryTime;

    Self.ReceiveRequest(Sub);
  finally
    Sub.Free;
  end;
end;

procedure TestTIdSipInboundSubscription.ReceiveSubscribeWithExpiresInContact(Duration: Cardinal);
var
  Subscribe: TIdSipRequest;
begin
  Subscribe := Self.Module.CreateSubscribe(Self.Destination, TIdSipTestPackage.EventPackage);
  try
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
              Notify.FirstSubscriptionState.SubState,
              'Unexpected substate');
  CheckEquals(EventReasonTimeout,
              Notify.FirstSubscriptionState.Reason,
              'Unexpected substate reason');
  CheckEquals(SubscriptionSubStateTerminated,
              Self.Action.State,
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
                                Self.Dispatcher.Transport.LastResponse,
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
var
  Notify: TIdSipRequest;
begin
  Self.Action.Accept;

  Self.MarkSentRequestCount;
  Self.Action.Notify(Body, MimeType);

  CheckRequestSent('No request sent');

  Notify := Self.LastSentRequest;
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
                 Response.FirstExpires.NumericValue,
                 'We didn''t ignore the Contact expires parameter');
  CheckEquals(Self.Package.InboundSubscriptionDuration,
              Response.FirstExpires.NumericValue,
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
              Response.FirstMinExpires.NumericValue,
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
              Response.FirstExpires.NumericValue,
              'Wrong Expires value');
end;

procedure TestTIdSipInboundSubscription.TestReceiveRefreshingSubscribe;
const
  ArbExpiresValue = 100;
var
  SubCount: Integer;
begin
  Self.Action.Accept;

  SubCount := Self.Core.CountOf(MethodSubscribe);
  Self.MarkSentRequestCount;
  Self.MarkSentResponseCount;
  Self.ReceiveRefreshingSubscribe(Self.Action,
                                  Self.Dispatcher.Transport.LastResponse,
                                  ArbExpiresValue);

  CheckResponseSent('No response sent');
  CheckNotEquals(SIPAccepted,
                 Self.LastSentResponse.StatusCode,
                 Self.Action.Method + ' started a new subscription: it didn''t match the existing one');
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

procedure TestTIdSipInboundSubscription.TestReceiveRefreshingSubscribeIntervalTooBrief;
var
  Response: TIdSipResponse;
  SubCount: Integer;
begin
  Self.Action.Accept;

  SubCount := Self.Core.CountOf(MethodSubscribe);
  Self.MarkSentRequestCount;

  Self.ReceiveRefreshingSubscribe(Self.Action,
                                  Self.Dispatcher.Transport.LastResponse,
                                  Self.Package.MinimumExpiryTime - 1);

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPIntervalTooBrief,
              Response.StatusCode,
              'Unexpected response sent');
  Check(Response.HasHeader(MinExpiresHeader),
        'No Min-Expires header');
  CheckEquals(Self.Package.MinimumExpiryTime,
              Response.FirstMinExpires.NumericValue,
              'Min-Expires value');
  Check(SubCount >= Self.Core.CountOf(MethodSubscribe),
        'Rejecting a refreshing subscription shouldn''t kill the subscription');
end;

procedure TestTIdSipInboundSubscription.TestReceiveSubscribe;
begin
  CheckEquals(Self.ActionRequest.FirstEvent.EventPackage,
              Self.Action.EventPackage,
              'EventPackage');

  CheckEquals(Self.ActionRequest.FirstEvent.ID,
              Self.Action.ID,
              'ID');

  CheckEquals(SubscriptionSubstatePending,
              Self.Action.State,
              'Action state');

  CheckExpiresScheduled(Self.ActionRequest.FirstExpires.NumericValue,
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
              Self.Action.State,
              'NOTIFY didn''t alter action state');
end;

procedure TestTIdSipInboundSubscription.TestSendNotifyNetworkFailure;
var
  SubCount: Integer;
begin
  Self.Action.Accept;

  SubCount := Self.Core.CountOf(MethodSubscribe);

  Self.Dispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Action.Notify('', '');

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
//* TestTIdSipOutboundSubscription                                             *
//******************************************************************************
//* TestTIdSipOutboundSubscription Public methods ******************************

procedure TestTIdSipOutboundSubscription.SetUp;
begin
  inherited SetUp;

  Self.Password := 'password';

  Self.ChallengeResponse := Self.CreateChallengeResponse;
  Self.ReceivedNotify    := TIdSipRequest.Create;
  Self.RemoteRealmInfo   := TIdRealmInfo.Create;
  Self.RemoteRealmInfo.Username := Self.Core.Username;
  Self.RemoteRealmInfo.Realm    := Self.Core.Realm;

  Self.Authenticator.AddUser(Self.Core.Username,
                             Self.Core.Realm,
                             '');

  // It looks odd that we set this value before the others. If we don't then the
  // Subscribe will refresh after 0 seconds, and the DebugTimer will fire Waits
  // with a zero wait time immediately, which isn't what we want.
  Self.ArbExpiresValue := 22;

  Self.Subscription := Self.EstablishSubscription;

  Self.ArbRetryAfterValue      := 42;
  Self.RenewSubscriptionFired  := false;
  Self.SubscriptionEstablished := false;
  Self.SubscriptionExpired     := false;
  Self.SubscriptionNotified    := false;
  Self.UnknownReason           := 'unknown-reason';
end;

procedure TestTIdSipOutboundSubscription.TearDown;
begin
  Self.RemoteRealmInfo.Free;
  Self.ReceivedNotify.Free;
  Self.ChallengeResponse.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundSubscription Protected methods ***************************

function TestTIdSipOutboundSubscription.CreateAction: TIdSipAction;
begin
  Result := Self.CreateSubscription;
end;

procedure TestTIdSipOutboundSubscription.OnRenewedSubscription(UserAgent: TIdSipAbstractUserAgent;
                                                               Subscription: TIdSipOutboundSubscription);
begin
  inherited OnRenewedSubscription(UserAgent, Subscription);

  Self.RenewSubscriptionFired := true;
end;

//* TestTIdSipOutboundSubscription Private methods *****************************

procedure TestTIdSipOutboundSubscription.CheckExpires(ExpectedRefreshTime: Cardinal);
begin
  Check(Self.DebugTimer.EventCount > 0,
        'No events scheduled at all');

  CheckEquals(ExpectedRefreshTime*1000,
              Self.DebugTimer.LastEventScheduled.DebugWaitTime,
              'Wrong Expiry time');

  Self.MarkSentRequestCount;
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionsWait);
  Self.CheckRequestSent('No request sent, thus no refresh was scheduled');

  Check(not Self.Subscription.Terminating,
        'Subscription can''t terminating, since we''ve just refreshed');
  Check(not Self.Subscription.IsTerminated,
        'Subscription can''t be terminated, since we''ve not received a '
      + 'terminating NOTIFY');
end;

procedure TestTIdSipOutboundSubscription.CheckNoRetryScheduled(const MsgPrefix: String);
begin
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipSubscriptionRetryWait);

  Check(not Self.RenewSubscriptionFired,
        'OnRenewSubscription fired, so a '
       + TIdSipSubscriptionRetryWait.ClassName + ' was scheduled');
end;

procedure TestTIdSipOutboundSubscription.CheckRetryScheduled(const MsgPrefix: String);
begin
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipSubscriptionRetryWait);

  Check(Self.RenewSubscriptionFired,
        'OnRenewSubscription didn''t fire, so no '
       + TIdSipSubscriptionRetryWait.ClassName + ' scheduled');
end;

procedure TestTIdSipOutboundSubscription.CheckTerminatedSubscription(Subscription: TIdSipSubscription;
                                                                     const MsgPrefix: String);
begin
  CheckRequestSent(MsgPrefix + ': No request sent');
  CheckEquals(Subscription.Method,
              Self.LastSentRequest.Method,
              MsgPrefix + ': Unexpected request sent');
  CheckEquals(0,
              Self.LastSentRequest.FirstExpires.NumericValue,
              MsgPrefix + ': Wrong Expires value');
  Check(Subscription.Terminating,
        MsgPrefix + ': Not marked as terminating');
end;

procedure TestTIdSipOutboundSubscription.CheckTerminatedSubscriptionWithNoResubscribe(const Reason: String);
begin
  Check(Self.SubscriptionNotified,
        Reason + ': Subscription didn''t notify listeners of received NOTIFY');

  Check(Self.SubscriptionExpired,
        Reason + ': Subscription didn''t expire');
end;

procedure TestTIdSipOutboundSubscription.CheckTerminatedSubscriptionWithResubscribe(const Reason: String);
begin
  Check(Self.SubscriptionNotified,
        Reason + ': Subscription didn''t notify listeners of received NOTIFY');

  Check(Self.RenewSubscriptionFired,
        Reason + ': Subscription didn''t notify of the new subscription');
end;

function TestTIdSipOutboundSubscription.CreateChallengeResponse: TIdSipResponse;
begin
  Self.Core.RequireAuthentication := true;
  try
    Self.MarkSentResponseCount;
    Self.ReceiveInvite;
    CheckResponseSent('No challenge response sent: '
                    + 'TestTIdSipOutboundSubscription.CreateChallengeResponse');

    Result := TIdSipResponse.Create;
    Result.Assign(Self.LastSentResponse);
  finally
    Self.Core.RequireAuthentication := false;
  end;
end;

function TestTIdSipOutboundSubscription.CreateNotify(Subscribe: TIdSipRequest;
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

function TestTIdSipOutboundSubscription.CreateSubscription: TIdSipOutboundSubscription;
begin
  Result := Self.Module.Subscribe(Self.Destination,
                                  TIdSipTestPackage.EventPackage) as TIdSipOutboundSubscription;
  Result.AddListener(Self);
  Result.Send;
end;

function TestTIdSipOutboundSubscription.EstablishSubscription: TIdSipOutboundSubscription;
begin
  Result := Self.CreateSubscription;
  Self.EstablishSubscription(Result);
end;

procedure TestTIdSipOutboundSubscription.EstablishSubscription(Sub: TIdSipOutboundSubscription);
begin
  Self.ReceiveOkFor(Sub, Self.ArbExpiresValue);
  Self.ReceiveNotify(Sub.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateActive);
end;

procedure TestTIdSipOutboundSubscription.OnEstablishedSubscription(Subscription: TIdSipOutboundSubscription;
                                                                   Notify: TIdSipRequest);
begin
  Self.SubscriptionEstablished := true;
end;

procedure TestTIdSipOutboundSubscription.OnExpiredSubscription(Subscription: TIdSipOutboundSubscription;
                                                               Notify: TIdSipRequest);
begin
  Self.SubscriptionExpired := true;
end;

procedure TestTIdSipOutboundSubscription.OnNotify(Subscription: TIdSipOutboundSubscription;
                                                  Notify: TIdSipRequest);
begin
  Self.SubscriptionNotified := true;
  Self.ReceivedNotify.Assign(Notify);
end;

procedure TestTIdSipOutboundSubscription.ReceiveNotify(Subscribe: TIdSipRequest;
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
      Notify.FirstSubscriptionState.Reason := Reason;

    if (Expires > 0) then
      Notify.FirstSubscriptionState.Expires := Expires;

    if (RetryAfter > 0) then
      Notify.FirstSubscriptionState.RetryAfter := RetryAfter;

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
procedure TestTIdSipOutboundSubscription.ReceiveNotifyTerminated(Sub: TIdSipOutboundSubscription);
begin
  Self.ReceiveNotify(Sub.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonRejected);
end;

procedure TestTIdSipOutboundSubscription.ReceiveOkFor(Sub: TIdSipOutboundSubscription;
                                                      Expires: Cardinal);
begin
  Self.ReceiveOkFor(Sub.InitialRequest, Expires);
end;

procedure TestTIdSipOutboundSubscription.ReceiveOkFor(Subscribe: TIdSipRequest;
                                                      Expires: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Subscribe, SIPOK);
  try
    Response.FirstContact.Assign(Subscribe.ToHeader);
    Response.FirstExpires.NumericValue := Expires;

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

//* TestTIdSipOutboundSubscription Published methods ***************************

procedure TestTIdSipOutboundSubscription.TestAddListener;
var
  Listener: TIdSipTestSubscriptionListener;
begin
  Listener := TIdSipTestSubscriptionListener.Create;
  try
    Self.Subscription.AddListener(Listener);
    Self.ReceiveNotifyTerminated(Self.Subscription);

    Check(Listener.ExpiredSubscription,
          'Test case not notified of terminated subscription; '
        + 'thus, not added as listener');
  finally
    Listener.Free;
  end;
end;

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
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionsWait);
  Check(not Self.Subscription.IsTerminated,
        'Subscription terminated after the first re-SUBSCRIBE');
  CheckRequestSent('No request sent: no re-SUBSCRIBE');

  // Receive a 200 OK for the re-SUBSCRIBE
  Self.ReceiveOkFor(Self.LastSentRequest,
                    TIdSipTestPackage.DefaultSubscriptionDuration);

  Self.MarkSentRequestCount;
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionsWait);
  Check(not Self.Subscription.IsTerminated,
        'Subscription terminated after the second re-SUBSCRIBE');
  CheckRequestSent('No request sent: no second re-SUBSCRIBE');
end;

procedure TestTIdSipOutboundSubscription.TestMatchNotify;
var
  Notify: TIdSipRequest;
  Sub:    TIdSipOutboundSubscription;
begin
  Sub := Self.CreateSubscription;

  Notify := TIdSipRequest.Create;
  try
    Notify.Method := MethodNotify;

    Check(not Sub.Match(Notify), 'Only matching request method');

    Notify.CallID := Sub.InitialRequest.CallID;
    Check(not Sub.Match(Notify), 'Only matching request method, Call-ID');

    Notify.AddHeader(EventHeaderFull).Value := Sub.InitialRequest.FirstEvent.FullValue;
    Check(not Sub.Match(Notify), 'Only matching request method, Call-ID, Event');

    Notify.ToHeader.Tag := Sub.InitialRequest.From.Tag;
    Check(Sub.Match(Notify), 'Matching request method, Call-ID, Event, From-tag-and-To-tag');

    Notify.Method := MethodInvite;
    Check(not Sub.Match(Notify), 'Matches everything except method');
  finally
    Notify.Free;
  end;
end;

procedure TestTIdSipOutboundSubscription.TestReceive2xx;
const
  ExpireTime = 1000;
begin
  Self.CreateSubscription;
  Self.ReceiveOkFor(Self.LastSentRequest, ExpireTime);

  Check(not Self.SubscriptionEstablished,
        'Subscriptions are only established when we receive a NOTIFY saying so');

  Check(Self.DebugTimer.EventCount > 0,
        'No refresh scheduled');

  CheckEquals(ExpireTime*1000,
              Self.DebugTimer.LastEventScheduled.DebugWaitTime,
              'Refresh''s scheduled time wrong');
end;

procedure TestTIdSipOutboundSubscription.TestReceive2xxWithNoExpires;
var
  Sub: TIdSipOutboundSubscription;
begin
  Sub := Self.CreateSubscription;
  Self.ReceiveResponse(SIPAccepted);

  Check(Self.DebugTimer.EventCount > 0,
        'No refresh scheduled');

  CheckEquals(Sub.Duration*1000,
              Self.DebugTimer.LastEventScheduled.DebugWaitTime,
              'Refresh''s scheduled time wrong');
end;

procedure TestTIdSipOutboundSubscription.TestReceiveActiveNotify;
var
  Sub: TIdSipOutboundSubscription;
begin
  Sub := Self.CreateSubscription;
  Self.ReceiveOk(Sub.InitialRequest);

  Check(not Self.SubscriptionEstablished,
        'No subscription''s established until a NOTIFY says so');

  Self.ReceiveNotify(Sub.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateActive);

  Check(Self.SubscriptionEstablished,
        'Subscription didn''t notify listeners of established subscription');
end;

procedure TestTIdSipOutboundSubscription.TestReceiveActiveNotifyWithExpires;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateActive,
                     '',
                     0,
                     Self.ArbExpiresValue);

  Self.CheckExpires(Self.ArbExpiresValue);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveNotify;
begin
  Self.MarkSentResponseCount;

  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateActive);

  Check(Self.SubscriptionNotified,
        'Subscription didn''t notify listeners of received NOTIFY');
  Check(Self.ReceivedNotify.Equals(Self.Dispatcher.Transport.LastRequest),
        'Wrong NOTIFY in the notification');

  CheckResponseSent('No response to the NOTIFY sent');
  CheckEquals(SIPOK,
              Self.LastSentResponse.StatusCode,
              'Unexpected response');
end;
{
procedure TestTIdSipOutboundSubscription.TestReceiveNotifyNoAuthorization;
begin
  Fail('Authorization implementation deferred');
  Self.MarkSentResponseCount;

  Self.ReceiveNotifyNoAuth(Self.Subscription.InitialRequest,
                           Self.Dispatcher.Transport.LastResponse,
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
                             Self.Dispatcher.Transport.LastResponse,
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
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstatePending,
                     '',
                     0,
                     Self.ArbExpiresValue);

  Self.CheckExpires(Self.ArbExpiresValue);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyDeactivated;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonDeactivated);

  Self.CheckTerminatedSubscriptionWithResubscribe(EventReasonDeactivated);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyDeactivatedWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonDeactivated,
                     Self.ArbRetryAfterValue);

  Self.CheckNoRetryScheduled(EventReasonDeactivated);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyGiveUp;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonGiveUp);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonGiveUp);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyGiveUpWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonGiveUp);

  Self.CheckRetryScheduled(EventReasonGiveUp);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyNoResource;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonNoResource);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonNoResource);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyNoResourceWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonNoResource);

  Self.CheckNoRetryScheduled(EventReasonNoResource);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyProbation;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonProbation);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonProbation);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyProbationWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonProbation,
                     Self.ArbRetryAfterValue);

  Self.CheckRetryScheduled(EventReasonProbation);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyRejected;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonRejected);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonRejected);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyRejectedWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonRejected);

  Self.CheckNoRetryScheduled(EventReasonRejected);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyTimeout;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonTimeout);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonTimeout);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyTimeoutWithRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonTimeout,
                     Self.ArbRetryAfterValue);

  Self.CheckNoRetryScheduled(EventReasonTimeout);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyWithNoReason;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated);

  Self.CheckTerminatedSubscriptionWithNoResubscribe('no reason');
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyWithNoReasonAndRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     '',
                     Self.ArbRetryAfterValue);

  Self.CheckRetryScheduled('no reason');
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyWithUnknownReason;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     Self.UnknownReason);

  Self.CheckTerminatedSubscriptionWithResubscribe(UnknownReason);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyWithUnknownReasonAndRetryAfter;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     Self.UnknownReason,
                     Self.ArbRetryAfterValue);

  Self.CheckRetryScheduled(Self.UnknownReason);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTimeoutNotify;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonTimeout);

  Self.CheckTerminatedSubscriptionWithResubscribe(EventReasonTimeout);
end;

procedure TestTIdSipOutboundSubscription.TestRemoveListener;
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
    Check(not Listener.ExpiredSubscription,
          'Test case notified of failure; thus, not removed as listener');
  finally
    Sub.RemoveListener(Listener);
    Listener.Free;
  end;
end;

procedure TestTIdSipOutboundSubscription.TestRefresh;
begin
  //  --- SUBSCRIBE --->
  // <---  200 OK   ---
  //    <time passes>
  //  --- SUBSCRIBE --->
  // <---  200 OK   ---

  Self.MarkSentRequestCount;

  Self.Subscription.Refresh(1000);

  CheckRequestSent('No request sent');
  CheckEquals(Self.Subscription.Method,
              Self.LastSentRequest.Method,
              'Unexpected request sent');
  CheckEquals(Self.Subscription.EventPackage,
              Self.LastSentRequest.FirstEvent.Value,
              'Wrong Event header');
  CheckEquals(Self.Subscription.ID,
              Self.LastSentRequest.FirstEvent.ID,
              'Wrong event ID');
  CheckEquals(Self.Subscription.InitialRequest.CallID,
              Self.LastSentRequest.CallID,
              'Refresh must happen in the context of the original dialog: Call-ID');
  CheckEquals(Self.Subscription.InitialRequest.From.Tag,
              Self.LastSentRequest.From.Tag,
              'Refresh must happen in the context of the original dialog: From tag');
  CheckEquals(Self.Subscription.InitialRequest.ToHeader.Tag,
              Self.LastSentRequest.ToHeader.Tag,
              'Refresh must happen in the context of the original dialog: To tag');
end;

procedure TestTIdSipOutboundSubscription.TestRefreshUpdatesExpiryTime;


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

procedure TestTIdSipOutboundSubscription.TestRefreshReceives481;
begin
  //  ---                SUBSCRIBE                --->
  // <---                  200 OK                 ---
  //                   <time passes>
  //  ---                SUBSCRIBE                --->
  // <--- 481 Call Leg/Transaction Does Not Exist ---

  Self.Subscription.Refresh(1000);
  Self.ReceiveResponse(SIPCallLegOrTransactionDoesNotExist);

  Check(Self.SubscriptionExpired,
        'Subscription didn''t expire (or didn''t notify us)');
end;

procedure TestTIdSipOutboundSubscription.TestRefreshReceives4xx;
begin
  //  ---      SUBSCRIBE      --->
  // <---       200 OK        ---
  //          <time passes>
  //  ---      SUBSCRIBE      --->
  // <--- 408 Request Timeout --->

  Self.Subscription.Refresh(1000);
  Self.ReceiveResponse(SIPRequestTimeout);
  Check(not Self.SubscriptionExpired,
        'Subscription mustn''t expire, but still exist until its Duration runs out');
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

procedure TestTIdSipOutboundSubscription.TestTerminateBeforeEstablished;
var
  Sub: TIdSipOutboundSubscription;
begin
  Sub := Self.CreateSubscription;

  Self.MarkSentRequestCount;
  Sub.Terminate;
  Self.CheckTerminatedSubscription(Sub, 'Terminate before established');
end;

procedure TestTIdSipOutboundSubscription.TestTerminate;
begin
  Self.MarkSentRequestCount;
  Self.Subscription.Terminate;
  Self.CheckTerminatedSubscription(Self.Subscription, 'Terminate');
end;

procedure TestTIdSipOutboundSubscription.TestUnsubscribe;
begin
  Self.MarkSentRequestCount;
  Self.Subscription.Unsubscribe;
  Self.CheckTerminatedSubscription(Self.Subscription, 'Unsubscribe');
end;

//******************************************************************************
//* TestTIdSipInboundReferral                                                  *
//******************************************************************************
//* TestTIdSipInboundReferral Protected methods ********************************

procedure TestTIdSipInboundReferral.ReceiveSubscribeRequest;
begin
  // cf. TestTIdSipInboundSubscription.ReceiveSubscribeRequest.
  Self.Module.AddPackage(TIdSipReferPackage);
  Self.ReceiveRefer(Self.Core.Contact);
end;

//* TestTIdSipInboundReferral Private methods **********************************

procedure TestTIdSipInboundReferral.ReceiveRefer(Target: TIdSipAddressHeader);
var
  Refer: TIdSipRequest;
begin
  Refer := Self.Module.CreateRefer(Self.Destination, Target);
  try
    Self.ReceiveRequest(Refer);
  finally
    Refer.Free;
  end;
end;

//* TestTIdSipInboundReferral Published methods ********************************

procedure TestTIdSipInboundReferral.TestRejectUnsupportedReferToUri;
var
  HttpReferTo: TIdSipAddressHeader;
begin
  HttpReferTo := TIdSipFromHeader.Create;
  try
    HttpReferTo.Address.Uri := 'http://www.example.com/';

    Self.MarkSentResponseCount;
    Self.ReceiveRefer(HttpReferTo);
    CheckResponseSent('No request sent');
    CheckEquals(SIPBadRequest,
                Self.LastSentResponse.StatusCode         ,
                'Unexpected response');
  finally
    HttpReferTo.Free;
  end;
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

procedure TestTIdSipSubscriptionExpires.OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
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
//* TestTIdSipSubscriptionRetryWait                                            *
//******************************************************************************
//* TestTIdSipSubscriptionRetryWait Public methods *****************************

procedure TestTIdSipSubscriptionRetryWait.SetUp;
begin
  inherited SetUp;

  Self.Wait := TIdSipSubscriptionRetryWait.Create;
  Self.Wait.EventPackage := TIdSipTestPackage.EventPackage;
  Self.Wait.Target.Value := 'sip:foo@bar';
  Self.Wait.UserAgent := Self.Core;
end;

procedure TestTIdSipSubscriptionRetryWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdSipSubscriptionRetryWait Published methods **************************

procedure TestTIdSipSubscriptionRetryWait.TestTrigger;
begin
  Self.Wait.Trigger;

  Check(Self.OnRenewedSubscriptionFired,
        'OnRenewedSubscription didn''t fire');
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
//* TTestSubscribeMethod                                                       *
//******************************************************************************
//* TTestSubscribeMethod Public methods ****************************************

procedure TTestSubscribeMethod.SetUp;
begin
  inherited SetUp;

  Self.Listener  := TIdSipTestSubscribeListener.Create;
  Self.Response  := TIdSipResponse.Create;
  Self.Subscribe := TIdSipOutboundSubscribe.Create(Self.UA);
end;

procedure TTestSubscribeMethod.TearDown;
begin
  Self.Subscribe.Free;
  Self.Response.Free;
  Self.Listener.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipOutboundSubscribeFailedMethod                                    *
//******************************************************************************
//* TestTIdSipOutboundSubscribeFailedMethod Public methods *********************

procedure TestTIdSipOutboundSubscribeFailedMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipOutboundSubscribeFailedMethod.Create;
  Self.Method.Response  := Self.Response;
  Self.Method.Subscribe := Self.Subscribe;
end;

procedure TestTIdSipOutboundSubscribeFailedMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundSubscribeFailedMethod Published methods ******************

procedure TestTIdSipOutboundSubscribeFailedMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.Failed, 'Listener not notified of failure');
  Check(Self.Response = Self.Listener.ResponseParam,
        'Response param');
  Check(Self.Subscribe = Self.Listener.SubscribeAgentParam,
        'SubscribeAgent param');
end;

//******************************************************************************
//* TestTIdSipOutboundSubscribeSucceededMethod                                 *
//******************************************************************************
//* TestTIdSipOutboundSubscribeSucceededMethod Public methods ******************

procedure TestTIdSipOutboundSubscribeSucceededMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipOutboundSubscribeSucceededMethod.Create;
  Self.Method.Subscribe := Self.Subscribe;
end;

procedure TestTIdSipOutboundSubscribeSucceededMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundSubscribeSucceededMethod Published methods ***************

procedure TestTIdSipOutboundSubscribeSucceededMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.Succeeded, 'Listener not notified of Succeedure');
  Check(Self.Subscribe = Self.Listener.SubscribeAgentParam,
        'SubscribeAgent param');
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
  Self.Request.FirstEvent.EventPackage := TIdSipTestPackage.EventPackage;

  Self.Module.AddPackage(TIdSipTestPackage);

  Self.Dispatcher.MockLocator.AddA(Self.Request.LastHop.SentBy, '127.0.0.1');
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

  Self.Subscription := TIdSipInboundSubscription.CreateInbound(Self.UA, Self.Request, false);
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
