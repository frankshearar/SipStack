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
  IdSipAuthentication, IdSipCore, IdSipMessage, IdSipSubscribeModule,
  TestFrameworkSip, TestFrameworkSipTU;

type
  TSubscribeTestCase = class(TTestCaseTU,
                             IIdSipSubscribeModuleListener)
  private
  protected
    Module:                     TIdSipSubscribeModule;
    OnRenewedSubscriptionFired: Boolean;
    OnSubscriptionRequestFired: Boolean;
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
    procedure CheckPackageFound(PackageType: TIdSipEventPackageClass);
  published
    procedure TestAddListener;
    procedure TestAddPackage;
    procedure TestPackage;
    procedure TestRejectUnknownEventSubscriptionRequest;
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

  TestTIdSipOutboundSubscribe = class(TestTIdSipSubscribe,
                                      IIdSipSubscribeListener)
  private
    EventPackage: String;
    Failed:       Boolean;
    ID:           String;
    Succeeded:    Boolean;

    function  CreateSubscribe: TIdSipOutboundSubscribe;
    procedure OnFailure(SubscribeAgent: TIdSipOutboundSubscribe;
                        Response: TIdSipResponse);
    procedure OnSuccess(SubscribeAgent: TIdSipOutboundSubscribe;
                        Response: TIdSipResponse);
  protected
    procedure ConfigureSubscribe(Action: TIdSipOutboundSubscribe);
    function  CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
  published
    procedure TestMatchNotify;
    procedure TestMatchResponse;
    procedure TestReceive2xx;
    procedure TestReceiveFailure;
    procedure TestSubscribeRequest;
  end;

  TestTIdSipOutboundUnsubscribe = class(TestTIdSipOutboundSubscribe)
  private
    function CreateUnsubscribe: TIdSipOutboundUnsubscribe;
  protected
    function CreateAction: TIdSipAction; override;
  published
    procedure TestSend;
  end;

  TSubscribeModuleActionTestCase = class(TestTIdSipAction,
                                         IIdSipSubscribeModuleListener)
  protected
    Module: TIdSipSubscribeModule;

    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipOutboundSubscription); virtual;
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipInboundSubscription); virtual;
  public
    procedure SetUp; override;
  end;

  TestTIdSipInboundSubscription = class(TSubscribeModuleActionTestCase)
  private
    SubscribeAction:  TIdSipInboundSubscription;
    SubscribeRequest: TIdSipRequest;

    procedure CheckExpiresScheduled(ExpectedExpires: Cardinal;
                                    const Msg: String);
    procedure ReceiveSubscribe(const EventPackage: String;
                               ExpiryTime: Cardinal = 0);
    procedure ReceiveSubscribeWithoutExpires(const EventPackage: String);
    procedure ReceiveSubscribeWithExpiresInContact(Duration: Cardinal);
  protected
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipInboundSubscription); override;
  public
    procedure SetUp; override;
  published
    procedure TestAccept;
    procedure TestIsInbound; override;
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
    procedure TestReceiveExpiresTooShort;
    procedure TestReceiveNoExpires;
    procedure TestReceiveSubscribe;
    procedure TestReceiveSubscribeReturnsAccepted;
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
    procedure ReceiveNotifyNoAuth(Subscribe: TIdSipRequest;
                                  Response: TIdSipResponse;
                                  const State: String);
    procedure ReceiveNotifyWrongAuth(Subscribe: TIdSipRequest;
                                     Response: TIdSipResponse;
                                     const State: String);
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

  TTestNotifyMethod = class(TActionMethodTestCase)
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

  TTestSubscribeMethod = class(TActionMethodTestCase)
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

  TestTIdSipOutboundSubscriptionMethod = class(TActionMethodTestCase)
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

  TSubscribeModuleTestCase = class(TActionMethodTestCase)
  protected
    Listener: TIdSipTestSubscribeModuleListener;
    Module:   TIdSipSubscribeModule;
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
  IdSipDialog, IdTimerQueue, SysUtils, TestFramework;

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
  Result.AddTest(TestTIdSipOutboundSubscribe.Suite);
  Result.AddTest(TestTIdSipOutboundUnsubscribe.Suite);
  Result.AddTest(TestTIdSipInboundSubscription.Suite);
  Result.AddTest(TestTIdSipOutboundSubscription.Suite);
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

  Self.Module := Self.Core.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  Self.Module.AddListener(Self);
  Self.Module.AddPackage(TIdSipTestPackage);

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
  Self.UserAgentParam             := UserAgent;
end;

procedure TSubscribeTestCase.ReceiveSubscribe(const EventPackage: String;
                                              ExpiryTime: Cardinal = 0);
var
  Sub: TIdSipRequest;
begin
  Sub := Self.Module.CreateSubscribe(Self.Destination, EventPackage);
  try
    if (ExpiryTime > 0) then
      Sub.FirstExpires.NumericValue := ExpiryTime;

    Self.ReceiveRequest(Sub);
  finally
    Sub.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipSubscribeModule                                                  *
//******************************************************************************

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

//* TestTIdSipSubscribeModule Published methods ********************************

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
    Check(Self.Dispatcher.Transport.LastRequest.Equals(L.SubscriptionParam.InitialRequest),
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

procedure TestTIdSipSubscribeModule.TestRejectUnknownEventSubscriptionRequest;
begin
  Self.MarkSentResponseCount;

  Self.ReceiveSubscribe('Foo.bar');

  CheckResponseSent('No response sent');
  CheckEquals(SIPBadEvent,
              Self.LastSentResponse.StatusCode,
              'Unexpected response');
  CheckHasHeader(Self.LastSentResponse, AllowEventsHeaderFull);
  CheckEquals(Self.Module.AllowedEvents,
              Self.LastSentResponse.FirstHeader(AllowEventsHeaderFull).Value,
              'Wrong Allow-Events value');

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
  Self.Core.Call(Self.Destination, '', '').Send;

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

procedure TestTIdSipOutboundSubscribe.ConfigureSubscribe(Action: TIdSipOutboundSubscribe);
begin
  Action.Target       := Self.Destination;
  Action.EventPackage := Self.EventPackage;
  Action.ID           := Self.ID;
  Action.AddListener(Self);
end;

function TestTIdSipOutboundSubscribe.CreateAction: TIdSipAction;
begin
  Result := Self.CreateSubscribe;
end;

//* TestTIdSipOutboundSubscribe Private methods ********************************

function TestTIdSipOutboundSubscribe.CreateSubscribe: TIdSipOutboundSubscribe;
begin
  Result := Self.Core.AddOutboundAction(TIdSipOutboundSubscribe) as TIdSipOutboundSubscribe;
  Self.ConfigureSubscribe(Result);
  Result.Send;
end;

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
  Sub:    TIdSipOutboundSubscribe;
begin
  Sub := Self.CreateSubscribe;

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
  Sub: TIdSipOutboundSubscribe;
begin
  Sub := Self.CreateSubscribe;

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

procedure TestTIdSipOutboundSubscribe.TestSubscribeRequest;
var
  Events: TIdSipHeadersFilter;
  Sub: TIdSipOutboundSubscribe;
begin
  Sub := Self.CreateSubscribe;
  CheckEquals(MethodSubscribe,
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
//* TestTIdSipOutboundUnsubscribe                                              *
//******************************************************************************
//* TestTIdSipOutboundUnsubscribe Protected methods ****************************

function TestTIdSipOutboundUnsubscribe.CreateAction: TIdSipAction;
begin
  Result := Self.CreateUnsubscribe;
end;

//* TestTIdSipOutboundUnsubscribe Private methods ******************************

function TestTIdSipOutboundUnsubscribe.CreateUnsubscribe: TIdSipOutboundUnsubscribe;
begin
  Result := Self.Core.AddOutboundAction(TIdSipOutboundUnsubscribe) as TIdSipOutboundUnsubscribe;
  Self.ConfigureSubscribe(Result);
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
//* TSubscribeModuleActionTestCase                                             *
//******************************************************************************
//* TSubscribeModuleActionTestCase Public methods ******************************

procedure TSubscribeModuleActionTestCase.SetUp;
begin
  inherited SetUp;

  Self.Module := Self.Core.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  Self.Module.AddListener(Self);

  Self.Module.AddPackage(TIdSipTestPackage);
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
//* TestTIdSipInboundSubscription                                              *
//******************************************************************************
//* TestTIdSipInboundSubscription Public methods *******************************

procedure TestTIdSipInboundSubscription.SetUp;
begin
  inherited SetUp;

  Self.ReceiveSubscribe(TIdSipTestPackage.EventPackage);

  Check(Assigned(Self.SubscribeAction),
        'No SUBSCRIBE received');

  Self.SubscribeRequest := Self.SubscribeAction.InitialRequest;
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
  Subscribe := Self.Module.CreateSubscribe(Self.Destination, 'Foo');
  try
    Subscribe.RemoveAllHeadersNamed(ExpiresHeader);
    Subscribe.FirstContact.Expires := Duration;

    Self.ReceiveRequest(Subscribe);
  finally
    Subscribe.Free;
  end;
end;

//* TestTIdSipInboundSubscription Protected methods ****************************

procedure TestTIdSipInboundSubscription.OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                                              Subscription: TIdSipInboundSubscription);
begin
  Self.SubscribeAction := Subscription;
end;

//* TestTIdSipInboundSubscription Published methods ****************************

procedure TestTIdSipInboundSubscription.TestAccept;
var
  Notify: TIdSipRequest;
begin
  Self.MarkSentRequestCount;

  Self.SubscribeAction.Accept;

  CheckRequestSent('No request sent');

  Notify := Self.LastSentRequest;
  CheckEquals(MethodNotify,
              Notify.Method,
              'Unexpected request sent');
  Check(Notify.HasHeader(SubscriptionStateHeader),
        'No Subscription-State header');
  CheckEquals(SubscriptionSubStateActive,
              Notify.FirstSubscriptionState.SubState,
              'Unexpected substate');
  CheckEquals(SubscriptionSubStateActive,
              Self.SubscribeAction.State,
              'Subscription state');
end;

procedure TestTIdSipInboundSubscription.TestIsInbound;
begin
  Check(Self.SubscribeAction.IsInbound,
        Self.SubscribeAction.ClassName + ' not marked as inbound');
end;

procedure TestTIdSipInboundSubscription.TestIsInvite;
begin
  Check(not Self.SubscribeAction.IsInvite,
        Self.SubscribeAction.ClassName + ' marked as an Invite');
end;

procedure TestTIdSipInboundSubscription.TestIsOptions;
begin
  Check(not Self.SubscribeAction.IsOptions,
        Self.SubscribeAction.ClassName + ' marked as an Options');
end;

procedure TestTIdSipInboundSubscription.TestIsRegistration;
begin
  Check(not Self.SubscribeAction.IsRegistration,
        Self.SubscribeAction.ClassName + ' marked as a Registration');
end;

procedure TestTIdSipInboundSubscription.TestIsSession;
begin
  Check(not Self.SubscribeAction.IsSession,
        Self.SubscribeAction.ClassName + ' marked as a Session');
end;

procedure TestTIdSipInboundSubscription.TestReceiveExpiresTooShort;
const
  MinExpTime = 42;
var
  Response: TIdSipResponse;
begin
  Self.Module.MinimumExpiryTime := MinExpTime;

  Self.MarkSentResponseCount;
  Self.ReceiveSubscribe(TIdSipTestPackage.EventPackage, MinExpTime - 1);

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPIntervalTooBrief,
              Response.StatusCode,
              'Unexpected response sent');
  Check(Response.HasHeader(MinExpiresHeader),
        'No Min-Expires header');
  CheckEquals(Self.Module.MinimumExpiryTime,
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

procedure TestTIdSipInboundSubscription.TestReceiveSubscribe;
begin
  CheckEquals(Self.SubscribeRequest.FirstEvent.EventPackage,
              Self.SubscribeAction.EventPackage,
              'EventPackage');

  CheckEquals(Self.SubscribeRequest.FirstEvent.ID,
              Self.SubscribeAction.ID,
              'ID');

  CheckExpiresScheduled(Self.SubscribeRequest.FirstExpires.NumericValue,
                        'Subscription won''t expire');
end;

procedure TestTIdSipInboundSubscription.TestReceiveSubscribeReturnsAccepted;
begin
  Self.MarkSentResponseCount;

  Self.ReceiveSubscribe(TIdSipTestPackage.EventPackage);

  CheckResponseSent('No response sent');
  CheckEquals(SIPAccepted,
              Self.LastSentResponse.StatusCode,
              'Unexpected response sent');
end;

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
begin
  Self.CreateSubscription;
  Self.ReceiveResponse(SIPAccepted);

  Check(not Self.SubscriptionEstablished,
        'Subscriptions are only established when we receive a NOTIFY saying so');
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
  CheckNotEquals(Self.Subscription.InitialRequest.CallID,
                 Self.LastSentRequest.CallID,
                 'Refresh must use a freshly-generated Call-ID');
  CheckNotEquals(Self.Subscription.InitialRequest.From.Tag,
                 Self.LastSentRequest.From.Tag,
                 'Refresh must use a freshly-generated From tag');
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
//* TestTIdSipSubscriptionExpires                                              *
//******************************************************************************
//* TestTIdSipSubscriptionExpires Public methods *******************************

procedure TestTIdSipSubscriptionExpires.SetUp;
begin
  inherited SetUp;

  Self.OutSubscription := Self.Module.Subscribe(Self.Destination,
                                                TIdSipTestPackage.EventPackage);

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
  Self.Request  := TIdSipTestResources.CreateBasicRequest;
  Self.Module   := Self.UA.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
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

  Self.Subscription := TIdSipInboundSubscription.Create(Self.UA, Self.Request, false);
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
