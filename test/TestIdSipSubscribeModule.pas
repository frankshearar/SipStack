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
  IdSipCore, IdSipMessage, IdSipSubscribeModule, TestFrameworkSip,
  TestFrameworkSipTU;

type
  TestTIdSipSubscribeModule = class(TTestCaseTU,
                                    IIdSipSubscribeModuleListener)
  private
    Module:                     TIdSipSubscribeModule;
    OnRenewedSubscriptionFired: Boolean;
    OnSubscriptionRequestFired: Boolean;
    UserAgentParam:             TIdSipAbstractUserAgent;

    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipOutboundSubscription);
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipInboundSubscription);
  public
    procedure SetUp; override;
  published
    procedure TestAddListener;
    procedure TestAddPackage;
    procedure TestRejectUnknownEventSubscriptionRequest;
    procedure TestRemoveListener;
    procedure TestSubscribe;
    procedure TestSubscriptionRequest;
  end;

  TestTIdSipUserAgentWithSubscribeModule = class(TTestCaseTU)
  private
    procedure ReceiveOptions;
  published
    procedure TestReceiveNotifyForUnmatchedDialog;
    procedure TestReceiveOptions;
    procedure TestSendInvite;
  end;

  TestTIdSipInboundSubscribe = class(TestTIdSipAction)
  private
    Subscription:         TIdSipInboundSubscribe;
    SubscriptionDuration: Cardinal;
    SubscribeRequest:     TIdSipRequest;

    procedure CheckDuration(AcceptedDuration: Cardinal;
                            ExpectedDuration: Cardinal);
    procedure ReceiveSubscribeWithExpiresInContact(Duration: Cardinal);
    procedure ReceiveSubscribeWithoutExpires;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAccept;
    procedure TestAcceptWithExpiresInRequestContact;
    procedure TestAcceptWithMaximalDuration;
    procedure TestAcceptWithNoExpiresInRequest;
    procedure TestIsInbound; override;
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsRegistration; override;
    procedure TestIsUnsubscribe;
    procedure TestIsSession; override;
  end;

  TestTIdSipOutboundSubscribe = class(TestTIdSipAction,
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
    function CreateAction: TIdSipAction; override;
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

  TestTIdSipInboundSubscription = class(TestTIdSipAction)
  private
    SubscribeAction:  TIdSipInboundSubscription;
    SubscribeRequest: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsInbound; override;
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
  end;

  TestTIdSipOutboundSubscription = class(TestTIdSipAction,
                                         IIdSipSubscribeModuleListener,
                                         IIdSipSubscriptionListener)
  private
    Module:                  TIdSipSubscribeModule;
    ReceivedNotify:          TIdSipRequest;
    RenewSubscriptionFired:  Boolean;
    Subscription:            TIdSipOutboundSubscription;
    SubscriptionEstablished: Boolean;
    SubscriptionExpired:     Boolean;
    SubscriptionNotified:    Boolean;

    procedure CheckTerminatedSubscription(Subscription: TIdSipSubscription;
                                          const MsgPrefix: String);
    procedure CheckTerminatedSubscriptionWithNoResubscribe(const Reason: String);
    procedure CheckTerminatedSubscriptionWithResubscribe(const Reason: String);
    function  CreateSubscription: TIdSipOutboundSubscription;
    function  EstablishSubscription: TIdSipOutboundSubscription;

    procedure OnEstablishedSubscription(Subscription: TIdSipOutboundSubscription;
                                        Notify: TIdSipRequest);
    procedure OnExpiredSubscription(Subscription: TIdSipOutboundSubscription;
                                    Notify: TIdSipRequest);
    procedure OnNotify(Subscription: TIdSipOutboundSubscription;
                       Notify: TIdSipRequest);
    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipOutboundSubscription);
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipInboundSubscription);
    procedure ReceiveDeactivatedNotify(Subscribe: TIdSipRequest;
                                       Response: TIdSipResponse);
    procedure ReceiveDeactivatedNotifyWithRetry(Subscribe: TIdSipRequest;
                                                Response: TIdSipResponse);
    procedure ReceiveNotify(Subscribe: TIdSipRequest;
                            Response: TIdSipResponse;
                            const State: String;
                            const Reason: String = '');
  protected
    function CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddListener;
    procedure TestMatchNotify;
    procedure TestReceive2xx;
    procedure TestReceiveActiveNotify;
    procedure TestReceiveNotify;
    procedure TestReceiveTerminatingNotifyDeactivated;
    procedure TestReceiveTerminatingNotifyDeactivatedWithRetryAfter;
    procedure TestReceiveTerminatingNotifyNoResource;
    procedure TestReceiveTerminatingNotifyRejected;
    procedure TestReceiveTerminatingNotifyWithNoReason;
    procedure TestReceiveTerminatingNotifyWithUnknownReason;
    procedure TestReceiveTimeoutNotify;
    procedure TestRefreshThenReceiveDeactivatedNotify;
    procedure TestRemoveListener;
    procedure TestRefresh;
    procedure TestRefreshReceives481;
    procedure TestRefreshReceives4xx;
    procedure TestSubscribe;
    procedure TestTerminateBeforeEstablished;
    procedure TestTerminate;
    procedure TestUnsubscribe;
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
  IdSipDialog, TestFramework;

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
  Result.AddTest(TestTIdSipInboundSubscribe.Suite);
  Result.AddTest(TestTIdSipOutboundSubscribe.Suite);
  Result.AddTest(TestTIdSipOutboundUnsubscribe.Suite);
  Result.AddTest(TestTIdSipInboundSubscription.Suite);
  Result.AddTest(TestTIdSipOutboundSubscription.Suite);
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
//* TestTIdSipSubscribeModule                                                  *
//******************************************************************************
//* TestTIdSipSubscribeModule Public methods ***********************************

procedure TestTIdSipSubscribeModule.SetUp;
begin
  inherited SetUp;

  Self.Module := Self.Core.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  Self.Module.AddListener(Self);
  Self.Module.AddPackage(TIdSipTestPackage);

  Self.OnRenewedSubscriptionFired := false;
  Self.OnSubscriptionRequestFired := false;
end;

//* TestTIdSipSubscribeModule Private methods **********************************

procedure TestTIdSipSubscribeModule.OnRenewedSubscription(UserAgent: TIdSipAbstractUserAgent;
                                                          Subscription: TIdSipOutboundSubscription);
begin
  Self.OnRenewedSubscriptionFired := true;
  Self.UserAgentParam             := UserAgent;
end;

procedure TestTIdSipSubscribeModule.OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                                          Subscription: TIdSipInboundSubscription);
begin
  Self.OnSubscriptionRequestFired := true;
  Self.UserAgentParam             := UserAgent;
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

    Self.ReceiveSubscribe('Foo');

    Check(not L.SubscriptionRequest,
          'Listener notified of subscription request, thus not removed');
  finally
    L.Free;
  end;
end;

procedure TestTIdSipSubscribeModule.TestSubscribe;
const
  EventPackage = 'Foo';
var
  Sub: TIdSipOutboundSubscription;
begin
  Sub := Self.Module.Subscribe(Self.Destination, EventPackage);

  Self.MarkSentRequestCount;
  Sub.Send;
  CheckRequestSent('No request sent');
  CheckEquals(MethodSubscribe,
              Self.LastSentRequest.Method,
              'Unexpected response sent');

  CheckEquals(EventPackage,
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
  Self.Core.AddModule(TIdSipSubscribeModule);

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
  SubModule: TIdSipSubscribeModule;
begin
  SubModule := Self.Core.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  Self.MarkSentResponseCount;

  Self.ReceiveOptions;

  CheckResponseSent('No response sent');
  Response := Self.LastSentResponse;

  Check(Response.HasHeader(AllowEventsHeaderFull),
        'No Allow-Events header');
  CheckEquals(SubModule.AllowedEvents,
              Response.FirstHeader(AllowEventsHeaderFull).Value,
              'Allow-Events value');
end;

procedure TestTIdSipUserAgentWithSubscribeModule.TestSendInvite;
var
  Invite:    TIdSipRequest;
  SubModule: TIdSipSubscribeModule;
begin
  SubModule := Self.Core.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;

  Self.MarkSentRequestCount;
  Self.Core.Call(Self.Destination, '', '').Send;

  CheckRequestSent('No request sent');

  Invite := Self.LastSentRequest;
  Check(Invite.HasHeader(AllowEventsHeaderFull),
        'No Allow-Events header');
  CheckEquals(SubModule.AllowedEvents,
              Invite.FirstHeader(AllowEventsHeaderFull).Value,
              'Allow-Events value');
end;

//******************************************************************************
//* TestTIdSipInboundSubscribe                                                 *
//******************************************************************************
//* TestTIdSipInboundSubscribe Public methods **********************************

procedure TestTIdSipInboundSubscribe.SetUp;
begin
  inherited SetUp;

  Self.Core.AddModule(TIdSipSubscribeModule);
  Self.SubscribeRequest := Self.Core.CreateSubscribe(Self.Destination, 'Foo');
  Self.Subscription := TIdSipInboundSubscribe.Create(Self.Core, Self.SubscribeRequest);

  Self.SubscriptionDuration := 1000;
end;

procedure TestTIdSipInboundSubscribe.TearDown;
begin
  Self.Subscription.Free;
  Self.SubscribeRequest.Free;

  inherited TearDown;
end;

//* TestTIdSipInboundSubscribe Private methods *********************************

procedure TestTIdSipInboundSubscribe.CheckDuration(AcceptedDuration: Cardinal;
                                                   ExpectedDuration: Cardinal);
begin
  Self.MarkSentResponseCount;
  Self.Subscription.Accept(AcceptedDuration);
  CheckResponseSent('No response sent');

  CheckEquals(ExpectedDuration,
              Self.LastSentResponse.FirstExpires.NumericValue,
              'Wrong duration');
end;

procedure TestTIdSipInboundSubscribe.ReceiveSubscribeWithExpiresInContact(Duration: Cardinal);
var
  Subscribe: TIdSipRequest;
begin
  Subscribe := Self.Core.CreateSubscribe(Self.Destination, 'Foo');
  try
    Subscribe.RemoveAllHeadersNamed(ExpiresHeader);
    Subscribe.FirstContact.Expires := Duration;

    Self.Subscription.ReceiveRequest(Subscribe);
  finally
    Subscribe.Free;
  end;
end;

procedure TestTIdSipInboundSubscribe.ReceiveSubscribeWithoutExpires;
var
  Subscribe: TIdSipRequest;
begin
  Subscribe := Self.Core.CreateSubscribe(Self.Destination, 'Foo');
  try
    Subscribe.RemoveAllHeadersNamed(ExpiresHeader);

    Self.Subscription.ReceiveRequest(Subscribe);
  finally
    Subscribe.Free;
  end;
end;

//* TestTIdSipInboundSubscribe Published methods *******************************

procedure TestTIdSipInboundSubscribe.TestAccept;
begin
  Self.ReceiveSubscribe('Foo');

  Self.CheckDuration(Self.SubscriptionDuration, Self.SubscriptionDuration);

  CheckEquals(SIPAccepted,
              Self.LastSentResponse.StatusCode,
              'Unexpected response sent');
  Check(Self.LastSentResponse.HasHeader(ExpiresHeader),
        'No Expires header');
end;

procedure TestTIdSipInboundSubscribe.TestAcceptWithExpiresInRequestContact;
begin
  Self.ReceiveSubscribeWithExpiresInContact(Self.SubscriptionDuration * 2);

  Self.CheckDuration(Self.SubscriptionDuration, Self.SubscriptionDuration);
end;

procedure TestTIdSipInboundSubscribe.TestAcceptWithMaximalDuration;
begin
  Self.ReceiveSubscribe('Foo');

  Self.CheckDuration(Self.Subscription.InitialRequest.FirstExpires.NumericValue + 1,
                     Self.Subscription.InitialRequest.FirstExpires.NumericValue);
end;

procedure TestTIdSipInboundSubscribe.TestAcceptWithNoExpiresInRequest;
begin
  Self.ReceiveSubscribeWithoutExpires;

  Self.CheckDuration(Self.SubscriptionDuration, Self.SubscriptionDuration);
end;

procedure TestTIdSipInboundSubscribe.TestIsInbound;
var
  Action: TIdSipAction;
begin
  Self.Invite.Method := MethodSubscribe;
  Action := TIdSipInboundSubscribe.Create(Self.Core, Self.Invite);
  try
    Check(Action.IsInbound,
          Action.ClassName + ' not marked as inbound');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundSubscribe.TestIsInvite;
var
  Action: TIdSipAction;
begin
  Self.Invite.Method := MethodSubscribe;
  Action := TIdSipInboundSubscribe.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsInvite,
          Action.ClassName + ' marked as an Invite');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundSubscribe.TestIsOptions;
var
  Action: TIdSipAction;
begin
  Self.Invite.Method := MethodSubscribe;
  Action := TIdSipInboundSubscribe.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsOptions,
          Action.ClassName + ' marked as an Options');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundSubscribe.TestIsRegistration;
var
  Action: TIdSipAction;
begin
  Self.Invite.Method := MethodSubscribe;
  Action := TIdSipInboundSubscribe.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsRegistration,
          Action.ClassName + ' marked as a Registration');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundSubscribe.TestIsUnsubscribe;
var
  S:         TIdSipInboundSubscribe;
  Subscribe: TIdSipRequest;
begin
  // With a non-zero Expires
  Subscribe := Self.Core.CreateSubscribe(Self.Destination, 'Foo');
  try
    S := TIdSipInboundSubscribe.Create(Self.Core, Subscribe);
    try
      Check(not S.IsUnsubscribe,
            'Marked as an unsubscribe, but Expires = '
          + Self.Subscription.InitialRequest.FirstExpires.Value);
    finally
      S.Free;
    end;
  finally
    Subscribe.Free;
  end;

  // With a zero Expires
  Subscribe := Self.Core.CreateSubscribe(Self.Destination, 'Foo');
  try
    Subscribe.FirstExpires.NumericValue := 0;

    S := TIdSipInboundSubscribe.Create(Self.Core, Subscribe);
    try
      Check(S.IsUnsubscribe,
            'Not marked as an unsubscribe, but Expires = '
          + S.InitialRequest.FirstExpires.Value);
    finally
      S.Free;
    end;
  finally
    Subscribe.Free;
  end;
end;

procedure TestTIdSipInboundSubscribe.TestIsSession;
var
  Action: TIdSipAction;
begin
  Self.Invite.Method := MethodSubscribe;
  Action := TIdSipInboundSubscribe.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsSession,
          Action.ClassName + ' marked as a Session');
  finally
    Action.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipOutboundSubscribe                                                *
//******************************************************************************
//* TestTIdSipOutboundSubscribe Public methods *********************************

procedure TestTIdSipOutboundSubscribe.SetUp;
begin
  inherited SetUp;

  Self.EventPackage := 'Foo';
  Self.Failed       := false;
  Self.ID           := 'id1';
  Self.Succeeded    := false;
end;

//* TestTIdSipOutboundSubscribe Protected methods ******************************

function TestTIdSipOutboundSubscribe.CreateAction: TIdSipAction;
begin
  Result := Self.CreateSubscribe;
end;

//* TestTIdSipOutboundSubscribe Private methods ********************************

function TestTIdSipOutboundSubscribe.CreateSubscribe: TIdSipOutboundSubscribe;
begin
  Result := Self.Core.AddOutboundAction(TIdSipOutboundSubscribe) as TIdSipOutboundSubscribe;
  Result.Destination  := Self.Destination;
  Result.EventPackage := Self.EventPackage;
  Result.ID           := Self.ID;
  Result.AddListener(Self);
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
  Result.Destination  := Self.Destination;
  Result.EventPackage := Self.EventPackage;
  Result.AddListener(Self);
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
//* TestTIdSipInboundSubscription                                              *
//******************************************************************************
//* TestTIdSipInboundSubscription Public methods *******************************

procedure TestTIdSipInboundSubscription.SetUp;
begin
  inherited SetUp;

  Self.Core.AddModule(TIdSipSubscribeModule);

  Self.SubscribeRequest := Self.Core.CreateSubscribe(Self.Destination, 'Foo');
  Self.SubscribeAction  := TIdSipInboundSubscription.Create(Self.Core, Self.SubscribeRequest);
end;

procedure TestTIdSipInboundSubscription.TearDown;
begin
  Self.SubscribeAction.Free;
  Self.SubscribeRequest.Free;

  inherited TearDown;
end;

//* TestTIdSipInboundSubscription Published methods ****************************

procedure TestTIdSipInboundSubscription.TestIsInbound;
var
  Action: TIdSipAction;
begin
  Self.Invite.Method := MethodSubscribe;
  Action := TIdSipInboundSubscription.Create(Self.Core, Self.Invite);
  try
    Check(Action.IsInbound,
          Action.ClassName + ' not marked as inbound');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundSubscription.TestIsInvite;
var
  Action: TIdSipAction;
begin
  Self.Invite.Method := MethodSubscribe;
  Action := TIdSipInboundSubscription.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsInvite,
          Action.ClassName + ' marked as an Invite');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundSubscription.TestIsOptions;
var
  Action: TIdSipAction;
begin
  Self.Invite.Method := MethodSubscribe;
  Action := TIdSipInboundSubscription.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsOptions,
          Action.ClassName + ' marked as an Options');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundSubscription.TestIsRegistration;
var
  Action: TIdSipAction;
begin
  Self.Invite.Method := MethodSubscribe;
  Action := TIdSipInboundSubscription.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsRegistration,
          Action.ClassName + ' marked as a Registration');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundSubscription.TestIsSession;
var
  Action: TIdSipAction;
begin
  Self.Invite.Method := MethodSubscribe;
  Action := TIdSipInboundSubscription.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsSession,
          Action.ClassName + ' marked as a Session');
  finally
    Action.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipOutboundSubscription                                             *
//******************************************************************************
//* TestTIdSipOutboundSubscription Public methods ******************************

procedure TestTIdSipOutboundSubscription.SetUp;
begin
  inherited SetUp;

  Self.ReceivedNotify := TIdSipRequest.Create;

  Self.Module := Self.Core.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  Self.Module.AddListener(Self);

  Self.Subscription := Self.EstablishSubscription;
  
  Self.RenewSubscriptionFired  := false;
  Self.SubscriptionEstablished := false;
  Self.SubscriptionExpired     := false;
  Self.SubscriptionNotified    := false;
end;

procedure TestTIdSipOutboundSubscription.TearDown;
begin
  Self.ReceivedNotify.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundSubscription Protected methods ***************************

function TestTIdSipOutboundSubscription.CreateAction: TIdSipAction;
begin
  Result := Self.CreateSubscription;
end;

//* TestTIdSipOutboundSubscription Private methods *****************************

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

function TestTIdSipOutboundSubscription.CreateSubscription: TIdSipOutboundSubscription;
begin
  Result := Self.Module.Subscribe(Self.Destination, 'Foo') as TIdSipOutboundSubscription;
  Result.AddListener(Self);
  Result.Send;
end;

function TestTIdSipOutboundSubscription.EstablishSubscription: TIdSipOutboundSubscription;
begin
  Result := Self.CreateSubscription;
  Self.ReceiveOkFrom(Self.LastSentRequest,
                     Result.InitialRequest.ToHeader.Address.AsString);
  Self.ReceiveNotify(Result.InitialRequest,
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

procedure TestTIdSipOutboundSubscription.OnRenewedSubscription(UserAgent: TIdSipAbstractUserAgent;
                                                               Subscription: TIdSipOutboundSubscription);
begin
  Self.RenewSubscriptionFired := true;
end;

procedure TestTIdSipOutboundSubscription.OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                                               Subscription: TIdSipInboundSubscription);
begin
end;

procedure TestTIdSipOutboundSubscription.ReceiveDeactivatedNotify(Subscribe: TIdSipRequest;
                                                                  Response: TIdSipResponse);
begin
  Self.ReceiveNotify(Subscribe,
                     Response,
                     SubscriptionSubstateTerminated,
                     EventReasonDeactivated);
end;

procedure TestTIdSipOutboundSubscription.ReceiveDeactivatedNotifyWithRetry(Subscribe: TIdSipRequest;
                                                                           Response: TIdSipResponse);
var
  Notify:       TIdSipRequest;
  RemoteDialog: TIdSipDialog;
begin
  RemoteDialog := TIdSipDialog.CreateInboundDialog(Subscribe,
                                                   Response,
                                                   false);
  try
    Notify := Self.Core.CreateNotify(RemoteDialog,
                                     Subscribe,
                                     SubscriptionSubstateTerminated);
    try
      Notify.FirstSubscriptionState.Reason     := EventReasonDeactivated;
      Notify.FirstSubscriptionState.RetryAfter := 100;

      Self.ReceiveRequest(Notify);
    finally
      Notify.Free;
    end;
  finally
    RemoteDialog.Free;
  end;
end;

procedure TestTIdSipOutboundSubscription.ReceiveNotify(Subscribe: TIdSipRequest;
                                                       Response: TIdSipResponse;
                                                       const State: String;
                                                       const Reason: String = '');
var
  Notify:       TIdSipRequest;
  RemoteDialog: TIdSipDialog;
begin
  RemoteDialog := TIdSipDialog.CreateInboundDialog(Subscribe,
                                                   Response,
                                                   false);
  try
    Notify := Self.Core.CreateNotify(RemoteDialog,
                                     Subscribe,
                                     State);
    try
      if (Reason <> '') then
        Notify.FirstSubscriptionState.Reason := Reason;

      Self.ReceiveRequest(Notify);
    finally
      Notify.Free;
    end;
  finally
    RemoteDialog.Free;
  end;
end;

//* TestTIdSipOutboundSubscription Published methods ***************************

procedure TestTIdSipOutboundSubscription.TestAddListener;
var
  Listener: TIdSipTestSubscriptionListener;
  Sub:      TIdSipOutboundSubscription;
begin
  Sub := Self.CreateSubscription;

  Listener := TIdSipTestSubscriptionListener.Create;
  try
    Sub.AddListener(Listener);

    Self.ReceiveResponse(SIPNotImplemented);
    Check(Listener.ExpiredSubscription,
          'Test case not notified of failure; thus, not added as listener');
  finally
    Sub.RemoveListener(Listener);
    Listener.Free;
  end;
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

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyDeactivated;
begin
  Self.ReceiveDeactivatedNotify(Self.Subscription.InitialRequest,
                                Self.Dispatcher.Transport.LastResponse);

  Self.CheckTerminatedSubscriptionWithResubscribe(EventReasonDeactivated);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyDeactivatedWithRetryAfter;
begin
  Self.ReceiveDeactivatedNotifyWithRetry(Self.Subscription.InitialRequest,
                                         Self.Dispatcher.Transport.LastResponse);

  Check(Self.SubscriptionNotified,
        'Subscription didn''t notify listeners of received NOTIFY');

  CheckRequestSent('No request sent to refresh subscription: maybe the '
                 + 'Subscription obeyed (incorrectly) the retry-after?');
  CheckEquals(MethodSubscribe,
              Self.LastSentRequest.Method,
              'Unexpected request sent');
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyNoResource;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonNoResource);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonNoResource);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyRejected;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonRejected);

  Self.CheckTerminatedSubscriptionWithNoResubscribe(EventReasonRejected);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyWithNoReason;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated);

  Check(Self.SubscriptionNotified,
        'Subscription didn''t notify listeners of received NOTIFY');

  Check(Self.SubscriptionExpired,
        'Subscription didn''t expire');
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTerminatingNotifyWithUnknownReason;
const
  UnknownReason = 'unknown-reason';
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     UnknownReason);

  Self.CheckTerminatedSubscriptionWithResubscribe(UnknownReason);
end;

procedure TestTIdSipOutboundSubscription.TestReceiveTimeoutNotify;
begin
  Self.ReceiveNotify(Self.Subscription.InitialRequest,
                     Self.Dispatcher.Transport.LastResponse,
                     SubscriptionSubstateTerminated,
                     EventReasonTimeout);

  Self.CheckTerminatedSubscriptionWithResubscribe(EventReasonTimeout);
end;

procedure TestTIdSipOutboundSubscription.TestRefreshThenReceiveDeactivatedNotify;
begin
  Self.Subscription.Refresh;

  Self.MarkSentRequestCount;
  Self.ReceiveDeactivatedNotify(Self.Subscription.InitialRequest,
                                Self.Dispatcher.Transport.LastResponse);

  CheckRequestSent('No refreshing request sent');
  CheckEquals(MethodSubscribe,
              Self.LastSentRequest.Method,
              'Unexpected request');
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

  Self.MarkSentRequestCount;
  Self.Subscription.Refresh;
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

procedure TestTIdSipOutboundSubscription.TestRefreshReceives481;
begin
  //  ---                SUBSCRIBE                --->
  // <---                  200 OK                 ---
  //                   <time passes>
  //  ---                SUBSCRIBE                --->
  // <--- 481 Call Leg/Transaction Does Not Exist ---

  Self.Subscription.Refresh;
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

  Self.Subscription.Refresh;
  Self.ReceiveResponse(SIPRequestTimeout);
  Check(not Self.SubscriptionExpired,
        'Subscription mustn''t expire, but still exist until its Duration runs out');
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

  Self.Subscription := Self.Module.Subscribe(Self.UA.From, 'Foo');

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

  Self.Subscription := TIdSipInboundSubscription.Create(Self.UA, Self.Request);
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
