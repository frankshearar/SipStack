{
  (c) 2005 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipInviteModule;

interface

uses
  Classes, IdConnectionBindings, IdRtp, IdSipCore, IdSipDialog,
  IdSipInviteModule, IdSipLocation, IdSipMessage, IdSipSubscribeModule,
  IdSipTransport, IdSipUserAgent, IdTimerQueue, TestFrameworkSip,
  TestFrameworkSipTU;

type
  // This class attempts to isolate an intermittent bug that surfaces in the
  // TearDown of TestTerminateDuringRedirect.
  TestDebug = class(TTestCaseTU,
                    IIdSipActionListener,
                    IIdSipSessionListener)
  private
    FailReason: String;
    Session:    TIdSipOutboundSession;

    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse);
    procedure OnEndedSession(Session: TIdSipSession;
                             ErrorCode: Cardinal;
                             const Reason: String);
    procedure OnEstablishedSession(Session: TIdSipSession;
                                   const RemoteSessionDescription: String;
                                   const MimeType: String);
    procedure OnModifySession(Session: TIdSipSession;
                              const RemoteSessionDescription: String;
                              const MimeType: String);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Answer: TIdSipResponse);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
    procedure OnProgressedSession(Session: TIdSipSession;
                                  Progress: TIdSipResponse);
    procedure OnReferral(Session: TIdSipSession;
                         Refer: TIdSipRequest;
                         Binding: TIdConnectionBindings);
    procedure OnTerminated(Action: TIdSipAction);
    procedure ReceiveMovedTemporarily(Invite: TIdSipRequest;
                                      const Contacts: array of String); overload;
    procedure ReceiveMovedTemporarily(const Contacts: array of String); overload;
  public
    procedure SetUp; override;
  published
    procedure TestSendSetsInitialRequest;
    procedure TestTerminateDuringRedirect;
  end;

  TestTIdSipInviteModule = class(TTestCaseTU)
  private
    Dlg:    TIdSipDialog;
    Module: TIdSipInviteModule;
  public
    procedure SetUp; override;

    procedure CheckCommaSeparatedHeaders(const ExpectedValues: String;
                                         Header: TIdSipHeader;
                                         const Msg: String);
    procedure CheckCreateRequest(Dest: TIdSipToHeader;
                                 Request: TIdSipRequest);
    function  ConvertListToHeader(List: TStrings): String;
  published
    procedure TestAddListener;
    procedure TestCreateAck;
    procedure TestCreateBye;
    procedure TestCreateInvite;
    procedure TestCreateInviteInsideDialog;
    procedure TestCreateInviteWithBody;
    procedure TestCreateInviteWithGruu;
    procedure TestCreateReInvite;
    procedure TestDoNotDisturb;
    procedure TestReceiveByeForUnmatchedDialog;
    procedure TestReceiveByeWithoutTags;
    procedure TestReceiveInviteWithMultipleReplacesHeaders;
    procedure TestReceiveInviteWithNoContactHeader;
    procedure TestRejectUnknownContentType;
    procedure TestRemoveListener;
    procedure TestReplaceCall;
  end;

  TestTIdSipOutboundBye = class(TestTIdSipAction)
  private
    Dialog: TIdSipDialog;
  protected
    procedure AdjustNameResolutionForInDialogActions(DestinationIP: String); override;
    function  CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsInbound; override;
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsOwned; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
  end;

  TestTIdSipOutboundCancel = class(TestTIdSipAction)
  protected
    function  CreateAction: TIdSipAction; override;
  published
    procedure TestAbandonAuthentication; override;
    procedure TestAuthentication; override;
    procedure TestAuthenticationChallenge; override;
    procedure TestIsInbound; override;
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsOwned; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
    procedure TestMultipleAuthentication; override;
    procedure TestResend; override;
    procedure TestResendBeforeSend; override;
    procedure TestResendWithAuthForSameRealm; override;
    procedure TestResendWithProxyAuth; override;
  end;

  TestTIdSipInboundInvite = class(TestTIdSipAction,
                                  IIdSipInboundInviteListener)
  private
    Answer:         String;
    AnswerMimeType: String;
    Dialog:         TIdSipDialog;
    Failed:         Boolean;
    InviteAction:   TIdSipInboundInvite;
    Module:         TIdSipInviteModule;
    OnSuccessFired: Boolean;

    procedure CheckAck(InviteAction: TIdSipInboundInvite);
    procedure CheckAckWithDifferentCSeq(InviteAction: TIdSipInboundInvite);
    procedure OnFailure(InviteAgent: TIdSipInboundInvite);
    procedure OnSuccess(InviteAgent: TIdSipInboundInvite;
                        Ack: TIdSipMessage);
    procedure ReceiveAck;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAccept;
    procedure TestCancelAfterAccept;
    procedure TestCancelBeforeAccept;
    procedure TestInviteWithNoOffer;
    procedure TestIsInbound; override;
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsOutbound; override;
    procedure TestIsOwned; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
    procedure TestLastResponse;
    procedure TestLocalGruu; override;
    procedure TestMatchAck;
    procedure TestMatchAckToReInvite;
    procedure TestMatchAckToReInviteWithDifferentCSeq;
    procedure TestMatchAckWithDifferentCSeq;
    procedure TestMatchInitialInvite;
    procedure TestMethod;
    procedure TestNotifyOfNetworkFailure;
    procedure TestNotifyOfSuccess;
    procedure TestReceiveAck;
    procedure TestReceiveResentAck;
    procedure TestRedirectCall;
    procedure TestRedirectCallPermanent;
    procedure TestRejectCallBusy;
    procedure TestRejectCallStatusCode;
    procedure TestRejectCallStatusCodeAndText;
    procedure TestRemoveListener;
    procedure TestResendOk;
    procedure TestRing;
    procedure TestRingWithGruu;
    procedure TestRingWithSuppressLocalResponses;
    procedure TestSendProvisional;
    procedure TestSendProvisionalWithGruu;
    procedure TestSendProvisionalWithSuppressLocalResponses;
    procedure TestSendTrying;
    procedure TestSendTryingWithSuppressLocalResponses;
    procedure TestTerminateAfterAccept;
    procedure TestTerminateBeforeAccept;
    procedure TestTerminateSignalled; override;
    procedure TestTimeOut;
  end;

  TestTIdSipOutboundInvite = class(TestTIdSipAction,
                                   IIdSipOwnedActionListener,
                                   IIdSipInviteListener,
                                   IIdSipMessageModuleListener,
                                   IIdSipTransactionUserListener)
  private
    Dialog:                   TIdSipDialog;
    DroppedUnmatchedResponse: Boolean;
    InviteMimeType:           String;
    InviteOffer:              String;
    OnCallProgressFired:      Boolean;
    OnDialogEstablishedFired: Boolean;
    OnFailureFired:           Boolean;
    OnRedirectFired:          Boolean;
    OnSuccessFired:           Boolean;
    SdpMimeType:              String;
    ToHeaderTag:              String;

    procedure CheckReceiveFailed(StatusCode: Cardinal);
    procedure CheckReceiveOk(StatusCode: Cardinal);
    procedure CheckReceiveProvisional(StatusCode: Cardinal);
    procedure CheckReceiveRedirect(StatusCode: Cardinal);
    function  CreateArbitraryDialog: TIdSipDialog;
    procedure OnAddAction(UserAgent: TIdSipAbstractCore;
                          Action: TIdSipAction);
    procedure OnCallProgress(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse);
    procedure OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                  NewDialog: TidSipDialog);
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                        Message: TIdSipMessage;
                                        Binding: TIdConnectionBindings);
    procedure OnFailure(Action: TIdSipAction;
                        Response: TIdSipResponse;
                        const Reason: String);
    procedure OnRedirect(Action: TIdSipAction;
                         Response: TIdSipResponse);
    procedure OnRemoveAction(UserAgent: TIdSipAbstractCore;
                             Action: TIdSipAction);
    procedure OnSuccess(Action: TIdSipAction;
                        Msg: TIdSipMessage);
  protected
    procedure CheckSendDialogEstablishingRequestWithGruu;
    function  CreateInitialInvite: TIdSipOutboundInitialInvite;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAbandonAuthentication; override;
    procedure TestAddListener;
    procedure TestAnswerInAck;
    procedure TestCancelAfterAccept;
    procedure TestCancelBeforeAccept;
    procedure TestCancelBeforeProvisional;
    procedure TestCancelReceiveInviteOkBeforeCancelOk;
    procedure TestCancelWithOutboundProxy; virtual;
    procedure TestInviteTwice;
    procedure TestIsInvite; override;
    procedure TestIsOwned; override;
    procedure TestMatchOwnAck;
    procedure TestMethod;
    procedure TestOfferInInvite;
    procedure TestReceive2xxSchedulesTransactionCompleted;
    procedure TestReceiveProvisional;
    procedure TestReceiveGlobalFailed;
    procedure TestReceiveOk;
    procedure TestReceiveRedirect;
    procedure TestReceiveRequestFailed;
    procedure TestReceiveRequestFailedAfterAckSent;
    procedure TestReceiveServerFailed;
    procedure TestRemoveListener;
    procedure TestSendToSipsUri; virtual;
    procedure TestSendTwice;
    procedure TestSendWithGruu; virtual;
    procedure TestTerminateBeforeAccept;
    procedure TestTerminateAfterAccept;
    procedure TestTerminateSignalled; override;
    procedure TestTransactionCompleted;
  end;

  TestTIdSipOutboundInitialInvite = class(TestTIdSipOutboundInvite)
  private
    function CreateInvite: TIdSipOutboundInitialInvite;
  protected
    function CreateAction: TIdSipAction; override;
  published
    procedure TestSendWithGruu; override;
    procedure TestSendWithMaxForwards;
  end;

  TestTIdSipOutboundRedirectedInvite = class(TestTIdSipOutboundInvite)
  private
    function CreateInvite: TIdSipOutboundRedirectedInvite;
  protected
    function CreateAction: TIdSipAction; override;
  published
    procedure TestRedirectedInvite;
    procedure TestSendWithGruu; override;
  end;

  TestTIdSipOutboundReInvite = class(TestTIdSipOutboundInvite)
  private
    Dialog:            TIdSipDialog;
    InOutboundSession: Boolean;
    LocalGruu:         TIdSipContactHeader;

    function CreateInvite: TIdSipOutboundReInvite;
  protected
    function CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCancelWithOutboundProxy; override;
    procedure TestSendInInboundSessionWithAuthentication;
    procedure TestSendToSipsUri; override;
    procedure TestSendUsesMappedRoutes; override;
    procedure TestSendWithGruu; override;
  end;

  TestTIdSipOutboundReplacingInvite = class(TestTIdSipOutboundInvite)
  private
    CallID:  String;
    FromTag: String;
    ToTag:   String;

    function CreateInvite: TIdSipOutboundReplacingInvite;
  protected
    function CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSend;
  end;

  TestTIdSipSession = class(TestTIdSipAction,
                            IIdSipSessionListener,
                            IIdSipTransactionUserListener)
  protected
    DroppedUnmatchedResponse:  Boolean;
    ErrorCode:                 Cardinal;
    MimeType:                  String;
    Module:                    TIdSipInviteModule;
    OnEndedSessionFired:       Boolean;
    OnEstablishedSessionFired: Boolean;
    OnModifiedSessionFired:    Boolean;
    OnModifySessionFired:      Boolean;
    OnReferralFired:           Boolean;
    Reason:                    String;
    ReceivingBinding:          TIdConnectionBindings;
    RemoteSessionDescription:  String;
    SdpMimeType:               String;

    procedure CheckHeadersEqual(ExpectedMessage: TIdSipMessage;
                                ReceivedMessage: TIdSipMessage;
                                const HeaderName: String;
                                const Msg: String);
    procedure CheckResendWaitTime(Milliseconds: Cardinal;
                                  const Msg: String); virtual;
    function  CreateAndEstablishSession: TIdSipSession;
    function  CreateRemoteReInvite(LocalDialog: TIdSipDialog): TIdSipRequest;
    procedure EstablishSession(Session: TIdSipSession); virtual;
    function  MultiStreamSdp: String;
    procedure OnAddAction(UserAgent: TIdSipAbstractCore;
                          Action: TIdSipAction);
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                        Message: TIdSipMessage;
                                        Binding: TIdConnectionBindings);
    procedure OnEndedSession(Session: TIdSipSession;
                             ErrorCode: Cardinal;
                             const Reason: String); virtual;
    procedure OnEstablishedSession(Session: TIdSipSession;
                                   const RemoteSessionDescription: String;
                                   const MimeType: String); virtual;
    procedure OnModifiedSession(Session: TIdSipSession;
                                Answer: TIdSipResponse); virtual;
    procedure OnModifySession(Session: TIdSipSession;
                              const RemoteSessionDescription: String;
                              const MimeType: String); virtual;
    procedure OnProgressedSession(Session: TIdSipSession;
                                  Progress: TIdSipResponse); virtual;
    procedure OnReferral(Session: TIdSipSession;
                         Refer: TIdSipRequest;
                         Binding: TIdConnectionBindings);
    procedure OnRemoveAction(UserAgent: TIdSipAbstractCore;
                             Action: TIdSipAction);
    procedure ReceiveRemoteReInvite(Session: TIdSipSession);
    procedure ResendWith(Session: TIdSipSession;
                         AuthenticationChallenge: TIdSipResponse);
    function  SimpleSdp: String;
  public
    procedure SetUp; override;
  published
    procedure TestAckToInDialogInviteMatchesInvite;
    procedure TestDontMatchResponseToModify;
    procedure TestDontMatchResponseToInitialRequest;
    procedure TestInboundModify;
    procedure TestIsSession; override;
    procedure TestMatchBye;
    procedure TestMatchByeWithDifferingGridParameter;
    procedure TestMatchInitialRequest;
    procedure TestMatchInboundModify;
    procedure TestMatchInboundModifyAck;
    procedure TestMatchReferWithCorrectGridParameter;
    procedure TestMatchReferWithIncorrectGridParameter;
    procedure TestMatchTargetDialog;
    procedure TestModify;
    procedure TestModifyBeforeFullyEstablished;
    procedure TestModifyDuringModification;
    procedure TestModifyGlareInbound;
    procedure TestModifyGlareOutbound;
    procedure TestModifyNetworkFailure;
    procedure TestModifyRejected;
    procedure TestModifyRejectedWithTimeout;
    procedure TestModifyWaitTime;
    procedure TestReceiveBye;
    procedure TestReceiveByeWithPendingRequests;
    procedure TestReceiveInDialogReferWithNoSubscribeModule;
    procedure TestReceiveInDialogRefer;
    procedure TestReceiveOutOfDialogByeTargettingLocalGruu;
    procedure TestRejectInviteWhenInboundModificationInProgress;
    procedure TestRejectInviteWhenOutboundModificationInProgress;
    procedure TestRemodify;
    procedure TestRemoveSessionListener; virtual;
    procedure TestTerminateByeTransactionTimesOut;
  end;

  TestTIdSipInboundSession = class(TestTIdSipSession,
                                   IIdRTPDataListener,
                                   IIdSipInviteModuleListener,
                                   IIdSipMessageModuleListener)
  private
    RemoteContentType:      String;
    RemoteDesc:             String;
    SentRequestTerminated:  Boolean;
    Session:                TIdSipInboundSession;

    procedure CheckDefaultPreferredTransport(Response: TIdSipResponse; ExpectParam: Boolean; MsgPrefix: String);
    procedure CheckPreferredTransport(Session: TIdSipInboundSession; PreferredTransport: String; MsgPrefix: String);
    procedure CheckPreferredTransportWithoutSettingPreferredTransport(Session: TIdSipInboundSession; ExpectedTransport: String; MsgPrefix: String);
    procedure CheckRedirectCall(TemporaryMove: Boolean);
    procedure CheckSendProvisionalWithInappropriateStatusCode(Session: TIdSipInboundSession;
                                                 StatusCode: Cardinal);
    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdConnectionBindings);
    procedure ReceiveAckWithBody(const SessionDesc,
                                 ContentType: String);
  protected
    procedure CheckResendWaitTime(Milliseconds: Cardinal;
                                  const Msg: String); override;
    function  CreateAction: TIdSipAction; override;
    procedure EstablishSession(Session: TIdSipSession); override;
    procedure OnEndedSession(Session: TIdSipSession;
                             ErrorCode: Cardinal;
                             const Reason: String); override;
    procedure OnInboundCall(UserAgent: TIdSipInviteModule;
                            Session: TIdSipInboundSession);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Binding: TIdConnectionBindings); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAcceptCall;
    procedure TestAcceptCallWithGruu;
    procedure TestAcceptCallWithAnswerInAck;
    procedure TestAcceptCallWithNoSDPInAck;
    procedure TestAddSessionListener;
    procedure TestCancelAfterAccept;
    procedure TestCancelBeforeAccept;
    procedure TestCancelNotifiesSession;
    procedure TestInterleavedResponseSendTimingFailure;
    procedure TestInviteExpires;
    procedure TestInviteHasNoOffer;
    procedure TestInviteHasOffer;
    procedure TestIsInbound; override;
    procedure TestIsOutbound; override;
    procedure TestIsOutboundCall;
    procedure TestInviteWithReplaces;
    procedure TestLocalGruu; override;
    procedure TestMethod;
    procedure TestNotifyListenersOfEstablishedSession;
    procedure TestNotifyListenersOfEstablishedSessionInviteHasNoBody;
    procedure TestOkUsesGruuWhenUaDoes;
    procedure TestInboundModifyBeforeFullyEstablished;
    procedure TestInboundModifyReceivesNoAck;
    procedure TestProvisionalResponseContactUsesPreferredTransportToFQDN;
    procedure TestProvisionalResponseContactUsesPreferredTransportToIpAddress;
    procedure TestProvisionalResponseContactForAddressWithDefaultPreferredTransport;
    procedure TestProvisionalResponseContactForAddressWithNoPreferredTransport;
    procedure TestRaceConditionAckLost;
    procedure TestRaceConditionRetransmittedInitialInviteIgnored;
    procedure TestReceiveCallMassiveInvite;
    procedure TestReceiveCallSendsTrying;
    procedure TestReceiveOutOfOrderReInvite;
    procedure TestRedirectCall;
    procedure TestRedirectCallPermanent;
    procedure TestRejectCallBusy;
    procedure TestRejectCallStatusCode;
    procedure TestRejectCallStatusCodeAndText;
    procedure TestRemoveSessionListener; override;
    procedure TestRing;
    procedure TestRingWithGruu;
    procedure TestRingWithSuppressLocalResponses;
    procedure TestSendProvisional;
    procedure TestSendProvisionalAfterDialogEstablished;
    procedure TestSendProvisionalAfterEarlyDialogEstablished;
    procedure TestSendProvisionalWithDefaults;
    procedure TestSendProvisionalWithGruu;
    procedure TestSendProvisionalWithInappropriateStatusCode;
    procedure TestSupportsExtension;
    procedure TestSuppressLocalResponses;
    procedure TestTerminate;
    procedure TestTerminateAlmostEstablishedSession;
    procedure TestTerminateSignalled; override;
    procedure TestTerminateUnestablishedSession;
  end;

  TestTIdSipOutboundSession = class(TestTIdSipSession,
                                    IIdSipInviteModuleListener)
  private
    FailFirstInviteSend:      Boolean;
    HairpinCall:              TIdSipInboundSession;
    LocalMimeType:            String;
    LocalDescription:         String;
    OnProgressedSessionFired: Boolean;
    RemoteDesc:               String;
    RemoteMimeType:           String;
    Session:                  TIdSipOutboundSession;

    procedure OnInboundCall(UserAgent: TIdSipInviteModule;
                            Session: TIdSipInboundSession);
    procedure ReceiveForbidden;
    procedure ReceiveOKWithRecordRoute;
    procedure ReceiveProvisionalResponse(StatusCode: Cardinal; SDP: String);
    procedure ReceiveRemoteDecline;
  protected
    MimeType: String;
    SDP:      String;

    procedure CheckResendWaitTime(Milliseconds: Cardinal;
                                  const Msg: String); override;
    function  CreateAction: TIdSipAction; override;
    procedure EstablishSession(Session: TIdSipSession); override;
    procedure OnEstablishedSession(Session: TIdSipSession;
                                   const RemoteSessionDescription: String;
                                   const MimeType: String); override;
    procedure OnProgressedSession(Session: TIdSipSession;
                                  Progress: TIdSipResponse); override;
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Binding: TIdConnectionBindings); override;
  public
    procedure SetUp; override;
  published
    procedure TestAbandonAuthentication; override;
    procedure TestAck;
    procedure TestAckFromRecordRouteResponse;
    procedure TestAckWithAuthorization;
    procedure TestAckWithMultipleAuthorization;
    procedure TestAckWithProxyAuthorization;
    procedure TestActionListenersArentNotifiedReSessionEvents;
    procedure TestByeCarriesInviteAuthorization;
    procedure TestCall;
    procedure TestCallDoesntClaimToModifySession;
    procedure TestCallFlowToGateway;
    procedure TestCallNetworkFailure;
    procedure TestCallRemoteRefusal;
    procedure TestCallSecure;
    procedure TestCallSipsUriUsesTls;
    procedure TestCallWithGruu;
    procedure TestCallWithMaxForwards;
    procedure TestCallWithOffer;
    procedure TestCallWithoutOffer;
    procedure TestCancelReceiveInviteOkBeforeCancelOk;
    procedure TestCircularRedirect;
    procedure TestDialogNotEstablishedOnTryingResponse;
    procedure TestDoubleRedirect;
    procedure TestEmptyTargetSetMeansTerminate;
    procedure TestEstablishedSessionSetsInitialRequestToTag;
    procedure TestGlobalFailureEndsSession;
    procedure TestHairpinCall;
    procedure TestHangUp;
    procedure TestIsOutboundCall;
    procedure TestMethod;
    procedure TestModifyUsesAuthentication;
    procedure TestNetworkFailuresLookLikeSessionFailures;
    procedure TestOneNetworkFailureDoesntFailWholeRedirection;
    procedure TestRaceConditionCrossoverOfCancelAndInvites200OK;
    procedure TestReceive1xxNotifiesListeners;
    procedure TestReceive2xxSendsAck;
    procedure TestReceive3xxSendsNewInvite;
    procedure TestReceive3xxWithOneContact;
    procedure TestReceive3xxWithNoContacts;
    procedure TestReceiveFailureResponseAfterSessionEstablished;
    procedure TestReceiveFailureResponseNotifiesOnce;
    procedure TestReceiveFailureSetsReason;
    procedure TestReceiveFinalResponseSendsAck;
    procedure TestRedirectAndAccept;
    procedure TestRedirectMultipleOks;
    procedure TestRedirectNoMoreTargets;
    procedure TestRedirectWithMaxForwards;
    procedure TestRedirectWithMultipleContacts;
    procedure TestRedirectWithNoSuccess;
    procedure TestRemoveSessionListener; override;
    procedure TestSendSetsInitialRequest;
    procedure TestSendWithGruu;
    procedure TestSupportsExtension;
    procedure TestTerminateDuringRedirect;
    procedure TestTerminateEstablishedSession;
    procedure TestTerminateNetworkFailure;
    procedure TestTerminateSignalled; override;
    procedure TestTerminateUnestablishedSession;
  end;

  TestSessionReplacer = class(TTestCaseTU,
                              IIdSipInviteModuleListener,
                              IIdSipMessageModuleListener,
                              IIdSipSubscribeModuleListener,
                              IIdSipTransactionUserListener)
  private
    Alice:          TIdSipUserAgent;
    AlicesNewPhone: TIdSipUserAgent;
    Bob:            TIdSipUserAgent;
    InboundCall:    TIdSipInboundSession;
    ParkPlace:      TIdSipUserAgent;
    ReceivingUA:    TIdSipAbstractCore;
    Refer:          TIdSipInboundSubscription;

    function  CreateTransferringUA(const Address: String): TIdSipUserAgent;
    procedure OnAddAction(UserAgent: TIdSipAbstractCore;
                          Action: TIdSipAction);
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                        Message: TIdSipMessage;
                                        Binding: TIdConnectionBindings);
    procedure OnInboundCall(UserAgent: TIdSipInviteModule;
                            Session: TIdSipInboundSession);
    procedure OnRemoveAction(UserAgent: TIdSipAbstractCore;
                             Action: TIdSipAction);
    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipOutboundSubscription);
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipInboundSubscription);
    function  SubscribeModuleOf(UA: TIdSipUserAgent): TIdSipSubscribeModule;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSessionReplacer;
  end;

  TestTIdSipInviteModuleOnInboundCallMethod = class(TActionMethodTestCase)
  private
    Invite:   TIdSipRequest;
    Method:   TIdSipInviteModuleInboundCallMethod;
    Listener: TIdSipTestInviteModuleListener;
    Session:  TIdSipInboundSession;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipInboundInviteFailureMethod = class(TActionMethodTestCase)
  private
    Invite: TIdSipRequest;
    Method: TIdSipInboundInviteFailureMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipInboundInviteSuccessMethod = class(TActionMethodTestCase)
  private
    Invite: TIdSipRequest;
    Method: TIdSipInboundInviteSuccessMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TInviteMethodTestCase = class(TActionMethodTestCase)
  private
    Invite:   TIdSipOutboundInvite;
    Listener: TIdSipTestInviteListener;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipInviteCallProgressMethod = class(TInviteMethodTestCase)
  private
    Method: TIdSipInviteCallProgressMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipInviteDialogEstablishedMethod = class(TActionMethodTestCase)
  private
    Method: TIdSipInviteDialogEstablishedMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestInviteMethod = class(TActionMethodTestCase)
  private
    Invite:   TIdSipOutboundInvite;
    Listener: TIdSipTestInviteListener;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestSessionMethod = class(TActionMethodTestCase)
  protected
    Listener: TIdSipTestSessionListener;
    Session:  TIdSipSession;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipEndedSessionMethod = class(TestSessionMethod)
  private
    Method: TIdSipEndedSessionMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipEstablishedSessionMethod = class(TestSessionMethod)
  private
    Method: TIdSipEstablishedSessionMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipModifiedSessionMethod = class(TestSessionMethod)
  private
    Answer: TIdSipResponse;
    Method: TIdSipModifiedSessionMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipSessionModifySessionMethod = class(TestSessionMethod)
  private
    Session: TIdSipOutboundSession;
    Method:  TIdSipSessionModifySessionMethod;
  public
    procedure SetUp; override;
  published
    procedure TestRun;
  end;

  TestTIdSipProgressedSessionMethod = class(TestSessionMethod)
  private
    Method:   TIdSipProgressedSessionMethod;
    Progress: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipSessionReferralMethod = class(TestSessionMethod)
  private
    Method: TIdSipSessionReferralMethod;
    Refer:  TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TIdSipSessionWaitClass = class of TIdSipSessionWait;

  TIdSipSessionWaitTestCase = class(TTestCaseTU)
  protected
    Wait: TIdSipSessionWait;

    procedure CheckTriggerDoesNothing(Wait: TIdSipSessionWait;
                                      Msg: String); virtual;
    function WaitType: TIdSipSessionWaitClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTriggerWithIDOfNonexistentObject;
    procedure TestTriggerWithIDOfWrongTypeOfObject;
  end;

  TIdSipSessionModifyingWaitTestCase = class(TIdSipSessionWaitTestCase)
  protected
    Call:            TIdSipSession;
    InitialMimeType: String;
    InitialOffer:    String;
    NewMimeType:     String;
    NewOffer:        String;

    function EstablishCall: TIdSipSession;
  public
    procedure SetUp; override;
  end;

  TestTIdSipSessionAcceptCallModify = class(TIdSipSessionModifyingWaitTestCase)
  private
    procedure ReceiveModify(LocalSession: TIdSipSession);
  protected
    procedure CheckTriggerDoesNothing(Wait: TIdSipSessionWait;
                                      Msg: String); override;
    function  WaitType: TIdSipSessionWaitClass; override;
  published
    procedure TestTrigger;
  end;

  TestTIdSipSessionModifyWait = class(TIdSipSessionModifyingWaitTestCase)
  protected
    procedure CheckTriggerDoesNothing(Wait: TIdSipSessionWait;
                                      Msg: String); override;
    function  WaitType: TIdSipSessionWaitClass; override;
  published
    procedure TestTrigger;
  end;

  TIdSipInboundSessionWaitTestCase = class(TIdSipSessionWaitTestCase)
  private
    L:       TIdSipTestInviteModuleListener;
    Session: TIdSipInboundSession;
  protected
    procedure CheckTriggerDoesNothing(Wait: TIdSipSessionWait;
                                      Msg: String); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipSessionAcceptWait = class(TIdSipInboundSessionWaitTestCase)
  private
    Answer:   String;
    MimeType: String;
  protected
    function WaitType: TIdSipSessionWaitClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestTrigger;
  end;

  TestTIdSipSessionRedirectWait = class(TIdSipInboundSessionWaitTestCase)
  private
    Target:  TIdSipAddressHeader;
  protected
    function WaitType: TIdSipSessionWaitClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTriggerWithTemporaryFalse;
    procedure TestTriggerWithTemporaryTrue;
  end;

  TestTIdSipSendProvisionalWait = class(TIdSipInboundSessionWaitTestCase)
  private
    StatusCode: Cardinal;
    StatusText: String;
  protected
    function WaitType: TIdSipSessionWaitClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestTrigger;
  end;

  TestTIdSipSessionRejectWait = class(TIdSipInboundSessionWaitTestCase)
  private
    ReasonPhrase: String;
    StatusCode:   Cardinal;
  protected
    function WaitType: TIdSipSessionWaitClass; override;
  public
    procedure SetUp; override;
  published
//    procedure TestTrigger;
  end;

implementation

uses
  IdException, IdRegisteredObject, IdSimpleParser, IdSipDns,
  IdSipMockTransactionDispatcher, IdSipTransaction, SysUtils, TestFramework;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipInviteModule unit tests');
//  Result.AddTest(TestDebug.Suite);
  Result.AddTest(TestTIdSipInviteModule.Suite);
  Result.AddTest(TestTIdSipOutboundBye.Suite);
  Result.AddTest(TestTIdSipOutboundCancel.Suite);
  Result.AddTest(TestTIdSipInboundInvite.Suite);
  Result.AddTest(TestTIdSipOutboundInitialInvite.Suite);
  Result.AddTest(TestTIdSipOutboundRedirectedInvite.Suite);
  Result.AddTest(TestTIdSipOutboundReInvite.Suite);
  Result.AddTest(TestTIdSipOutboundReplacingInvite.Suite);
  Result.AddTest(TestTIdSipInboundSession.Suite);
  Result.AddTest(TestTIdSipOutboundSession.Suite);
//  Result.AddTest(TestSessionReplacer.Suite);
  Result.AddTest(TestTIdSipInviteModuleOnInboundCallMethod.Suite);
  Result.AddTest(TestTIdSipInboundInviteFailureMethod.Suite);
  Result.AddTest(TestTIdSipInboundInviteSuccessMethod.Suite);
  Result.AddTest(TestTIdSipInviteCallProgressMethod.Suite);
  Result.AddTest(TestTIdSipInviteDialogEstablishedMethod.Suite);
  Result.AddTest(TestTIdSipEndedSessionMethod.Suite);
  Result.AddTest(TestTIdSipEstablishedSessionMethod.Suite);
  Result.AddTest(TestTIdSipModifiedSessionMethod.Suite);
  Result.AddTest(TestTIdSipSessionModifySessionMethod.Suite);
  Result.AddTest(TestTIdSipProgressedSessionMethod.Suite);
  Result.AddTest(TestTIdSipSessionReferralMethod.Suite);
  Result.AddTest(TestTIdSipSessionAcceptCallModify.Suite);
  Result.AddTest(TestTIdSipSessionModifyWait.Suite);
  Result.AddTest(TestTIdSipSessionAcceptWait.Suite);
  Result.AddTest(TestTIdSipSessionRedirectWait.Suite);
  Result.AddTest(TestTIdSipSendProvisionalWait.Suite);
  Result.AddTest(TestTIdSipSessionRejectWait.Suite);
end;

//******************************************************************************
//* TestDebug                                                                  *
//******************************************************************************
//* TestDebug Private methods **************************************************

procedure TestDebug.OnAuthenticationChallenge(Action: TIdSipAction;
                                              Response: TIdSipResponse);
begin
end;

procedure TestDebug.OnEndedSession(Session: TIdSipSession;
                                   ErrorCode: Cardinal;
                                   const Reason: String);
begin
end;

procedure TestDebug.OnEstablishedSession(Session: TIdSipSession;
                                         const RemoteSessionDescription: String;
                                         const MimeType: String);
begin
end;

procedure TestDebug.OnModifySession(Session: TIdSipSession;
                                    const RemoteSessionDescription: String;
                                    const MimeType: String);
begin
end;

procedure TestDebug.OnModifiedSession(Session: TIdSipSession;
                                      Answer: TIdSipResponse);
begin
end;

procedure TestDebug.OnNetworkFailure(Action: TIdSipAction;
                                     ErrorCode: Cardinal;
                                     const Reason: String);
begin
  Self.FailReason := Reason;
end;

procedure TestDebug.OnProgressedSession(Session: TIdSipSession;
                                        Progress: TIdSipResponse);
begin
end;

procedure TestDebug.OnReferral(Session: TIdSipSession;
                               Refer: TIdSipRequest;
                               Binding: TIdConnectionBindings);
begin
end;

procedure TestDebug.OnTerminated(Action: TIdSipAction);
begin
end;

procedure TestDebug.ReceiveMovedTemporarily(Invite: TIdSipRequest;
                                            const Contacts: array of String);
var
  I:        Integer;
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Invite,
                                          SIPMovedTemporarily);
  try
    for I := Low(Contacts) to High(Contacts) do
      Response.AddHeader(ContactHeaderFull).Value := Contacts[I];

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestDebug.ReceiveMovedTemporarily(const Contacts: array of String);
begin
  Self.ReceiveMovedTemporarily(Self.LastSentRequest, Contacts);
end;

//* TestDebug Public methods ***************************************************

procedure TestDebug.SetUp;
begin
  inherited SetUp;

  Self.Session := Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '');
  Self.Session.AddSessionListener(Self);
  Self.Session.Send;
end;

//* TestDebug Published methods ************************************************

procedure TestDebug.TestSendSetsInitialRequest;
var
  Session: TIdSipOutboundSession;
begin
  Session := Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '') as TIdSipOutboundSession;
  Session.AddSessionListener(Self);
  Session.Send;

  Check(Session.InitialRequest.Equals(Self.LastSentRequest),
        'Sending the session didn''t set the session''s InitialRequest');
end;

procedure TestDebug.TestTerminateDuringRedirect;
var
  Contacts: array of String;
  I:        Integer;
begin
  //                             Request count
  //  ---       INVITE      ---> #0
  // <---   302 (foo,bar)   ---
  //  ---        ACK        --->
  //  ---    INVITE (foo)   ---> #1
  //  ---    INVITE (bar)   ---> #2
  // <---     100 (foo)     --- (we receive 100s so the InviteActions will send CANCELs immediately)
  // <---     100 (bar)     ---
  // <Terminate the connection attempt>
  //  ---    CANCEL (foo)   ---> #3
  // <--- 200 (foo, CANCEL) ---
  //  ---    CANCEL (bar)   ---> #4
  // <--- 200 (bar, CANCEL) ---

  SetLength(Contacts, 2);
  Contacts[0] := 'sip:foo@bar.org';
  Contacts[1] := 'sip:bar@bar.org';

  Self.ReceiveMovedTemporarily(Contacts);

  Check(Self.SentRequestCount >= 3,
        'Not enough requests sent: 1 + 2 INVITEs: ' + Self.FailReason);

  Self.ReceiveTrying(Self.SentRequestAt(1));
  Self.ReceiveTrying(Self.SentRequestAt(2));

  Self.MarkSentRequestCount;
  Self.Session.Terminate;

  CheckEquals(Self.RequestCount + Cardinal(Length(Contacts)),
              Self.SentRequestCount,
              'Session didn''t attempt to terminate all INVITEs');

  Check(Self.SentRequestCount >= 5,
        'Not enough requests sent: 1 + 2 INVITEs, 2 CANCELs');

  for I := 0 to 1 do begin
    CheckEquals(Contacts[I],
                Self.SentRequestAt(I + 3).RequestUri.Uri,
                'CANCEL to ' + Contacts[I]);
    CheckEquals(MethodCancel,
                Self.SentRequestAt(I + 3).Method,
                'Request method to ' + Contacts[I]);
  end;
end;

//******************************************************************************
//* TestTIdSipInviteModule                                                     *
//******************************************************************************
//* TestTIdSipInviteModule Public **********************************************

procedure TestTIdSipInviteModule.SetUp;
var
  Invite:   TIdSipRequest;
  Response: TIdSipResponse;
begin
  inherited SetUp;

  Self.Module := Self.Core.ModuleFor(MethodInvite) as TIdSipInviteModule;

  Invite := TIdSipTestResources.CreateBasicRequest;
  try
    Response := TIdSipTestResources.CreateBasicResponse;
    try
      Self.Dlg := TIdSipDialog.CreateInboundDialog(Invite,
                                                   Response,
                                                   false);
      Self.Dlg.ReceiveRequest(Invite);
      Self.Dlg.ReceiveResponse(Response)
    finally
      Response.Free;
    end;
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipInviteModule.CheckCommaSeparatedHeaders(const ExpectedValues: String;
                                                            Header: TIdSipHeader;
                                                            const Msg: String);
var
  Hdr:    TIdSipCommaSeparatedHeader;
  I:      Integer;
  Values: TStringList;
begin
  CheckEquals(TIdSipCommaSeparatedHeader.ClassName,
              Header.ClassName,
              Msg + ': Unexpected header type in CheckCommaSeparatedHeaders');

  Hdr := Header as TIdSipCommaSeparatedHeader;
  Values := TStringList.Create;
  try
    Values.CommaText := ExpectedValues;

    for I := 0 to Values.Count - 1 do
      CheckEquals(Values[I],
                  Hdr.Values[I],
                  Msg + ': ' + IntToStr(I + 1) + 'th value');
  finally
    Values.Free;
  end;
end;

procedure TestTIdSipInviteModule.CheckCreateRequest(Dest: TIdSipToHeader;
                                                    Request: TIdSipRequest);
begin
  CheckEquals(Dest.Address,
              Request.RequestUri,
              'Request-URI not properly set');

  Check(Request.HasHeader(CallIDHeaderFull), 'No Call-ID header added');
  CheckNotEquals('',
                 (Request.FirstHeader(CallIDHeaderFull) as TIdSipCallIdHeader).Value,
                 'Call-ID must not be empty');

  Check(Request.HasHeader(ContactHeaderFull), 'No Contact header added');

  CheckEquals(Request.From.DisplayName,
              Self.Core.From.DisplayName,
              'From.DisplayName');
  CheckEquals(Request.From.Address,
              Self.Core.From.Address,
              'From.Address');
    Check(Request.From.HasTag,
          'Requests MUST have a From tag; cf. RFC 3261 section 8.1.1.3');

  CheckEquals(Request.RequestUri,
              Request.ToHeader.Address,
              'To header incorrectly set');

  CheckEquals(1,
              Request.Path.Length,
              'New requests MUST have a Via header; cf. RFC 3261 section 8.1.1.7');
  Check(Request.LastHop.HasBranch,
        'New requests MUST have a branch; cf. RFC 3261 section 8.1.1.7');
  CheckEquals(UdpTransport,
              Request.LastHop.Transport,
              'UDP should be the default transport');
end;

function TestTIdSipInviteModule.ConvertListToHeader(List: TStrings): String;
begin
  Result := StringReplace(List.CommaText, ',', ', ', [rfReplaceAll]);
end;

//* TestTIdSipInviteModule Published *******************************************

procedure TestTIdSipInviteModule.TestAddListener;
var
  L1, L2: TIdSipTestInviteModuleListener;
begin
  L1 := TIdSipTestInviteModuleListener.Create;
  try
    L2 := TIdSipTestInviteModuleListener.Create;
    try
      Self.Module.AddListener(L1);
      Self.Module.AddListener(L2);

      Self.ReceiveInvite;

      Check(L1.InboundCall, 'First listener not notified of inbound call');
      Check(L2.InboundCall, 'Second listener not notified of inbound call');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipInviteModule.TestCreateAck;
var
  Ack: TIdSipRequest;
begin
  Ack := Self.Module.CreateAck(Self.Dlg);
  try
    CheckEquals(1, Ack.Path.Count, 'Wrong number of Via headers');
  finally
    Ack.Free;
  end;
end;

procedure TestTIdSipInviteModule.TestCreateBye;
var
  Bye: TIdSipRequest;
begin
  Bye := Self.Module.CreateBye(Self.Dlg);
  try
    CheckEquals(MethodBye, Bye.Method, 'Unexpected method');
    CheckEquals(Bye.Method,
                Bye.CSeq.Method,
                'CSeq method doesn''t match request method');
  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipInviteModule.TestCreateInvite;
const
  MaxForwards = 42;
var
  Dest:    TIdSipToHeader;
  Request: TIdSipRequest;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna';
    Request := Self.Module.CreateInvite(Self.Core.From, Dest, '', '', MaxForwards);
    try
      Self.CheckCreateRequest(Dest, Request);
      CheckEquals(MaxForwards, Request.MaxForwards, 'Max-Forwards');
      CheckEquals(MethodInvite, Request.Method, 'Incorrect method');

      Check(not Request.ToHeader.HasTag,
            'This request is outside of a dialog, hence MUST NOT have a '
          + 'To tag. See RFC:3261, section 8.1.1.2');

      Check(Request.HasHeader(CSeqHeader), 'No CSeq header');
      Check(not Request.HasHeader(ContentDispositionHeader),
            'Needless Content-Disposition header');

      Check(Request.HasHeader(AllowHeader), 'No Allow header');
      CheckCommaSeparatedHeaders(Self.Core.KnownMethods,
                                 Request.FirstHeader(AllowHeader),
                                 'Allow header');

      Check(Request.HasHeader(SupportedHeaderFull), 'No Supported header');
      CheckEquals(Self.Core.AllowedExtensions,
                  Request.FirstHeader(SupportedHeaderFull).Value,
                  'Supported header value');

      Check(Request.HasHeader(ContactHeaderFull), 'No Contact header');
      Check(not Request.FirstContact.HasParameter(TagParam), 'Contact headers don''t use "tag" parameters');
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipInviteModule.TestCreateInviteInsideDialog;
var
  Invite: TIdSipRequest;
begin
  Invite := Self.Module.CreateReInvite(Self.Dlg, '', '');
  try
      Check(Invite.ToHeader.HasTag,
            'This request is inside a dialog, hence MUST have a '
          + 'To tag. See RFC:3261, section 12.2.1.1');
      CheckEquals(Self.Dlg.ID.RemoteTag,
                  Invite.ToHeader.Tag,
                  'To tag');

      Check(Invite.HasHeader(CSeqHeader), 'No CSeq header');
      Check(not Invite.HasHeader(ContentDispositionHeader),
            'Needless Content-Disposition header');

    Check(Invite.HasHeader(AllowHeader), 'No Allow header');
    CheckCommaSeparatedHeaders(Self.Core.KnownMethods,
                               Invite.FirstHeader(AllowHeader),
                               'Allow header');

    Check(Invite.HasHeader(SupportedHeaderFull), 'No Supported header');
    CheckEquals(Self.Core.AllowedExtensions,
                Invite.FirstHeader(SupportedHeaderFull).Value,
                'Supported header value');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipInviteModule.TestCreateInviteWithBody;
var
  Invite: TIdSipRequest;
  Body:   String;
begin
  Body := 'foo fighters';

  Invite := Self.Module.CreateInvite(Self.Core.From, Self.Destination, Body, 'text/plain', TIdSipRequest.DefaultMaxForwards);
  try
    CheckEquals(Length(Body), Invite.ContentLength, 'Content-Length');
    CheckEquals(Body,         Invite.Body,          'Body');

    Check(Invite.HasHeader(ContentDispositionHeader),
          'Missing Content-Disposition');
    CheckEquals(DispositionSession,
                Invite.ContentDisposition.Value,
                'Content-Disposition value');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipInviteModule.TestCreateInviteWithGruu;
var
  Invite: TIdSipRequest;
begin
  Self.UseGruu;

  Invite := Self.Module.CreateInvite(Self.Core.From, Self.Destination, '', '', TIdSipRequest.DefaultMaxForwards);
  try
    Self.CheckCreateRequest(Self.Destination, Invite);
    Check(not Invite.FirstContact.Address.HasGrid,
          '"grid" parameter automatically added to Contact');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipInviteModule.TestCreateReInvite;
var
  Invite: TIdSipRequest;
begin
  Invite := Self.Module.CreateReInvite(Self.Dlg, 'foo', 'bar');
  try
    CheckEquals(MethodInvite, Invite.Method, 'Method');
    CheckEquals('foo',        Invite.Body, 'Body');
    CheckEquals('bar',        Invite.ContentType, 'Content-Type');

    CheckEquals(Self.Dlg.ID.CallID,
                Invite.CallID,
                'Call-ID');
    CheckEquals(Self.Dlg.ID.LocalTag,
                Invite.From.Tag,
                'From tag');
    CheckEquals(Self.Dlg.ID.RemoteTag,
                Invite.ToHeader.Tag,
                'To tag');
    CheckEquals(Self.Dlg.LocalSequenceNo,
                Invite.CSeq.SequenceNo,
                'CSeq sequence no');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipInviteModule.TestDoNotDisturb;
var
  SessionCount: Cardinal;
begin
  Self.Core.DoNotDisturb := true;
  Self.MarkSentResponseCount;
  SessionCount  := Self.Core.SessionCount;

  Self.ReceiveInvite;
  CheckResponseSent('No response sent when UA set to Do Not Disturb');

  CheckEquals(SIPTemporarilyUnavailable,
              Self.LastSentResponse.StatusCode,
              'Wrong response sent');
  CheckEquals(Self.Core.DoNotDisturbMessage,
              Self.LastSentResponse.StatusText,
              'Wrong status text');
  CheckEquals(SessionCount,
              Self.Core.SessionCount,
              'New session created despite Do Not Disturb');
end;

procedure TestTIdSipInviteModule.TestReceiveByeForUnmatchedDialog;
var
  Bye:      TIdSipRequest;
  Response: TIdSipResponse;
begin
  Bye := Self.Core.CreateRequest(MethodInvite, Self.Core.From, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    Bye.Method          := MethodBye;
    Bye.CSeq.SequenceNo := $deadbeef;
    Bye.CSeq.Method     := Bye.Method;

    Self.MarkSentResponseCount;

    Self.ReceiveRequest(Bye);

    CheckResponseSent('No response sent');
    Response := Self.LastSentResponse;
    CheckEquals(SIPCallLegOrTransactionDoesNotExist,
                Response.StatusCode,
                'Response Status-Code')

  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipInviteModule.TestReceiveByeWithoutTags;
var
  Bye:      TIdSipRequest;
  Response: TIdSipResponse;
begin
  Bye := Self.Core.CreateRequest(MethodInvite, Self.Core.From, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    Bye.Method          := MethodBye;
    Bye.From.Value      := Bye.From.Address.URI;     // strip the tag
    Bye.ToHeader.Value  := Bye.ToHeader.Address.URI; // strip the tag
    Bye.CSeq.SequenceNo := $deadbeef;
    Bye.CSeq.Method     := Bye.Method;

    Self.MarkSentResponseCount;

    Self.ReceiveRequest(Bye);

    CheckResponseSent('No response sent');
    Response := Self.LastSentResponse;
    CheckEquals(SIPCallLegOrTransactionDoesNotExist,
                Response.StatusCode,
                'Response Status-Code')
  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipInviteModule.TestReceiveInviteWithMultipleReplacesHeaders;
var
  BadRequest: TIdSipRequest;
begin
  BadRequest := Self.Invite.Copy as TIdSipRequest;
  try
    BadRequest.AddHeader(SupportedHeaderFull).Value := ExtensionReplaces;
    BadRequest.AddHeader(ReplacesHeader).Value := '1;from-tag=2;to-tag=3';
    BadRequest.AddHeader(ReplacesHeader).Value := '2;from-tag=3;to-tag=4';

    Self.MarkSentResponseCount;
    Self.ReceiveRequest(BadRequest);

    CheckResponseSent('No response sent');
    CheckEquals(SIPBadRequest,
                Self.LastSentResponse.StatusCode,
                'Unexpected response');
  finally
    BadRequest.Free;
  end;
end;

procedure TestTIdSipInviteModule.TestReceiveInviteWithNoContactHeader;
var
  BadRequest: TIdSipRequest;
begin
  BadRequest := Self.Invite.Copy as TIdSipRequest;
  try
    BadRequest.RemoveAllHeadersNamed(ContactHeaderFull);

    Self.MarkSentResponseCount;
    Self.ReceiveRequest(BadRequest);

    CheckResponseSent('No response sent');
    CheckEquals(SIPBadRequest,
                Self.LastSentResponse.StatusCode,
                'Unexpected response');
    CheckEquals(MissingContactHeader,
                Self.LastSentResponse.StatusText,
                'Unexpected response Status-Text');
  finally
    BadRequest.Free;
  end;
end;

procedure TestTIdSipInviteModule.TestRejectUnknownContentType;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;

  Self.Invite.ContentType := 'text/xml';

  Self.ReceiveInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPUnsupportedMediaType, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(AcceptHeader), 'No Accept header');
  CheckEquals(Self.ConvertListToHeader(Self.Module.AllowedContentTypes),
              Response.FirstHeader(AcceptHeader).Value,
              'Accept value');
end;

procedure TestTIdSipInviteModule.TestRemoveListener;
var
  Listener: TIdSipTestInviteModuleListener;
begin
  Listener := TIdSipTestInviteModuleListener.Create;
  try
    Self.Module.AddListener(Listener);
    Self.Module.RemoveListener(Listener);

    Self.ReceiveInvite;

    Check(not Listener.InboundCall,
          'First listener notified of inbound call, ergo not removed');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipInviteModule.TestReplaceCall;
var
  Session: TIdSipOutboundSession;
begin
  Session := Self.Module.ReplaceCall(Self.Invite, Self.Destination, '', '');

  Self.MarkSentRequestCount;
  Session.Send;
  CheckRequestSent('No request sent');
  Check(Self.LastSentRequest.HasHeader(ReplacesHeader),
        'No Replaces header, hence the session''s not a dialog replacer');
end;

//******************************************************************************
//* TestTIdSipOutboundBye                                                      *
//******************************************************************************
//* TestTIdSipOutboundBye Public methods ***************************************

procedure TestTIdSipOutboundBye.SetUp;
var
  OK: TIdSipResponse;
begin
  inherited SetUp;

  // For the purposes of this test, it doesn't matter whether the dialog is an
  // inbound or outbound dialog, nor does the particular value of the remote
  // end's Contact - as long as there IS a Contact. Obviously, the test uses
  // the local Contact, which in the field is completely wrong (except for
  // hairpinned calls), but it doesn't matter here.
  OK := TIdSipResponse.InResponseTo(Self.Invite, SIPOK, Self.Invite.FirstContact);
  try
    Self.Dialog := TIdSipDialog.CreateOutboundDialog(Self.Invite, OK, false);
  finally
    OK.Free;
  end;
end;

procedure TestTIdSipOutboundBye.TearDown;
begin
  Self.Dialog.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundBye Protected methods ************************************

procedure TestTIdSipOutboundBye.AdjustNameResolutionForInDialogActions(DestinationIP: String);
var
  RemoteTargetHost: String;
begin
  RemoteTargetHost := Self.Invite.FirstContact.Address.Host;
  Self.Locator.RemoveNameRecords(RemoteTargetHost);
  Self.Locator.AddA(RemoteTargetHost, DestinationIP);
end;

function TestTIdSipOutboundBye.CreateAction: TIdSipAction;
var
  Bye: TIdSipOutboundBye;
begin
  Bye := Self.Core.AddOutboundAction(TIdSipOutboundBye) as TIdSipOutboundBye;
//  Bye := TIdSipOutboundBye.Create(Self.Core);
  Bye.Dialog         := Self.Dialog;
  Bye.OriginalInvite := Self.Invite;
  Bye.AddActionListener(Self);
  Bye.Send;

  Result := Bye;
end;

//* TestTIdSipOutboundBye Published methods ************************************

procedure TestTIdSipOutboundBye.TestIsInbound;
begin
  Check(not Self.CreateAction.IsInbound,
        'Outbound BYE marked as an inbound action');
end;

procedure TestTIdSipOutboundBye.TestIsInvite;
begin
  Check(not Self.CreateAction.IsInvite,
        'Outbound BYE marked as an INVITE');
end;

procedure TestTIdSipOutboundBye.TestIsOptions;
begin
  Check(not Self.CreateAction.IsOptions,
        'Outbound BYE marked as an OPTIONS');
end;

procedure TestTIdSipOutboundBye.TestIsOwned;
begin
  Check(Self.CreateAction.IsOwned,
        'Outbound BYE not marked as an owned action');
end;

procedure TestTIdSipOutboundBye.TestIsRegistration;
begin
  Check(not Self.CreateAction.IsInbound,
        'Outbound BYE marked as a REGISTER');
end;

procedure TestTIdSipOutboundBye.TestIsSession;
begin
  Check(not Self.CreateAction.IsInbound,
        'Outbound BYE marked as a Session');
end;

//******************************************************************************
//* TestTIdSipOutboundCancel
//******************************************************************************
//* TestTIdSipOutboundCancel Protected methods *********************************

function TestTIdSipOutboundCancel.CreateAction: TIdSipAction;
var
  Cancel: TIdSipOutboundCancel;
begin
  // Given that this is a new Action, and that you can only CANCEL an INVITE
  // once, we must make Self.Invite look like a different INVITE. Otherwise, if
  // we try Cancel.Send the CANCEL will match an ongoing server transaction and
  // not send a request to the network.
  Self.Invite.LastHop.Branch := Self.Core.NextBranch;

  Cancel := Self.Core.AddOutboundAction(TIdSipOutboundCancel) as TIdSipOutboundCancel;
  Cancel.OriginalInvite := Self.Invite;
  Cancel.AddActionListener(Self);
  Cancel.Send;

  Result := Cancel;
end;

//* TestTIdSipOutboundCancel Published methods *********************************

procedure TestTIdSipOutboundCancel.TestAbandonAuthentication;
begin
  // You can't resubmit CANCELs, so it's a sign of a broken SIP implementation
  // if the targetted UA/proxy returns anything other than a 200 OK to your
  // CANCEL (like, say, a 401 Unauthorized or a 407 Proxy Authentication
  // Required). This test, which matches that of other objects, just shows
  // that Cancel objects terminate correctly.

  inherited TestAbandonAuthentication;
end;

procedure TestTIdSipOutboundCancel.TestAuthentication;
var
  Action:         TIdSipAction;
  AuthCreds:      TIdSipAuthorizationHeader;
  InitialRequest: TIdSipRequest;
begin
  Action := Self.CreateAction;

  InitialRequest := Action.InitialRequest.Copy as TIdSipRequest;
  try
    Self.ReceiveUnauthorized(WWWAuthenticateHeader, QopAuth);

    Self.MarkSentRequestCount;

    AuthCreds := Self.CreateAuthorization(Self.LastSentResponse);
    try
      Action.Resend(AuthCreds);
      CheckNoRequestSent('You cannot resubmit a CANCEL: Resend must be a no-op.');
    finally
      AuthCreds.Free;
    end;
  finally
    InitialRequest.Free;
  end;
end;

procedure TestTIdSipOutboundCancel.TestAuthenticationChallenge;
var
  Action:    TIdSipAction;
  AuthCreds: TIdSipAuthorizationHeader;
begin
  // This test only makes sense for OUTbound actions.
  if Self.IsInboundTest then Exit;

  Action := Self.CreateAction;

  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');

  Check(not Self.AuthenticationChallenged,
        'The TIdSipOutboundCancel notified its listeners of an authentication '
      + 'challeng about which they can do nothing.');

  Self.MarkSentRequestCount;

  AuthCreds := Self.CreateAuthorization(Self.LastSentResponse);
  try
    Action.Resend(AuthCreds);
  finally
    AuthCreds.Free;
  end;

  CheckNoRequestSent('You cannot resubmit a CANCEL: Resend must be a no-op.');
end;

procedure TestTIdSipOutboundCancel.TestIsInbound;
begin
  Check(not Self.CreateAction.IsInbound,
        'Outbound CANCEL marked as an inbound action');
end;

procedure TestTIdSipOutboundCancel.TestIsInvite;
begin
  Check(not Self.CreateAction.IsInvite,
        'Outbound CANCEL marked as an INVITE');
end;

procedure TestTIdSipOutboundCancel.TestIsOptions;
begin
  Check(not Self.CreateAction.IsOptions,
        'Outbound CANCEL marked as an OPTIONS');
end;

procedure TestTIdSipOutboundCancel.TestIsOwned;
begin
  Check(Self.CreateAction.IsOwned,
        'Outbound CANCEL not marked as an owned action');
end;

procedure TestTIdSipOutboundCancel.TestIsRegistration;
begin
  Check(not Self.CreateAction.IsInbound,
        'Outbound CANCEL marked as a REGISTER');
end;

procedure TestTIdSipOutboundCancel.TestIsSession;
begin
  Check(not Self.CreateAction.IsInbound,
        'Outbound BYE marked as a Session');
end;

procedure TestTIdSipOutboundCancel.TestMultipleAuthentication;
begin
  // This test makes no sense for a CANCEL. You can't issue multiple
  // authentication challenges to a CANCEL, because a CANCEL cannot be
  // resubmitted: you send out a CANCEL, you (erroneously) receive a
  // 401 Unauthorized, and you won't send out a new CANCEL, so you cannot
  // receive a second challenge.
  Check(true, 'This check simply stops "Empty Test" failures');
end;

procedure TestTIdSipOutboundCancel.TestResend;
var
  Action:    TIdSipAction;
  AuthCreds: TIdSipAuthorizationHeader;
begin
  Action := Self.CreateAction;

  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');
  AuthCreds := Self.CreateAuthorization(Self.LastSentResponse);
  try
    Self.MarkSentRequestCount;
    Action.Resend(AuthCreds);
    CheckNoRequestSent('You cannot resubmit a CANCEL: Resend must be a no-op.');
  finally
    AuthCreds.Free;
  end;
end;

procedure TestTIdSipOutboundCancel.TestResendBeforeSend;
var
  Action:          TIdSipAction;
  AuthCreds:       TIdSipAuthorizationHeader;
  ThrowawayAction: TIdSipAction;
begin
  // You cannot resubmit CANCELs, so there's really no sense in invoking
  // Cancel.Resend. This test merely demonstrates that the Cancel object's
  // behaviour matches those of other Actions, and that the Cancel really
  // doesn't allow resubmissions.

  // This looks very strange: We create an Action using the polymorphic
  // CreateAction. No surprise there. But CreateAction calls Send() on its
  // result, and for this test we don't want that. So we instantiate another
  // Action using the class type of the result of CreateAction.
  ThrowawayAction := Self.CreateAction;

  // Normally we wouldn't bother with receiving a response, but it allows us
  // to call CreateAuthorization later, without messing with junk.
  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');

  Action := Self.Core.AddOutboundAction(TIdSipActionClass(ThrowawayAction.ClassType));

  Self.MarkSentRequestCount;
  AuthCreds := Self.CreateAuthorization(Self.LastSentResponse);
  Action.Resend(AuthCreds);
  Self.CheckNoRequestSent('You cannot resubmit a CANCEL: Resend must be a no-op.');
end;

procedure TestTIdSipOutboundCancel.TestResendWithAuthForSameRealm;
begin
  // CANCELs MUST NOT be challenged, and if they are we mustn't resubmit them.
  // This test's behaviour would just copy TestResend anyway.
  Check(true, 'This check simply stops "Empty Test" failures');
end;

procedure TestTIdSipOutboundCancel.TestResendWithProxyAuth;
var
  Action:    TIdSipAction;
  ProxyAuth: TIdSipAuthorizationHeader;
begin
  Action := Self.CreateAction;
  Self.ReceiveUnauthorized(ProxyAuthenticateHeader, '');

  ProxyAuth := Self.CreateAuthorization(Self.LastSentResponse);
  try
    Self.MarkSentRequestCount;

    Action.Resend(ProxyAuth);
    CheckNoRequestSent('You cannot resubmit a CANCEL: Resend must be a no-op.');
  finally
    ProxyAuth.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipInboundInvite                                                    *
//******************************************************************************
//* TestTIdSipInboundInvite Public methods *************************************

procedure TestTIdSipInboundInvite.SetUp;
var
  Ok: TIdSipResponse;
begin
  inherited SetUp;

  Self.Module := Self.Core.ModuleFor(MethodInvite) as TIdSipInviteModule;

  Self.Dispatcher.TransportType := UdpTransport;
  Self.Invite.LastHop.Transport := Self.Dispatcher.TransportType;
  Ok := TIdSipResponse.InResponseTo(Self.Invite, SIPOK);
  try
    Ok.ToHeader.Tag := Self.Core.NextTag;
    Self.Dialog := TIdSipDialog.CreateInboundDialog(Self.Invite, Ok, true);
  finally
    Ok.Free;
  end;

  Self.Answer         := '';
  Self.Failed         := false;
  Self.OnSuccessFired := false;

  Self.InviteAction := TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, Self.Binding);
  Self.InviteAction.AddListener(Self);
end;

procedure TestTIdSipInboundInvite.TearDown;
begin
  Self.InviteAction.Free;
  Self.Dialog.Free;

  inherited TearDown;
end;

//* TestTIdSipInboundInvite Private methods ************************************

procedure TestTIdSipInboundInvite.CheckAck(InviteAction: TIdSipInboundInvite);
var
  Ack:          TIdSipRequest;
  RemoteDialog: TIdSipDialog;
begin
  InviteAction.Accept('', '');

  RemoteDialog := TIdSipDialog.CreateOutboundDialog(InviteAction.InitialRequest,
                                                    Self.LastSentResponse,
                                                    false);
  try
    RemoteDialog.ReceiveRequest(InviteAction.InitialRequest);
    RemoteDialog.ReceiveResponse(Self.LastSentResponse);

    Ack := Self.Module.CreateAck(RemoteDialog);
    try
      Check(InviteAction.Match(Ack),
            'ACK must match the InviteAction');
    finally
      Ack.Free;
    end;
  finally
    RemoteDialog.Free;
  end;
end;

procedure TestTIdSipInboundInvite.CheckAckWithDifferentCSeq(InviteAction: TIdSipInboundInvite);
var
  Ack:          TIdSipRequest;
  RemoteDialog: TIdSipDialog;
begin
  InviteAction.Accept('', '');

  RemoteDialog := TIdSipDialog.CreateOutboundDialog(InviteAction.InitialRequest,
                                                    Self.LastSentResponse,
                                                    false);
  try
    RemoteDialog.ReceiveRequest(InviteAction.InitialRequest);
    RemoteDialog.ReceiveResponse(Self.LastSentResponse);

    Ack := Self.Module.CreateAck(RemoteDialog);
    try
      Ack.CSeq.Increment;
      Check(not InviteAction.Match(Ack),
            'ACK must not match the InviteAction');
    finally
      Ack.Free;
    end;
  finally
    RemoteDialog.Free;
  end;
end;

procedure TestTIdSipInboundInvite.OnFailure(InviteAgent: TIdSipInboundInvite);
begin
  Self.Failed := true;
end;

procedure TestTIdSipInboundInvite.OnSuccess(InviteAgent: TIdSipInboundInvite;
                                            Ack: TIdSipMessage);
begin
  Self.Answer         := Ack.Body;
  Self.AnswerMimeType := Ack.ContentType;
  Self.OnSuccessFired := true;
end;

procedure TestTIdSipInboundInvite.ReceiveAck;
var
  Ack: TIdSipRequest;
begin
  Ack := Self.InviteAction.InitialRequest.AckFor(Self.LastSentResponse);
  try
    Self.InviteAction.ReceiveRequest(Ack, Self.Binding);
  finally
    Ack.Free;
  end;
end;

//* TestTIdSipInboundInvite Published methods **********************************

procedure TestTIdSipInboundInvite.TestAccept;
var
  Body:        String;
  ContentType: String;
  Response:    TIdSipResponse;
begin
  Self.MarkSentResponseCount;

  Body        := 'foo';
  ContentType := 'bar';
  Self.InviteAction.Accept(Body, ContentType);

  CheckResponseSent('No response sent');
  Response := Self.LastSentResponse;
  CheckEquals(SIPOK,
              Response.StatusCode,
              'Unexpected Status-Code');

  Check(Response.From.HasTag,                  'No From tag');
  Check(Response.ToHeader.HasTag,              'No To tag');
  Check(Response.HasHeader(ContactHeaderFull), 'No Contact header');

  Check(Response.ToHeader.HasTag,
        'To (local) tag missing');
  CheckEquals(Body,
              Response.Body,
              'Body');
  CheckEquals(ContentType,
              Response.ContentType,
              'Content-Type');
end;

procedure TestTIdSipInboundInvite.TestCancelAfterAccept;
var
  Cancel:         TIdSipRequest;
  CancelResponse: TIdSipResponse;
  InviteResponse: TIdSipResponse;
begin
  // <--- INVITE ---
  //  --- 200 OK --->
  // <---  ACK   ---
  // <--- CANCEL ---
  //  --- 200 OK --->

  Self.InviteAction.Accept('', '');

  Self.MarkSentResponseCount;
  Cancel := Self.Invite.CreateCancel;
  try
    Self.InviteAction.ReceiveRequest(Cancel, Self.Binding);
  finally
    Cancel.Free;
  end;

  Check(not Self.InviteAction.IsTerminated,
        'Action terminated');
  Check(not Self.Failed,
        'Listeners notified of (false) failure');

  CheckResponseSent('No response sent');

  CancelResponse := Self.LastSentResponse;
  InviteResponse := Self.SecondLastSentResponse;

  CheckEquals(SIPOK,
              CancelResponse.StatusCode,
              'Unexpected Status-Code for CANCEL response');
  CheckEquals(MethodCancel,
              CancelResponse.CSeq.Method,
              'Unexpected CSeq method for CANCEL response');

  CheckEquals(SIPOK,
              InviteResponse.StatusCode,
              'Unexpected Status-Code for INVITE response');
  CheckEquals(MethodInvite,
              InviteResponse.CSeq.Method,
              'Unexpected CSeq method for INVITE response');
end;

procedure TestTIdSipInboundInvite.TestCancelBeforeAccept;
var
  Cancel: TIdSipRequest;
begin
  // <---         INVITE         ---
  // <---         CANCEL         ---
  //  ---         200 OK         ---> (for the CANCEL)
  //  --- 487 Request Terminated ---> (for the INVITE)
  // <---           ACK          ---

  Cancel := Self.Invite.CreateCancel;
  try
    Self.InviteAction.ReceiveRequest(Cancel, Self.Binding);
  finally
    Cancel.Free;
  end;

  Check(Self.InviteAction.IsTerminated,
        Self.ClassName + ': Action not marked as terminated');
  Check(Self.Failed,
        Self.ClassName + ': Listeners not notified of failure');
end;

procedure TestTIdSipInboundInvite.TestInviteWithNoOffer;
var
  Ack:      TIdSipRequest;
  Action:   TIdSipInboundInvite;
  Answer:   String;
  MimeType: String;
  Offer:    String;
begin
  // <---       INVITE        ---
  //  --- 200 OK (with offer) --->
  // <---  ACK (with answer)  ---

  Answer   := TIdSipTestResources.BasicSDP('4.3.2.1');
  Offer    := TIdSipTestResources.BasicSDP('1.2.3.4');
  MimeType := 'application/sdp';

  Self.Invite.Body := '';
  Self.Invite.RemoveAllHeadersNamed(ContentTypeHeaderFull);

  Action := TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, Self.Binding);
  Action.AddListener(Self);

  Self.MarkSentResponseCount;
  Action.Accept(Offer,
                MimeType);

  Self.CheckResponseSent('No 2xx sent');
  CheckEquals(Offer,
              Self.LastSentResponse.Body,
              'Body of 2xx');
  CheckEquals(MimeType,
              Self.LastSentResponse.ContentType,
              'Content-Type of 2xx');

  Ack := Self.Invite.AckFor(Self.LastSentResponse);
  try
    Ack.Body                        := Answer;
    Ack.ContentDisposition.Handling := DispositionSession;
    Ack.ContentLength               := Length(Answer);
    Ack.ContentType                 := Self.LastSentResponse.ContentType;

    Action.ReceiveRequest(Ack, Self.Binding);
  finally
    Ack.Free;
  end;

  Check(Self.OnSuccessFired,
        'InviteAction never received the ACK');

  CheckEquals(Answer,
              Self.Answer,
              'ACK''s body');
  CheckEquals(Self.LastSentResponse.ContentType,
              Self.AnswerMimeType,
              'ACK''s Content-Type');
end;

procedure TestTIdSipInboundInvite.TestIsInbound;
begin
  Check(Self.InviteAction.IsInbound,
        Self.InviteAction.ClassName + ' not marked as inbound');
end;

procedure TestTIdSipInboundInvite.TestIsInvite;
begin
  Check(Self.InviteAction.IsInvite,
        Self.InviteAction.ClassName + ' not marked as a Invite');
end;

procedure TestTIdSipInboundInvite.TestIsOptions;
begin
  Check(not Self.InviteAction.IsOptions,
        Self.InviteAction.ClassName + ' marked as an Options');
end;

procedure TestTIdSipInboundInvite.TestIsOutbound;
begin
  Check(not Self.InviteAction.IsOutbound,
        Self.InviteAction.ClassName + ' marked as outbound');
end;

procedure TestTIdSipInboundInvite.TestIsOwned;
begin
  Check(Self.InviteAction.IsOwned,
        Self.InviteAction.ClassName + ' not marked as being owned');
end;

procedure TestTIdSipInboundInvite.TestIsRegistration;
begin
  Check(not Self.InviteAction.IsRegistration,
        Self.InviteAction.ClassName + ' marked as a Registration');
end;

procedure TestTIdSipInboundInvite.TestIsSession;
begin
  Check(not Self.InviteAction.IsSession,
        Self.InviteAction.ClassName + ' marked as a Session');
end;

procedure TestTIdSipInboundInvite.TestLastResponse;
begin
  Self.InviteAction.Ring;
  Check(Self.InviteAction.LastResponse.Equals(Self.LastSentResponse),
        'Sent 180 Ringing not stored in LastResponse');

  Self.InviteAction.Accept('', '');
  Check(Self.InviteAction.LastResponse.Equals(Self.LastSentResponse),
        'Sent 200 OK not stored in LastResponse');

  Self.InviteAction.ResendOk;
  Check(Self.InviteAction.LastResponse.Equals(Self.LastSentResponse),
        'Re-sent 200 OK not stored in LastResponse');
end;

procedure TestTIdSipInboundInvite.TestLocalGruu;
var
  OkGrid:   String;
  RingGrid: String;
begin
  Self.InviteAction.UseGruu := true;

  Self.MarkSentResponseCount;
  Self.InviteAction.Ring;
  CheckResponseSent('No 180 Ringing sent');

  Check(Self.LastSentResponse.FirstContact.Address.HasGrid,
        '180 Ringing''s Contact address has no "grid" parameter');
  CheckEquals(Self.LastSentResponse.FirstContact.AsString,
              Self.InviteAction.LocalGruu.AsString,
              'InviteAction''s LocalGruu doesn''t match Contact in 180 Ringing');
  RingGrid := Self.LastSentResponse.FirstContact.Address.Grid;

  Self.MarkSentResponseCount;
  Self.InviteAction.Accept('', '');
  CheckResponseSent('No 200 OK sent');

  Check(Self.LastSentResponse.FirstContact.Address.HasGrid,
        '200 OK''s Contact address has no "grid" parameter');
  CheckEquals(Self.LastSentResponse.FirstContact.AsString,
              Self.InviteAction.LocalGruu.AsString,
              'InviteAction''s LocalGruu doesn''t match Contact in 200 OK');
  OkGrid := Self.LastSentResponse.FirstContact.Address.Grid;

  CheckEquals(OkGrid, RingGrid, 'Two responses, with different GRUUs');
end;

procedure TestTIdSipInboundInvite.TestMatchAck;
begin
  Self.InviteAction.Accept('', '');

  Self.CheckAck(Self.InviteAction);
end;

procedure TestTIdSipInboundInvite.TestMatchAckToReInvite;
var
  Action: TIdSipInboundInvite;
begin
  // We want an in-dialog action
  Self.Invite.ToHeader.Tag := Self.Core.NextTag;

  Action := TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, Self.Binding);
  try
    Action.Accept('', '');

    Self.CheckAck(Action);
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundInvite.TestMatchAckToReInviteWithDifferentCSeq;
var
  Action: TIdSipInboundInvite;
begin
  // We want an in-dialog action
  Self.Invite.ToHeader.Tag := Self.Core.NextTag;

  Action := TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, Self.Binding);
  try
    Self.CheckAckWithDifferentCSeq(Action);
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundInvite.TestMatchAckWithDifferentCSeq;
begin
  Self.CheckAckWithDifferentCSeq(Self.InviteAction);
end;

procedure TestTIdSipInboundInvite.TestMatchInitialInvite;
begin
  Check(Self.InviteAction.Match(Self.InviteAction.InitialRequest),
        'InviteAction doesn''t match INVITE');
end;

procedure TestTIdSipInboundInvite.TestMethod;
begin
  CheckEquals(MethodInvite,
              Self.InviteAction.Method,
              'Inbound INVITE Method');
end;

procedure TestTIdSipInboundInvite.TestNotifyOfNetworkFailure;
var
  L1, L2: TIdSipTestInboundInviteListener;
begin
  L1 := TIdSipTestInboundInviteListener.Create;
  try
    L2 := TIdSipTestInboundInviteListener.Create;
    try
      Self.InviteAction.AddActionListener(L1);
      Self.InviteAction.AddActionListener(L2);

      Self.InviteAction.Accept('', '');
      Self.InviteAction.NetworkFailureSending(Self.InviteAction.InitialRequest);

      Check(Self.InviteAction.IsTerminated, 'Action not marked as terminated');
      Check(L1.NetworkFailed, 'TIdSipInboundInvite didn''t notify L1');
      Check(L2.NetworkFailed, 'TIdSipInboundInvite didn''t notify L2');
    finally
       Self.InviteAction.RemoveListener(L2);
       L2.Free;
    end;
  finally
     Self.InviteAction.RemoveListener(L1);
    L1.Free;
  end;
end;

procedure TestTIdSipInboundInvite.TestNotifyOfSuccess;
var
  L1, L2: TIdSipTestInboundInviteListener;
begin
  L1 := TIdSipTestInboundInviteListener.Create;
  try
    L2 := TIdSipTestInboundInviteListener.Create;
    try
      Self.InviteAction.AddListener(L1);
      Self.InviteAction.AddListener(L2);

      Self.InviteAction.Accept('', '');

      Self.ReceiveAck;

      Check(L1.Succeeded, 'TIdSipInboundInvite didn''t notify L1 of action success');
      Check(L2.Succeeded, 'TIdSipInboundInvite didn''t notify L2 of action success');
    finally
       Self.InviteAction.RemoveListener(L2);
       L2.Free;
    end;
  finally
     Self.InviteAction.RemoveListener(L1);
    L1.Free;
  end;
end;

procedure TestTIdSipInboundInvite.TestReceiveAck;
begin
  Self.InviteAction.Accept('', '');
  Self.ReceiveAck;
  Check(Self.InviteAction.IsTerminated, 'InviteAction not terminated');

  Self.MarkSentResponseCount;
  Self.DebugTimer.TriggerAllEventsUpToFirst(TIdSipActionsWait);
  CheckNoResponseSent('A 200 OK was sent after receiving an ACK');
end;

procedure TestTIdSipInboundInvite.TestReceiveResentAck;
var
  Ack:      TIdSipRequest;
  Listener: TIdSipTestInboundInviteListener;
begin
  Self.InviteAction.Accept('', '');

  Ack := Self.InviteAction.InitialRequest.AckFor(Self.LastSentResponse);
  try
    Self.InviteAction.ReceiveRequest(Ack, Self.Binding);

    Listener := TIdSipTestInboundInviteListener.Create;
    try
      Self.InviteAction.AddListener(Listener);

      Self.InviteAction.ReceiveRequest(Ack, Self.Binding);
      Check(not Listener.Succeeded, 'The InboundInvite renotified its listeners of success');
    finally
      Listener.Free;
    end;
  finally
    Ack.Free;
  end;
end;

procedure TestTIdSipInboundInvite.TestRedirectCall;
var
  Dest:         TIdSipAddressHeader;
  SentResponse: TIdSipResponse;
begin
  Self.MarkSentResponseCount;

  Dest := TIdSipAddressHeader.Create;
  try
    Dest.DisplayName := 'Wintermute';
    Dest.Address.Uri := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';

    Self.InviteAction.Redirect(Dest);
    CheckResponseSent('No response sent');

    SentResponse := Self.LastSentResponse;
    CheckEquals(SIPMovedTemporarily,
                SentResponse.StatusCode,
                'Wrong response sent');
    Check(SentResponse.HasHeader(ContactHeaderFull),
          'No Contact header');
    CheckEquals(Dest.DisplayName,
                SentResponse.FirstContact.DisplayName,
                'Contact display name');
    CheckEquals(Dest.Address.Uri,
                SentResponse.FirstContact.Address.Uri,
                'Contact address');

    Check(Self.InviteAction.IsTerminated,
          'Action didn''t terminate');
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipInboundInvite.TestRedirectCallPermanent;
var
  Dest:         TIdSipAddressHeader;
  SentResponse: TIdSipResponse;
begin
  Self.MarkSentResponseCount;

  Dest := TIdSipAddressHeader.Create;
  try
    Dest.DisplayName := 'Wintermute';
    Dest.Address.Uri := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';

    Self.InviteAction.Redirect(Dest, false);
    CheckResponseSent('No response sent');

    SentResponse := Self.LastSentResponse;
    CheckEquals(SIPMovedPermanently,
                SentResponse.StatusCode,
                'Wrong response sent');
    Check(SentResponse.HasHeader(ContactHeaderFull),
          'No Contact header');
    CheckEquals(Dest.DisplayName,
                SentResponse.FirstContact.DisplayName,
                'Contact display name');
    CheckEquals(Dest.Address.Uri,
                SentResponse.FirstContact.Address.Uri,
                'Contact address');

    Check(Self.InviteAction.IsTerminated,
          'Action didn''t terminate');
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipInboundInvite.TestRejectCallBusy;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;
  Self.InviteAction.RejectCallBusy;
  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPBusyHere,
              Response.StatusCode,
              'Unexpected Status-Code');
  Check(Self.InviteAction.IsTerminated,
        'Action not terminated');
end;

procedure TestTIdSipInboundInvite.TestRejectCallStatusCode;
const
  StatusCode = SIPNotFound;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;
  Self.InviteAction.RejectCall(StatusCode);
  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(StatusCode,
              Response.StatusCode,
              'Unexpected Status-Code');
  Check(Self.InviteAction.IsTerminated,
        'Action not terminated');
end;

procedure TestTIdSipInboundInvite.TestRejectCallStatusCodeAndText;
const
  StatusCode = SIPNotFound;
  StatusText = 'No such user';
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;
  Self.InviteAction.RejectCall(StatusCode, StatusText);
  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(StatusCode,
              Response.StatusCode,
              'Unexpected Status-Code');
  CheckEquals(StatusText,
              Response.StatusText,
              'Unexpected Status-Text');
  Check(Self.InviteAction.IsTerminated,
        'Action not terminated');
end;

procedure TestTIdSipInboundInvite.TestRemoveListener;
var
  L: TIdSipTestInboundInviteListener;
begin
  L := TIdSipTestInboundInviteListener.Create;
  try
    Self.InviteAction.AddListener(L);
    Self.InviteAction.RemoveListener(L);

    Self.InviteAction.Accept('', '');
    Self.ReceiveAck;

    Check(not L.Succeeded, 'Listener notified, hence not removed');
  finally
    L.Free;
  end;
end;

procedure TestTIdSipInboundInvite.TestResendOk;
var
  Ack:        TIdSipRequest;
  I:          Integer;
  OriginalOk: TIdSipResponse;
begin
  Self.InviteAction.Accept('', '');

  // We make sure that repeated calls to ResendOk, well, resend the OK.
  OriginalOk := TIdSipResponse.Create;
  try
    OriginalOk.Assign(Self.LastSentResponse);

    for I := 1 to 2 do begin
      Self.MarkSentResponseCount;
      Self.InviteAction.ResendOk;

      CheckResponseSent(IntToStr(I) + ': Response not resent');
      CheckEquals(SIPOK,
                  Self.LastSentResponse.StatusCode,
                  IntToStr(I) + ': Unexpected response code');
      Check(OriginalOk.Equals(Self.LastSentResponse),
            IntToStr(I) + ': Unexpected OK');
    end;
  finally
    OriginalOk.Free;
  end;

  // But once we receive an ACK, we don't want to resend the OK.
  Ack := Self.Invite.AckFor(Self.LastSentResponse);
  try
    Self.InviteAction.ReceiveRequest(Ack, Self.Binding);
  finally
    Ack.Free;
  end;

  Self.MarkSentResponseCount;
  Self.InviteAction.ResendOk;
  CheckNoResponseSent('The action sent an OK after it received an ACK');
end;

procedure TestTIdSipInboundInvite.TestRing;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;
  Self.InviteAction.Ring;

  CheckResponseSent('No ringing response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPRinging,
              Response.StatusCode,
              'Unexpected Status-Code');
  Check(Response.ToHeader.HasTag,
        'To header doesn''t have tag');
  Check(Response.ToHeader.HasTag,
        'To (local) tag');
end;

procedure TestTIdSipInboundInvite.TestRingWithGruu;
var
  Response: TIdSipResponse;
begin
  Self.InviteAction.UseGruu := true;

  Self.MarkSentResponseCount;
  Self.InviteAction.Ring;

  CheckResponseSent('No ringing response sent');

  Response := Self.LastSentResponse;
  Check(Response.HasHeader(SupportedHeaderFull),
        'Response lacks a Supported header');
  Check(Response.SupportsExtension(ExtensionGruu),
        'Supported header lacks indication of GRUU support');
  Check(Response.FirstContact.Address.HasGrid,
        '180 Ringing lacks a Contact with a "grid" parameter');
end;

procedure TestTIdSipInboundInvite.TestRingWithSuppressLocalResponses;
begin
  Self.InviteAction.SuppressLocalResponses := true;

  Self.MarkSentResponseCount;
  Self.InviteAction.Ring;
  CheckResponseSent('(Manually requested) ringing response suppressed');
end;

procedure TestTIdSipInboundInvite.TestSendProvisional;
const
  ReasonPhrase = 'Progress of some kind has been made';
  StatusCode   = SIPQueued;
begin
  Self.MarkSentResponseCount;
  Self.InviteAction.SendProvisional(StatusCode, ReasonPhrase);

  CheckResponseSent('No session progress response sent');

  CheckEquals(StatusCode,
              Self.LastSentResponse.StatusCode,
              'Unexpected Status-Code');
  CheckEquals(ReasonPhrase,
              Self.LastSentResponse.StatusText,
              'Unexpected Reason-Phrase');
end;

procedure TestTIdSipInboundInvite.TestSendProvisionalWithGruu;
var
  Response: TIdSipResponse;
begin
  Self.InviteAction.UseGruu := true;

  Self.InviteAction.Ring;

  Self.MarkSentResponseCount;
  Self.InviteAction.SendProvisional(SIPSessionProgress, RSSIPSessionProgress);

  CheckResponseSent('No session progress response sent');

  Response := Self.LastSentResponse;
  Check(Response.HasHeader(SupportedHeaderFull),
        'Response lacks a Supported header');
  Check(Response.SupportsExtension(ExtensionGruu),
        'Supported header in Response lacks indication of GRUU support');
  Check(Response.FirstContact.Address.HasGrid,
        'Response lacks a Contact with a "grid" parameter');
end;

procedure TestTIdSipInboundInvite.TestSendProvisionalWithSuppressLocalResponses;
var
  W: TIdSipActionsWait;
begin
  Self.InviteAction.SuppressLocalResponses := true;

  Self.MarkSentResponseCount;
  Self.InviteAction.SendProvisional(SIPSessionProgress, 'Progress');
  CheckResponseSent('(Manually requested) provisional response suppressed');

  W := Self.DebugTimer.LastEventScheduled(TIdSipActionsWait) as TIdSipActionsWait;

  if (W <> nil) then
    CheckNotEquals(TIdSipInboundInviteSessionProgress.ClassName, W.BlockType.ClassName,
                   'Automatic session progress responses scheduled');
end;

procedure TestTIdSipInboundInvite.TestSendTrying;
begin
  Self.MarkSentResponseCount;
  Self.InviteAction.SendTrying;

  CheckResponseSent('No trying response sent');

  CheckEquals(SIPTrying,
              Self.LastSentResponse.StatusCode,
              'Unexpected Status-Code');
end;

procedure TestTIdSipInboundInvite.TestSendTryingWithSuppressLocalResponses;
begin
  Self.InviteAction.SuppressLocalResponses := true;

  Self.MarkSentResponseCount;
  Self.InviteAction.SendTrying;

  CheckResponseSent('Trying response was suppressed');
end;

procedure TestTIdSipInboundInvite.TestTerminateAfterAccept;
var
  Ack: TIdSipRequest;
begin
  // This is a bit of an odd case. You receive an INVITE, and accept it,
  // sending a 200 OK. Then, before you receive the ACK from the remote party,
  // you change your mind and terminate the session. Since you've not received
  // the ACK, you can't send a BYE - the session's not established - and you
  // can't send a CANCEL - you've already sent a final response. The answer?
  // Wait until you do receive the ACK, and send a BYE immediately. Actually,
  // the owning session will send the BYE - the InboundInvite need do nothing
  // but report when the ACK arrives.

  Self.InviteAction.Accept('', '');

  Self.MarkSentResponseCount;
  Self.InviteAction.Terminate;

  CheckNoResponseSent('Response sent');
  Check(not Self.InviteAction.IsTerminated,
        'Action marked as terminated when it''s just terminatING');

  Ack := Self.InviteAction.InitialRequest.AckFor(Self.LastSentResponse);
  try
    Self.InviteAction.ReceiveRequest(Ack, Self.Binding);
  finally
    Ack.Free;
  end;

  Check(Self.InviteAction.IsTerminated,
        'Action not marked as terminated after receiving the ACK.');
end;

procedure TestTIdSipInboundInvite.TestTerminateBeforeAccept;
begin
  Self.MarkSentResponseCount;

  Self.InviteAction.Terminate;

  CheckResponseSent(Self.ClassName + ': No response sent');

  CheckEquals(SIPRequestTerminated,
              Self.LastSentResponse.StatusCode,
              Self.ClassName + ': Unexpected Status-Code');

  Check(Self.InviteAction.IsTerminated,
        Self.ClassName + ': Action not marked as terminated');
end;

procedure TestTIdSipInboundInvite.TestTerminateSignalled;
var
  L: TIdSipTestActionListener;
begin
  L := TIdSipTestActionListener.Create;
  try
    Self.InviteAction.AddActionListener(L);
    try
      Self.InviteAction.TimeOut;
      Check(L.Terminated,
            Self.ClassName + ': Listeners not notified of termination');
    finally
      Self.InviteAction.RemoveActionListener(L);
    end;
  finally
    L.Free;
  end;
end;

procedure TestTIdSipInboundInvite.TestTimeOut;
begin
  Self.MarkSentResponseCount;

  Self.InviteAction.TimeOut;

  CheckResponseSent('No response sent');

  CheckEquals(SIPRequestTerminated,
              Self.LastSentResponse.StatusCode,
              'Unexpected Status-Code');

  Check(Self.InviteAction.IsTerminated,
        'Action not marked as terminated');
  Check(Self.Failed,
        'Listeners not notified of failure');
end;

//******************************************************************************
//* TestTIdSipOutboundInvite                                                   *
//******************************************************************************
//* TestTIdSipOutboundInvite Public methods ************************************

procedure TestTIdSipOutboundInvite.SetUp;
begin
  inherited SetUp;

  Self.Core.AddListener(Self);

  // We create Self.Dialog in Self.OnDialogEstablished

  Self.SdpMimeType := 'application/sdp';

  Self.DroppedUnmatchedResponse := false;
  Self.InviteMimeType           := Self.SdpMimeType;
  Self.InviteOffer              := TIdSipTestResources.BasicSDP('1.2.3.4');
  Self.OnCallProgressFired      := false;
  Self.OnDialogEstablishedFired := false;
  Self.OnFailureFired           := false;
  Self.OnRedirectFired          := false;
  Self.OnSuccessFired           := false;
end;

procedure TestTIdSipOutboundInvite.TearDown;
begin
  Self.Dialog.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundInvite Protected methods *********************************

procedure TestTIdSipOutboundInvite.CheckSendDialogEstablishingRequestWithGruu;
var
  Invite: TIdSipOutboundInvite;
begin
  Self.UseGruu;

  Self.MarkSentRequestCount;
  Invite := Self.CreateAction as TIdSipOutboundInvite;
  CheckRequestSent(Invite.ClassName + ': No request sent');

  CheckEquals(MethodInvite,
              Self.LastSentRequest.Method,
              Invite.ClassName + ': Method of sent request');

  CheckEquals(Self.LastSentRequest.FirstContact.AsString,
              Invite.LocalGruu.AsString,
              'LocalGruu not set');
  Check(Invite.LocalGruu.Address.HasGrid,
        Invite.ClassName + ': Local GRUU doesn''t have a "grid" parameter');
end;

function TestTIdSipOutboundInvite.CreateInitialInvite: TIdSipOutboundInitialInvite;
begin
  Result := Self.Core.AddOutboundAction(TIdSipOutboundInitialInvite) as TIdSipOutboundInitialInvite;
  Result.Destination := Self.Destination;
  Result.MimeType    := Self.InviteMimeType;
  Result.Offer       := Self.InviteOffer;
  Result.Send;
end;

//* TestTIdSipOutboundInvite Private methods ***********************************

procedure TestTIdSipOutboundInvite.CheckReceiveFailed(StatusCode: Cardinal);
var
  InviteCount: Integer;
begin
  Self.CreateAction;

  InviteCount := Self.Core.CountOf(MethodInvite);
  Self.ReceiveResponse(StatusCode);

  Check(Self.OnFailureFired,
        'OnFailure didn''t fire after receiving a '
      + IntToStr(StatusCode) + ' response');
  Check(Self.Core.CountOf(MethodInvite) < InviteCount,
        'Invite action not destroyed after receiving a '
      + IntToStr(StatusCode) + ' response');
end;

procedure TestTIdSipOutboundInvite.CheckReceiveOk(StatusCode: Cardinal);
begin
  Self.CreateAction;
  Self.ReceiveResponse(StatusCode);

  Check(Self.OnSuccessFired,
        'OnSuccess didn''t fire after receiving a '
      + IntToStr(StatusCode) + ' response');
end;

procedure TestTIdSipOutboundInvite.CheckReceiveProvisional(StatusCode: Cardinal);
begin
  Self.CreateAction;
  Self.ReceiveResponse(StatusCode);

  Check(Self.OnCallProgressFired,
        'OnCallProgress didn''t fire after receiving a '
      + IntToStr(StatusCode) + ' response');
end;

procedure TestTIdSipOutboundInvite.CheckReceiveRedirect(StatusCode: Cardinal);
begin
  Self.CreateAction;

  Self.ReceiveResponse(StatusCode);

  Check(Self.OnRedirectFired,
        'OnRedirect didn''t fire after receiving a '
      + IntToStr(StatusCode) + ' response');
end;

function TestTIdSipOutboundInvite.CreateArbitraryDialog: TIdSipDialog;
var
  Response: TIdSipResponse;
begin
  Self.Invite.RequestUri := Self.Destination.Address;
  Response := Self.Core.CreateResponse(Self.Invite, SIPOK);
  try
    Result := TIdSipDialog.CreateInboundDialog(Self.Invite, Response, false);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.OnAddAction(UserAgent: TIdSipAbstractCore;
                                               Action: TIdSipAction);
begin
  // Do nothing.
end;

procedure TestTIdSipOutboundInvite.OnCallProgress(InviteAgent: TIdSipOutboundInvite;
                                                  Response: TIdSipResponse);
begin
  Self.OnCallProgressFired := true;
end;

procedure TestTIdSipOutboundInvite.OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                                       NewDialog: TidSipDialog);
begin
  Self.Dialog := NewDialog.Copy;
  InviteAgent.Dialog := Self.Dialog;

  Self.OnDialogEstablishedFired := true;
  Self.ToHeaderTag := NewDialog.ID.RemoteTag;
end;

procedure TestTIdSipOutboundInvite.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                                             Message: TIdSipMessage;
                                                             Binding: TIdConnectionBindings);
begin
  Self.DroppedUnmatchedResponse := true;
end;

procedure TestTIdSipOutboundInvite.OnFailure(Action: TIdSipAction;
                                             Response: TIdSipResponse;
                                             const Reason: String);
begin
  Self.OnFailureFired := true;
end;

procedure TestTIdSipOutboundInvite.OnRedirect(Action: TIdSipAction;
                                              Response: TIdSipResponse);
begin
  Self.OnRedirectFired := true;
end;

procedure TestTIdSipOutboundInvite.OnRemoveAction(UserAgent: TIdSipAbstractCore;
                                                  Action: TIdSipAction);
begin
  // Do nothing.
end;

procedure TestTIdSipOutboundInvite.OnSuccess(Action: TIdSipAction;
                                             Msg: TIdSipMessage);
begin
  Self.OnSuccessFired := true;
end;

//* TestTIdSipOutboundInvite Published methods *********************************

procedure TestTIdSipOutboundInvite.TestAbandonAuthentication;
var
  Action: TIdSipOutboundInvite;
begin
  // This test only makes sense for OUTbound actions.
  if Self.IsInboundTest then Exit;

  Self.MarkSentRequestCount;
  Action := Self.CreateAction as TIdSipOutboundInvite;
  CheckRequestSent(Self.ClassName + ': Action didn''t send a request');

  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');

  Action.Terminate;
  Check(Action.IsTerminated,
        Self.ClassName + ': Action not terminated');
end;

procedure TestTIdSipOutboundInvite.TestAddListener;
var
  L1, L2: TIdSipTestInviteListener;
  Invite: TIdSipOutboundInvite;
begin
  Self.MarkSentRequestCount;
  Invite := Self.CreateAction as TIdSipOutboundInvite;
  CheckRequestSent(Invite.ClassName + ': No INVITE sent');

  L1 := TIdSipTestInviteListener.Create;
  try
    L2 := TIdSipTestInviteListener.Create;
    try
      Invite.AddInviteListener(L1);
      Invite.AddInviteListener(L2);

      Self.ReceiveRinging(Self.LastSentRequest);

      Check(L1.CallProgress, 'L1 not informed of call progress');
      Check(L2.CallProgress, 'L2 not informed of call progress');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestAnswerInAck;
var
  Invite:      TIdSipOutboundInvite;
begin
  //  ---       INVITE        --->
  // <--- 200 OK (with offer) ---
  //  ---  ACK (with answer)  --->

  Self.InviteOffer    := '';
  Self.InviteMimeType := '';
  Invite := Self.CreateAction as TIdSipOutboundInvite;

  // Sanity check
  CheckEquals('',
              Self.LastSentRequest.Body,
              Invite.ClassName + ': You just sent an INVITE with a body!');

  Invite.Offer    := TIdSipTestResources.BasicSDP('1.2.3.4');
  Invite.MimeType := Self.SdpMimeType;

  Self.MarkSentAckCount;
  Self.ReceiveOkWithBody(Invite.InitialRequest,
                         TIdSipTestResources.BasicSDP('4.3.2.1'),
                         Invite.MimeType);

  CheckAckSent(Invite.ClassName + ': No ACK sent');
  CheckEquals(Invite.Offer,
              Self.LastSentAck.Body,
              Invite.ClassName + ': Incorrect answer');
  CheckEquals(Invite.MimeType,
              Self.LastSentAck.ContentType,
              Invite.ClassName + ': Incorrect answer type');
end;

procedure TestTIdSipOutboundInvite.TestCancelAfterAccept;
var
  OutboundInvite: TIdSipOutboundInvite;
begin
  OutboundInvite := Self.CreateAction as TIdSipOutboundInvite;

  Self.ReceiveOk(Self.LastSentRequest);

  Self.MarkSentRequestCount;

  OutboundInvite.Cancel;

  CheckNoRequestSent('Action sent a CANCEL for a fully established call');
end;

procedure TestTIdSipOutboundInvite.TestCancelBeforeAccept;
var
  Invite:            TIdSipRequest;
  InviteCount:       Integer;
  OutboundInvite:    TIdSipOutboundInvite;
  RequestTerminated: TIdSipResponse;
begin
  //  ---         INVITE         --->
  // <---       180 Ringing      ---
  //  ---         CANCEL         --->
  // <---         200 OK         ---  (for the CANCEL)
  // <--- 487 Request Terminated ---  (for the INVITE)
  //  ---           ACK          --->

  //  ---         INVITE         --->
  OutboundInvite := Self.CreateAction as TIdSipOutboundInvite;

  InviteCount := Self.Core.CountOf(MethodInvite);
  Invite := TIdSipRequest.Create;
  try
    Invite.Assign(Self.LastSentRequest);
    // Note that Invite's To header has no tag because we haven't established
    // a dialog.
    RequestTerminated := TIdSipResponse.InResponseTo(Invite, SIPRequestTerminated);
    try
      // <---       180 Ringing      ---
      Self.ReceiveRinging(Invite);

      Check(Self.OnDialogEstablishedFired,
            Self.ClassName + ': No dialog established');

      // Now that we have established a dialog, the Request Terminated response
      // will contain that dialog ID.
      RequestTerminated.ToHeader.Tag := Self.ToHeaderTag;

      Self.MarkSentRequestCount;

      //  ---         CANCEL         --->
      OutboundInvite.Cancel;

      CheckRequestSent(Self.ClassName + ': No CANCEL sent');
      CheckEquals(MethodCancel,
                  Self.LastSentRequest.Method,
                  Self.ClassName + ': The request sent wasn''t a CANCEL');
      Check(not OutboundInvite.IsTerminated,
            Self.ClassName + ': No Request Terminated received means no termination');

      // <---         200 OK         ---  (for the CANCEL)
      Self.ReceiveOk(Self.LastSentRequest);

      // <--- 487 Request Terminated ---  (for the INVITE)
      //  ---           ACK          --->
      Self.MarkSentACKCount;
      Self.ReceiveResponse(RequestTerminated);

      CheckAckSent(Self.ClassName + ': No ACK sent');

      Check(Self.Core.CountOf(MethodInvite) < InviteCount,
            Self.ClassName + ': Action not terminated');
    finally
      RequestTerminated.Free;
    end;
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestCancelBeforeProvisional;
var
  Invite:            TIdSipRequest;
  InviteCount:       Integer;
  OutboundInvite:    TIdSipOutboundInvite;
  RequestTerminated: TIdSipResponse;
begin
  //  ---         INVITE         --->
  //  (UAC initiates cancel, but no provisional response = don't send CANCEL yet.)
  // <---       180 Ringing      ---
  // (Ah! A provisional response! Let's send that pending CANCEL)
  //  ---         CANCEL         --->
  // <---         200 OK         ---  (for the CANCEL)
  // <--- 487 Request Terminated ---  (for the INVITE)
  //  ---           ACK          --->

  //  ---         INVITE         --->
  OutboundInvite := Self.CreateAction as TIdSipOutboundInvite;

  InviteCount := Self.Core.CountOf(MethodInvite);
  Invite := TIdSipRequest.Create;
  try
    Invite.Assign(Self.LastSentRequest);
    // Note that Invite's To header has no tag because we haven't established
    // a dialog. Therefore the RequestTerminated won't match the INVITE's
    // dialog - we have to wait until the action receives the 180 Ringing before
    // we can set the To tag.
    RequestTerminated := TIdSipResponse.InResponseTo(Invite, SIPRequestTerminated);
    try
      Self.MarkSentRequestCount;

      OutboundInvite.Cancel;

      CheckNoRequestSent('CANCEL sent before the session receives a '
                       + 'provisional response');

      Check(not OutboundInvite.IsTerminated,
            'No Request Terminated received means no termination');

     // <---       180 Ringing      ---
     //  ---         CANCEL         --->
     Self.ReceiveRinging(Self.LastSentRequest);
     Check(Self.OnDialogEstablishedFired,
           'No dialog established');
     // Now that we have the remote tag we can:
     RequestTerminated.ToHeader.Tag := Self.ToHeaderTag;

      // <---         200 OK         ---  (for the CANCEL)
      Self.ReceiveOk(Self.LastSentRequest);

      // <--- 487 Request Terminated ---  (for the INVITE)
      //  ---           ACK          --->

      Self.MarkSentACKCount;
      Self.ReceiveResponse(RequestTerminated);

      CheckAckSent('No ACK sent');

      Check(Self.Core.CountOf(MethodInvite) < InviteCount,
            'Action not terminated');
    finally
      RequestTerminated.Free;
    end;
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestCancelReceiveInviteOkBeforeCancelOk;
var
  Action:    TIdSipOutboundInvite;
  Cancel:    TIdSipRequest;
  ClassName: String;
  Invite:    TIdSipRequest;
begin
  //  ---          INVITE         --->
  // <---        100 Trying       ---
  //  ---          CANCEL         --->
  // <--- 200 OK (for the INVITE) ---
  //  ---           ACK           --->
  // <--- 200 OK (for the CANCEL) ---
  //  ---           BYE           --->
  // <---   200 OK (for the BYE)  ---

  Action := Self.CreateAction as TIdSipOutboundInvite;
  ClassName := Action.ClassName;

  Invite := TIdSipRequest.Create;
  try
    Cancel := TIdSipRequest.Create;
    try
      Invite.Assign(Self.LastSentRequest);
      Self.ReceiveTrying(Invite);

      Action.Cancel;
      Cancel.Assign(Self.LastSentRequest);

      Self.MarkSentAckCount;
      Self.MarkSentRequestCount;
      Self.ReceiveOk(Invite);
      Self.ReceiveOk(Cancel);

      CheckRequestSent(ClassName + ': No request sent to terminate the cancelled session');
      CheckEquals(MethodBye,
                  Self.LastSentRequest.Method,
                  ClassName + ': Terminating request');

      CheckAckSent(ClassName + ': No ACK sent in response to the 2xx');
      CheckEquals(Invite.Body,
                  Self.LastSentAck.Body,
                  ClassName + ': ACK body');
      CheckEquals(Invite.ContentType,
                  Self.LastSentAck.ContentType,
                  ClassName + ': ACK Content-Type');
      Check(Invite.ContentDisposition.Equals(Self.LastSentAck.ContentDisposition),
            ClassName + ': ACK Content-Disposition');
    finally
      Cancel.Free;
    end;
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestCancelWithOutboundProxy;
const
  ProxyUri = 'sip:proxy.tessier-ashpool.co.luna';
var
  Invite: TIdSipAction;
  Uri:    TIdSipUri;
begin
  Uri := TIdSipUri.Create(ProxyUri);
  try
    Self.Core.DefaultRoutePath.AddRoute(Uri);

    Invite := Self.CreateAction;
    Self.ReceiveTrying(Self.LastSentRequest);

    Self.MarkSentRequestCount;
    Invite.Terminate;
    CheckRequestSent(Self.ClassName + ': No CANCEL sent');
    CheckEquals(MethodCancel, Self.LastSentRequest.Method, Self.ClassName + ': Unexpected request sent');

    Check(Self.LastSentRequest.HasRoute, Self.ClassName + ': Route header not added to CANCEL of (out-of-dialog) INVITE');
    Check(Invite.InitialRequest.Route.Equals(Self.LastSentRequest.Route),
          Self.ClassName + ': RFC 3261, section 9.1: To support stateless proxies, the CANCEL MUST have the same route path as the INVITE it''s cancelling');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestInviteTwice;
var
  Invite: TIdSipAction;
begin
  Invite := Self.CreateAction;

  try
    Invite.Send;
    Fail('Failed to bail out calling Invite a 2nd time');
  except
    on EIdSipTransactionUser do;
  end;
end;

procedure TestTIdSipOutboundInvite.TestIsInvite;
begin
  Check(Self.CreateAction.IsInvite, 'INVITE action not marked as such');
end;

procedure TestTIdSipOutboundInvite.TestIsOwned;
var
  Invite: TIdSipAction;
begin
  Invite := Self.CreateAction;

  Check(Invite.IsOwned,
        Invite.ClassName + ' not marked as being owned');
end;

procedure TestTIdSipOutboundInvite.TestMatchOwnAck;
var
  Action:    TIdSipAction;
  ClassName: String;
begin
  Action := Self.CreateAction;
  ClassName := Action.ClassName;

  Self.MarkSentAckCount;
  Self.ReceiveOk(Self.LastSentRequest);
  CheckAckSent(ClassName + ': No ACK sent');
end;

procedure TestTIdSipOutboundInvite.TestMethod;
begin
  CheckEquals(MethodInvite,
              Self.CreateAction.Method,
              'Outbound INVITE Method');
end;

procedure TestTIdSipOutboundInvite.TestOfferInInvite;
var
  Action:    TIdSipAction;
  ClassName: String;
begin
  //  ---    INVITE (with offer)   --->
  // <---   200 OK (with answer)   ---
  //  --- ACK (with copy of offer) --->

  Self.MarkSentRequestCount;
  Action := Self.CreateAction;
  ClassName := Action.ClassName;
  CheckRequestSent(ClassName + ': No initial INVITE sent');

  CheckEquals(Self.InviteOffer,
              Self.LastSentRequest.Body,
              ClassName + ': Body of INVITE');
  CheckEquals(Self.InviteMimeType,
              Self.LastSentRequest.ContentType,
              ClassName + ': Content-Type of INVITE');

  Self.MarkSentAckCount;
  Self.ReceiveOkWithBody(Self.LastSentRequest,
                         TIdSipTestResources.BasicSDP('4.3.2.1'),
                         SdpMimeType);

  CheckAckSent(ClassName + ': No ACK sent');
  CheckEquals(Self.LastSentRequest.Body,
              Self.LastSentAck.Body,
              ClassName + ': Body of ACK doesn''t match INVITE');
  CheckEquals(Self.LastSentRequest.ContentType,
              Self.LastSentAck.ContentType,
              ClassName + ': Content-Type of ACK doesn''t match INVITE');
end;

procedure TestTIdSipOutboundInvite.TestReceive2xxSchedulesTransactionCompleted;
var
  Invite: TIdSipAction;
begin
  // RFC 3261, section 13.2.2.4 says
  //   The UAC core considers the INVITE transaction completed 64*T1 seconds
  //   after the reception of the first 2xx response.  At this point all the
  //   early dialogs that have not transitioned to established dialogs are
  //   terminated.  Once the INVITE transaction is considered completed by
  //   the UAC core, no more new 2xx responses are expected to arrive.
  //
  // This test makes sure we don't schedule this when we send the INVITE.

  Invite := Self.CreateAction;
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionsWait);

  Check(not Invite.IsTerminated,
        'OutboundInvite terminated prematurely: it incorrectly scheduled '
      + 'a TIdSipOutboundInviteTransactionComplete');

  Self.ReceiveOk(Self.LastSentRequest);

  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionsWait);

  Check(Invite.IsTerminated,
        'OutboundInvite didn''t schedule a TIdSipOutboundInviteTransactionComplete');
end;

procedure TestTIdSipOutboundInvite.TestReceiveProvisional;
var
  StatusCode: Integer;
begin
  StatusCode := SIPLowestProvisionalCode;
//  for StatusCode := SIPLowestProvisionalCode to SIPHighestProvisionalCode do
    Self.CheckReceiveProvisional(StatusCode);
end;

procedure TestTIdSipOutboundInvite.TestReceiveGlobalFailed;
var
  StatusCode: Integer;
begin
  StatusCode := SIPLowestGlobalFailureCode;
//  for StatusCode := SIPLowestGlobalFailureCode to SIPHighestGlobalFailureCode do
    Self.CheckReceiveFailed(StatusCode);
end;

procedure TestTIdSipOutboundInvite.TestReceiveOk;
var
  StatusCode: Integer;
begin
  StatusCode := SIPLowestOkCode;
//  for StatusCode := SIPLowestOkCode to SIPHighestOkCode do
    Self.CheckReceiveOk(StatusCode);
end;

procedure TestTIdSipOutboundInvite.TestReceiveRedirect;
var
  StatusCode: Integer;
begin
  StatusCode := SIPLowestRedirectionCode;
//  for StatusCode := SIPLowestRedirectionCode to SIPHighestRedirectionCode do
    Self.CheckReceiveRedirect(StatusCode);
end;

procedure TestTIdSipOutboundInvite.TestReceiveRequestFailed;
var
  StatusCode: Integer;
begin
  StatusCode := SIPLowestFailureCode;

//  for StatusCode := SIPLowestFailureCode to SIPUnauthorized - 1 do
    Self.CheckReceiveFailed(StatusCode);
{
  for StatusCode := SIPUnauthorized + 1 to SIPProxyAuthenticationRequired - 1 do
    Self.CheckReceiveFailed(StatusCode);

  for StatusCode := SIPProxyAuthenticationRequired + 1 to SIPHighestFailureCode do
    Self.CheckReceiveFailed(StatusCode);
}
end;

procedure TestTIdSipOutboundInvite.TestReceiveRequestFailedAfterAckSent;
var
  InviteRequest: TIdSipRequest;
begin
  //  ---          INVITE         --->
  // <---          200 OK         ---
  //  ---           ACK           --->
  // <--- 503 Service Unavailable ---

  // This situation should never arise: the remote end's sending a failure
  // response to a request it has already accepted. Still, I've seen it happen
  // once before...

  Self.CreateAction;

  InviteRequest := TIdSipRequest.Create;
  try
    InviteRequest.Assign(Self.LastSentRequest);

    Self.MarkSentAckCount;
    Self.ReceiveOk(InviteRequest);
    CheckAckSent('No ACK sent');

    Self.ReceiveServiceUnavailable(InviteRequest);

    Check(Self.DroppedUnmatchedResponse,
          'Invite action didn''t terminate, so the Transaction-User core '
        + 'didn''t drop the message');
  finally
    InviteRequest.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestReceiveServerFailed;
var
  StatusCode: Integer;
begin
  StatusCode := SIPLowestServerFailureCode;
//  for StatusCode := SIPLowestServerFailureCode to SIPHighestServerFailureCode do
    Self.CheckReceiveFailed(StatusCode);
end;

procedure TestTIdSipOutboundInvite.TestRemoveListener;
var
  L1, L2: TIdSipTestInviteListener;
  Invite: TIdSipOutboundInvite;
begin
  Invite := Self.CreateAction as TIdSipOutboundInvite;

  L1 := TIdSipTestInviteListener.Create;
  try
    L2 := TIdSipTestInviteListener.Create;
    try
      Invite.AddInviteListener(L1);
      Invite.AddInviteListener(L2);
      Invite.RemoveInviteListener(L2);

      Self.ReceiveRinging(Self.LastSentRequest);

      Check(L1.CallProgress,
            'First listener not notified');
      Check(not L2.CallProgress,
            'Second listener erroneously notified, ergo not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestSendToSipsUri;
var
  Invite: TIdSipAction;
begin
  Self.Destination.Address.Scheme := SipsScheme;

  Self.MarkSentRequestCount;
  Invite := Self.CreateAction;

  CheckEquals(Self.Destination.Address.Scheme,
              Invite.LocalGruu.Address.Scheme,
              Invite.ClassName + ': LocalGruu doesn''t use a SIPS URI');

  CheckRequestSent(Invite.ClassName + ': No INVITE sent');
  CheckEquals(Self.Destination.Address.Scheme,
              Self.LastSentRequest.FirstContact.Address.Scheme,
              Invite.ClassName + ': SIPS URI not used when contacting a SIPS URI');
end;

procedure TestTIdSipOutboundInvite.TestSendTwice;
var
  Invite: TIdSipAction;
begin
  Invite := Self.CreateAction;
  try
    Invite.Send;
    Fail(Invite.ClassName + ': Failed to bail out calling Send a 2nd time');
  except
    on EIdSipTransactionUser do;
  end;
end;

procedure TestTIdSipOutboundInvite.TestSendWithGruu;
var
  Invite: TIdSipOutboundInvite;
begin
  Self.UseGruu;

  Self.MarkSentRequestCount;
  Invite := Self.CreateAction as TIdSipOutboundInvite;
  CheckRequestSent(Self.ClassName + ': No request sent');

  CheckEquals(MethodInvite,
              Self.LastSentRequest.Method,
              Self.ClassName + ': Method of sent request');

  CheckEquals(Self.LastSentRequest.FirstContact.AsString,
              Invite.LocalGruu.AsString,
              Self.ClassName + ': LocalGruu host not set');
  Check(not Invite.LocalGruu.Address.HasGrid,
        Self.ClassName + ': Local GRUU has a "grid" parameter but isn''t a '
      + 'dialog-creating request');
end;

procedure TestTIdSipOutboundInvite.TestTerminateBeforeAccept;
var
  OutboundInvite: TIdSipOutboundInvite;
begin
  //  ---         INVITE         --->
  // <---       180 Ringing      ---
  //  ---         CANCEL         --->
  // <---         200 OK         --- (for the CANCEL)
  // <--- 487 Request Terminated ---
  //  ---          ACK           --->
  OutboundInvite := Self.CreateAction as TIdSipOutboundInvite;
  Self.ReceiveRinging(Self.LastSentRequest);
  Self.MarkSentRequestCount;

  OutboundInvite.Terminate;

  CheckRequestSent(Self.ClassName + ': Action didn''t send a CANCEL');
  Check(not OutboundInvite.IsTerminated,
        Self.ClassName + ': The Action can''t terminate until it receives the '
      + '487 Request Terminated response');

  Self.ReceiveResponse(OutboundInvite.InitialRequest, SIPRequestTerminated);
  Check(OutboundInvite.IsTerminated,
        Self.ClassName + ': The 487 arrived but the Action didn''t terminate');
end;

procedure TestTIdSipOutboundInvite.TestTerminateAfterAccept;
var
  OutboundInvite: TIdSipOutboundInvite;
begin
  //  --- INVITE --->
  // <--- 200 OK ---
  //  ---  ACK   --->
  //  ---  BYE   --->
  // <--- 200 OK ---
  OutboundInvite := Self.CreateAction as TIdSipOutboundInvite;

  Self.ReceiveOk(Self.LastSentRequest);

  Self.MarkSentRequestCount;

  OutboundInvite.Terminate;

  CheckNoRequestSent(Self.ClassName
                   + ': Action sent a CANCEL for a fully established call');
end;

procedure TestTIdSipOutboundInvite.TestTerminateSignalled;
var
  L:              TIdSipTestActionListener;
  OutboundInvite: TIdSipOutboundInvite;
begin
  OutboundInvite := Self.CreateAction as TIdSipOutboundInvite;

  L := TIdSipTestActionListener.Create;
  try
    OutboundInvite.AddActionListener(L);
    try
      Self.ReceiveOk(Self.LastSentRequest);
      OutboundInvite.Terminate;
      Check(L.Terminated,
            Self.ClassName + ': Listeners not notified of termination');
    finally
      OutboundInvite.RemoveActionListener(L);
    end;
  finally
    L.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestTransactionCompleted;
var
  Invite: TIdSipOutboundInvite;
begin
  Invite := Self.CreateAction as TIdSipOutboundInvite;
  Invite.TransactionCompleted;
  Check(Invite.IsTerminated,
        Self.ClassName + ': Outbound INVITE not marked as terminated');
end;

//******************************************************************************
//* TestTIdSipOutboundInitialInvite                                            *
//******************************************************************************
//* TestTIdSipOutboundInitialInvite Protected methods **************************

function TestTIdSipOutboundInitialInvite.CreateAction: TIdSipAction;
var
  Invite: TIdSipOutboundInitialInvite;
begin
  Invite := Self.CreateInvite as TIdSipOutboundInitialInvite;
  Invite.Send;

  Result := Invite;
end;

//* TestTIdSipOutboundInitialInvite Private methods ****************************

function TestTIdSipOutboundInitialInvite.CreateInvite: TIdSipOutboundInitialInvite;
var
  Invite: TIdSipOutboundInitialInvite;
begin
  Invite := Self.Core.AddOutboundAction(TIdSipOutboundInitialInvite) as TIdSipOutboundInitialInvite;
  Invite.Destination := Self.Destination;
  Invite.MimeType    := Self.InviteMimeType;
  Invite.Offer       := Self.InviteOffer;
  Invite.AddActionListener(Self);
  Invite.AddOwnedActionListener(Self);
  Invite.AddInviteListener(Self);

  Result := Invite;
end;

//* TestTIdSipOutboundInitialInvite Published methods **************************

procedure TestTIdSipOutboundInitialInvite.TestSendWithGruu;
begin
  Self.CheckSendDialogEstablishingRequestWithGruu;
end;

procedure TestTIdSipOutboundInitialInvite.TestSendWithMaxForwards;
const
  NewMaxForwards = 42;
var
  Invite: TIdSipOutboundInitialInvite;
begin

  // This horrible code creates a TIdSipOutboundInvite of the same type as the
  // other tests in this test suite.
  Invite := Self.CreateInvite;
  Invite.MaxForwards := NewMaxForwards;

  Self.MarkSentRequestCount;
  Invite.MaxForwards := NewMaxForwards;
  Invite.Send;
  CheckRequestSent('No INVITE sent');
  CheckEquals(NewMaxForwards, Self.LastSentRequest.MaxForwards, 'Max-Forwards not overridden');
end;

//******************************************************************************
//* TestTIdSipOutboundRedirectedInvite                                         *
//******************************************************************************
//* TestTIdSipOutboundRedirectedInvite Protected methods ***********************

function TestTIdSipOutboundRedirectedInvite.CreateAction: TIdSipAction;
var
  Redirect: TIdSipOutboundRedirectedInvite;
begin
  // We do this to send the initial INVITE
  Self.CreateInitialInvite;

  // Then we send the redirected INVITE
  Redirect := Self.CreateInvite;
  Redirect.Contact         := Self.Destination;
  Redirect.OriginalRequest := Self.LastSentRequest;
  Redirect.AddActionListener(Self);
  Redirect.AddOwnedActionListener(Self);
  Redirect.AddInviteListener(Self);
  Redirect.Send;

  Result := Redirect;
end;

//* TestTIdSipOutboundRedirectedInvite Private methods *************************

function TestTIdSipOutboundRedirectedInvite.CreateInvite: TIdSipOutboundRedirectedInvite;
begin
  Result := Self.Core.AddOutboundAction(TIdSipOutboundRedirectedInvite) as TIdSipOutboundRedirectedInvite;
end;

//* TestTIdSipOutboundRedirectedInvite Published methods ***********************

procedure TestTIdSipOutboundRedirectedInvite.TestRedirectedInvite;
var
  Invite: TIdSipOutboundRedirectedInvite;
begin
  Self.MarkSentRequestCount;
  Invite := Self.CreateAction as TIdSipOutboundRedirectedInvite;
  CheckRequestSent('No INVITE sent');

  CheckEquals(Invite.OriginalRequest.CallID,
              Invite.InitialRequest.CallID,
              'Call-ID mismatch between original and new INVITEs');
  CheckEquals(Invite.OriginalRequest.From.Tag,
              Invite.InitialRequest.From.Tag,
              'From tag mismatch between original and new INVITEs');
  Check(not Invite.InitialRequest.ToHeader.HasTag,
        'New INVITE mustn''t have a To tag');
end;

procedure TestTIdSipOutboundRedirectedInvite.TestSendWithGruu;
begin
  Self.CheckSendDialogEstablishingRequestWithGruu;
end;

//******************************************************************************
//* TestTIdSipOutboundReInvite                                                 *
//******************************************************************************
//* TestTIdSipOutboundReInvite Public methods **********************************

procedure TestTIdSipOutboundReInvite.SetUp;
begin
  inherited SetUp;

  Self.Dialog := Self.CreateArbitraryDialog;
  Self.LocalGruu := TIdSipContactHeader.Create;
  Self.LocalGruu.Address.Username := 'case';
  Self.LocalGruu.Address.Grid     := 'decafbad';
  Self.LocalGruu.Address.Host     := Self.LanIP;
  Self.LocalGruu.Address.IsGruu   := true;
  Self.LocalGruu.Address.Port     := DefaultSipPort;
  Self.LocalGruu.Address.Scheme   := SipScheme;
  Self.LocalGruu.IsUnset          := true;

  Self.InOutboundSession := true;
end;

procedure TestTIdSipOutboundReInvite.TearDown;
begin
  Self.LocalGruu.Free;
  Self.Dialog.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundReInvite Protected methods *******************************

function TestTIdSipOutboundReInvite.CreateAction: TIdSipAction;
var
  Invite: TIdSipOutboundReInvite;
begin
  Self.Dialog.RemoteTarget.Uri := Self.Destination.Address.Uri;

  Invite := Self.CreateInvite;
  Invite.LocalGruu         := Self.LocalGruu;
  Invite.Dialog            := Self.Dialog;
  Invite.MimeType          := Self.InviteMimeType;
  Invite.InOutboundSession := Self.InOutboundSession;
  Invite.Offer             := Self.InviteOffer;
  Invite.OriginalInvite    := Self.Invite;
  Invite.AddActionListener(Self);
  Invite.AddOwnedActionListener(Self);
  Invite.AddInviteListener(Self);
  Invite.Send;

  Result := Invite;
end;

//* TestTIdSipOutboundReInvite Private methods *********************************

function TestTIdSipOutboundReInvite.CreateInvite: TIdSipOutboundReInvite;
begin
  Result := Self.Core.AddOutboundAction(TIdSipOutboundReInvite) as TIdSipOutboundReInvite;
end;

//* TestTIdSipOutboundReInvite Published methods *******************************

procedure TestTIdSipOutboundReInvite.TestCancelWithOutboundProxy;
const
  Uri = 'sip:proxy.tessier-ashpool.co.luna';
var
  Invite: TIdSipAction;
  ProxyUri: TIdSipUri;
begin
  Check(Self.Dialog.RouteSet.IsEmpty,
        Self.ClassName + ': For the purposes of this test the dialog''s route set must be empty');

  ProxyUri := TIdSipUri.Create(Uri);
  try
    Self.Core.DefaultRoutePath.AddRoute(ProxyUri);
  finally
    ProxyUri.Free;
  end;

  Invite := Self.CreateAction;
  Self.ReceiveTrying(Self.LastSentRequest);

  Self.MarkSentRequestCount;
  Invite.Terminate;
  CheckRequestSent(Self.ClassName + ': No request sent');
  CheckEquals(MethodCancel, Self.LastSentRequest.Method, Self.ClassName + ': No CANCEL sent');
  Check(Self.Dialog.RouteSet.Equals(Self.LastSentRequest.Route),
        Self.ClassName + ': RFC 3261, section 9.1: To support stateless proxies, the CANCEL MUST have the same route path as the INVITE it''s cancelling');
end;

procedure TestTIdSipOutboundReInvite.TestSendInInboundSessionWithAuthentication;
begin
  Self.Invite.AddHeader(AuthorizationHeader).Value := '';
  Self.InOutboundSession := false;

  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent(Self.ClassName + ': No request sent');

  CheckEquals(MethodInvite,
              Self.LastSentRequest.Method,
              Self.ClassName + ': Method of sent request');
  Check(not Self.LastSentRequest.HasAuthorization,
        'You can''t use the other party''s authorization credentials when '
      + 'sending them a re-INVITE');
end;

procedure TestTIdSipOutboundReInvite.TestSendToSipsUri;
begin
  // A re-INVITE of course takes place in the context of an existing dialog; if
  // the dialog was a secure one, then our LocalGruu of necessity must also be
  // a SIPS URI.
  Self.LocalGruu.Address.Scheme := SipsScheme;

  inherited TestSendToSipsUri;
end;

procedure TestTIdSipOutboundReInvite.TestSendUsesMappedRoutes;
begin
  CheckLocalAddress(Self.LocalGruu.Address.Host, Self.LanDestination, 'LAN destination', Self.LanIP);
  CheckLocalAddress(Self.LocalGruu.Address.Host, '127.0.0.2', 'Loopback destination', '127.0.0.1');
end;

procedure TestTIdSipOutboundReInvite.TestSendWithGruu;
var
  Invite: TIdSipOutboundInvite;
begin
  Self.UseGruu;

  Self.MarkSentRequestCount;
  Invite := Self.CreateAction as TIdSipOutboundInvite;
  CheckRequestSent(Self.ClassName + ': No request sent');

  CheckEquals(MethodInvite,
              Self.LastSentRequest.Method,
              Self.ClassName + ': Method of sent request');

  CheckEquals(Self.LocalGruu.AsString,
              Invite.LocalGruu.AsString,
              Self.ClassName + ': LocalGruu not set (to that of the session-established local GRUU)');
end;

//******************************************************************************
//* TestTIdSipOutboundReplacingInvite                                          *
//******************************************************************************
//* TestTIdSipOutboundReplacingInvite Public methods ***************************

procedure TestTIdSipOutboundReplacingInvite.SetUp;
begin
  inherited SetUp;

  Self.CallID  := 'call@localhost';
  Self.FromTag := 'fromtag';
  Self.ToTag   := 'totag';
end;

procedure TestTIdSipOutboundReplacingInvite.TearDown;
begin
  inherited TearDown;
end;

//* TestTIdSipOutboundReplacingInvite Protected methods ************************

function TestTIdSipOutboundReplacingInvite.CreateAction: TIdSipAction;
var
  Invite: TIdSipOutboundReplacingInvite;
begin
  Invite := Self.CreateInvite;
  Invite.CallID  := Self.CallID;
  Invite.FromTag := Self.FromTag;
  Invite.ToTag   := Self.ToTag;

  Invite.Destination := Self.Destination;
  Invite.Dialog      := Self.Dialog;
  Invite.MimeType    := Self.InviteMimeType;
  Invite.Offer       := Self.InviteOffer;
  Invite.AddActionListener(Self);
  Invite.AddOwnedActionListener(Self);
  Invite.AddInviteListener(Self);
  Invite.Send;

  Result := Invite;
end;

//* TestTIdSipOutboundReplacingInvite Private methods **************************

function TestTIdSipOutboundReplacingInvite.CreateInvite: TIdSipOutboundReplacingInvite;
begin
  Result := Self.Core.AddOutboundAction(TIdSipOutboundReplacingInvite) as TIdSipOutboundReplacingInvite;
end;

//* TestTIdSipOutboundReplacingInvite Published methods ************************

procedure TestTIdSipOutboundReplacingInvite.TestSend;
var
  Invite: TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent(Self.ClassName + ': No request sent');

  Invite := Self.LastSentRequest;
  CheckEquals(MethodInvite,
              Invite.Method,
              Self.ClassName + ': wrong request type sent');
  CheckEquals(Self.CallID,
              Invite.Replaces.CallID,
              Self.ClassName + ': Replaces'' Call-ID');
  CheckEquals(Self.FromTag,
              Invite.Replaces.FromTag,
              Self.ClassName + ': Replaces'' from-tag');
  CheckEquals(Self.ToTag,
              Invite.Replaces.ToTag,
              Self.ClassName + ': Replaces'' to-tag');
end;

//******************************************************************************
//* TestTIdSipSession                                                          *
//******************************************************************************
//* TestTIdSipSession Public methods *******************************************

procedure TestTIdSipSession.SetUp;
begin
  inherited SetUp;

  Self.Module := Self.Core.ModuleFor(MethodInvite) as TIdSipInviteModule;

  Self.MimeType                  := '';
  Self.OnEndedSessionFired       := false;
  Self.OnEstablishedSessionFired := false;
  Self.OnModifiedSessionFired    := false;
  Self.OnModifySessionFired      := false;
  Self.OnReferralFired           := false;
  Self.RemoteSessionDescription  := '';
  Self.SdpMimeType               := 'application/sdp';
end;

//* TestTIdSipSession Protected methods ****************************************

procedure TestTIdSipSession.CheckHeadersEqual(ExpectedMessage: TIdSipMessage;
                                              ReceivedMessage: TIdSipMessage;
                                              const HeaderName: String;
                                              const Msg: String);
var
  ExpectedHeaders: TIdSipHeadersFilter;
  I:               Integer;
  ReceivedHeaders: TIdSipHeadersFilter;
begin
  ExpectedHeaders := TIdSipHeadersFilter.Create(ExpectedMessage.Headers,
                                                ProxyAuthorizationHeader);
  try
    ReceivedHeaders := TIdSipHeadersFilter.Create(ReceivedMessage.Headers,
                                                  ProxyAuthorizationHeader);
    try
      I := 0;
      ExpectedHeaders.First;
      ReceivedHeaders.First;

      while ExpectedHeaders.HasNext and ReceivedHeaders.HasNext do begin
        CheckEquals(ExpectedHeaders.CurrentHeader.FullValue,
                    ReceivedHeaders.CurrentHeader.FullValue,
                    Msg + ': Header mismatch at index ' + IntToStr(I));

        ExpectedHeaders.Next;
        ReceivedHeaders.Next;
        Inc(I);
      end;

      Check(ExpectedHeaders.HasNext = ReceivedHeaders.HasNext,
            Msg + ': Number of headers doesn''t match');
    finally
      ReceivedHeaders.Free;
    end;
  finally
    ExpectedHeaders.Free;
  end;
end;

procedure TestTIdSipSession.CheckResendWaitTime(Milliseconds: Cardinal;
                                                const Msg: String);
begin
  Check(Milliseconds mod 10 = 0, Msg);
end;

function TestTIdSipSession.CreateAndEstablishSession: TIdSipSession;
var
  NewSession: TIdSipSession;
begin
  NewSession := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(NewSession);

  Result := NewSession;
end;

function TestTIdSipSession.CreateRemoteReInvite(LocalDialog: TIdSipDialog): TIdSipRequest;
begin
  Result := Self.Module.CreateReInvite(LocalDialog,
                                       Self.SimpleSdp,
                                       SdpMimeType);
  try
    Result.ToHeader.Tag    := LocalDialog.ID.LocalTag;
    Result.From.Tag        := LocalDialog.ID.RemoteTag;
    Result.CSeq.SequenceNo := LocalDialog.RemoteSequenceNo + 1;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

procedure TestTIdSipSession.EstablishSession(Session: TIdSipSession);
begin
  Fail(Self.ClassName + ' must override EstablishSession');
end;

function TestTIdSipSession.MultiStreamSdp: String;
begin
  Result := 'v=0' + CRLF
          + 'o=wintermute 2890844526 2890842807 IN IP4 127.0.0.1' + CRLF
          + 's=Minimum Session Info' + CRLF
          + 'm=audio 10000 RTP/AVP 0' + CRLF
          + 'c=IN IP4 127.0.0.1' + CRLF
          + 'm=text 11000 RTP/AVP 98' + CRLF
          + 'c=IN IP4 127.0.0.1' + CRLF
          + 'a=rtpmap:98 t140/1000';
end;

procedure TestTIdSipSession.OnAddAction(UserAgent: TIdSipAbstractCore;
                                        Action: TIdSipAction);
begin
  // Do nothing.
end;

procedure TestTIdSipSession.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                                      Message: TIdSipMessage;
                                                      Binding: TIdConnectionBindings);
begin
  Self.DroppedUnmatchedResponse := true;
end;

procedure TestTIdSipSession.OnEndedSession(Session: TIdSipSession;
                                           ErrorCode: Cardinal;
                                           const Reason: String);
begin
  Self.OnEndedSessionFired := true;
  Self.ErrorCode           := ErrorCode;
  Self.Reason              := Reason;
end;

procedure TestTIdSipSession.OnEstablishedSession(Session: TIdSipSession;
                                                 const RemoteSessionDescription: String;
                                                 const MimeType: String);
begin
  Self.OnEstablishedSessionFired := true;
end;

procedure TestTIdSipSession.OnModifiedSession(Session: TIdSipSession;
                                              Answer: TIdSipResponse);
begin
  Self.OnModifiedSessionFired := true;

  Self.RemoteSessionDescription := Answer.Body;
  Self.MimeType                 := Answer.ContentType;
end;

procedure TestTIdSipSession.OnModifySession(Session: TIdSipSession;
                                            const RemoteSessionDescription: String;
                                            const MimeType: String);
begin
  Self.OnModifySessionFired := true;

  Self.RemoteSessionDescription := RemoteSessionDescription;
  Self.MimeType                 := MimeType;
end;

procedure TestTIdSipSession.OnProgressedSession(Session: TIdSipSession;
                                                Progress: TIdSipResponse);
begin
  // Do nothing.
end;

procedure TestTIdSipSession.OnReferral(Session: TIdSipSession;
                                       Refer: TIdSipRequest;
                                       Binding: TIdConnectionBindings);
begin
  Self.ReceivingBinding := Binding;

  Self.OnReferralFired := true;
end;

procedure TestTIdSipSession.OnRemoveAction(UserAgent: TIdSipAbstractCore;
                                           Action: TIdSipAction);
begin
  // Do nothing.
end;

procedure TestTIdSipSession.ReceiveRemoteReInvite(Session: TIdSipSession);
begin
  // At this point Invite represents the INVITE we sent out
  Self.Invite.LastHop.Branch  := Self.Invite.LastHop.Branch + '1';
  Self.Invite.CallID          := Session.Dialog.ID.CallID;
  Self.Invite.From.Tag        := Session.Dialog.ID.RemoteTag;
  Self.Invite.ToHeader.Tag    := Session.Dialog.ID.LocalTag;
  Self.Invite.CSeq.SequenceNo := Session.Dialog.RemoteSequenceNo + 1;

  Self.Invite.Body          := Self.RemoteSessionDescription;
  Self.Invite.ContentType   := Self.MimeType;
  Self.Invite.ContentLength := Length(Self.Invite.Body);

  // Now it represents an INVITE received from the network
  Self.ReceiveRequest(Self.Invite)
end;

procedure TestTIdSipSession.ResendWith(Session: TIdSipSession;
                                       AuthenticationChallenge: TIdSipResponse);
var
  AuthCreds: TIdSipAuthorizationHeader;
begin
  AuthCreds := Self.CreateAuthorization(AuthenticationChallenge);
  try
    Session.Resend(AuthCreds);
  finally
    AuthCreds.Free;
  end;
end;

function TestTIdSipSession.SimpleSdp: String;
begin
  Result := 'v=0' + CRLF
          + 'o=wintermute 2890844526 2890842807 IN IP4 127.0.0.1' + CRLF
          + 's=Minimum Session Info' + CRLF
          + 'm=text 11000 RTP/AVP 98' + CRLF
          + 'c=IN IP4 127.0.0.1' + CRLF
          + 'a=rtpmap:98 t140/1000';
end;

//* TestTIdSipSession Published methods ****************************************

procedure TestTIdSipSession.TestAckToInDialogInviteMatchesInvite;
var
  Ack:     TIdSipRequest;
  Session: TIdSipSession;
begin
  Session := Self.CreateAndEstablishSession;
  Self.ReceiveRemoteReInvite(Session);

  Check(Self.OnModifySessionFired,
        Session.ClassName + ': OnModifySession didn''t fire');

  Session.AcceptModify('', '');

  // The last request was the inbound re-INVITE.
  Ack := Self.LastSentRequest.AckFor(Self.LastSentResponse);
  try
    Check(not Session.Match(Ack),
          Session.ClassName + ': ACK mustn''t match the Session');
  finally
    Ack.Free;
  end;
end;

procedure TestTIdSipSession.TestDontMatchResponseToModify;
var
  Ok:      TIdSipResponse;
  Session: TIdSipSession;
begin
  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);
  Check(Session.DialogEstablished,
        Session.ClassName + ': No dialog established');
  Session.Modify('', '');

  Ok := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                    SIPOK);
  try
    Check(not Session.Match(Ok),
          Session.ClassName + ': Responses to outbound re-INVITEs must only '
        + 'match the OutboundInvites');
  finally
    Ok.Free;
  end;
end;

procedure TestTIdSipSession.TestDontMatchResponseToInitialRequest;
var
  Ok:      TIdSipResponse;
  Session: TIdSipSession;
begin
  Session := Self.CreateAction as TIdSipSession;

  Ok := TIdSipResponse.InResponseTo(Session.InitialRequest, SIPOK);
  try
    Ok.ToHeader.Tag := Self.Core.NextTag; // Just for completeness' sake
    Check(not Session.Match(Ok),
          Session.ClassName + ': Responses to the initial INVITE must only '
        + 'match the (In|Out)boundInvite');
  finally
    Ok.Free;
  end;
end;

procedure TestTIdSipSession.TestInboundModify;
var
  LocalSessionDescription: String;
  LocalMimeType:           String;
  OK:                      TIdSipResponse;
  Session:                 TIdSipSession;
begin
  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);

  LocalMimeType                 := SdpMimeType;
  LocalSessionDescription       := Format(DummySDP, ['127.0.0.1']);
  Self.MimeType                 := SdpMimeType;
  Self.RemoteSessionDescription := Self.SimpleSdp;

  Self.ReceiveRemoteReInvite(Session);

  Self.MarkSentResponseCount;
  Session.AcceptModify(LocalSessionDescription, LocalMimeType);
  CheckResponseSent('No response sent');
  OK := Self.LastSentResponse;
  Check(OK.ToHeader.HasTag,
        'To header of 200 OK lacks a tag');
  Check(OK.From.HasTag,
        'From header of 200 OK lacks a tag');

  Self.ReceiveAck;

  Check(Self.OnModifySessionFired,
        Session.ClassName + ': OnModifySession didn''t fire');
  CheckEquals(MimeType,
              Session.LocalMimeType,
              'Session.LocalMimeType');
  CheckEquals(LocalSessionDescription,
              Session.LocalSessionDescription,
              'Session.LocalSessionDescription');
  CheckEquals(Self.MimeType,
              Session.RemoteMimeType,
              'Session.RemoteMimeType');
  CheckEquals(Self.RemoteSessionDescription,
              Session.RemoteSessionDescription,
              'Session.RemoteSessionDescription');
end;

procedure TestTIdSipSession.TestIsSession;
var
  Action: TIdSipAction;
begin
  Action := Self.CreateAction;
  // Self.UA owns the action!
  Check(Action.IsSession,
        Action.ClassName + ' not marked as a Session');
end;

procedure TestTIdSipSession.TestMatchBye;
var
  Bye:     TIdSipRequest;
  Session: TIdSipSession;
begin
  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);
  Check(Session.DialogEstablished,
        Session.ClassName + ': No dialog established');

  Bye := Self.CreateRemoteReInvite(Session.Dialog);
  try
    Bye.Method := MethodBye;

    Check(Session.Match(Bye),
          Session.ClassName + ': BYE must match session');
  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipSession.TestMatchByeWithDifferingGridParameter;
var
  Bye:     TIdSipRequest;
  Session: TIdSipSession;
begin
  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);
  Check(Session.DialogEstablished,
        Session.ClassName + ': No dialog established');

  Bye := Self.CreateRemoteReInvite(Session.Dialog);
  try
    // Force a mismatch between the BYE's target GRUU and the local GRUU.
    Bye.RequestUri.Grid := Session.LocalGruu.Grid + '1';
    Bye.Method := MethodBye;

    Check(Session.Match(Bye),
          Session.ClassName + ': BYE must match session');
  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipSession.TestMatchInitialRequest;
var
  Session: TIdSipSession;
begin
  Session := Self.CreateAction as TIdSipSession;

  Check(not Session.Match(Session.InitialRequest),
        Session.ClassName + ': The initial INVITE must only match the '
      + '(In|Out)boundInvite');
end;

procedure TestTIdSipSession.TestMatchInboundModify;
var
  ReInvite: TIdSipRequest;
  Session:  TIdSipSession;
begin
  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);
  Check(Session.DialogEstablished,
        Session.ClassName + ': No dialog established');

  ReInvite := Self.CreateRemoteReInvite(Session.Dialog);
  try
    Check(Session.Match(ReInvite),
          Session.ClassName + ': In-dialog INVITE must match session');
  finally
    ReInvite.Free;
  end;
end;

procedure TestTIdSipSession.TestMatchInboundModifyAck;
var
  Ack:     TIdSipRequest;
  Session: TIdSipSession;
begin
  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);
  Check(Session.DialogEstablished,
        Session.ClassName + ': No dialog established');

  Self.ReceiveRemoteReInvite(Session);
  Session.AcceptModify('', '');

  Self.DroppedUnmatchedResponse := false;
  Ack := Self.LastSentRequest.AckFor(Self.LastSentResponse);
  try
    Check(not Session.Match(Ack),
          Session.ClassName
        + ': ACK for in-dialog INVITE must not match session');

    Self.ReceiveRequest(Ack);
    Check(not Self.DroppedUnmatchedResponse,
          Session.ClassName + ': Dropped ACK for in-dialog INVITE');
  finally
    Ack.Free;
  end;
end;

procedure TestTIdSipSession.TestMatchReferWithCorrectGridParameter;
var
  Refer:   TIdSipRequest;
  Session: TIdSipSession;
  SubMod:  TIdSipSubscribeModule;
begin
  Self.UseGruu;

  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);

  SubMod := Self.Core.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  Refer := SubMod.CreateRefer(Self.Core.From, Self.Destination, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    Refer.RequestUri.Grid := Session.LocalGruu.Grid;

    Check(Session.Match(Refer),
          'Session should match any request whose "grid" parameter matches that '
        + 'of the GRUU that the stack sent out in creating this session');
  finally
    Refer.Free;
  end;
end;

procedure TestTIdSipSession.TestMatchReferWithIncorrectGridParameter;
var
  Refer:   TIdSipRequest;
  Session: TIdSipSession;
  SubMod:  TIdSipSubscribeModule;
begin
  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);

  SubMod := Self.Core.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  Refer := SubMod.CreateRefer(Self.Destination, Self.Destination, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    if Session.IsInbound then
      Refer.RequestUri := Session.InitialRequest.FirstContact.Address
    else
      Refer.RequestUri := Session.RemoteContact.Address;

    Refer.RequestUri.Grid := Refer.RequestUri.Grid + '-x';

    Check(not Session.Match(Refer),
          'Session mustn''t match any request whose "grid" parameter doesn''t match '
        + 'that of the GRUU that the stack sent out in creating this session');
  finally
    Refer.Free;
  end;
end;

procedure TestTIdSipSession.TestMatchTargetDialog;
var
  Session: TIdSipSession;
  SubMod:  TIdSipSubscribeModule;
  TDRefer: TIdSipRequest;
begin
  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);

  SubMod := Self.Core.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  TDRefer := SubMod.CreateRefer(Self.Destination, Self.Destination, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    Check(not Session.Match(TDRefer),
          Session.ClassName + ': shouldn''t match an arbitrary (out-of-dialog) REFER');
    TDRefer.TargetDialog.LocalTag := Session.Dialog.ID.LocalTag;
    TDRefer.TargetDialog.RemoteTag := Session.Dialog.ID.RemoteTag;
    TDRefer.TargetDialog.CallID := Session.Dialog.ID.CallID;

    Check(Session.Match(TDRefer),
          Session.ClassName + ': didn''t match a REFER with a Target-Dialog containing its dialog ID');

    TDRefer.TargetDialog.CallID := Session.Dialog.ID.CallID + '1';
    Check(not Session.Match(TDRefer),
          Session.ClassName + ': matched a REFER with a Target-Dialog not containing its dialog ID');
  finally
    TDRefer.Free;
  end;
end;

procedure TestTIdSipSession.TestModifyBeforeFullyEstablished;
var
  Session: TIdSipSession;
begin
  Session := Self.CreateAction as TIdSipSession;

  try
    Session.Modify('', '');
    Fail('Failed to bail out starting a modify before session''s established');
  except
     on EIdSipTransactionUser do;
  end;
end;

procedure TestTIdSipSession.TestModifyDuringModification;
var
  Session: TIdSipSession;
begin
  Session := Self.CreateAndEstablishSession;
  Session.Modify('', '');

  try
    Session.Modify('', '');
    Fail('Failed to bail out starting a new modify while one''s in progress');
  except
    on EIdSipTransactionUser do;
  end;
end;

procedure TestTIdSipSession.TestModifyGlareInbound;
var
  Session: TIdSipSession;
begin
  // Essentially, we and Remote send INVITEs simultaneously.
  // We send ours, and it arrives after the remote end's sent us its INVITE.
  // When we receive its INVITE, we reject it with a 491 Request Pending.

  Session := Self.CreateAndEstablishSession;
  Session.Modify('', '');

  Self.MarkSentResponseCount;
  Self.ReceiveRemoteReInvite(Session);
  CheckResponseSent(Session.ClassName + ': No response sent');
  CheckEquals(SIPRequestPending,
              Self.LastSentResponse.StatusCode,
              Session.ClassName + ': Unexpected response');
end;

procedure TestTIdSipSession.TestModifyGlareOutbound;
const
  Body = 'random data';
var
  EventCount:  Integer;
  LatestEvent: TIdWait;
  Session:     TIdSipSession;
begin
  // Essentially, we and Remote send INVITEs simultaneously
  // We send ours and, because the remote end's sent its before ours arrives,
  // we receive its 491 Request Pending. We schedule a time to resend our
  // INVITE.

  Session := Self.CreateAndEstablishSession;

  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionsWait);
  Session.Modify(Body, 'text/plain');

  Self.DebugTimer.TriggerImmediateEvents := false;
  EventCount := Self.DebugTimer.EventCount;
  Self.ReceiveResponse(SIPRequestPending);

  Self.DebugTimer.LockTimer;
  try
    Check(EventCount < Self.DebugTimer.EventCount,
          Session.ClassName + ': No timer added');

    LatestEvent := Self.DebugTimer.LastEventScheduled(TIdSipActionsWait);

    Check(Assigned(LatestEvent),
          Session.ClassName + ': Wrong notify event');
    Self.CheckResendWaitTime(LatestEvent.DebugWaitTime,
                             Session.ClassName + ': Bad wait time (was '
                           + IntToStr(LatestEvent.DebugWaitTime) + ' milliseconds)');
  finally
    Self.DebugTimer.UnlockTimer;
  end;

  Self.MarkSentRequestCount;
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionsWait);
  CheckRequestSent('No request sent: event not scheduled?');
  CheckEquals(MethodInvite,
              Self.LastSentRequest.Method,
              'Unexpected request');
  CheckEquals(Body,
              Self.LastSentRequest.Body,
              'Wrong message sent?');
end;

procedure TestTIdSipSession.TestModifyNetworkFailure;
var
  OldSessionCount: Integer;
  Session:         TIdSipSession;
  L:               TIdSipTestActionListener;
begin
  Session := Self.CreateAndEstablishSession;

  L := TIdSipTestActionListener.Create;
  try
    OldSessionCount := Self.Core.SessionCount;

    Session.AddActionListener(L);
    Session.Modify('new session desc', 'text/plain');
    Self.Dispatcher.Transport.FireOnException(Self.LastSentRequest, EIdConnectTimeout, '10051', 'Host not found');

    Check(L.NetworkFailed, 'Listener not notified of failure: did the ReInvite notify the Session?');
    Check(Self.Core.SessionCount < OldSessionCount, 'Session not torn down');
  finally
    // We don't need to remove the listener here because the Session's dead.
    L.Free;
  end;
end;

procedure TestTIdSipSession.TestModifyRejected;
var
  OldSessionDescription: String;
  OldSessionMimeType:    String;
  Session: TIdSipSession;
begin
  Session := Self.CreateAndEstablishSession;

  OldSessionDescription := Session.LocalSessionDescription;
  OldSessionMimeType    := Session.LocalMimeType;

  Session.Modify('new session desc', 'text/plain');
  Self.ReceiveServiceUnavailable(Self.LastSentRequest);

  CheckEquals(OldSessionDescription,
              Session.LocalSessionDescription,
              'Session description altered');
  CheckEquals(OldSessionMimeType,
              Session.LocalMimeType,
              'Session MIME type altered');
end;

procedure TestTIdSipSession.TestModifyRejectedWithTimeout;
var
  ClassName:    String;
  Session:      TIdSipSession;
  SessionCount: Integer;
begin
  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);
  ClassName := Session.ClassName;

  Session.Modify('', '');

  Self.MarkSentRequestCount;
  SessionCount := Self.Core.SessionCount;

  Self.ReceiveResponse(SIPRequestTimeout);

  CheckRequestSent(ClassName + ': No request sent');
  CheckEquals(MethodBye,
              Self.LastSentRequest.Method,
              ClassName + ': Unexpected request sent');
  Check(Self.Core.SessionCount < SessionCount,
        ClassName + ': Session not terminated');
end;

procedure TestTIdSipSession.TestModifyWaitTime;
var
  I:       Integer;
  Session: TIdSipSession;
begin
  Session := Self.CreateAction as TIdSipSession;

  // The modify wait time is random; this test does not guarantee that the wait
  // time is always correct!
  for I := 1 to 100 do
    CheckResendWaitTime(Session.ModifyWaitTime, Session.ClassName);
end;

procedure TestTIdSipSession.TestReceiveBye;
var
  Session:  TIdSipSession;
begin
  Self.Dispatcher.Transport.WriteLog := true;
  try
    Session := Self.CreateAndEstablishSession;

    Self.ReceiveBye(Session.Dialog);

    Check(Self.OnEndedSessionFired, 'OnEndedSession didn''t fire');
  finally
    Self.Dispatcher.Transport.WriteLog := false;
  end;
end;

procedure TestTIdSipSession.TestReceiveByeWithPendingRequests;
var
  Bye:      TIdSipRequest;
  ReInvite: TIdSipRequest;
  Session:  TIdSipSession;
begin
  // <---         INVITE          ---
  //  ---         200 OK          --->
  // <---          ACK            ---
  // <---         INVITE          ---
  // <---          BYE            ---
  //  ---  487 Request Terminated --- (for the re-INVITE)
  // <---          ACK            ---
  //  ---         200 OK          ---> (for the BYE)
  Session := Self.CreateAndEstablishSession;

  Self.ReceiveRemoteReInvite(Session);

  ReInvite := TIdSipRequest.Create;
  try
    ReInvite.Assign(Self.Invite);

    Self.MarkSentResponseCount;

    Bye := Self.CreateRemoteBye(Session.Dialog);
    try
      Self.ReceiveRequest(Bye);

      Check(Self.ResponseCount + 2 <= Self.SentResponseCount,
            Self.ClassName + ': No responses to both BYE and re-INVITE');

      Check(Bye.InSameDialogAs(Self.LastSentResponse),
            Self.ClassName + ': No response for BYE');
      CheckEquals(SIPOK,
                  Self.LastSentResponse.StatusCode,
                  Self.ClassName + ': Wrong response for BYE');

      Check(ReInvite.Match(Self.SecondLastSentResponse),
            Self.ClassName + ': No response for re-INVITE');
      CheckEquals(SIPRequestTerminated,
                  Self.SecondLastSentResponse.StatusCode,
                  Self.ClassName + ': Wrong response for re-INVITE');
    finally
      Bye.Free;
    end;
  finally
    ReInvite.Free;
  end;
end;

procedure TestTIdSipSession.TestReceiveInDialogReferWithNoSubscribeModule;
var
  Refer:   TIdSipRequest;
  Session: TIdSipSession;
  SubMod:  TIdSipSubscribeModule;
  Them:    TIdSipFromToHeader;
  Us:      TIdSipAddressHeader;
begin
  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);

  if Session.IsOutboundCall then begin
    Them    := Session.InitialRequest.ToHeader;
    Us      := Session.InitialRequest.From;
    Us.Grid := Session.InitialRequest.FirstContact.Grid;
  end
  else begin
    Them    := Session.InitialRequest.From;
    Us      := Session.InitialRequest.ToHeader;
    Us.Grid := Self.LastSentResponse.FirstContact.Grid;
  end;

  Us.RemoveParameter(TagParam);

  SubMod := TIdSipSubscribeModule.Create(Self.Core);
  try
    Refer := SubMod.CreateRefer(Them, Us, Self.Destination, TIdSipRequest.DefaultMaxForwards);
    try
      Refer.CallID       := Session.Dialog.ID.CallID;
      Refer.From.Tag     := Session.Dialog.ID.RemoteTag;
      Refer.ToHeader.Tag := Session.Dialog.ID.LocalTag;

      Self.MarkSentResponseCount;
      Self.ReceiveRequest(Refer);
      CheckResponseSent('No response sent');
      CheckEquals(SIPNotImplemented,
                  Self.LastSentResponse.StatusCode,
                  'Unexpected response sent');
    finally
      Refer.Free;
    end;
  finally
    SubMod.Free;
  end;
end;

procedure TestTIdSipSession.TestReceiveInDialogRefer;
var
  Refer:   TIdSipRequest;
  Session: TIdSipSession;
  SubMod:  TIdSipSubscribeModule;
  Them:    TIdSipFromToHeader;
  Us:      TIdSipAddressHeader;
begin
  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);

  if Session.IsOutboundCall then begin
    Them    := Session.InitialRequest.ToHeader;
    Us      := Session.InitialRequest.From;
    Us.Grid := Session.InitialRequest.FirstContact.Grid;
  end
  else begin
    Them    := Session.InitialRequest.From;
    Us      := Session.InitialRequest.ToHeader;
    Us.Grid := Self.LastSentResponse.FirstContact.Grid;
  end;

  Us.RemoveParameter(TagParam);

  SubMod := Self.Core.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  SubMod.AddPackage(TIdSipReferPackage);
  Refer := SubMod.CreateRefer(Them, Us, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    Refer.CallID       := Session.Dialog.ID.CallID;
    Refer.From.Tag     := Session.Dialog.ID.RemoteTag;
    Refer.ToHeader.Tag := Session.Dialog.ID.LocalTag;

    Check(Session.Match(Refer),
          'Session should match the REFER since they both use the same dialog');

    Self.ReceiveRequest(Refer);
    Check(Self.OnReferralFired,
          'Session didn''t receive the REFER');

    Check(Assigned(Self.ReceivingBinding),
          'Session didn''t pass the Binding to its listeners');
  finally
    Refer.Free;
  end;
end;

procedure TestTIdSipSession.TestReceiveOutOfDialogByeTargettingLocalGruu;
var
  Bye:     TIdSipRequest;
  Session: TIdSipSession;
begin
  Self.UseGruu;

  Session := Self.CreateAndEstablishSession;

  Bye := Self.Core.CreateRequest(MethodBye, Self.Core.From, Session.LocalGruu, TIdSipRequest.DefaultMaxForwards);
  try
    Self.MarkSentResponseCount;
    Self.ReceiveRequest(Bye);

    Check(not Session.IsTerminated, 'Session terminated because it accepted an out-of-dialog BYE!');
    CheckResponseSent('No response sent for the BYE');
    CheckEquals(SIPCallLegOrTransactionDoesNotExist,
                Self.LastSentResponse.StatusCode,
                'Unexpected response sent');

  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipSession.TestModify;
var
  Session: TIdSipSession;
begin
  Session := Self.CreateAndEstablishSession;

  Self.MarkSentRequestCount;
  Session.Modify(Self.SimpleSdp, SdpMimeType);
  CheckRequestSent(Session.ClassName + ': No INVITE sent');

  Self.ReceiveOkWithBody(Self.LastSentRequest,
                         Format(DummySDP, ['127.0.0.1']),
                         SdpMimeType);
  Check(Self.OnModifiedSessionFired,
        Session.ClassName + ': OnModifiedSession didn''t fire');

  CheckEquals(Self.SimpleSdp,
              Session.LocalSessionDescription,
              'Session.LocalSessionDescription');
  CheckEquals(SdpMimeType,
              Session.LocalMimeType,
              'Session.LocalMimeType');
  CheckEquals(Format(DummySDP, ['127.0.0.1']),
              Self.RemoteSessionDescription,
              'RemoteSessionDescription');
  CheckEquals(SdpMimeType,
              Self.MimeType,
              'MimeType');
  CheckEquals(Self.RemoteSessionDescription,
              Session.RemoteSessionDescription,
              'Session.RemoteSessionDescription');
  CheckEquals(Self.MimeType,
              Session.RemoteMimeType,
              'Session.RemoteMimeType');
end;

procedure TestTIdSipSession.TestRejectInviteWhenInboundModificationInProgress;
var
  FirstInvite: TIdSipRequest;
  Session:     TIdSipSession;
begin
  //           <established session>
  //  <---           INVITE 1           ---
  //  <---           INVITE 2           ---
  //   ---  491 Request Pending (for 2) --->
  //  <---         ACK (for 2)          ---
  //   ---        200 OK (for 1)        --->
  //  <---        ACK (for 1)           ---

  FirstInvite := TIdSipRequest.Create;
  try
    Session := Self.CreateAndEstablishSession;

    Self.ReceiveRemoteReInvite(Session);
    FirstInvite.Assign(Self.LastSentRequest);
    Check(Self.OnModifySessionFired,
          Session.ClassName + ': OnModifySession didn''t fire');

    Self.MarkSentResponseCount;
    Self.OnModifySessionFired := false;
    Self.ReceiveRemoteReInvite(Session);
    Check(not Self.OnModifySessionFired,
          Session.ClassName + ': OnModifySession fired for a 2nd modify');
    CheckResponseSent(Session.ClassName + ': No 491 response sent');
    CheckEquals(SIPRequestPending,
                Self.LastSentResponse.StatusCode,
                Session.ClassName + ': Unexpected response to 2nd INVITE');
    Check(Self.Invite.Match(Self.LastSentResponse),
          Session.ClassName + ': Response doesn''t match 2nd INVITE');
    Self.ReceiveAck;
    Check(Session.ModificationInProgress,
          Session.ClassName + ': Modification should still be ongoing');

    Self.MarkSentResponseCount;
    Session.AcceptModify('', '');

    CheckResponseSent(Session.ClassName + ': No 200 response sent');
    CheckEquals(SIPOK,
                Self.LastSentResponse.StatusCode,
                Session.ClassName + ': Unexpected response to 1st INVITE');
    Check(FirstInvite.Match(Self.LastSentResponse),
          Session.ClassName + ': Response doesn''t match 1st INVITE');

    Self.ReceiveAckFor(FirstInvite,
                       Self.LastSentResponse);
    Check(not Session.ModificationInProgress,
          Session.ClassName + ': Modification should have finished');
  finally
    FirstInvite.Free;
  end;
end;

procedure TestTIdSipSession.TestRejectInviteWhenOutboundModificationInProgress;
var
  FirstInvite: TIdSipRequest;
  Session:     TIdSipSession;
begin
  //          <established session>
  //   ---           INVITE 1           --->
  //  <---           INVITE 2           ---
  //   ---  491 Request Pending (for 2) --->
  //  <---         ACK (for 2)          ---
  //  <---        200 OK (for 1)        ---
  //   ---        ACK (for 1)           --->

  FirstInvite := TIdSipRequest.Create;
  try
    Session := Self.CreateAndEstablishSession;
    Session.AddSessionListener(Self);

    Self.MarkSentRequestCount;
    Session.Modify('', '');
    CheckRequestSent('No modifying INVITE sent: ' + Self.FailReason);
    FirstInvite.Assign(Self.LastSentRequest);

    Self.MarkSentResponseCount;
    Self.ReceiveRemoteReInvite(Session);
    CheckResponseSent(Session.ClassName + ': No 491 response sent');
    CheckEquals(SIPRequestPending,
                Self.LastSentResponse.StatusCode,
                Session.ClassName + ': Unexpected response');
    Self.ReceiveAck;

    Self.MarkSentAckCount;
    Self.ReceiveOk(FirstInvite);
    CheckAckSent(Session.ClassName + ': No ACK sent');
  finally
    FirstInvite.Free;
  end;
end;

procedure TestTIdSipSession.TestRemodify;
const
  Body = 'random data';
var
  Session: TIdSipSession;
begin
  //      <establish session>
  //  ---       INVITE 1       --->
  // <---  491 Request Pending ---
  //  ---         ACK          --->
  //         <time passes>
  //  ---       INVITE 2       ---> (with same body as INVITE 1)

  Session := Self.CreateAndEstablishSession;
  Session.Modify(Body, 'text/plain');

  // Inbound sessions can have a ModifyWaitTime of 0 - and the DebugTimer's
  // set so that it fires zero wait events immediately. Thus, for this test,
  // we switch off that behaviour.
  Self.DebugTimer.TriggerImmediateEvents := false;

  Self.ReceiveResponse(Self.LastSentRequest, SIPRequestPending);

  Self.MarkSentRequestCount;
  Session.Remodify;
  CheckRequestSent(Self.ClassName + ': No request sent');
  CheckEquals(MethodInvite,
              Self.LastSentRequest.Method,
              Self.ClassName + ': Unexpected request sent');
  CheckEquals(Body,
              Self.LastSentRequest.Body,
              Self.ClassName + ': Unexpected body in request');
end;

procedure TestTIdSipSession.TestRemoveSessionListener;
begin
  Fail(Self.ClassName + ' MUST override this test');
end;

procedure TestTIdSipSession.TestTerminateByeTransactionTimesOut;
var
  Session: TIdSipSession;
  T:       TIdSipTestTimerQueueListener;
begin
  // The bug known as PR 404 manifested by an access violation during the
  // running of a TIdSipClientNonInviteTransactionTimerFWait. A TIdSipSession
  // listened to its TIdSipOutboundBye, but then terminated. When the BYE failed
  // (because, say, the proxy connecting the two parties failed, or the remote
  // party simply disappeared), that TIdSipOutboundBye notified its listeners -
  // including the terminated and freed TIdSipSession. The TIdSipSession mustn't
  // listen to the OutboundBye because as far as the local party's concerned it
  // doesn't matter whether the BYE transaction succeeds or fails - the local
  // end of the session is still torn down as soon as that BYE is sent.                                                                      

  Session := Self.CreateAndEstablishSession;
  Session.Terminate;
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionSendWait);
  Self.Core.Actions.CleanOutTerminatedActions;

  T := TIdSipTestTimerQueueListener.Create;
  try
    Self.DebugTimer.AddListener(T);
    Self.DebugTimer.TriggerAllEventsOfType(TIdSipClientNonInviteTransactionTimerFWait);

    if T.ExceptionFired then
      Fail('Exception occured: ' + T.ExceptionType.ClassName + ': ' + T.ExceptionMessage
         + ' while executing a ' + T.WaitType.ClassName);

  finally
    Self.DebugTimer.RemoveListener(T);
    T.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipInboundSession                                                   *
//******************************************************************************
//* TestTIdSipInboundSession Public methods ************************************

procedure TestTIdSipInboundSession.SetUp;
begin
  inherited SetUp;

  Self.Core.AddListener(Self);
  Self.Core.InviteModule.AddListener(Self);

  Self.OnEndedSessionFired    := false;
  Self.OnModifiedSessionFired := false;
  Self.SentRequestTerminated  := false;

  Self.Invite.ContentType   := SdpMimeType;
  Self.Invite.Body          := Self.SimpleSdp;
  Self.Invite.ContentLength := Length(Self.Invite.Body);
end;

procedure TestTIdSipInboundSession.TearDown;
begin
  Self.Core.TerminateAllCalls;

  inherited TearDown;
end;

//* TestTIdSipInboundSession Protected methods *********************************

procedure TestTIdSipInboundSession.CheckResendWaitTime(Milliseconds: Cardinal;
                                                       const Msg: String);
begin
  Check(Milliseconds <= 2000, Msg);

  inherited CheckResendWaitTime(Milliseconds, Msg);
end;

function TestTIdSipInboundSession.CreateAction: TIdSipAction;
begin
  Self.Invite.Body := Self.RemoteDesc;

  if (Self.Invite.Body <> '') then
    Self.Invite.ContentType := Self.RemoteContentType;

  Self.Invite.ContentLength := Length(Self.RemoteDesc);

  Self.Invite.LastHop.Branch := Self.Core.NextBranch;
  Self.Invite.From.Tag       := Self.Core.NextTag;
  Self.ReceiveInvite;

  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Result := Self.Session;
end;

procedure TestTIdSipInboundSession.EstablishSession(Session: TIdSipSession);
begin
  (Session as TIdSipInboundSession).AcceptCall('', '');
  Self.ReceiveAck;
end;

procedure TestTIdSipInboundSession.OnEndedSession(Session: TIdSipSession;
                                                  ErrorCode: Cardinal;
                                                  const Reason: String);
begin
  inherited OnEndedSession(Session, ErrorCode, Reason);
  Self.ActionFailed := true;

  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipInboundSession.OnInboundCall(UserAgent: TIdSipInviteModule;
                                                 Session: TIdSipInboundSession);
begin
  Self.Session := Session;
  Self.Session.AddSessionListener(Self);
end;

procedure TestTIdSipInboundSession.OnSendResponse(Response: TIdSipResponse;
                                                  Sender: TIdSipTransport;
                                                  Binding: TIdConnectionBindings);
begin
  inherited OnSendResponse(Response, Sender, Binding);

  if (Response.StatusCode = SIPRequestTerminated) then
    Self.SentRequestTerminated := true;
end;

//* TestTIdSipInboundSession Private methods ***********************************

procedure TestTIdSipInboundSession.CheckDefaultPreferredTransport(Response: TIdSipResponse; ExpectParam: Boolean; MsgPrefix: String);
begin
  Check(Response.HasContact, MsgPrefix + ': No Contact header: 100 Trying');
  CheckEquals(ExpectParam, Response.FirstContact.Address.HasParameter(TransportParam),
              MsgPrefix + ': Transport param presence');
end;

procedure TestTIdSipInboundSession.CheckPreferredTransport(Session: TIdSipInboundSession; PreferredTransport: String; MsgPrefix: String);
begin
  Self.Core.Dispatcher.SetPreferredTransportTypeFor(Format('%s/%s', [Self.LanNetwork, Self.LanNetmask]), PreferredTransport);

  Self.CheckPreferredTransportWithoutSettingPreferredTransport(Session, PreferredTransport, MsgPrefix);
end;

procedure TestTIdSipInboundSession.CheckPreferredTransportWithoutSettingPreferredTransport(Session: TIdSipInboundSession; ExpectedTransport: String; MsgPrefix: String);
begin
  Self.MarkSentResponseCount;
  Session.SendProvisional;
  CheckResponseSent(MsgPrefix + ': No 183 Session Progress sent');
  Check(Self.LastSentResponse.HasContact, MsgPrefix + ': No Contact header: 183 Session Progress');
  CheckEquals(ExpectedTransport, Self.LastSentResponse.FirstContact.Address.Transport,
              MsgPrefix + ': Transport param not set: 180 Ringing');

  Self.MarkSentResponseCount;
  Session.AcceptCall('', '');
  CheckResponseSent(MsgPrefix + ': No 200 OK sent');
  Check(Self.LastSentResponse.HasContact, MsgPrefix + ': No Contact header: 200 OK');
  CheckEquals(ExpectedTransport, Self.LastSentResponse.FirstContact.Address.Transport,
              MsgPrefix + ': Transport param not set: 200 OK');
end;

procedure TestTIdSipInboundSession.CheckRedirectCall(TemporaryMove: Boolean);
var
  Dest:         TIdSipAddressHeader;
  SentResponse: TIdSipResponse;
begin
  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');
  Self.MarkSentResponseCount;

  Dest := TIdSipAddressHeader.Create;
  try
    Dest.DisplayName := 'Wintermute';
    Dest.Address.Uri := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';

    Self.Session.RedirectCall(Dest, TemporaryMove);
    CheckResponseSent('No response sent');

    SentResponse := Self.LastSentResponse;
    CheckEquals(TIdSipInboundInvite.RedirectStatusCode(TemporaryMove),
                SentResponse.StatusCode,
                'Wrong response sent');
    Check(SentResponse.HasHeader(ContactHeaderFull),
          'No Contact header');
    CheckEquals(Dest.DisplayName,
                SentResponse.FirstContact.DisplayName,
                'Contact display name');
    CheckEquals(Dest.Address.Uri,
                SentResponse.FirstContact.Address.Uri,
                'Contact address');

    Check(Self.OnEndedSessionFired, 'OnEndedSession didn''t fire');
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipInboundSession.CheckSendProvisionalWithInappropriateStatusCode(Session: TIdSipInboundSession;
                                                                      StatusCode: Cardinal);
begin
  try
    Session.SendProvisional(StatusCode);
    Fail('StatusCode ' + IntToStr(StatusCode) + ' is not a provisional Status-Code');
  except
    on EIdSipTransactionUser do;
  end;
end;

procedure TestTIdSipInboundSession.OnNewData(Data: TIdRTPPayload;
                                             Binding: TIdConnectionBindings);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipInboundSession.ReceiveAckWithBody(const SessionDesc,
                                                      ContentType: String);
var
  Ack: TIdSipRequest;
begin
  Ack := Self.Invite.AckFor(Self.LastSentResponse);
  try
    Ack.Body          := SessionDesc;
    Ack.ContentType   := ContentType;
    Ack.ContentLength := Length(Ack.Body);

    Self.ReceiveRequest(Ack);
  finally
    Ack.Free;
  end;
end;

//* TestTIdSipInboundSession Published methods ****************************************

procedure TestTIdSipInboundSession.TestAcceptCall;
var
  Answer:         String;
  AnswerMimeType: String;
begin
  Self.RemoteContentType := SdpMimeType;
  Self.RemoteDesc        := TIdSipTestResources.BasicSDP('proxy.tessier-ashpool.co.luna');

  Self.CreateAction;

  CheckEquals(Self.LastSentRequest.FirstContact.AsString,
              Self.Session.RemoteContact.AsString,
              'RemoteContact');
  CheckEquals(Self.LastSentRequest.From.Value,
              Self.Session.RemoteParty.Value,
              'RemoteParty');

  CheckEquals(Self.RemoteDesc,
              Self.Session.RemoteSessionDescription,
              'RemoteSessionDescription');
  CheckEquals(Self.RemoteContentType,
              Self.Session.RemoteMimeType,
              'RemoteMimeType');

  Answer         := TIdSipTestResources.BasicSDP('public.booth.org');
  AnswerMimeType := SdpMimeType;

  Self.MarkSentResponseCount;
  Self.Session.AcceptCall(Answer, AnswerMimeType);
  Self.CheckResponseSent('No 200 OK sent');

  Check(not Self.Session.IsEarly, 'Dialog still in Early state');

  Check(Self.Session.DialogEstablished,
        'Dialog not established');
  CheckNotNull(Self.Session.Dialog,
               'Dialog object wasn''t created');

  CheckEquals(Answer,         Self.Session.LocalSessionDescription, 'LocalSessionDescription');
  CheckEquals(AnswerMimeType, Self.Session.LocalMimeType,           'LocalMimeType');

  CheckEquals(Self.LastSentResponse.FirstContact.AsString,
              Self.Session.LocalGruu.AsString,
              'LocalGRUU property not set');
end;

procedure TestTIdSipInboundSession.TestAcceptCallWithGruu;
begin
  Self.UseGruu;

  Self.CreateAction;

  Self.MarkSentResponseCount;
  Self.Session.AcceptCall('', '');
  Self.CheckResponseSent('No 200 OK sent');

  // draft-ietf-sip-gruu section 8.1
  Check(Self.LastSentResponse.FirstContact.Address.HasParameter(GridParam),
        'GRUUs sent out in 200 OKs to INVITEs should have a "grid" parameter');
  CheckEquals(Self.LastSentResponse.FirstContact.AsString,
              Self.Session.LocalGruu.AsString,
              'LocalGRUU property not set');
end;

procedure TestTIdSipInboundSession.TestAcceptCallWithAnswerInAck;
begin
  // <--- INVITE without SDP ---
  //  --- 200 OK with offer  --->
  // <---  ACK with answer   ---

  Self.RemoteDesc := '';
  Self.CreateAction;
  CheckEquals(Self.RemoteDesc,
              Self.Session.RemoteSessionDescription,
              'Remote session description known');
  CheckEquals('',
              Self.Session.LocalSessionDescription,
              'Local session description known');

  Self.Session.AcceptCall(Self.SimpleSdp, SdpMimeType);
  CheckEquals(Self.SimpleSdp,
              Self.Session.LocalSessionDescription,
              'Local session description (offer) not known');

  Self.ReceiveAckWithBody(Self.SimpleSdp, SdpMimeType);
  CheckEquals(Self.SimpleSdp,
              Self.Session.RemoteSessionDescription,
              'Remote session description (answer) not known');
end;

procedure TestTIdSipInboundSession.TestAcceptCallWithNoSDPInAck;
var
  LocalDesc:  String;
  RemoteDesc: String;
begin
  // This case shouldn't happen with a SIP-compliant UA. SIPcon1 does (as of
  // 2006/10/06) send an ACK with no SDP, so it behooves us to behave sanely. We
  // just ignore the ACK's (lack of) session description and treat the INVITE's
  // as the remote session description.
  //
  // <--- INVITE with offer  ---
  //  --- 200 OK with answer --->
  // <---  ACK with no SDP   ---

  Self.RemoteContentType := SdpMimeType;
  Self.RemoteDesc        := Self.MultiStreamSdp;

  LocalDesc  := Self.SimpleSdp;
  RemoteDesc := Self.RemoteDesc;

  Self.CreateAction;
  CheckEquals(RemoteDesc,
              Self.Session.RemoteSessionDescription,
              'Remote session description (offer) not known');
  CheckEquals('',
              Self.Session.LocalSessionDescription,
              'Local session description known');

  Self.Session.AcceptCall(LocalDesc, SdpMimeType);
  CheckEquals(LocalDesc,
              Self.Session.LocalSessionDescription,
              'Local session description (answer) not known');

  Self.ReceiveAckWithBody('', '');
  CheckEquals(RemoteDesc,
              Self.Session.RemoteSessionDescription,
              'Remote session description (answer) not known');
end;

procedure TestTIdSipInboundSession.TestAddSessionListener;
var
  L1, L2: TIdSipTestSessionListener;
begin
  Self.CreateAction;
  Self.Session.AcceptCall('', '');
  Self.ReceiveAck;

  L1 := TIdSipTestSessionListener.Create;
  try
    L2 := TIdSipTestSessionListener.Create;
    try
      Self.Session.AddSessionListener(L1);
      Self.Session.AddSessionListener(L2);

      Self.Session.Terminate;

      Check(L1.EndedSession, 'First listener not notified');
      Check(L2.EndedSession, 'Second listener not notified');

      Self.Session.RemoveSessionListener(L1);
      Self.Session.RemoveSessionListener(L2);
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipInboundSession.TestCancelAfterAccept;
var
  CancelResponse: TIdSipResponse;
  InviteResponse: TIdSipResponse;
  SessionCount:   Integer;
begin
  // <--- INVITE ---
  //  --- 200 OK --->
  // <---  ACK   ---
  // <--- CANCEL ---
  //  --- 200 OK --->
  Self.CreateAction;
  Self.Session.AcceptCall('', '');

  Self.MarkSentResponseCount;
  SessionCount  := Self.Core.SessionCount;
  Self.ReceiveCancel;

  CheckEquals(SessionCount,
              Self.Core.SessionCount,
              'Session terminated and the UA cleaned it up');
  Check(not Self.Session.IsTerminated,
        'Session terminated');
  CheckResponseSent('No response sent');

  CancelResponse := Self.LastSentResponse;
  InviteResponse := Self.SecondLastSentResponse;

  CheckEquals(SIPOK,
              CancelResponse.StatusCode,
              'Unexpected Status-Code for CANCEL response');
  CheckEquals(MethodCancel,
              CancelResponse.CSeq.Method,
              'Unexpected CSeq method for CANCEL response');

  CheckEquals(SIPOK,
              InviteResponse.StatusCode,
              'Unexpected Status-Code for INVITE response');
  CheckEquals(MethodInvite,
              InviteResponse.CSeq.Method,
              'Unexpected CSeq method for INVITE response');
end;

procedure TestTIdSipInboundSession.TestCancelBeforeAccept;
var
  SessionCount: Integer;
begin
  // <---         INVITE         ---
  // <---         CANCEL         ---
  //  ---         200 OK         ---> (for the CANCEL)
  //  --- 487 Request Terminated ---> (for the INVITE)
  // <---           ACK          ---
  Self.CreateAction;
  SessionCount := Self.Core.SessionCount;

  Self.ReceiveCancel;

  // The UA clears out terminated sessions as soon as it finishes handling
  // a message, so the session should have terminated.
  Check(Self.Core.SessionCount < SessionCount,
        Self.ClassName + ': Session didn''t terminate');

  Check(Self.OnEndedSessionFired,
        Self.ClassName + ': Session didn''t notify listeners of ended session');
  CHeckEquals(NoError,
              Self.ErrorCode,
              Self.ClassName + ': A remote cancel''s not an erroneous condition. ErrorCode set.');
  CheckEquals('',
              Self.Reason,
              Self.ClassName + ': Reason param set');
end;

procedure TestTIdSipInboundSession.TestReceiveOutOfOrderReInvite;
var
  Response: TIdSipResponse;
begin
  // <--- INVITE (Branch = z9hG4bK776asdhds)  ---
  //  ---         100 Trying                  --->
  //  ---         180 Ringing                 --->
  //  ---         200 OK                      --->
  // <--- INVITE (Branch = z9hG4bK776asdhds1) ---
  //  ---         100 Trying                  --->
  //  ---         180 Ringing                 --->
  //  ---         500 Internal Server Error   --->

  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Self.Session.AcceptCall('', '');

  Self.Invite.LastHop.Branch := Self.Invite.LastHop.Branch + '1';
  Self.Invite.CSeq.SequenceNo := Self.Invite.CSeq.SequenceNo - 1;
  Self.Invite.ToHeader.Tag := Self.LastSentResponse.ToHeader.Tag;

  Self.MarkSentResponseCount;
  Self.ReceiveInvite;
  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPInternalServerError,
              Response.StatusCode,
              'Unexpected response (' + Response.StatusText + ')');
  CheckEquals(RSSIPRequestOutOfOrder,
              Response.StatusText,
              'Unexpected response, status text');
end;

procedure TestTIdSipInboundSession.TestCancelNotifiesSession;
var
  SessionCount: Integer;
begin
  Self.CreateAction;
  SessionCount := Self.Core.SessionCount;

  Self.ReceiveCancel;

  Check(Self.OnEndedSessionFired,
        Self.ClassName + ': No notification of ended session');

  Check(Self.Core.SessionCount < SessionCount,
        Self.ClassName + ': Session not marked as terminated');
end;

procedure TestTIdSipInboundSession.TestInterleavedResponseSendTimingFailure;
begin
  // 1. We receive an INVITE, and automatically send a 100 Trying and a 180
  //    Ringing. There're two possible locations for the responses.
  // 2. We accept the call, sending a 200 OK.
  // 3. The first location fails for the 180 Ringing.
  // 4. The first 200 OK succeeds, because we receive an ACK.
  // 5. Result: a successfully established session.

  Self.CreateAction;

  // Set up two destinations for the responses.
  Self.Locator.NameRecords.Clear;
  Self.Locator.AddA(Self.Session.InitialRequest.LastHop.SentBy, '127.0.0.1');
  Self.Locator.AddA(Self.Session.InitialRequest.LastHop.SentBy, '127.0.0.2');

  Self.Session.AcceptCall('', '');

  // Failed 180 Ringing
  Self.Dispatcher.Transport.FireOnException(Self.SecondLastSentResponse, EIdConnectTimeout, '10051', 'Host not found');

  Self.ReceiveAckWithBody('', '');

  Check(Self.OnEstablishedSessionFired, 'No session established');
end;

procedure TestTIdSipInboundSession.TestInviteExpires;
begin
  Self.MarkSentResponseCount;

  Self.Invite.Expires.NumericValue := 50;
  Self.CreateAction;

  Self.DebugTimer.TriggerEarliestEvent;

  CheckResponseSent('No response sent');
  CheckEquals(SIPRequestTerminated,
              Self.LastSentResponse.StatusCode,
              'Unexpected response sent');

  CheckEquals(0, Self.Core.SessionCount, 'Expired session not cleaned up');
end;

procedure TestTIdSipInboundSession.TestInviteHasNoOffer;
var
  Answer:     String;
  AnswerType: String;
  Offer:      String;
  OfferType:  String;
begin
  // <--- INVITE (with no body) ---
  //  ---  200 OK (with offer)  ---
  // <---   ACK (with answer)   ---
  Self.RemoteContentType := '';
  Self.RemoteDesc        := '';
  Self.CreateAction;

  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Offer := TIdSipTestResources.BasicSDP('localhost');
  OfferType := SdpMimeType;

  Self.MarkSentResponseCount;
  Self.Session.AcceptCall(Offer, OfferType);

  CheckResponseSent('No 200 OK sent');
  CheckEquals(Offer,
              Self.LastSentResponse.Body,
              'Offer');
  CheckEquals(OfferType,
              Self.LastSentResponse.ContentType,
              'Offer MIME type');

  Answer     := TIdSipTestResources.BasicSDP('remotehost');
  AnswerType := SdpMimeType;

  Self.ReceiveAckWithBody(Answer, AnswerType);
  CheckEquals(Answer,
              Self.Session.RemoteSessionDescription,
              'RemoteSessionDescription');
  CheckEquals(AnswerType,
              Self.Session.RemoteMimeType,
              'RemoteMimeType');
end;

procedure TestTIdSipInboundSession.TestInviteHasOffer;
var
  Answer:     String;
  AnswerType: String;
begin
  // <---    INVITE (with offer)     ---
  //  ---    200 OK (with answer)    ---
  // <--- ACK (with repeat of offer) ---
  Self.RemoteContentType := SdpMimeType;
  Self.RemoteDesc        := TIdSipTestResources.BasicSDP('1.2.3.4');
  Self.CreateAction;

  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Answer := TIdSipTestResources.BasicSDP('localhost');
  AnswerType := SdpMimeType;

  Self.MarkSentResponseCount;
  Self.Session.AcceptCall(Answer, AnswerType);

  CheckResponseSent('No 200 OK sent');
  CheckEquals(Answer,
              Self.LastSentResponse.Body,
              'Answer');
  CheckEquals(AnswerType,
              Self.LastSentResponse.ContentType,
              'Answer MIME type');
end;

procedure TestTIdSipInboundSession.TestIsInbound;
begin
  Self.CreateAction;
  Check(Self.Session.IsInbound,
        Self.Session.ClassName + ' not marked as inbound');
end;

procedure TestTIdSipInboundSession.TestIsOutbound;
begin
  Self.CreateAction;
  Check(not Self.Session.IsOutbound,
        Self.Session.ClassName + ' marked as outbound');
end;

procedure TestTIdSipInboundSession.TestIsOutboundCall;
begin
  Self.CreateAction;
  Check(not Self.Session.IsOutboundCall,
        'Inbound session; IsOutboundCall');
end;

procedure TestTIdSipInboundSession.TestInviteWithReplaces;
var
  Replaces: TIdSipRequest;
begin
  Self.CreateAction;

  Replaces := Self.Core.InviteModule.CreateInvite(Self.Core.From, Self.Destination, '', '', TIdSipRequest.DefaultMaxForwards);
  try
    Replaces.Replaces.CallID  := Self.Session.Dialog.ID.CallID;
    Replaces.Replaces.FromTag := Self.Session.Dialog.ID.RemoteTag;
    Replaces.Replaces.ToTag   := Self.Session.Dialog.ID.LocalTag;

    Check(Self.Session.Match(Replaces),
          'INVITE with Replaces header');
  finally
    Replaces.Free;
  end;
end;

procedure TestTIdSipInboundSession.TestLocalGruu;
var
  Session: TIdSipSession;
begin
  Self.UseGruu;

  Self.MarkSentResponseCount;
  Session := Self.CreateAndEstablishSession;
  CheckResponseSent('No 200 OK sent');

  Check(Self.LastSentResponse.FirstContact.Address.HasGrid,
        '200 OK Remote Target has no "grid" parameter');

  CheckEquals(Self.LastSentResponse.FirstContact.Grid,
              Session.LocalGruu.Grid,
              'LocalGRUU''s "grid" doesn''t match that in the 200 OK''s Contact');
  CheckEquals(Self.LastSentResponse.FirstContact.AsString,
              Session.LocalGruu.AsString,
              'LocalGRUU doesn''t match the 200 OK''s Contact');
end;

procedure TestTIdSipInboundSession.TestMethod;
begin
  CheckEquals(MethodInvite,
              Self.CreateAction.Method,
              'Inbound session; Method');
end;

procedure TestTIdSipInboundSession.TestNotifyListenersOfEstablishedSession;
var
  Answer:         String;
  AnswerMimeType: String;
  Listener:       TIdSipTestSessionListener;
begin
  Answer         := TIdSipTestResources.BasicSDP('public.booth.org');
  AnswerMimeType := SdpMimeType;
  Self.RemoteContentType := SdpMimeType;
  Self.RemoteDesc        := TIdSipTestResources.BasicSDP('proxy.tessier-ashpool.co.luna');
  Self.CreateAction;

  Listener := TIdSipTestSessionListener.Create;
  try
    Self.Session.AddSessionListener(Listener);
    Self.Session.AcceptCall(Answer, AnswerMimeType);

    Self.ReceiveAckWithBody(Self.RemoteDesc, Self.RemoteContentType);

    Check(Listener.EstablishedSession, 'No EstablishedSession notification');
  finally
    Self.Session.RemoveSessionListener(Listener);
    Listener.Free;
  end;
end;

procedure TestTIdSipInboundSession.TestNotifyListenersOfEstablishedSessionInviteHasNoBody;
var
  Answer:         String;
  AnswerMimeType: String;
  Listener:       TIdSipTestSessionListener;
begin
  Answer         := TIdSipTestResources.BasicSDP('public.booth.org');
  AnswerMimeType := SdpMimeType;
  Self.RemoteContentType := '';
  Self.RemoteDesc        := '';
  Self.CreateAction;

  Listener := TIdSipTestSessionListener.Create;
  try
    Self.Session.AddSessionListener(Listener);
    Self.Session.AcceptCall(Answer, AnswerMimeType);

    Self.ReceiveAckWithBody(Self.RemoteDesc, Self.RemoteContentType);

    Check(Listener.EstablishedSession, 'No EstablishedSession notification');
  finally
    Self.Session.RemoveSessionListener(Listener);
    Listener.Free;
  end;
end;

procedure TestTIdSipInboundSession.TestOkUsesGruuWhenUaDoes;
var
  Ok: TIdSipResponse;
begin
  Self.UseGruu;

  // Set up the remote stack to use GRUUs
  Self.Invite.Supported.Values.Add(ExtensionGruu);

  Self.MarkSentResponseCount;
  Self.CreateAndEstablishSession;
  Self.CheckResponseSent('No response sent');
  Ok := Self.LastSentResponse;
  Check(Ok.HasHeader(SupportedHeaderFull),
        'OK missing a Supported header');
  Check(Ok.SupportsExtension(ExtensionGruu),
        'OK''s Supported header doesn''t indicate support of GRUU');
  Check(Self.LastSentResponse.FirstContact.IsGruu,
        'GRUU not used as OK''s Contact');
end;

procedure TestTIdSipInboundSession.TestInboundModifyBeforeFullyEstablished;
var
  InternalServerError: TIdSipResponse;
  Invite:              TIdSipRequest;
  Ringing:             TIdSipResponse;
begin
  //  <---           INVITE          --- (with CSeq: n INVITE)
  //   ---         100 Trying        --->
  //   ---         180 Ringing       --->
  //  <---           INVITE          --- (with CSeq: n+1 INVITE)
  //   --- 500 Internal Server Error ---> (with Retry-After)
  //  <---            ACK            ---
  //   --->         200 OK           --->
  //  <---            ACK            ---

  // We need the Ringing response to get the To tag - Ringing establishes the
  // dialog!
  Self.CreateAction;

  Ringing := Self.LastSentResponse;
  CheckEquals(SIPRinging,
              Ringing.StatusCode,
              'Sanity check');
  Check(Assigned(Self.Session), 'OnInboundCall not called');
  Check(Self.Session.DialogEstablished,
        'Session should have established a dialog - it''s sent a 180, after all');

  Self.MarkSentResponseCount;
  Invite := TIdSipRequest.Create;
  try
    Invite.Assign(Self.Session.InitialRequest);
    Invite.LastHop.Branch  := Self.Core.NextBranch;
    Invite.CSeq.SequenceNo := Self.Session.InitialRequest.CSeq.SequenceNo + 1;
    Invite.ToHeader.Tag    := Ringing.ToHeader.Tag;
    Self.ReceiveRequest(Invite);
  finally
    Invite.Free;
  end;

  CheckResponseSent('No response sent');

  InternalServerError := Self.LastSentResponse;
  CheckEquals(SIPInternalServerError,
              InternalServerError.StatusCode,
              'Unexpected response');
  Check(InternalServerError.HasHeader(RetryAfterHeader),
        'No Retry-After header');
  Check(InternalServerError.RetryAfter.NumericValue <= MaxPrematureInviteRetry,
        'Bad Retry-After value (' + IntToStr(InternalServerError.RetryAfter.NumericValue) + ')');

  Self.ReceiveAck;
end;

procedure TestTIdSipInboundSession.TestInboundModifyReceivesNoAck;
begin
  // <---    INVITE   ---
  //  --- 180 Ringing --->
  // <---     ACK     ---
  // <---    INVITE   ---
  //  ---    200 OK   --->
  //   <no ACK returned>
  //  ---     BYE     --->
  Self.CreateAction;

  Check(Assigned(Self.Session), 'OnInboundCall not called');
  Self.Session.AcceptCall('', '');
  Self.ReceiveAck;

  Self.ReceiveRemoteReInvite(Self.Session);
  Check(Self.OnModifySessionFired,
        'OnModifySession didn''t fire');
  Self.Session.AcceptModify('', '');

  Self.MarkSentRequestCount;

  // This will fire all Resend OK attempts (and possibly some other events),
  // making the inbound INVITE fail.
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionsWait);

  CheckRequestSent('No BYE sent to terminate the dialog');

  CheckEquals(MethodBye,
              Self.LastSentRequest.Method,
              'Unexpected request sent');
end;

procedure TestTIdSipInboundSession.TestProvisionalResponseContactUsesPreferredTransportToFQDN;
const
  PreferredTransport = SctpTransport;
var
  IPAddress: String;
  Mask:      String;
  Network:   String;
  Session:   TIdSipInboundSession;
  Target:    String;
begin
  // Test that Contact URIs in response to requests from a particular fully
  // qualified domain name address space obeys the PreferredTransportType for
  // the network block to which that name resolves. (A name can resolve to more
  // than one IP address!

  IPAddress := Self.Dispatcher.Transport.PeerIP;
  Mask      := TIdIPAddressParser.MaskToAddress(24, Id_IPv4);
  Network   := TIdIPAddressParser.NetworkForIPv4(IPAddress, Mask);

  Session := Self.CreateAction as TIdSipInboundSession;
  Target  := Session.InitialRequest.LastHop.SentBy;

  Self.Core.Dispatcher.SetPreferredTransportTypeFor(Format('%s/%s', [IPAddress, Mask]), PreferredTransport);

  Self.Locator.RemoveNameRecords(Target);
  Self.Locator.AddA(Target, IPAddress);

  Self.CheckPreferredTransportWithoutSettingPreferredTransport(Session, PreferredTransport, 'FQDN sent-by');
end;

procedure TestTIdSipInboundSession.TestProvisionalResponseContactUsesPreferredTransportToIpAddress;
const
  PreferredTransport = SctpTransport;
var
  IPAddress: String;
  Mask:      String;
  Network:   String;
  Session: TIdSipInboundSession;
begin
  // Test that Contact URIs in response to requests from a particular network
  // address space obey the PreferredTransportType for that address space.

  IPAddress := Self.Dispatcher.Transport.PeerIP;
  Mask      := TIdIPAddressParser.MaskToAddress(24, Id_IPv4);
  Network   := TIdIPAddressParser.NetworkFor(IPAddress, Mask);

  Self.Invite.LastHop.SentBy := Self.Dispatcher.Transport.PeerIP;
  Self.Core.Dispatcher.SetPreferredTransportTypeFor(Format('%s/%s', [IPAddress, Mask]), PreferredTransport);

  Session := Self.CreateAction as TIdSipInboundSession;
  Self.CheckPreferredTransportWithoutSettingPreferredTransport(Session, PreferredTransport, 'IP address sent-by');
end;

procedure TestTIdSipInboundSession.TestProvisionalResponseContactForAddressWithDefaultPreferredTransport;
const
  PreferredTransport = SctpTransport;
var
  IPAddress: String;
  Mask:      String;
  Network:   String;
  Session:   TIdSipInboundSession;
begin
  IPAddress := Self.Dispatcher.Transport.PeerIP;
  Mask      := TIdIPAddressParser.MaskToAddress(24, Id_IPv4);
  Network   := TIdIPAddressParser.NetworkFor(IPAddress, Mask);

  Self.Invite.LastHop.SentBy := IPAddress;
  Session := Self.CreateAction as TIdSipInboundSession;

  Self.Core.Dispatcher.DefaultPreferredTransportType := SctpTransport;
  Self.MarkSentRequestCount;
  Session.SendProvisional;
  CheckResponseSent('No 183 Session Progress');
  CheckDefaultPreferredTransport(Self.LastSentResponse, true, 'Changed default transport');
end;

procedure TestTIdSipInboundSession.TestProvisionalResponseContactForAddressWithNoPreferredTransport;
const
  PreferredTransport = SctpTransport;
var
  IPAddress: String;
  Mask:      String;
  Network:   String;
  Session:   TIdSipInboundSession;
begin
  IPAddress := Self.Dispatcher.Transport.PeerIP;
  Mask      := TIdIPAddressParser.MaskToAddress(24, Id_IPv4);
  Network   := TIdIPAddressParser.NetworkFor(IPAddress, Mask);

  Self.Invite.LastHop.SentBy := IPAddress;
  Session := Self.CreateAction as TIdSipInboundSession;

  Self.Core.Dispatcher.DefaultPreferredTransportType := '';
  Self.MarkSentRequestCount;
  Session.SendProvisional;
  CheckResponseSent('No 183 Session Progress sent');
  CheckDefaultPreferredTransport(Self.LastSentResponse, false, 'Default default transport');
end;

procedure TestTIdSipInboundSession.TestRaceConditionAckLost;
var
  AlicesDialog: TIdSipDialog;
  BobsSession:  TIdSipInboundSession;
begin
  // from draft-hasebe-sipping-race-examples-00.txt, section 3.1.6:
  //   Alice                     Bob
  //     |                        |
  //     |       INVITE F1        |
  //     |----------------------->|
  //     |    180 Ringing F2      |
  //     |<-----------------------|
  //     |                        |
  //     |       200 OK F3        |
  //     |<-----------------------|
  //     |   ACK F4(packet loss)  |
  //     |-------------->X        |
  //     |   Both Way RTP Media   |
  //     |<======================>|
  //     | BYE F6         200 F5  |
  //     |---------     ----------|
  //     |          \ /           |
  //     |           X            |
  //     |          / \           |
  //     |<--------     --------->|
  //     |       200 OK F7        |
  //     |<-----------------------|
  //     |                        |
  //     |                        |

  BobsSession := Self.CreateAction as TIdSipInboundSession;
  BobsSession.AcceptCall('', '');

  // Trigger the scheduled 200 OK resend.
  Self.DebugTimer.TriggerAllEventsOfType(TIdSipActionsWait);

  Self.MarkSentResponseCount;
  AlicesDialog := TIdSipDialog.CreateInboundDialog(BobsSession.InitialRequest,
                                                   Self.LastSentResponse,
                                                   false);
  try
    Self.ReceiveBye(AlicesDialog);
  finally
    AlicesDialog.Free;
  end;

  CheckResponseSent('No response sent to the BYE');
  CheckEquals(MethodBye,
              Self.LastSentResponse.CSeq.Method,
              'Response not sent in response to BYE');
  Check(BobsSession.IsTerminated,
        'Bob''s session isn''t terminated');
end;

procedure TestTIdSipInboundSession.TestRaceConditionRetransmittedInitialInviteIgnored;
var
  BobsSession:  TIdSipInboundSession;
  SessionCount: Integer;
begin
  // from draft-hasebe-sipping-race-examples-00.txt, section 3.1.1:
  //   Alice                             Bob
  //     |                                |
  //     |         ini-INVITE F1          |
  //     |------------------------------->|
  //     |      180 F2(Packet loss)       |
  //     |         X<---------------------|
  //     |                                |
  //     | ini-INVITE F4           200 F3 |
  //     |-------------     --------------| Terminate(ServerTransaction)
  //     |              \ /               |
  //     |               X                |
  //     |              / \               |
  //     |<------------     ------------->|
  //     |             ACK F5             |
  //     |------------------------------->|
  //     |                                |
  //     |                                |

  // Bob's provisional responses are lost in the network, so Alice retransmits
  // the ini-INVITE. Despite the retransmitted ini-INVITE not having a To tag,
  // Bob's UA must recognise the retransmitted ini-INVITE as a retransmission
  // and ignore it.

  BobsSession := Self.CreateAction as TIdSipInboundSession;
  BobsSession.AcceptCall('', '');

  SessionCount := Self.Core.SessionCount;
  Self.ReceiveInvite;
  Check(SessionCount = Self.Core.SessionCount,
        'Retransmitted ini-INVITE started a new dialog '
      + '(draft-hasebe-sipping-race-examples-00, section 3.1.1');
end;

procedure TestTIdSipInboundSession.TestReceiveCallMassiveInvite;
const
  MassiveInvite =
    'INVITE sip:lisa@192.168.0.33 SIP/2.0'#13#10
  + 'From: "Thomas"<sip:thomas@sip.example.com>;tag=22b8828-2200a8c0-13c4-29a0f-7bc38b07-29a0f'#13#10
  + 'To: "lisa"<sip:lisa@192.168.0.33>'#13#10
  + 'Call-ID: 22eff48-2200a8c0-13c4-29a0f-25e4a1b7-29a0f@sip.example.com'#13#10
  + 'CSeq: 1 INVITE'#13#10
  + 'Via: SIP/2.0/UDP 192.168.0.34:5060;branch=z9hG4bK-29a0f-a29cd2d-35eb934d'#13#10
  + 'Expires: 1800'#13#10
  + 'Max-Forwards: 70'#13#10
  + 'Supported: 100rel,replaces'#13#10
  + 'User-Agent: FranceTelecom/eConf'#13#10
  + 'Accept: application/sdp,audio/telephone-event,text/plain,text/html,application/media_control+xml,application/mc+xml,application/dtmf-relay,message/sipfrag'#13#10
  + 'Contact: <sip:thomas@192.168.0.34>'#13#10
  + 'Allow: INVITE,ACK,BYE,CANCEL,OPTIONS,REFER,PRACK,INFO,MESSAGE,SUBSCRIBE,NOTIFY'#13#10
  + 'Content-Type: application/SDP'#13#10
  + 'Content-Length: 738'#13#10
  + #13#10
  + 'v=0'#13#10
  + 'o=anonymous 1159541940 1159541940 IN IP4 192.168.0.34'#13#10
  + 's=-'#13#10
  + 'i=eConf 4.1'#13#10
  + 'c=IN IP4 192.168.0.34'#13#10
  + 'b=AS:384'#13#10
  + 't=0 0'#13#10
  + 'm=audio 6000 RTP/AVP 102 104 9 108 4 8 0 116'#13#10
  + 'a=rtpmap:102 X-G72x1/16000'#13#10
  + 'a=rtpmap:104 X-G72x24/16000'#13#10
  + 'a=rtpmap:9 G722/8000'#13#10
  + 'a=rtpmap:108 X-G72xH/8000'#13#10
  + 'a=rtpmap:4 G723/8000'#13#10
  + 'a=rtpmap:8 PCMA/8000'#13#10
  + 'a=rtpmap:0 PCMU/8000'#13#10
  + 'a=rtpmap:116 t'#13#10
  + 'elephone-event/8000'#13#10
  + 'a=fmtp:116 0-15'#13#10
  + 'a=sendrecv'#13#10
  + 'm=video 6002 RTP/AVP 97 34 31'#13#10
  + 'b=AS:352'#13#10
  + 'a=rtpmap:97 H263-1998/90000'#13#10
  + 'a=rtpmap:34 H263/90000'#13#10
  + 'a=rtpmap:31 H261/90000'#13#10
  + 'a=TIAS:352000'#13#10
  + 'a=fmtp:97 CIF=1 QCIF=1/I=1 J=1 T=1 N=4 K=1'#13#10
  + 'a=fmtp:34 CIF=1 QCIF=1'#13#10
  + 'a=fmtp:31 CIF=1 QCIF=1'#13#10
  + 'a=sendrecv'#13#10
  + 'm=text 6006 RTP/AVP 98 99'#13#10
  + 'a=rtpmap:98 T140/1000'#13#10
  + 'a=rtpmap:99 RED/1000'#13#10
  + 'a=fmtp:99 98/98'#13#10
  + 'a=sendrecv'#13#10;
var
  Invite: TIdSipRequest;
begin
  // Interoperability testing with FranceTelecom/eConf revealed a bug in this
  // stack, where a 487 Request Terminated is sent in response to a (fragmented)
  // INVITE over UDP. This test proves that the Transaction-User layer isn't to
  // blame.

  Invite := TIdSipRequest.ReadRequestFrom(MassiveInvite);
  try
    Self.MarkSentResponseCount;
    Self.ReceiveRequest(Invite);

    Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
    CheckResponseSent('No 100 Trying or 180 Ringing sent');
    CheckEquals(SIPRinging, Self.LastSentResponse.StatusCode, 'Unexpected response sent');

    // This SDP answer is hopelessly invalid, but we don't care about that in
    // this test.
    Self.MarkSentResponseCount;
    Self.Session.AcceptCall('', '');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipInboundSession.TestReceiveCallSendsTrying;
begin
  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckEquals(Self.ResponseCount + 2,
              Self.SentResponseCount,
              'Session should have sent a 100 Trying and a 180 Ringing');
  CheckEquals(SIPTrying,
              Self.SecondLastSentResponse.StatusCode,
              'First response should be a 100 Trying');
  CheckEquals(SIPRinging,
              Self.LastSentResponse.StatusCode,
              'Second response should be a 180 Ringing');
end;

procedure TestTIdSipInboundSession.TestRedirectCall;
begin
  Self.CheckRedirectCall(true);
end;

procedure TestTIdSipInboundSession.TestRedirectCallPermanent;
begin
  Self.CheckRedirectCall(false);
end;

procedure TestTIdSipInboundSession.TestRejectCallBusy;
begin
  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Self.MarkSentResponseCount;
  Self.Session.RejectCallBusy;
  CheckResponseSent('No response sent');
  CheckEquals(SIPBusyHere,
              Self.LastSentResponse.StatusCode,
              'Wrong response sent');

  Check(Self.OnEndedSessionFired, 'OnEndedSession didn''t fire');
end;

procedure TestTIdSipInboundSession.TestRejectCallStatusCode;
const
  StatusCode = SIPNotFound;
begin
  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Self.MarkSentResponseCount;
  Self.Session.RejectCall(StatusCode);

  CheckResponseSent('No response sent');
  CheckEquals(StatusCode,
              Self.LastSentResponse.StatusCode,
              'Wrong response sent');

  Check(Self.OnEndedSessionFired, 'OnEndedSession didn''t fire');
end;

procedure TestTIdSipInboundSession.TestRejectCallStatusCodeAndText;
const
  StatusCode = SIPNotFound;
  StatusText = 'That dude isn''t here';
begin
  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Self.MarkSentResponseCount;
  Self.Session.RejectCall(StatusCode, StatusText);

  CheckResponseSent('No response sent');
  CheckEquals(StatusCode,
              Self.LastSentResponse.StatusCode,
              'Status-Code not set');
  CheckEquals(StatusText,
              Self.LastSentResponse.StatusText,
              'Status-Text not set');

  Check(Self.OnEndedSessionFired, 'OnEndedSession didn''t fire');
end;

procedure TestTIdSipInboundSession.TestRemoveSessionListener;
var
  L1, L2: TIdSipTestSessionListener;
begin
  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Self.Session.AcceptCall('', '');

  L1 := TIdSipTestSessionListener.Create;
  try
    L2 := TIdSipTestSessionListener.Create;
    try
      Self.Session.AddSessionListener(L1);
      Self.Session.AddSessionListener(L2);
      Self.Session.RemoveSessionListener(L2);

      Self.Session.Terminate;

      Check(L1.EndedSession,
            'First listener not notified');
      Check(not L2.EndedSession,
            'Second listener erroneously notified, ergo not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipInboundSession.TestRing;
var
  Ringing: TIdSipResponse;
begin
  Self.MarkSentResponseCount;
  Self.CreateAction; // This sends a 180 Ringing
  Check(Assigned(Self.Session), 'OnInboundCall not called');
  CheckResponseSent('No response sent');

  Ringing := Self.LastSentResponse;
  CheckEquals(Self.Invite.CallID,
              Ringing.CallID,
              'Ringing Call-ID doesn''t match INVITE''s Call-ID');
  CheckEquals(Self.Invite.CallID,
              Self.Session.Dialog.ID.CallID,
              'Session''s Call-ID doesn''t match INVITE''s Call-ID');
  CheckEquals(Ringing.ToHeader.Tag,
              Self.Session.Dialog.ID.LocalTag,
              'Session''s remote-tag doesn''t match 180''s To tag');
  CheckEquals(Self.Invite.From.Tag,
              Self.Session.Dialog.ID.RemoteTag,
              'Session''s local-tag doesn''t match INVITE''s From tag');
end;

procedure TestTIdSipInboundSession.TestRingWithGruu;
begin
  Self.UseGruu;

  Self.MarkSentResponseCount;
  Self.CreateAction; // This sends a 180 Ringing
  Check(Assigned(Self.Session), 'OnInboundCall not called');
  CheckResponseSent('No response sent');

  Check(Self.LastSentResponse.FirstContact.IsGruu,
        'Response didn''t use GRUU as Contact');
  Check(Self.LastSentResponse.FirstContact.Address.HasGrid,
        'Dialog-creating response (180 Ringing) has no "grid" parameter');
end;

procedure TestTIdSipInboundSession.TestRingWithSuppressLocalResponses;
begin
  Self.Module.SuppressLocalResponses := true;

  Self.MarkSentResponseCount;
  Self.CreateAction;
  CheckResponseSent('No response sent');
  CheckNotEquals(SIPRinging, Self.LastSentResponse.StatusCode, 'Ringing response not suppressed');
end;

procedure TestTIdSipInboundSession.TestSendProvisional;
const
  ReasonPhrase = 'Progress of some kind has been made';
  StatusCode   = SIPQueued;
begin
  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Self.MarkSentResponseCount;
  Self.Session.SendProvisional(StatusCode, ReasonPhrase);
  CheckResponseSent('No response sent');

  CheckEquals(StatusCode,
              Self.LastSentResponse.StatusCode,
              'Unexpected Status-Code');
  CheckEquals(ReasonPhrase,
              Self.LastSentResponse.StatusText,
              'Unexpected Reason-Phrase');
end;

procedure TestTIdSipInboundSession.TestSendProvisionalAfterDialogEstablished;
begin
  // It would be an error to send a provisional response after establishing a
  // dialog, so we just do nothing (in other words don't send a response).

  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');
  Self.Session.AcceptCall('', '');

  Self.MarkSentResponseCount;
  Self.Session.SendProvisional;
  CheckNoResponseSent('A provisional response sent after the dialog''s established');
end;

procedure TestTIdSipInboundSession.TestSendProvisionalAfterEarlyDialogEstablished;
begin
  // If we've established an _early_ dialog, then we may still send provisional
  // responses.

  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Self.MarkSentResponseCount;
  Self.Session.SendProvisional;
  CheckResponseSent('A provisional response not sent after the dialog''s established (as early)');

  Self.MarkSentResponseCount;
  Self.Session.SendProvisional;
  CheckResponseSent('A subsequent provisional response not sent');
end;

procedure TestTIdSipInboundSession.TestSendProvisionalWithInappropriateStatusCode;
var
  I: Integer;
begin
  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  CheckSendProvisionalWithInappropriateStatusCode(Self.Session, 0);
  CheckSendProvisionalWithInappropriateStatusCode(Self.Session, 999);

  for I := SIPLowestOkCode to SIPHighestStatusCode do
    CheckSendProvisionalWithInappropriateStatusCode(Self.Session, I);
end;

procedure TestTIdSipInboundSession.TestSendProvisionalWithDefaults;
begin
  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Self.MarkSentResponseCount;
  Self.Session.SendProvisional;
  CheckResponseSent('No response sent');
  CheckEquals(SIPSessionProgress,
              Self.LastSentResponse.StatusCode,
              'Unexpected Status-Code');
  CheckEquals(RSSIPSessionProgress,
              Self.LastSentResponse.StatusText,
              'Unexpected Reason-Phrase');
end;

procedure TestTIdSipInboundSession.TestSendProvisionalWithGruu;
var
  Response: TIdSipResponse;
begin
  Self.Core.UseGruu := true;

  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Self.MarkSentResponseCount;
  Self.Session.SendProvisional;
  CheckResponseSent('No session progress response sent');

  Response := Self.LastSentResponse;

  Check(Response.HasHeader(SupportedHeaderFull),
        'Response lacks a Supported header');
  Check(Response.SupportsExtension(ExtensionGruu),
        'Supported header lacks indication of GRUU support');
  Check(Response.FirstContact.Address.HasGrid,
        'Response lacks a Contact with a "grid" parameter');
end;

procedure TestTIdSipInboundSession.TestSupportsExtension;
const
  ExtensionFoo = 'foo';
  Extensions   = ExtensionTargetDialog + ', ' + ExtensionFoo;
var
  Session: TIdSipSession;
begin
  Self.Invite.AddHeader(SupportedHeaderFull).Value := Extensions;
  Session := Self.CreateAndEstablishSession;
  Check(Session.SupportsExtension(ExtensionTargetDialog),
        Self.ClassName + ': '
      + ExtensionTargetDialog + ' must be supported, since both we and '
      + 'the remote party support it');
  Check(not Session.SupportsExtension(ExtensionFoo),
        Self.ClassName + ': '
      + ExtensionFoo + ' must not be supported, since only the remote '
      + 'party supports it');
end;

procedure TestTIdSipInboundSession.TestSuppressLocalResponses;
var
  Session: TIdSipInboundSession;
begin
  Self.Module.SuppressLocalResponses := true;

  Session := Self.CreateAction as TIdSipInboundSession;
  Check(Session.SuppressLocalResponses, 'Session not told to suppress 180 Ringing');

  CheckNotEquals(SIPRinging, Self.LastSentResponse.StatusCode, '180 Ringing sent');
end;

procedure TestTIdSipInboundSession.TestTerminate;
var
  Request:      TIdSipRequest;
  SessionCount: Integer;
begin
  Self.CreateAndEstablishSession;

  SessionCount := Self.Core.SessionCount;
  Self.Session.Terminate;

  CheckRequestSent('No BYE sent');

  Request := Self.LastSentRequest;
  Check(Request.IsBye, 'Unexpected last request');

  Check(Self.Core.SessionCount < SessionCount,
        'Session not marked as terminated');
end;

procedure TestTIdSipInboundSession.TestTerminateAlmostEstablishedSession;
var
  Request:      TIdSipRequest;
  SessionCount: Integer;
begin
  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');
  Self.Session.AcceptCall('', '');
  SessionCount := Self.Core.SessionCount;

  // Until we receive an ACK to our INVITE, our session has not fully established.
  Self.MarkSentRequestCount;
  Self.Session.Terminate;

  CheckNoRequestSent('BYE sent prematurely');

  Self.ReceiveAck;

  CheckRequestSent('No BYE sent');

  Request := Self.LastSentRequest;
  Check(Request.IsBye, 'Unexpected last request');

  Check(Self.Core.SessionCount < SessionCount,
        'Session not marked as terminated');
end;

procedure TestTIdSipInboundSession.TestTerminateSignalled;
var
  L: TIdSipTestActionListener;
  Session: TIdSipSession;
begin
  L := TIdSipTestActionListener.Create;
  try
    Session := Self.CreateAndEstablishSession;

    Session.AddActionListener(L);
    try
      Self.Session.Terminate;

      Check(L.Terminated, Self.ClassName + ': Listeners not notified of termination');
    finally
      Session.RemoveActionListener(L);
    end;
  finally
    L.Free;
  end;
end;

procedure TestTIdSipInboundSession.TestTerminateUnestablishedSession;
var
  Response:     TIdSipResponse;
  SessionCount: Integer;
begin
  Self.CreateAction;
  Check(Assigned(Self.Session), Self.ClassName + ': OnInboundCall not called');

  Self.MarkSentResponseCount;
  SessionCount  := Self.Core.SessionCount;

  Self.Session.Terminate;

  CheckResponseSent(Self.ClassName + ': No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPRequestTerminated,
              Response.StatusCode,
              Self.ClassName + ': Unexpected last response');

  Check(Self.Core.SessionCount < SessionCount,
        Self.ClassName + ': Session not marked as terminated');
end;

//******************************************************************************
//* TestTIdSipOutboundSession                                                  *
//******************************************************************************
//* TestTIdSipOutboundSession Public methods ***********************************

procedure TestTIdSipOutboundSession.SetUp;
begin
  inherited SetUp;

  Self.MimeType := SdpMimeType;
  Self.SDP :='v=0'#13#10
           + 'o=franks 123456 123456 IN IP4 127.0.0.1'#13#10
           + 's=-'#13#10
           + 'c=IN IP4 127.0.0.1'#13#10
           + 'm=audio 8000 RTP/AVP 0'#13#10;

  Self.MarkSentRequestCount;
  Self.Session := Self.CreateAction as TIdSipOutboundSession;
  CheckRequestSent(Self.ClassName + ': No INVITE sent');

  Self.RemoteMimeType           := '';
  Self.OnEndedSessionFired      := false;
  Self.OnModifiedSessionFired   := false;
  Self.OnProgressedSessionFired := false;
  Self.RemoteDesc               := '';

  Self.FailFirstInviteSend := false;
end;

//* TestTIdSipOutboundSession Protectedivate methods ***************************

procedure TestTIdSipOutboundSession.CheckResendWaitTime(Milliseconds: Cardinal;
                                                       const Msg: String);
begin
  Check((2100 <= Milliseconds) and (Milliseconds <= 4000), Msg);

  inherited CheckResendWaitTime(Milliseconds, Msg);
end;

function TestTIdSipOutboundSession.CreateAction: TIdSipAction;
var
  Session: TIdSipOutboundSession;
begin
  Session := Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, Self.SDP, Self.MimeType);
  Session.AddActionListener(Self);
  Session.AddSessionListener(Self);
  Session.Send;

  Result := Session;
end;

procedure TestTIdSipOutboundSession.EstablishSession(Session: TIdSipSession);
begin
  Self.ReceiveOk(Self.LastSentRequest);
end;

procedure TestTIdSipOutboundSession.OnEstablishedSession(Session: TIdSipSession;
                                                         const RemoteSessionDescription: String;
                                                         const MimeType: String);
begin
  inherited OnEstablishedSession(Session, RemoteSessionDescription, MimeType);

  Self.RemoteDesc     := RemoteSessionDescription;
  Self.RemoteMimeType := MimeType;

  Session.LocalSessionDescription := Self.LocalDescription;
  Session.LocalMimeType           := Self.LocalMimeType;
end;

procedure TestTIdSipOutboundSession.OnProgressedSession(Session: TIdSipSession;
                                                        Progress: TIdSipResponse);
begin
  inherited OnProgressedSession(Session, Progress);

  Self.OnProgressedSessionFired := true;
end;

procedure TestTIdSipOutboundSession.OnSendRequest(Request: TIdSipRequest;
                                                  Sender: TIdSipTransport;
                                                  Binding: TIdConnectionBindings);
begin
  inherited OnSendRequest(Request, Sender, Binding);

  if Request.IsInvite then begin
    if Self.FailFirstInviteSend then begin
      Self.FailFirstInviteSend := false;

      raise EIdConnectTimeout.Create('TestTIdSipOutboundSession.OnSendRequest');
    end;
  end;
end;

//* TestTIdSipOutboundSession Private methods **********************************

procedure TestTIdSipOutboundSession.OnInboundCall(UserAgent: TIdSipInviteModule;
                                                  Session: TIdSipInboundSession);
begin
  Self.HairpinCall := Session;
end;

procedure TestTIdSipOutboundSession.ReceiveForbidden;
begin
  Self.ReceiveResponse(Self.LastSentRequest, SIPForbidden);
end;

procedure TestTIdSipOutboundSession.ReceiveOKWithRecordRoute;
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                          SIPOK);
  try
    Response.RecordRoute.Add(RecordRouteHeader).Value := '<sip:127.0.0.1>';
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSession.ReceiveProvisionalResponse(StatusCode: Cardinal; SDP: String);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.LastSentRequest, StatusCode);
  try
    if (SDP <> '') then begin
      Response.Body := SDP;
      Response.ContentType := SdpMimeType;
      Response.ContentLength := Length(Response.Body);
    end;

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSession.ReceiveRemoteDecline;
begin
  Self.ReceiveResponse(Self.LastSentRequest, SIPDecline);
end;

//* TestTIdSipOutboundSession Published methods ********************************

procedure TestTIdSipOutboundSession.TestAbandonAuthentication;
var
  Action: TIdSipOutboundSession;
begin
  // This test only makes sense for OUTbound actions.
  if Self.IsInboundTest then Exit;

  Action := Self.CreateAction as TIdSipOutboundSession;

  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');

  Self.MarkSentRequestCount;
  Action.Terminate;
  Check(Action.IsTerminated,
        Self.ClassName + ': Action not terminated');
  CheckNoRequestSent(Self.ClassName + ': The Session sent a '
                   + Self.LastSentRequest.Method
                   + ' message after abandoning the authorization');

end;

procedure TestTIdSipOutboundSession.TestAck;
var
  Ack:    TIdSipRequest;
  Invite: TIdSipRequest;
begin
  Invite := TIdSipRequest.Create;
  try
    Invite.Assign(Self.LastSentRequest);

    Self.ReceiveOk(Self.LastSentRequest);

    Ack := Self.LastSentACK;

    CheckEquals(Self.Session.Dialog.RemoteTarget,
                Ack.RequestUri,
                'Request-URI');
    CheckEquals(Invite.CSeq.SequenceNo,
                Ack.CSeq.SequenceNo,
                'CSeq sequence number');

    CheckEquals(Invite.Body,
                Ack.Body,
                'Offer');
    CheckEquals(Length(Ack.Body),
                Ack.ContentLength,
                'Content-Length');
    CheckEquals(Invite.ContentType,
                Ack.ContentType,
                'Content-Type');
    CheckEquals(Invite.ContentDisposition.Value,
                Ack.ContentDisposition.Value,
                'Content-Disposition');
    Check(Ack.ContentDisposition.IsSession,
          'Content-Disposition handling');
    CheckNotEquals(Invite.LastHop.Branch,
                   Ack.LastHop.Branch,
                   'Branch must differ - a UAS creates an ACK as an '
                 + 'in-dialog request');
  finally
    Invite.Destroy;
  end;
end;

procedure TestTIdSipOutboundSession.TestAckFromRecordRouteResponse;
var
  Ack: TIdSipRequest;
begin
  Self.ReceiveOKWithRecordRoute;
  Ack := Self.LastSentACK;

  Check(not Ack.Route.IsEmpty, 'No Route headers');
end;

procedure TestTIdSipOutboundSession.TestAckWithAuthorization;
var
  Ack:       TIdSipRequest;
  AuthCreds: TIdSipAuthorizationHeader;
  Invite:    TIdSipRequest;
begin
  Self.MarkSentAckCount;
  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');
  CheckAckSent('No ACK sent for the challenge');

  AuthCreds := Self.CreateAuthorization(Self.LastSentResponse);
  try
    Self.Session.Resend(AuthCreds);
  finally
    AuthCreds.Free;
  end;

  Invite := Self.LastSentRequest.Copy as TIdSipRequest;
  try
    Self.MarkSentAckCount;
    Self.ReceiveOk(Invite);
    CheckAckSent('No ACK sent for the OK: DroppedUnmatchedResponse = '
               + Self.BoolToStr(Self.DroppedUnmatchedResponse));

    Ack := Self.LastSentRequest;

    Check(Ack.HasAuthorization, 'ACK lacks Authorization header');
    CheckEquals(Invite.FirstAuthorization.FullValue,
                Ack.FirstAuthorization.FullValue,
                'Authorization');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestAckWithMultipleAuthorization;
var
  ProxyAuthCreds: TIdSipProxyAuthorizationHeader;
  UAAuthCreds:    TIdSipAuthorizationHeader;
begin
  ProxyAuthCreds := TIdSipProxyAuthorizationHeader.Create;
  try
    UAAuthCreds := TIdSipAuthorizationHeader.Create;
    try
      ProxyAuthCreds.Realm := 'alpha';

      Self.MarkSentAckCount;
      Self.ReceiveUnauthorized(ProxyAuthenticateHeader, '');
      CheckAckSent('No ACK sent to the Proxy-Authenticate challenge');

      Self.MarkSentRequestCount;
      Self.Session.Resend(ProxyAuthCreds);
      CheckRequestSent('No resend sent');
      Check(Self.LastSentRequest.HasProxyAuthorizationFor(ProxyAuthCreds.Realm),
            'Resend missing proxy authorization credentials');

      Self.MarkSentAckCount;
      Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');
      CheckAckSent('No ACK sent to the WWW-Authenticate challenge');

      Self.MarkSentRequestCount;
      Self.Session.Resend(UAAuthCreds);
      CheckRequestSent('No re-resend sent');
      Check(Self.LastSentRequest.HasProxyAuthorizationFor(ProxyAuthCreds.Realm),
            'Re-resend missing proxy authorization credentials');
      Check(Self.LastSentRequest.HasAuthorizationFor(UAAuthCreds.Realm),
            'Re-resend missing UA authorization credentials');
    finally
      UAAuthCreds.Free;
    end;
  finally
    ProxyAuthCreds.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestAckWithProxyAuthorization;
var
  Ack:       TIdSipRequest;
  AuthCreds: TIdSipAuthorizationHeader;
  Invite:    TIdSipRequest;
begin
  Self.MarkSentAckCount;
  Self.ReceiveUnauthorized(ProxyAuthenticateHeader, '');
  CheckAckSent('No ACK sent for the challenge');

  AuthCreds := Self.CreateAuthorization(Self.LastSentResponse);
  try
    Self.Session.Resend(AuthCreds);
  finally
    AuthCreds.Free;
  end;

  Invite := Self.LastSentRequest.Copy as TIdSipRequest;
  try
    Self.MarkSentAckCount;
    Self.ReceiveOk(Self.LastSentRequest);
    CheckAckSent('No ACK sent for the OK: DroppedUnmatchedResponse = '
               + Self.BoolToStr(Self.DroppedUnmatchedResponse));

    Ack := Self.LastSentRequest;

    Check(Ack.HasProxyAuthorization, 'ACK lacks Proxy-Authorization header');
    CheckEquals(Invite.FirstProxyAuthorization.FullValue,
                Ack.FirstProxyAuthorization.FullValue,
                'Proxy-Authorization');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestActionListenersArentNotifiedReSessionEvents;
var
  L: TIdSipTestActionListener;
begin
  // PR 544: Sessions used their superclass's ActionListeners, which are not
  // necessarily IIdSipSessionListeners. This test simply raises an
  // EIntfCastError should the bug crop up again.

  L := TIdSipTestActionListener.Create;
  try
    Self.Session.AddActionListener(L);

    try
      Self.ReceiveProvisionalResponse(SIPSessionProgress, '');
    except
      on EIntfCastError do
        Fail('Session''s using TIdSipAction.ActionListeners to notify, rather than its own SessionListeners');
    end;
  finally
    Self.Session.RemoveActionListener(L);
    L.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestByeCarriesInviteAuthorization;
var
  Invite:  TIdSipRequest;
  Session: TIdSipOutboundSession;
begin
  //  ---              INVITE               --->
  // <--- 407 Proxy Authentication Required ---
  //  ---               ACK                 --->
  //  ---              INVITE               ---> (with proxy credentials)
  // <--- 407 Proxy Authentication Required ---
  //  ---               ACK                 --->
  //  ---              INVITE               ---> (with 2x proxy credentials)
  // <---         401 Unauthorized          ---
  //  ---               ACK                 --->
  //  ---              INVITE               ---> (with 2x proxy, UA credentials)
  // <---              200 OK               ---
  //  ---               ACK                 --->
  // ===========================================
  //                Media streams
  // ===========================================
  //  ---               BYE                 ---> (with 2x proxy, UA credentials)

  Session := Self.Module.Call(Self.Core.From, Self.Destination, '', '');

  // 1st proxy challenge
  Session.Send;
  Self.ReceiveUnauthorized(ProxyAuthenticateHeader, '');
  Check(not Self.DroppedUnmatchedResponse,
        'First 407 Proxy Authentication Required dropped');

  // 2nd proxy challenge
  Self.ResendWith(Session, Self.LastSentResponse);
  Self.ReceiveUnauthorized(ProxyAuthenticateHeader, QopAuthInt);
  Check(not Self.DroppedUnmatchedResponse,
        'Second 407 Proxy Authentication Required dropped');

  // UA challenge
  Self.ResendWith(Session, Self.LastSentResponse);
  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');
  Check(not Self.DroppedUnmatchedResponse,
        '401 Unauthorized dropped');

  Self.ResendWith(Session, Self.LastSentResponse);
  Self.ReceiveOk(Self.LastSentRequest);
  Check(not Self.DroppedUnmatchedResponse,
        '200 OK dropped');

  Invite := Self.LastSentRequest.Copy as TIdSipRequest;
  try
    Check(Session.DialogEstablished,
          'Dialog not established: did something drop a message? '
        + 'DroppedUnmatchedResponse = ' + Self.BoolToStr(Self.DroppedUnmatchedResponse));

    Self.MarkSentRequestCount;
    Session.Terminate;
    CheckRequestSent('No BYE sent');
    CheckEquals(MethodBye,
                Self.LastSentRequest.Method,
                'Unexpected message sent to terminate a session');
    Check(Self.LastSentRequest.HasAuthorizationFor(Invite.FirstAuthorization.Realm),
          'BYE''s missing INVITE''s credentials: Authorization');
    CheckHeadersEqual(Invite, Self.LastSentRequest, ProxyAuthorizationHeader,
                      'BYE''s Proxy-Authorization');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestCall;
var
  Invite:     TIdSipRequest;
  SessCount:  Integer;
  Session:    TIdSipSession;
  TranCount:  Integer;
  Answer:     String;
  AnswerType: String;
begin
  Self.MarkSentRequestCount;
  SessCount    := Self.Core.SessionCount;
  TranCount    := Self.Dispatcher.TransactionCount;

  Self.SDP      := TIdSipTestResources.BasicSDP('proxy.tessier-ashpool.co.luna');
  Self.MimeType := SdpMimeType;

  Session := Self.CreateAction as TIdSipSession;

  CheckEquals(Self.SDP,
              Session.LocalSessionDescription,
              'LocalSessionDescription');
  CheckEquals(Self.MimeType,
              Session.LocalMimeType,
              'LocalMimeType');

  CheckRequestSent('no INVITE sent');
  Invite := Self.LastSentRequest;

  CheckEquals(TranCount + 1,
              Self.Dispatcher.TransactionCount,
              'no client INVITE transaction created');

  CheckEquals(SessCount + 1,
              Self.Core.SessionCount,
              'no new session created');

  Self.ReceiveRinging(Invite);

  Check(Session.IsEarly,
        'Dialog in incorrect state: should be Early');
  Check(Session.DialogEstablished,
        'Dialog not established');
  Check(not Session.Dialog.IsSecure,
        'Dialog secure when TLS not used');

  CheckEquals(Self.LastSentResponse.CallID,
              Session.Dialog.ID.CallID,
              'Dialog''s Call-ID');
  CheckEquals(Self.LastSentResponse.From.Tag,
              Session.Dialog.ID.LocalTag,
              'Dialog''s Local Tag');
  CheckEquals(Self.LastSentResponse.ToHeader.Tag,
              Session.Dialog.ID.RemoteTag,
              'Dialog''s Remote Tag');

  Answer     := TIdSipTestResources.BasicSDP('sip.fried-neurons.org');
  AnswerType := SdpMimeType;
  Self.ReceiveOkWithBody(Invite, Answer, AnswerType);

  CheckEquals(Self.LastSentResponse.FirstContact.AsString,
              Session.RemoteContact.AsString,
              'RemoteContact');
  CheckEquals(Self.LastSentRequest.ToHeader.Value,
              Session.RemoteParty.Value,
              'RemoteParty');
  CheckEquals(Answer,
              Session.RemoteSessionDescription,
              'RemoteSessionDescription');
  CheckEquals(AnswerType,
              Session.RemoteMimeType,
              'RemoteMimeType');

  Check(not Session.IsEarly, 'Dialog in incorrect state: shouldn''t be early');
end;

procedure TestTIdSipOutboundSession.TestCallDoesntClaimToModifySession;
var
  Session: TIdSipOutboundSession;
  L:       TIdSipTestSessionListener;
begin
  Session := Self.CreateAction as TIdSipOutboundSession;
  L := TIdSipTestSessionListener.Create;
  try
    Session.AddSessionListener(L);
    try
      Self.EstablishSession(Session);

      Check(not L.ModifiedSession, 'Listener told the session was modified, on initial INVITE');
    finally
      Session.RemoveSessionListener(L);
    end;
  finally
    L.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestCallFlowToGateway;
var
  Session: TIdSipOutboundSession;
begin
  // This test simulates a call to the RNID PSTN textphone gateway.
  //  ---           INVITE            --->
  // <---     183 Session Progress    ---
  // <---         180 Ringing         ---
  // <---           200 OK            ---
  //  ---            ACK              --->

  Session := Self.CreateAction as TIdSipOutboundSession;
  Self.ReceiveProvisionalResponse(SIPSessionProgress, '');

  CheckEquals('',
              Session.RemoteSessionDescription,
              'Session''s remote description wrong after Session Progress');

  Self.ReceiveProvisionalResponse(SIPRinging, '');

  CheckEquals('',
              Session.RemoteSessionDescription,
              'Session''s remote description wrong after Ringing');

  Self.ReceiveOk(Session.InitialRequest, Self.SDP);

  CheckEquals(Self.SDP,
              Session.RemoteSessionDescription,
              'Session''s remote description wrong after OK');
  CheckEquals(Self.SDP,
              Self.RemoteDesc,
              'Notifiers given wrong remote description');
  CheckEquals(SdpMimeType,
              Self.RemoteMimeType,
              'Notifiers given wrong remote description');
end;

procedure TestTIdSipOutboundSession.TestCallNetworkFailure;
var
  SessionCount: Cardinal;
begin
  SessionCount := Self.Core.SessionCount;

  Self.CreateAction;
  Self.Dispatcher.Transport.FireOnException(Self.LastSentRequest,
                                            EIdConnectException,
                                            '10061',
                                            'Connection refused');

  CheckEquals(SessionCount,
              Self.Core.SessionCount,
              'Core should have axed the failed session');
end;

procedure TestTIdSipOutboundSession.TestCallRemoteRefusal;
begin
  Self.ReceiveForbidden;

  Check(Self.OnEndedSessionFired, 'OnEndedSession wasn''t triggered');
end;

procedure TestTIdSipOutboundSession.TestCallSecure;
var
  Response: TIdSipResponse;
  Session:  TIdSipSession;
begin
  Self.Dispatcher.TransportType := TlsTransport;

  Self.Destination.Address.Scheme := SipsScheme;
  Session := Self.CreateAction as TIdSipSession;

  Response := Self.Core.CreateResponse(Self.LastSentRequest,
                                       SIPRinging);
  try
    Self.ReceiveResponse(Response);

    Response.StatusCode := SIPOK;
    Check(Session.Dialog.IsSecure, 'Dialog not secure when TLS used');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestCallSipsUriUsesTls;
begin
  Self.Dispatcher.TransportType := TlsTransport;
  Self.Destination.Address.Scheme := SipsScheme;

  Self.MarkSentRequestCount;

  Self.CreateAction;

  CheckRequestSent('INVITE wasn''t sent over TLS');
end;

procedure TestTIdSipOutboundSession.TestCallWithGruu;
var
  Invite: TIdSipRequest;
begin
  Self.UseGruu;

  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('no INVITE sent');

  Invite := Self.LastSentRequest;

  // draft-ietf-sip-gruu section 8.1: requests should use the UA's GRUU. Our UA
  // uses one GRUU per local interface.
  Check(Invite.FirstContact.IsGruu,
        'INVITE''s not using a GRUU');

  CheckEquals(Self.LanIP,
              Invite.FirstContact.Address.Host,
              'INVITE didn''t use UA''s GRUU');

  Check(Invite.FirstContact.Address.HasParameter(GridParam),
        'GRUUs sent out in INVITEs should have a "grid" parameter');
end;

procedure TestTIdSipOutboundSession.TestCallWithMaxForwards;
const
  MaxForwards = 42;
var
  Session: TIdSipOutboundSession;
begin
  Session := Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, Self.SDP, Self.MimeType);
  Session.AddActionListener(Self);
  Session.AddSessionListener(Self);
  Session.MaxForwards := 42;

  Self.MarkSentRequestCount;
  Session.Send;
  CheckRequestSent('No INVITE sent');
  CheckEquals(MaxForwards, Self.LastSentRequest.MaxForwards, 'Max-Forwards not overridden');
end;

procedure TestTIdSipOutboundSession.TestCallWithOffer;
var
  Answer:      String;
  ContentType: String;
begin
  //  ---     INVITE (with offer)     --->
  // <---       200 (with answer)     ---
  //  ---  ACK (with repeat of offer) --->

  Answer      := TIdSipTestResources.BasicSDP('1.1.1.1');
  ContentType := SdpMimeType;

  Check(Self.LastSentRequest.ContentDisposition.IsSession,
        'Content-Disposition');
  CheckEquals(Self.SDP,
              Self.LastSentRequest.Body,
              'INVITE offer');
  CheckEquals(SdpMimeType,
              Self.LastSentRequest.ContentType,
              'INVITE offer mime type');

  Self.MarkSentAckCount;

  Self.ReceiveOkWithBody(Self.LastSentRequest,
                         Answer,
                         ContentType);

  Check(Self.OnEstablishedSessionFired,
        'OnEstablishedSession didn''t fire');

  CheckEquals(Answer,
              Self.RemoteDesc,
              'Remote session description');
  CheckEquals(ContentType,
              Self.RemoteMimeType,
              'Remote description''s MIME type');

  CheckAckSent('No ACK sent');
  CheckEquals(Self.LastSentRequest.Body,
              Self.LastSentAck.Body,
              'ACK offer');
  CheckEquals(Self.LastSentRequest.ContentType,
              Self.LastSentAck.ContentType,
              'ACK offer MIME type');
end;

procedure TestTIdSipOutboundSession.TestCallWithoutOffer;
var
  OfferType: String;
  Offer:     String;
  Session:   TIdSipOutboundSession;
begin
  //  ---       INVITE      --->
  // <--- 200 (with offer)  ---
  //  --- ACK (with answer) --->

  OfferType := SdpMimeType;
  Offer     := TIdSipTestResources.BasicSDP('1.1.1.1');

  Self.MimeType := '';
  Self.SDP      := '';

  Session := Self.CreateAction as TIdSipOutboundSession;

  CheckEquals(Self.SDP,
              Self.LastSentRequest.Body,
              'INVITE body');
  CheckEquals(Self.MimeType,
              Self.LastSentRequest.ContentType,
              'INVITE Content-Type');

  Self.LocalDescription := TIdSipTestResources.BasicSDP('localhost');
  Self.LocalMimeType    := SdpMimeType;

  Self.MarkSentAckCount;
  Self.ReceiveOkWithBody(Self.LastSentRequest,
                         Offer,
                         OfferType);

  Check(Self.OnEstablishedSessionFired,
        'OnEstablishedSession didn''t fire');
  CheckEquals(Offer,
              Self.RemoteDesc,
              'Remote description');
  CheckEquals(OfferType,
              Self.RemoteMimeType,
              'Remote description''s MIME type');

  CheckAckSent('No ACK sent');
  CheckEquals(Session.LocalSessionDescription,
              Self.LastSentAck.Body,
              'ACK answer');
  CheckEquals(Session.LocalMimeType,
              Self.LastSentAck.ContentType,
              'ACK answer MIME type');
end;

procedure TestTIdSipOutboundSession.TestCancelReceiveInviteOkBeforeCancelOk;
var
  Cancel: TIdSipRequest;
  Invite: TIdSipRequest;
begin
  //  ---          INVITE         --->
  // <---        100 Trying       ---
  //  ---          CANCEL         --->
  // <--- 200 OK (for the INVITE) ---
  //  ---           ACK           --->
  // <--- 200 OK (for the CANCEL) ---
  //  ---           BYE           --->
  // <---   200 OK (for the BYE)  ---

  Invite := TIdSipRequest.Create;
  try
    Cancel := TIdSipRequest.Create;
    try
      Invite.Assign(Self.LastSentRequest);
      Self.ReceiveTrying(Invite);

      Self.Session.Cancel;
      Cancel.Assign(Self.LastSentRequest);

      Self.MarkSentRequestCount;
      Self.ReceiveOk(Invite);
      Self.ReceiveOk(Cancel);

      Check(Self.OnEndedSessionFired,
            'Listeners not notified of end of session');
    finally
      Cancel.Free;
    end;
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestCircularRedirect;
var
  Action:    TIdSipAction;
  ClassName: String;
begin
  //  ---   INVITE (original)   --->
  // <--- 302 Moved Temporarily ---
  //  ---          ACK          --->
  //  --- INVITE (redirect #1)  --->
  // <--- 302 Moved Temporarily ---
  //  ---          ACK          --->
  //  --- INVITE (redirect #2)  --->
  // <--- 302 Moved Temporarily ---
  //  ---          ACK          --->
  //  --- INVITE (redirect #1)  ---> again!
  // <--- 302 Moved Temporarily ---
  //  ---          ACK          --->

  Action := Self.CreateAction;
  ClassName := Action.ClassName;
  Self.ReceiveMovedTemporarily('sip:foo@bar.org');
  Self.ReceiveMovedTemporarily('sip:bar@bar.org');

  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily('sip:foo@bar.org');
  CheckNoRequestSent('The ' + ClassName + ' accepted the run-around');
end;

procedure TestTIdSipOutboundSession.TestDialogNotEstablishedOnTryingResponse;
var
  SentInvite: TIdSipRequest;
  Session:    TIdSipSession;
begin
  Self.MarkSentRequestCount;

  Session := Self.CreateAction as TIdSipSession;
  Check(not Session.DialogEstablished, 'Brand new session');

  CheckRequestSent('The INVITE wasn''t sent');
  SentInvite := Self.LastSentRequest;

  Self.ReceiveTryingWithNoToTag(SentInvite);
  Check(not Session.DialogEstablished,
        'Dialog established after receiving a 100 Trying');

  Self.ReceiveRinging(SentInvite);
  Check(Session.DialogEstablished,
        'Dialog not established after receiving a 180 Ringing');
end;

procedure TestTIdSipOutboundSession.TestDoubleRedirect;
var
  Action: TIdSipAction;
  Method: String;
begin
  //  ---   INVITE (original)   --->
  // <--- 302 Moved Temporarily ---
  //  ---          ACK          --->
  //  --- INVITE (redirect #1)  --->
  // <--- 302 Moved Temporarily ---
  //  ---          ACK          --->
  //  --- INVITE (redirect #2)  --->
  // <--- 302 Moved Temporarily ---
  //  ---          ACK          --->

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

procedure TestTIdSipOutboundSession.TestEmptyTargetSetMeansTerminate;
begin
  Self.ReceiveMovedTemporarily('sip:foo@bar.org');
  Self.ReceiveForbidden;
  Check(Self.OnEndedSessionFired, 'Session didn''t end: ' + Self.FailReason);
end;

procedure TestTIdSipOutboundSession.TestEstablishedSessionSetsInitialRequestToTag;
begin
  Self.ReceiveRinging(Self.LastSentRequest);
  CheckEquals(Self.LastSentResponse.ToHeader.Tag,
              Self.Session.InitialRequest.ToHeader.Tag,
              'Session.InitialRequest''s To tag not set');
end;

procedure TestTIdSipOutboundSession.TestGlobalFailureEndsSession;
var
  SessionCount: Integer;
begin
  SessionCount := Self.Core.SessionCount;

  Self.ReceiveRemoteDecline;

  Check(Self.OnEndedSessionFired,
        'No notification of ended session');

  Check(Self.Core.SessionCount < SessionCount,
        'Session not torn down because of a global failure');
end;

procedure TestTIdSipOutboundSession.TestHairpinCall;
var
  Session: TIdSipSession;
begin
  Self.Core.InviteModule.AddListener(Self);

  Session := Self.Module.Call(Self.Core.From, Self.Core.From, '', '');
  Session.Send;
  Self.Dispatcher.Transport.FireOnRequest(Self.LastSentRequest);
  Check(Assigned(Self.HairpinCall), 'We didn''t receive the INVITE');
end;

procedure TestTIdSipOutboundSession.TestHangUp;
begin
  Self.ReceiveOk(Self.LastSentRequest);

  Self.MarkSentRequestCount;
  Self.Session.Terminate;

  CheckRequestSent('No BYE sent');
  CheckEquals(MethodBye,
              Self.LastSentRequest.Method,
              'TU didn''t sent a BYE');
  Self.ReceiveOk(Self.LastSentRequest);
end;

procedure TestTIdSipOutboundSession.TestIsOutboundCall;
begin
  Check(Self.Session.IsOutboundCall,
        'Outbound session; IsOutboundCall');
end;

procedure TestTIdSipOutboundSession.TestMethod;
begin
  CheckEquals(MethodInvite,
              Self.CreateAction.Method,
              'Outbound session; Method');
end;

procedure TestTIdSipOutboundSession.TestModifyUsesAuthentication;
var
  AuthCreds: TIdSipAuthorizationHeader;
  Invite:    TIdSipRequest;
  Modify:    TIdSipRequest;
begin
  // n, n+1, n+2, ..., n+m is the sequence of branch IDs generated by Self.Core.
  //  ---      INVITE      ---> (with branch n)
  // <--- 401 Unauthorized ---  (with branch n)
  //  ---      INVITE      ---> (with branch n+1)
  // <---      200 OK      ---  (with branch n+1)
  //  ---        ACK       ---> (with branch n+1)
  //  ---      INVITE      ---> (modify) (with branch n+2)

  Invite := TIdSipRequest.Create;
  try
    Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');

    Self.MarkSentRequestCount;

    AuthCreds := Self.CreateAuthorization(Self.LastSentResponse);
    try
      Self.Session.Resend(AuthCreds);
    finally
      AuthCreds.Free;
    end;

    CheckRequestSent('No resend of INVITE with Authorization');
    Invite.Assign(Self.LastSentRequest);
    Check(Invite.HasAuthorization,
          'Resend INVITE has no Authorization header');

    Self.ReceiveOk(Self.LastSentRequest);
    Check(not Self.Session.IsEarly,
          'The UA didn''t update the InviteAction''s InitialRequest as a'
             + ' result of the authentication challenge.');

    Self.Session.Modify('', '');

    Modify := Self.LastSentRequest;
    Check(Modify.HasAuthorization,
          'No Authorization header');
    CheckEquals(Invite.FirstAuthorization.Value,
                Modify.FirstAuthorization.Value,
                'Authorization header');
    CheckEquals(Invite.CSeq.SequenceNo + 1,
                Modify.CSeq.SequenceNo,
                'Unexpected sequence number in the modify');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestNetworkFailuresLookLikeSessionFailures;
begin
  // We'd sent an INVITE. We receive a 200 OK, but our ACK fails.
  Self.ReceiveOk(Self.LastSentRequest);
  Self.Dispatcher.Transport.FireOnException(Self.LastSentAck,
                                            EIdConnectException,
                                            '10061',
                                            'Connection refused');

  Check(Assigned(Self.ActionParam), 'OnNetworkFailure didn''t fire');
  Check(Self.ActionParam = Self.Session,
        'Session must signal the network error as _its_ error, not the '
      + 'Invite''s');
end;

procedure TestTIdSipOutboundSession.TestOneNetworkFailureDoesntFailWholeRedirection;
var
  Contacts: array of String;
begin
  SetLength(Contacts, 2);
  Contacts[0] := 'sip:foo@bar.org';
  Contacts[1] := 'sip:bar@bar.org';

  Self.MarkSentRequestCount;

  Self.ReceiveMovedTemporarily(Contacts);

  // ARG! Why do they make Length return an INTEGER? And WHY Abs() too?
  CheckEquals(Self.RequestCount + Cardinal(Length(Contacts)),
              Self.SentRequestCount,
              'Session didn''t attempt to contact all Contacts: ' + Self.FailReason);

  // Receive a 200 OK to the 2nd, successfully sent, INVITE.
  Self.ReceiveOk(Self.LastSentRequest);

  Check(Self.OnEstablishedSessionFired,
        'The session didn''t receive the 200 OK, or has a confused state');
end;

procedure TestTIdSipOutboundSession.TestRaceConditionCrossoverOfCancelAndInvites200OK;
var
  AlicesSession: TIdSipOutboundSession;
  Cancel:        TIdSipRequest;
  SessionCount:  Integer;
begin
  // from draft-hasebe-sipping-race-examples-00.txt, section 3.1.2:
  //   Alice                     Bob
  //     |                        |
  //     |       INVITE F1        |
  //     |----------------------->|
  //     |    180 Ringing F2      |
  //     |<-----------------------|
  //     |                        |
  //     |CANCEL F3     200 OK F4 |
  //     |---------     ----------|
  //     |          \ /           |
  //     |           X            |
  //     |          / \           |
  //     |<--------     --------->|
  //     |                        |
  //     | ACK F6         481 F5  |
  //     |---------     ----------|
  //     |          \ /           |
  //     |           X            |
  //     |          / \           |
  //     |<--------     --------->|
  //     |                        |
  //     |   Both Way RTP Media   |
  //     |<======================>|
  //     |         BYE F7         |
  //     |----------------------->|
  //     |         200 F8         |
  //     |<-----------------------|
  //     |                        |
  //     |                        |

  Cancel := TIdSipRequest.Create;
  try
    AlicesSession := Self.CreateAction as TIdSipOutboundSession;
    SessionCount := Self.Core.CountOf(MethodInvite);

    Self.ReceiveRinging(AlicesSession.InitialRequest);

    AlicesSession.Cancel;
    Cancel.Assign(Self.LastSentRequest);

    Self.MarkSentAckCount;
    Self.MarkSentRequestCount;

    Self.ReceiveOk(AlicesSession.InitialRequest);
    Self.ReceiveResponse(Cancel, SIPCallLegOrTransactionDoesNotExist);
    CheckAckSent('No ACK to the 200 OK sent');
    CheckRequestSent('No BYE sent');

    CheckEquals(MethodBye,
                Self.LastSentRequest.Method,
                'Unexpected request sent');
    Check(Self.Core.CountOf(MethodInvite) < SessionCount,
          'Session not terminated');
  finally
    Cancel.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestReceive1xxNotifiesListeners;
begin
  Self.ReceiveTrying(Self.LastSentRequest);

  Check(Self.OnProgressedSessionFired,
        'Listeners not notified of progress for initial INVITE');

  Self.EstablishSession(Self.Session);

  Self.OnProgressedSessionFired := false;
  Self.Session.Modify('', '');

  Self.ReceiveTrying(Self.LastSentRequest);

  Check(Self.OnProgressedSessionFired,
        'Listeners not notified of progress for modify INVITE');
end;

procedure TestTIdSipOutboundSession.TestReceive2xxSendsAck;
var
  Ack:    TIdSipRequest;
  Invite: TIdSipRequest;
  Ok:     TIdSipResponse;
begin
  Ok := Self.CreateRemoteOk(Self.LastSentRequest);
  try
    Self.MarkSentAckCount;
    Self.ReceiveResponse(Ok);

    CheckAckSent('Original ACK');

    Self.MarkSentAckCount;
    Self.ReceiveResponse(Ok);
    Check(not Self.DroppedUnmatchedResponse,
          'Dropped the retransmitted OK');
    CheckAckSent('No retransmitted ACK to the retransmitted 200 OK');

    Ack := Self.LastSentAck;
    CheckEquals(MethodAck, Ack.Method, 'Unexpected method');
    Invite := Self.Session.InitialRequest;
    CheckEquals(Invite.CSeq.SequenceNo,
                Ack.CSeq.SequenceNo,
                'CSeq numerical portion');
    CheckEquals(MethodAck,
                Ack.CSeq.Method,
                'CSeq method');
  finally
    Ok.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestReceive3xxSendsNewInvite;
const
  NewAddress = 'sip:foo@bar.org';
var
  OriginalInvite: TIdSipRequest;
begin
  OriginalInvite := TIdSipRequest.Create;
  try
    OriginalInvite.Assign(Self.LastSentRequest);

    Self.MarkSentRequestCount;
    Self.ReceiveMovedPermanently(NewAddress);

    CheckRequestSent('Session didn''t send a new INVITE: ' + Self.FailReason);
  finally
    OriginalInvite.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestReceive3xxWithOneContact;
var
  Contact:     String;
  InviteCount: Integer;
  RequestUri:  TIdSipUri;
begin
  //  ---         INVITE        --->
  // <--- 302 Moved Temporarily ---
  //  ---          ACK          --->
  //  ---         INVITE        --->
  // <---     403 Forbidden     ---
  //  ---          ACK          --->

  Contact      := 'sip:foo@bar.org';
  InviteCount  := Self.Core.CountOf(MethodInvite);
  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily(Contact);

  CheckRequestSent('No new INVITE sent: ' + Self.FailReason);
  CheckEquals(InviteCount,
              Self.Core.CountOf(MethodInvite),
              'The Core should have one new INVITE and have destroyed one old one');

  RequestUri := Self.LastSentRequest.RequestUri;
  CheckEquals(Contact,
              RequestUri.Uri,
              'Request-URI');

  Self.ReceiveForbidden;
  Check(Self.Core.CountOf(MethodInvite) < InviteCount,
        'The Core didn''t destroy the second INVITE');
  Check(Self.OnEndedSessionFired,
        'Listeners not notified of failed call');
end;

procedure TestTIdSipOutboundSession.TestReceive3xxWithNoContacts;
var
  Redirect: TIdSipResponse;
begin
  Redirect := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                          SIPMovedPermanently);
  try
    Redirect.ToHeader.Tag := Self.Core.NextTag;
    Self.ReceiveResponse(Redirect);

    Check(Self.OnEndedSessionFired,
          'Session didn''t end despite a redirect with no Contact headers');
    CheckEquals(RedirectWithNoContacts, Self.ErrorCode, 'Stack reports wrong error code');
    CheckNotEquals('',
                   Self.Reason,
                   'Reason param not set');
  finally
    Redirect.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestReceiveFailureResponseAfterSessionEstablished;
var
  Invite: TIdSipRequest;
begin
  //  ---          INVITE         --->
  // <---          200 OK         ---
  //  ---           ACK           --->
  // <--- 503 Service Unavailable --- (in response to the INVITE!)

  // This situation should never arise: the remote end's sending a failure
  // response to a request it has already accepted. Still, I've seen it happen
  // once before...

  Invite := TIdSipRequest.Create;
  try
    Invite.Assign(Self.LastSentRequest);

    Self.MarkSentAckCount;
    Self.ReceiveOk(Invite);
    CheckAckSent('No ACK sent');

    Self.ReceiveServiceUnavailable(Invite);

    Check(not Self.Session.IsTerminated,
          'The Session received the response: the Transaction-User layer didn''t '
        + 'drop the message, or the Session Matched the request');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestReceiveFailureResponseNotifiesOnce;
var
  L:       TIdSipTestSessionListenerEndedCounter;
  Session: TIdSipOutboundSession;
begin
  Session := Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, Self.SDP, SdpMimeType);
  L := TIdSipTestSessionListenerEndedCounter.Create;
  try
    Session.AddSessionListener(L);
    Session.Send;

    Self.ReceiveResponse(SIPDecline);

    CheckEquals(1, L.EndedNotificationCount, 'Not notified only once');
  finally
    L.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestReceiveFailureSetsReason;
begin
  Self.CreateAction;
  Self.ReceiveBusyHere(Self.LastSentRequest);

  Check(Self.OnEndedSessionFired,
        'OnEndedSession didn''t fire');
  CheckNotEquals('',
                 Self.Reason,
                 'Reason param not set');
end;

procedure TestTIdSipOutboundSession.TestReceiveFinalResponseSendsAck;
var
  I: Integer;
begin
  // Of course this works. That's because the transaction sends the ACK for a
  // non-2xx final response.
  for I := SIPRedirectionResponseClass to SIPGlobalFailureResponseClass do begin
    Self.MarkSentAckCount;

    Self.CreateAction;

    Self.ReceiveResponse(I*100);
    CheckAckSent('Session didn''t send an ACK to a final response, '
               + Self.LastSentResponse.Description);
  end;
end;

procedure TestTIdSipOutboundSession.TestRedirectAndAccept;
var
  Contact:     String;
  InviteCount: Integer;
  RequestUri:  TIdSipUri;
begin
  //  ---         INVITE        --->
  // <--- 302 Moved Temporarily ---
  //  ---          ACK          --->
  //  ---         INVITE        --->
  // <---         200 OK        ---
  //  ---          ACK          --->

  Contact      := 'sip:foo@bar.org';
  InviteCount  := Self.Core.CountOf(MethodInvite);
  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily(Contact);

  CheckRequestSent('No new INVITE sent: ' + Self.FailReason);
  CheckEquals(InviteCount,
              Self.Core.CountOf(MethodInvite),
              'The Core should have one new INVITE and have destroyed one old one');

  RequestUri := Self.LastSentRequest.RequestUri;
  CheckEquals(Contact,
              RequestUri.Uri,
              'Request-URI');

  Self.ReceiveOk(Self.LastSentRequest);

  Check(Self.OnEstablishedSessionFired,
        'Listeners not notified of a successful call');
end;

procedure TestTIdSipOutboundSession.TestRedirectMultipleOks;
const
  FirstInvite    = 0;
  FirstRedirect  = 1;
  SecondRedirect = 2;
  ThirdRedirect  = 3;
  Bye            = 4;
  Cancel         = 5;
var
  Contacts: array of String;
begin
  //                               Request number:
  //  ---       INVITE        ---> #0
  // <---   302 (foo,bar,baz) ---
  //  ---        ACK          --->
  //  ---     INVITE(foo)     ---> #1
  //  ---     INVITE(bar)     ---> #2
  //  ---     INVITE(baz)     ---> #3
  // <---      200 (bar)      ---
  //  ---        ACK          --->
  // <---      200 (foo)      ---
  //  ---        ACK          --->
  //  ---        BYE          ---> #4 (because we've already established a session)
  // <---    200 (foo,BYE)    ---
  // <---      100 (baz)      ---
  //  ---       CANCEL        ---> #5
  // <---  200 (baz,CANCEL)   ---
  //
  // In summary, we send an INVITE. The redirect server (or whatever) redirects
  // us to foo, bar and baz. The INVITE to bar succeeds first, so we try
  // cancel/terminate the other INVITEs. Since we've by now received a 200 OK
  // for foo, we send foo a BYE. With no response (other than a provisional
  // one) from baz, we send a CANCEL. Of course, if baz hadn't already sent us
  // a provisional response, we would only send a CANCEL once we received a
  // response from baz.

  SetLength(Contacts, 3);
  Contacts[0] := 'sip:foo@bar.org';
  Contacts[1] := 'sip:bar@bar.org';
  Contacts[2] := 'sip:baz@bar.org';

  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily(Contacts);

  // ARG! Why do they make Length return an INTEGER? And WHY Abs() too?
  CheckEquals(Self.RequestCount + Cardinal(Length(Contacts)),
              Self.SentRequestCount,
              'Session didn''t attempt to contact all Contacts: ' + Self.FailReason);

  Self.MarkSentRequestCount;
  Self.ReceiveOkFrom(Self.SentRequestAt(SecondRedirect), Contacts[1]);
  Self.ReceiveOkFrom(Self.SentRequestAt(FirstRedirect), Contacts[0]);
  Self.ReceiveTryingFrom(Self.SentRequestAt(ThirdRedirect), Contacts[2]);

  // We expect a BYE in response to the 1st UA's 2xx and a CANCEL to the 2nd
  // UA's 1xx.

  // ARG! Why do they make Length return an INTEGER? And WHY Abs() too?
  CheckEquals(Self.RequestCount + Cardinal(Length(Contacts) - 1),
              Self.SentRequestCount,
              'Session didn''t try to kill all but one of the redirected INVITEs');

  CheckRequestSent('We expect the session to send _something_');
  CheckEquals(MethodBye,
              Self.SentRequestAt(Bye).Method,
              'Unexpected first request sent');
  CheckEquals(Contacts[0],
              Self.SentRequestAt(Bye).RequestUri.Uri,
              'Unexpected target for the first BYE');
  CheckEquals(MethodCancel,
              Self.SentRequestAt(Cancel).Method,
              'Unexpected second request sent');
  CheckEquals(Contacts[2],
              Self.SentRequestAt(Cancel).RequestUri.Uri,
              'Unexpected target for the second BYE');
end;

procedure TestTIdSipOutboundSession.TestRedirectNoMoreTargets;
var
  Contacts: array of String;
begin
  //                                           Request number:
  //  ---              INVITE             ---> #0
  // <---          302 (foo,bar)          ---
  //  ---               ACK               --->
  //  ---           INVITE (foo)          ---> #1
  //  ---           INVITE (bar)          ---> #2
  // <--- 302 (from foo, referencing bar) ---
  // <--- 302 (from bar, referencing foo) ---
  //  ---          ACK (to foo)           --->
  //  ---          ACK (to bar)           --->

  SetLength(Contacts, 2);
  Contacts[0] := 'sip:foo@bar.org';
  Contacts[1] := 'sip:bar@bar.org';

  Self.ReceiveMovedTemporarily(Contacts);

  Check(Self.SentRequestCount >= 3,
        'Not enough requests sent: 1 + 2 INVITEs: ' + Self.FailReason);

  Self.ReceiveMovedTemporarily(Self.SentRequestAt(1), Contacts[1]);
  Self.ReceiveMovedTemporarily(Self.SentRequestAt(2), Contacts[0]);

  Check(Self.OnEndedSessionFired,
        'Session didn''t notify listeners of ended session');
  CheckEquals(RedirectWithNoMoreTargets, Self.ErrorCode,
              'Session reported wrong error code for no more redirect targets');
  CheckNotEquals('',
                 Self.Reason,
                 'Reason param not set');
end;

procedure TestTIdSipOutboundSession.TestRedirectWithMaxForwards;
const
  MaxForwards = 42;
var
  Session: TIdSipOutboundSession;
begin
  Session := Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, Self.SDP, Self.MimeType);
  Session.MaxForwards := MaxForwards;

  Session.AddActionListener(Self);
  Session.AddSessionListener(Self);
  Session.Send;

  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily('sip:foo');
  CheckRequestSent('No INVITE sent');

  CheckEquals(MaxForwards, Self.LastSentRequest.MaxForwards, 'Max-Forwards not overridden');
end;

procedure TestTIdSipOutboundSession.TestRedirectWithMultipleContacts;
var
  Contacts: array of String;
begin
  SetLength(Contacts, 2);
  Contacts[0] := 'sip:foo@bar.org';
  Contacts[1] := 'sip:bar@bar.org';

  Self.MarkSentRequestCount;

  Self.ReceiveMovedTemporarily(Contacts);

  // ARG! Why do they make Length return an INTEGER? And WHY Abs() too?
  CheckEquals(Self.RequestCount + Cardinal(Length(Contacts)),
              Self.SentRequestCount,
              'Session didn''t attempt to contact all Contacts: ' + Self.FailReason);
end;

procedure TestTIdSipOutboundSession.TestRedirectWithNoSuccess;
var
  Contacts: array of String;
begin
  //                             Request number:
  //  ---       INVITE      ---> #0
  // <---   302 (foo,bar)   ---
  //  ---        ACK        --->
  //  ---    INVITE (foo)   ---> #1
  //  ---    INVITE (bar)   ---> #2
  // <---     486 (foo)     ---
  // <---     486 (bar)     ---
  //  ---    ACK (to foo)   --->
  //  ---    ACK (to bar)   --->

  SetLength(Contacts, 2);
  Contacts[0] := 'sip:foo@bar.org';
  Contacts[1] := 'sip:bar@bar.org';

  Self.ReceiveMovedTemporarily(Contacts);

  Check(Self.SentRequestCount >= 3,
        'Not enough requests sent: 1 + 2 INVITEs: ' + Self.FailReason);

  Self.ReceiveBusyHere(Self.SentRequestAt(1));
  Self.ReceiveBusyHere(Self.SentRequestAt(2));

  Check(Self.OnEndedSessionFired,
        'Session didn''t notify listeners of ended session');
  CheckEquals(RedirectWithNoSuccess, Self.ErrorCode,
              'Session reported wrong error code for no successful rings');
  CheckNotEquals('',
                 Self.Reason,
                 'Reason param not set');
end;

procedure TestTIdSipOutboundSession.TestRemoveSessionListener;
var
  L1, L2: TIdSipTestSessionListener;
begin
  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Self.ReceiveOk(Self.Session.InitialRequest);

  L1 := TIdSipTestSessionListener.Create;
  try
    L2 := TIdSipTestSessionListener.Create;
    try
      Self.Session.AddSessionListener(L1);
      Self.Session.AddSessionListener(L2);
      Self.Session.RemoveSessionListener(L2);

      Self.Session.Terminate;

      Check(L1.EndedSession,
            'First listener not notified');
      Check(not L2.EndedSession,
            'Second listener erroneously notified, ergo not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestSendSetsInitialRequest;
var
  Session: TIdSipAction;
begin
  Session := Self.CreateAction;
  Check(Session.InitialRequest.Equals(Self.LastSentRequest),
        'Sending the session didn''t set the session''s InitialRequest');

  // I don't know why, but if you leave these lines out,
  // TestTerminateDuringRedirect will (sometimes) fail in its TearDown.
  Self.ReceiveTrying(Self.LastSentRequest);
  Session.Terminate;
end;

procedure TestTIdSipOutboundSession.TestSendWithGruu;
var
  Session: TIdSipSession;
begin
  Self.UseGruu;

  Self.MarkSentRequestCount;
  Session := Self.CreateAndEstablishSession;
  CheckRequestSent(Session.ClassName + ': No INVITE sent');

  CheckEquals(Self.LastSentRequest.FirstContact.AsString,
              Session.LocalGruu.AsString,
              Session.ClassName + ': LocalGruu not set');
  Check(Session.LocalGruu.Address.HasGrid,
        Session.ClassName + ': Local GRUU doesn''t have a "grid" parameter');
end;

procedure TestTIdSipOutboundSession.TestSupportsExtension;
var
  MissingExtension: String;
  OK:               TIdSipResponse;
  Session:          TIdSipOutboundSession;
begin
  CheckNotEquals('',
                 Self.Module.AllowedExtensions,
                 Self.ClassName
               + ': Sanity check: our InviteModule should support at least one '
               + 'extension');

  Session := Self.Module.Call(Self.Core.From, Self.Destination, '', '');
  Session.Send;
  Check(Session.InitialRequest.HasHeader(SupportedHeaderFull),
        Self.ClassName
      + ': Sanity check: the InviteModule MUST insert a Supported header');

  OK := TIdSipResponse.InResponseTo(Session.InitialRequest, SIPOK, Self.Invite.FirstContact);
  try
    OK.Supported.Value := Self.Module.AllowedExtensions;

    MissingExtension := OK.Supported.Values[0];
    OK.Supported.Values.Delete(0);
    Self.ReceiveResponse(OK);

    Check(Session.SupportsExtension(OK.Supported.Values[0]),
          Self.ClassName
        + ': Session MUST support ' + OK.Supported.Values[0] + ' since both we '
        + 'and the remote party do');

    Check(not Session.SupportsExtension(MissingExtension),
          Self.ClassName
        + ': Session MUST NOT support ' + OK.Supported.Values[0] + ' since '
        + 'only we do');
  finally
    OK.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestTerminateDuringRedirect;
var
  Contacts: array of String;
  I:        Integer;
begin
  //                             Request count
  //  ---       INVITE      ---> #0
  // <---   302 (foo,bar)   ---
  //  ---        ACK        --->
  //  ---    INVITE (foo)   ---> #1
  //  ---    INVITE (bar)   ---> #2
  // <---     100 (foo)     --- (we receive 100s so the InviteActions will send CANCELs immediately)
  // <---     100 (bar)     ---
  // <Terminate the connection attempt>
  //  ---    CANCEL (foo)   ---> #3
  // <--- 200 (foo, CANCEL) ---
  //  ---    CANCEL (bar)   ---> #4
  // <--- 200 (bar, CANCEL) ---

  SetLength(Contacts, 2);
  Contacts[0] := 'sip:foo@bar.org';
  Contacts[1] := 'sip:bar@bar.org';

  Self.ReceiveMovedTemporarily(Contacts);

  Check(Self.SentRequestCount >= 3,
        'Not enough requests sent: 1 + 2 INVITEs: ' + Self.FailReason);

  Self.ReceiveTrying(Self.SentRequestAt(1));
  Self.ReceiveTrying(Self.SentRequestAt(2));

  Self.MarkSentRequestCount;
  Self.Session.Terminate;

  // ARG! Why do they make Length return an INTEGER? And WHY Abs() too?
  CheckEquals(Self.RequestCount + Cardinal(Length(Contacts)),
              Self.SentRequestCount,
              'Session didn''t attempt to terminate all INVITEs');

  Check(Self.SentRequestCount >= 5,
        'Not enough requests sent: 1 + 2 INVITEs, 2 CANCELs');

  for I := 0 to 1 do begin
    CheckEquals(Contacts[I],
                Self.SentRequestAt(I + 3).RequestUri.Uri,
                'CANCEL to ' + Contacts[I]);
    CheckEquals(MethodCancel,
                Self.SentRequestAt(I + 3).Method,
                'Request method to ' + Contacts[I]);
  end;
end;

procedure TestTIdSipOutboundSession.TestTerminateEstablishedSession;
var
  SessionCount: Integer;
begin
  Self.ReceiveOk(Self.LastSentRequest);

  Self.MarkSentRequestCount;
  SessionCount := Self.Core.SessionCount;
  Self.Session.Terminate;

  CheckRequestSent('No request sent');
  CheckEquals(MethodBye,
              Self.LastSentRequest.Method,
              'Session didn''t terminate with a BYE');

  Check(Self.Core.SessionCount < SessionCount,
        'Session not marked as terminated');
end;

procedure TestTIdSipOutboundSession.TestTerminateNetworkFailure;
var
  SessionCount: Integer;
begin
  Self.ReceiveOk(Self.LastSentRequest);

  // Send a BYE, only the attempt fails in the network.
  SessionCount := Self.Core.SessionCount;
  Self.Session.Terminate;

  Self.Dispatcher.Transport.FireOnException(Self.LastSentRequest,
                                            EIdConnectException,
                                            '10061',
                                            'Connection refused');

  Check(Self.Core.SessionCount < SessionCount,
        'Session not marked as terminated');
  CheckEquals(MethodBye,
              Self.LastSentRequest.Method,
              'Last request we attempted to send');
end;

procedure TestTIdSipOutboundSession.TestTerminateSignalled;
var
  L: TIdSipTestActionListener;
begin
  L := TIdSipTestActionListener.Create;
  try
    Self.Session.AddActionListener(L);
    try
      Self.EstablishSession(Self.Session);
      Self.Session.Terminate;

      Check(L.Terminated,
            Self.ClassName + ': Listeners not notified of termination');
    finally
      Self.Session.RemoveActionListener(L);
    end;
  finally
    L.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestTerminateUnestablishedSession;
var
  Invite:            TIdSipRequest;
  Request:           TIdSipRequest;
  RequestTerminated: TIdSipResponse;
  SessionCount:      Integer;
begin
  // When you Terminate a Session, the Session should attempt to CANCEL its
  // initial INVITE (if it hasn't yet received a final response):
  //
  //  ---                INVITE               --->
  // <---              100 Trying             ---
  // <----------user terminates session---------->
  //  ---                CANCEL               --->
  // <---         200 OK (for CANCEL)         ---
  // <--- 481 Request Terminated (for INVITE) ---

  Self.MarkSentRequestCount;

  Invite := TIdSipRequest.Create;
  try
    Invite.Assign(Self.LastSentRequest);

    // We don't actually send CANCELs when we've not received a provisional
    // response.
    Self.ReceiveTrying(Invite);

    RequestTerminated := TIdSipResponse.InResponseTo(Invite, SIPRequestTerminated);
    try
      RequestTerminated.ToHeader.Tag := 'something-arbitrary';

      SessionCount := Self.Core.SessionCount;
      Self.Session.Terminate;

      CheckRequestSent(Self.ClassName + ': no CANCEL sent');

      Request := Self.LastSentRequest;
      CheckEquals(MethodCancel,
                  Request.Method,
                  Self.ClassName + ': Session didn''t terminate with a CANCEL');

      Self.ReceiveResponse(RequestTerminated);

      Check(Self.Core.SessionCount < SessionCount,
            Self.ClassName + ': Session not marked as terminated');
    finally
      RequestTerminated.Free;
    end;
  finally
    Invite.Free;
  end;
end;

//******************************************************************************
//* TestSessionReplacer                                                        *
//******************************************************************************
//* TestSessionReplacer Public methods *****************************************

procedure TestSessionReplacer.SetUp;
begin
  inherited SetUp;

  Self.Alice          := Self.CreateTransferringUA('sip:alice@127.0.0.1');
  Self.AlicesNewPhone := Self.CreateTransferringUA('sip:alice@127.0.0.2');
  Self.Bob            := Self.CreateTransferringUA('sip:bob@127.0.0.3');
  Self.ParkPlace      := Self.CreateTransferringUA('sip:parkingplace@127.0.0.4');
end;

procedure TestSessionReplacer.TearDown;
begin
  Self.ParkPlace.Free;
  Self.Bob.Free;
  Self.Alice.Free;

  inherited TearDown;
end;

//* TestSessionReplacer Private methods ****************************************

function TestSessionReplacer.CreateTransferringUA(const Address: String): TIdSipUserAgent;
var
  SubMod: TIdSipSubscribeModule;
begin
  Result := Self.CreateUserAgent(Address);
  Result.InviteModule.AddListener(Self);

  (Result.Dispatcher as TIdSipMockTransactionDispatcher).Transport.AddBinding(TIdIPAddressParser.IncIPAddress(Self.LanIP),
                                                                              5060);

  SubMod := Result.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule;
  SubMod.AddPackage(TIdSipReferPackage);
  SubMod.AddListener(Self);
end;

procedure TestSessionReplacer.OnAddAction(UserAgent: TIdSipAbstractCore;
                                          Action: TIdSipAction);
begin
  // Do nothing.
end;

procedure TestSessionReplacer.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                                        Message: TIdSipMessage;
                                                        Binding: TIdConnectionBindings);
begin
  // It'd be nice to fail here, but three UAs all use this same procedure; for
  // each response, two of the three will drop the response as unmatched.
end;

procedure TestSessionReplacer.OnInboundCall(UserAgent: TIdSipInviteModule;
                                            Session: TIdSipInboundSession);
begin
  Self.InboundCall := Session;
  Self.ReceivingUA := UserAgent.UserAgent;
end;

procedure TestSessionReplacer.OnRemoveAction(UserAgent: TIdSipAbstractCore;
                                             Action: TIdSipAction);
begin
  // Do nothing.
end;

procedure TestSessionReplacer.OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                                    Subscription: TIdSipOutboundSubscription);
begin
end;

procedure TestSessionReplacer.OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                                    Subscription: TIdSipInboundSubscription);
begin
  Self.ReceivingUA := UserAgent as TIdSipUserAgent;
  Self.Refer       := Subscription;
end;

function TestSessionReplacer.SubscribeModuleOf(UA: TIdSipUserAgent): TIdSipSubscribeModule;
begin
  Result := UA.ModuleFor(MethodSubscribe) as TIdSipSubscribeModule;
end;

//* TestSessionReplacer Published methods **************************************

procedure TestSessionReplacer.TestSessionReplacer;
var
  AlicesReferToBob: TIdSipOutboundReferral;
  AlicesReplace:    TIdSipOutboundSession;
  BobsCallToAlice:  TIdSipOutboundSession;
  BobsCallToPP:     TIdSipOutboundSession;
  BobsContact:      TIdSipContactHeader;
begin
// cf. RFC 3891, section 1 for the inspiration for this test:
//        Alice          Alice                             Parking
//        phone1         phone2            Bob               Place
//        |               |                 |                   |
//        |<===============================>|                   |
//        |               |                 |                   |
//        |        Alice transfers Bob to Parking Place         |
//        |               |                 |                   |
//        |------------REFER/200----------->|                   |
//        |<--NOTIFY/200 (trying)-----------|--INVITE/200/ACK-->|
//        |<--NOTIFY/200 (success)----------|<=================>|
//        |------------BYE/200------------->|                   |
//        |               |                 |                   |
//        |               |                 |                   |
//        |  Alice later retrieves call from another phone      |
//        |               |                 |                   |
//        |               |-INV w/Replaces->|                   |
//        |               |<--200-----------|                   |
//        |               |---ACK---------->|----BYE/200------->|
//        |               |<===============>|                   |
//        |               |                 |                   |

  BobsContact := TIdSipContactHeader.Create;
  try
    // Bob calls Alice; Alice answers
    BobsCallToAlice := Bob.InviteModule.Call(Bob.From, Alice.From, '', '');
    BobsCallToAlice.Send;

    BobsContact.Assign(Self.LastSentRequest.FirstContact);
    Check(Assigned(Self.InboundCall) and (Self.ReceivingUA = Self.Alice),
          'Alice''s UA isn''t ringing');
    Self.InboundCall.AcceptCall('', '');

    // Alice refers Bob to the Parking Place
    AlicesReferToBob := Self.SubscribeModuleOf(Alice).Refer(BobsContact,
                                                            Self.ParkPlace.From);
    AlicesReferToBob.Send;
    Check(Assigned(Self.Refer) and (Self.ReceivingUA = Self.Bob),
          'Bob''s UA didn''t receive the REFER');
    CheckEquals(TIdSipInboundReferral.ClassName,
                Self.Refer.ClassName,
                'Unexpected subscription request');

    // and Bob calls the Parking Place, which automatically answers.
    BobsCallToPP := Self.Bob.InviteModule.Call(Bob.From, Self.ParkPlace.From, '', '');
    BobsCallToPP.Send;
    Check(Assigned(Self.InboundCall) and (Self.ReceivingUA = Self.ParkPlace),
          'The Parking Place UA isn''t ringing');
    Self.InboundCall.AcceptCall('', '');
    (Self.Refer as TIdSipInboundReferral).ReferenceSucceeded;

    // Now Alice retrieves the call from the Parking Place
    AlicesReplace := Self.AlicesNewPhone.InviteModule.ReplaceCall(BobsCallToPP.InitialRequest,
                                                                  Self.Bob.From,
                                                                  '',
                                                                  '');
    AlicesReplace.Send;
    Check(Assigned(Self.InboundCall) and (Self.ReceivingUA = Self.Bob),
          'Bob''s UA isn''t ringing');
  finally
    BobsContact.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipInviteModuleOnInboundCallMethod                                  *
//******************************************************************************
//* TestTIdSipInviteModuleOnInboundCallMethod Public methods *******************

procedure TestTIdSipInviteModuleOnInboundCallMethod.SetUp;
begin
  inherited SetUp;

  Self.Invite   := TIdSipTestResources.CreateBasicRequest;

  Self.Listener := TIdSipTestInviteModuleListener.Create;
  Self.Session  := TIdSipInboundSession.CreateInbound(Self.UA, Self.Invite, Self.Binding);

  Self.Method := TIdSipInviteModuleInboundCallMethod.Create;
  Self.Method.Session   := Self.Session;
  Self.Method.UserAgent := Self.UA.InviteModule;
end;

procedure TestTIdSipInviteModuleOnInboundCallMethod.TearDown;
begin
  Self.Method.Free;
  Self.Session.Free; // The UA doesn't own this action - we created it ourselves.
  Self.Listener.Free;
  Self.Invite.Free;

 inherited TearDown;
end;

//* TestTIdSipInviteModuleOnInboundCallMethod Published methods ****************

procedure TestTIdSipInviteModuleOnInboundCallMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.InboundCall,
        'Listener not notified');
  Check(Self.Session = Self.Listener.SessionParam,
        'Session param');
  Check(Self.UA.InviteModule = Self.Listener.UserAgentParam,
        'UserAgent param');
end;

//******************************************************************************
//* TestTIdSipInboundInviteFailureMethod                                       *
//******************************************************************************
//* TestTIdSipInboundInviteFailureMethod Public methods ************************

procedure TestTIdSipInboundInviteFailureMethod.SetUp;
begin
  inherited SetUp;

  Self.Invite := TIdSipTestResources.CreateBasicRequest;

  Self.Method := TIdSipInboundInviteFailureMethod.Create;
  Self.Method.Invite := TIdSipInboundInvite.CreateInbound(Self.UA, Self.Invite, Self.Binding);
end;

procedure TestTIdSipInboundInviteFailureMethod.TearDown;
begin
  Self.Method.Invite.Free;
  Self.Method.Free;
  Self.Invite.Free;

  inherited TearDown;
end;

//* TestTIdSipInboundInviteFailureMethod Published methods *********************

procedure TestTIdSipInboundInviteFailureMethod.TestRun;
var
  Listener: TIdSipTestInboundInviteListener;
begin
  Listener := TIdSipTestInboundInviteListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.Failed, 'Listener not notified');
    Check(Self.Method.Invite = Listener.InviteAgentParam,
          'InviteAgent param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipInboundInviteSuccessMethod                                       *
//******************************************************************************
//* TestTIdSipInboundInviteSuccessMethod Public methods ************************

procedure TestTIdSipInboundInviteSuccessMethod.SetUp;
begin
  inherited SetUp;

  Self.Invite := TIdSipTestResources.CreateBasicRequest;

  Self.Method := TIdSipInboundInviteSuccessMethod.Create;
  Self.Method.Invite := TIdSipInboundInvite.CreateInbound(Self.UA, Self.Invite, Self.Binding);
end;

procedure TestTIdSipInboundInviteSuccessMethod.TearDown;
begin
  Self.Method.Invite.Free;
  Self.Method.Free;
  Self.Invite.Free;

  inherited TearDown;
end;

//* TestTIdSipInboundInviteSuccessMethod Published methods *********************

procedure TestTIdSipInboundInviteSuccessMethod.TestRun;
var
  Listener: TIdSipTestInboundInviteListener;
begin
  Listener := TIdSipTestInboundInviteListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.Succeeded, 'Listener not notified');
    Check(Self.Method.Invite = Listener.InviteAgentParam,
          'InviteAgent param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TInviteMethodTestCase                                                      *
//******************************************************************************
//* TInviteMethodTestCase Public methods ***************************************

procedure TInviteMethodTestCase.SetUp;
var
  Nowhere: TIdSipAddressHeader;
begin
  inherited SetUp;

  Nowhere := TIdSipAddressHeader.Create;
  try
    Self.Invite := TIdSipOutboundInvite.Create(Self.UA);
  finally
    Nowhere.Free;
  end;

  Self.Listener := TIdSipTestInviteListener.Create;
end;

procedure TInviteMethodTestCase.TearDown;
begin
  Self.Listener.Free;
  Self.Invite.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipInviteCallProgressMethod                                         *
//******************************************************************************
//* TestTIdSipInviteCallProgressMethod Public methods **************************

procedure TestTIdSipInviteCallProgressMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipInviteCallProgressMethod.Create;
  Self.Method.Invite   := Self.Invite;
  Self.Method.Response := Self.Response;
end;

procedure TestTIdSipInviteCallProgressMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipInviteCallProgressMethod Published methods ***********************

procedure TestTIdSipInviteCallProgressMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.CallProgress,
        'Listener not notified');
  Check(Self.Invite = Self.Listener.InviteAgentParam,
        'InviteAgent param');
  Check(Self.Response = Self.Listener.ResponseParam,
        'Response param');
end;

//******************************************************************************
//* TestTIdSipInviteDialogEstablishedMethod                                    *
//******************************************************************************
//* TestTIdSipInviteDialogEstablishedMethod Public methods *********************

procedure TestTIdSipInviteDialogEstablishedMethod.SetUp;
var
  Nowhere: TIdSipAddressHeader;
begin
  inherited SetUp;

  Self.Method := TIdSipInviteDialogEstablishedMethod.Create;

  Nowhere := TIdSipAddressHeader.Create;
  try
    Self.Method.Invite := TIdSipOutboundInvite.Create(Self.UA);
    Self.Method.Dialog := TIdSipDialog.Create;
  finally
    Nowhere.Free;
  end;
end;

procedure TestTIdSipInviteDialogEstablishedMethod.TearDown;
begin
  Self.Method.Invite.Free;
  Self.Method.Dialog.Free;
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipInviteDialogEstablishedMethod Published methods ******************

procedure TestTIdSipInviteDialogEstablishedMethod.TestRun;
var
  Listener: TIdSipTestInviteListener;
begin
  Listener := TIdSipTestInviteListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.DialogEstablished, 'Listener not notified');
    Check(Self.Method.Invite = Listener.InviteAgentParam,
          'InviteAgent param');
    Check(Self.Method.Response = Listener.ResponseParam,
          'Response param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestInviteMethod                                                           *
//******************************************************************************
//* TestInviteMethod Public methods ********************************************

procedure TestInviteMethod.SetUp;

begin
  inherited SetUp;

  Self.Invite   := Self.UA.AddOutboundAction(TIdSipOutboundInitialInvite) as TIdSipOutboundInitialInvite;
  Self.Listener := TIdSipTestInviteListener.Create;
end;

procedure TestInviteMethod.TearDown;
begin
  Self.Listener.Free;
  // Self.UA owns Self.Invite!

  inherited TearDown;
end;

//******************************************************************************
//* TestSessionMethod                                                          *
//******************************************************************************
//* TestSessionMethod Public methods *******************************************

procedure TestSessionMethod.SetUp;
begin
  inherited SetUp;

  Self.Listener := TIdSipTestSessionListener.Create;
  Self.Session  := TIdSipOutboundSession.Create(Self.UA);
end;

procedure TestSessionMethod.TearDown;
begin
  Self.Session.Free;
  Self.Listener.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipEndedSessionMethod                                               *
//******************************************************************************
//* TestTIdSipEndedSessionMethod Public methods ********************************

procedure TestTIdSipEndedSessionMethod.SetUp;
const
  ArbValue = 42;
begin
  inherited SetUp;

  Self.Method := TIdSipEndedSessionMethod.Create;

  Self.Method.Session   := Self.Session;
  Self.Method.ErrorCode := ArbValue;
  Self.Method.Reason    := 'arbitrary reason';
end;

procedure TestTIdSipEndedSessionMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipEndedSessionMethod Published methods *****************************

procedure TestTIdSipEndedSessionMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Method.Session = Self.Listener.SessionParam,
        'Session param');
  CheckEquals(Self.Method.ErrorCode,
              Self.Listener.ErrorCodeParam,
              'ErrorCode param');
  CheckEquals(Self.Method.Reason,
              Self.Listener.ReasonParam,
              'Reason param');
end;

//******************************************************************************
//* TestTIdSipEstablishedSessionMethod                                         *
//******************************************************************************
//* TestTIdSipEstablishedSessionMethod Public methods **************************

procedure TestTIdSipEstablishedSessionMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipEstablishedSessionMethod.Create;

  Self.Method.RemoteSessionDescription := 'I describe a session''s media';
  Self.Method.MimeType                 := 'text/plain';
  Self.Method.Session                  := Self.Session;
end;

procedure TestTIdSipEstablishedSessionMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipEstablishedSessionMethod Published methods ***********************

procedure TestTIdSipEstablishedSessionMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Method.Session = Self.Listener.SessionParam,
        'Session param');
  CheckEquals(Self.Method.MimeType,
              Self.Listener.MimeType,
              'MimeType param');
  CheckEquals(Self.Method.RemoteSessionDescription,
              Self.Listener.RemoteSessionDescription,
              'RemoteSessionDescription param');
end;

//******************************************************************************
//* TestTIdSipModifiedSessionMethod                                            *
//******************************************************************************
//* TestTIdSipModifiedSessionMethod Public methods *****************************

procedure TestTIdSipModifiedSessionMethod.SetUp;
begin
  inherited SetUp;

  Self.Answer := TIdSipResponse.Create;

  Self.Method := TIdSipModifiedSessionMethod.Create;

  Self.Method.Session := Self.Session;
  Self.Method.Answer  := Self.Answer;
end;

procedure TestTIdSipModifiedSessionMethod.TearDown;
begin
  Self.Method.Free;
  Self.Answer.Free;

  inherited TearDown;
end;

//* TestTIdSipModifiedSessionMethod Published methods **************************

procedure TestTIdSipModifiedSessionMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Method.Answer = Self.Listener.AnswerParam,
        'Answer param');
  Check(Self.Method.Session = Self.Listener.SessionParam,
        'Session param');
end;

//******************************************************************************
//* TestTIdSipSessionModifySessionMethod                                       *
//******************************************************************************
//* TestTIdSipSessionModifySessionMethod Public methods ************************

procedure TestTIdSipSessionModifySessionMethod.SetUp;
var
  Invite: TIdSipRequest;
begin
  inherited SetUp;

  Self.Method := TIdSipSessionModifySessionMethod.Create;

  Invite := TIdSipTestResources.CreateBasicRequest;
  try
    Self.Session := Self.UA.InviteModule.Call(Invite.From, Invite.ToHeader, '', '');

    Self.Method.RemoteSessionDescription := Invite.Body;
    Self.Method.Session                  := Self.Session;
    Self.Method.MimeType                 := Invite.ContentType;
  finally
    Invite.Free;
  end;
end;

//* TestTIdSipSessionModifySessionMethod Published methods *********************

procedure TestTIdSipSessionModifySessionMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Method.Session = Self.Listener.SessionParam,
        'Modify param');
  CheckEquals(Self.Method.MimeType,
              Self.Listener.MimeType,
              'MimeType');
  CheckEquals(Self.Method.RemoteSessionDescription,
              Self.Listener.RemoteSessionDescription,
              'RemoteSessionDescription');
end;

//******************************************************************************
//* TestTIdSipProgressedSessionMethod                                          *
//******************************************************************************
//* TestTIdSipProgressedSessionMethod Public methods ***************************

procedure TestTIdSipProgressedSessionMethod.SetUp;
begin
  inherited SetUp;

  Self.Progress := TIdSipResponse.Create;

  Self.Method := TIdSipProgressedSessionMethod.Create;

  Self.Method.Progress := Self.Progress;
  Self.Method.Session  := Self.Session;
end;

procedure TestTIdSipProgressedSessionMethod.TearDown;
begin
  Self.Method.Free;
  Self.Progress.Free;

  inherited TearDown;
end;

//* TestTIdSipProgressedSessionMethod Published methods ************************

procedure TestTIdSipProgressedSessionMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.ProgressedSession,
        'Listener not notified');
  Check(Self.Method.Progress = Self.Listener.ProgressParam,
        'Progress param');
  Check(Self.Method.Session = Self.Listener.SessionParam,
        'Session param');
end;

//******************************************************************************
//* TestTIdSipSessionReferralMethod                                            *
//******************************************************************************
//* TestTIdSipSessionReferralMethod Public methods *****************************

procedure TestTIdSipSessionReferralMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipSessionReferralMethod.Create;
  Self.Refer  :=  TIdSipRequest.Create;

  Self.Method.Refer   := Self.Refer;
  Self.Method.Session := Self.Session;
  Self.Method.Binding := Self.Binding;
end;

procedure TestTIdSipSessionReferralMethod.TearDown;
begin
  Self.Refer.Free;
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipSessionReferralMethod Published methods **************************

procedure TestTIdSipSessionReferralMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.Referral,
        'Listener not notified');
  Check(Self.Method.Refer = Self.Listener.ReferParam,
        'Refer param');
  Check(Self.Method.Session = Self.Listener.SessionParam,
        'Session param');
  Check(Self.Method.Binding = Self.Listener.BindingParam,
        'Binding param');
end;

//******************************************************************************
//* TIdSipSessionWaitTestCase                                                  *
//******************************************************************************
//* TIdSipSessionWaitTestCase Public methods ***********************************

procedure TIdSipSessionWaitTestCase.SetUp;
begin
  inherited SetUp;

  Self.Wait := Self.WaitType.Create;
end;

procedure TIdSipSessionWaitTestCase.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TIdSipSessionWaitTestCase Protected methods ********************************

procedure TIdSipSessionWaitTestCase.CheckTriggerDoesNothing(Wait: TIdSipSessionWait;
                                                            Msg: String);
begin
  Fail(Self.ClassName + ' must override this method');
end;

function TIdSipSessionWaitTestCase.WaitType: TIdSipSessionWaitClass;
begin
  Result := nil;
  Fail(Self.ClassName + ' must override WaitType');
end;

//* TIdSipSessionWaitTestCase Published methods ********************************

procedure TIdSipSessionWaitTestCase.TestTriggerWithIDOfNonexistentObject;
begin
  // Check that the Wait does nothing if its SessionID doesn't point to a
  // registered object.

  Self.Wait.SessionID := 'fake ID';
  Self.CheckTriggerDoesNothing(Self.Wait, 'Wait didn''t check object type before triggering');
end;

procedure TIdSipSessionWaitTestCase.TestTriggerWithIDOfWrongTypeOfObject;
var
  ArbitraryObject: TIdSipAction;
begin
  // This test checks two things:
  //   1. If you give the Wait the ID of an object that isn't a TIdSipSession,
  //      the Wait does nothing, and
  //   2. the Wait doesn't blow up.

  ArbitraryObject := TIdSipOutboundCancel.Create(Self.Core);
  try
    Self.Wait.SessionID := ArbitraryObject.ID;

    CheckTriggerDoesNothing(Self.Wait, 'Wait didn''t check object type before triggering');
  finally
    ArbitraryObject.Free;
  end;
end;

//******************************************************************************
//* TIdSipSessionModifyingWaitTestCase                                         *
//******************************************************************************
//* TIdSipSessionModifyingWaitTestCase Public methods **************************

procedure TIdSipSessionModifyingWaitTestCase.SetUp;
var
  W: TIdSipSessionModifyingWait;
begin
  inherited SetUp;

  Self.InitialMimeType := 'text/plain';
  Self.InitialOffer    := 'haha';
  Self.NewMimeType     := 'text/html';
  Self.NewOffer        := '<b>haha</b>';

  // Self.Core will free this.
  Self.Call := Self.EstablishCall;

  W := Self.Wait as TIdSipSessionModifyingWait;
  W.ContentType := Self.NewMimeType;
  W.Offer       := Self.NewOffer;
  W.SessionID   := Self.Call.ID;
end;

//* TIdSipSessionModifyingWaitTestCase Protected methods ***********************

function TIdSipSessionModifyingWaitTestCase.EstablishCall: TIdSipSession;
begin
  Self.MarkSentRequestCount;
  Result := Self.Core.InviteModule.Call(Self.Core.From,
                                        Self.Destination,
                                        Self.InitialOffer,
                                        Self.InitialMimeType);
  Result.Send;
  CheckRequestSent('No INVITE sent');

  Self.MarkSentAckCount;
  Self.ReceiveOk(Self.LastSentRequest);
  CheckAckSent('No ACK sent');
end;

//******************************************************************************
//* TestTIdSipSessionAcceptCallModify                                          *
//******************************************************************************
//* TestTIdSipSessionAcceptCallModify Protected methods ************************

procedure TestTIdSipSessionAcceptCallModify.CheckTriggerDoesNothing(Wait: TIdSipSessionWait;
                                                                    Msg: String);
begin
  Self.MarkSentResponseCount;
  Self.Wait.Trigger;
  CheckNoResponseSent(Msg);
end;

function TestTIdSipSessionAcceptCallModify.WaitType: TIdSipSessionWaitClass;
begin
  Result := TIdSipSessionAcceptCallModify;
end;

//* TestTIdSipSessionAcceptCallModify Private methods **************************

procedure TestTIdSipSessionAcceptCallModify.ReceiveModify(LocalSession: TIdSipSession);
var
  Modifier: TIdSipRequest;
begin
  Modifier := TIdSipRequest.Create;
  try
    Modifier.Assign(LocalSession.InitialRequest);
    Modifier.From.Tag := LocalSession.Dialog.ID.RemoteTag;
    Modifier.ToHeader.Tag := LocalSession.Dialog.ID.LocalTag;

    Modifier.CSeq.SequenceNo := LocalSession.Dialog.RemoteSequenceNo + 1;

    Self.ReceiveRequest(Modifier);
  finally
    Modifier.Free;
  end;
end;

//* TestTIdSipSessionAcceptCallModify Published methods ************************

procedure TestTIdSipSessionAcceptCallModify.TestTrigger;
begin
  Self.ReceiveModify(Self.Call);
  Check(Self.Call.ModificationInProgress, 'Session didn''t receive modifying INVITE');

  Self.MarkSentResponseCount;
  Self.Wait.Trigger;
  CheckResponseSent('No response sent');
  CheckEquals(SIPOK, Self.LastSentResponse.StatusCode, 'Unexpected response sent');
  CheckEquals(Self.NewMimeType, Self.LastSentResponse.ContentType, 'Content-Type');
  CheckEquals(Self.NewOffer, Self.LastSentResponse.Body, 'Answer');
end;

//******************************************************************************
//* TestTIdSipSessionModifyWait                                                *
//******************************************************************************
//* TestTIdSipSessionModifyWait Protected methods ******************************

procedure TestTIdSipSessionModifyWait.CheckTriggerDoesNothing(Wait: TIdSipSessionWait;
                                                              Msg: String);
begin
  Self.MarkSentRequestCount;
  Self.Wait.Trigger;
  CheckNoRequestSent(Msg);
end;

function TestTIdSipSessionModifyWait.WaitType: TIdSipSessionWaitClass;
begin
  Result := TIdSipSessionModifyWait;
end;

//* TestTIdSipSessionModifyWait Published methods ******************************

procedure TestTIdSipSessionModifyWait.TestTrigger;
begin
  Self.MarkSentRequestCount;
  Self.Wait.Trigger;
  CheckRequestSent('No INVITE sent');
  CheckEquals(MethodInvite, Self.LastSentRequest.Method, 'Unexpected request sent');
  CheckEquals(Self.NewMimeType, Self.LastSentRequest.ContentType, 'Content-Type');
  CheckEquals(Self.NewOffer, Self.LastSentRequest.Body, 'Offer');
end;

//******************************************************************************
//* TIdSipInboundSessionWaitTestCase                                           *
//******************************************************************************
//* TIdSipInboundSessionWaitTestCase Public methods ****************************

procedure TIdSipInboundSessionWaitTestCase.SetUp;
begin
  inherited SetUp;

  Self.L := TIdSipTestInviteModuleListener.Create;
  Self.Core.InviteModule.AddListener(Self.L);

  Self.ReceiveInvite;
  Self.Session := Self.L.SessionParam;

  Self.Wait.SessionID := Self.Session.ID;
end;

procedure TIdSipInboundSessionWaitTestCase.TearDown;
begin
  Self.Core.InviteModule.RemoveListener(Self.L);
  Self.L.Free;

  inherited TearDown;
end;

//* TIdSipInboundSessionWaitTestCase Protected methods *************************

procedure TIdSipInboundSessionWaitTestCase.CheckTriggerDoesNothing(Wait: TIdSipSessionWait;
                                                                   Msg: String);
begin
  Self.MarkSentResponseCount;
  Self.Wait.Trigger;
  CheckNoResponseSent(Msg);
end;

//******************************************************************************
//* TestTIdSipSessionAcceptWait                                                *
//******************************************************************************
//* TestTIdSipSessionAcceptWait Public methods *********************************

procedure TestTIdSipSessionAcceptWait.SetUp;
var
  W: TIdSipSessionAcceptWait;
begin
  inherited SetUp;

  Self.Answer   := 'haha';
  Self.MimeType := 'text/plain';

  W := Self.Wait as TIdSipSessionAcceptWait;
  W.ContentType := Self.MimeType;
  W.Offer       := Self.Answer;
end;

//* TestTIdSipSessionAcceptWait Protected methods ******************************

function TestTIdSipSessionAcceptWait.WaitType: TIdSipSessionWaitClass;
begin
  Result := TIdSipSessionAcceptWait;
end;

//* TestTIdSipSessionAcceptWait Published methods ******************************

procedure TestTIdSipSessionAcceptWait.TestTrigger;
begin
  Self.MarkSentResponseCount;
  Self.Wait.Trigger;
  CheckResponseSent('No 200 OK response sent');
  CheckEquals(SIPOK, Self.LastSentResponse.StatusCode, 'Unexpected response');
  CheckEquals(Self.MimeType, Self.LastSentResponse.ContentType, 'Answer MIME type');
  CheckEquals(Self.Answer, Self.LastSentResponse.Body, 'Answer body');
end;

//******************************************************************************
//* TestTIdSipSessionRedirectWait                                              *
//******************************************************************************
//* TestTIdSipSessionRedirectWait Public methods *******************************

procedure TestTIdSipSessionRedirectWait.SetUp;
var
  W: TIdSipSessionRedirectWait;
begin
  inherited SetUp;

  Self.Target  := TIdSipToHeader.Create;
  Self.Target.Value := 'sip:charlie@example.com';

  W := Self.Wait as TIdSipSessionRedirectWait;
  W.NewTarget := Self.Target;
end;

procedure TestTIdSipSessionRedirectWait.TearDown;
begin
  Self.Target.Free;

  inherited TearDown;
end;

//* TestTIdSipSessionRedirectWait Protected methods ****************************

function TestTIdSipSessionRedirectWait.WaitType: TIdSipSessionWaitClass;
begin
  Result := TIdSipSessionRedirectWait;
end;

//* TestTIdSipSessionRedirectWait Published methods ****************************

procedure TestTIdSipSessionRedirectWait.TestTriggerWithTemporaryFalse;
var
  W: TIdSipSessionRedirectWait;
begin
  W := Self.Wait as TIdSipSessionRedirectWait;
  W.Temporary := false;

  Self.MarkSentResponseCount;
  Self.Wait.Trigger;
  CheckResponseSent('No response sent');
  CheckEquals(TIdSipInboundInvite.RedirectStatusCode(W.Temporary),
              Self.LastSentResponse.StatusCode,
              'Unexpected response sent');
  CheckEquals(Self.Target.Address.AsString,
              Self.LastSentResponse.FirstContact.Address.AsString,
              'Contact doesn''t contain the redirect target');
end;

procedure TestTIdSipSessionRedirectWait.TestTriggerWithTemporaryTrue;
var
  W: TIdSipSessionRedirectWait;
begin
  W := Self.Wait as TIdSipSessionRedirectWait;
  W.Temporary := true;

  Self.MarkSentResponseCount;
  Self.Wait.Trigger;
  CheckResponseSent('No response sent');
  CheckEquals(TIdSipInboundInvite.RedirectStatusCode(W.Temporary),
              Self.LastSentResponse.StatusCode,
              'Unexpected response sent');
  CheckEquals(Self.Target.Address.AsString,
              Self.LastSentResponse.FirstContact.Address.AsString,
              'Contact doesn''t contain the redirect target');              
end;

//******************************************************************************
//* TestTIdSipSendProvisionalWait                                              *
//******************************************************************************
//* TestTIdSipSendProvisionalWait Public methods *******************************

procedure TestTIdSipSendProvisionalWait.SetUp;
var
  W: TIdSipSendProvisionalWait;
begin
  inherited SetUp;

  Self.StatusCode := SIPQueued;
  Self.StatusText := 'Far side is taking its time';

  W := Self.Wait as TIdSipSendProvisionalWait;
  W.StatusCode := Self.StatusCode;
  W.StatusText := Self.StatusText;
end;

//* TestTIdSipSendProvisionalWait Protected methods ****************************

function TestTIdSipSendProvisionalWait.WaitType: TIdSipSessionWaitClass;
begin
  Result := TIdSipSendProvisionalWait;
end;

//* TestTIdSipSendProvisionalWait Published methods ****************************

procedure TestTIdSipSendProvisionalWait.TestTrigger;
begin
  Self.MarkSentResponseCount;
  Self.Wait.Trigger;
  CheckResponseSent('No response sent');
  CheckEquals(Self.StatusCode, Self.LastSentResponse.StatusCode, 'Unexpected Status-Code');
  CheckEquals(Self.StatusText, Self.LastSentResponse.StatusText, 'Unexpected Status-Text');
end;

//******************************************************************************
//* TestTIdSipSessionRejectWait                                                *
//******************************************************************************
//* TestTIdSipSessionRejectWait Public methods *********************************

procedure TestTIdSipSessionRejectWait.SetUp;
var
  W: TIdSipSessionRejectWait;
begin
  inherited SetUp;

  Self.StatusCode := SIPBusyHere;
  Self.ReasonPhrase := 'Call back later';

  W := Self.Wait as TIdSipSessionRejectWait;
  W.StatusCode := Self.StatusCode;
  W.StatusText := Self.ReasonPhrase;
end;

//* TestTIdSipSessionRejectWait Protected methods ******************************

function TestTIdSipSessionRejectWait.WaitType: TIdSipSessionWaitClass;
begin
  Result := TIdSipSessionRejectWait;
end;

//* TestTIdSipSessionRejectWait Published methods ******************************
{
procedure TestTIdSipSessionRejectWait.TestTrigger;
begin
  Self.MarkSentResponseCount;
  Self.Wait.Trigger;
  CheckResponseSent('No response sent');

  CheckEquals(Self.StatusCode,   Self.LastSentResponse.StatusCode, 'Unexpected Status-Code');
  CheckEquals(Self.ReasonPhrase, Self.LastSentResponse.StatusText, 'Unexpected ReasonPhrase');
end;
}
initialization
  RegisterTest('Invite Module tests', Suite);
end.
