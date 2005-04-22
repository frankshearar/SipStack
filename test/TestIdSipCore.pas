{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipCore;

interface

uses
  Classes, IdObservable, IdRTP, IdSdp, IdSimpleParser, IdSipCore, IdSipDialog,
  IdSipDialogID, IdSipMessage, IdSipMockTransactionDispatcher, IdSipTransport,
  IdTimerQueue, SyncObjs, TestFramework, TestFrameworkSip;

type
  TestTIdSipAbstractCore = class(TTestCaseTU)
  private
    ScheduledEventFired: Boolean;

    procedure ScheduledEvent(Sender: TObject);
  public
    procedure SetUp; override;
  published
    procedure TestNextCallID;
    procedure TestNextTag;
    procedure TestNotifyOfChange;
    procedure TestScheduleEvent;
  end;

  TestTIdSipRegistrations = class(TTestCase)
  private
    Regs: TIdSipRegistrations;
    Uri:  TIdSipUri;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddKnownRegistrar;
    procedure TestCallIDFor;
    procedure TestNextSequenceNoFor;
  end;

  TIdSipNullAction = class(TIdSipAction)
  protected
    function  CreateNewAttempt: TIdSipRequest; override;
  public
    class function Method: String; override;
  end;

  TestTIdSipActions = class(TTestCaseTU)
  private
    ActionProcUsed:      String;
    Actions:             TIdSipActions;
    DidntFindActionName: String;
    FoundAction:         TIdSipAction;
    FoundActionName:     String;
    FoundSession:        TIdSipSession;
    Options:             TIdSipRequest;

    procedure RecordSession(Session: TIdSipSession;
                            Invite: TIdSipRequest);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestActionCount;
    procedure TestAddActionNotifiesObservers;
    procedure TestAddObserver;
    procedure TestCleanOutTerminatedActions;
    procedure TestFindActionAndPerformBlock;
    procedure TestFindActionAndPerformBlockNoActions;
    procedure TestFindActionAndPerformBlockNoMatch;
    procedure TestFindActionAndPerformOrBlock;
    procedure TestFindActionAndPerformOrBlockNoMatch;
    procedure TestFindSessionAndPerform;
    procedure TestFindSessionAndPerformNoMatch;
    procedure TestFindSessionAndPerformNoSessions;
    procedure TestInviteCount;
    procedure TestRemoveObserver;
    procedure TestTerminateAllActions;
  end;

  TestTIdSipUserAgent = class(TTestCaseTU,
                              IIdObserver,
                              IIdSipActionListener,
                              IIdSipTransportSendingListener,
                              IIdSipSessionListener,
                              IIdSipUserAgentListener)
  private
    Dlg:                 TIdSipDialog;
    FailReason:          String;
    ID:                  TIdSipDialogID;
    InboundCallMimeType: String;
    InboundCallOffer:    String;
    LocalSequenceNo:     Cardinal;
    LocalUri:            TIdSipURI;
    OnChangedEvent:      TEvent;
    OnEndedSessionFired: Boolean;
    OnInboundCallFired:  Boolean;
    Password:            String;
    RemoteSequenceNo:    Cardinal;
    RemoteTarget:        TIdSipURI;
    RemoteUri:           TIdSipURI;
    RouteSet:            TIdSipHeaders;
    SendEvent:           TEvent;
    Session:             TIdSipInboundSession;
    SessionEstablished:  Boolean;
    TryAgain:            Boolean;
    UserAgentParam:      TIdSipAbstractUserAgent;

    procedure CheckCommaSeparatedHeaders(const ExpectedValues: String;
                                         Header: TIdSipHeader;
                                         const Msg: String);
    procedure CheckCreateRequest(Dest: TIdSipToHeader;
                                 Request: TIdSipRequest);
    procedure OnAuthenticationChallenge(UserAgent: TIdSipAbstractUserAgent;
                                        Challenge: TIdSipResponse;
                                        var Username: String;
                                        var Password: String;
                                        var TryAgain: Boolean);
    procedure OnChanged(Observed: TObject);
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractUserAgent;
                                        Message: TIdSipMessage;
                                        Receiver: TIdSipTransport);
    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String);
    procedure OnEstablishedSession(Session: TIdSipSession;
                                   const RemoteSessionDescription: String;
                                   const MimeType: String);
    procedure OnInboundCall(UserAgent: TIdSipAbstractUserAgent;
                            Session: TIdSipInboundSession);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Answer: TIdSipResponse);
    procedure OnModifySession(Modify: TIdSipInboundInvite);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               const Reason: String);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport);
    procedure ReceiveBye(Dialog: TIdSipDialog);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAcksDontMakeTransactions;
    procedure TestAcceptCallSchedulesResendOk;
    procedure TestActionsNotifyUAObservers;
    procedure TestAddAllowedContentType;
    procedure TestAddAllowedContentTypeMalformed;
    procedure TestAddAllowedLanguage;
    procedure TestAddAllowedLanguageLanguageAlreadyPresent;
    procedure TestAddAllowedMethod;
    procedure TestAddAllowedMethodMethodAlreadyPresent;
    procedure TestAddAllowedScheme;
    procedure TestAddAllowedSchemeSchemeAlreadyPresent;
    procedure TestAddObserver;
    procedure TestAddUserAgentListener;
    procedure TestAuthenticateWithNoAttachedAuthenticator;
    procedure TestByeWithAuthentication;
    procedure TestCallUsingProxy;
    procedure TestCancelNotifiesTU;
    procedure TestConcurrentCalls;
    procedure TestContentTypeDefault;
    procedure TestCreateAck;
    procedure TestCreateBye;
    procedure TestCreateInvite;
    procedure TestCreateInviteInsideDialog;
    procedure TestCreateInviteWithBody;
    procedure TestCreateOptions;
    procedure TestCreateRegister;
    procedure TestCreateRegisterReusesCallIDForSameRegistrar;
    procedure TestCreateReInvite;
    procedure TestCreateRequest;
    procedure TestCreateRequestSipsRequestUri;
    procedure TestCreateRequestUserAgent;
    procedure TestCreateRequestWithTransport;
    procedure TestCreateResponseToTagMissing;
    procedure TestCreateResponseUserAgent;
    procedure TestCreateResponseUserAgentBlank;
    procedure TestDeclinedCallNotifiesListeners;
    procedure TestDialogLocalSequenceNoMonotonicallyIncreases;
    procedure TestDispatchToCorrectSession;
    procedure TestDoNotDisturb;
    procedure TestDontReAuthenticate;
    procedure TestHasUnknownAccept;
    procedure TestHasUnknownContentEncoding;
    procedure TestHasUnknownContentType;
    procedure TestInboundCall;
    procedure TestInviteExpires;
    procedure TestInviteRaceCondition;
    procedure TestIsMethodAllowed;
    procedure TestIsSchemeAllowed;
    procedure TestLoopDetection;
    procedure TestMergedRequest;
    procedure TestModuleForString;
    procedure TestNotificationOfNewSession;
    procedure TestNotificationOfNewSessionRobust;
    procedure TestOutboundCallAndByeToXlite;
    procedure TestOutboundInviteSessionProgressResends;
    procedure TestOutboundInviteTerminatesWhenNoResponse;
    procedure TestReceiveByeForUnmatchedDialog;
    procedure TestReceiveByeForDialog;
    procedure TestReceiveByeDestroysTerminatedSession;
    procedure TestReceiveByeWithoutTags;
    procedure TestReceiveOptions;
    procedure TestReceiveResponseWithMultipleVias;
    procedure TestRejectMalformedAuthorizedRequest;
    procedure TestRejectNoContact;
    procedure TestRejectUnauthorizedRequest;
    procedure TestRejectUnknownContentEncoding;
    procedure TestRejectUnknownContentLanguage;
    procedure TestRejectUnknownContentType;
    procedure TestRejectUnknownExtension;
    procedure TestRejectUnknownScheme;
    procedure TestRejectUnsupportedMethod;
    procedure TestRejectUnsupportedSipVersion;
    procedure TestRemoveObserver;
    procedure TestRemoveUserAgentListener;
    procedure TestReregister;
    procedure TestRFC2543InviteCallFlow;
    procedure TestScheduleEventActionClosure;
    procedure TestSetContact;
    procedure TestSetContactMailto;
    procedure TestSetContactWildCard;
    procedure TestSetFrom;
    procedure TestSetFromMailto;
    procedure TestSimultaneousInAndOutboundCall;
    procedure TestTerminateAllCalls;
    procedure TestUnknownAcceptValue;
    procedure TestUnmatchedAckGetsDropped;
    procedure TestViaMatchesTransportParameter;
  end;

  TestTIdSipAction = class(TTestCaseTU,
                           IIdSipActionListener)
  protected
    ActionFailed: Boolean;
    ActionParam:  TIdSipAction;
    FailReason:   String;

    function  CreateAction: TIdSipAction; virtual;
    procedure OnNetworkFailure(Action: TIdSipAction;
                               const Reason: String);
    procedure ReceiveBadExtensionResponse;
    procedure ReceiveOkWithBody(Invite: TIdSipRequest;
                                const Body: String;
                                const ContentType: String);
  public
    procedure SetUp; override;
  published
    procedure TestIsInbound; virtual;
    procedure TestIsInvite; virtual;
    procedure TestIsOptions; virtual;
    procedure TestIsRegistration; virtual;
    procedure TestIsSession; virtual;
{
    procedure TestReceiveResponseBadExtension; // Currently our stack can't sent Requires; ergo we can't test in the usual fashion
    procedure TestReceiveResponseBadExtensionWithoutRequires;
}
  end;

  // These tests exercise the SIP discovery algorithms as defined in RFC 3263.
  TestLocation = class(TTestCaseTU,
                       IIdSipActionListener,
                       IIdSipInviteListener)
  private
    InviteOffer:    String;
    InviteMimeType: String;
    NetworkFailure: Boolean;
    TransportParam: String;

    function  CreateAction: TIdSipOutboundInitialInvite;
    procedure OnFailure(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse;
                        const Reason: String);
    procedure OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                  NewDialog: TIdSipDialog);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               const Reason: String);
    procedure OnRedirect(InviteAgent: TIdSipOutboundInvite;
                         Redirect: TIdSipResponse);
    procedure OnSuccess(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse);
  public
    procedure SetUp; override;
  published
    procedure TestAllLocationsFail;
    procedure TestLooseRoutingProxy;
    procedure TestStrictRoutingProxy;
    procedure TestUseCorrectTransport;
    procedure TestUseTransportParam;
    procedure TestUseUdpByDefault;
    procedure TestVeryLargeMessagesUseAReliableTransport;
  end;

  TestTIdSipInboundInvite = class(TestTIdSipAction,
                                  IIdSipInboundInviteListener)
  private
    Answer:         String;
    AnswerMimeType: String;
    Dialog:         TIdSipDialog;
    Failed:         Boolean;
    InviteAction:   TIdSipInboundInvite;
    OnSuccessFired: Boolean;

    procedure CheckAck(InviteAction: TIdSipInboundInvite);
    procedure CheckAckWithDifferentCSeq(InviteAction: TIdSipInboundInvite);
    procedure OnFailure(InviteAgent: TIdSipInboundInvite);
    procedure OnSuccess(InviteAgent: TIdSipInboundInvite;
                        Ack: TIdSipRequest);
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
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
    procedure TestMatchAck;
    procedure TestMatchAckToReInvite;
    procedure TestMatchAckToReInviteWithDifferentCSeq;
    procedure TestMatchAckWithDifferentCSeq;
    procedure TestMethod;
    procedure TestNotifyOfNetworkFailure;
    procedure TestRedirectCall;
    procedure TestRedirectCallPermanent;
    procedure TestRejectCallBusy;
    procedure TestResendOk;
    procedure TestRing;
    procedure TestSendSessionProgress;
    procedure TestTerminateAfterAccept;
    procedure TestTerminateBeforeAccept;
    procedure TestTimeOut;
  end;

  TestTIdSipOutboundInvite = class(TestTIdSipAction,
                                   IIdSipInviteListener)
  private
    Dialog:                   TIdSipDialog;
    InviteMimeType:           String;
    InviteOffer:              String;
    OnDialogEstablishedFired: Boolean;
    OnFailureFired:           Boolean;
    OnRedirectFired:          Boolean;
    ToHeaderTag:              String;

    procedure CheckReceiveFailed(StatusCode: Cardinal);
    function  CreateArbitraryDialog: TIdSipDialog;
    procedure OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                  NewDialog: TidSipDialog);
    procedure OnFailure(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse;
                        const Reason: String);
    procedure OnRedirect(Invite: TIdSipOutboundInvite;
                         Response: TIdSipResponse);
    procedure OnSuccess(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse);
  protected
    function  CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddListener;
    procedure TestAnswerInAck;
    procedure TestCancelAfterAccept;
    procedure TestCancelBeforeAccept;
    procedure TestCancelBeforeProvisional;
    procedure TestCancelReceiveInviteOkBeforeCancelOk;
    procedure TestInviteTwice;
    procedure TestIsInvite; override;
    procedure TestMethod;
    procedure TestOfferInInvite;
    procedure TestReceiveGlobalFailed;
    procedure TestReceiveRedirect;
    procedure TestReceiveRequestFailed;
    procedure TestReceiveServerFailed;
    procedure TestRemoveListener;
    procedure TestTerminateBeforeAccept;
    procedure TestTerminateAfterAccept;
    procedure TestTransactionCompleted;
  end;

  TestTIdSipOutboundRedirectedInvite = class(TestTIdSipOutboundInvite)
  protected
    function CreateAction: TIdSipAction; override;
  published
    procedure TestRedirectedInvite;
  end;

  TestTIdSipOutboundReInvite = class(TestTIdSipOutboundInvite)
  private
    Dialog: TIdSipDialog;
  protected
    function CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReInviteTwice;
  end;

  TestTIdSipInboundOptions = class(TestTIdSipAction)
  private
    procedure ReceiveOptions;
  published
    procedure TestIsInbound; override;
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
    procedure TestOptions;
    procedure TestOptionsWhenDoNotDisturb;
  end;

  TestTIdSipOutboundOptions = class(TestTIdSipAction,
                                    IIdSipOptionsListener)
  private
    ReceivedResponse: Boolean;

    procedure OnResponse(OptionsAgent: TIdSipOutboundOptions;
                         Response: TIdSipResponse);
  protected
    function CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
  published
    procedure TestAddListener;
    procedure TestIsOptions; override;
    procedure TestReceiveResponse;
    procedure TestRemoveListener;
  end;

  TestTIdSipRegistration = class(TestTIdSipAction)
  published
    procedure TestIsRegistration; override;
  end;

  TestTIdSipInboundRegistration = class(TestTIdSipRegistration)
  published
    procedure TestIsInbound; override;
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
  end;

  TestTIdSipOutboundRegistration = class(TestTIdSipRegistration,
                                         IIdSipRegistrationListener)
  private
    Contacts:   TIdSipContacts;
    MinExpires: Cardinal;
    Registrar:  TIdSipAbstractUserAgent;
    Succeeded:  Boolean;

    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts;
                        const Reason: String);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts);
    procedure ReceiveRemoteIntervalTooBrief;
  protected
    function RegistrarAddress: TIdSipUri;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddListener;
    procedure TestMethod;
    procedure TestReceiveFail;
    procedure TestReceiveIntervalTooBrief;
    procedure TestReceiveMovedPermanently;
    procedure TestReceiveOK;
    procedure TestRemoveListener;
    procedure TestReregisterTime;
    procedure TestSequenceNumberIncrements;
    procedure TestUsername;
  end;

  TExpiryProc = procedure(ExpiryTime: Cardinal) of object;

  TestTIdSipOutboundRegister = class(TestTIdSipOutboundRegistration)
  private
    procedure CheckAutoReregister(ReceiveResponse: TExpiryProc;
                                  EventIsScheduled: Boolean;
                                  const MsgPrefix: String);
    procedure ReceiveOkWithContactExpiresOf(ExpiryTime: Cardinal);
    procedure ReceiveOkWithExpiresOf(ExpiryTime: Cardinal);
    procedure ReceiveOkWithNoExpires(ExpiryTime: Cardinal);
  protected
    function  CreateAction: TIdSipAction; override;
  published
    procedure TestAutoReregister;
    procedure TestAutoReregisterContactHasExpires;
    procedure TestAutoReregisterNoExpiresValue;
    procedure TestAutoReregisterSwitchedOff;
    procedure TestReceiveIntervalTooBriefForOneContact;
    procedure TestRegister;
  end;

  TestTIdSipOutboundRegistrationQuery = class(TestTIdSipOutboundRegistration)
  protected
    function CreateAction: TIdSipAction; override;
  published
    procedure TestFindCurrentBindings;
  end;

  TestTIdSipOutboundUnregister = class(TestTIdSipOutboundRegistration)
  private
    Bindings: TIdSipContacts;
    WildCard: Boolean;
  protected
    function CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUnregisterAll;
    procedure TestUnregisterSeveralContacts;
  end;

  TestTIdSipSession = class(TestTIdSipAction,
                            IIdSipSessionListener)
  protected
    InboundModify:             TIdSipInboundInvite;
    MultiStreamSdp:            TIdSdpPayload;
    OnEndedSessionFired:       Boolean;
    OnEstablishedSessionFired: Boolean;
    OnModifiedSessionFired:    Boolean;
    OnModifySessionFired:      Boolean;
    SimpleSdp:                 TIdSdpPayload;

    procedure CheckResendWaitTime(Milliseconds: Cardinal;
                                  const Msg: String); virtual;
    function  CreateAndEstablishSession: TIdSipSession;
    function  CreateMultiStreamSdp: TIdSdpPayload;
    function  CreateRemoteReInvite(LocalDialog: TIdSipDialog): TIdSipRequest;
    function  CreateSimpleSdp: TIdSdpPayload;
    procedure EstablishSession(Session: TIdSipSession); virtual; abstract;
    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String); virtual;
    procedure OnEstablishedSession(Session: TIdSipSession;
                                   const RemoteSessionDescription: String;
                                   const MimeType: String); virtual;
    procedure OnModifiedSession(Session: TIdSipSession;
                                Answer: TIdSipResponse); virtual;
    procedure OnModifySession(Modify: TIdSipInboundInvite); virtual;
    procedure ReceiveRemoteReInvite(Session: TIdSipSession);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAckToInDialogInviteMatchesInvite;
    procedure TestInboundModify;
    procedure TestIsSession; override;
    procedure TestMatchBye;
    procedure TestMatchInitialRequest;
    procedure TestMatchModify;
    procedure TestMatchResponseToModify;
    procedure TestMatchResponseToInitialRequest;
    procedure TestModify;
    procedure TestModifyBeforeFullyEstablished;
    procedure TestModifyDuringModification;
    procedure TestModifyGlareInbound;
    procedure TestModifyGlareOutbound;
    procedure TestModifyRejectedWithTimeout;
    procedure TestModifyWaitTime;
    procedure TestReceiveByeWithPendingRequests;
    procedure TestRejectInviteWhenInboundModificationInProgress;
    procedure TestRejectInviteWhenOutboundModificationInProgress;
  end;

  TestTIdSipInboundSession = class(TestTIdSipSession,
                                   IIdRTPDataListener,
                                   IIdSipTransportSendingListener,
                                   IIdSipUserAgentListener)
  private
    RemoteContentType:      String;
    RemoteDesc:             String;
    SentRequestTerminated:  Boolean;
    Session:                TIdSipInboundSession;

    procedure OnAuthenticationChallenge(UserAgent: TIdSipAbstractUserAgent;
                                        Challenge: TIdSipResponse;
                                        var Username: String;
                                        var Password: String;
                                        var TryAgain: Boolean); overload;
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractUserAgent;
                                        Message: TIdSipMessage;
                                        Receiver: TIdSipTransport);
    procedure OnInboundCall(UserAgent: TIdSipAbstractUserAgent;
                            Session: TIdSipInboundSession);
    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdConnection);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport);
    procedure ReceiveAckWithBody(const SessionDesc,
                                 ContentType: String);
  protected
    procedure CheckResendWaitTime(Milliseconds: Cardinal;
                                  const Msg: String); override;
    function  CreateAction: TIdSipAction; override;
    procedure EstablishSession(Session: TIdSipSession); override;
    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String); override;
    procedure OnEstablishedSession(Session: TIdSipSession;
                                   const RemoteSessionDescription: String;
                                   const MimeType: String); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAcceptCall;
    procedure TestAddSessionListener;
    procedure TestCancelAfterAccept;
    procedure TestCancelBeforeAccept;
    procedure TestCancelNotifiesSession;
    procedure TestInviteHasNoOffer;
    procedure TestInviteHasOffer;
    procedure TestIsInbound; override;
    procedure TestIsOutboundCall;
    procedure TestMethod;
    procedure TestNotifyListenersOfEstablishedSession;
    procedure TestNotifyListenersOfEstablishedSessionInviteHasNoBody;
    procedure TestInboundModifyBeforeFullyEstablished;
    procedure TestInboundModifyReceivesNoAck;
    procedure TestReceiveBye;
    procedure TestReceiveOutOfOrderReInvite;
    procedure TestRedirectCall;
    procedure TestRejectCallBusy;
    procedure TestRemoveSessionListener;
    procedure TestTerminate;
    procedure TestTerminateUnestablishedSession;
  end;

  TestTIdSipOutboundSession = class(TestTIdSipSession,
                                    IIdSipUserAgentListener)
  private
    LocalMimeType:    String;
    LocalDescription: String;
    OnDroppedMessage: Boolean;
    RemoteDesc:       String;
    RemoteMimeType:   String;
    Session:          TIdSipOutboundSession;

    procedure OnAuthenticationChallenge(UserAgent: TIdSipAbstractUserAgent;
                                        Challenge: TIdSipResponse;
                                        var Username: String;
                                        var Password: String;
                                        var TryAgain: Boolean);
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractUserAgent;
                                        Message: TIdSipMessage;
                                        Receiver: TIdSipTransport);
    procedure OnInboundCall(UserAgent: TIdSipAbstractUserAgent;
                            Session: TIdSipInboundSession);
    procedure ReceiveRemoteDecline;
    procedure ReceiveForbidden;
    procedure ReceiveMovedTemporarily(const Contact: String); overload;
    procedure ReceiveMovedTemporarily(const Contacts: array of String); overload;
    procedure ReceiveOKWithRecordRoute;
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
  public
    procedure SetUp; override;
  published
    procedure TestAck;
    procedure TestAckFromRecordRouteResponse;
    procedure TestAckWithAuthorization;
    procedure TestAckWithProxyAuthorization;
    procedure TestCall;
    procedure TestCallRemoteRefusal;
    procedure TestCallNetworkFailure;
    procedure TestCallSecure;
    procedure TestCallSipsUriOverTcp;
    procedure TestCallSipUriOverTls;
    procedure TestCallWithOffer;
    procedure TestCallWithoutOffer;
    procedure TestCancelReceiveInviteOkBeforeCancelOk;
    procedure TestCircularRedirect;
    procedure TestDialogNotEstablishedOnTryingResponse;
    procedure TestDoubleRedirect;
    procedure TestEmptyTargetSetMeansTerminate;
    procedure TestGlobalFailureEndsSession;
    procedure TestHangUp;
    procedure TestIsOutboundCall;
    procedure TestMethod;
    procedure TestModifyUsesAuthentication;
    procedure TestNetworkFailuresLookLikeSessionFailures;
    procedure TestReceive2xxSendsAck;
    procedure TestReceive3xxSendsNewInvite;
    procedure TestReceive3xxWithOneContact;
    procedure TestReceive3xxWithNoContacts;
    procedure TestReceiveFailureResponseNotifiesOnce;
    procedure TestReceiveFinalResponseSendsAck;
    procedure TestRedirectAndAccept;
    procedure TestRedirectMultipleOks;
    procedure TestRedirectWithMultipleContacts;
    procedure TestTerminateDuringRedirect;
    procedure TestTerminateEstablishedSession;
    procedure TestTerminateUnestablishedSession;
  end;

  TActionMethodTestCase = class(TTestCase)
  private
    Dispatcher: TIdSipMockTransactionDispatcher;
    Response:   TIdSipResponse;
    UA:         TIdSipUserAgent;
  public
    procedure SetUp; override;
    procedure TearDown; override;
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

  TestTIdSipInviteDialogEstablishedMethod = class(TActionMethodTestCase)
  private
    Method: TIdSipInviteDialogEstablishedMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipInviteFailureMethod = class(TActionMethodTestCase)
  private
    Method: TIdSipInviteFailureMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipInviteRedirectMethod = class(TActionMethodTestCase)
  private
    Method: TIdSipInviteRedirectMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Run;
  end;

  TestTIdSipInviteSuccessMethod = class(TActionMethodTestCase)
  private
    Method: TIdSipInviteSuccessMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipOptionsResponseMethod = class(TActionMethodTestCase)
  private
    Method: TIdSipOptionsResponseMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestRegistrationMethod = class(TActionMethodTestCase)
  protected
    Bindings: TIdSipContacts;
    Reg:      TIdSipOutboundRegistration;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipRegistrationFailedMethod = class(TestRegistrationMethod)
  private
    Method: TIdSipRegistrationFailedMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipRegistrationSucceededMethod = class(TestRegistrationMethod)
  private
    Method: TIdSipRegistrationSucceededMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestSessionMethod = class(TActionMethodTestCase)
  protected
    Session: TIdSipSession;
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
    Modify: TIdSipInboundInvite;
    Method: TIdSipSessionModifySessionMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipUserAgentAuthenticationChallengeMethod = class(TTestCase)
  private
    Challenge: TIdSipResponse;
    L1:        TIdSipTestUserAgentListener;
    L2:        TIdSipTestUserAgentListener;
    Method:    TIdSipUserAgentAuthenticationChallengeMethod;
    UserAgent: TIdSipUserAgent;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFirstListenerDoesntSetPassword;
    procedure TestFirstListenerSetsPassword;
    procedure TestFirstListenerDoesntSetUsername;
    procedure TestFirstListenerSetsUsername;
    procedure TestNoListenerSetsPassword;
    procedure TestRun;
    procedure TestTryAgain;
  end;

  TestTIdSipUserAgentDroppedUnmatchedMessageMethod = class(TTestCase)
  private
    Method:   TIdSipUserAgentDroppedUnmatchedMessageMethod;
    Receiver: TIdSipTransport;
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipUserAgentInboundCallMethod = class(TActionMethodTestCase)
  private
    Method:  TIdSipUserAgentInboundCallMethod;
    Request: TIdSipRequest;
    Session: TIdSipInboundSession;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

implementation

uses
  IdException, IdSipAuthentication, IdSipDns, IdSipLocator, IdSipMockTransport,
  SysUtils;

type
  TIdSipCoreWithExposedNotify = class(TIdSipAbstractCore)
  public
    function  CreateRequest(Dest: TIdSipAddressHeader): TIdSipRequest; overload; override;
    function  CreateRequest(Dialog: TIdSipDialog): TIdSipRequest; overload; override;
    procedure TriggerNotify;
  end;

const
  DefaultTimeout = 5000;
  // SFTF: Sip Foundry Test Framework. cf. http://www.sipfoundry.org/sftf/
  SFTFInvite = 'INVITE sip:abc@80.168.137.82 SIP/2.0'#13#10
             + 'Via: SIP/2.0/UDP 81.86.64.25;branch=z9hG4bK-SCb-0-1105373135.55-81.86.64.25-first-request;rport=5060;received=81.86.64.25'#13#10
             + 'Via: SIP/2.0/UDP proxy1.example.com;branch=z9hG4bK-SCb-0-1105373135.55-81.86.64.25-proxy1-request1-fake'#13#10
             + 'Via: SIP/2.0/UDP ua.example.com;branch=z9hG4bK-SCb-0-1105373135.55-81.86.64.25-ua-request-fake'#13#10
             + 'From: sip:sc@81.86.64.25;tag=SCt-0-1105373135.56-81.86.64.25~case905'#13#10
             + 'Call-ID: 137057836-41e2a7cf@81.86.64.25'#13#10
             + 'Content-Length: 150'#13#10
             + 'Max-Forwards: 70'#13#10
             + 'To: sip:abc@80.168.137.82'#13#10
             + 'Contact: sip:sc@81.86.64.25'#13#10
             + 'CSeq: 1 INVITE'#13#10
             + 'Supported:'#13#10
             + 'Content-Type: application/sdp'#13#10
             + #13#10
             + 'v=0'#13#10
             + 'o=sc 1105373135 1105373135 IN IP4 81.86.64.25'#13#10
             + 's=Dummy on hold SDP'#13#10
             + 'c=IN IP4 0.0.0.0'#13#10
             + 'm=audio 65534 RTP/AVP 0'#13#10
             + 'a=rtpmap:0 PCMU/8000'#13#10
             + 'a=recvonly'#13#10;
  SFTFMergedInvite = 'INVITE sip:abc@80.168.137.82 SIP/2.0'#13#10
                   + 'Via: SIP/2.0/UDP 81.86.64.25;branch=z9hG4bK-SCb-0-1105373135.55-81.86.64.25-second-request;rport=5060;received=81.86.64.25'#13#10
                   + 'Via: SIP/2.0/UDP proxy2.example.com;branch=z9hG4bK-SCb-0-1105373135.55-81.86.64.25-proxy2-request1-fake'#13#10
                   + 'Via: SIP/2.0/UDP ua.example.com;branch=z9hG4bK-SCb-0-1105373135.55-81.86.64.25-ua-request-fake'#13#10
                   + 'From: sip:sc@81.86.64.25;tag=SCt-0-1105373135.56-81.86.64.25~case905'#13#10
                   + 'Call-ID: 137057836-41e2a7cf@81.86.64.25'#13#10
                   + 'Content-Length: 150'#13#10
                   + 'Max-Forwards: 70'#13#10
                   + 'To: sip:abc@80.168.137.82'#13#10
                   + 'Contact: sip:sc@81.86.64.25'#13#10
                   + 'CSeq: 1 INVITE'#13#10
                   + 'Supported:'#13#10
                   + 'Content-Type: application/sdp'#13#10
                   + #13#10
                   + 'v=0'#13#10
                   + 'o=sc 1105373135 1105373135 IN IP4 81.86.64.25'#13#10
                   + 's=Dummy on hold SDP'#13#10
                   + 'c=IN IP4 0.0.0.0'#13#10
                   + 'm=audio 65534 RTP/AVP 0'#13#10
                   + 'a=rtpmap:0 PCMU/8000'#13#10
                   + 'a=recvonly'#13#10;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipCore unit tests');
  Result.AddTest(TestTIdSipAbstractCore.Suite);
  Result.AddTest(TestTIdSipRegistrations.Suite);
  Result.AddTest(TestTIdSipActions.Suite);
  Result.AddTest(TestTIdSipUserAgent.Suite);
  Result.AddTest(TestLocation.Suite);
  Result.AddTest(TestTIdSipInboundInvite.Suite);
  Result.AddTest(TestTIdSipOutboundInvite.Suite);
  Result.AddTest(TestTIdSipOutboundReInvite.Suite);
  Result.AddTest(TestTIdSipInboundOptions.Suite);
  Result.AddTest(TestTIdSipOutboundOptions.Suite);
  Result.AddTest(TestTIdSipInboundRegistration.Suite);
  Result.AddTest(TestTIdSipOutboundRegister.Suite);
  Result.AddTest(TestTIdSipOutboundRegistrationQuery.Suite);
  Result.AddTest(TestTIdSipOutboundUnregister.Suite);
  Result.AddTest(TestTIdSipInboundSession.Suite);
  Result.AddTest(TestTIdSipOutboundSession.Suite);
  Result.AddTest(TestTIdSipInboundInviteFailureMethod.Suite);
  Result.AddTest(TestTIdSipInviteDialogEstablishedMethod.Suite);
  Result.AddTest(TestTIdSipInviteFailureMethod.Suite);
  Result.AddTest(TestTIdSipInviteRedirectMethod.Suite);
  Result.AddTest(TestTIdSipInviteSuccessMethod.Suite);
  Result.AddTest(TestTIdSipOptionsResponseMethod.Suite);
  Result.AddTest(TestTIdSipRegistrationFailedMethod.Suite);
  Result.AddTest(TestTIdSipRegistrationSucceededMethod.Suite);
  Result.AddTest(TestTIdSipEndedSessionMethod.Suite);
  Result.AddTest(TestTIdSipEstablishedSessionMethod.Suite);
  Result.AddTest(TestTIdSipModifiedSessionMethod.Suite);
  Result.AddTest(TestTIdSipSessionModifySessionMethod.Suite);
  Result.AddTest(TestTIdSipUserAgentAuthenticationChallengeMethod.Suite);
  Result.AddTest(TestTIdSipUserAgentDroppedUnmatchedMessageMethod.Suite);
  Result.AddTest(TestTIdSipUserAgentInboundCallMethod.Suite);
end;

//******************************************************************************
//* TIdSipCoreWithExposedNotify                                                *
//******************************************************************************
//* TIdSipCoreWithExposedNotify Public methods *********************************

function TIdSipCoreWithExposedNotify.CreateRequest(Dest: TIdSipAddressHeader): TIdSipRequest;
begin
  Result := nil;
end;

function TIdSipCoreWithExposedNotify.CreateRequest(Dialog: TIdSipDialog): TIdSipRequest;
begin
  Result := nil;
end;

procedure TIdSipCoreWithExposedNotify.TriggerNotify;
begin
  Self.NotifyOfChange;
end;

//******************************************************************************
//* TestTIdSipAbstractCore                                                     *
//******************************************************************************
//* TestTIdSipAbstractCore Public methods **************************************

procedure TestTIdSipAbstractCore.SetUp;
begin
  inherited SetUp;

  Self.ScheduledEventFired := false;
end;

//* TestTIdSipAbstractCore Private methods *************************************

procedure TestTIdSipAbstractCore.ScheduledEvent(Sender: TObject);
begin
  Self.ScheduledEventFired := true;
  Self.ThreadEvent.SetEvent;
end;


//* TestTIdSipAbstractCore Published methods ***********************************

procedure TestTIdSipAbstractCore.TestNextCallID;
var
  CallID: String;
begin
  CallID := Self.Core.NextCallID;

  Fetch(CallID, '@');

  CheckEquals(Self.Core.HostName, CallID, 'HostName not used');
end;

procedure TestTIdSipAbstractCore.TestNextTag;
var
  I:    Integer;
  Tags: TStringList;
begin
  // This is a woefully inadequate test. cf. RFC 3261, section 19.3

  Tags := TStringList.Create;
  try
    for I := 1 to 100 do
      Tags.Add(Self.Core.NextTag);

    // Find duplicates
    Tags.Sort;
    CheckNotEquals('', Tags[0], 'No null tags may be generated');

    for I := 1 to Tags.Count - 1 do begin
      CheckNotEquals('', Tags[I], 'No null tags may be generated (Tag #'
                                + IntToStr(I) + ')');

      CheckNotEquals(Tags[I-1], Tags[I], 'Duplicate tag generated');
    end;
  finally
  end;
end;

procedure TestTIdSipAbstractCore.TestNotifyOfChange;
var
  C: TIdSipCoreWithExposedNotify;
  O: TIdObserverListener;
begin
  C := TIdSipCoreWithExposedNotify.Create;
  try
    O := TIdObserverListener.Create;
    try
      C.AddObserver(O);
      C.TriggerNotify;
      Check(O.Changed,
            'Observer not notified');
      Check(O.Data = C,
           'Core didn''t return itself as parameter in the notify');
    finally
      O.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestScheduleEvent;
var
  EventCount: Integer;
begin
  EventCount := Self.DebugTimer.EventCount;
  Self.Core.ScheduleEvent(Self.ScheduledEvent, 50, Self.Invite.Copy);
  Check(EventCount < DebugTimer.EventCount,
        'Event not scheduled');
end;

//******************************************************************************
//* TestTIdSipRegistrations                                                    *
//******************************************************************************
//* TestTIdSipRegistrations Public methods *************************************

procedure TestTIdSipRegistrations.SetUp;
begin
  inherited SetUp;

  Self.Regs := TIdSipRegistrations.Create;
  Self.Uri  := TIdSipUri.Create('sip:registrar.tessier-ashpool.co.luna');
end;

procedure TestTIdSipRegistrations.TearDown;
begin
  Self.Uri.Free;
  Self.Regs.Free;

  inherited TearDown;
end;

//* TestTIdSipRegistrations Published methods **********************************

procedure TestTIdSipRegistrations.TestAddKnownRegistrar;
begin
  try
    Self.Regs.CallIDFor(Self.Uri);
  except
    on EIdSipRegistrarNotFound do;
  end;

  Self.Regs.AddKnownRegistrar(Self.Uri, '', 0);

  Self.Regs.CallIDFor(Self.Uri);
end;

procedure TestTIdSipRegistrations.TestCallIDFor;
const
  CallID = '329087234@casephone.fried-neurons.org';
begin
  // Registrar not known:
  try
    Self.Regs.CallIDFor(Self.Uri);
  except
    on EIdSipRegistrarNotFound do;
  end;

  Self.Regs.AddKnownRegistrar(Self.Uri, CallID, 0);
  CheckEquals(CallID,
              Self.Regs.CallIDFor(Self.Uri),
              'Call-ID');
end;

procedure TestTIdSipRegistrations.TestNextSequenceNoFor;
const
  SequenceNo = $decafbad;
var
  I: Cardinal;
begin
  // Registrar not known:
  try
    Self.Regs.NextSequenceNoFor(Self.Uri);
  except
    on EIdSipRegistrarNotFound do;
  end;

  Self.Regs.AddKnownRegistrar(Self.Uri, '', SequenceNo);

  for I := 0 to 9 do
  CheckEquals(IntToHex(SequenceNo + I, 8),
              IntToHex(Self.Regs.NextSequenceNoFor(Self.Uri), 8),
              'Next sequence number #' + IntToStr(I + 1));
end;

//******************************************************************************
//* TIdSipNullAction                                                           *
//******************************************************************************
//* TIdSipNullAction Public methods ********************************************

class function TIdSipNullAction.Method: String;
begin
  Result := '';
end;

//* TIdSipNullAction Protected methods *****************************************

function TIdSipNullAction.CreateNewAttempt: TIdSipRequest;
begin
  Result := nil;
end;

//******************************************************************************
//* TestTIdSipActions                                                          *
//******************************************************************************
//* TestTIdSipActions Public methods *******************************************

procedure TestTIdSipActions.SetUp;
begin
  inherited SetUp;

  Self.Actions := TIdSipActions.Create;
  Self.Options := TIdSipRequest.Create;
  Self.Options.Assign(Self.Invite);
  Self.Options.Method := MethodOptions;

  Self.ActionProcUsed      := '';
  Self.DidntFindActionName := 'DidntFindAction';
  Self.FoundActionName     := 'FoundActionName';
end;

procedure TestTIdSipActions.TearDown;
begin
  Self.Options.Free;
  Self.Actions.Free;

  inherited TearDown;
end;

//* TestTIdSipActions Private methods ******************************************

procedure TestTIdSipActions.RecordSession(Session: TIdSipSession;
                                          Invite: TIdSipRequest);
begin
  Self.FoundSession := Session;
end;

//* TestTIdSipActions Published methods ****************************************

procedure TestTIdSipActions.TestActionCount;
var
  I: Integer;
begin
  for I := 1 to 5 do begin
    Self.Actions.Add(TIdSipNullAction.Create(Self.Core));
    CheckEquals(I, Self.Actions.Count, 'Action not added');
  end;
end;

procedure TestTIdSipActions.TestAddActionNotifiesObservers;
var
  L1: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    Self.Actions.AddObserver(L1);

    Self.Actions.Add(TIdSipInboundInvite.Create(Self.Core, Self.Invite));

    Check(L1.Changed, 'L1 not notified');
  finally
    Self.Actions.RemoveObserver(L1);
    L1.Free;
  end;
end;

procedure TestTIdSipActions.TestAddObserver;
var
  L1, L2: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    L2 := TIdObserverListener.Create;
    try
      Self.Actions.AddObserver(L1);
      Self.Actions.AddObserver(L2);

      Self.Actions.Add(TIdSipInboundInvite.Create(Self.Core, Self.Invite));

      Check(L1.Changed, 'L1 not notified, thus not added');
      Check(L2.Changed, 'L2 not notified, thus not added');
    finally
      Self.Actions.RemoveObserver(L2);
      L2.Free;
    end;
  finally
    Self.Actions.RemoveObserver(L1);
    L1.Free;
  end;
end;

procedure TestTIdSipActions.TestCleanOutTerminatedActions;
var
  A:           TIdSipAction;
  ActionCount: Integer;
  O:           TIdObserverListener;
begin
  A := TIdSipNullAction.Create(Self.Core);
  Self.Actions.Add(A);

  ActionCount := Self.Actions.Count;
  A.Terminate;

  O := TIdObserverListener.Create;
  try
    Self.Actions.AddObserver(O);

    Self.Actions.CleanOutTerminatedActions;

    Check(Self.Actions.Count < ActionCount,
          'Terminated action not destroyed');
    Check(O.Changed, 'Observers not notified of change');
  finally
    Self.Actions.RemoveObserver(O);
    O.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionAndPerformBlock;
var
  A:      TIdSipAction;
  Finder: TIdSipActionFinder;
begin
  Self.Actions.Add(TIdSipInboundOptions.Create(Self.Core, Self.Options));
  A := Self.Actions.Add(TIdSipInboundInvite.Create(Self.Core, Self.Invite));
  Self.Actions.Add(TIdSipOutboundOptions.Create(Self.Core));

  Finder := TIdSipActionFinder.Create;
  try
    Self.Actions.FindActionAndPerform(A.InitialRequest, Finder);

    Check(Finder.Action = A, 'Wrong action found');
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionAndPerformBlockNoActions;
var
  Finder: TIdSipActionFinder;
begin
  Finder := TIdSipActionFinder.Create;
  try
    Self.Actions.FindActionAndPerform(Self.Options, Finder);

    Check(not Assigned(Finder.Action), 'An action found in an empty list');
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionAndPerformBlockNoMatch;
var
  Finder: TIdSipActionFinder;
begin
  Self.Actions.Add(TIdSipInboundInvite.Create(Self.Core, Self.Invite));

  Finder := TIdSipActionFinder.Create;
  try
    Self.Actions.FindActionAndPerform(Self.Options, Finder);

    Check(not Assigned(Finder.Action), 'An action found');
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionAndPerformOrBlock;
var
  A:      TIdSipAction;
  Finder: TIdSipActionFinder;
  Switch: TIdSipActionSwitch;
begin
  Self.Actions.Add(TIdSipInboundOptions.Create(Self.Core, Self.Options));
  A := Self.Actions.Add(TIdSipInboundInvite.Create(Self.Core, Self.Invite));
  Self.Actions.Add(TIdSipOutboundOptions.Create(Self.Core));

  Finder := TIdSipActionFinder.Create;
  try
    Switch := TIdSipActionSwitch.Create;
    try
      Self.Actions.FindActionAndPerformOr(A.InitialRequest,
                                          Finder,
                                          Switch);

      Check(Assigned(Finder.Action), 'Didn''t find action');
      Check(not Switch.Executed, 'Alternative block executed');
    finally
      Switch.Free;
    end;
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionAndPerformOrBlockNoMatch;
var
  Finder: TIdSipActionFinder;
  Switch: TIdSipActionSwitch;
begin
  Self.Actions.Add(TIdSipInboundInvite.Create(Self.Core, Self.Invite));

  Finder := TIdSipActionFinder.Create;
  try
    Switch := TIdSipActionSwitch.Create;
    try
      Self.Actions.FindActionAndPerformOr(Self.Options,
                                          Finder,
                                          Switch);

      Check(not Assigned(Finder.Action), 'Found action');
      Check(Switch.Executed, 'Alternative block didn''t execute');
    finally
      Switch.Free;
    end;
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestFindSessionAndPerform;
var
  S: TIdSipAction;
begin
  Self.Actions.Add(TIdSipInboundOptions.Create(Self.Core, Self.Options));
  S := Self.Actions.Add(TIdSipInboundSession.Create(Self.Core, Self.Invite, false));
  Self.Actions.Add(TIdSipOutboundOptions.Create(Self.Core));

  Self.Actions.FindSessionAndPerform(S.InitialRequest, Self.RecordSession);

  Check(Self.FoundSession = S, 'Wrong session found');
end;

procedure TestTIdSipActions.TestFindSessionAndPerformNoMatch;
begin
  Self.Actions.Add(TIdSipInboundOptions.Create(Self.Core, Self.Options));

  Self.Actions.FindSessionAndPerform(Self.Invite, Self.RecordSession);

  Check(not Assigned(Self.FoundAction), 'A session found');
end;

procedure TestTIdSipActions.TestFindSessionAndPerformNoSessions;
begin
  Self.Actions.FindSessionAndPerform(Self.Invite, Self.RecordSession);

  Check(not Assigned(Self.FoundSession), 'Session found in an empty list');
end;

procedure TestTIdSipActions.TestInviteCount;
begin
  CheckEquals(0, Self.Actions.InviteCount, 'No messages received');

  Self.Actions.Add(TIdSipInboundInvite.Create(Self.Core, Self.Invite));
  CheckEquals(1, Self.Actions.InviteCount, 'One INVITE');

  Self.Actions.Add(TIdSipInboundOptions.Create(Self.Core, Self.Options));
  CheckEquals(1, Self.Actions.InviteCount, 'One INVITE, one OPTIONS');

  Self.Actions.Add(TIdSipOutboundInvite.Create(Self.Core));
  CheckEquals(2, Self.Actions.InviteCount, 'Two INVITEs, one OPTIONS');

  Self.Actions.Add(TIdSipOutboundSession.Create(Self.Core));
  CheckEquals(2,
              Self.Actions.InviteCount,
              'Two INVITEs, one OPTIONS, and a Session');
end;

procedure TestTIdSipActions.TestRemoveObserver;
var
  L1, L2: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    L2 := TIdObserverListener.Create;
    try
      Self.Actions.AddObserver(L1);
      Self.Actions.AddObserver(L2);
      Self.Actions.RemoveObserver(L1);

      Self.Actions.Add(TIdSipInboundInvite.Create(Self.Core, Self.Invite));

      Check(not L1.Changed, 'L1 notified, thus not removed');
      Check(L2.Changed, 'L2 not notified, thus not added');
    finally
      Self.Actions.RemoveObserver(L2);
      L2.Free;
    end;
  finally
    Self.Actions.RemoveObserver(L1);
    L1.Free;
  end;
end;

procedure TestTIdSipActions.TestTerminateAllActions;
begin
  // We don't add INVITEs here because INVITEs need additional events to
  // properly terminate: an INVITE needs to wait for a final response, etc.
  Self.Actions.Add(TIdSipInboundOptions.Create(Self.Core, Self.Options));
  Self.Actions.Add(TIdSipOutboundRegistrationQuery.Create(Self.Core));
  Self.Actions.Add(TIdSipOutboundRegister.Create(Self.Core));

  Self.Actions.TerminateAllActions;
  Self.Actions.CleanOutTerminatedActions;
  CheckEquals(0,
              Self.Actions.Count,
              'Actions container didn''t terminate all actions');
end;

//******************************************************************************
//* TestTIdSipUserAgent                                                        *
//******************************************************************************
//* TestTIdSipUserAgent Public methods *****************************************

procedure TestTIdSipUserAgent.SetUp;
var
  C:        TIdSipContactHeader;
  F:        TIdSipFromHeader;
  Invite:   TIdSipRequest;
  Response: TIdSipResponse;
begin
  inherited SetUp;

  Self.Dispatcher.AddTransportSendingListener(Self);

  Self.OnChangedEvent := TSimpleEvent.Create;

  Self.Core.AddUserAgentListener(Self);

  Self.ID := TIdSipDialogID.Create('1', '2', '3');

  Self.LocalSequenceNo := 13;
  Self.LocalUri        := TIdSipURI.Create('sip:case@fried.neurons.org');
  Self.LocalSequenceNo := 42;
  Self.RemoteTarget    := TIdSipURI.Create('sip:sip-proxy1.tessier-ashpool.co.luna');
  Self.RemoteUri       := TIdSipURI.Create('sip:wintermute@tessier-ashpool.co.luna');

  Self.RouteSet := TIdSipHeaders.Create;
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1>';
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:6000>';
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:8000>';

  Invite := TIdSipTestResources.CreateBasicRequest;
  try
    Response := TIdSipTestResources.CreateBasicResponse;
    try
      Self.Dlg := TIdSipDialog.Create(Invite,
                                      Response,
                                      Self.ID,
                                      Self.LocalSequenceNo,
                                      Self.RemoteSequenceNo,
                                      Self.LocalUri,
                                      Self.RemoteUri,
                                      Self.RemoteTarget,
                                      false,
                                      Self.RouteSet);
    finally
      Response.Free;
    end;
  finally
    Invite.Free;
  end;

  C := TIdSipContactHeader.Create;
  try
    C.Value := 'sip:wintermute@tessier-ashpool.co.luna';
    Self.Core.Contact := C;
  finally
    C.Free;
  end;

  F := TIdSipFromHeader.Create;
  try
    F.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';
    Self.Core.From := F;
  finally
    F.Free;
  end;

  Self.SendEvent := TSimpleEvent.Create;

  Self.OnEndedSessionFired := false;
  Self.OnInboundCallFired  := false;
  Self.Password            := 'mycotoxin';
  Self.TryAgain            := true;
  Self.SessionEstablished  := false;

  Self.Locator.AddA(Self.Core.From.Address.Host, '127.0.0.1');
end;

procedure TestTIdSipUserAgent.TearDown;
begin
  Self.SendEvent.Free;
  Self.Dlg.Free;
  Self.RouteSet.Free;
  Self.RemoteUri.Free;
  Self.RemoteTarget.Free;
  Self.LocalUri.Free;
  Self.ID.Free;
  Self.OnChangedEvent.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgent Private methods ****************************************

procedure TestTIdSipUserAgent.CheckCommaSeparatedHeaders(const ExpectedValues: String;
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

procedure TestTIdSipUserAgent.CheckCreateRequest(Dest: TIdSipToHeader;
                                                 Request: TIdSipRequest);
var
  Contact: TIdSipContactHeader;
begin
  CheckEquals(Dest.Address,
              Request.RequestUri,
              'Request-URI not properly set');

  Check(Request.HasHeader(CallIDHeaderFull), 'No Call-ID header added');
  CheckNotEquals('',
                 (Request.FirstHeader(CallIDHeaderFull) as TIdSipCallIdHeader).Value,
                 'Call-ID must not be empty');

  Check(Request.HasHeader(ContactHeaderFull), 'No Contact header added');
  Contact := Request.FirstContact;
  Check(Contact.Equals(Self.Core.Contact), 'Contact header incorrectly set');

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

procedure TestTIdSipUserAgent.OnAuthenticationChallenge(UserAgent: TIdSipAbstractUserAgent;
                                                        Challenge: TIdSipResponse;
                                                        var Username: String;
                                                        var Password: String;
                                                        var TryAgain: Boolean);
begin
  Password := Self.Password;
  TryAgain := Self.TryAgain;
  Username := Self.Core.Username;
end;

procedure TestTIdSipUserAgent.OnChanged(Observed: TObject);
begin
  Self.OnChangedEvent.SetEvent;
end;

procedure TestTIdSipUserAgent.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractUserAgent;
                                                        Message: TIdSipMessage;
                                                        Receiver: TIdSipTransport);
begin
end;

procedure TestTIdSipUserAgent.OnEndedSession(Session: TIdSipSession;
                                             const Reason: String);
begin
  Self.OnEndedSessionFired := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUserAgent.OnEstablishedSession(Session: TIdSipSession;
                                                   const RemoteSessionDescription: String;
                                                   const MimeType: String);
begin
  Self.InboundCallMimeType := MimeType;
  Self.InboundCallOffer    := RemoteSessionDescription;
  Self.SessionEstablished  := true;
end;

procedure TestTIdSipUserAgent.OnInboundCall(UserAgent: TIdSipAbstractUserAgent;
                                            Session: TIdSipInboundSession);
begin
  Self.InboundCallMimeType := Session.RemoteMimeType;
  Self.InboundCallOffer    := Session.RemoteSessionDescription;
  Self.UserAgentParam      := UserAgent;
  Self.OnInboundCallFired := true;

  Session.AddSessionListener(Self);
  Self.Session := Session;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUserAgent.OnModifiedSession(Session: TIdSipSession;
                                                Answer: TIdSipResponse);
begin
end;

procedure TestTIdSipUserAgent.OnModifySession(Modify: TIdSipInboundInvite);
begin
end;

procedure TestTIdSipUserAgent.OnNetworkFailure(Action: TIdSipAction;
                                               const Reason: String);
begin
  Self.FailReason := Reason;
end;

procedure TestTIdSipUserAgent.OnSendRequest(Request: TIdSipRequest;
                                            Sender: TIdSipTransport);
begin
end;

procedure TestTIdSipUserAgent.OnSendResponse(Response: TIdSipResponse;
                                             Sender: TIdSipTransport);
begin
  if (Response.StatusCode = SIPSessionProgress) then
    Self.SendEvent.SetEvent;
end;

procedure TestTIdSipUserAgent.ReceiveBye(Dialog: TIdSipDialog);
var
  Bye: TIdSipRequest;
begin
  Bye := Self.CreateRemoteBye(Dialog);
  try
    Self.ReceiveRequest(Bye);
  finally
    Bye.Free;
  end;
end;

//* TestTIdSipUserAgent Published methods **************************************

procedure TestTIdSipUserAgent.TestAcksDontMakeTransactions;
var
  Ack:       TIdSipRequest;
  RemoteDlg: TIdSipDialog;
  TranCount: Cardinal;
begin
  Self.ReceiveInvite;

  Check(Assigned(Self.Session), 'TU not informed of inbound call');
  Self.Session.AcceptCall('', '');

  TranCount := Self.Dispatcher.TransactionCount;

  RemoteDlg := TIdSipDialog.CreateOutboundDialog(Self.LastSentRequest,
                                                 Self.LastSentResponse,
                                                 false);
  try
    Ack := RemoteDlg.CreateAck;
    try
      Self.ReceiveRequest(Ack);

      CheckEquals(TranCount,
                Self.Dispatcher.TransactionCount,
                  'A transaction got made in response to an ACK');
      CheckEquals(1,
                  Self.Core.SessionCount,
                  'ACK wasn''t simply dropped by the TU');
    finally
      Ack.Free;
    end;
  finally
    RemoteDlg.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestAcceptCallSchedulesResendOk;
begin
  Self.ReceiveInvite;
  Check(Assigned(Self.Session), 'TU not informed of inbound call');
  Self.MarkSentResponseCount;
  
  Self.Session.AcceptCall('', '');
  Self.DebugTimer.TriggerEarliestEvent;
  CheckResponseSent('No OK sent');
  CheckEquals(SIPOK, Self.LastSentResponse.StatusCode, 'Unexpected response sent');

  Self.MarkSentResponseCount;
  Self.DebugTimer.TriggerEarliestEvent;
  CheckResponseSent('No OK resent');
  CheckEquals(SIPOK, Self.LastSentResponse.StatusCode, 'Unexpected response resent');
end;

procedure TestTIdSipUserAgent.TestActionsNotifyUAObservers;
var
  L1: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    Self.Core.AddObserver(L1);

    Self.ReceiveInvite;

    Check(L1.Changed, 'L1 not notified');
  finally
    Self.Core.RemoveObserver(L1);
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestAddAllowedContentType;
var
  ContentTypes: TStrings;
begin
  ContentTypes := TStringList.Create;
  try
    Self.Core.AddAllowedContentType(SdpMimeType);
    Self.Core.AddAllowedContentType(PlainTextMimeType);

    ContentTypes.CommaText := Self.Core.AllowedContentTypes;

    CheckEquals(2, ContentTypes.Count, 'Number of allowed Content-Types');

    CheckEquals(SdpMimeType,       ContentTypes[0], SdpMimeType);
    CheckEquals(PlainTextMimeType, ContentTypes[1], PlainTextMimeType);
  finally
    ContentTypes.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestAddAllowedContentTypeMalformed;
var
  ContentTypes: String;
begin
  ContentTypes := Self.Core.AllowedContentTypes;
  Self.Core.AddAllowedContentType(' ');
  CheckEquals(ContentTypes,
              Self.Core.AllowedContentTypes,
              'Malformed Content-Type was allowed');
end;

procedure TestTIdSipUserAgent.TestAddAllowedLanguage;
var
  Languages: TStrings;
begin
  Languages := TStringList.Create;
  try
    Self.Core.AddAllowedLanguage('en');
    Self.Core.AddAllowedLanguage('af');

    Languages.CommaText := Self.Core.AllowedLanguages;

    CheckEquals(2, Languages.Count, 'Number of allowed Languages');

    CheckEquals('en', Languages[0], 'en first');
    CheckEquals('af', Languages[1], 'af second');
  finally
    Languages.Free;
  end;

  try
    Self.Core.AddAllowedLanguage(' ');
    Fail('Failed to forbid adding a malformed language ID');
  except
    on EIdException do;
  end;
end;

procedure TestTIdSipUserAgent.TestAddAllowedLanguageLanguageAlreadyPresent;
var
  Languages: TStrings;
begin
  Languages := TStringList.Create;
  try
    Self.Core.AddAllowedLanguage('en');
    Self.Core.AddAllowedLanguage('en');

    Languages.CommaText := Self.Core.AllowedLanguages;

    CheckEquals(1, Languages.Count, 'en was re-added');
  finally
    Languages.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestAddAllowedMethod;
var
  Methods: TStringList;
begin
  Methods := TStringList.Create;
  try
    Methods.CommaText := Self.Core.AllowedMethods;
    Methods.Sort;

    CheckEquals(MethodAck,     Methods[0], 'ACK first');
    CheckEquals(MethodBye,     Methods[1], 'BYE second');
    CheckEquals(MethodCancel,  Methods[2], 'CANCEL third');
    CheckEquals(MethodInvite,  Methods[3], 'INVITE fourth');
    CheckEquals(MethodOptions, Methods[4], 'OPTIONS fifth');

    CheckEquals(5, Methods.Count, 'Number of allowed methods');
  finally
    Methods.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestAddAllowedMethodMethodAlreadyPresent;
var
  Methods: TStrings;
  MethodCount: Cardinal;
begin
  Methods := TStringList.Create;
  try
    Self.Core.AddModule(TIdSipInviteModule);
    Methods.CommaText := Self.Core.AllowedMethods;
    MethodCount := Methods.Count;

    Self.Core.AddModule(TIdSipInviteModule);
    Methods.CommaText := Self.Core.AllowedMethods;

    CheckEquals(MethodCount, Methods.Count, MethodInvite + ' was re-added');
  finally
    Methods.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestAddAllowedScheme;
var
  Schemes: TStrings;
begin
  Schemes := TStringList.Create;
  try
    Self.Core.AddAllowedScheme(SipScheme);
    Self.Core.AddAllowedScheme(SipsScheme);

    Schemes.CommaText := Self.Core.AllowedSchemes;

    CheckEquals(2, Schemes.Count, 'Number of allowed Schemes');

    CheckEquals(SipScheme,  Schemes[0], 'SIP first');
    CheckEquals(SipsScheme, Schemes[1], 'SIPS second');
  finally
    Schemes.Free;
  end;

  try
    Self.Core.AddAllowedScheme(' ');
    Fail('Failed to forbid adding a malformed URI scheme');
  except
    on EIdException do;
  end;
end;

procedure TestTIdSipUserAgent.TestAddAllowedSchemeSchemeAlreadyPresent;
var
  Schemes: TStrings;
begin
  Schemes := TStringList.Create;
  try
    Self.Core.AddAllowedScheme(SipScheme);

    Schemes.CommaText := Self.Core.AllowedSchemes;

    CheckEquals(1, Schemes.Count, 'SipScheme was re-added');
  finally
    Schemes.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestAddObserver;
var
  L1, L2: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    L2 := TIdObserverListener.Create;
    try
      Self.Core.AddObserver(L1);
      Self.Core.AddObserver(L2);

      Self.ReceiveInvite;

      Check(L1.Changed and L2.Changed, 'Not all Listeners notified, hence not added');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestAddUserAgentListener;
var
  L1, L2: TIdSipTestUserAgentListener;
begin
  L1 := TIdSipTestUserAgentListener.Create;
  try
    L2 := TIdSipTestUserAgentListener.Create;
    try
      Self.Core.AddUserAgentListener(L1);
      Self.Core.AddUserAgentListener(L2);

      Self.ReceiveInvite;

      Check(L1.InboundCall and L2.InboundCall,
            'Not all Listeners notified, hence not added');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestAuthenticateWithNoAttachedAuthenticator;
begin
  // We make sure that no access violations occur just because we've not
  // attached an authenticator to the Core.
  Self.Core.RequireAuthentication := true;
  Self.Invite.AddHeader(AuthorizationHeader);
  Self.ReceiveInvite;
end;

procedure TestTIdSipUserAgent.TestByeWithAuthentication;
var
  Session: TIdSipOutboundSession;
begin
  //  ---      INVITE      --->
  // <---      200 OK      --->
  //  ---        ACK       --->
  // ==========================
  //       Media streams
  // ==========================
  //  ---        BYE       --->
  // <--- 401 Unauthorized ---
  //  ---        BYE       --->
  // <---      200 OK      --->

  Session := Self.Core.Call(Self.Destination, '', '');
  Session.AddSessionListener(Self);
  Session.Send;

  Self.MarkSentAckCount;
  Self.ReceiveOk(Self.LastSentRequest);
  CheckAckSent('No ACK sent: ' + Self.FailReason);

  Session.Terminate;

  // This is a bit tricky - the Transaction layer reissues the request, not the
  // Transaction-User layer. All the TU layer does is provide an authentication
  // token.
  Self.MarkSentRequestCount;
  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');
  Self.CheckRequestSent('No re-issue of a BYE');
end;

procedure TestTIdSipUserAgent.TestCallUsingProxy;
const
  ProxyUri = 'sip:proxy.tessier-ashpool.co.luna';
var
  Invite: TIdSipRequest;
begin
  Self.Core.Proxy.Uri := ProxyUri;
  Self.Core.HasProxy := true;

  Self.Core.Call(Self.Destination, '', '').Send;

  Invite := Self.LastSentRequest;
  Check(Invite.HasHeader(RouteHeader),
        'No Route header added');

  Invite.Route.First;
  CheckEquals(ProxyUri,
              Invite.Route.CurrentRoute.Address.Uri,
              'Route points to wrong proxy');
end;

procedure TestTIdSipUserAgent.TestCancelNotifiesTU;
var
  SessCount: Integer;
begin
  Self.ReceiveInvite;
  SessCount := Self.Core.SessionCount;
  Self.ReceiveCancel;

  Check(Self.OnEndedSessionFired,
        'UA not notified of remote CANCEL');
  Check(Self.Core.SessionCount < SessCount,
        'UA didn''t remove cancelled session');
end;

procedure TestTIdSipUserAgent.TestConcurrentCalls;
var
  AckOne:    TIdSipRequest;
  AckTwo:    TIdSipRequest;
  ByeOne:    TIdSipRequest;
  ByeTwo:    TIdSipRequest;
  DialogOne: TIdSipDialog;
  DialogTwo: TIdSipDialog;
  InviteOne: TIdSipRequest;
  InviteTwo: TIdSipRequest;
begin
  // <---    INVITE #1   ---
  //  ---     100 #1     --->
  //  ---     180 #1     --->
  //  ---     200 #1     --->
  // <---     ACK #1     ---
  //  ---   200 #1 (ACK) --->
  // <---    INVITE #2   ---
  //  ---     100 #2     --->
  //  ---     180 #2     --->
  //  ---     200 #2     --->
  // <---     ACK #2     ---
  //  ---   200 #2 (ACK) --->
  // <---     BYE #1     ---
  //  ---   200 #1 (BYE) --->
  // <---     BYE #2     ---
  //  ---   200 #2 (BYE) --->

  Self.Dispatcher.Transport.WriteLog := true;

  InviteOne := TIdSipTestResources.CreateBasicRequest;
  try
    InviteTwo := TIdSipTestResources.CreateBasicRequest;
    try
      InviteOne.CallID         := '1.' + InviteOne.CallID;
      InviteOne.From.Tag       := '1';
      InviteOne.LastHop.Branch := InviteOne.LastHop.Branch + '1';
      InviteTwo.CallID         := '2.' + InviteTwo.CallID;
      InviteTwo.From.Tag       := '2';
      InviteTwo.LastHop.Branch := InviteTwo.LastHop.Branch + '2';

      Self.ReceiveRequest(InviteOne);
      Check(Self.OnInboundCallFired, 'OnInboundCall didn''t fire for 1st INVITE');
      Self.Session.AcceptCall('', '');

      // DialogOne represents the remote agent's dialog for the 1st INVITE.
      DialogOne := TIdSipDialog.CreateInboundDialog(InviteOne,
                                                    Self.LastSentResponse,
                                                    InviteOne.RequestUri.IsSecure);
      try
        AckOne := DialogOne.CreateAck;
        try
          Self.ReceiveRequest(AckOne);
        finally
          AckOne.Free;
        end;

        Self.OnInboundCallFired := false;
        Self.ReceiveRequest(InviteTwo);
        Check(Self.OnInboundCallFired, 'OnInboundCall didn''t fire for 2nd INVITE');
        Self.Session.AcceptCall('', '');

        // DialogTwo represents the remote agent's dialog for the 2nd INVITE.
        DialogTwo := TIdSipDialog.CreateInboundDialog(InviteTwo,
                                                      Self.LastSentResponse,
                                                      InviteTwo.RequestUri.IsSecure);
        try
          AckTwo := DialogTwo.CreateAck;
          try
            Self.ReceiveRequest(AckTwo);
          finally
            AckTwo.Free;
          end;

          Self.MarkSentResponseCount;
          ByeOne := DialogOne.CreateRequest;
          try
            Self.ReceiveBye(DialogOne);
          finally
            ByeOne.Free;
          end;

          CheckResponseSent('No response sent for the 1st INVITE''s BYE');
          CheckEquals(SIPOK,
                      Self.LastSentResponse.StatusCode,
                      'Unexpected response for the 1st INVITE''s BYE');

          Self.MarkSentResponseCount;
          ByeTwo := DialogTwo.CreateRequest;
          try
            Self.ReceiveBye(DialogTwo);
          finally
            ByeTwo.Free;
          end;

          CheckResponseSent('No response sent for the 2nd INVITE''s BYE');
          CheckEquals(SIPOK,
                      Self.LastSentResponse.StatusCode,
                      'Unexpected response for the 2nd INVITE''s BYE');
        finally
          DialogTwo.Free;
        end;
      finally
        DialogOne.Free;
      end;
    finally
      InviteTwo.Free;
    end;
  finally
    InviteOne.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestContentTypeDefault;
begin
  CheckEquals(SdpMimeType,
              Self.Core.AllowedContentTypes,
              'AllowedContentTypes');
end;

procedure TestTIdSipUserAgent.TestCreateAck;
var
  Ack: TIdSipRequest;
begin
  Ack := Self.Core.CreateAck(Self.Dlg);
  try
    CheckEquals(1, Ack.Path.Count, 'Wrong number of Via headers');
  finally
    Ack.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateBye;
var
  Bye: TIdSipRequest;
begin
  Bye := Self.Core.CreateBye(Self.Dlg);
  try
    CheckEquals(MethodBye, Bye.Method, 'Unexpected method');
    CheckEquals(Bye.Method,
                Bye.CSeq.Method,
                'CSeq method doesn''t match request method');
  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateInvite;
var
  Dest:    TIdSipToHeader;
  Request: TIdSipRequest;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna';
    Request := Self.Core.CreateInvite(Dest, '', '');
    try
      Self.CheckCreateRequest(Dest, Request);
      CheckEquals(MethodInvite, Request.Method, 'Incorrect method');

      Check(not Request.ToHeader.HasTag,
            'This request is outside of a dialog, hence MUST NOT have a '
          + 'To tag. See RFC:3261, section 8.1.1.2');

      Check(Request.HasHeader(CSeqHeader), 'No CSeq header');
      Check(not Request.HasHeader(ContentDispositionHeader),
            'Needless Content-Disposition header');

      Check(Request.HasHeader(AllowHeader), 'No Allow header');
      CheckCommaSeparatedHeaders(Self.Core.AllowedMethods,
                                 Request.FirstHeader(AllowHeader),
                                 'Allow header');

      Check(Request.HasHeader(SupportedHeaderFull), 'No Supported header');
      CheckEquals(Self.Core.AllowedExtensions,
                  Request.FirstHeader(SupportedHeaderFull).Value,
                  'Supported header value');
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateInviteInsideDialog;
var
  Invite: TIdSipRequest;
begin
  Invite := Self.Core.CreateReInvite(Self.Dlg, '', '');
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
    CheckCommaSeparatedHeaders(Self.Core.AllowedMethods,
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

procedure TestTIdSipUserAgent.TestCreateInviteWithBody;
var
  Invite: TIdSipRequest;
  Body:   String;
begin
  Body := 'foo fighters';

  Invite := Self.Core.CreateInvite(Self.Destination, Body, 'text/plain');
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

procedure TestTIdSipUserAgent.TestCreateOptions;
var
  Options: TIdSipRequest;
begin
  Options := Self.Core.CreateOptions(Self.Destination);
  try
    CheckEquals(MethodOptions, Options.Method,      'Incorrect method');
    CheckEquals(MethodOptions, Options.CSeq.Method, 'Incorrect CSeq method');
    Check(Options.HasHeader(AcceptHeader),          'Missing Accept header');
    CheckEquals(Self.Core.AllowedContentTypes,
                Options.FirstHeader(AcceptHeader).Value,
                'Accept value');
  finally
    Options.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateRegister;
var
  Register: TIdSipRequest;
begin
  Register := Self.Core.CreateRegister(Self.Destination);
  try
    CheckEquals(MethodRegister, Register.Method,      'Incorrect method');
    CheckEquals(MethodRegister, Register.CSeq.Method, 'Incorrect CSeq method');
    CheckEquals('', Register.RequestUri.Username, 'Request-URI Username');
    CheckEquals('', Register.RequestUri.Password, 'Request-URI Password');

    CheckEquals(Self.Core.Contact.Value,
                Register.FirstHeader(ContactHeaderFull).Value,
                'Contact');
    CheckEquals(Self.Core.Contact.Value,
                Register.ToHeader.Value,
                'To');
    CheckEquals(Register.ToHeader.Value,
                Register.From.Value,
                'From');
  finally
    Register.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateRegisterReusesCallIDForSameRegistrar;
var
  FirstCallID:  String;
  Reg:          TIdSipRequest;
  SecondCallID: String;
begin
  Reg := Self.Core.CreateRegister(Self.Destination);
  try
    FirstCallID := Reg.CallID;
  finally
    Reg.Free;
  end;

  Reg := Self.Core.CreateRegister(Self.Destination);
  try
    SecondCallID := Reg.CallID;
  finally
    Reg.Free;
  end;

  CheckEquals(FirstCallID,
              SecondCallID,
              'Call-ID SHOULD be the same for same registrar');

  Self.Destination.Address.Uri := 'sip:enki.org';
  Reg := Self.Core.CreateRegister(Self.Destination);
  try
    CheckNotEquals(FirstCallID,
                   Reg.CallID,
                   'Call-ID SHOULD be different for new registrar');
  finally
    Reg.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateReInvite;
var
  Invite: TIdSipRequest;
begin
  Invite := Self.Core.CreateReInvite(Self.Dlg, 'foo', 'bar');
  try
    CheckEquals(MethodInvite, Invite.Method, 'Method');
    CheckEquals('foo',        Invite.Body, 'Body');
    CheckEquals('bar',        Invite.ContentType, 'Content-Type');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateRequest;
var
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna';
    Request := Self.Core.CreateRequest(Dest);
    try
      Self.CheckCreateRequest(Dest, Request);
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateRequestSipsRequestUri;
var
  Contact: TIdSipContactHeader;
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sips:wintermute@tessier-ashpool.co.luna';
    Request := Self.Core.CreateRequest(Dest);
    try
      Contact := Request.FirstContact;
      CheckEquals(SipsScheme,
                  Contact.Address.Scheme,
                  'Contact doesn''t have a SIPS URI');
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateRequestUserAgent;
var
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Self.Core.UserAgentName := 'SATAN/1.0';

  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna';
    Request := Self.Core.CreateRequest(Dest);
    try
      CheckEquals(Self.Core.UserAgentName,
                  Request.FirstHeader(UserAgentHeader).Value,
                  'User-Agent header not set');
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateRequestWithTransport;
var
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp';
    Request := Self.Core.CreateRequest(Dest);
    try
      CheckEquals(UdpTransport,
                  Request.LastHop.Transport,
                  'UDP transport not specified');
    finally
      Request.Free;
    end;

    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna;transport=tcp';
    Request := Self.Core.CreateRequest(Dest);
    try
      CheckEquals(TcpTransport,
                  Request.LastHop.Transport,
                  'TCP transport not specified');
    finally
      Request.Free;
    end;

    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna;transport=foo';
    Request := Self.Core.CreateRequest(Dest);
    try
      CheckEquals('FOO',
                  Request.LastHop.Transport,
                  'foo transport not specified');
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateResponseToTagMissing;
var
  Response: TIdSipResponse;
begin
  // This culls the parameters
  Self.Invite.ToHeader.Value := Self.Invite.ToHeader.Value;

  Response := Self.Core.CreateResponse(Self.Invite, SIPOK);
  try
    Check(Response.ToHeader.HasTag,
          'To is missing a tag');

    CheckEquals(Response.ToHeader.Address,
                Self.Invite.ToHeader.Address,
                'To header address mismatch');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateResponseUserAgent;
var
  Response: TIdSipResponse;
begin
  Self.Core.UserAgentName := 'SATAN/1.0';
  Self.Invite.RequestUri.URI := 'sip:wintermute@tessier-ashpool.co.luna';

  Response := Self.Core.CreateResponse(Self.Invite, SIPOK);
  try
    CheckEquals(Self.Core.UserAgentName,
                Response.FirstHeader(ServerHeader).Value,
                'User-Agent header not set');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateResponseUserAgentBlank;
var
  Response: TIdSipResponse;
begin
  Self.Core.UserAgentName := '';
  Self.Invite.RequestUri.URI := 'sip:wintermute@tessier-ashpool.co.luna';

  Response := Self.Core.CreateResponse(Self.Invite, SIPOK);
  try
    Check(not Response.HasHeader(UserAgentHeader),
          'User-Agent header not removed because it''s blank');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestDeclinedCallNotifiesListeners;
var
  O: TIdObserverListener;
begin
  Self.Core.Call(Self.Destination, '', '').Send;

  O := TIdObserverListener.Create;
  try
    Self.Core.AddObserver(O);

    Self.ReceiveResponse(SIPDecline);

    Check(O.Changed, 'Clearing up a terminated action should notify observers');
  finally
    Self.Core.RemoveObserver(O);
    O.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestDialogLocalSequenceNoMonotonicallyIncreases;
var
  BaseSeqNo: Cardinal;
  R:         TIdSipRequest;
begin
  R := Self.Core.CreateRequest(Self.Dlg);
  try
     BaseSeqNo := R.CSeq.SequenceNo;
  finally
    R.Free;
  end;

  R := Self.Core.CreateRequest(Self.Dlg);
  try
    CheckEquals(BaseSeqNo + 1,
                R.CSeq.SequenceNo,
                'Not monotonically increasing by one');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestDispatchToCorrectSession;
var
  SessionOne: TIdSipInboundSession;
  SessionTwo: TIdSipInboundSession;
begin
  // 1. Receive two inbound sessions.
  // 2. Receive a BYE for one of them.
  // 3. Check that the correct session died, and the other didn't.

  Self.ReceiveInvite;
  Check(Assigned(Self.Session),
        'OnInboundCall didn''t fire');
  SessionOne := Self.Session;

  Self.Invite.LastHop.Branch := Self.Invite.LastHop.Branch + '1';
  Self.Invite.From.Tag       := Self.Invite.From.Tag + '1';
  Self.Invite.ToHeader.Tag   := Self.Invite.ToHeader.Tag + '1';
  Self.ReceiveInvite;
  Check(Self.Session <> SessionOne,
        'OnInboundCall didn''t fire a second time');
  SessionTwo := Self.Session;
  CheckEquals(2,
              Self.Core.SessionCount,
              'Number of sessions after two INVITEs');


  SessionTwo.AcceptCall('', '');
  Check(SessionTwo.DialogEstablished, 'SessionTwo''s dialog wasn''t established');

  SessionTwo.AddSessionListener(Self);
  Self.ThreadEvent.ResetEvent;
  Self.ExceptionMessage := 'SessionTwo wasn''t terminated';
  Self.ReceiveBye(SessionTwo.Dialog);

  Check(not SessionOne.IsTerminated, 'SessionOne was terminated');
  CheckEquals(1,
              Self.Core.SessionCount,
              'Number of sessions after one BYE');
end;

procedure TestTIdSipUserAgent.TestDoNotDisturb;
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

procedure TestTIdSipUserAgent.TestDontReAuthenticate;
begin
  Self.TryAgain := false;

  Self.Core.Call(Self.Destination, '', '').Send;

  Self.MarkSentRequestCount;
  Self.ReceiveUnauthorized(ProxyAuthenticateHeader, QopAuthInt);

  CheckNoRequestSent('Reattempted authentication');
end;

procedure TestTIdSipUserAgent.TestHasUnknownAccept;
begin
  Self.Invite.RemoveHeader(Self.Invite.FirstHeader(AcceptHeader));

  Check(not Self.Core.HasUnknownAccept(Self.Invite),
        'Vacuously true');

  Self.Invite.AddHeader(AcceptHeader).Value := SdpMimeType;
  Check(not Self.Core.HasUnknownAccept(Self.Invite),
        SdpMimeType + ' MUST supported');

  Self.Invite.RemoveHeader(Self.Invite.FirstHeader(AcceptHeader));
  Self.Invite.AddHeader(AcceptHeader);
  Check(Self.Core.HasUnknownAccept(Self.Invite),
        'Nothing else is supported');
end;

procedure TestTIdSipUserAgent.TestHasUnknownContentEncoding;
begin
  Self.Invite.Headers.Remove(Self.Invite.FirstHeader(ContentEncodingHeaderFull));

  Check(not Self.Core.HasUnknownContentEncoding(Self.Invite),
        'Vacuously true');

  Self.Invite.AddHeader(ContentEncodingHeaderFull);
  Check(Self.Core.HasUnknownContentEncoding(Self.Invite),
        'No encodings are supported');
end;

procedure TestTIdSipUserAgent.TestHasUnknownContentType;
begin
  Self.Invite.RemoveHeader(Self.Invite.FirstHeader(ContentTypeHeaderFull));

  Check(not Self.Core.HasUnknownContentType(Self.Invite),
        'Vacuously true');

  Self.Invite.AddHeader(ContentTypeHeaderFull).Value := SdpMimeType;
  Check(not Self.Core.HasUnknownContentType(Self.Invite),
        SdpMimeType + ' MUST supported');

  Self.Invite.RemoveHeader(Self.Invite.FirstHeader(ContentTypeHeaderFull));
  Self.Invite.AddHeader(ContentTypeHeaderFull);
  Check(Self.Core.HasUnknownContentType(Self.Invite),
        'Nothing else is supported');
end;

procedure TestTIdSipUserAgent.TestInboundCall;
begin
  Self.Invite.Body          := TIdSipTestResources.BasicSDP('foo.com');
  Self.Invite.ContentLength := Length(Self.Invite.Body);
  Self.Invite.ContentType   := SdpMimeType;

  Self.ReceiveInvite;

  CheckEquals(Self.Invite.Body,
              Self.InboundCallOffer,
              'Offer');
  CheckEquals(Self.Invite.ContentType,
              Self.InboundCallMimeType,
              'Offer MIME type');
  Check(Self.Core = Self.UserAgentParam,
        'UserAgent param of Session''s InboundCall notification wrong');
end;

procedure TestTIdSipUserAgent.TestInviteExpires;
begin
  Self.Core.AddObserver(Self);

  Self.MarkSentResponseCount;

  Self.Invite.FirstExpires.NumericValue := 50;
  Self.ReceiveInvite;

  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');

  Self.DebugTimer.TriggerEarliestEvent;

  CheckResponseSent('No response sent');
  CheckEquals(SIPRequestTerminated,
              Self.LastSentResponse.StatusCode,
              'Unexpected response sent');

  CheckEquals(0, Self.Core.SessionCount, 'Expired session not cleaned up');
end;

procedure TestTIdSipUserAgent.TestInviteRaceCondition;
begin
  CheckEquals(0,
              Self.Core.InviteCount,
              'Sanity check - new test should have no ongoing INVITE actions');

  Self.MarkSentResponseCount;
  Self.ReceiveInvite;
  CheckEquals(1,
              Self.Core.InviteCount,
              'First INVITE didn''t make a new INVITE action');

  CheckResponseSent('No response sent');

  Self.ReceiveInvite;
  CheckEquals(1,
              Self.Core.InviteCount,
              'INVITE resend made a new INVITE action');
end;

procedure TestTIdSipUserAgent.TestIsMethodAllowed;
begin
  Check(not Self.Core.IsMethodAllowed(MethodRegister),
        MethodRegister + ' not allowed');

  Self.Core.AddModule(TIdSipRegisterModule);
  Check(Self.Core.IsMethodAllowed(MethodRegister),
        MethodRegister + ' not recognised as an allowed method');

  Check(not Self.Core.IsMethodAllowed(' '),
        ''' '' recognised as an allowed method');
end;

procedure TestTIdSipUserAgent.TestIsSchemeAllowed;
begin
  Check(not Self.Core.IsMethodAllowed(SipScheme),
        SipScheme + ' not allowed');

  Self.Core.AddAllowedScheme(SipScheme);
  Check(Self.Core.IsSchemeAllowed(SipScheme),
        SipScheme + ' not recognised as an allowed scheme');

  Check(not Self.Core.IsSchemeAllowed(' '),
        ''' '' not recognised as an allowed scheme');
end;

procedure TestTIdSipUserAgent.TestLoopDetection;
var
  Response: TIdSipResponse;
begin
  // cf. RFC 3261, section 8.2.2.2
  Self.Dispatcher.AddServerTransaction(Self.Invite, Self.Dispatcher.Transport);

  // wipe out the tag & give a different branch
  Self.Invite.ToHeader.Value := Self.Invite.ToHeader.Address.URI;
  Self.Invite.LastHop.Branch := Self.Invite.LastHop.Branch + '1';

  Self.MarkSentResponseCount;

  Self.ReceiveInvite;
  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPLoopDetected, Response.StatusCode, 'Status-Code');
end;

procedure TestTIdSipUserAgent.TestMergedRequest;
var
  FirstInvite:  TIdSipRequest;
  SecondInvite: TIdSipRequest;
begin
  FirstInvite := TIdSipRequest.ReadRequestFrom(SFTFInvite);
  try
    SecondInvite := TIdSipRequest.ReadRequestFrom(SFTFMergedInvite);
    try
      Self.ReceiveRequest(FirstInvite);
      Self.MarkSentResponseCount;
      Self.ReceiveRequest(SecondInvite);

      CheckResponseSent('No response sent');

      Check(SecondInvite.Match(Self.LastSentResponse),
            'Response not for 2nd INVITE');
      CheckEquals(SIPLoopDetected,
                  Self.LastSentResponse.StatusCode,
                  'Unexpected response');
    finally
      SecondInvite.Free;
    end;
  finally
    FirstInvite.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestModuleForString;
begin
  CheckNull(Self.Core.ModuleFor(''),
            'Empty string');
  CheckNull(Self.Core.ModuleFor(MethodRegister),
            MethodRegister + ' but no module added');

  Self.Core.AddModule(TIdSipRegisterModule);
  CheckNotNull(Self.Core.ModuleFor(MethodRegister),
               MethodRegister + ' but no module added');
  CheckEquals(TIdSipRegisterModule.ClassName,
              Self.Core.ModuleFor(MethodRegister).ClassName,
              MethodRegister + ' after module added: wrong type');
  CheckNull(Self.Core.ModuleFor(Lowercase(MethodRegister)),
            Lowercase(MethodRegister)
          + ': RFC 3261 defines REGISTER''s method as "REGISTER"');
end;

procedure TestTIdSipUserAgent.TestNotificationOfNewSession;
begin
  Self.ReceiveInvite;

  Check(Self.OnInboundCallFired, 'UI not notified of new session');
end;

procedure TestTIdSipUserAgent.TestNotificationOfNewSessionRobust;
var
  L1, L2: TIdSipTestUserAgentListener;
begin
  L1 := TIdSipTestUserAgentListener.Create;
  try
    L2 := TIdSipTestUserAgentListener.Create;
    try
      L1.FailWith := EParserError;

      Self.Core.AddUserAgentListener(L1);
      Self.Core.AddUserAgentListener(L2);

      Self.ReceiveInvite;

      Check(L2.InboundCall, 'L2 not notified');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestOutboundCallAndByeToXlite;
var
  Session: TIdSipSession;
begin
  Session := Self.Core.Call(Self.Destination, '', '');
  Session.AddSessionListener(Self);
  Session.Send;

  Self.ReceiveTrying(Self.LastSentRequest);
  Check(not Session.DialogEstablished,
        Self.LastSentResponse.Description
      + 's don''t make dialogs');

  Self.ReceiveRinging(Self.LastSentRequest);
  Check(Session.DialogEstablished,
        Self.LastSentResponse.Description
      + 's with To tags make dialogs');
  Check(Session.IsEarly,
        Self.LastSentResponse.Description
      + 's make early dialogs');

  Self.MarkSentAckCount;
  Self.ReceiveOk(Self.LastSentRequest);
  CheckAckSent('No ACK sent: ' + Self.FailReason);
  Check(not Session.IsEarly,
        Self.LastSentResponse.Description
      + 's make non-early dialogs');

  Self.ReceiveOk(Self.LastSentRequest);
  Self.ReceiveOk(Self.LastSentRequest);
  Self.ReceiveOk(Self.LastSentRequest);

  Self.Core.TerminateAllCalls;
  Check(Self.LastSentRequest.IsBye,
        'Must send a BYE to terminate an established session');
end;

procedure TestTIdSipUserAgent.TestOutboundInviteSessionProgressResends;
begin
  Self.MarkSentResponseCount;

  // Receive an INVITE. Ring. Wait.
  Self.Core.ProgressResendInterval := 50;

  Self.ReceiveInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');

  Self.DebugTimer.TriggerEarliestEvent;

  CheckResponseSent('No response sent');
  CheckEquals(SIPSessionProgress,
              Self.LastSentResponse.StatusCode,
              'Wrong response');
end;

procedure TestTIdSipUserAgent.TestOutboundInviteTerminatesWhenNoResponse;
begin
  Self.Core.Call(Self.Destination, '', '').Send;
  CheckEquals(1, Self.Core.InviteCount, 'Calling makes an INVITE');

  Self.DebugTimer.TriggerEarliestEvent;
  CheckEquals(0, Self.Core.InviteCount, 'If we never get a response then we give up');
end;

procedure TestTIdSipUserAgent.TestReceiveByeForUnmatchedDialog;
var
  Bye:      TIdSipRequest;
  Response: TIdSipResponse;
begin
  Bye := Self.Core.CreateRequest(Self.Destination);
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

procedure TestTIdSipUserAgent.TestReceiveByeForDialog;
var
  Response: TIdSipResponse;
begin
  Self.ReceiveInvite;

  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
  Self.Session.AcceptCall('', '');
  Self.ReceiveAck;

  Self.MarkSentResponseCount;
  Self.ReceiveBye(Self.Session.Dialog);

  CheckResponseSent('SOMETHING should have sent a response');

  Response := Self.LastSentResponse;
  CheckNotEquals(SIPCallLegOrTransactionDoesNotExist,
                 Response.StatusCode,
                 'UA tells us no matching dialog was found');
end;

procedure TestTIdSipUserAgent.TestReceiveByeDestroysTerminatedSession;
var
  O: TIdObserverListener;
begin
  O := TIdObserverListener.Create;
  try
    Self.ReceiveInvite;
    Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
    Self.Session.AcceptCall('', '');

    Self.Core.AddObserver(O);

    Self.ReceiveBye(Self.Session.Dialog);

    CheckEquals(0, Self.Core.SessionCount, 'Number of sessions after BYE');
    Check(O.Changed, 'Observer not notified after session ended');
  finally
    Self.Core.RemoveObserver(O);
    O.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestReceiveByeWithoutTags;
var
  Bye:      TIdSipRequest;
  Response: TIdSipResponse;
begin
  Bye := Self.Core.CreateRequest(Self.Destination);
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

procedure TestTIdSipUserAgent.TestReceiveOptions;
var
  Options:  TIdSipRequest;
  Response: TIdSipResponse;
begin
  Options := TIdSipRequest.Create;
  try
    Options.Method := MethodOptions;
    Options.RequestUri.Uri := 'sip:franks@192.168.0.254';
    Options.AddHeader(ViaHeaderFull).Value  := 'SIP/2.0/UDP roke.angband.za.org:3442';
    Options.From.Value := '<sip:sipsak@roke.angband.za.org:3442>';
    Options.ToHeader.Value := '<sip:franks@192.168.0.254>';
    Options.CallID := '1631106896@roke.angband.za.org';
    Options.CSeq.Value := '1 OPTIONS';
    Options.AddHeader(ContactHeaderFull).Value := '<sip:sipsak@roke.angband.za.org:3442>';
    Options.ContentLength := 0;
    Options.MaxForwards := 0;
    Options.AddHeader(UserAgentHeader).Value := 'sipsak v0.8.1';

    Self.Locator.AddA(Options.LastHop.SentBy, '127.0.0.1');

    Self.ReceiveRequest(Options);

    Response := Self.LastSentResponse;
    CheckEquals(SIPOK,
                Response.StatusCode,
                'We should accept all OPTIONS');
  finally
    Options.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestReceiveResponseWithMultipleVias;
var
  Response: TIdSipResponse;
begin
  Self.Core.Call(Self.Destination, '', '');

  Response := TIdSipResponse.InResponseTo(Self.Invite,
                                          SIPOK,
                                          Self.Core.Contact);
  try
    Response.AddHeader(Response.Path.LastHop);
    Self.ReceiveResponse(Response);
    Check(not Self.SessionEstablished,
          'Multiple-Via Response not dropped');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestRejectMalformedAuthorizedRequest;
var
  Auth:     TIdSipMockAuthenticator;
  Response: TIdSipResponse;
begin
  Auth := TIdSipMockAuthenticator.Create;
  try
    Self.Core.RequireAuthentication := true;
    Self.Core.Authenticator := Auth;
    Auth.FailWith := EAuthenticate;

    Self.MarkSentResponseCount;

    Self.Invite.AddHeader(AuthorizationHeader);
    Self.ReceiveInvite;
    CheckResponseSent('No response sent');

    Response := Self.LastSentResponse;
    CheckEquals(SIPBadRequest,
                Response.StatusCode,
                'Status code');
  finally
    Auth.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestRejectNoContact;
var
  Response: TIdSipResponse;
begin
  Self.Invite.RemoveHeader(Self.Invite.FirstContact);

  Self.MarkSentResponseCount;

  Self.ReceiveInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPBadRequest,        Response.StatusCode, 'Status-Code');
  CheckEquals(MissingContactHeader, Response.StatusText, 'Status-Text');
end;

procedure TestTIdSipUserAgent.TestRejectUnauthorizedRequest;
var
  Response: TIdSipResponse;
begin
  Self.Core.RequireAuthentication := true;

  Self.MarkSentResponseCount;
  Self.ReceiveInvite;
  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPUnauthorized,
              Response.StatusCode,
              'Status code');
  Check(Response.HasWWWAuthenticate,
        'No WWW-Authenticate header');
end;

procedure TestTIdSipUserAgent.TestRejectUnknownContentEncoding;
var
  Response: TIdSipResponse;
begin
  Self.Invite.FirstHeader(ContentTypeHeaderFull).Value := SdpMimeType;

  Self.MarkSentResponseCount;

  Self.Invite.AddHeader(ContentEncodingHeaderFull).Value := 'gzip';

  Self.ReceiveInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPUnsupportedMediaType, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(AcceptEncodingHeader), 'No Accept-Encoding header');
  CheckEquals('',
              Response.FirstHeader(AcceptEncodingHeader).Value,
              'Accept value');
end;

procedure TestTIdSipUserAgent.TestRejectUnknownContentLanguage;
var
  Response: TIdSipResponse;
begin
  Self.Core.AddAllowedLanguage('fr');

  Self.Invite.AddHeader(ContentLanguageHeader).Value := 'en_GB';

  Self.MarkSentResponseCount;

  Self.ReceiveInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPUnsupportedMediaType, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(AcceptLanguageHeader), 'No Accept-Language header');
  CheckEquals(Self.Core.AllowedLanguages,
              Response.FirstHeader(AcceptLanguageHeader).Value,
              'Accept-Language value');
end;

procedure TestTIdSipUserAgent.TestRejectUnknownContentType;
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
  CheckEquals(SdpMimeType,
              Response.FirstHeader(AcceptHeader).Value,
              'Accept value');
end;

procedure TestTIdSipUserAgent.TestRejectUnknownExtension;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;

  Self.Invite.AddHeader(RequireHeader).Value := '100rel';

  Self.ReceiveInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPBadExtension, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(UnsupportedHeader), 'No Unsupported header');
  CheckEquals(Self.Invite.FirstHeader(RequireHeader).Value,
              Response.FirstHeader(UnsupportedHeader).Value,
              'Unexpected Unsupported header value');
end;

procedure TestTIdSipUserAgent.TestRejectUnknownScheme;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;

  Self.Invite.RequestUri.URI := 'tel://1';
  Self.ReceiveInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPUnsupportedURIScheme, Response.StatusCode, 'Status-Code');
end;

procedure TestTIdSipUserAgent.TestRejectUnsupportedMethod;
var
  Response: TIdSipResponse;
begin
  Self.Invite.Method := MethodRegister;
  Self.Invite.CSeq.Method := Self.Invite.Method;

  Self.MarkSentResponseCount;

  Self.ReceiveInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  Check(Response.HasHeader(AllowHeader),
        'Allow header is mandatory. cf. RFC 3261 section 8.2.1');

  CheckCommaSeparatedHeaders(Self.Core.AllowedMethods,
                             Response.FirstHeader(AllowHeader),
                             'Allow header');
end;

procedure TestTIdSipUserAgent.TestRejectUnsupportedSipVersion;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;
  Self.Invite.SIPVersion := 'SIP/1.0';

  Self.ReceiveInvite;

  CheckEquals(Self.ResponseCount + 2, // Trying + reject
              Self.SentResponseCount,
              'No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPSIPVersionNotSupported,
              Response.StatusCode,
              'Status-Code');
end;

procedure TestTIdSipUserAgent.TestRemoveObserver;
var
  L1, L2: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    L2 := TIdObserverListener.Create;
    try
      Self.Core.AddObserver(L1);
      Self.Core.AddObserver(L2);
      Self.Core.RemoveObserver(L2);

      Self.ReceiveInvite;

      Check(L1.Changed and not L2.Changed,
            'Listener notified, hence not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestRemoveUserAgentListener;
var
  L1, L2: TIdSipTestUserAgentListener;
begin
  L1 := TIdSipTestUserAgentListener.Create;
  try
    L2 := TIdSipTestUserAgentListener.Create;
    try
      Self.Core.AddUserAgentListener(L1);
      Self.Core.AddUserAgentListener(L2);
      Self.Core.RemoveUserAgentListener(L2);

      Self.ReceiveInvite;

      Check(L1.InboundCall and not L2.InboundCall,
            'Listener notified, hence not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestReregister;
var
  Event: TIdSipMessageNotifyEventWait;
begin
  Self.Invite.Method := MethodRegister;

  Self.MarkSentRequestCount;

  Event := TIdSipMessageNotifyEventWait.Create;
  try
    Event.Message := Self.Invite.Copy;
    Self.Core.OnReregister(Event);
  finally
    Event.Free;
  end;

  Self.CheckRequestSent('No request resend');
  CheckEquals(MethodRegister,
              Self.LastSentRequest.Method,
              'Unexpected method in resent request');
end;

procedure TestTIdSipUserAgent.TestRFC2543InviteCallFlow;
const
  RawSippInvite = 'INVITE sip:service@80.168.137.82:5060 SIP/2.0'#13#10
                + 'Via: SIP/2.0/UDP 81.86.64.25:5060'#13#10
                + 'From: sipp <sip:sipp@81.86.64.25:5060>;tag=1'#13#10
                + 'To: sut <sip:service@80.168.137.82:5060>'#13#10
                + 'Call-ID: 1.87901.81.86.64.25@sipp.call.id'#13#10
                + 'CSeq: 1 INVITE'#13#10
                + 'Contact: sip:sipp@81.86.64.25:5060'#13#10
                + 'Max-Forwards: 70'#13#10
                + 'Subject: Performance Test'#13#10
                + 'Content-Length: 0'#13#10#13#10;
  RawSippAck = 'ACK sip:service@80.168.137.82:5060 SIP/2.0'#13#10
             + 'Via: SIP/2.0/UDP 81.86.64.25'#13#10
             + 'From: sipp <sip:sipp@81.86.64.25:5060>;tag=1'#13#10
             + 'To: sut <sip:service@80.168.137.82:5060>;tag=%s'#13#10
             + 'Call-ID: 1.87901.81.86.64.25@sipp.call.id'#13#10
             + 'CSeq: 1 ACK'#13#10
             + 'Contact: sip:sipp@81.86.64.25:5060'#13#10
             + 'Max-Forwards: 70'#13#10
             + 'Subject: Performance Test'#13#10
             + 'Content-Length: 0'#13#10#13#10;
  RawSippBye = 'BYE sip:service@80.168.137.82:5060 SIP/2.0'#13#10
             + 'Via: SIP/2.0/UDP 81.86.64.25'#13#10
             + 'From: sipp <sip:sipp@81.86.64.25:5060>;tag=1'#13#10
             + 'To: sut <sip:service@80.168.137.82:5060>;tag=%s'#13#10
             + 'Call-ID: 1.87901.81.86.64.25@sipp.call.id'#13#10
             + 'CSeq: 2 BYE'#13#10
             + 'Contact: sip:sipp@81.86.64.25:5060'#13#10
             + 'Max-Forwards: 70'#13#10
             + 'Subject: Performance Test'#13#10
             + 'Content-Length: 0'#13#10#13#10;
var
  SippAck:    TIdSipRequest;
  SippBye:    TIdSipRequest;
  SippInvite: TIdSipRequest;
begin
  // SIPp is a SIP testing tool: http://sipp.sourceforge.net/

  Self.Dispatcher.Transport.WriteLog := true;

  SippInvite := TIdSipRequest.ReadRequestFrom(RawSippInvite);
  try
    Self.MarkSentResponseCount;
    Self.ReceiveRequest(SippInvite);
    Check(Assigned(Self.Session),
          'OnInboundCall didn''t fire');
    Self.Session.AcceptCall('', '');

    SippAck := TIdSipRequest.ReadRequestFrom(Format(RawSippAck,
                                                    [Self.Session.Dialog.ID.LocalTag]));
    try
      Self.ReceiveRequest(SippAck);
    finally
      SippAck.Free;
    end;

    Self.MarkSentResponseCount;

    SippBye := TIdSipRequest.ReadRequestFrom(Format(RawSippBye,
                                                    [Self.Session.Dialog.ID.LocalTag]));
    try
      Self.ReceiveRequest(SippBye);
    finally
      SippBye.Free;
    end;

    CheckResponseSent('No response sent for the BYE');

    CheckEquals(SIPOK,
                Self.LastSentResponse.StatusCode,
                'Unexpected response');
  finally
    SippInvite.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestScheduleEventActionClosure;
var
  DebugTimer: TIdDebugTimerQueue;
  EventCount: Integer;
begin
  DebugTimer := TIdDebugTimerQueue.Create;
  try
    Self.Core.Timer := DebugTimer;

    EventCount := DebugTimer.EventCount;
    Self.Core.ScheduleEvent(TIdSipInboundInviteExpire, 50, Self.Invite.Copy);
    Check(EventCount < DebugTimer.EventCount,
          'Event not scheduled');
  finally
    Self.Core.Timer := nil;
    DebugTimer.Terminate;
  end;
end;

procedure TestTIdSipUserAgent.TestSetContact;
var
  C: TIdSipContactHeader;
begin
  C := TIdSipContactHeader.Create;
  try
    C.Value := 'sip:case@fried.neurons.org';
    Self.Core.Contact := C;

    Check(Self.Core.Contact.Equals(C),
                'Contact not set');
  finally
    C.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestSetContactMailTo;
var
  C: TIdSipContactHeader;
begin
  C := TIdSipContactHeader.Create;
  try
    try
      C.Value := 'mailto:wintermute@tessier-ashpool.co.luna';
      Self.Core.Contact := C;
      Fail('Only a SIP or SIPs URI may be specified');
    except
      on EBadHeader do;
    end;
  finally
    C.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestSetContactWildCard;
var
  C: TIdSipContactHeader;
begin
  C := TIdSipContactHeader.Create;
  try
    try
      C.Value := '*';
      Self.Core.Contact := C;
      Fail('Wildcard Contact headers make no sense in a response that sets up '
         + 'a dialog');
    except
      on EBadHeader do;
      on EAssertionFailed do;
    end;
  finally
    C.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestSetFrom;
var
  F: TIdSipFromHeader;
begin
  F := TIdSipFromHeader.Create;
  try
    F.Value := 'sip:case@fried.neurons.org';
    Self.Core.From := F;

    Check(Self.Core.From.Equals(F),
          'From not set');
  finally
    F.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestSetFromMailTo;
var
  F: TIdSipFromHeader;
begin
  F := TIdSipFromHeader.Create;
  try
    try
      F.Value := 'mailto:wintermute@tessier-ashpool.co.luna';
      Self.Core.From := F;
      Fail('Only a SIP or SIPs URI may be specified');
    except
      on EBadHeader do;
    end;
  finally
    F.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestSimultaneousInAndOutboundCall;
begin
  Self.Core.Call(Self.Destination, '', '').Send;
  Self.ReceiveTrying(Self.LastSentRequest);
  Self.ReceiveRinging(Self.LastSentRequest);

  Self.ReceiveInvite;
  Check(Assigned(Self.Session), 'TU not informed of inbound call');

  Self.Session.AcceptCall('', '');
  CheckEquals(2, Self.Core.SessionCount, 'Session count');
end;

procedure TestTIdSipUserAgent.TestTerminateAllCalls;
var
  InboundSession: TIdSipInboundSession;
  Sess:           TIdSipSession;
begin
  // We have:
  // * an established inbound call;
  // * an unestablished inbound call;
  // * an unestablished outbound call;
  // * an established outbound call.
  // When we terminate everything, we expect only the unestablished outbound
  // call to remain, because it can only terminate according to RFC 3261 section 9.1

  Self.ReceiveInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire, first INVITE');
  InboundSession := Self.Session;
  InboundSession.AddSessionListener(Self);
  InboundSession.AcceptCall('', '');
  Self.ReceiveAck;

  Self.Invite.LastHop.Branch := Self.Invite.LastHop.Branch + '1';
  Self.Invite.From.Tag       := Self.Invite.From.Tag + '1';
  Self.ReceiveInvite;

  Sess := Self.Core.Call(Self.Destination, '', '');
  Sess.AddSessionListener(Self);
  Sess.Send;
  Self.ReceiveTrying(Self.LastSentRequest);

  Sess := Self.Core.Call(Self.Destination, '', '');
  Sess.AddSessionListener(Self);
  Sess.Send;
  Self.ReceiveOk(Self.LastSentRequest);

  CheckEquals(4,
              Self.Core.SessionCount,
              'Session count');

  Self.Core.TerminateAllCalls;

  // This looks completely wrong, I know. However, we've sent a CANCEL to
  // terminate the not-yet-accepted INVITE we sent out with Call(). That
  // session won't end until we receive a 487 Request Terminated for that INVITE
  // or we receive a 200 OK (in which case we send a BYE and immediately tear
  // down the session), or we time out (because the remote end was an RFC 2543
  // UAS). cf RFC 3261 section 9.1
  CheckEquals(1,
              Self.Core.SessionCount,
              'Session count after TerminateAllCalls');
end;

procedure TestTIdSipUserAgent.TestUnknownAcceptValue;
begin
  Self.Invite.AddHeader(AcceptHeader).Value := 'text/unsupportedtextvalue';

  Self.MarkSentResponseCount;
  Self.ReceiveInvite;

  Self.CheckResponseSent('No response sent to INVITE');
  CheckEquals(SIPNotAcceptableClient,
              Self.LastSentResponse.StatusCode,
              'Inappropriate response');
  Check(Self.LastSentResponse.HasHeader(AcceptHeader),
        'Response missing Accept header');
  CheckEquals(Self.Core.AllowedContentTypes,
              Self.LastSentResponse.FirstHeader(AcceptHeader).Value,
              'Incorrect Accept header');
end;

procedure TestTIdSipUserAgent.TestUnmatchedAckGetsDropped;
var
  Ack:      TIdSipRequest;
  Listener: TIdSipTestUserAgentListener;
begin
  Listener := TIdSipTestUserAgentListener.Create;
  try
    Self.Core.AddUserAgentListener(Listener);

    Self.MarkSentResponseCount;
    Ack := TIdSipRequest.Create;
    try
      Ack.Assign(Self.Invite);
      Ack.Method      := MethodAck;
      Ack.CSeq.Method := Ack.Method;

      Self.ReceiveRequest(Ack);
    finally
      Ack.Free;
    end;

    Check(Listener.DroppedUnmatchedMessage,
          'Unmatched ACK not dropped');
    Check(Listener.UserAgentParam = Self.Core,
          'UserAgent param of Session''s DroppedUnmatchedMessage notification wrong');
    CheckNoResponseSent('Sent a response to an unmatched ACK');
  finally
    Self.Core.RemoveUserAgentListener(Listener);
    Listener.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestViaMatchesTransportParameter;
begin
  // Iterate over the registered transports? Or does
  // TIdSipTransport.TransportFor return the null transport instead?

  Self.Dispatcher.TransportType := UdpTransport;
  Self.Destination.Address.Transport := Self.Dispatcher.Transport.GetTransportType;
  Self.Core.Call(Self.Destination, '', '').Send;

  CheckEquals(Self.Dispatcher.Transport.GetTransportType,
              Self.LastSentRequest.LastHop.Transport,
              'Transport parameter = '
            + Self.Destination.Address.Transport);

  Self.Dispatcher.TransportType := TlsTransport;
  Self.Destination.Address.Transport := Self.Dispatcher.Transport.GetTransportType;
  Self.Core.Call(Self.Destination, '', '').Send;

  CheckEquals(Self.Dispatcher.Transport.GetTransportType,
              Self.LastSentRequest.LastHop.Transport,
              'Transport parameter = '
            + Self.Destination.Address.Transport);
end;

//******************************************************************************
//* TestTIdSipAction                                                           *
//******************************************************************************
//* TestTIdSipAction Public methods ********************************************

procedure TestTIdSipAction.SetUp;
begin
  inherited SetUp;

  Self.ActionFailed := false;
end;

//* TestTIdSipAction Protected methods *****************************************

function TestTIdSipAction.CreateAction: TIdSipAction;
begin
  raise Exception.Create(Self.ClassName
                       + ': Don''t call CreateAction on an inbound Action');
end;

procedure TestTIdSipAction.OnNetworkFailure(Action: TIdSipAction;
                                            const Reason: String);
begin
  Self.FailReason  := Reason;
  Self.ActionParam := Action;
end;

procedure TestTIdSipAction.ReceiveBadExtensionResponse;
begin
  Self.ReceiveResponse(SIPBadExtension);
end;

procedure TestTIdSipAction.ReceiveOkWithBody(Invite: TIdSipRequest;
                                             const Body: String;
                                             const ContentType: String);
var
  Ok: TIdSipResponse;
begin
  Ok := Self.CreateRemoteOk(Invite);
  try
    Ok.Body                        := Body;
    Ok.ContentDisposition.Handling := DispositionSession;
    Ok.ContentLength               := Length(Body);
    Ok.ContentType                 := ContentType;

    Self.ReceiveResponse(Ok);
  finally
    Ok.Free;
  end;
end;

//* TestTIdSipAction Published methods *****************************************

procedure TestTIdSipAction.TestIsInbound;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(not Action.IsInbound,
        Action.ClassName + ' marked as an inbound action');
end;

procedure TestTIdSipAction.TestIsInvite;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(not Action.IsInvite,
        Action.ClassName + ' marked as an Invite');
end;

procedure TestTIdSipAction.TestIsOptions;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(not Action.IsOptions,
        Action.ClassName + ' marked as an Options');
end;

procedure TestTIdSipAction.TestIsRegistration;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(not Action.IsRegistration,
        Action.ClassName + ' marked as a Registration');
end;

procedure TestTIdSipAction.TestIsSession;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(not Action.IsSession,
        Action.ClassName + ' marked as a Session');
end;
{
procedure TestTIdSipAction.TestReceiveResponseBadExtension;
var
  Action:          TIdSipAction;
  ActionClassname: String;
begin
  // CreateAction creates an Action owned by Self.Core. When we free Self.Core
  // then it'll free Action.
  Action          := Self.CreateAction;
  ActionClassname := Action.ClassName;
  Self.ReceiveBadExtensionResponse;

  Self.MarkSentRequestCount;

  CheckRequestSent(ActionClassname + ' request wasn''t reissued');
  Check(not Self.LastSentRequest.HasHeader(RequireHeader),
        'Require header still in 2nd attempt');
end;

procedure TestTIdSipAction.TestReceiveResponseBadExtensionWithoutRequires;
var
  Action:          TIdSipAction;
  ActionClassname: String;
begin

  // If we send a request that has no Requires header, but get a 420 Bad
  // Extension back (which can only come from a bad SIP implementation on the
  // remote end), then we must report a failure.

  // CreateAction creates an Action owned by Self.Core. When we free Self.Core
  // then it'll free Action.
  Action          := Self.CreateAction;
  ActionClassname := Action.ClassName;

  Self.ReceiveBadExtensionResponse;
  Check(Self.ActionFailed, ActionClassName + ' failure not reported');
end;
}
//******************************************************************************
//* TestLocation                                                               *
//******************************************************************************
//* TestLocation Public methods ************************************************

procedure TestLocation.SetUp;
begin
  inherited SetUp;

  Self.InviteMimeType := '';
  Self.InviteOffer    := '';
  Self.NetworkFailure := false;
  Self.TransportParam := SctpTransport;
end;

//* TestLocation Private methods ***********************************************

function TestLocation.CreateAction: TIdSipOutboundInitialInvite;
begin
  Result := Self.Core.AddOutboundAction(TIdSipOutboundInitialInvite) as TIdSipOutboundInitialInvite;
  Result.Destination := Self.Destination;
  Result.MimeType    := Self.InviteMimeType;
  Result.Offer       := Self.InviteOffer;
  Result.AddListener(Self);
  Result.Send;
end;

procedure TestLocation.OnFailure(InviteAgent: TIdSipOutboundInvite;
                                 Response: TIdSipResponse;
                                 const Reason: String);
begin
end;

procedure TestLocation.OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                           NewDialog: TIdSipDialog);
begin
end;

procedure TestLocation.OnNetworkFailure(Action: TIdSipAction;
                                        const Reason: String);
begin
  Self.NetworkFailure := true;
end;

procedure TestLocation.OnRedirect(InviteAgent: TIdSipOutboundInvite;
                                  Redirect: TIdSipResponse);
begin
end;

procedure TestLocation.OnSuccess(InviteAgent: TIdSipOutboundInvite;
                                 Response: TIdSipResponse);
begin
end;

//* TestLocation Published methods *********************************************

procedure TestLocation.TestAllLocationsFail;
var
  Locations: TIdSipLocations;
begin
  // SRV records point to Self.Destination.Address.Host;
  // Self.Destination.Address.Host resolves to two A records.

  Self.Locator.AddSRV(Self.Destination.Address.Host,
                      SrvTcpPrefix,
                      0,
                      0,
                      5060,
                      Self.Destination.Address.Host);
  Self.Locator.AddA   (Self.Destination.Address.Host, '127.0.0.1');
  Self.Locator.AddAAAA(Self.Destination.Address.Host, '::1');

  Self.Dispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.MarkSentRequestCount;
  Self.CreateAction;

  Locations := TIdSipLocations.Create;
  try
    Self.Locator.FindServersFor(Self.Destination.Address, Locations);

    // Locations.Count >= 0, so the typecast is safe.
    CheckEquals(Self.RequestCount + Cardinal(Locations.Count),
                Self.SentRequestCount,
                'Number of requests sent');
  finally
    Locations.Free;
  end;

  Check(Self.NetworkFailure,
        'No notification of failure after all locations attempted');
end;

procedure TestLocation.TestLooseRoutingProxy;
const
  ProxyAAAARecord = '::1';
  ProxyHost       = 'gw1.leo-ix.net';
  ProxyTransport  = SctpTransport;
  ProxyUri        = 'sip:' + ProxyHost + ';lr';
var
  RequestUriTransport: String;
begin
  RequestUriTransport := Self.Invite.LastHop.Transport;

  Self.Core.Proxy.Uri := ProxyUri;
  Self.Core.HasProxy  := true;

  Self.Locator.AddSRV(ProxyHost, SrvSctpPrefix, 0, 0, 5060, ProxyHost);
  Self.Locator.AddAAAA(ProxyHost, ProxyAAAARecord);

  Self.Locator.AddSRV(Self.Destination.Address.Host, SrvTcpPrefix, 0, 0,
                      5060, Self.Destination.Address.Host);

  Self.Locator.AddA(Self.Destination.Address.Host, '127.0.0.1');

  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No request sent');

  CheckEquals(ProxyTransport,
              Self.LastSentRequest.LastHop.Transport,
              'Wrong transport means UA gave Locator wrong URI');
end;

procedure TestLocation.TestStrictRoutingProxy;
const
  ProxyUri = 'sip:127.0.0.1;transport=' + TransportParamSCTP;
var
  RequestUriTransport: String;
begin
  RequestUriTransport := Self.Invite.LastHop.Transport;

  Self.Core.Proxy.Uri := ProxyUri;
  Self.Core.HasProxy  := true;

  Self.Destination.Address.Transport := TransportParamTCP;

  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No request sent');

  CheckEquals(RequestUriTransport,
              Self.LastSentRequest.LastHop.Transport,
              'Wrong transport means UA gave Locator wrong URI');
end;

procedure TestLocation.TestUseCorrectTransport;
const
  CorrectTransport = SctpTransport;
var
  Action: TIdSipAction;
  Domain: String;
begin
  Domain := Self.Destination.Address.Host;

  // NAPTR record points to SCTP SRV record whose target resolves to the A
  // record.
  Self.Locator.AddNAPTR(Domain, 0, 0, NaptrDefaultFlags, NaptrSctpService, SrvSctpPrefix + Domain);
  Self.Locator.AddSRV(Domain, SrvSctpPrefix, 0, 0, 5060, Domain);
  Self.Locator.AddSRV(Domain, SrvTcpPrefix,  1, 0, 5060, Domain);

  Self.MarkSentRequestCount;
  Action := Self.CreateAction;

  CheckRequestSent('No request sent');
  CheckEquals(CorrectTransport,
              Self.LastSentRequest.LastHop.Transport,
              'Incorrect transport');
  Check(Self.LastSentRequest.Equals(Action.InitialRequest),
        'Action''s InitialRequest not updated to the latest attempt');
end;

procedure TestLocation.TestUseTransportParam;
begin
  Self.Destination.Address.Transport := Self.TransportParam;

  Self.MarkSentRequestCount;
  Self.CreateAction;
  Self.CheckRequestSent('No request sent');

  CheckEquals(SctpTransport,
              Self.LastSentRequest.LastHop.Transport,
              'INVITE didn''t use transport param');
end;

procedure TestLocation.TestUseUdpByDefault;
begin
  Self.MarkSentRequestCount;
  Self.CreateAction;
  Self.CheckRequestSent('No request sent');

  CheckEquals(UdpTransport,
              Self.LastSentRequest.LastHop.Transport,
              'INVITE didn''t use UDP by default');
end;

procedure TestLocation.TestVeryLargeMessagesUseAReliableTransport;
begin
  Self.InviteOffer    := TIdSipTestResources.VeryLargeSDP('localhost');
  Self.InviteMimeType := SdpMimeType;

  Self.MarkSentRequestCount;
  Self.CreateAction;
  Self.CheckRequestSent('No request sent');

  CheckEquals(TcpTransport,
              Self.LastSentRequest.LastHop.Transport,
              'INVITE didn''t use a reliable transport despite the large size '
            + 'of the message');
end;

//******************************************************************************
//* TestTIdSipSession                                                          *
//******************************************************************************
//* TestTIdSipSession Public methods *******************************************

procedure TestTIdSipSession.SetUp;
begin
  inherited SetUp;

  Self.MultiStreamSdp := Self.CreateMultiStreamSdp;
  Self.SimpleSdp      := Self.CreateSimpleSdp;

  Self.InboundModify             := nil;
  Self.OnEndedSessionFired       := false;
  Self.OnEstablishedSessionFired := false;
  Self.OnModifiedSessionFired    := false;
  Self.OnModifySessionFired      := false;
end;

procedure TestTIdSipSession.TearDown;
begin
  Self.SimpleSdp.Free;
  Self.MultiStreamSdp.Free;

  inherited TearDown;
end;

//* TestTIdSipSession Protected methods ****************************************

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

function TestTIdSipSession.CreateMultiStreamSdp: TIdSdpPayload;
var
  Connection: TIdSdpConnection;
  MD:         TIdSdpMediaDescription;
begin
  Result := TIdSdpPayload.Create;
  Result.Version                := 0;

  Result.Origin.Username        := 'wintermute';
  Result.Origin.SessionID       := '2890844526';
  Result.Origin.SessionVersion  := '2890842807';
  Result.Origin.NetType         := Id_SDP_IN;
  Result.Origin.AddressType     := Id_IPv4;
  Result.Origin.Address         := '127.0.0.1';

  Result.SessionName            := 'Minimum Session Info';

  Connection := Result.AddConnection;
  Connection.NetType     := Id_SDP_IN;
  Connection.AddressType := Id_IPv4;
  Connection.Address     := '127.0.0.1';

  MD := Result.AddMediaDescription;
  MD.MediaType := mtAudio;
  MD.Port      := 10000;
  MD.Transport := AudioVisualProfile;
  MD.AddFormat('0');

  MD := Result.AddMediaDescription;
  MD.MediaType := mtText;
  MD.Port      := 11000;
  MD.Transport := AudioVisualProfile;
  MD.AddFormat('98');
  MD.AddAttribute(RTPMapAttribute, '98 t140/1000');
end;

function TestTIdSipSession.CreateRemoteReInvite(LocalDialog: TIdSipDialog): TIdSipRequest;
begin
  Result := Self.Core.CreateReInvite(LocalDialog,
                                     Self.SimpleSdp.AsString,
                                     Self.SimpleSdp.MimeType);
  try
    Result.ToHeader.Tag    := LocalDialog.ID.LocalTag;
    Result.From.Tag        := LocalDialog.ID.RemoteTag;
    Result.CSeq.SequenceNo := LocalDialog.RemoteSequenceNo + 1;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TestTIdSipSession.CreateSimpleSdp: TIdSdpPayload;
var
  Connection: TIdSdpConnection;
  MD:         TIdSdpMediaDescription;
begin
  Result := TIdSdpPayload.Create;
  Result.Version               := 0;

  Result.Origin.Username       := 'wintermute';
  Result.Origin.SessionID      := '2890844526';
  Result.Origin.SessionVersion := '2890842807';
  Result.Origin.NetType        := Id_SDP_IN;
  Result.Origin.AddressType    := Id_IPv4;
  Result.Origin.Address        := '127.0.0.1';

  Result.SessionName           := 'Minimum Session Info';

  MD := Result.AddMediaDescription;
  MD.MediaType := mtText;
  MD.Port      := 11000;
  MD.Transport := AudioVisualProfile;
  MD.AddFormat('98');
  MD.AddAttribute(RTPMapAttribute, '98 t140/1000');

  MD.Connections.Add(TIdSdpConnection.Create);
  Connection := MD.Connections[0];
  Connection.NetType     := Id_SDP_IN;
  Connection.AddressType := Id_IPv4;
  Connection.Address     := '127.0.0.1';
end;

procedure TestTIdSipSession.OnEndedSession(Session: TIdSipSession;
                                           const Reason: String);
begin
  Self.OnEndedSessionFired := true;
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
end;

procedure TestTIdSipSession.OnModifySession(Modify: TIdSipInboundInvite);
begin
  Self.OnModifySessionFired := true;
  Self.InboundModify := Modify;
end;

procedure TestTIdSipSession.ReceiveRemoteReInvite(Session: TIdSipSession);
begin
  // At this point Self.Invite represents the INVITE we sent out
  Self.Invite.LastHop.Branch  := Self.Invite.LastHop.Branch + '1';
  Self.Invite.CallID          := Session.Dialog.ID.CallID;
  Self.Invite.From.Tag        := Session.Dialog.ID.RemoteTag;
  Self.Invite.ToHeader.Tag    := Session.Dialog.ID.LocalTag;
  Self.Invite.CSeq.SequenceNo := Session.Dialog.RemoteSequenceNo + 1;

  // Now it represents an INVITE received from the network
  Self.ReceiveInvite;
end;

//* TestTIdSipSession Published methods ****************************************

procedure TestTIdSipSession.TestAckToInDialogInviteMatchesInvite;
var
  Ack:     TIdSipRequest;
  Session: TIdSipSession;
begin
  Session := Self.CreateAndEstablishSession;
  Self.ReceiveRemoteReInvite(Session);

  Check(Assigned(Self.InboundModify),
        Session.ClassName + ': OnModifySession didn''t fire');

  Self.InboundModify.Accept('', '');

  Ack := Self.InboundModify.InitialRequest.AckFor(Self.LastSentResponse);
  try
    Check(not Session.Match(Ack),
          Session.ClassName + ': ACK mustn''t match the Session');
    Check(Self.InboundModify.Match(Ack),
          Session.ClassName + ': ACK doesn''t match the InboundModify');
  finally
    Ack.Free;
  end;
end;

procedure TestTIdSipSession.TestInboundModify;
var
  Session: TIdSipSession;
begin
  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);

  Self.ReceiveRemoteReInvite(Session);
  Check(Self.OnModifySessionFired,
        Session.ClassName + ': OnModifySession didn''t fire');
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

procedure TestTIdSipSession.TestMatchInitialRequest;
var
  Session: TIdSipSession;
begin
  Session := Self.CreateAction as TIdSipSession;

  Check(not Session.Match(Session.InitialRequest),
        Session.ClassName + ': The initial INVITE must only match the '
      + '(In|Out)boundInvite');
end;

procedure TestTIdSipSession.TestMatchModify;
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

procedure TestTIdSipSession.TestMatchResponseToModify;
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

procedure TestTIdSipSession.TestMatchResponseToInitialRequest;
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
              Dispatcher.Transport.LastResponse.StatusCode,
              Session.ClassName + ': Unexpected response');
end;

procedure TestTIdSipSession.TestModifyGlareOutbound;
var
  DebugTimer:    TIdDebugTimerQueue;
  Event:         TNotifyEvent;
  EventCount:    Integer;
  LatestEvent:   TIdWait;
  Session:       TIdSipSession;
begin
  // Essentially, we and Remote send INVITEs simultaneously
  // We send ours and, because the remote end's sent its before ours arrives,
  // we receive its 491 Request Pending. We schedule a time to resend our
  // INVITE.

  Event := Self.Core.OnResendReInvite;
  DebugTimer := TIdDebugTimerQueue.Create;
  try
    Self.Core.Timer := DebugTimer;

    Session := Self.CreateAndEstablishSession;

    Session.Modify('', '');

    EventCount := DebugTimer.EventCount;
    Self.ReceiveResponse(SIPRequestPending);

    DebugTimer.LockTimer;
    try
      Check(EventCount < DebugTimer.EventCount,
            Session.ClassName + ': no timer added');

      LatestEvent := DebugTimer.EventAt(DebugTimer.EventCount - 1);
      Check(LatestEvent.MatchEvent(@Event),
            Session.ClassName + ': Wrong notify event');
      Self.CheckResendWaitTime(LatestEvent.DebugWaitTime,
                               Session.ClassName + ': Bad wait time (was '
                             + IntToStr(LatestEvent.DebugWaitTime) + ' milliseconds)');
    finally
      DebugTimer.UnlockTimer;
    end;
  finally
    Self.Core.Timer := nil;
    DebugTimer.Terminate;
  end;
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

procedure TestTIdSipSession.TestModify;
var
  Session: TIdSipSession;
begin
  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);

  Self.MarkSentRequestCount;
  Session.Modify('', '');
  CheckRequestSent(Session.ClassName + ': No INVITE sent');

  Self.ReceiveOk(Self.LastSentRequest);
  Check(Self.OnModifiedSessionFired,
        Session.ClassName + ': OnModifiedSession didn''t fire');
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
    FirstInvite.Assign(Self.InboundModify.InitialRequest);
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
    Self.InboundModify.Accept('', '');

    CheckResponseSent(Session.ClassName + ': No 200 response sent');
    CheckEquals(SIPOK,
                Self.LastSentResponse.StatusCode,
                Session.ClassName + ': Unexpected response to 1st INVITE');
    Check(FirstInvite.Match(Self.LastSentResponse),
          Session.ClassName + ': Response doesn''t match 1st INVITE');
    Self.ReceiveAckFor(Self.InboundModify.InitialRequest,
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

//******************************************************************************
//* TestTIdSipInboundInvite                                                    *
//******************************************************************************
//* TestTIdSipInboundInvite Public methods *************************************

procedure TestTIdSipInboundInvite.SetUp;
var
  Ok: TIdSipResponse;
begin
  inherited SetUp;

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

  Self.InviteAction := TIdSipInboundInvite.Create(Self.Core, Self.Invite);
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

    Ack := Self.Core.CreateAck(RemoteDialog);
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

    Ack := Self.Core.CreateAck(RemoteDialog);
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
                                            Ack: TIdSipRequest);
begin
  Self.Answer         := Ack.Body;
  Self.AnswerMimeType := Ack.ContentType;
  Self.OnSuccessFired := true;
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
    Self.InviteAction.ReceiveRequest(Cancel);
  finally
    Cancel.Free;
  end;

  Check(not Self.InviteAction.IsTerminated,
        'Action terminated');
  Check(not Self.Failed,
        'Listeners notified of (false) failure');

  CheckResponseSent('No response sent');

  CancelResponse := Self.LastSentResponse;
  InviteResponse := Self.Dispatcher.Transport.SecondLastResponse;

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
    Self.InviteAction.ReceiveRequest(Cancel);
  finally
    Cancel.Free;
  end;

  Check(Self.InviteAction.IsTerminated,
        'Action not marked as terminated');
  Check(Self.Failed,
        'Listeners not notified of failure');
end;

procedure TestTIdSipInboundInvite.TestInviteWithNoOffer;
var
  Ack:    TIdSipRequest;
  Action: TIdSipInboundInvite;
  Answer: String;
  Offer:  String;
begin
  // <---       INVITE        ---
  //  --- 200 OK (with offer) --->
  // <---  ACK (with answer)  ---

  Answer := TIdSipTestResources.BasicSDP('4.3.2.1');
  Offer  := TIdSipTestResources.BasicSDP('1.2.3.4');

  Self.Invite.Body := '';
  Self.Invite.RemoveAllHeadersNamed(ContentTypeHeaderFull);

  Action := TIdSipInboundInvite.Create(Self.Core, Self.Invite);
  Action.AddListener(Self);

  Self.MarkSentResponseCount;
  Action.Accept(Offer,
                SdpMimeType);

  Self.CheckResponseSent('No 2xx sent');
  CheckEquals(Offer,
              Self.LastSentResponse.Body,
              'Body of 2xx');
  CheckEquals(SdpMimeType,
              Self.LastSentResponse.ContentType,
              'Content-Type of 2xx');

  Ack := Self.Invite.AckFor(Self.LastSentResponse);
  try
    Ack.Body                        := Answer;
    Ack.ContentDisposition.Handling := DispositionSession;
    Ack.ContentLength               := Length(Answer);
    Ack.ContentType                 := Self.LastSentResponse.ContentType;

    Action.ReceiveRequest(Ack);
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
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundInvite.Create(Self.Core, Self.Invite);
  try
    Check(Action.IsInbound,
          Action.ClassName + ' not marked as inbound');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundInvite.TestIsInvite;
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundInvite.Create(Self.Core, Self.Invite);
  try
    Check(Action.IsInvite,
          Action.ClassName + 'not marked as a Invite');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundInvite.TestIsOptions;
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundInvite.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsOptions,
          Action.ClassName + ' marked as an Options');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundInvite.TestIsRegistration;
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundInvite.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsRegistration,
          Action.ClassName + ' marked as a Registration');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundInvite.TestIsSession;
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundInvite.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsSession,
          Action.ClassName + ' marked as a Session');
  finally
    Action.Free;
  end;
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

  Action := TIdSipInboundInvite.Create(Self.Core, Self.Invite);
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

  Action := TIdSipInboundInvite.Create(Self.Core, Self.Invite);
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

procedure TestTIdSipInboundInvite.TestMethod;
begin
  CheckEquals(MethodInvite,
              TIdSipInboundInvite.Method,
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
      Self.InviteAction.AddListener(L1);
      Self.InviteAction.AddListener(L2);

      Self.Dispatcher.Transport.FailWith := EIdConnectTimeout;

      Self.InviteAction.Accept('', '');

      Check(Self.InviteAction.IsTerminated, 'Action not marked as terminated');
      Check(L1.NetworkFailed, 'L1 not notified');
      Check(L2.NetworkFailed, 'L2 not notified');
    finally
       Self.InviteAction.RemoveListener(L2);
       L2.Free;
    end;
  finally
     Self.InviteAction.RemoveListener(L1);
    L1.Free;
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

procedure TestTIdSipInboundInvite.TestResendOk;
var
  Ack:        TIdSipRequest;
  I:          Integer;
  OriginalOk: TIdSipResponse;
begin
  // Does nothing if the Invite's not yet sent an OK
  Self.MarkSentResponseCount;
  Self.InviteAction.ResendOk;
  CheckNoResponseSent('The action sent an OK before it accepted the call');

  // Then we send an OK
  Self.InviteAction.Accept('', '');

  // And we make sure that repeated calls to ResendOk, well, resend the OK.
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
    Self.InviteAction.ReceiveRequest(Ack);
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
end;

procedure TestTIdSipInboundInvite.TestSendSessionProgress;
begin
  Self.MarkSentResponseCount;
  Self.InviteAction.SendSessionProgress;

  CheckResponseSent('No session progress response sent');

  CheckEquals(SIPSessionProgress,
              Self.LastSentResponse.StatusCode,
              'Unexpected Status-Code');
end;

procedure TestTIdSipInboundInvite.TestTerminateAfterAccept;
begin
  // This should never happen, really. If you accept a call then InviteAction
  // terminates. Thus by calling Terminate you try to terminate an
  // already-terminated action - which should do nothing. In fact, the UA should
  // have already destroyed the action.

  Self.InviteAction.Accept('', '');

  Self.MarkSentResponseCount;
  Self.InviteAction.Terminate;

  CheckNoResponseSent('Response sent');
  Check(Self.InviteAction.IsTerminated,
        'Action not marked as terminated');
end;

procedure TestTIdSipInboundInvite.TestTerminateBeforeAccept;
begin
  Self.MarkSentResponseCount;

  Self.InviteAction.Terminate;

  CheckResponseSent('No response sent');

  CheckEquals(SIPRequestTerminated,
              Self.LastSentResponse.StatusCode,
              'Unexpected Status-Code');

  Check(Self.InviteAction.IsTerminated,
        'Action not marked as terminated');
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

  // We create Self.Dialog in Self.OnDialogEstablished

  Self.InviteMimeType           := SdpMimeType;
  Self.InviteOffer              := TIdSipTestResources.BasicSDP('1.2.3.4');
  Self.OnDialogEstablishedFired := false;
  Self.OnFailureFired           := false;
  Self.OnRedirectFired          := false;
end;

procedure TestTIdSipOutboundInvite.TearDown;
begin
  Self.Dialog.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundInvite Protected methods *********************************

function TestTIdSipOutboundInvite.CreateAction: TIdSipAction;
var
  Invite: TIdSipOutboundInitialInvite;
begin
  Result := Self.Core.AddOutboundAction(TIdSipOutboundInitialInvite);

  Invite := Result as TIdSipOutboundInitialInvite;
  Invite.Destination := Self.Destination;
  Invite.MimeType    := Self.InviteMimeType;
  Invite.Offer       := Self.InviteOffer;
  Invite.AddListener(Self);
  Invite.Send;
end;

//* TestTIdSipOutboundInvite Private methods ***********************************

procedure TestTIdSipOutboundInvite.CheckReceiveFailed(StatusCode: Cardinal);
var
  InviteCount: Integer;
begin
  Self.CreateAction;

  InviteCount := Self.Core.InviteCount;
  Self.ReceiveResponse(StatusCode);

  Check(Self.OnFailureFired,
        'OnFailure didn''t fire after receiving a '
      + IntToStr(StatusCode) + ' response');
  Check(Self.Core.InviteCount < InviteCount,
        'Invite action not destroyed after receiving a '
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

procedure TestTIdSipOutboundInvite.OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                                       NewDialog: TidSipDialog);
begin
  Self.Dialog := NewDialog.Copy;
  InviteAgent.Dialog := Self.Dialog;

  Self.OnDialogEstablishedFired := true;
  Self.ToHeaderTag := NewDialog.ID.RemoteTag;
end;

procedure TestTIdSipOutboundInvite.OnFailure(InviteAgent: TIdSipOutboundInvite;
                                             Response: TIdSipResponse;
                                             const Reason: String);
begin
  Self.OnFailureFired := true;
end;

procedure TestTIdSipOutboundInvite.OnRedirect(Invite: TIdSipOutboundInvite;
                                              Response: TIdSipResponse);
begin
  Self.OnRedirectFired := true;
end;

procedure TestTIdSipOutboundInvite.OnSuccess(InviteAgent: TIdSipOutboundInvite;
                                             Response: TIdSipResponse);
begin
end;

//* TestTIdSipOutboundInvite Published methods *********************************

procedure TestTIdSipOutboundInvite.TestAddListener;
var
  L1, L2: TIdSipTestInviteListener;
  Invite: TIdSipOutboundInvite;
begin
  Invite := Self.CreateAction as TIdSipOutboundInvite;

  L1 := TIdSipTestInviteListener.Create;
  try
    L2 := TIdSipTestInviteListener.Create;
    try
      Invite.AddListener(L1);
      Invite.AddListener(L2);

      Self.ReceiveOk(Self.LastSentRequest);

      Check(L1.Success, 'L1 not informed of success');
      Check(L2.Success, 'L2 not informed of success');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestAnswerInAck;
var
  Invite: TIdSipOutboundInvite;
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
              'You just sent an INVITE with a body!');

  Invite.Offer    := TIdSipTestResources.BasicSDP('1.2.3.4');
  Invite.MimeType := SdpMimeType;

  Self.MarkSentAckCount;
  Self.ReceiveOkWithBody(Invite.InitialRequest,
                         TIdSipTestResources.BasicSDP('4.3.2.1'),
                         Invite.MimeType);

  CheckAckSent('No ACK sent');
  CheckEquals(Invite.Offer,
              Self.LastSentAck.Body,
              'Incorrect answer');
  CheckEquals(Invite.MimeType,
              Self.LastSentAck.ContentType,
              'Incorrect answer type');
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

  InviteCount := Self.Core.InviteCount;
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
            'No dialog established');

      // Now that we have established a dialog, the Request Terminated response
      // will contain that dialog ID.
      RequestTerminated.ToHeader.Tag := Self.ToHeaderTag;

      Self.MarkSentRequestCount;

      //  ---         CANCEL         --->
      OutboundInvite.Cancel;

      CheckRequestSent('No CANCEL sent');
      CheckEquals(MethodCancel,
                  Self.LastSentRequest.Method,
                  'The request sent wasn''t a CANCEL');
      Check(not OutboundInvite.IsTerminated,
            'No Request Terminated received means no termination');

      // <---         200 OK         ---  (for the CANCEL)
      Self.ReceiveOk(Self.LastSentRequest);

      // <--- 487 Request Terminated ---  (for the INVITE)
      //  ---           ACK          --->
      Self.MarkSentACKCount;
      Self.ReceiveResponse(RequestTerminated);

      CheckAckSent('No ACK sent');

      Check(Self.Core.InviteCount < InviteCount,
            'Action not terminated');
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

  InviteCount := Self.Core.InviteCount;
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

      Check(Self.Core.InviteCount < InviteCount,
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
  Action: TIdSipOutboundInvite;
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

  Action := Self.CreateAction as TIdSipOutboundInvite;

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

      CheckRequestSent('No request sent to terminate the cancelled session');
      CheckEquals(MethodBye,
                  Self.LastSentRequest.Method,
                  'Terminating request');

      CheckAckSent('No ACK sent in response to the 2xx');
      CheckEquals(Invite.Body,
                  Self.LastSentAck.Body,
                  'ACK body');
      CheckEquals(Invite.ContentType,
                  Self.LastSentAck.ContentType,
                  'ACK Content-Type');
      Check(Invite.ContentDisposition.Equals(Self.LastSentAck.ContentDisposition),
            'ACK Content-Disposition');
    finally
      Cancel.Free;
    end;
  finally
    Invite.Free;
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

procedure TestTIdSipOutboundInvite.TestMethod;
begin
  CheckEquals(MethodInvite,
              TIdSipOutboundInvite.Method,
              'Outbound INVITE Method');
end;

procedure TestTIdSipOutboundInvite.TestOfferInInvite;
begin
  //  ---    INVITE (with offer)   --->
  // <---   200 OK (with answer)   ---
  //  --- ACK (with copy of offer) --->

  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No initial INVITE sent');

  CheckEquals(Self.InviteOffer,
              Self.LastSentRequest.Body,
              'Body of INVITE');
  CheckEquals(Self.InviteMimeType,
              Self.LastSentRequest.ContentType,
              'Content-Type of INVITE');

  Self.MarkSentAckCount;
  Self.ReceiveOkWithBody(Self.LastSentRequest,
                         TIdSipTestResources.BasicSDP('4.3.2.1'),
                         SdpMimeType);

  CheckAckSent('No ACK sent');
  CheckEquals(Self.LastSentRequest.Body,
              Self.LastSentAck.Body,
              'Body of ACK doesn''t match INVITE');
  CheckEquals(Self.LastSentRequest.ContentType,
              Self.LastSentAck.ContentType,
              'Content-Type of ACK doesn''t match INVITE');
end;

procedure TestTIdSipOutboundInvite.TestReceiveGlobalFailed;
var
  StatusCode: Integer;
begin
  StatusCode := 600;
//  for StatusCode := 600 to 699 do
    Self.CheckReceiveFailed(StatusCode);
end;

procedure TestTIdSipOutboundInvite.TestReceiveRedirect;
begin
  Self.CreateAction;

  Self.ReceiveResponse(SIPMovedPermanently);

  Check(Self.OnRedirectFired,
        'OnRedirect didn''t fire after receiving a '
      + IntToStr(SIPMovedPermanently) + ' response');
end;

procedure TestTIdSipOutboundInvite.TestReceiveRequestFailed;
var
  StatusCode: Integer;
begin
  StatusCode := 400;

//  for StatusCode := 400 to SIPUnauthorized - 1 do
    Self.CheckReceiveFailed(StatusCode);
{
  for StatusCode := SIPUnauthorized + 1 to SIPProxyAuthenticationRequired - 1 do
    Self.CheckReceiveFailed(StatusCode);

  for StatusCode := SIPProxyAuthenticationRequired + 1 to 499 do
    Self.CheckReceiveFailed(StatusCode);
}
end;

procedure TestTIdSipOutboundInvite.TestReceiveServerFailed;
var
  StatusCode: Integer;
begin
  StatusCode := 500;
//  for StatusCode := 500 to 599 do
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
      Invite.AddListener(L1);
      Invite.AddListener(L2);
      Invite.RemoveListener(L2);

      Self.ReceiveOk(Self.LastSentRequest);

      Check(L1.Success,
            'First listener not notified');
      Check(not L2.Success,
            'Second listener erroneously notified, ergo not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestTerminateBeforeAccept;
var
  OutboundInvite: TIdSipOutboundInvite;
begin
  OutboundInvite := Self.CreateAction as TIdSipOutboundInvite;
  Self.ReceiveRinging(Self.LastSentRequest);
  Self.MarkSentRequestCount;

  OutboundInvite.Terminate;

  CheckRequestSent('Action didn''t send a CANCEL');
end;

procedure TestTIdSipOutboundInvite.TestTerminateAfterAccept;
var
  OutboundInvite: TIdSipOutboundInvite;
begin
  OutboundInvite := Self.CreateAction as TIdSipOutboundInvite;

  Self.ReceiveOk(Self.LastSentRequest);

  Self.MarkSentRequestCount;

  OutboundInvite.Terminate;

  CheckNoRequestSent('Action sent a CANCEL for a fully established call');
end;

procedure TestTIdSipOutboundInvite.TestTransactionCompleted;
var
  Invite: TIdSipOutboundInvite;
begin
  Invite := Self.CreateAction as TIdSipOutboundInvite;
  Invite.TransactionCompleted;
  Check(Invite.IsTerminated, 'Outbound INVITE not marked as terminated');
end;

//******************************************************************************
//* TestTIdSipOutboundRedirectedInvite                                         *
//******************************************************************************
//* TestTIdSipOutboundRedirectedInvite Protected methods ***********************

function TestTIdSipOutboundRedirectedInvite.CreateAction: TIdSipAction;
var
  Invite: TIdSipOutboundRedirectedInvite;
begin
  Result := Self.Core.AddOutboundRedirectedInvite;

  Invite := Result as TIdSipOutboundRedirectedInvite;
  Invite.Contact        := Self.Destination;
  Invite.OriginalInvite := Self.Invite;
  Invite.AddListener(Self);
  Invite.Send;
end;

//* TestTIdSipOutboundRedirectedInvite Published methods ***********************

procedure TestTIdSipOutboundRedirectedInvite.TestRedirectedInvite;
var
  Invite:         TIdSipOutboundRedirectedInvite;
  NewInvite:      TIdSipRequest;
  OriginalInvite: TIdSipRequest;
begin
  OriginalInvite := Self.Core.CreateInvite(Self.Destination, '', '');
  try
    Invite := Self.Core.AddOutboundRedirectedInvite;
    Self.MarkSentRequestCount;

    Invite.Contact := Self.Destination;
    Invite.OriginalInvite := OriginalInvite;
    Invite.Send;

    CheckRequestSent('No INVITE sent');

    NewInvite := Invite.InitialRequest;

    CheckEquals(OriginalInvite.CallID,
                NewInvite.CallID,
                'Call-ID mismatch between original and new INVITEs');
    CheckEquals(OriginalInvite.From.Tag,
                NewInvite.From.Tag,
                'From tag mismatch between original and new INVITEs');
    Check(not NewInvite.ToHeader.HasTag,
          'New INVITE mustn''t have a To tag');
  finally
    OriginalInvite.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipOutboundReInvite                                                 *
//******************************************************************************
//* TestTIdSipOutboundReInvite Public methods **********************************

procedure TestTIdSipOutboundReInvite.SetUp;
begin
  inherited SetUp;

  Self.Dialog := Self.CreateArbitraryDialog;
end;

procedure TestTIdSipOutboundReInvite.TearDown;
begin
  Self.Dialog.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundReInvite Protected methods *******************************

function TestTIdSipOutboundReInvite.CreateAction: TIdSipAction;
var
  Invite: TIdSipOutboundReInvite;
begin
  Result := Self.Core.AddOutboundReInvite;

  Self.Dialog.RemoteTarget.Uri := Self.Destination.Address.Uri;

  Invite := Result as TIdSipOutboundReInvite;
  Invite.Dialog         := Self.Dialog;
  Invite.MimeType       := Self.InviteMimeType;
  Invite.Offer          := Self.InviteOffer;
  Invite.OriginalInvite := Self.Invite;
  Invite.AddListener(Self);
  Invite.Send;
end;

//* TestTIdSipOutboundReInvite Published methods *******************************

procedure TestTIdSipOutboundReInvite.TestReInviteTwice;
var
  Invite: TIdSipOutboundReInvite;
begin
  Invite := TIdSipOutboundReInvite.Create(Self.Core);
  try
    Invite.OriginalInvite    := Self.Invite;
    Invite.Dialog            := Dialog;
    Invite.InOutboundSession := false;
    Invite.Send;

    try
      Invite.Send;
      Fail('Failed to bail out calling ReInvite a 2nd time');
    except
      on EIdSipTransactionUser do;
    end;
  finally
    Invite.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipInboundOptions                                                   *
//******************************************************************************
//* TestTIdSipInboundOptions Private methods ***********************************

procedure TestTIdSipInboundOptions.ReceiveOptions;
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

//* TestTIdSipInboundOptions Published methods *********************************

procedure TestTIdSipInboundOptions.TestIsInbound;
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundInvite.Create(Self.Core, Self.Invite);
  try
    Check(Action.IsInbound,
          Action.ClassName + ' not marked as inbound');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundOptions.TestIsInvite;
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundOptions.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsInvite,
          Action.ClassName + ' marked as a Invite');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundOptions.TestIsOptions;
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundOptions.Create(Self.Core, Self.Invite);
  try
    Check(Action.IsOptions,
          Action.ClassName + 'not marked as an Options');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundOptions.TestIsRegistration;
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundOptions.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsRegistration,
          Action.ClassName + ' marked as a Registration');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundOptions.TestIsSession;
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundOptions.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsSession,
          Action.ClassName + ' marked as a Session');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundOptions.TestOptions;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;
  Self.ReceiveOptions;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  Check(Response.HasHeader(AllowHeader),
        'No Allow header');
  CheckEquals(Self.Core.AllowedMethods,
              Response.FirstHeader(AllowHeader).FullValue,
              'Allow header');

  Check(Response.HasHeader(AcceptHeader),
        'No Accept header');
  CheckEquals(Self.Core.AllowedContentTypes,
              Response.FirstHeader(AcceptHeader).FullValue,
              'Accept header');

  Check(Response.HasHeader(AcceptEncodingHeader),
        'No Accept-Encoding header');
  CheckEquals(Self.Core.AllowedEncodings,
              Response.FirstHeader(AcceptEncodingHeader).FullValue,
              'Accept-Encoding header');

  Check(Response.HasHeader(AcceptLanguageHeader),
        'No Accept-Language header');
  CheckEquals(Self.Core.AllowedLanguages,
              Response.FirstHeader(AcceptLanguageHeader).FullValue,
              'Accept-Language header');

  Check(Response.HasHeader(SupportedHeaderFull),
        'No Supported header');
  CheckEquals(Self.Core.AllowedExtensions,
              Response.FirstHeader(SupportedHeaderFull).FullValue,
              'Supported header value');

  Check(Response.HasHeader(ContactHeaderFull),
        'No Contact header');
  Check(Self.Core.Contact.Equals(Response.FirstContact),
        'Contact header value');

  Check(Response.HasHeader(WarningHeader),
        'No Warning header');
  CheckEquals(Self.Core.Hostname,
              Response.FirstWarning.Agent,
              'Warning warn-agent');
end;

procedure TestTIdSipInboundOptions.TestOptionsWhenDoNotDisturb;
var
  Response: TIdSipResponse;
begin
  Self.Core.DoNotDisturb := true;

  Self.MarkSentResponseCount;
  Self.ReceiveOptions;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPTemporarilyUnavailable,
              Response.StatusCode,
              'Do Not Disturb');
end;

//******************************************************************************
//* TestTIdSipOutboundOptions                                                  *
//******************************************************************************
//* TestTIdSipOutboundOptions Public methods ***********************************

procedure TestTIdSipOutboundOptions.SetUp;
begin
  inherited SetUp;

  Self.ReceivedResponse := false;
end;

//* TestTIdSipOutboundOptions Protected methods ********************************

function TestTIdSipOutboundOptions.CreateAction: TIdSipAction;
var
  Options: TIdSipOutboundOptions;
begin
  Options := Self.Core.QueryOptions(Self.Destination);
  Options.AddListener(Self);
  Options.Send;
  Result := Options;
end;

//* TestTIdSipOutboundOptions Private methods **********************************

procedure TestTIdSipOutboundOptions.OnResponse(OptionsAgent: TIdSipOutboundOptions;
                                               Response: TIdSipResponse);
begin
  Self.ReceivedResponse := true;
end;

//* TestTIdSipOutboundOptions Published methods ********************************

procedure TestTIdSipOutboundOptions.TestAddListener;
var
  L1, L2:  TIdSipTestOptionsListener;
  Options: TIdSipOutboundOptions;
begin
  Options := Self.Core.QueryOptions(Self.Core.From);
  Options.Send;

  L1 := TIdSipTestOptionsListener.Create;
  try
    L2 := TIdSipTestOptionsListener.Create;
    try
      Options.AddListener(L1);
      Options.AddListener(L2);

      Self.ReceiveOk(Self.LastSentRequest);

      Check(L1.Response, 'L1 not informed of response');
      Check(L2.Response, 'L2 not informed of response');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipOutboundOptions.TestIsOptions;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(Action.IsOptions,
        Action.ClassName + ' marked as an Options');
end;

procedure TestTIdSipOutboundOptions.TestReceiveResponse;
var
  StatusCode: Cardinal;
begin
  for StatusCode := SIPOKResponseClass to SIPGlobalFailureResponseClass do begin
    Self.ReceivedResponse := false;
    Self.CreateAction;
    Self.ReceiveResponse(StatusCode * 100);
    Check(Self.ReceivedResponse,
          'Listeners not notified of response ' + IntToStr(StatusCode * 100));
  end;
end;

procedure TestTIdSipOutboundOptions.TestRemoveListener;
var
  L1, L2:  TIdSipTestOptionsListener;
  Options: TIdSipOutboundOptions;
begin
  Options := Self.Core.QueryOptions(Self.Core.From);
  Options.Send;

  L1 := TIdSipTestOptionsListener.Create;
  try
    L2 := TIdSipTestOptionsListener.Create;
    try
      Options.AddListener(L1);
      Options.AddListener(L2);
      Options.RemoveListener(L2);

      Self.ReceiveOk(Self.LastSentRequest);

      Check(L1.Response,
            'First listener not notified');
      Check(not L2.Response,
            'Second listener erroneously notified, ergo not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

//******************************************************************************
//*  TestTIdSipRegistration                                                    *
//******************************************************************************
//*  TestTIdSipRegistration Public methods *************************************

procedure TestTIdSipRegistration.TestIsRegistration;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(Action.IsRegistration,
        Action.ClassName + ' marked as a Registration');
end;

//******************************************************************************
//*  TestTIdSipInboundRegistration                                             *
//******************************************************************************
//*  TestTIdSipInboundRegistration Public methods ******************************

procedure TestTIdSipInboundRegistration.TestIsInbound;
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundInvite.Create(Self.Core, Self.Invite);
  try
    Check(Action.IsInbound,
          Action.ClassName + ' not marked as inbound');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundRegistration.TestIsInvite;
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundRegistration.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsInvite,
          Action.ClassName + ' marked as an Invite');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundRegistration.TestIsOptions;
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundRegistration.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsOptions,
          Action.ClassName + ' marked as an Options');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundRegistration.TestIsRegistration;
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundRegistration.Create(Self.Core, Self.Invite);
  try
    Check(Action.IsRegistration,
          Action.ClassName + 'not marked as a Registration');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundRegistration.TestIsSession;
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundRegistration.Create(Self.Core, Self.Invite);
  try
    Check(not Action.IsSession,
          Action.ClassName + ' marked as a Session');
  finally
    Action.Free;
  end;
end;

//******************************************************************************
//*  TestTIdSipOutboundRegistration                                            *
//******************************************************************************
//*  TestTIdSipOutboundRegistration Public methods *****************************

procedure TestTIdSipOutboundRegistration.SetUp;
const
  TwoHours = 7200;
begin
  inherited SetUp;

  Self.Registrar := TIdSipRegistrar.Create;
  Self.Registrar.From.Address.Uri := 'sip:talking-head.tessier-ashpool.co.luna';

  Self.Contacts := TIdSipContacts.Create;
  Self.Contacts.Add(ContactHeaderFull).Value := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';

  Self.Succeeded  := false;
  Self.MinExpires := TwoHours;
end;

procedure TestTIdSipOutboundRegistration.TearDown;
begin
  Self.Contacts.Free;
  Self.Registrar.Free;

  inherited TearDown;
end;

//*  TestTIdSipOutboundRegistration Protected methods **************************

function TestTIdSipOutboundRegistration.RegistrarAddress: TIdSipUri;
begin
  Self.Registrar.From.Address.Uri      := Self.Destination.Address.Uri;
  Self.Registrar.From.Address.Username := '';
  Result := Self.Registrar.From.Address;
end;

//*  TestTIdSipOutboundRegistration Private methods ****************************

procedure TestTIdSipOutboundRegistration.OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                                           CurrentBindings: TIdSipContacts;
                                           const Reason: String);
begin
  Self.ActionFailed := true;
end;

procedure TestTIdSipOutboundRegistration.OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                                           CurrentBindings: TIdSipContacts);
begin
  Self.Succeeded := true;
end;

procedure TestTIdSipOutboundRegistration.ReceiveRemoteIntervalTooBrief;
var
  Response: TIdSipResponse;
begin
  Response := Self.Registrar.CreateResponse(Self.LastSentRequest,
                                            SIPIntervalTooBrief);
  try
    Response.AddHeader(MinExpiresHeader).Value := IntToStr(Self.MinExpires);

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

//*  TestTIdSipOutboundRegistration Published methods **************************

procedure TestTIdSipOutboundRegistration.TestAddListener;
var
  L1, L2:       TIdSipTestRegistrationListener;
  Registration: TIdSipOutboundRegistration;
begin
  Registration := Self.CreateAction as TIdSipOutboundRegistration;

  L1 := TIdSipTestRegistrationListener.Create;
  try
    L2 := TIdSipTestRegistrationListener.Create;
    try
      Registration.AddListener(L1);
      Registration.AddListener(L2);

      Self.ReceiveOk(Self.LastSentRequest);

      Check(L1.Success, 'L1 not informed of success');
      Check(L2.Success, 'L2 not informed of success');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipOutboundRegistration.TestMethod;
begin
  CheckEquals(MethodRegister,
              TIdSipOutboundRegistration.Method,
              'Outbound registration; Method');
end;

procedure TestTIdSipOutboundRegistration.TestReceiveFail;
begin
  Self.CreateAction;
  Self.ReceiveResponse(SIPInternalServerError);
  Check(Self.ActionFailed, 'Registration succeeded');
end;

procedure TestTIdSipOutboundRegistration.TestReceiveIntervalTooBrief;
const
  OneHour = 3600;
begin
  Self.Contacts.First;
  Self.Contacts.CurrentContact.Expires := OneHour;
  Self.CreateAction;

  Self.MarkSentRequestCount;
  Self.ReceiveRemoteIntervalTooBrief;

  CheckRequestSent('No re-request issued');
  Check(Self.LastSentRequest.HasExpiry,
        'Re-request has no expiry');
  CheckEquals(Self.MinExpires,
              Self.LastSentRequest.QuickestExpiry,
              'Re-request minimum expires');

  Self.ReceiveOk(Self.LastSentRequest);
  Check(Self.Succeeded, '(Re-)Registration failed');
end;

procedure TestTIdSipOutboundRegistration.TestReceiveMovedPermanently;
begin
  Self.Locator.AddAAAA('fried.neurons.org', '::1');

  Self.CreateAction;
  Self.MarkSentRequestCount;
  Self.ReceiveMovedPermanently('sip:case@fried.neurons.org');
  CheckRequestSent('No request re-issued for REGISTER');
end;

procedure TestTIdSipOutboundRegistration.TestReceiveOK;
begin
  Self.CreateAction;
  Self.ReceiveOk(Self.LastSentRequest);
  Check(Self.Succeeded, 'Registration failed');
end;

procedure TestTIdSipOutboundRegistration.TestRemoveListener;
var
  L1, L2:       TIdSipTestRegistrationListener;
  Registration: TIdSipOutboundRegistration;
begin
  Registration := Self.CreateAction as TIdSipOutboundRegistration;
  L1 := TIdSipTestRegistrationListener.Create;
  try
    L2 := TIdSipTestRegistrationListener.Create;
    try
      Registration.AddListener(L1);
      Registration.AddListener(L2);
      Registration.RemoveListener(L2);

      Self.ReceiveOk(Self.LastSentRequest);

      Check(L1.Success,
            'First listener not notified');
      Check(not L2.Success,
            'Second listener erroneously notified, ergo not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipOutboundRegistration.TestReregisterTime;
const
  OneMinute     = 60;
  OneHour       = 60*OneMinute;
  OneDay        = 24*OneHour; // Seconds in a day
  FiveMinutes   = 5*OneMinute;
  TwentyMinutes = 20*OneMinute;
var
  Reg: TIdSipOutboundRegistration;
begin
  Reg := Self.CreateAction as TIdSipOutboundRegistration;

  CheckEquals(OneDay - FiveMinutes, Reg.ReregisterTime(OneDay), 'One day');
  CheckEquals(OneHour - FiveMinutes, Reg.ReregisterTime(OneHour), 'One hour');
  CheckEquals(TwentyMinutes - FiveMinutes,
              Reg.ReregisterTime(TwentyMinutes), '20 minutes');

  CheckEquals(FiveMinutes - OneMinute,
              Reg.ReregisterTime(FiveMinutes),
              '5 minutes');

  CheckEquals(4*30 div 5, Reg.ReregisterTime(30), '30 seconds');
  CheckEquals(1,          Reg.ReregisterTime(1), '1 second');
  CheckEquals(1,          Reg.ReregisterTime(0), 'Zero');
end;

procedure TestTIdSipOutboundRegistration.TestSequenceNumberIncrements;
var
  SeqNo: Cardinal;
begin
  Self.CreateAction;
  SeqNo := Self.LastSentRequest.CSeq.SequenceNo;
  Self.CreateAction;
  Check(SeqNo + 1 = Self.LastSentRequest.CSeq.SequenceNo,
        'CSeq sequence number didn''t increment');
end;

procedure TestTIdSipOutboundRegistration.TestUsername;
var
  Registration: TIdSipOutboundRegistration;
begin
  Registration := Self.CreateAction as TIdSipOutboundRegistration;

  Self.Core.From.DisplayName := 'foo';
  CheckEquals(Self.Core.Username,
              Registration.Username,
              'Username "foo"');

  Self.Core.From.DisplayName := 'bar';
  CheckEquals(Self.Core.Username,
              Registration.Username,
              'Username "bar"');
end;

//******************************************************************************
//* TestTIdSipOutboundRegister                                                 *
//******************************************************************************
//* TestTIdSipOutboundRegister Protected methods *******************************

function TestTIdSipOutboundRegister.CreateAction: TIdSipAction;
var
  Reg: TIdSipOutboundRegister;
begin
  Result := Self.Core.RegisterWith(Self.RegistrarAddress);

  Reg := Result as TIdSipOutboundRegister;
  Reg.AddListener(Self);
  Reg.Bindings  := Self.Contacts;
  Reg.Registrar := Self.RegistrarAddress;
  Result.Send;
end;

//* TestTIdSipOutboundRegister Private methods *********************************

procedure TestTIdSipOutboundRegister.CheckAutoReregister(ReceiveResponse: TExpiryProc;
                                                         EventIsScheduled: Boolean;
                                                         const MsgPrefix: String);
const
  ExpiryTime = 42;
var
  DebugTimer:    TIdDebugTimerQueue;
  Event:         TNotifyEvent;
  EventCount:    Integer;
  LatestEvent:   TIdWait;
begin
  Event := Self.Core.OnReregister;
  DebugTimer := TIdDebugTimerQueue.Create;
  try
    Self.Core.Timer := DebugTimer;

    Self.CreateAction;

    EventCount := DebugTimer.EventCount;
    ReceiveResponse(ExpiryTime);

    DebugTimer.LockTimer;
    try
      if EventIsScheduled then begin
        Check(EventCount < DebugTimer.EventCount,
              MsgPrefix + ': No timer added');

        LatestEvent := DebugTimer.EventAt(DebugTimer.EventCount - 1);
        Check(LatestEvent.MatchEvent(@Event),
              MsgPrefix + ': Wrong notify event');
        Check(LatestEvent.DebugWaitTime > 0,
              MsgPrefix + ': Bad wait time (' + IntToStr(LatestEvent.DebugWaitTime) + ')');
      end
      else
        CheckEquals(EventCount,
                    DebugTimer.EventCount,
                    MsgPrefix + ': Timer erroneously added');
    finally
      DebugTimer.UnlockTimer;
    end;
  finally
    Self.Core.Timer := nil;
    DebugTimer.Terminate;
  end;
end;

procedure TestTIdSipOutboundRegister.ReceiveOkWithContactExpiresOf(ExpiryTime: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Self.LastSentRequest);
  try
    Response.Contacts := Self.LastSentRequest.Contacts;
    Response.FirstContact.Expires := ExpiryTime;

    Response.AddHeader(ContactHeaderFull).Value := Response.FirstContact.AsAddressOfRecord
                                                 + '1;expires=' + IntToStr(ExpiryTime + 1);

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundRegister.ReceiveOkWithExpiresOf(ExpiryTime: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Self.LastSentRequest);
  try
    Response.Contacts := Self.LastSentRequest.Contacts;
    Response.FirstExpires.NumericValue := ExpiryTime;

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundRegister.ReceiveOkWithNoExpires(ExpiryTime: Cardinal);
begin
  Self.ReceiveOk(Self.LastSentRequest);
end;

//* TestTIdSipOutboundRegister Published methods *******************************

procedure TestTIdSipOutboundRegister.TestAutoReregister;
begin
  Self.Core.AutoReRegister := true;
  Self.CheckAutoReregister(Self.ReceiveOkWithExpiresOf,
                           true,
                           'Expires header');
end;

procedure TestTIdSipOutboundRegister.TestAutoReregisterContactHasExpires;
begin
  Self.Core.AutoReRegister := true;
  Self.CheckAutoReregister(Self.ReceiveOkWithContactExpiresOf,
                           true,
                           'Contact expires param');
end;

procedure TestTIdSipOutboundRegister.TestAutoReregisterNoExpiresValue;
begin
  Self.Core.AutoReRegister := true;
  Self.CheckAutoReregister(Self.ReceiveOkWithNoExpires,
                           false,
                           'No Expires header or expires param');
end;

procedure TestTIdSipOutboundRegister.TestAutoReregisterSwitchedOff;
begin
  Self.Core.AutoReRegister := false;
  Self.CheckAutoReregister(Self.ReceiveOkWithExpiresOf,
                           false,
                           'Expires header; Autoreregister = false');
end;

procedure TestTIdSipOutboundRegister.TestReceiveIntervalTooBriefForOneContact;
const
  OneHour = 3600;
var
  RequestContacts:      TIdSipContacts;
  SecondContactExpires: Cardinal;
begin
  // We try to be tricky: One contact has a (too-brief) expires of one hour.
  // The other has an expires of three hours. The registrar accepts a minimum
  // expires of two hours. We expect the registrar to reject the request with
  // a 423 Interval Too Brief, and for the SipRegistration to re-issue the
  // request leaving the acceptable contact alone and only modifying the
  // too-short contact.

  SecondContactExpires := OneHour*3;

  Self.Contacts.First;
  Self.Contacts.CurrentContact.Expires := OneHour;
  Self.Contacts.Add(ContactHeaderFull).Value := 'sip:wintermute@talking-head-2.tessier-ashpool.co.luna;expires='
                                              + IntToStr(SecondContactExpires);
  Self.CreateAction;

  Self.MarkSentRequestCount;
  Self.ReceiveRemoteIntervalTooBrief;

  CheckRequestSent('No re-request issued');
  Check(Self.LastSentRequest.HasExpiry,
        'Re-request has no expiry');
  CheckEquals(Self.MinExpires,
              Self.LastSentRequest.QuickestExpiry,
              'Re-request minimum expires');
  RequestContacts := TIdSipContacts.Create(Self.LastSentRequest.Headers);
  try
    RequestContacts.First;
    Check(RequestContacts.HasNext,
          'No Contacts');
    Check(RequestContacts.CurrentContact.WillExpire,
          'First contact missing expires');
    CheckEquals(Self.MinExpires,
                RequestContacts.CurrentContact.Expires,
                'First (too brief) contact');
    RequestContacts.Next;
    Check(RequestContacts.HasNext, 'Too few Contacts');
    Check(RequestContacts.CurrentContact.WillExpire,
          'Second contact missing expires');
    CheckEquals(SecondContactExpires,
                RequestContacts.CurrentContact.Expires,
                'Second, acceptable, contact');
  finally
    RequestContacts.Free;
  end;

  Self.ReceiveOk(Self.LastSentRequest);
  Check(Self.Succeeded, '(Re-)Registration failed');
end;

procedure TestTIdSipOutboundRegister.TestRegister;
var
  Request: TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No request sent');

  Request := Self.LastSentRequest;
  CheckEquals(Self.RegistrarAddress.Uri,
              Request.RequestUri.Uri,
              'Request-URI');
  CheckEquals(MethodRegister, Request.Method, 'Method');
  Check(Request.Contacts.Equals(Self.Contacts),
        'Bindings');
end;

//******************************************************************************
//* TestTIdSipOutboundRegistrationQuery                                        *
//******************************************************************************
//* TestTIdSipOutboundRegistrationQuery Protected methods **********************

function TestTIdSipOutboundRegistrationQuery.CreateAction: TIdSipAction;
var
  Reg: TIdSipOutboundRegistrationQuery;
begin
  Result := Self.Core.CurrentRegistrationWith(Self.RegistrarAddress);

  Reg := Result as TIdSipOutboundRegistrationQuery;
  Reg.AddListener(Self);
  Reg.Registrar := Self.RegistrarAddress;
  Result.Send;
end;

//* TestTIdSipOutboundRegistrationQuery Published methods **********************

procedure TestTIdSipOutboundRegistrationQuery.TestFindCurrentBindings;
var
  Request: TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No request sent');

  Request := Self.LastSentRequest;
  Check(Request.Contacts.IsEmpty,
        'Contact headers present');
end;

//******************************************************************************
//* TestTIdSipOutboundUnregister                                               *
//******************************************************************************
//* TestTIdSipOutboundUnregister Public methods ********************************

procedure TestTIdSipOutboundUnregister.SetUp;
begin
  inherited SetUp;

  Self.Bindings := TIdSipContacts.Create;
  Self.WildCard := false;
end;

procedure TestTIdSipOutboundUnregister.TearDown;
begin
  Self.Bindings.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundUnregister Protected methods *****************************

function TestTIdSipOutboundUnregister.CreateAction: TIdSipAction;
var
  Reg: TIdSipOutboundUnregister;
begin
  Result := Self.Core.UnregisterFrom(Self.RegistrarAddress);

  Reg := Result as TIdSipOutboundUnregister;
  Reg.Bindings   := Self.Bindings;
  Reg.IsWildCard := Self.WildCard;
  Reg.AddListener(Self);
  Result.Send;
end;

//* TestTIdSipOutboundUnregister Published methods *****************************

procedure TestTIdSipOutboundUnregister.TestUnregisterAll;
var
  Request: TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.WildCard := true;
  Self.CreateAction;
  CheckRequestSent('No request sent');

  Request := Self.LastSentRequest;
  CheckEquals(Self.RegistrarAddress.Uri,
              Request.RequestUri.Uri,
              'Request-URI');
  CheckEquals(MethodRegister, Request.Method, 'Method');
  CheckEquals(1, Request.Contacts.Count,
             'Contact count');
  Check(Request.FirstContact.IsWildCard,
        'First Contact');
  CheckEquals(0, Request.QuickestExpiry,
             'Request expiry');
end;

procedure TestTIdSipOutboundUnregister.TestUnregisterSeveralContacts;
var
  Request: TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.Bindings.Add(ContactHeaderFull).Value := 'sip:case@fried.neurons.org';
  Self.Bindings.Add(ContactHeaderFull).Value := 'sip:wintermute@tessier-ashpool.co.luna';

  Self.CreateAction;
  CheckRequestSent('No request sent');

  Request := Self.LastSentRequest;
  CheckEquals(Self.RegistrarAddress.Uri,
              Request.RequestUri.Uri,
              'Request-URI');
  CheckEquals(MethodRegister, Request.Method, 'Method');

  Request.Contacts.First;
  Self.Bindings.First;

  while Request.Contacts.HasNext do begin
    CheckEquals(Self.Bindings.CurrentContact.Value,
                Request.Contacts.CurrentContact.Value,
                'Different Contact');

    CheckEquals(0,
                Request.Contacts.CurrentContact.Expires,
                'Expiry of ' + Request.Contacts.CurrentContact.Value);
    Request.Contacts.Next;
    Self.Bindings.Next;
  end;

  CheckEquals(Self.Bindings.Count, Request.Contacts.Count,
             'Contact count');
end;

//******************************************************************************
//* TestTIdSipInboundSession                                                   *
//******************************************************************************
//* TestTIdSipInboundSession Public methods ************************************

procedure TestTIdSipInboundSession.SetUp;
begin
  inherited SetUp;

  Self.Core.AddUserAgentListener(Self);

  Self.OnEndedSessionFired    := false;
  Self.OnModifiedSessionFired := false;
  Self.SentRequestTerminated  := false;

  Self.Invite.ContentType   := SdpMimeType;
  Self.Invite.Body          := Self.SimpleSdp.AsString;
  Self.Invite.ContentLength := Length(Self.SimpleSdp.AsString);

  Self.Locator.AddA(Self.Invite.RequestUri.Host, '127.0.0.1');
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
    Self.Invite.ContentType   := Self.RemoteContentType;

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

//* TestTIdSipInboundSession Private methods ***********************************

procedure TestTIdSipInboundSession.OnAuthenticationChallenge(UserAgent: TIdSipAbstractUserAgent;
                                                             Challenge: TIdSipResponse;
                                                             var Username: String;
                                                             var Password: String;
                                                             var TryAgain: Boolean);
begin
end;

procedure TestTIdSipInboundSession.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractUserAgent;
                                                             Message: TIdSipMessage;
                                                             Receiver: TIdSipTransport);
begin
end;

procedure TestTIdSipInboundSession.OnEndedSession(Session: TIdSipSession;
                                                  const Reason: String);
begin
  inherited OnEndedSession(Session, Reason);
  Self.ActionFailed := true;

  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipInboundSession.OnEstablishedSession(Session: TIdSipSession;
                                                        const RemoteSessionDescription: String;
                                                        const MimeType: String);
begin
end;

procedure TestTIdSipInboundSession.OnInboundCall(UserAgent: TIdSipAbstractUserAgent;
                                                 Session: TIdSipInboundSession);
begin
  Self.Session := Session;
  Self.Session.AddSessionListener(Self);
end;

procedure TestTIdSipInboundSession.OnNewData(Data: TIdRTPPayload;
                                             Binding: TIdConnection);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipInboundSession.OnSendRequest(Request: TIdSipRequest;
                                                 Sender: TIdSipTransport);
begin
end;

procedure TestTIdSipInboundSession.OnSendResponse(Response: TIdSipResponse;
                                                  Sender: TIdSipTransport);
begin
  if (Response.StatusCode = SIPRequestTerminated) then
    Self.SentRequestTerminated := true;
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
  CheckEquals(Self.RemoteDesc,
              Self.Session.RemoteSessionDescription,
              'RemoteSessionDescription');
  CheckEquals(Self.RemoteContentType,
              Self.Session.RemoteMimeType,
              'RemoteMimeType');

  Answer         := TIdSipTestResources.BasicSDP('public.booth.org');
  AnswerMimeType := SdpMimeType;

  Self.Session.AcceptCall(Answer, AnswerMimeType);

  Check(Self.Session.DialogEstablished,
        'Dialog not established');
  CheckNotNull(Self.Session.Dialog,
               'Dialog object wasn''t created');
  CheckEquals(Answer,         Self.Session.LocalSessionDescription, 'LocalSessionDescription');
  CheckEquals(AnswerMimeType, Self.Session.LocalMimeType,           'LocalMimeType');
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
  InviteResponse := Self.Dispatcher.Transport.SecondLastResponse;

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
        'Session didn''t terminate');

  Check(Self.OnEndedSessionFired,
        'Session didn''t notify listeners of ended session');
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
        'No notification of ended session');

  Check(Self.Core.SessionCount < SessionCount,
        'Session not marked as terminated');
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
  CheckEquals(Self.Session.RemoteSessionDescription,
              Answer,
              'RemoteSessionDescription');
  CheckEquals(Self.Session.RemoteMimeType,
              AnswerType,
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
var
  Action: TIdSipAction;
begin
  Action := TIdSipInboundInvite.Create(Self.Core, Self.Invite);
  try
    Check(Action.IsInbound,
          Action.ClassName + ' not marked as inbound');
  finally
    Action.Free;
  end;
end;

procedure TestTIdSipInboundSession.TestIsOutboundCall;
begin
  Self.CreateAction;
  Check(not Self.Session.IsOutboundCall,
        'Inbound session; IsOutboundCall');
end;

procedure TestTIdSipInboundSession.TestMethod;
begin
  CheckEquals(MethodInvite,
              TIdSipInboundSession.Method,
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
  Check(InternalServerError.FirstRetryAfter.NumericValue <= MaxPrematureInviteRetry,
        'Bad Retry-After value (' + IntToStr(InternalServerError.FirstRetryAfter.NumericValue) + ')');

  Self.ReceiveAck;
end;

procedure TestTIdSipInboundSession.TestInboundModifyReceivesNoAck;
var
  I: Integer;
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
  Self.InboundModify.Accept('', '');

  Self.MarkSentRequestCount;
  // Time out waiting for the ACK
  for I := 1 to 7 do
    Self.InboundModify.ResendOk;

  CheckRequestSent('Requests sent');

  CheckEquals(MethodBye,
              Self.LastSentRequest.Method,
              'Request');
end;

procedure TestTIdSipInboundSession.TestReceiveBye;
begin
  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');
  Self.Session.AcceptCall('', '');

  Self.ReceiveBye(Self.Session.Dialog);

  Check(Self.OnEndedSessionFired, 'OnEndedSession didn''t fire');
end;

procedure TestTIdSipInboundSession.TestRedirectCall;
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

    Self.Session.RedirectCall(Dest);
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

    Check(Self.OnEndedSessionFired, 'OnEndedSession didn''t fire');
  finally
    Dest.Free;
  end;
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

procedure TestTIdSipInboundSession.TestTerminate;
var
  Request:      TIdSipRequest;
  SessionCount: Integer;
begin
  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Self.MarkSentRequestCount;
  Self.Session.AcceptCall('', '');

  SessionCount := Self.Core.SessionCount;
  Self.Session.Terminate;

  CheckRequestSent('no BYE sent');

  Request := Self.LastSentRequest;
  Check(Request.IsBye, 'Unexpected last request');

  Check(Self.Core.SessionCount < SessionCount,
        'Session not marked as terminated');
end;

procedure TestTIdSipInboundSession.TestTerminateUnestablishedSession;
var
  Response:     TIdSipResponse;
  SessionCount: Integer;
begin
  Self.CreateAction;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Self.MarkSentResponseCount;
  SessionCount  := Self.Core.SessionCount;

  Self.Session.Terminate;

  CheckResponseSent('no response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPRequestTerminated,
              Response.StatusCode,
              'Unexpected last response');

  Check(Self.Core.SessionCount < SessionCount,
        'Session not marked as terminated');
end;

//******************************************************************************
//* TestTIdSipOutboundSession                                                  *
//******************************************************************************
//* TestTIdSipOutboundSession Public methods ***********************************

procedure TestTIdSipOutboundSession.SetUp;
begin
  inherited SetUp;

  Self.Dispatcher.Transport.WriteLog := true;

  Self.MimeType := SdpMimeType;
  Self.SDP :='v=0'#13#10
           + 'o=franks 123456 123456 IN IP4 127.0.0.1'#13#10
           + 's=-'#13#10
           + 'c=IN IP4 127.0.0.1'#13#10
           + 'm=audio 8000 RTP/AVP 0'#13#10;

  Self.Core.AddUserAgentListener(Self);

  Self.Session := Self.CreateAction as TIdSipOutboundSession;

  Self.RemoteMimeType         := '';
  Self.OnDroppedMessage       := false;
  Self.OnEndedSessionFired    := false;
  Self.OnModifiedSessionFired := false;
  Self.RemoteDesc             := '';

  // DNS entries for redirected domains, etc.
  Self.Locator.AddA('bar.org',   '127.0.0.1');
  Self.Locator.AddA('quaax.org', '127.0.0.1');
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
  Session := Self.Core.Call(Self.Destination, Self.SDP, Self.MimeType);
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

//* TestTIdSipOutboundSession Private methods **********************************

procedure TestTIdSipOutboundSession.OnAuthenticationChallenge(UserAgent: TIdSipAbstractUserAgent;
                                                              Challenge: TIdSipResponse;
                                                              var Username: String;
                                                              var Password: String;
                                                              var TryAgain: Boolean);
begin
end;

procedure TestTIdSipOutboundSession.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractUserAgent;
                                                              Message: TIdSipMessage;
                                                              Receiver: TIdSipTransport);
begin
  Self.OnDroppedMessage := true;
end;

procedure TestTIdSipOutboundSession.OnInboundCall(UserAgent: TIdSipAbstractUserAgent;
                                                  Session: TIdSipInboundSession);
begin
end;

procedure TestTIdSipOutboundSession.ReceiveRemoteDecline;
var
  Decline: TIdSipResponse;
begin
  Decline := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                         SIPDecline);
  try
    Self.ReceiveResponse(Decline);
  finally
    Decline.Free;
  end;
end;

procedure TestTIdSipOutboundSession.ReceiveForbidden;
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Self.LastSentRequest,
                                       SIPForbidden);
  try
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSession.ReceiveMovedTemporarily(const Contact: String);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                          SIPMovedTemporarily);
  try
    Response.AddHeader(ContactHeaderFull).Value := Contact;
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSession.ReceiveMovedTemporarily(const Contacts: array of String);
var
  I:        Integer;
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                          SIPMovedTemporarily);
  try
    for I := Low(Contacts) to High(Contacts) do
      Response.AddHeader(ContactHeaderFull).Value := Contacts[I];
      
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;

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

//* TestTIdSipOutboundSession Published methods ********************************

procedure TestTIdSipOutboundSession.TestAck;
var
  Ack:    TIdSipRequest;
  Invite: TIdSipRequest;
begin
  Invite := TIdSipRequest.Create;
  try
    Invite.Assign(Self.LastSentRequest);

    Self.ReceiveOk(Self.LastSentRequest);

    Ack := Self.Dispatcher.Transport.LastACK;

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
  Ack := Self.Dispatcher.Transport.LastACK;

  Check(not Ack.Route.IsEmpty, 'No Route headers');
end;

procedure TestTIdSipOutboundSession.TestAckWithAuthorization;
var
  Ack:    TIdSipRequest;
  Invite: TIdSipRequest;
begin
  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');

  Invite := TIdSipRequest.Create;
  try
    Invite.Assign(Self.LastSentRequest);

    Self.ReceiveOk(Self.LastSentRequest);

    Ack := Self.LastSentRequest;

    Check(Ack.HasAuthorization, 'ACK lacks Authorization header');
    CheckEquals(Invite.FirstAuthorization.FullValue,
                Ack.FirstAuthorization.FullValue,
                'Authorization');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestAckWithProxyAuthorization;
var
  Ack:    TIdSipRequest;
  Invite: TIdSipRequest;
begin
  Self.ReceiveUnauthorized(ProxyAuthenticateHeader, '');

  Invite := TIdSipRequest.Create;
  try
    Invite.Assign(Self.LastSentRequest);

    Self.ReceiveOk(Self.LastSentRequest);

    Ack := Self.LastSentRequest;

    Check(Ack.HasProxyAuthorization, 'ACK lacks Proxy-Authorization header');
    CheckEquals(Invite.FirstProxyAuthorization.FullValue,
                Ack.FirstProxyAuthorization.FullValue,
                'Proxy-Authorization');
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

  CheckEquals(Self.Dispatcher.Transport.LastResponse.CallID,
              Session.Dialog.ID.CallID,
              'Dialog''s Call-ID');
  CheckEquals(Self.Dispatcher.Transport.LastResponse.From.Tag,
              Session.Dialog.ID.LocalTag,
              'Dialog''s Local Tag');
  CheckEquals(Self.Dispatcher.Transport.LastResponse.ToHeader.Tag,
              Session.Dialog.ID.RemoteTag,
              'Dialog''s Remote Tag');

  Answer     := TIdSipTestResources.BasicSDP('sip.fried-neurons.org');
  AnswerType := SdpMimeType;
  Self.ReceiveOkWithBody(Invite, Answer, AnswerType);

  CheckEquals(Answer,
              Session.RemoteSessionDescription,
              'RemoteSessionDescription');
  CheckEquals(AnswerType,
              Session.RemoteMimeType,
              'RemoteMimeType');

  Check(not Session.IsEarly, 'Dialog in incorrect state: shouldn''t be early');
end;

procedure TestTIdSipOutboundSession.TestCallRemoteRefusal;
begin
  Self.ReceiveForbidden;

  Check(Self.OnEndedSessionFired, 'OnEndedSession wasn''t triggered');
end;

procedure TestTIdSipOutboundSession.TestCallNetworkFailure;
var
  SessionCount: Cardinal;
begin
  SessionCount := Self.Core.SessionCount;
  Self.Dispatcher.Transport.FailWith := EIdConnectTimeout;

  Self.CreateAction;

  CheckEquals(SessionCount,
              Self.Core.SessionCount,
              'Core should have axed the failed session');
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

procedure TestTIdSipOutboundSession.TestCallSipsUriOverTcp;
var
  SentInvite: TIdSipRequest;
  Session:    TIdSipSession;
begin
  Self.Dispatcher.TransportType := TcpTransport;
  Self.Destination.Address.Scheme := SipsScheme;

  Self.MarkSentRequestCount;

  Session := Self.CreateAction as TIdSipSession;

  CheckRequestSent('INVITE wasn''t sent');
  SentInvite := Self.LastSentRequest;

  Self.ReceiveRinging(SentInvite);

  Check(not Session.Dialog.IsSecure, 'Dialog secure when TCP used');
end;

procedure TestTIdSipOutboundSession.TestCallSipUriOverTls;
var
  Response: TIdSipResponse;
  Session:  TIdSipSession;
begin
  Self.Dispatcher.TransportType := TcpTransport;

  Session := Self.CreateAction as TIdSipSession;

  Response := Self.Core.CreateResponse(Self.LastSentRequest,
                                       SIPOK);
  try
    Response.FirstContact.Address.Scheme := SipsScheme;
    Self.MarkSentAckCount;
    Self.ReceiveResponse(Response);
    CheckAckSent('No ACK sent: ' + Self.FailReason);

    Check(not Session.Dialog.IsSecure, 'Dialog secure when TLS used with a SIP URI');
  finally
    Response.Free;
  end;
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

  Self.ReceiveMovedTemporarily('sip:foo@bar.org');
  Self.ReceiveMovedTemporarily('sip:bar@bar.org');

  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily('sip:foo@bar.org');
  CheckNoRequestSent('The session accepted the run-around');
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

  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily('sip:foo@bar.org');
  CheckRequestSent('No redirected INVITE #1 sent: ' + Self.FailReason);
  CheckEquals('sip:foo@bar.org',
              Self.LastSentRequest.RequestUri.Uri,
              'Request-URI of redirect #1');

  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily('sip:baz@quaax.org');
  CheckRequestSent('No redirected INVITE #2 sent: ' + Self.FailReason);
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
              TIdSipOutboundSession.Method,
              'Outbound session; Method');
end;

procedure TestTIdSipOutboundSession.TestModifyUsesAuthentication;
var
  Invite: TIdSipRequest;
  Modify: TIdSipRequest;
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
    Self.MarkSentRequestCount;

    Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');

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
  Self.Dispatcher.Transport.FailWith := Exception;
  Self.ReceiveOk(Self.LastSentRequest);

  Check(Assigned(Self.ActionParam), 'OnNetworkFailure didn''t fire');
  Check(Self.ActionParam = Self.Session,
        'Session must signal the network error as _its_ error, not the '
      + 'Invite''s'); 
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
    CheckAckSent('Retransmission');

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
  InviteCount  := Self.Core.InviteCount;
  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily(Contact);

  CheckRequestSent('No new INVITE sent: ' + Self.FailReason);
  CheckEquals(InviteCount,
              Self.Core.InviteCount,
              'The Core should have one new INVITE and have destroyed one old one');

  RequestUri := Self.LastSentRequest.RequestUri;
  CheckEquals(Contact,
              RequestUri.Uri,
              'Request-URI');

  Self.ReceiveForbidden;
  Check(Self.Core.InviteCount < InviteCount,
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
  finally
    Redirect.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestReceiveFailureResponseNotifiesOnce;
var
  L:       TIdSipTestSessionListenerEndedCounter;
  Session: TIdSipOutboundSession;
begin
  Session := Self.Core.Call(Self.Destination, Self.SDP, SdpMimeType);
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
  InviteCount  := Self.Core.InviteCount;
  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily(Contact);

  CheckRequestSent('No new INVITE sent: ' + Self.FailReason);
  CheckEquals(InviteCount,
              Self.Core.InviteCount,
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

  SetLength(Contacts, 3);
  Contacts[0] := 'sip:foo@bar.org';
  Contacts[1] := 'sip:bar@bar.org';
  Contacts[2] := 'sip:baz@bar.org';

  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily(Contacts);

  // ARG! Why do they make Length return an INTEGER? And WHY Abs() too?
  CheckEquals(Self.RequestCount + Cardinal(Length(Contacts)),
              Self.Dispatcher.Transport.SentRequestCount,
              'Session didn''t attempt to contact all Contacts: ' + Self.FailReason);

  Self.MarkSentRequestCount;
  Self.ReceiveOkFrom(Self.SentRequestAt(SecondRedirect), Contacts[1]);
  Self.ReceiveOkFrom(Self.SentRequestAt(FirstRedirect), Contacts[0]);
  Self.ReceiveTryingFrom(Self.SentRequestAt(ThirdRedirect), Contacts[2]);

  // We expect a BYE in response to the 1st UA's 2xx and a CANCEL to the 2nd
  // UA's 1xx.

  // ARG! Why do they make Length return an INTEGER? And WHY Abs() too?
  CheckEquals(Self.RequestCount + Cardinal(Length(Contacts) - 1),
              Self.Dispatcher.Transport.SentRequestCount,
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
              Self.Dispatcher.Transport.SentRequestCount,
              'Session didn''t attempt to contact all Contacts: ' + Self.FailReason);
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
              Self.Dispatcher.Transport.SentRequestCount,
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

procedure TestTIdSipOutboundSession.TestTerminateUnestablishedSession;
var
  Invite:            TIdSipRequest;
  Request:           TIdSipRequest;
  RequestTerminated: TIdSipResponse;
  SessionCount:      Integer;
begin
  // When you Terminate a Session, the Session should attempt to CANCEL its
  // initial INVITE (if it hasn't yet received a final response).

  Self.MarkSentRequestCount;

  Invite := TIdSipRequest.Create;
  try
    Invite.Assign(Self.LastSentRequest);

    // We don't actually send CANCELs when we've not received a provisional
    // response.
    Self.ReceiveRinging(Invite);

    RequestTerminated := TIdSipResponse.InResponseTo(Invite, SIPRequestTerminated);
    try
      RequestTerminated.ToHeader.Tag := Self.Session.Dialog.ID.RemoteTag;

      SessionCount := Self.Core.SessionCount;
      Self.Session.Terminate;

      CheckRequestSent('no CANCEL sent');

      Request := Self.LastSentRequest;
      CheckEquals(MethodCancel,
                  Request.Method,
                  'Session didn''t terminate with a CANCEL');

      Self.ReceiveResponse(RequestTerminated);

      Check(Self.Core.SessionCount < SessionCount,
            'Session not marked as terminated');
    finally
      RequestTerminated.Free;
    end;
  finally
    Invite.Free;
  end;
end;

//******************************************************************************
//* TActionMethodTestCase                                                      *
//******************************************************************************
//* TActionMethodTestCase Public methods ***************************************

procedure TActionMethodTestCase.SetUp;
begin
  inherited SetUp;

  Self.Dispatcher := TIdSipMockTransactionDispatcher.Create;

  Self.UA := TIdSipUserAgent.Create;
  Self.UA.Dispatcher := Self.Dispatcher;

  Self.Response := TIdSipResponse.Create;
end;

procedure TActionMethodTestCase.TearDown;
begin
  Self.Response.Free;
  Self.UA.Free;
  Self.Dispatcher.Free;  

  inherited TearDown;
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
  Self.Method.Invite := TIdSipInboundInvite.Create(Self.UA, Self.Invite);
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

//* TestTIdSipInviteDialogEstablishedMethod Published methods ************************

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
//* TestTIdSipInviteFailureMethod                                              *
//******************************************************************************
//* TestTIdSipInviteFailureMethod Public methods *******************************

procedure TestTIdSipInviteFailureMethod.SetUp;
var
  Nowhere: TIdSipAddressHeader;
begin
  inherited SetUp;

  Self.Method := TIdSipInviteFailureMethod.Create;

  Nowhere := TIdSipAddressHeader.Create;
  try
    Self.Method.Invite   := Self.UA.AddOutboundAction(TIdSipOutboundInitialInvite) as TIdSipOutboundInitialInvite;
    Self.Method.Reason   := 'none';
    Self.Method.Response := Self.Response;
  finally
    Nowhere.Free;
  end;
end;

procedure TestTIdSipInviteFailureMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipInviteFailureMethod Published methods ****************************

procedure TestTIdSipInviteFailureMethod.TestRun;
var
  Listener: TIdSipTestInviteListener;
begin
  Listener := TIdSipTestInviteListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.Failure, 'Listener not notified');
    Check(Self.Method.Invite = Listener.InviteAgentParam,
          'InviteAgent param');
    Check(Self.Method.Response = Listener.ResponseParam,
          'Response param');
    CheckEquals(Self.Method.Reason,
                Listener.ReasonParam,
                'Reason param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipInviteRedirectMethod                                             *
//******************************************************************************
//* TestTIdSipInviteRedirectMethod Public methods ******************************

procedure TestTIdSipInviteRedirectMethod.SetUp;
var
  Nowhere: TIdSipAddressHeader;
begin
  inherited SetUp;

  Self.Method := TIdSipInviteRedirectMethod.Create;

  Nowhere := TIdSipAddressHeader.Create;
  try
    Self.Method.Invite := TIdSipOutboundInvite.Create(Self.UA);
  finally
    Nowhere.Free;
  end;

  Self.Method.Response := Self.Response;
end;

procedure TestTIdSipInviteRedirectMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipInviteRedirectMethod Published methods ***************************

procedure TestTIdSipInviteRedirectMethod.Run;
var
  Listener: TIdSipTestInviteListener;
begin
  Listener := TIdSipTestInviteListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.Redirect, 'Listener not notified');
    Check(Self.Method.Invite = Listener.InviteAgentParam,
          'Invite param');
    Check(Self.Method.Response = Listener.ResponseParam,
          'Response param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipInviteSuccessMethod                                              *
//******************************************************************************
//* TestTIdSipInviteSuccessMethod Public methods *******************************

procedure TestTIdSipInviteSuccessMethod.SetUp;
var
  Nowhere: TIdSipAddressHeader;
begin
  inherited SetUp;

  Self.Method := TIdSipInviteSuccessMethod.Create;

  Nowhere := TIdSipAddressHeader.Create;
  try
    Self.Method.Invite   := TIdSipOutboundInvite.Create(Self.UA);
    Self.Method.Response := Self.Response;
  finally
    Nowhere.Free;
  end;
end;

procedure TestTIdSipInviteSuccessMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipInviteSuccessMethod Published methods ****************************

procedure TestTIdSipInviteSuccessMethod.TestRun;
var
  Listener: TIdSipTestInviteListener;
begin
  Listener := TIdSipTestInviteListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.Success, 'Listener not notified');
    Check(Self.Method.Invite = Listener.InviteAgentParam,
          'InviteAgent param');
    Check(Self.Method.Response = Listener.ResponseParam,
          'Response param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipOptionsResponseMethod                                            *
//******************************************************************************
//* TestTIdSipOptionsResponseMethod Public methods *****************************

procedure TestTIdSipOptionsResponseMethod.SetUp;
var
  Nowhere: TIdSipAddressHeader;
begin
  inherited SetUp;

  Self.Method := TIdSipOptionsResponseMethod.Create;

  Nowhere := TIdSipAddressHeader.Create;
  try
    Self.Method.Options  := Self.UA.QueryOptions(Nowhere);
    Self.Method.Response := Self.Response;
  finally
    Nowhere.Free;
  end;
end;

procedure TestTIdSipOptionsResponseMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipOptionsResponseMethod Published methods **************************

procedure TestTIdSipOptionsResponseMethod.TestRun;
var
  Listener: TIdSipTestOptionsListener;
begin
  Listener := TIdSipTestOptionsListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.Response, 'Listener not notified');
    Check(Self.Method.Options = Listener.OptionsAgentParam,
          'OptionsAgent param');
    Check(Self.Method.Response = Listener.ResponseParam,
          'Response param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestRegistrationMethod                                                     *
//******************************************************************************
//* TestRegistrationMethod Public methods **************************************

procedure TestRegistrationMethod.SetUp;
var
  Registrar: TIdSipUri;
begin
  inherited SetUp;

  Registrar := TIdSipUri.Create;
  try
    Reg := Self.UA.RegisterWith(Registrar);
  finally
    Registrar.Free;
  end;

  Self.Bindings := TIdSipContacts.Create;
end;

procedure TestRegistrationMethod.TearDown;
begin
  Self.Bindings.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipRegistrationFailedMethod                                         *
//******************************************************************************
//* TestTIdSipRegistrationFailedMethod Public methods **************************

procedure TestTIdSipRegistrationFailedMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipRegistrationFailedMethod.Create;
  Self.Method.CurrentBindings := Self.Bindings;
  Self.Method.Reason          := 'No good reason';
  Self.Method.Registration    := Self.Reg;
end;

procedure TestTIdSipRegistrationFailedMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipRegistrationFailedMethod Published methods ***********************

procedure TestTIdSipRegistrationFailedMethod.TestRun;
var
  L: TIdSipTestRegistrationListener;
begin
  L := TIdSipTestRegistrationListener.Create;
  try
    Self.Method.Run(L);

    Check(L.Failure, 'Listener not notified');
    Check(Self.Method.CurrentBindings = L.CurrentBindingsParam,
          'CurrentBindings param');
    Check(Self.Method.Registration = L.RegisterAgentParam,
          'RegisterAgent param');
    CheckEquals(Self.Method.Reason, L.ReasonParam,
          'Reason param');
  finally
    L.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipRegistrationSucceededMethod                                      *
//******************************************************************************
//* TestTIdSipRegistrationSucceededMethod Public methods ***********************

procedure TestTIdSipRegistrationSucceededMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipRegistrationSucceededMethod.Create;
  Self.Method.CurrentBindings := Self.Bindings;
  Self.Method.Registration    := Self.Reg;
end;

procedure TestTIdSipRegistrationSucceededMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipRegistrationSucceededMethod Published methods ********************

procedure TestTIdSipRegistrationSucceededMethod.TestRun;
var
  L: TIdSipTestRegistrationListener;
begin
  L := TIdSipTestRegistrationListener.Create;
  try
    Self.Method.Run(L);

    Check(L.Success, 'Listener not notified');
    Check(Self.Method.CurrentBindings = L.CurrentBindingsParam,
          'CurrentBindings param');
    Check(Self.Method.Registration = L.RegisterAgentParam,
          'RegisterAgent param');
  finally
    L.Free;
  end;
end;

//******************************************************************************
//* TestSessionMethod                                                          *
//******************************************************************************
//* TestSessionMethod Public methods *******************************************

procedure TestSessionMethod.SetUp;
begin
  inherited SetUp;

  Self.Session := TIdSipOutboundSession.Create(Self.UA);
end;

procedure TestSessionMethod.TearDown;
begin
  Self.Session.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipEndedSessionMethod                                               *
//******************************************************************************
//* TestTIdSipEndedSessionMethod Public methods ********************************

procedure TestTIdSipEndedSessionMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipEndedSessionMethod.Create;

  Self.Method.Session := Self.Session;
  Self.Method.Reason  := 'No reason';
end;

procedure TestTIdSipEndedSessionMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipEndedSessionMethod Published methods *****************************

procedure TestTIdSipEndedSessionMethod.TestRun;
var
  L: TIdSipTestSessionListener;
begin
  L := TIdSipTestSessionListener.Create;
  try
    Self.Method.Run(L);

    Check(Self.Method.Session = L.SessionParam,
          'Session param');
    CheckEquals(Self.Method.Reason,
                L.ReasonParam,
                'Reason param');
  finally
    L.Free;
  end;
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
var
  L: TIdSipTestSessionListener;
begin
  L := TIdSipTestSessionListener.Create;
  try
    Self.Method.Run(L);

    Check(Self.Method.Session = L.SessionParam,
          'Session param');
    CheckEquals(Self.Method.MimeType,
                L.MimeType,
                'MimeType param');
    CheckEquals(Self.Method.RemoteSessionDescription,
                L.RemoteSessionDescription,
                'RemoteSessionDescription param');
  finally
    L.Free;
  end;
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
var
  L: TIdSipTestSessionListener;
begin
  L := TIdSipTestSessionListener.Create;
  try
    Self.Method.Run(L);

    Check(Self.Method.Answer = L.AnswerParam,
          'Answer param');
    Check(Self.Method.Session = L.SessionParam,
          'Session param');
  finally
    L.Free;
  end;
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

  Invite := TIdSipTestResources.CreateBasicRequest;
  try
    Self.Modify := Self.UA.AddInboundInvite(Invite);
  finally
    Invite.Free;
  end;

  Self.Method := TIdSipSessionModifySessionMethod.Create;

  Self.Method.Modify := Self.Modify;
end;

procedure TestTIdSipSessionModifySessionMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipSessionModifySessionMethod Published methods *********************

procedure TestTIdSipSessionModifySessionMethod.TestRun;
var
  L: TIdSipTestSessionListener;
begin
  L := TIdSipTestSessionListener.Create;
  try
    Self.Method.Run(L);

    Check(Self.Method.Modify = L.ModifyParam,
          'Modify param');
  finally
    L.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipUserAgentAuthenticationChallengeMethod                           *
//******************************************************************************
//* TestTIdSipUserAgentAuthenticationChallengeMethod Public methods ************

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.SetUp;
begin
  inherited SetUp;

  Self.Challenge := TIdSipResponse.Create;
  Self.UserAgent := TIdSipUserAgent.Create;
  Self.Method := TIdSipUserAgentAuthenticationChallengeMethod.Create;

  Self.Method.UserAgent := Self.UserAgent;

  Self.Method.Challenge := Self.Challenge;

  Self.L1 := TIdSipTestUserAgentListener.Create;
  Self.L2 := TIdSipTestUserAgentListener.Create;
end;

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TearDown;
begin
  Self.L2.Free;
  Self.L1.Free;
  Self.Method.Free;
  Self.UserAgent.Free;
  Self.Challenge.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgentAuthenticationChallengeMethod Published methods **********

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TestFirstListenerDoesntSetPassword;
begin
  Self.L2.Password := 'foo';

  Self.Method.Run(Self.L1);
  Self.Method.Run(Self.L2);

  CheckEquals(Self.L2.Password,
              Self.Method.FirstPassword,
              '2nd listener didn''t set password');
end;

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TestFirstListenerSetsPassword;
begin
  Self.L1.Password := 'foo';
  Self.L2.Password := 'bar';

  Self.Method.Run(Self.L1);
  Self.Method.Run(Self.L2);

  CheckEquals(Self.L1.Password,
              Self.Method.FirstPassword,
              'Returned password not 1st listener''s');
end;

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TestFirstListenerDoesntSetUsername;
begin
  Self.L2.Username := 'foo';

  Self.Method.Run(Self.L1);
  Self.Method.Run(Self.L2);

  CheckEquals(Self.L2.Username,
              Self.Method.FirstUsername,
              '2nd listener didn''t set Username');
end;

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TestFirstListenerSetsUsername;
begin
  Self.L1.Username := 'foo';
  Self.L2.Username := 'bar';

  Self.Method.Run(Self.L1);
  Self.Method.Run(Self.L2);

  CheckEquals(Self.L1.Username,
              Self.Method.FirstUsername,
              'Returned Username not 1st listener''s');
end;

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TestRun;
begin
  Self.L1.Password := 'foo';
  Self.L1.Username := 'foo';
  Self.L2.Password := 'bar';
  Self.L2.Username := 'bar';

  Self.Method.Run(Self.L1);
  Check(Self.L1.AuthenticationChallenge,
        'L1 not notified');
  CheckEquals(Self.L1.Password,
              Self.Method.FirstPassword,
              'L1 gives us the first password');
  CheckEquals(Self.L1.Username,
              Self.Method.FirstUsername,
              'L1 gives us the first username');

  Self.Method.Run(Self.L2);
  Check(Self.L2.AuthenticationChallenge,
        'L2 not notified');

  CheckEquals(Self.L1.Password,
              Self.Method.FirstPassword,
              'We ignore L2''s password');

  CheckEquals(Self.L1.Username,
              Self.Method.FirstUsername,
              'We ignore L2''s username');
end;

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TestTryAgain;
begin
  Self.L1.TryAgain := true;

  Self.Method.Run(Self.L1);

  Check(Self.Method.TryAgain, 'TryAgain not set');
end;

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TestNoListenerSetsPassword;
begin
  Self.Method.Run(Self.L1);
  Self.Method.Run(Self.L2);

  CheckEquals('',
              Self.Method.FirstPassword,
              'Something other than the listeners set the password');

  CheckEquals('',
              Self.Method.FirstUsername,
              'Something other than the listeners set the username');
end;

//******************************************************************************
//* TestTIdSipUserAgentDroppedUnmatchedMessageMethod                           *
//******************************************************************************
//* TestTIdSipUserAgentDroppedUnmatchedMessageMethod Public methods ************

procedure TestTIdSipUserAgentDroppedUnmatchedMessageMethod.SetUp;
begin
  inherited SetUp;

  Self.Receiver := TIdSipMockUdpTransport.Create;
  Self.Response := TIdSipResponse.Create;

  Self.Method := TIdSipUserAgentDroppedUnmatchedMessageMethod.Create;
  Self.Method.Receiver := Self.Receiver;
  Self.Method.Message := Self.Response.Copy;
end;

procedure TestTIdSipUserAgentDroppedUnmatchedMessageMethod.TearDown;
begin
  Self.Method.Free;
  Self.Response.Free;
  Self.Receiver.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgentDroppedUnmatchedMessageMethod Published methods *********

procedure TestTIdSipUserAgentDroppedUnmatchedMessageMethod.TestRun;
var
  L: TIdSipTestUserAgentListener;
begin
  L := TIdSipTestUserAgentListener.Create;
  try
    Self.Method.Run(L);

    Check(L.DroppedUnmatchedMessage, 'Listener not notified');
    Check(Self.Method.Receiver = L.ReceiverParam,
          'Receiver param');
    Check(Self.Method.Message = L.MessageParam,
          'Message param');
    Check(Self.Method.UserAgent = L.UserAgentParam,
          'UserAgent param');
  finally
    L.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipUserAgentInboundCallMethod                                       *
//******************************************************************************
//* TestTIdSipUserAgentInboundCallMethod Public methods ************************

procedure TestTIdSipUserAgentInboundCallMethod.SetUp;
begin
  inherited SetUp;

  Self.Request := TIdSipTestResources.CreateBasicRequest;

  Self.Dispatcher.MockLocator.AddA(Self.Request.LastHop.SentBy, '127.0.0.1');

  Self.Session := TIdSipInboundSession.Create(Self.UA,
                                              Self.Request,
                                              false);
  Self.Method := TIdSipUserAgentInboundCallMethod.Create;
  Self.Method.Session := Self.Session;
end;

procedure TestTIdSipUserAgentInboundCallMethod.TearDown;
begin
  Self.Method.Free;
  Self.Session.Free;
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgentInboundCallMethod Published methods *********************

procedure TestTIdSipUserAgentInboundCallMethod.TestRun;
var
  L: TIdSipTestUserAgentListener;
begin
  L := TIdSipTestUserAgentListener.Create;
  try
    Self.Method.Run(L);

    Check(L.InboundCall, 'Listener not notified');
    Check(Self.Method.Session = L.SessionParam,
          'Session param');
    Check(Self.Method.UserAgent = L.UserAgentParam,
          'UserAgent param');
  finally
    L.Free;
  end;
end;

initialization
  RegisterTest('Transaction User Cores', Suite);
end.
