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
  IdSipDialogID, IdSipMessage, IdSipMockCore, IdSipMockTransactionDispatcher,
  IdSipRegistration, IdSipTransaction, IdSipTransport, IdTimerQueue, SyncObjs,
  TestFramework, TestFrameworkEx, TestFrameworkSip;

type
  TTestCaseTU = class(TTestCaseSip)
  private
    procedure RemoveBody(Msg: TIdSipMessage);
  protected
    AckCount:      Cardinal;
    Core:          TIdSipUserAgentCore;
    Destination:   TIdSipToHeader;
    Dispatcher:    TIdSipMockTransactionDispatcher;
    Invite:        TIdSipRequest;
    RequestCount:  Cardinal;
    ResponseCount: Cardinal;

    function  CreateRemoteBye(LocalDialog: TIdSipDialog): TIdSipRequest;
    function  CreateRemoteOk(Invite: TIdSipRequest): TIdSipResponse;
    function  LastSentAck: TIdSipRequest;
    function  LastSentRequest: TIdSipRequest;
    function  LastSentResponse: TIdSipResponse;
    procedure MarkSentAckCount;
    procedure MarkSentRequestCount;
    procedure MarkSentResponseCount;
    procedure SendRequest(Request: TIdSipRequest);
    procedure SendResponse(Response: TIdSipResponse);
    function  SentAckCount: Cardinal;
    function  SentRequestCount: Cardinal;
    function  SentResponseCount: Cardinal;
    procedure SimulateAck;
    procedure SimulateAckFor(Request: TIdSipRequest;
                             Response: TIdSipResponse);
    procedure SimulateRemoteBye(LocalDialog: TIdSipDialog);
    procedure SimulateRemoteCancel;
    procedure SimulateRemoteInvite;

    procedure SimulateRemoteAccept(Invite: TIdSipRequest);
    procedure SimulateMovedPermanently(const SipUrl: String);
    procedure SimulateRemoteResponse(StatusCode: Cardinal); overload;
    procedure SimulateRemoteResponse(Response: TIdSipResponse); overload;
    procedure SimulateRemoteRinging(Invite: TIdSipRequest);
    procedure SimulateRemoteTryingWithNoToTag(Invite: TIdSipRequest);
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckAckSent(const Msg: String);
    procedure CheckNoRequestSent(const Msg: String);
    procedure CheckRequestSent(const Msg: String);
    procedure CheckNoResponseSent(const Msg: String);
    procedure CheckResponseSent(const Msg: String);
  end;

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

  TestTIdSipAbstractUserAgent = class(TestTIdSipAbstractCore)
  published
    procedure TestAddAllowedContentType;
    procedure TestAddAllowedContentTypeMalformed;
    procedure TestAddAllowedLanguage;
    procedure TestAddAllowedLanguageLanguageAlreadyPresent;
    procedure TestAddAllowedMethod;
    procedure TestAddAllowedMethodMethodAlreadyPresent;
    procedure TestAddAllowedScheme;
    procedure TestAddAllowedSchemeSchemeAlreadyPresent;
    procedure TestAuthenticateWithNoAttachedAuthenticator;
    procedure TestCleanOutTerminatedActions;
    procedure TestRejectMalformedAuthorizedRequest;
    procedure TestRejectUnauthorizedRequest;
  end;

  TestTIdSipUserAgentCore = class(TTestCaseTU,
                                  IIdObserver,
                                  IIdSipTransportSendingListener,
                                  IIdSipSessionListener,
                                  IIdSipUserAgentListener)
  private
    Dlg:                 TIdSipDialog;
    ID:                  TIdSipDialogID;
    LocalSequenceNo:     Cardinal;
    LocalUri:            TIdSipURI;
    OnChangedEvent:      TEvent;
    OnEndedSessionFired: Boolean;
    OnInboundCallFired:  Boolean;
    RemoteSequenceNo:    Cardinal;
    RemoteTarget:        TIdSipURI;
    RemoteUri:           TIdSipURI;
    RouteSet:            TIdSipHeaders;
    SendEvent:           TEvent;
    Session:             TIdSipInboundSession;
    SessionEstablished:  Boolean;

    procedure CheckCommaSeparatedHeaders(const ExpectedValues: String;
                                         Header: TIdSipHeader;
                                         const Msg: String);
    procedure CheckCreateRequest(Dest: TIdSipToHeader;
                                 Request: TIdSipRequest);
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Challenge: TIdSipResponse;
                                        var Username: String;
                                        var Password: String);
    procedure OnChanged(Observed: TObject);
    procedure OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                         Receiver: TIdSipTransport);
    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String);
    procedure OnEstablishedSession(Session: TIdSipSession);
    procedure OnInboundCall(Session: TIdSipInboundSession);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Answer: TIdSipResponse);
    procedure OnModifySession(Modify: TIdSipInboundInvite);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport);
    procedure SimulateRemoteBye(Dialog: TIdSipDialog);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddObserver;
    procedure TestAddUserAgentListener;
    procedure TestCallUsingProxy;
    procedure TestCancelNotifiesTU;
    procedure TestContentTypeDefault;
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
    procedure TestDialogLocalSequenceNoMonotonicallyIncreases;
    procedure TestDispatchToCorrectSession;
    procedure TestDoNotDisturb;
    procedure TestHasUnknownContentEncoding;
    procedure TestHasUnknownContentType;
    procedure TestInviteExpires;
    procedure TestInviteRaceCondition;
    procedure TestIsMethodAllowed;
    procedure TestIsSchemeAllowed;
    procedure TestLoopDetection;
    procedure TestNotificationOfNewSession;
    procedure TestNotificationOfNewSessionRobust;
    procedure TestOutboundInviteSessionProgressResends;
    procedure TestReceiveByeForUnmatchedDialog;
    procedure TestReceiveByeForDialog;
    procedure TestReceiveByeWithoutTags;
    procedure TestReceiveOptions;
    procedure TestReceiveResponseWithMultipleVias;
    procedure TestRejectNoContact;
    procedure TestRejectUnknownContentEncoding;
    procedure TestRejectUnknownContentLanguage;
    procedure TestRejectUnknownContentType;
    procedure TestRejectUnknownExtension;
    procedure TestRejectUnknownScheme;
    procedure TestRejectUnsupportedMethod;
    procedure TestRejectUnsupportedSipVersion;
    procedure TestRemoveObserver;
    procedure TestRemoveUserAgentListener;
    procedure TestSetContact;
    procedure TestSetContactMailto;
    procedure TestSetContactWildCard;
    procedure TestSetFrom;
    procedure TestSetFromMailto;
    procedure TestTerminateAllCalls;
    procedure TestViaMatchesTransportParameter;
  end;

  TestTIdSipAction = class(TTestCaseTU,
                           IIdSipActionListener)
  protected
    ActionFailed: Boolean;
    Challenged:   Boolean;
    Password:     String;
    Username:     String;

    function  CreateAction: TIdSipAction; virtual; abstract;
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Challenge: TIdSipResponse;
                                        var Username: String;
                                        var Password: String); virtual;
    procedure PerformAction(Action: TIdSipAction); virtual;
    procedure SimulateRejectProxyUnauthorized;
    procedure SimulateRemoteBadExtensionResponse;
    procedure SimulateRemoteOK;
    procedure SimulateRemoteOKFor(Request: TIdSipRequest);
  public
    procedure SetUp; override;

    procedure TestProxyAuthentication; virtual;
  published
    procedure TestIsInvite; virtual;
    procedure TestIsOptions; virtual;
    procedure TestIsRegistration; virtual;
    procedure TestIsSession; virtual;
{
    procedure TestReceiveResponseBadExtension; // Currently our stack can't sent Requires; ergo we can't test in the usual fashion
    procedure TestReceiveResponseBadExtensionWithoutRequires;
}
  end;

  TestTIdSipInboundInvite = class(TestTIdSipAction,
                                  IIdSipInboundInviteListener)
  private
    Dialog:       TIdSipDialog;
    Failed:       Boolean;
    InviteAction: TIdSipInboundInvite;

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
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
    procedure TestMatchAck;
    procedure TestMatchAckToReInvite;
    procedure TestMatchAckToReInviteWithDifferentCSeq;
    procedure TestMatchAckWithDifferentCSeq;
    procedure TestMethod;
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
    procedure PerformAction(Action: TIdSipAction); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddListener;
    procedure TestCancelAfterAccept;
    procedure TestCancelBeforeAccept;
    procedure TestCancelBeforeProvisional;
    procedure TestInviteThenReInvite;
    procedure TestInviteTwice;
    procedure TestIsInvite; override;
    procedure TestMethod;
    procedure TestProxyAuthentication; override;
    procedure TestReceiveGlobalFailed;
    procedure TestReceiveRedirect;
    procedure TestReceiveRequestFailed;
    procedure TestReceiveServerFailed;
    procedure TestRedirectedInvite;
    procedure TestReInviteThenInvite;
    procedure TestReInviteTwice;
    procedure TestRemoveListener;
    procedure TestTerminateBeforeAccept;
    procedure TestTerminateAfterAccept;
    procedure TestTransactionCompleted;
  end;

  TestTIdSipInboundOptions = class(TestTIdSipAction)
  private
    procedure SimulateRemoteOptions;
  published
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
    procedure TestOptions;
    procedure TestOptionsWhenDoNotDisturb;
  end;

  TestTIdSipOutboundOptions = class(TestTIdSipAction)
  protected
    function CreateAction: TIdSipAction; override;
    procedure PerformAction(Action: TIdSipAction); override;
  published
    procedure TestAddListener;
    procedure TestIsOptions; override;
    procedure TestProxyAuthentication; override;
    procedure TestRemoveListener;
  end;

  TestTIdSipRegistration = class(TestTIdSipAction)
  published
    procedure TestIsRegistration; override;
  end;

  TestTIdSipInboundRegistration = class(TestTIdSipRegistration)
  published
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
  end;

  TestTIdSipOutboundRegistration = class(TestTIdSipRegistration,
                                         IIdSipRegistrationListener)
  private
    Contacts:    TIdSipContacts;
    MinExpires:  Cardinal;
    Reg:         TIdSipOutboundRegistration;
    Registrar:   TIdSipUserAgentCore;
    Request:     TIdSipRequest;
    Succeeded:   Boolean;

    function  DigestForName(const Password: String): String;
    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts;
                        const Reason: String);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts);
    procedure SimulateRemoteIntervalTooBrief;
    procedure SimulateRemoteRejectProxyAuthenticationRequired;
  protected
    function CreateAction: TIdSipAction; override;
    procedure PerformAction(Action: TIdSipAction); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddListener;
    procedure TestCheckFirstListenerSetsPassword;
    procedure TestMethod;
    procedure TestRegister;
    procedure TestFindCurrentBindings;
    procedure TestProxyAuthentication; override;
    procedure TestReceiveFail;
    procedure TestReceiveIntervalTooBrief;
    procedure TestReceiveIntervalTooBriefForOneContact;
    procedure TestReceiveMovedPermanently;
    procedure TestReceiveOK;
    procedure TestReceiveUnauthorized;
    procedure TestRemoveListener;
    procedure TestSequenceNumberIncrements;
    procedure TestUnregister;
    procedure TestUsername;
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
    procedure OnEstablishedSession(Session: TIdSipSession); virtual;
    procedure OnModifiedSession(Session: TIdSipSession;
                                Answer: TIdSipResponse); virtual;
    procedure OnModifySession(Modify: TIdSipInboundInvite); virtual;
    procedure SimulateRemoteReInvite(Session: TIdSipSession);
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
    procedure TestRejectInviteWhenInboundModificationInProgress;
    procedure TestRejectInviteWhenOutboundModificationInProgress;
  end;

  TestTIdSipInboundSession = class(TestTIdSipSession,
                                   IIdRTPDataListener,
                                   IIdSipTransportSendingListener,
                                   IIdSipUserAgentListener)
  private
    SentRequestTerminated:  Boolean;
    Session:                TIdSipInboundSession;

    procedure OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                         Receiver: TIdSipTransport);
    procedure OnInboundCall(Session: TIdSipInboundSession);
    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdConnection);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport);
  protected
    procedure CheckResendWaitTime(Milliseconds: Cardinal;
                                  const Msg: String); override;
    function  CreateAction: TIdSipAction; override;
    procedure EstablishSession(Session: TIdSipSession); override;
    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String); override;
    procedure OnEstablishedSession(Session: TIdSipSession); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAcceptCall;
    procedure TestAddSessionListener;
    procedure TestCancelAfterAccept;
    procedure TestCancelBeforeAccept;
    procedure TestCancelNotifiesSession;
    procedure TestIsInboundCall;
    procedure TestIsOutboundCall;
    procedure TestMethod;
    procedure TestInboundModifyBeforeFullyEstablished;
    procedure TestInboundModifyReceivesNoAck;
    procedure TestReceiveBye;
//    procedure TestReceiveByeWithPendingRequests;
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
    OnDroppedResponse: Boolean;
    Session:           TIdSipOutboundSession;

    procedure OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                         Receiver: TIdSipTransport);
    procedure OnInboundCall(Session: TIdSipInboundSession);
    procedure SimulateRemoteDecline;
    procedure SimulateForbidden;
    procedure SimulateMovedTemporarily(const Contact: String);
    procedure SimulateRemoteOKWithRecordRoute;
    procedure SimulateRejectProxyUnauthorized;
    procedure SimulateRejectUnauthorized;
  protected
    SDP: String;

    procedure CheckResendWaitTime(Milliseconds: Cardinal;
                                  const Msg: String); override;
    function  CreateAction: TIdSipAction; override;
    procedure EstablishSession(Session: TIdSipSession); override;
    procedure PerformAction(Action: TIdSipAction); override;
  public
    procedure SetUp; override;
  published
    procedure TestAck;
    procedure TestAckFromRecordRouteResponse;
    procedure TestAckWithAuthorization;
    procedure TestAckWithProxyAuthorization;
    procedure TestCall;
    procedure TestCallTwice;
    procedure TestCallRemoteRefusal;
    procedure TestCallNetworkFailure;
    procedure TestCallSecure;
    procedure TestCallSipsUriOverTcp;
    procedure TestCallSipUriOverTls;
    procedure TestCallWithOffer;
    procedure TestGlobalFailureEndsSession;
    procedure TestHangUp;
    procedure TestIsInboundCall;
    procedure TestIsOutboundCall;
    procedure TestMethod;
    procedure TestModifyUsesAuthentication;
    procedure TestDialogNotEstablishedOnTryingResponse;
    procedure TestDoubleRedirect;
    procedure TestProxyAuthentication; override;
    procedure TestReceive2xxSendsAck;
    procedure TestReceive3xxSendsNewInvite;
    procedure TestReceive3xxWithOneContact;
    procedure TestReceive3xxWithNoContacts;
    procedure TestReceiveFinalResponseSendsAck;
    procedure TestRedirectAndAccept;
    procedure TestTerminateUnestablishedSession;
    procedure TestTerminateEstablishedSession;
  end;

  TIdModifyAuthHeaderProc = procedure(Auth: TIdSipAuthenticateHeader) of object;

  TestProxyAuthentication = class(TestTIdSipSession)
  private
    Opaque:   String;
    Password: String;
    Username: String;
    Session:  TIdSipOutboundSession;

  protected
    function  CreateAction: TIdSipAction; override;
    procedure EstablishSession(Session: TIdSipSession); override;
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Challenge: TIdSipResponse;
                                        var Username: String;
                                        var Password: String); override;
  public
    procedure SetUp; override;
  published
    procedure TestFailedAuthentication;
    procedure TestSuccessfulAuthentication;
  end;

  TestBugHunt = class(TTestCaseTU,
                      IIdSipUserAgentListener)
  private
    Session: TIdSipInboundSession;
    ToTag:   String;
  private
    function  CreateRemoteInvite: TIdSipRequest;
    procedure OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                         Receiver: TIdSipTransport);
    procedure OnInboundCall(Session: TIdSipInboundSession);
    procedure SimulateRemoteInvite;
    procedure SimulateRemoteOK;
    procedure SimulateRemoteRinging;
    procedure SimulateRemoteTrying;
  public
    procedure SetUp; override;
  published
    procedure TestOutboundCallAndByeToXlite;
    procedure TestSimultaneousInAndOutboundCall;
    procedure TestXlitesAckNonBug;
  end;

  TActionMethodTestCase = class(TTestCase)
  private
    Response: TIdSipResponse;
    UA:       TIdSipUserAgentCore;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipActionAuthenticationChallengeMethod = class(TActionMethodTestCase)
  private
    L1:     TIdSipTestRegistrationListener;
    L2:     TIdSipTestRegistrationListener;
    Method: TIdSipActionAuthenticationChallengeMethod;
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

  TestTIdSipUserAgentDroppedUnmatchedResponseMethod = class(TTestCase)
  private
    Method:   TIdSipUserAgentDroppedUnmatchedResponseMethod;
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
  IdException, IdGlobal, IdHashMessageDigest, IdInterfacedObject,
  IdSipAuthentication, IdSipConsts, IdSipMockTransport, IdUdpServer, SysUtils,
  TestMessages, Windows;

type
  TIdSipCoreWithExposedNotify = class(TIdSipAbstractCore)
  public
    function  CreateRequest(Dest: TIdSipAddressHeader): TIdSipRequest; overload; override;
    function  CreateRequest(Dialog: TIdSipDialog): TIdSipRequest; overload; override;
    procedure TriggerNotify;
  end;

const
  DefaultTimeout = 5000;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipCore unit tests');
  Result.AddTest(TestTIdSipAbstractCore.Suite);
  Result.AddTest(TestTIdSipAbstractUserAgent.Suite);
  Result.AddTest(TestTIdSipUserAgentCore.Suite);
  Result.AddTest(TestTIdSipInboundInvite.Suite);
  Result.AddTest(TestTIdSipOutboundInvite.Suite);
  Result.AddTest(TestTIdSipInboundOptions.Suite);
  Result.AddTest(TestTIdSipOutboundOptions.Suite);
  Result.AddTest(TestTIdSipInboundRegistration.Suite);
  Result.AddTest(TestTIdSipOutboundRegistration.Suite);
  Result.AddTest(TestTIdSipInboundSession.Suite);
  Result.AddTest(TestTIdSipOutboundSession.Suite);
  Result.AddTest(TestProxyAuthentication.Suite);
  Result.AddTest(TestBugHunt.Suite);
  Result.AddTest(TestTIdSipActionAuthenticationChallengeMethod.Suite);
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
  Result.AddTest(TestTIdSipUserAgentDroppedUnmatchedResponseMethod.Suite);
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
//* TTestCaseTU                                                                *
//******************************************************************************
//* TTestCaseTU Public methods *************************************************

procedure TTestCaseTU.SetUp;
begin
  inherited SetUp;

  Self.Destination := TIdSipToHeader.Create;
  Self.Destination.Value := 'sip:franks@localhost';

  Self.Dispatcher := TIdSipMockTransactionDispatcher.Create;
  Self.Dispatcher.Transport.LocalEchoMessages := false;
  Self.Dispatcher.Transport.TransportType := sttTCP;

  Self.Core := TIdSipUserAgentCore.Create;
  Self.Core.Dispatcher := Self.Dispatcher;

  Self.Core.Contact.Value := 'sip:wintermute@localhost';
  Self.Core.From.Value    := 'sip:wintermute@localhost';

  Self.Invite := TIdSipTestResources.CreateBasicRequest;
  Self.RemoveBody(Self.Invite);
end;

procedure TTestCaseTU.TearDown;
begin
  Self.Invite.Free;
  Self.Core.Free;
  Self.Dispatcher.Free;
  Self.Destination.Free;

  inherited TearDown;
end;

procedure TTestCaseTU.CheckAckSent(const Msg: String);
begin
  Check(Self.AckCount < Self.SentACKCount,
        Msg);
end;

procedure TTestCaseTU.CheckNoRequestSent(const Msg: String);
begin
  CheckEquals(Self.RequestCount,
              Self.SentRequestCount,
              Msg);
end;

procedure TTestCaseTU.CheckRequestSent(const Msg: String);
begin
  Check(Self.RequestCount < Self.SentRequestCount, Msg);
end;

procedure TTestCaseTU.CheckNoResponseSent(const Msg: String);
begin
  CheckEquals(Self.ResponseCount,
              Self.SentResponseCount,
              Msg);
end;

procedure TTestCaseTU.CheckResponseSent(const Msg: String);
begin
  Check(Self.ResponseCount < Self.SentResponseCount, Msg);
end;

//* TTestCaseTU Protected methods **********************************************

function TTestCaseTU.CreateRemoteBye(LocalDialog: TIdSipDialog): TIdSipRequest;
begin
  Result := Self.Core.CreateBye(LocalDialog);
  try
    Result.ToHeader.Tag := LocalDialog.ID.LocalTag;
    Result.From.Tag     := LocalDialog.ID.RemoteTag;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TTestCaseTU.CreateRemoteOk(Invite: TIdSipRequest): TIdSipResponse;
begin
  // This message appears to originate from the network. Invite originates from
  // us so presumably has no To tag. Having come from the network, the response
  // WILL have a To tag.
  Result := Self.Core.CreateResponse(Invite, SIPOK);
  Result.ToHeader.Tag := Self.Core.NextTag;
end;

function TTestCaseTU.LastSentAck: TIdSipRequest;
begin
  Result := Self.Dispatcher.Transport.LastACK;
end;

function TTestCaseTU.LastSentRequest: TIdSipRequest;
begin
  Result := Self.Dispatcher.Transport.LastRequest;
end;

function TTestCaseTU.LastSentResponse: TIdSipResponse;
begin
  Result := Self.Dispatcher.Transport.LastResponse;
end;

procedure TTestCaseTU.MarkSentAckCount;
begin
  Self.AckCount := Self.SentAckCount;
end;

procedure TTestCaseTU.MarkSentRequestCount;
begin
  Self.RequestCount := Self.SentRequestCount;
end;

procedure TTestCaseTU.MarkSentResponseCount;
begin
  Self.ResponseCount := Self.SentResponseCount;
end;

procedure TTestCaseTU.SendRequest(Request: TIdSipRequest);
begin
  Self.Dispatcher.Transport.FireOnRequest(Request);
end;

procedure TTestCaseTU.SendResponse(Response: TIdSipResponse);
begin
  Self.Dispatcher.Transport.FireOnResponse(Response);
end;

function TTestCaseTU.SentAckCount: Cardinal;
begin
  Result := Self.Dispatcher.Transport.ACKCount;
end;

function TTestCaseTU.SentRequestCount: Cardinal;
begin
  Result := Self.Dispatcher.Transport.SentRequestCount
end;

function TTestCaseTU.SentResponseCount: Cardinal;
begin
  Result := Self.Dispatcher.Transport.SentResponseCount
end;

procedure TTestCaseTU.SimulateAck;
var
  Ack: TIdSipRequest;
  T:   TIdSipMockTransport;
begin
  T := Self.Dispatcher.Transport;

  Ack := T.LastRequest.AckFor(T.LastResponse);
  try
    T.FireOnRequest(Ack);
  finally
    Ack.Free;
  end;
end;

procedure TTestCaseTU.SimulateAckFor(Request: TIdSipRequest;
                                     Response: TIdSipResponse);
var
  Ack: TIdSipRequest;
begin
  Ack := Request.AckFor(Response);
  try
    Self.SendRequest(Ack);
  finally
    Ack.Free;
  end;
end;

procedure TTestCaseTU.SimulateRemoteAccept(Invite: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  // This message appears to originate from the network. Invite originates from
  // us so presumably has no To tag. Having come from the network, the response
  // WILL have a To tag.
  Response := Self.CreateRemoteOk(Invite);
  try
    Self.SimulateRemoteResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TTestCaseTU.SimulateMovedPermanently(const SipUrl: String);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                          SIPMovedPermanently);
  try
    Response.AddHeader(ContactHeaderFull).Value := SipUrl;
    Self.SimulateRemoteResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TTestCaseTU.SimulateRemoteResponse(StatusCode: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Self.LastSentRequest,
                                       StatusCode);
  try
    Self.SimulateRemoteResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TTestCaseTU.SimulateRemoteResponse(Response: TIdSipResponse);
begin
  Self.SendResponse(Response);
end;

procedure TTestCaseTU.SimulateRemoteBye(LocalDialog: TIdSipDialog);
var
  Bye: TIdSipRequest;
begin
  Bye := Self.CreateRemoteBye(LocalDialog);
  try
    Self.SendRequest(Bye);
  finally
    Bye.Free;
  end;
end;

procedure TTestCaseTU.SimulateRemoteCancel;
var
  Cancel: TIdSipRequest;
begin
  Cancel := Self.Invite.CreateCancel;
  try
    Self.SendRequest(Cancel);
  finally
    Cancel.Free;
  end;
end;

procedure TTestCaseTU.SimulateRemoteInvite;
begin
  Self.SendRequest(Self.Invite);
end;

procedure TTestCaseTU.SimulateRemoteRinging(Invite: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Invite, SIPRinging);
  try
    Self.SimulateRemoteResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TTestCaseTU.SimulateRemoteTryingWithNoToTag(Invite: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Invite, SIPTrying);
  try
    // strip the To header tag
    Response.ToHeader.Value := Response.ToHeader.Value;

    Self.SimulateRemoteResponse(Response);
  finally
    Response.Free;
  end;
end;

//* TTestCaseTU Private methods ************************************************

procedure TTestCaseTU.RemoveBody(Msg: TIdSipMessage);
begin
  Msg.RemoveAllHeadersNamed(ContentTypeHeaderFull);
  Msg.Body := '';
  Msg.ToHeader.Value := Msg.ToHeader.DisplayName
                               + ' <' + Msg.ToHeader.Address.URI + '>';
  Msg.RemoveAllHeadersNamed(ContentTypeHeaderFull);
  Msg.ContentLength := 0;
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
  T: TIdTimerQueue;
begin
  T := TIdTimerQueue.Create;
  try
    Self.Core.Timer := T;
    Self.Core.ScheduleEvent(Self.ScheduledEvent, 50, nil);

    Self.ExceptionMessage := 'Waiting for scheduled event';
    Self.WaitForSignaled;
    Check(Self.ScheduledEventFired, 'Event didn''t fire');
  finally
    T.Terminate;
  end;
end;

//******************************************************************************
//* TestTIdSipAbstractUserAgent                                                *
//******************************************************************************
//* TestTIdSipAbstractUserAgent Published methods ******************************

procedure TestTIdSipAbstractUserAgent.TestAddAllowedContentType;
var
  ContentTypes: TStrings;
begin
  ContentTypes := TStringList.Create;
  try
    Self.Core.AddAllowedContentType(SdpMimeType);
    Self.Core.AddAllowedContentType(PlainTextMimeType);

    ContentTypes.CommaText := Self.Core.AllowedContentTypes;

    CheckEquals(2, ContentTypes.Count, 'Number of allowed ContentTypes');

    CheckEquals(SdpMimeType,       ContentTypes[0], SdpMimeType);
    CheckEquals(PlainTextMimeType, ContentTypes[1], PlainTextMimeType);
  finally
    ContentTypes.Free;
  end;
end;

procedure TestTIdSipAbstractUserAgent.TestAddAllowedContentTypeMalformed;
var
  ContentTypes: String;
begin
  ContentTypes := Self.Core.AllowedContentTypes;
  Self.Core.AddAllowedContentType(' ');
  CheckEquals(ContentTypes,
              Self.Core.AllowedContentTypes,
              'Malformed Content-Type was allowed');
end;

procedure TestTIdSipAbstractUserAgent.TestAddAllowedLanguage;
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

procedure TestTIdSipAbstractUserAgent.TestAddAllowedLanguageLanguageAlreadyPresent;
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

procedure TestTIdSipAbstractUserAgent.TestAddAllowedMethod;
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

procedure TestTIdSipAbstractUserAgent.TestAddAllowedMethodMethodAlreadyPresent;
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

procedure TestTIdSipAbstractUserAgent.TestAddAllowedScheme;
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

procedure TestTIdSipAbstractUserAgent.TestAddAllowedSchemeSchemeAlreadyPresent;
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

procedure TestTIdSipAbstractUserAgent.TestAuthenticateWithNoAttachedAuthenticator;
begin
  // We make sure that no access violations occur just because we've not
  // attached an authenticator to the Core.
  Self.Core.RequireAuthentication := true;
  Self.Invite.AddHeader(AuthorizationHeader);
  Self.SimulateRemoteInvite;
end;

procedure TestTIdSipAbstractUserAgent.TestCleanOutTerminatedActions;
var
  Session: TIdSipSession;
begin
  // This test doesn't really tell us much, just that CleanOutTerminatedActions
  // doesn't blow up when Core has terminated an unterminated actions.

  Session := Self.Core.Call(Self.Destination, '', '');
  Self.SimulateRemoteAccept(Self.LastSentRequest);
  Check(Session.DialogEstablished, 'Dialog not established for session');
  Self.SimulateRemoteBye(Session.Dialog);

  Self.Core.Call(Self.Destination, '', '');
  Self.Core.CleanOutTerminatedActions;
end;

procedure TestTIdSipAbstractUserAgent.TestRejectMalformedAuthorizedRequest;
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
    Self.SimulateRemoteInvite;
    CheckResponseSent('No response sent');

    Response := Self.LastSentResponse;
    CheckEquals(SIPBadRequest,
                Response.StatusCode,
                'Status code');
  finally
    Auth.Free;
  end;
end;

procedure TestTIdSipAbstractUserAgent.TestRejectUnauthorizedRequest;
var
  Response: TIdSipResponse;
begin
  Self.Core.RequireAuthentication := true;

  Self.MarkSentResponseCount;
  Self.SimulateRemoteInvite;
  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPUnauthorized,
              Response.StatusCode,
              'Status code');
  Check(Response.HasWWWAuthenticate,
        'No WWW-Authenticate header');
end;

//******************************************************************************
//* TestTIdSipUserAgentCore                                                    *
//******************************************************************************
//* TestTIdSipUserAgentCore Public methods *************************************

procedure TestTIdSipUserAgentCore.SetUp;
var
  C:        TIdSipContactHeader;
  F:        TIdSipFromHeader;
  Invite:   TIdSipRequest;
  Response: TIdSipResponse;
begin
  inherited SetUp;

  Self.Dispatcher.Transport.AddTransportSendingListener(Self);

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

  Invite := TIdSipRequest.Create;
  try
    Response := TIdSipResponse.Create;
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
  Self.SessionEstablished  := false;
end;

procedure TestTIdSipUserAgentCore.TearDown;
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

//* TestTIdSipUserAgentCore Private methods ************************************

procedure TestTIdSipUserAgentCore.CheckCommaSeparatedHeaders(const ExpectedValues: String;
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

procedure TestTIdSipUserAgentCore.CheckCreateRequest(Dest: TIdSipToHeader;
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
  Check(sttTCP = Request.LastHop.Transport,
        'TCP should be the default transport');
end;

procedure TestTIdSipUserAgentCore.OnAuthenticationChallenge(Action: TIdSipAction;
                                                            Challenge: TIdSipResponse;
                                                            var Username: String;
                                                            var Password: String);
begin
end;

procedure TestTIdSipUserAgentCore.OnChanged(Observed: TObject);
begin
  Self.OnChangedEvent.SetEvent;
end;

procedure TestTIdSipUserAgentCore.OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                                             Receiver: TIdSipTransport);
begin
end;

procedure TestTIdSipUserAgentCore.OnEndedSession(Session: TIdSipSession;
                                                 const Reason: String);
begin
  Self.OnEndedSessionFired := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUserAgentCore.OnEstablishedSession(Session: TIdSipSession);
begin
  Self.SessionEstablished := true;
end;

procedure TestTIdSipUserAgentCore.OnInboundCall(Session: TIdSipInboundSession);
begin
  Self.OnInboundCallFired := true;

  Session.AddSessionListener(Self);
  Self.Session := Session;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUserAgentCore.OnModifiedSession(Session: TIdSipSession;
                                                    Answer: TIdSipResponse);
begin
end;

procedure TestTIdSipUserAgentCore.OnModifySession(Modify: TIdSipInboundInvite);
begin
end;

procedure TestTIdSipUserAgentCore.OnSendRequest(Request: TIdSipRequest;
                                                Sender: TIdSipTransport);
begin
end;

procedure TestTIdSipUserAgentCore.OnSendResponse(Response: TIdSipResponse;
                                                 Sender: TIdSipTransport);
begin
  if (Response.StatusCode = SIPSessionProgress) then
    Self.SendEvent.SetEvent;
end;

procedure TestTIdSipUserAgentCore.SimulateRemoteBye(Dialog: TIdSipDialog);
var
  Bye: TIdSipRequest;
begin
  Bye := Self.CreateRemoteBye(Dialog);
  try
    Self.SendRequest(Bye);
  finally
    Bye.Free;
  end;
end;

//* TestTIdSipUserAgentCore Published methods **********************************

procedure TestTIdSipUserAgentCore.TestAddObserver;
var
  L1, L2: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    L2 := TIdObserverListener.Create;
    try
      Self.Core.AddObserver(L1);
      Self.Core.AddObserver(L2);

      Self.SimulateRemoteInvite;

      Check(L1.Changed and L2.Changed, 'Not all Listeners notified, hence not added');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestAddUserAgentListener;
var
  L1, L2: TIdSipTestUserAgentListener;
begin
  L1 := TIdSipTestUserAgentListener.Create;
  try
    L2 := TIdSipTestUserAgentListener.Create;
    try
      Self.Core.AddUserAgentListener(L1);
      Self.Core.AddUserAgentListener(L2);

      Self.SimulateRemoteInvite;

      Check(L1.InboundCall and L2.InboundCall,
            'Not all Listeners notified, hence not added');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCallUsingProxy;
const
  ProxyUri = 'sip:proxy.tessier-ashpool.co.luna';
var
  Invite: TIdSipRequest;
begin
  Self.Core.Proxy.Uri := ProxyUri;
  Self.Core.HasProxy := true;

  Self.Core.Call(Self.Destination, '', '');

  Invite := Self.LastSentRequest;
  Check(Invite.HasHeader(RouteHeader),
        'No Route header added');

  Invite.Route.First;
  CheckEquals(ProxyUri,
              Invite.Route.CurrentRoute.Address.Uri,
              'Route points to wrong proxy');
end;

procedure TestTIdSipUserAgentCore.TestCancelNotifiesTU;
var
  SessCount: Integer;
begin
  Self.SimulateRemoteInvite;
  SessCount := Self.Core.SessionCount;
  Self.SimulateRemoteCancel;

  Check(Self.OnEndedSessionFired,
        'UA not notified of remote CANCEL');
  Check(Self.Core.SessionCount < SessCount,
        'UA didn''t remove cancelled session');
end;

procedure TestTIdSipUserAgentCore.TestContentTypeDefault;
begin
  CheckEquals(SdpMimeType,
              Self.Core.AllowedContentTypes,
              'AllowedContentTypes');
end;

procedure TestTIdSipUserAgentCore.TestCreateBye;
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

procedure TestTIdSipUserAgentCore.TestCreateInvite;
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

procedure TestTIdSipUserAgentCore.TestCreateInviteInsideDialog;
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

procedure TestTIdSipUserAgentCore.TestCreateInviteWithBody;
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

procedure TestTIdSipUserAgentCore.TestCreateOptions;
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

procedure TestTIdSipUserAgentCore.TestCreateRegister;
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

procedure TestTIdSipUserAgentCore.TestCreateRegisterReusesCallIDForSameRegistrar;
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

procedure TestTIdSipUserAgentCore.TestCreateReInvite;
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

procedure TestTIdSipUserAgentCore.TestCreateRequest;
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

procedure TestTIdSipUserAgentCore.TestCreateRequestSipsRequestUri;
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

procedure TestTIdSipUserAgentCore.TestCreateRequestUserAgent;
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

procedure TestTIdSipUserAgentCore.TestCreateRequestWithTransport;
var
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp';
    Request := Self.Core.CreateRequest(Dest);
    try
      Check(Request.LastHop.Transport = sttUDP,
            'UDP transport not specified');
    finally
      Request.Free;
    end;

    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna;transport=tcp';
    Request := Self.Core.CreateRequest(Dest);
    try
      Check(Request.LastHop.Transport = sttTCP,
            'TCP transport not specified');
    finally
      Request.Free;
    end;

    try
      Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna;transport=foo';
      Request := Self.Core.CreateRequest(Dest);
      CheckNull(Request,
                'Return value not FreeAndNil''d');
      Fail('Failed to bail out on unknown transport');
    except
      on EConvertError do;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateResponseToTagMissing;
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

procedure TestTIdSipUserAgentCore.TestCreateResponseUserAgent;
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

procedure TestTIdSipUserAgentCore.TestCreateResponseUserAgentBlank;
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

procedure TestTIdSipUserAgentCore.TestDialogLocalSequenceNoMonotonicallyIncreases;
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

procedure TestTIdSipUserAgentCore.TestDispatchToCorrectSession;
var
  SessionOne: TIdSipInboundSession;
  SessionTwo: TIdSipInboundSession;
begin
  // 1. Receive two inbound sessions.
  // 2. Receive a BYE for one of them.
  // 3. Check that the correct session died, and the other didn't.

  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session),
        'OnInboundCall didn''t fire');
  SessionOne := Self.Session;

  Self.Invite.LastHop.Branch := Self.Invite.LastHop.Branch + '1';
  Self.Invite.From.Tag       := Self.Invite.From.Tag + '1';
  Self.Invite.ToHeader.Tag   := Self.Invite.ToHeader.Tag + '1';
  Self.SimulateRemoteInvite;
  Check(Self.Session <> SessionOne,
        'OnInboundCall didn''t fire a second time');
  SessionTwo := Self.Session;
  CheckEquals(2,
              Self.Core.SessionCount,
              'Number of sessions after two INVITEs');


  SessionTwo.AcceptCall('', '');

  SessionTwo.AddSessionListener(Self);
  Self.ThreadEvent.ResetEvent;
  Self.ExceptionMessage := 'SessionTwo wasn''t terminated';
  Self.SimulateRemoteBye(SessionTwo.Dialog);

  Check(not SessionOne.IsTerminated, 'SessionOne was terminated');
  CheckEquals(1,
              Self.Core.SessionCount,
              'Number of sessions after one BYE');
end;

procedure TestTIdSipUserAgentCore.TestDoNotDisturb;
var
  SessionCount: Cardinal;
begin
  Self.Core.DoNotDisturb := true;
  Self.MarkSentResponseCount;
  SessionCount  := Self.Core.SessionCount;

  Self.SimulateRemoteInvite;
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

procedure TestTIdSipUserAgentCore.TestHasUnknownContentEncoding;
begin
  Self.Invite.Headers.Remove(Self.Invite.FirstHeader(ContentEncodingHeaderFull));

  Check(not Self.Core.HasUnknownContentEncoding(Self.Invite),
        'Vacuously true');

  Self.Invite.AddHeader(ContentEncodingHeaderFull);
  Check(Self.Core.HasUnknownContentEncoding(Self.Invite),
        'No encodings are supported');
end;

procedure TestTIdSipUserAgentCore.TestHasUnknownContentType;
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

procedure TestTIdSipUserAgentCore.TestInviteExpires;
var
  Event: TIdNotifyEventWait;
begin
  Self.Core.AddObserver(Self);

  Self.MarkSentResponseCount;

  Self.Invite.FirstExpires.NumericValue := 50;
  Self.SimulateRemoteInvite;

  Self.ExceptionMessage := 'Waiting for OnInboundCall to fire';
  Self.WaitForSignaled;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');

  Event := TIdNotifyEventWait.Create;
  try
    Event.Data := Self.Invite.Copy;
    Self.Core.OnInboundSessionExpire(Event);
  finally
    Event.Free;
  end;

  CheckResponseSent('No response sent');
  CheckEquals(SIPRequestTerminated,
              Self.LastSentResponse.StatusCode,
              'Unexpected response sent');

  Self.WaitForSignaled(Self.OnChangedEvent,
                       'Waiting for OnChanged, signalling when the whole '
                     + 'shebang''s finished');
  CheckEquals(0, Self.Core.SessionCount, 'Expired session not cleaned up');
end;

procedure TestTIdSipUserAgentCore.TestInviteRaceCondition;
begin
  CheckEquals(0,
              Self.Core.InviteCount,
              'Sanity check - new test should have no ongoing INVITE actions');

  Self.MarkSentResponseCount;
  Self.SimulateRemoteInvite;
  CheckEquals(1,
              Self.Core.InviteCount,
              'First INVITE didn''t make a new INVITE action');

  CheckResponseSent('No response sent');

  Self.SimulateRemoteInvite;
  CheckEquals(1,
              Self.Core.InviteCount,
              'INVITE resend made a new INVITE action');
end;

procedure TestTIdSipUserAgentCore.TestIsMethodAllowed;
begin
  Check(not Self.Core.IsMethodAllowed(MethodRegister),
        MethodRegister + ' not allowed');

  Self.Core.AddModule(TIdSipRegisterModule);
  Check(Self.Core.IsMethodAllowed(MethodRegister),
        MethodRegister + ' not recognised as an allowed method');

  Check(not Self.Core.IsMethodAllowed(' '),
        ''' '' recognised as an allowed method');
end;

procedure TestTIdSipUserAgentCore.TestIsSchemeAllowed;
begin
  Check(not Self.Core.IsMethodAllowed(SipScheme),
        SipScheme + ' not allowed');

  Self.Core.AddAllowedScheme(SipScheme);
  Check(Self.Core.IsSchemeAllowed(SipScheme),
        SipScheme + ' not recognised as an allowed scheme');

  Check(not Self.Core.IsSchemeAllowed(' '),
        ''' '' not recognised as an allowed scheme');
end;

procedure TestTIdSipUserAgentCore.TestLoopDetection;
var
  Response: TIdSipResponse;
begin
  // cf. RFC 3261, section 8.2.2.2
  Self.Dispatcher.AddServerTransaction(Self.Invite, Self.Dispatcher.Transport);

  // wipe out the tag & give a different branch
  Self.Invite.ToHeader.Value := Self.Invite.ToHeader.Address.URI;
  Self.Invite.LastHop.Branch := Self.Invite.LastHop.Branch + '1';

  Self.MarkSentResponseCount;

  Self.SimulateRemoteInvite;
  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPLoopDetected, Response.StatusCode, 'Status-Code');
end;

procedure TestTIdSipUserAgentCore.TestNotificationOfNewSession;
begin
  Self.SimulateRemoteInvite;

  Check(Self.OnInboundCallFired, 'UI not notified of new session');
end;

procedure TestTIdSipUserAgentCore.TestNotificationOfNewSessionRobust;
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

      Self.SimulateRemoteInvite;

      Check(L2.InboundCall, 'L2 not notified');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestOutboundInviteSessionProgressResends;
var
  DebugTimer: TIdDebugTimerQueue;
begin
  DebugTimer := TIdDebugTimerQueue.Create(false);
  try
    Self.Core.Timer := DebugTimer;
    Self.MarkSentResponseCount;

    // Receive an INVITE. Ring. Wait.
    Self.Core.ProgressResendInterval := 50;

    Self.SimulateRemoteInvite;
    Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');

    Self.WaitForSignaled(Self.SendEvent);

    CheckResponseSent('No response sent');
    CheckEquals(SIPSessionProgress,
                Self.LastSentResponse.StatusCode,
                'Wrong response');
  finally
    Self.Core.Timer := nil;
    DebugTimer.Terminate;
  end;
end;

procedure TestTIdSipUserAgentCore.TestReceiveByeForUnmatchedDialog;
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

    Self.SendRequest(Bye);

    CheckResponseSent('No response sent');
    Response := Self.LastSentResponse;
    CheckEquals(SIPCallLegOrTransactionDoesNotExist,
                Response.StatusCode,
                'Response Status-Code')

  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestReceiveByeForDialog;
var
  Response: TIdSipResponse;
begin
  Self.SimulateRemoteInvite;

  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
  Self.Session.AcceptCall('', '');
  Self.SimulateAck;

  Self.MarkSentResponseCount;
  Self.SimulateRemoteBye(Self.Session.Dialog);

  CheckResponseSent('SOMETHING should have sent a response');

  Response := Self.LastSentResponse;
  CheckNotEquals(SIPCallLegOrTransactionDoesNotExist,
                 Response.StatusCode,
                 'UA tells us no matching dialog was found');
end;

procedure TestTIdSipUserAgentCore.TestReceiveByeWithoutTags;
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

    Self.SendRequest(Bye);

    CheckResponseSent('No response sent');
    Response := Self.LastSentResponse;
    CheckEquals(SIPCallLegOrTransactionDoesNotExist,
                Response.StatusCode,
                'Response Status-Code')
  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestReceiveOptions;
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

    Self.SendRequest(Options);

    Response := Self.LastSentResponse;
    CheckEquals(SIPOK,
                Response.StatusCode,
                'We should accept all OPTIONS');
  finally
    Options.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestReceiveResponseWithMultipleVias;
var
  Response: TIdSipResponse;
begin
  Self.Core.Call(Self.Destination, '', '');

  Response := TIdSipResponse.InResponseTo(Self.Invite,
                                          SIPOK,
                                          Self.Core.Contact);
  try
    Response.AddHeader(Response.Path.LastHop);
    Self.SimulateRemoteResponse(Response);
    Check(not Self.SessionEstablished,
          'Multiple-Via Response not dropped');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestRejectNoContact;
var
  Response: TIdSipResponse;
begin
  Self.Invite.RemoveHeader(Self.Invite.FirstContact);

  Self.MarkSentResponseCount;

  Self.SimulateRemoteInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPBadRequest,        Response.StatusCode, 'Status-Code');
  CheckEquals(MissingContactHeader, Response.StatusText, 'Status-Text');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnknownContentEncoding;
var
  Response: TIdSipResponse;
begin
  Self.Invite.FirstHeader(ContentTypeHeaderFull).Value := SdpMimeType;

  Self.MarkSentResponseCount;

  Self.Invite.AddHeader(ContentEncodingHeaderFull).Value := 'gzip';

  Self.SimulateRemoteInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPUnsupportedMediaType, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(AcceptEncodingHeader), 'No Accept-Encoding header');
  CheckEquals('',
              Response.FirstHeader(AcceptEncodingHeader).Value,
              'Accept value');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnknownContentLanguage;
var
  Response: TIdSipResponse;
begin
  Self.Core.AddAllowedLanguage('fr');

  Self.Invite.AddHeader(ContentLanguageHeader).Value := 'en_GB';

  Self.MarkSentResponseCount;

  Self.SimulateRemoteInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPUnsupportedMediaType, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(AcceptLanguageHeader), 'No Accept-Language header');
  CheckEquals(Self.Core.AllowedLanguages,
              Response.FirstHeader(AcceptLanguageHeader).Value,
              'Accept-Language value');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnknownContentType;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;

  Self.Invite.ContentType := 'text/xml';

  Self.SimulateRemoteInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPUnsupportedMediaType, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(AcceptHeader), 'No Accept header');
  CheckEquals(SdpMimeType,
              Response.FirstHeader(AcceptHeader).Value,
              'Accept value');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnknownExtension;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;

  Self.Invite.AddHeader(RequireHeader).Value := '100rel';

  Self.SimulateRemoteInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPBadExtension, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(UnsupportedHeader), 'No Unsupported header');
  CheckEquals(Self.Invite.FirstHeader(RequireHeader).Value,
              Response.FirstHeader(UnsupportedHeader).Value,
              'Unexpected Unsupported header value');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnknownScheme;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;

  Self.Invite.RequestUri.URI := 'tel://1';
  Self.SimulateRemoteInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPUnsupportedURIScheme, Response.StatusCode, 'Status-Code');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnsupportedMethod;
var
  Response: TIdSipResponse;
begin
  Self.Invite.Method := MethodRegister;
  Self.Invite.CSeq.Method := Self.Invite.Method;

  Self.MarkSentResponseCount;

  Self.SimulateRemoteInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  Check(Response.HasHeader(AllowHeader),
        'Allow header is mandatory. cf. RFC 3261 section 8.2.1');

  CheckCommaSeparatedHeaders(Self.Core.AllowedMethods,
                             Response.FirstHeader(AllowHeader),
                             'Allow header');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnsupportedSipVersion;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;
  Self.Invite.SIPVersion := 'SIP/1.0';

  Self.SimulateRemoteInvite;

  CheckEquals(Self.ResponseCount + 2, // Trying + reject
              Self.SentResponseCount,
              'No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPSIPVersionNotSupported,
              Response.StatusCode,
              'Status-Code');
end;

procedure TestTIdSipUserAgentCore.TestRemoveObserver;
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

      Self.SimulateRemoteInvite;

      Check(L1.Changed and not L2.Changed,
            'Listener notified, hence not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestRemoveUserAgentListener;
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

      Self.SimulateRemoteInvite;

      Check(L1.InboundCall and not L2.InboundCall,
            'Listener notified, hence not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestSetContact;
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

procedure TestTIdSipUserAgentCore.TestSetContactMailTo;
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

procedure TestTIdSipUserAgentCore.TestSetContactWildCard;
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

procedure TestTIdSipUserAgentCore.TestSetFrom;
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

procedure TestTIdSipUserAgentCore.TestSetFromMailTo;
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

procedure TestTIdSipUserAgentCore.TestTerminateAllCalls;
var
  FirstSession:  TIdSipInboundSession;
  SecondSession: TIdSipInboundSession;
begin
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire, first INVITE');
  FirstSession := Self.Session;
  FirstSession.AcceptCall('', '');
  Self.Session := nil;

  Self.Invite.LastHop.Branch := Self.Invite.LastHop.Branch + '1';
  Self.Invite.From.Tag       := Self.Invite.From.Tag + '1';
  Self.Invite.ToHeader.Tag   := Self.Invite.ToHeader.Tag + '1';

  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire, second INVITE');
  SecondSession := Self.Session;
  SecondSession.AcceptCall('', '');

  CheckEquals(2,
              Self.Core.SessionCount,
              'Session count');
  Self.Core.TerminateAllCalls;
  CheckEquals(0,
              Self.Core.SessionCount,
              'Session count after TerminateAllCalls');
end;

procedure TestTIdSipUserAgentCore.TestViaMatchesTransportParameter;
var
  Trans: TIdSipTransportType;
begin
  for Trans := Low(TIdSipTransportType) to High(TIdSipTransportType) do begin
    Self.Dispatcher.Transport.TransportType := Trans;
    Self.Destination.Address.Transport := TransportToStr(Trans);
    Self.Core.Call(Self.Destination, '', '');

    CheckEquals(TransportToStr(Trans),
                TransportToStr(Self.LastSentRequest.LastHop.Transport),
                'Transport parameter = '
              + Self.Destination.Address.Transport);
  end;
end;

//******************************************************************************
//* TestTIdSipAction                                                           *
//******************************************************************************
//* TestTIdSipAction Public methods ********************************************

procedure TestTIdSipAction.SetUp;
begin
  inherited SetUp;

  Self.ActionFailed := false;
  Self.Challenged   := false;

  Self.Username := 'case';
  Self.Password := 'mycotoxin';
end;

procedure TestTIdSipAction.TestProxyAuthentication;
var
  Action:         TIdSipAction;
  InitialRequest: TIdSipRequest;
  SequenceNo:     Cardinal;
  ReAttempt:      TIdSipRequest;
begin
  // It only makes sense to check for proxy authentication on outbound actions.
  // Half our actions are inbound, and half outbound. Our strategy is thus to
  // avoid code duplication by keeping the code here, in the base test class.
  // Outbound actions then make this procedure published by overriding this
  // method and just calling the inherited method. This makes the TestRunner
  // pick up the test in just those actions that need it. The other option is
  // to duplicate the code in those test cases that require it!

  Action := Self.CreateAction;
  Self.PerformAction(Action);

  InitialRequest := TIdSipRequest.Create;
  try
    InitialRequest.Assign(Self.LastSentRequest);
    Self.MarkSentRequestCount;
    SequenceNo   := InitialRequest.CSeq.SequenceNo;

    Self.SimulateRejectProxyUnauthorized;
    CheckRequestSent(Self.ClassName + ': no re-issue of '
                   + InitialRequest.Method + ' request');

    ReAttempt := Self.LastSentRequest;
    CheckEquals(SequenceNo + 1,
                ReAttempt.CSeq.SequenceNo,
                Self.ClassName + ': Re-' + InitialRequest.Method + ' CSeq sequence number');
    CheckEquals(InitialRequest.Method,
                ReAttempt.Method,
                Self.ClassName + ': Method of new attempt');
    CheckEquals(InitialRequest.RequestUri.Uri,
                ReAttempt.RequestUri.Uri,
                Self.ClassName + ': Re-' + InitialRequest.Method + ' Request-URI');
    Check(ReAttempt.HasProxyAuthorization,
          Self.ClassName + ': No Proxy-Authorization header in re-' + InitialRequest.Method);
  finally
    InitialRequest.Free;
  end;
end;

//* TestTIdSipAction Protected methods *****************************************

procedure TestTIdSipAction.OnAuthenticationChallenge(Action: TIdSipAction;
                                                     Challenge: TIdSipResponse;
                                                     var Username: String;
                                                     var Password: String);
begin
  Self.Challenged := true;

  Password := Self.Password;
  Username := Self.Username;
end;

procedure TestTIdSipAction.PerformAction(Action: TIdSipAction);
begin
  raise Exception.Create(Self.ClassName
                       + ': Don''t call this from inbound action tests');
end;

procedure TestTIdSipAction.SimulateRejectProxyUnauthorized;
var
  Challenge: TIdSipResponse;
begin
  Challenge := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                           SIPProxyAuthenticationRequired);
  try
    Challenge.AddHeader(ProxyAuthenticateHeader).Value := 'Digest realm="193.116.120.160",nonce="bfa807909eb7d5b960d7b23de1dc620ed82f40b5"';
    Self.SimulateRemoteResponse(Challenge);
  finally
    Challenge.Free;
  end;
end;

procedure TestTIdSipAction.SimulateRemoteBadExtensionResponse;
begin
  Self.SimulateRemoteResponse(SIPBadExtension);
end;

procedure TestTIdSipAction.SimulateRemoteOK;
begin
  Self.SimulateRemoteResponse(SIPOK);
end;

procedure TestTIdSipAction.SimulateRemoteOKFor(Request: TIdSipRequest);
var
  Ok: TIdSipResponse;
begin
  Ok := TIdSipResponse.InResponseTo(Request, SIPOK);
  try
    Ok.ToHeader.Tag := Self.Core.NextTag;

    Self.SimulateRemoteResponse(Ok);
  finally
    Ok.Free;
  end;
end;

//* TestTIdSipAction Published methods *****************************************

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
  Self.SimulateRemoteBadExtensionResponse;

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

  Self.SimulateRemoteBadExtensionResponse;
  Check(Self.ActionFailed, ActionClassName + ' failure not reported');
end;
}

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

procedure TestTIdSipSession.OnEstablishedSession(Session: TIdSipSession);
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

procedure TestTIdSipSession.SimulateRemoteReInvite(Session: TIdSipSession);
begin
  // At this point Self.Invite represents the INVITE we sent out
  Self.Invite.LastHop.Branch  := Self.Invite.LastHop.Branch + '1';
  Self.Invite.CallID          := Session.Dialog.ID.CallID;
  Self.Invite.From.Tag        := Session.Dialog.ID.RemoteTag;
  Self.Invite.ToHeader.Tag    := Session.Dialog.ID.LocalTag;
  Self.Invite.CSeq.SequenceNo := Session.Dialog.RemoteSequenceNo + 1;

  // Now it represents an INVITE received from the network
  Self.SimulateRemoteInvite;
end;

//* TestTIdSipSession Published methods ****************************************

procedure TestTIdSipSession.TestAckToInDialogInviteMatchesInvite;
var
  Ack:     TIdSipRequest;
  Session: TIdSipSession;
begin
  Session := Self.CreateAndEstablishSession;
  Self.SimulateRemoteReInvite(Session);

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

  Self.SimulateRemoteReInvite(Session);
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
  // Essentially, we and Remote send INVITEs simultaneously

  Session := Self.CreateAndEstablishSession;
  Session.Modify('', '');

  Self.MarkSentResponseCount;
  Self.SimulateRemoteReInvite(Session);
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

  Event := Self.Core.OnResendReInvite;
  DebugTimer := TIdDebugTimerQueue.Create(false);
  try
    Self.Core.Timer := DebugTimer;

    Session := Self.CreateAndEstablishSession;

    Session.Modify('', '');

    EventCount := DebugTimer.EventCount;
    Self.SimulateRemoteResponse(SIPRequestPending);

    Check(EventCount < DebugTimer.EventCount,
          Session.ClassName + ': no timer added');

    DebugTimer.LockTimer;
    try
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

  Self.SimulateRemoteResponse(SIPRequestTimeout);

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

procedure TestTIdSipSession.TestModify;
var
  Session: TIdSipSession;
begin
  Session := Self.CreateAction as TIdSipSession;
  Self.EstablishSession(Session);

  Self.MarkSentRequestCount;
  Session.Modify('', '');
  CheckRequestSent(Session.ClassName + ': No INVITE sent');

  Self.SimulateRemoteAccept(Self.LastSentRequest);
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

    Self.SimulateRemoteReInvite(Session);
    FirstInvite.Assign(Self.InboundModify.InitialRequest);
    Check(Self.OnModifySessionFired,
          Session.ClassName + ': OnModifySession didn''t fire');

    Self.MarkSentResponseCount;
    Self.OnModifySessionFired := false;
    Self.SimulateRemoteReInvite(Session);
    Check(not Self.OnModifySessionFired,
          Session.ClassName + ': OnModifySession fired for a 2nd modify');
    CheckResponseSent(Session.ClassName + ': No 491 response sent');
    CheckEquals(SIPRequestPending,
                Self.LastSentResponse.StatusCode,
                Session.ClassName + ': Unexpected response to 2nd INVITE');
    Check(Self.Invite.Match(Self.LastSentResponse),
          Session.ClassName + ': Response doesn''t match 2nd INVITE');
    Self.SimulateAck;
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
    Self.SimulateAckFor(Self.InboundModify.InitialRequest,
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

    Session.Modify('', '');
    FirstInvite.Assign(Self.LastSentRequest);

    Self.MarkSentResponseCount;
    Self.SimulateRemoteReInvite(Session);
    CheckResponseSent(Session.ClassName + ': No 491 response sent');
    CheckEquals(SIPRequestPending,
                Self.LastSentResponse.StatusCode,
                Session.ClassName + ': Unexpected response');
    Self.SimulateAck;

    Self.MarkSentAckCount;
    Self.SimulateRemoteOKFor(FirstInvite);
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

  Self.Failed       := false;
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
              'Inbound INVITE; Method');
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
  Invite: TIdSipOutboundInvite;
begin
  Result := Self.Core.AddOutboundInvite;

  Invite := Result as TIdSipOutboundInvite;
  Invite.Invite(Self.Destination, '', '');
  Invite.AddListener(Self);
end;

procedure TestTIdSipOutboundInvite.PerformAction(Action: TIdSipAction);
begin
  (Action as TIdSipOutboundInvite).Invite(Self.Destination, '', '');
end;

//* TestTIdSipOutboundInvite Private methods ***********************************

procedure TestTIdSipOutboundInvite.CheckReceiveFailed(StatusCode: Cardinal);
var
  InviteCount: Integer;
  Invite: TIdSipOutboundInvite;
begin
  Invite := Self.CreateAction as TIdSipOutboundInvite;
  Invite.AddListener(Self);

  InviteCount := Self.Core.InviteCount;
  Self.SimulateRemoteResponse(StatusCode);

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

      Self.SimulateRemoteOK;

      Check(L1.Success, 'L1 not informed of success');
      Check(L2.Success, 'L2 not informed of success');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestCancelAfterAccept;
var
  OutboundInvite: TIdSipOutboundInvite;
begin
  OutboundInvite := Self.CreateAction as TIdSipOutboundInvite;
  OutboundInvite.AddListener(Self);
  Self.SimulateRemoteOK;

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
  OutboundInvite.AddListener(Self);
  InviteCount := Self.Core.InviteCount;
  Invite := TIdSipRequest.Create;
  try
    Invite.Assign(Self.LastSentRequest);
    // Note that Invite's To header has no tag because we haven't established
    // a dialog.
    RequestTerminated := TIdSipResponse.InResponseTo(Invite, SIPRequestTerminated);
    try
      // <---       180 Ringing      ---
      Self.SimulateRemoteRinging(Invite);

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
      Self.SimulateRemoteOK;

      // <--- 487 Request Terminated ---  (for the INVITE)
      //  ---           ACK          --->
      Self.MarkSentACKCount;
      Self.SimulateRemoteResponse(RequestTerminated);

      CheckAckSent('No ACK sent');
      CheckEquals(MethodAck,
                  Self.LastSentRequest.Method,
                  'The request sent wasn''t a ACK');

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
  OutboundInvite.AddListener(Self);
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
     Self.SimulateRemoteRinging(Self.LastSentRequest);
     Check(Self.OnDialogEstablishedFired,
           'No dialog established');
     // Now that we have the remote tag we can:
     RequestTerminated.ToHeader.Tag := Self.ToHeaderTag;

      // <---         200 OK         ---  (for the CANCEL)
      Self.SimulateRemoteOK;

      // <--- 487 Request Terminated ---  (for the INVITE)
      //  ---           ACK          --->

      Self.MarkSentACKCount;
      Self.SimulateRemoteResponse(RequestTerminated);

      CheckAckSent('No ACK sent');
      CheckEquals(MethodAck,
                  Self.LastSentRequest.Method,
                  'The request sent wasn''t a ACK');

      Check(Self.Core.InviteCount < InviteCount,
            'Action not terminated');
    finally
      RequestTerminated.Free;
    end;
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestInviteThenReInvite;
var
  Dialog: TIdSipDialog;
  Invite: TIdSipOutboundInvite;
begin
  Invite := Self.Core.AddOutboundInvite;
  Invite.Invite(Self.Destination, '', '');

  Dialog := Self.CreateArbitraryDialog;
  try
    try
      Invite.ReInvite(Self.Invite, Dialog, false, '', '');
      Fail('Failed to bail out calling ReInvite after Invite');
    except
      on EIdSipTransactionUser do;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestInviteTwice;
var
  Invite: TIdSipOutboundInvite;
begin
  Invite := Self.Core.AddOutboundInvite;
  Invite.Invite(Self.Destination, '', '');

  try
    Invite.Invite(Self.Destination, '', '');
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
              'Outbound INVITE; Method');
end;

procedure TestTIdSipOutboundInvite.TestProxyAuthentication;
var
  Ack:           TIdSipRequest;
  Invite:        TIdSipOutboundInvite;
  InitialInvite: TIdSipRequest;
  SequenceNo:    Cardinal;
  ReInvite:      TIdSipRequest;
begin
  Invite := Self.Core.AddOutboundInvite;
  Invite.AddListener(Self);
  Invite.Invite(Self.Destination, '', '');

  InitialInvite := TIdSipRequest.Create;
  try
    InitialInvite.Assign(Invite.InitialRequest);
    Self.MarkSentRequestCount;
    SequenceNo   := Invite.InitialRequest.CSeq.SequenceNo;

    Self.SimulateRejectProxyUnauthorized;
    CheckRequestSent('no re-issue of request');

    ReInvite := Self.LastSentRequest;
    CheckEquals(SequenceNo + 1,
                ReInvite.CSeq.SequenceNo,
                'Re-INVITE CSeq sequence number');
    CheckEquals(MethodInvite,
                ReInvite.Method,
                'Method of new attempt');
    CheckEquals(InitialInvite.RequestUri.Uri,
                ReInvite.RequestUri.Uri,
                'Re-INVITE Request-URI');
    Check(ReInvite.HasProxyAuthorization,
          'No Proxy-Authorization header in re-INVITE');

    Self.MarkSentACKCount;
    Self.SimulateRemoteOK;

    CheckAckSent('No ACK sent');

    Ack := Self.LastSentACK;
    Check(Ack.HasProxyAuthorization,
          'No Proxy-Authorization header in ACK');
    CheckEquals(ReInvite.FirstProxyAuthorization.Value,
                Ack.FirstProxyAuthorization.Value,
                'ACK  must use same credentials as re-INVITE');
  finally
    InitialInvite.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestReceiveGlobalFailed;
var
  StatusCode: Integer;
begin
  for StatusCode := 600 to 699 do
    Self.CheckReceiveFailed(StatusCode);
end;

procedure TestTIdSipOutboundInvite.TestReceiveRedirect;
var
  Invite: TIdSipOutboundInvite;
begin
  Invite := Self.CreateAction as TIdSipOutboundInvite;
  Invite.AddListener(Self);

  Self.SimulateRemoteResponse(SIPMovedPermanently);

  Check(Self.OnRedirectFired,
        'OnRedirect didn''t fire after receiving a '
      + IntToStr(SIPMovedPermanently) + ' response');
end;

procedure TestTIdSipOutboundInvite.TestReceiveRequestFailed;
var
  StatusCode: Integer;
begin
  for StatusCode := 400 to SIPUnauthorized - 1 do
    Self.CheckReceiveFailed(StatusCode);

  for StatusCode := SIPUnauthorized + 1 to SIPProxyAuthenticationRequired - 1 do
    Self.CheckReceiveFailed(StatusCode);

  for StatusCode := SIPProxyAuthenticationRequired + 1 to 499 do
    Self.CheckReceiveFailed(StatusCode);
end;

procedure TestTIdSipOutboundInvite.TestReceiveServerFailed;
var
  StatusCode: Integer;
begin
  for StatusCode := 500 to 599 do
    Self.CheckReceiveFailed(StatusCode);
end;

procedure TestTIdSipOutboundInvite.TestRedirectedInvite;
var
  Invite:         TIdSipOutboundInvite;
  NewInvite:      TIdSipRequest;
  OriginalInvite: TIdSipRequest;
begin
  OriginalInvite := Self.Core.CreateInvite(Self.Destination, '', '');
  try
    Invite := Self.Core.AddOutboundInvite;
    Self.MarkSentRequestCount;

    Invite.RedirectedInvite(Self.Destination,
                            OriginalInvite);

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

procedure TestTIdSipOutboundInvite.TestReInviteThenInvite;
var
  Dialog: TIdSipDialog;
  Invite: TIdSipOutboundInvite;
begin
  Invite := Self.Core.AddOutboundInvite;

  Dialog := Self.CreateArbitraryDialog;
  try
    Invite.ReInvite(Self.Invite, Dialog, false, '', '');

    try
      Invite.Invite(Self.Destination, '', '');
      Fail('Failed to bail out calling Invite after ReInvite');
    except
      on EIdSipTransactionUser do;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TestTIdSipOutboundInvite.TestReInviteTwice;
var
  Dialog: TIdSipDialog;
  Invite: TIdSipOutboundInvite;
begin
  Invite := Self.Core.AddOutboundInvite;

  Dialog := Self.CreateArbitraryDialog;
  try
    Invite.ReInvite(Self.Invite, Dialog, false, '', '');

    try
      Invite.ReInvite(Self.Invite, Dialog, false, '', '');
      Fail('Failed to bail out calling ReInvite a 2nd time');
    except
      on EIdSipTransactionUser do;
    end;
  finally
    Dialog.Free;
  end;
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

      Self.SimulateRemoteOK;

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
  Self.SimulateRemoteRinging(Self.LastSentRequest);
  Self.MarkSentRequestCount;

  OutboundInvite.Terminate;

  CheckRequestSent('Action didn''t send a CANCEL');
end;

procedure TestTIdSipOutboundInvite.TestTerminateAfterAccept;
var
  OutboundInvite: TIdSipOutboundInvite;
begin
  OutboundInvite := Self.CreateAction as TIdSipOutboundInvite;
  OutboundInvite.AddListener(Self);
  Self.SimulateRemoteOK;

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
//* TestTIdSipInboundOptions                                                   *
//******************************************************************************
//* TestTIdSipInboundOptions Private methods ***********************************

procedure TestTIdSipInboundOptions.SimulateRemoteOptions;
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

    Self.SendRequest(Options);
  finally
    Options.Free;
  end;
end;

//* TestTIdSipInboundOptions Published methods *********************************

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
  Self.SimulateRemoteOptions;

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
  Self.SimulateRemoteOptions;

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

procedure TestTIdSipOutboundOptions.TestAddListener;
var
  L1, L2:  TIdSipTestOptionsListener;
  Options: TIdSipOutboundOptions;
begin
  Options := Self.Core.QueryOptions(Self.Core.From);

  L1 := TIdSipTestOptionsListener.Create;
  try
    L2 := TIdSipTestOptionsListener.Create;
    try
      Options.AddListener(L1);
      Options.AddListener(L2);

      Self.SimulateRemoteOK;

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

procedure TestTIdSipOutboundOptions.TestProxyAuthentication;
begin
  inherited TestProxyAuthentication;
end;

procedure TestTIdSipOutboundOptions.TestRemoveListener;
var
  L1, L2:  TIdSipTestOptionsListener;
  Options: TIdSipOutboundOptions;
begin
  Options := Self.Core.QueryOptions(Self.Core.From);

  L1 := TIdSipTestOptionsListener.Create;
  try
    L2 := TIdSipTestOptionsListener.Create;
    try
      Options.AddListener(L1);
      Options.AddListener(L2);
      Options.RemoveListener(L2);

      Self.SimulateRemoteOK;

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

//* TestTIdSipOutboundOptions Protected methods ********************************

function TestTIdSipOutboundOptions.CreateAction: TIdSipAction;
begin
  Result := Self.Core.QueryOptions(Self.Core.From);
end;

procedure TestTIdSipOutboundOptions.PerformAction(Action: TIdSipAction);
begin
  (Action as TIdSipOutboundOptions).QueryOptions(Self.Destination);
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

  Self.Registrar := TIdSipUserAgentCore.Create;
  Self.Registrar.From.Address.Uri := 'sip:talking-head.tessier-ashpool.co.luna';
  Self.Registrar.RemoveModule(TIdSipInviteModule);
  Self.Registrar.AddModule(TIdSipRegisterModule);

  Self.Request := TIdSipRequest.Create;
  Self.Request.Method := MethodRegister;
  Self.Request.RequestUri.Uri := 'sip:tessier-ashpool.co.luna';
  Self.Request.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TCP talking-head.tessier-ashpool.co.luna;branch='
                                               + BranchMagicCookie + 'f00L';
  Self.Request.ToHeader.Address.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.Request.AddHeader(ContactHeaderFull).Value := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';
  Self.Request.CSeq.Method := Self.Request.Method;
  Self.Request.CSeq.SequenceNo := 1024;
  Self.Request.CallID := '1@selftest.foo';

  Self.Contacts := TIdSipContacts.Create(Self.Request.Headers);

  Self.Reg := Self.Core.RegisterWith(Self.Registrar.From.Address);
  Self.Reg.AddListener(Self);

  Self.Succeeded  := false;
  Self.MinExpires := TwoHours;
end;

procedure TestTIdSipOutboundRegistration.TearDown;
begin
  Self.Contacts.Free;
  Self.Request.Free;
  Self.Registrar.Free;

  inherited TearDown;
end;

//*  TestTIdSipOutboundRegistration Protected methods **************************

function TestTIdSipOutboundRegistration.CreateAction: TIdSipAction;
begin
  Result := Self.Core.RegisterWith(Self.Registrar.From.Address);
  (Result as TIdSipOutboundRegistration).AddListener(Self);
end;

procedure TestTIdSipOutboundRegistration.PerformAction(Action: TIdSipAction);
begin
  (Action as TIdSipOutboundRegistration).RegisterWith(Self.Destination.Address,
                                                      Self.Contacts);
end;

//*  TestTIdSipOutboundRegistration Private methods ****************************

function TestTIdSipOutboundRegistration.DigestForName(const Password: String): String;
begin
  Result := KD(MD5(Self.Core.Username + ':'
                 + Self.Core.Realm + ':'
                 + Password),
                   Self.LastSentResponse.FirstProxyAuthenticate.Nonce + ':'
                 + MD5('REGISTER:' + Self.Registrar.From.Address.Uri),
                   MD5);
end;

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

procedure TestTIdSipOutboundRegistration.SimulateRemoteIntervalTooBrief;
var
  Response: TIdSipResponse;
begin
  Response := Self.Registrar.CreateResponse(Self.LastSentRequest,
                                            SIPIntervalTooBrief);
  try
    Response.AddHeader(MinExpiresHeader).Value := IntToStr(Self.MinExpires);

    Self.SimulateRemoteResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundRegistration.SimulateRemoteRejectProxyAuthenticationRequired;
var
  Auth:     TIdSipProxyAuthenticateHeader;
  Response: TIdSipResponse;
begin
  Response := Self.Registrar.CreateResponse(Self.LastSentRequest,
                                            SIPProxyAuthenticationRequired);
  try
    Response.AddHeader(ProxyAuthenticateHeader);
    Auth := Response.FirstProxyAuthenticate;
    Auth.AuthorizationScheme := DigestAuthorizationScheme;
    Auth.Realm := Self.Core.Realm;
    Auth.Nonce := 'deadbeef';

    Self.SimulateRemoteResponse(Response);
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
  Registration := Self.Core.RegisterWith(Self.Registrar.From.Address);

  L1 := TIdSipTestRegistrationListener.Create;
  try
    L2 := TIdSipTestRegistrationListener.Create;
    try
      Registration.AddListener(L1);
      Registration.AddListener(L2);

      Self.SimulateRemoteOK;

      Check(L1.Success, 'L1 not informed of success');
      Check(L2.Success, 'L2 not informed of success');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipOutboundRegistration.TestCheckFirstListenerSetsPassword;
var
  L1:           TIdSipTestRegistrationListener;
  L2:           TIdSipTestRegistrationListener;
  Registration: TIdSipOutboundRegistration;
  Request:      TIdSipRequest;
begin
  Registration := Self.Core.RegisterWith(Self.Registrar.From.Address);

  L1 := TIdSipTestRegistrationListener.Create;
  try
    L2 := TIdSipTestRegistrationListener.Create;
    try
      Registration.AddListener(L1);
      Registration.AddListener(L2);

      L1.Password := 'L1';
      L2.Password := 'L2';

      Self.MarkSentRequestCount;
      Self.SimulateRemoteRejectProxyAuthenticationRequired;

      CheckRequestSent('No request sent');
      Request := Self.LastSentRequest;

      Check(Request.HasProxyAuthorization,
            'Request must have Proxy-Authorization');
      // Check that the password is 'L1'
      CheckEquals(Self.DigestForName('L1'),
                  Request.FirstProxyAuthorization.Response,
                  'L1 didn''t get to set the password');
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

procedure TestTIdSipOutboundRegistration.TestRegister;
var
  Request: TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);
  CheckRequestSent('No request sent');

  Request := Self.LastSentRequest;
  CheckEquals(Self.Registrar.From.Address.Uri,
              Request.RequestUri.Uri,
              'Request-URI');
  CheckEquals(MethodRegister, Request.Method, 'Method');
  Check(Request.Contacts.Equals(Self.Contacts),
        'Bindings');
end;

procedure TestTIdSipOutboundRegistration.TestFindCurrentBindings;
var
  Request: TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.Reg.FindCurrentBindings(Self.Registrar.From.Address);
  CheckRequestSent('No request sent');

  Request := Self.LastSentRequest;
  Check(Request.Contacts.IsEmpty,
        'Contact headers present');
end;

procedure TestTIdSipOutboundRegistration.TestProxyAuthentication;
begin
  inherited TestProxyAuthentication;
end;

procedure TestTIdSipOutboundRegistration.TestReceiveFail;
begin
  Self.SimulateRemoteResponse(SIPInternalServerError);
  Check(Self.ActionFailed, 'Registration succeeded');
end;

procedure TestTIdSipOutboundRegistration.TestReceiveIntervalTooBrief;
const
  OneHour = 3600;
begin
  Self.Contacts.First;
  Self.Contacts.CurrentContact.Expires := OneHour;
  Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);

  Self.MarkSentRequestCount;
  Self.SimulateRemoteIntervalTooBrief;

  CheckRequestSent('No re-request issued');
  Check(Self.LastSentRequest.HasExpiry,
        'Re-request has no expiry');
  CheckEquals(Self.MinExpires,
              Self.LastSentRequest.QuickestExpiry,
              'Re-request minimum expires');

  Self.SimulateRemoteOK;
  Check(Self.Succeeded, '(Re-)Registration failed');
end;

procedure TestTIdSipOutboundRegistration.TestReceiveIntervalTooBriefForOneContact;
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
  Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);

  Self.MarkSentRequestCount;
  Self.SimulateRemoteIntervalTooBrief;

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

  Self.SimulateRemoteOK;
  Check(Self.Succeeded, '(Re-)Registration failed');
end;

procedure TestTIdSipOutboundRegistration.TestReceiveMovedPermanently;
begin
  Self.MarkSentRequestCount;
  Self.SimulateMovedPermanently('sip:case@fried.neurons.org');
  CheckRequestSent('No request re-issued for REGISTER');
end;

procedure TestTIdSipOutboundRegistration.TestReceiveOK;
begin
  Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);
  Self.SimulateRemoteOK;
  Check(Self.Succeeded, 'Registration failed');
end;

procedure TestTIdSipOutboundRegistration.TestReceiveUnauthorized;
begin
  Self.SimulateRejectProxyUnauthorized;
  Check(Self.Challenged,
        'No authentication challenge for REGISTER');
end;

procedure TestTIdSipOutboundRegistration.TestRemoveListener;
var
  L1, L2:       TIdSipTestRegistrationListener;
  Registration: TIdSipOutboundRegistration;
begin
  Registration := Self.Core.RegisterWith(Self.Registrar.From.Address);

  L1 := TIdSipTestRegistrationListener.Create;
  try
    L2 := TIdSipTestRegistrationListener.Create;
    try
      Registration.AddListener(L1);
      Registration.AddListener(L2);
      Registration.RemoveListener(L2);

      Self.SimulateRemoteOK;

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

procedure TestTIdSipOutboundRegistration.TestSequenceNumberIncrements;
var
  SeqNo: Cardinal;
begin
  Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);
  SeqNo := Self.LastSentRequest.CSeq.SequenceNo;
  Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);
  Check(SeqNo + 1 = Self.LastSentRequest.CSeq.SequenceNo,
        'CSeq sequence number didn''t increment');
end;

procedure TestTIdSipOutboundRegistration.TestUnregister;
var
  Request: TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.Reg.Unregister(Self.Registrar.From.Address);
  CheckRequestSent('No request sent');

  Request := Self.LastSentRequest;
  CheckEquals(Self.Registrar.From.Address.Uri,
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

procedure TestTIdSipOutboundRegistration.TestUsername;
var
  Reg: TIdSipOutboundRegistration;
begin
  Reg := Self.Core.RegisterWith(Self.Registrar.From.Address);

  Self.Core.From.DisplayName := 'foo';
  CheckEquals(Self.Core.Username,
              Reg.Username,
              'Username "foo"');

  Self.Core.From.DisplayName := 'bar';
  CheckEquals(Self.Core.Username,
              Reg.Username,
              'Username "bar"');
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

  Self.CreateAction;
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
  Self.Invite.LastHop.Branch := Self.Core.NextBranch;
  Self.Invite.From.Tag       := Self.Core.NextTag;
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Result := Self.Session;
end;

procedure TestTIdSipInboundSession.EstablishSession(Session: TIdSipSession);
begin
  (Session as TIdSipInboundSession).AcceptCall('', '');
  Self.SimulateAck;
end;

//* TestTIdSipInboundSession Private methods ***********************************

procedure TestTIdSipInboundSession.OnDroppedUnmatchedResponse(Response: TIdSipResponse;
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

procedure TestTIdSipInboundSession.OnEstablishedSession(Session: TIdSipSession);
begin
end;

procedure TestTIdSipInboundSession.OnInboundCall(Session: TIdSipInboundSession);
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

//* TestTIdSipInboundSession Published methods ****************************************

procedure TestTIdSipInboundSession.TestAcceptCall;
begin
  Self.Session.AcceptCall('', '');

  Check(Self.Session.DialogEstablished,
        'Dialog not established');
  CheckNotNull(Self.Session.Dialog,
               'Dialog object wasn''t created');
end;

procedure TestTIdSipInboundSession.TestAddSessionListener;
var
  L1, L2: TIdSipTestSessionListener;
begin
  Self.Session.AcceptCall('', '');
  Self.SimulateAck;

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

  Self.Session.AcceptCall('', '');

  Self.MarkSentResponseCount;
  SessionCount  := Self.Core.SessionCount;
  Self.SimulateRemoteCancel;

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
  SessionCount := Self.Core.SessionCount;

  Self.SimulateRemoteCancel;

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

  Self.Session.AcceptCall('', '');

  Self.Invite.LastHop.Branch := Self.Invite.LastHop.Branch + '1';
  Self.Invite.CSeq.SequenceNo := Self.Invite.CSeq.SequenceNo - 1;
  Self.Invite.ToHeader.Tag := Self.LastSentResponse.ToHeader.Tag;

  Self.MarkSentResponseCount;
  Self.SimulateRemoteInvite;
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
  SessionCount := Self.Core.SessionCount;

  Self.SimulateRemoteCancel;

  Check(Self.OnEndedSessionFired,
        'No notification of ended session');

  Check(Self.Core.SessionCount < SessionCount,
        'Session not marked as terminated');
end;

procedure TestTIdSipInboundSession.TestIsInboundCall;
begin
  Check(Self.Session.IsInboundCall,
        'Inbound session; IsInboundCall');
end;

procedure TestTIdSipInboundSession.TestIsOutboundCall;
begin
  Check(not Self.Session.IsOutboundCall,
        'Inbound session; IsOutboundCall');
end;

procedure TestTIdSipInboundSession.TestMethod;
begin
  CheckEquals(MethodInvite,
              TIdSipInboundSession.Method,
              'Inbound session; Method');
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
  Ringing := Self.LastSentResponse;
  CheckEquals(SIPRinging,
              Ringing.StatusCode,
              'Sanity check');
  Check(Self.Session.DialogEstablished,
        'Session should have established a dialog - it''s sent a 180, after all');

  Self.MarkSentResponseCount;
  Invite := TIdSipRequest.Create;
  try
    Invite.Assign(Self.Session.InitialRequest);
    Invite.LastHop.Branch  := Self.Core.NextBranch;
    Invite.CSeq.SequenceNo := Self.Session.InitialRequest.CSeq.SequenceNo + 1;
    Invite.ToHeader.Tag    := Ringing.ToHeader.Tag;
    Self.SendRequest(Invite);
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

  Self.SimulateAck;
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

  Self.Session.AcceptCall('', '');
  Self.SimulateAck;

  Self.SimulateRemoteReInvite(Self.Session);
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
  Self.Session.AcceptCall('', '');

  Self.SimulateRemoteBye(Self.Session.Dialog);

  Check(Self.OnEndedSessionFired, 'OnEndedSession didn''t fire');
end;
{
procedure TestTIdSipInboundSession.TestReceiveByeWithPendingRequests;
var
  ReInvite: TIdSipRequest;
begin
  Self.Session.AcceptCall('', '');

  Self.Dispatcher.Transport.AddTransportSendingListener(Self);

  // This must be a CLIENT transaction!
  ReInvite := Self.CreateRemoteReInvite(Self.Session.Dialog);
  try
    Self.SendRequest(ReInvite);
    Self.SimulateRemoteBye(Self.Session.Dialog);

    Check(Self.SentRequestTerminated,
          'Pending request wasn''t responded to with a 487 Request Terminated');
  finally
    ReInvite.Free;
  end;
end;
}
procedure TestTIdSipInboundSession.TestRedirectCall;
var
  Dest:         TIdSipAddressHeader;
  SentResponse: TIdSipResponse;
begin
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

  SDP :='v=0'#13#10
      + 'o=franks 123456 123456 IN IP4 127.0.0.1'#13#10
      + 's=-'#13#10
      + 'c=IN IP4 127.0.0.1'#13#10
      + 'm=audio 8000 RTP/AVP 0'#13#10;

  Self.Core.AddUserAgentListener(Self);

  Self.Session := Self.CreateAction as TIdSipOutboundSession;

  Self.OnDroppedResponse      := false;
  Self.OnEndedSessionFired    := false;
  Self.OnModifiedSessionFired := false;
end;

//* TestTIdSipOutboundSession Protectedivate methods ***************************

procedure TestTIdSipOutboundSession.CheckResendWaitTime(Milliseconds: Cardinal;
                                                       const Msg: String);
begin
  Check((2100 <= Milliseconds) and (Milliseconds <= 4000), Msg);

  inherited CheckResendWaitTime(Milliseconds, Msg);
end;

function TestTIdSipOutboundSession.CreateAction: TIdSipAction;
begin
  Result := Self.Core.Call(Self.Destination, Self.SDP, SdpMimeType);
  (Result as TIdSipSession).AddSessionListener(Self);
end;

procedure TestTIdSipOutboundSession.EstablishSession(Session: TIdSipSession);
begin
  Self.SimulateRemoteOK;
end;

procedure TestTIdSipOutboundSession.PerformAction(Action: TIdSipAction);
begin
  (Action as TIdSipOutboundSession).Call(Self.Destination, '', '');
end;

//* TestTIdSipOutboundSession Private methods **********************************

procedure TestTIdSipOutboundSession.OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                                               Receiver: TIdSipTransport);
begin
  Self.OnDroppedResponse := true;
end;

procedure TestTIdSipOutboundSession.OnInboundCall(Session: TIdSipInboundSession);
begin
end;

procedure TestTIdSipOutboundSession.SimulateRemoteDecline;
var
  Decline: TIdSipResponse;
begin
  Decline := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                         SIPDecline);
  try
    Self.SimulateRemoteResponse(Decline);
  finally
    Decline.Free;
  end;
end;

procedure TestTIdSipOutboundSession.SimulateForbidden;
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Self.LastSentRequest,
                                       SIPForbidden);
  try
    Self.SimulateRemoteResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSession.SimulateMovedTemporarily(const Contact: String);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                          SIPMovedTemporarily);
  try
    Response.AddHeader(ContactHeaderFull).Value := Contact;
    Self.SimulateRemoteResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSession.SimulateRemoteOKWithRecordRoute;
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                          SIPOK);
  try
    Response.RecordRoute.Add(RecordRouteHeader).Value := '<sip:127.0.0.1>';
    Self.SimulateRemoteResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSession.SimulateRejectProxyUnauthorized;
var
  Challenge: TIdSipResponse;
begin
  Challenge := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                           SIPProxyAuthenticationRequired);
  try
    Challenge.AddHeader(ProxyAuthenticateHeader);
    Challenge.AddHeader(AuthenticationInfoHeader);
    Self.SimulateRemoteResponse(Challenge);
  finally
    Challenge.Free;
  end;
end;

procedure TestTIdSipOutboundSession.SimulateRejectUnauthorized;
var
  Challenge: TIdSipResponse;
begin
  Challenge := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                           SIPUnauthorized);
  try
    Challenge.AddHeader(WWWAuthenticateHeader);
    Challenge.AddHeader(AuthenticationInfoHeader);
    Self.SimulateRemoteResponse(Challenge);
  finally
    Challenge.Free;
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

    Self.SimulateRemoteOK;

    Ack := Self.Dispatcher.Transport.LastACK;

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
  Self.SimulateRemoteOKWithRecordRoute;
  Ack := Self.Dispatcher.Transport.LastACK;

  Check(not Ack.Route.IsEmpty, 'No Route headers');
end;

procedure TestTIdSipOutboundSession.TestAckWithAuthorization;
var
  Ack:    TIdSipRequest;
  Invite: TIdSipRequest;
begin
  Self.SimulateRejectUnauthorized;

  Invite := TIdSipRequest.Create;
  try
    Invite.Assign(Self.LastSentRequest);

    Self.SimulateRemoteOK;

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
  Self.SimulateRejectProxyUnauthorized;

  Invite := TIdSipRequest.Create;
  try
    Invite.Assign(Self.LastSentRequest);

    Self.SimulateRemoteOK;

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
  Invite:       TIdSipRequest;
  Response:     TIdSipResponse;
  SessCount:    Integer;
  Session:      TIdSipSession;
  TranCount:    Integer;
begin
  Self.MarkSentRequestCount;
  SessCount    := Self.Core.SessionCount;
  TranCount    := Self.Dispatcher.TransactionCount;

  Session := Self.Core.Call(Self.Destination, '', '');

  CheckRequestSent('no INVITE sent');
  Invite := Self.LastSentRequest;

  CheckEquals(TranCount + 1,
              Self.Dispatcher.TransactionCount,
              'no client INVITE transaction created');

  CheckEquals(SessCount + 1,
              Self.Core.SessionCount,
              'no new session created');

  Response := Self.Core.CreateResponse(Invite, SIPRinging);
  try
    Self.SimulateRemoteResponse(Response);

    Check(Session.IsEarly,
          'Dialog in incorrect state: should be Early');
    Check(Session.DialogEstablished,
          'Dialog not established');
    Check(not Session.Dialog.IsSecure,
          'Dialog secure when TLS not used');

    CheckEquals(Response.CallID,
                Session.Dialog.ID.CallID,
                'Dialog''s Call-ID');
    CheckEquals(Response.From.Tag,
                Session.Dialog.ID.LocalTag,
                'Dialog''s Local Tag');
    CheckEquals(Response.ToHeader.Tag,
                Session.Dialog.ID.RemoteTag,
                'Dialog''s Remote Tag');
  finally
    Response.Free;
  end;

  Response := Self.Core.CreateResponse(Invite, SIPOK);
  try
    Response.ToHeader.Tag := Self.LastSentResponse.ToHeader.Tag;
    Response.From.Tag     := Self.LastSentResponse.From.Tag;
    Self.SimulateRemoteResponse(Response);

    Check(not Session.IsEarly, 'Dialog in incorrect state: shouldn''t be early');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestCallTwice;
var
  SessCount: Integer;
  Session:   TIdSipOutboundSession;
  TranCount: Integer;
begin
  SessCount := Self.Core.SessionCount;
  TranCount := Self.Dispatcher.TransactionCount;

  Session := Self.Core.Call(Self.Destination, '', '');
  Session.Call(Self.Destination, '', '');

  CheckEquals(SessCount + 1,
              Self.Core.SessionCount,
              'The TU layer created a second session');
  CheckEquals(TranCount + 1,
              Self.Dispatcher.TransactionCount,
              'Session created a second transaction');
end;

procedure TestTIdSipOutboundSession.TestCallRemoteRefusal;
begin
  Self.SimulateForbidden;

  Check(Self.OnEndedSessionFired, 'OnEndedSession wasn''t triggered');
end;

procedure TestTIdSipOutboundSession.TestCallNetworkFailure;
var
  SessionCount: Cardinal;
begin
  SessionCount := Self.Core.SessionCount;
  Self.Dispatcher.Transport.FailWith := EIdConnectTimeout;

  Self.Core.Call(Self.Destination, '', '');

  CheckEquals(SessionCount,
              Self.Core.SessionCount,
              'Core should have axed the failed session');
end;

procedure TestTIdSipOutboundSession.TestCallSecure;
var
  Response: TIdSipResponse;
  Session:  TIdSipSession;
begin
  Self.Dispatcher.Transport.TransportType := sttTLS;

  Self.Destination.Address.Scheme := SipsScheme;
  Session := Self.Core.Call(Self.Destination, '', '');

  Response := Self.Core.CreateResponse(Self.LastSentRequest,
                                       SIPRinging);
  try
    Self.SimulateRemoteResponse(Response);

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
  Self.MarkSentRequestCount;
  Self.Dispatcher.Transport.TransportType := sttTCP;
  Self.Destination.Address.Scheme := SipsScheme;

  Session := Self.Core.Call(Self.Destination, '', '');

  CheckRequestSent('INVITE wasn''t sent');
  SentInvite := Self.LastSentRequest;

  Self.SimulateRemoteRinging(SentInvite);

  Check(not Session.Dialog.IsSecure, 'Dialog secure when TCP used');
end;

procedure TestTIdSipOutboundSession.TestCallSipUriOverTls;
var
  Response: TIdSipResponse;
  Session:  TIdSipSession;
begin
  Self.Dispatcher.Transport.TransportType := sttTCP;

  Session := Self.Core.Call(Self.Destination, '', '');

  Response := Self.Core.CreateResponse(Self.LastSentRequest,
                                       SipRinging);
  try
    Response.FirstContact.Address.Scheme := SipsScheme;
    Response.StatusCode := SIPOK;
    Self.SimulateRemoteResponse(Response);

    Check(not Session.Dialog.IsSecure, 'Dialog secure when TLS used with a SIP URI');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestCallWithOffer;
begin
  Self.Core.Call(Self.Destination, Self.SDP, SdpMimeType);
  Check(Self.LastSentRequest.ContentDisposition.IsSession,
        'Content-Disposition');
end;

procedure TestTIdSipOutboundSession.TestGlobalFailureEndsSession;
var
  SessionCount: Integer;
begin
  SessionCount := Self.Core.SessionCount;

  Self.SimulateRemoteDecline;

  Check(Self.OnEndedSessionFired,
        'No notification of ended session');

  Check(Self.Core.SessionCount < SessionCount,
        'Session not torn down because of a global failure');
end;

procedure TestTIdSipOutboundSession.TestHangUp;
begin
  Self.SimulateRemoteOK;

  Self.MarkSentRequestCount;
  Self.Session.Terminate;

  CheckRequestSent('No BYE sent');
  CheckEquals(MethodBye,
              Self.LastSentRequest.Method,
        'TU didn''t sent a BYE');
  Self.SimulateRemoteOK;
end;

procedure TestTIdSipOutboundSession.TestIsInboundCall;
begin
  Check(not Self.Session.IsInboundCall,
        'Outbound session; IsInboundCall');
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
  //  ---      INVITE      --->
  // <--- 401 Unauthorized ---
  //  ---      INVITE      --->
  // <---      200 OK      ---
  //  ---        ACK       --->
  //  ---      INVITE      ---> (modify)

  Invite := TIdSipRequest.Create;
  try
    Self.MarkSentRequestCount;

    Self.SimulateRejectUnauthorized;
    Invite.Assign(Self.LastSentRequest);
    CheckRequestSent('No resend of INVITE with Authorization');
    Check(Invite.HasAuthorization,
          'Resend INVITE has no Authorization header');

    Self.SimulateRemoteOK;

    Self.Session.Modify('', '');

    Modify := Self.LastSentRequest;
    Check(Modify.HasAuthorization,
          'No Authorization header');
    CheckEquals(Invite.FirstAuthorization.Value,
                Modify.FirstAuthorization.Value,
                'Authorization header');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestDialogNotEstablishedOnTryingResponse;
var
  SentInvite: TIdSipRequest;
  Session:    TIdSipSession;
begin
  Self.MarkSentRequestCount;

  Session := Self.Core.Call(Self.Destination, '', '');
  Check(not Session.DialogEstablished, 'Brand new session');

  CheckRequestSent('The INVITE wasn''t sent');
  SentInvite := Self.LastSentRequest;

  Self.SimulateRemoteTryingWithNoToTag(SentInvite);
  Check(not Session.DialogEstablished,
        'Dialog established after receiving a 100 Trying');

  Self.SimulateRemoteRinging(SentInvite);
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
  Self.SimulateMovedTemporarily('sip:foo@bar.org');
  CheckRequestSent('No redirected INVITE #1 sent');
  CheckEquals('sip:foo@bar.org',
              Self.LastSentRequest.RequestUri.Uri,
              'Request-URI of redirect #1');

  Self.MarkSentRequestCount;
  Self.SimulateMovedTemporarily('sip:baz@quaax.org');
  CheckRequestSent('No redirected INVITE #2 sent');
  CheckEquals('sip:baz@quaax.org',
              Self.LastSentRequest.RequestUri.Uri,
              'Request-URI of redirect #2');
end;

procedure TestTIdSipOutboundSession.TestProxyAuthentication;
begin
  inherited TestProxyAuthentication;
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
    Self.SimulateRemoteResponse(Ok);

    CheckAckSent('Original ACK');

    Self.MarkSentAckCount;
    Self.SimulateRemoteResponse(Ok);
    CheckAckSent('Retransmission');

    Ack := Self.LastSentRequest;
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
  NewAddress = 'sip:foo';
var
  OriginalInvite: TIdSipRequest;
begin
  OriginalInvite := TIdSipRequest.Create;
  try
    OriginalInvite.Assign(Self.LastSentRequest);

    Self.MarkSentRequestCount;
    Self.SimulateMovedPermanently(NewAddress);

    CheckRequestSent('Session didn''t send a new INVITE');
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
  Self.SimulateMovedTemporarily(Contact);

  CheckRequestSent('No new INVITE sent');
  CheckEquals(InviteCount,
              Self.Core.InviteCount,
              'The Core should have one new INVITE and have destroyed one old one');

  RequestUri := Self.LastSentRequest.RequestUri;
  CheckEquals(Contact,
              RequestUri.Uri,
              'Request-URI');

  Self.SimulateForbidden;
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
    Self.SimulateRemoteResponse(Redirect);

    Check(Self.OnEndedSessionFired,
          'Session didn''t end despite a redirect with no Contact headers');
  finally
    Redirect.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestReceiveFinalResponseSendsAck;
var
  I: Integer;
begin
  // Of course this works. That's because the transaction sends the ACK for a
  // non-2xx final response.
  for I := 3 to 6 do begin
    Self.MarkSentAckCount;

    Self.Core.Call(Self.Destination, '', '');

    Self.SimulateRemoteResponse(I*100);
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
  Self.SimulateMovedTemporarily(Contact);

  CheckRequestSent('No new INVITE sent');
  CheckEquals(InviteCount,
              Self.Core.InviteCount,
              'The Core should have one new INVITE and have destroyed one old one');

  RequestUri := Self.LastSentRequest.RequestUri;
  CheckEquals(Contact,
              RequestUri.Uri,
              'Request-URI');

  Self.SimulateRemoteOK;

  Check(Self.OnEstablishedSessionFired,
        'Listeners not notified of a successful call');
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
    Self.SimulateRemoteRinging(Invite);

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

      Self.SimulateRemoteResponse(RequestTerminated);

      Check(Self.Core.SessionCount < SessionCount,
            'Session not marked as terminated');
    finally
      RequestTerminated.Free;
    end;
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestTerminateEstablishedSession;
var
  SessionCount: Integer;
begin
  Self.SimulateRemoteOK;

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

//******************************************************************************
//* TestProxyAuthentication                                                    *
//******************************************************************************
//* TestProxyAuthentication Public methods *************************************

procedure TestProxyAuthentication.SetUp;
begin
  inherited SetUp;

  Self.Session := Self.CreateAction as TIdSipOutboundSession;

  Self.Opaque := 'decafbadcafebabe';
  Self.Password := 'f00L';
  Self.Username := 'User';
end;

//* TestProxyAuthentication Protected methods **********************************

function TestProxyAuthentication.CreateAction: TIdSipAction;
begin
  Result := Self.Core.Call(Self.Destination, '', '');
  (Result as TIdSipSession).AddSessionListener(Self);
end;

procedure TestProxyAuthentication.EstablishSession(Session: TIdSipSession);
begin
  Self.SimulateRemoteOK;
end;

procedure TestProxyAuthentication.OnAuthenticationChallenge(Action: TIdSipAction;
                                                            Challenge: TIdSipResponse;
                                                            var Username: String;
                                                            var Password: String);
begin
  inherited OnAuthenticationChallenge(Action, Challenge, Username, Password);

  Password := Self.Password;
  Username := Self.Username;
end;

//* TestProxyAuthentication Published methods **********************************

procedure TestProxyAuthentication.TestFailedAuthentication;
var
  SequenceNo: Cardinal;
  ReInvite:   TIdSipRequest;
begin
  //  Failed authentications result in resends with (hopefully corrected)
  // credentials.

  Self.MarkSentRequestCount;
  SequenceNo   := Self.LastSentRequest.CSeq.SequenceNo;

  Self.SimulateRejectProxyUnauthorized;
  CheckRequestSent('no re-issue of request');

  ReInvite := Self.LastSentRequest;
  CheckEquals(SequenceNo + 1,
              ReInvite.CSeq.SequenceNo,
              'Re-INVITE CSeq sequence number');

  Self.MarkSentRequestCount;
  Self.SimulateRejectProxyUnauthorized;
  CheckRequestSent('no re-issue of request');
  ReInvite := Self.LastSentRequest;
  CheckEquals(SequenceNo + 2,
              ReInvite.CSeq.SequenceNo,
              'Re-INVITE CSeq sequence number');
end;

procedure TestProxyAuthentication.TestSuccessfulAuthentication;
var
  A1:         String;
  A2:         String;
  Auth:       TIdSipProxyAuthorizationHeader;
  SequenceNo: Cardinal;
  ReInvite:   TIdSipRequest;
  Response:   TIdSipResponse;
begin
  //  ---             INVITE                --->
  // <--- 407 Proxy Authentication Required ---
  //  ---              ACK                  --->
  //  ---             INVITE                --->

  Self.MarkSentRequestCount;
  SequenceNo   := Self.LastSentRequest.CSeq.SequenceNo;

  Self.SimulateRejectProxyUnauthorized;
  CheckRequestSent('no re-issue of request');

  ReInvite := Self.LastSentRequest;
  CheckEquals(SequenceNo + 1,
              ReInvite.CSeq.SequenceNo,
              'Re-INVITE CSeq sequence number');
  CheckEquals(MethodInvite,
              ReInvite.Method,
              'Method of new attempt');

  Check(ReInvite.HasProxyAuthorization, 'No Proxy-Authorization header');

  Auth := ReInvite.FirstProxyAuthorization;
  Response := Self.LastSentResponse;

  CheckEquals(Response.FirstProxyAuthenticate.Nonce,
              Auth.Nonce,
              'Nonce');

  CheckEquals(Response.FirstProxyAuthenticate.Realm,
              Auth.Realm,
              'Realm');

  CheckEquals(ReInvite.RequestUri.AsString,
              Auth.DigestUri,
              'URI');

  CheckEquals(ReInvite.From.Address.Username,
              Auth.Username,
              'Username');

  A1 := Auth.Username + ':' + Auth.Realm + ':' + Self.Password;
  A2 := ReInvite.Method + ':' + Auth.DigestUri;

  CheckEquals(KD(MD5(A1),
                 Auth.Nonce + ':' + MD5(A2),
                 MD5),
              Auth.Response,
              'Response');
end;

//******************************************************************************
//* TestBugHunt                                                                *
//******************************************************************************
//* TestBugHunt Public methods *************************************************

procedure TestBugHunt.SetUp;
begin
  inherited SetUp;

  Self.ToTag := 'faketag';
end;

//* TestBugHunt Privage methods ************************************************

function TestBugHunt.CreateRemoteInvite: TIdSipRequest;
var
  OurTo: TIdSipToHeader;
begin
  OurTo := Self.Core.Contact.AsToHeader;
  try
    Result := Self.Core.CreateInvite(OurTo, '', '');
  finally
    OurTo.Free;
  end;
end;

procedure TestBugHunt.OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                                 Receiver: TIdSipTransport);
begin
end;

procedure TestBugHunt.OnInboundCall(Session: TIdSipInboundSession);
begin
  Self.Session := Session;
end;

procedure TestBugHunt.SimulateRemoteInvite;
var
  Request: TIdSipRequest;
begin
  Request := Self.CreateRemoteInvite;
  try
    Self.SendRequest(Request);
  finally
    Request.Free;
  end;
end;

procedure TestBugHunt.SimulateRemoteOK;
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.LastSentRequest, SIPOK);
  try
    Response.ToHeader.Tag := Self.ToTag;
    Self.SimulateRemoteResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestBugHunt.SimulateRemoteRinging;
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.LastSentRequest, SIPRinging);
  try
    Response.ToHeader.Tag := Self.ToTag;
    Self.SimulateRemoteResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestBugHunt.SimulateRemoteTrying;
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.LastSentRequest, SIPTrying);
  try
    Response.ToHeader.Tag := Self.ToTag;
    Self.SimulateRemoteResponse(Response);
  finally
    Response.Free;
  end;
end;

//* TestBugHunt Published methods **********************************************

procedure TestBugHunt.TestOutboundCallAndByeToXlite;
var
  Session: TIdSipSession;
begin
  Session := Self.Core.Call(Self.Destination, '', '');

  Self.SimulateRemoteTrying;
  Check(not Session.DialogEstablished,
        Self.LastSentResponse.Description
      + 's don''t make dialogs');

  Self.SimulateRemoteRinging;
  Check(Session.DialogEstablished,
        Self.LastSentResponse.Description
      + 's with To tags make dialogs');
  Check(Session.IsEarly,
        Self.LastSentResponse.Description
      + 's make early dialogs');

  Self.SimulateRemoteOK;
  Check(not Session.IsEarly,
        Self.LastSentResponse.Description
      + 's make non-early dialogs');

  Self.SimulateRemoteOK;
  Self.SimulateRemoteOK;
  Self.SimulateRemoteOK;

  Self.Core.TerminateAllCalls;
  Check(Self.LastSentRequest.IsBye,
        'Must send a BYE to terminate an established session');
end;

procedure TestBugHunt.TestSimultaneousInAndOutboundCall;
begin
  Self.Core.AddUserAgentListener(Self);
  Self.Core.Call(Self.Destination, '', '');
  Self.SimulateRemoteTrying;
  Self.SimulateRemoteRinging;

  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'TU not informed of inbound call');

  Self.Session.AcceptCall('', '');
  CheckEquals(2, Self.Core.SessionCount, 'Session count');
end;

procedure TestBugHunt.TestXlitesAckNonBug;
var
  Ack:       TIdSipRequest;
  RemoteDlg: TIdSipDialog;
  TranCount: Cardinal;
begin
  Self.Core.AddUserAgentListener(Self);
  Self.SimulateRemoteInvite;

  Check(Assigned(Self.Session), 'TU not informed of inbound call');
  Self.Session.AcceptCall('', '');

  TranCount := Self.Dispatcher.TransactionCount;

  RemoteDlg := TIdSipDialog.CreateOutboundDialog(Self.LastSentRequest,
                                                 Self.LastSentResponse,
                                                 false);
  try
    Ack := RemoteDlg.CreateAck;
    try
      Self.SendRequest(Ack);

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

//******************************************************************************
//* TActionMethodTestCase                                                      *
//******************************************************************************
//* TActionMethodTestCase Public methods ***************************************

procedure TActionMethodTestCase.SetUp;
begin
  inherited SetUp;

  Self.UA := TIdSipUserAgentCore.Create;
  Self.UA.Dispatcher := TIdSipMockTransactionDispatcher.Create;

  Self.Response := TIdSipResponse.Create;
end;

procedure TActionMethodTestCase.TearDown;
begin
  Self.Response.Free;
  Self.UA.Dispatcher.Free;
  Self.UA.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipActionAuthenticationChallengeMethod                              *
//******************************************************************************
//* TestTIdSipActionAuthenticationChallengeMethod Public methods ***************

procedure TestTIdSipActionAuthenticationChallengeMethod.SetUp;
var
  Nowhere: TIdSipAddressHeader;
begin
  inherited SetUp;

  Self.Method := TIdSipActionAuthenticationChallengeMethod.Create;

  Nowhere := TIdSipAddressHeader.Create;
  try
    Self.Method.Action := Self.UA.Call(Nowhere, '', '');
  finally
    Nowhere.Free;
  end;

  Self.Method.Response := Self.Response;

  Self.L1 := TIdSipTestRegistrationListener.Create;
  Self.L2 := TIdSipTestRegistrationListener.Create;
end;

procedure TestTIdSipActionAuthenticationChallengeMethod.TearDown;
begin
  Self.L2.Free;
  Self.L1.Free;
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipActionAuthenticationChallengeMethod Published methods ****

procedure TestTIdSipActionAuthenticationChallengeMethod.TestFirstListenerDoesntSetPassword;
begin
  Self.L2.Password := 'foo';

  Self.Method.Run(Self.L1);
  Self.Method.Run(Self.L2);

  CheckEquals(Self.L2.Password,
              Self.Method.FirstPassword,
              '2nd listener didn''t set password');
end;

procedure TestTIdSipActionAuthenticationChallengeMethod.TestFirstListenerSetsPassword;
begin
  Self.L1.Password := 'foo';
  Self.L2.Password := 'bar';

  Self.Method.Run(Self.L1);
  Self.Method.Run(Self.L2);

  CheckEquals(Self.L1.Password,
              Self.Method.FirstPassword,
              'Returned password not 1st listener''s');
end;

procedure TestTIdSipActionAuthenticationChallengeMethod.TestFirstListenerDoesntSetUsername;
begin
  Self.L2.Username := 'foo';

  Self.Method.Run(Self.L1);
  Self.Method.Run(Self.L2);

  CheckEquals(Self.L2.Username,
              Self.Method.FirstUsername,
              '2nd listener didn''t set Username');
end;

procedure TestTIdSipActionAuthenticationChallengeMethod.TestFirstListenerSetsUsername;
begin
  Self.L1.Username := 'foo';
  Self.L2.Username := 'bar';

  Self.Method.Run(Self.L1);
  Self.Method.Run(Self.L2);

  CheckEquals(Self.L1.Username,
              Self.Method.FirstUsername,
              'Returned Username not 1st listener''s');
end;

procedure TestTIdSipActionAuthenticationChallengeMethod.TestRun;
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

procedure TestTIdSipActionAuthenticationChallengeMethod.TestNoListenerSetsPassword;
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
    Self.Method.Invite   := Self.UA.AddOutboundInvite;
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

  Self.Method.Session := Self.Session;
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
//* TestTIdSipUserAgentDroppedUnmatchedResponseMethod                          *
//******************************************************************************
//* TestTIdSipUserAgentDroppedUnmatchedResponseMethod Public methods ***********

procedure TestTIdSipUserAgentDroppedUnmatchedResponseMethod.SetUp;
begin
  inherited SetUp;

  Self.Receiver := TIdSipMockTransport.Create;
  Self.Response := TIdSipResponse.Create;

  Self.Method := TIdSipUserAgentDroppedUnmatchedResponseMethod.Create;
  Self.Method.Receiver := Self.Receiver;
  Self.Method.Response := Self.Response;
end;

procedure TestTIdSipUserAgentDroppedUnmatchedResponseMethod.TearDown;
begin
  Self.Method.Free;
  Self.Response.Free;
  Self.Receiver.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgentDroppedUnmatchedResponseMethod Published methods ********

procedure TestTIdSipUserAgentDroppedUnmatchedResponseMethod.TestRun;
var
  L: TIdSipTestUserAgentListener;
begin
  L := TIdSipTestUserAgentListener.Create;
  try
    Self.Method.Run(L);

    Check(L.DroppedUnmatchedResponse, 'Listener not notified');
    Check(Self.Method.Receiver = L.ReceiverParam,
          'Receiver param');
    Check(Self.Method.Response = L.ResponseParam,
          'Response param');
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
  finally
    L.Free;
  end;
end;

initialization
  RegisterTest('Transaction User Cores', Suite);
end.
