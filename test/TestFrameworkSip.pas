{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestFrameworkSip;

interface

uses
  Classes, Forms, IdConnectionBindings, IdInterfacedObject, IdObservable,
  IdRoutingTable, IdRTP, IdSdp, IdSipAuthentication, IdSipInviteModule,
  IdSipLocation, IdSipMessage, IdSipOptionsModule, IdSipCore, IdSipDialog,
  IdSipMockLocator, IdSipMockTransactionDispatcher, IdSipRegistration,
  IdSipStackInterface, IdSipSubscribeModule, IdSipTransaction, IdSipTransport,
  IdTimerQueue, IdSipUserAgent, Messages, SysUtils, TestFramework,
  TestFrameworkEx;

type
  // I provide an interface that allows a TIdSipStackWindow to dispatch
  // notifications from a TIdSipStackInterface to a test case.
  IIdSipStackInterface = interface
    ['{C97BD7D0-8554-4815-B6C8-5FD1D364DA0B}']

    procedure OnEvent(Stack: TIdSipStackInterface;
                      Event: Cardinal;
                      Data:  TIdEventData);
  end;

  TIdSipTestResources = class(TObject)
  private
    class function CreateCommonRequest: TIdSipRequest;
  public
    class function CreateBasicRequest: TIdSipRequest;
    class function CreateBasicResponse: TIdSipResponse;
    class function CreateLocalLoopRequest: TIdSipRequest;
    class function CreateLocalLoopResponse: TIdSipResponse;
    class function CreateRegister(FromUri: TIdSipUri; Contact: String): TIdSipRequest;
    class function BasicSDP(const Host: String): String;
    class function VeryLargeSDP(const Host: String): String;
  end;

  TTestCaseSip = class(TThreadingTestCase)
  private
    procedure RegisterMockTransports;
    procedure UnregisterAllTransports;
  public
    procedure CheckEquals(Expected,
                          Received: TIdSipURI;
                          const Msg: String); overload;
    procedure CheckEquals(Expected,
                          Received: TIdSipHeadersFilter;
                          const Msg: String); overload;
    procedure CheckHasHeader(SipMessage: TIdSipMessage;
                             const HeaderName: String;
                             const Msg: String = '');
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestCaseTU = class(TTestCaseSip,
                      IIdSipTransportListener,
                      IIdSipTransportSendingListener)
  private
    SentAcks:       TIdSipRequestList;
    SentRequests:   TIdSipRequestList;
    SentResponses:  TIdSipResponseList;

    procedure RemoveBody(Msg: TIdSipMessage);
  protected
    AckCount:       Cardinal;
    Authenticator:  TIdSipAbstractAuthenticator;
    Core:           TIdSipUserAgent;
    DebugTimer:     TIdDebugTimerQueue;
    Destination:    TIdSipToHeader;
    Dispatcher:     TIdSipMockTransactionDispatcher;
    Invite:         TIdSipRequest;
    LanDestination: String;
    LanGateway:     String;
    LanIP:          String;
    LanNetmask:     String;
    LanNetwork:     String;
    RequestCount:   Cardinal;
    ResponseCount:  Cardinal;
    RoutingTable:   TIdMockRoutingTable;

    procedure AddDefaultRoute(Table: TIdMockRoutingTable; Gateway, LocalAddress: String);
    procedure AddLanRoute(Table: TIdMockRoutingTable);
    procedure AddLoopbackRoute(Table: TIdMockRoutingTable);
    function  CreateRemoteBye(LocalDialog: TIdSipDialog): TIdSipRequest;
    function  CreateRemoteOk(Request: TIdSipRequest): TIdSipResponse;
    function  CreateRemoteUnauthorized(Request: TIdSipRequest;
                                       const AuthHeaderName: String;
                                       const Qop: String): TIdSipResponse;
    function  CreateUserAgent(const Address: String): TIdSipUserAgent;
    procedure FireConnectionException(Msg: TIdSipMessage);
    procedure FireOnException(Msg: TIdSipMessage;
                              ExceptionType: ExceptClass;
                              ExceptionMessage: String;
                              Reason: String);
    function  LastSentAck: TIdSipRequest; virtual;
    function  LastSentRequest: TIdSipRequest; virtual;
    function  LastSentResponse: TIdSipResponse; virtual;
    function  Locator: TIdSipMockLocator;
    procedure MarkSentAckCount;
    procedure MarkSentRequestCount;
    procedure MarkSentResponseCount;
    procedure OnException(FailedMessage: TIdSipMessage;
                          E: Exception;
                          const Reason: String); virtual;
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport;
                               Source: TIdConnectionBindings); virtual;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdConnectionBindings); virtual;
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String;
                                Source: TIdConnectionBindings); virtual;
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Destination: TIdConnectionBindings); virtual;
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Destination: TIdConnectionBindings); virtual;
    procedure ReceiveAck;
    procedure ReceiveAckFor(Request: TIdSipRequest;
                            Response: TIdSipResponse);
    procedure ReceiveBye(LocalDialog: TIdSipDialog);
    procedure ReceiveCancel;
    procedure ReceiveInvite;
    procedure ReceiveOk(Invite: TIdSipRequest;
                        const SDP: String = '');
    procedure ReceiveOkFrom(Invite: TIdSipRequest;
                            const Contact: String);
    procedure ReceiveMovedPermanently(const SipUrl: String);
    procedure ReceiveRequest(Request: TIdSipRequest); virtual;
    procedure ReceiveResponse(Response: TIdSipResponse); overload; virtual;
    procedure ReceiveResponse(StatusCode: Cardinal); overload;
    procedure ReceiveResponse(Request: TIdSipRequest;
                              StatusCode: Cardinal); overload;
    procedure ReceiveRinging(Invite: TIdSipRequest);
    procedure ReceiveTrying(Invite: TIdSipRequest);
    procedure ReceiveTryingFrom(Invite: TIdSipRequest;
                                const Contact: String);
    procedure ReceiveTryingWithNoToTag(Invite: TIdSipRequest);
    procedure ReceiveUnauthorized(const AuthHeaderName: String;
                                  const Qop: String);
    function  SecondLastSentRequest: TIdSipRequest;
    function  SecondLastSentResponse: TIdSipResponse;
    function  SentAckCount: Cardinal; virtual;
    function  SentRequestCount: Cardinal; virtual;
    function  SentResponseCount: Cardinal; virtual;
    function  SentRequestAt(Index: Integer): TIdSipRequest;
    procedure SetRoutingTableForSingleLanIP;
    function  ThirdLastSentRequest: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckAckSent(const Msg: String); virtual;
    procedure CheckNoRequestSent(const Msg: String);
    procedure CheckRequestSent(const Msg: String); virtual;
    procedure CheckNoResponseSent(const Msg: String);
    procedure CheckResponseSent(const Msg: String); virtual;
    procedure UseGruu;
  end;

  TActionMethodTestCase = class(TTestCase)
  protected
    Binding:    TIdConnectionBindings;
    Dispatcher: TIdSipMockTransactionDispatcher;
    Response:   TIdSipResponse;
    UA:         TIdSipUserAgent;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TIdSipExceptionRaisingHeader = class(TIdSipHeader)
  protected
    procedure Parse(const Value: String); override;
  end;

  TIdSipFailableListener = class(TIdInterfacedObject)
  private
    fFailWith: ExceptClass;
  protected
    fReasonParam: String;
  public
    constructor Create; override;

    property FailWith:    ExceptClass read fFailWith write fFailWith;
    property ReasonParam: String      read fReasonParam;
  end;

  TIdSipTestActionListener = class(TIdSipFailableListener,
                                   IIdSipActionListener)
  private
    fActionParam:              TIdSipAction;
    fAuthenticationChallenged: Boolean;
    fErrorCodeParam:           Cardinal;
    fNetworkFailed:            Boolean;
    fResponseParam:            TIdSipResponse;
    fTerminated:               Boolean;

    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
    procedure OnTerminated(Action: TIdSipAction);
  public
    constructor Create; override;

    property ActionParam:              TIdSipAction   read fActionParam;
    property AuthenticationChallenged: Boolean        read fAuthenticationChallenged;
    property ErrorCodeParam:           Cardinal       read fErrorCodeParam;
    property NetworkFailed:            Boolean        read fNetworkFailed;
    property ResponseParam:            TIdSipResponse read fResponseParam;
    property Terminated:               Boolean        read fTerminated;
  end;

  TIdSipTestDataListener = class(TIdSipFailableListener,
                                 IIdRtpDataListener)
  private
    fNewData:    Boolean;
    fNewUdpData: Boolean;
  public
    constructor Create; override;

    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdConnectionBindings);
    procedure OnNewUdpData(Data: TStream);

    property NewData:    Boolean read fNewData;
    property NewUdpData: Boolean read fNewUdpData;
  end;

  TIdSipTestMessageListener = class(TIdSipFailableListener,
                                    IIdSipMessageListener)
  private
    fException:             Boolean;
    fExceptionParam:        Exception;
    fMalformedMessage:      Boolean;
    fMalformedMessageParam: String;
    fReceivedFromParam:     TIdConnectionBindings;
    fReceivedRequest:       Boolean;
    fReceivedResponse:      Boolean;
    fRequestParam:          TIdSipRequest;
    fResponseParam:         TIdSipResponse;

    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnMalformedMessage(const Msg: String;
                                 const Reason: String;
                                 Source: TIdConnectionBindings);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdConnectionBindings);
  public
    constructor Create; override;

    property Exception:             Boolean                  read fException;
    property ExceptionParam:        Exception                read fExceptionParam;
    property MalformedMessage:      Boolean                  read fMalformedMessage;
    property MalformedMessageParam: String                   read fMalformedMessageParam;
    property ReceivedFromParam:     TIdConnectionBindings read fReceivedFromParam;
    property ReceivedRequest:       Boolean                  read fReceivedRequest;
    property ReceivedResponse:      Boolean                  read fReceivedResponse;
    property RequestParam:          TIdSipRequest            read fRequestParam;
    property ResponseParam:         TIdSipResponse           read fResponseParam;
  end;

  TIdSipTestObserver = class(TIdSipFailableListener,
                             IIdObserver)
  private
    fChanged: Boolean;

    procedure OnChanged(Observed: TObject);
  public
    constructor Create; override;

    property Changed: Boolean read fChanged;
  end;

  TIdSipTestInboundInviteListener = class(TIdSipTestActionListener,
                                          IIdSipInboundInviteListener)
  private
    fAckParam:          TIdSipMessage;
    fFailed:            Boolean;
    fInviteAgentParam:  TIdSipInboundInvite;
    fSucceeded:         Boolean;

    procedure OnFailure(InviteAgent: TIdSipInboundInvite);
    procedure OnSuccess(InviteAgent: TIdSipInboundInvite;
                        Ack: TIdSipMessage);
  public
    constructor Create; override;

    property AckParam:         TIdSipMessage       read fAckParam;
    property Failed:           Boolean             read fFailed;
    property InviteAgentParam: TIdSipInboundInvite read fInviteAgentParam;
    property Succeeded:        Boolean             read fSucceeded;
  end;

  TIdSipOwnedActionListener = class(TIdSipTestActionListener,
                                    IIdSipOwnedActionListener)
  private
    fFailure:       Boolean;
    fMsgParam:      TIdSipMessage;
    fRedirected:    Boolean;
    fRedirectParam: TIdSipResponse;
    fSuccess:       Boolean;

    procedure OnFailure(Action: TIdSipAction;
                        Response: TIdSipResponse;
                        const Reason: String);
    procedure OnRedirect(Action: TIdSipAction;
                         Redirect: TIdSipResponse);
    procedure OnSuccess(Action: TIdSipAction;
                        Msg: TIdSipMessage);
  public
    constructor Create; override;

    property Failure:       Boolean        read fFailure;
    property MsgParam:      TIdSipMessage  read fMsgParam;
    property Redirected:    Boolean        read fRedirected;
    property RedirectParam: TIdSipResponse read fRedirectParam;
    property Success:       Boolean        read fSuccess;
  end;

  TIdSipTestInviteListener = class(TIdSipOwnedActionListener,
                                   IIdSipInviteListener)
  private
    fCallProgress:      Boolean;
    fDialogEstablished: Boolean;
    fDialogParam:       TIdSipDialog;
    fInviteAgentParam:  TIdSipOutboundInvite;
    fReasonParam:       String;
    fResponseParam:     TIdSipResponse;

    procedure OnCallProgress(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse);
    procedure OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                  NewDialog: TidSipDialog);
  public
    constructor Create; override;

    property CallProgress:      Boolean              read fCallProgress;
    property DialogEstablished: Boolean              read fDialogEstablished;
    property DialogParam:       TIdSipDialog         read fDialogParam;
    property InviteAgentParam:  TIdSipOutboundInvite read fInviteAgentParam;
    property ReasonParam:       String               read fReasonParam;
    property ResponseParam:     TIdSipResponse       read fResponseParam;
  end;

  TIdSipTestInviteModuleListener = class(TIdSipFailableListener,
                                         IIdSipMessageModuleListener,
                                         IIdSipInviteModuleListener)
  private
    fInboundCall:    Boolean;
    fSessionParam:   TIdSipInboundSession;
    fUserAgentParam: TIdSipInviteModule;

    procedure OnInboundCall(UserAgent: TIdSipInviteModule;
                            Session: TIdSipInboundSession);
  public
    constructor Create; override;

    property InboundCall:    Boolean              read fInboundCall;
    property SessionParam:   TIdSipInboundSession read fSessionParam;
    property UserAgentParam: TIdSipInviteModule   read fUserAgentParam;
  end;

  TIdSipTestNotifyListener = class(TIdSipTestActionListener,
                                   IIdSipNotifyListener)
  private
    fFailed:           Boolean;
    fNotifyAgentParam: TIdSipOutboundNotify;
    fSucceeded:        Boolean;

    procedure OnFailure(NotifyAgent: TIdSipOutboundNotify;
                        Response: TIdSipResponse);
    procedure OnSuccess(NotifyAgent: TIdSipOutboundNotify;
                        Response: TIdSipResponse);
  public
    constructor Create; override;

    property Failed:           Boolean              read fFailed;
    property NotifyAgentParam: TIdSipOutboundNotify read fNotifyAgentParam;
    property Succeeded:        Boolean              read fSucceeded;
  end;


  TIdSipTestOptionsListener = class(TIdSipTestActionListener,
                                    IIdSipOptionsListener)
  private
    fOptionsAgentParam: TIdSipOutboundOptions;
    fResponseParam:     TIdSipResponse;
    fResponse:          Boolean;

    procedure OnResponse(OptionsAgent: TIdSipOutboundOptions;
                         Response: TIdSipResponse);
  public
    constructor Create; override;

    property OptionsAgentParam: TIdSipOutboundOptions read fOptionsAgentParam;
    property ResponseParam:     TIdSipResponse        read fResponseParam;
    property Response:          Boolean               read fResponse;
  end;

  TIdSipTestRegistrationListener = class(TIdSipTestActionListener,
                                         IIdSipRegistrationListener)
  private
    fCurrentBindingsParam: TIdSipContacts;
    fErrorCode:            Cardinal;
    fFailure:              Boolean;
    fReasonParam:          String;
    fResponseParam:        TIdSipResponse;
    fRegisterAgentParam:   TIdSipOutboundRegistrationBase;
    fSuccess:              Boolean;
  public
    constructor Create; override;

    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistrationBase;
                        ErrorCode: Cardinal;
                        const Reason: String);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistrationBase;
                        CurrentBindings: TIdSipContacts);

    property CurrentBindingsParam: TIdSipContacts                 read fCurrentBindingsParam;
    property ErrorCode:            Cardinal                       read fErrorCode;
    property Failure:              Boolean                        read fFailure;
    property ReasonParam:          String                         read fReasonParam;
    property ResponseParam:        TIdSipResponse                 read fResponseParam;
    property RegisterAgentParam:   TIdSipOutboundRegistrationBase read fRegisterAgentParam;
    property Success:              Boolean                        read fSuccess;
  end;

  TIdSipTestActionRedirectorListener = class(TIdSipFailableListener,
                                             IIdSipActionRedirectorListener)
  private
    fErrorCodeParam:        Cardinal;
    fFailed:                Boolean;
    fRedirectFailed:        Boolean;
    fNewAction:             Boolean;
    fNewActionParam:        TIdSipAction;
    fReasonParam:           String;
    fRedirectorParam:       TIdSipActionRedirector;
    fResponseParam:         TIdSipResponse;
    fSucceeded:             Boolean;
    fSuccessfulActionParam: TIdSipAction;

    procedure OnFailure(Redirector: TIdSipActionRedirector;
                        Response: TIdSipResponse);
    procedure OnNewAction(Redirector: TIdSipActionRedirector;
                          NewAction: TIdSipAction);
    procedure OnRedirectFailure(Redirector: TIdSipActionRedirector;
                                ErrorCode: Cardinal;
                                const Reason: String);
    procedure OnSuccess(Redirector: TIdSipActionRedirector;
                        SuccessfulAction: TIdSipAction;
                        Response: TIdSipResponse);
  public
    constructor Create; override;

    property ErrorCodeParam:        Cardinal               read fErrorCodeParam;
    property Failed:                Boolean                read fFailed;
    property RedirectFailed:        Boolean                read fRedirectFailed;
    property NewAction:             Boolean                read fNewAction;
    property NewActionParam:        TIdSipAction           read fNewActionParam;
    property ReasonParam:           String                 read fReasonParam;
    property RedirectorParam:       TIdSipActionRedirector read fRedirectorParam;
    property ResponseParam:         TIdSipResponse         read fResponseParam;
    property Succeeded:             Boolean                read fSucceeded;
    property SuccessfulActionParam: TIdSipAction           read fSuccessfulActionParam;
  end;

  TIdSipTestSessionListener = class(TIdSipTestActionListener,
                                    IIdSipSessionListener)
  private
    fAnswerParam:              TIdSipResponse;
    fBindingParam:             TIdConnectionBindings;
    fEndedSession:             Boolean;
    fErrorCodeParam:           Cardinal;
    fEstablishedSession:       Boolean;
    fMimeType:                 String;
    fModifiedSession:          Boolean;
    fModifySession:            Boolean;
    fNewSession:               Boolean;
    fProgressedSession:        Boolean;
    fProgressParam:            TIdSipResponse;
    fReasonParam:              String;
    fRedirect:                 Boolean;
    fReferParam:               TIdSipRequest;
    fReferral:                 Boolean;
    fRemoteSessionDescription: String;
    fSessionParam:             TIdSipSession;
  public
    constructor Create; override;

    procedure OnRedirect(Action: TIdSipAction;
                         Redirect: TIdSipResponse);
    procedure OnEndedSession(Session: TIdSipSession;
                             ErrorCode: Cardinal;
                             const Reason: String); virtual;
    procedure OnEstablishedSession(Session: TIdSipSession;
                                   const RemoteSessionDescription: String;
                                   const MimeType: String);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Answer: TIdSipResponse);
    procedure OnModifySession(Session: TIdSipSession;
                              const RemoteSessionDescription: String;
                              const MimeType: String);
    procedure OnNewSession(Session: TIdSipSession);
    procedure OnProgressedSession(Session: TIdSipSession;
                                  Progress: TIdSipResponse);
    procedure OnReferral(Session: TIdSipSession;
                         Refer: TIdSipRequest;
                         Binding: TIdConnectionBindings);

    property AnswerParam:              TIdSipResponse           read fAnswerParam;
    property BindingParam:             TIdConnectionBindings read fBindingParam;
    property EndedSession:             Boolean                  read fEndedSession;
    property ErrorCodeParam:           Cardinal                 read fErrorCodeParam;
    property EstablishedSession:       Boolean                  read fEstablishedSession;
    property MimeType:                 String                   read fMimeType;
    property ModifiedSession:          Boolean                  read fModifiedSession;
    property ModifySession:            Boolean                  read fModifySession;
    property NewSession:               Boolean                  read fNewSession;
    property ProgressParam:            TIdSipResponse           read fProgressParam;
    property ProgressedSession:        Boolean                  read fProgressedSession;
    property ReasonParam:              String                   read fReasonParam;
    property Redirect:                 Boolean                  read fRedirect;
    property ReferParam:               TIdSipRequest            read fReferParam;
    property Referral:                 Boolean                  read fReferral;
    property RemoteSessionDescription: String                   read fRemoteSessionDescription;
    property SessionParam:             TIdSipSession            read fSessionParam;
  end;

  TIdSipTestSessionListenerEndedCounter = class(TIdSipTestSessionListener)
  private
    fEndedNotificationCount: Integer;
  public
    constructor Create; override;

    procedure OnEndedSession(Session: TIdSipSession;
                             ErrorCode: Cardinal;
                             const Reason: String); override;

    property EndedNotificationCount: Integer read fEndedNotificationCount;
  end;

  TIdSipTestSubscriptionListener = class(TIdSipFailableListener,
                                         IIdSipSubscriptionListener)
  private
    fEstablishedSubscription: Boolean;
    fExpiredSubscription:     Boolean;
    fFailedSubscription:      Boolean;
    fNotify:                  Boolean;
    fNotifyParam:             TIdSipRequest;
    fRenewedSubscription:     Boolean;
    fResponseParam:           TIdSipResponse;
    fSubscriptionParam:       TIdSipOutboundSubscription;

    procedure OnEstablishedSubscription(Subscription: TIdSipOutboundSubscription;
                                        Notify: TIdSipRequest);
    procedure OnExpiredSubscription(Subscription: TIdSipOutboundSubscription;
                                    Notify: TIdSipRequest);
    procedure OnFailure(Subscription: TIdSipOutboundSubscription;
                        Response: TIdSipResponse);
    procedure OnNotify(Subscription: TIdSipOutboundSubscription;
                       Notify: TIdSipRequest);
  public
    constructor Create; override;

    property EstablishedSubscription: Boolean                    read fEstablishedSubscription;
    property ExpiredSubscription:     Boolean                    read fExpiredSubscription;
    property FailedSubscription:      Boolean                    read fFailedSubscription;
    property Notify:                  Boolean                    read fNotify;
    property NotifyParam:             TIdSipRequest              read fNotifyParam;
    property RenewedSubscription:     Boolean                    read fRenewedSubscription;
    property ResponseParam:           TIdSipResponse             read fResponseParam;
    property SubscriptionParam:       TIdSipOutboundSubscription read fSubscriptionParam;
  end;

  TIdSipTestTransactionListener = class(TIdSipFailableListener,
                                        IIdSipTransactionListener)
  private
    fBindingParam:       TIdConnectionBindings;
    fFailed:             Boolean;
    fFailedMessageParam: TIdSipMessage;
    fReasonParam:        String;
    fReceivedRequest:    Boolean;
    fReceivedResponse:   Boolean;
    fRequestParam:       TIdSipRequest;
    fResponseParam:      TIdSipResponse;
    fTerminated:         Boolean;
    fTransactionParam:   TIdSipTransaction;

    procedure OnFail(Transaction: TIdSipTransaction;
                     FailedMessage: TIdSipMessage;
                     const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Binding: TIdConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Binding: TIdConnectionBindings);
    procedure OnTerminated(Transaction: TIdSipTransaction);
  public
    constructor Create; override;

    property BindingParam:       TIdConnectionBindings read fBindingParam;
    property Failed:             Boolean                  read fFailed;
    property FailedMessageParam: TIdSipMessage            read fFailedMessageParam;
    property ReasonParam:        String                   read fReasonParam;
    property ReceivedRequest:    Boolean                  read fReceivedRequest;
    property ReceivedResponse:   Boolean                  read fReceivedResponse;
    property RequestParam:       TIdSipRequest            read fRequestParam;
    property ResponseParam:      TIdSipResponse           read fResponseParam;
    property Terminated:         Boolean                  read fTerminated;
    property TransactionParam:   TIdSipTransaction        read fTransactionParam;
  end;


  TIdSipTestTransactionDispatcherListener = class(TIdSipFailableListener,
                                                  IIdSipTransactionDispatcherListener)
  private
    fBindingParam:              TIdConnectionBindings;
    fDispatcherParam:           TIdSipTransactionDispatcher;
    fFailedMessageParam:        TIdSipMessage;
    fRaisedException:           Boolean;
    fReasonParam:               String;
    fReceivedRequest:           Boolean;
    fReceivedResponse:          Boolean;
    fReceivedUnhandledRequest:  Boolean;
    fReceivedUnhandledResponse: Boolean;
    fRequestParam:              TIdSipRequest;
    fResponseParam:             TIdSipResponse;

    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Binding: TIdConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Binding: TIdConnectionBindings);
    procedure OnTransportException(FailedMessage: TIdSipMessage;
                                   const Reason: String);
  public
    constructor Create; override;

    property BindingParam:              TIdConnectionBindings       read fBindingParam;
    property DispatcherParam:           TIdSipTransactionDispatcher read fDispatcherParam;
    property FailedMessageParam:        TIdSipMessage               read fFailedMessageParam;
    property RaisedException:           Boolean                     read fRaisedException;
    property ReceivedRequest:           Boolean                     read fReceivedRequest;
    property ReceivedResponse:          Boolean                     read fReceivedResponse;
    property ReceivedUnhandledRequest:  Boolean                     read fReceivedUnhandledRequest;
    property ReceivedUnhandledResponse: Boolean                     read fReceivedUnhandledResponse;
    property RequestParam:              TIdSipRequest               read fRequestParam;
    property ResponseParam:             TIdSipResponse              read fResponseParam;
  end;

  TIdSipTestTransactionUserListener = class(TIdSipFailableListener,
                                            IIdSipTransactionUserListener)
  private
    fAbstractUserAgentParam:  TIdSipAbstractCore;
    fActionAdded:             Boolean;
    fActionParam:             TIdSipAction;
    fActionRemoved:           Boolean;
    fBindingParam:            TIdConnectionBindings;
    fDroppedUnmatchedMessage: Boolean;
    fMessageParam:            TIdSipMessage;

    procedure OnAddAction(UserAgent: TIdSipAbstractCore;
                          Action: TIdSipAction);
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                        Message: TIdSipMessage;
                                        Binding: TIdConnectionBindings);
    procedure OnRemoveAction(UserAgent: TIdSipAbstractCore;
                             Action: TIdSipAction);
  public
    constructor Create; override;

    property AbstractUserAgentParam:  TIdSipAbstractCore    read fAbstractUserAgentParam;
    property ActionAdded:             Boolean               read fActionAdded;
    property ActionParam:             TIdSipAction          read fActionParam;
    property ActionRemoved:           Boolean               read fActionRemoved;
    property BindingParam:            TIdConnectionBindings read fBindingParam;
    property DroppedUnmatchedMessage: Boolean               read fDroppedUnmatchedMessage;
    property MessageParam:            TIdSipMessage         read fMessageParam;
  end;

  TIdSipTestSubscribeModuleListener = class(TIdSipFailableListener,
                                            IIdSipSubscribeModuleListener)
  private
    fRenewedSubscription: Boolean;
    fSubscriptionParam:   TIdSipSubscription;
    fSubscriptionRequest: Boolean;
    fUserAgentParam:      TIdSipAbstractCore;

    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipOutboundSubscription);
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipInboundSubscription);
  public
    constructor Create; override;

    property RenewedSubscription: Boolean              read fRenewedSubscription;
    property SubscriptionParam:   TIdSipSubscription   read fSubscriptionParam;
    property SubscriptionRequest: Boolean              read fSubscriptionRequest;
    property UserAgentParam:      TIdSipAbstractCore   read fUserAgentParam;
  end;

  TIdSipTestTimerQueueListener = class(TIdInterfacedObject,
                                       IIdTimerQueueListener)
  private
    fExceptionFired:   Boolean;
    fExceptionMessage: String;
    fExceptionType:    ExceptClass;
    fTimerParam:       TIdTimerQueue;
    fWaitType:         TIdWaitClass;

    procedure OnException(Timer: TIdTimerQueue;
                          Error: Exception;
                          Wait: TIdWait);
  public
    constructor Create; override;

    property ExceptionFired:   Boolean       read fExceptionFired;
    property ExceptionMessage: String        read fExceptionMessage;
    property ExceptionType:    ExceptClass   read fExceptionType;
    property TimerParam:       TIdTimerQueue read fTimerParam;
    property WaitType:         TIdWaitClass  read fWaitType;
  end;

  TIdSipActionFinder = class(TIdSipActionClosure)
  private
    fAction: TIdSipAction;
  public
    procedure Execute(Action: TIdSipAction); override;

    property Action: TIdSipAction read fAction;
  end;

  TIdSipActionSwitch = class(TIdSipActionClosure)
  private
    fExecuted: Boolean;
  public
    constructor Create; override;

    procedure Execute(Action: TIdSipAction); override;

    property Executed: Boolean read fExecuted;
  end;

  // I provide a message queue to which a StackInterface can send messages. Then
  // I route them back to a test case.
  TIdSipStackWindow = class(TCustomForm)
  private
    fTestCase: IIdSipStackInterface;

    function  IsStackMessage(Msg: TMessage): Boolean;
    procedure NotifyTestCase(Msg: TIdSipEventMessage);
  public
    constructor CreateNew(AOwner: TComponent; TestCase: IIdSipStackInterface); reintroduce;

    procedure DefaultHandler(var Message); override;

    property TestCase: IIdSipStackInterface read fTestCase;
  end;

  // constants used in tests
const
  CertPasswd     = 'test';
  DefaultTimeout = 1000;
  DummySDP       = 'v=0'#13#10
                 + 'o=sc 1105373135 1105373135 IN IP4 %s'#13#10
                 + 's=Dummy on hold SDP'#13#10
                 + 'c=IN IP4 0.0.0.0'#13#10
                 + 'm=audio 65534 RTP/AVP 0'#13#10
                 + 'a=rtpmap:0 PCMU/8000'#13#10
                 + 'a=recvonly'#13#10;
  RootCert       = '..\etc\cacert.pem';
  ServerCert     = '..\etc\newcert.pem';
  ServerKey      = '..\etc\newkey.pem';

implementation

uses
  IdException, IdNotification, IdSipMockTransport;

//******************************************************************************
//* TIdSipTestResources                                                        *
//******************************************************************************
//* TIdSipTestResources Public methods *****************************************

class function TIdSipTestResources.CreateBasicRequest: TIdSipRequest;
begin
  Result := Self.CreateCommonRequest;
  Result.RequestUri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Result.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/UDP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
  Result.ToHeader.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>;tag=1928301775';
  Result.From.Value := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
  Result.AddHeader(ContactHeaderFull).Value := 'sip:wintermute@tessier-ashpool.co.luna';
end;

class function TIdSipTestResources.CreateBasicResponse: TIdSipResponse;
var
  Request: TIdSipRequest;
begin
  Request := Self.CreateBasicRequest;
  try
    Result := TIdSipResponse.InResponseTo(Request, SIPOK);
    Result.AddHeader(ContactHeaderFull).Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';
  finally
    Request.Free;
  end;
end;

class function TIdSipTestResources.CreateLocalLoopRequest: TIdSipRequest;
begin
  Result := Self.CreateCommonRequest;
  Result.RequestUri.Uri := 'sip:franks@127.0.0.1';
  Result.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TCP 127.0.0.1;branch=z9hG4bK776asdhds';
  Result.ToHeader.Value := 'Wintermute <sip:franks@127.0.0.1>';
  Result.From.Value := 'Case <sip:franks@127.0.0.1>;tag=1928301774';
  Result.AddHeader(ContactHeaderFull).Value := 'sip:franks@127.0.0.1';
end;

class function TIdSipTestResources.CreateLocalLoopResponse: TIdSipResponse;
var
  Request: TIdSipRequest;
begin
  Request := Self.CreateLocalLoopRequest;
  try
    Result := TIdSipResponse.InResponseTo(Request, SIPBusyHere);
  finally
    Request.Free;
  end;
end;

class function TIdSipTestResources.CreateRegister(FromUri: TIdSipUri; Contact: String): TIdSipRequest;
var
  H: TIdSipAddressHeader;
begin
  H := TIdSipToHeader.Create;
  try
    Result := Self.CreateBasicRequest;
    Result.Method := MethodRegister;
    Result.CSeq.Method := Result.Method;
    Result.From.Value     := FromUri.AsString;
    Result.RequestUri.Uri := Contact;
    Result.ToHeader.Value := FromUri.AsString;
    Result.RemoveAllHeadersNamed(ContactHeaderFull);
    Result.RemoveAllHeadersNamed(ContentTypeHeaderFull);
    Result.Body := '';
    Result.ContentLength := Length(Result.Body);

  Result.AddHeader(ContactHeaderFull).Value := Contact;
  finally
    H.Free;
  end;
end;

class function TIdSipTestResources.BasicSDP(const Host: String): String;
begin
  Result := 'v=0'#13#10
          + 'o=sc 1106835019 1106835019 IN IP4 ' + Host + #13#10
          + 's=Dummy on hold SDP'#13#10
          + 'c=IN IP4 0.0.0.0'#13#10
          + 'm=audio 65534 RTP/AVP 0'#13#10
          + 'a=rtpmap:0 PCMU/8000'#13#10;
end;

class function TIdSipTestResources.VeryLargeSDP(const Host: String): String;
begin
  Result := 'v=0'#13#10
         + 'o=sc 1106835019 1106835019 IN IP4 ' + Host + #13#10
         + 's=Dummy on hold SDP'#13#10;

  while (Length(Result) < MaximumUDPMessageSize) do
    Result := Result + 'i=Junk session info lies here just to tick off your local SDP parser'#13#10;
end;

//* TIdSipTestResources Private methods ****************************************

class function TIdSipTestResources.CreateCommonRequest: TIdSipRequest;
begin
  Result := TIdSipRequest.Create;
  Result.Method := MethodInvite;
  Result.ContentType := SdpMimeType;
  Result.Body := Self.BasicSDP('127.0.0.1');
  Result.ContentLength := Length(Result.Body);
  Result.MaxForwards := 70;
  Result.SIPVersion := SipVersion;
  Result.CallID := 'a84b4c76e66710@gw1.leo-ix.org';
  Result.CSeq.Method := Result.Method;
  Result.CSeq.SequenceNo := 314159;
end;

//******************************************************************************
//* TTestCaseSip                                                               *
//******************************************************************************
//* TTestCaseSip Public methods ************************************************

procedure TTestCaseSip.CheckEquals(Expected,
                                   Received: TIdSipURI;
                                   const Msg: String);
begin
  CheckEquals(Expected.URI, Received.URI, Msg);
end;

procedure TTestCaseSip.CheckEquals(Expected,
                                       Received: TIdSipHeadersFilter;
                                       const Msg: String);
var
  I: Cardinal;
begin
  Expected.First;
  Received.First;

  I := 1;
  while Expected.HasNext and Received.HasNext do begin
    CheckEquals(Expected.CurrentHeader.Value,
                Received.CurrentHeader.Value,
                Msg + ': ' + IntToStr(I) + 'st header');
    Expected.Next;
    Received.Next;
    Inc(I);
  end;

  CheckEquals(Expected.Count,
              Received.Count,
              Msg + ': Number of headers');
end;

procedure TTestCaseSip.CheckHasHeader(SipMessage: TIdSipMessage;
                                      const HeaderName: String;
                                      const Msg: String = '');
var
  ErrorMessage: String;
begin
  ErrorMessage := 'Missing ' + HeaderName + ' header';
  if (Msg <> '') then
    ErrorMessage := Msg + ': ' + ErrorMessage;

  Check(SipMessage.HasHeader(HeaderName), ErrorMessage);
end;

procedure TTestCaseSip.SetUp;
begin
  try
    // Exceptions could occur in the descendant SetUp, and we don't want that
    // messing up our transport registrations: we're affecting a global
    // structure.
    inherited SetUp;
  finally
    Self.UnregisterAllTransports;
    Self.RegisterMockTransports;
  end;
end;

procedure TTestCaseSip.TearDown;
begin
  Self.UnregisterAllTransports;

  inherited TearDown;
end;

//* TTestCaseSip Private methods ***********************************************

procedure TTestCaseSip.RegisterMockTransports;
begin
  TIdSipTransportRegistry.RegisterTransportType(SctpTransport, TIdSipMockSctpTransport);
  TIdSipTransportRegistry.RegisterTransportType(TcpTransport,  TIdSipMockTcpTransport);
  TIdSipTransportRegistry.RegisterTransportType(TlsTransport,  TIdSipMockTlsTransport);
  TIdSipTransportRegistry.RegisterTransportType(UdpTransport,  TIdSipMockUdpTransport);
end;

procedure TTestCaseSip.UnregisterAllTransports;
begin
  TIdSipTransportRegistry.UnregisterTransportType(UdpTransport);
  TIdSipTransportRegistry.UnregisterTransportType(TlsTransport);
  TIdSipTransportRegistry.UnregisterTransportType(TcpTransport);
  TIdSipTransportRegistry.UnregisterTransportType(SctpTransport);
end;

//******************************************************************************
//* TTestCaseTU                                                                *
//******************************************************************************
//* TTestCaseTU Public methods *************************************************

procedure TTestCaseTU.SetUp;
begin
  inherited SetUp;

  Self.LanDestination := '10.0.0.8';
  Self.LanGateway     := '10.0.0.1';
  Self.LanIP          := '10.0.0.6';
  Self.LanNetmask     := '255.0.0.0';
  Self.LanNetwork     := '10.0.0.0';

  Self.Destination := TIdSipToHeader.Create;
  Self.Destination.Value := 'sip:franks@remotehost';

  Self.SentAcks      := TIdSipRequestList.Create;
  Self.SentRequests  := TIdSipRequestList.Create;
  Self.SentResponses := TIdSipResponseList.Create;

  Self.Core := Self.CreateUserAgent('sip:case@localhost');
  Self.Authenticator := Self.Core.Authenticator;
  Self.Dispatcher    := Self.Core.Dispatcher as TIdSipMockTransactionDispatcher;
  Self.RoutingTable  := Self.Core.RoutingTable as TIdMockRoutingTable;

  Self.Dispatcher.AddTransportListener(Self, FirstNotified);
  Self.Dispatcher.AddTransportSendingListener(Self, FirstNotified);

  Self.DebugTimer := Self.Dispatcher.DebugTimer;
  Self.DebugTimer.TriggerImmediateEvents := true;

  Self.Invite := TIdSipTestResources.CreateBasicRequest;
  Self.RemoveBody(Self.Invite);
  // The address differs from the transports attached to Self.Core so we
  // don't construct hairpin calls.
  Self.Locator.AddA(Self.Invite.LastHop.SentBy, Self.LanDestination);
  Self.Locator.AddA(Self.Invite.RequestUri.Host, Self.LanDestination);

  Self.SetRoutingTableForSingleLanIP;

  CheckNotEquals('',
                 Self.Dispatcher.Transport.FirstIPBound,
                 'Dispatcher.Transport.FirstIPBound returned the empty string: add a binding');
end;

procedure TTestCaseTU.TearDown;
begin
  Self.Invite.Free;
  Self.Core.Free;
  Self.SentResponses.Free;
  Self.SentRequests.Free;  
  Self.SentAcks.Free;
  Self.Destination.Free;

  // The UserAgent kills the Dispatcher, Authenticator

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

procedure TTestCaseTU.UseGruu;
begin
  // Set up this stack to use the GRUU extension. Make sure that the GRUU
  // can resolve to an IP.

  Self.Core.UseGruu := true;
end;

//* TTestCaseTU Protected methods **********************************************

procedure TTestCaseTU.AddDefaultRoute(Table: TIdMockRoutingTable; Gateway, LocalAddress: String);
begin
  Self.RoutingTable.AddOsRoute('0.0.0.0', '0.0.0.0', Gateway, 1, '1', LocalAddress);
end;

procedure TTestCaseTU.AddLanRoute(Table: TIdMockRoutingTable);
begin
  Self.RoutingTable.AddOsRoute(Self.LanNetwork, Self.LanNetmask, Self.LanGateway, 1, '1', Self.LanIP);
end;

procedure TTestCaseTU.AddLoopbackRoute(Table: TIdMockRoutingTable);
begin
  Table.AddOsRoute('127.0.0.0', '255.0.0.0', '127.0.0.1', 1, '1', '127.0.0.1');
end;

function TTestCaseTU.CreateRemoteBye(LocalDialog: TIdSipDialog): TIdSipRequest;
begin
  Result := Self.Core.InviteModule.CreateBye(LocalDialog);
  try
    Result.RequestUri           := LocalDialog.LocalURI;
    Result.FirstContact.Address := LocalDialog.RemoteTarget;
    Result.ToHeader.Tag         := LocalDialog.ID.LocalTag;
    Result.From.Tag             := LocalDialog.ID.RemoteTag;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TTestCaseTU.CreateRemoteOk(Request: TIdSipRequest): TIdSipResponse;
begin
  // This message appears to originate from the network. Invite originates from
  // us so presumably has no To tag. Having come from the network, the response
  // WILL have a To tag.
  Result := Self.Core.CreateResponse(Request, SIPOK);
  Result.FirstContact.Value := Request.ToHeader.Address.AsString;
  Result.ToHeader.Tag := Self.Core.NextTag;
end;

function TTestCaseTU.CreateRemoteUnauthorized(Request: TIdSipRequest;
                                              const AuthHeaderName: String;
                                              const Qop: String): TIdSipResponse;
var
  Auth: TIdSipAuthenticateHeader;
begin
  Result := TIdSipResponse.InResponseTo(Request,
                                        SIPUnauthorized);

  Result.ToHeader.Tag := Self.Core.NextTag;
  if (AuthHeaderName = ProxyAuthorizationHeader) then
    Result.StatusCode := SIPProxyAuthenticationRequired;

  Auth := Result.AddHeader(AuthHeaderName) as TIdSipAuthenticateHeader;
  Auth.AuthorizationScheme := DigestAuthorizationScheme;
  Auth.Realm               := 'SFTF';
  Auth.Nonce               := '5369704365727434313433';
  Auth.Qop                 := Qop;

  Result.AddHeader(AuthenticationInfoHeader);
end;

function TTestCaseTU.CreateUserAgent(const Address: String): TIdSipUserAgent;
var
  MockAuthenticator: TIdSipMockAuthenticator;
  MockLocator:       TIdSipMockLocator;
begin
  MockAuthenticator := TIdSipMockAuthenticator.Create;

  Result := TIdSipUserAgent.Create;
  Result.Authenticator := MockAuthenticator;
  Result.Dispatcher    := TIdSipMockTransactionDispatcher.Create;
  Result.From.Value    := Address;
  Result.Locator       := Result.Dispatcher.Locator;
  Result.RoutingTable  := Result.Dispatcher.RoutingTable;
  Result.Timer         := Result.Dispatcher.Timer;

  MockAuthenticator.AuthenticateAllRequests := true;

  // Make sure we have a sane DNS setup so that actions don't terminate
  // themselves after they try find locations to which to send their messages.
  MockLocator := Result.Locator as TIdSipMockLocator;
  MockLocator.AddA(Self.Destination.Address.Host, '10.0.0.2');
  MockLocator.AddA(Result.From.Address.Host,      Self.LanIP);
  MockLocator.AddA('localhost',                   '127.0.0.1');
end;

procedure TTestCaseTU.FireConnectionException(Msg: TIdSipMessage);
begin
  Self.FireOnException(Msg,
                       EIdConnectException,
                       '10061',
                       'Connection refused');
end;

procedure TTestCaseTU.FireOnException(Msg: TIdSipMessage;
                                      ExceptionType: ExceptClass;
                                      ExceptionMessage: String;
                                      Reason: String);
begin
  Self.Dispatcher.Transport.FireOnException(Msg,
                                            ExceptionType,
                                            ExceptionMessage,
                                            Reason);
end;

function TTestCaseTU.LastSentAck: TIdSipRequest;
begin
  Result := Self.SentAcks.Last;
end;

function TTestCaseTU.LastSentRequest: TIdSipRequest;
begin
  Result := Self.SentRequests.Last;
end;

function TTestCaseTU.LastSentResponse: TIdSipResponse;
begin
  Result := Self.SentResponses.Last;
end;

function TTestCaseTU.Locator: TIdSipMockLocator;
begin
  Result := Self.Dispatcher.MockLocator;
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

procedure TTestCaseTU.OnException(FailedMessage: TIdSipMessage;
                                  E: Exception;
                                  const Reason: String);
begin
end;

procedure TTestCaseTU.OnReceiveRequest(Request: TIdSipRequest;
                                       Receiver: TIdSipTransport;
                                       Source: TIdConnectionBindings);
begin
  if Request.IsAck then
    Self.SentAcks.AddCopy(Request)
  else
    Self.SentRequests.AddCopy(Request);
end;

procedure TTestCaseTU.OnReceiveResponse(Response: TIdSipResponse;
                                        Receiver: TIdSipTransport;
                                        Source: TIdConnectionBindings);
begin
  Self.SentResponses.AddCopy(Response);
end;

procedure TTestCaseTU.OnRejectedMessage(const Msg: String;
                                        const Reason: String;
                                        Source: TIdConnectionBindings);
begin
end;

procedure TTestCaseTU.OnSendRequest(Request: TIdSipRequest;
                                    Sender: TIdSipTransport;
                                    Destination: TIdConnectionBindings);
begin
  if Request.IsAck then
    Self.SentAcks.AddCopy(Request)
  else
    Self.SentRequests.AddCopy(Request);
end;

procedure TTestCaseTU.OnSendResponse(Response: TIdSipResponse;
                                     Sender: TIdSipTransport;
                                     Destination: TIdConnectionBindings);
begin
  Self.SentResponses.AddCopy(Response);
end;

procedure TTestCaseTU.ReceiveAck;
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

procedure TTestCaseTU.ReceiveAckFor(Request: TIdSipRequest;
                                    Response: TIdSipResponse);
var
  Ack: TIdSipRequest;
begin
  Ack := Request.AckFor(Response);
  try
    Self.ReceiveRequest(Ack);
  finally
    Ack.Free;
  end;
end;

procedure TTestCaseTU.ReceiveOk(Invite: TIdSipRequest;
                                const SDP: String = '');
var
  Response: TIdSipResponse;
begin
  // This message appears to originate from the network. Invite originates from
  // us so presumably has no To tag. Having come from the network, the response
  // WILL have a To tag.
  Response := Self.CreateRemoteOk(Invite);
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

procedure TTestCaseTU.ReceiveOkFrom(Invite: TIdSipRequest;
                                    const Contact: String);
var
  Response: TIdSipResponse;
begin
  // This message appears to originate from the network. Invite originates from
  // us so presumably has no To tag. Having come from the network, the response
  // WILL have a To tag.
  Response := Self.CreateRemoteOk(Invite);
  try
    Response.FirstContact.Value := Contact;
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TTestCaseTU.ReceiveMovedPermanently(const SipUrl: String);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                          SIPMovedPermanently);
  try
    Response.AddHeader(ContactHeaderFull).Value := SipUrl;
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TTestCaseTU.ReceiveRequest(Request: TIdSipRequest);
begin
  Self.Dispatcher.Transport.FireOnRequest(Request);
end;

procedure TTestCaseTU.ReceiveResponse(Response: TIdSipResponse);
begin
  Self.Dispatcher.Transport.FireOnResponse(Response);
end;

procedure TTestCaseTU.ReceiveResponse(StatusCode: Cardinal);
begin
  Self.ReceiveResponse(Self.LastSentRequest, StatusCode);
end;

procedure TTestCaseTU.ReceiveResponse(Request: TIdSipRequest;
                                      StatusCode: Cardinal);
var
  RemoteContact: TIdSipContactHeader;
  Response:      TIdSipResponse;
begin
  RemoteContact := TIdSipContactHeader.Create;
  try
    RemoteContact.Value := Self.Destination.FullValue;

    Response := Self.Core.CreateResponse(Request,
                                         StatusCode,
                                         RemoteContact);
    try
      Self.ReceiveResponse(Response);
    finally
      Response.Free;
    end;
  finally
    RemoteContact.Free;
  end;
end;

procedure TTestCaseTU.ReceiveBye(LocalDialog: TIdSipDialog);
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

procedure TTestCaseTU.ReceiveCancel;
var
  Cancel: TIdSipRequest;
begin
  Cancel := Self.Invite.CreateCancel;
  try
    Self.ReceiveRequest(Cancel);
  finally
    Cancel.Free;
  end;
end;

procedure TTestCaseTU.ReceiveInvite;
begin
  Self.ReceiveRequest(Self.Invite);
end;

procedure TTestCaseTU.ReceiveRinging(Invite: TIdSipRequest);
begin
  Self.ReceiveResponse(Invite, SIPRinging);
end;

procedure TTestCaseTU.ReceiveTrying(Invite: TIdSipRequest);
begin
  Self.ReceiveResponse(Invite, SIPTrying);
end;

procedure TTestCaseTU.ReceiveTryingFrom(Invite: TIdSipRequest;
                                        const Contact: String);
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Invite, SIPTrying);
  try
    Response.FirstContact.Value := Contact;
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TTestCaseTU.ReceiveTryingWithNoToTag(Invite: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Invite, SIPTrying);
  try
    // strip the To header tag
    Response.ToHeader.Value := Response.ToHeader.Value;

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TTestCaseTU.ReceiveUnauthorized(const AuthHeaderName: String;
                                          const Qop: String);
var
  Challenge: TIdSipResponse;
begin
  Challenge := Self.CreateRemoteUnauthorized(Self.LastSentRequest,
                                             AuthHeaderName,
                                             Qop);
  try
    Self.ReceiveResponse(Challenge);
  finally
    Challenge.Free;
  end;
end;

function TTestCaseTU.SecondLastSentRequest: TIdSipRequest;
begin
  Result := Self.SentRequests.SecondLast;
end;

function TTestCaseTU.SecondLastSentResponse: TIdSipResponse;
begin
  Result := Self.SentResponses.SecondLast;
end;

function TTestCaseTU.SentAckCount: Cardinal;
begin
  Result := Self.SentAcks.Count;
end;

function TTestCaseTU.SentRequestCount: Cardinal;
begin
  Result := Self.SentRequests.Count;
end;

function TTestCaseTU.SentResponseCount: Cardinal;
begin
  Result := Self.SentResponses.Count;
end;

function TTestCaseTU.SentRequestAt(Index: Integer): TIdSipRequest;
begin
  Result := Self.SentRequests[Index];
end;

procedure TTestCaseTU.SetRoutingTableForSingleLanIP;
begin
  // This machine has one loopback IP address, and one LAN IP address. It sits
  // on a network with a NATting gateway to the internet.

  Self.AddLanRoute(Self.RoutingTable);
  Self.AddLoopbackRoute(Self.RoutingTable);
  Self.AddDefaultRoute(Self.RoutingTable, Self.LanGateway, Self.LanIP);
end;

function TTestCaseTU.ThirdLastSentRequest: TIdSipRequest;
begin
  Result := Self.Dispatcher.Transport.ThirdLastRequest;
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
//* TActionMethodTestCase                                                      *
//******************************************************************************
//* TActionMethodTestCase Public methods ***************************************

procedure TActionMethodTestCase.SetUp;
begin
  inherited SetUp;

  Self.Binding           := TIdConnectionBindings.Create;
  Self.Binding.LocalIP   := '127.0.0.1';
  Self.Binding.LocalPort := 5060;
  Self.Binding.PeerIP    := '127.0.0.1';
  Self.Binding.PeerPort  := 5060;
  Self.Binding.Transport := UdpTransport;

  Self.Dispatcher := TIdSipMockTransactionDispatcher.Create;

  Self.UA := TIdSipUserAgent.Create;
  Self.UA.Dispatcher := Self.Dispatcher;

  Self.Response := TIdSipResponse.Create;
end;

procedure TActionMethodTestCase.TearDown;
begin
  Self.Response.Free;
  Self.UA.Free; // The UA frees the dispatcher
  Self.Binding.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TIdSipExceptionRaisingHeader                                               *
//******************************************************************************
//* TIdSipExceptionRaisingHeader Protected methods *****************************

procedure TIdSipExceptionRaisingHeader.Parse(const Value: String);
begin
  raise EBadHeader.Create(Self.ClassName + '.Parse');
end;

//******************************************************************************
//* TIdSipFailableListener                                                     *
//******************************************************************************
//* TIdSipFailableListener Public methods **************************************

constructor TIdSipFailableListener.Create;
begin
  inherited Create;

  Self.fReasonParam := '';
end;

//******************************************************************************
//* TIdSipTestActionListener                                                   *
//******************************************************************************
//* TIdSipTestActionListener Public methods ************************************

constructor TIdSipTestActionListener.Create;
begin
  inherited Create;

  Self.FailWith                  := nil;
  Self.fAuthenticationChallenged := false;
  Self.fNetworkFailed            := false;
  Self.fTerminated               := false;
end;

//* TIdSipTestActionListener Private methods ***********************************

procedure TIdSipTestActionListener.OnAuthenticationChallenge(Action: TIdSipAction;
                                                       Response: TIdSipResponse);
begin
  Self.fActionParam              := Action;
  Self.fAuthenticationChallenged := true;
  Self.fResponseParam            := Response;
end;

procedure TIdSipTestActionListener.OnNetworkFailure(Action: TIdSipAction;
                                              ErrorCode: Cardinal;
                                              const Reason: String);
begin
  Self.fActionParam    := Action;
  Self.fErrorCodeParam := ErrorCode;
  Self.fNetworkFailed  := true;
  Self.fReasonParam    := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFailure');
end;

procedure TIdSipTestActionListener.OnTerminated(Action: TIdSipAction);
begin
  Self.fActionParam := Action;
  Self.fTerminated  := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnTerminated');
end;

//******************************************************************************
//* TIdSipTestDataListener                                                     *
//******************************************************************************
//* TIdSipTestDataListener Public methods **************************************

constructor TIdSipTestDataListener.Create;
begin
  inherited Create;

  Self.fNewData    := false;
  Self.fNewUdpData := false;
end;

procedure TIdSipTestDataListener.OnNewData(Data: TIdRTPPayload;
                                           Binding: TIdConnectionBindings);
begin
  Self.fNewData := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnNewData');
end;

procedure TIdSipTestDataListener.OnNewUdpData(Data: TStream);
begin
  Self.fNewUdpData := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnNewUdpData');
end;

//******************************************************************************
//* TIdSipTestMessageListener                                                  *
//******************************************************************************
//* TIdSipTestMessageListener Public methods ***********************************

constructor TIdSipTestMessageListener.Create;
begin
  inherited Create;

  Self.fException        := false;
  Self.fMalformedMessage := false;
  Self.fReceivedRequest  := false;
  Self.fReceivedResponse := false;
end;

//* TIdSipTestMessageListener Private methods **********************************

procedure TIdSipTestMessageListener.OnException(E: Exception;
                                                const Reason: String);
begin
  Self.fException      := true;
  Self.fExceptionParam := E;
  Self.fReasonParam    := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnException');
end;

procedure TIdSipTestMessageListener.OnMalformedMessage(const Msg: String;
                                                       const Reason: String;
                                                       Source: TIdConnectionBindings);
begin
  Self.fMalformedMessage      := true;
  Self.fMalformedMessageParam := Msg;
  Self.fReasonParam           := Reason;
  Self.fReceivedFromParam     := Source;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnMalformedMessage');
end;

procedure TIdSipTestMessageListener.OnReceiveRequest(Request: TIdSipRequest;
                                                     ReceivedFrom: TIdConnectionBindings);
begin
  Self.fReceivedRequest   := true;
  Self.fRequestParam      := Request;
  Self.fReceivedFromParam := ReceivedFrom;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveRequest');
end;

procedure TIdSipTestMessageListener.OnReceiveResponse(Response: TIdSipResponse;
                                                      ReceivedFrom: TIdConnectionBindings);
begin
  Self.fReceivedResponse  := true;
  Self.fResponseParam     := Response;
  Self.fReceivedFromParam := ReceivedFrom;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveResponse');
end;

//******************************************************************************
//* TIdSipTestObserver                                                         *
//******************************************************************************
//* TIdSipTestObserver Public methods ******************************************

constructor TIdSipTestObserver.Create;
begin
  inherited Create;

  Self.fChanged := false;
end;

//* TIdSipTestObserver Private methods *****************************************

procedure TIdSipTestObserver.OnChanged(Observed: TObject);
begin
  Self.fChanged := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnChanged');
end;

//******************************************************************************
//* TIdSipTestInboundInviteListener                                            *
//******************************************************************************
//* TIdSipTestInboundInviteListener Public methods *****************************

constructor TIdSipTestInboundInviteListener.Create;
begin
  inherited Create;

  Self.fAckParam         := nil;
  Self.fFailed           := false;
  Self.fInviteAgentParam := nil;
  Self.fSucceeded        := false;
end;

//* TIdSipTestInboundInviteListener Private methods ****************************

procedure TIdSipTestInboundInviteListener.OnFailure(InviteAgent: TIdSipInboundInvite);
begin
  Self.fFailed           := true;
  Self.fInviteAgentParam := InviteAgent;
end;

procedure TIdSipTestInboundInviteListener.OnSuccess(InviteAgent: TIdSipInboundInvite;
                                                    Ack: TIdSipMessage);
begin
  Self.fAckParam         := Ack;
  Self.fInviteAgentParam := InviteAgent;
  Self.fSucceeded        := true;
end;

//******************************************************************************
//* TIdSipOwnedActionListener                                                  *
//******************************************************************************
//* TIdSipOwnedActionListener Public methods ***********************************

constructor TIdSipOwnedActionListener.Create;
begin
  inherited Create;

  Self.fFailure    := false;
  Self.fRedirected := false;
  Self.fSuccess    := false;
end;

//* TIdSipOwnedActionListener Private methods **********************************

procedure TIdSipOwnedActionListener.OnFailure(Action: TIdSipAction;
                                              Response: TIdSipResponse;
                                              const Reason: String);
begin
  Self.fFailure       := true;
  Self.fActionParam   := Action;
  Self.fResponseParam := Response;
  Self.fReasonParam   := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFailure');
end;

procedure TIdSipOwnedActionListener.OnRedirect(Action: TIdSipAction;
                                               Redirect: TIdSipResponse);
begin
  Self.fRedirected    := true;
  Self.fActionParam   := Action;
  Self.fRedirectParam := Redirect;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnRedirect');
end;

procedure TIdSipOwnedActionListener.OnSuccess(Action: TIdSipAction;
                                              Msg: TIdSipMessage);
begin
  Self.fActionParam := Action;
  Self.fMsgParam    := Msg;
  Self.fSuccess     := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnSuccess');
end;

//******************************************************************************
//* TIdSipTestInviteListener                                                   *
//******************************************************************************
//* TIdSipTestInviteListener Public methods ************************************

constructor TIdSipTestInviteListener.Create;
begin
  inherited Create;

  Self.fCallProgress      := false;
  Self.fDialogEstablished := false;
end;

//* TIdSipTestInviteListener Private methods **********************************

procedure TIdSipTestInviteListener.OnCallProgress(InviteAgent: TIdSipOutboundInvite;
                                                  Response: TIdSipResponse);
begin
  Self.fCallProgress     := true;
  Self.fInviteAgentParam := InviteAgent;
  Self.fResponseParam    := Response;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnCallProgress');
end;

procedure TIdSipTestInviteListener.OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                                       NewDialog: TidSipDialog);
begin
  Self.fDialogEstablished := true;
  Self.fInviteAgentParam  := InviteAgent;
  Self.fDialogParam       := NewDialog;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnDialogEstablished');
end;

//******************************************************************************
//* TIdSipTestInviteModuleListener                                             *
//******************************************************************************
//* TIdSipTestInviteModuleListener Public methods ******************************

constructor TIdSipTestInviteModuleListener.Create;
begin
  inherited Create;

  Self.fInboundCall := false;
end;

//* TIdSipTestInviteModuleListener Private methods *****************************

procedure TIdSipTestInviteModuleListener.OnInboundCall(UserAgent: TIdSipInviteModule;
                                                       Session: TIdSipInboundSession);
begin
  Self.fInboundCall    := true;
  Self.fSessionParam   := Session;
  Self.fUserAgentParam := UserAgent;
end;

//******************************************************************************
//* TIdSipTestNotifyListener                                                   *
//******************************************************************************
//* TIdSipTestNotifyListener Public methods ************************************

constructor TIdSipTestNotifyListener.Create;
begin
  inherited Create;

  Self.fFailed    := false;
  Self.fSucceeded := false;
end;

//* TIdSipTestNotifyListener Published methods *********************************

procedure TIdSipTestNotifyListener.OnFailure(NotifyAgent: TIdSipOutboundNotify;
                                             Response: TIdSipResponse);
begin
  Self.fFailed           := true;
  Self.fNotifyAgentParam := NotifyAgent;
  Self.fResponseParam    := Response;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFailure');
end;

procedure TIdSipTestNotifyListener.OnSuccess(NotifyAgent: TIdSipOutboundNotify;
                                             Response: TIdSipResponse);
begin
  Self.fSucceeded        := true;
  Self.fNotifyAgentParam := NotifyAgent;
  Self.fResponseParam    := Response;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnSuccess');
end;

//******************************************************************************
//* TIdSipTestOptionsListener                                                  *
//******************************************************************************
//* TIdSipTestOptionsListener Public methods ***********************************

constructor TIdSipTestOptionsListener.Create;
begin
  inherited Create;

  Self.fResponse := false;
end;

//* TIdSipTestOptionsListener Private methods **********************************

procedure TIdSipTestOptionsListener.OnResponse(OptionsAgent: TIdSipOutboundOptions;
                                               Response: TIdSipResponse);
begin
  Self.fOptionsAgentParam := OptionsAgent;
  Self.fResponseParam     := Response;
  Self.fResponse          := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnResponse');
end;

//******************************************************************************
//* TIdSipTestRegistrationListener                                             *
//******************************************************************************
//* TIdSipTestRegistrationListener Public methods ******************************

constructor TIdSipTestRegistrationListener.Create;
begin
  inherited Create;

  Self.fFailure := false;
  Self.fSuccess := false;
end;

//* TIdSipRegistrationListener Private methods *********************************

procedure TIdSipTestRegistrationListener.OnFailure(RegisterAgent: TIdSipOutboundRegistrationBase;
                                                   ErrorCode: Cardinal;
                                                   const Reason: String);
begin
  Self.fErrorCodeParam       := ErrorCode;
  Self.fFailure              := true;
  Self.fReasonParam          := Reason;
  Self.fRegisterAgentParam   := RegisterAgent;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFailure');
end;

procedure TIdSipTestRegistrationListener.OnSuccess(RegisterAgent: TIdSipOutboundRegistrationBase;
                                                   CurrentBindings: TIdSipContacts);
begin
  Self.fCurrentBindingsParam := CurrentBindings;
  Self.fRegisterAgentParam   := RegisterAgent;
  Self.fSuccess              := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnSuccess');
end;

//******************************************************************************
//* TIdSipTestActionRedirectorListener                                         *
//******************************************************************************
//* TIdSipTestActionRedirectorListener Public methods **************************

constructor TIdSipTestActionRedirectorListener.Create;
begin
  inherited Create;

  Self.fFailed         := false;
  Self.fRedirectFailed := false;
  Self.fNewAction      := false;
  Self.fSucceeded      := false;
end;

//* TIdSipTestActionRedirectorListener Private methods *************************

procedure TIdSipTestActionRedirectorListener.OnFailure(Redirector: TIdSipActionRedirector;
                                                       Response: TIdSipResponse);
begin
  Self.fFailed          := true;
  Self.fRedirectorParam := Redirector;
  Self.fResponseParam   := Response;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFailure');
end;

procedure TIdSipTestActionRedirectorListener.OnNewAction(Redirector: TIdSipActionRedirector;
                                                         NewAction: TIdSipAction);
begin
  Self.fNewAction       := true;
  Self.fNewActionParam  := NewAction;
  Self.fRedirectorParam := Redirector;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnNewAction');
end;

procedure TIdSipTestActionRedirectorListener.OnRedirectFailure(Redirector: TIdSipActionRedirector;
                                                               ErrorCode: Cardinal;
                                                               const Reason: String);
begin
  Self.fErrorCodeParam  := ErrorCode;
  Self.fRedirectFailed   := true;
  Self.fReasonParam     := Reason;
  Self.fRedirectorParam := Redirector;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnRedirectFailure');
end;

procedure TIdSipTestActionRedirectorListener.OnSuccess(Redirector: TIdSipActionRedirector;
                                                       SuccessfulAction: TIdSipAction;
                                                       Response: TIdSipResponse);
begin
  Self.fSucceeded             := true;
  Self.fRedirectorParam       := Redirector;
  Self.fResponseParam         := Response;
  Self.fSuccessfulActionParam := SuccessfulAction;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnSuccess');
end;

//******************************************************************************
//* TIdSipTestSessionListener                                                  *
//******************************************************************************
//* TIdSipTestSessionListener Public methods ***********************************

constructor TIdSipTestSessionListener.Create;
begin
  inherited Create;

  Self.fEndedSession             := false;
  Self.fEstablishedSession       := false;
  Self.fMimeType                 := '';
  Self.fModifiedSession          := false;
  Self.fNewSession               := false;
  Self.fProgressedSession        := false;
  Self.fRedirect                 := false;
  Self.fReferral                 := false;
  Self.fRemoteSessionDescription := '';
end;

procedure TIdSipTestSessionListener.OnRedirect(Action: TIdSipAction;
                                               Redirect: TIdSipResponse);
begin
  Self.fRedirect := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnRedirect');
end;

procedure TIdSipTestSessionListener.OnEndedSession(Session: TIdSipSession;
                                                   ErrorCode: Cardinal;
                                                   const Reason: String);
begin
  Self.fEndedSession   := true;
  Self.fErrorCodeParam := ErrorCode;
  Self.fReasonParam    := Reason;
  Self.fSessionParam   := Session;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnEndedSession');
end;

procedure TIdSipTestSessionListener.OnEstablishedSession(Session: TIdSipSession;
                                                         const RemoteSessionDescription: String;
                                                         const MimeType: String);
begin
  Self.fEstablishedSession       := true;
  Self.fSessionParam             := Session;
  Self.fRemoteSessionDescription := RemoteSessionDescription;
  Self.fMimeType                 := MimeType;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnEstablishedSession');
end;

procedure TIdSipTestSessionListener.OnModifiedSession(Session: TIdSipSession;
                                                      Answer: TIdSipResponse);
begin
  Self.fAnswerParam     := Answer;
  Self.fModifiedSession := true;
  Self.fSessionParam    := Session;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnModifiedSession');
end;

procedure TIdSipTestSessionListener.OnModifySession(Session: TIdSipSession;
                              const RemoteSessionDescription: String;
                              const MimeType: String);
begin
  Self.fModifySession            := true;
  Self.fSessionParam             := Session;
  Self.fRemoteSessionDescription := RemoteSessionDescription;
  Self.fMimeType                 := MimeType;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnModifiedSession');
end;

procedure TIdSipTestSessionListener.OnNewSession(Session: TIdSipSession);
begin
  Self.fNewSession   := true;
  Self.fSessionParam := Session;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnNewSession');
end;

procedure TIdSipTestSessionListener.OnProgressedSession(Session: TIdSipSession;
                                                        Progress: TIdSipResponse);
begin
  Self.fProgressedSession := true;
  Self.fProgressParam     := Progress;
  Self.fSessionParam      := Session;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnProgressedSession');
end;

procedure TIdSipTestSessionListener.OnReferral(Session: TIdSipSession;
                                               Refer: TIdSipRequest;
                                               Binding: TIdConnectionBindings);
begin
  Self.fBindingParam         := Binding;
  Self.fReferral             := true;
  Self.fReferParam           := Refer;
  Self.fSessionParam         := Session;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReferral');
end;

//******************************************************************************
//* TIdSipTestSessionListenerEndedCounter                                      *
//******************************************************************************
//* TIdSipTestSessionListenerEndedCounter Public methods ***********************

constructor TIdSipTestSessionListenerEndedCounter.Create;
begin
  inherited Create;

  Self.fEndedNotificationCount := 0;
end;

procedure TIdSipTestSessionListenerEndedCounter.OnEndedSession(Session: TIdSipSession;
                                                               ErrorCode: Cardinal;
                                                               const Reason: String);
begin
  inherited OnEndedSession(Session, ErrorCode, Reason);

  Inc(Self.fEndedNotificationCount);
end;

//******************************************************************************
//* TIdSipTestSubscriptionListener                                             *
//******************************************************************************
//* TIdSipTestSubscriptionListener Public methods ******************************

constructor TIdSipTestSubscriptionListener.Create;
begin
  inherited Create;

  Self.fEstablishedSubscription := false;
  Self.fExpiredSubscription     := false;
  Self.fFailedSubscription      := false;
  Self.fNotify                  := false;
  Self.fRenewedSubscription     := false;
end;

//* TIdSipTestSubscriptionListener Private methods ******************************

procedure TIdSipTestSubscriptionListener.OnEstablishedSubscription(Subscription: TIdSipOutboundSubscription;
                                                                   Notify: TIdSipRequest);
begin
  Self.fEstablishedSubscription := true;
  Self.fNotifyParam             := Notify;
  Self.fSubscriptionParam       := Subscription;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnEstablishedSubscription');
end;

procedure TIdSipTestSubscriptionListener.OnExpiredSubscription(Subscription: TIdSipOutboundSubscription;
                                                               Notify: TIdSipRequest);
begin
  Self.fExpiredSubscription := true;
  Self.fNotifyParam         := Notify;
  Self.fSubscriptionParam   := Subscription;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnExpiredSubscription');
end;

procedure TIdSipTestSubscriptionListener.OnFailure(Subscription: TIdSipOutboundSubscription;
                                                   Response: TIdSipResponse);
begin
  Self.fFailedSubscription := true;
  Self.fResponseParam      := Response;
  Self.fSubscriptionParam  := Subscription;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFailedSubscription');
end;

procedure TIdSipTestSubscriptionListener.OnNotify(Subscription: TIdSipOutboundSubscription;
                                                  Notify: TIdSipRequest);
begin
  Self.fNotify            := true;
  Self.fSubscriptionParam := Subscription;
  Self.fNotifyParam       := Notify;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnNotify');
end;

//******************************************************************************
//* TIdSipTestTransactionListener                                              *
//******************************************************************************
//* TIdSipTestTransactionListener Public methods *******************************

constructor TIdSipTestTransactionListener.Create;
begin
  inherited Create;

  Self.fFailed           := false;
  Self.fReceivedRequest  := false;
  Self.fReceivedResponse := false;
  Self.fTerminated       := false;
end;

//* TIdSipTestTransactionListener Private methods ******************************

procedure TIdSipTestTransactionListener.OnFail(Transaction: TIdSipTransaction;
                                               FailedMessage: TIdSipMessage;
                                               const Reason: String);
begin
  Self.fFailed             := true;
  Self.fFailedMessageParam := FailedMessage;
  Self.fReasonParam        := Reason;
  Self.fTransactionParam   := Transaction;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFail');
end;

procedure TIdSipTestTransactionListener.OnReceiveRequest(Request: TIdSipRequest;
                                                         Transaction: TIdSipTransaction;
                                                         Binding: TIdConnectionBindings);
begin
  Self.fBindingParam     := Binding;
  Self.fReceivedRequest  := true;
  Self.fRequestParam     := Request;
  Self.fTransactionParam := Transaction;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveRequest');
end;

procedure TIdSipTestTransactionListener.OnReceiveResponse(Response: TIdSipResponse;
                                                          Transaction: TIdSipTransaction;
                                                          Binding: TIdConnectionBindings);
begin
  Self.fBindingParam     := Binding;
  Self.fReceivedResponse := true;
  Self.fResponseParam    := Response;
  Self.fTransactionParam := Transaction;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveResponse');
end;

procedure TIdSipTestTransactionListener.OnTerminated(Transaction: TIdSipTransaction);
begin
  Self.fTerminated       := true;
  Self.fTransactionParam := Transaction;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnTerminated');
end;

//******************************************************************************
//* TIdSipTestTransactionDispatcherListener                                    *
//******************************************************************************
//* TIdSipTestTransactionDispatcherListener Public methods *********************

constructor TIdSipTestTransactionDispatcherListener.Create;
begin
  inherited Create;

  Self.fRaisedException           := false;
  Self.fReceivedRequest           := false;
  Self.fReceivedResponse          := false;
  Self.fReceivedUnhandledRequest  := false;
  Self.fReceivedUnhandledResponse := false;
end;

//* TIdSipTestTransactionDispatcherListener Private methods ********************

procedure TIdSipTestTransactionDispatcherListener.OnReceiveRequest(Request: TIdSipRequest;
                                                                   Binding: TIdConnectionBindings);
begin
  Self.fBindingParam    := Binding;
  Self.fReceivedRequest := true;
  Self.fRequestParam    := Request;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveRequest');
end;

procedure TIdSipTestTransactionDispatcherListener.OnReceiveResponse(Response: TIdSipResponse;
                                                                    Binding: TIdConnectionBindings);
begin
  Self.fBindingParam     := Binding;
  Self.fReceivedResponse := true;
  Self.fResponseParam    := Response;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveResponse');
end;

procedure TIdSipTestTransactionDispatcherListener.OnTransportException(FailedMessage: TIdSipMessage;
                                                                       const Reason: String);
begin
  Self.fFailedMessageParam := FailedMessage;
  Self.fRaisedException    := true;
  Self.fReasonParam        := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnTransportException');
end;

//******************************************************************************
//* TIdSipTestTransactionUserListener
//******************************************************************************
//* TIdSipTestTransactionUserListener Public methods ***************************

constructor TIdSipTestTransactionUserListener.Create;
begin
  inherited Create;

  Self.fActionAdded             := false;
  Self.fDroppedUnmatchedMessage := false;
  Self.fActionRemoved           := false;
end;

//* TIdSipTestTransactionUserListener Private methods **************************

procedure TIdSipTestTransactionUserListener.OnAddAction(UserAgent: TIdSipAbstractCore;
                                                        Action: TIdSipAction);
begin
  Self.fAbstractUserAgentParam := UserAgent;
  Self.fActionAdded            := true;
  Self.fActionParam            := Action;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnAddAction');
end;


procedure TIdSipTestTransactionUserListener.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                                                      Message: TIdSipMessage;
                                                                      Binding: TIdConnectionBindings);
begin
  Self.fAbstractUserAgentParam  := UserAgent;
  Self.fBindingParam            := Binding;
  Self.fDroppedUnmatchedMessage := true;
  Self.fMessageParam            := Message;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnDroppedUnmatchedMessage');
end;

procedure TIdSipTestTransactionUserListener.OnRemoveAction(UserAgent: TIdSipAbstractCore;
                                                           Action: TIdSipAction);
begin
  Self.fAbstractUserAgentParam := UserAgent;
  Self.fActionRemoved          := true;
  Self.fActionParam            := Action;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnRemoveAction');
end;
//******************************************************************************
//* TIdSipTestSubscribeModuleListener                                          *
//******************************************************************************
//* TIdSipTestSubscribeModuleListener Public methods ***************************

constructor TIdSipTestSubscribeModuleListener.Create;
begin
  inherited Create;


  Self.fRenewedSubscription := false;
  Self.fSubscriptionRequest := false;
end;

//* TIdSipTestSubscribeModuleListener Private methods **************************

procedure TIdSipTestSubscribeModuleListener.OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                                                  Subscription: TIdSipOutboundSubscription);
begin
  Self.fRenewedSubscription := true;
  Self.fSubscriptionParam   := Subscription;
  Self.fUserAgentParam      := UserAgent;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnInboundCall');
end;

procedure TIdSipTestSubscribeModuleListener.OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                                                  Subscription: TIdSipInboundSubscription);
begin
  Self.fSubscriptionRequest := true;
  Self.fSubscriptionParam   := Subscription;
  Self.fUserAgentParam      := UserAgent;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnInboundCall');
end;

//******************************************************************************
//* TIdSipTestTimerQueueListener                                               *
//******************************************************************************
//* TIdSipTestTimerQueueListener Public methods ********************************

constructor TIdSipTestTimerQueueListener.Create;
begin
  inherited Create;

  Self.fExceptionFired := false;
end;

//* TIdSipTestTimerQueueListener Private methods *******************************

procedure TIdSipTestTimerQueueListener.OnException(Timer: TIdTimerQueue;
                                                   Error: Exception;
                                                   Wait: TIdWait);
begin
  Self.fExceptionFired   := true;
  Self.fExceptionMessage := Error.Message;
  Self.fExceptionType    := ExceptClass(Error.ClassType);
  Self.fTimerParam       := Timer;
  Self.fWaitType         := TIdWaitClass(Wait);
end;

//******************************************************************************
//* TIdSipActionFinder                                                         *
//******************************************************************************
//* TIdSipActionFinder Public methods ******************************************

procedure TIdSipActionFinder.Execute(Action: TIdSipAction);
begin
  Self.fAction := Action;
end;

//******************************************************************************
//* TIdSipActionSwitch                                                         *
//******************************************************************************
//* TIdSipActionSwitch Public methods ******************************************

constructor TIdSipActionSwitch.Create;
begin
  inherited Create;

  Self.fExecuted := false;
end;

procedure TIdSipActionSwitch.Execute(Action: TIdSipAction);
begin
  Self.fExecuted := true;
end;

//******************************************************************************
//* TIdSipStackWindow                                                          *
//******************************************************************************
//* TIdSipStackWindow Public methods *******************************************

constructor TIdSipStackWindow.CreateNew(AOwner: TComponent; TestCase: IIdSipStackInterface);
begin
  inherited CreateNew(AOwner, 0);

  Self.fTestCase := TestCase;
end;

procedure TIdSipStackWindow.DefaultHandler(var Message);
begin
  inherited DefaultHandler(Message);

  if Self.IsStackMessage(TMessage(Message)) then
    Self.NotifyTestCase(TIdSipEventMessage(Message));
end;

//* TIdSipStackWindow Private methods ******************************************

function TIdSipStackWindow.IsStackMessage(Msg: TMessage): Boolean;
begin
  Result := ((Msg.Msg >= CM_BASE) and (Msg.Msg <= CM_LAST));
end;

procedure TIdSipStackWindow.NotifyTestCase(Msg: TIdSipEventMessage);
begin
  try
    Self.TestCase.OnEvent(Msg.Data.Stack, Msg.Data.Event, Msg.Data.Data);
  finally
    Msg.Data.Free;
  end;
end;

end.
