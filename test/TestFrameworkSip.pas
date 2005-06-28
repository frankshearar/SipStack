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
  Classes, IdInterfacedObject, IdObservable, IdRTP, IdSdp, IdSipMessage,
  IdSipCore, IdSipDialog, IdSipMockLocator, IdSipMockTransactionDispatcher,
  IdSipTcpClient, IdSipTcpServer, IdSipTransaction, IdSipTransport,
  IdTimerQueue, SysUtils, TestFrameworkEx;

type
  TIdSipTestResources = class(TObject)
  private
    class function CreateCommonRequest: TIdSipRequest;
  public
    class function CreateBasicRequest: TIdSipRequest;
    class function CreateBasicResponse: TIdSipResponse;
    class function CreateLocalLoopRequest: TIdSipRequest;
    class function CreateLocalLoopResponse: TIdSipResponse;
    class function BasicSDP(const Host: String): String;
    class function VeryLargeSDP(const Host: String): String;
  end;

  TTestCaseSip = class(TThreadingTestCase)
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

  TTestCaseTU = class(TTestCaseSip)
  private
    procedure RemoveBody(Msg: TIdSipMessage);
  protected
    AckCount:      Cardinal;
    Core:          TIdSipUserAgent;
    DebugTimer:    TIdDebugTimerQueue;
    Destination:   TIdSipToHeader;
    Dispatcher:    TIdSipMockTransactionDispatcher;
    Invite:        TIdSipRequest;
    RequestCount:  Cardinal;
    ResponseCount: Cardinal;

    function  CreateRemoteBye(LocalDialog: TIdSipDialog): TIdSipRequest;
    function  CreateRemoteOk(Request: TIdSipRequest): TIdSipResponse;
    function  LastSentAck: TIdSipRequest;
    function  LastSentRequest: TIdSipRequest;
    function  LastSentResponse: TIdSipResponse;
    function  Locator: TIdSipMockLocator;
    procedure MarkSentAckCount;
    procedure MarkSentRequestCount;
    procedure MarkSentResponseCount;
    procedure ReceiveAck;
    procedure ReceiveAckFor(Request: TIdSipRequest;
                            Response: TIdSipResponse);
    procedure ReceiveBye(LocalDialog: TIdSipDialog);
    procedure ReceiveCancel;
    procedure ReceiveInvite;
    procedure ReceiveOk(Invite: TIdSipRequest);
    procedure ReceiveOkFrom(Invite: TIdSipRequest;
                            const Contact: String);
    procedure ReceiveMovedPermanently(const SipUrl: String);
    procedure ReceiveRequest(Request: TIdSipRequest);
    procedure ReceiveResponse(Response: TIdSipResponse); overload;
    procedure ReceiveResponse(StatusCode: Cardinal); overload;
    procedure ReceiveResponse(Request: TIdSipRequest;
                              StatusCode: Cardinal); overload;
    procedure ReceiveRinging(Invite: TIdSipRequest);
    procedure ReceiveSubscribe(const EventPackage: String);
    procedure ReceiveTrying(Invite: TIdSipRequest);
    procedure ReceiveTryingFrom(Invite: TIdSipRequest;
                                const Contact: String);
    procedure ReceiveTryingWithNoToTag(Invite: TIdSipRequest);
    procedure ReceiveUnauthorized(const AuthHeaderName: String;
                                  const Qop: String);
    function  SecondLastSentRequest: TIdSipRequest;
    function  SecondLastSentResponse: TIdSipResponse;
    function  SentAckCount: Cardinal;
    function  SentRequestCount: Cardinal;
    function  SentResponseCount: Cardinal;
    function  SentRequestAt(Index: Integer): TIdSipRequest;
    function  ThirdLastSentRequest: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckAckSent(const Msg: String);
    procedure CheckNoRequestSent(const Msg: String);
    procedure CheckRequestSent(const Msg: String);
    procedure CheckNoResponseSent(const Msg: String);
    procedure CheckResponseSent(const Msg: String);
  end;

  TIdSipExceptionRaisingHeader = class(TIdSipHeader)
  protected
    procedure Parse(const Value: String); override;
  end;

  TIdSipMockListener = class(TIdInterfacedObject,
                             IIdSipActionListener)
  private
    fActionParam:              TIdSipAction;
    fAuthenticationChallenged: Boolean;
    fErrorCodeParam:           Cardinal;
    fFailWith:                 ExceptClass;
    fNetworkFailed:            Boolean;
    fResponseParam:            TIdSipResponse;

    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
  protected
    fReasonParam: String;
  public
    constructor Create; virtual;

    property ActionParam:    TIdSipAction   read fActionParam;
    property ErrorCodeParam: Cardinal       read fErrorCodeParam;
    property FailWith:       ExceptClass    read fFailWith write fFailWith;
    property NetworkFailed:  Boolean        read fNetworkFailed;
    property ReasonParam:    String         read fReasonParam;
    property ResponseParam:  TIdSipResponse read fResponseParam;
  end;

  TIdSipTestDataListener = class(TIdSipMockListener,
                                 IIdRtpDataListener)
  private
    fNewData:    Boolean;
    fNewUdpData: Boolean;
  public
    constructor Create; override;

    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdConnection);
    procedure OnNewUdpData(Data: TStream);

    property NewData:    Boolean read fNewData;
    property NewUdpData: Boolean read fNewUdpData;
  end;

  TIdSipTestMessageListener = class(TIdSipMockListener,
                                    IIdSipMessageListener)
  private
    fException:             Boolean;
    fExceptionParam:        Exception;
    fMalformedMessage:      Boolean;
    fMalformedMessageParam: String;
    fReceivedFromParam:     TIdSipConnectionBindings;
    fReceivedRequest:       Boolean;
    fReceivedResponse:      Boolean;
    fRequestParam:          TIdSipRequest;
    fResponseParam:         TIdSipResponse;

    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnMalformedMessage(const Msg: String;
                                 const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdSipConnectionBindings);
  public
    constructor Create; override;

    property Exception:             Boolean                  read fException;
    property ExceptionParam:        Exception                read fExceptionParam;
    property MalformedMessage:      Boolean                  read fMalformedMessage;
    property MalformedMessageParam: String                   read fMalformedMessageParam;
    property ReceivedFromParam:     TIdSipConnectionBindings read fReceivedFromParam;
    property ReceivedRequest:       Boolean                  read fReceivedRequest;
    property ReceivedResponse:      Boolean                  read fReceivedResponse;
    property RequestParam:          TIdSipRequest            read fRequestParam;
    property ResponseParam:         TIdSipResponse           read fResponseParam;
  end;

  TIdSipTestObserver = class(TIdSipMockListener,
                             IIdObserver)
  private
    fChanged: Boolean;

    procedure OnChanged(Observed: TObject);
  public
    constructor Create; override;

    property Changed: Boolean read fChanged;
  end;

  TIdSipTestInboundInviteListener = class(TIdSipMockListener,
                                          IIdSipInboundInviteListener)
  private
    fAckParam:          TIdSipRequest;
    fFailed:            Boolean;
    fInviteAgentParam:  TIdSipInboundInvite;
    fSucceeded:         Boolean;

    procedure OnFailure(InviteAgent: TIdSipInboundInvite);
    procedure OnSuccess(InviteAgent: TIdSipInboundInvite;
                        Ack: TIdSipRequest);
  public
    constructor Create; override;

    property AckParam:         TIdSipRequest       read fAckParam;
    property Failed:           Boolean             read fFailed;
    property InviteAgentParam: TIdSipInboundInvite read fInviteAgentParam;
    property Succeeded:        Boolean             read fSucceeded;
  end;


  TIdSipTestInviteListener = class(TIdSipMockListener,
                                   IIdSipInviteListener)
  private
    fCallProgress:      Boolean;
    fDialogEstablished: Boolean;
    fDialogParam:       TIdSipDialog;
    fFailure:           Boolean;
    fInviteAgentParam:  TIdSipOutboundInvite;
    fReasonParam:       String;
    fRedirect:          Boolean;
    fResponseParam:     TIdSipResponse;
    fSuccess:           Boolean;

    procedure OnCallProgress(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse);
    procedure OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                  NewDialog: TidSipDialog);
    procedure OnFailure(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse;
                        const Reason: String);
    procedure OnRedirect(InviteAgent: TIdSipOutboundInvite;
                         Redirect: TIdSipResponse);
    procedure OnSuccess(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse);
  public
    constructor Create; override;

    property CallProgress:      Boolean              read fCallProgress;
    property DialogEstablished: Boolean              read fDialogEstablished;
    property DialogParam:       TIdSipDialog         read fDialogParam;
    property Failure:           Boolean              read fFailure;
    property InviteAgentParam:  TIdSipOutboundInvite read fInviteAgentParam;
    property ReasonParam:       String               read fReasonParam;
    property Redirect:          Boolean              read fRedirect;
    property ResponseParam:     TIdSipResponse       read fResponseParam;
    property Success:           Boolean              read fSuccess;
  end;

  TIdSipTestNotifyListener = class(TIdSipMockListener,
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


  TIdSipTestOptionsListener = class(TIdSipMockListener,
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

  TIdSipTestRegistrationListener = class(TIdSipMockListener,
                                         IIdSipRegistrationListener)
  private
    fCurrentBindingsParam: TIdSipContacts;
    fFailure:              Boolean;
    fReasonParam:          String;
    fRegisterAgentParam:   TIdSipOutboundRegistration;
    fSuccess:              Boolean;
  public
    constructor Create; override;

    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts;
                        const Reason: String);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts);

    property CurrentBindingsParam: TIdSipContacts             read fCurrentBindingsParam;
    property Failure:              Boolean                    read fFailure;
    property ReasonParam:          String                     read fReasonParam;
    property RegisterAgentParam:   TIdSipOutboundRegistration read fRegisterAgentParam;
    property Success:              Boolean                    read fSuccess;
  end;

  TIdSipTestSessionListener = class(TIdSipMockListener,
                                    IIdSipSessionListener)
  private
    fAnswerParam:              TIdSipResponse;
    fEndedSession:             Boolean;
    fErrorCodeParam:           Cardinal;
    fEstablishedSession:       Boolean;
    fMimeType:                 String;
    fModifiedSession:          Boolean;
    fModifySession:            Boolean;
    fNewSession:               Boolean;
    fProgressedSession:        Boolean;
    fProgressParam:            TIdSipResponse;
    fRedirect:                 Boolean;
    fRemoteSessionDescription: String;
    fSessionParam:             TIdSipSession;
  public
    constructor Create; override;

    procedure OnRedirect(Action: TIdSipAction;
                         Redirect: TIdSipResponse);
    procedure OnEndedSession(Session: TIdSipSession;
                             ErrorCode: Cardinal); virtual;
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

    property AnswerParam:              TIdSipResponse      read fAnswerParam;
    property EndedSession:             Boolean             read fEndedSession;
    property ErrorCodeParam:           Cardinal            read fErrorCodeParam;
    property EstablishedSession:       Boolean             read fEstablishedSession;
    property MimeType:                 String              read fMimeType;
    property ModifiedSession:          Boolean             read fModifiedSession;
    property ModifySession:            Boolean             read fModifySession;
    property NewSession:               Boolean             read fNewSession;
    property ProgressParam:            TIdSipResponse      read fProgressParam;
    property ProgressedSession:        Boolean             read fProgressedSession;
    property Redirect:                 Boolean             read fRedirect;
    property RemoteSessionDescription: String              read fRemoteSessionDescription;
    property SessionParam:             TIdSipSession       read fSessionParam;
  end;

  TIdSipTestSessionListenerEndedCounter = class(TIdSipTestSessionListener)
  private
    fEndedNotificationCount: Integer;
  public
    constructor Create; override;

    procedure OnEndedSession(Session: TIdSipSession;
                             ErrorCode: Cardinal); override;

    property EndedNotificationCount: Integer read fEndedNotificationCount;
  end;

  TIdSipTestSubscribeListener = class(TIdSipMockListener,
                                      IIdSipSubscribeListener)
  private
    fFailed:              Boolean;
    fSubscribeAgentParam: TIdSipOutboundSubscribe;
    fSucceeded:           Boolean;

    procedure OnFailure(SubscribeAgent: TIdSipOutboundSubscribe;
                        Response: TIdSipResponse);
    procedure OnSuccess(SubscribeAgent: TIdSipOutboundSubscribe;
                        Response: TIdSipResponse);
  public
    constructor Create; override;

    property Failed:              Boolean                 read fFailed;
    property SubscribeAgentParam: TIdSipOutboundSubscribe read fSubscribeAgentParam;
    property Succeeded:           Boolean                 read fSucceeded;
  end;

  TIdSipTestSubscriptionListener = class(TIdSipMockListener,
                                         IIdSipSubscriptionListener)
  private
    fEstablishedSubscription: Boolean;
    fExpiredSubscription:     Boolean;
    fNotify:                  Boolean;
    fNotifyParam:             TIdSipRequest;
    fSubscriptionParam:       TIdSipOutboundSubscription;

    procedure OnEstablishedSubscription(Subscription: TIdSipOutboundSubscription;
                                        Response: TIdSipResponse);
    procedure OnExpiredSubscription(Subscription: TIdSipOutboundSubscription;
                                    Notify: TIdSipRequest);
    procedure OnNotify(Subscription: TIdSipOutboundSubscription;
                       Notify: TIdSipRequest);
  public
    constructor Create; override;

    property EstablishedSubscription: Boolean                    read fEstablishedSubscription;
    property ExpiredSubscription:     Boolean                    read fExpiredSubscription;
    property Notify:                  Boolean                    read fNotify;
    property NotifyParam:             TIdSipRequest              read fNotifyParam;
    property SubscriptionParam:       TIdSipOutboundSubscription read fSubscriptionParam;
  end;

  TIdSipTestTransactionListener = class(TIdSipMockListener,
                                        IIdSipTransactionListener)
  private
    fFailed:                  Boolean;
    fReasonParam:             String;
    fReceivedRequest:         Boolean;
    fReceivedResponse:        Boolean;
    fReceiverParam:           TIdSipTransport;
    fRequestParam:            TIdSipRequest;
    fResponseParam:           TIdSipResponse;
    fTerminated:              Boolean;
    fTransactionParam:        TIdSipTransaction;

    procedure OnFail(Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport);
    procedure OnTerminated(Transaction: TIdSipTransaction);
  public
    constructor Create; override;

    property Failed:                  Boolean           read fFailed;
    property ReasonParam:             String            read fReasonParam;
    property ReceivedRequest:         Boolean           read fReceivedRequest;
    property ReceivedResponse:        Boolean           read fReceivedResponse;
    property ReceiverParam:           TIdSipTransport   read fReceiverParam;
    property RequestParam:            TIdSipRequest     read fRequestParam;
    property ResponseParam:           TIdSipResponse    read fResponseParam;
    property Terminated:              Boolean           read fTerminated;
    property TransactionParam:        TIdSipTransaction read fTransactionParam;
  end;


  TIdSipTestTransportListener = class(TIdSipMockListener,
                                      IIdSipTransportListener)
  private
    fException:        Boolean;
    fExceptionParam:   Exception;
    fMsgParam:         String;
    fReasonParam:      String;
    fReceivedRequest:  Boolean;
    fReceivedResponse: Boolean;
    fReceiverParam:    TIdSipTransport;
    fRejectedMessage:  Boolean;
    fRequestParam:     TIdSipRequest;
    fResponseParam:    TIdSipResponse;

    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
  public
    constructor Create; override;

    property Exception:        Boolean         read fException;
    property ExceptionParam:   Exception       read fExceptionParam;
    property MsgParam:         String          read fMsgParam;
    property ReasonParam:      String          read fReasonParam;
    property ReceivedRequest:  Boolean         read fReceivedRequest;
    property ReceivedResponse: Boolean         read fReceivedResponse;
    property ReceiverParam:    TIdSipTransport read fReceiverParam;
    property RejectedMessage:  Boolean         read fRejectedMessage;
    property RequestParam:     TIdSipRequest   read fRequestParam;
    property ResponseParam:    TIdSipResponse  read fResponseParam;

  end;

  TIdSipTestTransportSendingListener = class(TIdSipMockListener,
                                             IIdSipTransportSendingListener)
  private
    fRequestParam:  TIdSipRequest;
    fResponseParam: TIdSipResponse;
    fSenderParam:   TIdSipTransport;
    fSentRequest:   Boolean;
    fSentResponse:  Boolean;

    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport);
  public
    constructor Create; override;

    property RequestParam:  TIdSipRequest   read fRequestParam;
    property ResponseParam: TIdSipResponse  read fResponseParam;
    property SenderParam:   TIdSipTransport read fSenderParam;
    property SentRequest:   Boolean         read fSentRequest;
    property SentResponse:  Boolean         read fSentResponse;

  end;

  TIdSipTestTransactionDispatcherListener = class(TIdSipMockListener,
                                                  IIdSipTransactionDispatcherListener)
  private
    fAuthenticationChallenge:   Boolean;
    fChallengeParam:            TIdSipResponse;
    fChallengeResponseBranch:   String;
    fDispatcherParam:           TIdSipTransactionDispatcher;
    fReceivedRequest:           Boolean;
    fReceivedResponse:          Boolean;
    fReceivedUnhandledRequest:  Boolean;
    fReceivedUnhandledResponse: Boolean;
    fReceiverParam:             TIdSipTransport;
    fRequestParam:              TIdSipRequest;
    fResponseParam:             TIdSipResponse;
    fTryAgain:                  Boolean;

    procedure OnAuthenticationChallenge(Dispatcher: TIdSipTransactionDispatcher;
                                        Challenge: TIdSipResponse;
                                        ChallengeResponse: TIdSipRequest;
                                        var TryAgain: Boolean);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport);
  public
    constructor Create; override;

    property AuthenticationChallenge:   Boolean                     read fAuthenticationChallenge;
    property ChallengeParam:            TIdSipResponse              read fChallengeParam;
    property ChallengeResponseBranch:   String                      read fChallengeResponseBranch write fChallengeResponseBranch;
    property DispatcherParam:           TIdSipTransactionDispatcher read fDispatcherParam;
    property ReceivedRequest:           Boolean                     read fReceivedRequest;
    property ReceivedResponse:          Boolean                     read fReceivedResponse;
    property ReceivedUnhandledRequest:  Boolean                     read fReceivedUnhandledRequest;
    property ReceivedUnhandledResponse: Boolean                     read fReceivedUnhandledResponse;
    property ReceiverParam:             TIdSipTransport             read fReceiverParam;
    property RequestParam:              TIdSipRequest               read fRequestParam;
    property ResponseParam:             TIdSipResponse              read fResponseParam;
    property TryAgain:                  Boolean                     read fTryAgain write fTryAgain;
  end;

  TIdSipTestUserAgentListener = class(TIdSipMockListener,
                                      IIdSipUserAgentListener)
  private
    fAuthenticationChallenge: Boolean;
    fDroppedUnmatchedMessage: Boolean;
    fInboundCall:             Boolean;
    fPassword:                String;
    fReceiverParam:           TIdSipTransport;
    fResponseParam:           TIdSipResponse;
    fMessageParam:            TIdSipMessage;
    fSessionParam:            TIdSipInboundSession;
    fSubscriptionParam:       TIdSipInboundSubscription;
    fSubscriptionRequest:     Boolean;
    fTryAgain:                Boolean;
    fUserAgentParam:          TIdSipAbstractUserAgent;
    fUsername:                String;

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
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipInboundSubscription);
  public
    constructor Create; override;

    property AuthenticationChallenge: Boolean                   read fAuthenticationChallenge;
    property DroppedUnmatchedMessage: Boolean                   read fDroppedUnmatchedMessage;
    property InboundCall:             Boolean                   read fInboundCall;
    property Password:                String                    read fPassword write fPassword;
    property ReceiverParam:           TIdSipTransport           read fReceiverParam;
    property ResponseParam:           TIdSipResponse            read fResponseParam;
    property MessageParam:            TIdSipMessage             read fMessageParam;
    property SessionParam:            TIdSipInboundSession      read fSessionParam;
    property SubscriptionParam:       TIdSipInboundSubscription read fSubscriptionParam;
    property SubscriptionRequest:     Boolean                   read fSubscriptionRequest;
    property TryAgain:                Boolean                   read fTryAgain write fTryAgain;
    property UserAgentParam:          TIdSipAbstractUserAgent   read fUserAgentParam;
    property Username:                String                    read fUsername write fUsername;
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
    constructor Create;

    procedure Execute(Action: TIdSipAction); override;

    property Executed: Boolean read fExecuted;
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
  IdSipConsts, IdSipMockTransport;

//******************************************************************************
//* TIdSipTestResources                                                        *
//******************************************************************************
//* TIdSipTestResources Public methods *****************************************

class function TIdSipTestResources.CreateBasicRequest: TIdSipRequest;
begin
  Result := Self.CreateCommonRequest;
  Result.RequestUri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Result.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
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
    Result := TIdSipResponse.InResponseTo(Request, SIPBusyHere);
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
  inherited SetUp;

  TIdSipTransportRegistry.RegisterTransport(SctpTransport, TIdSipMockSctpTransport);
  TIdSipTransportRegistry.RegisterTransport(TcpTransport,  TIdSipMockTcpTransport);
  TIdSipTransportRegistry.RegisterTransport(TlsTransport,  TIdSipMockTlsTransport);
  TIdSipTransportRegistry.RegisterTransport(UdpTransport,  TIdSipMockUdpTransport);
end;

procedure TTestCaseSip.TearDown;
begin
  TIdSipTransportRegistry.UnregisterTransport(UdpTransport);
  TIdSipTransportRegistry.UnregisterTransport(TlsTransport);
  TIdSipTransportRegistry.UnregisterTransport(TcpTransport);
  TIdSipTransportRegistry.UnregisterTransport(SctpTransport);

  inherited TearDown;
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

  Self.Core := TIdSipUserAgent.Create;
  Self.Core.Dispatcher := Self.Dispatcher;
  Self.Core.Locator    := Self.Locator;

  Self.Core.Contact.Value := 'sip:case@localhost';
  Self.Core.From.Value    := 'sip:case@localhost';

  Self.Invite := TIdSipTestResources.CreateBasicRequest;
  Self.RemoveBody(Self.Invite);

  Self.DebugTimer := TIdDebugTimerQueue.Create(false);
  Self.DebugTimer.FireImmediateEvents := true;
  Self.Core.Timer := DebugTimer;

  // Make sure we have a sane DNS setup so that actions don't terminate
  // themselves after they try find locations to which to send their messages.
  Self.Locator.AddA(Self.Destination.Address.Host, '127.0.0.1');
  Self.Locator.AddA(Self.Core.From.Address.Host,   '127.0.0.1');
  Self.Locator.AddA('localhost',                   '127.0.0.1');
  Self.Locator.AddA(Self.Invite.LastHop.SentBy,    '127.0.0.1');
end;

procedure TTestCaseTU.TearDown;
begin
  Self.DebugTimer.Terminate;

  Self.Invite.Free;
  Self.Core.Free;
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

function TTestCaseTU.CreateRemoteOk(Request: TIdSipRequest): TIdSipResponse;
begin
  // This message appears to originate from the network. Invite originates from
  // us so presumably has no To tag. Having come from the network, the response
  // WILL have a To tag.
  Result := Self.Core.CreateResponse(Request, SIPOK);
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

procedure TTestCaseTU.ReceiveOk(Invite: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  // This message appears to originate from the network. Invite originates from
  // us so presumably has no To tag. Having come from the network, the response
  // WILL have a To tag.
  Response := Self.CreateRemoteOk(Invite);
  try
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
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Request,
                                       StatusCode);
  try
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
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

procedure TTestCaseTU.ReceiveSubscribe(const EventPackage: String);
var
  Sub: TIdSipRequest;
begin
  Sub := Self.Core.CreateSubscribe(Self.Destination, EventPackage);
  try
    Self.ReceiveRequest(Sub);
  finally
    Sub.Free;
  end;
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
  Auth:      TIdSipAuthenticateHeader;
  Challenge: TIdSipResponse;
begin
  Challenge := TIdSipResponse.InResponseTo(Self.LastSentRequest,
                                           SIPUnauthorized);
  try
    Auth := Challenge.AddHeader(AuthHeaderName) as TIdSipAuthenticateHeader;
    Auth.AuthorizationScheme := DigestAuthorizationScheme;
    Auth.Realm               := 'SFTF';
    Auth.Nonce               := '5369704365727434313433';
    Auth.Qop                 := Qop;

    Challenge.AddHeader(AuthenticationInfoHeader);

    Self.ReceiveResponse(Challenge);
  finally
    Challenge.Free;
  end;
end;

function TTestCaseTU.SecondLastSentRequest: TIdSipRequest;
begin
  Result := Self.Dispatcher.Transport.SecondLastRequest;
end;

function TTestCaseTU.SecondLastSentResponse: TIdSipResponse;
begin
  Result := Self.Dispatcher.Transport.SecondLastResponse;
end;

function TTestCaseTU.SentAckCount: Cardinal;
begin
  Result := Self.Dispatcher.Transport.ACKCount;
end;

function TTestCaseTU.SentRequestCount: Cardinal;
begin
  Result := Self.Dispatcher.Transport.SentRequestCount;
end;

function TTestCaseTU.SentResponseCount: Cardinal;
begin
  Result := Self.Dispatcher.Transport.SentResponseCount;
end;

function TTestCaseTU.SentRequestAt(Index: Integer): TIdSipRequest;
begin
  Result := Self.Dispatcher.Transport.RequestAt(Index);
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
//* TIdSipExceptionRaisingHeader                                               *
//******************************************************************************
//* TIdSipExceptionRaisingHeader Protected methods *****************************

procedure TIdSipExceptionRaisingHeader.Parse(const Value: String);
begin
  raise EBadHeader.Create(Self.ClassName + '.Parse');
end;

//******************************************************************************
//* TIdSipMockListener                                                         *
//******************************************************************************
//* TIdSipMockListener Public methods ******************************************

constructor TIdSipMockListener.Create;
begin
  inherited Create;

  Self.FailWith                  := nil;
  Self.fAuthenticationChallenged := false;
  Self.fNetworkFailed            := false;
  Self.fReasonParam              := '';
end;

//* TIdSipMockListener Private methods *****************************************

procedure TIdSipMockListener.OnAuthenticationChallenge(Action: TIdSipAction;
                                                       Response: TIdSipResponse);
begin
  Self.fActionParam              := Action;
  Self.fAuthenticationChallenged := true;
  Self.fResponseParam            := Response;
end;

procedure TIdSipMockListener.OnNetworkFailure(Action: TIdSipAction;
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
                                           Binding: TIdConnection);
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
                                                       const Reason: String);
begin
  Self.fMalformedMessage      := true;
  Self.fMalformedMessageParam := Msg;
  Self.fReasonParam           := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnMalformedMessage');
end;

procedure TIdSipTestMessageListener.OnReceiveRequest(Request: TIdSipRequest;
                                                     ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.fReceivedRequest   := true;
  Self.fRequestParam      := Request;
  Self.fReceivedFromParam := ReceivedFrom;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveRequest');
end;

procedure TIdSipTestMessageListener.OnReceiveResponse(Response: TIdSipResponse;
                                                      ReceivedFrom: TIdSipConnectionBindings);
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
                                                    Ack: TIdSipRequest);
begin
  Self.fAckParam         := Ack;
  Self.fInviteAgentParam := InviteAgent;
  Self.fSucceeded        := true;
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
  Self.fFailure           := false;
  Self.fSuccess           := false;
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

procedure TIdSipTestInviteListener.OnFailure(InviteAgent: TIdSipOutboundInvite;
                                             Response: TIdSipResponse;
                                             const Reason: String);
begin
  Self.fFailure          := true;
  Self.fInviteAgentParam := InviteAgent;
  Self.fResponseParam    := Response;
  Self.fReasonParam      := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFailure');
end;

procedure TIdSipTestInviteListener.OnRedirect(InviteAgent: TIdSipOutboundInvite;
                                              Redirect: TIdSipResponse);
begin
  Self.fInviteAgentParam := InviteAgent;
  Self.fRedirect         := true;
  Self.fResponseParam    := Redirect;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnRedirect');
end;

procedure TIdSipTestInviteListener.OnSuccess(InviteAgent: TIdSipOutboundInvite;
                                             Response: TIdSipResponse);
begin
  Self.fInviteAgentParam := InviteAgent;
  Self.fResponseParam     := Response;
  Self.fSuccess           := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnSuccess');
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

procedure TIdSipTestRegistrationListener.OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                                                   CurrentBindings: TIdSipContacts;
                                                   const Reason: String);
begin
  Self.fCurrentBindingsParam := CurrentBindings;
  Self.fFailure              := true;
  Self.fRegisterAgentParam   := RegisterAgent;
  Self.fReasonParam          := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFailure');
end;

procedure TIdSipTestRegistrationListener.OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                                                   CurrentBindings: TIdSipContacts);
begin
  Self.fCurrentBindingsParam := CurrentBindings;
  Self.fRegisterAgentParam   := RegisterAgent;
  Self.fSuccess              := true;

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
                                                   ErrorCode: Cardinal);
begin
  Self.fEndedSession   := true;
  Self.fErrorCodeParam := ErrorCode;
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
                                                               ErrorCode: Cardinal);
begin
  inherited OnEndedSession(Session, ErrorCode);

  Inc(Self.fEndedNotificationCount);
end;

//******************************************************************************
//* TIdSipTestSubscribeListener                                                *
//******************************************************************************
//* TIdSipTestSubscribeListener Public methods *********************************

constructor TIdSipTestSubscribeListener.Create;
begin
  inherited Create;

  Self.fFailed    := false;
  Self.fSucceeded := false;
end;

//* TIdSipTestSubscribeListener Private methods ********************************

procedure TIdSipTestSubscribeListener.OnFailure(SubscribeAgent: TIdSipOutboundSubscribe;
                                                Response: TIdSipResponse);
begin
  Self.fFailed              := true;
  Self.fSubscribeAgentParam := SubscribeAgent;
  Self.fResponseParam       := Response;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFailure');
end;

procedure TIdSipTestSubscribeListener.OnSuccess(SubscribeAgent: TIdSipOutboundSubscribe;
                                                Response: TIdSipResponse);
begin
  Self.fResponseParam       := Response;
  Self.fSubscribeAgentParam := SubscribeAgent;
  Self.fSucceeded           := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnSuccess');
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
  Self.fNotify                  := false;
end;

//* TIdSipTestSubscriptionListener Private methods ******************************

procedure TIdSipTestSubscriptionListener.OnEstablishedSubscription(Subscription: TIdSipOutboundSubscription;
                                                                   Response: TIdSipResponse);
begin
  Self.fEstablishedSubscription := true;
  Self.fResponseParam           := Response;
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
                                               const Reason: String);
begin
  Self.fFailed           := true;
  Self.fReasonParam      := Reason;
  Self.fTransactionParam := Transaction;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFail');
end;

procedure TIdSipTestTransactionListener.OnReceiveRequest(Request: TIdSipRequest;
                                                         Transaction: TIdSipTransaction;
                                                         Receiver: TIdSipTransport);
begin
  Self.fReceivedRequest  := true;
  Self.fReceiverParam    := Receiver;
  Self.fRequestParam     := Request;
  Self.fTransactionParam := Transaction;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveRequest');
end;

procedure TIdSipTestTransactionListener.OnReceiveResponse(Response: TIdSipResponse;
                                                          Transaction: TIdSipTransaction;
                                                          Receiver: TIdSipTransport);
begin
  Self.fReceivedResponse := true;
  Self.fReceiverParam    := Receiver;
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
//* TIdSipTestTransportListener                                                *
//******************************************************************************
//* TIdSipTestTransportListener Public methods *********************************

constructor TIdSipTestTransportListener.Create;
begin
  inherited Create;

  Self.fException        := false;
  Self.fReceivedRequest  := false;
  Self.fReceivedResponse := false;
  Self.fRejectedMessage  := false;
end;

//* TIdSipTestTransportListener Private methods ********************************

procedure TIdSipTestTransportListener.OnException(E: Exception;
                                                  const Reason: String);
begin
  Self.fException      := true;
  Self.fExceptionParam := E;
  Self.fReasonParam    := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnException');
end;

procedure TIdSipTestTransportListener.OnReceiveRequest(Request: TIdSipRequest;
                                                       Receiver: TIdSipTransport);
begin
  Self.fReceiverParam   := Receiver;
  Self.fRequestParam    := Request;
  Self.fReceivedRequest := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveRequest');
end;

procedure TIdSipTestTransportListener.OnReceiveResponse(Response: TIdSipResponse;
                                                        Receiver: TIdSipTransport);
begin
  Self.fReceiverParam    := Receiver;
  Self.fResponseParam    := Response;
  Self.fReceivedResponse := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveResponse');
end;

procedure TIdSipTestTransportListener.OnRejectedMessage(const Msg: String;
                                                        const Reason: String);
begin
  Self.fMsgParam        := Msg;
  Self.fReasonParam     := Reason;
  Self.fRejectedMessage := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnRejectedMessage');

end;

//******************************************************************************
//* TIdSipTestTransportSendingListener                                         *
//******************************************************************************
//* TIdSipTestTransportSendingListener Public methods **************************

constructor TIdSipTestTransportSendingListener.Create;
begin
  inherited Create;

  Self.fSentRequest      := false;
  Self.fSentResponse     := false;
end;

//* TIdSipTestTransportSendingListener Private methods *************************


procedure TIdSipTestTransportSendingListener.OnSendRequest(Request: TIdSipRequest;
                                                           Sender: TIdSipTransport);
begin
  Self.fRequestParam := Request;
  Self.fSenderParam  := Sender;
  Self.fSentRequest  := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnSendRequest');
end;

procedure TIdSipTestTransportSendingListener.OnSendResponse(Response: TIdSipResponse;
                                                            Sender: TIdSipTransport);
begin
  Self.fResponseParam := Response;
  Self.fSenderParam   := Sender;
  Self.fSentResponse  := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnSendResponse');
end;

//******************************************************************************
//* TIdSipTestTransactionDispatcherListener                                    *
//******************************************************************************
//* TIdSipTestTransactionDispatcherListener Public methods *********************

constructor TIdSipTestTransactionDispatcherListener.Create;
begin
  inherited Create;

  Self.fAuthenticationChallenge   := false;
  Self.fReceivedRequest           := false;
  Self.fReceivedResponse          := false;
  Self.fReceivedUnhandledRequest  := false;
  Self.fReceivedUnhandledResponse := false;

  // Usually you'd want to re-issue a request that the UAS challenged.
  Self.TryAgain := true;
end;

//* TIdSipTestTransactionDispatcherListener Private methods ********************

procedure TIdSipTestTransactionDispatcherListener.OnAuthenticationChallenge(Dispatcher: TIdSipTransactionDispatcher;
                                                                            Challenge: TIdSipResponse;
                                                                            ChallengeResponse: TIdSipRequest;
                                                                            var TryAgain: Boolean);
begin
  Self.fAuthenticationChallenge := true;
  Self.fDispatcherParam         := Dispatcher;
  Self.fChallengeParam          := Challenge;
  Self.fChallengeResponseBranch := ChallengeResponse.LastHop.Branch;

  // We set the var parameter, not our instance variable!
  TryAgain := Self.TryAgain;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnAuthenticationChallenge');
end;

procedure TIdSipTestTransactionDispatcherListener.OnReceiveRequest(Request: TIdSipRequest;
                                                              Receiver: TIdSipTransport);
begin
  Self.fReceivedRequest := true;
  Self.fReceiverParam   := Receiver;
  Self.fRequestParam    := Request;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveRequest');
end;

procedure TIdSipTestTransactionDispatcherListener.OnReceiveResponse(Response: TIdSipResponse;
                                                               Receiver: TIdSipTransport);
begin
  Self.fReceivedResponse := true;
  Self.fReceiverParam    := Receiver;
  Self.fResponseParam    := Response;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveResponse');
end;

//******************************************************************************
//* TIdSipTestUserAgentListener                                                *
//******************************************************************************
//* TIdSipTestUserAgentListener Public methods *********************************

constructor TIdSipTestUserAgentListener.Create;
begin
  inherited Create;

  Self.fAuthenticationChallenge := false;
  Self.fDroppedUnmatchedMessage := false;
  Self.fInboundCall             := false;
  Self.fSubscriptionRequest     := false;
end;

//* TIdSipTestUserAgentListener Private methods ********************************

procedure TIdSipTestUserAgentListener.OnAuthenticationChallenge(UserAgent: TIdSipAbstractUserAgent;
                                                                Challenge: TIdSipResponse;
                                                                var Username: String;
                                                                var Password: String;
                                                                var TryAgain: Boolean);
begin
  Self.fUserAgentParam          := UserAgent;
  Self.fAuthenticationChallenge := true;
  Self.fResponseParam           := Challenge;

  // We set the var parameter, not our instance variable!
  Password := Self.Password;
  TryAgain := Self.TryAgain;
  Username := Self.Username;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnAuthenticationChallenge');
end;

procedure TIdSipTestUserAgentListener.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractUserAgent;
                                                                Message: TIdSipMessage;
                                                                Receiver: TIdSipTransport);
begin
  Self.fDroppedUnmatchedMessage := true;
  Self.fReceiverParam           := Receiver;
  Self.fMessageParam            := Message;
  Self.fUserAgentParam          := UserAgent;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnDroppedUnmatchedMessage');
end;

procedure TIdSipTestUserAgentListener.OnInboundCall(UserAgent: TIdSipAbstractUserAgent;
                                                    Session: TIdSipInboundSession);
begin
  Self.fInboundCall    := true;
  Self.fSessionParam   := Session;
  Self.fUserAgentParam := UserAgent;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnInboundCall');
end;

procedure TIdSipTestUserAgentListener.OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                                            Subscription: TIdSipInboundSubscription);
begin
  Self.fSubscriptionRequest := true;
  Self.fSubscriptionParam   := Subscription;
  Self.fUserAgentParam      := UserAgent;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnInboundCall');
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

end.
