unit IdSipCore;

// Some overarching principles followed in this implementation of a SIP/2.0
// (RFC 3261) stack:
// * We rely on short-circuited evaluation of Boolean expressions.
// * We manually manage the lifetime of all objects. We do NOT use reference
//   counting for objects that implement interfaces.
// * We use Value Objects when possible.
// * If an object A receives some object B that it expects to store as data
//   then A must store a COPY of B. Typical objects are: TIdSipURI,
//   TIdSipDialogID, TIdSipMessage.
// * Each layer has references to the layers beneath it. We try to make each layer
//   aware of ONLY the layer immediately below it, but that's not always
//   possible. We NEVER let a lower layer know about layers above it. Thus, the
//   transport layer DOES NOT know about transactions, etc.
// * We propogate messages up the stack using Events or Listeners, and method
//   calls to propogate messages down the stack. We give preference to the more
//   flexible Listeners.
// * We avoid typecasting as much as possible by using polymorphism and, in
//   certain situations where polymorphism can't cut it, the Visitor pattern.
// * TObjectLists always manage the lifetime of the objects they contain. Except
//   in the case of Transports in the Dispatcher.

interface

uses
  Classes, Contnrs, IdSdp, IdSipDialog, IdException, IdInterfacedObject,
  IdSipMessage, IdSipTimer, IdSipTransaction, IdSipTransport, SyncObjs;

type
  TIdSipAction = class;
  TIdSipRegistration = class;
  TIdSipSession = class;
  TIdSipSessionEvent = procedure(Session: TIdSipSession) of object;

  // I watch other objects for changes. When they change (in their arbitrary
  // fashion) they tell me, and I update my own state accordingly.
  // Unfortunately I never know the type of Observed and have to typecast
  // the Observed, but c'est la vie.
  IIdSipObserver = interface
    ['{665CFE94-8EFD-4710-A5CC-ED01BCF7961E}']
    procedure OnChanged(Observed: TObject);
  end;

  // I provide a protocol for using a registrar. You send a REGISTER, and
  // listen for the events below.
  //
  // You can use OnAuthenticationChallenge to authenticate to a proxy (or
  // registrar). Note that we cannot distinguish between (1) you contacting
  // the proxy/registrar for the first time and it asking for credentials,
  // and (2) you offering invalid credentials.
  //
  // OnFailure and OnSuccess, apart from the obvious, tell you that the
  // registration agent has terminated, and that you should remove all
  // of your references to it.
  IIdSipRegistrationListener = interface
    ['{D3FA9A3D-ED8A-48D3-8068-38E8F9EE2140}']
    procedure OnAuthenticationChallenge(RegisterAgent: TIdSipRegistration;
                                        Response: TIdSipResponse);
    procedure OnFailure(RegisterAgent: TIdSipRegistration;
                        CurrentBindings: TIdSipContacts;
                        const Reason: String);
    procedure OnSuccess(RegisterAgent: TIdSipRegistration;
                        CurrentBindings: TIdSipContacts);
  end;

  // I am the protocol of things that listen for Sessions:
  // * OnNewSession tells us that someone wants to talk to us - we may refuse or
  //   allow the session.
  // * OnSessionEstablished tells us when a session has been fully established.
  // * OnSessionEnded tells us when the session's finished. This could be
  //   because someone hung up, or because the outbound call failed (the request
  //   timed out, a transport error occurec, etc). OnSessionEnded lets us clean
  //   up. The Session referenced becomes invalid after this point. In other
  //   words, you'd better say goodbye to the Session in your implementation of
  //   this method. Accessing your reference to the Session will probably fail
  //   with an access violation.
  IIdSipSessionListener = interface
    ['{59B3C476-D3CA-4C5E-AA2B-2BB587A5A716}']
    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String);
    procedure OnEstablishedSession(Session: TIdSipSession);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Invite: TIdSipRequest);
  end;

  TIdSipInboundSession = class;

  IIdSipUserAgentListener = interface
    ['{E365D17F-054B-41AB-BB18-0C339715BFA3}']
    procedure OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                         Receiver: TIdSipTransport);
    procedure OnInboundCall(Session: TIdSipInboundSession);
  end;

  TIdSipUserAgentReaction =
    (uarAccept,
     uarBadRequest,
     uarExpireTooBrief,
     uarForbidden,
     uarLoopDetected,
     uarMissingContact,
     uarNotFound,
     uarUnsupportedExtension,
     uarTooManyVias,
     uarUnauthorized,
     uarUnsupportedContentEncoding,
     uarUnsupportedContentLanguage,
     uarUnsupportedContentType,
     uarUnsupportedMethod,
     uarUnsupportedScheme,
     uarUnsupportedSipVersion);

  // TODO: there's redundance with this Hostname, and the Hostnames of the
  // transports attached to this core. It's not clear how to set up the
  // hostnames and bindings of the stack.
  TIdSipAbstractCore = class(TIdInterfacedObject,
                             IIdSipUnhandledMessageListener)
  private
    fDispatcher: TIdSipTransactionDispatcher;
    fHostName:   String;

    function  DefaultHostName: String;
    procedure OnReceiveUnhandledRequest(Request: TIdSipRequest;
                                        Receiver: TIdSipTransport);
    procedure OnReceiveUnhandledResponse(Response: TIdSipResponse;
                                         Receiver: TIdSipTransport);
    procedure SetDispatcher(Value: TIdSipTransactionDispatcher);
  protected
    procedure ActOnRequest(Request: TIdSipRequest;
                           Receiver: TIdSipTransport); virtual;
    procedure ActOnResponse(Response: TIdSipResponse;
                           Receiver: TIdSipTransport); virtual;
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport); virtual;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport); virtual;
    procedure RejectRequest(Reaction: TIdSipUserAgentReaction;
                            Request: TIdSipRequest); virtual;
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; virtual;
    function  WillAcceptResponse(Response: TIdSipResponse): TIdSipUserAgentReaction; virtual;
  public
    constructor Create; virtual;

    function  CreateRequest(Dest: TIdSipAddressHeader): TIdSipRequest; overload; virtual; abstract;
    function  CreateRequest(Dialog: TIdSipDialog): TIdSipRequest; overload; virtual; abstract;
    function  CreateResponse(Request: TIdSipRequest;
                             ResponseCode: Cardinal): TIdSipResponse; virtual; abstract;
    function  NextCallID: String;

    property Dispatcher: TIdSipTransactionDispatcher read fDispatcher write SetDispatcher;
    property HostName:   String                      read fHostName write fHostName;
  end;

  // I represent the heart of a User Agent. We split User Agent functionality
  // into multiple levels because some SIP entities (like registrars) behave
  // very similarly to "normal" User Agents. In fact, we can regard a registrar
  // as simply a User Agent that responds to REGISTER methods.

  // I provide the canonical place to reject messages that have correct syntax
  // but that we don't or can't accept. This includes unsupported SIP versions,
  // unrecognised methods, etc.
  TIdSipAbstractUserAgent = class(TIdSipAbstractCore)
  private
    fAllowedContentTypeList: TStrings;
    fAllowedLanguageList:    TStrings;
    fAllowedMethodList:      TStrings;
    fAllowedSchemeList:      TStrings;
    fFrom:                   TIdSipFromHeader;
    fUserAgentName:          String;

    function  GetFrom: TIdSipFromHeader;
    procedure RejectRequestMethodNotAllowed(Request: TIdSipRequest);
    procedure RejectRequestUnknownContentEncoding(Request: TIdSipRequest);
    procedure RejectRequestUnknownContentLanguage(Request: TIdSipRequest);
    procedure RejectRequestUnknownContentType(Request: TIdSipRequest);
    procedure RejectUnsupportedSipVersion(Request: TIdSipRequest);
    procedure SetFrom(Value: TIdSipFromHeader);
  protected
    procedure AddLocalHeaders(OutboundRequest: TIdSipRequest); virtual;
    procedure PrepareResponse(Response: TIdSipResponse;
                              Request: TIdSipRequest);
    procedure RejectRequest(Reaction: TIdSipUserAgentReaction;
                            Request: TIdSipRequest); override;
    procedure RejectRequestBadExtension(Request: TIdSipRequest);
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; override;

    property AllowedContentTypeList: TStrings read fAllowedContentTypeList;
    property AllowedLanguageList:    TStrings read fAllowedLanguageList;
    property AllowedMethodList:      TStrings read fAllowedMethodList;
    property AllowedSchemeList:      TStrings read fAllowedSchemeList;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddAllowedContentType(const MimeType: String);
    procedure AddAllowedLanguage(const LanguageID: String);
    procedure AddAllowedMethod(const Method: String);
    procedure AddAllowedScheme(const Scheme: String);
    function  AllowedContentTypes: String;
    function  AllowedExtensions: String;
    function  AllowedLanguages: String;
    function  AllowedMethods: String;
    function  AllowedSchemes: String;
    function  CreateRequest(Dest: TIdSipAddressHeader): TIdSipRequest; overload; override;
    function  CreateResponse(Request: TIdSipRequest;
                             ResponseCode: Cardinal): TIdSipResponse; overload; override;
    function  HasUnknownContentEncoding(Request: TIdSipRequest): Boolean;
    function  HasUnknownContentLanguage(Request: TIdSipRequest): Boolean;
    function  HasUnknownContentType(Request: TIdSipRequest): Boolean;
    function  IsExtensionAllowed(const Extension: String): Boolean;
    function  IsMethodAllowed(const Method: String): Boolean;
    function  IsSchemeAllowed(const Scheme: String): Boolean;
    function  NextBranch: String;
    function  NextInitialSequenceNo: Cardinal;
    function  NextTag: String;
    procedure ReturnResponse(Request: TIdSipRequest;
                             Reason: Cardinal);

    property From:          TIdSipFromHeader read GetFrom write SetFrom;
    property UserAgentName: String           read fUserAgentName write fUserAgentName;
  end;

  // I keep track of information a User Agent needs when making a REGISTER to
  // a particular registrar.
  TIdSipRegistrationInfo = class(TObject)
  private
    fCallID:     String;
    fRegistrar:  TIdSipUri;
    fSequenceNo: Cardinal;
  public
    constructor Create;
    destructor  Destroy; override;

    property CallID:     String    read fCallID write fCallID;
    property Registrar:  TIdSipUri read fRegistrar;
    property SequenceNo: Cardinal  read fSequenceNo write fSequenceNo;
  end;

  TIdSipOutboundSession = class;

  // I (usually) represent a human being in the SIP network. I:
  // * inform any listeners when new sessions become established, modified or
  //   terminated;
  // * allow my users to make outgoing "calls";
  // * clean up established Sessions
  TIdSipUserAgentCore = class(TIdSipAbstractUserAgent)
  private
    ActionLock:               TCriticalSection;
    Actions:                  TObjectList;
    fContact:                 TIdSipContactHeader;
    fDoNotDisturb:            Boolean;
    fDoNotDisturbMessage:     String;
    fHasProxy:                Boolean;
    fProxy:                   TIdSipUri;
    KnownRegistrars:          TObjectList;
    ObserverLock:             TCriticalSection;
    Observers:                TList;
    RegistrationListenerLock: TCriticalSection;
    RegistrationListeners:    TList;
    UserAgentListenerLock:    TCriticalSection;
    UserAgentListeners:       TList;

    function  ActionAt(Index: Integer): TIdSipAction;
    function  AddFork(RootSession: TIdSipOutboundSession;
                      Response: TIdSipResponse): TIdSipOutboundSession;
    function  AddInboundSession(Invite: TIdSipRequest;
                                Receiver: TIdSipTransport): TIdSipInboundSession;
    procedure AddKnownRegistrar(Registrar: TIdSipUri;
                                const CallID: String;
                                SequenceNo: Cardinal);
    function  AddOutboundSession: TIdSipOutboundSession;
    function  AddRegistration: TIdSipRegistration;
    function  CallIDFor(Registrar: TIdSipUri): String;
    function  DefaultFrom: String;
    function  DefaultUserAgent: String;
    function  FindAction(Msg: TIdSipMessage): TIdSipAction;
    function  ForkCall(Response: TIdSipResponse): TIdSipOutboundSession;
    function  GetContact: TIdSipContactHeader;
    function  IndexOfRegistrar(Registrar: TIdSipUri): Integer;
    function  KnowsRegistrar(Registrar: TIdSipUri): Boolean;
    function  NextSequenceNoFor(Registrar: TIdSipUri): Cardinal;
    procedure NotifyOfInboundCall(Session: TIdSipInboundSession);
    procedure NotifyOfChange;
    procedure NotifyOfDroppedResponse(Response: TIdSipResponse;
                                      Receiver: TIdSipTransport);

    procedure OnInboundSessionExpire(Sender: TObject);
    procedure ProcessAck(Ack: TIdSipRequest);
    procedure ProcessInvite(Invite: TIdSipRequest;
                            Receiver: TIdSipTransport);
    function  RegistrarAt(Index: Integer): TIdSipRegistrationInfo;
    procedure RejectBadRequest(Request: TIdSipRequest;
                               const Reason: String);
    procedure RejectDoNotDisturb(Request: TIdSipRequest;
                                 const Reason: String);
    procedure SendByeToAppropriateSession(Bye: TIdSipRequest);
    procedure SetContact(Value: TIdSipContactHeader);
    procedure SetProxy(Value: TIdSipUri);
    procedure TurnIntoInvite(OutboundRequest: TIdSipRequest;
                             const Offer: String;
                             const OfferMimeType: String);
  protected
    procedure ActOnRequest(Request: TIdSipRequest;
                           Receiver: TIdSipTransport); override;
    procedure ActOnResponse(Response: TIdSipResponse;
                           Receiver: TIdSipTransport); override;
    procedure AddLocalHeaders(OutboundRequest: TIdSipRequest); override;
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport); override;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport); override;
    procedure RejectRequest(Reaction: TIdSipUserAgentReaction;
                            Request: TIdSipRequest); override;
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddObserver(const Listener: IIdSipObserver);
    procedure AddUserAgentListener(const Listener: IIdSipUserAgentListener);
    function  Call(Dest: TIdSipAddressHeader;
                   const InitialOffer: String;
                   const MimeType: String): TIdSipOutboundSession;
    function  CreateBye(Dialog: TIdSipDialog): TIdSipRequest;
    function  CreateInvite(Dest: TIdSipAddressHeader;
                           const Body: String;
                           const MimeType: String): TIdSipRequest;
    function  CreateRegister(Registrar: TIdSipToHeader): TIdSipRequest;
    function  CreateReInvite(Dialog: TIdSipDialog;
                             const Body: String;
                             const MimeType: String): TIdSipRequest;
    function  CreateRequest(Dialog: TIdSipDialog): TIdSipRequest; overload; override;
    function  CreateResponse(Request: TIdSipRequest;
                             ResponseCode: Cardinal): TIdSipResponse; override;
    function  CurrentRegistrationWith(Registrar: TIdSipUri): TIdSipRegistration;
    function  RegisterWith(Registrar: TIdSipUri): TIdSipRegistration;
    function  RegistrationCount: Integer;
    procedure RemoveObserver(const Listener: IIdSipObserver);
    procedure RemoveAction(Action: TIdSipAction);
    procedure RemoveUserAgentListener(const Listener: IIdSipUserAgentListener);
    function  SessionCount: Integer;
    procedure HangUpAllCalls;
    function  UnregisterFrom(Registrar: TIdSipUri): TIdSipRegistration;
    function  Username: String;

    property Contact:             TIdSipContactHeader read GetContact write SetContact;
    property DoNotDisturb:        Boolean             read fDoNotDisturb write fDoNotDisturb;
    property DoNotDisturbMessage: String              read fDoNotDisturbMessage write fDoNotDisturbMessage;
    property HasProxy:            Boolean             read fHasProxy write fHasProxy;
    property Proxy:               TIdSipUri           read fProxy write SetProxy;
  end;

  TIdSipProcedure = procedure(ObjectOrIntf: Pointer) of object;

  // I represent an asynchronous message send between SIP entities - INVITEs,
  // REGISTERs and the like - where we care what the remote end answers.
  // With CANCELs and BYEs, for instance, we don't care how the remote end
  // answers.
  TIdSipAction = class(TIdInterfacedObject)
  private
    fInitialRequest: TIdSipRequest;
    fIsTerminated:   Boolean;
    UA:              TIdSipUserAgentCore;

  protected
    procedure ActionSucceeded(Response: TIdSipResponse); virtual;
    procedure ApplyTo(List: TList;
                      Lock: TCriticalSection;
                      Proc: TIdSipProcedure);
    procedure CopyList(Source: TList;
                       Lock: TCriticalSection;
                       Copy: TList);
    procedure MarkAsTerminated; virtual;
    procedure NotifyOfFailure(Response: TIdSipResponse); virtual;
    function  ReceiveFailureResponse(Response: TIdSipResponse): Boolean; virtual;
    function  ReceiveGlobalFailureResponse(Response: TIdSipResponse): Boolean; virtual;
    function  ReceiveOKResponse(Response: TIdSipResponse;
                                UsingSecureTransport: Boolean): Boolean; virtual;
    function  ReceiveProvisionalResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): Boolean; virtual;
    function  ReceiveRedirectionResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): Boolean; virtual;
    function  ReceiveServerFailureResponse(Response: TIdSipResponse): Boolean; virtual;
  public
    constructor Create(UA: TIdSipUserAgentCore); virtual;
    destructor  Destroy; override;

    function  IsRegistration: Boolean; virtual;
    function  IsSession: Boolean; virtual;
    function  Match(Msg: TIdSipMessage): Boolean; virtual;
    procedure ReceiveRequest(Request: TIdSipRequest); virtual;
    procedure ReceiveResponse(Response: TIdSipResponse;
                              UsingSecureTransport: Boolean); virtual;
    procedure Terminate; virtual;

    property InitialRequest: TIdSipRequest read fInitialRequest;
    property IsTerminated:   Boolean       read fIsTerminated;
  end;

  TIdSipActionClass = class of TIdSipAction;

  // As per section 13.3.1.4 of RFC 3261, a Session will resend a 2xx response
  // to an INVITE until it receives an ACK. Thus I provide an exponential
  // back-off timer starting with an interval of T1 milliseconds and capping
  // the interval at T2 milliseconds.
  TIdSipSessionTimer = class(TObject)
  private
    Lock:    TCriticalSection;
    T1:      Cardinal;
    T2:      Cardinal;
    Session: TIdSipInboundSession;
    Timer:   TIdSipTimer;

    procedure OnTimer(Sender: TObject);
  public
    constructor Create(Session: TIdSipInboundSession;
                       T1: Cardinal;
                       T2: Cardinal);
    destructor  Destroy; override;

    procedure Fire;
    procedure Start;
    procedure Stop;

    function Interval: Cardinal;
  end;

  // I am a SIP session. As such, I represent both what my dialog represents
  // (a long-term relationship between two peers in a SIP network) and also
  // the media streams initiated between those peers.
  //
  // Note that when you call my Terminate method, my owning UserAgent will
  // destroy me, and your reference to me will no longer be valid. The same
  // thing goes for when I notify you that I have terminated via
  // OnEndedSession.
  TIdSipSession = class(TIdSipAction)
  private
    DialogLock:           TCriticalSection;
    fDialog:              TIdSipDialog;
    fPayloadProcessor:    TIdSdpPayloadProcessor;
    fReceivedAck:         Boolean;
    OpenTransactionLock:  TCriticalSection;
    OpenTransactions:     TObjectList;
    SessionListenerLock:  TCriticalSection;
    SessionListeners:     TList;
    UsingSecureTransport: Boolean;

    procedure MarkAsTerminatedProc(ObjectOrIntf: Pointer);
    procedure NotifyOfModifiedSession(Invite: TIdSipRequest);
    procedure ProcessBye(Request: TIdSipRequest);
    procedure RejectOutOfOrderRequest(Request: TIdSipRequest);
    procedure RejectRequest(Request: TIdSipRequest);
    procedure TerminateOpenTransaction(Request: TIdSipRequest);
  protected
    FullyEstablished: Boolean;

    procedure ActionSucceeded(Response: TIdSipResponse); override;
    procedure AddOpenTransaction(Request: TIdSipRequest);
    function  GetInvite: TIdSipRequest; virtual;
    procedure MarkAsTerminated; override;
    procedure NotifyOfEndedSession(const Reason: String);
    procedure NotifyOfEstablishedSession;
    procedure NotifyOfFailure(Response: TIdSipResponse); override;
    procedure SendBye; virtual;
  public
    constructor Create(UA: TIdSipUserAgentCore); override;
    destructor  Destroy; override;

    procedure AddSessionListener(const Listener: IIdSipSessionListener);
    function  IsEarly: Boolean;
    function  DialogEstablished: Boolean;
    function  IsInboundCall: Boolean; virtual; abstract;
    function  IsOutboundCall: Boolean;
    function  IsSession: Boolean; override;
    function  Match(Msg: TIdSipMessage): Boolean; override;
    procedure Modify(const Offer, ContentType: String);
    function  PendingTransactionCount: Integer;
    procedure ReceiveRequest(Request: TIdSipRequest); override;
    procedure RemoveSessionListener(const Listener: IIdSipSessionListener);

    property Dialog:           TIdSipDialog           read fDialog;
    property Invite:           TIdSipRequest          read GetInvite;
    property PayloadProcessor: TIdSdpPayloadProcessor read fPayloadProcessor;
    property ReceivedAck:      Boolean                read fReceivedAck;
  end;

  TIdSipInboundSession = class(TIdSipSession)
  private
    LastResponse: TIdSipResponse;
    OkTimer:      TIdSipSessionTimer;

    function  CreateInboundDialog(Response: TIdSipResponse): TIdSipDialog;
    procedure TerminatePendingInvite;
  public
    constructor Create(UA: TIdSipUserAgentCore;
                       Invite: TIdSipRequest;
                       UsingSecureTransport: Boolean); reintroduce; virtual;
    destructor  Destroy; override;

    function  AcceptCall(const Offer, ContentType: String): String;
    procedure ForwardCall(NewDestination: TIdSipAddressHeader);
    function  IsInboundCall: Boolean; override;
    procedure ReceiveRequest(Request: TIdSipRequest); override;
    procedure RejectCallBusy;
    procedure ResendLastResponse; virtual;
    procedure Terminate; override;
    procedure TimeOut;
  end;

  TIdSipOutboundSession = class(TIdSipSession)
  private
    InCall: Boolean;

    function  CreateOutboundDialog(Response: TIdSipResponse;
                                   UsingSecureTransport: Boolean): TIdSipDialog;
    procedure Initialize;
    procedure SendAck(Final: TIdSipResponse);
    procedure SendCancel;
    procedure TerminateAfterSendingCancel;
  protected
    function  ReceiveOKResponse(Response: TIdSipResponse;
                                UsingSecureTransport: Boolean): Boolean; override;
    function  ReceiveProvisionalResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): Boolean; override;
    function  ReceiveRedirectionResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): Boolean; override;
    procedure SendBye; override;
  public
    constructor Create(UA: TIdSipUserAgentCore); overload; override;
    constructor Create(UA: TIdSipUserAgentCore;
                       Invite: TIdSipRequest); reintroduce; overload;

    procedure Call(Dest: TIdSipAddressHeader;
                   const InitialOffer: String;
                   const MimeType: String);
    procedure Cancel;
    function  CanForkOn(Response: TIdSipResponse): Boolean;
    function  Fork(OkResponse: TIdSipResponse): TIdSipOutboundSession;
    function  IsInboundCall: Boolean; override;
    procedure Terminate; override;
  end;

  // I piggyback on a transaction in a blocking I/O fashion to provide a UAC
  // with a way to register with a registrar. I take care of things like
  // doing stuff with error responses, asking for authentication, etc.
  //
  // It makes no sense to access me once my Transaction has terminated. In
  // other words once you've received notification of my success or failure,
  // erase your references to me.
  TIdSipRegistration = class(TIdSipAction)
  private
    Bindings:     TIdSipContacts;
    ListenerLock: TCriticalSection;
    Listeners:    TList;

    function  CreateRegister(Registrar: TIdSipUri;
                             Bindings: TIdSipContacts): TIdSipRequest;
    procedure NotifyOfAuthenticationChallenge(Response: TIdSipResponse);
    procedure NotifyOfSuccess(Response: TIdSipResponse);
    procedure ReissueRequest(Registrar: TIdSipUri;
                             MinimumExpiry: Cardinal);
    procedure RetryWithoutExtensions(Registrar: TIdSipUri;
                                     Response: TIdSipResponse);
    procedure Send(Request: TIdSipRequest);
  protected
    procedure ActionSucceeded(Response: TIdSipResponse); override;
    procedure NotifyOfFailure(Response: TIdSipResponse); override;
    function  ReceiveFailureResponse(Response: TIdSipResponse): Boolean; override;
    function  ReceiveRedirectionResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): Boolean; override;
  public
    constructor Create(UA: TIdSipUserAgentCore); override;
    destructor  Destroy; override;

    procedure AddListener(const Listener: IIdSipRegistrationListener);
    procedure FindCurrentBindings(Registrar: TIdSipUri);
    function  IsRegistration: Boolean; override;
    function  Match(Msg: TIdSipMessage): Boolean; override;
    procedure RegisterWith(Registrar: TIdSipUri; Bindings: TIdSipContacts); overload;
    procedure RegisterWith(Registrar: TIdSipUri; Contact: TIdSipContactHeader); overload;
    procedure RemoveListener(const Listener: IIdSipRegistrationListener);
    procedure Terminate; override;
    procedure Unregister(Registrar: TIdSipUri);
  end;

  EIdSipBadSyntax = class(EIdException);

const
  MissingContactHeader = 'Missing Contact Header';

implementation

uses
  IdGlobal, IdSimpleParser, IdSipConsts, IdSipDialogID, IdRandom, IdStack,
  SysUtils, IdUDPServer;

const
  BusyHere        = 'Incoming call rejected - busy here';
  CallForwarded   = 'Incoming call forwarded';
  InviteTimeout   = 'Incoming call timed out';
  LocalCancel     = 'Local end cancelled call';
  LocalHangUp     = 'Local end hung up';
  RemoteHangUp    = 'Remote end hung up';

//******************************************************************************
//* TIdSipAbstractCore                                                         *
//******************************************************************************
//* TIdSipAbstractCore Public methods ******************************************

constructor TIdSipAbstractCore.Create;
begin
  inherited Create;

  Self.HostName := Self.DefaultHostName;
end;

function TIdSipAbstractCore.NextCallID: String;
begin
  Result := GRandomNumber.NextHexString + '@' + Self.HostName;
end;

//* TIdSipAbstractCore Protected methods ***************************************

procedure TIdSipAbstractCore.ActOnRequest(Request: TIdSipRequest;
                                          Receiver: TIdSipTransport);
begin
end;

procedure TIdSipAbstractCore.ActOnResponse(Response: TIdSipResponse;
                                           Receiver: TIdSipTransport);
begin
end;

procedure TIdSipAbstractCore.OnReceiveRequest(Request: TIdSipRequest;
                                              Receiver: TIdSipTransport);
begin
end;

procedure TIdSipAbstractCore.OnReceiveResponse(Response: TIdSipResponse;
                                               Receiver: TIdSipTransport);
begin
end;

procedure TIdSipAbstractCore.RejectRequest(Reaction: TIdSipUserAgentReaction;
                                           Request: TIdSipRequest);
begin
  // Do nothing - subclasses will return the necessary response.
end;

function TIdSipAbstractCore.WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  Result := uarAccept;
end;

function  TIdSipAbstractCore.WillAcceptResponse(Response: TIdSipResponse): TIdSipUserAgentReaction;
begin
  // cf RFC 3261 section 8.1.3.3
  if (Response.Path.Count > 1) then
    Result := uarTooManyVias
  else
    Result := uarAccept;
end;

//* TIdSipAbstractCore Private methods *****************************************

function TIdSipAbstractCore.DefaultHostName: String;
begin
  Result := 'localhost';
end;

procedure TIdSipAbstractCore.OnReceiveUnhandledRequest(Request: TIdSipRequest;
                                                       Receiver: TIdSipTransport);
var
  Reaction: TIdSipUserAgentReaction;
begin
  Reaction := Self.WillAcceptRequest(Request);
  if (Reaction = uarAccept) then
    Self.ActOnRequest(Request, Receiver)
  else
    Self.RejectRequest(Reaction, Request);
end;

procedure TIdSipAbstractCore.OnReceiveUnhandledResponse(Response: TIdSipResponse;
                                                        Receiver: TIdSipTransport);
begin
  // We silently discard unacceptable responses.
  if (Self.WillAcceptResponse(Response) = uarAccept) then
    Self.ActOnResponse(Response, Receiver);
end;

procedure TIdSipAbstractCore.SetDispatcher(Value: TIdSipTransactionDispatcher);
begin
  fDispatcher := Value;

  fDispatcher.AddUnhandledMessageListener(Self);
end;

//******************************************************************************
//* TIdSipAbstractUserAgent                                                    *
//******************************************************************************
//* TIdSipAbstractUserAgent Public methods *************************************

constructor TIdSipAbstractUserAgent.Create;
begin
  inherited Create;

  Self.fAllowedContentTypeList := TStringList.Create;
  Self.fAllowedLanguageList    := TStringList.Create;
  Self.fAllowedMethodList      := TStringList.Create;
  Self.fAllowedSchemeList      := TStringList.Create;
end;

destructor TIdSipAbstractUserAgent.Destroy;
begin
  Self.AllowedSchemeList.Free;
  Self.AllowedMethodList.Free;
  Self.AllowedLanguageList.Free;
  Self.AllowedContentTypeList.Free;

  inherited Destroy;
end;

procedure TIdSipAbstractUserAgent.AddAllowedContentType(const MimeType: String);
begin
  if (Trim(MimeType) <> '') then begin
    if (Self.AllowedContentTypeList.IndexOf(MimeType) = -1) then
      Self.AllowedContentTypeList.Add(MimeType);
  end;
end;

procedure TIdSipAbstractUserAgent.AddAllowedLanguage(const LanguageID: String);
begin
  if (Trim(LanguageID) = '') then
    raise EIdSipBadSyntax.Create('Not a valid language identifier');

  if (Self.AllowedLanguageList.IndexOf(LanguageID) = -1) then
    Self.AllowedLanguageList.Add(LanguageID);
end;

procedure TIdSipAbstractUserAgent.AddAllowedMethod(const Method: String);
begin
  if not TIdSipParser.IsToken(Method) then
    raise EIdSipBadSyntax.Create('Not a token');

  if (Self.AllowedMethodList.IndexOf(Method) = -1) then
    Self.AllowedMethodList.Add(Method);
end;

procedure TIdSipAbstractUserAgent.AddAllowedScheme(const Scheme: String);
begin
  if not TIdSipParser.IsScheme(Scheme) then
    raise EIdSipBadSyntax.Create('Not a valid scheme');

  if (Self.AllowedSchemeList.IndexOf(Scheme) = -1) then
    Self.AllowedSchemeList.Add(Scheme);
end;

function TIdSipAbstractUserAgent.AllowedContentTypes: String;
begin
  Result := Self.AllowedContentTypeList.CommaText;
end;

function TIdSipAbstractUserAgent.AllowedExtensions: String;
begin
  Result := '';
end;

function TIdSipAbstractUserAgent.AllowedLanguages: String;
begin
  Result := Self.AllowedLanguageList.CommaText;
end;

function TIdSipAbstractUserAgent.AllowedMethods: String;
begin
  Result := Self.AllowedMethodList.CommaText;
end;

function TIdSipAbstractUserAgent.AllowedSchemes: String;
begin
  Result := Self.AllowedSchemeList.CommaText;
end;

function TIdSipAbstractUserAgent.CreateRequest(Dest: TIdSipAddressHeader): TIdSipRequest;
begin
  Result := TIdSipRequest.Create;
  try
    Result.RequestUri     := Dest.Address;
    Result.From           := Self.From;
    Result.From.Tag       := Self.NextTag;
    Result.ToHeader.Value := Dest.FullValue;

    Result.CallID          := Self.NextCallID;
    Result.CSeq.SequenceNo := Self.NextInitialSequenceNo;

    Self.AddLocalHeaders(Result);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipAbstractUserAgent.CreateResponse(Request: TIdSipRequest;
                                                ResponseCode: Cardinal): TIdSipResponse;
begin
  Result := TIdSipResponse.InResponseTo(Request,
                                        ResponseCode);

  Self.PrepareResponse(Result, Request);
end;

function TIdSipAbstractUserAgent.HasUnknownContentEncoding(Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(ContentEncodingHeaderFull);
end;

function TIdSipAbstractUserAgent.HasUnknownContentLanguage(Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(ContentLanguageHeader)
       and (Self.AllowedLanguageList.IndexOf(Request.FirstHeader(ContentLanguageHeader).Value) = -1);
end;

function TIdSipAbstractUserAgent.HasUnknownContentType(Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(ContentTypeHeaderFull)
       and (Self.AllowedContentTypeList.IndexOf(Request.FirstHeader(ContentTypeHeaderFull).Value) = -1);
end;

function TIdSipAbstractUserAgent.IsExtensionAllowed(const Extension: String): Boolean;
begin
  Result := false;
end;

function TIdSipAbstractUserAgent.IsMethodAllowed(const Method: String): Boolean;
begin
  Result := Self.AllowedMethodList.IndexOf(Method) >= 0;
end;

function TIdSipAbstractUserAgent.IsSchemeAllowed(const Scheme: String): Boolean;
begin
  Result := Self.AllowedSchemeList.IndexOf(Scheme) >= 0;
end;

function TIdSipAbstractUserAgent.NextBranch: String;
begin
  Result := GRandomNumber.NextSipUserAgentBranch;
end;

function TIdSipAbstractUserAgent.NextInitialSequenceNo: Cardinal;
begin
  Result := GRandomNumber.NextCardinal($7FFFFFFF);
end;

function TIdSipAbstractUserAgent.NextTag: String;
begin
  Result := GRandomNumber.NextSipUserAgentTag;
end;

procedure TIdSipAbstractUserAgent.ReturnResponse(Request: TIdSipRequest;
                                                 Reason: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, Reason);
  try
    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

//* TIdSipAbstractUserAgent Protected methods **********************************

procedure TIdSipAbstractUserAgent.AddLocalHeaders(OutboundRequest: TIdSipRequest);
var
  Transport: String;
begin
  // The transport must be discovered using RFC 3263
  // TODO: Lies. Pure hack to get X-Lite talking
  if (Pos(TransportParam, OutboundRequest.RequestUri.AsString) > 0) then
    Transport := TransportParamUDP // Todo: replace IdUri completely. It's just crap.
  else
    Transport := TransportParamTCP;

  OutboundRequest.AddHeader(ViaHeaderFull).Value := SipVersion + '/' + Transport + ' ' + Self.HostName;
  OutboundRequest.LastHop.Branch := Self.NextBranch;

  if (Self.UserAgentName <> '') then
    OutboundRequest.AddHeader(UserAgentHeader).Value := Self.UserAgentName;
end;

procedure TIdSipAbstractUserAgent.PrepareResponse(Response: TIdSipResponse;
                                                  Request: TIdSipRequest);
begin
  if not Request.ToHeader.HasTag then
    Response.ToHeader.Tag := Self.NextTag;

  if (Self.UserAgentName <> '') then
    Response.AddHeader(ServerHeader).Value := Self.UserAgentName;
end;

procedure TIdSipAbstractUserAgent.RejectRequest(Reaction: TIdSipUserAgentReaction;
                                                Request: TIdSipRequest);
begin
  inherited RejectRequest(Reaction, Request);

  case Reaction of
    uarLoopDetected:
      Self.ReturnResponse(Request, SIPLoopDetected);
    uarUnsupportedContentEncoding:
      Self.RejectRequestUnknownContentEncoding(Request);
    uarUnsupportedContentLanguage:
      Self.RejectRequestUnknownContentLanguage(Request);
    uarUnsupportedContentType:
      Self.RejectRequestUnknownContentType(Request);
    uarUnsupportedExtension:
      Self.RejectRequestBadExtension(Request);
    uarUnsupportedMethod:
      Self.RejectRequestMethodNotAllowed(Request);
    uarUnsupportedScheme:
      Self.ReturnResponse(Request, SIPUnsupportedURIScheme);
    uarUnSupportedSipVersion:
      Self.RejectUnsupportedSipVersion(Request);
  end;
end;

procedure TIdSipAbstractUserAgent.RejectRequestBadExtension(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  // We simply reject ALL Requires
  Response := Self.CreateResponse(Request, SIPBadExtension);
  try
    Response.AddHeader(UnsupportedHeader).Value := Request.FirstHeader(RequireHeader).Value;

    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

function TIdSipAbstractUserAgent.WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  Result := inherited WillAcceptRequest(Request);

  // cf RFC 3261 section 8.2
  if (Result = uarAccept) then begin
    if (Request.SIPVersion <> SipVersion) then
      Result := uarUnsupportedSipVersion
    // inspect the method - 8.2.1
    else if not Request.IsAck and not Self.IsMethodAllowed(Request.Method) then
      Result := uarUnsupportedMethod
    // inspect the headers - 8.2.2
    // To & Request-URI - 8.2.2.1
    else if not Self.IsSchemeAllowed(Request.RequestUri.Scheme) then
      Result := uarUnsupportedScheme
    // Merged requests - 8.2.2.2
    else if not Request.ToHeader.HasTag and Self.Dispatcher.LoopDetected(Request) then
      Result := uarLoopDetected
    // Require - 8.2.2.3
    else if Request.HasHeader(RequireHeader) then
      Result := uarUnsupportedExtension
    // Content processing - 8.2.3
    else if Self.HasUnknownContentEncoding(Request) then
      Result := uarUnsupportedContentEncoding
    else if Self.HasUnknownContentLanguage(Request) then
      Result := uarUnsupportedContentLanguage
    else if Self.HasUnknownContentType(Request) then
      Result := uarUnsupportedContentType;
  end;
end;

//* TIdSipAbstractUserAgent Private methods ************************************

function TIdSipAbstractUserAgent.GetFrom: TIdSipFromHeader;
begin
  if not Assigned(fFrom) then
    fFrom := TIdSipFromHeader.Create;

  Result := fFrom;
end;

procedure TIdSipAbstractUserAgent.RejectRequestMethodNotAllowed(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPMethodNotAllowed);
  try
    Response.StatusText := Response.StatusText + ' (' + Request.Method + ')';
    Response.AddHeader(AllowHeader).Value := Self.AllowedMethods;

    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectRequestUnknownContentEncoding(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptEncodingHeader).Value := '';

    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectRequestUnknownContentLanguage(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptLanguageHeader).Value := Self.AllowedLanguages;

    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectRequestUnknownContentType(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptHeader).Value := Self.AllowedContentTypes;

    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectUnsupportedSipVersion(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPSIPVersionNotSupported);
  try
    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.SetFrom(Value: TIdSipFromHeader);
begin
  Self.From.Assign(Value);
end;

//******************************************************************************
//* TIdSipRegistrationInfo                                                     *
//******************************************************************************
//* TIdSipRegistrationInfo Public methods **************************************

constructor TIdSipRegistrationInfo.Create;
begin
  inherited Create;

  Self.fRegistrar := TIdSipUri.Create;
end;

destructor TIdSipRegistrationInfo.Destroy;
begin
  Self.Registrar.Free;

  inherited Destroy;
end;

//******************************************************************************
//* TIdSipUserAgentCore                                                        *
//******************************************************************************
//* TIdSipUserAgentCore Public methods *****************************************

constructor TIdSipUserAgentCore.Create;
begin
  inherited Create;

  Self.fProxy := TIdSipUri.Create('');

  Self.KnownRegistrars := TObjectList.Create(true);

  Self.ActionLock               := TCriticalSection.Create;
  Self.Actions                  := TObjectList.Create;
  Self.ObserverLock             := TCriticalSection.Create;
  Self.Observers                := TList.Create;
  Self.RegistrationListenerLock := TCriticalSection.Create;
  Self.RegistrationListeners    := TList.Create;
  Self.UserAgentListenerLock    := TCriticalSection.Create;
  Self.UserAgentListeners       := TList.Create;

  Self.fAllowedContentTypeList := TStringList.Create;
  Self.fAllowedLanguageList    := TStringList.Create;

  Self.AddAllowedContentType(SdpMimeType);
  Self.AddAllowedMethod(MethodBye);
  Self.AddAllowedMethod(MethodCancel);
  Self.AddAllowedMethod(MethodInvite);
  Self.AddAllowedScheme(SipScheme);

  Self.DoNotDisturb        := false;
  Self.DoNotDisturbMessage := RSSIPTemporarilyUnavailable;
  Self.From.Value          := Self.DefaultFrom;
  Self.HasProxy            := false;
  Self.HostName            := Self.DefaultHostName;
  Self.UserAgentName       := Self.DefaultUserAgent;
end;

destructor TIdSipUserAgentCore.Destroy;
begin
  Self.Contact.Free;
  Self.From.Free;
  Self.UserAgentListeners.Free;
  Self.UserAgentListenerLock.Free;
  Self.RegistrationListeners.Free;
  Self.RegistrationListenerLock.Free;
  Self.Observers.Free;
  Self.ObserverLock.Free;
  Self.Actions.Free;
  Self.ActionLock.Free;  
  Self.KnownRegistrars.Free;
  Self.Proxy.Free;

  inherited Destroy;
end;

procedure TIdSipUserAgentCore.AddObserver(const Listener: IIdSipObserver);
begin
  Self.ObserverLock.Acquire;
  try
    Self.Observers.Add(Pointer(Listener));
  finally
    Self.ObserverLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.AddUserAgentListener(const Listener: IIdSipUserAgentListener);
begin
  Self.UserAgentListenerLock.Acquire;
  try
    Self.UserAgentListeners.Add(Pointer(Listener));
  finally
    Self.UserAgentListenerLock.Release;
  end;
end;

function TIdSipUserAgentCore.Call(Dest: TIdSipAddressHeader;
                                  const InitialOffer: String;
                                  const MimeType: String): TIdSipOutboundSession;
begin
  Result := Self.AddOutboundSession;
  Result.Call(Dest, InitialOffer, MimeType);
end;

function TIdSipUserAgentCore.CreateBye(Dialog: TIdSipDialog): TIdSipRequest;
begin
  try
    Result := Self.CreateRequest(Dialog);
    Result.Method      := MethodBye;
    Result.CSeq.Method := Result.Method;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipUserAgentCore.CreateInvite(Dest: TIdSipAddressHeader;
                                          const Body: String;
                                          const MimeType: String): TIdSipRequest;
begin
  Result := Self.CreateRequest(Dest);
  try
    Self.TurnIntoInvite(Result, Body, MimeType);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipUserAgentCore.CreateRegister(Registrar: TIdSipToHeader): TIdSipRequest;
begin
  Result := Self.CreateRequest(Registrar);
  try
    Self.AddKnownRegistrar(Registrar.Address,
                           Result.CallID,
                           Result.CSeq.SequenceNo);

    Result.Method := MethodRegister;
    Result.RequestUri.EraseUserInfo;

    Result.CSeq.Method     := MethodRegister;
    Result.CSeq.SequenceNo := Self.NextSequenceNoFor(Registrar.Address);

    Result.CallID := Self.CallIDFor(Registrar.Address);

    Result.ToHeader.Value := Self.Contact.Value;
    Result.From.Value     := Self.Contact.Value;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipUserAgentCore.CreateReInvite(Dialog: TIdSipDialog;
                                            const Body: String;
                                            const MimeType: String): TIdSipRequest;
begin
  Result := Self.CreateRequest(Dialog);
  try
    Self.TurnIntoInvite(Result, Body, MimeType);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipUserAgentCore.CreateRequest(Dialog: TIdSipDialog): TIdSipRequest;
begin
  Result := Dialog.CreateRequest;
  try
    Self.AddLocalHeaders(Result);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipUserAgentCore.CreateResponse(Request: TIdSipRequest;
                                            ResponseCode: Cardinal): TIdSipResponse;
begin
  Result := TIdSipResponse.InResponseTo(Request,
                                        ResponseCode,
                                        Self.Contact);

  Self.PrepareResponse(Result, Request);
end;

function TIdSipUserAgentCore.CurrentRegistrationWith(Registrar: TIdSipUri): TIdSipRegistration;
begin
  Result := Self.AddRegistration;

  Result.RegisterWith(Registrar, Self.Contact);
end;

function TIdSipUserAgentCore.RegisterWith(Registrar: TIdSipUri): TIdSipRegistration;
begin
  Result := Self.AddRegistration;

  Result.RegisterWith(Registrar, Self.Contact);
end;

function TIdSipUserAgentCore.RegistrationCount: Integer;
var
  I: Integer;
begin
  // Return the number of ongoing registration attempts
  Self.ActionLock.Acquire;
  try
    Result := 0;

    for I := 0 to Self.Actions.Count - 1 do
      if Self.ActionAt(I).IsRegistration
        and not Self.ActionAt(I).IsTerminated then Inc(Result);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.RemoveObserver(const Listener: IIdSipObserver);
begin
  Self.ObserverLock.Acquire;
  try
    Self.Observers.Remove(Pointer(Listener));
  finally
    Self.ObserverLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.RemoveAction(Action: TIdSipAction);
begin
//  if not Action.IsTerminated then
//    Action.Terminate;

  Self.ActionLock.Acquire;
  try
    Self.Actions.Remove(Action);
  finally
    Self.ActionLock.Release;
  end;
  Self.NotifyOfChange;
end;

procedure TIdSipUserAgentCore.RemoveUserAgentListener(const Listener: IIdSipUserAgentListener);
begin
  Self.UserAgentListenerLock.Acquire;
  try
    Self.UserAgentListeners.Remove(Pointer(Listener));
  finally
    Self.UserAgentListenerLock.Release;
  end;
end;

function TIdSipUserAgentCore.SessionCount: Integer;
var
  I: Integer;
begin
  // Return the number of ongoing Sessions
  Self.ActionLock.Acquire;
  try
    Result := 0;

    for I := 0 to Self.Actions.Count - 1 do
      if Self.ActionAt(I).IsSession
        and not Self.ActionAt(I).IsTerminated then
        Inc(Result);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.HangUpAllCalls;
var
  CopyOfSessions: TObjectList;
  I:              Integer;
begin
  // We copy the sessions because when they terminate we lose our reference
  // to them and we remove them from Self.Actions
  Self.ActionLock.Acquire;
  try
    CopyOfSessions := TObjectList.Create(false);
    try
      for I := 0 to Self.Actions.Count - 1 do
        if Self.ActionAt(I).IsSession and not Self.ActionAt(I).IsTerminated then
          CopyOfSessions.Add(Self.ActionAt(I));

      for I := 0 to CopyOfSessions.Count - 1 do
        (CopyOfSessions[I] as TIdSipAction).Terminate;
    finally
      CopyOfSessions.Free;
    end;
  finally
    Self.ActionLock.Release;
  end;
end;

function TIdSipUserAgentCore.UnregisterFrom(Registrar: TIdSipUri): TIdSipRegistration;
begin
  Result := Self.AddRegistration;

  Result.Unregister(Registrar);
end;

function TIdSipUserAgentCore.Username: String;
begin
  Result := Self.From.Address.Username;
end;

//* TIdSipUserAgentCore Protected methods **************************************

procedure TIdSipUserAgentCore.ActOnRequest(Request: TIdSipRequest;
                                           Receiver: TIdSipTransport);
begin
  inherited ActOnRequest(Request, Receiver);

  // Processing the request - 8.2.5
  if Request.IsInvite then
    Self.ProcessInvite(Request, Receiver)
  else if Request.IsAck then
    Self.ProcessAck(Request)
  else if Request.IsBye then
    Self.SendByeToAppropriateSession(Request);

  // TIdSipSession generates the response - 8.2.6
end;

procedure TIdSipUserAgentCore.ActOnResponse(Response: TIdSipResponse;
                                            Receiver: TIdSipTransport);
var
  Session: TIdSipAction;
begin
  inherited ActOnResponse(Response, Receiver);

  // User Agents drop unmatched responses on the floor.
  // Except for 2xx's on a client INVITE. And these no longer belong to
  // a transaction, since the receipt of a 200 terminates a client INVITE
  // immediately.
  if Response.IsOK then begin
    Session := Self.FindAction(Response);

    if not Assigned(Session) then
      Session := Self.ForkCall(Response);

    if Assigned(Session) then
      Session.ReceiveResponse(Response, Receiver.IsSecure);
  end
  else
    Self.NotifyOfDroppedResponse(Response, Receiver);
end;

procedure TIdSipUserAgentCore.AddLocalHeaders(OutboundRequest: TIdSipRequest);
begin
  inherited AddLocalHeaders(OutboundRequest);

  OutboundRequest.AddHeader(Self.Contact);

  if Self.HasProxy then
    OutboundRequest.Route.AddRoute(Self.Proxy);

  if OutboundRequest.HasSipsUri then
    OutboundRequest.FirstContact.Address.Scheme := SipsScheme;
end;

procedure TIdSipUserAgentCore.OnReceiveRequest(Request: TIdSipRequest;
                                               Receiver: TIdSipTransport);
var
  Action: TIdSipAction;
begin
  Action := Self.FindAction(Request);

  if Assigned(Action) then
    Action.ReceiveRequest(Request);
end;

procedure TIdSipUserAgentCore.OnReceiveResponse(Response: TIdSipResponse;
                                                Receiver: TIdSipTransport);
var
  Action:               TIdSipAction;
  UsingSecureTransport: Boolean;
begin
  UsingSecureTransport := Receiver.IsSecure;

  Action := Self.FindAction(Response);
  
  if Assigned(Action) then
    Action.ReceiveResponse(Response, UsingSecureTransport)
  else
    Self.NotifyOfDroppedResponse(Response, Receiver);
end;

procedure TIdSipUserAgentCore.RejectRequest(Reaction: TIdSipUserAgentReaction;
                                            Request: TIdSipRequest);
begin
  inherited RejectRequest(Reaction, Request);

  case Reaction of
    uarMissingContact:
      Self.RejectBadRequest(Request, MissingContactHeader);
  end;
end;

function TIdSipUserAgentCore.WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  Result := inherited WillAcceptRequest(Request);

  if (Result = uarAccept) then begin
    // Section 8.1.1.8 says that a request that can start a dialog (like an
    // INVITE), MUST contain a Contact.
    if Request.IsInvite and not Request.HasHeader(ContactHeaderFull) then
      Result := uarMissingContact;
  end;
end;

//* TIdSipUserAgentCore Private methods ****************************************

function TIdSipUserAgentCore.ActionAt(Index: Integer): TIdSipAction;
begin
  // Precondition: you've invoked Self.ActionLock.Acquire
  Result := Self.Actions[Index] as TIdSipAction;
end;

function TIdSipUserAgentCore.AddFork(RootSession: TIdSipOutboundSession;
                                     Response: TIdSipResponse): TIdSipOutboundSession;
begin
  // RFC 3261, section 13.2.2.4

  Self.ActionLock.Acquire;
  try
    Result := RootSession.Fork(Response);
    Self.Actions.Add(Result);
  finally
    Self.ActionLock.Release;
  end;

  Self.NotifyOfChange;
end;                  

function TIdSipUserAgentCore.AddInboundSession(Invite: TIdSipRequest;
                                               Receiver: TIdSipTransport): TIdSipInboundSession;
begin
  Result := TIdSipInboundSession.Create(Self, Invite, Receiver.IsSecure);
  try
    Self.ActionLock.Acquire;
    try
      Self.Actions.Add(Result);
    finally
      Self.ActionLock.Release;
    end;

    Self.NotifyOfInboundCall(Result);
    Self.NotifyOfChange;

    if Invite.HasHeader(ExpiresHeader) then begin
      TIdSipSingleShotTimer.Create(Self.OnInboundSessionExpire,
                                   Invite.FirstExpires.NumericValue,
                                   Invite.Copy);
    end;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

procedure TIdSipUserAgentCore.AddKnownRegistrar(Registrar: TIdSipUri;
                                                const CallID: String;
                                                SequenceNo: Cardinal);
var
  NewReg: TIdSipRegistrationInfo;
begin
  if not Self.KnowsRegistrar(Registrar) then begin
    NewReg := TIdSipRegistrationInfo.Create;
    Self.KnownRegistrars.Add(NewReg);

    NewReg.CallID        := CallID;
    NewReg.Registrar.Uri := Registrar.Uri;
    NewReg.SequenceNo    := SequenceNo;
  end;
end;

function TIdSipUserAgentCore.AddOutboundSession: TIdSipOutboundSession;
begin
  Result := TIdSipOutboundSession.Create(Self);
  try
    Self.ActionLock.Acquire;
    try
      Self.Actions.Add(Result);
    finally
      Self.ActionLock.Release;
    end;

    Self.NotifyOfChange;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipUserAgentCore.AddRegistration: TIdSipRegistration;
begin
  Result := TIdSipRegistration.Create(Self);
  try
    Self.ActionLock.Acquire;
    try
      Self.Actions.Add(Result);
    finally
      Self.ActionLock.Release;
    end;
    Self.NotifyOfChange;
  except
    FreeAndNil(Result);

    raise;
  end;
end;


function TIdSipUserAgentCore.CallIDFor(Registrar: TIdSipUri): String;
begin
  Assert(Self.KnowsRegistrar(Registrar), 'A registrar wasn''t added');
  Result := Self.RegistrarAt(Self.IndexOfRegistrar(Registrar)).CallID
end;

function TIdSipUserAgentCore.DefaultFrom: String;
begin
  Result := 'unknown <sip:unknown@' + Self.HostName + '>';
end;

function TIdSipUserAgentCore.DefaultUserAgent: String;
begin
  Result := 'Indy SIP/2.0 Server v0.1';
end;

function TIdSipUserAgentCore.FindAction(Msg: TIdSipMessage): TIdSipAction;
var
  I:      Integer;
  Action: TIdSipAction;
begin
  Result := nil;
  Self.ActionLock.Acquire;
  try
    I := 0;
    while (I < Self.Actions.Count) and not Assigned(Result) do begin
      Action := Self.Actions[I] as TIdSipAction;
      if Action.Match(Msg) and not Action.IsTerminated then
        Result := Action
      else
        Inc(I);
    end;
  finally
    Self.ActionLock.Release;
  end;
end;

function TIdSipUserAgentCore.ForkCall(Response: TIdSipResponse): TIdSipOutboundSession;
var
  Root:    TIdSipOutboundSession;
  I:       Integer;
  Session: TIdSipOutboundSession;
begin
  // We received a second (or third, or...) 2xx in response to an INVITE.
  // We create a new outbound session. This means we need to find the
  // INVITE.

  Result := nil;
  Root   := nil;
  Self.ActionLock.Acquire;
  try
    I := 0;
    while (I < Self.Actions.Count) and not Assigned(Root) do begin
      if (Self.Actions[I] is TIdSipSession) and
        (Self.Actions[I] as TIdSipSession).IsInboundCall then Inc(I)
      else begin
        Session := Self.Actions[I] as TIdSipOutboundSession;
        if Session.CanForkOn(Response) then
          Root := Session
        else
          Inc(I);
      end;
    end;

    if Assigned(Root) then begin
      Result := Self.AddFork(Root, Response);
    end;

  finally
    Self.ActionLock.Release;
  end;
end;

function TIdSipUserAgentCore.GetContact: TIdSipContactHeader;
begin
  if not Assigned(fContact) then
    fContact := TIdSipContactHeader.Create;

  Result := fContact;
end;

function TIdSipUserAgentCore.IndexOfRegistrar(Registrar: TIdSipUri): Integer;
begin
  // Precondition: something's acquired the RegistrationLock
  Result := 0;
  while (Result < Self.KnownRegistrars.Count) do
    if Self.RegistrarAt(Result).Registrar.Equals(Registrar) then
      Break
    else
      Inc(Result);

  if (Result >= Self.KnownRegistrars.Count) then
    Result := -1;
end;

function TIdSipUserAgentCore.KnowsRegistrar(Registrar: TIdSipUri): Boolean;
begin
  // Precondition: something's acquired the RegistrationLock
  Result := Self.IndexOfRegistrar(Registrar) <> -1;
end;

function TIdSipUserAgentCore.NextSequenceNoFor(Registrar: TIdSipUri): Cardinal;
var
  RegInfo: TIdSipRegistrationInfo;
begin
  Assert(Self.KnowsRegistrar(Registrar), 'A registrar wasn''t added');

  RegInfo := Self.RegistrarAt(Self.IndexOfRegistrar(Registrar));
  Result := RegInfo.SequenceNo;
  RegInfo.SequenceNo := Result + 1;
end;

procedure TIdSipUserAgentCore.NotifyOfInboundCall(Session: TIdSipInboundSession);
var
  I: Integer;
begin
  Self.UserAgentListenerLock.Acquire;
  try
    for I := 0 to Self.UserAgentListeners.Count - 1 do
      IIdSipUserAgentListener(Self.UserAgentListeners[I]).OnInboundCall(Session);
  finally
    Self.UserAgentListenerLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.NotifyOfChange;
var
  I: Integer;
begin
  Self.ObserverLock.Acquire;
  try
    for I := 0 to Self.Observers.Count - 1 do
      IIdSipObserver(Self.Observers[I]).OnChanged(Self);
  finally
    Self.ObserverLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.NotifyOfDroppedResponse(Response: TIdSipResponse;
                                                      Receiver: TIdSipTransport);
var
  I: Integer;
begin
  Self.UserAgentListenerLock.Acquire;
  try
    for I := 0 to Self.UserAgentListeners.Count - 1 do
      IIdSipUserAgentListener(Self.UserAgentListeners[I]).OnDroppedUnmatchedResponse(Response,
                                                                                     Receiver);
  finally
    Self.UserAgentListenerLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.OnInboundSessionExpire(Sender: TObject);
var
  Session: TIdSipAction;
begin
  Self.ActionLock.Acquire;
  try
    Session := Self.FindAction((Sender as TIdSipSingleShotTimer).Data as TIdSipRequest);

    if Assigned(Session) then
      (Session as TIdSipInboundSession).TimeOut;
  finally
    Self.ActionLock.Release;
  end;

  (Sender as TIdSipSingleShotTimer).Data.Free;
end;

procedure TIdSipUserAgentCore.ProcessAck(Ack: TIdSipRequest);
var
  Session: TIdSipAction;
begin
  Session := Self.FindAction(Ack);

  // If Session = nil then we didn't match the ACK against any session, and so
  // we just drop it on the floor. There's no point in replying.
  if Assigned(Session) then
    Session.ReceiveRequest(Ack);
end;

procedure TIdSipUserAgentCore.ProcessInvite(Invite: TIdSipRequest;
                                            Receiver: TIdSipTransport);
var
  Session: TIdSipAction;
begin
  Assert(Invite.IsInvite,
         'ProcessInvite accepts only INVITE messages, '
       + 'not ' + Invite.Method + ' messages');

  if Self.DoNotDisturb then begin
    Self.RejectDoNotDisturb(Invite,
                            Self.DoNotDisturbMessage);
    Exit;
  end;

  Session := Self.FindAction(Invite);

  if Assigned(Session) then
    Session.ReceiveRequest(Invite)
  else
    Self.AddInboundSession(Invite, Receiver);
end;

function TIdSipUserAgentCore.RegistrarAt(Index: Integer): TIdSipRegistrationInfo;
begin
  Result := Self.KnownRegistrars[Index] as TIdSipRegistrationInfo;
end;

procedure TIdSipUserAgentCore.RejectBadRequest(Request: TIdSipRequest;
                                               const Reason: String);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPBadRequest);
  try
    Response.StatusText := Reason;

    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipUserAgentCore.RejectDoNotDisturb(Request: TIdSipRequest;
                                                 const Reason: String);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPTemporarilyUnavailable);
  try
    Response.StatusText := Reason;

    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipUserAgentCore.SendByeToAppropriateSession(Bye: TIdSipRequest);
var
  Session: TIdSipAction;
begin
  Session := Self.FindAction(Bye);

  if Assigned(Session) then
    Session.ReceiveRequest(Bye)
  else
    Self.ReturnResponse(Bye,
                        SIPCallLegOrTransactionDoesNotExist);
end;

procedure TIdSipUserAgentCore.SetContact(Value: TIdSipContactHeader);
begin
  Assert(not Value.IsWildCard,
         'You may not use a wildcard Contact header for a User Agent''s '
       + 'Contact');

  Self.Contact.Assign(Value);
end;

procedure TIdSipUserAgentCore.SetProxy(Value: TIdSipUri);
begin
  Self.Proxy.Uri := Value.Uri;
end;

procedure TIdSipUserAgentCore.TurnIntoInvite(OutboundRequest: TIdSipRequest;
                                             const Offer: String;
                                             const OfferMimeType: String);
begin
  OutboundRequest.Method      := MethodInvite;
  OutboundRequest.CSeq.Method := MethodInvite;

  OutboundRequest.Body := Offer;
  OutboundRequest.ContentLength := Length(Offer);

  if (OutboundRequest.ContentLength > 0) then begin
    OutboundRequest.ContentDisposition.Value := DispositionSession;
    OutboundRequest.ContentType              := OfferMimeType;
  end;

  OutboundRequest.AddHeader(AllowHeader).Value := Self.AllowedMethods;
  // TODO: We need to add a proper extension support thing, as well as do a
  // proper payload processor capable of handling multiple content types
  OutboundRequest.AddHeader(AcceptHeader).Value := Self.AllowedContentTypes;
  OutboundRequest.AddHeader(SupportedHeaderFull).Value := Self.AllowedExtensions;
end;

//******************************************************************************
//* TIdSipAction                                                               *
//******************************************************************************
//* TIdSipAction Public methods ************************************************

constructor TIdSipAction.Create(UA: TIdSipUserAgentCore);
begin
  inherited Create;

  Self.UA := UA;

  Self.fInitialRequest := TIdSipRequest.Create;
end;

destructor TIdSipAction.Destroy;
begin
  Self.InitialRequest.Free;

  inherited Destroy;
end;

function TIdSipAction.IsRegistration: Boolean;
begin
  Result := false;
end;

function TIdSipAction.IsSession: Boolean;
begin
  Result := false;
end;

function TIdSipAction.Match(Msg: TIdSipMessage): Boolean;
begin
  Result := false;
end;

procedure TIdSipAction.ReceiveRequest(Request: TIdSipRequest);
begin
end;

procedure TIdSipAction.ReceiveResponse(Response: TIdSipResponse;
                                       UsingSecureTransport: Boolean);
var
  Succeeded: Boolean;
begin
  // Each of the ReceiveXXXResponse functions returns true if we succeeded
  // in our Action, or we could re-issue the request. They only return
  // false when the action failed irrecoverably.

  case Response.StatusCode div 100 of
    SIPProvisionalResponseClass:
      Succeeded := Self.ReceiveProvisionalResponse(Response,
                                                   UsingSecureTransport);
    SIPOKResponseClass:
      Succeeded := Self.ReceiveOKResponse(Response,
                                          UsingSecureTransport);
    SIPRedirectionResponseClass:
      Succeeded := Self.ReceiveRedirectionResponse(Response,
                                                   UsingSecureTransport);
    SIPFailureResponseClass:
      Succeeded := Self.ReceiveFailureResponse(Response);
    SIPServerFailureResponseClass:
      Succeeded := Self.ReceiveServerFailureResponse(Response);
    SIPGlobalFailureResponseClass:
      Succeeded := Self.ReceiveGlobalFailureResponse(Response);
  else
    // This should never happen - response status codes lie in the range
    // 100 <= S < 700, so we just reject these obviously malformed responses
    // by silently discarding them.
    Succeeded := false;
  end;

  if Succeeded then
    Self.ActionSucceeded(Response)
  else
    Self.NotifyOfFailure(Response);
end;

procedure TIdSipAction.Terminate;
begin
  Self.MarkAsTerminated;
end;

//* TIdSipAction Protected methods *********************************************

procedure TIdSipAction.ActionSucceeded(Response: TIdSipResponse);
begin
end;

procedure TIdSipAction.ApplyTo(List: TList;
                               Lock: TCriticalSection;
                               Proc: TIdSipProcedure);
var
  Copy: TList;
  I: Integer;
begin
  Copy := TList.Create;
  try
    Self.CopyList(List, Lock, Copy);

    for I := 0 to Copy.Count - 1 do
      Proc(Copy[I]);
  finally
    Copy.Free;
  end;
end;

procedure TIdSipAction.CopyList(Source: TList;
                                Lock: TCriticalSection;
                                Copy: TList);
var
  I: Integer;
begin
  Copy.Clear;
  Lock.Acquire;
  try
    for I := 0 to Source.Count - 1 do
      Copy.Add(Source[I]);
  finally
    Lock.Release;
  end;
end;

procedure TIdSipAction.MarkAsTerminated;
begin
  Self.fIsTerminated := true;
end;

procedure TIdSipAction.NotifyOfFailure(Response: TIdSipResponse);
begin
end;

function TIdSipAction.ReceiveFailureResponse(Response: TIdSipResponse): Boolean;
begin
  Result := false;
end;

function TIdSipAction.ReceiveGlobalFailureResponse(Response: TIdSipResponse): Boolean;
begin
  Result := false;
end;

function TIdSipAction.ReceiveOKResponse(Response: TIdSipResponse;
                                        UsingSecureTransport: Boolean): Boolean;
begin
  Result := true;
end;

function TIdSipAction.ReceiveProvisionalResponse(Response: TIdSipResponse;
                                                 UsingSecureTransport: Boolean): Boolean;
begin
  Result := false;
end;

function TIdSipAction.ReceiveRedirectionResponse(Response: TIdSipResponse;
                                                 UsingSecureTransport: Boolean): Boolean;
var
  ReInvite: TIdSipRequest;
begin
  Result := false;
  if Response.HasHeader(ContactHeaderFull) then begin
    ReInvite := TIdSipRequest.Create;
    try
      ReInvite.Assign(Self.InitialRequest);
      ReInvite.LastHop.Branch := Self.UA.NextBranch;
      ReInvite.FirstContact.Assign(Response.FirstContact);

      Self.UA.Dispatcher.AddClientTransaction(ReInvite);
      Self.UA.Dispatcher.SendRequest(ReInvite);
    finally
      ReInvite.Free;
    end;
    Result := true;
  end;
end;

function TIdSipAction.ReceiveServerFailureResponse(Response: TIdSipResponse): Boolean;
begin
  Result := false;
end;

//******************************************************************************
//* TIdSipSessionTimer                                                         *
//******************************************************************************
//* TIdSipSessionTimer Public methods ******************************************

constructor TIdSipSessionTimer.Create(Session: TIdSipInboundSession;
                                      T1: Cardinal;
                                      T2: Cardinal);
begin
  inherited Create;

  Self.Lock := TCriticalSection.Create;

  Self.Session := Session;
  Self.T1      := T1;
  Self.T2      := T2;

  Self.Timer := TIdSipTimer.Create;
  Self.Timer.Interval := Self.T1;
  Self.Timer.OnTimer  := Self.OnTimer;
end;

destructor TIdSipSessionTimer.Destroy;
begin
  Self.Timer.TerminateAndWaitFor;
  Self.Timer.Free;

  Self.Lock.Free;

  inherited Destroy;
end;

procedure TIdSipSessionTimer.Fire;
begin
  Self.Lock.Acquire;
  try
    Self.Timer.Interval := Min(2*Self.Timer.Interval, Self.T2);
  finally
    Self.Lock.Release;
  end;

  Self.Session.ResendLastResponse;
end;

procedure TIdSipSessionTimer.Start;
begin
  Self.Timer.Start;
end;

procedure TIdSipSessionTimer.Stop;
begin
  Self.Timer.Stop;
end;

function TIdSipSessionTimer.Interval: Cardinal;
begin
  Self.Lock.Acquire;
  try
    Result := Self.Timer.Interval;
  finally
    Self.Lock.Release;
  end;
end;

//* TIdSipSessionTimer Private methods *****************************************

procedure TIdSipSessionTimer.OnTimer(Sender: TObject);
begin
  Self.Fire;
end;

//******************************************************************************
//* TIdSipSession                                                              *
//******************************************************************************
//* TIdSipSession Public methods ***********************************************

constructor TIdSipSession.Create(UA: TIdSipUserAgentCore);
begin
  inherited Create(UA);

  Self.DialogLock := TCriticalSection.Create;

  Self.fIsTerminated := false;
  Self.fReceivedAck  := false;

  Self.fPayloadProcessor := TIdSdpPayloadProcessor.Create;
  Self.PayloadProcessor.Host     := Self.UA.HostName;
  Self.PayloadProcessor.Username := Self.UA.Username;

  Self.OpenTransactionLock := TCriticalSection.Create;
  Self.OpenTransactions    := TObjectList.Create(true);

  Self.SessionListenerLock := TCriticalSection.Create;
  Self.SessionListeners    := TList.Create;

  Self.FullyEstablished := false;
end;

destructor TIdSipSession.Destroy;
begin
  Self.SessionListeners.Free;
  Self.SessionListenerLock.Free;

  Self.OpenTransactions.Free;
  Self.OpenTransactionLock.Free;

  Self.PayloadProcessor.Free;

  Self.DialogLock.Free;

  inherited Destroy;
end;

procedure TIdSipSession.AddSessionListener(const Listener: IIdSipSessionListener);
begin
  Self.SessionListenerLock.Acquire;
  try
    Self.SessionListeners.Add(Pointer(Listener));
  finally
    Self.SessionListenerLock.Release;
  end;
end;

function TIdSipSession.IsEarly: Boolean;
begin
  // This relies on short-circuited boolean expression evaluation
  Result := not Self.DialogEstablished or Self.Dialog.IsEarly;
end;

function TIdSipSession.DialogEstablished: Boolean;
begin
  Self.DialogLock.Acquire;
  try
    Result := Assigned(Self.fDialog);
  finally
    Self.DialogLock.Release;
  end;
end;

function TIdSipSession.IsOutboundCall: Boolean;
begin
  Result := not Self.IsInboundCall;
end;

function TIdSipSession.IsSession: Boolean;
begin
  Result := true;
end;

function TIdSipSession.Match(Msg: TIdSipMessage): Boolean;
var
  DialogID: TIdSipDialogID;
begin
  DialogID := TIdSipDialogID.Create(Msg.CallID,
                                    Msg.ToHeader.Tag,
                                    Msg.From.Tag);
  try
   Result := (Self.DialogEstablished and Self.Dialog.ID.Equals(DialogID))
          or Self.Invite.Match(Msg);
  finally
    DialogID.Free;
  end;
end;

procedure TIdSipSession.Modify(const Offer, ContentType: String);
begin
end;

function TIdSipSession.PendingTransactionCount: Integer;
begin
  Self.OpenTransactionLock.Acquire;
  try
    Result := Self.OpenTransactions.Count;
  finally
    Self.OpenTransactionLock.Release;
  end;
end;

procedure TIdSipSession.ReceiveRequest(Request: TIdSipRequest);
begin
  if Self.IsTerminated then begin
    Self.RejectRequest(Request);
    Exit;
  end;

  if Request.IsBye then begin
    Self.ProcessBye(Request);
  end
  else if Request.IsInvite then begin
    if Self.Dialog.IsOutOfOrder(Request) then begin
      Self.RejectOutOfOrderRequest(Request);
      Exit;
    end;

//    if Request.Disposition.IsSession then
//      Self.PayloadProcessor.RemoteSessionDescription := Request.Body;

    Self.AddOpenTransaction(Request);
    Self.NotifyOfModifiedSession(Request);
  end;
end;

procedure TIdSipSession.RemoveSessionListener(const Listener: IIdSipSessionListener);
begin
  Self.SessionListenerLock.Acquire;
  try
    Self.SessionListeners.Remove(Pointer(Listener));
  finally
    Self.SessionListenerLock.Release;
  end;
end;

//* TIdSipSession Protected methods ********************************************

procedure TIdSipSession.ActionSucceeded(Response: TIdSipResponse);
begin
  if Self.DialogEstablished then
    Self.Dialog.HandleMessage(Response);
end;

procedure TIdSipSession.AddOpenTransaction(Request: TIdSipRequest);
var
  NewRequest: TIdSipRequest;
begin
  Self.OpenTransactionLock.Acquire;
  try
    NewRequest := TIdSipRequest.Create;
    NewRequest.Assign(Request);
    Self.OpenTransactions.Add(NewRequest);
  finally
    Self.OpenTransactionLock.Release;
  end;
end;

function TIdSipSession.GetInvite: TIdSipRequest;
begin
  Result := Self.InitialRequest;
end;

procedure TIdSipSession.MarkAsTerminated;
begin
  inherited MarkAsTerminated;

  Self.ApplyTo(Self.OpenTransactions,
               Self.OpenTransactionLock,
               Self.MarkAsTerminatedProc);
end;

procedure TIdSipSession.NotifyOfEndedSession(const Reason: String);
var
  Copy: TList;
  I:    Integer;
begin
  Copy := TList.Create;
  try
    Self.CopyList(Self.SessionListeners, Self.SessionListenerLock, Copy);

    for I := 0 to Copy.Count - 1 do
        IIdSipSessionListener(Copy[I]).OnEndedSession(Self, Reason);
  finally
    Copy.Free;
  end;

  Self.UA.RemoveAction(Self);
end;

procedure TIdSipSession.NotifyOfEstablishedSession;
var
  Copy: TList;
  I:    Integer;
begin
  Copy := TList.Create;
  try
    Self.CopyList(Self.SessionListeners, Self.SessionListenerLock, Copy);

    for I := 0 to Copy.Count - 1 do
      IIdSipSessionListener(Copy[I]).OnEstablishedSession(Self);
  finally
    Copy.Free;
  end;
end;

procedure TIdSipSession.NotifyOfFailure(Response: TIdSipResponse);
begin
  Self.MarkAsTerminated;
  Self.NotifyOfEndedSession(Response.Description);
end;

procedure TIdSipSession.SendBye;
var
  Bye: TIdSipRequest;
begin
  Bye := Self.UA.CreateBye(Self.Dialog);
  try
    // We don't listen to the new transaction because we assume the BYE
    // succeeds immediately.
    Self.UA.Dispatcher.AddClientTransaction(Bye).SendRequest;
  finally
    Bye.Free;
  end;
end;

//* TIdSipSession Private methods **********************************************

procedure TIdSipSession.MarkAsTerminatedProc(ObjectOrIntf: Pointer);
begin
  Self.TerminateOpenTransaction(TIdSipRequest(ObjectOrIntf));
end;

procedure TIdSipSession.NotifyOfModifiedSession(Invite: TIdSipRequest);
var
  Copy: TList;
  I:    Integer;
begin
  Copy := TList.Create;
  try
    Self.CopyList(Self.SessionListeners, Self.SessionListenerLock, Copy);

    for I := 0 to Copy.Count - 1 do
        IIdSipSessionListener(Copy[I]).OnModifiedSession(Self, Invite);
  finally
    Copy.Free;
  end;
end;

procedure TIdSipSession.ProcessBye(Request: TIdSipRequest);
var
  OK: TIdSipResponse;
begin
  Self.MarkAsTerminated;
  Self.Dialog.HandleMessage(Request);

  OK := Self.UA.CreateResponse(Request, SIPOK);
  try
    Self.UA.Dispatcher.SendResponse(OK);
  finally
    OK.Free;
  end;
  Self.NotifyOfEndedSession(RemoteHangUp);
end;

procedure TIdSipSession.RejectOutOfOrderRequest(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.UA.CreateResponse(Request,
                                     SIPInternalServerError);
  try
    Response.StatusText := RSSIPRequestOutOfOrder;
    Self.UA.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipSession.RejectRequest(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.UA.CreateResponse(Request,
                                     SIPRequestTerminated);
  try
    Self.UA.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipSession.TerminateOpenTransaction(Request: TIdSipRequest);
begin
  Self.RejectRequest(Request);
end;

//******************************************************************************
//* TIdSipInboundSession                                                       *
//******************************************************************************
//* TIdSipInboundSession Public methods ****************************************

constructor TIdSipInboundSession.Create(UA: TIdSipUserAgentCore;
                                        Invite: TIdSipRequest;
                                        UsingSecureTransport: Boolean);
begin
  inherited Create(UA);

  Self.InitialRequest.Assign(Invite);

  Self.UsingSecureTransport := UsingSecureTransport;

  Self.OkTimer := TIdSipSessionTimer.Create(Self, DefaultT1, DefaultT2);

  Self.PayloadProcessor.RemoteSessionDescription := Invite.Body;
end;

destructor TIdSipInboundSession.Destroy;
begin
  Self.OkTimer.Free;

  Self.LastResponse.Free;

  inherited Destroy;
end;

function TIdSipInboundSession.AcceptCall(const Offer, ContentType: String): String;
var
  OkResponse: TIdSipResponse;
begin
  // Offer contains a description of what data we expect to receive. Sometimes
  // we cannot meet this offer (e.g., the offer says "receive on port 8000" but
  // port 8000's already bound. We thus try to honour the offer as closely as
  // possible, and return the _actual_ offer sent.

  // The type of payload processor depends on the ContentType passed in!
  Self.PayloadProcessor.StartListening(Offer);

  OkResponse := Self.UA.CreateResponse(Self.InitialRequest, SIPOK);
  try
    Result        := Self.PayloadProcessor.LocalSessionDescription;
    OkResponse.Body := Result;

    OkResponse.ContentLength := Length(OkResponse.Body);
    OkResponse.ContentType   := ContentType;
    OkResponse.ToHeader.Tag  := Self.UA.NextTag;

    Self.DialogLock.Acquire;
    try
      if not Self.DialogEstablished then begin
        fDialog := Self.CreateInboundDialog(OkResponse);
        Self.NotifyOfEstablishedSession;
      end;

      Self.Dialog.HandleMessage(Self.InitialRequest);
      Self.Dialog.HandleMessage(OkResponse);
    finally
      Self.DialogLock.Release;
    end;
    Self.UA.Dispatcher.SendResponse(OkResponse);
    Self.OkTimer.Start;
  finally
    OkResponse.Free;
  end;

  Self.FullyEstablished := true;
end;

procedure TIdSipInboundSession.ForwardCall(NewDestination: TIdSipAddressHeader);
var
  RedirectResponse: TIdSipResponse;
begin
  RedirectResponse := TIdSipResponse.InResponseTo(Self.InitialRequest,
                                                  SIPMovedTemporarily);
  try
    RedirectResponse.AddHeader(ContactHeaderFull).Value := NewDestination.FullValue;
    Self.UA.Dispatcher.SendResponse(RedirectResponse);
  finally
    RedirectResponse.Free;
  end;

  Self.NotifyOfEndedSession(CallForwarded);
end;

function TIdSipInboundSession.IsInboundCall: Boolean;
begin
  Result := true;
end;

procedure TIdSipInboundSession.ReceiveRequest(Request: TIdSipRequest);
begin
  if Request.IsAck then begin
    Self.fReceivedAck := true;

    if Assigned(Self.OkTimer) then
      Self.OkTimer.Stop;
  end
  else
    inherited ReceiveRequest(Request);
end;

procedure TIdSipInboundSession.RejectCallBusy;
var
  BusyHereResponse: TIdSipResponse;
begin
  BusyHereResponse := TIdSipResponse.InResponseTo(Self.InitialRequest,
                                                  SIPBusyHere);
  try
    Self.UA.Dispatcher.SendResponse(BusyHereResponse);
  finally
    BusyHereResponse.Free;
  end;

  Self.NotifyOfEndedSession(BusyHere);
end;

procedure TIdSipInboundSession.ResendLastResponse;
begin
  if Assigned(Self.LastResponse) then
    Self.UA.Dispatcher.Send(Self.LastResponse);
end;

procedure TIdSipInboundSession.Terminate;
begin
  inherited Terminate;

  if Self.FullyEstablished then
    Self.SendBye
  else
    Self.TerminatePendingInvite;

  Self.NotifyOfEndedSession(LocalHangUp);
end;

procedure TIdSipInboundSession.TimeOut;
var
  Response: TIdSipResponse;
begin
  if not Self.FullyEstablished then begin
    Self.MarkAsTerminated;

    Response := Self.UA.CreateResponse(Self.InitialRequest,
                                       SIPRequestTerminated);
    try
      Self.UA.Dispatcher.SendResponse(Response);
    finally
      Response.Free;
    end;

    Self.NotifyOfEndedSession(InviteTimeout);
  end;
end;

//* TIdSipInboundSession Private methods ***************************************

function TIdSipInboundSession.CreateInboundDialog(Response: TIdSipResponse): TIdSipDialog;
begin
  Result := TIdSipDialog.CreateInboundDialog(Self.InitialRequest,
                                             Response,
                                             Self.UsingSecureTransport);

  Self.LastResponse := TIdSipResponse.Create;
  Self.LastResponse.Assign(Response);
end;

procedure TIdSipInboundSession.TerminatePendingInvite;
var
  TerminateResponse: TIdSipResponse;
begin
  TerminateResponse := Self.UA.CreateResponse(Self.InitialRequest,
                                              SIPBusyHere);
  try
    Self.UA.Dispatcher.Send(TerminateResponse);
  finally
    TerminateResponse.Free;
  end;
end;

//******************************************************************************
//* TIdSipOutboundSession                                                      *
//******************************************************************************
//* TIdSipOutboundSession Public methods ***************************************

constructor TIdSipOutboundSession.Create(UA: TIdSipUserAgentCore);
begin
  inherited Create(UA);

  Self.Initialize;
end;

constructor TIdSipOutboundSession.Create(UA: TIdSipUserAgentCore;
                                         Invite: TIdSipRequest);
begin
  inherited Create(UA);

  Self.Initialize;

  Self.Invite.Assign(Invite);
  Self.Invite.ToHeader.Tag := '';
end;

procedure TIdSipOutboundSession.Call(Dest: TIdSipAddressHeader;
                                     const InitialOffer: String;
                                     const MimeType: String);
var
  Invite: TIdSipRequest;
begin
  if not Self.InCall then begin
    Self.InCall := true;

    Invite := Self.UA.CreateInvite(Dest, InitialOffer, MimeType);
    try
      Self.InitialRequest.Assign(Invite);
      Self.UA.Dispatcher.AddClientTransaction(Invite);
      Self.UA.Dispatcher.SendRequest(Self.InitialRequest);
    finally
      Invite.Free;
    end;
  end;
end;

procedure TIdSipOutboundSession.Cancel;
begin
  if Self.FullyEstablished then Exit;

  Self.SendCancel;
  Self.TerminateAfterSendingCancel;
end;

function TIdSipOutboundSession.CanForkOn(Response: TIdSipResponse): Boolean;
begin
  Result := (Self.InitialRequest.CallID = Response.CallID)
        and (Self.InitialRequest.From.Tag = Response.From.Tag);
end;

function TIdSipOutboundSession.Fork(OkResponse: TIdSipResponse): TIdSipOutboundSession;
begin
  Result := TIdSipOutboundSession.Create(Self.UA, Invite);
end;

function TIdSipOutboundSession.IsInboundCall: Boolean;
begin
  Result := false;
end;

procedure TIdSipOutboundSession.Terminate;
begin
  // The contorted logic below breaks down like this:
  // If we've established a session, things work as expected and we send a BYE
  // and commit suicide via NotifyOfEndedSession.
  // If we send an INVITE we MUST NOT send a CANCEL until we've received at
  // least one response from the remote end. That means that while we have
  // started terminating, we have not finished, and cannot until we've
  // received a response.
  if Self.FullyEstablished then begin
    Self.MarkAsTerminated;
    Self.SendBye;
    Self.NotifyOfEndedSession(LocalHangUp);
  end
  else
    Self.Cancel;
end;

//* TIdSipOutboundSession Protected methods ************************************

function TIdSipOutboundSession.ReceiveOKResponse(Response: TIdSipResponse;
                                                 UsingSecureTransport: Boolean): Boolean;
begin
  // REMEMBER: A 2xx response to an INVITE DOES NOT take place in a transaction!
  // A 2xx response immediately terminates a client INVITE transaction so that
  // the ACK can get passed up to the UA (as an unhandled request).

  Self.DialogLock.Acquire;
  try
    if not Self.DialogEstablished then begin
      fDialog := Self.CreateOutboundDialog(Response, UsingSecureTransport);
      Self.PayloadProcessor.RemoteSessionDescription := Response.Body;
      Self.NotifyOfEstablishedSession;
    end;
  finally
    Self.DialogLock.Release;
  end;

  Self.SendAck(Response);

  Result := true;
  Self.FullyEstablished := true;
end;

function TIdSipOutboundSession.ReceiveProvisionalResponse(Response: TIdSipResponse;
                                                          UsingSecureTransport: Boolean): Boolean;
begin
  // We should check for "and Response.ToHeader.HasTag" but that would prevent
  // us connecting to X-Lite, the non-compliant SIP phone.
  if not Response.IsTrying and Response.ToHeader.HasTag then begin
    Self.DialogLock.Acquire;
    try
      if not Self.DialogEstablished then begin
        fDialog := Self.CreateOutboundDialog(Response, UsingSecureTransport);
        Self.NotifyOfEstablishedSession;
      end;
    finally
      Self.DialogLock.Release;
    end;
  end;

  Result := true;
end;

function TIdSipOutboundSession.ReceiveRedirectionResponse(Response: TIdSipResponse;
                                                          UsingSecureTransport: Boolean): Boolean;
begin
  Result := false;
  Self.SendAck(Response);

  if Response.HasHeader(ContactHeaderFull) then begin
    Self.UA.Call(Response.FirstContact,
                 Self.InitialRequest.Body,
                 Self.InitialRequest.ContentType);
    Result := true;
  end;
end;

procedure TIdSipOutboundSession.SendBye;
begin
  inherited SendBye;

  Self.MarkAsTerminated;
//  Self.NotifyOfEndedSession(LocalHangUp);
end;

//* TIdSipOutboundSession Private methods **************************************

function TIdSipOutboundSession.CreateOutboundDialog(Response: TIdSipResponse;
                                                    UsingSecureTransport: Boolean): TIdSipDialog;
begin
  Self.UsingSecureTransport := UsingSecureTransport;

  Result := TIdSipDialog.CreateOutboundDialog(Self.InitialRequest,
                                              Response,
                                              Self.UsingSecureTransport);

  Self.NotifyOfEstablishedSession;
end;

procedure TIdSipOutboundSession.Initialize;
begin
  Self.InCall := false;
end;

procedure TIdSipOutboundSession.SendAck(Final: TIdSipResponse);
var
  Ack: TIdSipRequest;
begin
  Ack := Self.InitialRequest.AckFor(Final);
  try
    Self.UA.Dispatcher.Send(Ack);
  finally
    Ack.Free;
  end;
end;

procedure TIdSipOutboundSession.SendCancel;
var
  Cancel: TIdSipRequest;
begin
  Cancel := Self.InitialRequest.CreateCancel;
  try
    Self.UA.Dispatcher.Send(Cancel);
  finally
    Cancel.Free;
  end;
end;

procedure TIdSipOutboundSession.TerminateAfterSendingCancel;
begin
  Self.MarkAsTerminated;
  Self.NotifyOfEndedSession(LocalCancel);
end;  

//******************************************************************************
//* TIdSipRegistration                                                         *
//******************************************************************************
//* TIdSipRegistration Public methods ******************************************

constructor TIdSipRegistration.Create(UA: TIdSipUserAgentCore);
begin
  inherited Create(UA);

  Self.Bindings     := TIdSipContacts.Create;
  Self.ListenerLock := TCriticalSection.Create;
  Self.Listeners    := TList.Create;
end;

destructor TIdSipRegistration.Destroy;
begin
  Self.Listeners.Free;
  Self.ListenerLock.Free;
  Self.Bindings.Free;

  inherited Destroy;
end;

procedure TIdSipRegistration.AddListener(const Listener: IIdSipRegistrationListener);
begin
  Self.ListenerLock.Acquire;
  try
    Self.Listeners.Add(Pointer(Listener));
  finally
    Self.ListenerLock.Release;
  end;
end;

procedure TIdSipRegistration.FindCurrentBindings(Registrar: TIdSipUri);
var
  BlankBindings: TIdSipContacts;
begin
  BlankBindings := TIdSipContacts.Create;
  try
    Self.RegisterWith(Registrar, BlankBindings);
  finally
    BlankBindings.Free;
  end;
end;

function TIdSipRegistration.IsRegistration: Boolean;
begin
  Result := true;
end;

function TIdSipRegistration.Match(Msg: TIdSipMessage): Boolean;
begin
  Result := Self.InitialRequest.Match(Msg);
end;

procedure TIdSipRegistration.RegisterWith(Registrar: TIdSipUri; Bindings: TIdSipContacts);
var
  Request: TIdSipRequest;
begin
  Request := Self.CreateRegister(Registrar, Bindings);
  try
    Self.Bindings.Clear;
    Self.Bindings.Add(Bindings);

    Self.Send(Request);
  finally
    Request.Free;
  end;
end;

procedure TIdSipRegistration.RegisterWith(Registrar: TIdSipUri; Contact: TIdSipContactHeader);
var
  Binding: TIdSipContacts;
begin
  Binding := TIdSipContacts.Create;
  try
    Binding.Add(Contact);

    Self.RegisterWith(Registrar, Binding);
  finally
    Binding.Free;
  end;
end;

procedure TIdSipRegistration.RemoveListener(const Listener: IIdSipRegistrationListener);
begin
  Self.ListenerLock.Acquire;
  try
    Self.Listeners.Remove(Pointer(Listener));
  finally
    Self.ListenerLock.Release;
  end;
end;

procedure TIdSipRegistration.Terminate;
begin
  Self.UA.RemoveAction(Self);
end;

procedure TIdSipRegistration.Unregister(Registrar: TIdSipUri);
var
  RemovalBindings: TIdSipContacts;
  Request:         TIdSipRequest;
begin
  RemovalBindings := TIdSipContacts.Create;
  try
    Self.Bindings.Clear;
    Self.Bindings.Add(ContactHeaderFull);
    Self.Bindings.First;
    Self.Bindings.CurrentContact.IsWildCard := true;
//    Self.Bindings.CurrentContact.Expires := 0;

    Request := Self.CreateRegister(Registrar, Bindings);
    try
      Request.FirstExpires.NumericValue := 0;

      Self.Send(Request);
    finally
      Request.Free;
    end;
  finally
    RemovalBindings.Free;
  end;
end;

//* TIdSipRegistration Protected methods ***************************************

procedure TIdSipRegistration.ActionSucceeded(Response: TIdSipResponse);
begin
  Self.NotifyOfSuccess(Response);
end;

procedure TIdSipRegistration.NotifyOfFailure(Response: TIdSipResponse);
var
  Copy:            TList;
  CurrentBindings: TIdSipContacts;
  I:               Integer;
begin
  CurrentBindings := TIdSipContacts.Create(Response.Headers);
  try
    Copy := TList.Create;
    try
      Self.CopyList(Self.Listeners, Self.ListenerLock, Copy);

      for I := 0 to Copy.Count - 1 do
        IIdSipRegistrationListener(Copy[I]).OnFailure(Self,
                                                      CurrentBindings,
                                                      Response.Description);
    finally
      Copy.Free;
    end;
  finally
    CurrentBindings.Free;
  end;

  Self.Terminate;
end;

function TIdSipRegistration.ReceiveFailureResponse(Response: TIdSipResponse): Boolean;
begin
  Result := true;
  case Response.StatusCode of
    SIPUnauthorized,
    SIPProxyAuthenticationRequired:
      Self.NotifyOfAuthenticationChallenge(Response);
    SIPIntervalTooBrief: Self.ReissueRequest(Self.InitialRequest.RequestUri,
                                             Response.FirstMinExpires.NumericValue);
    SIPBadExtension: begin
      Result := Self.InitialRequest.HasHeader(RequireHeader);
      if Result then
        Self.RetryWithoutExtensions(Self.InitialRequest.RequestUri,
                                    Response);
    end;
  else
    Result := false;
  end;
end;

function TIdSipRegistration.ReceiveRedirectionResponse(Response: TIdSipResponse;
                                                       UsingSecureTransport: Boolean): Boolean;
begin
  Result := false;

  if Response.HasHeader(ContactHeaderFull) then begin
    Self.UA.RegisterWith(Response.FirstContact.Address);

    Result := true;
  end;
end;

//* TIdSipRegistration Private methods *****************************************

function TIdSipRegistration.CreateRegister(Registrar: TIdSipUri;
                                           Bindings: TIdSipContacts): TIdSipRequest;
var
  OldContacts: TIdSipContacts;
  ToHeader: TIdSipToHeader;
begin
  ToHeader := TIdSipToHeader.Create;
  try
    ToHeader.Address := Registrar;

    Result := Self.UA.CreateRegister(ToHeader);

    // Bindings explicitly carries all Contact information. Thus we must remove
    // any Contact information already in Result.
    OldContacts := TIdSipContacts.Create(Result.Headers);
    try
      OldContacts.Clear;
    finally
      OldContacts.Free;
    end;

    Result.AddHeaders(Bindings);
  finally
    ToHeader.Free;
  end;
end;

procedure TIdSipRegistration.NotifyOfAuthenticationChallenge(Response: TIdSipResponse);
var
  Copy: TList;
  I:    Integer;
begin
  Copy := TList.Create;
  try
    Self.CopyList(Self.Listeners, Self.ListenerLock, Copy);

    for I := 0 to Copy.Count - 1 do
      IIdSipRegistrationListener(Copy[I]).OnAuthenticationChallenge(Self, Response);
  finally
    Copy.Free;
  end;
end;

procedure TIdSipRegistration.NotifyOfSuccess(Response: TIdSipResponse);
var
  Copy:            TList;
  CurrentBindings: TIdSipContacts;
  I:               Integer;
begin
  CurrentBindings := TIdSipContacts.Create(Response.Headers);
  try
    Copy := TList.Create;
    try
      Self.CopyList(Self.Listeners, Self.ListenerLock, Copy);

      for I := 0 to Copy.Count - 1 do
        IIdSipRegistrationListener(Copy[I]).OnSuccess(Self, CurrentBindings);
    finally
      Copy.Free;
    end;
  finally
    CurrentBindings.Free;
  end;

  Self.Terminate;
end;

procedure TIdSipRegistration.ReissueRequest(Registrar: TIdSipUri;
                                            MinimumExpiry: Cardinal);
var
  Bindings: TIdSipContacts;
  Request: TIdSipRequest;
begin
  // We received a 423 Interval Too Brief from the registrar. Therefore we
  // make a new REGISTER request with the registrar's minimum expiry.
  Request := Self.CreateRegister(Registrar, Self.Bindings);
  try
    Bindings := TIdSipContacts.Create(Request.Headers);
    try
      Bindings.First;
      while Bindings.HasNext do begin
        if Bindings.CurrentContact.WillExpire then
          Bindings.CurrentContact.Expires := Max(Bindings.CurrentContact.Expires,
                                                     MinimumExpiry);
        Bindings.Next;
      end;
    finally
      Bindings.Free;
    end;

    Request.FirstExpires.NumericValue := MinimumExpiry;
    Self.Send(Request);
  finally
    Request.Free;
  end;
end;

procedure TIdSipRegistration.RetryWithoutExtensions(Registrar: TIdSipUri;
                                                    Response: TIdSipResponse);
var
  Bindings: TIdSipContacts;
  Request: TIdSipRequest;
begin
  Bindings := TIdSipContacts.Create;
  try
    Bindings.Add(Self.UA.Contact);

    Request := Self.CreateRegister(Registrar, Bindings);
    try
      if not Response.HasHeader(UnsupportedHeader) then begin
        // A 420 Bad Extension MUST have an unsupported header. In the
        // interests of accepting liberally though, we just drop all
        // Requires.
        Request.RemoveAllHeadersNamed(RequireHeader);
      end
      else
        Request.FirstRequire.RemoveValues(Response.FirstUnsupported);

      Self.Send(Request);
    finally
      Request.Free;
    end;
  finally
    Bindings.Free;
  end;
end;

procedure TIdSipRegistration.Send(Request: TIdSipRequest);
begin
  Self.InitialRequest.Assign(Request);

  Self.UA.Dispatcher.AddClientTransaction(Request);
  Self.UA.Dispatcher.SendRequest(Request);
end;

end.
