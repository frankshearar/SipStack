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
                                        Transaction: TIdSipTransaction;
                                        Receiver: TIdSipTransport); overload;
    procedure OnReceiveUnhandledResponse(Response: TIdSipResponse;
                                         Transaction: TIdSipTransaction;
                                         Receiver: TIdSipTransport); overload;
    procedure SetDispatcher(Value: TIdSipTransactionDispatcher);
  protected
    procedure ActOnRequest(Request: TIdSipRequest;
                           Transaction: TIdSipTransaction;
                           Receiver: TIdSipTransport); virtual;
    procedure ActOnResponse(Response: TIdSipResponse;
                           Transaction: TIdSipTransaction;
                           Receiver: TIdSipTransport); virtual;
    procedure RejectRequest(Reaction: TIdSipUserAgentReaction;
                            Request: TIdSipRequest;
                            Transaction: TIdSipTransaction); virtual;
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; virtual;
    function  WillAcceptResponse(Response: TIdSipResponse): TIdSipUserAgentReaction; virtual;
  public
    constructor Create; virtual;

    function  CreateRequest(Dest: TIdSipToHeader): TIdSipRequest; overload; virtual; abstract;
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
    procedure RejectRequestMethodNotAllowed(Request: TIdSipRequest;
                                            Transaction: TIdSipTransaction);
    procedure RejectRequestUnknownContentEncoding(Request: TIdSipRequest;
                                                  Transaction: TIdSipTransaction);
    procedure RejectRequestUnknownContentLanguage(Request: TIdSipRequest;
                                                  Transaction: TIdSipTransaction);
    procedure RejectRequestUnknownContentType(Request: TIdSipRequest;
                                              Transaction: TIdSipTransaction);
    procedure RejectUnsupportedSipVersion(Request: TIdSipRequest;
                                          Transaction: TIdSipTransaction);
    procedure SetFrom(Value: TIdSipFromHeader);
  protected
    procedure PrepareResponse(Response: TIdSipResponse;
                              Request: TIdSipRequest);
    procedure RejectRequest(Reaction: TIdSipUserAgentReaction;
                            Request: TIdSipRequest;
                            Transaction: TIdSipTransaction); override;
    procedure RejectRequestBadExtension(Request: TIdSipRequest;
                                        Transaction: TIdSipTransaction);
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
    function  AllowedLanguages: String;
    function  AllowedMethods: String;
    function  AllowedSchemes: String;
    function  CreateRequest(Dest: TIdSipToHeader): TIdSipRequest; overload; override;
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
                             Reason: Cardinal;
                             Transaction: TIdSipTransaction);

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
    fContact:                 TIdSipContactHeader;
    fHasProxy:                Boolean;
    fProxy:                   TIdSipUri;
    KnownRegistrars:          TObjectList;
    ObserverLock:             TCriticalSection;
    Observers:                TList;
    RegistrationListenerLock: TCriticalSection;
    RegistrationListeners:    TList;
    RegistrationLock:         TCriticalSection;
    Registrations:            TObjectList;
    SessionLock:              TCriticalSection;
    Sessions:                 TObjectList;
    UserAgentListenerLock:    TCriticalSection;
    UserAgentListeners:       TList;

    function  AddInboundSession(Invite: TIdSipRequest;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport): TIdSipInboundSession;
    procedure AddKnownRegistrar(Registrar: TIdSipUri;
                                const CallID: String;
                                SequenceNo: Cardinal);
    function  AddOutboundSession: TIdSipOutboundSession;
    function  AddRegistration: TIdSipRegistration;
    function  CallIDFor(Registrar: TIdSipUri): String;
    function  DefaultFrom: String;
    function  DefaultHostName: String;
    function  DefaultUserAgent: String;
    function  FindSession(const Msg: TIdSipMessage): TIdSipSession;
    function  GetContact: TIdSipContactHeader;
    function  IndexOfRegistrar(Registrar: TIdSipUri): Integer;
    function  KnowsRegistrar(Registrar: TIdSipUri): Boolean;
    function  NextSequenceNoFor(Registrar: TIdSipUri): Cardinal;
    procedure NotifyOfInboundCall(Session: TIdSipInboundSession);
    procedure NotifyOfChange;
    procedure NotifyOfDroppedResponse(Response: TIdSipResponse;
                                      Receiver: TIdSipTransport);
    procedure ProcessAck(Ack: TIdSipRequest;
                         Transaction: TIdSipTransaction;
                         Receiver: TIdSipTransport);
    procedure ProcessInvite(Invite: TIdSipRequest;
                            Transaction: TIdSipTransaction;
                            Receiver: TIdSipTransport);
    function  RegistrarAt(Index: Integer): TIdSipRegistrationInfo;
    procedure RejectBadRequest(Request: TIdSipRequest;
                               const Reason: String;
                               Transaction: TIdSipTransaction);
    procedure SendByeToAppropriateSession(Bye: TIdSipRequest;
                                          Transaction: TIdSipTransaction;
                                          Receiver: TIdSipTransport);
    procedure SetContact(Value: TIdSipContactHeader);
    procedure SetProxy(Value: TIdSipUri);
  protected
    procedure ActOnRequest(Request: TIdSipRequest;
                           Transaction: TIdSipTransaction;
                           Receiver: TIdSipTransport); override;
    procedure ActOnResponse(Response: TIdSipResponse;
                           Transaction: TIdSipTransaction;
                           Receiver: TIdSipTransport); override;
    procedure RejectRequest(Reaction: TIdSipUserAgentReaction;
                            Request: TIdSipRequest;
                            Transaction: TIdSipTransaction); override;
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddObserver(const Listener: IIdSipObserver);
    procedure AddUserAgentListener(const Listener: IIdSipUserAgentListener);
    function  Call(Dest: TIdSipToHeader;
                   const InitialOffer: String;
                   const MimeType: String): TIdSipOutboundSession;
    function  CreateBye(Dialog: TIdSipDialog): TIdSipRequest;
    function  CreateInvite(Dest: TIdSipToHeader;
                           const Body: String;
                           const MimeType: String): TIdSipRequest;
    function  CreateRegister(Registrar: TIdSipToHeader): TIdSipRequest;
    function  CreateRequest(Dest: TIdSipToHeader): TIdSipRequest; overload; override;
    function  CreateRequest(Dialog: TIdSipDialog): TIdSipRequest; overload; override;
    function  CreateResponse(Request: TIdSipRequest;
                             ResponseCode: Cardinal): TIdSipResponse; override;
    function  CurrentRegistrationWith(Registrar: TIdSipUri): TIdSipRegistration;
    function  RegisterWith(Registrar: TIdSipUri): TIdSipRegistration;
    function  RegistrationCount: Integer;
    procedure RemoveObserver(const Listener: IIdSipObserver);
    procedure RemoveRegistration(Registration: TIdSipRegistration);
    procedure RemoveSession(Session: TIdSipSession);
    procedure RemoveUserAgentListener(const Listener: IIdSipUserAgentListener);
    function  SessionCount: Integer;
    procedure HangUpAllCalls;
    function  UnregisterFrom(Registrar: TIdSipUri): TIdSipRegistration;
    function  Username: String;

    property Contact:  TIdSipContactHeader read GetContact write SetContact;
    property HasProxy: Boolean             read fHasProxy write fHasProxy;
    property Proxy:    TIdSipUri           read fProxy write SetProxy;
  end;

  // I represent an asynchronous message send between SIP entities - INVITEs,
  // REGISTERs and the like - where we care what the remote end answers.
  // With CANCELs and BYEs, for instance, we don't care how the remote end
  // answers.
  TIdSipAction = class(TIdInterfacedObject,
                       IIdSipTransactionListener)
  private
    UA: TIdSipUserAgentCore;
  protected
    CurrentTransaction: TIdSipTransaction;

    function CurrentRequest: TIdSipRequest;

    procedure ActionSucceeded(Response: TIdSipResponse); virtual;
    procedure CopyList(Source: TList;
                       Lock: TCriticalSection;
                       Copy: TList);
    procedure NotifyOfFailure(Response: TIdSipResponse); virtual;
    procedure OnFail(Transaction: TIdSipTransaction;
                     const Reason: String); virtual;
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Receiver: TIdSipTransport); virtual;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport); virtual;
    procedure OnTerminated(Transaction: TIdSipTransaction); virtual;
    function  ReceiveFailureResponse(Response: TIdSipResponse;
                                     Transaction: TIdSipTransaction;
                                     Receiver: TIdSipTransport): Boolean; virtual;
    function  ReceiveGlobalFailureResponse(Response: TIdSipResponse;
                                           Transaction: TIdSipTransaction;
                                           Receiver: TIdSipTransport): Boolean; virtual;
    function  ReceiveOKResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport): Boolean; virtual;
    function  ReceiveProvisionalResponse(Response: TIdSipResponse;
                                         Transaction: TIdSipTransaction;
                                         Receiver: TIdSipTransport): Boolean; virtual;
    function  ReceiveRedirectionResponse(Response: TIdSipResponse;
                                         Transaction: TIdSipTransaction;
                                         Receiver: TIdSipTransport): Boolean; virtual;
    function  ReceiveServerFailureResponse(Response: TIdSipResponse;
                                           Transaction: TIdSipTransaction;
                                           Receiver: TIdSipTransport): Boolean; virtual;
  public
    constructor Create(UA: TIdSipUserAgentCore); virtual;
    destructor  Destroy; override;
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
    Session: TIdSipSession;
    Timer:   TIdSipTimer;

    procedure OnTimer(Sender: TObject);
  public
    constructor Create(Session: TIdSipSession;
                       T1: Cardinal;
                       T2: Cardinal);
    destructor  Destroy; override;

    procedure Fire;
    procedure Start;
    procedure Stop;

    function Interval: Cardinal;
  end;

  TIdSipProcedure = procedure(ObjectOrIntf: Pointer) of object;

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
    fDialog:             TIdSipDialog;
    fIsEstablished:      Boolean;
    fIsTerminated:       Boolean;
    fPayloadProcessor:   TIdSdpPayloadProcessor;
    fReceivedAck:        Boolean;
    InitialRequest:      TIdSipRequest;
    InitialTran:         TIdSipTransaction;
    InitialTransport:    TIdSipTransport;
    LastResponse:        TIdSipResponse;
    OkTimer:             TIdSipSessionTimer;
    OpenTransactionLock: TCriticalSection;
    OpenTransactions:    TList;
    SessionListenerLock: TCriticalSection;
    SessionListeners:    TList;

    procedure AddOpenTransaction(Transaction: TIdSipTransaction);
    procedure ApplyTo(List: TList;
                      Lock: TCriticalSection;
                      Proc: TIdSipProcedure);
    procedure MarkAsTerminated;
    procedure MarkAsTerminatedProc(ObjectOrIntf: Pointer);
    procedure NotifyOfEndedSession(const Reason: String);
    procedure NotifyOfEstablishedSession;
    procedure NotifyOfModifiedSession(Invite: TIdSipRequest);
    procedure RejectOutOfOrderRequest(Request: TIdSipRequest;
                                      Transaction: TIdSipTransaction);
    procedure RejectRequest(Request: TIdSipRequest;
                            Transaction: TIdSipTransaction);
    procedure RemoveTransaction(Transaction: TIdSipTransaction);
    procedure SendBye;
    procedure SendCancel;
    procedure TerminateOpenTransaction(Transaction: TIdSipTransaction);

    property IsEstablished: Boolean read fIsEstablished write fIsEstablished;
  protected
    procedure ActionSucceeded(Response: TIdSipResponse); override;
    function  GetInvite: TIdSipRequest; virtual;
    procedure NotifyOfFailure(Response: TIdSipResponse); override;
    procedure OnFail(Transaction: TIdSipTransaction;
                     const Reason: String); override;
    procedure OnTerminated(Transaction: TIdSipTransaction); override;
  public
    constructor Create(UA: TIdSipUserAgentCore); overload; override;
    destructor  Destroy; override;

    procedure AddSessionListener(const Listener: IIdSipSessionListener);
    procedure Cancel;
    function  DialogEstablished: Boolean;
    function  IsInboundCall: Boolean; virtual; abstract;
    procedure Terminate;
    procedure Modify(const Offer, ContentType: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Receiver: TIdSipTransport); override;
    procedure RemoveSessionListener(const Listener: IIdSipSessionListener);
    procedure ResendLastResponse; virtual;

    property Dialog:           TIdSipDialog           read fDialog;
    property Invite:           TIdSipRequest          read GetInvite;
    property IsTerminated:     Boolean                read fIsTerminated;
    property PayloadProcessor: TIdSdpPayloadProcessor read fPayloadProcessor;
    property ReceivedAck:      Boolean                read fReceivedAck;
  end;

  TIdSipInboundSession = class(TIdSipSession)
  private
    function  CreateInboundDialog(Response: TIdSipResponse): TIdSipDialog;
  public
    constructor Create(UA: TIdSipUserAgentCore;
                       Invite: TIdSipRequest;
                       InitialTransaction: TIdSipTransaction;
                       Receiver: TIdSipTransport); reintroduce;

    function AcceptCall(const Offer, ContentType: String): String;
    function IsInboundCall: Boolean; override;
  end;

  TIdSipOutboundSession = class(TIdSipSession)
  private
    function  CreateOutboundDialog(Response: TIdSipResponse;
                                   Transaction: TIdSipTransaction;
                                   Receiver: TIdSipTransport): TIdSipDialog;
    procedure SendAck(Final: TIdSipResponse);
  protected
    function ReceiveOKResponse(Response: TIdSipResponse;
                               Transaction: TIdSipTransaction;
                               Receiver: TIdSipTransport): Boolean; override;
    function ReceiveProvisionalResponse(Response: TIdSipResponse;
                                        Transaction: TIdSipTransaction;
                                        Receiver: TIdSipTransport): Boolean; override;
    function ReceiveRedirectionResponse(Response: TIdSipResponse;
                                        Transaction: TIdSipTransaction;
                                        Receiver: TIdSipTransport): Boolean; override;
  public
    procedure Call(Dest: TIdSipToHeader;
                   const InitialOffer: String;
                   const MimeType: String);
    function  IsInboundCall: Boolean; override;
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
    procedure Terminate;
  protected
    procedure ActionSucceeded(Response: TIdSipResponse); override;
    procedure NotifyOfFailure(Response: TIdSipResponse); override;
    function  ReceiveFailureResponse(Response: TIdSipResponse;
                                     Transaction: TIdSipTransaction;
                                     Receiver: TIdSipTransport): Boolean; override;
  public
    constructor Create(UA: TIdSipUserAgentCore); override;
    destructor  Destroy; override;

    procedure AddListener(const Listener: IIdSipRegistrationListener);
    procedure FindCurrentBindings(Registrar: TIdSipUri);
    procedure RegisterWith(Registrar: TIdSipUri; Bindings: TIdSipContacts); overload;
    procedure RegisterWith(Registrar: TIdSipUri; Contact: TIdSipContactHeader); overload;
    procedure RemoveListener(const Listener: IIdSipRegistrationListener);
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
  RemoteHangUp = 'Remote end hung up';

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
                                          Transaction: TIdSipTransaction;
                                          Receiver: TIdSipTransport);
begin
end;

procedure TIdSipAbstractCore.ActOnResponse(Response: TIdSipResponse;
                                           Transaction: TIdSipTransaction;
                                           Receiver: TIdSipTransport);
begin
end;

procedure TIdSipAbstractCore.RejectRequest(Reaction: TIdSipUserAgentReaction;
                                           Request: TIdSipRequest;
                                           Transaction: TIdSipTransaction);
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
                                                       Transaction: TIdSipTransaction;
                                                       Receiver: TIdSipTransport);
var
  Reaction: TIdSipUserAgentReaction;
begin
  Reaction := Self.WillAcceptRequest(Request);
  if (Reaction = uarAccept) then
    Self.ActOnRequest(Request, Transaction, Receiver)
  else
    Self.RejectRequest(Reaction, Request, Transaction);
end;

procedure TIdSipAbstractCore.OnReceiveUnhandledResponse(Response: TIdSipResponse;
                                                        Transaction: TIdSipTransaction;
                                                        Receiver: TIdSipTransport);
begin
  // We silently discard unacceptable responses.
  if (Self.WillAcceptResponse(Response) = uarAccept) then
    Self.ActOnResponse(Response, Transaction, Receiver);
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

  Self.fAllowedMethodList := TStringList.Create;
  Self.fAllowedSchemeList := TStringList.Create;
end;

destructor TIdSipAbstractUserAgent.Destroy;
begin
  Self.AllowedSchemeList.Free;
  Self.AllowedMethodList.Free;

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

function TIdSipAbstractUserAgent.CreateRequest(Dest: TIdSipToHeader): TIdSipRequest;
var
  Transport: String;
begin
  Result := TIdSipRequest.Create;
//  Result := Dest.Address.CreateRequest;
  try
    Result.RequestUri := Dest.Address;

    Result.CallID          := Self.NextCallID;
    Result.CSeq.SequenceNo := Self.NextInitialSequenceNo;
    Result.From            := Self.From;
    Result.From.Tag        := Self.NextTag;
    Result.MaxForwards     := Result.DefaultMaxForwards;
    Result.ToHeader        := Dest;

    // The transport must be discovered using RFC 3263
    // TODO: Lies. Pure hack to get X-Lite talking
    if (Pos(TransportParam, Dest.AsString) > 0) then
      Transport := TransportParamUDP // Todo: replace IdUri completely. It's just crap.
    else
      Transport := TransportParamTCP;

    Result.AddHeader(ViaHeaderFull).Value := SipVersion + '/' + Transport + ' localhost';
    Result.LastHop.Branch := Self.NextBranch;

    if (Self.UserAgentName <> '') then
      Result.AddHeader(UserAgentHeader).Value := Self.UserAgentName;
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
                                                 Reason: Cardinal;
                                                 Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, Reason);
  try
    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

//* TIdSipAbstractUserAgent Protected methods **********************************

procedure TIdSipAbstractUserAgent.PrepareResponse(Response: TIdSipResponse;
                                                  Request: TIdSipRequest);
begin
  if not Request.ToHeader.HasTag then
    Response.ToHeader.Tag := Self.NextTag;

  if (Self.UserAgentName <> '') then
    Response.AddHeader(ServerHeader).Value := Self.UserAgentName;
end;

procedure TIdSipAbstractUserAgent.RejectRequest(Reaction: TIdSipUserAgentReaction;
                                                Request: TIdSipRequest;
                                                Transaction: TIdSipTransaction);
begin
  inherited RejectRequest(Reaction, Request, Transaction);

  case Reaction of
    uarLoopDetected:
      Self.ReturnResponse(Request, SIPLoopDetected, Transaction);
    uarUnsupportedContentEncoding:
      Self.RejectRequestUnknownContentEncoding(Request, Transaction);
    uarUnsupportedContentLanguage:
      Self.RejectRequestUnknownContentLanguage(Request, Transaction);
    uarUnsupportedContentType:
      Self.RejectRequestUnknownContentType(Request, Transaction);
    uarUnsupportedExtension:
      Self.RejectRequestBadExtension(Request, Transaction);
    uarUnsupportedMethod:
      Self.RejectRequestMethodNotAllowed(Request, Transaction);
    uarUnsupportedScheme:
      Self.ReturnResponse(Request, SIPUnsupportedURIScheme, Transaction);
    uarUnSupportedSipVersion:
      Self.RejectUnsupportedSipVersion(Request, Transaction);
  end;
end;

procedure TIdSipAbstractUserAgent.RejectRequestBadExtension(Request: TIdSipRequest;
                                                            Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  // We simply reject ALL Requires
  Response := Self.CreateResponse(Request, SIPBadExtension);
  try
    Response.AddHeader(UnsupportedHeader).Value := Request.FirstHeader(RequireHeader).Value;

    Transaction.SendResponse(Response);
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

procedure TIdSipAbstractUserAgent.RejectRequestMethodNotAllowed(Request: TIdSipRequest;
                                                                Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPMethodNotAllowed);
  try
    Response.StatusText := Response.StatusText + ' (' + Request.Method + ')';
    Response.AddHeader(AllowHeader).Value := Self.AllowedMethods;

    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectRequestUnknownContentEncoding(Request: TIdSipRequest;
                                                                      Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptEncodingHeader).Value := '';

    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectRequestUnknownContentLanguage(Request: TIdSipRequest;
                                                                      Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptLanguageHeader).Value := Self.AllowedLanguages;

    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectRequestUnknownContentType(Request: TIdSipRequest;
                                                                  Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptHeader).Value := SdpMimeType;

    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectUnsupportedSipVersion(Request: TIdSipRequest;
                                                              Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPSIPVersionNotSupported);
  try
    Response.AddHeader(AcceptHeader).Value := SdpMimeType;

    Transaction.SendResponse(Response);
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

  Self.fProxy   := TIdSipUri.Create('');
  Self.HasProxy := false;

  Self.KnownRegistrars := TObjectList.Create(true);

  Self.ObserverLock             := TCriticalSection.Create;
  Self.Observers                := TList.Create;
  Self.RegistrationListenerLock := TCriticalSection.Create;
  Self.RegistrationListeners    := TList.Create;
  Self.RegistrationLock         := TCriticalSection.Create;
  Self.Registrations            := TObjectList.Create;
  Self.SessionLock              := TCriticalSection.Create;
  Self.Sessions                 := TObjectList.Create;
  Self.UserAgentListenerLock    := TCriticalSection.Create;
  Self.UserAgentListeners       := TList.Create;

  Self.fAllowedContentTypeList := TStringList.Create;
  Self.fAllowedLanguageList    := TStringList.Create;

  Self.AddAllowedContentType(SdpMimeType);
  Self.AddAllowedMethod(MethodBye);
  Self.AddAllowedMethod(MethodCancel);
  Self.AddAllowedMethod(MethodInvite);

  Self.AddAllowedScheme(SipScheme);

  Self.From.Value    := Self.DefaultFrom;
  Self.HostName      := Self.DefaultHostName;
  Self.UserAgentName := Self.DefaultUserAgent;
end;

destructor TIdSipUserAgentCore.Destroy;
begin
  Self.AllowedLanguageList.Free;
  Self.AllowedContentTypeList.Free;
  Self.Contact.Free;
  Self.From.Free;
  Self.UserAgentListeners.Free;
  Self.UserAgentListenerLock.Free;
  Self.Sessions.Free;
  Self.SessionLock.Free;
  Self.Registrations.Free;
  Self.RegistrationLock.Free;
  Self.RegistrationListeners.Free;
  Self.RegistrationListenerLock.Free;
  Self.Observers.Free;
  Self.ObserverLock.Free;
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

function TIdSipUserAgentCore.Call(Dest: TIdSipToHeader;
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

function TIdSipUserAgentCore.CreateInvite(Dest: TIdSipToHeader;
                                          const Body: String;
                                          const MimeType: String): TIdSipRequest;
begin
  Result := Self.CreateRequest(Dest);
  try
    Result.Method      := MethodInvite;
    Result.CSeq.Method := MethodInvite;

    Result.Body := Body;
    Result.ContentLength := Length(Body);

    if (Result.ContentLength > 0) then begin
      Result.ContentDisposition.Value := DispositionSession;
      Result.ContentType              := MimeType;
    end;
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

function TIdSipUserAgentCore.CreateRequest(Dest: TIdSipToHeader): TIdSipRequest;
begin
  Result := inherited CreateRequest(Dest);

  Result.AddHeader(Self.Contact);

  if Self.HasProxy then
    Result.Route.AddRoute(Self.Proxy);

  if Dest.HasSipsUri then
    Result.FirstContact.Address.Scheme := SipsScheme;
end;

function TIdSipUserAgentCore.CreateRequest(Dialog: TIdSipDialog): TIdSipRequest;
begin
  Result := Dialog.CreateRequest;
  try
    Result.AddHeader(ViaHeaderFull).Value := SipVersion + '/TCP localhost';
    Result.LastHop.Branch := Self.NextBranch;
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
begin
  // Return the number of ongoing registration attempts
  Self.RegistrationLock.Acquire;
  try
    Result := Self.Registrations.Count;
  finally
    Self.RegistrationLock.Release;
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

procedure TIdSipUserAgentCore.RemoveRegistration(Registration: TIdSipRegistration);
begin
  Self.RegistrationLock.Acquire;
  try
    Self.Registrations.Remove(Registration);
  finally
    Self.RegistrationLock.Release;
  end;
  Self.NotifyOfChange;
end;

procedure TIdSipUserAgentCore.RemoveSession(Session: TIdSipSession);
begin
  Self.SessionLock.Acquire;
  try
    Self.Sessions.Remove(Session);
  finally
    Self.SessionLock.Release;
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
begin
  Self.SessionLock.Acquire;
  try
    Result := Self.Sessions.Count;
  finally
    Self.SessionLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.HangUpAllCalls;
var
  CopyOfSessions: TObjectList;
  I:              Integer;
begin
  CopyOfSessions := TObjectList.Create(false);
  try
    Self.SessionLock.Acquire;
    try
      for I := 0 to Self.Sessions.Count - 1 do
        CopyOfSessions.Add(Self.Sessions[I]);
    finally
      Self.SessionLock.Release;
    end;

    for I := 0 to CopyOfSessions.Count - 1 do
      (CopyOfSessions[I] as TIdSipSession).Terminate;
  finally
    CopyOfSessions.Free;
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
                                           Transaction: TIdSipTransaction;
                                           Receiver: TIdSipTransport);
begin
  inherited ActOnRequest(Request, Transaction, Receiver);

  // Processing the request - 8.2.5
  if Request.IsInvite then
    Self.ProcessInvite(Request, Transaction, Receiver)
  else if Request.IsAck then
    Self.ProcessAck(Request, Transaction, Receiver)
  else if Request.IsBye then
    Self.SendByeToAppropriateSession(Request, Transaction, Receiver)
  else if Request.IsCancel then
    raise Exception.Create('Handling CANCELs not implemented yet');

  // TIdSipSession generates the response - 8.2.6
end;

procedure TIdSipUserAgentCore.ActOnResponse(Response: TIdSipResponse;
                                            Transaction: TIdSipTransaction;
                                            Receiver: TIdSipTransport);
var
  Session: TIdSipSession;
begin
  inherited ActOnResponse(Response, Transaction, Receiver);

  // User Agents drop unmatched responses on the floor.
  // Except for 2xx's on a client INVITE. And these no longer belong to
  // a transaction, since the receipt of a 200 terminates a client INVITE
  // immediately. Hence the unusual clause below.
  if Response.IsOK and Transaction.IsNull then begin
    Session := Self.FindSession(Response);
    if Assigned(Session) then
      Session.OnReceiveResponse(Response, Transaction, Receiver);
  end
  else
    Self.NotifyOfDroppedResponse(Response, Receiver);
end;

procedure TIdSipUserAgentCore.RejectRequest(Reaction: TIdSipUserAgentReaction;
                                            Request: TIdSipRequest;
                                            Transaction: TIdSipTransaction);
begin
  inherited RejectRequest(Reaction, Request, Transaction);

  case Reaction of
    uarMissingContact:
      Self.RejectBadRequest(Request, MissingContactHeader, Transaction);
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

function TIdSipUserAgentCore.AddInboundSession(Invite: TIdSipRequest;
                                               Transaction: TIdSipTransaction;
                                               Receiver: TIdSipTransport): TIdSipInboundSession;
begin
  Result := TIdSipInboundSession.Create(Self, Invite, Transaction, Receiver);
  try
    Self.SessionLock.Acquire;
    try
      Self.Sessions.Add(Result);
    finally
      Self.SessionLock.Release;
    end;

    Self.NotifyOfInboundCall(Result);
    Self.NotifyOfChange;
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
    NewReg.CallID := CallID;
    NewReg.Registrar.Uri := Registrar.Uri;
    NewReg.SequenceNo := SequenceNo;
  end;
end;

function TIdSipUserAgentCore.AddOutboundSession: TIdSipOutboundSession;
begin
  Result := TIdSipOutboundSession.Create(Self);
  try
    Self.SessionLock.Acquire;
    try
      Self.Sessions.Add(Result);
    finally
      Self.SessionLock.Release;
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
    Self.RegistrationLock.Acquire;
    try
      Self.Registrations.Add(Result);
    finally
      Self.RegistrationLock.Release;
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

function TIdSipUserAgentCore.DefaultHostName: String;
begin
  Result := 'localhost';
end;

function TIdSipUserAgentCore.DefaultUserAgent: String;
begin
  Result := 'Indy SIP/2.0 Server v0.1';
end;

function TIdSipUserAgentCore.FindSession(const Msg: TIdSipMessage): TIdSipSession;
var
  DialogID: TIdSipDialogID;
  I:        Integer;
  Session:  TIdSipSession;
begin
  Result := nil;
  if Msg.IsRequest then
    DialogID := TIdSipDialogID.Create(Msg.CallID,
                                      Msg.ToHeader.Tag,
                                      Msg.From.Tag)
  else
    DialogID := TIdSipDialogID.Create(Msg.CallID,
                                      Msg.From.Tag,
                                      Msg.ToHeader.Tag);
  try
    Self.SessionLock.Acquire;
    try
      I := 0;
      while (I < Self.Sessions.Count) and not Assigned(Result) do begin
        Session := Self.Sessions[I] as TIdSipSession;
        if Session.IsEstablished and Session.Dialog.ID.Equals(DialogID) then
          Result := Session
        else
          Inc(I);
      end;
    finally
      Self.SessionLock.Release;
    end;
  finally
    DialogID.Free;
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

procedure TIdSipUserAgentCore.ProcessAck(Ack: TIdSipRequest;
                                         Transaction: TIdSipTransaction;
                                         Receiver: TIdSipTransport);
var
  Session: TIdSipSession;
begin
  Session := Self.FindSession(Ack);

  // If Session = nil then we didn't match the ACK against any session, and so
  // we just drop it on the floor. There's no point in replying.
  if Assigned(Session) then
    Session.OnReceiveRequest(Ack, Transaction, Receiver);
end;

procedure TIdSipUserAgentCore.ProcessInvite(Invite: TIdSipRequest;
                                            Transaction: TIdSipTransaction;
                                            Receiver: TIdSipTransport);
var
  Session: TIdSipSession;
begin
  Assert(Invite.IsInvite,
         'ProcessInvite needs INVITE messages, '
       + 'not ' + Invite.Method + ' messages');

  Session := Self.FindSession(Invite);

  if Assigned(Session) then
    Session.OnReceiveRequest(Invite, Transaction, Receiver)
  else
    Self.AddInboundSession(Invite, Transaction, Receiver);
end;

function TIdSipUserAgentCore.RegistrarAt(Index: Integer): TIdSipRegistrationInfo;
begin
  Result := Self.KnownRegistrars[Index] as TIdSipRegistrationInfo;
end;

procedure TIdSipUserAgentCore.RejectBadRequest(Request: TIdSipRequest;
                                               const Reason: String;
                                               Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPBadRequest);
  try
    Response.StatusText := Reason;

    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipUserAgentCore.SendByeToAppropriateSession(Bye: TIdSipRequest;
                                                          Transaction: TIdSipTransaction;
                                                          Receiver: TIdSipTransport);
var
  Session: TIdSipSession;
begin
  Session := Self.FindSession(Bye);

  if Assigned(Session) then
    Session.OnReceiveRequest(Bye,
                             Transaction,
                             Receiver)
  else
    Self.ReturnResponse(Bye,
                        SIPCallLegOrTransactionDoesNotExist,
                        Transaction);

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

//******************************************************************************
//* TIdSipAction                                                               *
//******************************************************************************
//* TIdSipAction Public methods ************************************************

constructor TIdSipAction.Create(UA: TIdSipUserAgentCore);
begin
  inherited Create;

  Self.UA := UA;
end;

destructor TIdSipAction.Destroy;
begin
  inherited Destroy;
end;

//* TIdSipAction Protected methods *********************************************

function TIdSipAction.CurrentRequest: TIdSipRequest;
begin
  Result := Self.CurrentTransaction.InitialRequest;
end;

procedure TIdSipAction.ActionSucceeded(Response: TIdSipResponse);
begin
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

procedure TIdSipAction.NotifyOfFailure(Response: TIdSipResponse);
begin
end;

procedure TIdSipAction.OnFail(Transaction: TIdSipTransaction;
                              const Reason: String);
begin
end;

procedure TIdSipAction.OnReceiveRequest(Request: TIdSipRequest;
                                        Transaction: TIdSipTransaction;
                                        Receiver: TIdSipTransport);
begin
end;

procedure TIdSipAction.OnReceiveResponse(Response: TIdSipResponse;
                                         Transaction: TIdSipTransaction;
                                         Receiver: TIdSipTransport);
var
  Succeeded: Boolean;
begin
  // Each of the ReceiveXXXResponse functions returns true if we succeeded
  // in our Action, or we could re-issue the request. They only return
  // false when the action failed irrecoverably.

  case Response.StatusCode div 100 of
    SIPProvisionalResponseClass:
      Succeeded := Self.ReceiveProvisionalResponse(Response,
                                                   Transaction,
                                                   Receiver);
    SIPOKResponseClass:
      Succeeded := Self.ReceiveOKResponse(Response,
                                          Transaction,
                                          Receiver);
    SIPRedirectionResponseClass:
      Succeeded := Self.ReceiveRedirectionResponse(Response,
                                                   Transaction,
                                                   Receiver);
    SIPFailureResponseClass:
      Succeeded := Self.ReceiveFailureResponse(Response,
                                               Transaction,
                                               Receiver);
    SIPServerFailureResponseClass:
      Succeeded := Self.ReceiveServerFailureResponse(Response,
                                                     Transaction,
                                                     Receiver);
    SIPGlobalFailureResponseClass:
      Succeeded := Self.ReceiveGlobalFailureResponse(Response,
                                                     Transaction,
                                                     Receiver);
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

procedure TIdSipAction.OnTerminated(Transaction: TIdSipTransaction);
begin
  Transaction.RemoveTransactionListener(Self);
end;

function TIdSipAction.ReceiveFailureResponse(Response: TIdSipResponse;
                                     Transaction: TIdSipTransaction;
                                     Receiver: TIdSipTransport): Boolean;
begin
  Result := false;
end;

function TIdSipAction.ReceiveGlobalFailureResponse(Response: TIdSipResponse;
                                           Transaction: TIdSipTransaction;
                                           Receiver: TIdSipTransport): Boolean;
begin
  Result := false;
end;

function TIdSipAction.ReceiveOKResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport): Boolean;
begin
  Result := true;
end;

function TIdSipAction.ReceiveProvisionalResponse(Response: TIdSipResponse;
                                         Transaction: TIdSipTransaction;
                                         Receiver: TIdSipTransport): Boolean;
begin
  Result := false;
end;

function TIdSipAction.ReceiveRedirectionResponse(Response: TIdSipResponse;
                                                 Transaction: TIdSipTransaction;
                                                 Receiver: TIdSipTransport): Boolean;
var
  ReInvite: TIdSipRequest;
  Tran:     TIdSipTransaction;
begin
  Result := false;
  if Response.HasHeader(ContactHeaderFull) then begin
    ReInvite := TIdSipRequest.Create;
    try
      ReInvite.Assign(Transaction.InitialRequest);
      ReInvite.FirstContact.Assign(Response.FirstContact);

      Tran := Self.UA.Dispatcher.AddClientTransaction(ReInvite);
      Tran.AddTransactionListener(Self);
      Tran.SendRequest;
    finally
      ReInvite.Free;
    end;
    Result := true;
  end;
end;

function TIdSipAction.ReceiveServerFailureResponse(Response: TIdSipResponse;
                                           Transaction: TIdSipTransaction;
                                           Receiver: TIdSipTransport): Boolean;
begin
  Result := false;
end;

//******************************************************************************
//* TIdSipSessionTimer                                                         *
//******************************************************************************
//* TIdSipSessionTimer Public methods ******************************************

constructor TIdSipSessionTimer.Create(Session: TIdSipSession;
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

  Self.fIsEstablished := false;
  Self.fIsTerminated  := false;
  Self.fReceivedAck   := false;

  Self.fPayloadProcessor := TIdSdpPayloadProcessor.Create;
  Self.PayloadProcessor.Host     := Self.UA.HostName;
  Self.PayloadProcessor.Username := Self.UA.Username;

  Self.IsEstablished := false;

  Self.OkTimer := TIdSipSessionTimer.Create(Self, DefaultT1, DefaultT2);

  Self.OpenTransactionLock := TCriticalSection.Create;
  Self.OpenTransactions    := TList.Create;

  Self.SessionListenerLock := TCriticalSection.Create;
  Self.SessionListeners    := TList.Create;

  Self.InitialRequest := TIdSipRequest.Create;
end;

destructor TIdSipSession.Destroy;
begin
  Self.InitialRequest.Free;

  Self.SessionListeners.Free;
  Self.SessionListenerLock.Free;

  Self.OpenTransactions.Free;
  Self.OpenTransactionLock.Free;

  Self.OkTimer.Free;

  Self.PayloadProcessor.Free;
  
  Self.LastResponse.Free;

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

procedure TIdSipSession.Cancel;
begin
end;

procedure TIdSipSession.Terminate;
begin
  if Self.DialogEstablished then
    Self.SendBye
  else 
    Self.SendCancel;

  Self.MarkAsTerminated;
  Self.UA.RemoveSession(Self);
end;

procedure TIdSipSession.Modify(const Offer, ContentType: String);
begin
end;

procedure TIdSipSession.OnReceiveRequest(Request: TIdSipRequest;
                                         Transaction: TIdSipTransaction;
                                         Receiver: TIdSipTransport);
var
  OK: TIdSipResponse;
begin
  if Self.IsTerminated then begin
    Self.RejectRequest(Request, Transaction);
    Exit;
  end;

  if Request.IsBye then begin
    Self.MarkAsTerminated;
    Self.Dialog.HandleMessage(Request);

    OK := Self.UA.CreateResponse(Request, SIPOK);
    try
      Transaction.SendResponse(OK);
    finally
      OK.Free;
    end;
    Self.NotifyOfEndedSession(RemoteHangUp);
  end
  else if Request.IsAck then begin
    Self.fReceivedAck := true;

    if Assigned(Self.OkTimer) then
      Self.OkTimer.Stop;
  end
  else if Request.IsInvite then begin
    if Self.Dialog.IsOutOfOrder(Request) then begin
      Self.RejectOutOfOrderRequest(Request, Transaction);
      Exit;
    end;

//    if Request.Disposition.IsSession then
//      Self.PayloadProcessor.RemoteSessionDescription := Request.Body;

    Self.AddOpenTransaction(Transaction);
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

procedure TIdSipSession.ResendLastResponse;
begin
  if Assigned(Self.LastResponse) then
    Self.UA.Dispatcher.Send(Self.LastResponse);
end;

//* TIdSipSession Protected methods ********************************************

procedure TIdSipSession.ActionSucceeded(Response: TIdSipResponse);
begin
  if Self.DialogEstablished then
    Self.Dialog.HandleMessage(Response);
end;

function TIdSipSession.GetInvite: TIdSipRequest;
begin
  Result := Self.InitialRequest;
end;

procedure TIdSipSession.NotifyOfFailure(Response: TIdSipResponse);
begin
  Self.MarkAsTerminated;
  Self.NotifyOfEndedSession(Response.Description);
end;

procedure TIdSipSession.OnFail(Transaction: TIdSipTransaction;
                               const Reason: String);
begin
  if (Transaction = Self.InitialTran) then
    Self.MarkAsTerminated;
end;

procedure TIdSipSession.OnTerminated(Transaction: TIdSipTransaction);
begin
  inherited OnTerminated(Transaction);

  Self.RemoveTransaction(Transaction);
end;

//* TIdSipSession Private methods **********************************************

procedure TIdSipSession.AddOpenTransaction(Transaction: TIdSipTransaction);
begin
  Self.OpenTransactionLock.Acquire;
  try
    Self.OpenTransactions.Add(Transaction);
  finally
    Self.OpenTransactionLock.Release;
  end;
end;

procedure TIdSipSession.ApplyTo(List: TList;
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

function TIdSipSession.DialogEstablished: Boolean;
begin
  Result := Assigned(Self.fDialog);
end;

procedure TIdSipSession.MarkAsTerminated;
begin
  Self.fIsTerminated := true;

  Self.ApplyTo(Self.OpenTransactions,
               Self.OpenTransactionLock,
               Self.MarkAsTerminatedProc);
end;

procedure TIdSipSession.MarkAsTerminatedProc(ObjectOrIntf: Pointer);
begin
  Self.TerminateOpenTransaction(TIdSipTransaction(ObjectOrIntf));
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

  Self.UA.RemoveSession(Self);
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

  Self.IsEstablished := true;
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

procedure TIdSipSession.RejectOutOfOrderRequest(Request: TIdSipRequest;
                                                Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.UA.CreateResponse(Request,
                                     SIPInternalServerError);
  try
    Response.StatusText := RSSIPRequestOutOfOrder;
    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipSession.RejectRequest(Request: TIdSipRequest;
                                      Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.UA.CreateResponse(Request,
                                     SIPRequestTerminated);
  try
    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipSession.RemoveTransaction(Transaction: TIdSipTransaction);
begin
  Self.OpenTransactionLock.Acquire;
  try
    Self.OpenTransactions.Remove(Transaction);
  finally
    Self.OpenTransactionLock.Release;
  end;
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

procedure TIdSipSession.SendCancel;
var
  Cancel: TIdSipRequest;
begin
  Cancel := Self.CurrentRequest.CreateCancel;
  try
    // We don't listen to the new transaction because we assume the CANCEL
    // succeeds immediately.
    Self.UA.Dispatcher.AddClientTransaction(Cancel).SendRequest;
  finally
    Cancel.Free;
  end;
end;

procedure TIdSipSession.TerminateOpenTransaction(Transaction: TIdSipTransaction);
begin
  if not Transaction.IsClient then
    Self.RejectRequest(Transaction.InitialRequest, Transaction);
end;

//******************************************************************************
//* TIdSipInboundSession                                                       *
//******************************************************************************
//* TIdSipInboundSession Public methods ****************************************

constructor TIdSipInboundSession.Create(UA: TIdSipUserAgentCore;
                                        Invite: TIdSipRequest;
                                        InitialTransaction: TIdSipTransaction;
                                        Receiver: TIdSipTransport);
begin
  inherited Create(UA);

  Self.InitialTran      := InitialTransaction;
  Self.InitialTransport := Receiver;

  Self.InitialTran.AddTransactionListener(Self);

  Self.PayloadProcessor.RemoteSessionDescription := Invite.Body;

  Self.CurrentTransaction := Self.InitialTran;
end;

function TIdSipInboundSession.AcceptCall(const Offer, ContentType: String): String;
var
  Response: TIdSipResponse;
begin
  // Offer contains a description of what data we expect to receive. Sometimes
  // we cannot meet this offer (e.g., the offer says "receive on port 8000" but
  // port 8000's already bound. We thus try to honour the offer as closely as
  // possible, and return the _actual_ offer sent.

  // The type of payload processor depends on the ContentType passed in!
  Self.PayloadProcessor.StartListening(Offer);

  Response := Self.UA.CreateResponse(Self.CurrentRequest, SIPOK);
  try
    Result        := Self.PayloadProcessor.LocalSessionDescription;
    Response.Body := Result;

    Response.ContentLength := Length(Response.Body);
    Response.ContentType   := ContentType;
    Response.ToHeader.Tag  := Self.UA.NextTag;

    if not Self.DialogEstablished then begin
      fDialog := Self.CreateInboundDialog(Response);
      Self.NotifyOfEstablishedSession;
    end;

    Self.Dialog.HandleMessage(Self.CurrentRequest);
    Self.Dialog.HandleMessage(Response);

    Self.InitialTran.SendResponse(Response);
    Self.OkTimer.Start;
  finally
    Response.Free;
  end;
end;

function TIdSipInboundSession.IsInboundCall: Boolean;
begin
  Result := true;
end;

//* TIdSipInboundSession Private methods ***************************************

function TIdSipInboundSession.CreateInboundDialog(Response: TIdSipResponse): TIdSipDialog;
begin
  Result := TIdSipDialog.CreateInboundDialog(Self.CurrentRequest,
                                             Response,
                                             Self.InitialTransport);
  Self.LastResponse := TIdSipResponse.Create;
  Self.LastResponse.Assign(Response);
end;

//******************************************************************************
//* TIdSipOutboundSession                                                      *
//******************************************************************************
//* TIdSipOutboundSession Public methods ***************************************

procedure TIdSipOutboundSession.Call(Dest: TIdSipToHeader;
                                     const InitialOffer: String;
                                     const MimeType: String);
var
  Invite: TIdSipRequest;
begin
  if not Self.IsInboundCall and not Assigned(Self.InitialTran) then begin
    Invite := Self.UA.CreateInvite(Dest, InitialOffer, MimeType);
    try
      Self.InitialRequest.Assign(Invite);
      Self.InitialTran := Self.UA.Dispatcher.AddClientTransaction(Invite);
      Self.InitialTran.AddTransactionListener(Self);
      Self.InitialTran.SendRequest;

      Self.CurrentTransaction := Self.InitialTran;
    finally
      Invite.Free;
    end;
  end;
end;

function TIdSipOutboundSession.IsInboundCall: Boolean;
begin
  Result := false;
end;

//* TIdSipOutboundSession Protected methods ************************************

function TIdSipOutboundSession.ReceiveOKResponse(Response: TIdSipResponse;
                                                 Transaction: TIdSipTransaction;
                                                 Receiver: TIdSipTransport): Boolean;
begin
  // REMEMBER: A 2xx response to an INVITE DOES NOT take place in a transaction!
  // A 2xx response immediately terminates a client INVITE transaction so that
  // the ACK can get passed up to the UA (as an unhandled request).

  if not Self.DialogEstablished then begin
    fDialog := Self.CreateOutboundDialog(Response, Transaction, Receiver);
    Self.PayloadProcessor.RemoteSessionDescription := Response.Body;
    Self.NotifyOfEstablishedSession;
  end;

  Self.SendAck(Response);

  Result := true;
end;

function TIdSipOutboundSession.ReceiveProvisionalResponse(Response: TIdSipResponse;
                                                          Transaction: TIdSipTransaction;
                                                          Receiver: TIdSipTransport): Boolean;
begin
  // We should check for "and Response.ToHeader.HasTag" but that would prevent
  // us connecting to X-Lite, the non-compliant SIP phone.
  if not Response.IsTrying and Response.ToHeader.HasTag then begin
    if not Self.DialogEstablished then begin
      fDialog := Self.CreateOutboundDialog(Response, Transaction, Receiver);
      Self.NotifyOfEstablishedSession;
    end;
  end;

  Result := true;
end;

function TIdSipOutboundSession.ReceiveRedirectionResponse(Response: TIdSipResponse;
                                                          Transaction: TIdSipTransaction;
                                                          Receiver: TIdSipTransport): Boolean;
begin
  Self.SendAck(Response);
  Result := inherited ReceiveRedirectionResponse(Response, Transaction, Receiver);
end;

//* TIdSipOutboundSession Private methods **************************************

function TIdSipOutboundSession.CreateOutboundDialog(Response: TIdSipResponse;
                                                    Transaction: TIdSipTransaction;
                                                    Receiver: TIdSipTransport): TIdSipDialog;
begin
  Result := TIdSipDialog.CreateOutboundDialog(Transaction.InitialRequest,
                                              Response,
                                              Receiver);
  Self.NotifyOfEstablishedSession;
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

function TIdSipRegistration.ReceiveFailureResponse(Response: TIdSipResponse;
                                                   Transaction: TIdSipTransaction;
                                                   Receiver: TIdSipTransport): Boolean;
begin
  Result := true;
  case Response.StatusCode of
    SIPUnauthorized,
    SIPProxyAuthenticationRequired:
      Self.NotifyOfAuthenticationChallenge(Response);
    SIPIntervalTooBrief: Self.ReissueRequest(Transaction.InitialRequest.RequestUri,
                                             Response.FirstMinExpires.NumericValue);
    SIPBadExtension: begin
      Result := Transaction.InitialRequest.HasHeader(RequireHeader);
      if Result then
        Self.RetryWithoutExtensions(Transaction.InitialRequest.RequestUri,
                                    Response);
    end;
  else
    Result := false;
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
  Self.CurrentTransaction := Self.UA.Dispatcher.AddClientTransaction(Request);
  
  Self.CurrentTransaction.AddTransactionListener(Self);
  Self.CurrentTransaction.SendRequest;
end;

procedure TIdSipRegistration.Terminate;
begin
  Self.UA.RemoveRegistration(Self);
end;

end.
