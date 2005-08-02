{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
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
//   aware of ONLY the layer immediately below it, but we can't always do that.
//   We NEVER let a lower layer know about layers above it. Thus, the transport
//   layer DOES NOT know about transactions, etc.
// * We propogate messages up the stack using Events or Listeners, and method
//   calls to propogate messages down the stack. We give preference to the more
//   flexible Listeners.
// * We avoid typecasting as much as possible by using polymorphism and, in
//   certain situations where polymorphism can't cut it, the Visitor pattern.
// * TObjectLists almost always manage the lifetime of the objects they contain.
// * Threads belong to the process in which they run. It doesn't really make sense
//   for us to refer to a class that instantiates a thread as the thread's owner,
//   so
//   (a) all threads should FreeOnTerminate, and
//   (b) all classes that instantiate threads should not free the threads, but
//      just Terminate (and possibly nil any references to the threads).

{
CODE FROM THE TRANSACTION LAYER TO ASSIMILATE

procedure TestTIdSipTransactionDispatcher.TestSendVeryBigMessageWithTcpFailure;
var
  TcpResponseCount: Cardinal;
  UdpResponseCount: Cardinal;
begin
  Self.MockTransport.TransportType := TcpTransport;
  Self.MockTransport.FailWith      := EIdConnectTimeout;

  TcpResponseCount := Self.MockTcpTransport.SentResponseCount;
  UdpResponseCount := Self.MockUdpTransport.SentResponseCount;

  while (Length(Self.Response200.AsString) < MaximumUDPMessageSize) do
    Self.Response200.AddHeader(SubjectHeaderFull).Value := 'In R''lyeh dead Cthulhu lies dreaming';

  Self.Response200.LastHop.Transport := Self.MockUdpTransport.TransportType;
  Self.D.SendToTransport(Self.Response200);

  Check(UdpResponseCount < Self.MockUdpTransport.SentResponseCount,
        'No response sent down UDP');
  CheckEquals(TcpResponseCount, Self.MockTcpTransport.SentResponseCount,
              'TCP response was sent');
end;
}
interface

uses
  Classes, Contnrs, IdBaseThread, IdSipDialog, IdSipDialogID, IdException,
  IdInterfacedObject, IdNotification, IdObservable, IdSipAuthentication,
  IdSipLocator, IdSipMessage, IdSipRegistration, IdSipTransaction,
  IdSipTransport, IdTimerQueue, SyncObjs;

const
  SipStackVersion = '0.5';

type
  TIdSipAction = class;
  TIdSipActionClass = class of TIdSipAction;

  // I provide a protocol for generic Actions.
  // OnAuthenticationChallenge right now isn't used: it's here in anticipation
  // of a rewrite of the stack's authentication mechanism.
  IIdSipActionListener = interface
    ['{C3255325-A52E-46FF-9C21-478880FB350A}']
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Challenge: TIdSipResponse);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
  end;

  TIdSipInboundInvite = class;

  IIdSipInboundInviteListener = interface
    ['{DE147123-E768-464A-924A-411BAA0C0B53}']
    procedure OnFailure(InviteAgent: TIdSipInboundInvite);
    procedure OnSuccess(InviteAgent: TIdSipInboundInvite;
                        Ack: TIdSipRequest);
  end;

  TIdSipOutboundInvite = class;

  IIdSipInviteListener = interface(IIdSipActionListener)
    ['{8694DF86-3012-41AE-9854-A623A486743F}']
    procedure OnCallProgress(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse);
    procedure OnFailure(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse;
                        const Reason: String);
    procedure OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                  NewDialog: TIdSipDialog);
    procedure OnRedirect(InviteAgent: TIdSipOutboundInvite;
                         Redirect: TIdSipResponse);
    procedure OnSuccess(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse);
  end;

  TIdSipOutboundOptions = class;

  IIdSipOptionsListener = interface(IIdSipActionListener)
    ['{3F2ED4DF-4854-4255-B156-F4581AEAEDA3}']
    procedure OnResponse(OptionsAgent: TIdSipOutboundOptions;
                         Response: TIdSipResponse);
  end;

  TIdSipOutboundRegistration = class;

  // I provide a protocol for using a registrar. You send a REGISTER, and
  // listen for the events below.
  //
  // OnFailure and OnSuccess, apart from the obvious, tell you that the
  // registration agent has terminated, and that you should remove all
  // of your references to it.
  IIdSipRegistrationListener = interface(IIdSipActionListener)
    ['{D3FA9A3D-ED8A-48D3-8068-38E8F9EE2140}']
    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts;
                        Response: TIdSipResponse);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts);
  end;

  TIdSipSession = class;

  // I am the protocol of things that listen for Sessions:
  // * OnEndedSession tells us when the session's finished. This could be
  //   because someone hung up, or because the outbound call failed (the request
  //   timed out, a transport error occurec, etc). OnSessionEnded lets us clean
  //   up. The Session referenced becomes invalid after this point. In other
  //   words, you'd better say goodbye to the Session in your implementation of
  //   this method. Accessing your reference to the Session after this method
  //   has finished will probably fail with an access violation.
  // * OnEstablishedSession tells us when a session has been fully established.
  //   For inbound calls this means receipt of an ACK; for outbound calls, the
  //   receipt of a 200 OK.
  // * OnModifySession fires when we receive an in-dialog INVITE - an INVITE
  //   that offers a modified session description.
  // * OnModifiedSession tells us the answer the remote side gave us for an
  //   INVITE we sent to modify the session description. In other words, at this
  //   point we know that our requested session modification succeeded or
  //   failed.
  // * OnProgressedSession tells us of any provisional responses received by
  //   this session's Invites. This INCLUDES provisional responses to
  //   ModifyInvites.
  IIdSipSessionListener = interface(IIdSipActionListener)
    ['{59B3C476-D3CA-4C5E-AA2B-2BB587A5A716}']
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
    procedure OnProgressedSession(Session: TIdSipSession;
                                  Progress: TIdSipResponse);
  end;

  TIdSipAbstractUserAgent = class;
  TIdSipInboundSession = class;

  // You can use OnAuthenticationChallenge to authenticate to a proxy (or
  // registrar or user agent server). Note that we cannot distinguish between
  // (1) you contacting the proxy/registrar for the first time and it asking
  // for credentials, and (2) you offering invalid credentials.
  IIdSipUserAgentListener = interface
    ['{E365D17F-054B-41AB-BB18-0C339715BFA3}']
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
  end;

  TIdSipUserAgentReaction =
    (uarAccept,
     uarBadAuthorization,
     uarBadRequest,
     uarDoNotDisturb,
     uarExpireTooBrief,
     uarForbidden,
     uarLoopDetected,
     uarMethodNotAllowed,
     uarMissingContact,
     uarNotFound,
     uarUnsupportedExtension,
     uarTooManyVias,
     uarUnauthorized,
     uarUnsupportedAccept,
     uarUnsupportedContentEncoding,
     uarUnsupportedContentLanguage,
     uarUnsupportedContentType,
     uarUnsupportedMethod,
     uarUnsupportedScheme,
     uarUnsupportedSipVersion);

  // I represent a closure that contains some block of code involving an Action.
  // I also represent the null action closure.
  TIdSipActionClosure = class(TObject)
  public
    procedure Execute(Action: TIdSipAction); virtual;
  end;

  TIdSipActionClosureClass = class of TIdSipActionClosure;

  // TODO: there's redundance with this Hostname, and the Hostnames of the
  // transports attached to this core. It's not clear how to set up the
  // hostnames and bindings of the stack.
  TIdSipAbstractCore = class(TIdInterfacedObject,
                             IIdSipTransactionDispatcherListener)
  private
    fAuthenticator:         TIdSipAbstractAuthenticator;
    fDispatcher:            TIdSipTransactionDispatcher;
    fHostName:              String;
    fLocator:               TIdSipAbstractLocator;
    fRealm:                 String;
    fRequireAuthentication: Boolean;
    fTimer:                 TIdTimerQueue;
    fUserAgentName:         String;
    Observed:               TIdObservable;
    TimerLock:              TCriticalSection;
    UserAgentListeners:     TIdNotificationList;

    function  DefaultHostName: String;
    procedure RejectBadAuthorization(Request: TIdSipRequest);
    procedure SetDispatcher(Value: TIdSipTransactionDispatcher);
    procedure SetRealm(const Value: String);
  protected
    procedure ActOnRequest(Request: TIdSipRequest;
                           Receiver: TIdSipTransport); virtual;
    procedure ActOnResponse(Response: TIdSipResponse;
                            Receiver: TIdSipTransport); virtual;
    procedure MaybeChangeTransport(Msg: TIdSipMessage);
    procedure NotifyOfChange;
    procedure OnAuthenticationChallenge(Dispatcher: TIdSipTransactionDispatcher;
                                        Challenge: TIdSipResponse;
                                        ChallengeResponse: TIdSipRequest;
                                        var TryAgain: Boolean); virtual;
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport); virtual;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport); virtual;
    procedure PrepareResponse(Response: TIdSipResponse;
                              Request: TIdSipRequest); virtual;
    procedure RejectBadRequest(Request: TIdSipRequest;
                               const Reason: String);
    procedure RejectRequest(Reaction: TIdSipUserAgentReaction;
                            Request: TIdSipRequest); virtual;
    procedure RejectRequestUnauthorized(Request: TIdSipRequest);
    procedure SetAuthenticator(Value: TIdSipAbstractAuthenticator); virtual;
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; virtual;
    function  WillAcceptResponse(Response: TIdSipResponse): TIdSipUserAgentReaction; virtual;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddObserver(const Listener: IIdObserver);
    procedure AddUserAgentListener(const Listener: IIdSipUserAgentListener);
    function  Authenticate(Request: TIdSipRequest): Boolean;
    function  CreateChallengeResponse(Request: TIdSipRequest): TIdSipResponse;
    function  CreateChallengeResponseAsUserAgent(Request: TIdSipRequest): TIdSipResponse;
    function  CreateRequest(const Method: String;
                            Dest: TIdSipAddressHeader): TIdSipRequest; overload; virtual; abstract;
    function  CreateRequest(const Method: String;
                            Dialog: TIdSipDialog): TIdSipRequest; overload; virtual; abstract;
    function  CreateResponse(Request: TIdSipRequest;
                             ResponseCode: Cardinal): TIdSipResponse; virtual;
    function  NextCallID: String;
    function  NextNonce: String;
    function  NextTag: String;
    procedure RemoveObserver(const Listener: IIdObserver);
    procedure RemoveUserAgentListener(const Listener: IIdSipUserAgentListener);
    procedure ScheduleEvent(Event: TNotifyEvent;
                            WaitTime: Cardinal;
                            Msg: TIdSipMessage); overload;
    procedure SendRequest(Request: TIdSipRequest;
                          Dest: TIdSipLocation);
    procedure SendResponse(Response: TIdSipResponse);

    property Authenticator:         TIdSipAbstractAuthenticator read fAuthenticator write SetAuthenticator;
    property Dispatcher:            TIdSipTransactionDispatcher read fDispatcher write SetDispatcher;
    property HostName:              String                      read fHostName write fHostName;
    property Locator:               TIdSipAbstractLocator       read fLocator write fLocator;
    property Realm:                 String                      read fRealm write SetRealm;
    property RequireAuthentication: Boolean                     read fRequireAuthentication write fRequireAuthentication;
    property Timer:                 TIdTimerQueue               read fTimer write fTimer;
    property UserAgentName:         String                      read fUserAgentName write fUserAgentName;
  end;

  // I keep track of information a User Agent needs when making a REGISTER to
  // a particular registrar.
  TIdSipRegistrationInfo = class(TObject)
  private
    fCallID:     String;
    fRegistrar:  TIdSipUri;
    fSequenceNo: Cardinal;

    procedure SetRegistrar(Value: TIdSipUri);
  public
    constructor Create;
    destructor  Destroy; override;

    property CallID:     String    read fCallID write fCallID;
    property Registrar:  TIdSipUri read fRegistrar write SetRegistrar;
    property SequenceNo: Cardinal  read fSequenceNo write fSequenceNo;
  end;

  // I store registration information for registrars with which you've
  // registered. 
  TIdSipRegistrations = class(TObject)
  private
    KnownRegistrars: TObjectList;
    Lock:            TCriticalSection;

    function IndexOfRegistrar(Registrar: TIdSipUri): Integer;
    function KnowsRegistrar(Registrar: TIdSipUri): Boolean;
    function RegistrarAt(Index: Integer): TIdSipRegistrationInfo;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddKnownRegistrar(Registrar: TIdSipUri;
                                const CallID: String;
                                SequenceNo: Cardinal);
    function  CallIDFor(Registrar: TIdSipUri): String;
    function  NextSequenceNoFor(Registrar: TIdSipUri): Cardinal;
  end;

  TIdSipSessionProc = procedure(Session: TIdSipSession;
                                Invite: TIdSipRequest) of object;

  // I maintain a list of Actions. You may query me for various statistics, as
  // well as do things to particular actions.
  // The FindFooAndPerform methods require some explanation. The Event
  // parameter Data property must point to a copy of a TIdSipRequest.
  // FindFooAndPerform will destroy the Request.
  TIdSipActions = class(TObject)
  private
    ActionLock: TCriticalSection;
    Actions:    TObjectList;
    Observed:   TIdObservable;

    function  ActionAt(Index: Integer): TIdSipAction;
    function  FindAction(Msg: TIdSipMessage): TIdSipAction; overload;
    function  FindAction(const ActionID: String): TIdSipAction; overload;
    procedure LockActions;
    procedure UnlockActions;
  public
    constructor Create;
    destructor  Destroy; override;

    function  Add(Action: TIdSipAction): TIdSipAction;
    procedure AddObserver(const Listener: IIdObserver);
    function  AddOutboundAction(UserAgent: TIdSipAbstractUserAgent;
                                ActionType: TIdSipActionClass): TIdSipAction;
    procedure CleanOutTerminatedActions;
    function  Count: Integer;
    function  CountOf(const MethodName: String): Integer;
    procedure FindActionAndPerform(const ID: String;
                                   Block: TIdSipActionClosure); overload;
    procedure FindActionAndPerform(Msg: TIdSipMessage;
                                   Block: TIdSipActionClosure); overload;
    procedure FindActionAndPerformOr(const ID: String;
                                     FoundBlock: TIdSipActionClosure;
                                     NotFoundBlock: TIdSipActionClosure); overload;
    procedure FindActionAndPerformOr(Msg: TIdSipMessage;
                                     FoundBlock: TIdSipActionClosure;
                                     NotFoundBlock: TIdSipActionClosure); overload;
    function  InviteCount: Integer;
    function  NextActionID: String;
    function  OptionsCount: Integer;
    procedure Perform(Msg: TIdSipMessage; Block: TIdSipActionClosure);
    function  RegistrationCount: Integer;    
    procedure RemoveObserver(const Listener: IIdObserver);
    function  SessionCount: Integer;
    procedure TerminateAllActions;
  end;

  // I represent an event that will execute a block (BlockType) on an action in
  // a list of actions.
  TIdSipActionsWait = class(TIdSipMessageWait)
  private
    fActions:   TIdSipActions;
    fBlockType: TIdSipActionClosureClass;
  public
    procedure Trigger; override;

    property Actions:   TIdSipActions            read fActions write fActions;
    property BlockType: TIdSipActionClosureClass read fBlockType write fBlockType;
  end;

  TIdSipIDActionsWait = class(TIdSipActionsWait)
  private
    fActionID: String;
  public
    procedure Trigger; override;

    property ActionID: String read fActionID write fActionID;
  end;

  // I represent the (possibly deferred) execution of something my Action needs
  // done. That is, when you invoke my Trigger, I call Action.Send.
  TIdSipActionSendWait = class(TIdWait)
  private
    fAction: TIdSipAction;
  public
    procedure Trigger; override;

    property Action: TIdSipAction read fAction write fAction;
  end;

  TIdSipActionsWaitClass = class of TIdSipActionsWait;

  // I represent a closure that a UserAgent uses to, for instance, process a
  // request or response.
  TIdUserAgentClosure = class(TIdSipActionClosure)
  private
    fReceiver:  TIdSipTransport;
    fRequest:   TIdSipRequest;
    fUserAgent: TIdSipAbstractUserAgent;
  public
    property Receiver:  TIdSipTransport         read fReceiver write fReceiver;
    property Request:   TIdSipRequest           read fRequest write fRequest;
    property UserAgent: TIdSipAbstractUserAgent read fUserAgent write fUserAgent;
  end;

  // I give my Request to the Action or create a new Action to which I give the
  // Request. I also drop an unmatched ACK, and respond with 481 Call Leg/
  // Transaction Does Not Exist as the case may be.
  TIdSipUserAgentActOnRequest = class(TIdUserAgentClosure)
  public
    procedure Execute(Action: TIdSipAction); override;
  end;

  // I give the affected Action my Response, or drop the (unmatched) response.
  TIdSipUserAgentActOnResponse = class(TIdUserAgentClosure)
  private
    fResponse: TIdSipResponse;
  public
    procedure Execute(Action: TIdSipAction); override;

    property Response: TIdSipResponse read fResponse write fResponse;
  end;

  // I represent the closure that will change the initial request of an Action.
  // For instance, I could change the dialog of the affected Action.
  TIdSipUserAgentUpdateAction = class(TIdUserAgentClosure)
  public
    procedure Execute(Action: TIdSipAction); override;
  end;

  TIdSipMessageModule = class;
  TIdSipMessageModuleClass = class of TIdSipMessageModule;
  TIdSipOutboundRegister = class;
  TIdSipOutboundRegistrationQuery = class;
  TIdSipOutboundUnregister = class;

  // I (usually) represent a human being in the SIP network. I:
  // * inform any listeners when new sessions become established, modified or
  //   terminated;
  // * allow my users to make outgoing "calls";
  // * clean up established Sessions
  //
  // I provide the canonical place to reject messages that have correct syntax
  // but that we don't or can't accept. This includes unsupported SIP versions,
  // unrecognised methods, etc.
  TIdSipAbstractUserAgent = class(TIdSipAbstractCore,
                                  IIdObserver)
  private
    fActions:                TIdSipActions;
    fAllowedContentTypeList: TStrings;
    fAllowedLanguageList:    TStrings;
    fAllowedSchemeList:      TStrings;
    fContact:                TIdSipContactHeader;
    fFrom:                   TIdSipFromHeader;
    fKeyring:                TIdKeyRing;
    ModuleLock:              TCriticalSection;
    Modules:                 TObjectList;

    procedure AddModuleSpecificHeaders(OutboundMessage: TIdSipMessage);
    function  ConvertToHeader(ValueList: TStrings): String;
    function  CreateRequestHandler(Request: TIdSipRequest;
                                   Receiver: TIdSipTransport): TIdSipUserAgentActOnRequest;
    function  CreateResponseHandler(Response: TIdSipResponse;
                                    Receiver: TIdSipTransport): TIdSipUserAgentActOnResponse;
    function  DefaultFrom: String;
    function  DefaultUserAgent: String;
    function  GetContact: TIdSipContactHeader;
    function  GetFrom: TIdSipFromHeader;
    function  ModuleAt(Index: Integer): TIdSipMessageModule;
    procedure NotifyModulesOfFree;
    procedure NotifyOfAuthenticationChallenge(Response: TIdSipResponse;
                                              var Username: String;
                                              var Password: String;
                                              var TryAgain: Boolean);
    procedure NotifyOfDroppedMessage(Message: TIdSipMessage;
                                     Receiver: TIdSipTransport);
    procedure OnChanged(Observed: TObject);
    procedure ReturnMethodNotAllowed(Request: TIdSipRequest);
    procedure RejectRequestBadExtension(Request: TIdSipRequest);
    procedure RejectRequestMethodNotSupported(Request: TIdSipRequest);
    procedure RejectRequestUnknownAccept(Request: TIdSipRequest);
    procedure RejectRequestUnknownContentEncoding(Request: TIdSipRequest);
    procedure RejectRequestUnknownContentLanguage(Request: TIdSipRequest);
    procedure RejectRequestUnknownContentType(Request: TIdSipRequest);
    procedure RejectUnsupportedSipVersion(Request: TIdSipRequest);
    procedure SetContact(Value: TIdSipContactHeader);
    procedure SetFrom(Value: TIdSipFromHeader);

    // Pull these into UserAgent proper:
    procedure NotifyOfInboundCall(Session: TIdSipInboundSession);
    procedure UpdateAffectedActionWithRequest(FindMsg: TIdSipMessage;
                                              NewRequest: TIdSipRequest);
  protected
    procedure ActOnRequest(Request: TIdSipRequest;
                           Receiver: TIdSipTransport); override;
    procedure ActOnResponse(Response: TIdSipResponse;
                            Receiver: TIdSipTransport); override;
    function  AddInboundAction(Request: TIdSipRequest;
                               Receiver: TIdSipTransport): TIdSipAction; overload;
    procedure AddLocalHeaders(OutboundRequest: TIdSipRequest); virtual;
    function  CreateActionsClosure(ClosureType: TIdSipActionsWaitClass;
                                   Msg: TIdSipMessage): TIdSipActionsWait;
    function  ListHasUnknownValue(Request: TIdSipRequest;
                                  ValueList: TStrings;
                                  const HeaderName: String): Boolean;
    procedure OnAuthenticationChallenge(Dispatcher: TIdSipTransactionDispatcher;
                                        Challenge: TIdSipResponse;
                                        ChallengeResponse: TIdSipRequest;
                                        var TryAgain: Boolean); override;
    procedure PrepareResponse(Response: TIdSipResponse;
                              Request: TIdSipRequest); override;
    procedure RejectRequest(Reaction: TIdSipUserAgentReaction;
                            Request: TIdSipRequest); override;
    function  ResponseForInvite: Cardinal; virtual;
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; override;

    property Actions:                TIdSipActions read fActions;
    property AllowedContentTypeList: TStrings      read fAllowedContentTypeList;
    property AllowedLanguageList:    TStrings      read fAllowedLanguageList;
    property AllowedSchemeList:      TStrings      read fAllowedSchemeList;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddAllowedContentType(const MimeType: String);
    procedure AddAllowedLanguage(const LanguageID: String);
    procedure AddAllowedScheme(const Scheme: String);
    function  AddModule(ModuleType: TIdSipMessageModuleClass): TIdSipMessageModule;
    function  AddOutboundAction(ActionType: TIdSipActionClass): TIdSipAction;
    function  AllowedContentTypes: String;
    function  AllowedEncodings: String;
    function  AllowedExtensions: String;
    function  AllowedLanguages: String;
    function  AllowedMethods(RequestUri: TIdSipUri): String;
    function  AllowedSchemes: String;
    function  CountOf(const MethodName: String): Integer;
    function  CreateOptions(Dest: TIdSipAddressHeader): TIdSipRequest;
    function  CreateRequest(const Method: String;
                            Dest: TIdSipAddressHeader): TIdSipRequest; overload; override;
    function  CreateRequest(const Method: String;
                            Dialog: TIdSipDialog): TIdSipRequest; overload; override;
    function  CreateResponse(Request: TIdSipRequest;
                             ResponseCode: Cardinal): TIdSipResponse; override;
    procedure FindServersFor(Request: TIdSipRequest;
                             Result: TIdSipLocations); overload;
    procedure FindServersFor(Response: TIdSipResponse;
                             Result: TIdSipLocations); overload;
    function  HasUnknownAccept(Request: TIdSipRequest): Boolean;
    function  HasUnknownContentEncoding(Request: TIdSipRequest): Boolean;
    function  HasUnknownContentLanguage(Request: TIdSipRequest): Boolean;
    function  HasUnknownContentType(Request: TIdSipRequest): Boolean;
    function  IsExtensionAllowed(const Extension: String): Boolean;
    function  IsMethodAllowed(RequestUri: TIdSipUri;
                              const Method: String): Boolean;
    function  IsMethodSupported(const Method: String): Boolean;
    function  IsSchemeAllowed(const Scheme: String): Boolean;
    function  KnownMethods: String;
    function  ModuleFor(Request: TIdSipRequest): TIdSipMessageModule; overload;
    function  ModuleFor(const Method: String): TIdSipMessageModule; overload;
    function  ModuleFor(ModuleType: TIdSipMessageModuleClass): TIdSipMessageModule; overload;
    function  NextActionID: String;
    function  NextBranch: String;
    function  NextInitialSequenceNo: Cardinal;
    function  OptionsCount: Integer;
    function  QueryOptions(Server: TIdSipAddressHeader): TIdSipOutboundOptions;
    procedure RemoveModule(ModuleType: TIdSipMessageModuleClass);
    procedure ReturnResponse(Request: TIdSipRequest;
                             Reason: Cardinal);
    procedure ScheduleEvent(BlockType: TIdSipActionClosureClass;
                            WaitTime: Cardinal;
                            Copy: TIdSipMessage); overload;
    procedure ScheduleEvent(BlockType: TIdSipActionClosureClass;
                            WaitTime: Cardinal;
                            Copy: TIdSipMessage;
                            const ActionID: String); overload;
    procedure ScheduleEvent(WaitTime: Cardinal;
                            Wait: TIdWait); overload;
    function  Username: String;
    function  UsesModule(ModuleType: TIdSipMessageModuleClass): Boolean;

    // Move to UserAgent:
    procedure TerminateAllCalls; // move to InviteModule
    function  UsingDefaultContact: Boolean;

    property Contact: TIdSipContactHeader read GetContact write SetContact;
    property From:    TIdSipFromHeader    read GetFrom write SetFrom;
    property Keyring: TIdKeyRing          read fKeyring;
  end;

  TIdSipInviteModule = class;
  TIdSipOutboundSession = class;
  TIdSipOutboundRegisterModule = class;

  TIdSipUserAgent = class(TIdSipAbstractUserAgent)
  private
    fDoNotDisturb:        Boolean;
    fDoNotDisturbMessage: String;
    fHasProxy:            Boolean;
    fProxy:               TIdSipUri;
    fRegisterModule:      TIdSipOutboundRegisterModule;
    fInviteModule:        TIdSipInviteModule;

    function  AddOutboundSession: TIdSipOutboundSession;
    function  GetInitialResendInterval: Cardinal;
    function  GetProgressResendInterval: Cardinal;
    procedure SetInitialResendInterval(Value: Cardinal);
    procedure SetProgressResendInterval(Value: Cardinal);
    procedure SetProxy(Value: TIdSipUri);
  protected
    procedure AddLocalHeaders(OutboundRequest: TIdSipRequest); override;
    function  ResponseForInvite: Cardinal; override;
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function  Call(Dest: TIdSipAddressHeader;
                   const LocalSessionDescription: String;
                   const MimeType: String): TIdSipOutboundSession;
    function  SessionCount: Integer;

    property DoNotDisturb:           Boolean                      read fDoNotDisturb write fDoNotDisturb;
    property DoNotDisturbMessage:    String                       read fDoNotDisturbMessage write fDoNotDisturbMessage;
    property HasProxy:               Boolean                      read fHasProxy write fHasProxy;
    property InitialResendInterval:  Cardinal                     read GetInitialResendInterval write SetInitialResendInterval;
    property InviteModule:           TIdSipInviteModule           read fInviteModule;
    property ProgressResendInterval: Cardinal                     read GetProgressResendInterval write SetProgressResendInterval;
    property Proxy:                  TIdSipUri                    read fProxy write SetProxy;
    property RegisterModule:         TIdSipOutboundRegisterModule read fRegisterModule;
  end;

  TIdSipRegisterModule = class;

  TIdSipRegistrar = class(TIdSipAbstractUserAgent)
  private
    RegisterModule: TIdSipRegisterModule;

    function  GetBindingDB: TIdSipAbstractBindingDatabase;
    function  GetDefaultRegistrationExpiryTime: Cardinal;
    function  GetMinimumExpiryTime: Cardinal;
    procedure SetBindingDB(Value: TIdSipAbstractBindingDatabase);
    procedure SetDefaultRegistrationExpiryTime(Value: Cardinal);
    procedure SetMinimumExpiryTime(Value: Cardinal);
  public
    constructor Create; override;

    function RegistrationCount: Integer;

    property BindingDB:                     TIdSipAbstractBindingDatabase read GetBindingDB write SetBindingDB;
    property DefaultRegistrationExpiryTime: Cardinal                      read GetDefaultRegistrationExpiryTime write SetDefaultRegistrationExpiryTime;
    property MinimumExpiryTime:             Cardinal                      read GetMinimumExpiryTime write SetMinimumExpiryTime;
  end;

  // Given a configuration file, I create a stack.
  // The configuration file consists of lines. Each line is a complete and
  // independent setting consisting of a Directive, at least one space, and the
  // settings for that Directive.
  //
  // Currently we support the following Directives: Listen, NameServer,
  // Register.
  //
  // Here's a summary of the formats for each directive:
  //   NameServer: <domain name or IP>:<port>
  //   NameServer: MOCK
  //   Listen: <transport name><SP><host|IPv4 address|IPv6 reference|AUTO>:<port>
  //   Register: <SIP/S URI>
  //   Proxy: <SIP/S URI>
  //   From: "Count Zero" <sip:countzero@jammer.org>
  //   Contact: sip:wintermute@tessier-ashpool.co.luna
  TIdSipStackConfigurator = class(TObject)
  private
    procedure AddAuthentication(UserAgent: TIdSipAbstractUserAgent;
                                const AuthenticationLine: String);
    procedure AddAutoContact(UserAgent: TIdSipAbstractUserAgent);
    procedure AddContact(UserAgent: TIdSipAbstractUserAgent;
                      const ContactLine: String);
    procedure AddFrom(UserAgent: TIdSipAbstractUserAgent;
                      const FromLine: String);
    procedure AddLocator(UserAgent: TIdSipAbstractUserAgent;
                         const NameServerLine: String);
    procedure AddProxy(UserAgent: TIdSipUserAgent;
                       const ProxyLine: String);
    procedure AddTransport(Dispatcher: TIdSipTransactionDispatcher;
                           const TransportLine: String);
    procedure CheckUri(Uri: TIdSipUri;
                       const FailMsg: String);
    function  CreateLayers(Context: TIdTimerQueue): TIdSipUserAgent;
    procedure EatDirective(var Line: String);
    procedure InstantiateMissingObjectsAsDefaults(UserAgent: TIdSipAbstractUserAgent);
    procedure ParseFile(UserAgent: TIdSipUserAgent;
                        Configuration: TStrings;
                        PendingActions: TObjectList);
    procedure ParseLine(UserAgent: TIdSipUserAgent;
                        const ConfigurationLine: String;
                        PendingActions: TObjectList);
    procedure RegisterUA(UserAgent: TIdSipUserAgent;
                         const RegisterLine: String;
                         PendingActions: TObjectList);
    procedure SendPendingActions(Actions: TObjectList);
  public
    function CreateUserAgent(Configuration: TStrings;
                             Context: TIdTimerQueue): TIdSipUserAgent; overload;
  end;

  IIdSipMessageModuleListener = interface
    ['{4C5192D0-6AE1-4F59-A31A-FDB3D30BC617}']
  end;

  // I and my subclasses represent chunks of Transaction-User Core
  // functionality: the ability to process REGISTERs, say, or OPTIONS, or the
  // requests involved with establishing a call.
  TIdSipMessageModule = class(TObject)
  private
    fUserAgent: TIdSipAbstractUserAgent;
  public
    constructor Create(UA: TIdSipAbstractUserAgent); virtual;

    function  Accept(Request: TIdSipRequest;
                     UsingSecureTransport: Boolean): TIdSipAction; virtual;
    procedure AddLocalHeaders(OutboundMessage: TIdSipMessage); virtual;
    function  AcceptsMethods: String; virtual;
    procedure CleanUp; virtual;
    function  WillAccept(Request: TIdSipRequest): Boolean; virtual;

    property UserAgent: TIdSipAbstractUserAgent read fUserAgent;
  end;

  TIdSipInviteModule = class(TIdSipMessageModule)
  private
    fInitialResendInterval:  Cardinal; // in milliseconds
    fProgressResendInterval: Cardinal; // in milliseconds

    procedure TurnIntoInvite(OutboundRequest: TIdSipRequest;
                             const Offer: String;
                             const OfferMimeType: String);
  public
    constructor Create(UA: TIdSipAbstractUserAgent); override;

    function Accept(Request: TIdSipRequest;
                    UsingSecureTransport: Boolean): TIdSipAction; override;
    function AcceptsMethods: String; override;
    function AddInboundInvite(Invite: TIdSipRequest;
                              UsingSecureTransport: Boolean): TIdSipInboundInvite;
    function CreateAck(Dialog: TIdSipDialog): TIdSipRequest;
    function CreateBye(Dialog: TIdSipDialog): TIdSipRequest;
    function CreateInvite(Dest: TIdSipAddressHeader;
                          const Body: String;
                          const MimeType: String): TIdSipRequest;
    function  CreateReInvite(Dialog: TIdSipDialog;
                             const Body: String;
                             const MimeType: String): TIdSipRequest;
    function WillAccept(Request: TIdSipRequest): Boolean; override;

    property InitialResendInterval:  Cardinal read fInitialResendInterval write fInitialResendInterval;
    property ProgressResendInterval: Cardinal read fProgressResendInterval write fProgressResendInterval;
  end;

  TIdSipOptionsModule = class(TIdSipMessageModule)
  public
    function Accept(Request: TIdSipRequest;
                    UsingSecureTransport: Boolean): TIdSipAction; override;
    function AcceptsMethods: String; override;
    function WillAccept(Request: TIdSipRequest): Boolean; override;
  end;

  // I implement that functionality necessary for a User Agent to respond to
  // REGISTER messages, that is, to act as a registrar.
  TIdSipRegisterModule = class(TIdSipMessageModule)
  private
    fBindingDB:         TIdSipAbstractBindingDatabase;
    fMinimumExpiryTime: Cardinal; // in seconds
  public
    function Accept(Request: TIdSipRequest;
                    UsingSecureTransport: Boolean): TIdSipAction; override;
    function AcceptsMethods: String; override;
    function WillAccept(Request: TIdSipRequest): Boolean; override;

    property BindingDB:         TIdSipAbstractBindingDatabase read fBindingDB write fBindingDB;
    property MinimumExpiryTime: Cardinal                      read fMinimumExpiryTime write fMinimumExpiryTime;
  end;

  // I implement that functionality necessary for a User Agent to issue REGISTER
  // messages, that is, to register with a registrar.
  TIdSipOutboundRegisterModule = class(TIdSipMessageModule)
  private
    fAutoReRegister: Boolean;
    fHasRegistrar:   Boolean;
    fRegistrar:      TIdSipUri;
    KnownRegistrars: TIdSipRegistrations;

    procedure SetRegistrar(Value: TIdSipUri);
  public
    constructor Create(UA: TIdSipAbstractUserAgent); override;
    destructor  Destroy; override;

    function  Accept(Request: TIdSipRequest;
                     UsingSecureTransport: Boolean): TIdSipAction; override;
    function  AcceptsMethods: String; override;
    procedure CleanUp; override;
    function  CreateRegister(Registrar: TIdSipToHeader): TIdSipRequest;
    function  CurrentRegistrationWith(Registrar: TIdSipUri): TIdSipOutboundRegistrationQuery;
    procedure OnReregister(Event: TObject);
    function  RegisterWith(Registrar: TIdSipUri): TIdSipOutboundRegister;
    function  RegistrationCount: Integer;
    function  UnregisterFrom(Registrar: TIdSipUri): TIdSipOutboundUnregister;
    function  WillAccept(Request: TIdSipRequest): Boolean; override;

    property AutoReRegister: Boolean   read fAutoReRegister write fAutoReRegister;
    property HasRegistrar:   Boolean   read fHasRegistrar write fHasRegistrar;
    property Registrar:      TIdSipUri read fRegistrar write SetRegistrar;
  end;

  // I represent an asynchronous message send between SIP entities - INVITEs,
  // REGISTERs and the like - where we care what the remote end answers.
  // With CANCELs and BYEs, for instance, we don't care how the remote end
  // answers.
  //
  // Owned actions are actions that other actions control. For example, Sessions
  // are Actions. Sessions use Invites (among other things), and Sessions
  // control those Invites. Thus, the Invites are Owned.
  //
  // Note that both in- and out-bound actions subclass Action. Thus this class
  // contains methods that are sometimes inapplicable to a particular action.
  TIdSipActionStatus = (asSuccess, asFailure, asInterim);
  TIdSipAction = class(TIdInterfacedObject)
  private
    fID:             String;
    fInitialRequest: TIdSipRequest;
    fIsOwned:        Boolean;
    fIsTerminated:   Boolean;
    fUA:             TIdSipAbstractUserAgent;
    NonceCount:      Cardinal;
    SentRequest:     Boolean;

    function  GetUsername: String;
    procedure SetUsername(const Value: String);
    function  TrySendRequest(Request: TIdSipRequest;
                             Targets: TIdSipLocations;
                             TryAgain: Boolean = true): Boolean;
  protected
    Listeners:       TIdNotificationList;
    TargetLocations: TIdSipLocations;

    procedure ActionSucceeded(Response: TIdSipResponse); virtual;
    procedure AddListeners(Listeners: TIdNotificationList);
    function  CreateNewAttempt: TIdSipRequest; virtual; abstract;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); virtual;
    procedure MarkAsTerminated; virtual;
    procedure NotifyOfFailure(Response: TIdSipResponse); virtual;
    procedure NotifyOfNetworkFailure(ErrorCode: Cardinal;
                                     const Reason: String); virtual;
    function  ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionStatus; virtual;
    function  ReceiveGlobalFailureResponse(Response: TIdSipResponse): TIdSipActionStatus; virtual;

    function  ReceiveOKResponse(Response: TIdSipResponse;
                                UsingSecureTransport: Boolean): TIdSipActionStatus; virtual;
    procedure ReceiveOtherRequest(Request: TIdSipRequest); virtual;
    function  ReceiveProvisionalResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): TIdSipActionStatus; virtual;
    function  ReceiveRedirectionResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): TIdSipActionStatus; virtual;
    function  ReceiveServerFailureResponse(Response: TIdSipResponse): TIdSipActionStatus; virtual;
    procedure SendRequest(Request: TIdSipRequest;
                          TryAgain: Boolean = true); virtual;
    procedure SendResponse(Response: TIdSipResponse); virtual;

    property UA: TIdSipAbstractUserAgent read fUA;
  public
    class function Method: String; virtual; abstract;

    constructor Create(UA: TIdSipAbstractUserAgent); virtual;
    constructor CreateInbound(UA: TIdSipAbstractUserAgent;
                              Request: TIdSipRequest;
                              UsingSecureTransport: Boolean); virtual;
    destructor  Destroy; override;

    function  IsInbound: Boolean; virtual;
    function  IsInvite: Boolean; virtual;
    function  IsOptions: Boolean; virtual;
    function  IsRegistration: Boolean; virtual;
    function  IsSession: Boolean; virtual;
    function  Match(Msg: TIdSipMessage): Boolean; virtual;
    procedure ReceiveRequest(Request: TIdSipRequest); virtual;
    procedure ReceiveResponse(Response: TIdSipResponse;
                              UsingSecureTransport: Boolean); virtual;
    procedure Send; virtual;
    procedure Terminate; virtual;

    property ID:             String        read fID;
    property InitialRequest: TIdSipRequest read fInitialRequest;
    property IsOwned:        Boolean       read fIsOwned;
    property IsTerminated:   Boolean       read fIsTerminated;
    property Username:       String        read GetUsername write SetUsername;
  end;

  // I provide basic facilities for all Actions that need to handle INVITEs, BYEs, CANCELs,
  TIdSipInviteBase = class(TIdSipAction)
  protected
    Module: TIdSipInviteModule;

    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    procedure ReceiveAck(Ack: TIdSipRequest); virtual;
    procedure ReceiveBye(Bye: TIdSipRequest); virtual;
    procedure ReceiveCancel(Cancel: TIdSipRequest); virtual;
    procedure ReceiveInvite(Invite: TIdSipRequest); virtual;
  public
    class function Method: String; override;

    procedure ReceiveRequest(Request: TIdSipRequest); override;
  end;

  TIdSipInvite = class(TIdSipInviteBase)
  protected
    function  CreateNewAttempt: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    function IsInvite: Boolean; override;
  end;

  // I encapsulate the call flows around an inbound INVITE, both in-dialog and
  // not.
  // As per section 13.3.1.4 of RFC 3261, a Session will resend a 2xx response
  // to an INVITE until it receives an ACK. Thus I provide an exponential
  // back-off timer starting with an interval of T1 milliseconds and capping
  // the interval at T2 milliseconds. InitialResend, MaxResendInterval and
  // ResendInterval provide the numbers, my UA provides the timer.
  //
  // I consider myself to have succeeded (in other words I call OnSuccess on my
  // listeners) once I receive an ACK to my 2xx response.
  TIdSipInboundInvite = class(TIdSipInvite)
  private
    fLocalMimeType:           String;
    fLocalSessionDescription: String;
    fLocalTag:                String;
    fMaxResendInterval:       Cardinal; // in milliseconds
    InviteModule:             TIdSipInviteModule;
    LastResponse:             TIdSipResponse;
    ReceivedAck:              Boolean;
    ResendInterval:           Cardinal;
    SentFinalResponse:        Boolean;

    function  GetInitialResendInterval: Cardinal;
    function  GetProgressResendInterval: Cardinal;
    procedure NotifyOfFailure; reintroduce; overload;
    procedure NotifyOfSuccess(Ack: TIdSipRequest);
    procedure ScheduleResendOk(Interval: Cardinal);
    procedure SendCancelResponse(Cancel: TIdSipRequest);
    procedure SendSimpleResponse(StatusCode: Cardinal);
  protected
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    procedure ReceiveAck(Ack: TIdSipRequest); override;
    procedure ReceiveCancel(Cancel: TIdSipRequest); override;
    procedure ReceiveInvite(Invite: TIdSipRequest); override;
    procedure SendResponse(Response: TIdSipResponse); override;
  public
    destructor Destroy; override;

    procedure Accept(const Offer, ContentType: String);
    procedure AddListener(const Listener: IIdSipInboundInviteListener);
    function  IsInbound: Boolean; override;
    function  Match(Msg: TIdSipMessage): Boolean; override;
    procedure Redirect(NewDestination: TIdSipAddressHeader;
                       Temporary: Boolean = true);
    procedure RejectCallBusy;
    procedure RemoveListener(const Listener: IIdSipInboundInviteListener);
    procedure ResendOk;
    procedure Ring;
    procedure SendSessionProgress;
    procedure Terminate; override;
    procedure TimeOut;

    property LocalSessionDescription: String   read fLocalSessionDescription;
    property LocalMimeType:           String   read fLocalMimeType;
    property LocalTag:                String   read fLocalTag write fLocalTag;
    property InitialResendInterval:   Cardinal read GetInitialResendInterval;
    property MaxResendInterval:       Cardinal read fMaxResendInterval write fMaxResendInterval;
    property ProgressResendInterval:  Cardinal read GetProgressResendInterval;
  end;

  // I encapsulate the call flows around an outbound INVITE, both in-dialog
  // and not.
  // I guarantee that I will notify my listeners of the OnDialogEvent before the
  // OnSuccess event.
  //
  // I consider myself to have succeeded (in other words I call OnSuccess on
  // my listeners) when I receive a 2xx response and have sent an ACK.
  TIdSipOutboundInvite = class(TIdSipInvite)
  private
    AnswerResponse:                 TIdSipResponse;
    Cancelling:                     Boolean;
    CancelRequest:                  TIdSipRequest;
    DialogEstablished:              Boolean;
    fDialog:                        TIdSipDialog;
    fInOutboundSession:             Boolean;
    fMimeType:                      String;
    fOffer:                         String;
    ReceivedFinalResponse:          Boolean;
    HasReceivedProvisionalResponse: Boolean;
    SentCancel:                     Boolean;

    procedure NotifyOfCallProgress(Response: TIdSipResponse);
    procedure NotifyOfDialogEstablished(Response: TIdSipResponse;
                                        UsingSecureTransport: Boolean);
    procedure NotifyOfRedirect(Response: TIdSipResponse);
    procedure NotifyOfSuccess(Response: TIdSipResponse);
    procedure SendAckFor(Response: TIdSipResponse;
                         UsingSecureTransport: Boolean);
    procedure SendBye(Response: TIdSipResponse;
                      UsingSecureTransport: Boolean);
    procedure SendCancel;
    procedure SetAckBody(Ack: TIdSipRequest);
  protected
    procedure ActionSucceeded(Response: TIdSipResponse); override;
    function  CreateInvite: TIdSipRequest; virtual; abstract;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    procedure NotifyOfFailure(Response: TIdSipResponse); override;
    function  ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionStatus; override;
    function  ReceiveGlobalFailureResponse(Response: TIdSipResponse): TIdSipActionStatus; override;
    function  ReceiveOKResponse(Response: TIdSipResponse;
                                UsingSecureTransport: Boolean): TIdSipActionStatus; override;
    function  ReceiveProvisionalResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): TIdSipActionStatus; override;
    function  ReceiveRedirectionResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): TIdSipActionStatus; override;
    function  ReceiveServerFailureResponse(Response: TIdSipResponse): TIdSipActionStatus; override;
  public
    destructor Destroy; override;

    procedure AddListener(const Listener: IIdSipInviteListener);
    procedure Cancel;
    function  Match(Msg: TIdSipMessage): Boolean; override;
    procedure RemoveListener(const Listener: IIdSipInviteListener);
    procedure Send; override;
    procedure SendAck(Dialog: TIdSipDialog;
                      FinalResponse: TIdSipResponse);
    procedure TransactionCompleted;
    procedure Terminate; override;

    property Dialog:            TIdSipDialog read fDialog write fDialog;
    property InOutboundSession: Boolean      read fInOutboundSession write fInOutboundSession;
    property Offer:             String       read fOffer write fOffer;
    property MimeType:          String       read fMimeType write fMimeType;
  end;

  TIdSipOutboundInitialInvite = class(TIdSipOutboundInvite)
  private
    fDestination: TIdSipAddressHeader;

    procedure SetDestination(Value: TIdSipAddressHeader);
  protected
    function  CreateInvite: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    destructor Destroy; override;

    property Destination: TIdSipAddressHeader read fDestination write SetDestination;
  end;

  TIdSipOutboundRedirectedInvite = class(TIdSipOutboundInvite)
  private
    fContact:        TIdSipAddressHeader;
    fOriginalInvite: TIdSipRequest;

    procedure SetContact(Value: TIdSipAddressHeader);
    procedure SetOriginalInvite(Value: TIdSipRequest);
  protected
    function  CreateInvite: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    destructor Destroy; override;

    property Contact:        TIdSipAddressHeader read fContact write SetContact;
    property OriginalInvite: TIdSipRequest       read fOriginalInvite write SetOriginalInvite;
  end;

  TIdSipOutboundReInvite = class(TIdSipOutboundInvite)
  private
    fOriginalInvite: TIdSipRequest;

    procedure SetOriginalInvite(Value: TIdSipRequest);
  protected
    function CreateInvite: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    destructor Destroy; override;

    property OriginalInvite: TIdSipRequest read fOriginalInvite write SetOriginalInvite;
  end;

  // Use me to send an INVITE with a Replace header (cf. RFC 3891), for
  // instance for call transfer (cf. RFC 3515 and
  // draft-ietf-sipping-cc-transfer).
  TIdSipOutboundReplacingInvite = class(TIdSipOutboundInitialInvite)
  private
    fCallID:  String;
    fFromTag: String;
    fToTag:   String;
  protected
    function CreateInvite: TIdSipRequest; override;
  public
    property CallID:  String read fCallID write fCallID;
    property FromTag: String read fFromTag write fFromTag;
    property ToTag:   String read fToTag write fToTag;
  end;

  TIdSipOptions = class(TIdSipAction)
  protected
    function CreateNewAttempt: TIdSipRequest; override;
  public
    class function Method: String; override;

    function IsOptions: Boolean; override;
  end;

  TIdSipInboundOptions = class(TIdSipOptions)
  public
    function  IsInbound: Boolean; override;
    procedure ReceiveRequest(Options: TIdSipRequest); override;
  end;

  TIdSipOutboundOptions = class(TIdSipOptions)
  private
    fServer: TIdSipAddressHeader;

    procedure NotifyOfResponse(Response: TIdSipResponse);
    procedure SetServer(Value: TIdSipAddressHeader);
  protected
    procedure ActionSucceeded(Response: TIdSipResponse); override;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    procedure NotifyOfFailure(Response: TIdSipResponse); override;
  public
    destructor Destroy; override;

    procedure AddListener(const Listener: IIdSipOptionsListener);
    procedure RemoveListener(const Listener: IIdSipOptionsListener);
    procedure Send; override;

    property Server: TIdSipAddressHeader read fServer write SetServer;
  end;

  TIdSipRegistration = class(TIdSipAction)
  protected
    OutModule: TIdSipOutboundRegisterModule;

    function CreateNewAttempt: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    class function Method: String; override;

    function IsRegistration: Boolean; override;
  end;

  TIdSipInboundRegistration = class(TIdSipRegistration)
  private
    RegisterModule: TIdSipRegisterModule;

    function  AcceptRequest(Request: TIdSipRequest): Boolean;
    function  BindingDB: TIdSipAbstractBindingDatabase;
    procedure RejectExpireTooBrief(Request: TIdSipRequest);
    procedure RejectFailedRequest(Request: TIdSipRequest);
    procedure RejectForbidden(Request: TIdSipRequest);
    procedure RejectNotFound(Request: TIdSipRequest);
    procedure RejectRequest(Request: TIdSipRequest;
                            StatusCode: Cardinal);
    procedure SendSimpleResponse(Request: TIdSipRequest;
                                StatusCode: Cardinal);
  protected
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    function IsInbound: Boolean; override;
    procedure ReceiveRequest(Register: TIdSipRequest); override;
  end;

  // I piggyback on a transaction in a blocking I/O fashion to provide a UAC
  // with a way to register with a registrar. I take care of things like
  // doing stuff with error responses, asking for authentication, etc.
  //
  // It makes no sense to access me once my Transaction has terminated. In
  // other words once you've received notification of my success or failure,
  // erase your references to me.
  TIdSipOutboundRegistration = class(TIdSipRegistration)
  private
    fBindings:  TIdSipContacts;
    fRegistrar: TIdSipUri;

    procedure ReissueRequestWithLongerExpiry(Registrar: TIdSipUri;
                                             MinimumExpiry: Cardinal);
    procedure RetryWithoutExtensions(Registrar: TIdSipUri;
                                     Response: TIdSipResponse);
    procedure SetBindings(Value: TIdSipContacts);
    procedure SetRegistrar(Value: TIdSipUri);
  protected
    procedure ActionSucceeded(Response: TIdSipResponse); override;
    function  CreateRegister(Registrar: TIdSipUri;
                             Bindings: TIdSipContacts): TIdSipRequest; virtual;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    procedure NotifyOfFailure(Response: TIdSipResponse); override;
    procedure NotifyOfSuccess(Response: TIdSipResponse); virtual;
    function  ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionStatus; override;
    function  ReceiveRedirectionResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): TIdSipActionStatus; override;
    procedure RegisterWith(Registrar: TIdSipUri;
                           Bindings: TIdSipContacts); overload;
    procedure RegisterWith(Registrar: TIdSipUri;
                           Contact: TIdSipContactHeader); overload;
    procedure SendRequest(Request: TIdSipRequest;
                          TryAgain: Boolean = true); overload; override;
    procedure Unregister(Registrar: TIdSipUri);
  public
    destructor Destroy; override;

    procedure AddListener(const Listener: IIdSipRegistrationListener);
    function  ReregisterTime(Expires: Cardinal): Cardinal;
    procedure RemoveListener(const Listener: IIdSipRegistrationListener);

    property Bindings:  TIdSipContacts read fBindings write SetBindings;
    property Registrar: TIdSipUri read fRegistrar write SetRegistrar;
  end;

  TIdSipOutboundRegistrationQuery = class(TIdSipOutboundRegistration)
  public
    procedure Send; override;
  end;

  TIdSipOutboundRegister = class(TIdSipOutboundRegistration)
  public
    procedure Send; override;
  end;

  TIdSipOutboundUnRegister = class(TIdSipOutboundRegistration)
  private
    fIsWildCard: Boolean;
  protected
    function  CreateRegister(Registrar: TIdSipUri;
                             Bindings: TIdSipContacts): TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    procedure Send; override;

    property IsWildCard: Boolean   read fIsWildCard write fIsWildCard;
  end;

  // I am a SIP session. As such, I represent both what my dialog represents
  // (a long-term relationship between two peers in a SIP network) and also
  // the media streams initiated between those peers.
  //
  // Note that when you call my Terminate method, my owning UserAgent will
  // destroy me, and your reference to me will no longer be valid. The same
  // thing goes for when I notify you that I have terminated via
  // OnEndedSession.
  TIdSipSession = class(TIdSipInviteBase,
                        IIdSipActionListener,
                        IIdSipInviteListener,
                        IIdSipInboundInviteListener)
  private
    DialogLock:                TCriticalSection;
    fDialog:                   TIdSipDialog;
    fLocalSessionDescription:  String;
    fLocalMimeType:            String;
    fReceivedAck:              Boolean;
    fRemoteSessionDescription: String;
    fRemoteMimeType:           String;
    LastModifyDescription:     String;
    LastModifyMimeType:        String;
    UsingSecureTransport:      Boolean;

    procedure NotifyOfModifiedSession(Answer: TIdSipResponse);
    procedure RejectOutOfOrderRequest(Request: TIdSipRequest);
    procedure RejectPrematureInvite(Invite: TIdSipRequest);
    procedure RejectReInvite(Invite: TIdSipRequest);
    procedure RejectRequest(Request: TIdSipRequest);
    procedure RescheduleModify(InviteAgent: TIdSipInvite);
    procedure TerminateAnyPendingRequests;
  protected
    FullyEstablished: Boolean;
    ModifyAttempt:    TIdSipInvite;
    ModifyLock:       TCriticalSection;

    procedure ActionSucceeded(Response: TIdSipResponse); override;
    function  CreateDialogIDFrom(Msg: TIdSipMessage): TIdSipDialogID; virtual; abstract;
    function  CreateNewAttempt: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    function  GetDialog: TIdSipDialog; virtual;
    function  GetInvite: TIdSipRequest; virtual;
    procedure NotifyOfEndedSession(ErrorCode: Cardinal;
                                   const Reason: String);
    procedure NotifyOfEstablishedSession(const RemoteSessionDescription: String;
                                         const MimeType: String);
    procedure NotifyOfFailure(Response: TIdSipResponse); overload; override;
    procedure NotifyOfModifySession(Modify: TIdSipInboundInvite);
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse); virtual;
    procedure OnCallProgress(InviteAgent: TIdSipOutboundInvite;
                             Response: TIdSipResponse); virtual;
    procedure OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                  NewDialog: TIdSipDialog); virtual;
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String); virtual;
    procedure OnFailure(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse;
                        const Reason: String); overload; virtual;
    procedure OnFailure(InviteAgent: TIdSipInboundInvite); overload; virtual;
    procedure OnRedirect(InviteAgent: TIdSipOutboundInvite;
                         Redirect: TIdSipResponse); virtual;
    procedure OnSuccess(InviteAgent: TIdSipInboundInvite;
                        Ack: TIdSipRequest); overload; virtual;
    procedure OnSuccess(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse); overload; virtual;
    procedure ReceiveBye(Bye: TIdSipRequest); override;
    procedure ReceiveInitialInvite(Invite: TIdSipRequest); virtual;
    procedure ReceiveInvite(Invite: TIdSipRequest); override;
    procedure SendBye; virtual;
  public
    class function Method: String; override;

    destructor Destroy; override;

    procedure AcceptModify(const LocalSessionDescription: String;
                           const MimeType: String);
    procedure AddSessionListener(const Listener: IIdSipSessionListener);
    function  IsEarly: Boolean;
    function  DialogEstablished: Boolean;
    function  DialogMatches(DialogID: TIdSipDialogID): Boolean; overload;
    function  DialogMatches(Msg: TIdSipMessage): Boolean; overload;
    function  IsOutboundCall: Boolean;
    function  IsSession: Boolean; override;
    function  ModificationInProgress: Boolean;
    procedure Modify(const Offer, ContentType: String);
    function  ModifyWaitTime: Cardinal; virtual;
    procedure ReceiveRequest(Request: TIdSipRequest); override;
    procedure Remodify;
    procedure RemoveSessionListener(const Listener: IIdSipSessionListener);

    property Dialog:                   TIdSipDialog read GetDialog;
    property LocalSessionDescription:  String       read fLocalSessionDescription write fLocalSessionDescription;
    property LocalMimeType:            String       read fLocalMimeType write fLocalMimeType;
    property ReceivedAck:              Boolean      read fReceivedAck;
    property RemoteSessionDescription: String       read fRemoteSessionDescription write fRemoteSessionDescription;
    property RemoteMimeType:           String       read fRemoteMimeType write fRemoteMimeType;
  end;

  TIdSipInboundSession = class(TIdSipSession)
  private
    InitialInvite: TIdSipInboundInvite;

    function CreateInboundDialog(const LocalTag: String): TIdSipDialog;
  protected
    function  CreateDialogIDFrom(Msg: TIdSipMessage): TIdSipDialogID; override;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    procedure OnFailure(InviteAgent: TIdSipInboundInvite); override;
    procedure OnSuccess(InviteAgent: TIdSipInboundInvite;
                        Ack: TIdSipRequest); override;
    procedure OnSuccess(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse); override;
    procedure ReceiveCancel(Cancel: TIdSipRequest); override;
    procedure ReceiveInitialInvite(Invite: TIdSipRequest); override;
  public
    function  AcceptCall(const Offer, ContentType: String): String;
    function  IsInbound: Boolean; override;
    function  Match(Msg: TIdSipMessage): Boolean; override;
    function  ModifyWaitTime: Cardinal; override;
    procedure RedirectCall(NewDestination: TIdSipAddressHeader);
    procedure RejectCallBusy;
    procedure Ring;
    procedure Terminate; override;
  end;

  // Outbound Sessions behave somewhat differently to inbound Sessions, even
  // disregarding the direction of a call. When you make an outbound call,
  // you could get a redirection response (a 3xx). By this stage you have a
  // fully established dialog. When you make a new call to the contact in the
  // 3xx, you use the same Call-ID and From tag, but you'll establish a WHOLE
  // NEW dialog. As a result, DialogEstablished can switch from false to true
  // to false to true etc etc. And the Dialog property can return completely
  // different objects, possibly landing you in hot water (a la dangling
  // pointer style) should you keep a reference to the Dialog property.
  TIdSipOutboundSession = class(TIdSipSession)
  private
    fDestination:         TIdSipAddressHeader;
    InitialInvite:        TIdSipOutboundInitialInvite;
    TargetUriSet:         TIdSipContacts;
    RedirectedInvites:    TObjectList;
    RedirectedInviteLock: TCriticalSection;

    procedure AddNewRedirect(OriginalInvite: TIdSipRequest;
                             Contact: TIdSipContactHeader);
    procedure InitialiseUsing(OutboundInviteType: TIdSipActionClass);
    function  NoMoreRedirectedInvites: Boolean;
    procedure RemoveFinishedRedirectedInvite(InviteAgent: TIdSipAction);
    procedure SetDestination(Value: TIdSipAddressHeader);
    procedure TerminateAllRedirects;
  protected
    function  CreateDialogIDFrom(Msg: TIdSipMessage): TIdSipDialogID; override;
    procedure OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                  NewDialog: TIdSipDialog); override;
    procedure OnFailure(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse;
                        const Reason: String); override;
    procedure OnRedirect(InviteAgent: TIdSipOutboundInvite;
                         Redirect: TIdSipResponse); override;
    procedure OnSuccess(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse); override;
    procedure SendBye; override;
  public
    constructor Create(UA: TIdSipAbstractUserAgent); override;
    constructor CreateSessionReplacer(UA: TIdSipAbstractUserAgent;
                                      Session: TIdSipSession);
    destructor Destroy; override;

    procedure Cancel;
    function  CanForkOn(Response: TIdSipResponse): Boolean;
    function  Match(Msg: TIdSipMessage): Boolean; override;
    function  ModifyWaitTime: Cardinal; override;
    procedure Send; override;
    procedure Terminate; override;

    property Destination: TIdSipAddressHeader read fDestination write SetDestination;
  end;

  TIdSipInboundInviteExpire = class(TIdSipActionClosure)
  public
    procedure Execute(Action: TIdSipAction); override;
  end;

  TIdSipInboundInviteResendOk = class(TIdSipActionClosure)
  public
    procedure Execute(Action: TIdSipAction); override;
  end;

  TIdSipInboundInviteSessionProgress = class(TIdSipActionClosure)
  public
    procedure Execute(Action: TIdSipAction); override;
  end;

  TIdSipOutboundInviteTransactionComplete = class(TIdSipActionClosure)
  public
    procedure Execute(Action: TIdSipAction); override;
  end;

  TIdSipSessionResendReInvite = class(TIdSipActionClosure)
  public
    procedure Execute(Action: TIdSipAction); override;
  end;

  TIdSipActionNetworkFailureMethod = class(TIdNotification)
  private
    fActionAgent: TIdSipAction;
    fErrorCode:   Cardinal;
    fReason:      String;
  public
    procedure Run(const Subject: IInterface); override;

    property ActionAgent: TIdSipAction read fActionAgent write fActionAgent;
    property ErrorCode:   Cardinal     read fErrorCode write fErrorCode;
    property Reason:      String       read fReason write fReason;
  end;

  TIdSipInviteMethod = class(TIdNotification)
  private
    fInvite: TIdSipInboundInvite;
  public
    property Invite: TIdSipInboundInvite read fInvite write fInvite;
  end;

  TIdSipInboundInviteFailureMethod = class(TIdSipInviteMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSipInboundInviteSuccessMethod = class(TIdSipInviteMethod)
  private
    fAck: TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property Ack: TIdSipRequest read fAck write fAck;
  end;

  TIdSipOutboundInviteMethod = class(TIdSipInviteMethod)
  private
    fInvite:   TIdSipOutboundInvite;
    fResponse: TIdSipResponse;
  public
    property Invite:   TIdSipOutboundInvite read fInvite write fInvite;
    property Response: TIdSipResponse read fResponse write fResponse;
  end;

  TIdSipInviteCallProgressMethod = class(TIdSipOutboundInviteMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSipInviteDialogEstablishedMethod = class(TIdSipOutboundInviteMethod)
  private
    fDialog: TIdSipDialog;
  public
    procedure Run(const Subject: IInterface); override;

    property Dialog: TIdSipDialog read fDialog write fDialog;
  end;

  TIdSipInviteFailureMethod = class(TIdSipOutboundInviteMethod)
  private
    fReason: String;
  public
    procedure Run(const Subject: IInterface); override;

    property Reason: String read fReason write fReason;
  end;

  TIdSipInviteRedirectMethod = class(TIdSipOutboundInviteMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSipInviteSuccessMethod = class(TIdSipOutboundInviteMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSipOptionsResponseMethod = class(TIdNotification)
  private
    fOptions:  TIdSipOutboundOptions;
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Options:  TIdSipOutboundOptions read fOptions write fOptions;
    property Response: TIdSipResponse        read fResponse write fResponse;
  end;

  TIdSipRegistrationMethod = class(TIdNotification)
  private
    fCurrentBindings: TIdSipContacts;
    fRegistration:    TIdSipOutboundRegistration;
  public
    property CurrentBindings: TIdSipContacts             read fCurrentBindings write fCurrentBindings;
    property Registration:    TIdSipOutboundRegistration read fRegistration write fRegistration;
  end;

  TIdSipRegistrationFailedMethod = class(TIdSipRegistrationMethod)
  private
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Response: TIdSipResponse read fResponse write fResponse;
  end;

  TIdSipRegistrationSucceededMethod = class(TIdSipRegistrationMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSipSessionMethod = class(TIdNotification)
  private
    fSession: TIdSipSession;
  public
    property Session: TIdSipSession read fSession write fSession;
  end;

  TIdSipEndedSessionMethod = class(TIdSipSessionMethod)
  private
    fErrorCode: Cardinal;
    fReason:    String;
  public
    procedure Run(const Subject: IInterface); override;

    property ErrorCode: Cardinal read fErrorCode write fErrorCode;
    property Reason:    String   read fReason write fReason;
  end;

  TIdSipEstablishedSessionMethod = class(TIdSipSessionMethod)
  private
    fMimeType:                 String;
    fRemoteSessionDescription: String;
  public
    procedure Run(const Subject: IInterface); override;

    property MimeType:                 String read fMimeType write fMimeType;
    property RemoteSessionDescription: String read fRemoteSessionDescription write fRemoteSessionDescription;
  end;

  TIdSipModifiedSessionMethod = class(TIdSipSessionMethod)
  private
    fAnswer: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Answer: TIdSipResponse read fAnswer write fAnswer;
  end;

  TIdSipProgressedSessionMethod = class(TIdSipSessionMethod)
  private
    fProgress: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Progress: TIdSipResponse read fProgress write fProgress;
  end;

  // We subclass TIdSipEstablishedSessionMethod solely for reusing
  // property declarations.
  TIdSipSessionModifySessionMethod = class(TIdSipEstablishedSessionMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSipUserAgentMethod = class(TIdNotification)
  private
    fUserAgent: TIdSipAbstractUserAgent;
  public
    property UserAgent: TIdSipAbstractUserAgent read fUserAgent write fUserAgent;
  end;

  // Ask the listeners for a username/password pair. First listener to set
  // either Password or Username wins.
  TIdSipUserAgentAuthenticationChallengeMethod = class(TIdSipUserAgentMethod)
  private
    fChallenge:     TIdSipResponse;
    fFirstPassword: String;
    fFirstUsername: String;
    fTryAgain:      Boolean;
  public
    procedure Run(const Subject: IInterface); override;

    property Challenge:     TIdSipResponse          read fChallenge write fChallenge;
    property FirstPassword: String                  read fFirstPassword write fFirstPassword;
    property FirstUsername: String                  read fFirstUsername write fFirstUsername;
    property TryAgain:      Boolean                 read fTryAgain write fTryAgain;
  end;

  TIdSipUserAgentDroppedUnmatchedMessageMethod = class(TIdSipUserAgentMethod)
  private
    fReceiver: TIdSipTransport;
    fMessage:  TIdSipMessage;
  public
    procedure Run(const Subject: IInterface); override;

    property Receiver: TIdSipTransport read fReceiver write fReceiver;
    property Message:  TIdSipMessage  read fMessage write fMessage;
  end;

  TIdSipUserAgentInboundCallMethod = class(TIdSipUserAgentMethod)
  private
    fSession: TIdSipInboundSession;
  public
    procedure Run(const Subject: IInterface); override;

    property Session: TIdSipInboundSession read fSession write fSession;
  end;

  EIdSipBadSyntax = class(EIdException);
  EIdSipRegistrarNotFound = class(EIdException)
  public
    constructor Create(const Msg: string); reintroduce;
  end;
  EIdSipTransactionUser = class(EIdException);

// Stack error codes
const
  NoError                        = 0;
  BusyHere                       = NoError;
  CallRedirected                 = NoError;
  LocalHangUp                    = NoError;
  InboundActionFailed            = NoError + 1;
  NoLocationFound                = NoError + 2;
  NoLocationSucceeded            = NoError + 3;
  RedirectWithNoContacts         = NoError + 4;
  RedirectWithNoMoreTargets      = NoError + 5;
  RedirectWithNoSuccess          = NoError + 6;
  RemoteCancel                   = NoError;
  RemoteHangUp                   = NoError;

// Configuration file constants
const
  AuthenticationDirective = 'Authentication';
  AutoKeyword             = 'AUTO';
  ContactDirective        = ContactHeaderFull;
  FromDirective           = FromHeaderFull;
  ListenDirective         = 'Listen';
  MockKeyword             = 'MOCK';
  NameServerDirective     = 'NameServer';
  ProxyDirective          = 'Proxy';
  RegisterDirective       = 'Register';

// Generally useful constants
const
  BadAuthorizationTokens     = 'Bad Authorization tokens';
  MalformedConfigurationLine = 'Malformed configuration line: %s';
  MaximumUDPMessageSize      = 1300;
  MaxPrematureInviteRetry    = 10;
  MissingContactHeader       = 'Missing Contact Header';
  OneMinute                  = 60;
  OneHour                    = 60*OneMinute;
  FiveMinutes                = 5*OneMinute;
  TwentyMinutes              = 20*OneMinute;

implementation

uses
  IdHashMessageDigest, IdSimpleParser, IdSipConsts, IdSipIndyLocator,
  IdSipMockLocator, IdRandom, IdSdp, IdSystem, IdUnicode, Math, SysUtils;

const
  ItemNotFoundIndex = -1;

const
  RSBusyHere                  = 'Incoming call rejected - busy here';
  RSCallRedirected            = 'Incoming call redirected';
  RSLocalHangUp               = 'Local end hung up';
  RSInboundActionFailed       = 'For an inbound %s, sending a response failed because: %s';
  RSNoLocationFound           = 'No destination addresses found for URI %s';
  RSNoLocationSucceeded       = 'Attempted message sends to all destination addresses failed for URI %s';
  RSNoReason                  = '';
  RSRedirectWithNoContacts    = 'Call redirected to nowhere';
  RSRedirectWithNoMoreTargets = 'Call redirected but no more targets';
  RSRedirectWithNoSuccess     = 'Call redirected but no target answered';
  RSRemoteCancel              = 'Remote end cancelled call';
  RSRemoteHangUp              = 'Remote end hung up';

// Exception messages
const
  CannotModifyBeforeEstablished  = 'Cannot modify a session before it''s fully established';
  CannotModifyDuringModification = 'Cannot modify a session while a modification is in progress';
  MethodInProgress               = 'A(n) %s is already in progress';
  NoSuchRegistrar                = 'No such registrar known: %s';
  OutboundActionFailed           = 'An outbound %s failed because: %s';
  PrematureInviteMessage         = 'Don''t attempt to modify the session before it''s fully established';

//******************************************************************************
//* TIdSipActionClosure                                                        *
//******************************************************************************
//* TIdSipActionClosure Public methods *****************************************

procedure TIdSipActionClosure.Execute(Action: TIdSipAction);
begin
end;

//******************************************************************************
//* TIdSipAbstractCore                                                         *
//******************************************************************************
//* TIdSipAbstractCore Public methods ******************************************

constructor TIdSipAbstractCore.Create;
begin
  inherited Create;

  Self.Observed  := TIdObservable.Create;
  Self.TimerLock := TCriticalSection.Create;

  Self.UserAgentListeners := TIdNotificationList.Create;
  Self.UserAgentListeners.AddExpectedException(EParserError);

  Self.HostName              := Self.DefaultHostName;
  Self.Realm                 := Self.HostName;
  Self.RequireAuthentication := false;
end;

destructor TIdSipAbstractCore.Destroy;
begin
  Self.UserAgentListeners.Free;
  Self.TimerLock.Free;
  Self.Observed.Free;

  inherited Destroy;
end;

procedure TIdSipAbstractCore.AddObserver(const Listener: IIdObserver);
begin
  Self.Observed.AddObserver(Listener);
end;

procedure TIdSipAbstractCore.AddUserAgentListener(const Listener: IIdSipUserAgentListener);
begin
  Self.UserAgentListeners.AddListener(Listener);
end;

function TIdSipAbstractCore.Authenticate(Request: TIdSipRequest): Boolean;
begin
  // We should ALWAYS have an authenticator attached: see TIdSipStackConfigurator.
  Result := Assigned(Self.Authenticator) and Self.Authenticator.Authenticate(Request);
end;

function TIdSipAbstractCore.CreateChallengeResponse(Request: TIdSipRequest): TIdSipResponse;
begin
  Result := Self.Authenticator.CreateChallengeResponse(Request);
  Self.PrepareResponse(Result, Request);
end;

function TIdSipAbstractCore.CreateChallengeResponseAsUserAgent(Request: TIdSipRequest): TIdSipResponse;
begin
  Result := Self.Authenticator.CreateChallengeResponseAsUserAgent(Request);
  Self.PrepareResponse(Result, Request);
end;

function TIdSipAbstractCore.CreateResponse(Request: TIdSipRequest;
                                           ResponseCode: Cardinal): TIdSipResponse;
begin
  Result := TIdSipResponse.InResponseTo(Request,
                                        ResponseCode);

  Self.PrepareResponse(Result, Request);
end;

function TIdSipAbstractCore.NextCallID: String;
begin
  Result := GRandomNumber.NextHexString + '@' + Self.HostName;
end;

function TIdSipAbstractCore.NextNonce: String;
begin
  Result := GRandomNumber.NextHexString;
end;

function TIdSipAbstractCore.NextTag: String;
begin
  Result := GRandomNumber.NextSipUserAgentTag;
end;

procedure TIdSipAbstractCore.RemoveObserver(const Listener: IIdObserver);
begin
  Self.Observed.RemoveObserver(Listener);
end;

procedure TIdSipAbstractCore.RemoveUserAgentListener(const Listener: IIdSipUserAgentListener);
begin
  Self.UserAgentListeners.RemoveListener(Listener);
end;

procedure TIdSipAbstractCore.ScheduleEvent(Event: TNotifyEvent;
                                           WaitTime: Cardinal;
                                           Msg: TIdSipMessage);
var
  RequestEvent: TIdSipMessageNotifyEventWait;
begin
  Self.TimerLock.Acquire;
  try
    if Assigned(Self.Timer) then begin
      RequestEvent := TIdSipMessageNotifyEventWait.Create;
      RequestEvent.Message := Msg;
      RequestEvent.Event   := Event;
      Self.Timer.AddEvent(WaitTime, RequestEvent);
    end;
  finally
    Self.TimerLock.Release;
  end;
end;

procedure TIdSipAbstractCore.SendRequest(Request: TIdSipRequest;
                                         Dest: TIdSipLocation);
begin
  Self.MaybeChangeTransport(Request);

  Self.Dispatcher.SendRequest(Request, Dest);
end;

procedure TIdSipAbstractCore.SendResponse(Response: TIdSipResponse);
begin
  Self.MaybeChangeTransport(Response);

  Self.Dispatcher.SendResponse(Response);
end;

//* TIdSipAbstractCore Protected methods ***************************************

procedure TIdSipAbstractCore.ActOnRequest(Request: TIdSipRequest;
                                          Receiver: TIdSipTransport);
begin
  // By default do nothing
end;

procedure TIdSipAbstractCore.ActOnResponse(Response: TIdSipResponse;
                                           Receiver: TIdSipTransport);
begin
  // By default do nothing
end;

procedure TIdSipAbstractCore.MaybeChangeTransport(Msg: TIdSipMessage);
var
  MsgLen:       Cardinal;
  RewrittenVia: Boolean;
begin
  MsgLen := Length(Msg.AsString);
  RewrittenVia := (MsgLen > MaximumUDPMessageSize)
              and (Msg.LastHop.Transport = UdpTransport);

  if RewrittenVia then
    Msg.LastHop.Transport := TcpTransport;
end;

procedure TIdSipAbstractCore.NotifyOfChange;
begin
  Self.Observed.NotifyListenersOfChange(Self);
end;

procedure TIdSipAbstractCore.OnAuthenticationChallenge(Dispatcher: TIdSipTransactionDispatcher;
                                                       Challenge: TIdSipResponse;
                                                       ChallengeResponse: TIdSipRequest;
                                                       var TryAgain: Boolean);
begin
  // Usually we want to re-issue a challenged request.

  TryAgain := true;
end;

procedure TIdSipAbstractCore.OnReceiveRequest(Request: TIdSipRequest;
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

procedure TIdSipAbstractCore.OnReceiveResponse(Response: TIdSipResponse;
                                               Receiver: TIdSipTransport);
begin
  if (Self.WillAcceptResponse(Response) = uarAccept) then
    Self.ActOnResponse(Response, Receiver);
end;

procedure TIdSipAbstractCore.PrepareResponse(Response: TIdSipResponse;
                                             Request: TIdSipRequest);
begin
  if not Request.ToHeader.HasTag then
    Response.ToHeader.Tag := Self.NextTag;

  if (Self.UserAgentName <> '') then
    Response.AddHeader(ServerHeader).Value := Self.UserAgentName;
end;

procedure TIdSipAbstractCore.RejectBadRequest(Request: TIdSipRequest;
                                              const Reason: String);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPBadRequest);
  try
    Response.StatusText := Reason;

    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractCore.RejectRequest(Reaction: TIdSipUserAgentReaction;
                                           Request: TIdSipRequest);
begin
 case Reaction of
   uarUnauthorized:     Self.RejectRequestUnauthorized(Request);
   uarBadAuthorization: Self.RejectBadAuthorization(Request);
 end;
end;

procedure TIdSipAbstractCore.RejectRequestUnauthorized(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateChallengeResponse(Request);
  try
    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractCore.SetAuthenticator(Value: TIdSipAbstractAuthenticator);
begin
  Self.fAuthenticator := Value;
  Self.fAuthenticator.Realm := Self.Realm;

  Self.fAuthenticator.IsProxy := false;
end;

function TIdSipAbstractCore.WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  Result := uarAccept;

  if Self.RequireAuthentication then begin
    try
      if not Self.Authenticate(Request) then
        Result := uarUnauthorized;
    except
      on EAuthenticate do
        Result := uarBadAuthorization;
    end;
  end;
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

procedure TIdSipAbstractCore.RejectBadAuthorization(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPBadRequest);
  try
    Response.StatusText := BadAuthorizationTokens;

    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractCore.SetDispatcher(Value: TIdSipTransactionDispatcher);
begin
  fDispatcher := Value;

  fDispatcher.AddTransactionDispatcherListener(Self);
end;

procedure TIdSipAbstractCore.SetRealm(const Value: String);
begin
  Self.fRealm := Value;

  if Assigned(Self.Authenticator) then
    Self.Authenticator.Realm := Self.Realm;
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

//* TIdSipRegistrationInfo Private methods *************************************

procedure TIdSipRegistrationInfo.SetRegistrar(Value: TIdSipUri);
begin
  Self.fRegistrar.Uri := Value.Uri;
end;

//******************************************************************************
//* TIdSipRegistrations                                                        *
//******************************************************************************
//* TIdSipRegistrations Public methods *****************************************

constructor TIdSipRegistrations.Create;
begin
  inherited Create;

  Self.Lock            := TCriticalSection.Create;
  Self.KnownRegistrars := TObjectList.Create(true);
end;

destructor TIdSipRegistrations.Destroy;
begin
  Self.Lock.Acquire;
  try
    Self.KnownRegistrars.Free;
  finally
    Self.Lock.Release;
  end;
  Self.Lock.Free;

  inherited Destroy;
end;

procedure TIdSipRegistrations.AddKnownRegistrar(Registrar: TIdSipUri;
                                                const CallID: String;
                                                SequenceNo: Cardinal);
var
  NewReg: TIdSipRegistrationInfo;
begin
  Self.Lock.Acquire;
  try
    if not Self.KnowsRegistrar(Registrar) then begin
      NewReg := TIdSipRegistrationInfo.Create;
      Self.KnownRegistrars.Add(NewReg);

      NewReg.CallID     := CallID;
      NewReg.Registrar  := Registrar;
      NewReg.SequenceNo := SequenceNo;
    end;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipRegistrations.CallIDFor(Registrar: TIdSipUri): String;
var
  Index: Integer;
begin
  Self.Lock.Acquire;
  try
    Index := Self.IndexOfRegistrar(Registrar);

    if (Index = ItemNotFoundIndex) then
      raise EIdSipRegistrarNotFound.Create(Registrar.Uri);

    Result := Self.RegistrarAt(Index).CallID;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipRegistrations.NextSequenceNoFor(Registrar: TIdSipUri): Cardinal;
var
  Index:   Integer;
  RegInfo: TIdSipRegistrationInfo;
begin
  Result := 0;

  Self.Lock.Acquire;
  try
    Index := Self.IndexOfRegistrar(Registrar);

    if (Index = ItemNotFoundIndex) then
      raise EIdSipRegistrarNotFound.Create(Registrar.Uri);

    RegInfo := Self.RegistrarAt(Index);
    Result := RegInfo.SequenceNo;
    RegInfo.SequenceNo := Result + 1;
  finally
    Self.Lock.Release;
  end;
end;

//* TIdSipRegistrations Private methods ****************************************

function TIdSipRegistrations.IndexOfRegistrar(Registrar: TIdSipUri): Integer;
begin
  Result := 0;
  while (Result < Self.KnownRegistrars.Count) do
    if Self.RegistrarAt(Result).Registrar.Equals(Registrar) then
      Break
    else
      Inc(Result);

  if (Result >= Self.KnownRegistrars.Count) then
    Result := ItemNotFoundIndex;
end;

function TIdSipRegistrations.KnowsRegistrar(Registrar: TIdSipUri): Boolean;
begin
  Result := Self.IndexOfRegistrar(Registrar) <> ItemNotFoundIndex;
end;

function TIdSipRegistrations.RegistrarAt(Index: Integer): TIdSipRegistrationInfo;
begin
  Result := Self.KnownRegistrars[Index] as TIdSipRegistrationInfo;
end;

//******************************************************************************
//* TIdSipActions                                                              *
//******************************************************************************
//* TIdSipActions Public methods ***********************************************

constructor TIdSipActions.Create;
begin
  inherited Create;

  Self.ActionLock := TCriticalSection.Create;
  Self.Actions    := TObjectList.Create;

  Self.Observed := TIdObservable.Create;
end;

destructor TIdSipActions.Destroy;
begin
  Self.Observed.Free;

  Self.LockActions;
  try
    Self.Actions.Free;
  finally
    Self.UnlockActions;
  end;
  Self.ActionLock.Free;

  inherited Destroy;
end;

function TIdSipActions.Add(Action: TIdSipAction): TIdSipAction;
begin
  Result := Action;

  Self.LockActions;
  try
    try
      Self.Actions.Add(Action);
    except
      if (Self.Actions.IndexOf(Action) <> ItemNotFoundIndex) then
        Self.Actions.Remove(Action)
      else
        Action.Free;

        Result := nil;
      raise;
    end;
  finally
    Self.UnlockActions;
  end;

  Self.Observed.NotifyListenersOfChange;
end;

procedure TIdSipActions.AddObserver(const Listener: IIdObserver);
begin
  Self.Observed.AddObserver(Listener);
end;

function TIdSipActions.AddOutboundAction(UserAgent: TIdSipAbstractUserAgent;
                                         ActionType: TIdSipActionClass): TIdSipAction;
begin
  Self.LockActions;
  try
    Result := Self.Add(ActionType.Create(UserAgent));
  finally
    Self.UnlockActions;
  end;
end;

procedure TIdSipActions.CleanOutTerminatedActions;
var
  Changed:      Boolean;
  I:            Integer;
  InitialCount: Integer;
begin
  Self.LockActions;
  try
    InitialCount := Self.Actions.Count;

    I := 0;
    while (I < Self.Actions.Count) do
      if Self.ActionAt(I).IsTerminated then
        Self.Actions.Delete(I)
      else
        Inc(I);

    Changed := InitialCount <> Self.Actions.Count;
  finally
    Self.UnlockActions;
  end;

  if Changed then
    Self.Observed.NotifyListenersOfChange;
end;

function TIdSipActions.Count: Integer;
begin
  // Return the number of actions, both terminated and ongoing.
  Self.LockActions;
  try
    Result := Self.Actions.Count;
  finally
    Self.UnlockActions;
  end;
end;

function TIdSipActions.CountOf(const MethodName: String): Integer;
var
  I: Integer;
begin
  // Return the number of ongoing (non-session) actions of type MethodName.
  Self.LockActions;
  try
    Result := 0;

    // We don't count Sessions because Sessions contain other Actions - they
    // look and act more like containers of Actions than Actions themselves.
    for I := 0 to Self.Actions.Count - 1 do
      if not Self.ActionAt(I).IsSession
        and (Self.ActionAt(I).Method = MethodName)
        and not Self.ActionAt(I).IsTerminated then Inc(Result);
  finally
    Self.UnlockActions;
  end;
end;

procedure TIdSipActions.FindActionAndPerform(const ID: String;
                                             Block: TIdSipActionClosure);
var
  NullBlock: TIdSipActionClosure;
begin
  NullBlock := TIdSipActionClosure.Create;
  try
    Self.FindActionAndPerformOr(ID, Block, NullBlock);
  finally
    NullBlock.Free;
  end;
end;

procedure TIdSipActions.FindActionAndPerform(Msg: TIdSipMessage;
                                             Block: TIdSipActionClosure);
var
  NullBlock: TIdSipActionClosure;
begin
  NullBlock := TIdSipActionClosure.Create;
  try
    Self.FindActionAndPerformOr(Msg, Block, NullBlock);
  finally
    NullBlock.Free;
  end;
end;

procedure TIdSipActions.FindActionAndPerformOr(const ID: String;
                                               FoundBlock: TIdSipActionClosure;
                                               NotFoundBlock: TIdSipActionClosure);
var
  Action: TIdSipAction;
begin
  Self.LockActions;
  try
    Action := Self.FindAction(ID);

    if Assigned(Action) then
      FoundBlock.Execute(Action)
    else
      NotFoundBlock.Execute(nil);
  finally
    Self.UnlockActions;
  end;

  Self.CleanOutTerminatedActions;
end;

procedure TIdSipActions.FindActionAndPerformOr(Msg: TIdSipMessage;
                                               FoundBlock: TIdSipActionClosure;
                                               NotFoundBlock: TIdSipActionClosure);
var
  Action: TIdSipAction;
begin
  Self.LockActions;
  try
    Action := Self.FindAction(Msg);

    if Assigned(Action) then
      FoundBlock.Execute(Action)
    else
      NotFoundBlock.Execute(nil);
  finally
    Self.UnlockActions;
  end;

  Self.CleanOutTerminatedActions;
end;

function TIdSipActions.InviteCount: Integer;
begin
  Result := Self.CountOf(MethodInvite);
end;

function TIdSipActions.NextActionID: String;
var
  I:             Integer;
  NoActionHasID: Boolean;
begin
  // Produce a token such that no action has that token as an ID.

  Self.ActionLock.Acquire;
  try
    repeat
      Result := GRandomNumber.NextHexString;

      NoActionHasID := true;
      for I := 0 to Self.Actions.Count - 1 do
        NoActionHasID := NoActionHasID and (Result <> Self.ActionAt(I).ID);
    until NoActionHasID;
  finally
    Self.ActionLock.Release;
  end;
end;

function TIdSipActions.OptionsCount: Integer;
begin
  Result := Self.CountOf(MethodOptions);
end;

procedure TIdSipActions.Perform(Msg: TIdSipMessage; Block: TIdSipActionClosure);
var
  Action: TIdSipAction;
begin
  // Find the action, and execute Block regardless of whether we found the
  // action. FindAction returns nil in this case.

  Self.LockActions;
  try
    Action := Self.FindAction(Msg);

    Block.Execute(Action);
  finally
    Self.UnlockActions;
  end;

  Self.CleanOutTerminatedActions;
end;

function TIdSipActions.RegistrationCount: Integer;
begin
  Result := Self.CountOf(MethodRegister);
end;

procedure TIdSipActions.RemoveObserver(const Listener: IIdObserver);
begin
  Self.Observed.RemoveObserver(Listener);
end;

function TIdSipActions.SessionCount: Integer;
var
  I: Integer;
begin
  // Return the number of ongoing Sessions
  Self.LockActions;
  try
    Result := 0;

    for I := 0 to Self.Actions.Count - 1 do
      if Self.ActionAt(I).IsSession
        and not Self.ActionAt(I).IsTerminated then
        Inc(Result);
  finally
    Self.UnlockActions;
  end;
end;

procedure TIdSipActions.TerminateAllActions;
var
  I: Integer;
begin
  Self.LockActions;
  try
    for I := 0 to Self.Actions.Count - 1 do
      if not Self.ActionAt(I).IsOwned
        and not Self.ActionAt(I).IsTerminated then
        Self.ActionAt(I).Terminate;
  finally
    Self.UnlockActions;
  end;
end;

//* TIdSipActions Private methods **********************************************

function TIdSipActions.ActionAt(Index: Integer): TIdSipAction;
begin
  // Precondition: you've invoked Self.LockActions
  Result := Self.Actions[Index] as TIdSipAction;
end;

function TIdSipActions.FindAction(Msg: TIdSipMessage): TIdSipAction;
var
  Action: TIdSipAction;
  I:      Integer;
begin
  // Precondition: You've locked Self.ActionLock.
  Result := nil;

  I := 0;
  while (I < Self.Actions.Count) and not Assigned(Result) do begin
    Action := Self.Actions[I] as TIdSipAction;
    if not Action.IsTerminated and Action.Match(Msg) then
      Result := Action
    else
      Inc(I);
  end;
end;

function TIdSipActions.FindAction(const ActionID: String): TIdSipAction;
var
  Action: TIdSipAction;
  I:      Integer;
begin
  // Precondition: You've locked Self.ActionLock.
  Result := nil;

  I := 0;
  while (I < Self.Actions.Count) and not Assigned(Result) do begin
    Action := Self.Actions[I] as TIdSipAction;
    if not Action.IsTerminated and (Action.ID = ActionID) then
      Result := Action
    else
      Inc(I);
  end;
end;

procedure TIdSipActions.LockActions;
begin
  Self.ActionLock.Acquire;
end;

procedure TIdSipActions.UnlockActions;
begin
  Self.ActionLock.Release;
end;

//******************************************************************************
//* TIdSipActionsWait                                                          *
//******************************************************************************
//* TIdSipActionsWait Public methods *******************************************

procedure TIdSipActionsWait.Trigger;
var
  Block: TIdSipActionClosure;
begin
  Block := Self.BlockType.Create;
  try
    Self.Actions.FindActionAndPerform(Self.Message, Block);
  finally
    Block.Free;
  end;
end;

//******************************************************************************
//* TIdSipIDActionsWait                                                        *
//******************************************************************************
//* TIdSipIDActionsWait Public methods *****************************************

procedure TIdSipIDActionsWait.Trigger;
var
  Block: TIdSipActionClosure;
begin
  Block := Self.BlockType.Create;
  try
    Self.Actions.FindActionAndPerform(Self.ActionID, Block);
  finally
    Block.Free;
  end;
end;

//******************************************************************************
//* TIdSipActionSendWait                                                       *
//******************************************************************************
//* TIdSipActionSendWait Public methods ****************************************

procedure TIdSipActionSendWait.Trigger;
begin
  Self.Action.Send;
end;

//******************************************************************************
//* TIdSipUserAgentActOnRequest                                                *
//******************************************************************************
//* TIdSipUserAgentActOnRequest Public methods *********************************

procedure TIdSipUserAgentActOnRequest.Execute(Action: TIdSipAction);
begin
  // Processing the request - cf. RFC 3261, section 8.2.5
  // Action generates the response - cf. RFC 3261, section 8.2.6

  if Assigned(Action) then
    Action.ReceiveRequest(Request);

  if not Assigned(Action) then
    Action := Self.UserAgent.AddInboundAction(Self.Request, Self.Receiver);

  if not Assigned(Action) then begin
    if Request.IsAck then
      Self.UserAgent.NotifyOfDroppedMessage(Self.Request, Self.Receiver)
    else begin
      Self.UserAgent.ReturnResponse(Self.Request,
                                    SIPCallLegOrTransactionDoesNotExist);
    end;
  end;
end;

//******************************************************************************
//* TIdSipUserAgentActOnResponse                                               *
//******************************************************************************
//* TIdSipUserAgentActOnResponse Public methods ********************************

procedure TIdSipUserAgentActOnResponse.Execute(Action: TIdSipAction);
begin
  // User Agents drop unmatched responses on the floor.
  // Except for 2xx's on a client INVITE. And these no longer belong to
  // a transaction, since the receipt of a 200 terminates a client INVITE
  // immediately.
  if Assigned(Action) then
    Action.ReceiveResponse(Self.Response, Self.Receiver.IsSecure)
  else

    Self.UserAgent.NotifyOfDroppedMessage(Self.Response, Self.Receiver);
end;

//******************************************************************************
//* TIdSipUserAgentUpdateAction                                                *
//******************************************************************************
//* TIdSipUserAgentUpdateAction Public methods *********************************

procedure TIdSipUserAgentUpdateAction.Execute(Action: TIdSipAction);
begin
  Action.InitialRequest.Assign(Self.Request);
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
  Self.fAllowedSchemeList      := TStringList.Create;

  Self.ModuleLock := TCriticalSection.Create;
  Self.Modules    := TObjectList.Create(true);

  Self.fActions                := TIdSipActions.Create;
  Self.fAllowedContentTypeList := TStringList.Create;
  Self.fAllowedLanguageList    := TStringList.Create;
  Self.fKeyring                := TIdKeyRing.Create;

  Self.Actions.AddObserver(Self);

  Self.AddAllowedContentType(SdpMimeType);

  Self.AddModule(TIdSipOptionsModule);

  Self.AddAllowedScheme(SipScheme);

  Self.Contact.Value := Self.DefaultFrom;
  Self.From.Value    := Self.DefaultFrom;
  Self.HostName      := Self.DefaultHostName;
  Self.UserAgentName := Self.DefaultUserAgent;
end;

destructor TIdSipAbstractUserAgent.Destroy;
begin
  Self.NotifyModulesOfFree;

  Self.Contact.Free;
  Self.From.Free;

  Self.Keyring.Free;
  Self.AllowedSchemeList.Free;
  Self.AllowedLanguageList.Free;
  Self.AllowedContentTypeList.Free;
  Self.Actions.Free;

  Self.ModuleLock.Acquire;
  try
    Self.Modules.Free;
  finally
    Self.ModuleLock.Release;
  end;
  Self.ModuleLock.Free;

  inherited Destroy;
end;

procedure TIdSipAbstractUserAgent.AddAllowedContentType(const MimeType: String);
begin
  if (Trim(MimeType) <> '') then begin
    if (Self.AllowedContentTypeList.IndexOf(MimeType) = ItemNotFoundIndex) then
      Self.AllowedContentTypeList.Add(MimeType);
  end;
end;

procedure TIdSipAbstractUserAgent.AddAllowedLanguage(const LanguageID: String);
begin
  if (Trim(LanguageID) = '') then
    raise EIdSipBadSyntax.Create('Not a valid language identifier');

  if (Self.AllowedLanguageList.IndexOf(LanguageID) = ItemNotFoundIndex) then
    Self.AllowedLanguageList.Add(LanguageID);
end;

procedure TIdSipAbstractUserAgent.AddAllowedScheme(const Scheme: String);
begin
  if not TIdSipParser.IsScheme(Scheme) then
    raise EIdSipBadSyntax.Create('Not a valid scheme');

  if (Self.AllowedSchemeList.IndexOf(Scheme) = ItemNotFoundIndex) then
    Self.AllowedSchemeList.Add(Scheme);
end;

function TIdSipAbstractUserAgent.AddModule(ModuleType: TIdSipMessageModuleClass): TIdSipMessageModule;
begin
  Self.ModuleLock.Acquire;
  try
    if not Self.UsesModule(ModuleType) then begin
      Result := ModuleType.Create(Self);
      Self.Modules.Add(Result);
    end
    else begin
      Result := Self.ModuleFor(ModuleType);
    end;
  finally
    Self.ModuleLock.Release;
  end;
end;

function TIdSipAbstractUserAgent.AddOutboundAction(ActionType: TIdSipActionClass): TIdSipAction;
begin
  Result := Self.Actions.AddOutboundAction(Self, ActionType);
end;

function TIdSipAbstractUserAgent.AllowedContentTypes: String;
begin
  Result := Self.ConvertToHeader(Self.AllowedContentTypeList);
end;

function TIdSipAbstractUserAgent.AllowedEncodings: String;
begin
  Result := '';
end;

function TIdSipAbstractUserAgent.AllowedExtensions: String;
begin
  Result := '';
end;

function TIdSipAbstractUserAgent.AllowedLanguages: String;
begin
  Result := Self.ConvertToHeader(Self.AllowedLanguageList);
end;

function TIdSipAbstractUserAgent.AllowedMethods(RequestUri: TIdSipUri): String;
begin
  // TODO: This if fake.
  Result := Self.KnownMethods;
end;

function TIdSipAbstractUserAgent.AllowedSchemes: String;
begin
  Result := Self.ConvertToHeader(Self.AllowedSchemeList);
end;

function TIdSipAbstractUserAgent.CountOf(const MethodName: String): Integer;
begin
  Result := Self.Actions.CountOf(MethodName);
end;

function TIdSipAbstractUserAgent.CreateOptions(Dest: TIdSipAddressHeader): TIdSipRequest;
begin
  Result := Self.CreateRequest(MethodOptions, Dest);
  try
    Result.AddHeader(AcceptHeader).Value := Self.AllowedContentTypes;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipAbstractUserAgent.CreateRequest(const Method: String;
                                               Dest: TIdSipAddressHeader): TIdSipRequest;
begin
  Result := TIdSipRequest.Create;
  try
    Result.CallID         := Self.NextCallID;
    Result.From           := Self.From;
    Result.From.Tag       := Self.NextTag;
    Result.Method         := Method;
    Result.RequestUri     := Dest.Address;
    Result.ToHeader.Value := Dest.FullValue;

    Result.CSeq.Method     := Result.Method;
    Result.CSeq.SequenceNo := Self.NextInitialSequenceNo;

    Self.AddLocalHeaders(Result);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipAbstractUserAgent.CreateRequest(const Method: String;
                                               Dialog: TIdSipDialog): TIdSipRequest;
begin
  Result := Dialog.CreateRequest;
  try
    Result.Method      := Method;
    Result.CSeq.Method := Method;

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
                                        ResponseCode,
                                        Self.Contact);

  Self.PrepareResponse(Result, Request);
end;

procedure TIdSipAbstractUserAgent.FindServersFor(Request: TIdSipRequest;
                                                 Result: TIdSipLocations);
begin
  Self.Locator.FindServersFor(Request.DestinationUri, Result);
end;

procedure TIdSipAbstractUserAgent.FindServersFor(Response: TIdSipResponse;
                                                 Result: TIdSipLocations);
begin
  Self.Locator.FindServersFor(Response, Result);
end;

function TIdSipAbstractUserAgent.HasUnknownAccept(Request: TIdSipRequest): Boolean;
begin
  Result := Self.ListHasUnknownValue(Request,
                                     Self.AllowedContentTypeList,
                                     AcceptHeader);
end;

function TIdSipAbstractUserAgent.HasUnknownContentEncoding(Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(ContentEncodingHeaderFull);
end;

function TIdSipAbstractUserAgent.HasUnknownContentLanguage(Request: TIdSipRequest): Boolean;
begin
  Result := Self.ListHasUnknownValue(Request,
                                     Self.AllowedLanguageList,
                                     ContentLanguageHeader);
end;

function TIdSipAbstractUserAgent.HasUnknownContentType(Request: TIdSipRequest): Boolean;
begin
  Result := Self.ListHasUnknownValue(Request,
                                     Self.AllowedContentTypeList,
                                     ContentTypeHeaderFull);
end;

function TIdSipAbstractUserAgent.IsExtensionAllowed(const Extension: String): Boolean;
begin
  Result := false;
end;

function TIdSipAbstractUserAgent.IsMethodAllowed(RequestUri: TIdSipUri;
                                                 const Method: String): Boolean;
begin
  // TODO: This is just a stub at the moment. Eventually we want to support
  // controlling rights for multiple URIs so that, for instance, we could allow a
  // non-User Agent to say "yes, you can SUBSCRIBE to A's state, but not to B's". 
  Result := Self.IsMethodSupported(Method);
end;

function TIdSipAbstractUserAgent.IsMethodSupported(const Method: String): Boolean;
begin
  Result := TIdSipParser.IsToken(Method)
        and (Pos(Lowercase(Method), Lowercase(Self.KnownMethods)) > 0);
end;

function TIdSipAbstractUserAgent.IsSchemeAllowed(const Scheme: String): Boolean;
begin
  Result := Self.AllowedSchemeList.IndexOf(Scheme) >= 0;
end;

function TIdSipAbstractUserAgent.KnownMethods: String;
var
  I: Integer;
begin
  Result := '';
  Self.ModuleLock.Acquire;
  try
    for I := 0 to Self.Modules.Count - 1 do
      Result := Result + (Self.Modules[I] as TIdSipMessageModule).AcceptsMethods + ', ';

    if (Result <> '') then
      Delete(Result, Length(Result) - 1, 2);
  finally
    Self.ModuleLock.Release;
  end;
end;

function TIdSipAbstractUserAgent.ModuleFor(Request: TIdSipRequest): TIdSipMessageModule;
var
  I: Integer;
begin
  Result := nil;

  I := 0;
  Self.ModuleLock.Acquire;
  try
    while (I < Self.Modules.Count) and not Assigned(Result) do
      if (Self.Modules[I] as TIdSipMessageModule).WillAccept(Request) then
        Result := Self.Modules[I] as TIdSipMessageModule
      else
        Inc(I);
  finally
    Self.ModuleLock.Release;
  end;
end;

function TIdSipAbstractUserAgent.ModuleFor(const Method: String): TIdSipMessageModule;
var
  R: TIdSipRequest;
begin
  R := TIdSipRequest.Create;
  try
    R.Method := Method;

    Result := Self.ModuleFor(R);
  finally
    R.Free;
  end;
end;

function TIdSipAbstractUserAgent.ModuleFor(ModuleType: TIdSipMessageModuleClass): TIdSipMessageModule;
var
  I: Integer;
begin
  I := 0;
  Result := nil;

  Self.ModuleLock.Acquire;
  try
    while (I < Self.Modules.Count) and not Assigned(Result) do begin
      if (Self.Modules[I] is ModuleType) then
        Result := Self.Modules[I] as TIdSipMessageModule
      else Inc(I);
    end;
  finally
    Self.ModuleLock.Release;
  end;
end;

function TIdSipAbstractUserAgent.NextActionID: String;
begin
  Result := Self.Actions.NextActionID;
end;

function TIdSipAbstractUserAgent.NextBranch: String;
begin
  Result := GRandomNumber.NextSipUserAgentBranch;
end;

function TIdSipAbstractUserAgent.NextInitialSequenceNo: Cardinal;
begin
  Result := GRandomNumber.NextCardinal($7FFFFFFF);
end;

function TIdSipAbstractUserAgent.OptionsCount: Integer;
begin
  Result := Self.Actions.OptionsCount;
end;

function TIdSipAbstractUserAgent.QueryOptions(Server: TIdSipAddressHeader): TIdSipOutboundOptions;
begin
  Result := Self.AddOutboundAction(TIdSipOutboundOptions) as TIdSipOutboundOptions;
  Result.Server := Server;
end;

procedure TIdSipAbstractUserAgent.RemoveModule(ModuleType: TIdSipMessageModuleClass);
var
  I: Integer;
begin
  Self.ModuleLock.Acquire;
  try
    I := 0;
    while (I < Self.Modules.Count) do begin
      if ((Self.Modules[I] as TIdSipMessageModule).ClassType = ModuleType) then begin
        Self.Modules.Delete(I);
        Break;
      end
      else
        Inc(I);
    end;
  finally
    Self.ModuleLock.Release;
  end;
end;

procedure TIdSipAbstractUserAgent.ReturnResponse(Request: TIdSipRequest;
                                                 Reason: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, Reason);
  try
    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.TerminateAllCalls;
begin
  // This is WRONG! It will also terminate subscriptions, which are not calls!
  Self.Actions.TerminateAllActions;
end;

function TIdSipAbstractUserAgent.UsingDefaultContact: Boolean;
begin
  Result := Pos(Self.Contact.Address.Uri, Self.DefaultFrom) > 0;
end;

procedure TIdSipAbstractUserAgent.ScheduleEvent(BlockType: TIdSipActionClosureClass;
                                                WaitTime: Cardinal;
                                                Copy: TIdSipMessage);
var
  Event: TIdSipActionsWait;
begin
  if not Assigned(Self.Timer) then
    Exit;

  Event := Self.CreateActionsClosure(TIdSipActionsWait, Copy);
  Event.BlockType := BlockType;
  Self.ScheduleEvent(WaitTime, Event);
end;

procedure TIdSipAbstractUserAgent.ScheduleEvent(BlockType: TIdSipActionClosureClass;
                                                WaitTime: Cardinal;
                                                Copy: TIdSipMessage;
                                                const ActionID: String);
var
  Event: TIdSipIDActionsWait;
begin
  if not Assigned(Self.Timer) then
    Exit;

  Event := Self.CreateActionsClosure(TIdSipIDActionsWait, Copy) as TIdSipIDActionsWait;
  Event.BlockType := BlockType;
  Event.ActionID  := ActionID;
  Self.ScheduleEvent(WaitTime, Event);
end;

procedure TIdSipAbstractUserAgent.ScheduleEvent(WaitTime: Cardinal;
                                                Wait: TIdWait);
begin
  if not Assigned(Self.Timer) then
    Exit;

  Self.TimerLock.Acquire;
  try
    Self.Timer.AddEvent(WaitTime, Wait);
  finally
    Self.TimerLock.Release;
  end;
end;

function TIdSipAbstractUserAgent.Username: String;
begin
  Result := Self.From.Address.Username;
end;

function TIdSipAbstractUserAgent.UsesModule(ModuleType: TIdSipMessageModuleClass): Boolean;
begin
  Result := Assigned(Self.ModuleFor(ModuleType));
end;

//* TIdSipAbstractUserAgent Protected methods **********************************

procedure TIdSipAbstractUserAgent.ActOnRequest(Request: TIdSipRequest;
                                               Receiver: TIdSipTransport);
var
  Actor: TIdSipUserAgentActOnRequest;
begin
  inherited ActOnRequest(Request, Receiver);

  Actor := Self.CreateRequestHandler(Request, Receiver);
  try
    Self.Actions.Perform(Request, Actor);
  finally
    Actor.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.ActOnResponse(Response: TIdSipResponse;
                                                Receiver: TIdSipTransport);
var
  Actor: TIdSipUserAgentActOnResponse;
begin
  inherited ActOnResponse(Response, Receiver);

  Actor := Self.CreateResponseHandler(Response, Receiver);
  try
    Self.Actions.Perform(Response, Actor);
  finally
    Actor.Free;
  end;
end;

function TIdSipAbstractUserAgent.AddInboundAction(Request: TIdSipRequest;
                                                  Receiver: TIdSipTransport): TIdSipAction;
var
  Module: TIdSipMessageModule;
begin
  Module := Self.ModuleFor(Request);

  if Assigned(Module) then begin
    Result := Module.Accept(Request, Receiver.IsSecure);

    if Assigned(Result) then begin
      Self.Actions.Add(Result);
    end;
  end
  else
    Result := nil;
end;

procedure TIdSipAbstractUserAgent.AddLocalHeaders(OutboundRequest: TIdSipRequest);
var
  Transport: String;
begin
  // You might think we need to find out the appropriate transport to use before
  // we send the message. Yes, we do. We do so when the Action actually sends
  // the request in Action.Send(Request|Response).

  // cf RFC 3263, section 4.1
  if OutboundRequest.ToHeader.Address.HasParameter(TransportParam) then
    Transport := OutboundRequest.ToHeader.Address.Transport
  else
    Transport := TransportParamUDP;

  if not OutboundRequest.IsAck then begin
    OutboundRequest.AddHeader(ViaHeaderFull);
    OutboundRequest.LastHop.SipVersion := SipVersion;
    OutboundRequest.LastHop.Transport  := ParamToTransport(Transport);
    OutboundRequest.LastHop.SentBy     := Self.HostName;
    OutboundRequest.LastHop.Branch     := Self.NextBranch;
  end;

  if (Self.UserAgentName <> '') then
    OutboundRequest.AddHeader(UserAgentHeader).Value := Self.UserAgentName;

  OutboundRequest.AddHeader(Self.Contact);

  if OutboundRequest.HasSipsUri then
    OutboundRequest.FirstContact.Address.Scheme := SipsScheme;

  Self.AddModuleSpecificHeaders(OutboundRequest);    
end;

function TIdSipAbstractUserAgent.CreateActionsClosure(ClosureType: TIdSipActionsWaitClass;
                                                      Msg: TIdSipMessage): TIdSipActionsWait;
begin
  Result := ClosureType.Create;
  Result.Actions := Self.Actions;
  Result.Message := Msg.Copy;
end;

function TIdSipAbstractUserAgent.ListHasUnknownValue(Request: TIdSipRequest;
                                                     ValueList: TStrings;
                                                     const HeaderName: String): Boolean;
begin
  Result := Request.HasHeader(HeaderName)
       and (ValueList.IndexOf(Request.FirstHeader(HeaderName).Value) = ItemNotFoundIndex);
end;

procedure TIdSipAbstractUserAgent.OnAuthenticationChallenge(Dispatcher: TIdSipTransactionDispatcher;
                                                            Challenge: TIdSipResponse;
                                                            ChallengeResponse: TIdSipRequest;
                                                            var TryAgain: Boolean);
var
  AuthHeader:      TIdSipAuthorizationHeader;
  ChallengeHeader: TIdSipAuthenticateHeader;
  Password:        String;
  RealmInfo:       TIdRealmInfo;
  Username:        String;
begin
  // We've received a 401 or 407 response. At this level of the stack we know
  // this response matches a request that we sent out since the transaction
  // layer drops unmatched responses.
  //
  // Now we've a few cases:
  // 1. The response matches something like an INVITE, OPTIONS, etc. FindAction
  //    will return a reference to this action;
  // 2. The response matches a BYE, in which case FindAction will return nil.
  //    Since we consider the session terminated as soon as we send the BYE,
  //    we cannot match the response to an action.
  //
  // In case 1, we find the action and update its initial request. In case 2,
  // we just fake things a bit - we re-issue the request with incremented
  // sequence number and an authentication token, and hope for the best. Really,
  // UASs shouldn't challenge BYEs - since the UAC has left the session,
  // there's no real way to defend against a spoofed BYE: if the UAC did send
  // the BYE, it's left the conversation. If it didn't, the UAC will simply
  // drop your challenge.

  inherited OnAuthenticationChallenge(Dispatcher, Challenge, ChallengeResponse, TryAgain);

  Self.NotifyOfAuthenticationChallenge(Challenge, Username, Password, TryAgain);
  try
    if not TryAgain then Exit;

    ChallengeHeader := Challenge.AuthenticateHeader;

    // All challenges MUST have either a Proxy-Authenticate header or a
    // WWW-Authenticate header. If a response without one of these headers makes
    // it all the way here, we simply cannot do anything with it.
    if not Assigned(ChallengeHeader) then
      Exit;

    Self.Keyring.AddKey(ChallengeHeader,
                        ChallengeResponse.RequestUri.AsString,
                        Username);

    // This may look a bit like a time-of-check/time-of-use race condition
    // ("what if something frees the RealmInfo before you use it?") but it's
    // not - you can't remove realms from the Keyring, only add them.                    
    RealmInfo := Self.Keyring.Find(ChallengeHeader.Realm,
                                   ChallengeResponse.RequestUri.AsString);

    AuthHeader := RealmInfo.CreateAuthorization(Challenge,
                                                ChallengeResponse.Method,
                                                ChallengeResponse.Body,
                                                Password);
    try
      ChallengeResponse.AddHeader(AuthHeader);
    finally
      AuthHeader.Free;
    end;

    Self.UpdateAffectedActionWithRequest(Challenge, ChallengeResponse);
  finally
    // Write over the buffer that held the password.
    FillChar(Password, Length(Password), 0);
  end;
end;

procedure TIdSipAbstractUserAgent.PrepareResponse(Response: TIdSipResponse;
                                                  Request: TIdSipRequest);
begin
  inherited PrepareResponse(Response, Request);

  Self.AddModuleSpecificHeaders(Response);
end;

procedure TIdSipAbstractUserAgent.RejectRequest(Reaction: TIdSipUserAgentReaction;
                                                Request: TIdSipRequest);
begin
  case Reaction of
    uarDoNotDisturb:
          Self.ReturnResponse(Request,
                              SIPTemporarilyUnavailable);
    uarLoopDetected:
      Self.ReturnResponse(Request, SIPLoopDetected);
    uarMethodNotAllowed:
      Self.ReturnMethodNotAllowed(Request);
    uarMissingContact:
      Self.RejectBadRequest(Request, MissingContactHeader);
    uarUnsupportedAccept:
      Self.RejectRequestUnknownAccept(Request);
    uarUnsupportedContentEncoding:
      Self.RejectRequestUnknownContentEncoding(Request);
    uarUnsupportedContentLanguage:
      Self.RejectRequestUnknownContentLanguage(Request);
    uarUnsupportedContentType:
      Self.RejectRequestUnknownContentType(Request);
    uarUnsupportedExtension:
      Self.RejectRequestBadExtension(Request);
    uarUnsupportedMethod:
      Self.RejectRequestMethodNotSupported(Request);
    uarUnsupportedScheme:
      Self.ReturnResponse(Request, SIPUnsupportedURIScheme);
    uarUnSupportedSipVersion:
      Self.RejectUnsupportedSipVersion(Request);
  else
    inherited RejectRequest(Reaction, Request);
  end;
end;

function TIdSipAbstractUserAgent.ResponseForInvite: Cardinal;
begin
  // If we receive an INVITE (or an OPTIONS), what response code
  // would we return? If we don't wish to be disturbed, we return
  // SIPTemporarilyUnavailable; if we have no available lines, we
  // return SIPBusyHere, etc.

  Result := SIPOK;
end;

function TIdSipAbstractUserAgent.WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  Result := inherited WillAcceptRequest(Request);

  // cf RFC 3261 section 8.2
  if (Result = uarAccept) then begin
    if (Request.SIPVersion <> SipVersion) then
      Result := uarUnsupportedSipVersion
    // inspect the method - 8.2.1
    else if not Self.IsMethodSupported(Request.Method) then
      Result := uarUnsupportedMethod
    else if not Self.IsMethodAllowed(Request.RequestUri, Request.Method) then
      Result := uarMethodNotAllowed
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
    else if Self.HasUnknownAccept(Request) then
      Result := uarUnsupportedAccept
    else if Self.HasUnknownContentEncoding(Request) then
      Result := uarUnsupportedContentEncoding
    else if Self.HasUnknownContentLanguage(Request) then
      Result := uarUnsupportedContentLanguage
    else if Self.HasUnknownContentType(Request) then
      Result := uarUnsupportedContentType
    // Section 8.1.1.8 says that a request that can start a dialog (like an
    // INVITE), MUST contain a Contact.
    else if Request.IsInvite and not Request.HasHeader(ContactHeaderFull) then
      Result := uarMissingContact
  end;
end;

//* TIdSipAbstractUserAgent Private methods ************************************

procedure TIdSipAbstractUserAgent.AddModuleSpecificHeaders(OutboundMessage: TIdSipMessage);
var
  I: Integer;
begin
  Self.ModuleLock.Acquire;
  try
    for I := 0 to Self.Modules.Count - 1 do
      Self.ModuleAt(I).AddLocalHeaders(OutboundMessage);
  finally
    Self.ModuleLock.Release;
  end;
end;

function TIdSipAbstractUserAgent.ConvertToHeader(ValueList: TStrings): String;
begin
  Result := StringReplace(ValueList.CommaText, ',', ', ', [rfReplaceAll]);
end;

function TIdSipAbstractUserAgent.CreateRequestHandler(Request: TIdSipRequest;
                                                      Receiver: TIdSipTransport): TIdSipUserAgentActOnRequest;
begin
  Result := TIdSipUserAgentActOnRequest.Create;

  Result.Receiver  := Receiver;
  Result.Request   := Request;
  Result.UserAgent := Self;
end;

function TIdSipAbstractUserAgent.CreateResponseHandler(Response: TIdSipResponse;
                                                       Receiver: TIdSipTransport): TIdSipUserAgentActOnResponse;
begin
  Result := TIdSipUserAgentActOnResponse.Create;

  Result.Receiver  := Receiver;
  Result.Response  := Response;
  Result.UserAgent := Self;
end;

function TIdSipAbstractUserAgent.DefaultFrom: String;
begin
  Result := 'unknown <sip:unknown@' + Self.HostName + '>';
end;

function TIdSipAbstractUserAgent.DefaultUserAgent: String;
begin
  Result := 'RNID SipStack v' + SipStackVersion;
end;

function TIdSipAbstractUserAgent.GetContact: TIdSipContactHeader;
begin
  if not Assigned(fContact) then
    fContact := TIdSipContactHeader.Create;

  Result := fContact;
end;

function TIdSipAbstractUserAgent.GetFrom: TIdSipFromHeader;
begin
  if not Assigned(fFrom) then
    fFrom := TIdSipFromHeader.Create;

  Result := fFrom;
end;

function TIdSipAbstractUserAgent.ModuleAt(Index: Integer): TIdSipMessageModule;
begin
  Result := Self.Modules[Index] as TIdSipMessageModule;
end;

procedure TIdSipAbstractUserAgent.NotifyModulesOfFree;
var
  I: Integer;
begin
  Self.ModuleLock.Acquire;
  try
    for I := 0 to Self.Modules.Count - 1 do
      Self.ModuleAt(I).CleanUp;
  finally
    Self.ModuleLock.Release;
  end;
end;

procedure TIdSipAbstractUserAgent.NotifyOfAuthenticationChallenge(Response: TIdSipResponse;
                                                                  var Username: String;
                                                                  var Password: String;
                                                                  var TryAgain: Boolean);
var
  Notification: TIdSipUserAgentAuthenticationChallengeMethod;
begin
  Notification := TIdSipUserAgentAuthenticationChallengeMethod.Create;
  try
    Notification.Challenge := Response;
    Notification.TryAgain  := TryAgain;
    Notification.UserAgent := Self;

    Self.UserAgentListeners.Notify(Notification);

    Password := Notification.FirstPassword;
    TryAgain := Notification.TryAgain;
    Username := Notification.FirstUsername;
  finally
    Notification.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.NotifyOfInboundCall(Session: TIdSipInboundSession);
var
  Notification: TIdSipUserAgentInboundCallMethod;
begin
  Notification := TIdSipUserAgentInboundCallMethod.Create;
  try
    Notification.Session   := Session;
    Notification.UserAgent := Self;

    Self.UserAgentListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.NotifyOfDroppedMessage(Message: TIdSipMessage;
                                                         Receiver: TIdSipTransport);
var
  Notification: TIdSipUserAgentDroppedUnmatchedMessageMethod;
begin
  Notification := TIdSipUserAgentDroppedUnmatchedMessageMethod.Create;
  try
    Notification.Message   := Message;
    Notification.Receiver  := Receiver;
    Notification.UserAgent := Self;

    Self.UserAgentListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.OnChanged(Observed: TObject);
begin
  Self.NotifyOfChange;
end;

procedure TIdSipAbstractUserAgent.ReturnMethodNotAllowed(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPMethodNotAllowed);
  try
    Response.AddHeader(AllowHeader).Value := Self.AllowedMethods(Request.RequestUri);

    Self.SendResponse(Response);
  finally
    Response.Free;
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

    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectRequestMethodNotSupported(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPNotImplemented);
  try
    Response.StatusText := Response.StatusText + ' (' + Request.Method + ')';
    Response.AddHeader(AllowHeader).Value := Self.KnownMethods;

    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectRequestUnknownAccept(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPNotAcceptableClient);
  try
    Response.AddHeader(AcceptHeader).Value := Self.AllowedContentTypes;

    Self.SendResponse(Response);
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

    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectRequestUnknownContentLanguage(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  // It seems a stretch to say that an unsupported language would fall under
  //"unsupported media type, but the RFC says so (RFC 3261, cf section 8.2.3)
  Response := Self.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptLanguageHeader).Value := Self.AllowedLanguages;

    Self.SendResponse(Response);
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

    Self.SendResponse(Response);
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
    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.SetContact(Value: TIdSipContactHeader);
begin
  Assert(not Value.IsWildCard,
         'You may not use a wildcard Contact header for a User Agent''s '
       + 'Contact');

  Self.Contact.Assign(Value);

  if Self.Contact.IsMalformed then
    raise EBadHeader.Create(Self.Contact.Name);
end;

procedure TIdSipAbstractUserAgent.SetFrom(Value: TIdSipFromHeader);
begin
  Self.From.Assign(Value);

  if Self.From.IsMalformed then
    raise EBadHeader.Create(Self.From.Name);
end;

procedure TIdSipAbstractUserAgent.UpdateAffectedActionWithRequest(FindMsg: TIdSipMessage;
                                                                  NewRequest: TIdSipRequest);
var
  UpdateAffectedAction: TIdSipUserAgentUpdateAction;
begin
  UpdateAffectedAction := TIdSipUserAgentUpdateAction.Create;
  try
    UpdateAffectedAction.UserAgent := Self;
    UpdateAffectedAction.Request   := NewRequest;

    Self.Actions.FindActionAndPerform(FindMsg, UpdateAffectedAction);
  finally
    UpdateAffectedAction.Free;
  end;
end;

//******************************************************************************
//* TIdSipUserAgent                                                            *
//******************************************************************************
//* TIdSipUserAgent Public methods *********************************************

constructor TIdSipUserAgent.Create;
begin
  inherited Create;

  Self.fInviteModule   := Self.AddModule(TIdSipInviteModule) as TIdSipInviteModule;
  Self.fRegisterModule := Self.AddModule(TIdSipOutboundRegisterModule) as TIdSipOutboundRegisterModule;

  Self.DoNotDisturb           := false;
  Self.DoNotDisturbMessage    := RSSIPTemporarilyUnavailable;
  Self.fProxy                 := TIdSipUri.Create('');
  Self.HasProxy               := false;
  Self.InitialResendInterval  := DefaultT1;
end;

destructor TIdSipUserAgent.Destroy;
begin
  // Because we create TIdSipUserAgents from a StackConfigurator factory method,
  // we must clean up certain objects to which we have references, viz.,
  // Self.Dispatcher and Self.Authenticator.
  // Thus we destroy these objects AFTER the inherited Destroy, because the base
  // class could well expect these objects to still exit.

  Self.Proxy.Free;

  inherited Destroy;

  Self.Dispatcher.Free;
  Self.Authenticator.Free;
end;

function TIdSipUserAgent.Call(Dest: TIdSipAddressHeader;
                              const LocalSessionDescription: String;
                              const MimeType: String): TIdSipOutboundSession;
begin
  Result := Self.AddOutboundSession;
  Result.Destination             := Dest;
  Result.LocalSessionDescription := LocalSessionDescription;
  Result.LocalMimeType           := MimeType;
end;

function TIdSipUserAgent.SessionCount: Integer;
begin
  Result := Self.Actions.SessionCount;
end;

//* TIdSipUserAgent Protected methods ******************************************

procedure TIdSipUserAgent.AddLocalHeaders(OutboundRequest: TIdSipRequest);
begin
  inherited AddLocalHeaders(OutboundRequest);

  if Self.HasProxy then
    OutboundRequest.Route.AddRoute(Self.Proxy);
end;

function TIdSipUserAgent.ResponseForInvite: Cardinal;
begin
  // If we receive an INVITE (or an OPTIONS), what response code
  // would we return? If we don't wish to be disturbed, we return
  // SIPTemporarilyUnavailable; if we have no available lines, we
  // return SIPBusyHere, etc.

  if Self.DoNotDisturb then
    Result := SIPTemporarilyUnavailable
  else
    Result := inherited ResponseForInvite;
end;

function TIdSipUserAgent.WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  Result := inherited WillAcceptRequest(Request);

  if (Result = uarAccept) then begin
    if (Request.IsInvite or Request.IsOptions) and Self.DoNotDisturb then
      Result := uarDoNotDisturb;
  end;
end;

//* TIdSipUserAgent Private methods ********************************************

function TIdSipUserAgent.AddOutboundSession: TIdSipOutboundSession;
begin
  Result := Self.AddOutboundAction(TIdSipOutboundSession) as TIdSipOutboundSession;
end;

function TIdSipUserAgent.GetInitialResendInterval: Cardinal;
begin
  Result := Self.InviteModule.InitialResendInterval;
end;

function TIdSipUserAgent.GetProgressResendInterval: Cardinal;
begin
  Result := Self.InviteModule.ProgressResendInterval;
end;

procedure TIdSipUserAgent.SetInitialResendInterval(Value: Cardinal);
begin
  Self.InviteModule.InitialResendInterval := Value;
end;

procedure TIdSipUserAgent.SetProgressResendInterval(Value: Cardinal);
begin
  Self.InviteModule.ProgressResendInterval := Value;
end;

procedure TIdSipUserAgent.SetProxy(Value: TIdSipUri);
begin
  Self.Proxy.Uri := Value.Uri;
end;

//******************************************************************************
//* TIdSipRegistrar                                                            *
//******************************************************************************
//* TIdSipRegistrar Public methods *********************************************

constructor TIdSipRegistrar.Create;
begin
  inherited Create;

  Self.RegisterModule := Self.AddModule(TIdSipRegisterModule) as TIdSipRegisterModule;
end;

function TIdSipRegistrar.RegistrationCount: Integer;
begin
  Result := Self.CountOf(MethodRegister);
end;

//* TIdSipRegistrar Private methods ********************************************

function TIdSipRegistrar.GetBindingDB: TIdSipAbstractBindingDatabase;
begin
  Result := Self.RegisterModule.BindingDB;
end;

function TIdSipRegistrar.GetDefaultRegistrationExpiryTime: Cardinal;
begin
  Result := Self.BindingDB.DefaultExpiryTime;
end;

function TIdSipRegistrar.GetMinimumExpiryTime: Cardinal;
begin
  Result := Self.RegisterModule.MinimumExpiryTime;
end;

procedure TIdSipRegistrar.SetBindingDB(Value: TIdSipAbstractBindingDatabase);
begin
  Self.RegisterModule.BindingDB := Value;
end;

procedure TIdSipRegistrar.SetDefaultRegistrationExpiryTime(Value: Cardinal);
begin
  Self.BindingDB.DefaultExpiryTime := Value;
end;

procedure TIdSipRegistrar.SetMinimumExpiryTime(Value: Cardinal);
begin
  Self.RegisterModule.MinimumExpiryTime := Value;
end;

//******************************************************************************
//* TIdSipStackConfigurator                                                    *
//******************************************************************************
//* TIdSipStackConfigurator Public methods *************************************

function TIdSipStackConfigurator.CreateUserAgent(Configuration: TStrings;
                                                 Context: TIdTimerQueue): TIdSipUserAgent;
var
  PendingActions: TObjectList;
begin
  try
    Result := Self.CreateLayers(Context);

    PendingActions := TObjectList.Create(false);
    try
      Self.ParseFile(Result, Configuration, PendingActions);
      Self.InstantiateMissingObjectsAsDefaults(Result);
      Self.SendPendingActions(PendingActions);
    finally
      PendingActions.Free;
    end;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

//* TIdSipStackConfigurator Private methods ************************************

procedure TIdSipStackConfigurator.AddAuthentication(UserAgent: TIdSipAbstractUserAgent;
                                                    const AuthenticationLine: String);
var
  Line: String;
begin
  Line := AuthenticationLine;
  Self.EatDirective(Line);

  if IsEqual(Trim(Line), MockKeyword) then
    UserAgent.Authenticator := TIdSipMockAuthenticator.Create;
end;

procedure TIdSipStackConfigurator.AddAutoContact(UserAgent: TIdSipAbstractUserAgent);
begin
  UserAgent.Contact.DisplayName      := UTF16LEToUTF8(GetFullUserName);
  UserAgent.Contact.Address.Username := UTF16LEToUTF8(GetUserName);
  UserAgent.Contact.Address.Host     := LocalAddress;
end;

procedure TIdSipStackConfigurator.AddContact(UserAgent: TIdSipAbstractUserAgent;
                                             const ContactLine: String);
var
  Line: String;
begin
  Line := ContactLine;
  Self.EatDirective(Line);

  if (Trim(Line) = AutoKeyword) then
    Self.AddAutoContact(UserAgent)
  else begin
    UserAgent.Contact.Value := Line;

    if UserAgent.Contact.IsMalformed then
      raise EParserError.Create(Format(MalformedConfigurationLine, [ContactLine]));
  end;    
end;

procedure TIdSipStackConfigurator.AddFrom(UserAgent: TIdSipAbstractUserAgent;
                                          const FromLine: String);
var
  Line: String;
begin
  Line := FromLine;
  Self.EatDirective(Line);

  UserAgent.From.Value := Line;

  if UserAgent.From.IsMalformed then
    raise EParserError.Create(Format(MalformedConfigurationLine, [FromLine]));
end;

procedure TIdSipStackConfigurator.AddLocator(UserAgent: TIdSipAbstractUserAgent;
                                             const NameServerLine: String);
var
  Host: String;
  Line: String;
  Loc:  TIdSipIndyLocator;
  Port: String;
begin
  // See class comment for the format for this directive.
  Line := NameServerLine;
  Self.EatDirective(Line);

  Host := Fetch(Line, ':');
  Port := Fetch(Line, ' ');

  if IsEqual(Host, MockKeyword) then
    UserAgent.Locator := TIdSipMockLocator.Create
  else begin
    if not TIdSimpleParser.IsNumber(Port) then
      raise EParserError.Create(Format(MalformedConfigurationLine, [NameServerLine]));

    Loc := TIdSipIndyLocator.Create;
    Loc.NameServer := Host;
    Loc.Port       := StrToInt(Port);

    UserAgent.Locator := Loc;
    UserAgent.Dispatcher.Locator := UserAgent.Locator;
  end;
end;

procedure TIdSipStackConfigurator.AddProxy(UserAgent: TIdSipUserAgent;
                                           const ProxyLine: String);
var
  Line: String;
begin
  Line := ProxyLine;
  Self.EatDirective(Line);

  UserAgent.HasProxy := true;

  UserAgent.Proxy.Uri := Trim(Line);

  Self.CheckUri(UserAgent.Proxy, Format(MalformedConfigurationLine, [ProxyLine]));
end;

procedure TIdSipStackConfigurator.AddTransport(Dispatcher: TIdSipTransactionDispatcher;
                                               const TransportLine: String);
var
  HostAndPort:  TIdSipHostAndPort;
  Line:         String;
  NewTransport: TIdSipTransport;
  Transport:    String;
begin
  // See class comment for the format for this directive.
  Line := TransportLine;

  Self.EatDirective(Line);
  Transport := Fetch(Line, ' ');

  NewTransport := TIdSipTransportRegistry.TransportFor(Transport).Create;
  Dispatcher.AddTransport(NewTransport);
  NewTransport.Timer := Dispatcher.Timer;

  HostAndPort := TIdSipHostAndPort.Create;
  try
    HostAndPort.Value := Line;

    if (HostAndPort.Host = AutoKeyword) then
      NewTransport.Address := LocalAddress
    else
      NewTransport.Address := HostAndPort.Host;

    NewTransport.HostName := NewTransport.Address;
    NewTransport.Port     := HostAndPort.Port;
  finally
    HostAndPort.Free;
  end;
end;

procedure TIdSipStackConfigurator.CheckUri(Uri: TIdSipUri;
                                           const FailMsg: String);
begin
  if not TIdSimpleParser.IsFQDN(Uri.Host)
    and not TIdIPAddressParser.IsIPv4Address(Uri.Host)
    and not TIdIPAddressParser.IsIPv6Reference(Uri.Host) then
    raise EParserError.Create(FailMsg);
end;

function TIdSipStackConfigurator.CreateLayers(Context: TIdTimerQueue): TIdSipUserAgent;
begin
  Result := TIdSipUserAgent.Create;
  Result.Timer := Context;
  Result.Dispatcher := TIdSipTransactionDispatcher.Create(Result.Timer, nil);
end;

procedure TIdSipStackConfigurator.EatDirective(var Line: String);
begin
  Fetch(Line, ':');
  Line := Trim(Line);
end;

procedure TIdSipStackConfigurator.InstantiateMissingObjectsAsDefaults(UserAgent: TIdSipAbstractUserAgent);
begin
  if not Assigned(UserAgent.Authenticator) then
    UserAgent.Authenticator := TIdSipAuthenticator.Create;

  if not Assigned(UserAgent.Locator) then
    UserAgent.Locator := TIdSipIndyLocator.Create;

  if UserAgent.UsingDefaultContact then
    Self.AddAutoContact(UserAgent);
end;

procedure TIdSipStackConfigurator.ParseFile(UserAgent: TIdSipUserAgent;
                                            Configuration: TStrings;
                                            PendingActions: TObjectList);
var
  I: Integer;
begin
  for I := 0 to Configuration.Count - 1 do
    Self.ParseLine(UserAgent, Configuration[I], PendingActions);
end;

procedure TIdSipStackConfigurator.ParseLine(UserAgent: TIdSipUserAgent;
                                            const ConfigurationLine: String;
                                            PendingActions: TObjectList);
var
  FirstToken: String;
  Line:       String;
begin
  Line := ConfigurationLine;
  FirstToken := Trim(Fetch(Line, ':', false));

  if      IsEqual(FirstToken, AuthenticationDirective) then
    Self.AddAuthentication(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, ContactDirective) then
    Self.AddContact(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, FromDirective) then
    Self.AddFrom(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, ListenDirective) then
    Self.AddTransport(UserAgent.Dispatcher, ConfigurationLine)
  else if IsEqual(FirstToken, NameServerDirective) then
    Self.AddLocator(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, ProxyDirective) then
    Self.AddProxy(UserAgent,  ConfigurationLine)
  else if IsEqual(FirstToken, RegisterDirective) then
    Self.RegisterUA(UserAgent, ConfigurationLine, PendingActions)
end;

procedure TIdSipStackConfigurator.RegisterUA(UserAgent: TIdSipUserAgent;
                                             const RegisterLine: String;
                                             PendingActions: TObjectList);
var
  Line: String;
  Reg:  TIdSipOutboundRegisterModule;
begin
  // See class comment for the format for this directive.
  Line := RegisterLine;
  Self.EatDirective(Line);

  Line := Trim(Line);

  Reg := UserAgent.RegisterModule;

  Reg.AutoReRegister := true;
  Reg.HasRegistrar := true;
  Reg.Registrar.Uri := Line;
  PendingActions.Add(Reg.RegisterWith(Reg.Registrar));
end;

procedure TIdSipStackConfigurator.SendPendingActions(Actions: TObjectList);
var
  I: Integer;
begin
  for I := 0 to Actions.Count - 1 do
    (Actions[I] as TIdSipAction).Send;
end;

//******************************************************************************
//* TIdSipMessageModule                                                        *
//******************************************************************************
//* TIdSipMessageModule Public methods *****************************************

constructor TIdSipMessageModule.Create(UA: TIdSipAbstractUserAgent);
begin
  inherited Create;

  Self.fUserAgent := UA;
end;

function TIdSipMessageModule.Accept(Request: TIdSipRequest;
                                    UsingSecureTransport: Boolean): TIdSipAction;
begin
  Result := nil;
end;

procedure TIdSipMessageModule.AddLocalHeaders(OutboundMessage: TIdSipMessage);
begin
end;

function TIdSipMessageModule.AcceptsMethods: String;
begin
  Result := '';
end;

procedure TIdSipMessageModule.CleanUp;
begin
  // When the User Agent frees, it calls this method. Put any cleanup stuff
  // here.
end;

function TIdSipMessageModule.WillAccept(Request: TIdSipRequest): Boolean;
begin
  Result := false;
end;

//******************************************************************************
//* TIdSipInviteModule                                                         *
//******************************************************************************
//* TIdSipInviteModule Public methods ******************************************

constructor TIdSipInviteModule.Create(UA: TIdSipAbstractUserAgent);
begin
  inherited Create(UA);

  Self.ProgressResendInterval := OneMinute*1000;
end;

function TIdSipInviteModule.Accept(Request: TIdSipRequest;
                                   UsingSecureTransport: Boolean): TIdSipAction;
var
  ExpectedStatusCode: Cardinal;
  Session:            TIdSipInboundSession;
begin
  ExpectedStatusCode := Self.UserAgent.ResponseForInvite;
  if (ExpectedStatusCode <> SIPOK) then begin
    Result := nil;
    Self.UserAgent.ReturnResponse(Request, ExpectedStatusCode)
  end
  else begin
    Session := TIdSipInboundSession.CreateInbound(Self.UserAgent,
                                                  Request,
                                                  UsingSecureTransport);

    Self.UserAgent.NotifyOfInboundCall(Session);

    if Request.HasHeader(ExpiresHeader) then
      Self.UserAgent.ScheduleEvent(TIdSipInboundInviteExpire,
                                   Request.FirstExpires.NumericValue,
                                   Request);
    Result := Session;
  end;
end;

function TIdSipInviteModule.AcceptsMethods: String;
begin
  Result := MethodAck + ', '
          + MethodBye + ', '
          + MethodCancel + ', '
          + MethodInvite;
end;

function TIdSipInviteModule.AddInboundInvite(Invite: TIdSipRequest;
                                             UsingSecureTransport: Boolean): TIdSipInboundInvite;
begin
  // This accesses the PROTECTED property of the UserAgent.
  Result := Self.UserAgent.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.UserAgent, Invite, UsingSecureTransport)) as TIdSipInboundInvite;
end;

function TIdSipInviteModule.CreateAck(Dialog: TIdSipDialog): TIdSipRequest;
begin
  try
    Result := Dialog.CreateAck;
    Self.UserAgent.AddLocalHeaders(Result); // TODO: this is a PROTECTED METHOD!
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipInviteModule.CreateBye(Dialog: TIdSipDialog): TIdSipRequest;
begin
  try
    Result := Self.UserAgent.CreateRequest(MethodBye, Dialog);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipInviteModule.CreateInvite(Dest: TIdSipAddressHeader;
                                         const Body: String;
                                         const MimeType: String): TIdSipRequest;
begin
  Result := Self.UserAgent.CreateRequest(MethodInvite, Dest);
  try
    Self.TurnIntoInvite(Result, Body, MimeType);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipInviteModule.CreateReInvite(Dialog: TIdSipDialog;
                                           const Body: String;
                                           const MimeType: String): TIdSipRequest;
begin
  Result := Self.UserAgent.CreateRequest(MethodInvite, Dialog);
  try
    Self.TurnIntoInvite(Result, Body, MimeType);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipInviteModule.WillAccept(Request: TIdSipRequest): Boolean;
begin
  Result := Request.IsInvite;
end;

//* TIdSipInviteModule Private methods *****************************************

procedure TIdSipInviteModule.TurnIntoInvite(OutboundRequest: TIdSipRequest;
                                            const Offer: String;
                                            const OfferMimeType: String);
begin
  OutboundRequest.Body := Offer;
  OutboundRequest.ContentLength := Length(Offer);

  if (OutboundRequest.ContentLength > 0) then begin
    OutboundRequest.ContentDisposition.Value := DispositionSession;
    OutboundRequest.ContentType              := OfferMimeType;
  end;

  OutboundRequest.AddHeader(AllowHeader).Value := Self.UserAgent.KnownMethods;
  // TODO: We need to add a proper extension support thing
  OutboundRequest.AddHeader(AcceptHeader).Value := Self.UserAgent.AllowedContentTypes;
  OutboundRequest.AddHeader(SupportedHeaderFull).Value := Self.UserAgent.AllowedExtensions;
end;

//******************************************************************************
//* TIdSipOptionsModule                                                        *
//******************************************************************************
//* TIdSipOptionsModule Public methods *****************************************

function TIdSipOptionsModule.Accept(Request: TIdSipRequest;
                                    UsingSecureTransport: Boolean): TIdSipAction;
begin
  Result := TIdSipInboundOptions.CreateInbound(Self.UserAgent,
                                               Request,
                                               UsingSecureTransport);
end;

function TIdSipOptionsModule.AcceptsMethods: String;
begin
  Result := MethodOptions;
end;

function TIdSipOptionsModule.WillAccept(Request: TIdSipRequest): Boolean;
begin
  Result := Request.IsOptions;
end;

//******************************************************************************
//* TIdSipRegisterModule                                                       *
//******************************************************************************
//* TIdSipRegisterModule Public methods ****************************************

function TIdSipRegisterModule.Accept(Request: TIdSipRequest;
                                     UsingSecureTransport: Boolean): TIdSipAction;
begin
  Result := TIdSipInboundRegistration.CreateInbound(Self.UserAgent,
                                                    Request,
                                                    UsingSecureTransport);
end;

function TIdSipRegisterModule.AcceptsMethods: String;
begin
  Result := MethodRegister;
end;

function TIdSipRegisterModule.WillAccept(Request: TIdSipRequest): Boolean;
begin
  Result := Request.IsRegister;
end;

//******************************************************************************
//* TIdSipOutboundRegisterModule                                               *
//******************************************************************************
//* TIdSipOutboundRegisterModule Public methods ********************************

constructor TIdSipOutboundRegisterModule.Create(UA: TIdSipAbstractUserAgent);
begin
  inherited Create(UA);

  Self.KnownRegistrars := TIdSipRegistrations.Create;
  Self.fRegistrar      := TIdSipUri.Create('');

  Self.AutoReRegister := true;
  Self.HasRegistrar   := false;
end;

destructor TIdSipOutboundRegisterModule.Destroy;
begin
  Self.Registrar.Free;
  Self.KnownRegistrars.Free;

  inherited Destroy;
end;

function TIdSipOutboundRegisterModule.Accept(Request: TIdSipRequest;
                                             UsingSecureTransport: Boolean): TIdSipAction;
begin
  // As a purely UAC module, don't accept ANY requests.
  Result := nil;
end;

function TIdSipOutboundRegisterModule.AcceptsMethods: String;
begin
  // As a purely UAC module, don't offer to accept ANY requests.
  Result := '';
end;

procedure TIdSipOutboundRegisterModule.CleanUp;
begin
  if Self.HasRegistrar then
    Self.UnregisterFrom(Self.Registrar).Send;
end;

function TIdSipOutboundRegisterModule.CreateRegister(Registrar: TIdSipToHeader): TIdSipRequest;
begin
  Result := Self.UserAgent.CreateRequest(MethodRegister, Registrar);
  try
    Self.KnownRegistrars.AddKnownRegistrar(Registrar.Address,
                                           Result.CallID,
                                           Result.CSeq.SequenceNo);

    Result.RequestUri.EraseUserInfo;
    Result.CSeq.SequenceNo := Self.KnownRegistrars.NextSequenceNoFor(Registrar.Address);

    Result.CallID := Self.KnownRegistrars.CallIDFor(Registrar.Address);

    Result.ToHeader.Assign(Self.UserAgent.Contact);
    Result.From.Assign(Self.UserAgent.Contact);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipOutboundRegisterModule.CurrentRegistrationWith(Registrar: TIdSipUri): TIdSipOutboundRegistrationQuery;
begin
  Result := Self.UserAgent.AddOutboundAction(TIdSipOutboundRegistrationQuery) as TIdSipOutboundRegistrationQuery;
end;

procedure TIdSipOutboundRegisterModule.OnReregister(Event: TObject);
var
  Request: TIdSipRequest;
begin
  Request := (Event as TIdSipMessageNotifyEventWait).Message as TIdSipRequest;
  Self.RegisterWith(Request.RequestUri).Send;
end;

function TIdSipOutboundRegisterModule.RegisterWith(Registrar: TIdSipUri): TIdSipOutboundRegister;
begin
  Result := Self.UserAgent.AddOutboundAction(TIdSipOutboundRegister) as TIdSipOutboundRegister;
  Result.Bindings.Add(Self.UserAgent.Contact);
  Result.Registrar := Registrar;
end;

function TIdSipOutboundRegisterModule.RegistrationCount: Integer;
begin
  Result := Self.UserAgent.CountOf(MethodRegister);
end;

function TIdSipOutboundRegisterModule.UnregisterFrom(Registrar: TIdSipUri): TIdSipOutboundUnregister;
begin
  Result := Self.UserAgent.AddOutboundAction(TIdSipOutboundUnregister) as TIdSipOutboundUnregister;
  Result.Bindings.Add(Self.UserAgent.Contact);
  Result.Registrar := Registrar;
end;

function TIdSipOutboundRegisterModule.WillAccept(Request: TIdSipRequest): Boolean;
begin
  // As a purely UAC module, don't accept ANY requests.
  Result := false;
end;

//* TIdSipOutboundRegisterModule Private methods *******************************

procedure TIdSipOutboundRegisterModule.SetRegistrar(Value: TIdSipUri);
begin
  Self.Registrar.Uri := Value.Uri;
end;

//******************************************************************************
//* TIdSipAction                                                               *
//******************************************************************************
//* TIdSipAction Public methods ************************************************

constructor TIdSipAction.Create(UA: TIdSipAbstractUserAgent);
begin
  inherited Create;

  Self.Initialise(UA, nil, false);
end;

constructor TIdSipAction.CreateInbound(UA: TIdSipAbstractUserAgent;
                                       Request: TIdSipRequest;
                                       UsingSecureTransport: Boolean);
begin
  inherited Create;

  Self.Initialise(UA, Request, UsingSecureTransport);
  Self.ReceiveRequest(Request);
end;

destructor TIdSipAction.Destroy;
begin
  Self.TargetLocations.Free;
  Self.Listeners.Free;
  Self.InitialRequest.Free;

  inherited Destroy;
end;

function TIdSipAction.IsInbound: Boolean;
begin
  Result := false;
end;

function TIdSipAction.IsInvite: Boolean;
begin
  Result := false;
end;

function TIdSipAction.IsOptions: Boolean;
begin
  Result := false;
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
  if Msg.IsRequest and (Msg as TIdSipRequest).IsCancel then
    Result := Self.InitialRequest.MatchCancel(Msg as TIdSipRequest)
  else
    Result := Self.InitialRequest.Match(Msg);
end;

procedure TIdSipAction.ReceiveRequest(Request: TIdSipRequest);
begin
  Self.ReceiveOtherRequest(Request);
end;

procedure TIdSipAction.ReceiveResponse(Response: TIdSipResponse;
                                       UsingSecureTransport: Boolean);
var
  Succeeded: TIdSipActionStatus;
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
    // 100 <= S < 700, so we handle these obviously malformed responses by
    // treating them as failure responses.
    Succeeded := asFailure;
  end;

  case Succeeded of
    asSuccess: if Response.IsOK then
      Self.ActionSucceeded(Response);
    asFailure:
      Self.NotifyOfFailure(Response);
  end;
end;

procedure TIdSipAction.Send;
begin
  if Self.SentRequest then
    raise EIdSipTransactionUser.Create(Format(MethodInProgress, [Self.Method]));
end;

procedure TIdSipAction.Terminate;
begin
  Self.MarkAsTerminated;
end;

//* TIdSipAction Protected methods *********************************************

procedure TIdSipAction.ActionSucceeded(Response: TIdSipResponse);
begin
  // By default do nothing.
end;

procedure TIdSipAction.AddListeners(Listeners: TIdNotificationList);
begin
  // WARNING: This will add all the listeners in Listeners to Self.Listeners.
  // You expect that. Note, though, that YOU must make sure Listeners contains
  // listeners of a type that Self expects.

  if Assigned(Listeners) then
    Self.Listeners.Add(Listeners);
end;

procedure TIdSipAction.Initialise(UA: TIdSipAbstractUserAgent;
                                  Request: TIdSipRequest;
                                  UsingSecureTransport: Boolean);
begin
  Self.fUA := UA;

  Self.fID             := Self.UA.NextActionID;
  Self.fInitialRequest := TIdSipRequest.Create;
  Self.fIsOwned        := false;
  Self.fIsTerminated   := false;
  Self.Listeners       := TIdNotificationList.Create;
  Self.NonceCount      := 0;
  Self.SentRequest     := false;

  if Self.IsInbound then
    Self.InitialRequest.Assign(Request);
end;

procedure TIdSipAction.MarkAsTerminated;
begin
  Self.fIsTerminated := true;
end;

procedure TIdSipAction.NotifyOfFailure(Response: TIdSipResponse);
begin
  // By default do nothing
end;

procedure TIdSipAction.NotifyOfNetworkFailure(ErrorCode: Cardinal;
                                              const Reason: String);
var
  Notification: TIdSipActionNetworkFailureMethod;
begin
  Notification := TIdSipActionNetworkFailureMethod.Create;
  try
    Notification.ActionAgent := Self;
    Notification.ErrorCode   := ErrorCode;
    Notification.Reason      := Reason;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;

  Self.MarkAsTerminated;
end;

function TIdSipAction.ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionStatus;
begin
  Result := asFailure;
{
  case Response.StatusCode of
    SIPUnauthorized,
    SIPProxyAuthenticationRequired: begin
      Self.NotifyOfAuthenticationChallenge(Response);
      Result := asInterim;
    end;
  else
    Result := asFailure;
  end;
}
end;

function TIdSipAction.ReceiveGlobalFailureResponse(Response: TIdSipResponse): TIdSipActionStatus;
begin
  Result := asFailure;
end;

function TIdSipAction.ReceiveOKResponse(Response: TIdSipResponse;
                                        UsingSecureTransport: Boolean): TIdSipActionStatus;
begin
  Result := asSuccess;
end;

procedure TIdSipAction.ReceiveOtherRequest(Request: TIdSipRequest);
begin
end;

function TIdSipAction.ReceiveProvisionalResponse(Response: TIdSipResponse;
                                                 UsingSecureTransport: Boolean): TIdSipActionStatus;
begin
  Result := asFailure;
end;

function TIdSipAction.ReceiveRedirectionResponse(Response: TIdSipResponse;
                                                 UsingSecureTransport: Boolean): TIdSipActionStatus;
begin
  Result := asFailure;
end;

function TIdSipAction.ReceiveServerFailureResponse(Response: TIdSipResponse): TIdSipActionStatus;
begin
  Result := asFailure;
end;

procedure TIdSipAction.SendRequest(Request: TIdSipRequest;
                                   TryAgain: Boolean = true);
var
  FailReason:      String;
  TargetLocations: TIdSipLocations;
begin
  Self.SentRequest := true;

  if (Self.NonceCount = 0) then
    Inc(Self.NonceCount);

  // cf RFC 3263, section 4.3
  TargetLocations := TIdSipLocations.Create;
  try
    Self.UA.FindServersFor(Request, TargetLocations);

    if TargetLocations.IsEmpty then begin
      // The Locator should at the least return a location based on the
      // Request-URI. Thus this clause should never execute. Still, this
      // clause protects the code that follows.

      FailReason := Format(RSNoLocationFound, [Request.DestinationUri]);
      Self.NotifyOfNetworkFailure(NoLocationFound,
                                  Format(OutboundActionFailed,
                                         [Self.Method, FailReason]));
      Exit;
    end;

    if not Self.TrySendRequest(Request, TargetLocations, TryAgain) then begin
      FailReason := Format(RSNoLocationSucceeded, [Request.DestinationUri]);
      Self.NotifyOfNetworkFailure(NoLocationSucceeded,
                                  Format(OutboundActionFailed,
                                         [Self.Method, FailReason]));
    end;
  finally
    TargetLocations.Free;
  end;
end;

procedure TIdSipAction.SendResponse(Response: TIdSipResponse);
begin
  // RFC 3263, section 5
  try
    Self.UA.SendResponse(Response);
  except
    on E: EIdSipTransport do
      Self.NotifyOfNetworkFailure(InboundActionFailed,
                                  Format(RSInboundActionFailed, [Self.Method, E.Message]));
  end;
end;

//* TIdSipAction Private methods ***********************************************

function TIdSipAction.GetUsername: String;
begin
  Result := Self.UA.Username;
end;

procedure TIdSipAction.SetUsername(const Value: String);
begin
  Self.UA.From.DisplayName := Value;
end;

function TIdSipAction.TrySendRequest(Request: TIdSipRequest;
                                     Targets: TIdSipLocations;
                                     TryAgain: Boolean = true): Boolean;
var
  ActualRequest: TIdSipRequest;
  CurrentTarget: Integer;
  NewAttempt:    TIdSipRequest;
begin
  // Result indicates success.

  CurrentTarget := 0;
  Result        := false;

  ActualRequest := TIdSipRequest.Create;
  try
    ActualRequest.Assign(Request);

    while not Result and (CurrentTarget < Targets.Count) do begin
      ActualRequest.LastHop.Transport := Targets[CurrentTarget].Transport;

      try
        Self.UA.SendRequest(ActualRequest, Targets[CurrentTarget]);
        Result := true;

        // Synchronise our state to what actually went down to the network.
        if Request.Match(Self.InitialRequest) then
          Self.InitialRequest.Assign(ActualRequest);
      except
        on E: EIdSipTransport do begin
          // We don't care about whether some request sends, like BYEs for
          // INVITEs, actually reach the far side. In the case of a BYE, the
          // session is terminated as soon as we send the BYE (RFC 3261, section
          // 15.1.1).
          if not TryAgain then
            Break;

          // Maybe we should log this?
          NewAttempt := Self.CreateNewAttempt;
          try
            ActualRequest.Assign(NewAttempt);
          finally
            NewAttempt.Free;
          end;
        end;
      end;

      Inc(CurrentTarget);
    end;
  finally
    ActualRequest.Free;
  end;
end;

//******************************************************************************
//* TIdSipInviteBase                                                           *
//******************************************************************************
//* TIdSipInviteBase Public methods ********************************************

class function TIdSipInviteBase.Method: String;
begin
  Result := MethodInvite;
end;

procedure TIdSipInviteBase.ReceiveRequest(Request: TIdSipRequest);
begin
       if Request.IsAck       then Self.ReceiveAck(Request)
  else if Request.IsBye       then Self.ReceiveBye(Request)
  else if Request.IsCancel    then Self.ReceiveCancel(Request)
  else if Request.IsInvite    then Self.ReceiveInvite(Request)
  else
    inherited ReceiveRequest(Request);
end;

//* TIdSipInviteBase Protected methods *****************************************

procedure TIdSipInviteBase.Initialise(UA: TIdSipAbstractUserAgent;
                                      Request: TIdSipRequest;
                                      UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.Module := Self.UA.ModuleFor(Self.Method) as TIdSipInviteModule;
end;

procedure TIdSipInviteBase.ReceiveAck(Ack: TIdSipRequest);
begin
  Assert(Ack.IsAck,
         'TIdSipInvite.ReceiveAck must only receive ACKs');
  // By default do nothing
end;

procedure TIdSipInviteBase.ReceiveBye(Bye: TIdSipRequest);
begin
  Assert(Bye.IsBye,
         'TIdSipInvite.ReceiveBye must only receive BYEs');
  // By default do nothing
end;

procedure TIdSipInviteBase.ReceiveCancel(Cancel: TIdSipRequest);
var
  Ok: TIdSipResponse;
begin
  Assert(Cancel.IsCancel,
         'TIdSipInvite.ReceiveCancel must only receive CANCELs');

  Ok := TIdSipResponse.InResponseTo(Cancel, SIPOK);
  try
    Self.SendResponse(Ok);
  finally
    Ok.Free;
  end;
end;

procedure TIdSipInviteBase.ReceiveInvite(Invite: TIdSipRequest);
begin
  Assert(Invite.IsInvite,
         'TIdSipInvite.ReceiveInvite must only receive INVITEs');
end;

//******************************************************************************
//* TIdSipInvite                                                               *
//******************************************************************************
//* TIdSipInvite Public methods ************************************************

function TIdSipInvite.IsInvite: Boolean;
begin
  Result := true;
end;

//* TIdSipInvite Protected methods *********************************************

function TIdSipInvite.CreateNewAttempt: TIdSipRequest;
var
  TempTo: TIdSipToHeader;
begin
  TempTo := TIdSipToHeader.Create;
  try
    TempTo.Address := Self.InitialRequest.RequestUri;

    Result := Self.Module.CreateInvite(TempTo,
                                       Self.InitialRequest.Body,
                                       Self.InitialRequest.ContentType);
  finally
    TempTo.Free;
  end;
end;

procedure TIdSipInvite.Initialise(UA: TIdSipAbstractUserAgent;
                                  Request: TIdSipRequest;
                                  UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  // Invites are always owned by a Session
  Self.fIsOwned := true;
end;

//******************************************************************************
//* TIdSipInboundInvite                                                        *
//******************************************************************************
//* TIdSipInboundInvite Public methods *****************************************

destructor TIdSipInboundInvite.Destroy;
begin
  Self.LastResponse.Free;

  inherited Destroy;
end;

procedure TIdSipInboundInvite.Accept(const Offer, ContentType: String);
var
  Ok: TIdSipResponse;
begin
  Self.ResendInterval    := Self.InitialResendInterval;
  Self.MaxResendInterval := 64*Self.ResendInterval;

  Self.fLocalSessionDescription := Offer;
  Self.fLocalMimeType           := ContentType;

  Ok := Self.UA.CreateResponse(Self.InitialRequest, SIPOK);
  try
    Ok.Body          := Offer;
    Ok.ContentType   := ContentType;
    Ok.ContentLength := Length(Offer);
    Ok.ToHeader.Tag  := Self.LocalTag;

    Self.SendResponse(Ok);
  finally
    Ok.Free;
  end;

  Self.ScheduleResendOk(Self.ResendInterval);
end;

procedure TIdSipInboundInvite.AddListener(const Listener: IIdSipInboundInviteListener);
begin
  Self.Listeners.AddListener(Listener);
end;

function TIdSipInboundInvite.IsInbound: Boolean;
begin
  Result := true;
end;

function TIdSipInboundInvite.Match(Msg: TIdSipMessage): Boolean;
var
  Ack: TIdSipRequest;
begin
  if Msg.IsRequest and (Msg as TIdSipRequest).IsAck then begin
    Ack := Msg as TIdSipRequest;
    Result := (Self.InitialRequest.From.Tag = Ack.From.Tag)
          and (Self.InitialRequest.CallID = Ack.CallID)
          and (Self.LocalTag = Ack.ToHeader.Tag)
          and (Self.InitialRequest.CSeq.SequenceNo = Ack.CSeq.SequenceNo);
  end
  else
    Result := inherited Match(Msg);
end;

procedure TIdSipInboundInvite.Redirect(NewDestination: TIdSipAddressHeader;
                                       Temporary: Boolean = true);
var
  RedirectResponse: TIdSipResponse;
  RedirectType:     Cardinal;
begin
  if Temporary then
    RedirectType := SIPMovedTemporarily
  else
    RedirectType := SIPMovedPermanently;

  RedirectResponse := Self.UA.CreateResponse(Self.InitialRequest,
                                             RedirectType);
  try
    RedirectResponse.AddHeader(ContactHeaderFull).Value := NewDestination.FullValue;
    Self.SendResponse(RedirectResponse);
  finally
    RedirectResponse.Free;
  end;
end;

procedure TIdSipInboundInvite.RejectCallBusy;
begin
  Self.SendSimpleResponse(SIPBusyHere);
end;

procedure TIdSipInboundInvite.RemoveListener(const Listener: IIdSipInboundInviteListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

procedure TIdSipInboundInvite.ResendOk;
begin
  if Self.SentFinalResponse and not Self.ReceivedAck then begin
    Self.SendResponse(Self.LastResponse);
    Self.ResendInterval := 2*Self.ResendInterval;

    if (Self.ResendInterval > Self.MaxResendInterval) then
      Self.NotifyOfFailure
    else
      Self.ScheduleResendOk(Self.ResendInterval);
  end;
end;

procedure TIdSipInboundInvite.Ring;
begin
  if not Self.SentFinalResponse then begin
    Self.SendSimpleResponse(SIPRinging);

    Self.UA.ScheduleEvent(TIdSipInboundInviteSessionProgress,
                          Self.ProgressResendInterval,
                          Self.InitialRequest,
                          Self.ID);
  end;
end;

procedure TIdSipInboundInvite.SendSessionProgress;
begin
  if not Self.SentFinalResponse then begin
    Self.SendSimpleResponse(SIPSessionProgress);

    Self.UA.ScheduleEvent(TIdSipInboundInviteSessionProgress,
                          Self.ProgressResendInterval,
                          Self.InitialRequest,
                          Self.ID);
  end;
end;

procedure TIdSipInboundInvite.Terminate;
begin
  if not Self.SentFinalResponse then
    Self.SendSimpleResponse(SIPRequestTerminated);

  inherited Terminate;
end;

procedure TIdSipInboundInvite.TimeOut;
begin
  // Either the INVITE that caused my creation had an Expires header (and that
  // time has now arrived), or my UA has decided that the user's taken too long
  // to answer. Either way, we've decided to time out the inbound INVITE.

  Self.Terminate;
  Self.NotifyOfFailure;
end;

//* TIdSipInboundInvite Protected methods **************************************

procedure TIdSipInboundInvite.Initialise(UA: TIdSipAbstractUserAgent;
                                         Request: TIdSipRequest;
                                         UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.InviteModule := Self.UA.ModuleFor(MethodInvite) as TIdSipInviteModule;

  Self.LastResponse      := TIdSipResponse.Create;
  Self.ReceivedAck       := false;
  Self.ResendInterval    := Self.InitialResendInterval;
end;

procedure TIdSipInboundInvite.ReceiveAck(Ack: TIdSipRequest);
begin
  inherited ReceiveAck(Ack);

  if not Self.ReceivedAck then begin
    Self.ReceivedAck := true;
    Self.NotifyOfSuccess(Ack);
  end;
end;

procedure TIdSipInboundInvite.ReceiveCancel(Cancel: TIdSipRequest);
begin
  inherited ReceiveCancel(Cancel);

  if not Self.SentFinalResponse then begin
    Self.SendCancelResponse(Cancel);
    Self.NotifyOfFailure;
  end;
end;

procedure TIdSipInboundInvite.ReceiveInvite(Invite: TIdSipRequest);
begin
  inherited ReceiveInvite(Invite);

  // Do nothing. Invite contains a resend of the INVITE that made the UA create
  // this action, and we've already sent a response (at the least a 180
  // Ringing). That response should stop the far side resending the INVITEs, but
  // the far side might have sent Invite before it received our response. Either
  // way, we need do nothing.
end;

procedure TIdSipInboundInvite.SendResponse(Response: TIdSipResponse);
begin
  if Self.InitialRequest.Match(Response) then begin
    if not Self.SentFinalResponse then begin
      Self.SentFinalResponse := Response.IsFinal;
      Self.LastResponse.Assign(Response);
    end;

    if not Response.IsOK and Response.IsFinal then
      Self.MarkAsTerminated;
  end;

  inherited SendResponse(Response);
end;

//* TIdSipInboundInvite Private methods ****************************************

function TIdSipInboundInvite.GetInitialResendInterval: Cardinal;
begin
  Result := Self.InviteModule.InitialResendInterval;
end;

function TIdSipInboundInvite.GetProgressResendInterval: Cardinal;
begin
  Result := Self.InviteModule.ProgressResendInterval;
end;

procedure TIdSipInboundInvite.NotifyOfFailure;
var
  Notification: TIdSipInboundInviteFailureMethod;
begin
  Notification := TIdSipInboundInviteFailureMethod.Create;
  try
    Notification.Invite := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;

  Self.MarkAsTerminated;
end;

procedure TIdSipInboundInvite.NotifyOfSuccess(Ack: TIdSipRequest);
var
  Notification: TIdSipInboundInviteSuccessMethod;
begin
  Notification := TIdSipInboundInviteSuccessMethod.Create;
  try
    Notification.Ack    := Ack;
    Notification.Invite := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipInboundInvite.ScheduleResendOk(Interval: Cardinal);
begin
  Self.UA.ScheduleEvent(TIdSipInboundInviteResendOk,
                        Interval,
                        Self.InitialRequest,
                        Self.ID);
end;

procedure TIdSipInboundInvite.SendCancelResponse(Cancel: TIdSipRequest);
var
  Ok: TIdSipResponse;
begin
  Ok := Self.UA.CreateResponse(Cancel, SIPOK);
  try
    Self.SendResponse(Ok);
  finally
    Ok.Free;
  end;
end;

procedure TIdSipInboundInvite.SendSimpleResponse(StatusCode: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.UA.CreateResponse(Self.InitialRequest,
                                     StatusCode);
  try
    Response.ToHeader.Tag := Self.LocalTag;
    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

//******************************************************************************
//* TIdSipOutboundInvite                                                       *
//******************************************************************************
//* TIdSipOutboundInvite Public methods ****************************************

destructor TIdSipOutboundInvite.Destroy;
begin
  Self.CancelRequest.Free;
  Self.AnswerResponse.Free;

  inherited Destroy;
end;

procedure TIdSipOutboundInvite.AddListener(const Listener: IIdSipInviteListener);
begin
  Self.Listeners.AddListener(Listener);
end;

procedure TIdSipOutboundInvite.Cancel;
begin
  if Self.ReceivedFinalResponse then Exit;

  Self.Cancelling := true;

  if Self.HasReceivedProvisionalResponse then
    Self.SendCancel;
end;

function TIdSipOutboundInvite.Match(Msg: TIdSipMessage): Boolean;
begin
  if Self.ReceivedFinalResponse and Msg.IsResponse then
    Result := Self.AnswerResponse.Equals(Msg)
  else
    Result := inherited Match(Msg);
end;

procedure TIdSipOutboundInvite.RemoveListener(const Listener: IIdSipInviteListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

procedure TIdSipOutboundInvite.Send;
var
  Invite: TIdSipRequest;
begin
  inherited Send;

  Invite := Self.CreateInvite;
  try
    Self.InitialRequest.Assign(Invite);
    Self.SendRequest(Invite);
  finally
    Invite.Free;
  end;
end;

procedure TIdSipOutboundInvite.SendAck(Dialog: TIdSipDialog;
                                       FinalResponse: TIdSipResponse);
var
  Ack: TIdSipRequest;
begin
  Ack := Self.Module.CreateAck(Dialog);
  try
    Self.SetAckBody(Ack);

    // cf. RFC 3261 section 22.1
    if Self.InitialRequest.HasAuthorization then
      Ack.FirstAuthorization.Value := Self.InitialRequest.FirstAuthorization.FullValue;
    if Self.InitialRequest.HasProxyAuthorization then
      Ack.FirstProxyAuthorization.Value := Self.InitialRequest.FirstProxyAuthorization.FullValue;

    Self.SendRequest(Ack);
  finally
    Ack.Free;
  end;
end;

procedure TIdSipOutboundInvite.TransactionCompleted;
begin
  Self.MarkAsTerminated;
end;

procedure TIdSipOutboundInvite.Terminate;
begin
  Self.Cancel;
end;

//* TIdSipOutboundInvite Protected methods *************************************

procedure TIdSipOutboundInvite.ActionSucceeded(Response: TIdSipResponse);
begin
  Self.NotifyOfSuccess(Response);

  // We only call SendAck here because we need to give the listeners (especially
  // the Session that created this Invite) time to process the message. The
  // Session especially needs to have its Dialog receive the response to set up
  // the Dialog, otherwise the ACK will not be well formed.
  if Response.IsOK then
    Self.SendAck(Self.Dialog, Response);
end;

procedure TIdSipOutboundInvite.Initialise(UA: TIdSipAbstractUserAgent;
                                          Request: TIdSipRequest;
                                          UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  // We only instantiate CancelRequest when we actually send a Cancel.
  // See SendCancel.
  Self.AnswerResponse                 := TIdSipResponse.Create;
  Self.Cancelling                     := false;
  Self.HasReceivedProvisionalResponse := false;
  Self.ReceivedFinalResponse          := false;
  Self.SentCancel                     := false;

  Self.CancelRequest := TIdSipRequest.Create;
end;

procedure TIdSipOutboundInvite.NotifyOfFailure(Response: TIdSipResponse);
var
  Notification: TIdSipInviteFailureMethod;
begin
  Notification := TIdSipInviteFailureMethod.Create;
  try
    Notification.Invite   := Self;
    Notification.Reason   := Response.Description;
    Notification.Response := Response;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;

  Self.MarkAsTerminated;
end;

function TIdSipOutboundInvite.ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionStatus;
begin
  Result := inherited ReceiveFailureResponse(Response);

  if not Self.ReceivedFinalResponse then begin
    Self.ReceivedFinalResponse := true;
    Self.AnswerResponse.Assign(Response);
  end;
end;

function TIdSipOutboundInvite.ReceiveGlobalFailureResponse(Response: TIdSipResponse): TIdSipActionStatus;
begin
  Result := inherited ReceiveGlobalFailureResponse(Response);

  if not Self.ReceivedFinalResponse then begin
    Self.ReceivedFinalResponse := true;
    Self.AnswerResponse.Assign(Response);
  end;
end;

function TIdSipOutboundInvite.ReceiveOKResponse(Response: TIdSipResponse;
                                                UsingSecureTransport: Boolean): TIdSipActionStatus;
begin
  // REMEMBER: A 2xx response to an INVITE DOES NOT take place in a transaction!
  // A 2xx response immediately terminates a client INVITE transaction so that
  // the ACK can get passed up to the UA (as an unhandled request).
  Result := asFailure;

  if Self.Cancelling and Self.CancelRequest.Match(Response) then begin
    // We received a 2xx for the CANCEL. Do nothing.
  end
  else begin
    // Either we're not cancelling, or the 2xx doesn't match the CANCEL and
    // thus must match the INVITE.

    if not Self.ReceivedFinalResponse then begin
      Self.ReceivedFinalResponse := true;
      Self.AnswerResponse.Assign(Response);

      // cf. RFC 3261, section 13.2.2.4 (last two paragraphs)
      if Response.IsOK then
        Self.UA.ScheduleEvent(TIdSipOutboundInviteTransactionComplete,
                              64*DefaultT1,
                              Self.InitialRequest,
                              Self.ID);
    end;

    if Self.Cancelling then begin
      // (a) Don't bother notifying of an established dialog - the dialog's
      //     cancelled and we'll tear it down immediately.
      // (b) Send the BYE to tear down the cancelled session.
      Self.SendAckFor(Response, UsingSecureTransport);
      Self.SendBye(Response, UsingSecureTransport);
    end
    else begin
      Result := asSuccess;

      if not Self.DialogEstablished then begin
        Self.NotifyOfDialogEstablished(Response, UsingSecureTransport);

        Assert(Assigned(Self.Dialog),
               'Nothing set this Invite''s Dialog property');
      end
      else begin
        // Catchall clause. We shouldn't ever reach this.
        Result := inherited ReceiveOKResponse(Response, UsingSecureTransport);
      end;
    end;
  end;
end;

function TIdSipOutboundInvite.ReceiveProvisionalResponse(Response: TIdSipResponse;
                                                         UsingSecureTransport: Boolean): TIdSipActionStatus;
begin
  Result := asSuccess;

  Self.HasReceivedProvisionalResponse := true;
  Self.NotifyOfCallProgress(Response);  

  if not Self.DialogEstablished
    and not Response.IsTrying
    and Response.ToHeader.HasTag then
      Self.NotifyOfDialogEstablished(Response, UsingSecureTransport);

  if Self.Cancelling and not Self.SentCancel then
    Self.SendCancel;
end;

function TIdSipOutboundInvite.ReceiveRedirectionResponse(Response: TIdSipResponse;
                                                         UsingSecureTransport: Boolean): TIdSipActionStatus;
begin
  Result := inherited ReceiveRedirectionResponse(Response, UsingSecureTransport);

  if not Self.ReceivedFinalResponse then begin
    Self.ReceivedFinalResponse := true;
    Self.AnswerResponse.Assign(Response);
  end;

  Self.NotifyOfRedirect(Response);
end;

function TIdSipOutboundInvite.ReceiveServerFailureResponse(Response: TIdSipResponse): TIdSipActionStatus;
begin
  Result := inherited ReceiveServerFailureResponse(Response);

  if not Self.ReceivedFinalResponse then begin
    Self.ReceivedFinalResponse := true;
    Self.AnswerResponse.Assign(Response);
  end;
end;

//* TIdSipOutboundInvite Private methods ***************************************

procedure TIdSipOutboundInvite.NotifyOfCallProgress(Response: TIdSipResponse);
var
  Notification: TIdSipInviteCallProgressMethod;
begin
  Notification := TIdSipInviteCallProgressMethod.Create;
  try
    Notification.Invite   := Self;
    Notification.Response := Response;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipOutboundInvite.NotifyOfDialogEstablished(Response: TIdSipResponse;
                                                         UsingSecureTransport: Boolean);
var
  Dialog:       TIdSipDialog;
  Notification: TIdSipInviteDialogEstablishedMethod;
begin
  if not Self.DialogEstablished then begin
    Self.DialogEstablished := true;

    Dialog := TIdSipDialog.CreateOutboundDialog(Self.InitialRequest,
                                                Response,
                                                UsingSecureTransport);
    try
      Dialog.ReceiveRequest(Self.InitialRequest);
      Dialog.ReceiveResponse(Response);

      Notification := TIdSipInviteDialogEstablishedMethod.Create;
      try
        Notification.Invite   := Self;
        Notification.Dialog := Dialog;

        Self.Listeners.Notify(Notification);
      finally
        Notification.Free;
      end;
    finally
      Dialog.Free;
    end;
  end;
end;

procedure TIdSipOutboundInvite.NotifyOfRedirect(Response: TIdSipResponse);
var
  Notification: TIdSipInviteRedirectMethod;
begin
  Notification := TIdSipInviteRedirectMethod.Create;
  try
    Notification.Invite   := Self;
    Notification.Response := Response;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;


procedure TIdSipOutboundInvite.NotifyOfSuccess(Response: TIdSipResponse);
var
  Notification: TIdSipInviteSuccessMethod;
begin
  Notification := TIdSipInviteSuccessMethod.Create;
  try
    Notification.Invite   := Self;
    Notification.Response := Response;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipOutboundInvite.SendAckFor(Response: TIdSipResponse;
                                          UsingSecureTransport: Boolean);
var
  Ack: TIdSipRequest;
  Dlg: TIdSipDialog;
begin
  // We only call this when we've no interest in establishing a dialog - when
  // we get a 2xx to our INVITE after sending a CANCEL.
  Dlg := TIdSipDialog.CreateOutboundDialog(Self.InitialRequest,
                                           Response,
                                           UsingSecureTransport);
  try
    Ack := Self.Module.CreateAck(Dlg);
    try
      // We're not actually interested in setting up the session media. However,
      // the remote end expects to see an ACK with an offer so that's what we
      // give the remote end.
      Self.SetAckBody(Ack);

      Self.SendRequest(Ack);
    finally
      Ack.Free;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TIdSipOutboundInvite.SendBye(Response: TIdSipResponse;
                                       UsingSecureTransport: Boolean);
var
  Bye: TIdSipRequest;
  Dlg: TIdSipDialog;
begin
  Dlg := TIdSipDialog.CreateOutboundDialog(Self.InitialRequest,
                                           Response,
                                           UsingSecureTransport);
  try
    Bye := Self.Module.CreateBye(Dlg);
    try
      Self.SendRequest(Bye);
    finally
      Bye.Free;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TIdSipOutboundInvite.SendCancel;
var
  Cancel: TIdSipRequest;
begin
  Assert(not Self.SentCancel, 'SendCancel already invoked');
  Self.SentCancel := true;

  Cancel := Self.InitialRequest.CreateCancel;
  try
    Self.CancelRequest.Assign(Cancel);
  finally
    Cancel.Free;
  end;

  Self.SendRequest(Self.CancelRequest);
end;

procedure TIdSipOutboundInvite.SetAckBody(Ack: TIdSipRequest);
begin
  if Self.InitialRequest.HasBody then begin
    // cf. RFC 3261, section 13.2.2.4
    Ack.Body        := Self.InitialRequest.Body;
    Ack.ContentType := Self.InitialRequest.ContentType;
  end
  else begin
    Ack.Body        := Self.Offer;
    Ack.ContentType := Self.MimeType;
  end;
  Ack.ContentDisposition.Value := DispositionSession;
  Ack.ContentLength := Length(Ack.Body);
end;

//******************************************************************************
//* TIdSipOutboundInitialInvite                                                *
//******************************************************************************
//* TIdSipOutboundInitialInvite Public methods *********************************

destructor TIdSipOutboundInitialInvite.Destroy;
begin
  Self.fDestination.Free;

  inherited Destroy;
end;

//* TIdSipOutboundInitialInvite Protected methods ******************************

function TIdSipOutboundInitialInvite.CreateInvite: TIdSipRequest;
begin
  Result := Self.Module.CreateInvite(Self.Destination, Self.Offer, Self.MimeType);
end;

procedure TIdSipOutboundInitialInvite.Initialise(UA: TIdSipAbstractUserAgent;
                                                 Request: TIdSipRequest;
                                                 UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.fDestination := TIdSipAddressHeader.Create;
end;

//* TIdSipOutboundInitialInvite Private methods ********************************

procedure TIdSipOutboundInitialInvite.SetDestination(Value: TIdSipAddressHeader);
begin
  Self.fDestination.Assign(Value);
end;

//******************************************************************************
//* TIdSipOutboundRedirectedInvite                                             *
//******************************************************************************
//* TIdSipOutboundRedirectedInvite Public methods ******************************

destructor TIdSipOutboundRedirectedInvite.Destroy;
begin
  Self.fOriginalInvite.Free;
  Self.fContact.Free;

  inherited Destroy;
end;

//* TIdSipOutboundRedirectedInvite Protected methods ***************************

function TIdSipOutboundRedirectedInvite.CreateInvite: TIdSipRequest;
begin
  // Use this method in the context of a redirect to an INVITE.
  // cf RFC 3261,  section 8.1.3.4

  Result := TIdSipRequest.Create;
  Result.Assign(Self.OriginalInvite);
  Result.CSeq.SequenceNo := Self.UA.NextInitialSequenceNo;
  Result.LastHop.Branch := Self.UA.NextBranch;
  Result.RequestUri := Self.Contact.Address;
end;

procedure TIdSipOutboundRedirectedInvite.Initialise(UA: TIdSipAbstractUserAgent;
                                                    Request: TIdSipRequest;
                                                    UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.fContact        := TIdSipAddressHeader.Create;
  Self.fOriginalInvite := TIdSipRequest.Create;
end;

//* TIdSipOutboundRedirectedInvite Private methods *****************************

procedure TIdSipOutboundRedirectedInvite.SetContact(Value: TIdSipAddressHeader);
begin
  Self.fContact.Assign(Value);
end;

procedure TIdSipOutboundRedirectedInvite.SetOriginalInvite(Value: TIdSipRequest);
begin
  Self.OriginalInvite.Assign(Value);
end;

//******************************************************************************
//* TIdSipOutboundReInvite                                                     *
//******************************************************************************
//* TIdSipOutboundReInvite Public methods **************************************

destructor TIdSipOutboundReInvite.Destroy;
begin
  Self.fOriginalInvite.Free;

  inherited Destroy;
end;

//* TIdSipOutboundReInvite Protected methods ***********************************

function TIdSipOutboundReInvite.CreateInvite: TIdSipRequest;
begin
  Result := Self.Module.CreateReInvite(Self.Dialog, Self.Offer, Self.MimeType);
  // Re-INVITEs use the same credentials as the original INVITE that
  // established the dialog.
  Result.CopyHeaders(Self.OriginalInvite, AuthorizationHeader);
  Result.CopyHeaders(Self.OriginalInvite, ProxyAuthorizationHeader);
end;

procedure TIdSipOutboundReInvite.Initialise(UA: TIdSipAbstractUserAgent;
                                            Request: TIdSipRequest;
                                            UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.fOriginalInvite := TIdSipRequest.Create;
end;

//* TIdSipOutboundReInvite Private methods *************************************

procedure TIdSipOutboundReInvite.SetOriginalInvite(Value: TIdSipRequest);
begin
  Self.fOriginalInvite.Assign(Value);
end;

//******************************************************************************
//* TIdSipOutboundReplacingInvite                                              *
//******************************************************************************
//* TIdSipOutboundReplacingInvite Protected methods ****************************

function TIdSipOutboundReplacingInvite.CreateInvite: TIdSipRequest;
begin
  Result := Self.Module.CreateInvite(Self.Destination, Self.Offer, Self.MimeType);
  Result.AddHeader(ReplacesHeader);
  Result.FirstReplaces.CallID  := Self.CallID;
  Result.FirstReplaces.FromTag := Self.FromTag;
  Result.FirstReplaces.ToTag   := Self.ToTag;
end;

//******************************************************************************
//* TIdSipOptions                                                              *
//******************************************************************************
//* TIdSipOptions Public methods ***********************************************

class function TIdSipOptions.Method: String;
begin
  Result := MethodOptions;
end;

function TIdSipOptions.IsOptions: Boolean;
begin
  Result := true;
end;

//* TIdSipOptions Protected methods ********************************************

function TIdSipOptions.CreateNewAttempt: TIdSipRequest;
var
  TempTo: TIdSipToHeader;
begin
  TempTo := TIdSipToHeader.Create;
  try
    TempTo.Address := Self.InitialRequest.RequestUri;

    Result := Self.UA.CreateOptions(TempTo);
  finally
    TempTo.Free;
  end;
end;

//******************************************************************************
//* TIdSipInboundOptions                                                       *
//******************************************************************************
//* TIdSipInboundOptions Public methods ****************************************

function TIdSipInboundOptions.IsInbound: Boolean;
begin
  Result := true;
end;

procedure TIdSipInboundOptions.ReceiveRequest(Options: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Assert(Options.IsOptions, 'TIdSipAction.ReceiveOptions must only receive OPTIONSes');

  Response := Self.UA.CreateResponse(Options,
                                     Self.UA.ResponseForInvite);
  try
    Response.AddHeader(AcceptHeader).Value := Self.UA.AllowedContentTypes;
    Response.AddHeader(AllowHeader).Value  := Self.UA.KnownMethods;
    Response.AddHeader(AcceptEncodingHeader).Value := Self.UA.AllowedEncodings;
    Response.AddHeader(AcceptLanguageHeader).Value := Self.UA.AllowedLanguages;
    Response.AddHeader(SupportedHeaderFull).Value := Self.UA.AllowedExtensions;
    Response.AddHeader(ContactHeaderFull).Assign(Self.UA.Contact);

    // For OPTIONS "traceroute"-like functionality. cf RFC 3261, section 11.2
    Response.FirstWarning.Code  := WarningMisc;
    Response.FirstWarning.Agent := Self.UA.HostName;
    // This should contain the IP of the transport that received the OPTIONS.
    Response.FirstWarning.Text  := '';

    Self.SendResponse(Response);
  finally
    Response.Free;
  end;

  Self.Terminate;
end;

//******************************************************************************
//* TIdSipOutboundOptions                                                      *
//******************************************************************************
//* TIdSipOutboundOptions Public methods ***************************************

destructor TIdSipOutboundOptions.Destroy;
begin
  Self.fServer.Free;

  inherited Destroy;
end;

procedure TIdSipOutboundOptions.AddListener(const Listener: IIdSipOptionsListener);
begin
  Self.Listeners.AddListener(Listener);
end;

procedure TIdSipOutboundOptions.RemoveListener(const Listener: IIdSipOptionsListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

procedure TIdSipOutboundOptions.Send;
var
  Options: TIdSipRequest;
begin
  inherited Send;

  Options := Self.UA.CreateOptions(Self.Server);
  try
    Self.InitialRequest.Assign(Options);
    Self.SendRequest(Options);
  finally
    Options.Free;
  end;
end;

//* TIdSipOutboundOptions Protected methods ************************************

procedure TIdSipOutboundOptions.ActionSucceeded(Response: TIdSipResponse);
begin
  Self.NotifyOfResponse(Response);
end;

procedure TIdSipOutboundOptions.Initialise(UA: TIdSipAbstractUserAgent;
                                           Request: TIdSipRequest;
                                           UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.fServer := TIdSipAddressHeader.Create;
end;

procedure TIdSipOutboundOptions.NotifyOfFailure(Response: TIdSipResponse);
begin
  Self.NotifyOfResponse(Response);
end;

//* TIdSipOutboundOptions Private methods **************************************

procedure TIdSipOutboundOptions.NotifyOfResponse(Response: TIdSipResponse);
var
  Notification: TIdSipOptionsResponseMethod;
begin
  Notification := TIdSipOptionsResponseMethod.Create;
  try
    Notification.Options  := Self;
    Notification.Response := Response;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;

  Self.Terminate;
end;

procedure TIdSipOutboundOptions.SetServer(Value: TIdSipAddressHeader);
begin
  Self.fServer.Assign(Value);
end;

//******************************************************************************
//* TIdSipRegistration                                                         *
//******************************************************************************
//* TIdSipRegistration Public methods ******************************************

class function TIdSipRegistration.Method: String;
begin
  Result := MethodRegister;
end;

function TIdSipRegistration.IsRegistration: Boolean;
begin
  Result := true;
end;

//* TIdSipRegistration Protected methods ***************************************

function TIdSipRegistration.CreateNewAttempt: TIdSipRequest;
var
  TempTo: TIdSipToHeader;
begin
  TempTo := TIdSipToHeader.Create;
  try
    TempTo.Address := Self.InitialRequest.RequestUri;

    Result := Self.OutModule.CreateRegister(TempTo);
  finally
    TempTo.Free;
  end;
end;

procedure TIdSipRegistration.Initialise(UA: TIdSipAbstractUserAgent;
                                        Request: TIdSipRequest;
                                        UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.OutModule := UA.ModuleFor(TIdSipOutboundRegisterModule) as TIdSipOutboundRegisterModule;
end;

//******************************************************************************
//* TIdSipInboundRegistration                                                  *
//******************************************************************************
//* TIdSipInboundRegistration Public methods ***********************************

function TIdSipInboundRegistration.IsInbound: Boolean;
begin
  Result := true;
end;

procedure TIdSipInboundRegistration.ReceiveRequest(Register: TIdSipRequest);
var
  Bindings: TIdSipContacts;
  Date:     TIdSipDateHeader;
  Response: TIdSipResponse;
begin
  Assert(Register.IsRegister, 'TIdSipAction.ReceiveRegister must only receive REGISTERs');

  if not Self.AcceptRequest(Register) then Exit;

  if (Register.ContactCount = 1)
    and Register.FirstContact.IsWildCard
    and (Register.QuickestExpiry = 0) then begin

    if not Self.BindingDB.RemoveAllBindings(Register) then
      Self.RejectFailedRequest(Register)
    else
      Self.SendSimpleResponse(Register, SIPOK);
    Exit;
  end;

  if not Self.BindingDB.AddBindings(Register) then begin
    Self.RejectFailedRequest(Register);
    Exit;
  end;

  Bindings := TIdSipContacts.Create;
  try
    if Self.BindingDB.BindingsFor(Register,
                                  Bindings) then begin
      Response := Self.UA.CreateResponse(Register, SIPOK);
      try
        Response.AddHeaders(Bindings);

        Date := TIdSipDateHeader.Create;
        try
          Date.Time.SetFromTDateTime(Now);
          Response.AddHeader(Date);
        finally
          Date.Free;
        end;

        Self.SendResponse(Response);
      finally
        Response.Free;
      end;
    end
    else begin
      Self.RejectFailedRequest(Register);
    end;
  finally
    Bindings.Free;
  end;

  Self.Terminate;
end;

//* TIdSipInboundRegistration Protected methods ********************************

procedure TIdSipInboundRegistration.Initialise(UA: TIdSipAbstractUserAgent;
                                               Request: TIdSipRequest;
                                               UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.InitialRequest.Assign(Request);
  Self.RegisterModule := Self.UA.ModuleFor(MethodRegister) as TIdSipRegisterModule;

  Assert(Assigned(Self.RegisterModule),
         'The Transaction-User layer cannot process REGISTER methods without adding the Registration module to it');
end;

//* TIdSipInboundRegistration Private methods **********************************

function TIdSipInboundRegistration.AcceptRequest(Request: TIdSipRequest): Boolean;
begin
  // cf RFC 3261 section 10.3
  // Steps 1, 2 & 3 - covered by Self.UA
  Result := true;

  // Step 4
  if not Self.BindingDB.IsAuthorized(Request.From, Request.RequestUri) then begin
    Self.RejectForbidden(Request);
    Result := false;
    Exit;
  end;

  // Step 5
  if not Self.BindingDB.IsValid(Request) then begin
    Self.RejectNotFound(Request);
    Result := false;
    Exit;
  end;

  // Step 6 (or part thereof)
  if Request.HasHeader(ContactHeaderFull) then begin
    if Request.FirstContact.IsWildCard then begin
      if (Request.ContactCount > 1) then begin
        Self.RejectRequest(Request, SIPBadRequest);
        Result := false;
        Exit;
      end;

      if Request.FirstContact.WillExpire
        and (Request.FirstContact.Expires = 0) then
          Result := true
      else begin
        Self.RejectRequest(Request, SIPBadRequest);
        Result := false;
        Exit;
      end;
    end
    else if Request.HasExpiry and (Request.QuickestExpiry < Self.RegisterModule.MinimumExpiryTime) then begin
      Self.RejectExpireTooBrief(Request);
      Result := false;
    end;
  end;

  // Steps 7 & 8 in ReceiveRequest
end;

function TIdSipInboundRegistration.BindingDB: TIdSipAbstractBindingDatabase;
begin
  Result := Self.RegisterModule.BindingDB;
end;

procedure TIdSipInboundRegistration.RejectExpireTooBrief(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.UA.CreateResponse(Request,
                                     SIPIntervalTooBrief);
  try
    Response.AddHeader(MinExpiresHeader).Value := IntToStr(Self.RegisterModule.MinimumExpiryTime);
    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipInboundRegistration.RejectFailedRequest(Request: TIdSipRequest);
begin
  Self.RejectRequest(Request, SIPInternalServerError);
end;

procedure TIdSipInboundRegistration.RejectForbidden(Request: TIdSipRequest);
begin
  Self.RejectRequest(Request, SIPForbidden);
end;

procedure TIdSipInboundRegistration.RejectNotFound(Request: TIdSipRequest);
begin
  Self.RejectRequest(Request, SIPNotFound);
end;

procedure TIdSipInboundRegistration.RejectRequest(Request: TIdSipRequest;
                                                  StatusCode: Cardinal);
begin
  Self.SendSimpleResponse(Request, StatusCode);
end;

procedure TIdSipInboundRegistration.SendSimpleResponse(Request: TIdSipRequest;
                                                       StatusCode: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.UA.CreateResponse(Request,
                                     StatusCode);
  try
    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

//******************************************************************************
//* TIdSipOutboundRegistration                                                 *
//******************************************************************************
//* TIdSipOutboundRegistration Public methods **********************************

destructor TIdSipOutboundRegistration.Destroy;
begin
  Self.fRegistrar.Free;
  Self.fBindings.Free;

  inherited Destroy;
end;

procedure TIdSipOutboundRegistration.AddListener(const Listener: IIdSipRegistrationListener);
begin
  Self.Listeners.AddListener(Listener);
end;

function TIdSipOutboundRegistration.ReregisterTime(Expires: Cardinal): Cardinal;
begin
  // Expires magnitude:                  Result
  // Expires >= 20 minutes               Expires - 5 minutes
  // 1 minute <= Expires < 20 minutes    Expires - 1 minute
  // Expires < 1 minute                  0.8 * Expires

  // Postcondition: Result > 0

  if (Expires <= 1) then
    Result := 1
  else if (Expires < OneMinute) then
    Result := 4*(Expires div 5)
  else if (Expires < TwentyMinutes) then
    Result := Expires - OneMinute
  else
    Result := Expires - FiveMinutes;
end;

procedure TIdSipOutboundRegistration.RemoveListener(const Listener: IIdSipRegistrationListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

//* TIdSipOutboundRegistration Protected methods *******************************

procedure TIdSipOutboundRegistration.ActionSucceeded(Response: TIdSipResponse);
begin
  Self.NotifyOfSuccess(Response);
end;

function TIdSipOutboundRegistration.CreateRegister(Registrar: TIdSipUri;
                                                   Bindings: TIdSipContacts): TIdSipRequest;
var
  ToHeader: TIdSipToHeader;
begin
  ToHeader := TIdSipToHeader.Create;
  try
    ToHeader.Address := Registrar;

    Result := Self.OutModule.CreateRegister(ToHeader);

    // Bindings explicitly carries all Contact information. Thus we must remove
    // any Contact information already in Result.
    Result.Headers.RemoveAll(ContactHeaderFull);

    Result.AddHeaders(Bindings);
  finally
    ToHeader.Free;
  end;
end;

procedure TIdSipOutboundRegistration.Initialise(UA: TIdSipAbstractUserAgent;
                                                Request: TIdSipRequest;
                                                UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.fBindings := TIdSipContacts.Create;
  Self.fRegistrar := TIdSipUri.Create('');
end;

procedure TIdSipOutboundRegistration.NotifyOfFailure(Response: TIdSipResponse);
var
  CurrentBindings: TIdSipContacts;
  Notification:    TIdSipRegistrationFailedMethod;
begin
  CurrentBindings := TIdSipContacts.Create(Response.Headers);
  try
    Notification := TIdSipRegistrationFailedMethod.Create;
    try
      Notification.CurrentBindings := CurrentBindings;
      Notification.Registration    := Self;
      Notification.Response        := Response;

      Self.Listeners.Notify(Notification);
    finally
      Notification.Free;
    end;
  finally
    CurrentBindings.Free;
  end;

  Self.Terminate;
end;

procedure TIdSipOutboundRegistration.NotifyOfSuccess(Response: TIdSipResponse);
var
  CurrentBindings: TIdSipContacts;
  ExpireTime:      Cardinal;
  Notification:    TIdSipRegistrationSucceededMethod;
  OurContact:      TIdSipContactHeader;
begin
  CurrentBindings := TIdSipContacts.Create(Response.Headers);
  try
    Notification := TIdSipRegistrationSucceededMethod.Create;
    try
      Notification.CurrentBindings := CurrentBindings;
      Notification.Registration    := Self;

      Self.Listeners.Notify(Notification);
    finally
      Notification.Free;
    end;

    if Self.OutModule.AutoReRegister then begin
      // OurContact should always be assigned, because we've supposedly just
      // REGISTERed it. If it's not assigned then the registrar didn't actually
      // save our registration, and still had the cheek to return a 2xx rather
      // than some sort've error response.
      OurContact := CurrentBindings.ContactFor(Self.InitialRequest.FirstContact);
      if Assigned(OurContact) then begin

        // ExpireTime represents a seconds value.
        // Using 0 as a sentinel value works because it means "now" - and
        // registrars really shouldn't return a 0. Remember, if a UAC sends a
        // REGISTER with an Expires of 0, the registrar will unregister those
        // contacts!
        ExpireTime := 0;
        if OurContact.WillExpire then
          ExpireTime := OurContact.Expires
        else if Response.HasHeader(ExpiresHeader) then
          ExpireTime := Response.FirstExpires.NumericValue;

        if (ExpireTime > 0) then
          Self.UA.ScheduleEvent(Self.OutModule.OnReregister,
                                Self.ReregisterTime(ExpireTime)*1000, // in milliseconds
                                Self.InitialRequest.Copy);
      end;
    end;
  finally
    CurrentBindings.Free;
  end;

  Self.Terminate;
end;

function TIdSipOutboundRegistration.ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionStatus;
begin
  Result := inherited ReceiveFailureResponse(Response);

  if (Result = asFailure) then begin
    case Response.StatusCode of
      SIPIntervalTooBrief: begin
        Self.ReissueRequestWithLongerExpiry(Self.InitialRequest.RequestUri,
                                            Response.FirstMinExpires.NumericValue);
        Result := asSuccess;
      end;

      SIPBadExtension: begin
        if Self.InitialRequest.HasHeader(RequireHeader) then begin
          Self.RetryWithoutExtensions(Self.InitialRequest.RequestUri,
                                      Response);
          Result := asSuccess;
        end;
      end;
    else
      Result := asFailure;
    end;
  end;
end;

function TIdSipOutboundRegistration.ReceiveRedirectionResponse(Response: TIdSipResponse;
                                                               UsingSecureTransport: Boolean): TIdSipActionStatus;
var
  NewAttempt: TIdSipOutboundRegister;
begin
  Result := asFailure;

  if Response.HasHeader(ContactHeaderFull) then begin
    NewAttempt := Self.OutModule.RegisterWith(Response.FirstContact.Address);
    NewAttempt.AddListeners(Self.Listeners);
    NewAttempt.Send;

    Result := asSuccess;
  end;
end;

procedure TIdSipOutboundRegistration.RegisterWith(Registrar: TIdSipUri;
                                                  Bindings: TIdSipContacts);
var
  Request: TIdSipRequest;
begin
  Request := Self.CreateRegister(Registrar, Bindings);
  try
    Self.SendRequest(Request);
  finally
    Request.Free;
  end;
end;

procedure TIdSipOutboundRegistration.RegisterWith(Registrar: TIdSipUri;
                                                  Contact: TIdSipContactHeader);
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

procedure TIdSipOutboundRegistration.SendRequest(Request: TIdSipRequest;
                                                 TryAgain: Boolean = true);
begin
  Self.InitialRequest.Assign(Request);

  inherited SendRequest(Request, TryAgain);
end;

procedure TIdSipOutboundRegistration.Unregister(Registrar: TIdSipUri);
var
  RemovalBindings: TIdSipContacts;
  Request:         TIdSipRequest;
begin
  RemovalBindings := TIdSipContacts.Create;
  try
    RemovalBindings.Add(ContactHeaderFull);
    RemovalBindings.First;
    RemovalBindings.CurrentContact.IsWildCard := true;

    Request := Self.CreateRegister(Registrar, RemovalBindings);
    try
      Request.FirstExpires.NumericValue := 0;

      Self.SendRequest(Request);
    finally
      Request.Free;
    end;
  finally
    RemovalBindings.Free;
  end;
end;

//* TIdSipOutboundRegistration Private methods *********************************

procedure TIdSipOutboundRegistration.ReissueRequestWithLongerExpiry(Registrar: TIdSipUri;
                                                                    MinimumExpiry: Cardinal);
var
  Bindings: TIdSipContacts;
  OriginalBindings: TIdSipContacts;
  Request: TIdSipRequest;
begin
  // We received a 423 Interval Too Brief from the registrar. Therefore we
  // make a new REGISTER request with the registrar's minimum expiry.
  OriginalBindings := TIdSipContacts.Create(Self.InitialRequest.Headers);
  try
    Request := Self.CreateRegister(Registrar, OriginalBindings);
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
      Self.SendRequest(Request);
    finally
      Request.Free;
    end;
  finally
    OriginalBindings.Free;
  end;
end;

procedure TIdSipOutboundRegistration.RetryWithoutExtensions(Registrar: TIdSipUri;
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

      Self.SendRequest(Request);
    finally
      Request.Free;
    end;
  finally
    Bindings.Free;
  end;
end;

procedure TIdSipOutboundRegistration.SetBindings(Value: TIdSipContacts);
begin
  Self.fBindings.Clear;
  Self.fBindings.Add(Value);
end;

procedure TIdSipOutboundRegistration.SetRegistrar(Value: TIdSipUri);
begin
  Self.fRegistrar.Uri := Value.Uri;
end;

//******************************************************************************
//* TIdSipOutboundRegistrationQuery                                            *
//******************************************************************************
//* TIdSipOutboundRegistrationQuery Public methods *****************************

procedure TIdSipOutboundRegistrationQuery.Send;
begin
  inherited Send;

  Self.Bindings.Clear;
  Self.RegisterWith(Self.Registrar, Self.Bindings);
end;

//******************************************************************************
//* TIdSipOutboundRegister                                                     *
//******************************************************************************
//* TIdSipOutboundRegister Public methods **************************************

procedure TIdSipOutboundRegister.Send;
begin
  inherited Send;

  Self.RegisterWith(Self.Registrar, Self.Bindings);
end;

//******************************************************************************
//* TIdSipOutboundUnRegister                                                   *
//******************************************************************************
//* TIdSipOutboundUnRegister Public methods ************************************

procedure TIdSipOutboundUnRegister.Send;
begin
  inherited Send;

  if Self.IsWildCard then begin
    Self.Bindings.Clear;
    Self.Bindings.Add(ContactHeaderFull);
    Self.Bindings.First;
    Self.Bindings.CurrentContact.IsWildCard := true;
  end else begin
    Self.Bindings.First;
    while Self.Bindings.HasNext do begin
      Self.Bindings.CurrentContact.Expires := ExpireNow;
      Self.Bindings.Next;
    end;
  end;

  Self.RegisterWith(Self.Registrar, Self.Bindings);
end;

//* TIdSipOutboundUnRegister Protected methods *********************************

function TIdSipOutboundUnRegister.CreateRegister(Registrar: TIdSipUri;
                                                 Bindings: TIdSipContacts): TIdSipRequest;
begin
  Result := inherited CreateRegister(Registrar, Bindings);

  Result.FirstExpires.NumericValue := ExpireNow;
end;

procedure TIdSipOutboundUnRegister.Initialise(UA: TIdSipAbstractUserAgent;
                                              Request: TIdSipRequest;
                                              UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.IsWildCard := false;
end;

//******************************************************************************
//* TIdSipSession                                                              *
//******************************************************************************
//* TIdSipSession Public methods ***********************************************

class function TIdSipSession.Method: String;
begin
  Result := MethodInvite;
end;

destructor TIdSipSession.Destroy;
begin
  Self.ModifyLock.Free;

  Self.DialogLock.Acquire;
  try
    Self.fDialog.Free;
  finally
    Self.DialogLock.Release;
  end;
  Self.DialogLock.Free;

  inherited Destroy;
end;

procedure TIdSipSession.AcceptModify(const LocalSessionDescription: String;
                                     const MimeType: String);
begin
  Self.ModifyLock.Acquire;
  try
    if Self.ModificationInProgress then begin
      (Self.ModifyAttempt as TIdSipInboundInvite).Accept(LocalSessionDescription,
                                                         MimeType);
    end;
  finally
    Self.ModifyLock.Release;
  end;
end;

procedure TIdSipSession.AddSessionListener(const Listener: IIdSipSessionListener);
begin
  Self.Listeners.AddListener(Listener);
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
    Result := Self.Dialog <> nil;
  finally
    Self.DialogLock.Release;
  end;
end;

function TIdSipSession.DialogMatches(DialogID: TIdSipDialogID): Boolean;
begin
  Self.DialogLock.Acquire;
  try
    if Self.DialogEstablished then
      Result := Self.Dialog.ID.Equals(DialogID)
    else
      Result := false;
  finally
    Self.DialogLock.Release;
  end;
end;

function TIdSipSession.DialogMatches(Msg: TIdSipMessage): Boolean;
var
  DialogID: TIdSipDialogID;
begin
  DialogID := Self.CreateDialogIDFrom(Msg);
  try
    Result := Self.DialogMatches(DialogID);
  finally
    DialogID.Free;
  end;
end;

function TIdSipSession.IsOutboundCall: Boolean;
begin
  Result := not Self.IsInbound;
end;

function TIdSipSession.IsSession: Boolean;
begin
  Result := true;
end;

function TIdSipSession.ModificationInProgress: Boolean;
begin
  Result := Assigned(Self.ModifyAttempt);
end;

procedure TIdSipSession.Modify(const Offer, ContentType: String);
var
  ReInvite: TIdSipOutboundReInvite;
begin
  if not Self.FullyEstablished then
    raise EIdSipTransactionUser.Create(CannotModifyBeforeEstablished);

  Self.ModifyLock.Acquire;
  try
    if Self.ModificationInProgress then
      raise EIdSipTransactionUser.Create(CannotModifyDuringModification);

    ReInvite := Self.UA.AddOutboundAction(TIdSipOutboundReInvite) as TIdSipOutboundReInvite;
    ReInvite.MimeType          := ContentType;
    ReInvite.Dialog            := Self.Dialog;
    ReInvite.InOutboundSession := Self.IsOutboundCall;
    ReInvite.Offer             := Offer;
    ReInvite.OriginalInvite    := Self.InitialRequest;
    ReInvite.AddListener(Self);
    ReInvite.Send;

    Self.ModifyAttempt := ReInvite;
    Self.LastModifyDescription := Offer;
    Self.LastModifyMimeType    := ContentType;
  finally
    Self.ModifyLock.Release;
  end;
end;

function TIdSipSession.ModifyWaitTime: Cardinal;
begin
  // The amount of time, in milliseconds, to wait before re-attempting a modify
  // that glared. See RFC 3261, cf section 14.1
  Result := 0;
end;

procedure TIdSipSession.ReceiveRequest(Request: TIdSipRequest);
begin
  if Self.IsTerminated then begin
    Self.RejectRequest(Request);
    Exit;
  end
  else inherited ReceiveRequest(Request);
end;

procedure TIdSipSession.Remodify;
begin
  // Reattempt the previously-attempted modify. Don't call this; Notifications
  // (like TIdSipSessionResendReInvite) call this.

  Self.Modify(Self.LastModifyDescription, Self.LastModifyMimeType);
end;

procedure TIdSipSession.RemoveSessionListener(const Listener: IIdSipSessionListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

//* TIdSipSession Protected methods ********************************************

procedure TIdSipSession.ActionSucceeded(Response: TIdSipResponse);
begin
  if Self.DialogEstablished then
    Self.Dialog.ReceiveResponse(Response);
end;

function TIdSipSession.CreateNewAttempt: TIdSipRequest;
begin
  Result := Self.Module.CreateInvite(Self.InitialRequest.ToHeader,
                                     Self.InitialRequest.Body,
                                     Self.InitialRequest.ContentType);
end;

procedure TIdSipSession.Initialise(UA: TIdSipAbstractUserAgent;
                                   Request: TIdSipRequest;
                                   UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.DialogLock := TCriticalSection.Create;
  Self.ModifyLock := TCriticalSection.Create;

  Self.fReceivedAck     := false;
  Self.FullyEstablished := false;
end;                                   

function TIdSipSession.GetDialog: TIdSipDialog;
begin
  Result := Self.fDialog;
end;

function TIdSipSession.GetInvite: TIdSipRequest;
begin
  Result := Self.InitialRequest;
end;

procedure TIdSipSession.NotifyOfEndedSession(ErrorCode: Cardinal;
                                             const Reason: String);
var
  Notification: TIdSipEndedSessionMethod;
begin
  Notification := TIdSipEndedSessionMethod.Create;
  try
    Notification.ErrorCode := ErrorCode;
    Notification.Reason    := Reason;
    Notification.Session   := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipSession.NotifyOfEstablishedSession(const RemoteSessionDescription: String;
                                                   const MimeType: String);
var
  Notification: TIdSipEstablishedSessionMethod;
begin
  Notification := TIdSipEstablishedSessionMethod.Create;
  try
    Notification.Session                  := Self;
    Notification.RemoteSessionDescription := RemoteSessionDescription;
    Notification.MimeType                 := MimeType;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipSession.NotifyOfFailure(Response: TIdSipResponse);
begin
  Self.MarkAsTerminated;
  Self.NotifyOfEndedSession(Response.StatusCode, Response.StatusText);
end;

procedure TIdSipSession.NotifyOfModifySession(Modify: TIdSipInboundInvite);
var
  Notification: TIdSipSessionModifySessionMethod;
begin
  Notification := TIdSipSessionModifySessionMethod.Create;
  try
    Notification.MimeType                 := Modify.InitialRequest.ContentType;
    Notification.RemoteSessionDescription := Modify.InitialRequest.Body;
    Notification.Session                  := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipSession.OnAuthenticationChallenge(Action: TIdSipAction;
                                                  Response: TIdSipResponse);
begin
  raise Exception.Create('implement TIdSipSession.OnAuthenticationChallenge');
end;

procedure TIdSipSession.OnCallProgress(InviteAgent: TIdSipOutboundInvite;
                                       Response: TIdSipResponse);
var
  Notification: TIdSipProgressedSessionMethod;
begin
  Notification := TIdSipProgressedSessionMethod.Create;
  try
    Notification.Progress := Response;
    Notification.Session  := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipSession.OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                            NewDialog: TIdSipDialog);
begin
  Self.DialogLock.Acquire;
  try
    if Self.DialogEstablished then
      InviteAgent.Dialog := Self.Dialog;
  finally
    Self.DialogLock.Release;
  end;
end;

procedure TIdSipSession.OnNetworkFailure(Action: TIdSipAction;
                                         ErrorCode: Cardinal;
                                         const Reason: String);
begin
  Self.NotifyOfNetworkFailure(ErrorCode, Reason);
end;

procedure TIdSipSession.OnFailure(InviteAgent: TIdSipOutboundInvite;
                                  Response: TIdSipResponse;
                                  const Reason: String);
begin
  Self.ModifyLock.Acquire;
  try
    if (InviteAgent = Self.ModifyAttempt) then begin
      Self.ModifyAttempt := nil;
      case Response.StatusCode of
        //  We attempted to modify the session. The remote end has also
        // attempted to do so, and sent an INVITE before our INVITE arrived.
        // Thus it rejects our attempt with a 491 Request Pending.
        SIPRequestPending: Self.RescheduleModify(InviteAgent);

       // If we receive a 408 Request Timeout or a 481 Call Leg Or Transaction
       // Does Not Exist from our attempted modify then the remote end's
       // disappeared or our session died. We have no choice but to terminate.
        SIPRequestTimeout,
        SIPCallLegOrTransactionDoesNotExist: Self.Terminate;
      else
        // The modify attempt failed. What should we do? Todo!
      end;
    end;
  finally
    Self.ModifyLock.Release;
  end;
end;

procedure TIdSipSession.OnFailure(InviteAgent: TIdSipInboundInvite);
begin
  Self.ModifyLock.Acquire;
  try
    if (InviteAgent = Self.ModifyAttempt) then
      Self.ModifyAttempt := nil;

    Self.Terminate;
  finally
    Self.ModifyLock.Release;
  end;
end;

procedure TIdSipSession.OnRedirect(InviteAgent: TIdSipOutboundInvite;
                                   Redirect: TIdSipResponse);
begin
end;

procedure TIdSipSession.OnSuccess(InviteAgent: TIdSipInboundInvite;
                                  Ack: TIdSipRequest);
begin
  // TODO: Notify listeners of modification, possibly
  Self.ModifyLock.Acquire;
  try
    if (InviteAgent = Self.ModifyAttempt) then begin
      Self.ModifyAttempt := nil;

      Self.LocalSessionDescription  := InviteAgent.LocalSessionDescription;
      Self.LocalMimeType            := InviteAgent.LocalMimeType;
      Self.RemoteSessionDescription := Ack.Body;
      Self.RemoteMimeType           := Ack.ContentType;
    end;
  finally
    Self.ModifyLock.Release;
  end;
end;

procedure TIdSipSession.OnSuccess(InviteAgent: TIdSipOutboundInvite;
                                  Response: TIdSipResponse);
begin
  Self.DialogLock.Acquire;
  try
    if Self.DialogEstablished then
      Self.Dialog.ReceiveResponse(Response);
  finally
    Self.DialogLock.Release;
  end;

  Self.ModifyLock.Acquire;
  try
    if (InviteAgent = Self.ModifyAttempt) then begin
      Self.ModifyAttempt := nil;

      Self.LocalSessionDescription  := InviteAgent.InitialRequest.Body;
      Self.LocalMimeType            := InviteAgent.InitialRequest.ContentType;
      Self.RemoteSessionDescription := Response.Body;
      Self.RemoteMimeType           := Response.ContentType;
    end;
  finally
    Self.ModifyLock.Release;
  end;
end;

procedure TIdSipSession.ReceiveBye(Bye: TIdSipRequest);
var
  OK: TIdSipResponse;
begin
  inherited ReceiveBye(Bye);

  Self.TerminateAnyPendingRequests;

  Self.MarkAsTerminated;
  Self.Dialog.ReceiveRequest(Bye);

  OK := Self.UA.CreateResponse(Bye, SIPOK);
  try
    Self.SendResponse(OK);
  finally
    OK.Free;
  end;

  Self.NotifyOfEndedSession(RemoteHangUp, RSNoReason);
end;

procedure TIdSipSession.ReceiveInitialInvite(Invite: TIdSipRequest);
begin
  // By default do nothing
end;

procedure TIdSipSession.ReceiveInvite(Invite: TIdSipRequest);
var
  Modify: TIdSipInboundInvite;
begin
  // Invite matches this Session's dialog.
  inherited ReceiveInvite(Invite);

  Self.DialogLock.Acquire;
  try
    if not Self.DialogEstablished then
      // No dialog? For an inbound call? Then Invite represents the initial
      // request that caused the creation of this session.
      Self.ReceiveInitialInvite(Invite)
    else begin
      if Self.Dialog.IsOutOfOrder(Invite) then begin
        Self.RejectOutOfOrderRequest(Invite);
        Exit;
      end;

      // If we've not sent a final response, reject with 500 + Retry-After.
      if not Self.FullyEstablished then begin
        Self.RejectPrematureInvite(Invite);
        Exit;
      end;

      Self.ModifyLock.Acquire;
      try
        if not Self.ModificationInProgress then begin
          Modify := Self.Module.AddInboundInvite(Invite, Self.UsingSecureTransport);
          Self.ModifyAttempt := Modify;
          Modify.AddListener(Self);
          Self.NotifyOfModifySession(Modify);
        end
        else
          Self.RejectReInvite(Invite);
      finally
        Self.ModifyLock.Release;
      end;
    end;
  finally
    Self.DialogLock.Release;
  end;
end;

procedure TIdSipSession.SendBye;
var
  Bye: TIdSipRequest;
begin
  Bye := Self.Module.CreateBye(Self.Dialog);
  try
    // TODO: Verify this as correct behaviour. Otherwise we must use SIP discovery stuff
    Bye.LastHop.Transport := Self.InitialRequest.LastHop.Transport;

    // We don't listen to the new transaction because we assume the BYE
    // succeeds immediately.
    Self.SendRequest(Bye, false);
  finally
    Bye.Free;
  end;
end;

//* TIdSipSession Private methods **********************************************

procedure TIdSipSession.NotifyOfModifiedSession(Answer: TIdSipResponse);
var
  Notification: TIdSipModifiedSessionMethod;
begin
  Notification := TIdSipModifiedSessionMethod.Create;
  try
    Notification.Session := Self;
    Notification.Answer  := Answer;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipSession.RejectOutOfOrderRequest(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.UA.CreateResponse(Request,
                                     SIPInternalServerError);
  try
    Response.StatusText := RSSIPRequestOutOfOrder;
    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipSession.RejectPrematureInvite(Invite: TIdSipRequest);
var
  Response:   TIdSipResponse;
  RetryAfter: TIdSipRetryAfterHeader;
begin
  Response := Self.UA.CreateResponse(Invite,
                                     SIPInternalServerError);
  try
    Response.AddHeader(RetryAfterHeader);
    RetryAfter := Response.FirstRetryAfter;

    RetryAfter.NumericValue := GRandomNumber.NextCardinal(MaxPrematureInviteRetry);
    RetryAfter.Comment      := PrematureInviteMessage;
    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipSession.RejectReInvite(Invite: TIdSipRequest);
var
  RequestPending: TIdSipResponse;
begin
  RequestPending := Self.UA.CreateResponse(Invite, SIPRequestPending);
  try
    Self.SendResponse(RequestPending);
  finally
    RequestPending.Free;
  end;
end;

procedure TIdSipSession.RejectRequest(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.UA.CreateResponse(Request,
                                     SIPRequestTerminated);
  try
    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipSession.RescheduleModify(InviteAgent: TIdSipInvite);
begin
  // Precondition: You've acquired ModifyLock.
  Self.UA.ScheduleEvent(TIdSipSessionResendReInvite,
                        Self.ModifyWaitTime,
                        InviteAgent.InitialRequest,
                        Self.ID);
end;

procedure TIdSipSession.TerminateAnyPendingRequests;
begin
  // cf RFC 3261, section 15.1.2
  Self.ModifyLock.Acquire;
  try
    if Assigned(Self.ModifyAttempt) and Self.ModifyAttempt.IsInbound then
      Self.ModifyAttempt.Terminate;
      
    Self.ModifyAttempt := nil;
  finally
    Self.ModifyLock.Release;
  end;
end;

//******************************************************************************
//* TIdSipInboundSession                                                       *
//******************************************************************************
//* TIdSipInboundSession Public methods ****************************************

function TIdSipInboundSession.AcceptCall(const Offer, ContentType: String): String;
begin
  Self.LocalSessionDescription := Offer;
  Self.LocalMimeType           := ContentType;

  Self.InitialInvite.Accept(Offer, ContentType);
  Self.FullyEstablished := true;
end;

function TIdSipInboundSession.IsInbound: Boolean;
begin
  Result := true;
end;

function TIdSipInboundSession.Match(Msg: TIdSipMessage): Boolean;
var
  MatchesReInvite: Boolean;
begin
  // If the response matches the reinvite, DON'T match the response.
  // Otherwise, check against the dialog. Yes, that's "response" because
  // Waits use messages to find actions for things like
  // TIdSipInboundInvite.ResendOK.
  Self.ModifyLock.Acquire;
  try
    MatchesReInvite := Self.ModificationInProgress
                   and Self.ModifyAttempt.Match(Msg);
  finally
    Self.ModifyLock.Release;
  end;

  if Msg.IsRequest and (Msg as TIdSipRequest).IsAck then
    Result := false
  else if MatchesReInvite then
    Result := false
  else if Msg.IsRequest and (Msg as TIdSipRequest).IsCancel then
    Result := Self.InitialRequest.MatchCancel(Msg as TIdSipRequest)
  else
    Result := not Self.InitialRequest.Equals(Msg)
          and Self.DialogMatches(Msg);
end;

function TIdSipInboundSession.ModifyWaitTime: Cardinal;
begin
  // 0s <= WaitTime <= 2s, in 10ms units
  Result := GRandomNumber.NextCardinal(20)*10;
end;

procedure TIdSipInboundSession.RedirectCall(NewDestination: TIdSipAddressHeader);
var
  RedirectResponse: TIdSipResponse;
begin
  RedirectResponse := Self.UA.CreateResponse(Self.InitialRequest,
                                             SIPMovedTemporarily);
  try
    RedirectResponse.AddHeader(ContactHeaderFull).Value := NewDestination.FullValue;
    Self.SendResponse(RedirectResponse);
  finally
    RedirectResponse.Free;
  end;

  Self.NotifyOfEndedSession(CallRedirected, RSNoReason);
end;

procedure TIdSipInboundSession.RejectCallBusy;
begin
  Self.InitialInvite.RejectCallBusy;

  Self.NotifyOfEndedSession(BusyHere, RSNoReason);
end;

procedure TIdSipInboundSession.Ring;
begin
  Self.DialogLock.Acquire;
  try
    if not Self.DialogEstablished then begin
      Self.fDialog := Self.CreateInboundDialog(Self.UA.NextTag);
      Self.Dialog.ReceiveRequest(Self.InitialRequest);
      Self.InitialInvite.LocalTag := Self.Dialog.ID.LocalTag;

      Self.InitialInvite.Ring;
      Self.InitialRequest.Assign(Self.InitialInvite.InitialRequest);
      Self.InitialRequest.ToHeader.Tag := Self.Dialog.ID.LocalTag;
    end;
  finally
    Self.DialogLock.Release;
  end;
end;

procedure TIdSipInboundSession.Terminate;
begin
  if Self.FullyEstablished then
    Self.SendBye
  else
    Self.InitialInvite.Terminate;

  Self.NotifyOfEndedSession(LocalHangUp, RSNoReason);

  inherited Terminate;
end;

//* TIdSipInboundSession Protected methods *************************************

function TIdSipInboundSession.CreateDialogIDFrom(Msg: TIdSipMessage): TIdSipDialogID;
begin
  Result := TIdSipDialogID.Create(Msg.CallID,
                                  Msg.ToHeader.Tag,
                                  Msg.From.Tag);
end;

procedure TIdSipInboundSession.Initialise(UA: TIdSipAbstractUserAgent;
                                          Request: TIdSipRequest;
                                          UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.InitialInvite := Self.Module.AddInboundInvite(Request, UsingSecureTransport);
  Self.InitialInvite.AddListener(Self);

  Self.RemoteSessionDescription := Request.Body;
  Self.RemoteMimeType           := Request.ContentType;
end;

procedure TIdSipInboundSession.OnFailure(InviteAgent: TIdSipInboundInvite);
begin
  if (Self.InitialInvite = InviteAgent) then
    Self.Terminate
  else
    inherited OnFailure(InviteAgent);
end;

procedure TIdSipInboundSession.OnSuccess(InviteAgent: TIdSipInboundInvite;
                                         Ack: TIdSipRequest);
begin
  inherited OnSuccess(InviteAgent, Ack);

  if (InviteAgent = Self.InitialInvite) then begin
    if (Self.RemoteSessionDescription = '') then begin
      Self.RemoteSessionDescription := Ack.Body;
      Self.RemoteMimeType           := Ack.ContentType;
    end;

    Self.NotifyOfEstablishedSession(Self.InitialInvite.InitialRequest.Body,
                                    Self.InitialInvite.InitialRequest.ContentType);
  end;
end;

procedure TIdSipInboundSession.OnSuccess(InviteAgent: TIdSipOutboundInvite;
                                         Response: TIdSipResponse);
begin
  inherited OnSuccess(InviteAgent, Response);

  Self.NotifyOfModifiedSession(Response);
end;

procedure TIdSipInboundSession.ReceiveCancel(Cancel: TIdSipRequest);
begin
  inherited ReceiveCancel(Cancel);

  if not Self.FullyEstablished then begin
    Self.RejectRequest(Self.InitialRequest);
    Self.NotifyOfEndedSession(RemoteCancel, RSNoReason);
    Self.MarkAsTerminated;
  end;
end;

procedure TIdSipInboundSession.ReceiveInitialInvite(Invite: TIdSipRequest);
begin
  Self.RemoteSessionDescription := Invite.Body;
  Self.RemoteMimeType           := Invite.ContentType;

  Self.Ring;
end;

//* TIdSipInboundSession Private methods ***************************************

function TIdSipInboundSession.CreateInboundDialog(const LocalTag: String): TIdSipDialog;
var
  ArbResponse: TIdSipResponse;
begin
  ArbResponse := TIdSipResponse.InResponseTo(Self.InitialRequest, SIPOK);
  try
    ArbResponse.ToHeader.Tag := LocalTag;

    Result := TIdSipDialog.CreateInboundDialog(Self.InitialRequest,
                                               ArbResponse,
                                               Self.UsingSecureTransport);
    Result.ReceiveResponse(ArbResponse);
  finally
    ArbResponse.Free;
  end;
end;

//******************************************************************************
//* TIdSipOutboundSession                                                      *
//******************************************************************************
//* TIdSipOutboundSession Public methods ***************************************

constructor TIdSipOutboundSession.Create(UA: TIdSipAbstractUserAgent);
begin
  inherited Create(UA);

  Self.InitialiseUsing(TIdSipOutboundInitialInvite);
end;

constructor TIdSipOutboundSession.CreateSessionReplacer(UA: TIdSipAbstractUserAgent;
                                                        Session: TIdSipSession);
var
  Replacer: TIdSipOutboundReplacingInvite;
begin
  inherited Create(UA);

  Self.InitialiseUsing(TIdSipOutboundReplacingInvite);

  Replacer := Self.InitialInvite as TIdSipOutboundReplacingInvite;
  Replacer.CallID  := Session.InitialRequest.CallID;
  Replacer.FromTag := Session.InitialRequest.From.Tag;
  Replacer.ToTag   := Session.InitialRequest.ToHeader.Tag;
end;

destructor TIdSipOutboundSession.Destroy;
begin
  Self.RedirectedInviteLock.Acquire;
  try
    Self.RedirectedInvites.Free;
  finally
    Self.RedirectedInviteLock.Release;
  end;
  Self.RedirectedInviteLock.Free;

  Self.TargetUriSet.Free;
  Self.fDestination.Free;

  inherited Destroy;
end;

procedure TIdSipOutboundSession.Cancel;
begin
  if Self.FullyEstablished then Exit;

  Self.InitialInvite.Cancel;
  Self.TerminateAllRedirects;
end;

function TIdSipOutboundSession.CanForkOn(Response: TIdSipResponse): Boolean;
begin
  Result := (Self.InitialInvite.InitialRequest.CallID = Response.CallID)
        and (Self.InitialInvite.InitialRequest.From.Tag = Response.From.Tag);
end;

function TIdSipOutboundSession.Match(Msg: TIdSipMessage): Boolean;
var
  MatchesReInvite: Boolean;
begin
  // If the response matches the reinvite, DON'T match the response.
  // Otherwise, check against the dialog.
  Self.ModifyLock.Acquire;
  try
    MatchesReInvite := Self.ModificationInProgress
                   and Self.ModifyAttempt.Match(Msg);
  finally
    Self.ModifyLock.Release;
  end;

  if Msg.IsRequest and (Msg as TIdSipRequest).IsAck then
    Result := false
  else if MatchesReInvite then
    Result := false
  else
    Result := not Self.InitialRequest.Equals(Msg)
          and Self.DialogMatches(Msg);
end;

function TIdSipOutboundSession.ModifyWaitTime: Cardinal;
begin
  // 2.1s <= WaitTime <= 4s, in 10ms units
  Result := GRandomNumber.NextCardinal(190)*10 + 2100
end;

procedure TIdSipOutboundSession.Send;
begin
  inherited Send;

  Self.InitialInvite.Destination := Self.Destination;
  Self.InitialInvite.Offer       := Self.LocalSessionDescription;
  Self.InitialInvite.MimeType    := Self.LocalMimeType;
  Self.InitialInvite.Send;
  Self.InitialRequest.Assign(Self.InitialInvite.InitialRequest);
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
    Self.NotifyOfEndedSession(LocalHangUp, RSNoReason);
  end
  else
    Self.Cancel;
end;

//* TIdSipOutboundSession Protected methods ************************************

function TIdSipOutboundSession.CreateDialogIDFrom(Msg: TIdSipMessage): TIdSipDialogID;
begin
  if Msg.IsRequest then
    Result := TIdSipDialogID.Create(Msg.CallID,
                                    Msg.ToHeader.Tag,
                                    Msg.From.Tag)
  else
    Result := TIdSipDialogID.Create(Msg.CallID,
                                    Msg.From.Tag,
                                    Msg.ToHeader.Tag);
end;

procedure TIdSipOutboundSession.OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                                    NewDialog: TIdSipDialog);
begin
  Self.DialogLock.Acquire;
  try
    if not Self.DialogEstablished then begin
      Self.fDialog := NewDialog.Copy;
      Self.InitialRequest.ToHeader.Tag := Self.Dialog.ID.RemoteTag;
    end;
  finally
    Self.DialogLock.Release;
  end;

  inherited OnDialogEstablished(InviteAgent, NewDialog);
end;

procedure TIdSipOutboundSession.OnFailure(InviteAgent: TIdSipOutboundInvite;
                                          Response: TIdSipResponse;
                                          const Reason: String);
begin
  if (Self.ModifyAttempt = InviteAgent) then
    inherited OnFailure(InviteAgent, Response, Reason)
  else if Response.IsRedirect then
    Self.RemoveFinishedRedirectedInvite(InviteAgent)
  else begin
    if (InviteAgent = Self.InitialInvite) then begin
      Self.InitialInvite := nil;
      Self.MarkAsTerminated;
      Self.NotifyOfEndedSession(Response.StatusCode,
                                Response.StatusText);
      Exit;
    end;

    Self.RemoveFinishedRedirectedInvite(InviteAgent);

    if Self.NoMoreRedirectedInvites then begin
      Self.MarkAsTerminated;
      Self.NotifyOfEndedSession(RedirectWithNoSuccess,
                                RSRedirectWithNoSuccess);
    end;
  end;
end;

procedure TIdSipOutboundSession.OnRedirect(InviteAgent: TIdSipOutboundInvite;
                                           Redirect: TIdSipResponse);
var
  NewTargetsAdded: Boolean;
begin
  // cf RFC 3261, section 8.1.3.4.

  if not Self.FullyEstablished then begin
    if Redirect.Contacts.IsEmpty then begin
      Self.MarkAsTerminated;
      Self.NotifyOfEndedSession(RedirectWithNoContacts,
                                RSRedirectWithNoContacts);
    end
    else begin
      // Suppose we receive a 180 and then a 302. Then we have an established
      // dialog, which we have to tear down first, before we attempt another
      // target.
      Self.DialogLock.Acquire;
      try
        if Self.DialogEstablished then
          Self.fDialog.Free;
      finally
        Self.DialogLock.Release;
      end;

      // Of course, if we receive a 3xx then that INVITE's over.
      Self.RemoveFinishedRedirectedInvite(InviteAgent);

      // We receive 3xxs with Contacts. We add these to our target URI set. We
      // send INVITEs to these URIs in some order. If we get 3xxs back from
      // these new targets we add the new Contacts to the target set. We of
      // course don't reattempt to INVITE a target that we've already contacted!
      // Sooner or later we'll either exhaust all the target URIs and report a
      // failed call, or a target will send a 2xx and fully establish a call, in
      // which case we simply do nothing with any other (redirect or failure)
      // responses.
      NewTargetsAdded := false;
      Redirect.Contacts.First;
      while Redirect.Contacts.HasNext do begin
        if not Self.TargetUriSet.HasContact(Redirect.Contacts.CurrentContact) then begin
          Self.AddNewRedirect(InviteAgent.InitialRequest,
                              Redirect.Contacts.CurrentContact);
          NewTargetsAdded := true;
        end;
        Redirect.Contacts.Next;
      end;

      Self.TargetUriSet.Add(Redirect.Contacts);

      if not NewTargetsAdded and Self.NoMoreRedirectedInvites then begin
        Self.MarkAsTerminated;
        Self.NotifyOfEndedSession(RedirectWithNoMoreTargets,
                                  RSRedirectWithNoMoreTargets);
      end;
    end;
  end;

  Self.RemoveFinishedRedirectedInvite(InviteAgent);
end;

procedure TIdSipOutboundSession.OnSuccess(InviteAgent: TIdSipOutboundInvite;
                                          Response: TIdSipResponse);
begin
  inherited OnSuccess(InviteAgent, Response);

  if not Self.FullyEstablished then begin
    Self.FullyEstablished := true;
    // This lets us store Authorization credentials for future use in things
    // like modifying INVITEs.
    Self.InitialRequest.Assign(InviteAgent.InitialRequest);

    Self.RemoveFinishedRedirectedInvite(InviteAgent);
    Self.TerminateAllRedirects;

    Self.RemoteSessionDescription := Response.Body;
    Self.RemoteMimeType           := Response.ContentType;

    Self.NotifyOfEstablishedSession(Self.RemoteSessionDescription,
                                    Self.RemoteMimeType);

    InviteAgent.Offer    := Self.LocalSessionDescription;
    InviteAgent.MimeType := Self.LocalMimeType;
  end
  else
    Self.NotifyOfModifiedSession(Response);
end;

procedure TIdSipOutboundSession.SendBye;
var
  Bye: TIdSipRequest;
begin
  Bye := Self.Module.CreateBye(Self.Dialog);
  try
    // TODO: Verify this as correct behaviour. Otherwise we must use SIP discovery stuff
    Bye.LastHop.Transport := Self.InitialRequest.LastHop.Transport;

    // We don't listen to the new transaction because we assume the BYE
    // succeeds immediately.
    Self.SendRequest(Bye, false);
  finally
    Bye.Free;
  end;

  Self.MarkAsTerminated;
end;

//* TIdSipOutboundSession Private methods **************************************

procedure TIdSipOutboundSession.AddNewRedirect(OriginalInvite: TIdSipRequest;
                                               Contact: TIdSipContactHeader);
var
  Redirect: TIdSipOutboundRedirectedInvite;
begin
  Redirect := Self.UA.AddOutboundAction(TIdSipOutboundRedirectedInvite) as TIdSipOutboundRedirectedInvite;

  Self.RedirectedInviteLock.Acquire;
  try
    Self.RedirectedInvites.Add(Redirect);
  finally
    Self.RedirectedInviteLock.Release;
  end;

  Redirect.Contact := Contact;
  Redirect.OriginalInvite := OriginalInvite;
  Redirect.AddListener(Self);
  Redirect.Send;
end;

procedure TIdSipOutboundSession.InitialiseUsing(OutboundInviteType: TIdSipActionClass);
begin
  Self.fDestination := TIdSipAddressHeader.Create;

  Self.TargetUriSet := TIdSipContacts.Create;

  Self.InitialInvite := Self.UA.Actions.AddOutboundAction(Self.UA, OutboundInviteType) as TIdSipOutboundInitialInvite;
  Self.InitialInvite.AddListener(Self);

  // The UA manages the lifetimes of all outbound INVITEs!
  Self.RedirectedInvites    := TObjectList.Create(false);
  Self.RedirectedInviteLock := TCriticalSection.Create;
end;

function TIdSipOutboundSession.NoMoreRedirectedInvites: Boolean;
begin
  Self.RedirectedInviteLock.Acquire;
  try
    Result := Self.RedirectedInvites.Count = 0;
  finally
    Self.RedirectedInviteLock.Release;
  end;
end;

procedure TIdSipOutboundSession.RemoveFinishedRedirectedInvite(InviteAgent: TIdSipAction);
begin
  Self.RedirectedInviteLock.Acquire;
  try
    Self.RedirectedInvites.Remove(InviteAgent);
  finally
    Self.RedirectedInviteLock.Release;
  end;
end;

procedure TIdSipOutboundSession.SetDestination(Value: TIdSipAddressHeader);
begin
  Self.fDestination.Assign(Value);
end;

procedure TIdSipOutboundSession.TerminateAllRedirects;
var
  I: Integer;
begin
  Self.RedirectedInviteLock.Acquire;
  try
    for I := 0 to Self.RedirectedInvites.Count - 1 do
      (Self.RedirectedInvites[I] as TIdSipOutboundRedirectedInvite).Terminate;
  finally
    Self.RedirectedInviteLock.Release;
  end;
end;

//******************************************************************************
//* TIdSipInboundInviteExpire                                                  *
//******************************************************************************
//* TIdSipInboundInviteExpire Public methods ***********************************

procedure TIdSipInboundInviteExpire.Execute(Action: TIdSipAction);
begin
  if (Action is TIdSipInboundInvite) then
    (Action as TIdSipInboundInvite).TimeOut;
end;

//******************************************************************************
//* TIdSipInboundInviteResendOk                                                *
//******************************************************************************
//* TIdSipInboundInviteResendOk Public methods *********************************

procedure TIdSipInboundInviteResendOk.Execute(Action: TIdSipAction);
begin
  if (Action is TIdSipInboundInvite) then
    (Action as TIdSipInboundInvite).ResendOk;
end;

//******************************************************************************
//* TIdSipInboundInviteSessionProgress                                         *
//******************************************************************************
//* TIdSipInboundInviteSessionProgress Public methods **************************


procedure TIdSipInboundInviteSessionProgress.Execute(Action: TIdSipAction);
begin
  if (Action is TIdSipInboundInvite) then
    (Action as TIdSipInboundInvite).SendSessionProgress;
end;

//******************************************************************************
//* TIdSipOutboundInviteTransactionComplete                                    *
//******************************************************************************
//* TIdSipOutboundInviteTransactionComplete Public methods *********************

procedure TIdSipOutboundInviteTransactionComplete.Execute(Action: TIdSipAction);
begin
  if (Action is TIdSipOutboundInvite) then
    (Action as TIdSipOutboundInvite).TransactionCompleted;
end;

//******************************************************************************
//* TIdSipSessionResendReInvite                                                *
//******************************************************************************
//* TIdSipSessionResendReInvite Public methods *********************************

procedure TIdSipSessionResendReInvite.Execute(Action: TIdSipAction);
var
  Session: TIdSipSession;
begin
  if not (Action is TIdSipSession) then Exit;

  Session := Action as TIdSipSession;

  if not Session.IsTerminated then
    Session.Remodify;
end;

//******************************************************************************
//* TIdSipActionNetworkFailureMethod                                           *
//******************************************************************************
//* TIdSipActionNetworkFailureMethod Public methods ****************************

procedure TIdSipActionNetworkFailureMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipActionListener).OnNetworkFailure(Self.ActionAgent,
                                                     Self.ErrorCode,
                                                     Self.Reason);
end;

//******************************************************************************
//* TIdSipInboundInviteFailureMethod                                           *
//******************************************************************************
//* TIdSipInboundInviteFailureMethod Public methods ****************************

procedure TIdSipInboundInviteFailureMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipInboundInviteListener).OnFailure(Self.Invite);
end;

//******************************************************************************
//* TIdSipInboundInviteSuccessMethod                                           *
//******************************************************************************
//* TIdSipInboundInviteSuccessMethod Public methods ****************************

procedure TIdSipInboundInviteSuccessMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipInboundInviteListener).OnSuccess(Self.Invite, Self.Ack);
end;

//******************************************************************************
//* TIdSipInviteCallProgressMethod                                             *
//******************************************************************************
//* TIdSipInviteCallProgressMethod Public methods ******************************

procedure TIdSipInviteCallProgressMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipInviteListener).OnCallProgress(Self.Invite,
                                                   Self.Response);
end;

//******************************************************************************
//* TIdSipInviteDialogEstablishedMethod                                        *
//******************************************************************************
//* TIdSipInviteDialogEstablishedMethod Public methods *************************

procedure TIdSipInviteDialogEstablishedMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipInviteListener).OnDialogEstablished(Self.Invite,
                                                        Self.Dialog);
end;

//******************************************************************************
//* TIdSipInviteFailureMethod                                                  *
//******************************************************************************
//* TIdSipInviteFailureMethod Public methods ***********************************

procedure TIdSipInviteFailureMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipInviteListener).OnFailure(Self.Invite,
                                              Self.Response,
                                              Self.Reason);
end;

//******************************************************************************
//* TIdSipInviteRedirectMethod                                                 *
//******************************************************************************
//* TIdSipInviteRedirectMethod Public methods **********************************

procedure TIdSipInviteRedirectMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipInviteListener).OnRedirect(Self.Invite,
                                               Self.Response);
end;

//******************************************************************************
//* TIdSipInviteSuccessMethod                                                  *
//******************************************************************************
//* TIdSipInviteSuccessMethod Public methods ***********************************

procedure TIdSipInviteSuccessMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipInviteListener).OnSuccess(Self.Invite,
                                              Self.Response);
end;

//******************************************************************************
//* TIdSipOptionsResponseMethod                                                *
//******************************************************************************
//* TIdSipOptionsResponseMethod Public methods *********************************

procedure TIdSipOptionsResponseMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipOptionsListener).OnResponse(Self.Options,
                                               Self.Response);
end;

//******************************************************************************
//* TIdSipRegistrationFailedMethod                                             *
//******************************************************************************
//* TIdSipRegistrationFailedMethod Public methods ******************************

procedure TIdSipRegistrationFailedMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipRegistrationListener).OnFailure(Self.Registration,
                                                    Self.CurrentBindings,
                                                    Self.Response);
end;

//******************************************************************************
//* TIdSipRegistrationSucceededMethod                                          *
//******************************************************************************
//* TIdSipRegistrationSucceededMethod Public methods ***************************

procedure TIdSipRegistrationSucceededMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipRegistrationListener).OnSuccess(Self.Registration,
                                                    Self.CurrentBindings);
end;

//******************************************************************************
//* TIdSipUserAgentFailMethod                                                  *
//******************************************************************************
//* TIdSipUserAgentFailMethod Public methods ***********************************

procedure TIdSipEndedSessionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSessionListener).OnEndedSession(Self.Session,
                                                    Self.ErrorCode,
                                                    Self.Reason);
end;

//******************************************************************************
//* TIdSipEstablishedSessionMethod                                             *
//******************************************************************************
//* TIdSipEstablishedSessionMethod Public methods ******************************

procedure TIdSipEstablishedSessionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSessionListener).OnEstablishedSession(Self.Session,
                                                          Self.RemoteSessionDescription,
                                                          Self.MimeType);
end;

//******************************************************************************
//* TIdSipModifiedSessionMethod                                                *
//******************************************************************************
//* TIdSipModifiedSessionMethod Public methods *********************************

procedure TIdSipModifiedSessionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSessionListener).OnModifiedSession(Self.Session,
                                                       Self.Answer);
end;

//******************************************************************************
//* TIdSipSessionModifySessionMethod                                           *
//******************************************************************************
//* TIdSipSessionModifySessionMethod Public methods ****************************

procedure TIdSipSessionModifySessionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSessionListener).OnModifySession(Self.Session,
                                                     Self.RemoteSessionDescription,
                                                     Self.MimeType);
end;

//******************************************************************************
//* TIdSipProgressedSessionMethod                                              *
//******************************************************************************
//* TIdSipProgressedSessionMethod Public methods *******************************

procedure TIdSipProgressedSessionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSessionListener).OnProgressedSession(Self.Session,
                                                         Self.Progress);
end;

//******************************************************************************
//* TIdSipUserAgentAuthenticationChallengeMethod                               *
//******************************************************************************
//* TIdSipUserAgentAuthenticationChallengeMethod Public methods ****************

procedure TIdSipUserAgentAuthenticationChallengeMethod.Run(const Subject: IInterface);
var
  Password: String;
  Username: String;
  Listener: IIdSipUserAgentListener;
begin
  Listener := Subject as IIdSipUserAgentListener;

  Listener.OnAuthenticationChallenge(Self.UserAgent,
                                     Self.Challenge,
                                     Username,
                                     Password,
                                     Self.fTryAgain);

  if (Self.FirstPassword = '') then
    Self.FirstPassword := Password;
  if (Self.FirstUsername = '') then
    Self.FirstUsername := Username;
end;

//******************************************************************************
//* TIdSipUserAgentDroppedUnmatchedMessageMethod                               *
//******************************************************************************
//* TIdSipUserAgentDroppedUnmatchedMessageMethod Public methods ****************

procedure TIdSipUserAgentDroppedUnmatchedMessageMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipUserAgentListener).OnDroppedUnmatchedMessage(Self.UserAgent,
                                                                 Self.Message,
                                                                 Self.Receiver);
end;

//******************************************************************************
//* TIdSipUserAgentInboundCallMethod                                           *
//******************************************************************************
//* TIdSipUserAgentInboundCallMethod Public methods ****************************

procedure TIdSipUserAgentInboundCallMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipUserAgentListener).OnInboundCall(Self.UserAgent,
                                                     Self.Session);
end;

//******************************************************************************
//* EIdSipRegistrarNotFound                                                    *
//******************************************************************************
//* EIdSipRegistrarNotFound Public methods *************************************

constructor EIdSipRegistrarNotFound.Create(const Msg: string);
begin
  inherited Create(Format(NoSuchRegistrar, [Msg]));
end;

end.
