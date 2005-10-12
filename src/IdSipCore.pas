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
// * Each layer has references to the layers beneath it. We try to make each
//   layer aware of ONLY the layer immediately below it, but we can't always do
//   that. We NEVER let a lower layer know about layers above it. Thus, the
//   transport layer DOES NOT know about transactions, etc.
// * We propogate messages up the stack using Events or Listeners, and method
//   calls to propogate messages down the stack. We give preference to the more
//   flexible Listeners.
// * We avoid typecasting as much as possible by using polymorphism and, in
//   certain situations where (type-based) polymorphism can't cut it, the
//   Visitor pattern.
// * TObjectLists almost always manage the lifetime of the objects they contain.
// * One single thread forms the core of the stack: a TIdTimerQueue. Each
//   transport has its own thread that does nothing but receive messages and
//   add them to the timer queue for processing, and sending messages to the
//   network.
// * Threads belong to the process in which they run. It doesn't really make
//   sense for us to refer to a class that instantiates a thread as the thread's
//   owner, so
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
  IdSipLocator, IdSipMessage, IdSipTransaction, IdSipTransport, IdTimerQueue,
  SyncObjs;

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

  TIdSipOutboundOptions = class;

  IIdSipOptionsListener = interface(IIdSipActionListener)
    ['{3F2ED4DF-4854-4255-B156-F4581AEAEDA3}']
    procedure OnResponse(OptionsAgent: TIdSipOutboundOptions;
                         Response: TIdSipResponse);
  end;

  TIdSipAbstractCore = class;

  IIdSipTransactionUserListener = interface
    ['{0AE275B0-4C4D-470B-821B-7F88719E822D}']
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                        Message: TIdSipMessage;
                                        Receiver: TIdSipTransport);
  end;

  // I represent a closure that contains some block of code involving an Action.
  // I also represent the null action closure.
  TIdSipActionClosure = class(TObject)
  public
    procedure Execute(Action: TIdSipAction); virtual;
  end;

  TIdSipActionClosureClass = class of TIdSipActionClosure;

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
    function  AddOutboundAction(UserAgent: TIdSipAbstractCore;
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
    fUserAgent: TIdSipAbstractCore;
  public
    property Receiver:  TIdSipTransport         read fReceiver write fReceiver;
    property Request:   TIdSipRequest           read fRequest write fRequest;
    property UserAgent: TIdSipAbstractCore read fUserAgent write fUserAgent;
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

  TIdSipMessageModule = class;
  TIdSipMessageModuleClass = class of TIdSipMessageModule;

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

  // I (usually) represent a human being in the SIP network. I:
  // * inform any listeners when new sessions become established, modified or
  //   terminated;
  // * allow my users to make outgoing "calls";
  // * clean up established Sessions
  //
  // I provide the canonical place to reject messages that have correct syntax
  // but that we don't or can't accept. This includes unsupported SIP versions,
  // unrecognised methods, etc.
  //
  // TODO: there's redundance with this Hostname, and the Hostnames of the
  // transports attached to this core. It's not clear how to set up the
  // hostnames and bindings of the stack.
  TIdSipAbstractCore = class(TIdInterfacedObject,
                                  IIdObserver,
                                  IIdSipTransactionDispatcherListener)
  private
    fActions:                TIdSipActions;
    fAllowedContentTypeList: TStrings;
    fAllowedLanguageList:    TStrings;
    fAllowedSchemeList:      TStrings;
    fAuthenticator:          TIdSipAbstractAuthenticator;
    fContact:                TIdSipContactHeader;
    fDispatcher:             TIdSipTransactionDispatcher;
    fFrom:                   TIdSipFromHeader;
    fHostName:               String;
    fKeyring:                TIdKeyRing;
    fLocator:                TIdSipAbstractLocator;
    fRealm:                  String;
    fRequireAuthentication:  Boolean;
    fTimer:                  TIdTimerQueue;
    fUserAgentName:          String;
    ModuleLock:              TCriticalSection;
    Modules:                 TObjectList;
    NullModule:              TIdSipMessageModule;
    Observed:                TIdObservable;

    procedure AddModuleSpecificHeaders(OutboundMessage: TIdSipMessage);
    function  ConvertToHeader(ValueList: TStrings): String;
    function  CreateRequestHandler(Request: TIdSipRequest;
                                   Receiver: TIdSipTransport): TIdSipUserAgentActOnRequest;
    function  CreateResponseHandler(Response: TIdSipResponse;
                                    Receiver: TIdSipTransport): TIdSipUserAgentActOnResponse;
    function  DefaultFrom: String;
    function  DefaultHostName: String;
    function  DefaultUserAgent: String;
    function  GetContact: TIdSipContactHeader;
    function  GetFrom: TIdSipFromHeader;
    procedure MaybeChangeTransport(Msg: TIdSipMessage);
    function  ModuleAt(Index: Integer): TIdSipMessageModule;
    procedure NotifyModulesOfFree;
    procedure OnChanged(Observed: TObject);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport); virtual;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport); virtual;
    procedure RejectBadAuthorization(Request: TIdSipRequest);
    procedure RejectBadRequest(Request: TIdSipRequest;
                               const Reason: String);
    procedure RejectMethodNotAllowed(Request: TIdSipRequest);
    procedure RejectRequestBadExtension(Request: TIdSipRequest);
    procedure RejectRequestMethodNotSupported(Request: TIdSipRequest);
    procedure RejectRequestUnknownAccept(Request: TIdSipRequest);
    procedure RejectRequestUnknownContentEncoding(Request: TIdSipRequest);
    procedure RejectRequestUnknownContentLanguage(Request: TIdSipRequest);
    procedure RejectRequestUnknownContentType(Request: TIdSipRequest);
    procedure RejectUnsupportedSipVersion(Request: TIdSipRequest);
    procedure SetContact(Value: TIdSipContactHeader);
    procedure SetDispatcher(Value: TIdSipTransactionDispatcher);
    procedure SetFrom(Value: TIdSipFromHeader);
    procedure SetRealm(const Value: String);
  protected
    procedure ActOnRequest(Request: TIdSipRequest;
                           Receiver: TIdSipTransport); virtual;
    procedure ActOnResponse(Response: TIdSipResponse;
                            Receiver: TIdSipTransport); virtual;
    function  AddInboundAction(Request: TIdSipRequest;
                               Receiver: TIdSipTransport): TIdSipAction; overload;
    function  CreateActionsClosure(ClosureType: TIdSipActionsWaitClass;
                                   Msg: TIdSipMessage): TIdSipActionsWait;
    function  ListHasUnknownValue(Request: TIdSipRequest;
                                  ValueList: TStrings;
                                  const HeaderName: String): Boolean;
    procedure NotifyOfChange;
    procedure NotifyOfDroppedMessage(Message: TIdSipMessage;
                                     Receiver: TIdSipTransport); virtual;
    procedure PrepareResponse(Response: TIdSipResponse;
                              Request: TIdSipRequest);
    procedure RejectRequest(Reaction: TIdSipUserAgentReaction;
                            Request: TIdSipRequest);
    procedure RejectRequestUnauthorized(Request: TIdSipRequest);
    procedure SetAuthenticator(Value: TIdSipAbstractAuthenticator); virtual;
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; virtual;
    function  WillAcceptResponse(Response: TIdSipResponse): TIdSipUserAgentReaction; virtual;

    property AllowedContentTypeList: TStrings read fAllowedContentTypeList;
    property AllowedLanguageList:    TStrings read fAllowedLanguageList;
    property AllowedSchemeList:      TStrings read fAllowedSchemeList;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddAllowedContentType(const MimeType: String);
    procedure AddAllowedContentTypes(MimeTypes: TStrings);
    procedure AddAllowedLanguage(const LanguageID: String);
    procedure AddAllowedScheme(const Scheme: String);
    procedure AddLocalHeaders(OutboundRequest: TIdSipRequest); virtual;
    function  AddModule(ModuleType: TIdSipMessageModuleClass): TIdSipMessageModule;
    procedure AddObserver(const Listener: IIdObserver);
    function  AddOutboundAction(ActionType: TIdSipActionClass): TIdSipAction;
    function  AllowedContentTypes: String;
    function  AllowedEncodings: String;
    function  AllowedExtensions: String;
    function  AllowedLanguages: String;
    function  AllowedMethods(RequestUri: TIdSipUri): String;
    function  AllowedSchemes: String;
    function  Authenticate(Request: TIdSipRequest): Boolean;
    function  CountOf(const MethodName: String): Integer;
    function  CreateChallengeResponse(Request: TIdSipRequest): TIdSipResponse;
    function  CreateChallengeResponseAsUserAgent(Request: TIdSipRequest): TIdSipResponse;
    function  CreateOptions(Dest: TIdSipAddressHeader): TIdSipRequest;
    function  CreateRequest(const Method: String;
                            Dest: TIdSipAddressHeader): TIdSipRequest; overload;
    function  CreateRequest(const Method: String;
                            Dialog: TIdSipDialog): TIdSipRequest; overload;
    function  CreateResponse(Request: TIdSipRequest;
                             ResponseCode: Cardinal): TIdSipResponse;
    procedure FindServersFor(Request: TIdSipRequest;
                             Result: TIdSipLocations); overload;
    procedure FindServersFor(Response: TIdSipResponse;
                             Result: TIdSipLocations); overload;
    function  HasUnknownAccept(Request: TIdSipRequest): Boolean;
    function  HasUnknownContentEncoding(Request: TIdSipRequest): Boolean;
    function  HasUnknownContentLanguage(Request: TIdSipRequest): Boolean;
    function  HasUnknownContentType(Request: TIdSipRequest): Boolean;
    function  HasUnsupportedExtension(Request: TIdSipRequest): Boolean;
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
    function  NextCallID: String;
    function  NextInitialSequenceNo: Cardinal;
    function  NextNonce: String;
    function  NextTag: String;
    function  OptionsCount: Integer;
    function  QueryOptions(Server: TIdSipAddressHeader): TIdSipOutboundOptions;
    procedure RemoveModule(ModuleType: TIdSipMessageModuleClass);
    procedure RemoveObserver(const Listener: IIdObserver);
    function  ResponseForInvite: Cardinal; virtual;
    procedure ReturnResponse(Request: TIdSipRequest;
                             Reason: Cardinal);
    procedure ScheduleEvent(BlockType: TIdSipActionClosureClass;
                            WaitTime: Cardinal;
                            Copy: TIdSipMessage); overload;
    procedure ScheduleEvent(BlockType: TIdSipActionClosureClass;
                            WaitTime: Cardinal;
                            Copy: TIdSipMessage;
                            const ActionID: String); overload;
    procedure ScheduleEvent(Event: TNotifyEvent;
                            WaitTime: Cardinal;
                            Msg: TIdSipMessage); overload;
    procedure ScheduleEvent(WaitTime: Cardinal;
                            Wait: TIdWait); overload;
    procedure SendRequest(Request: TIdSipRequest;
                          Dest: TIdSipLocation);
    procedure SendResponse(Response: TIdSipResponse);
    function  Username: String;
    function  UsesModule(ModuleType: TIdSipMessageModuleClass): Boolean;

    // Move to UserAgent:
    procedure TerminateAllCalls; // move to InviteModule
    function  UsingDefaultContact: Boolean;

    property Actions:               TIdSipActions               read fActions;
    property Authenticator:         TIdSipAbstractAuthenticator read fAuthenticator write SetAuthenticator;
    property Contact:               TIdSipContactHeader         read GetContact write SetContact;
    property Dispatcher:            TIdSipTransactionDispatcher read fDispatcher write SetDispatcher;
    property From:                  TIdSipFromHeader            read GetFrom write SetFrom;
    property HostName:              String                      read fHostName write fHostName;
    property Keyring:               TIdKeyRing                  read fKeyring;
    property Locator:               TIdSipAbstractLocator       read fLocator write fLocator;
    property Realm:                 String                      read fRealm write SetRealm;
    property RequireAuthentication: Boolean                     read fRequireAuthentication write fRequireAuthentication;
    property Timer:                 TIdTimerQueue               read fTimer write fTimer;
    property UserAgentName:         String                      read fUserAgentName write fUserAgentName;
  end;

  IIdSipMessageModuleListener = interface
    ['{4C5192D0-6AE1-4F59-A31A-FDB3D30BC617}']
  end;

  // I and my subclasses represent chunks of Transaction-User Core
  // functionality: the ability to process REGISTERs, say, or OPTIONS, or the
  // requests involved with establishing a call.
  TIdSipMessageModule = class(TObject)
  private
    fUserAgent: TIdSipAbstractCore;
  protected
    AcceptsMethodsList:     TStringList;
    AllowedContentTypeList: TStrings;
    Listeners:              TIdNotificationList;

    procedure RejectBadRequest(Request: TIdSipRequest;
                               const Reason: String);
  public
    constructor Create(UA: TIdSipAbstractCore); virtual;
    destructor  Destroy; override;

    function  Accept(Request: TIdSipRequest;
                     UsingSecureTransport: Boolean): TIdSipAction; virtual;
    procedure AddLocalHeaders(OutboundMessage: TIdSipMessage); virtual;
    function  AcceptsMethods: String; virtual;
    function  AllowedContentTypes: TStrings;
    function  AllowedExtensions: String; virtual;
    procedure CleanUp; virtual;
    function  IsNull: Boolean; virtual;
    function  WillAccept(Request: TIdSipRequest): Boolean; virtual;

    property UserAgent: TIdSipAbstractCore read fUserAgent;
  end;

  // I represent the module selected when a request doesn't match any other
  // module.
  TIdSipNullMessageModule = class(TIdSipMessageModule)
  public
    function Accept(Request: TIdSipRequest;
                    UsingSecureTransport: Boolean): TIdSipAction; override;
    function IsNull: Boolean; override;
    function WillAccept(Request: TIdSipRequest): Boolean; override;
  end;

  TIdSipOptionsModule = class(TIdSipMessageModule)
  public
    constructor Create(UA: TIdSipAbstractCore); override;

    function Accept(Request: TIdSipRequest;
                    UsingSecureTransport: Boolean): TIdSipAction; override;
    function AcceptsMethods: String; override;
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
  //
  // Proxies and User Agents can challenge an Action, forcing us to re-issue an
  // action with authorisation credentials. We represent this by the following
  // state machine:
  //
  //                                    +------+
  //                                    |      |
  //                                    V      |
  // +-------------+    +------+    +--------+ |  +----------+
  // | Initialised |--->| Sent |--->| Resent |-+->| Finished |
  // +-------------+    +------+    +--------+    +----------+
  //                        |                          ^
  //                        |                          |
  //                        +--------------------------+
  //
  // We can re-enter the Resent state several times because we may need to
  // authenticate to multiple proxies, and possibly the remote User Agent too,
  // resending the request with its collection of authorisation credentials each
  // time.

  TIdSipActionResult = (arUnknown, arSuccess, arFailure, arInterim);
  TIdSipActionState = (asInitialised, asSent, asResent, asFinished);
  TIdSipAction = class(TIdInterfacedObject)
  private
    fID:             String;
    fInitialRequest: TIdSipRequest;
    fIsTerminated:   Boolean;
    fResult:         TIdSipActionResult;
    fUA:             TIdSipAbstractCore;
    NonceCount:      Cardinal;

    function  CreateResend(AuthorizationCredentials: TIdSipAuthorizationHeader): TIdSipRequest;
    function  GetUsername: String;
    procedure SetUsername(const Value: String);
    function  TrySendRequest(Request: TIdSipRequest;
                             Targets: TIdSipLocations;
                             TryAgain: Boolean = true): Boolean;
  protected
    fIsOwned:        Boolean;
    Listeners:       TIdNotificationList;
    State:           TIdSipActionState;
    TargetLocations: TIdSipLocations;

    procedure ActionSucceeded(Response: TIdSipResponse); virtual;
    procedure AddListeners(Listeners: TIdNotificationList);
    function  CreateNewAttempt: TIdSipRequest; virtual; abstract;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); virtual;
    procedure MarkAsTerminated; virtual;
    procedure NotifyOfAuthenticationChallenge(Challenge: TIdSipResponse);
    procedure NotifyOfFailure(Response: TIdSipResponse); virtual;
    procedure NotifyOfNetworkFailure(ErrorCode: Cardinal;
                                     const Reason: String); virtual;
    function  ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionResult; virtual;
    function  ReceiveGlobalFailureResponse(Response: TIdSipResponse): TIdSipActionResult; virtual;

    function  ReceiveOKResponse(Response: TIdSipResponse;
                                UsingSecureTransport: Boolean): TIdSipActionResult; virtual;
    procedure ReceiveOtherRequest(Request: TIdSipRequest); virtual;
    function  ReceiveProvisionalResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): TIdSipActionResult; virtual;
    function  ReceiveRedirectionResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): TIdSipActionResult; virtual;
    function  ReceiveServerFailureResponse(Response: TIdSipResponse): TIdSipActionResult; virtual;
    procedure SendRequest(Request: TIdSipRequest;
                          TryAgain: Boolean = true); virtual;
    procedure SendResponse(Response: TIdSipResponse); virtual;
    procedure SetResult(Value: TIdSipActionResult);

    property UA: TIdSipAbstractCore read fUA;
  public
    class function Method: String; virtual; abstract;

    constructor Create(UA: TIdSipAbstractCore); virtual;
    constructor CreateInbound(UA: TIdSipAbstractCore;
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
    procedure Resend(AuthorizationCredentials: TIdSipAuthorizationHeader); virtual;
    procedure Send; virtual;
    procedure Terminate; virtual;

    property ID:             String             read fID;
    property InitialRequest: TIdSipRequest      read fInitialRequest;
    property IsOwned:        Boolean            read fIsOwned;
    property IsTerminated:   Boolean            read fIsTerminated;
    property Result:         TIdSipActionResult read fResult;
    property Username:       String             read GetUsername write SetUsername;
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
    function  CreateNewAttempt: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractCore;
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

  TIdSipActionMethod = class(TIdNotification)
  private
    fActionAgent: TIdSipAction;
  public
    property ActionAgent: TIdSipAction read fActionAgent write fActionAgent;
  end;

  TIdSipActionAuthenticationChallengeMethod = class(TIdSipActionMethod)
  private
    fChallenge: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Challenge: TIdSipResponse read fChallenge write fChallenge;
  end;

  TIdSipActionNetworkFailureMethod = class(TIdSipActionMethod)
  private
    fErrorCode: Cardinal;
    fReason:    String;
  public
    procedure Run(const Subject: IInterface); override;

    property ErrorCode: Cardinal read fErrorCode write fErrorCode;
    property Reason:    String   read fReason write fReason;
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

  TIdSipAbstractCoreMethod = class(TIdNotification)
  private
    fUserAgent: TIdSipAbstractCore;
  public
    property UserAgent: TIdSipAbstractCore read fUserAgent write fUserAgent;
  end;

  TIdSipUserAgentDroppedUnmatchedMessageMethod = class(TIdSipAbstractCoreMethod)
  private
    fReceiver: TIdSipTransport;
    fMessage:  TIdSipMessage;
  public
    procedure Run(const Subject: IInterface); override;

    property Receiver: TIdSipTransport read fReceiver write fReceiver;
    property Message:  TIdSipMessage  read fMessage write fMessage;
  end;

  EIdSipBadSyntax = class(EIdException);
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
  SupportEventDirective   = 'SupportEvent';

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

implementation

uses
  IdHashMessageDigest, IdSipConsts, IdSipIndyLocator, IdRandom, IdSdp, Math,
  SysUtils, TypInfo;

const
  ItemNotFoundIndex = -1;

// Exception messages
const
  MethodInProgress     = 'A(n) %s is already in progress';
  OutboundActionFailed = 'An outbound %s failed because: %s';

//******************************************************************************
//* Unit private functions & procedures                                        *
//******************************************************************************

function ReactionToStr(Reaction: TIdSipUserAgentReaction): String;
begin
  Result := GetEnumName(TypeInfo(TIdSipUserAgentReaction), Integer(Reaction));
end;

//******************************************************************************
//* TIdSipActionClosure                                                        *
//******************************************************************************
//* TIdSipActionClosure Public methods *****************************************

procedure TIdSipActionClosure.Execute(Action: TIdSipAction);
begin
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

function TIdSipActions.AddOutboundAction(UserAgent: TIdSipAbstractCore;
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
//      Self.UserAgent.ReturnResponse(Self.Request,
//                                    SIPCallLegOrTransactionDoesNotExist);
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
//* TIdSipAbstractCore                                                         *
//******************************************************************************
//* TIdSipAbstractCore Public methods ******************************************

constructor TIdSipAbstractCore.Create;
begin
  inherited Create;

  Self.fAllowedContentTypeList := TStringList.Create;
  Self.fAllowedLanguageList    := TStringList.Create;
  Self.fAllowedSchemeList      := TStringList.Create;

  Self.ModuleLock := TCriticalSection.Create;
  Self.Modules    := TObjectList.Create(true);
  Self.NullModule := TIdSipNullMessageModule.Create(Self);
  Self.Observed   := TIdObservable.Create;

  Self.fActions                := TIdSipActions.Create;
  Self.fAllowedContentTypeList := TStringList.Create;
  Self.fAllowedLanguageList    := TStringList.Create;
  Self.fKeyring                := TIdKeyRing.Create;

  Self.Actions.AddObserver(Self);

  Self.AddAllowedContentType(SdpMimeType);

  Self.AddModule(TIdSipOptionsModule);

  Self.AddAllowedScheme(SipScheme);

  Self.Contact.Value         := Self.DefaultFrom;
  Self.From.Value            := Self.DefaultFrom;
  Self.HostName              := Self.DefaultHostName;
  Self.Realm                 := Self.HostName;
  Self.RequireAuthentication := false;
  Self.UserAgentName         := Self.DefaultUserAgent;
end;

destructor TIdSipAbstractCore.Destroy;
begin
  Self.NotifyModulesOfFree;

  Self.Contact.Free;
  Self.From.Free;

  Self.Keyring.Free;
  Self.AllowedSchemeList.Free;
  Self.AllowedLanguageList.Free;
  Self.AllowedContentTypeList.Free;
  Self.Actions.Free;


  Self.Observed.Free;
  Self.NullModule.Free;
  Self.ModuleLock.Acquire;
  try
    Self.Modules.Free;
  finally
    Self.ModuleLock.Release;
  end;
  Self.ModuleLock.Free;

  inherited Destroy;
end;

procedure TIdSipAbstractCore.AddAllowedContentType(const MimeType: String);
begin
  if (Trim(MimeType) <> '') then begin
    if (Self.AllowedContentTypeList.IndexOf(MimeType) = ItemNotFoundIndex) then
      Self.AllowedContentTypeList.Add(MimeType);
  end;
end;

procedure TIdSipAbstractCore.AddAllowedContentTypes(MimeTypes: TStrings);
var
  I: Integer;
begin
  for I := 0 to MimeTypes.Count - 1 do
    Self.AddAllowedContentType(MimeTypes[I]);
end;

procedure TIdSipAbstractCore.AddAllowedLanguage(const LanguageID: String);
begin
  if (Trim(LanguageID) = '') then
    raise EIdSipBadSyntax.Create('Not a valid language identifier');

  if (Self.AllowedLanguageList.IndexOf(LanguageID) = ItemNotFoundIndex) then
    Self.AllowedLanguageList.Add(LanguageID);
end;

procedure TIdSipAbstractCore.AddAllowedScheme(const Scheme: String);
begin
  if not TIdSipParser.IsScheme(Scheme) then
    raise EIdSipBadSyntax.Create('Not a valid scheme');

  if (Self.AllowedSchemeList.IndexOf(Scheme) = ItemNotFoundIndex) then
    Self.AllowedSchemeList.Add(Scheme);
end;

procedure TIdSipAbstractCore.AddLocalHeaders(OutboundRequest: TIdSipRequest);
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

function TIdSipAbstractCore.AddModule(ModuleType: TIdSipMessageModuleClass): TIdSipMessageModule;
begin
  Self.ModuleLock.Acquire;
  try
    if not Self.UsesModule(ModuleType) then begin
      Result := ModuleType.Create(Self);
      Self.Modules.Add(Result);
      Self.AddAllowedContentTypes(Result.AllowedContentTypes);
    end
    else begin
      Result := Self.ModuleFor(ModuleType);
    end;
  finally
    Self.ModuleLock.Release;
  end;
end;

procedure TIdSipAbstractCore.AddObserver(const Listener: IIdObserver);
begin
  Self.Observed.AddObserver(Listener);
end;

function TIdSipAbstractCore.AddOutboundAction(ActionType: TIdSipActionClass): TIdSipAction;
begin
  Result := Self.Actions.AddOutboundAction(Self, ActionType);
end;

function TIdSipAbstractCore.AllowedContentTypes: String;
begin
  Result := Self.ConvertToHeader(Self.AllowedContentTypeList);
end;

function TIdSipAbstractCore.AllowedEncodings: String;
begin
  Result := '';
end;

function TIdSipAbstractCore.AllowedExtensions: String;
begin
  Result := ExtensionReplaces;
end;

function TIdSipAbstractCore.AllowedLanguages: String;
begin
  Result := Self.ConvertToHeader(Self.AllowedLanguageList);
end;

function TIdSipAbstractCore.AllowedMethods(RequestUri: TIdSipUri): String;
begin
  // TODO: This if fake.
  Result := Self.KnownMethods;
end;

function TIdSipAbstractCore.AllowedSchemes: String;
begin
  Result := Self.ConvertToHeader(Self.AllowedSchemeList);
end;

function TIdSipAbstractCore.Authenticate(Request: TIdSipRequest): Boolean;
begin
  // We should ALWAYS have an authenticator attached: see TIdSipStackConfigurator.
  Result := Assigned(Self.Authenticator) and Self.Authenticator.Authenticate(Request);
end;

function TIdSipAbstractCore.CountOf(const MethodName: String): Integer;
begin
  Result := Self.Actions.CountOf(MethodName);
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

function TIdSipAbstractCore.CreateOptions(Dest: TIdSipAddressHeader): TIdSipRequest;
begin
  Result := Self.CreateRequest(MethodOptions, Dest);
  try
    Result.AddHeader(AcceptHeader).Value := Self.AllowedContentTypes;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipAbstractCore.CreateRequest(const Method: String;
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

function TIdSipAbstractCore.CreateRequest(const Method: String;
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

function TIdSipAbstractCore.CreateResponse(Request: TIdSipRequest;
                                                ResponseCode: Cardinal): TIdSipResponse;
begin
  Result := TIdSipResponse.InResponseTo(Request,
                                        ResponseCode,
                                        Self.Contact);

  Self.PrepareResponse(Result, Request);
end;

procedure TIdSipAbstractCore.FindServersFor(Request: TIdSipRequest;
                                                 Result: TIdSipLocations);
begin
  Self.Locator.FindServersFor(Request.DestinationUri, Result);
end;

procedure TIdSipAbstractCore.FindServersFor(Response: TIdSipResponse;
                                                 Result: TIdSipLocations);
begin
  Self.Locator.FindServersFor(Response, Result);
end;

function TIdSipAbstractCore.HasUnknownAccept(Request: TIdSipRequest): Boolean;
begin
  Result := Self.ListHasUnknownValue(Request,
                                     Self.AllowedContentTypeList,
                                     AcceptHeader);
end;

function TIdSipAbstractCore.HasUnknownContentEncoding(Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(ContentEncodingHeaderFull);
end;

function TIdSipAbstractCore.HasUnknownContentLanguage(Request: TIdSipRequest): Boolean;
begin
  Result := Self.ListHasUnknownValue(Request,
                                     Self.AllowedLanguageList,
                                     ContentLanguageHeader);
end;

function TIdSipAbstractCore.HasUnknownContentType(Request: TIdSipRequest): Boolean;
begin
  Result := Self.ListHasUnknownValue(Request,
                                     Self.AllowedContentTypeList,
                                     ContentTypeHeaderFull);
end;

function TIdSipAbstractCore.HasUnsupportedExtension(Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(RequireHeader);
end;

function TIdSipAbstractCore.IsExtensionAllowed(const Extension: String): Boolean;
begin
  Result := false;
end;

function TIdSipAbstractCore.IsMethodAllowed(RequestUri: TIdSipUri;
                                                 const Method: String): Boolean;
begin
  // TODO: This is just a stub at the moment. Eventually we want to support
  // controlling rights for multiple URIs so that, for instance, we could allow a
  // non-User Agent to say "yes, you can SUBSCRIBE to A's state, but not to B's". 
  Result := Self.IsMethodSupported(Method);
end;

function TIdSipAbstractCore.IsMethodSupported(const Method: String): Boolean;
begin
  Result := not Self.ModuleFor(Method).IsNull;
end;

function TIdSipAbstractCore.IsSchemeAllowed(const Scheme: String): Boolean;
begin
  Result := Self.AllowedSchemeList.IndexOf(Scheme) >= 0;
end;

function TIdSipAbstractCore.KnownMethods: String;
const
  Delimiter = ', ';
var
  I:              Integer;
  ModulesMethods: String;
begin
  Result := '';
  Self.ModuleLock.Acquire;
  try
    for I := 0 to Self.Modules.Count - 1 do begin
      ModulesMethods := (Self.Modules[I] as TIdSipMessageModule).AcceptsMethods;
      if (ModulesMethods <> '') then
        Result := Result + ModulesMethods + Delimiter;
    end;

    if (Result <> '') then
      Delete(Result, Length(Result) - 1, Length(Delimiter));
  finally
    Self.ModuleLock.Release;
  end;
end;

function TIdSipAbstractCore.ModuleFor(Request: TIdSipRequest): TIdSipMessageModule;
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

  if (Result = nil) then
    Result := Self.NullModule;
end;

function TIdSipAbstractCore.ModuleFor(const Method: String): TIdSipMessageModule;
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

function TIdSipAbstractCore.ModuleFor(ModuleType: TIdSipMessageModuleClass): TIdSipMessageModule;
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

function TIdSipAbstractCore.NextActionID: String;
begin
  Result := Self.Actions.NextActionID;
end;

function TIdSipAbstractCore.NextBranch: String;
begin
  Result := GRandomNumber.NextSipUserAgentBranch;
end;

function TIdSipAbstractCore.NextCallID: String;
begin
  Result := GRandomNumber.NextHexString + '@' + Self.HostName;
end;

function TIdSipAbstractCore.NextInitialSequenceNo: Cardinal;
begin
  Result := GRandomNumber.NextCardinal($7FFFFFFF);
end;

function TIdSipAbstractCore.NextNonce: String;
begin
  Result := GRandomNumber.NextHexString;
end;

function TIdSipAbstractCore.NextTag: String;
begin
  Result := GRandomNumber.NextSipUserAgentTag;
end;

function TIdSipAbstractCore.OptionsCount: Integer;
begin
  Result := Self.Actions.OptionsCount;
end;

function TIdSipAbstractCore.QueryOptions(Server: TIdSipAddressHeader): TIdSipOutboundOptions;
begin
  Result := Self.AddOutboundAction(TIdSipOutboundOptions) as TIdSipOutboundOptions;
  Result.Server := Server;
end;

procedure TIdSipAbstractCore.RemoveModule(ModuleType: TIdSipMessageModuleClass);
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

procedure TIdSipAbstractCore.RemoveObserver(const Listener: IIdObserver);
begin
  Self.Observed.RemoveObserver(Listener);
end;

function TIdSipAbstractCore.ResponseForInvite: Cardinal;
begin
  // If we receive an INVITE (or an OPTIONS), what response code
  // would we return? If we don't wish to be disturbed, we return
  // SIPTemporarilyUnavailable; if we have no available lines, we
  // return SIPBusyHere, etc.

  Result := SIPOK;
end;

procedure TIdSipAbstractCore.ReturnResponse(Request: TIdSipRequest;
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

procedure TIdSipAbstractCore.TerminateAllCalls;
begin
  // This is WRONG! It will also terminate subscriptions, which are not calls!
  Self.Actions.TerminateAllActions;
end;

function TIdSipAbstractCore.UsingDefaultContact: Boolean;
begin
  Result := Pos(Self.Contact.Address.Uri, Self.DefaultFrom) > 0;
end;

procedure TIdSipAbstractCore.ScheduleEvent(BlockType: TIdSipActionClosureClass;
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

procedure TIdSipAbstractCore.ScheduleEvent(BlockType: TIdSipActionClosureClass;
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

procedure TIdSipAbstractCore.ScheduleEvent(Event: TNotifyEvent;
                                                WaitTime: Cardinal;
                                                Msg: TIdSipMessage);
var
  RequestEvent: TIdSipMessageNotifyEventWait;
begin
  if Assigned(Self.Timer) then begin
    RequestEvent := TIdSipMessageNotifyEventWait.Create;
    RequestEvent.Message := Msg;
    RequestEvent.Event   := Event;
    Self.Timer.AddEvent(WaitTime, RequestEvent);
  end;
end;

procedure TIdSipAbstractCore.ScheduleEvent(WaitTime: Cardinal;
                                                Wait: TIdWait);
begin
  if not Assigned(Self.Timer) then
    Exit;

  Self.Timer.AddEvent(WaitTime, Wait);
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

function TIdSipAbstractCore.Username: String;
begin
  Result := Self.From.Address.Username;
end;

function TIdSipAbstractCore.UsesModule(ModuleType: TIdSipMessageModuleClass): Boolean;
begin
  Result := Assigned(Self.ModuleFor(ModuleType));
end;

//* TIdSipAbstractCore Protected methods ***************************************

procedure TIdSipAbstractCore.ActOnRequest(Request: TIdSipRequest;
                                               Receiver: TIdSipTransport);
var
  Actor: TIdSipUserAgentActOnRequest;
begin
  Actor := Self.CreateRequestHandler(Request, Receiver);
  try
    Self.Actions.Perform(Request, Actor);
  finally
    Actor.Free;
  end;
end;

procedure TIdSipAbstractCore.ActOnResponse(Response: TIdSipResponse;
                                                Receiver: TIdSipTransport);
var
  Actor: TIdSipUserAgentActOnResponse;
begin
  Actor := Self.CreateResponseHandler(Response, Receiver);
  try
    Self.Actions.Perform(Response, Actor);
  finally
    Actor.Free;
  end;
end;

function TIdSipAbstractCore.AddInboundAction(Request: TIdSipRequest;
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

function TIdSipAbstractCore.CreateActionsClosure(ClosureType: TIdSipActionsWaitClass;
                                                      Msg: TIdSipMessage): TIdSipActionsWait;
begin
  Result := ClosureType.Create;
  Result.Actions := Self.Actions;
  Result.Message := Msg.Copy;
end;

function TIdSipAbstractCore.ListHasUnknownValue(Request: TIdSipRequest;
                                                     ValueList: TStrings;
                                                     const HeaderName: String): Boolean;
begin
  Result := Request.HasHeader(HeaderName)
       and (ValueList.IndexOf(Request.FirstHeader(HeaderName).Value) = ItemNotFoundIndex);
end;

procedure TIdSipAbstractCore.NotifyOfChange;
begin
  Self.Observed.NotifyListenersOfChange(Self);
end;

procedure TIdSipAbstractCore.NotifyOfDroppedMessage(Message: TIdSipMessage;
                                                         Receiver: TIdSipTransport);
begin
  // By default do nothing.
end;
{
procedure TIdSipAbstractCore.OnAuthenticationChallenge(Dispatcher: TIdSipTransactionDispatcher;
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

  // Usually we want to re-issue a challenged request:
  TryAgain := true;

  // But listeners can decide not to:
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
}
procedure TIdSipAbstractCore.PrepareResponse(Response: TIdSipResponse;
                                                  Request: TIdSipRequest);
begin
  if not Request.ToHeader.HasTag then
    Response.ToHeader.Tag := Self.NextTag;

  if (Self.UserAgentName <> '') then
    Response.AddHeader(ServerHeader).Value := Self.UserAgentName;

  Self.AddModuleSpecificHeaders(Response);
end;

procedure TIdSipAbstractCore.RejectRequest(Reaction: TIdSipUserAgentReaction;
                                                Request: TIdSipRequest);
begin
  case Reaction of
    uarBadAuthorization:
      Self.RejectBadAuthorization(Request);
    uarDoNotDisturb:
          Self.ReturnResponse(Request,
                              SIPTemporarilyUnavailable);
    uarLoopDetected:
      Self.ReturnResponse(Request, SIPLoopDetected);
    uarMethodNotAllowed:
      Self.RejectMethodNotAllowed(Request);
    uarMissingContact:
      Self.RejectBadRequest(Request, MissingContactHeader);
    uarUnauthorized:
      Self.RejectRequestUnauthorized(Request);
//    uarUnsupportedAccept:  // I think this stems from a MISreading of RFC 3261, section 8.2.3
//      Self.RejectRequestUnknownAccept(Request);
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
    // What do we do here? We've rejected the request for a good reason, but have
    // forgotten to implement the bit where we send a reasonable response.
    raise Exception.Create(Self.ClassName
                         + '.RejectRequest: Can''t handle a reaction '
                         + ReactionToStr(Reaction));
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

  // cf RFC 3261 section 8.2
  if Self.RequireAuthentication then begin
    try
      if not Self.Authenticate(Request) then
        Result := uarUnauthorized;
    except
      on EAuthenticate do
        Result := uarBadAuthorization;
    end;
  end
  else if (Request.SIPVersion <> SipVersion) then
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
  else if Self.HasUnsupportedExtension(Request) then
    Result := uarUnsupportedExtension
  // Content processing - 8.2.3
//  else if Self.HasUnknownAccept(Request) then // What is this?? Who CARES what's in an Accept?     
//    Result := uarUnsupportedAccept
  else if Self.HasUnknownContentEncoding(Request) then
    Result := uarUnsupportedContentEncoding
  else if Self.HasUnknownContentLanguage(Request) then
    Result := uarUnsupportedContentLanguage
  else if Self.HasUnknownContentType(Request) then
    Result := uarUnsupportedContentType
  // Section 8.1.1.8 says that a request that can start a dialog (like an
  // INVITE), MUST contain a Contact.
  else if Request.IsInvite and not Request.HasHeader(ContactHeaderFull) then
    Result := uarMissingContact;
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

procedure TIdSipAbstractCore.AddModuleSpecificHeaders(OutboundMessage: TIdSipMessage);
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

function TIdSipAbstractCore.ConvertToHeader(ValueList: TStrings): String;
begin
  Result := StringReplace(ValueList.CommaText, ',', ', ', [rfReplaceAll]);
end;

function TIdSipAbstractCore.CreateRequestHandler(Request: TIdSipRequest;
                                                      Receiver: TIdSipTransport): TIdSipUserAgentActOnRequest;
begin
  Result := TIdSipUserAgentActOnRequest.Create;

  Result.Receiver  := Receiver;
  Result.Request   := Request;
  Result.UserAgent := Self;
end;

function TIdSipAbstractCore.CreateResponseHandler(Response: TIdSipResponse;
                                                       Receiver: TIdSipTransport): TIdSipUserAgentActOnResponse;
begin
  Result := TIdSipUserAgentActOnResponse.Create;

  Result.Receiver  := Receiver;
  Result.Response  := Response;
  Result.UserAgent := Self;
end;

function TIdSipAbstractCore.DefaultFrom: String;
begin
  Result := 'unknown <sip:unknown@' + Self.HostName + '>';
end;

function TIdSipAbstractCore.DefaultHostName: String;
begin
  Result := 'localhost';
end;

function TIdSipAbstractCore.DefaultUserAgent: String;
begin
  Result := 'RNID SipStack v' + SipStackVersion;
end;

function TIdSipAbstractCore.GetContact: TIdSipContactHeader;
begin
  if not Assigned(fContact) then
    fContact := TIdSipContactHeader.Create;

  Result := fContact;
end;

function TIdSipAbstractCore.GetFrom: TIdSipFromHeader;
begin
  if not Assigned(fFrom) then
    fFrom := TIdSipFromHeader.Create;

  Result := fFrom;
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

function TIdSipAbstractCore.ModuleAt(Index: Integer): TIdSipMessageModule;
begin
  Result := Self.Modules[Index] as TIdSipMessageModule;
end;

procedure TIdSipAbstractCore.NotifyModulesOfFree;
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

procedure TIdSipAbstractCore.OnChanged(Observed: TObject);
begin
  Self.NotifyOfChange;
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

procedure TIdSipAbstractCore.RejectMethodNotAllowed(Request: TIdSipRequest);
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

procedure TIdSipAbstractCore.RejectRequestBadExtension(Request: TIdSipRequest);
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

procedure TIdSipAbstractCore.RejectRequestMethodNotSupported(Request: TIdSipRequest);
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

procedure TIdSipAbstractCore.RejectRequestUnknownAccept(Request: TIdSipRequest);
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

procedure TIdSipAbstractCore.RejectRequestUnknownContentEncoding(Request: TIdSipRequest);
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

procedure TIdSipAbstractCore.RejectRequestUnknownContentLanguage(Request: TIdSipRequest);
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

procedure TIdSipAbstractCore.RejectRequestUnknownContentType(Request: TIdSipRequest);
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

procedure TIdSipAbstractCore.RejectUnsupportedSipVersion(Request: TIdSipRequest);
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

procedure TIdSipAbstractCore.SetContact(Value: TIdSipContactHeader);
begin
  Assert(not Value.IsWildCard,
         'You may not use a wildcard Contact header for a User Agent''s '
       + 'Contact');

  Self.Contact.Assign(Value);

  if Self.Contact.IsMalformed then
    raise EBadHeader.Create(Self.Contact.Name);
end;

procedure TIdSipAbstractCore.SetDispatcher(Value: TIdSipTransactionDispatcher);
begin
  fDispatcher := Value;

  fDispatcher.AddTransactionDispatcherListener(Self);
end;

procedure TIdSipAbstractCore.SetFrom(Value: TIdSipFromHeader);
begin
  Self.From.Assign(Value);

  if Self.From.IsMalformed then
    raise EBadHeader.Create(Self.From.Name);
end;

procedure TIdSipAbstractCore.SetRealm(const Value: String);
begin
  Self.fRealm := Value;

  if Assigned(Self.Authenticator) then
    Self.Authenticator.Realm := Self.Realm;
end;

//******************************************************************************
//* TIdSipMessageModule                                                        *
//******************************************************************************
//* TIdSipMessageModule Public methods *****************************************

constructor TIdSipMessageModule.Create(UA: TIdSipAbstractCore);
begin
  inherited Create;

  Self.AcceptsMethodsList     := TStringList.Create;
  Self.AcceptsMethodsList.CaseSensitive := true;
  Self.AllowedContentTypeList := TStringList.Create;
  Self.Listeners              := TIdNotificationList.Create;

  Self.fUserAgent             := UA;
end;

destructor TIdSipMessageModule.Destroy;
begin
  Self.Listeners.Free;
  Self.AllowedContentTypeList.Free;
  Self.AcceptsMethodsList.Free;

  inherited Destroy;
end;

function TIdSipMessageModule.Accept(Request: TIdSipRequest;
                                    UsingSecureTransport: Boolean): TIdSipAction;
begin
  Result := nil;

  if not Request.IsInvite and Request.HasReplaces then begin
    Self.RejectBadRequest(Request, 'Non-INVITE request with Replaces header');
  end;
end;

procedure TIdSipMessageModule.AddLocalHeaders(OutboundMessage: TIdSipMessage);
begin
end;

function TIdSipMessageModule.AcceptsMethods: String;
begin
  Result := StringReplace(Self.AcceptsMethodsList.CommaText,
                          ',',
                          ', ',
                          [rfReplaceAll]);
end;

function TIdSipMessageModule.AllowedContentTypes: TStrings;
begin
  Result := Self.AllowedContentTypeList;
end;

function TIdSipMessageModule.AllowedExtensions: String;
begin
  Result := '';
end;

procedure TIdSipMessageModule.CleanUp;
begin
  // When the User Agent frees, it calls this method. Put any cleanup stuff
  // here.
end;

function TIdSipMessageModule.IsNull: Boolean;
begin
  Result := false;
end;

function TIdSipMessageModule.WillAccept(Request: TIdSipRequest): Boolean;
begin
  Result := Self.AcceptsMethodsList.IndexOf(Request.Method) <> ItemNotFoundIndex;
end;

//* TIdSipMessageModule Protected methods **************************************

procedure TIdSipMessageModule.RejectBadRequest(Request: TIdSipRequest;
                                               const Reason: String);
var
  Response: TIdSipResponse;
begin
  Response := Self.UserAgent.CreateResponse(Request, SIPBadRequest);
  try
    Response.StatusText := Reason;
    Self.UserAgent.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

//******************************************************************************
//* TIdSipNullMessageModule                                                    *
//******************************************************************************
//* TIdSipNullMessageModule Public methods *************************************

function TIdSipNullMessageModule.Accept(Request: TIdSipRequest;
                                 UsingSecureTransport: Boolean): TIdSipAction;
begin
  Result := nil;
  Self.UserAgent.ReturnResponse(Request,
                                SIPCallLegOrTransactionDoesNotExist);
end;

function TIdSipNullMessageModule.IsNull: Boolean;
begin
  Result := true;
end;

function TIdSipNullMessageModule.WillAccept(Request: TIdSipRequest): Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TIdSipOptionsModule                                                        *
//******************************************************************************
//* TIdSipOptionsModule Public methods *****************************************

constructor TIdSipOptionsModule.Create(UA: TIdSipAbstractCore);
begin
  inherited Create(UA);;

  Self.AcceptsMethodsList.Add(MethodOptions);
end;

function TIdSipOptionsModule.Accept(Request: TIdSipRequest;
                                    UsingSecureTransport: Boolean): TIdSipAction;
begin
  Result := inherited Accept(Request, UsingSecureTransport);

  if not Assigned(Result) then
    Result := TIdSipInboundOptions.CreateInbound(Self.UserAgent,
                                                 Request,
                                                 UsingSecureTransport);
end;

function TIdSipOptionsModule.AcceptsMethods: String;
begin
  Result := MethodOptions;
end;

//******************************************************************************
//* TIdSipAction                                                               *
//******************************************************************************
//* TIdSipAction Public methods ************************************************

constructor TIdSipAction.Create(UA: TIdSipAbstractCore);
begin
  inherited Create;

  Self.Initialise(UA, nil, false);
end;

constructor TIdSipAction.CreateInbound(UA: TIdSipAbstractCore;
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
  Succeeded: TIdSipActionResult;
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
    Succeeded := arFailure;
  end;

  Self.SetResult(Succeeded);

  case Succeeded of
    arSuccess: if Response.IsOK then
      Self.ActionSucceeded(Response);
    arFailure:
      Self.NotifyOfFailure(Response);
  end;
end;

procedure TIdSipAction.Resend(AuthorizationCredentials: TIdSipAuthorizationHeader);
var
  AuthedRequest: TIdSipRequest;
begin
  if (Self.State = asInitialised) then
    raise EIdSipTransactionUser.Create('You cannot REsend if you didn''t send'
                                     + ' in the first place');

  Self.State := asResent;

  AuthedRequest := Self.CreateResend(AuthorizationCredentials);
  try
    Self.InitialRequest.Assign(AuthedRequest);
    Self.SendRequest(AuthedRequest);
  finally
    AuthedRequest.Free;
  end;
end;

procedure TIdSipAction.Send;
begin
  if (Self.State <> asInitialised) then
    raise EIdSipTransactionUser.Create(Format(MethodInProgress, [Self.Method]));

  Self.State := asSent;  
end;

procedure TIdSipAction.Terminate;
begin
  Self.MarkAsTerminated;
end;

//* TIdSipAction Protected methods *********************************************

procedure TIdSipAction.ActionSucceeded(Response: TIdSipResponse);
begin
  // By default do nothing.
  Self.State := asFinished;
end;

procedure TIdSipAction.AddListeners(Listeners: TIdNotificationList);
begin
  // WARNING: This will add all the listeners in Listeners to Self.Listeners.
  // You expect that. Note, though, that YOU must make sure Listeners contains
  // listeners of a type that Self expects.

  if Assigned(Listeners) then
    Self.Listeners.Add(Listeners);
end;

procedure TIdSipAction.Initialise(UA: TIdSipAbstractCore;
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
  Self.State           := asInitialised;

  Self.SetResult(arUnknown);

  if Self.IsInbound then
    Self.InitialRequest.Assign(Request);
end;

procedure TIdSipAction.MarkAsTerminated;
begin
  Self.fIsTerminated := true;
end;

procedure TIdSipAction.NotifyOfAuthenticationChallenge(Challenge: TIdSipResponse);
var
  Notification: TIdSipActionAuthenticationChallengeMethod;
begin
  Notification := TIdSipActionAuthenticationChallengeMethod.Create;
  try
    Notification.ActionAgent := Self;
    Notification.Challenge   := Challenge;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipAction.NotifyOfFailure(Response: TIdSipResponse);
begin
  // By default do nothing
  Self.State := asFinished;
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

function TIdSipAction.ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionResult;
begin
  case Response.StatusCode of
    SIPUnauthorized,
    SIPProxyAuthenticationRequired: begin
      Self.NotifyOfAuthenticationChallenge(Response);
      Result := arInterim;
    end;
  else
    Result := arFailure;
  end;
end;

function TIdSipAction.ReceiveGlobalFailureResponse(Response: TIdSipResponse): TIdSipActionResult;
begin
  Result := arFailure;
end;

function TIdSipAction.ReceiveOKResponse(Response: TIdSipResponse;
                                        UsingSecureTransport: Boolean): TIdSipActionResult;
begin
  Result := arSuccess;
end;

procedure TIdSipAction.ReceiveOtherRequest(Request: TIdSipRequest);
begin
end;

function TIdSipAction.ReceiveProvisionalResponse(Response: TIdSipResponse;
                                                 UsingSecureTransport: Boolean): TIdSipActionResult;
begin
  Result := arFailure;
end;

function TIdSipAction.ReceiveRedirectionResponse(Response: TIdSipResponse;
                                                 UsingSecureTransport: Boolean): TIdSipActionResult;
begin
  Result := arFailure;
end;

function TIdSipAction.ReceiveServerFailureResponse(Response: TIdSipResponse): TIdSipActionResult;
begin
  Result := arFailure;
end;

procedure TIdSipAction.SendRequest(Request: TIdSipRequest;
                                   TryAgain: Boolean = true);
var
  FailReason:      String;
  TargetLocations: TIdSipLocations;
begin
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

procedure TIdSipAction.SetResult(Value: TIdSipActionResult);
begin
  Self.fResult := Value;
end;

//* TIdSipAction Private methods ***********************************************

function TIdSipAction.CreateResend(AuthorizationCredentials: TIdSipAuthorizationHeader): TIdSipRequest;
begin
  Result := Self.CreateNewAttempt;

  // cf. RFC 3665, section 3.3, messages F1 and F4.
  Result.CallID   := Self.InitialRequest.CallID;
  Result.From.Tag := Self.InitialRequest.From.Tag;

  // The re-attempt's created like an in-dialog request (even though it's not
  // really): cf. RFC 3261, section 14.
  // Note that since we may not have a dialog established (this is the initial
  // INVITE, for instance), we can not ask a Dialog to create this message.
  Result.CSeq.SequenceNo := Self.InitialRequest.CSeq.SequenceNo + 1;

  Result.CopyHeaders(Self.InitialRequest, AuthorizationHeader);
  Result.CopyHeaders(Self.InitialRequest, ProxyAuthorizationHeader);
  Result.AddHeader(AuthorizationCredentials);
end;

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

  Options := Self.CreateNewAttempt;
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

function TIdSipOutboundOptions.CreateNewAttempt: TIdSipRequest;
begin
  Result := Self.UA.CreateOptions(Self.Server);
end;

procedure TIdSipOutboundOptions.Initialise(UA: TIdSipAbstractCore;
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
//* TIdSipActionAuthenticationChallengeMethod                                  *
//******************************************************************************
//* TIdSipActionAuthenticationChallengeMethod Public methods *******************

procedure TIdSipActionAuthenticationChallengeMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipActionListener).OnAuthenticationChallenge(Self.ActionAgent,
                                                              Self.Challenge);
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
//* TIdSipOptionsResponseMethod                                                *
//******************************************************************************
//* TIdSipOptionsResponseMethod Public methods *********************************

procedure TIdSipOptionsResponseMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipOptionsListener).OnResponse(Self.Options,
                                               Self.Response);
end;

//******************************************************************************
//* TIdSipUserAgentDroppedUnmatchedMessageMethod                               *
//******************************************************************************
//* TIdSipUserAgentDroppedUnmatchedMessageMethod Public methods ****************

procedure TIdSipUserAgentDroppedUnmatchedMessageMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransactionUserListener).OnDroppedUnmatchedMessage(Self.UserAgent,
                                                                       Self.Message,
                                                                       Self.Receiver);
end;

end.
