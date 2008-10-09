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
  Classes, Contnrs, IdConnectionBindings, IdSipDialog, IdException,
  IdInterfacedObject, IdNotification, IdObservable, IdRegisteredObject,
  IdRoutingTable, IdSipAuthentication, IdSipLocation, IdSipLocator,
  IdSipMessage, IdSipProxyDescription, IdSipTransaction, IdSipTransport,
  IdSipTransportAddressSpace, IdTimerQueue, PluggableLogging, StringDictionary,
  SysUtils;

const
  SipStackVersion = '0.6';

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
    procedure OnTerminated(Action: TIdSipAction);
  end;

  // In OnSuccess we use a TIdSipMessage because, for instance,
  // TIdSipInboundInvite succeeds when it receives an ACK.
  IIdSipOwnedActionListener = interface(IIdSipActionListener)
    ['{801E7678-473F-4904-8BEE-C1B7D603D2CA}']
    procedure OnFailure(Action: TIdSipAction;
                        Response: TIdSipResponse;
                        const Reason: String);
    procedure OnRedirect(Action: TIdSipAction;
                         Redirect: TIdSipResponse);
    procedure OnSuccess(Action: TIdSipAction;
                        Msg: TIdSipMessage);
  end;

  // Some Actions will fail because the remote party took too long to respond.
  // In these cases the Response parameter will be nil.
  // Some Actions (currently only INVITEs) will succeed upon receipt of a
  // request (an ACK); hence, OnSuccess contains a TIdSipMessage parameter
  // instead of the expected TIdSipResponse parameter.
  IIdSipOutboundActionListener = interface(IIdSipOwnedActionListener)
    ['{7A285B4A-FC96-467C-A1C9-B95DCCD61101}']
    procedure OnFailure(Action: TIdSipAction;
                        Response: TIdSipResponse);
    procedure OnSuccess(Action: TIdSipAction;
                        Msg: TIdSipMessage);
  end;

  TIdSipAbstractCore = class;

  IIdSipTransactionUserListener = interface
    ['{0AE275B0-4C4D-470B-821B-7F88719E822D}']
    procedure OnAddAction(UserAgent: TIdSipAbstractCore;
                          Action: TIdSipAction);
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                        Message: TIdSipMessage;
                                        Binding: TIdConnectionBindings);
    procedure OnRemoveAction(UserAgent: TIdSipAbstractCore;
                             Action: TIdSipAction);
  end;

  // I represent a closure that contains some block of code involving an Action.
  // I also represent the null action closure.
  TIdSipActionClosure = class(TObject)
  public
    constructor Create; virtual;

    procedure Execute(Action: TIdSipAction); virtual;
  end;

  TIdSipActionClosureClass = class of TIdSipActionClosure;

  TIdSipActionProc = procedure(Action: TIdSipAction) of object;

  // I maintain a list of Actions. You may query me for various statistics, as
  // well as do things to particular actions.
  // The FindFooAndPerform methods require some explanation. The Event
  // parameter Data property must point to a copy of a TIdSipRequest.
  // FindFooAndPerform will destroy the Request.
  TIdSipActions = class(TObject)
  private
    Actions:         TObjectList;
    fOnAddAction:    TIdSipActionProc; // We use callbacks here because this class is
    fOnRemoveAction: TIdSipActionProc; // meant to be completely hidden by the Core.
    Observed:        TIdObservable;

    function  ActionAt(Index: Integer): TIdSipAction;
    procedure AddCurrentActionList(Keys: TStringDictionary);
    procedure DeleteAction(Index: Integer);
    function  FindAction(Msg: TIdSipMessage; ClientAction: Boolean): TIdSipAction; overload;
    function  FindAction(const ActionID: String): TIdSipAction; overload;
    procedure MarkAllActionsAsTerminated;
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
                                   Block: TIdSipActionClosure);
    procedure FindActionAndPerformOr(const ID: String;
                                     FoundBlock: TIdSipActionClosure;
                                     NotFoundBlock: TIdSipActionClosure);
    function  FindActionForGruu(const LocalGruu: String): TIdSipAction;
    function  FindActionThatSent(Request: TIdSipRequest): TIdSipAction;
    function  FindActionWithInitialRequest(Request: TIdSipRequest): TIdSipAction;
    function  InviteCount: Integer;
    function  OptionsCount: Integer;
    procedure Perform(Msg: TIdSipMessage; Block: TIdSipActionClosure; ClientAction: Boolean);
    function  RegistrationCount: Integer;
    procedure RemoveObserver(const Listener: IIdObserver);
    function  SessionCount: Integer;
    procedure Status(Keys: TStringDictionary);
    procedure TerminateAllActions;

    property OnAddAction:    TIdSipActionProc read fOnAddAction write fOnAddAction;
    property OnRemoveAction: TIdSipActionProc read fOnRemoveAction write fOnRemoveAction;
  end;

  // I represent an event that will execute a block (BlockType) on an action in
  // a list of actions.
  TIdSipActionsWait = class(TIdSipMessageWait)
  private
    fActionID:  String;
    fActions:   TIdSipActions;
    fBlockType: TIdSipActionClosureClass;
  public
    procedure Trigger; override;

    property ActionID:  String                   read fActionID write fActionID;
    property Actions:   TIdSipActions            read fActions write fActions;
    property BlockType: TIdSipActionClosureClass read fBlockType write fBlockType;
  end;

  TIdSipActionWait = class(TIdWait)
  private
    fActionID: String;
  public
    property ActionID: String read fActionID write fActionID;
  end;

  TIdSipActionWaitClass = class of TIdSipActionWait;

  // I represent the scheduled execution of an Action authenticating against a
  // UAS.

  TIdSipActionAuthenticateWait = class(TIdSipActionWait)
  private
    Credentials: TIdSipAuthorizationHeader;
  public
    destructor  Destroy; override;

    procedure SetCredentials(Credentials: TIdSipAuthorizationHeader);
    procedure Trigger; override;
  end;

  // I represent the (possibly deferred) execution of something my Action needs
  // done. That is, when you invoke my Trigger, I call Action.Send.
  TIdSipActionSendWait = class(TIdSipActionWait)
  public
    procedure Trigger; override;
  end;

  TIdSipActionTerminateWait = class(TIdSipActionWait)
  public
    procedure Trigger; override;
  end;

  TIdSipActionsWaitClass = class of TIdSipActionsWait;

  // I represent a closure that takes a message that we couldn't send, and
  // matches it to the Action that sent it. That Action can then try resend
  // the message, if appropriate.
  TIdSipActionNetworkFailure = class(TIdSipActionClosure)
  private
    fFailedMessage: TIdSipMessage;
    fReason:        String;
  public
    procedure Execute(Action: TIdSipAction); override;

    property FailedMessage: TIdSipMessage read fFailedMessage write fFailedMessage;
    property Reason:        String        read fReason write fReason;
  end;

  // I represent a closure that a UserAgent uses to, for instance, process a
  // request or response.
  TIdUserAgentClosure = class(TIdSipActionClosure)
  private
    fBinding:   TIdConnectionBindings;
    fRequest:   TIdSipRequest;
    fUserAgent: TIdSipAbstractCore;

    procedure SetBinding(Value: TIdConnectionBindings);
  public
    constructor Create; override;
    destructor  Destroy; override;

    property Binding:   TIdConnectionBindings read fBinding write SetBinding;
    property Request:   TIdSipRequest         read fRequest write fRequest;
    property UserAgent: TIdSipAbstractCore    read fUserAgent write fUserAgent;
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

  TIdSipUserAgentReaction = Cardinal;

  TIdAddressOfRecordConnectionList = class(TObject)
  private
    fAddressOfRecord: TIdSipUri;
    fConnections:     TIdConnectionBindingsSet;

    procedure SetAddressOfRecord(Value: TIdSipUri);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(Connection: TIdConnectionBindings);
    function  Count: Integer;
    procedure Remove(Connection: TIdConnectionBindings);

    property AddressOfRecord: TIdSipUri                read fAddressOfRecord write SetAddressOfRecord;
    property Connections:     TIdConnectionBindingsSet read fConnections;
  end;

  // I hold (AOR, ConnectionBindings) pairs.
  TIdConnectionAssociationSet = class(TObject)
  private
    AORs: TObjectList;

    function  CreateNewEntry(AOR: TIdSipUri): TIdAddressOfRecordConnectionList;
    function  BindingsContaining(Connection: TIdConnectionBindings): TIdAddressOfRecordConnectionList;
    function  BindingsFor(AOR: TIdSipUri): TIdAddressOfRecordConnectionList;
    function  EntryAt(Index: Integer): TIdAddressOfRecordConnectionList;
    procedure InternalRemove(Entry: TIdAddressOfRecordConnectionList;
                             Binding: TIdConnectionBindings);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(AOR: TIdSipUri; Binding: TIdConnectionBindings);
    procedure ConnectionsFor(AOR: TIdSipUri; Bindings: TIdConnectionBindingsSet);
    function  Count: Integer;
    procedure Remove(AOR: TIdSipUri; Binding: TIdConnectionBindings);
    procedure RemoveConnection(Binding: TIdConnectionBindings);
  end;

  // I represent the Transaction-User core. I provide policy for the stack, and
  // I keep track of the various chunks of the rest of the Transaction-User
  // layer - modules, etc.
  //
  // I provide the canonical place to reject messages that have correct syntax
  // but that we don't or can't accept. This includes unsupported SIP versions,
  // unrecognised methods, etc.
  //
  // I also provide helper functionality for Actions.
  //
  // TODO: there's redundance with this Hostname, and the Hostnames of the
  // transports attached to this core. It's not clear how to set up the
  // hostnames and bindings of the stack.
  TIdSipAbstractCore = class(TIdInterfacedObject,
                             IIdObserver,
                             IIdSipConnectionListener,
                             IIdSipTransactionDispatcherListener,
                             IIdSipTransportManagementListener)
  private
    fActions:               TIdSipActions;
    fAllowedLanguageList:   TStrings;
    fAllowedSchemeList:     TStrings;
    fAuthenticator:         TIdSipAbstractAuthenticator;
    fDispatcher:            TIdSipTransactionDispatcher;
    fHostName:              String;
    fInstanceID:            String;
    fKeyring:               TIdKeyRing;
    fLocator:               TIdSipAbstractLocator;
    fRealm:                 String;
    fRoutingTable:          TIdRoutingTable;
    fTimer:                 TIdTimerQueue;
    fUseInboundConnections: Boolean;
    fUseGruu:               Boolean;
    fUserAgentName:         String;
    Listeners:              TIdNotificationList;
    Modules:                TObjectList;
    NullModule:             TIdSipMessageModule;
    Observed:               TIdObservable;
    OpenInboundConnections: TIdConnectionAssociationSet;
    Proxies:                TIdProxyDescriptions;

    procedure AddConnection(Msg: TIdSipMessage; Binding: TIdConnectionBindings);
    procedure AddModuleSpecificHeaders(OutboundMessage: TIdSipMessage);
    procedure CollectAllowedExtensions(ExtensionList: TStrings);
    function  ConvertToHeader(ValueList: TStrings): String;
    function  CreateRequestHandler(Request: TIdSipRequest;
                                   Binding: TIdConnectionBindings): TIdSipUserAgentActOnRequest;
    function  CreateResponseHandler(Response: TIdSipResponse;
                                    Binding: TIdConnectionBindings): TIdSipUserAgentActOnResponse;
    function  DefaultHostName: String;
    function  DefaultUserAgent: String;
    function  GetDefaultRoutePath: TIdSipRoutePath;
    procedure LogDroppedMessage(Message: TIdSipMessage;
                                Binding: TIdConnectionBindings);
    function  ModuleAt(Index: Integer): TIdSipMessageModule;
    procedure NotifyModulesOfFree;
    procedure NotifyOfAddedAction(Action: TIdSipAction);
    procedure NotifyOfRemovedAction(Action: TIdSipAction);
    procedure OnAddedTransport(Transport: TIdSipTransport);
    procedure OnChanged(Observed: TObject);
    procedure OnConnection(Transport: TIdSipTransport;
                           Connection: TIdConnectionBindings);
    procedure OnDisconnection(Transport: TIdSipTransport;
                              Connection: TIdConnectionBindings);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Binding: TIdConnectionBindings); virtual;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Binding: TIdConnectionBindings); virtual;
    procedure OnRemovedTransport(Transport: TIdSipTransport);
    procedure OnTransportException(FailedMessage: TIdSipMessage;
                                   const Reason: String); virtual;
    procedure PrependConnectionLocations(Msg: TIdSipMessage; Targets: TIdSipLocations);
    procedure RejectBadAuthorization(Request: TIdSipRequest);
    procedure RejectMethodNotAllowed(Request: TIdSipRequest);
    procedure RejectRequestBadExtension(Request: TIdSipRequest);
    procedure RejectRequestMethodNotSupported(Request: TIdSipRequest);
    procedure RejectUnsupportedSipVersion(Request: TIdSipRequest);
    procedure SetDefaultRoutePath(Value: TIdSipRoutePath);
    procedure SetDispatcher(Value: TIdSipTransactionDispatcher);
    procedure SetInstanceID(Value: String);
    procedure SetRealm(const Value: String);
  protected
    procedure ActOnRequest(Request: TIdSipRequest;
                           Binding: TIdConnectionBindings); virtual;
    procedure ActOnResponse(Response: TIdSipResponse;
                            Binding: TIdConnectionBindings); virtual;
    function  CreateActionsClosure(ClosureType: TIdSipActionsWaitClass;
                                   Msg: TIdSipMessage): TIdSipActionsWait;
    function  ListHasUnknownValue(Request: TIdSipRequest;
                                  ValueList: TStrings;
                                  const HeaderName: String): Boolean;
    procedure NotifyOfChange;
    procedure NotifyOfDroppedMessage(Message: TIdSipMessage;
                                     Binding: TIdConnectionBindings); virtual;
    procedure PrepareResponse(Response: TIdSipResponse;
                              Request: TIdSipRequest);
    procedure RejectRequest(Reaction: TIdSipUserAgentReaction;
                            Request: TIdSipRequest);
    procedure RejectRequestUnauthorized(Request: TIdSipRequest);
    procedure SetAuthenticator(Value: TIdSipAbstractAuthenticator); virtual;
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; virtual;
    function  WillAcceptResponse(Response: TIdSipResponse): TIdSipUserAgentReaction; virtual;

    property AllowedLanguageList: TStrings read fAllowedLanguageList;
    property AllowedSchemeList:   TStrings read fAllowedSchemeList;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function  AddAction(Action: TIdSipAction): TIdSipAction;
    procedure AddAllowedLanguage(const LanguageID: String);
    procedure AddAllowedScheme(const Scheme: String);
    function  AddInboundAction(Request: TIdSipRequest;
                               Binding: TIdConnectionBindings): TIdSipAction;
    procedure AddListener(Listener: IIdSipTransactionUserListener);
    procedure AddLocalHeaders(OutboundRequest: TIdSipRequest; InDialogRequest: Boolean); virtual;
    function  AddModule(ModuleType: TIdSipMessageModuleClass): TIdSipMessageModule;
    procedure AddNullRoutePath(AddressSpace: String);
    procedure AddObserver(const Listener: IIdObserver);
    function  AddOutboundAction(ActionType: TIdSipActionClass): TIdSipAction; virtual;
    procedure AddRoute(AddressSpace: String; SipEntity: TIdSipUri);
    function  AllowedContentTypes: String;
    function  AllowedEncodings: String;
    function  AllowedExtensions: String;
    function  AllowedLanguages: String;
    function  AllowedMethods(RequestUri: TIdSipUri): String;
    function  AllowedSchemes: String;
    function  Authenticate(Request: TIdSipRequest): TIdSipUserAgentReaction;
    procedure ClearAllPreferredTransportTypes;
    procedure ClearAllRoutePaths;
    function  CountOf(const MethodName: String): Integer;
    function  CreateChallengeResponse(Request: TIdSipRequest): TIdSipResponse;
    function  CreateChallengeResponseAsUserAgent(Request: TIdSipRequest): TIdSipResponse;
    function  CreateRedirectedRequest(OriginalRequest: TIdSipRequest;
                                      Contact: TIdSipAddressHeader): TIdSipRequest;
    function  CreateRequest(const Method: String;
                            From: TIdSipAddressHeader;
                            Dest: TIdSipAddressHeader;
                            MaxForwards: Cardinal): TIdSipRequest; overload;
    function  CreateRequest(const Method: String;
                            Dialog: TIdSipDialog): TIdSipRequest; overload;
    function  CreateResponse(Request: TIdSipRequest;
                             ResponseCode: Cardinal): TIdSipResponse; overload;
    function  CreateResponse(Request: TIdSipRequest;
                             ResponseCode: Cardinal;
                             Contact: TIdSipContactHeader): TIdSipResponse; overload;
    function  FindActionForGruu(const LocalGruu: String): TIdSipAction;
    procedure FindServersFor(Request: TIdSipRequest;
                             Result: TIdSipLocations); overload;
    procedure FindServersFor(Response: TIdSipResponse;
                             Result: TIdSipLocations); overload;
    function  HasUnknownContentEncoding(Request: TIdSipRequest): Boolean;
    function  HasUnknownContentLanguage(Request: TIdSipRequest): Boolean;
    function  HasUnsupportedExtension(Msg: TIdSipMessage): Boolean;
    function  IsExtensionAllowed(const Extension: String): Boolean;
    function  IsMethodAllowed(RequestUri: TIdSipUri;
                              const Method: String): Boolean;
    function  IsMethodSupported(const Method: String): Boolean;
    function  IsSchemeAllowed(const Scheme: String): Boolean;
    function  IsSourceOf(Request: TIdSipRequest): Boolean;
    function  KnownMethods: String;
    procedure Log(Description: String;
                  Severity: TSeverityLevel;
                  EventRef: Cardinal;
                  DebugInfo: String);
    function  ModuleFor(Request: TIdSipRequest): TIdSipMessageModule; overload;
    function  ModuleFor(const Method: String): TIdSipMessageModule; overload;
    function  ModuleFor(ModuleType: TIdSipMessageModuleClass): TIdSipMessageModule; overload;
    function  NextBranch: String;
    function  NextCallID: String;
    function  NextGrid: String;
    function  NextInitialSequenceNo: Cardinal;
    function  NextNonce: String;
    function  NextTag: String;
    function  QueryOptions(Server: TIdSipAddressHeader): TIdSipAction;
    procedure RemoveListener(Listener: IIdSipTransactionUserListener);
    procedure RemoveModule(ModuleType: TIdSipMessageModuleClass);
    procedure RemoveObserver(const Listener: IIdObserver);
    procedure RemoveRoutePathTo(AddressSpace: String);
    function  RequiresUnsupportedExtension(Request: TIdSipRequest): Boolean;
    function  ResponseForInvite: Cardinal; virtual;
    procedure ReturnResponse(Request: TIdSipRequest;
                             Reason: Cardinal);
    function  RoutePathFor(Request: TIdSipRequest): TIdSipRoutePath; overload;
    function  RoutePathFor(Target: TIdSipLocation): TIdSipRoutePath; overload;
    procedure ScheduleEvent(BlockType: TIdSipActionClosureClass;
                            WaitTime: Cardinal;
                            Copy: TIdSipMessage;
                            const ActionID: String); overload;
    procedure ScheduleEvent(WaitTime: Cardinal;
                            Wait: TIdWait); overload;
    procedure SendRequest(Request: TIdSipRequest;
                          Dest: TIdSipLocation);
    procedure SendResponse(Response: TIdSipResponse);
    procedure StartAllTransports;
    procedure StopAllTransports;
    function  UsesModule(ModuleType: TIdSipMessageModuleClass): Boolean; overload;
    function  UsesModule(Method: String): Boolean; overload;

    // Move to UserAgent:
    procedure TerminateAllCalls; // move to InviteModule

    property Actions:                       TIdSipActions               read fActions;
    property Authenticator:                 TIdSipAbstractAuthenticator read fAuthenticator write SetAuthenticator;
    property DefaultRoutePath:              TIdSipRoutePath             read GetDefaultRoutePath write SetDefaultRoutePath;
    property Dispatcher:                    TIdSipTransactionDispatcher read fDispatcher write SetDispatcher;
    property HostName:                      String                      read fHostName write fHostName;
    property InstanceID:                    String                      read fInstanceID write SetInstanceID;
    property Keyring:                       TIdKeyRing                  read fKeyring;
    property Locator:                       TIdSipAbstractLocator       read fLocator write fLocator;
    property Realm:                         String                      read fRealm write SetRealm;
    property RoutingTable:                  TIdRoutingTable             read fRoutingTable write fRoutingTable;
    property Timer:                         TIdTimerQueue               read fTimer write fTimer;
    property UseGruu:                       Boolean                     read fUseGruu write fUseGruu;
    property UseInboundConnections:         Boolean                     read fUseInboundConnections write fUseInboundConnections;
    property UserAgentName:                 String                      read fUserAgentName write fUserAgentName;
  end;

  IIdSipMessageModuleListener = interface
    ['{4C5192D0-6AE1-4F59-A31A-FDB3D30BC617}']
  end;

  // I and my subclasses represent chunks of Transaction-User Core
  // functionality: the ability to process REGISTERs, say, or OPTIONS, or the
  // requests involved with establishing a call.
  TIdSipMessageModule = class(TIdInterfacedObject)
  private
    fUserAgent: TIdSipAbstractCore;

    function  ConvertToHeader(ValueList: TStrings): String;
    function  CreateListWithoutDuplicates(CaseSensitive: Boolean): TStringList;
    procedure RejectRequestUnknownAccept(Request: TIdSipRequest);
    procedure RejectRequestUnknownContentEncoding(Request: TIdSipRequest);
    procedure RejectRequestUnknownContentLanguage(Request: TIdSipRequest);
    procedure RejectRequestUnknownContentType(Request: TIdSipRequest);
    procedure RejectRequestUnsupportedExtension(Request: TIdSipRequest);
  protected
    AcceptsMethodsList:     TStringList;
    AllowedContentTypeList: TStrings;
    Listeners:              TIdNotificationList;

    function  AcceptRequest(Request: TIdSipRequest;
                            Binding: TIdConnectionBindings): TIdSipAction; virtual;
    function  ListHasUnknownValue(Request: TIdSipRequest;
                                  ValueList: TStrings;
                                  const HeaderName: String): Boolean;
    procedure RejectBadRequest(Request: TIdSipRequest;
                               const Reason: String);
    procedure RejectRequest(Reaction: TIdSipUserAgentReaction;
                            Request: TIdSipRequest); virtual;
    procedure ReturnResponse(Request: TIdSipRequest;
                             Reason: Cardinal);
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; virtual;
  public
    constructor Create(UA: TIdSipAbstractCore); virtual;
    destructor  Destroy; override;

    function  Accept(Request: TIdSipRequest;
                     Binding: TIdConnectionBindings): TIdSipAction; virtual;
    procedure AddAllowedContentType(const MimeType: String);
    procedure AddAllowedContentTypes(MimeTypes: TStrings);
    procedure AddLocalHeaders(OutboundMessage: TIdSipMessage); virtual;
    function  AcceptsMethods: String; virtual;
    function  AllowedContentTypes: TStrings; overload;
    function  AllowedExtensions: String; virtual;
    procedure CleanUp; virtual;
    procedure Configure(Params: TIdSipHeaderParameters); virtual;
    function  HasKnownAccept(Request: TIdSipRequest): Boolean;
    function  HasUnknownContentType(Request: TIdSipRequest): Boolean;
    function  IsNull: Boolean; virtual;
    function  SupportsMimeType(const MimeType: String): Boolean;
    function  WillAccept(Request: TIdSipRequest): Boolean; virtual;

    property UserAgent: TIdSipAbstractCore read fUserAgent;
  end;

  // I represent the module selected when a request doesn't match any other
  // module.
  TIdSipNullModule = class(TIdSipMessageModule)
  protected
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; override;
  public
    function IsNull: Boolean; override;
    function WillAccept(Request: TIdSipRequest): Boolean; override;
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
  // Proxies and User Agents can challenge an outbound Action, forcing us to re-
  // issue an action with authorisation credentials. We represent this by the
  // following state machine:
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
  //
  // LocalParty contains the address-of-record of the user on this side of the
  // dialog. For an inbound INVITE, that means LocalParty contains the address
  // in the To header; for an outbound INVITE, LocalParty contains the address
  // that will go in the From header.

  TIdSipActionResult = (arUnknown, arSuccess, arFailure, arInterim);
  TIdSipActionState = (asInitialised, asSent, asResent, asFinished);
  TIdSipAction = class(TIdInterfacedObject)
  private
    ActionListeners:       TIdNotificationList;
    AddressSpaceLocations: TIdSipLocations;
    AttemptedLocations:    TIdSipLocations;
    fInitialRequest:       TIdSipRequest;
    fIsTerminated:         Boolean;
    fLocalGruu:            TIdSipContactHeader;
    fLocalParty:           TIdSipAddressHeader;
    fMaxForwards:          Cardinal;
    fResult:               TIdSipActionResult;
    fUA:                   TIdSipAbstractCore;
    NonceCount:            Cardinal;
    TargetLocations:       TIdSipLocations;

    function  CreateResend(AuthorizationCredentials: TIdSipAuthorizationHeader): TIdSipRequest;
    function  GetUseGruu: Boolean;
    function  GetUsername: String;
    procedure SetLocalGruu(Value: TIdSipContactHeader);
    procedure SetLocalParty(Value: TIdSipAddressHeader);
    procedure SetUseGruu(Value: Boolean);
    procedure SetUsername(Value: String);
    procedure TryAnotherRoutePath(Request: TIdSipRequest;
                                  Targets: TIdSipLocations);
    procedure TrySendRequest(Request: TIdSipRequest;
                             Targets: TIdSipLocations);
    procedure TrySendRequestAgain(Request: TIdSipRequest;
                                  Targets: TIdSipLocations);
  protected
    fIsOwned: Boolean;
    State:    TIdSipActionState;

    procedure ActionSucceeded(Response: TIdSipResponse); virtual;
    function  CreateNewAttempt: TIdSipRequest; virtual;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         Binding: TIdConnectionBindings); virtual;
    procedure MarkAsTerminated; virtual;
    procedure NotifyOfAuthenticationChallenge(Challenge: TIdSipResponse); virtual;
    procedure NotifyOfFailure(Response: TIdSipResponse); virtual;
    procedure NotifyOfNetworkFailure(ErrorCode: Cardinal;
                                     const Reason: String); virtual;
    procedure NotifyOfTermination;
    function  ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionResult; virtual;
    function  ReceiveGlobalFailureResponse(Response: TIdSipResponse): TIdSipActionResult; virtual;

    function  ReceiveOKResponse(Response: TIdSipResponse;
                                Binding: TIdConnectionBindings): TIdSipActionResult; virtual;
    procedure ReceiveOtherRequest(Request: TIdSipRequest;
                                  Binding: TIdConnectionBindings); virtual;
    function  ReceiveProvisionalResponse(Response: TIdSipResponse;
                                         Binding: TIdConnectionBindings): TIdSipActionResult; virtual;
    function  ReceiveRedirectionResponse(Response: TIdSipResponse;
                                         Binding: TIdConnectionBindings): TIdSipActionResult; virtual;
    function  ReceiveServerFailureResponse(Response: TIdSipResponse): TIdSipActionResult; virtual;
    procedure SendRequest(Request: TIdSipRequest);
    procedure SendResponse(Response: TIdSipResponse); overload; virtual;
    procedure SetContactUri(Request: TIdSipRequest; Target: TIdSipLocation); virtual;
    procedure SetResult(Value: TIdSipActionResult);
    procedure SetStateToFinished;
    procedure SetStateToResent;
    procedure SetStateToSent;
  public
    constructor Create(UA: TIdSipAbstractCore); overload; virtual;
    constructor CreateInbound(UA: TIdSipAbstractCore;
                              Request: TIdSipRequest;
                              Binding: TIdConnectionBindings); virtual;
    destructor  Destroy; override;

    procedure AddActionListener(Listener: IIdSipActionListener);
    function  IntrospectionCountKey: String; virtual;
    function  IsInbound: Boolean; virtual;
    function  IsInvite: Boolean; virtual;
    function  IsOptions: Boolean; virtual;
    function  IsOutbound: Boolean;
    function  IsRegistration: Boolean; virtual;
    function  IsSession: Boolean; virtual;
    function  Match(Msg: TIdSipMessage): Boolean; virtual;
    function  Method: String; virtual;
    procedure NetworkFailureSending(Msg: TIdSipMessage); virtual;
    function  OutOfDialog: Boolean; virtual;
    procedure ReceiveRequest(Request: TIdSipRequest;
                             Binding: TIdConnectionBindings); virtual;
    procedure ReceiveResponse(Response: TIdSipResponse;
                              Binding: TIdConnectionBindings); virtual;
    procedure RemoveActionListener(Listener: IIdSipActionListener);
    procedure Resend(AuthorizationCredentials: TIdSipAuthorizationHeader); virtual;
    procedure Send; virtual;
    procedure Terminate; virtual;

    property InitialRequest: TIdSipRequest       read fInitialRequest;
    property IsOwned:        Boolean             read fIsOwned;
    property IsTerminated:   Boolean             read fIsTerminated;
    property LocalGruu:      TIdSipContactHeader read fLocalGruu write SetLocalGruu;
    property LocalParty:     TIdSipAddressHeader read fLocalParty write SetLocalParty;
    property MaxForwards:    Cardinal            read fMaxForwards write fMaxForwards;
    property Result:         TIdSipActionResult  read fResult;
    property UA:             TIdSipAbstractCore  read fUA;
    property UseGruu:        Boolean             read GetUseGruu write SetUseGruu;
    property Username:       String              read GetUsername write SetUsername;
  end;

  // I encapsulate the call flow around a single request send and response.
  TIdSipOwnedAction = class(TIdSipAction)
  private
    OwnedActionListeners: TIdNotificationList;
  protected
    procedure ActionSucceeded(Response: TIdSipResponse); override;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         Binding: TIdConnectionBindings); override;
    procedure NotifyOfFailure(Response: TIdSipResponse); override;
    procedure NotifyOfRedirect(Response: TIdSipResponse);
    procedure NotifyOfSuccess(Msg: TIdSipMessage); virtual;
    function  ReceiveRedirectionResponse(Response: TIdSipResponse;
                                         Binding: TIdConnectionBindings): TIdSipActionResult; override;
  public
    destructor Destroy; override;

    procedure AddOwnedActionListener(Listener: IIdSipOwnedActionListener);
    procedure Cancel; virtual;
    procedure RemoveOwnedActionListener(Listener: IIdSipOwnedActionListener);
  end;

  TIdSipRedirectedAction = class(TIdSipOwnedAction)
  private
    fContact:         TIdSipAddressHeader;
    fMethod:          String;
    fOriginalRequest: TIdSipRequest;

    procedure SetContact(Value: TIdSipAddressHeader);
    procedure SetOriginalRequest(Value: TIdSipRequest);
  protected
    function  CreateNewAttempt: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         Binding: TIdConnectionBindings); override;
  public
    destructor Destroy; override;

    function  Method: String; override;
    procedure SetMethod(const Method: String);

    property Contact:         TIdSipAddressHeader read fContact write SetContact;
    property OriginalRequest: TIdSipRequest       read fOriginalRequest write SetOriginalRequest;
  end;

  TIdSipActionRedirector = class;
  // * OnNewAction allows you to manipulate the new attempt to send a message.
  //   For instance, it allows you to listen for OnDialogEstablished
  //   notifications from an OutboundInvite.
  // * OnFailure returns the failure response to the last attempt to send the
  //   message. No more notifications will occur after this.
  // * OnRedirectFailure tells you that, for instance, there were no locations
  //   returned by the redirecting response, or that no locations could be
  //   reached (because of a series of network failures, say). Like OnFailure,
  //   this is a "final" notification.
  // * OnSuccess does just what it says: it returns the first successful action
  //   (and response).
  IIdSipActionRedirectorListener = interface
    ['{A538DE4D-DC73-44D2-A888-E7B7B5FA2BF0}']
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
  end;

  // I represent an action that uses owned actions to accomplish something. My
  // subclasses, for instance, use owned actions to handle redirection
  // responses.
  TIdSipOwningAction = class(TIdSipAction,
                             IIdSipActionListener,
                             IIdSipActionRedirectorListener,
                             IIdSipOwnedActionListener)
  private
    ListeningTo: TObjectList;
  protected
    procedure AddSelfAsListenerTo(Action: TIdSipAction); virtual;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         Binding: TIdConnectionBindings); override;

    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse); virtual;
    procedure OnFailure(Action: TIdSipAction;
                        Response: TIdSipResponse;
                        const Reason: String); overload; virtual;
    procedure OnFailure(Redirector: TIdSipActionRedirector;
                        Response: TIdSipResponse); overload; virtual;
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String); virtual;
    procedure OnNewAction(Redirector: TIdSipActionRedirector;
                          NewAction: TIdSipAction); virtual;
    procedure OnRedirect(Action: TIdSipAction;
                         Redirect: TIdSipResponse); virtual;
    procedure OnRedirectFailure(Redirector: TIdSipActionRedirector;
                                ErrorCode: Cardinal;
                                const Reason: String); virtual;
    procedure OnSuccess(Action: TIdSipAction;
                        Msg: TIdSipMessage); overload; virtual;
    procedure OnSuccess(Redirector: TIdSipActionRedirector;
                        SuccessfulAction: TIdSipAction;
                        Response: TIdSipResponse); overload; virtual;
    procedure OnTerminated(Action: TIdSipAction); virtual;
    procedure RemoveSelfAsListenerFrom(Action: TIdSipAction); virtual;
  public
    destructor Destroy; override;

    function CreateInitialAction: TIdSipOwnedAction; virtual;
    function CreateRedirectedAction(OriginalRequest: TIdSipRequest;
                                    Contact: TIdSipContactHeader): TIdSipOwnedAction; virtual;
  end;

  // I encapsulate the logic surrounding receiving 3xx class responses to a
  // request and sending out new, redirected, requests. When I complete then
  // either I have received a 2xx class response for one of the (sub)actions,
  // indicating the success of the action, or some failure response (4xx, 5xx,
  // 6xx).
  TIdSipActionRedirector = class(TIdInterfacedObject,
                                 IIdSipActionListener,
                                 IIdSipOwnedActionListener)
  private
    fCancelling:          Boolean;
    fFullyEstablished:    Boolean;
    fInitialAction:       TIdSipOwnedAction;
    Listeners:            TIdNotificationList;
    OwningAction:         TIdSipOwningAction;
    RedirectedActions:    TObjectList;
    TargetUriSet:         TIdSipContacts;
    UA:                   TIdSipAbstractCore;

    procedure AddNewRedirect(OriginalRequest: TIdSipRequest;
                             Contact: TIdSipContactHeader);
    function  HasOutstandingRedirects: Boolean;
    procedure NotifyOfFailure(ErrorCode: Cardinal;
                              const Reason: String);
    procedure NotifyOfNewAction(Action: TIdSipAction);
    procedure NotifyOfSuccess(Action: TIdSipAction;
                              Response: TIdSipResponse);
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Challenge: TIdSipResponse);
    procedure OnFailure(Action: TIdSipAction;
                        Response: TIdSipResponse;
                        const Reason: String);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
    procedure OnRedirect(Action: TIdSipAction;
                         Redirect: TIdSipResponse);
    procedure OnSuccess(Action: TIdSipAction;
                        Response: TIdSipMessage);
    procedure OnTerminated(Action: TIdSipAction);
    procedure RemoveFinishedRedirectedInvite(Agent: TIdSipAction);
    procedure TerminateAllRedirects;
  public
    constructor Create(OwningAction: TIdSipOwningAction);
    destructor  Destroy; override;

    procedure AddListener(const Listener: IIdSipActionRedirectorListener);
    procedure Cancel;
    function  Contains(OwnedAction: TIdSipAction): Boolean;
    procedure RemoveListener(const Listener: IIdSipActionRedirectorListener);
    procedure Resend(ChallengedAction: TIdSipAction;
                     AuthorizationCredentials: TIdSipAuthorizationHeader);
    procedure Send;
    procedure Terminate;

    property Cancelling:       Boolean           read fCancelling write fCancelling;
    property FullyEstablished: Boolean           read fFullyEstablished write fFullyEstablished;
    property InitialAction:    TIdSipOwnedAction read fInitialAction;
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

  TIdSipActionTerminatedMethod = class(TIdSipActionMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSipOwnedActionMethod = class(TIdSipActionMethod)
  end;

  TIdSipOwnedActionFailureMethod = class(TIdSipOwnedActionMethod)
  private
    fReason: String;
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Reason:   String         read fReason write fReason;
    property Response: TIdSipResponse read fResponse write fResponse;
  end;

  TIdSipOwnedActionRedirectMethod = class(TIdSipOwnedActionMethod)
  private
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Response: TIdSipResponse read fResponse write fResponse;
  end;

  TIdSipOwnedActionSuccessMethod = class(TIdSipOwnedActionMethod)
  private
    fMsg: TIdSipMessage;
  public
    procedure Run(const Subject: IInterface); override;

    property Msg: TIdSipMessage read fMsg write fMsg;
  end;

  TIdSipActionRedirectorMethod = class(TIdNotification)
  private
    fRedirector: TIdSipActionRedirector;
  public
    property Redirector: TIdSipActionRedirector read fRedirector write fRedirector;
  end;

  TIdSipRedirectorNewActionMethod = class(TIdSipActionRedirectorMethod)
  private
    fNewAction: TIdSipAction;
  public
    procedure Run(const Subject: IInterface); override;

    property NewAction: TIdSipAction read fNewAction write fNewAction;
  end;

  TIdSipRedirectorRedirectFailureMethod = class(TIdSipActionRedirectorMethod)
  private
    fErrorCode: Cardinal;
    fReason:    String;
  public
    procedure Run(const Subject: IInterface); override;

    property ErrorCode: Cardinal read fErrorCode write fErrorCode;
    property Reason:    String   read fReason write fReason;
  end;

  TIdSipRedirectorSuccessMethod = class(TIdSipActionRedirectorMethod)
  private
    fResponse:         TIdSipResponse;
    fSuccessfulAction: TIdSipAction;

  public
    procedure Run(const Subject: IInterface); override;

    property Response:         TIdSipResponse read fResponse write fResponse;
    property SuccessfulAction: TIdSipAction   read fSuccessfulAction write fSuccessfulAction;
  end;

  TIdSipAbstractCoreMethod = class(TIdNotification)
  private
    fUserAgent: TIdSipAbstractCore;
  public
    property UserAgent: TIdSipAbstractCore read fUserAgent write fUserAgent;
  end;

  TIdSipUserAgentAddActionMethod = class(TIdSipAbstractCoreMethod)
  private
    fAction: TIdSipAction;
  public
    procedure Run(const Subject: IInterface); override;

    property Action: TIdSipAction read fAction write fAction;
  end;

  TIdSipUserAgentDroppedUnmatchedMessageMethod = class(TIdSipAbstractCoreMethod)
  private
    fBinding: TIdConnectionBindings;
    fMessage: TIdSipMessage;
  public
    procedure Run(const Subject: IInterface); override;

    property Binding: TIdConnectionBindings read fBinding write fBinding;
    property Message: TIdSipMessage         read fMessage write fMessage;
  end;

  TIdSipUserAgentRemoveActionMethod = class(TIdSipAbstractCoreMethod)
  private
    fAction: TIdSipAction;
  public
    procedure Run(const Subject: IInterface); override;

    property Action: TIdSipAction read fAction write fAction;
  end;

  EIdSipBadSyntax = class(EIdException);
  EIdSipTransactionUser = class(EIdException);

// Transaction-User reactions. Don't forget to update ReactionToStr if you add
// a new value here!
const
  uarAccept                     = 0;
  uarBadAuthorization           = 1;
  uarBadRequest                 = 2;
  uarDoNotDisturb               = 3;
  uarDoNothing                  = 4;
  uarExpireTooBrief             = 5;
  uarForbidden                  = 6;
  uarLoopDetected               = 7;
  uarMethodNotAllowed           = 8;
  uarMissingContact             = 9;
  uarNonInviteWithReplaces      = 10;
  uarNoSuchCall                 = 11;
  uarNotFound                   = 12;
  uarUnsupportedExtension       = 13;
  uarTooManyVias                = 14;
  uarUnauthorized               = 15;
  uarUnsupportedAccept          = 16;
  uarUnsupportedContentEncoding = 17;
  uarUnsupportedContentLanguage = 18;
  uarUnsupportedContentType     = 19;
  uarUnsupportedMethod          = 20;
  uarUnsupportedScheme          = 21;
  uarUnsupportedSipVersion      = 22;

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

// Generally useful constants
const
  BadAuthorizationTokens      = 'Bad Authorization tokens';
  HighestInitialSequenceNo    = $7FFFFFFF; // cf. RFC 3261, section 19.3
  MalformedConfigurationLine  = 'Malformed configuration line: %s';
  MaxPrematureInviteRetry     = 10;
  MissingContactHeader        = 'Missing Contact Header';
  NonInviteWithReplacesHeader = 'Non-INVITE request with Replaces header';
  OneMinute                   = 60;
  OneHour                     = 60*OneMinute;
  FiveMinutes                 = 5*OneMinute;
  TwentyMinutes               = 20*OneMinute;

// Introspection constants
const
  IntrospecTransactionUserNamespace = 'TransactionUser';
  IntrospecSeparator                = '.';

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

function InNamespace(Namespace, Key: String): String;

implementation

uses
  IdRandom, IdSdp, IdSipInviteModule, IdSipOptionsModule, IdSipRegistration,
  IdSipSubscribeModule, RuntimeSafety;

const
  ItemNotFoundIndex = -1;

// Exception messages
const
  ContradictoryCreateRequestInvocation = 'You tried to create a %s request '
                                       + 'with a URI specifying a %s method';
  MessageSendFailureMalformed          = 'Cannot send malformed message: %s';
  MessageSendFailureUnknownExtension   = 'Cannot send message with unknown '
                                       + 'extension, one of: %s';
  MessageSendFailureUnknownMethod      = 'Cannot send message with unknown '
                                       + 'method %s';
  MethodInProgress                     = 'A(n) %s is already in progress';
  NotAnUUIDURN                         = '"%s" is not a valid UUID URN';
  OutboundActionFailed                 = 'An outbound %s failed because: %s';

//******************************************************************************
//* Unit public functions & procedures                                         *
//******************************************************************************

function InNamespace(Namespace, Key: String): String;
begin
  // Given an introspection key, put the key in the namespace denoted by
  // Namespace.

  Result := Namespace + IntrospecSeparator + Key;
end;

//******************************************************************************
//* Unit private functions & procedures                                        *
//******************************************************************************

function ReactionToStr(Reaction: TIdSipUserAgentReaction): String;
begin
  case Reaction of
    uarAccept:                     Result := 'uarAccept';
    uarBadAuthorization:           Result := 'uarBadAuthorization';
    uarBadRequest:                 Result := 'uarBadRequest';
    uarDoNotDisturb:               Result := 'uarDoNotDisturb';
    uarDoNothing:                  Result := 'uarDoNothing';
    uarExpireTooBrief:             Result := 'uarExpireTooBrief';
    uarForbidden:                  Result := 'uarForbidden';
    uarLoopDetected:               Result := 'uarLoopDetected';
    uarMethodNotAllowed:           Result := 'uarMethodNotAllowed';
    uarMissingContact:             Result := 'uarMissingContact';
    uarNonInviteWithReplaces:      Result := 'uarNonInviteWithReplaces';
    uarNoSuchCall:                 Result := 'uarNoSuchCall';
    uarNotFound:                   Result := 'uarNotFound';
    uarUnsupportedExtension:       Result := 'uarUnsupportedExtension';
    uarTooManyVias:                Result := 'uarTooManyVias';
    uarUnauthorized:               Result := 'uarUnauthorized';
    uarUnsupportedAccept:          Result := 'uarUnsupportedAccept';
    uarUnsupportedContentEncoding: Result := 'uarUnsupportedContentEncoding';
    uarUnsupportedContentLanguage: Result := 'uarUnsupportedContentLanguage';
    uarUnsupportedContentType:     Result := 'uarUnsupportedContentType';
    uarUnsupportedMethod:          Result := 'uarUnsupportedMethod';
    uarUnsupportedScheme:          Result := 'uarUnsupportedScheme';
    uarUnsupportedSipVersion:      Result := 'uarUnsupportedSipVersion';
  else
    Result := IntToStr(Reaction);
  end;
end;

//******************************************************************************
//* TIdSipActionClosure                                                        *
//******************************************************************************
//* TIdSipActionClosure Public methods *****************************************

constructor TIdSipActionClosure.Create;
begin
  inherited Create;
end;

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

  Self.Actions  := TObjectList.Create;
  Self.Observed := TIdObservable.Create;
end;

destructor TIdSipActions.Destroy;
begin
  Self.Observed.Free;

  Self.MarkAllActionsAsTerminated;
  Self.Actions.Free;

  inherited Destroy;
end;

function TIdSipActions.Add(Action: TIdSipAction): TIdSipAction;
begin
  Result := Action;

  try
    Self.Actions.Add(Action);

    if Assigned(Self.OnAddAction) then
      Self.OnAddAction(Action);
  except
    if (Self.Actions.IndexOf(Action) <> ItemNotFoundIndex) then
      Self.Actions.Remove(Action)
    else
      FreeAndNil(Result);
    raise;
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
  Result := Self.Add(ActionType.Create(UserAgent));
end;

procedure TIdSipActions.CleanOutTerminatedActions;
var
  Changed: Boolean;
  I:       Integer;
begin
  Changed := false;
  I       := 0;
  while (I < Self.Count) do
    if Self.ActionAt(I).IsTerminated then begin
      Self.DeleteAction(I);
      Changed := true;
    end
    else
      Inc(I);

  if Changed then
    Self.Observed.NotifyListenersOfChange;
end;

function TIdSipActions.Count: Integer;
begin
  // Return the number of actions, both terminated and ongoing.
  Result := Self.Actions.Count;
end;

function TIdSipActions.CountOf(const MethodName: String): Integer;
var
  I: Integer;
begin
  // Return the number of ongoing (non-session) actions of type MethodName.
  Result := 0;

  // We don't count Sessions because Sessions contain other Actions - they
  // look and act more like containers of Actions than Actions themselves.
  for I := 0 to Self.Actions.Count - 1 do
    if not Self.ActionAt(I).IsSession
      and (Self.ActionAt(I).Method = MethodName)
      and not Self.ActionAt(I).IsTerminated then Inc(Result);
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

procedure TIdSipActions.FindActionAndPerformOr(const ID: String;
                                               FoundBlock: TIdSipActionClosure;
                                               NotFoundBlock: TIdSipActionClosure);
var
  Action: TIdSipAction;
begin
  Action := Self.FindAction(ID);

  if Assigned(Action) then
    FoundBlock.Execute(Action)
  else
    NotFoundBlock.Execute(nil);

  Self.CleanOutTerminatedActions;
end;

function TIdSipActions.FindActionForGruu(const LocalGruu: String): TIdSipAction;
var
  Action: TIdSipAction;
  Gruu:   TIdSipUri;
  I:      Integer;
begin
  // Return the non-Owned action that uses LocalGruu as its Contact.

  Gruu := TIdSipUri.Create(LocalGruu);
  try
    Result := nil;
    I      := 0;
    while (I < Self.Count) and not Assigned(Result) do begin
      Action := Self.ActionAt(I);
      if not Action.IsOwned and Action.LocalGruu.Address.Equals(Gruu) then
        Result := Action
      else
        Inc(I);
    end;
  finally
    Gruu.Free;
  end;
end;

function TIdSipActions.FindActionThatSent(Request: TIdSipRequest): TIdSipAction;
var
  A: TIdSipAction;
  I: Integer;
begin
  // Find the action that SENT request.

  Result := nil;
  for I := 0 to Self.Actions.Count - 1 do begin
    A := Self.ActionAt(I);
    if A.IsOutbound and A.InitialRequest.Equals(Request) then begin
      Result := A;
      Break;
    end;
  end;
end;

function TIdSipActions.FindActionWithInitialRequest(Request: TIdSipRequest): TIdSipAction;
var
  I: Integer;
begin
  // Find the action that SENT OR RECEIVED request.

  Result := nil;
  for I := 0 to Self.Actions.Count - 1 do begin
    if Self.ActionAt(I).InitialRequest.Equals(Request) then begin
      Result := Self.ActionAt(I);
      Break;
    end;
  end;
end;

function TIdSipActions.InviteCount: Integer;
begin
  Result := Self.CountOf(MethodInvite);
end;

function TIdSipActions.OptionsCount: Integer;
begin
  Result := Self.CountOf(MethodOptions);
end;

procedure TIdSipActions.Perform(Msg: TIdSipMessage; Block: TIdSipActionClosure; ClientAction: Boolean);
var
  Action: TIdSipAction;
begin
  // Find the action, and execute Block regardless of whether we found the
  // action. FindAction returns nil in this case.

  Action := Self.FindAction(Msg, ClientAction);

  Block.Execute(Action);

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
  Result := 0;

  for I := 0 to Self.Actions.Count - 1 do
    if Self.ActionAt(I).IsSession
      and not Self.ActionAt(I).IsTerminated then
      Inc(Result);
end;

procedure TIdSipActions.Status(Keys: TStringDictionary);
begin
  // Return a dictionary containing information about the current state of all
  // actions.
  //
  // For now, this returns only a count of each kind of action currently
  // executing. 
  Self.AddCurrentActionList(Keys);
end;

procedure TIdSipActions.TerminateAllActions;
var
  I: Integer;
begin
  for I := 0 to Self.Actions.Count - 1 do
    if not Self.ActionAt(I).IsOwned
      and not Self.ActionAt(I).IsTerminated then
      Self.ActionAt(I).Terminate;
end;

//* TIdSipActions Private methods **********************************************

function TIdSipActions.ActionAt(Index: Integer): TIdSipAction;
begin
  // Precondition: you've invoked Self.LockActions
  Result := Self.Actions[Index] as TIdSipAction;
end;

procedure TIdSipActions.AddCurrentActionList(Keys: TStringDictionary);
  procedure Increment(H: TStringList; Index: Integer);
  begin
    // Given H, a TStringList that stores a string key and a cardinal value (in
    // Objects[Index]), increment the integer value of the key at index Index.
    H.Objects[Index] := Pointer(Cardinal(H.Objects[Index]) + 1);
  end;
var
  I:         Integer;
  Key:       String;
  Histogram: TStringList;
begin
  // Collect the number of each type of Action currently in progress.
  Histogram := TStringList.Create;
  try
    Histogram.Duplicates := dupIgnore;
    for I := 0 to Self.Actions.Count - 1 do begin
      Key := Self.ActionAt(I).IntrospectionCountKey;

      if (Histogram.IndexOf(Key) = ItemNotFoundIndex) then
        Histogram.Add(Key);

      Increment(Histogram, Histogram.IndexOf(Key));
    end;

    Histogram.Sort;

    for I := 0 to Histogram.Count - 1 do
      Keys.Add(Histogram[I], IntToStr(Cardinal(Histogram.Objects[I])));
  finally
    Histogram.Free;
  end;
end;

procedure TIdSipActions.DeleteAction(Index: Integer);
var
  TypeOfClass: String;
begin
  try
    TypeOfClass := Self.ActionAt(Index).ClassName;
    if Assigned(Self.OnRemoveAction) then
      Self.OnRemoveAction(Self.ActionAt(Index));

    Self.Actions.Delete(Index)
  except
    on E: Exception do begin
      raise ExceptClass(E.ClassType).Create(Format('%s (While trying to delete %s)', [E.Message, TypeOfClass]));
    end;
  end;
end;

function TIdSipActions.FindAction(Msg: TIdSipMessage; ClientAction: Boolean): TIdSipAction;
var
  Action: TIdSipAction;
  I:      Integer;
begin
  // Precondition: You've locked Self.ActionLock.
  Result := nil;

  I := 0;
  while (I < Self.Actions.Count) and not Assigned(Result) do begin
    Action := Self.Actions[I] as TIdSipAction;

    // First, if an Action's Terminated we're not interested in dispatching to it.
    // Second, the message has to match the Action (as defined per the type of Action).
    // Third, OwningActions don't typically handle messages directly: Sessions use
    // Invites, for instance.
    // Fourth, do we match the message against the UAC actions (actions we
    // initiated) or against the UAS actions?
    if not Action.IsTerminated
      and Action.Match(Msg) then begin

      if Action.IsOwned then begin
        if (Action.IsInbound = not ClientAction) then
          Result := Action;
      end
      else begin
        Result := Action;
      end;
    end;

    if not Assigned(Result) then
      Inc(I);
  end;
end;

function TIdSipActions.FindAction(const ActionID: String): TIdSipAction;
var
  Action: TObject;
begin
  Action := TIdObjectRegistry.FindObject(ActionID);

  if not Assigned(Action) then begin
    Result := nil;
    Exit;
  end;

  if not (Action is TIdSipAction) then
    raise ERegistry.Create(ActionID + ' does not point to a TIdSipAction, but a ' + Action.ClassName);

  Result := Action as TIdSipAction;
end;

procedure TIdSipActions.MarkAllActionsAsTerminated;
var
  I: Integer;
begin
  // Some Actions listen to others. By first marking all Actions as Terminated,
  // the listened-to Actions can tell their Listeners to _stop_ listening. Thus,
  // those Actions that need to keep track of which actions they listen to, can
  // safely remove themselves as Listeners.
  //
  // Another possibility is keeping the Actions in "reverse topological order" -
  // the Action at index N listens only to Actions with index > N. Sounds
  // horrible! Hm, one could REMOVE actions in reverse topological order: while
  // we still have actions, remove those actions which have no listeners. Each
  // iteration causes a cascade of Listener removal.

  for I := 0 to Self.Count - 1 do
    Self.ActionAt(I).MarkAsTerminated;
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
    Self.Actions.FindActionAndPerform(Self.ActionID, Block);
  finally
    Block.Free;
  end;
end;

//******************************************************************************
//* TIdSipActionAuthenticateWait                                               *
//******************************************************************************
//* TIdSipActionAuthenticateWait Public methods ********************************

destructor TIdSipActionAuthenticateWait.Destroy;
begin
  Self.Credentials.Free;

  inherited Destroy;
end;

procedure TIdSipActionAuthenticateWait.SetCredentials(Credentials: TIdSipAuthorizationHeader);
begin
  Self.Credentials := TIdSipHeader.ConstructHeader(Credentials.Name) as TIdSipAuthorizationHeader;
  Self.Credentials.Assign(Credentials);
end;

procedure TIdSipActionAuthenticateWait.Trigger;
var
  Action: TObject;
begin
  Action := TIdObjectRegistry.FindObject(Self.ActionID);

  if Assigned(Action) and (Action is TIdSipAction) then begin
    if Assigned(Self.Credentials) then
      (Action as TIdSipAction).Resend(Self.Credentials);
  end;
end;

//******************************************************************************
//* TIdSipActionSendWait                                                       *
//******************************************************************************
//* TIdSipActionSendWait Public methods ****************************************

procedure TIdSipActionSendWait.Trigger;
var
  Action: TObject;
begin
  Action := TIdObjectRegistry.FindObject(Self.ActionID);

  if Assigned(Action) and (Action is TIdSipAction) then
    (Action as TIdSipAction).Send;
end;

//******************************************************************************
//* TIdSipActionTerminateWait                                                  *
//******************************************************************************
//* TIdSipActionTerminateWait **************************************************

procedure TIdSipActionTerminateWait.Trigger;
var
  Action: TObject;
begin
  Action := TIdObjectRegistry.FindObject(Self.ActionID);

  if Assigned(Action) and (Action is TIdSipAction) then
    (Action as TIdSipAction).Terminate;
end;

//******************************************************************************
//* TIdSipActionNetworkFailure                                                 *
//******************************************************************************
//* TIdSipActionNetworkFailure Public methods **********************************

procedure TIdSipActionNetworkFailure.Execute(Action: TIdSipAction);
begin
  if Assigned(Action) then
    Action.NetworkFailureSending(Self.FailedMessage);
end;

//******************************************************************************
//* TIdUserAgentClosure                                                        *
//******************************************************************************
//* TIdUserAgentClosure Public methods *****************************************

constructor TIdUserAgentClosure.Create;
begin
  inherited Create;

  Self.fBinding := TIdConnectionBindings.Create;
end;

destructor TIdUserAgentClosure.Destroy;
begin
  Self.Binding.Free;

  inherited Destroy;
end;

//* TIdUserAgentClosure Public methods *****************************************

procedure TIdUserAgentClosure.SetBinding(Value: TIdConnectionBindings);
begin
  Self.Binding.Assign(Value);
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
    Action.ReceiveRequest(Request, Self.Binding);

  if not Assigned(Action) then
    Action := Self.UserAgent.AddInboundAction(Self.Request, Self.Binding);

  if not Assigned(Action) then begin
    if Request.IsAck then
      Self.UserAgent.NotifyOfDroppedMessage(Self.Request, Self.Binding);
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
    Action.ReceiveResponse(Self.Response, Self.Binding)
  else
    Self.UserAgent.NotifyOfDroppedMessage(Self.Response, Self.Binding);
end;

//******************************************************************************
//* TIdAddressOfRecordConnectionList                                           *
//******************************************************************************
//* TIdAddressOfRecordConnectionList Public methods ****************************

constructor TIdAddressOfRecordConnectionList.Create;
begin
  inherited Create;

  Self.fAddressOfRecord := TIdSipUri.Create('');
  Self.fConnections     := TIdConnectionBindingsSet.Create;
end;

destructor TIdAddressOfRecordConnectionList.Destroy;
begin
  Self.fConnections.Free;
  Self.fAddressOfRecord.Free;

  inherited Destroy;
end;

procedure TIdAddressOfRecordConnectionList.Add(Connection: TIdConnectionBindings);
begin
  Self.Connections.Add(Connection);
end;

function TIdAddressOfRecordConnectionList.Count: Integer;
begin
  Result := Self.Connections.Count;
end;

procedure TIdAddressOfRecordConnectionList.Remove(Connection: TIdConnectionBindings);
begin
  Self.Connections.Remove(Connection);
end;

//* TIdAddressOfRecordConnectionList Private methods ***************************

procedure TIdAddressOfRecordConnectionList.SetAddressOfRecord(Value: TIdSipUri);
begin
  Self.fAddressOfRecord.Uri := Value.AsString;
end;

//******************************************************************************
//* TIdConnectionAssociationSet                                                *
//******************************************************************************
//* TIdConnectionAssociationSet Public methods *********************************

constructor TIdConnectionAssociationSet.Create;
begin
  inherited Create;

  Self.AORs := TObjectList.Create(true);
end;

destructor TIdConnectionAssociationSet.Destroy;
begin
  Self.AORs.Free;

  inherited Destroy;
end;

procedure TIdConnectionAssociationSet.Add(AOR: TIdSipUri; Binding: TIdConnectionBindings);
var
  Entry: TIdAddressOfRecordConnectionList;
begin
  Entry := Self.BindingsFor(AOR);

  if not Assigned(Entry) then
    Entry := Self.CreateNewEntry(AOR);

  if not Entry.Connections.HasBinding(Binding) then
    Entry.Add(Binding);
end;

procedure TIdConnectionAssociationSet.ConnectionsFor(AOR: TIdSipUri; Bindings: TIdConnectionBindingsSet);
var
  Entry: TIdAddressOfRecordConnectionList;
begin
  Entry := Self.BindingsFor(AOR);

  if not Assigned(Entry) then Exit;

  Bindings.Clear;
  Bindings.Assign(Entry.Connections);
end;

function TIdConnectionAssociationSet.Count: Integer;
begin
  Result := Self.AORs.Count;
end;

procedure TIdConnectionAssociationSet.Remove(AOR: TIdSipUri; Binding: TIdConnectionBindings);
begin
  Self.InternalRemove(Self.BindingsFor(AOR), Binding);
end;

procedure TIdConnectionAssociationSet.RemoveConnection(Binding: TIdConnectionBindings);
begin
  Self.InternalRemove(Self.BindingsContaining(Binding), Binding);
end;

//* TIdConnectionAssociationSet Private methods ********************************

function TIdConnectionAssociationSet.CreateNewEntry(AOR: TIdSipUri): TIdAddressOfRecordConnectionList;
begin
  Result := TIdAddressOfRecordConnectionList.Create;
  Self.AORs.Add(Result);
  Result.AddressOfRecord.Uri := AOR.AsString;
end;

function TIdConnectionAssociationSet.BindingsContaining(Connection: TIdConnectionBindings): TIdAddressOfRecordConnectionList;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Self.AORs.Count - 1 do begin
    if Self.EntryAt(I).Connections.HasBinding(Connection) then begin
      Result := Self.EntryAt(I);
      Break;
    end;
  end;
end;

function TIdConnectionAssociationSet.BindingsFor(AOR: TIdSipUri): TIdAddressOfRecordConnectionList;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Self.AORs.Count - 1 do begin
    if Self.EntryAt(I).AddressOfRecord.Equals(AOR) then begin
      Result := Self.EntryAt(I);
      Break;
    end;
  end;
end;

function TIdConnectionAssociationSet.EntryAt(Index: Integer): TIdAddressOfRecordConnectionList;
begin
  Result := Self.AORs[Index] as TIdAddressOfRecordConnectionList;
end;

procedure TIdConnectionAssociationSet.InternalRemove(Entry: TIdAddressOfRecordConnectionList;
                                                     Binding: TIdConnectionBindings);
begin
  if not Assigned(Entry) then Exit;

  Entry.Remove(Binding);

  if Entry.Connections.IsEmpty then
    Self.AORs.Remove(Entry);
end;

//******************************************************************************
//* TIdSipAbstractCore                                                         *
//******************************************************************************
//* TIdSipAbstractCore Public methods ******************************************

constructor TIdSipAbstractCore.Create;
begin
  inherited Create;

  Self.fActions := TIdSipActions.Create;
  Self.Actions.AddObserver(Self);
  Self.Actions.OnAddAction    := Self.NotifyOfAddedAction;
  Self.Actions.OnRemoveAction := Self.NotifyOfRemovedAction;

  Self.fAllowedLanguageList := TStringList.Create;
  Self.fAllowedSchemeList   := TStringList.Create;
  Self.fKeyring             := TIdKeyRing.Create;

  Self.Listeners  := TIdNotificationList.Create;
  Self.Listeners.AddExpectedException(EParserError);

  Self.Modules                := TObjectList.Create(true);
  Self.NullModule             := TIdSipNullModule.Create(Self);
  Self.Observed               := TIdObservable.Create;
  Self.OpenInboundConnections := TIdConnectionAssociationSet.Create;
  Self.Proxies                := TIdProxyDescriptions.Create;

  Self.AddModule(TIdSipOptionsModule);
  Self.AddAllowedScheme(SipScheme);

  Self.HostName      := Self.DefaultHostName;
  Self.Realm         := Self.HostName;
  Self.UserAgentName := Self.DefaultUserAgent;
end;

destructor TIdSipAbstractCore.Destroy;
begin
  Self.NotifyModulesOfFree;

  Self.Proxies.Free;
  Self.OpenInboundConnections.Free;
  Self.Observed.Free;
  Self.NullModule.Free;
  Self.Modules.Free;
  Self.Listeners.Free;
  Self.Keyring.Free;
  Self.AllowedSchemeList.Free;
  Self.AllowedLanguageList.Free;
  Self.Actions.Free;

  inherited Destroy;
end;

function TIdSipAbstractCore.AddAction(Action: TIdSipAction): TIdSipAction;
begin
  Result := Self.Actions.Add(Action);
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
  if not TIdUri.IsScheme(Scheme) then
    raise EIdSipBadSyntax.Create('Not a valid scheme');

  if (Self.AllowedSchemeList.IndexOf(Scheme) = ItemNotFoundIndex) then
    Self.AllowedSchemeList.Add(Scheme);
end;

function TIdSipAbstractCore.AddInboundAction(Request: TIdSipRequest;
                                             Binding: TIdConnectionBindings): TIdSipAction;
var
  Module: TIdSipMessageModule;
begin
  Module := Self.ModuleFor(Request);

  if Assigned(Module) then begin
    Result := Module.Accept(Request, Binding);

    if Assigned(Result) then begin
      Self.AddAction(Result);
    end;
  end
  else
    Result := nil;
end;

procedure TIdSipAbstractCore.AddListener(Listener: IIdSipTransactionUserListener);
begin
  Self.Listeners.AddListener(Listener);
end;

procedure TIdSipAbstractCore.AddLocalHeaders(OutboundRequest: TIdSipRequest; InDialogRequest: Boolean);
begin
  // You might think we need to find out the appropriate transport to use before
  // we send the message. Yes, we do. We do so when the Action actually sends
  // the request in Action.Send(Request|Response).

  if not OutboundRequest.IsAck and OutboundRequest.Path.IsEmpty then begin
    OutboundRequest.AddHeader(ViaHeaderFull);
    OutboundRequest.LastHop.SipVersion := SipVersion;
    OutboundRequest.LastHop.Transport  := OutboundRequest.DefaultTransport;
    OutboundRequest.LastHop.SentBy     := Self.HostName;
    OutboundRequest.LastHop.Branch     := Self.NextBranch;
  end;

  if (Self.UserAgentName <> '') then
    OutboundRequest.AddHeader(UserAgentHeader).Value := Self.UserAgentName;

  Self.AddModuleSpecificHeaders(OutboundRequest);
  OutboundRequest.Supported.Value := Self.AllowedExtensions;
end;

function TIdSipAbstractCore.AddModule(ModuleType: TIdSipMessageModuleClass): TIdSipMessageModule;
begin
  if not Self.UsesModule(ModuleType) then begin
    Result := ModuleType.Create(Self);
    Self.Modules.Add(Result);
  end
  else begin
    Result := Self.ModuleFor(ModuleType);
  end;
end;

procedure TIdSipAbstractCore.AddNullRoutePath(AddressSpace: String);
var
  EmptyPath: TIdSipRoutePath;
begin
  EmptyPath := TIdSipRoutePath.Create;
  try
    Self.Proxies.AddDescription(AddressSpace, EmptyPath);
  finally
    EmptyPath.Free;
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

procedure TIdSipAbstractCore.AddRoute(AddressSpace: String; SipEntity: TIdSipUri);
begin
  // Make sure that requests sent to targets in AddressSpace pass through
  // SipEntity.
  Self.Proxies.AddRouteFor(AddressSpace, SipEntity);
end;

function TIdSipAbstractCore.AllowedContentTypes: String;
var
  ContentTypes: TStringList;
  I:            Integer;
begin
  // Collect a list of all known MIME types from the modules, and ensure there
  // are no duplicates.

  ContentTypes := TStringList.Create;
  try
    ContentTypes.Duplicates := dupIgnore;
    ContentTypes.Sorted     := true;

    for I := 0 to Self.Modules.Count - 1 do
      ContentTypes.AddStrings(Self.ModuleAt(I).AllowedContentTypes);

    ContentTypes.Add(SdpMimeType);

    Result := Self.ConvertToHeader(ContentTypes);
  finally
    ContentTypes.Free;
  end;
end;

function TIdSipAbstractCore.AllowedEncodings: String;
begin
  Result := '';
end;

function TIdSipAbstractCore.AllowedExtensions: String;
var
  Extensions: TStringList;
begin
  Extensions := TStringList.Create;
  try
    Extensions.Duplicates := dupIgnore;
    Extensions.Sorted     := true;
    Self.CollectAllowedExtensions(Extensions);

    Extensions.Add(ExtensionGruu);

    Result := Self.ConvertToHeader(Extensions);
  finally
    Extensions.Free;
  end;
end;

function TIdSipAbstractCore.AllowedLanguages: String;
begin
  Result := Self.ConvertToHeader(Self.AllowedLanguageList);
end;

function TIdSipAbstractCore.AllowedMethods(RequestUri: TIdSipUri): String;
begin
  // TODO: This is fake.
  Result := Self.KnownMethods;
end;

function TIdSipAbstractCore.AllowedSchemes: String;
begin
  Result := Self.ConvertToHeader(Self.AllowedSchemeList);
end;

function TIdSipAbstractCore.Authenticate(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  try
    // We should ALWAYS have an authenticator attached: see TIdSipStackConfigurator.
    if Assigned(Self.Authenticator) and Self.Authenticator.Authenticate(Request) then
      Result := uarAccept
    else
      Result := uarUnauthorized;
  except
    on EAuthenticate do
      Result := uarBadAuthorization;
  end;
end;

procedure TIdSipAbstractCore.ClearAllPreferredTransportTypes;
begin
  Self.Dispatcher.ClearAllPreferredTransportTypes;
end;

procedure TIdSipAbstractCore.ClearAllRoutePaths;
begin
  Self.Proxies.ClearAllParameters;
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

function TIdSipAbstractCore.CreateRedirectedRequest(OriginalRequest: TIdSipRequest;
                                                    Contact: TIdSipAddressHeader): TIdSipRequest;
begin
  Result := TIdSipRequest.Create;
  Result.Assign(OriginalRequest);
  Result.CSeq.SequenceNo := Self.NextInitialSequenceNo;
  Result.LastHop.Branch  := Self.NextBranch;
  Result.RequestUri      := Contact.Address;
end;

function TIdSipAbstractCore.CreateRequest(const Method: String;
                                          From: TIdSipAddressHeader;
                                          Dest: TIdSipAddressHeader;
                                          MaxForwards: Cardinal): TIdSipRequest;
begin
  if Dest.Address.HasMethod then begin
    if (Method <> Dest.Address.Method) then
      raise EIdSipTransactionUser.Create(Format(ContradictoryCreateRequestInvocation,
                                                [Method, Dest.Address.Method]));
  end;

  Result := Dest.Address.CreateRequest;
  try
    Result.CallID         := Self.NextCallID;
    Result.From.Value     := From.FullValue;
    Result.From.Tag       := Self.NextTag;
    Result.MaxForwards    := MaxForwards;
    Result.Method         := Method;
    Result.ToHeader.Value := Dest.FullValue;

    Result.CSeq.Method     := Result.Method;
    Result.CSeq.SequenceNo := Self.NextInitialSequenceNo;

    Self.AddLocalHeaders(Result, false);
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

    Self.AddLocalHeaders(Result, true);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipAbstractCore.CreateResponse(Request: TIdSipRequest;
                                           ResponseCode: Cardinal): TIdSipResponse;
begin
  Result := TIdSipResponse.InResponseTo(Request,
                                        ResponseCode);

  Self.PrepareResponse(Result, Request);
end;

function TIdSipAbstractCore.CreateResponse(Request: TIdSipRequest;
                                           ResponseCode: Cardinal;
                                           Contact: TIdSipContactHeader): TIdSipResponse;
begin
  Result := TIdSipResponse.InResponseTo(Request,
                                        ResponseCode,
                                        Contact);

  Self.PrepareResponse(Result, Request);
end;

function TIdSipAbstractCore.FindActionForGruu(const LocalGruu: String): TIdSipAction;
begin
  Result := Self.Actions.FindActionForGruu(LocalGruu);
end;

procedure TIdSipAbstractCore.FindServersFor(Request: TIdSipRequest;
                                            Result: TIdSipLocations);
begin
  Self.Locator.FindServersFor(Request.DestinationUri, Result);

  if Self.UseInboundConnections then
    Self.PrependConnectionLocations(Request, Result);
end;

procedure TIdSipAbstractCore.FindServersFor(Response: TIdSipResponse;
                                            Result: TIdSipLocations);
begin
  Self.Locator.FindServersFor(Response, Result);
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

function TIdSipAbstractCore.HasUnsupportedExtension(Msg: TIdSipMessage): Boolean;
var
  I: Integer;
begin
  if not Msg.HasHeader(SupportedHeaderFull) then begin
    Result := false;
    Exit;
  end;

  Result := true;
  for I := 0 to Msg.Supported.Values.Count - 1 do
    Result := Result and Self.IsExtensionAllowed(Msg.Supported.Values[I]);

  Result := not Result;
end;

function TIdSipAbstractCore.IsExtensionAllowed(const Extension: String): Boolean;
begin
  Result := Pos(Extension, Self.AllowedExtensions) > 0;
end;

function TIdSipAbstractCore.IsMethodAllowed(RequestUri: TIdSipUri;
                                            const Method: String): Boolean;
begin
  // Return true iff a request of method Method may be issued against
  // RequestUri.
  //
  // Compare with IsMethodSupported.
  //
  // TODO: This is just a stub at the moment. Eventually we want to support
  // controlling rights for multiple URIs so that, for instance, we could allow a
  // non-User Agent to say "yes, you can SUBSCRIBE to A's state, but not to B's".
  
  Result := Self.IsMethodSupported(Method);
end;

function TIdSipAbstractCore.IsMethodSupported(const Method: String): Boolean;
begin
  // Return true iff this stack can process a request of method Method,
  // which is equivalent to "do we have a MessageModule that can or will process
  // this method?
  //
  // Compare with IsMethodAllowed.

  Result := not Self.ModuleFor(Method).IsNull;
end;

function TIdSipAbstractCore.IsSchemeAllowed(const Scheme: String): Boolean;
begin
  Result := Self.AllowedSchemeList.IndexOf(Scheme) >= 0;
end;

function TIdSipAbstractCore.IsSourceOf(Request: TIdSipRequest): Boolean;
begin
  // Did an action of mine send Request?
   
  Result := Assigned(Self.Actions.FindActionThatSent(Request));
end;

function TIdSipAbstractCore.KnownMethods: String;
const
  Delimiter = ', ';
var
  I:              Integer;
  ModulesMethods: String;
begin
  // I provide a comma-separated list of all the SIP methods I (i.e., my various
  // modules) support.

  Result := '';
  for I := 0 to Self.Modules.Count - 1 do begin
    ModulesMethods := Self.ModuleAt(I).AcceptsMethods;
    if (ModulesMethods <> '') then
      Result := Result + ModulesMethods + Delimiter;
  end;

  if (Result <> '') then
    Delete(Result, Length(Result) - 1, Length(Delimiter));
end;

procedure TIdSipAbstractCore.Log(Description: String;
                                 Severity: TSeverityLevel;
                                 EventRef: Cardinal;
                                 DebugInfo: String);
begin
  LogEntry(Description, Self.ClassName, Severity, EventRef, DebugInfo);
end;

function TIdSipAbstractCore.ModuleFor(Request: TIdSipRequest): TIdSipMessageModule;
var
  I: Integer;
begin
  Result := Self.NullModule;

  I := 0;
  while (I < Self.Modules.Count) and Result.IsNull do
    if Self.ModuleAt(I).WillAccept(Request) then
      Result := Self.Modules[I] as TIdSipMessageModule
    else
      Inc(I);

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
  Result := Self.NullModule;

  while (I < Self.Modules.Count) and Result.IsNull do begin
    if (Self.Modules[I] is ModuleType) then
      Result := Self.Modules[I] as TIdSipMessageModule
    else Inc(I);
  end;
end;

function TIdSipAbstractCore.NextBranch: String;
begin
  Result := BranchMagicCookie + GRandomNumber.Next128bitNumber;
end;

function TIdSipAbstractCore.NextCallID: String;
begin
  Result := GRandomNumber.NextHexString + '@' + Self.HostName;
end;

function TIdSipAbstractCore.NextGrid: String;
begin
  Result := GRandomNumber.NextHexString;
end;

function TIdSipAbstractCore.NextInitialSequenceNo: Cardinal;
begin
  Result := GRandomNumber.NextCardinal(HighestInitialSequenceNo);
end;

function TIdSipAbstractCore.NextNonce: String;
begin
  Result := GRandomNumber.NextHexString;
end;

function TIdSipAbstractCore.NextTag: String;
begin
  Result := GRandomNumber.NextSipUserAgentTag;
end;

function TIdSipAbstractCore.QueryOptions(Server: TIdSipAddressHeader): TIdSipAction;
var
  Module: TIdSipMessageModule;
begin
  Module := Self.ModuleFor(MethodOptions);

  Assert(Assigned(Module), 'All SIP UAs MUST support OPTIONS messages');
  Result := (Module as TIdSipOptionsModule).QueryOptions(Server);
end;

procedure TIdSipAbstractCore.RemoveListener(Listener: IIdSipTransactionUserListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

procedure TIdSipAbstractCore.RemoveModule(ModuleType: TIdSipMessageModuleClass);
var
  I: Integer;
begin
  I := 0;
  while (I < Self.Modules.Count) do begin
    if (Self.ModuleAt(I).ClassType = ModuleType) then begin
      Self.Modules.Delete(I);
      Break;
    end
    else
      Inc(I);
  end;
end;

procedure TIdSipAbstractCore.RemoveObserver(const Listener: IIdObserver);
begin
  Self.Observed.RemoveObserver(Listener);
end;

procedure TIdSipAbstractCore.RemoveRoutePathTo(AddressSpace: String);
begin
  Self.Proxies.RemoveDescription(AddressSpace);
end;

function TIdSipAbstractCore.RequiresUnsupportedExtension(Request: TIdSipRequest): Boolean;
var
  I: Integer;
begin
  Result := false;

  if not Request.HasHeader(RequireHeader) then Exit;

  for I := 0 to Request.Require.Values.Count - 1 do
    Result := Result or not Self.IsExtensionAllowed(Request.Require.Values[I]);
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

function TIdSipAbstractCore.RoutePathFor(Request: TIdSipRequest): TIdSipRoutePath;
begin
  Result := Self.Proxies.RoutePathFor(Request.ToHeader.Address.Host);
end;

function TIdSipAbstractCore.RoutePathFor(Target: TIdSipLocation): TIdSipRoutePath;
begin
  Result := Self.Proxies.RoutePathFor(Target.IPAddress);
end;

procedure TIdSipAbstractCore.ScheduleEvent(BlockType: TIdSipActionClosureClass;
                                           WaitTime: Cardinal;
                                           Copy: TIdSipMessage;
                                           const ActionID: String);
var
  Event: TIdSipActionsWait;
begin
  if not Assigned(Self.Timer) then
    Exit;

  Event := Self.CreateActionsClosure(TIdSipActionsWait, Copy) as TIdSipActionsWait;
  Event.BlockType := BlockType;
  Event.ActionID  := ActionID;
  Self.ScheduleEvent(WaitTime, Event);
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
  // cf. RFC 3261, section 18.1.1
  if Request.ExceedsMaximumUdpMessageSize and (Request.LastHop.Transport = UdpTransport) then
    Dest.Transport := TcpTransport;

  Request.LastHop.Transport := Dest.Transport;

  if Self.RequiresUnsupportedExtension(Request) then
    raise EIdSipTransactionUser.Create(Format(MessageSendFailureUnknownExtension,
                                              [Request.Require.Value]));

  if Self.HasUnsupportedExtension(Request) then
    raise EIdSipTransactionUser.Create(Format(MessageSendFailureUnknownExtension,
                                              [Request.Supported.Value]));

  // If we know how to receive the message, or we're a UA and we're sending a
  // REGISTER, then send the message. Otherwise, raise an exception.
  if    (Self.ModuleFor(Request.Method).IsNull
    and (Self.ModuleFor(TIdSipOutboundRegisterModule).IsNull or not Request.IsRegister)) then
    raise EIdSipTransactionUser.Create(Format(MessageSendFailureUnknownMethod,
                                              [Request.Method]));

  if Request.IsMalformed then
    raise EIdSipTransactionUser.Create(Format(MessageSendFailureMalformed,
                                              [Request.ParseFailReason]));

  Self.Dispatcher.SendRequest(Request, Dest);
end;

procedure TIdSipAbstractCore.SendResponse(Response: TIdSipResponse);
begin
  if Self.HasUnsupportedExtension(Response) then
    raise EIdSipTransactionUser.Create(Format(MessageSendFailureUnknownExtension,
                                              [Response.Supported.Value]));

  if Response.IsMalformed then
    raise EIdSipTransactionUser.Create(Format(MessageSendFailureMalformed,
                                              [Response.ParseFailReason]));

  Self.Dispatcher.SendResponse(Response);
end;

procedure TIdSipAbstractCore.StartAllTransports;
begin
  Self.Dispatcher.StartAllTransports;
end;

procedure TIdSipAbstractCore.StopAllTransports;
begin
  Self.Dispatcher.StopAllTransports;
end;

function TIdSipAbstractCore.UsesModule(ModuleType: TIdSipMessageModuleClass): Boolean;
begin
  Result := not Self.ModuleFor(ModuleType).IsNull;
end;

function TIdSipAbstractCore.UsesModule(Method: String): Boolean;
begin
  Result := not Self.ModuleFor(Method).IsNull;
end;

//* TIdSipAbstractCore Protected methods ***************************************

procedure TIdSipAbstractCore.ActOnRequest(Request: TIdSipRequest;
                                          Binding: TIdConnectionBindings);
var
  Actor: TIdSipUserAgentActOnRequest;
begin
  Actor := Self.CreateRequestHandler(Request, Binding);
  try
    Self.Actions.Perform(Request, Actor, false);
  finally
    Actor.Free;
  end;

  Self.AddConnection(Request, Binding);
end;

procedure TIdSipAbstractCore.ActOnResponse(Response: TIdSipResponse;
                                           Binding: TIdConnectionBindings);
var
  Actor: TIdSipUserAgentActOnResponse;
begin
  Actor := Self.CreateResponseHandler(Response, Binding);
  try
    Self.Actions.Perform(Response, Actor, true);
  finally
    Actor.Free;
  end;

  Self.AddConnection(Response, Binding);
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
                                                    Binding: TIdConnectionBindings);
var
  Notification: TIdSipUserAgentDroppedUnmatchedMessageMethod;
begin
  Self.LogDroppedMessage(Message, Binding);

  Notification := TIdSipUserAgentDroppedUnmatchedMessageMethod.Create;
  try
    Notification.Binding   := Binding;
    Notification.Message   := Message;
    Notification.UserAgent := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
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

  Response.AddHeader(SupportedHeaderFull).Value := Self.AllowedExtensions;

  Self.AddModuleSpecificHeaders(Response);
end;

procedure TIdSipAbstractCore.RejectRequest(Reaction: TIdSipUserAgentReaction;
                                           Request: TIdSipRequest);
begin
  case Reaction of
    uarBadAuthorization:
      Self.RejectBadAuthorization(Request);
    uarLoopDetected:
      Self.ReturnResponse(Request, SIPLoopDetected);
    uarUnauthorized:
      Self.RejectRequestUnauthorized(Request);
    uarUnsupportedExtension:
      Self.RejectRequestBadExtension(Request);
    uarUnsupportedMethod:
      Self.RejectRequestMethodNotSupported(Request);
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
  // cf RFC 3261 section 8.2
  Result := Self.Authenticate(Request);
  if (Result <> uarAccept) then Exit;

  // inspect the method - 8.2.1
  // This result code means "I have no modules that can accept this method"
  if not Self.IsMethodSupported(Request.Method) then
    Result := uarUnsupportedMethod
  // Merged requests - 8.2.2.2
  else if not Request.ToHeader.HasTag and Self.Dispatcher.LoopDetected(Request) then
    Result := uarLoopDetected
  // Require - 8.2.2.3
  else if Self.RequiresUnsupportedExtension(Request) then
    Result := uarUnsupportedExtension;
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

procedure TIdSipAbstractCore.AddConnection(Msg: TIdSipMessage; Binding: TIdConnectionBindings);
var
  RemoteAOR: TIdSipUri;
begin
  // Something's possibly made a new connection to us. We note the AOR of the
  // message sender, and the binding on which we received the message.

  if Msg.IsRequest then
    RemoteAOR := Msg.From.Address
  else
    RemoteAOR := Msg.ToHeader.Address;

  Self.OpenInboundConnections.Add(RemoteAOR, Binding);
end;

procedure TIdSipAbstractCore.AddModuleSpecificHeaders(OutboundMessage: TIdSipMessage);
var
  I: Integer;
begin
  for I := 0 to Self.Modules.Count - 1 do
    Self.ModuleAt(I).AddLocalHeaders(OutboundMessage);
end;

procedure TIdSipAbstractCore.CollectAllowedExtensions(ExtensionList: TStrings);
var
  I:                Integer;
  ModuleExtensions: TStrings;
begin
  ExtensionList.Clear;

  ModuleExtensions := TStringList.Create;
  try
    for I := 0 to Self.Modules.Count - 1 do begin
      ModuleExtensions.CommaText := Self.ModuleAt(I).AllowedExtensions;
      ExtensionList.AddStrings(ModuleExtensions);
    end;
  finally
    ModuleExtensions.Free;
  end;
end;

function TIdSipAbstractCore.ConvertToHeader(ValueList: TStrings): String;
begin
  Result := StringReplace(ValueList.CommaText, ',', ', ', [rfReplaceAll]);
end;

function TIdSipAbstractCore.CreateRequestHandler(Request: TIdSipRequest;
                                                 Binding: TIdConnectionBindings): TIdSipUserAgentActOnRequest;
begin
  Result := TIdSipUserAgentActOnRequest.Create;

  Result.Binding   := Binding;
  Result.Request   := Request;
  Result.UserAgent := Self;
end;

function TIdSipAbstractCore.CreateResponseHandler(Response: TIdSipResponse;
                                                  Binding: TIdConnectionBindings): TIdSipUserAgentActOnResponse;
begin
  Result := TIdSipUserAgentActOnResponse.Create;

  Result.Binding   := Binding;
  Result.Response  := Response;
  Result.UserAgent := Self;
end;

function TIdSipAbstractCore.DefaultHostName: String;
begin
  Result := 'localhost';
end;

function TIdSipAbstractCore.DefaultUserAgent: String;
begin
  Result := 'RNID SipStack v' + SipStackVersion;
end;

function TIdSipAbstractCore.GetDefaultRoutePath: TIdSipRoutePath;
begin
  Result := Self.Proxies.DefaultRoutePath;
end;

procedure TIdSipAbstractCore.LogDroppedMessage(Message: TIdSipMessage;
                                               Binding: TIdConnectionBindings);
const
  LogMsg = 'Dropped %s from %s because it doesn''t match any ongoing actions';
begin
  Self.Log(Format(LogMsg, [Message.Description, Binding.AsString]),
           slDebug,
           LogEventRefDroppedMessage,
           Message.AsString);
end;

function TIdSipAbstractCore.ModuleAt(Index: Integer): TIdSipMessageModule;
begin
  Result := Self.Modules[Index] as TIdSipMessageModule;
end;

procedure TIdSipAbstractCore.NotifyModulesOfFree;
var
  I: Integer;
begin
  for I := 0 to Self.Modules.Count - 1 do
    Self.ModuleAt(I).CleanUp;
end;

procedure TIdSipAbstractCore.NotifyOfAddedAction(Action: TIdSipAction);
var
  Notification: TIdSipUserAgentAddActionMethod;
begin
  Notification := TIdSipUserAgentAddActionMethod.Create;
  try
    Notification.Action    := Action;
    Notification.UserAgent := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipAbstractCore.NotifyOfRemovedAction(Action: TIdSipAction);
var
  Notification: TIdSipUserAgentRemoveActionMethod;
begin
  Notification := TIdSipUserAgentRemoveActionMethod.Create;
  try
    Notification.Action    := Action;
    Notification.UserAgent := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipAbstractCore.OnAddedTransport(Transport: TIdSipTransport);
begin
  Transport.AddConnectionListener(Self);
end;

procedure TIdSipAbstractCore.OnChanged(Observed: TObject);
begin
  Self.NotifyOfChange;
end;

procedure TIdSipAbstractCore.OnConnection(Transport: TIdSipTransport;
                                          Connection: TIdConnectionBindings);
begin
  // Do nothing: ActOn(Request|Response) adds the inbound connections (and we
  // don't care about outbound connections, because those go to targets (peers)
  // found through RFC 3263.
end;

procedure TIdSipAbstractCore.OnDisconnection(Transport: TIdSipTransport;
                                             Connection: TIdConnectionBindings);
begin
  Self.OpenInboundConnections.RemoveConnection(Connection);
end;

procedure TIdSipAbstractCore.OnReceiveRequest(Request: TIdSipRequest;
                                              Binding: TIdConnectionBindings);
var
  Reaction: TIdSipUserAgentReaction;
begin
  Reaction := Self.WillAcceptRequest(Request);
  if (Reaction = uarAccept) then
    Self.ActOnRequest(Request, Binding)
  else
    Self.RejectRequest(Reaction, Request);
end;

procedure TIdSipAbstractCore.OnReceiveResponse(Response: TIdSipResponse;
                                               Binding: TIdConnectionBindings);
begin
  if (Self.WillAcceptResponse(Response) = uarAccept) then
    Self.ActOnResponse(Response, Binding);
end;

procedure TIdSipAbstractCore.OnRemovedTransport(Transport: TIdSipTransport);
begin
  Transport.RemoveConnectionListener(Self);
end;

procedure TIdSipAbstractCore.OnTransportException(FailedMessage: TIdSipMessage;
                                                  const Reason: String);
var
  SendFailed: TIdSipActionNetworkFailure;
begin
  SendFailed := TIdSipActionNetworkFailure.Create;
  try
    SendFailed.FailedMessage := FailedMessage;
    SendFailed.Reason        := Reason;

    // Failing to send a Request means a UAC action's Send failed.
    Self.Actions.Perform(FailedMessage, SendFailed, FailedMessage.IsRequest);
  finally
    SendFailed.Free;
  end;
end;

procedure TIdSipAbstractCore.PrependConnectionLocations(Msg: TIdSipMessage; Targets: TIdSipLocations);
var
  Conns:     TIdConnectionBindingsSet;
  I:         Integer;
  Loc:       TIdSipLocation;
  RemoteAOR: String;
  URI:       TIdSipUri;
begin
  // Targets contains the possible machines to contact for an AOR. We look in
  // our list of open connections for any connections to any of those machines.
  // If there are any, we add Locations representing the peer end of those
  // connections to the front of Targets. (Thus, the transport layer will try
  // send Msg down those open connections first.)

  if Msg.IsRequest then
    RemoteAOR := Msg.ToHeader.AsAddressOfRecord
  else
    RemoteAOR := Msg.From.AsAddressOfRecord;

  URI := TIdSipUri.Create(RemoteAOR);
  try
    Conns := TIdConnectionBindingsSet.Create;
    try
      Self.OpenInboundConnections.ConnectionsFor(URI, Conns);

      for I := 0 to Conns.Count - 1 do begin
        Loc := TIdSipLocation.CreatePeerLocation(Conns[I]);
        try
          Targets.AddLocationToFront(Loc);
        finally
          Loc.Free;
        end;
      end;
    finally
      Conns.Free;
    end;
  finally
    URI.Free;
  end;
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

procedure TIdSipAbstractCore.SetDefaultRoutePath(Value: TIdSipRoutePath);
begin
  Self.DefaultRoutePath.Clear;
  Self.DefaultRoutePath.Add(Value);
end;

procedure TIdSipAbstractCore.SetDispatcher(Value: TIdSipTransactionDispatcher);
var
  I: Integer;
begin
  Self.fDispatcher := Value;

  Self.fDispatcher.AddTransactionDispatcherListener(Self);

  for I := 0 to Self.fDispatcher.TransportCount - 1 do
    Self.fDispatcher.Transports[I].AddConnectionListener(Self);
end;

procedure TIdSipAbstractCore.SetInstanceID(Value: String);
begin
  if not TIdSipParser.IsUuidUrn(Value) then
    raise EIdSipTransactionUser.Create(Format(NotAnUUIDURN, [Value]));

  Self.fInstanceID := Value;
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

  Self.AcceptsMethodsList     := Self.CreateListWithoutDuplicates(true);
  Self.AllowedContentTypeList := Self.CreateListWithoutDuplicates(false);
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
                                    Binding: TIdConnectionBindings): TIdSipAction;
var
  WillAccept: TIdSipUserAgentReaction;
begin
  WillAccept := Self.WillAcceptRequest(Request);

  if (WillAccept = uarAccept) then
    Result := Self.AcceptRequest(Request, Binding)
  else begin
    Result := nil;
    Self.RejectRequest(WillAccept, Request);
  end;
end;

procedure TIdSipMessageModule.AddAllowedContentType(const MimeType: String);
begin
  if (Trim(MimeType) <> '') then begin
    if (Self.AllowedContentTypeList.IndexOf(MimeType) = ItemNotFoundIndex) then
      Self.AllowedContentTypeList.Add(MimeType);
  end;
end;

procedure TIdSipMessageModule.AddAllowedContentTypes(MimeTypes: TStrings);
var
  I: Integer;
begin
  for I := 0 to MimeTypes.Count - 1 do
    Self.AddAllowedContentType(MimeTypes[I]);
end;

procedure TIdSipMessageModule.AddLocalHeaders(OutboundMessage: TIdSipMessage);
begin
end;

function TIdSipMessageModule.AcceptsMethods: String;
begin
  Result := Self.ConvertToHeader(Self.AcceptsMethodsList);
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

procedure TIdSipMessageModule.Configure(Params: TIdSipHeaderParameters);
begin
  // This method allows subclasses to configure themselves based on Params.
  // The idea is that this allows the user of the stack to control the
  // module's configuration through a configuration file.
end;

function TIdSipMessageModule.HasKnownAccept(Request: TIdSipRequest): Boolean;
var
  I: Integer;
begin
  // No Accept header means the same as "Accept: application/sdp" - cf. RFC
  // 3261, section 11.2
  Result := not Request.HasHeader(AcceptHeader);

  if not Result then begin
    Result := Request.Accept.ValueCount = 0;

    if not Result then begin
      for I := 0 to Request.Accept.ValueCount - 1 do begin
        Result := Self.SupportsMimeType(Request.Accept.Values[I].Value);

        if Result then Break;
      end;
    end;
  end;
end;

function TIdSipMessageModule.HasUnknownContentType(Request: TIdSipRequest): Boolean;
begin
  Result := Self.ListHasUnknownValue(Request,
                                     Self.AllowedContentTypeList,
                                     ContentTypeHeaderFull);
end;

function TIdSipMessageModule.IsNull: Boolean;
begin
  Result := false;
end;

function TIdSipMessageModule.SupportsMimeType(const MimeType: String): Boolean;
begin
  Result := Self.AllowedContentTypeList.IndexOf(MimeType) <> ItemNotFoundIndex;
end;

function TIdSipMessageModule.WillAccept(Request: TIdSipRequest): Boolean;
begin
  // Return true if we're the kind of module that can process this request.

  Result := Self.AcceptsMethodsList.IndexOf(Request.Method) <> ItemNotFoundIndex;
end;

//* TIdSipMessageModule Protected methods **************************************

function TIdSipMessageModule.AcceptRequest(Request: TIdSipRequest;
                                           Binding: TIdConnectionBindings): TIdSipAction;
begin
  Result := nil;
end;

function TIdSipMessageModule.ListHasUnknownValue(Request: TIdSipRequest;
                                                 ValueList: TStrings;
                                                 const HeaderName: String): Boolean;
begin
  Result := Request.HasHeader(HeaderName)
       and (ValueList.IndexOf(Request.FirstHeader(HeaderName).Value) = ItemNotFoundIndex);
end;

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

procedure TIdSipMessageModule.RejectRequest(Reaction: TIdSipUserAgentReaction;
                                            Request: TIdSipRequest);
begin
  case Reaction of
    uarDoNotDisturb:
          Self.ReturnResponse(Request,
                              SIPTemporarilyUnavailable);
    uarDoNothing:; // Do nothing, and the Transaction-User core will drop the message.                          
    uarMethodNotAllowed:
      Self.UserAgent.RejectMethodNotAllowed(Request);
    uarMissingContact:
      Self.RejectBadRequest(Request, MissingContactHeader);
    uarNonInviteWithReplaces:
      Self.RejectBadRequest(Request, NonInviteWithReplacesHeader);
    uarNoSuchCall:
      Self.ReturnResponse(Request, SIPCallLegOrTransactionDoesNotExist);
    uarUnsupportedAccept:
      Self.RejectRequestUnknownAccept(Request);
    uarUnsupportedContentEncoding:
      Self.RejectRequestUnknownContentEncoding(Request);
    uarUnsupportedContentLanguage:
      Self.RejectRequestUnknownContentLanguage(Request);
    uarUnsupportedContentType:
      Self.RejectRequestUnknownContentType(Request);
    uarUnsupportedExtension:
      Self.RejectRequestUnsupportedExtension(Request);
    uarUnsupportedScheme:
      Self.ReturnResponse(Request, SIPUnsupportedURIScheme);
    uarUnSupportedSipVersion:
      Self.UserAgent.RejectUnsupportedSipVersion(Request);
  else
    // What do we do here? We've rejected the request for a good reason, but have
    // forgotten to implement the bit where we send a reasonable response.
    raise Exception.Create(Self.ClassName
                         + '.RejectRequest: Can''t handle a reaction '
                         + ReactionToStr(Reaction));
  end;
end;

procedure TIdSipMessageModule.ReturnResponse(Request: TIdSipRequest;
                                             Reason: Cardinal);
begin
  Self.UserAgent.ReturnResponse(Request, Reason);
end;

function TIdSipMessageModule.WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  Result := uarAccept;

  if (Request.SIPVersion <> SipVersion) then
    Result := uarUnsupportedSipVersion
  else if not Self.UserAgent.IsMethodAllowed(Request.RequestUri, Request.Method) then
    Result := uarMethodNotAllowed
  // inspect the headers - 8.2.2
  // To & Request-URI - 8.2.2.1
  else if not Self.UserAgent.IsSchemeAllowed(Request.RequestUri.Scheme) then
    Result := uarUnsupportedScheme
  // Content processing - 8.2.3
  // Does the Accept not contain ANY known MIME type?
  else if not Self.HasKnownAccept(Request) then
    Result := uarUnsupportedAccept
  else if not Self.HasKnownAccept(Request) then
    Result := uarUnsupportedAccept
  else if Self.UserAgent.HasUnknownContentEncoding(Request) then
    Result := uarUnsupportedContentEncoding
  else if Self.UserAgent.HasUnknownContentLanguage(Request) then
    Result := uarUnsupportedContentLanguage
  else if Self.HasUnknownContentType(Request) then
    Result := uarUnsupportedContentType
  else if not Request.IsInvite and Request.HasReplaces then
    Result := uarNonInviteWithReplaces;
end;

//* TIdSipMessageModule Private methods ****************************************

function TIdSipMessageModule.ConvertToHeader(ValueList: TStrings): String;
begin
  Result := StringReplace(ValueList.CommaText, ',', ', ', [rfReplaceAll]);
end;

function TIdSipMessageModule.CreateListWithoutDuplicates(CaseSensitive: Boolean): TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := CaseSensitive;
  Result.Duplicates    := dupIgnore;
  Result.Sorted        := true;
end;

procedure TIdSipMessageModule.RejectRequestUnknownAccept(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.UserAgent.CreateResponse(Request, SIPNotAcceptableClient);
  try
    Response.AddHeader(AcceptHeader).Value := Self.ConvertToHeader(Self.AllowedContentTypes);

    Self.UserAgent.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipMessageModule.RejectRequestUnknownContentEncoding(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.UserAgent.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptEncodingHeader).Value := '';

    Self.UserAgent.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipMessageModule.RejectRequestUnknownContentLanguage(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  // It seems a stretch to say that an unsupported language would fall under
  //"unsupported media type, but the RFC says so (RFC 3261, cf section 8.2.3)
  Response := Self.UserAgent.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptLanguageHeader).Value := Self.UserAgent.AllowedLanguages;

    Self.UserAgent.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipMessageModule.RejectRequestUnknownContentType(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.UserAgent.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.Accept.Value := Self.ConvertToHeader(Self.AllowedContentTypes);

    Self.UserAgent.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipMessageModule.RejectRequestUnsupportedExtension(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.UserAgent.CreateResponse(Request, SIPBadExtension);
  try
    Response.AddHeader(UnsupportedHeader).Value := Request.FirstHeader(RequireHeader).Value;

    Self.UserAgent.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

//******************************************************************************
//* TIdSipNullModule                                                           *
//******************************************************************************
//* TIdSipNullModule Public methods ********************************************

function TIdSipNullModule.IsNull: Boolean;
begin
  Result := true;
end;

function TIdSipNullModule.WillAccept(Request: TIdSipRequest): Boolean;
begin
  Result := true;
end;

//* TIdSipNullModule Public methods ********************************************

function TIdSipNullModule.WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  Result := uarNoSuchCall;
end;

//******************************************************************************
//* TIdSipAction                                                               *
//******************************************************************************
//* TIdSipAction Public methods ************************************************

constructor TIdSipAction.Create(UA: TIdSipAbstractCore);
begin
  inherited Create;

  Self.Initialise(UA, nil, nil);
end;

constructor TIdSipAction.CreateInbound(UA: TIdSipAbstractCore;
                                       Request: TIdSipRequest;
                                       Binding: TIdConnectionBindings);
begin
  inherited Create;

  Self.Initialise(UA, Request, Binding);
  Self.ReceiveRequest(Request, Binding);
end;

destructor TIdSipAction.Destroy;
begin
  Self.TargetLocations.Free;
  Self.LocalParty.Free;
  Self.LocalGruu.Free;
  Self.InitialRequest.Free;
  Self.AttemptedLocations.Free;
  Self.AddressSpaceLocations.Free;
  Self.ActionListeners.Free;

  inherited Destroy;
end;

procedure TIdSipAction.AddActionListener(Listener: IIdSipActionListener);
begin
  Self.ActionListeners.AddListener(Listener);
end;

function TIdSipAction.IntrospectionCountKey: String;
const
  Prefix = 'TIdSip';
begin
  // Return the keyname for the current count of active actions of this type.

  Result := Self.ClassName;

  // Strip the prefix, if it's there. It was a mistake on my (FrankShearar's)
  // part to put the stack in the Indy namespace.
  if (Copy(Result, 1, Length(Prefix)) = Prefix) then
    Result := Copy(Result, Length(Prefix) + 1, Length(Result));

  Result := InNamespace(IntrospecTransactionUserNamespace, Result + 'Count');
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

function TIdSipAction.IsOutbound: Boolean;
begin
  Result := not Self.IsInbound;
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

function TIdSipAction.Method: String;
begin
  RaiseAbstractError(Self.ClassName, 'Method');
end;

procedure TIdSipAction.NetworkFailureSending(Msg: TIdSipMessage);
var
  FailReason: String;
  NewAttempt: TIdSipRequest;
  Request:    TIdSipRequest;
begin
  // You tried to send a request. It failed. The UA core invokes this method to
  // try the next possible location.
  //
  // If all possible locations have failed, it's time to try find another
  // address space that could apply. See the comment in SendRequest.

  if Msg.IsRequest then begin
    Request := Msg as TIdSipRequest;
    if Request.IsAck then begin
      FailReason := Format(RSNoLocationSucceeded, [Request.DestinationUri]);
      Self.NotifyOfNetworkFailure(NoLocationSucceeded,
                                  Format(OutboundActionFailed,
                                         [Self.Method, FailReason]));
    end
    else begin
      if not Self.AddressSpaceLocations.IsEmpty then begin
        NewAttempt := Self.CreateNewAttempt;
        try
          Self.TrySendRequest(NewAttempt, Self.AddressSpaceLocations);
        finally
          NewAttempt.Free;
        end;
      end
      else
        Self.TrySendRequestAgain(Request, Self.TargetLocations);
    end;
  end;
end;

function TIdSipAction.OutOfDialog: Boolean;
begin
  // Return true if and only if this is an outbound action for an out-of-dialog
  // request. (We can spot out-of-dialog requests by the lack of a To tag.)
  Result := not Self.IsInbound and not Self.InitialRequest.ToHeader.HasTag;
end;

procedure TIdSipAction.ReceiveRequest(Request: TIdSipRequest;
                                      Binding: TIdConnectionBindings);
begin
  Self.ReceiveOtherRequest(Request, Binding);
end;

procedure TIdSipAction.ReceiveResponse(Response: TIdSipResponse;
                                       Binding: TIdConnectionBindings);
var
  Succeeded: TIdSipActionResult;
begin
  // Accept one final response only
  if (Self.State = asFinished) then Exit;

  // Each of the ReceiveXXXResponse functions returns true if we succeeded
  // in our Action, or we could re-issue the request. They only return
  // false when the action failed irrecoverably.

  case Response.StatusCode div 100 of
    SIPProvisionalResponseClass:
      Succeeded := Self.ReceiveProvisionalResponse(Response,
                                                   Binding);
    SIPOKResponseClass:
      Succeeded := Self.ReceiveOKResponse(Response,
                                          Binding);
    SIPRedirectionResponseClass:
      Succeeded := Self.ReceiveRedirectionResponse(Response,
                                                   Binding);
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

procedure TIdSipAction.RemoveActionListener(Listener: IIdSipActionListener);
begin
  Self.ActionListeners.RemoveListener(Listener);
end;

procedure TIdSipAction.Resend(AuthorizationCredentials: TIdSipAuthorizationHeader);
var
  AuthedRequest: TIdSipRequest;
begin
  Self.SetStateToResent;

  AuthedRequest := Self.CreateResend(AuthorizationCredentials);
  try
    Self.SendRequest(AuthedRequest);
  finally
    AuthedRequest.Free;
  end;
end;

procedure TIdSipAction.Send;
var
  Attempt: TIdSipRequest;
begin
  Self.SetStateToSent;

  Attempt := Self.CreateNewAttempt;
  try
    Self.SendRequest(Attempt);
  finally
    Attempt.Free;
  end;
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

function TIdSipAction.CreateNewAttempt: TIdSipRequest;
begin
  Result := nil;
  RaiseAbstractError(Self.ClassName, 'CreateNewAttempt');
end;

procedure TIdSipAction.Initialise(UA: TIdSipAbstractCore;
                                  Request: TIdSipRequest;
                                  Binding: TIdConnectionBindings);
begin
  Self.fUA := UA;

  Self.ActionListeners       := TIdNotificationList.Create;
  Self.AddressSpaceLocations := TIdSipLocations.Create;
  Self.AttemptedLocations    := TIdSipLocations.Create;
  Self.fLocalParty           := TIdSipAddressHeader.Create;
  Self.fInitialRequest       := TIdSipRequest.Create;
  Self.fIsOwned              := false;
  Self.fIsTerminated         := false;
  Self.fLocalGruu            := TIdSipContactHeader.Create;
  Self.fMaxForwards          := TIdSipRequest.DefaultMaxForwards;
  Self.NonceCount            := 0;
  Self.State                 := asInitialised;
  Self.TargetLocations       := TIdSipLocations.Create;

  // It's good practice to keep LocalGruu containing a valid URI, even though
  // the Contact will be altered just before it hits the network. We signal our
  // ignorance of the correct address/port by setting LocalGruu.IsUnset.
  Self.LocalGruu.Value   := 'sip:127.0.0.1';
  Self.LocalGruu.IsUnset := true;

  Self.UseGruu := UA.UseGruu;

  Self.SetResult(arUnknown);

  if Self.IsInbound then begin
    Self.InitialRequest.Assign(Request);
    Self.LocalParty := Request.ToHeader
  end
  else begin
    Self.LocalParty.Value := Self.LocalGruu.FullValue;
  end;
end;

procedure TIdSipAction.MarkAsTerminated;
begin
  Self.fIsTerminated := true;

  Self.NotifyOfTermination;
end;

procedure TIdSipAction.NotifyOfAuthenticationChallenge(Challenge: TIdSipResponse);
var
  Notification: TIdSipActionAuthenticationChallengeMethod;
begin
  Notification := TIdSipActionAuthenticationChallengeMethod.Create;
  try
    Notification.ActionAgent := Self;
    Notification.Challenge   := Challenge;

    Self.ActionListeners.Notify(Notification);
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

    Self.ActionListeners.Notify(Notification);
  finally
    Notification.Free;
  end;

  Self.MarkAsTerminated;
end;

procedure TIdSipAction.NotifyOfTermination;
var
  Notification: TIdSipActionTerminatedMethod;
begin
  Notification := TIdSipActionTerminatedMethod.Create;
  try
    Notification.ActionAgent := Self;

    Self.ActionListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

function TIdSipAction.ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionResult;
begin
  case Response.StatusCode of
    SIPUnauthorized,
    SIPProxyAuthenticationRequired: begin
      Self.NotifyOfAuthenticationChallenge(Response);
      Result := arInterim;
    end;
  else begin
    Result := arFailure;
    Self.SetStateToFinished;
  end;
  end;
end;

function TIdSipAction.ReceiveGlobalFailureResponse(Response: TIdSipResponse): TIdSipActionResult;
begin
  Result := arFailure;
  Self.SetStateToFinished;
end;

function TIdSipAction.ReceiveOKResponse(Response: TIdSipResponse;
                                        Binding: TIdConnectionBindings): TIdSipActionResult;
begin
  Result := arSuccess;
  Self.SetStateToFinished;
end;

procedure TIdSipAction.ReceiveOtherRequest(Request: TIdSipRequest;
                                           Binding: TIdConnectionBindings);
begin
end;

function TIdSipAction.ReceiveProvisionalResponse(Response: TIdSipResponse;
                                                 Binding: TIdConnectionBindings): TIdSipActionResult;
begin
  Result := arFailure;
end;

function TIdSipAction.ReceiveRedirectionResponse(Response: TIdSipResponse;
                                                 Binding: TIdConnectionBindings): TIdSipActionResult;
begin
  Result := arFailure;
  Self.SetStateToFinished;
end;

function TIdSipAction.ReceiveServerFailureResponse(Response: TIdSipResponse): TIdSipActionResult;
begin
  Result := arFailure;
  Self.SetStateToFinished;
end;

procedure TIdSipAction.SendRequest(Request: TIdSipRequest);
var
  FailReason: String;
begin
  if (Self.NonceCount = 0) then
    Inc(Self.NonceCount);

  Self.AttemptedLocations.Clear;

  // We have a two-step process. First, we apply RFC 3263 to the request. Since
  // the request has no Route headers (yet), we resolve the Request-URI for its
  // target locations. Then, for each location, we
  //   (*) add any necessary Route headers (as determined by the address spaces
  //       in Self.UA.ProxyDescriptions);
  //   (*) apply RFC 3263 to the request;
  //   (*) try contact any location we haven't already tried.
  // Should all locations for the Route-enhanced request fail, we try the next
  // location for the NON-Route-enhanced request (i.e., the original request).
  // Only if ALL locations fail for ALL Route-enhanced requests, do we give up.

  // cf RFC 3263, section 4.3
  Self.UA.FindServersFor(Request, Self.TargetLocations);

  if Self.TargetLocations.IsEmpty then begin
    // The Locator should at the least return a location based on the
    // Request-URI. Thus this clause should never execute. Still, this
    // clause protects the code that follows.

    // Synchronise our state to what actually went down to the network.
    // The condition means that an INVITE won't set its InitialRequest to a
    // CANCEL or BYE it's just sent. Perhaps we could eliminate this condition
    // by using TIdSipOutboundBye/Cancel objects. TODO.
    if not Request.IsAck then
      Self.InitialRequest.Assign(Request);

    FailReason := Format(RSNoLocationFound, [Request.DestinationUri]);
    Self.NotifyOfNetworkFailure(NoLocationFound,
                                Format(OutboundActionFailed,
                                       [Self.Method, FailReason]));
  end
  else begin
    Self.TryAnotherRoutePath(Request, Self.TargetLocations);
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

procedure TIdSipAction.SetContactUri(Request: TIdSipRequest; Target: TIdSipLocation);
begin
  // Some Actions use Contacts, and want those Contacts to indicate, say, a
  // preferred transport that they'd like other UAs to use when sending them
  // requests.
  //
  // By default, do nothing.
end;

procedure TIdSipAction.SetResult(Value: TIdSipActionResult);
begin
  Self.fResult := Value;
end;

procedure TIdSipAction.SetStateToFinished;
begin
  Self.State := asFinished;
end;

procedure TIdSipAction.SetStateToResent;
begin
  if (Self.State = asInitialised) then
    raise EIdSipTransactionUser.Create('You cannot REsend if you didn''t send'
                                     + ' in the first place');

  Self.State := asResent;
end;

procedure TIdSipAction.SetStateToSent;
begin
  if (Self.State <> asInitialised) then
    raise EIdSipTransactionUser.Create(Format(MethodInProgress, [Self.Method]));

  Self.State := asSent;
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

  Result.RemoveAllAuthorizationsFor(AuthorizationCredentials.Realm);

  Result.AddHeader(AuthorizationCredentials);
end;

function TIdSipAction.GetUseGruu: Boolean;
begin
  Result := Self.LocalGruu.IsGruu;
end;

function TIdSipAction.GetUsername: String;
begin
  Result := Self.LocalGruu.DisplayName;
end;

procedure TIdSipAction.SetLocalGruu(Value: TIdSipContactHeader);
begin
  Self.fLocalGruu.Assign(Value);
end;

procedure TIdSipAction.SetLocalParty(Value: TIdSipAddressHeader);
begin
  Self.fLocalParty.Assign(Value);
end;

procedure TIdSipAction.SetUseGruu(Value: Boolean);
begin
  Self.LocalGruu.IsGruu := Value;
end;

procedure TIdSipAction.SetUsername(Value: String);
begin
  Self.LocalGruu.DisplayName := Value;
end;

procedure TIdSipAction.TryAnotherRoutePath(Request: TIdSipRequest;
                                           Targets: TIdSipLocations);
begin
  // A CANCEL always has the same Route headers as the request it's cancelling.
  // An ACK is a special case. It's constructed much like an in-dialog, when sent
  // in response to a 200 OK. See RFC 3261, sections 13.2.2.4 and 17.1.1.3.
  if not Request.IsCancel then begin
    if Self.OutOfDialog and not Request.IsAck then begin
      Request.Route.Clear;
      Request.AddHeaders(Self.UA.RoutePathFor(Targets.First));
    end;
  end;
  Targets.RemoveFirst;

  Self.UA.FindServersFor(Request, Self.AddressSpaceLocations);
  Self.TrySendRequest(Request, Self.AddressSpaceLocations);
end;

procedure TIdSipAction.TrySendRequest(Request: TIdSipRequest;
                                      Targets: TIdSipLocations);
var
  CurrentTarget: TIdSipLocation;
  LocalAddress:  TIdSipLocation;
  LocalBindings: TIdSipLocations;
begin
  // We remove the current target from the set of targets first to prevent any
  // damage from bugs lower in the stack: should something cause the message
  // sending to try the next location, we must ensure that we don't retry
  // sending the message to an already-attempted location.

  // Don't try to contact any already-attempted locations!
  while (not Targets.IsEmpty) and Self.AttemptedLocations.Contains(Targets.First) do
    Targets.RemoveFirst;

  // It's possible that we've already tried ALL locations originally listed in
  // Targets. If so, there's nothing more to be done!
  if Targets.IsEmpty then begin
    Self.NetworkFailureSending(Request);
    Exit;
  end;

  CurrentTarget := Targets.First.Copy;
  try
    Self.AttemptedLocations.AddLocation(CurrentTarget);
    Targets.Remove(CurrentTarget);

    LocalBindings := TIdSipLocations.Create;
    try
      Self.UA.Dispatcher.LocalBindings(LocalBindings);

      // This means that a message that travels to the Target using SCTP will have
      // SIP/2.0/SCTP in its topmost Via. Remember, we try to avoid having the
      // transport layer change the message.
      Request.LastHop.Transport := CurrentTarget.Transport;

      LocalAddress := TIdSipLocation.Create;
      try
        Self.UA.RoutingTable.BestLocalAddress(LocalBindings, CurrentTarget, LocalAddress);
        Request.RewriteLocationHeaders(LocalAddress);

        Request.LastHop.IsUnset := LocalBindings.Contains(LocalAddress);
      finally
        LocalAddress.Free;
      end;

      Self.SetContactUri(Request, CurrentTarget);

      // Synchronise our state to what actually went down to the network.
      // The condition means that an INVITE won't set its InitialRequest to an
      // ACK it's just sent.
      if not Request.IsAck then
        Self.InitialRequest.Assign(Request);

      Self.UA.SendRequest(Request, CurrentTarget);
    finally
      LocalBindings.Free;
    end;
  finally
    CurrentTarget.Free;
  end;
end;

procedure TIdSipAction.TrySendRequestAgain(Request: TIdSipRequest;
                                           Targets: TIdSipLocations);
var
  FailReason: String;
begin
  // All the locations for this Route-enhanced request have failed. It's
  // time to try the next address space, if we have one. Targets contains
  // the locations we will look up in the UA's proxy descriptions.

  if Targets.IsEmpty then begin
    FailReason := Format(RSNoLocationSucceeded, [Request.DestinationUri]);
    Self.NotifyOfNetworkFailure(NoLocationSucceeded,
                                Format(OutboundActionFailed,
                                       [Self.Method, FailReason]));
  end
  else
    Self.TryAnotherRoutePath(Request, Targets);
end;

//******************************************************************************
//* TIdSipOwnedAction                                                          *
//******************************************************************************
//* TIdSipOwnedAction Public methods *******************************************

destructor TIdSipOwnedAction.Destroy;
begin
  Self.OwnedActionListeners.Free;

  inherited Destroy;
end;

procedure TIdSipOwnedAction.AddOwnedActionListener(Listener: IIdSipOwnedActionListener);
begin
  Self.OwnedActionListeners.AddListener(Listener);
end;

procedure TIdSipOwnedAction.Cancel;
begin
  // You can't cancel most actions: as of now (2006/01/30) you can only cancel
  // INVITE transactions. Thus, by default, we do nothing.
end;

procedure TIdSipOwnedAction.RemoveOwnedActionListener(Listener: IIdSipOwnedActionListener);
begin
  Self.OwnedActionListeners.RemoveListener(Listener);
end;

//* TIdSipOwnedAction Protected methods ****************************************

procedure TIdSipOwnedAction.ActionSucceeded(Response: TIdSipResponse);
begin
  Self.NotifyOfSuccess(Response);
end;

procedure TIdSipOwnedAction.Initialise(UA: TIdSipAbstractCore;
                                       Request: TIdSipRequest;
                                       Binding: TIdConnectionBindings);
begin
  inherited Initialise(UA, Request, Binding);

  Self.fIsOwned := true;

  Self.OwnedActionListeners := TIdNotificationList.Create;
end;

procedure TIdSipOwnedAction.NotifyOfFailure(Response: TIdSipResponse);
var
  Notification: TIdSipOwnedActionFailureMethod;
begin
  Notification := TIdSipOwnedActionFailureMethod.Create;
  try
    Notification.ActionAgent := Self;
    Notification.Reason      := Response.Description;
    Notification.Response    := Response;

    Self.OwnedActionListeners.Notify(Notification);
  finally
    Notification.Free;
  end;

  Self.MarkAsTerminated;
end;

procedure TIdSipOwnedAction.NotifyOfRedirect(Response: TIdSipResponse);
var
  Notification: TIdSipOwnedActionRedirectMethod;
begin
  Notification := TIdSipOwnedActionRedirectMethod.Create;
  try
    Notification.ActionAgent := Self;
    Notification.Response    := Response;

    Self.OwnedActionListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipOwnedAction.NotifyOfSuccess(Msg: TIdSipMessage);
var
  Notification: TIdSipOwnedActionSuccessMethod;
begin
  Notification := TIdSipOwnedActionSuccessMethod.Create;
  try
    Notification.ActionAgent := Self;
    Notification.Msg         := Msg;

    Self.OwnedActionListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

function TIdSipOwnedAction.ReceiveRedirectionResponse(Response: TIdSipResponse;
                                                      Binding: TIdConnectionBindings): TIdSipActionResult;
begin
  Result := inherited ReceiveRedirectionResponse(Response, Binding);

  Self.NotifyOfRedirect(Response);
end;

//******************************************************************************
//* TIdSipRedirectedAction                                                     *
//******************************************************************************
//* TIdSipRedirectedAction Public methods **************************************

destructor TIdSipRedirectedAction.Destroy;
begin
  Self.fOriginalRequest.Free;
  Self.fContact.Free;

  inherited Destroy;
end;

function TIdSipRedirectedAction.Method: String;
begin
  Result := Self.fMethod;
end;

procedure TIdSipRedirectedAction.SetMethod(const Method: String);
begin
  Self.fMethod := Method;
end;

//* TIdSipRedirectedAction Protected methods ***************************

function TIdSipRedirectedAction.CreateNewAttempt: TIdSipRequest;
begin
  // Use this method in the context of a redirect to a Action.
  // cf. RFC 3261, section 8.1.3.4

  Result := Self.UA.CreateRedirectedRequest(Self.OriginalRequest,
                                            Self.Contact);
end;

procedure TIdSipRedirectedAction.Initialise(UA: TIdSipAbstractCore;
                                            Request: TIdSipRequest;
                                            Binding: TIdConnectionBindings);
begin
  inherited Initialise(UA, Request, Binding);

  Self.fContact         := TIdSipAddressHeader.Create;
  Self.fOriginalRequest := TIdSipRequest.Create;
end;

//* TIdSipRedirectedAction Private methods *************************************

procedure TIdSipRedirectedAction.SetContact(Value: TIdSipAddressHeader);
begin
  Self.fContact.Assign(Value);
end;

procedure TIdSipRedirectedAction.SetOriginalRequest(Value: TIdSipRequest);
begin
  Self.OriginalRequest.Assign(Value);
end;

//******************************************************************************
//* TIdSipOwningAction                                                         *
//******************************************************************************
//* TIdSipOwningAction Public methods ******************************************

destructor TIdSipOwningAction.Destroy;
var
  I: Integer;
begin
  for I := 0 to Self.ListeningTo.Count - 1 do
    (Self.ListeningTo[I] as TIdSipAction).RemoveActionListener(Self);

  Self.ListeningTo.Free;

  inherited Destroy;
end;

function TIdSipOwningAction.CreateInitialAction: TIdSipOwnedAction;
begin
  raise Exception.Create(Self.ClassName
                       + ' must override TIdSipOwningAction.CreateInitialAction');
end;

function TIdSipOwningAction.CreateRedirectedAction(OriginalRequest: TIdSipRequest;
                                                   Contact: TIdSipContactHeader): TIdSipOwnedAction;
var
  Redir: TIdSipRedirectedAction;
begin
  Redir := Self.UA.AddOutboundAction(TIdSipRedirectedAction) as TIdSipRedirectedAction;
  Redir.Contact         := Contact;
  Redir.MaxForwards     := Self.MaxForwards;
  Redir.OriginalRequest := OriginalRequest;
  Redir.SetMethod(Self.Method);

  Result := Redir;
end;

//* TIdSipOwningAction Protected methods ***************************************

procedure TIdSipOwningAction.AddSelfAsListenerTo(Action: TIdSipAction);
begin
  Action.AddActionListener(Self);
  
  if Action.IsOwned then
  (Action as TIdSipOwnedAction).AddOwnedActionListener(Self);

  Self.ListeningTo.Add(Action);
end;

procedure TIdSipOwningAction.Initialise(UA: TIdSipAbstractCore;
                                        Request: TIdSipRequest;
                                        Binding: TIdConnectionBindings);
begin
  inherited Initialise(UA, Request, Binding);

  Self.ListeningTo := TObjectList.Create(false);
end;

procedure TIdSipOwningAction.OnAuthenticationChallenge(Action: TIdSipAction;
                                                       Response: TIdSipResponse);
begin
  // By default, do nothing.
end;

procedure TIdSipOwningAction.OnFailure(Action: TIdSipAction;
                                       Response: TIdSipResponse;
                                       const Reason: String);
begin
  // By default, do nothing.
end;

procedure TIdSipOwningAction.OnFailure(Redirector: TIdSipActionRedirector;
                                       Response: TIdSipResponse);
begin
  // By default, do nothing.
end;

procedure TIdSipOwningAction.OnNetworkFailure(Action: TIdSipAction;
                                              ErrorCode: Cardinal;
                                              const Reason: String);
begin
  // By default, do nothing.
end;

procedure TIdSipOwningAction.OnNewAction(Redirector: TIdSipActionRedirector;
                                         NewAction: TIdSipAction);
begin
  Self.AddSelfAsListenerTo(NewAction);
end;

procedure TIdSipOwningAction.OnRedirect(Action: TIdSipAction;
                                        Redirect: TIdSipResponse);
begin
  // By default, do nothing.
end;

procedure TIdSipOwningAction.OnRedirectFailure(Redirector: TIdSipActionRedirector;
                                               ErrorCode: Cardinal;
                                               const Reason: String);
begin
  // By default, do nothing.
end;

procedure TIdSipOwningAction.OnSuccess(Action: TIdSipAction;
                                       Msg: TIdSipMessage);
begin
  // By default, do nothing.
end;

procedure TIdSipOwningAction.OnSuccess(Redirector: TIdSipActionRedirector;
                                       SuccessfulAction: TIdSipAction;
                                       Response: TIdSipResponse);
begin
  // By default, do nothing.
end;

procedure TIdSipOwningAction.OnTerminated(Action: TIdSipAction);
begin
  Self.RemoveSelfAsListenerFrom(Action);
end;

procedure TIdSipOwningAction.RemoveSelfAsListenerFrom(Action: TIdSipAction);
begin
  Self.ListeningTo.Remove(Action);
  Action.RemoveActionListener(Self);

  if Action.IsOwned then
    (Action as TIdSipOwnedAction).RemoveOwnedActionListener(Self);
end;

//******************************************************************************
//* TIdSipActionRedirector                                                     *
//******************************************************************************
//* TIdSipActionRedirector Public methods **************************************

constructor TIdSipActionRedirector.Create(OwningAction: TIdSipOwningAction);
begin
  inherited Create;

  Self.OwningAction := OwningAction;
  Self.UA           := OwningAction.UA;

  Self.Listeners := TIdNotificationList.Create;

  // The UA manages the lifetimes of all outbound INVITEs!
  Self.RedirectedActions    := TObjectList.Create(false);
  Self.TargetUriSet := TIdSipContacts.Create;
end;

destructor TIdSipActionRedirector.Destroy;
begin
  Self.TargetUriSet.Free;

  Self.RedirectedActions.Free;

  Self.Listeners.Free;

  inherited Destroy;
end;

procedure TIdSipActionRedirector.AddListener(const Listener: IIdSipActionRedirectorListener);
begin
  Self.Listeners.AddListener(Listener);
end;

procedure TIdSipActionRedirector.Cancel;
begin
  if Self.FullyEstablished then Exit;
  if Self.Cancelling then Exit;

  if Assigned(Self.InitialAction) then
    Self.InitialAction.Terminate;

  Self.TerminateAllRedirects;

  Self.Cancelling := true;
end;

function TIdSipActionRedirector.Contains(OwnedAction: TIdSipAction): Boolean;
begin
  Result := (Self.InitialAction = OwnedAction)
         or (Self.RedirectedActions.IndexOf(OwnedAction) <> ItemNotFoundIndex);
end;

procedure TIdSipActionRedirector.RemoveListener(const Listener: IIdSipActionRedirectorListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

procedure TIdSipActionRedirector.Resend(ChallengedAction: TIdSipAction;
                                        AuthorizationCredentials: TIdSipAuthorizationHeader);
begin
  ChallengedAction.Resend(AuthorizationCredentials);
end;

procedure TIdSipActionRedirector.Send;
begin
  Self.fInitialAction := Self.OwningAction.CreateInitialAction;
  Self.InitialAction.AddOwnedActionListener(Self);

  Self.NotifyOfNewAction(Self.InitialAction);
  Self.InitialAction.Send;
end;

procedure TIdSipActionRedirector.Terminate;
begin
  if Assigned(Self.InitialAction) and (Self.InitialAction.Result = arInterim) then begin
    Self.InitialAction.Terminate;
  end
  else
    Self.Cancel;
end;

//* TIdSipActionRedirector Private methods *************************************

procedure TIdSipActionRedirector.AddNewRedirect(OriginalRequest: TIdSipRequest;
                                                Contact: TIdSipContactHeader);
var
  Redirect: TIdSipOwnedAction;
begin
  Redirect := Self.OwningAction.CreateRedirectedAction(OriginalRequest, Contact);

  Self.RedirectedActions.Add(Redirect);

  Redirect.AddOwnedActionListener(Self);
  Self.NotifyOfNewAction(Redirect);
  Redirect.Send;
end;

function TIdSipActionRedirector.HasOutstandingRedirects: Boolean;
begin
  Result := Self.RedirectedActions.Count <> 0;
end;

procedure TIdSipActionRedirector.NotifyOfFailure(ErrorCode: Cardinal;
                                                 const Reason: String);
var
  Notification: TIdSipRedirectorRedirectFailureMethod;
begin
  Notification := TIdSipRedirectorRedirectFailureMethod.Create;
  try
    Notification.ErrorCode  := ErrorCode;
    Notification.Reason     := Reason;
    Notification.Redirector := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipActionRedirector.NotifyOfNewAction(Action: TIdSipAction);
var
  Notification: TIdSipRedirectorNewActionMethod;
begin
  Notification := TIdSipRedirectorNewActionMethod.Create;
  try
    Notification.NewAction  := Action;
    Notification.Redirector := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipActionRedirector.NotifyOfSuccess(Action: TIdSipAction;
                                                 Response: TIdSipResponse);
var
  Notification: TIdSipRedirectorSuccessMethod;
begin
  Notification := TIdSipRedirectorSuccessMethod.Create;
  try
    Notification.Redirector       := Self;
    Notification.Response         := Response;
    Notification.SuccessfulAction := Action;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipActionRedirector.OnAuthenticationChallenge(Action: TIdSipAction;
                                                           Challenge: TIdSipResponse);
begin
  // Do nothing.
end;

procedure TIdSipActionRedirector.OnFailure(Action: TIdSipAction;
                                           Response: TIdSipResponse;
                                           const Reason: String);
begin
  if Response.IsRedirect then
    Self.RemoveFinishedRedirectedInvite(Action)
  else begin
    if (Action = Self.InitialAction) then begin
      Self.fInitialAction := nil;
      Self.NotifyOfFailure(Response.StatusCode,
                           Response.StatusText);
      Exit;
    end;

    Self.RemoveFinishedRedirectedInvite(Action);

    if not Self.HasOutstandingRedirects then begin
      Self.NotifyOfFailure(RedirectWithNoSuccess,
                           RSRedirectWithNoSuccess);
    end;
  end;
end;

procedure TIdSipActionRedirector.OnNetworkFailure(Action: TIdSipAction;
                                                  ErrorCode: Cardinal;
                                                  const Reason: String);
begin
//  Self.RemoveFinishedRedirectedInvite(Action);
end;

procedure TIdSipActionRedirector.OnRedirect(Action: TIdSipAction;
                                            Redirect: TIdSipResponse);
var
  NewTargetsAdded: Boolean;
begin
  // cf RFC 3261, section 8.1.3.4.

  if not Self.FullyEstablished then begin
    if Redirect.Contacts.IsEmpty then begin
      Self.NotifyOfFailure(RedirectWithNoContacts, RSRedirectWithNoContacts);
    end
    else begin
      // Of course, if we receive a 3xx then that INVITE's over.
      Self.RemoveFinishedRedirectedInvite(Action);

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
          Self.AddNewRedirect(Action.InitialRequest,
                              Redirect.Contacts.CurrentContact);
          NewTargetsAdded := true;
        end;
        Redirect.Contacts.Next;
      end;

      Self.TargetUriSet.Add(Redirect.Contacts);

      if not NewTargetsAdded and not Self.HasOutstandingRedirects then
        Self.NotifyOfFailure(RedirectWithNoMoreTargets,
                             RSRedirectWithNoMoreTargets);
    end;
  end;

  Self.RemoveFinishedRedirectedInvite(Action);

  if (Action = Self.InitialAction) then
    Self.fInitialAction := nil;
end;

procedure TIdSipActionRedirector.OnSuccess(Action: TIdSipAction;
                                           Response: TIdSipMessage);
begin
  if not Self.FullyEstablished then begin
    Self.FullyEstablished := true;

    Self.RemoveFinishedRedirectedInvite(Action);
    Self.TerminateAllRedirects;
    Self.NotifyOfSuccess(Action, Response as TIdSipResponse);

    if (Action = Self.InitialAction) then
      Self.fInitialAction := nil;
  end;
end;

procedure TIdSipActionRedirector.OnTerminated(Action: TIdSipAction);
begin
  (Action as TIdSipOwnedAction).RemoveOwnedActionListener(Self);
end;

procedure TIdSipActionRedirector.RemoveFinishedRedirectedInvite(Agent: TIdSipAction);
begin
  Self.RedirectedActions.Remove(Agent);

  if Self.Cancelling and not Self.HasOutstandingRedirects then
    Self.NotifyOfFailure(NoError, '');
end;

procedure TIdSipActionRedirector.TerminateAllRedirects;
var
  I: Integer;
begin
  for I := 0 to Self.RedirectedActions.Count - 1 do
    (Self.RedirectedActions[I] as TIdSipAction).Terminate;
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
//* TIdSipActionTerminatedMethod                                               *
//******************************************************************************
//* TIdSipActionTerminatedMethod Public methods ********************************

procedure TIdSipActionTerminatedMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipActionListener).OnTerminated(Self.ActionAgent);
end;

//******************************************************************************
//* TIdSipOwnedActionFailureMethod                                             *
//******************************************************************************
//* TIdSipOwnedActionFailureMethod Public methods ******************************

procedure TIdSipOwnedActionFailureMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipOwnedActionListener).OnFailure(Self.ActionAgent,
                                                   Self.Response,
                                                   Self.Reason);
end;

//******************************************************************************
//* TIdSipOwnedActionRedirectMethod                                            *
//******************************************************************************
//* TIdSipOwnedActionRedirectMethod Public methods *****************************

procedure TIdSipOwnedActionRedirectMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipOwnedActionListener).OnRedirect(Self.ActionAgent,
                                                    Self.Response);
end;

//******************************************************************************
//* TIdSipOwnedActionSuccessMethod                                             *
//******************************************************************************
//* TIdSipOwnedActionSuccessMethod Public methods ******************************

procedure TIdSipOwnedActionSuccessMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipOwnedActionListener).OnSuccess(Self.ActionAgent,
                                                   Self.Msg);
end;

//******************************************************************************
//* TIdSipRedirectorNewActionMethod                                            *
//******************************************************************************
//* TIdSipRedirectorNewActionMethod Public methods *****************************

procedure TIdSipRedirectorNewActionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipActionRedirectorListener).OnNewAction(Self.Redirector,
                                                          Self.NewAction);
end;

//******************************************************************************
//* TIdSipRedirectorRedirectFailureMethod                                      *
//******************************************************************************
//* TIdSipRedirectorRedirectFailureMethod Public methods ***********************

procedure TIdSipRedirectorRedirectFailureMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipActionRedirectorListener).OnRedirectFailure(Self.Redirector,
                                                                Self.ErrorCode,
                                                                Self.Reason);
end;

//******************************************************************************
//* TIdSipRedirectorSuccessMethod                                              *
//******************************************************************************
//* TIdSipRedirectorSuccessMethod Public methods *******************************

procedure TIdSipRedirectorSuccessMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipActionRedirectorListener).OnSuccess(Self.Redirector,
                                                        Self.SuccessfulAction,
                                                        Self.Response);
end;

//******************************************************************************
//* TIdSipUserAgentAddActionMethod                                             *
//******************************************************************************
//* TIdSipUserAgentAddActionMethod Public methods ******************************

procedure TIdSipUserAgentAddActionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransactionUserListener).OnAddAction(Self.UserAgent,
                                                         Self.Action);
end;

//******************************************************************************
//* TIdSipUserAgentDroppedUnmatchedMessageMethod                               *
//******************************************************************************
//* TIdSipUserAgentDroppedUnmatchedMessageMethod Public methods ****************

procedure TIdSipUserAgentDroppedUnmatchedMessageMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransactionUserListener).OnDroppedUnmatchedMessage(Self.UserAgent,
                                                                       Self.Message,
                                                                       Self.Binding);
end;

//******************************************************************************
//* TIdSipUserAgentRemoveActionMethod                                          *
//******************************************************************************
//* TIdSipUserAgentRemoveActionMethod Public methods ***************************

procedure TIdSipUserAgentRemoveActionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransactionUserListener).OnRemoveAction(Self.UserAgent,
                                                            Self.Action);
end;

end.
