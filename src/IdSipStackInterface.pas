{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipStackInterface;

interface

uses
  Classes, Contnrs, IdConnectionBindings, IdInterfacedObject, IdNotification,
  IdRegisteredObject, IdSipCore, IdSipDialogID, IdSipDns, IdSipInviteModule,
  IdSipLocation, IdSipMessage, IdSipOptionsModule, IdSipRegistration,
  IdSipSubscribeModule, IdSipTransaction, IdSipTransport, IdSipUserAgent,
  IdTimerQueue, Messages, PluggableLogging, StringDictionary, SyncObjs,
  SysUtils, Windows;

type
  TIdSipHandle = Cardinal;

  TIdSipStackInterface = class;
  TIdEventData = class;
  IIdSipStackListener = interface
    ['{BBC8C7F4-4031-4258-93B3-8CA71C9F8733}']
    procedure OnEvent(Stack: TIdSipStackInterface;
                      Event: Cardinal;
                      Data: TIdEventData);
  end;

  TIdActionAssociation = class(TObject)
  private
    fAction: TIdSipAction;
    fHandle: TIdSipHandle;
  public
    constructor Create(Action: TIdSipAction;
                       Handle: TIdSipHandle);

    property Action: TIdSipAction read fAction;
    property Handle: TIdSipHandle read fHandle;
  end;

  TIdSipStackInterfaceEventMethod = class;
  TIdSipStackInterfaceExtension = class;
  TIdSipStackInterfaceExtensionClass = class of TIdSipStackInterfaceExtension;
  TIdStackReconfiguredData = class;
  TIdStackReconfiguredDataClass = class of TIdStackReconfiguredData;

  TIdAsynchronousMessageResultData = class;

  // I provide a high-level interface to a SIP stack.
  // On one hand, I make sure that messages are sent in the context of the
  // stack's thread (its Timer). On the other, I make sure that events from the
  // network (e.g., an inbound call) result in messages posted to Application's
  // message queue.
  //
  // You receive Handles to actions by calling methods with the prefix "Make".
  // You can perform actions using those Handles using the other methods. If you
  // call a method of mine with an invalid handle (a handle for an action that's
  // finished, a handle I never gave you) or try issue an inappropriate command
  // using an otherwise valid handle (calling AcceptCall on an outbound call,
  // for instance) I will raise an EInvalidHandle exception.
  //
  // My current implementation is Windows-specific. Ultimately, of course, we
  // want to be OS-agnostic (at least, as much as we can be).
  //
  // Find the details on what to put in the Configuration TStrings by reading
  // the class comment of TIdSipStackConfigurator.
  TIdSipStackInterface = class(TIdInterfacedObject,
                               IIdTimerQueueListener,
                               IIdSipActionListener,
                               IIdSipInviteModuleListener,
                               IIdSipMessageModuleListener,
                               IIdSipOptionsListener,
                               IIdSipRegistrationListener,
                               IIdSipSessionListener,
                               IIdSipSubscribeModuleListener,
                               IIdSipSubscriptionListener,
                               IIdSipTransactionUserListener,
                               IIdSipTransportListener,
                               IIdSipTransportSendingListener)
  private
    ActionLock:      TCriticalSection;
    Actions:         TObjectList;
    fUiHandle:       HWnd;
    fUserAgent:      TIdSipUserAgent;
    SubscribeModule: TIdSipSubscribeModule;
    TimerQueue:      TIdTimerQueue;
    FHasBeenFreed:   PBoolean;

    function  ActionFor(Handle: TIdSipHandle): TIdSipAction;
    function  AssociationAt(Index: Integer): TIdActionAssociation;
    procedure Configure(Configuration: TStrings);
    function  GetAndCheckAction(Handle: TIdSipHandle;
                                ExpectedType: TIdSipActionClass): TIdSipAction;
    function  HandleFor(Action: TIdSipAction): TIdSipHandle;
    function  IndexOf(H: TIdSipHandle): Integer;
    function  HasHandle(H: TIdSipHandle): Boolean;
    procedure ListenToAllTransports;
    function  NewHandle: TIdSipHandle;
    procedure NotifyOfReconfiguration(Configuration: TStrings);
    procedure NotifyOfSentMessage(Msg: TIdSipMessage;
                                  Binding: TIdConnectionBindings);
    procedure NotifyOfStackShutdown;
    procedure NotifyOfStackStartup;
    procedure NotifyReferral(ActionHandle: TIdSipHandle;
                             NotifyType: TIdSipInboundReferralWaitClass;
                             Response: TIdSipResponse);
    procedure NotifySubscriptionEvent(Event: Cardinal;
                                      Subscription: TIdSipSubscription;
                                      Notify: TIdSipRequest);
    procedure OnAddAction(UserAgent: TIdSipAbstractCore;
                          Action: TIdSipAction);
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse); overload;
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                        Message: TIdSipMessage;
                                        Binding: TIdConnectionBindings);
    procedure OnEndedSession(Session: TIdSipSession;
                             ErrorCode: Cardinal;
                             const Reason: String);
    procedure OnEstablishedSession(Session: TIdSipSession;
                                   const RemoteSessionDescription: String;
                                   const MimeType: String);
    procedure OnEstablishedSubscription(Subscription: TIdSipOutboundSubscription;
                                        Notify: TIdSipRequest);
    procedure OnException(FailedMessage: TIdSipMessage;
                          E: Exception;
                          const Reason: String); overload;
    procedure OnException(Timer: TIdTimerQueue;
                          Error: Exception;
                          Wait: TIdWait); overload;
    procedure OnExpiredSubscription(Subscription: TIdSipOutboundSubscription;
                                    Notify: TIdSipRequest);
    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistrationBase;
                        ErrorCode: Cardinal;
                        const Reason: String); overload;
    procedure OnFailure(Subscription: TIdSipOutboundSubscription;
                        Response: TIdSipResponse); overload;
    procedure OnInboundCall(UserAgent: TIdSipInviteModule;
                            Session: TIdSipInboundSession); overload;
    procedure OnModifySession(Session: TIdSipSession;
                              const RemoteSessionDescription: String;
                              const MimeType: String);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Answer: TIdSipResponse);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
    procedure OnNotify(Subscription: TIdSipOutboundSubscription;
                       Notify: TIdSipRequest);
    procedure OnProgressedSession(Session: TIdSipSession;
                                  Progress: TIdSipResponse);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport;
                               Source: TIdConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdConnectionBindings);
    procedure OnReferral(Session: TIdSipSession;
                         Refer: TIdSipRequest;
                         Binding: TIdConnectionBindings);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String;
                                Source: TIdConnectionBindings);
    procedure OnRemoveAction(UserAgent: TIdSipAbstractCore;
                             Action: TIdSipAction);
    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipOutboundSubscription);
    procedure OnResponse(OptionsAgent: TIdSipOutboundOptions;
                         Response: TIdSipResponse);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Destination: TIdConnectionBindings);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Destination: TIdConnectionBindings);
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipInboundSubscription);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistrationBase;
                        CurrentBindings: TIdSipContacts);
    procedure OnTerminated(Action: TIdSipAction);
    procedure RemoveAction(Handle: TIdSipHandle);
    procedure SendAction(Action: TIdSipAction);
    procedure StopListeningToAllTransports;
  protected
    function  AddAction(Action: TIdSipAction): TIdSipHandle;
    function  CreateReconfigureNotificationData(Configuration: TStrings): TIdStackReconfiguredData; virtual;
    procedure NotifyEvent(Event: Cardinal;
                          Data: TIdEventData);
    procedure ParseLine(Directive, Configuration: String); virtual;
    procedure PostConfigurationActions; virtual;
    procedure PreConfigurationActions; virtual;
    function  ReconfiguredNotificationDataType: TIdStackReconfiguredDataClass; virtual;

    property UiHandle:  HWnd            read fUiHandle;
    property UserAgent: TIdSipUserAgent read fUserAgent;
  public
    constructor Create(UiHandle: HWnd;
                       TimerQueue: TIdTimerQueue;
                       Configuration: TStrings); reintroduce; virtual;
    destructor  Destroy; override;

    procedure AcceptCallModify(ActionHandle: TIdSipHandle;
                               const LocalSessionDescription: String;
                               const ContentType: String);
    procedure AnswerCall(ActionHandle: TIdSipHandle;
                         const Offer: String;
                         const ContentType: String);
    function  AttachExtension(EType: TIdSipStackInterfaceExtensionClass): TIdSipStackInterfaceExtension;
    procedure Authenticate(ActionHandle: TIdSipHandle;
                           Credentials: TIdSipAuthorizationHeader); 
    function  GruuOf(ActionHandle: TIdSipHandle): String;
    function  HandleOf(const LocalGruu: String): TIdSipHandle;
    procedure HangUp(ActionHandle: TIdSipHandle);
    function  MakeCall(From: TIdSipAddressHeader;
                       Dest: TIdSipAddressHeader;
                       const LocalSessionDescription: String;
                       const MimeType: String;
                       MaxForwards: Cardinal): TIdSipHandle; virtual;
    function  MakeOptionsQuery(Dest: TIdSipAddressHeader): TIdSipHandle;
    function  MakeRefer(Target: TIdSipAddressHeader;
                        Resource: TIdSipAddressHeader): TIdSipHandle;
    function  MakeRegistration(Registrar: TIdSipUri): TIdSipHandle; overload;
    function  MakeRegistration(Registrar: TIdSipUri;
                               Contacts: TIdSipContacts): TIdSipHandle; overload;
    function  MakeSubscription(Target: TIdSipAddressHeader;
                               const EventPackage: String): TIdSipHandle;
    function  MakeTransfer(Transferee: TIdSipAddressHeader;
                           TransferTarget: TIdSipAddressHeader;
                           Call: TIdSipHandle): TIdSipHandle;
    procedure ModifyCall(ActionHandle: TIdSipHandle;
                         const Offer: String;
                         const ContentType: String);
    procedure NotifyOfAsyncMessageResult(Data: TIdAsynchronousMessageResultData);
    procedure NotifyReferralDenied(ActionHandle: TIdSipHandle);
    procedure NotifyReferralFailed(ActionHandle: TIdSipHandle;
                                   Response: TIdSipResponse = nil);
    procedure NotifyReferralSucceeded(ActionHandle: TIdSipHandle);
    procedure NotifyReferralTrying(ActionHandle: TIdSipHandle);
    procedure NotifySubcriber(ActionHandle: TIdSipHandle;
                              const Notification: String;
                              const MimeType: String);
    procedure ReconfigureStack(NewConfiguration: TStrings);
    procedure RedirectCall(ActionHandle: TIdSipHandle;
                           NewTarget: TIdSipAddressHeader;
                           Temporary: Boolean = true);
    procedure RejectCall(ActionHandle: TIdSipHandle;
                         StatusCode: Cardinal;
                         StatusText: String = '');
    procedure Resume;
    procedure Send(ActionHandle: TIdSipHandle);
    procedure SendAsyncCall(ReferenceID: String);
    procedure SendProvisional(ActionHandle: TIdSipHandle;
                              StatusCode: Cardinal = SIPSessionProgress;
                              Description: String = RSSIPSessionProgress);
    procedure Terminate; overload;
    procedure Terminate(ActionHandle: TIdSipHandle); overload;
    procedure Terminate(HasBeenFreed: PBoolean); overload;
  end;

  // My subclasses allow you to extend the TIdSipStackInterface's interface
  // without cluttering up the core interface. Note that I give unrestricted
  // access to the StackInterface's internal UserAgent. That means that you
  // don't usually want to create my subclasses outside of Wait objects, because
  // of concurrency issues.
  TIdSipStackInterfaceExtension = class(TObject)
  private
    fUserAgent: TIdSipUserAgent;
  protected
    property UserAgent: TIdSipUserAgent read fUserAgent;
  public
    constructor Create(UA: TIdSipUserAgent); virtual;
  end;

  // I allow access to the UserAgent's REGISTER module.
  TIdSipColocatedRegistrarExtension = class(TIdSipStackInterfaceExtension)
  private
    DB:             TIdSipAbstractBindingDatabase;
    RegisterModule: TIdSipRegisterModule;
  public
    constructor Create(UA: TIdSipUserAgent); override;

    procedure TargetsFor(URI: TIdSipUri; Targets: TIdSipContacts);
  end;

  // You may instantiate me in the context of any thread, because my Log method
  // is fully reentrant. (More accurately: it'd fully reentrant if the logging
  // callback in PluggableLogging, is fully reentrant.)
  TIdLoggingExtension = class(TIdSipStackInterfaceExtension)
  public
    procedure Log(Severity: TSeverityLevel;
                  SourceDescription: String;
                  RefCode: Cardinal;
                  Description,
                  BinaryData: String);
  end;

  // I allow access to the UserAgent's TIdSipLocator.
  TIdSipNameServerExtension = class(TIdSipStackInterfaceExtension)
  private
    procedure AssertAddressWellFormed(IPAddressOrHost: String);
    function  ResolveAddress(IPAddressOrHost: String): String;
  public
    function  LocalAddressFor(Destination: String): String;
    function  LocalOrMappedAddressFor(Destination: String): String;
    procedure ResolveNamesFor(Host: String; IPAddresses: TIdDomainNameRecords);
  end;

  // I allow access to various network data in the stack: what bindings the
  // stack uses, the best local address to use to contact a remote host, etc.
  TIdSipNetworkExtension = class(TIdSipStackInterfaceExtension)
  public
    procedure GetBindings(Bindings: TIdSipLocations);
  end;

  // I collect all "sysctl"-like settings, and various statistics
  // generated/collected by the stack.
  TIdSipStatisticsExtension = class(TIdSipStackInterfaceExtension)
  public
    procedure CollectTransactionStatistics(Keys: TStringDictionary);
    procedure CollectTransactionUserStatistics(Keys: TStringDictionary);
    procedure CollectTransportStatistics(Keys: TStringDictionary);
    procedure CollectAllStatistics(Keys: TStringDictionary);
  end;

  // I contain data relating to a particular event.
  TIdEventData = class(TPersistent)
  private
    fHandle: TIdSipHandle;

    function TimestampLine: String;
  protected
    function Data: String; virtual;
    function EventName: String; virtual;
  public
    constructor Create; virtual;

    procedure Assign(Src: TPersistent); override;
    function  AsString: String;
    function  Copy: TIdEventData; virtual;

    property Handle: TIdSipHandle read fHandle write fHandle;
  end;

  TIdEventDataClass = class of TIdEventData;

  // An ErrorCode of 0 means "no error".
  // Usually the ErrorCode will map to a SIP response Status-Code.
  TIdInformationalData = class(TIdEventData)
  private
    fErrorCode: Cardinal;
    fReason:    String;

    procedure SetErrorCode(Value: Cardinal);
  protected
    function Data: String; override;
  public
    constructor Create; override;

    procedure Assign(Src: TPersistent); override;
    function  NotAnError: Boolean;

    property ErrorCode: Cardinal read fErrorCode write SetErrorCode;
    property Reason:    String   read fReason write fReason;
  end;

  TIdAuthenticationChallengeData = class(TIdEventData)
  private
    fChallenge:         TIdSipResponse;
    fChallengedRequest: TIdSipRequest;

    procedure SetChallenge(Response: TIdSipResponse);
    procedure SetChallengedRequest(Request: TIdSipRequest);
  protected
    function Data: String; override;
    function EventName: String; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Challenge:         TIdSipResponse read fChallenge write SetChallenge;
    property ChallengedRequest: TIdSipRequest  read fChallengedRequest write SetChallengedRequest;
  end;

  TIdFailData = class(TIdInformationalData);

  TIdNetworkFailureData = class(TIdFailData)
  protected
    function EventName: String; override;
  end;

  // You might think that we should have a Response property to indicate why a
  // call ended (due to a failure). You'd be wrong. A call COULD end because
  // the UAS returned 486 Busy Here. A call could also end because we sent or
  // received a BYE, in which case a Response field would be misleading.
  TIdCallEndedData = class(TIdInformationalData)
  protected
    function EventName: String; override;
  end;

  TIdDebugData = class(TIdEventData)
  private
    fEvent: Cardinal;
  protected
    function EventName: String; override;
  public
    procedure Assign(Src: TPersistent); override;

    property Event: Cardinal read fEvent write fEvent;
  end;

  TIdDebugMessageData = class(TIdDebugData)
  private
    fMessage: TIdSipMessage;

  protected
    function Data: String; override;
  public
    destructor Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Message: TIdSipMessage read fMessage write fMessage;
  end;

  TIdDebugDroppedMessageData = class(TIdDebugMessageData)
  private
    fBinding: TIdConnectionBindings;

  protected
    function Data: String; override;
    function EventName: String; override;
  public
    destructor Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Binding: TIdConnectionBindings read fBinding write fBinding;
  end;

  TIdDebugReceiveMessageData = class(TIdDebugMessageData)
  private
    fBinding: TIdConnectionBindings;
  protected
    function Data: String; override;
    function EventName: String; override;
  public
    destructor Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Binding: TIdConnectionBindings read fBinding write fBinding;
  end;

  TIdDebugSendMessageData = class(TIdDebugMessageData)
  private
    fBinding: TIdConnectionBindings;
  protected
    function Data: String; override;
    function EventName: String; override;
  public
    destructor Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Binding: TIdConnectionBindings read fBinding write fBinding;
  end;

  TIdDebugExceptionData = class(TIdDebugData)
  private
    fError:  String;
    fReason: String;
  protected
    function Data: String; override;
    function EventName: String; override;
  public
    procedure Assign(Src: TPersistent); override;

    property Error:  String read fError write fError;
    property Reason: String read fReason write fReason;
  end;

  TIdDebugTransportExceptionData = class(TIdDebugExceptionData)
  protected
    function EventName: String; override;
  end;

  TIdDebugWaitExceptionData = class(TIdDebugExceptionData)
  private
    fWait: TIdWait;

    procedure SetWait(Value: TIdWait);
  protected
    function Data: String; override;
    function EventName: String; override;
  public
    destructor Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Wait: TIdWait read fWait write SetWait;
  end;

  TIdDebugTransportRejectedMessageData = class(TIdDebugData)
  private
    fBinding: TIdConnectionBindings;
    fMsg:     String;
    fReason:  String;
  protected
    function Data: String; override;
    function EventName: String; override;
  public
    destructor Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Binding: TIdConnectionBindings read fBinding write fBinding;
    property Msg:     String                   read fMsg write fMsg;
    property Reason:  String                   read fReason write fReason;
  end;

  TIdQueryOptionsData = class(TIdEventData)
  private
    fResponse: TIdSipResponse;

    procedure SetResponse(Value: TIdSipResponse);
  protected
    function Data: String; override;
    function EventName: String; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Response: TIdSipResponse read fResponse write SetResponse;
  end;

  TIdRegistrationData = class(TIdEventData)
  private
    fContacts: TIdSipContacts;

    procedure SetContacts(Value: TIdSipContacts);
  protected
    function Data: String; override;
    function EventName: String; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Contacts: TIdSipContacts read fContacts write SetContacts;
  end;

  TIdFailedRegistrationData = class(TIdFailData)
  private
    RegistrationData: TIdRegistrationData;

  protected
    function EventName: String; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;
  end;

  TIdSessionData = class(TIdEventData)
  private
    fLocalContact:             TIdSipContactHeader;
    fLocalMimeType:            String;
    fLocalParty:               TIdSipAddressHeader;
    fLocalSessionDescription:  String;
    fRemoteContact:            TIdSipContactHeader;
    fRemoteMimeType:           String;
    fRemoteParty:              TIdSipAddressHeader;
    fRemoteSessionDescription: String;

    procedure SetLocalContact(Value: TIdSipContactHeader);
    procedure SetLocalParty(Value: TIdSipAddressHeader);
    procedure SetRemoteContact(Value: TIdSipContactHeader);
    procedure SetRemoteParty(Value: TIdSipAddressHeader);
  protected
    function Data: String; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property LocalContact:             TIdSipContactHeader read fLocalContact write SetLocalContact;
    property LocalMimeType:            String              read fLocalMimeType write fLocalMimeType;
    property LocalParty:               TIdSipAddressHeader read fLocalParty write SetLocalParty;
    property LocalSessionDescription:  String              read fLocalSessionDescription write fLocalSessionDescription;
    property RemoteContact:            TIdSipContactHeader read fRemoteContact write SetRemoteContact;
    property RemoteMimeType:           String              read fRemoteMimeType write fRemoteMimeType;
    property RemoteParty:              TIdSipAddressHeader read fRemoteParty write SetRemoteParty;
    property RemoteSessionDescription: String              read fRemoteSessionDescription write fRemoteSessionDescription;
  end;

  TIdSessionDataClass = class of TIdSessionData;

  TIdEstablishedSessionData = class(TIdSessionData)
  private
    fID: TIdSipDialogID;

    procedure SetID(Value: TIdSipDialogID);
  protected
    function EventName: String; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property ID: TIdSipDialogID read fID write SetID;
  end;

  TIdInboundCallData = class(TIdSessionData)
  private
    fInvite: TIdSipRequest;

    procedure SetInvite(Value: TIdSipRequest);
  protected
    function EventName: String; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Invite: TIdSipRequest read fInvite write SetInvite;
  end;

  TIdModifiedSessionData = class(TIdSessionData)
  protected
    function EventName: String; override;
  end;

  TIdModifySessionData = class(TIdSessionData)
  protected
    function EventName: String; override;
  end;

  TIdSessionProgressData = class(TIdSessionData)
  private
    fBanner:       String;
    fProgressCode: Cardinal;
  protected
    function Data: String; override;
    function EventName: String; override;
  public
    procedure Assign(Src: TPersistent); override;

    property Banner:       String   read fBanner write fBanner;
    property ProgressCode: Cardinal read fProgressCode write fProgressCode;
  end;

  TIdSubscriptionData = class(TIdEventData)
  private
    fEventPackage: String;
  protected
    function Data: String; override;
    function  GetEventPackage: String; virtual;
    procedure SetEventPackage(Value: String); virtual;
  public
    procedure Assign(Src: TPersistent); override;

    property EventPackage: String read fEventPackage write fEventPackage;
  end;

  TIdSubscriptionRequestData = class(TIdSubscriptionData)
  private
    fRequest: TIdSipRequest;

    function  GetFrom: TIdSipFromHeader;
    function  GetReferTo: TIdSipReferToHeader;
    function  GetRemoteContact: TIdSipContactHeader;
    function  GetTarget: TIdSipUri;
    procedure SetRequest(Value: TIdSipRequest);
  protected
    function  Data: String; override;
    function  EventName: String; override;
    function  GetEventPackage: String; override;
    procedure SetEventPackage(Value: String); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property From:          TIdSipFromHeader    read GetFrom;
    property ReferTo:       TIdSipReferToHeader read GetReferTo;
    property Request:       TIdSipRequest       read fRequest write SetRequest;
    property RemoteContact: TIdSipContactHeader read GetRemoteContact;
    property Target:        TIdSipUri           read GetTarget;
  end;

  TIdResubscriptionData = class(TIdSubscriptionData)
  end;

  // ReferAction contains the handle of the TIdSipInboundReferral that the stack
  // has allocated to handling this request.
  TIdSessionReferralData = class(TIdSubscriptionRequestData)
  private
    fReferAction: TIdSipHandle;
  protected
    function Data: String; override;
    function EventName: String; override;
  public
    procedure Assign(Src: TPersistent); override;

    property ReferAction: TIdSipHandle read fReferAction write fReferAction;
  end;

  TIdSubscriptionNotifyData = class(TIdEventData)
  private
    fEvent:  Cardinal;
    fNotify: TIdSipRequest;

    procedure SetNotify(Value: TIdSipRequest);
  protected
    function Data: String; override;
    function EventName: String; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Event:  Cardinal      read fEvent write fEvent;
    property Notify: TIdSipRequest read fNotify write SetNotify;
  end;

  TIdFailedSubscriptionData = class(TIdFailData)
  private
    fResponse: TIdSipResponse;

    procedure SetResponse(Value: TIdSipResponse);
  protected
    function Data: String; override;
    function EventName: String; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Response: TIdSipResponse read fResponse write SetResponse;
  end;

  TIdStackReconfiguredData = class(TIdEventData)
  private
    fActsAsRegistrar:  Boolean;
    fBindings:         TIdSipLocations;
    fRawConfiguration: TStrings;
    fRoutingTableType: String;

    procedure SetBindings(Value: TIdSipLocations);
    procedure SetRawConfiguration(Value: TStrings);
  protected
    function Data: String; override;
    function EventName: String; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property ActsAsRegistrar:  Boolean         read fActsAsRegistrar write fActsAsRegistrar;
    property Bindings:         TIdSipLocations read fBindings write SetBindings;
    property RawConfiguration: TStrings        read fRawConfiguration write SetRawConfiguration;
    property RoutingTableType: String          read fRoutingTableType write fRoutingTableType;
  end;

  // I contain the result of an asynchronous message send. That is, when you've
  // sent a message that generates a response, the StackInterface uses me to
  // return the data to you.
  //
  // ReferenceID contains the (registered) ID you gave to the stack's
  // SendAsyncCall.
  TIdAsynchronousMessageResultData = class(TIdEventData)
  private
    fReferenceID: String;
  protected
    function Data: String; override;
    function EventName: String; override;
  public
    procedure Assign(Src: TPersistent); override;

    property ReferenceID: String read fReferenceID write fReferenceID;
  end;

  TIdBooleanResultData = class(TIdAsynchronousMessageResultData)
  private
    fResult: Boolean;
  protected
    function Data: String; override;
  public
    procedure Assign(Src: TPersistent); override;

    property Result: Boolean read fResult write fResult;
  end;

  TIdDomainNameRecordsResultData = class(TIdAsynchronousMessageResultData)
  private
    fIPAddresses: TIdDomainNameRecords;

    procedure SetIPAddresses(Value: TIdDomainNameRecords);
  protected
    function Data: String; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property IPAddresses: TIdDomainNameRecords read fIPAddresses write SetIPAddresses;
  end;

  TIdGetBindingsData = class(TIdAsynchronousMessageResultData)
  private
    fBindings: TIdSipLocations;

    procedure SetBindings(Value: TIdSipLocations);
  protected
    function Data: String; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Bindings: TIdSipLocations read fBindings write SetBindings;
  end;

  TIdStringResultData = class(TIdAsynchronousMessageResultData)
  private
    fResult: String;
  protected
    function Data: String; override;
  public
    procedure Assign(Src: TPersistent); override;

    property Result: String read fResult write fResult;
  end;

  TIdStringDictionaryResultData = class(TIdAsynchronousMessageResultData)
  private
    fResult: TStringDictionary;

    function  DictionaryAsString(D: TStringDictionary): String;
    procedure SetResult(Value: TStringDictionary);
  protected
    function Data: String; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Result: TStringDictionary read fResult write SetResult;
  end;

  // I represent a reified method call, like my ancestor, that a
  // SipStackInterface uses to signal that something interesting happened (an
  // inbound call has arrived, a network failure occured, an action succeeded,
  // tc.)
  TIdSipStackInterfaceEventMethod = class(TIdNotification)
  private
    fData:   TIdEventData;
    fEvent:  Cardinal;
    fStack:  TIdSipStackInterface;
  public
    procedure Run(const Subject: IInterface); override;

    property Data:   TIdEventData         read fData  write fData;
    property Event:  Cardinal             read fEvent write fEvent;
    property Stack:  TIdSipStackInterface read fStack write fStack;
  end;

  TIdStackWait = class(TIdWait)
  private
    fStackID: String;
  protected
    procedure TriggerOnStack(Stack: TIdSipStackInterface); virtual;
  public
    procedure Trigger; override;

    property StackID: String read fStackID write fStackID;
  end;

  TIdStackShutdownWait = class(TIdStackWait)
  protected
    procedure TriggerOnStack(Stack: TIdSipStackInterface); override;
  end;

  TIdSipStackReconfigureStackInterfaceWait = class(TIdStackWait)
  private
    fConfiguration: TStrings;

    procedure SetConfiguration(Value: TStrings);
  protected
    procedure TriggerOnStack(Stack: TIdSipStackInterface); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    property Configuration: TStrings read fConfiguration write SetConfiguration;
  end;

  // When you want to send a message to something running in the context of a
  // TimerQueue, instantiate me. I generate a unique reference ID, which you
  // can use to differentiate between multiple asynchronous message sends.
  //
  // It's important that every TIdSipAsynchronousMessageWait have a unique
  // ReferenceID so that you can differentiate the results when you receive a
  // notification (in the form of a TIdAsynchronousMessageResultData). I
  // guarantee this by using a RegisteredObject to generate this ID.
  TIdAsynchronousMessageWait = class(TIdWait)
  private
    fStackID: String;
  protected
    procedure FireWait(Stack: TIdSipStackInterface); virtual;
  public
    procedure Trigger; override;

    property StackID: String read fStackID write fStackID;
  end;

  TIdGetBindingsWait = class(TIdAsynchronousMessageWait)
  protected
    procedure FireWait(Stack: TIdSipStackInterface); override;
  end;

  // Use me to find out if the stack identified by StackID is the source of a
  // request. That is, did an outbound action created by the stack send this
  // request?
  TIdIsSourceOfWait = class(TIdAsynchronousMessageWait)
  private
    fRequest: TIdSipRequest;

    procedure SetRequest(Value: TIdSipRequest);
  protected
    procedure FireWait(Stack: TIdSipStackInterface); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    property Request: TIdSipRequest read fRequest write SetRequest;
  end;

  TIdNetworkingMessageWait = class(TIdAsynchronousMessageWait)
  protected
    procedure FireWait(Stack: TIdSipStackInterface); override;
    procedure Ask(Stack: TIdSipStackInterface;
                  E: TIdSipNameServerExtension); virtual;
  end;

  // Given a DestinationAddress, I ask the stack to give the best local address
  // to use to contact something at DestinationAddress.
  TIdLocalAddressForWait = class(TIdNetworkingMessageWait)
  private
    fDestinationAddress: String;
  protected
    procedure Ask(Stack: TIdSipStackInterface;
                  E: TIdSipNameServerExtension); override;
  public
    property DestinationAddress: String read fDestinationAddress write fDestinationAddress;
  end;

  // Given a DestinationAddress, I ask the stack to give the best local address
  // or NATted address to use to contact something at DestinationAddress.
  TIdLocalOrMappedAddressForWait = class(TIdNetworkingMessageWait)
  private
    fDestinationAddress: String;
  protected
    procedure Ask(Stack: TIdSipStackInterface;
                  E: TIdSipNameServerExtension); override;
  public
    property DestinationAddress: String read fDestinationAddress write fDestinationAddress;
  end;

  // I do a name (A/AAAA) lookup on HostName.
  TIdResolveNamesForWait = class(TIdNetworkingMessageWait)
  private
    fHostName: String;
  protected
    procedure Ask(Stack: TIdSipStackInterface;
                  E: TIdSipNameServerExtension); override;
  public
    property HostName: String read fHostName write fHostName;
  end;

  TIdCollectStatisticsWait = class(TIdAsynchronousMessageWait)
  protected
    procedure FireWait(Stack: TIdSipStackInterface); override;
  end;

  // Raise me whenever someone tries to execute an action with a handle
  // referring to an object of the wrong type. (Example: calling
  // TIdSipStackInterface.SendProvisional with the Handle of an OPTIONS query).
  EInvalidHandle = class(Exception);

  // Raise me when the UserAgent doesn't support an action (e.g., it doesn't use
  // the SubscribeModule and the caller tried to MakeRefer).
  ENotSupported = class(Exception);

  // Raise me whenever something goes wrong in the network. That might be things
  // like networks being unreachable, or names failing to resolve.
  ENetworkError = class(Exception);

// Call management constants
const
  InvalidHandle = 0;

const
  CM_BASE = WM_USER;

  CM_SUCCESS                      = CM_BASE + 0;
  CM_FAIL                         = CM_BASE + 1;
  CM_NETWORK_FAILURE              = CM_BASE + 2;
  CM_CALL_REQUEST_NOTIFY          = CM_BASE + 3;
  CM_CALL_ENDED                   = CM_BASE + 4;
  CM_CALL_ESTABLISHED             = CM_BASE + 5;
  CM_CALL_REMOTE_MODIFY_REQUEST   = CM_BASE + 6;
  CM_CALL_OUTBOUND_MODIFY_SUCCESS = CM_BASE + 7;
  CM_CALL_PROGRESS                = CM_BASE + 8;
  CM_CALL_REFERRAL                = CM_BASE + 9;
  CM_AUTHENTICATION_CHALLENGE     = CM_BASE + 10;
  CM_SUBSCRIPTION_ESTABLISHED     = CM_BASE + 11;
  CM_SUBSCRIPTION_RECV_NOTIFY     = CM_BASE + 12;
  CM_SUBSCRIPTION_EXPIRED         = CM_BASE + 13;
  CM_SUBSCRIPTION_REQUEST_NOTIFY  = CM_BASE + 14;
  CM_SUBSCRIPTION_RESUBSCRIBED    = CM_BASE + 15;
  CM_QUERY_OPTIONS_RESPONSE       = CM_BASE + 16;
  CM_STACK_RECONFIGURED           = CM_BASE + 17;
  CM_ASYNC_MSG_RESULT             = CM_BASE + 18;

  CM_DEBUG = CM_BASE + 10000;

  CM_DEBUG_DROPPED_MSG            = CM_DEBUG + 0;
  CM_DEBUG_RECV_MSG               = CM_DEBUG + 1;
  CM_DEBUG_SEND_MSG               = CM_DEBUG + 2;
  CM_DEBUG_TRANSPORT_EXCEPTION    = CM_DEBUG + 3;
  CM_DEBUG_TRANSPORT_REJECTED_MSG = CM_DEBUG + 4;
  CM_DEBUG_STACK_STARTED          = CM_DEBUG + 5;
  CM_DEBUG_STACK_STOPPED          = CM_DEBUG + 6;
  CM_DEBUG_EXCEPTION              = CM_DEBUG + 7;
  CM_DEBUG_WAIT_EXCEPTION         = CM_DEBUG + 8;
  CM_LAST                         = CM_DEBUG_WAIT_EXCEPTION;

// Constants for TIdCallEndedData
const
  CallEndedSuccess        = 0;
  CallEndedFailure        = 1;
  CallEndedNotFound       = SIPNotFound;
  CallEndedRejected       = SIPBusyHere;
  CallServiceNotAvailable = SIPServiceUnavailable;

type
  TIdSipEventMessage = packed record
    Event:     Cardinal;
    Data:      TIdSipStackInterfaceEventMethod;
    Reserved1: DWord;
    Reserved2: DWord;
  end;

function EventNames(Event: Cardinal): String;

implementation

uses
  IdRandom, IdSimpleParser, IdSipAuthentication, IdSipIndyLocator,
  IdSipMockLocator, IdStack, IdUDPServer, IdRoutingTable;

const
  ActionNotAllowedForHandle = 'You cannot perform a %s action on a %s handle (%d)';
  NoSuchHandle              = 'No such handle (%d)';

function EventNames(Event: Cardinal): String;
begin
  case Event of
    CM_AUTHENTICATION_CHALLENGE:     Result := 'CM_AUTHENTICATION_CHALLENGE';
    CM_FAIL:                         Result := 'CM_FAIL';
    CM_NETWORK_FAILURE:              Result := 'CM_NETWORK_FAILURE';
    CM_CALL_ENDED:                   Result := 'CM_CALL_ENDED';
    CM_CALL_ESTABLISHED:             Result := 'CM_CALL_ESTABLISHED';
    CM_CALL_OUTBOUND_MODIFY_SUCCESS: Result := 'CM_CALL_OUTBOUND_MODIFY_SUCCESS';
    CM_CALL_PROGRESS:                Result := 'CM_CALL_PROGRESS';
    CM_CALL_REMOTE_MODIFY_REQUEST:   Result := 'CM_CALL_REMOTE_MODIFY_REQUEST';
    CM_CALL_REQUEST_NOTIFY:          Result := 'CM_CALL_REQUEST_NOTIFY';
    CM_SUBSCRIPTION_ESTABLISHED:     Result := 'CM_SUBSCRIPTION_ESTABLISHED';
    CM_SUBSCRIPTION_EXPIRED:         Result := 'CM_SUBSCRIPTION_EXPIRED';
    CM_SUBSCRIPTION_RECV_NOTIFY:     Result := 'CM_SUBSCRIPTION_RECV_NOTIFY';
    CM_SUBSCRIPTION_REQUEST_NOTIFY:  Result := 'CM_SUBSCRIPTION_REQUEST_NOTIFY';
    CM_SUBSCRIPTION_RESUBSCRIBED:    Result := 'CM_SUBSCRIPTION_RESUBSCRIBED';
    CM_SUCCESS:                      Result := 'CM_SUCCESS';
    CM_QUERY_OPTIONS_RESPONSE:       Result := 'CM_QUERY_OPTIONS_RESPONSE';
    CM_STACK_RECONFIGURED:           Result := 'CM_STACK_RECONFIGURED';
    CM_ASYNC_MSG_RESULT:             Result := 'CM_ASYNC_MSG_RESULT';

    CM_DEBUG_DROPPED_MSG:            Result := 'CM_DEBUG_DROPPED_MSG';
    CM_DEBUG_EXCEPTION:              Result := 'CM_DEBUG_EXCEPTION';
    CM_DEBUG_RECV_MSG:               Result := 'CM_DEBUG_RECV_MSG';
    CM_DEBUG_SEND_MSG:               Result := 'CM_DEBUG_SEND_MSG';
    CM_DEBUG_STACK_STARTED:          Result := 'CM_DEBUG_STACK_STARTED';
    CM_DEBUG_STACK_STOPPED:          Result := 'CM_DEBUG_STACK_STOPPED';
    CM_DEBUG_TRANSPORT_EXCEPTION:    Result := 'CM_DEBUG_TRANSPORT_EXCEPTION';
    CM_DEBUG_TRANSPORT_REJECTED_MSG: Result := 'CM_DEBUG_TRANSPORT_REJECTED_MSG';
    CM_DEBUG_WAIT_EXCEPTION:         Result := 'CM_DEBUG_WAIT_EXCEPTION';
  else
    Result := 'Unknown: ' + IntToStr(Event);
  end;
end;

//******************************************************************************
//* TIdActionAssociation                                                       *
//******************************************************************************
//* TIdActionAssociation Public methods ****************************************

constructor TIdActionAssociation.Create(Action: TIdSipAction;
                                        Handle: TIdSipHandle);
begin
  inherited Create;

  Self.fAction := Action;
  Self.fHandle := Handle;
end;

//******************************************************************************
//* TIdSipStackInterface                                                       *
//******************************************************************************
//* TIdSipStackInterface Public methods ****************************************

constructor TIdSipStackInterface.Create(UiHandle: HWnd;
                                        TimerQueue: TIdTimerQueue;
                                        Configuration: TStrings);
var
  Configurator: TIdSipStackConfigurator;
  Module:       TIdSipMessageModule;
begin
  inherited Create;

  Self.FHasBeenFreed:=nil;

  Self.TimerQueue := TimerQueue;
  Self.TimerQueue.AddListener(Self);

  Self.ActionLock := TCriticalSection.Create;
  Self.Actions    := TObjectList.Create(true);

  Self.fUiHandle := UiHandle;

  Configurator := TIdSipStackConfigurator.Create;
  try
    Self.fUserAgent := Configurator.CreateUserAgent(Configuration, Self.TimerQueue, [Self]);
    Self.UserAgent.InviteModule.AddListener(Self);
//    Self.UserAgent.AddTransportListener(Self);

    Self.ListenToAllTransports;

    Module := Self.UserAgent.ModuleFor(TIdSipSubscribeModule);

    if not Module.IsNull then begin
      Self.SubscribeModule := Module as TIdSipSubscribeModule;
      Self.SubscribeModule.AddListener(Self);
    end;
  finally
    Configurator.Free;
  end;

  Self.Configure(Configuration);
end;

destructor TIdSipStackInterface.Destroy;
begin
  Self.NotifyOfStackShutdown;

//  Self.DebugUnregister;

  Self.UserAgent.Free;

  Self.Actions.Free;
  Self.ActionLock.Free;

  if Assigned(Self.FHasBeenFreed) then
     Self.FHasBeenFreed^:=True;

  inherited Destroy;
end;

procedure TIdSipStackInterface.AcceptCallModify(ActionHandle: TIdSipHandle;
                                                const LocalSessionDescription: String;
                                                const ContentType: String);
var
  Action: TIdSipAction;
  Wait:   TIdSipSessionAcceptCallModify;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipSession);
    Wait   := TIdSipSessionAcceptCallModify.Create;
    Wait.ContentType := ContentType;
    Wait.Offer       := LocalSessionDescription;
    Wait.SessionID   := Action.ID;

   Self.TimerQueue.AddEvent(TriggerImmediately, Wait);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.AnswerCall(ActionHandle: TIdSipHandle;
                                          const Offer: String;
                                          const ContentType: String);
var
  Action: TIdSipAction;
  Wait:   TIdSipSessionAcceptWait;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipInboundSession);

    Wait := TIdSipSessionAcceptWait.Create;
    Wait.ContentType := ContentType;
    Wait.Offer       := Offer;
    Wait.SessionID   := Action.ID;

    Self.TimerQueue.AddEvent(TriggerImmediately, Wait);
  finally
    Self.ActionLock.Release;
  end;
end;

function TIdSipStackInterface.AttachExtension(EType: TIdSipStackInterfaceExtensionClass): TIdSipStackInterfaceExtension;
begin
  Result := EType.Create(Self.UserAgent);
end;

procedure TIdSipStackInterface.Authenticate(ActionHandle: TIdSipHandle;
                                            Credentials: TIdSipAuthorizationHeader);
var
  Action: TIdSipAction;
  Wait:   TIdSipActionAuthenticateWait;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipAction);

    Wait := TIdSipActionAuthenticateWait.Create;
    Wait.SetCredentials(Credentials);
    Wait.ActionID := Action.ID;

    Self.TimerQueue.AddEvent(TriggerImmediately, Wait);
  finally
    Self.ActionLock.Release;
  end;
end;

function TIdSipStackInterface.GruuOf(ActionHandle: TIdSipHandle): String;
var
  Action: TIdSipAction;
begin
  // Return the GRUU of the action referenced by ActionHandle. This can be the
  // empty string - typically if the stack doesn't support GRUU.

  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipSession);

    Result := Action.LocalGruu.FullValue;
  finally
    Self.ActionLock.Release;
  end;
end;

function TIdSipStackInterface.HandleOf(const LocalGruu: String): TIdSipHandle;
begin
  // Find the handle of the action that uses LocalGruu as a Contact (typically
  // either a Session or a Subscription/Referral).
  Self.ActionLock.Acquire;
  try
    Result := Self.HandleFor(Self.UserAgent.FindActionForGruu(LocalGruu));
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.HangUp(ActionHandle: TIdSipHandle);
var
  Action:        TIdSipAction;
  TerminateWait: TIdSipActionTerminateWait;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipSession);

    TerminateWait := TIdSipActionTerminateWait.Create;
    TerminateWait.ActionID := Action.ID;

    Self.TimerQueue.AddEvent(TriggerImmediately, TerminateWait);
  finally
    Self.ActionLock.Release;
  end;
end;

function TIdSipStackInterface.MakeCall(From: TIdSipAddressHeader;
                                       Dest: TIdSipAddressHeader;
                                       const LocalSessionDescription: String;
                                       const MimeType: String;
                                       MaxForwards: Cardinal): TIdSipHandle;
var
  Sess: TIdSipOutboundSession;
begin
  if From.IsMalformed then begin
    Result := InvalidHandle;
    Exit;
  end;

  if Dest.IsMalformed then begin
    Result := InvalidHandle;
    Exit;
  end;

  Sess := Self.UserAgent.InviteModule.Call(From, Dest, LocalSessionDescription, MimeType);
  Sess.MaxForwards := MaxForwards;
  Result := Self.HandleFor(Sess);
  Sess.AddSessionListener(Self);
end;

function TIdSipStackInterface.MakeOptionsQuery(Dest: TIdSipAddressHeader): TIdSipHandle;
var
  Options: TIdSipOutboundOptions;
begin
  if Dest.IsMalformed then begin
    Result := InvalidHandle;
    Exit;
  end;

  Options := Self.UserAgent.QueryOptions(Dest) as TIdSipOutboundOptions;
  Result := Self.HandleFor(Options);
  Options.AddListener(Self);
end;

function TIdSipStackInterface.MakeRefer(Target: TIdSipAddressHeader;
                                        Resource: TIdSipAddressHeader): TIdSipHandle;
var
  Ref: TIdSipOutboundReferral;
begin
  if Target.IsMalformed then begin
    Result := InvalidHandle;
    Exit;
  end;

  // Refer Target to the Resource by sending a REFER message to Target.

  // Check that the UA even supports REFER!
  if not Assigned(Self.SubscribeModule) then
    raise ENotSupported.Create(MethodRefer);

  Ref := Self.SubscribeModule.Refer(Target, Resource);
  Result := Self.HandleFor(Ref);
  Ref.AddListener(Self);
end;

function TIdSipStackInterface.MakeRegistration(Registrar: TIdSipUri): TIdSipHandle;
var
  Reg: TIdSipOutboundRegistrationBase;
begin
  if Registrar.IsMalformed then begin
    Result := InvalidHandle;
    Exit;
  end;

  if not Self.UserAgent.UsesModule(TIdSipOutboundRegisterModule) then begin
    Result := InvalidHandle;
    Exit;
  end;

  Reg := Self.UserAgent.RegisterWith(Registrar, Self.UserAgent.From);
  Result := Self.HandleFor(Reg);
  Reg.AddListener(Self);
end;

function TIdSipStackInterface.MakeRegistration(Registrar: TIdSipUri;
                                               Contacts: TIdSipContacts): TIdSipHandle;
var
  Reg: TIdSipOutboundRegistrationBase;
begin
  if Registrar.IsMalformed then begin
    Result := InvalidHandle;
    Exit;
  end;

  if Contacts.IsMalformed then begin
    Result := InvalidHandle;
    Exit;
  end;

  if not Self.UserAgent.UsesModule(TIdSipOutboundRegisterModule) then begin
    Result := InvalidHandle;
    Exit;
  end;

  Reg := Self.UserAgent.RegisterModule.RegisterWith(Registrar, Contacts);
  Result := Self.HandleFor(Reg);
  Reg.AddListener(Self);
end;

function TIdSipStackInterface.MakeSubscription(Target: TIdSipAddressHeader;
                                               const EventPackage: String): TIdSipHandle;
var
  Sub: TIdSipOutboundSubscription;
  SubMod: TIdSipSubscribeModule;
begin
  if Target.IsMalformed then begin
    Result := InvalidHandle;
    Exit;
  end;

  if not Self.UserAgent.UsesModule(MethodSubscribe) then begin
    Result := InvalidHandle;
    Exit;
  end;

  SubMod := Self.UserAgent.ModuleFor(MethodSubscribe) as TIdSipSubscribeModule;
  Sub := SubMod.Subscribe(Target, EventPackage);
  Result := Self.HandleFor(Sub);
  Sub.AddListener(Self);
end;

function TIdSipStackInterface.MakeTransfer(Transferee: TIdSipAddressHeader;
                                           TransferTarget: TIdSipAddressHeader;
                                           Call: TIdSipHandle): TIdSipHandle;
var
  Action:       TIdSipAction;
  Ref:          TIdSipOutboundReferral;
  Session:      TIdSipSession;
  TargetDialog: TIdSipDialogID;
begin
  // Transfer Transferee to TranserTarget using the (remote party's) dialog
  // ID of Call as authorization.

  if Transferee.IsMalformed
    or TransferTarget.IsMalformed then begin
    Result := InvalidHandle;
    Exit;
  end;

  // Check that the UA even supports REFER!
  if not Assigned(Self.SubscribeModule) then
    raise ENotSupported.Create(MethodRefer);

  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(Call, TIdSipSession);

    Session := Action as TIdSipSession;

    TargetDialog := Session.Dialog.ID.GetRemoteID;
    try
      Ref := Self.SubscribeModule.Transfer(Transferee,
                                           TransferTarget,
                                           TargetDialog);
      Result := Self.HandleFor(Ref);
      Ref.AddListener(Self);
    finally
      TargetDialog.Free;
    end;
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.ModifyCall(ActionHandle: TIdSipHandle;
                                          const Offer: String;
                                          const ContentType: String);
var
  Action: TIdSipAction;
  Wait:   TIdSipSessionModifyWait;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipSession);

    Wait := TIdSipSessionModifyWait.Create;
    Wait.SessionID := Action.ID;
    Wait.ContentType := ContentType;
    Wait.Offer := Offer;

    Self.TimerQueue.AddEvent(TriggerImmediately, Wait);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.NotifyOfAsyncMessageResult(Data: TIdAsynchronousMessageResultData);
begin
  Self.NotifyEvent(CM_ASYNC_MSG_RESULT, Data);
end;

procedure TIdSipStackInterface.NotifyReferralDenied(ActionHandle: TIdSipHandle);
begin
  Self.NotifyReferral(ActionHandle, TIdSipNotifyReferralDeniedWait, nil);
end;

procedure TIdSipStackInterface.NotifyReferralFailed(ActionHandle: TIdSipHandle;
                                                    Response: TIdSipResponse = nil);
begin
  Self.NotifyReferral(ActionHandle, TIdSipNotifyReferralFailedWait, Response);
end;

procedure TIdSipStackInterface.NotifyReferralSucceeded(ActionHandle: TIdSipHandle);
begin
  Self.NotifyReferral(ActionHandle, TIdSipNotifyReferralSucceededWait, nil);
end;

procedure TIdSipStackInterface.NotifyReferralTrying(ActionHandle: TIdSipHandle);
begin
  Self.NotifyReferral(ActionHandle, TIdSipNotifyReferralTryingWait, nil);
end;

procedure TIdSipStackInterface.NotifySubcriber(ActionHandle: TIdSipHandle;
                                               const Notification: String;
                                               const MimeType: String);
var
  Action: TIdSipAction;
  Wait:   TIdSipInboundSubscriptionNotifyWait;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipInboundSubscription);

    Wait := TIdSipInboundSubscriptionNotifyWait.Create;
    Wait.ActionID     := Action.ID;
    Wait.MimeType     := MimeType;
    Wait.Notification := Notification;

    Self.TimerQueue.AddEvent(TriggerImmediately, Wait);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.ReconfigureStack(NewConfiguration: TStrings);
var
  Wait: TIdSipStackReconfigureStackInterfaceWait;
begin
  Wait := TIdSipStackReconfigureStackInterfaceWait.Create;
  Wait.Configuration := NewConfiguration;
  Wait.StackID       := Self.ID;
  
  Self.TimerQueue.AddEvent(TriggerImmediately, Wait);
end;

procedure TIdSipStackInterface.RedirectCall(ActionHandle: TIdSipHandle;
                                            NewTarget: TIdSipAddressHeader;
                                            Temporary: Boolean = true);
var
  Action: TIdSipAction;
  Wait:   TIdSipSessionRedirectWait;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipInboundSession);

    Wait := TIdSipSessionRedirectWait.Create;
    Wait.NewTarget := NewTarget;
    Wait.SessionID := Action.ID;
    Wait.Temporary := Temporary;

    Self.TimerQueue.AddEvent(TriggerImmediately, Wait);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.RejectCall(ActionHandle: TIdSipHandle;
                                          StatusCode: Cardinal;
                                          StatusText: String = '');
var
  Action: TIdSipAction;
  Wait:   TIdSipSessionRejectWait;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipInboundSession);

    Wait := TIdSipSessionRejectWait.Create;
    Wait.SessionID  := Action.ID;
    Wait.StatusCode := StatusCode;
    Wait.StatusText := StatusText;

    Self.TimerQueue.AddEvent(TriggerImmediately, Wait);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.Resume;
var
  I: Integer;
begin
  // Start me first (since I'm the "heartbeat" thread).
  Self.TimerQueue.Resume;

  // THEN start my transport threads.
  for I := 0 to Self.UserAgent.Dispatcher.TransportCount - 1 do
    Self.UserAgent.Dispatcher.Transports[I].Start;

  Self.NotifyOfStackStartup;
end;

procedure TIdSipStackInterface.Send(ActionHandle: TIdSipHandle);
var
  Action: TIdSipAction;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipAction);

    Self.SendAction(Action);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.SendAsyncCall(ReferenceID: String);
var
  Wait: TObject;
begin
  // Invoke an asynchronous function call identified by ReferenceID.

  Wait := TIdObjectRegistry.FindObject(ReferenceID);

  if Assigned(Wait) and (Wait is TIdWait) then
    Self.TimerQueue.AddEvent(TriggerImmediately, Wait as TIdWait);
end;

procedure TIdSipStackInterface.SendProvisional(ActionHandle: TIdSipHandle;
                                               StatusCode: Cardinal = SIPSessionProgress;
                                               Description: String = RSSIPSessionProgress);
var
  Action: TIdSipAction;
  Wait:   TIdSipSendProvisionalWait;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipInboundSession);

    Wait := TIdSipSendProvisionalWait.Create;
    Wait.StatusCode := StatusCode;
    Wait.StatusText := Description;
    Wait.SessionID  := Action.ID;

    Self.TimerQueue.AddEvent(TriggerImmediately, Wait);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.Terminate;
var
  Wait: TIdStackShutdownWait;
begin
  Wait := TIdStackShutdownWait.Create;
  Wait.StackID := Self.ID;

  Self.TimerQueue.AddEvent(TriggerImmediately, Wait);
end;

procedure TIdSipStackInterface.Terminate(ActionHandle: TIdSipHandle);
var
  Action: TIdSipAction;
  Wait:   TIdSipActionTerminateWait;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipAction);

    Wait := TIdSipActionTerminateWait.Create;
    Wait.ActionID := Action.ID;

    Self.TimerQueue.AddEvent(TriggerImmediately, Wait);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.Terminate(HasBeenFreed: PBoolean);
begin
  Self.FHasBeenFreed:=HasBeenFreed;
  if Assigned(Self.FHasBeenFreed) then
     Self.FHasBeenFreed^:=False;

  Self.Terminate;
end;

//* TIdSipStackInterface Protected methods *************************************

function TIdSipStackInterface.AddAction(Action: TIdSipAction): TIdSipHandle;
var
  Assoc: TIdActionAssociation;
begin
  Self.ActionLock.Acquire;
  try
    Result := Self.NewHandle;
    Assoc := TIdActionAssociation.Create(Action, Result);
    Self.Actions.Add(Assoc);
  finally
    Self.ActionLock.Release;
  end;
end;

function TIdSipStackInterface.CreateReconfigureNotificationData(Configuration: TStrings): TIdStackReconfiguredData;
begin
  Result := Self.ReconfiguredNotificationDataType.Create;
  Result.ActsAsRegistrar  := Self.UserAgent.UsesModule(TIdSipRegisterModule);
  Result.Handle           := InvalidHandle;
  Result.RawConfiguration := Configuration;
  Result.RoutingTableType := Self.UserAgent.RoutingTable.ClassName;

  Self.UserAgent.Dispatcher.LocalBindings(Result.Bindings);
end;

procedure TIdSipStackInterface.NotifyEvent(Event: Cardinal;
                                           Data: TIdEventData);
var
  Notification: TIdSipStackInterfaceEventMethod;
begin
  // I create a COPY of the data in Data, and pass that to the message queue at
  // Self.UiHandle.

  Notification := TIdSipStackInterfaceEventMethod.Create;
  Notification.Data   := Data.Copy;
  Notification.Event  := Event;
  Notification.Stack  := Self;

  Self.UserAgent.Log(Format('Message %s sent to application', [EventNames(Event)]), slDebug, Event, Data.AsString);

  // The receiver of this message must free the Notification.
  PostMessage(Self.UiHandle, UINT(Notification.Event), WPARAM(Notification), 0)
end;

procedure TIdSipStackInterface.ParseLine(Directive, Configuration: String);
begin
  // I have no special processing directives, even though my subclasses might.
end;

procedure TIdSipStackInterface.PostConfigurationActions;
begin
  Self.ListenToAllTransports;
end;

procedure TIdSipStackInterface.PreConfigurationActions;
begin
  Self.StopListeningToAllTransports;
end;

function TIdSipStackInterface.ReconfiguredNotificationDataType: TIdStackReconfiguredDataClass;
begin
  Result := TIdStackReconfiguredData;
end;

//* TIdSipStackInterface Private methods ***************************************

function TIdSipStackInterface.ActionFor(Handle: TIdSipHandle): TIdSipAction;
var
  I: Integer;
begin
  // Precondition: ActionLock acquired.
  I      := 0;
  Result := nil;

  while (I < Self.Actions.Count) and not Assigned(Result) do begin
    if (Self.AssociationAt(I).Handle = Handle) then
      Result := Self.AssociationAt(I).Action
    else
      Inc(I);
  end;
end;

function TIdSipStackInterface.AssociationAt(Index: Integer): TIdActionAssociation;
begin
  Result := Self.Actions[Index] as TIdActionAssociation;
end;

procedure TIdSipStackInterface.Configure(Configuration: TStrings);
var
  Config:    String;
  Directive: String;
  I:         Integer;
begin
  Self.PreConfigurationActions;
  try
    for I := 0 to Configuration.Count - 1 do begin
      Config := Configuration[I];
      Directive := Trim(Fetch(Config, ':'));

      Self.ParseLine(Directive, Trim(Config));
    end;
  finally
    Self.PostConfigurationActions;
    Self.NotifyOfReconfiguration(Configuration);
  end;
end;

function TIdSipStackInterface.GetAndCheckAction(Handle: TIdSipHandle;
                                                ExpectedType: TIdSipActionClass): TIdSipAction;
begin
  Result := Self.ActionFor(Handle);

  if not Assigned(Result) then
    raise EInvalidHandle.Create(Format(NoSuchHandle, [Handle]));

  if not (Result is ExpectedType) then
    raise EInvalidHandle.Create(Format(ActionNotAllowedForHandle, [Result.ClassName, ExpectedType.ClassName, Handle]));
end;

function TIdSipStackInterface.HandleFor(Action: TIdSipAction): TIdSipHandle;
var
  I: Integer;
begin
  // Precondition: ActionLock acquired.
  I      := 0;
  Result := InvalidHandle;

  while (I < Self.Actions.Count) and (Result = InvalidHandle) do begin
    if (Self.AssociationAt(I).Action = Action) then
      Result := Self.AssociationAt(I).Handle
    else
      Inc(I);
  end;
end;

function TIdSipStackInterface.IndexOf(H: TIdSipHandle): Integer;
var
  Found: Boolean;
begin
  // Precondition: ActionLock acquired.

  if (Self.Actions.Count = 0) then begin
    Result := ItemNotFoundIndex;
    Exit;
  end;

  Found  := false;
  Result := 0;
  while (Result < Self.Actions.Count) and not Found do begin
    if (Self.AssociationAt(Result).Handle = H) then
      Found := true
    else
      Inc(Result);
  end;

  if not Found then
    Result := ItemNotFoundIndex;
end;

function TIdSipStackInterface.HasHandle(H: TIdSipHandle): Boolean;
begin
  // Precondition: ActionLock acquired.
  Result := Self.IndexOf(H) <> ItemNotFoundIndex;
end;

procedure TIdSipStackInterface.ListenToAllTransports;
var
  I: Integer;
begin
  for I := 0 to Self.UserAgent.Dispatcher.TransportCount - 1 do begin
    Self.UserAgent.Dispatcher.Transports[I].AddTransportListener(Self);
    Self.UserAgent.Dispatcher.Transports[I].AddTransportSendingListener(Self);
  end;
end;

function TIdSipStackInterface.NewHandle: TIdSipHandle;
begin
  // Precondition: ActionLock acquired.
  // Postcondition: Result contains a handle that's not assigned to any ongoing
  // action.

  repeat
    Result := GRandomNumber.NextCardinal;
  until not Self.HasHandle(Result);
end;

procedure TIdSipStackInterface.NotifyOfReconfiguration(Configuration: TStrings);
var
  Data: TIdStackReconfiguredData;
begin
  Data := Self.CreateReconfigureNotificationData(Configuration);
  try
    Self.NotifyEvent(CM_STACK_RECONFIGURED, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.NotifyOfSentMessage(Msg: TIdSipMessage;
                                                   Binding: TIdConnectionBindings);

var
  Data: TIdDebugSendMessageData;
begin
  Data := TIdDebugSendMessageData.Create;
  try
    Data.Handle  := InvalidHandle;
    Data.Binding := Binding.Copy;
    Data.Message := Msg.Copy;

    Self.NotifyEvent(CM_DEBUG_SEND_MSG, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.NotifyOfStackShutdown;
var
  Data: TIdDebugData;
begin
  Data := TIdDebugData.Create;
  try
    Data.Handle := InvalidHandle;
    Data.Event  := CM_DEBUG_STACK_STOPPED;

    Self.NotifyEvent(CM_DEBUG_STACK_STOPPED, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.NotifyOfStackStartup;
var
  Data: TIdDebugData;
begin
  Data := TIdDebugData.Create;
  try
    Data.Handle := InvalidHandle;
    Data.Event  := CM_DEBUG_STACK_STARTED;

    Self.NotifyEvent(CM_DEBUG_STACK_STARTED, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.NotifyReferral(ActionHandle: TIdSipHandle;
                                              NotifyType: TIdSipInboundReferralWaitClass;
                                              Response: TIdSipResponse);
var
  Action: TIdSipAction;
  Wait:   TIdSipInboundReferralWait;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipInboundReferral);

    Wait := NotifyType.Create;
    Wait.ActionID := Action.ID;
    Wait.Response := Response;

    Self.TimerQueue.AddEvent(TriggerImmediately, Wait);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.NotifySubscriptionEvent(Event: Cardinal;
                                                       Subscription: TIdSipSubscription;
                                                       Notify: TIdSipRequest);
var
  Data: TIdSubscriptionNotifyData;
begin
  Data := TIdSubscriptionNotifyData.Create;
  try
    Data.Handle := Self.HandleFor(Subscription);
    Data.Event  := Event;

    if (Notify <> nil) then
      Data.Notify := Notify;

    Self.NotifyEvent(Event, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnAddAction(UserAgent: TIdSipAbstractCore;
                                           Action: TIdSipAction);
const
  LogMsg = '%s with ID %s given handle %s';
var
  H: TIdSipHandle;
begin
  Action.AddActionListener(Self);
  H := Self.AddAction(Action);

  Self.UserAgent.Log(Format(LogMsg, [Action.ClassName, Action.ID, IntToHex(H, 8)]), slDebug, 0, '');
end;

procedure TIdSipStackInterface.OnAuthenticationChallenge(Action: TIdSipAction;
                                                         Response: TIdSipResponse);
var
  Data: TIdAuthenticationChallengeData;
begin
  // Owning actions take care of authentication.
  if Action.IsOwned then Exit;

  Data := TIdAuthenticationChallengeData.Create;
  try
    Data.Challenge         := Response;
    Data.ChallengedRequest := Action.InitialRequest;
    Data.Handle            := Self.HandleFor(Action);

    Self.NotifyEvent(CM_AUTHENTICATION_CHALLENGE, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                                         Message: TIdSipMessage;
                                                         Binding: TIdConnectionBindings);
var
  Data: TIdDebugDroppedMessageData;
begin
  Data := TIdDebugDroppedMessageData.Create;
  try
    Data.Binding := Binding.Copy;
    Data.Handle  := InvalidHandle;
    Data.Message := Message.Copy;

    Self.NotifyEvent(CM_DEBUG_DROPPED_MSG, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnEndedSession(Session: TIdSipSession;
                                              ErrorCode: Cardinal;
                                              const Reason: String);
var
  Data: TIdCallEndedData;
begin
  Data := TIdCallEndedData.Create;
  try
    Data.Handle := Self.HandleFor(Session);
    Data.ErrorCode := ErrorCode;
    Data.Reason    := Reason;
    Self.NotifyEvent(CM_CALL_ENDED, Data);

    Self.RemoveAction(Data.Handle);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnEstablishedSession(Session: TIdSipSession;
                                                    const RemoteSessionDescription: String;
                                                    const MimeType: String);
var
  Data: TIdEstablishedSessionData;
begin
  Data := TIdEstablishedSessionData.Create;
  try
    Data.Handle                   := Self.HandleFor(Session);
    Data.LocalContact             := Session.LocalGruu;
    Data.ID                       := Session.Dialog.ID;
    Data.LocalMimeType            := Session.LocalMimeType;
    Data.LocalParty               := Session.LocalParty;
    Data.LocalSessionDescription  := Session.LocalSessionDescription;
    Data.RemoteContact            := Session.RemoteContact;
    Data.RemoteMimeType           := MimeType;
    Data.RemoteParty              := Session.RemoteParty;
    Data.RemoteSessionDescription := RemoteSessionDescription;

    Self.NotifyEvent(CM_CALL_ESTABLISHED, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnEstablishedSubscription(Subscription: TIdSipOutboundSubscription;
                                                         Notify: TIdSipRequest);
begin
  Self.NotifySubscriptionEvent(CM_SUBSCRIPTION_ESTABLISHED,
                               Subscription,
                               Notify);
end;

procedure TIdSipStackInterface.OnException(FailedMessage: TIdSipMessage;
                                           E: Exception;
                                           const Reason: String);
var
  Data: TIdDebugTransportExceptionData;
begin
  Data := TIdDebugTransportExceptionData.Create;
  try
    Data.Handle := InvalidHandle;
    Data.Error  := E.ClassName;
    Data.Reason := Format('%s (%s)', [E.Message, Reason]);

    Self.NotifyEvent(CM_DEBUG_TRANSPORT_EXCEPTION, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnException(Timer: TIdTimerQueue; Error: Exception; Wait: TIdWait);
var
  Data: TIdDebugWaitExceptionData;
begin
  Data := TIdDebugWaitExceptionData.Create;
  try
    Data.Handle := InvalidHandle;
    Data.Error  := Error.ClassName;
    Data.Reason := Error.Message;
    Data.Wait   := Wait;

    Self.NotifyEvent(CM_DEBUG_WAIT_EXCEPTION, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnExpiredSubscription(Subscription: TIdSipOutboundSubscription;
                                                     Notify: TIdSipRequest);
begin
  Self.NotifySubscriptionEvent(CM_SUBSCRIPTION_EXPIRED,
                               Subscription,
                               Notify);
end;

procedure TIdSipStackInterface.OnFailure(RegisterAgent: TIdSipOutboundRegistrationBase;
                                         ErrorCode: Cardinal;
                                         const Reason: String);
var
  Data: TIdFailedRegistrationData;
begin
  Data := TIdFailedRegistrationData.Create;
  try
    Data.Handle    := Self.HandleFor(RegisterAgent);
    Data.ErrorCode := ErrorCode;
    Data.Reason    := Reason;

    Self.NotifyEvent(CM_FAIL, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnFailure(Subscription: TIdSipOutboundSubscription;
                                         Response: TIdSipResponse);
var
  Data: TIdFailedSubscriptionData;
begin
  Data := TIdFailedSubscriptionData.Create;
  try
    Data.Handle    := Self.HandleFor(Subscription);
    Data.ErrorCode := Response.StatusCode;
    Data.Reason    := Response.StatusText;
    Data.Response  := Response;

    Self.NotifyEvent(CM_FAIL, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnInboundCall(UserAgent: TIdSipInviteModule;
                                             Session: TIdSipInboundSession);
var
  Data: TIdInboundCallData;
begin
  Session.AddSessionListener(Self);
  Self.AddAction(Session);

  Data := TIdInboundCallData.Create;
  try
    Data.Handle                   := Self.HandleFor(Session);
    Data.Invite                   := Session.InitialRequest;
    Data.LocalContact             := Session.LocalGruu;
    Data.LocalMimeType            := Session.LocalMimeType;
    Data.LocalParty               := Session.LocalParty;
    Data.LocalSessionDescription  := Session.LocalSessionDescription;
    Data.RemoteContact            := Session.RemoteContact;
    Data.RemoteMimeType           := Session.RemoteMimeType;
    Data.RemoteParty              := Session.RemoteParty;
    Data.RemoteSessionDescription := Session.RemoteSessionDescription;

    Self.NotifyEvent(CM_CALL_REQUEST_NOTIFY, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnModifySession(Session: TIdSipSession;
                                               const RemoteSessionDescription: String;
                                               const MimeType: String);
var
  Data: TIdModifySessionData;
begin
  Data := TIdModifySessionData.Create;
  try
    Data.Handle := Self.HandleFor(Session);
    Data.RemoteContact            := Session.RemoteContact;
    Data.RemoteMimeType           := MimeType;
    Data.RemoteParty              := Session.RemoteParty;
    Data.RemoteSessionDescription := RemoteSessionDescription;

    Self.NotifyEvent(CM_CALL_REMOTE_MODIFY_REQUEST, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnModifiedSession(Session: TIdSipSession;
                                                 Answer: TIdSipResponse);
var
  Data: TIdModifiedSessionData;
begin
  Data := TIdModifiedSessionData.Create;
  try
    Data.Handle := Self.HandleFor(Session);
    Data.LocalMimeType            := Session.LocalMimeType;
    Data.LocalSessionDescription  := Session.LocalSessionDescription;
    Data.RemoteContact            := Session.RemoteContact;
    Data.RemoteMimeType           := Answer.ContentType;
    Data.RemoteParty              := Session.RemoteParty;
    Data.RemoteSessionDescription := Answer.Body;

    Self.NotifyEvent(CM_CALL_OUTBOUND_MODIFY_SUCCESS, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnNetworkFailure(Action: TIdSipAction;
                                                ErrorCode: Cardinal;
                                                const Reason: String);
var
  Data: TIdNetworkFailureData;
begin
  // Owning actions take care of notifying of network failure.
  if Action.IsOwned then Exit;

  Data := TIdNetworkFailureData.Create;
  try
    Data.Handle    := Self.HandleFor(Action);
    Data.ErrorCode := ErrorCode;
    Data.Reason    := Reason;
    Self.NotifyEvent(CM_NETWORK_FAILURE, Data);

    Self.RemoveAction(Data.Handle);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnNotify(Subscription: TIdSipOutboundSubscription;
                                        Notify: TIdSipRequest);
begin
  Self.NotifySubscriptionEvent(CM_SUBSCRIPTION_RECV_NOTIFY,
                               Subscription,
                               Notify);
end;

procedure TIdSipStackInterface.OnProgressedSession(Session: TIdSipSession;
                                                   Progress: TIdSipResponse);
var
  Data: TIdSessionProgressData;
begin
  Data := TIdSessionProgressData.Create;
  try
    Data.Banner                   := TIdUri.Decode(Progress.StatusText);
    Data.Handle                   := Self.HandleFor(Session);
    Data.LocalMimeType            := Session.LocalMimeType;
    Data.LocalSessionDescription  := Session.LocalSessionDescription;
    Data.ProgressCode             := Progress.StatusCode;
    Data.RemoteContact            := Session.RemoteContact;
    Data.RemoteMimeType           := Progress.ContentType;
    Data.RemoteParty              := Session.RemoteParty;
    Data.RemoteSessionDescription := Progress.Body;


    Self.NotifyEvent(CM_CALL_PROGRESS, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnReceiveRequest(Request: TIdSipRequest;
                                                Receiver: TIdSipTransport;
                                                Source: TIdConnectionBindings);
var
  Data: TIdDebugReceiveMessageData;
begin
  Data := TIdDebugReceiveMessageData.Create;
  try
    Data.Handle := InvalidHandle;
    Data.Binding := Source.Copy;
    Data.Message := Request.Copy;

    Self.NotifyEvent(CM_DEBUG_RECV_MSG, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnReceiveResponse(Response: TIdSipResponse;
                                                 Receiver: TIdSipTransport;
                                                 Source: TIdConnectionBindings);
var
  Data: TIdDebugReceiveMessageData;
begin
  Data := TIdDebugReceiveMessageData.Create;
  try
    Data.Handle := InvalidHandle;
    Data.Binding := Source.Copy;
    Data.Message := Response.Copy;

    Self.NotifyEvent(CM_DEBUG_RECV_MSG, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnReferral(Session: TIdSipSession;
                                          Refer: TIdSipRequest;
                                          Binding: TIdConnectionBindings);
begin
  // We receive notifications of REFER messages sent to Session's GRUU through
  // Session. Specifically, REFERs outside of Session's dialog will end up here.
  // Since we're notified that a message has arrived, the stack doesn't know of
  // the message as a call flow (a TIdSipAction, in other words). Thus, we
  // inform the stack to keep track of the call flow around this message.
  Self.UserAgent.AddInboundAction(Refer, Binding);
end;

procedure TIdSipStackInterface.OnRejectedMessage(const Msg: String;
                                                 const Reason: String;
                                                 Source: TIdConnectionBindings);
var
  Data: TIdDebugTransportRejectedMessageData;
begin
  Data := TIdDebugTransportRejectedMessageData.Create;
  try
    Data.Handle  := InvalidHandle;
    Data.Binding := Source.Copy;
    Data.Msg     := Msg;
    Data.Reason  := Reason;

    Self.NotifyEvent(CM_DEBUG_TRANSPORT_REJECTED_MSG, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnRemoveAction(UserAgent: TIdSipAbstractCore;
                                              Action: TIdSipAction);
const
  LogMsg = '%s with ID %s and handle %s removed';
var
  H: TIdSipHandle;
begin
  H := Self.HandleFor(Action);

  Action.RemoveActionListener(Self);
  Self.RemoveAction(H);

  Self.UserAgent.Log(Format(LogMsg, [Action.ClassName, Action.ID, IntToHex(H, 8)]), slDebug, 0, '');
end;

procedure TIdSipStackInterface.OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                                     Subscription: TIdSipOutboundSubscription);
var
  Data:   TIdResubscriptionData;
  Handle: TIdSipHandle;
begin
  Subscription.AddListener(Self);
  Handle := Self.AddAction(Subscription);

  Data := TIdResubscriptionData.Create;
  try
    Data.Handle       := Handle;
    Data.EventPackage := Subscription.EventPackage;

    Self.NotifyEvent(CM_SUBSCRIPTION_RESUBSCRIBED, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnResponse(OptionsAgent: TIdSipOutboundOptions;
                                          Response: TIdSipResponse);
var
  Data: TIdQueryOptionsData;
begin
  Data := TIdQueryOptionsData.Create;
  try
    Data.Handle   := Self.HandleFor(OptionsAgent);
    Data.Response := Response;

    Self.NotifyEvent(CM_QUERY_OPTIONS_RESPONSE, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnSendRequest(Request: TIdSipRequest;
                                             Sender: TIdSipTransport;
                                             Destination: TIdConnectionBindings);
begin
  Self.NotifyOfSentMessage(Request, Destination);
end;

procedure TIdSipStackInterface.OnSendResponse(Response: TIdSipResponse;
                                              Sender: TIdSipTransport;
                                              Destination: TIdConnectionBindings);
begin
  Self.NotifyOfSentMessage(Response, Destination);
end;

procedure TIdSipStackInterface.OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                                     Subscription: TIdSipInboundSubscription);
var
  Data: TIdSubscriptionRequestData;
begin
  Self.AddAction(Subscription);

  Data := TIdSubscriptionRequestData.Create;
  try
    Data.Handle       := Self.HandleFor(Subscription);
    Data.EventPackage := Subscription.EventPackage;
    Data.Request      := Subscription.InitialRequest;

    Self.NotifyEvent(CM_SUBSCRIPTION_REQUEST_NOTIFY, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnSuccess(RegisterAgent: TIdSipOutboundRegistrationBase;
                                         CurrentBindings: TIdSipContacts);
var
  Data: TIdRegistrationData;
begin
  Data := TIdRegistrationData.Create;
  try
    Data.Handle   := Self.HandleFor(RegisterAgent);
    Data.Contacts := CurrentBindings;

    Self.NotifyEvent(CM_SUCCESS, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnTerminated(Action: TIdSipAction);
begin
end;

procedure TIdSipStackInterface.RemoveAction(Handle: TIdSipHandle);
var
  I: Integer;
begin
  Self.ActionLock.Acquire;
  try
    I := Self.IndexOf(Handle);

    if (I <> ItemNotFoundIndex) then
      Self.Actions.Delete(I);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.SendAction(Action: TIdSipAction);
var
  Wait: TIdSipActionSendWait;
begin
  Wait := TIdSipActionSendWait.Create;
  Wait.ActionID := Action.ID;
  Self.UserAgent.ScheduleEvent(TriggerImmediately, Wait);
end;

procedure TIdSipStackInterface.StopListeningToAllTransports;
var
  I: Integer;
begin
  for I := 0 to Self.UserAgent.Dispatcher.TransportCount - 1 do begin
    Self.UserAgent.Dispatcher.Transports[I].RemoveTransportListener(Self);
    Self.UserAgent.Dispatcher.Transports[I].RemoveTransportSendingListener(Self);
  end;
end;

//******************************************************************************
//* TIdSipStackInterfaceExtension                                              *
//******************************************************************************
//* TIdSipStackInterfaceExtension Public methods *******************************

constructor TIdSipStackInterfaceExtension.Create(UA: TIdSipUserAgent);
begin
  inherited Create;

  Self.fUserAgent := UA;
end;

//******************************************************************************
//* TIdSipColocatedRegistrarExtension                                          *
//******************************************************************************
//* TIdSipColocatedRegistrarExtension Public methods ***************************

constructor TIdSipColocatedRegistrarExtension.Create(UA: TIdSipUserAgent);
begin
  inherited Create(UA);

  Assert(Self.UserAgent.UsesModule(TIdSipRegisterModule),
         'TIdSipColocatedRegistrarExtension needs a UA that supports receiving REGISTER methods');

  Self.RegisterModule := Self.UserAgent.ModuleFor(TIdSipRegisterModule) as TIdSipRegisterModule;
  Assert(Assigned(Self.RegisterModule.BindingDB), 'Register Module malformed: no BindingDatabase');

  Self.DB := Self.RegisterModule.BindingDB;
end;

procedure TIdSipColocatedRegistrarExtension.TargetsFor(URI: TIdSipUri; Targets: TIdSipContacts);
begin
  if not Self.DB.BindingsFor(URI, Targets, Self.DB.UseGruu) then begin
    // For now, do nothing: just return no valid targets.
    Targets.Clear;
  end;
end;

//******************************************************************************
//* TIdLoggingExtension                                                        *
//******************************************************************************
//* TIdLoggingExtension Public methods *****************************************

procedure TIdLoggingExtension.Log(Severity: TSeverityLevel;
                                  SourceDescription: String;
                                  RefCode: Cardinal;
                                  Description,
                                  BinaryData: String);
begin
  LogEntry(Description, SourceDescription, Severity, RefCode, BinaryData);
end;

//******************************************************************************
//* TIdSipNameServerExtension                                                  *
//******************************************************************************
//* TIdSipNameServerExtension Public methods ***********************************

function TIdSipNameServerExtension.LocalAddressFor(Destination: String): String;
begin
  // Destination may contain either a fully qualified domain name or an IPv4 or
  // IPv6 address.

  Self.AssertAddressWellFormed(Destination);

  Result := Self.UserAgent.RoutingTable.GetBestLocalAddress(Self.ResolveAddress(Destination));
end;

function TIdSipNameServerExtension.LocalOrMappedAddressFor(Destination: String): String;
begin
  Self.AssertAddressWellFormed(Destination);

  Result := Self.UserAgent.RoutingTable.LocalOrMappedAddressFor(Self.ResolveAddress(Destination));
end;

procedure TIdSipNameServerExtension.ResolveNamesFor(Host: String; IPAddresses: TIdDomainNameRecords);
begin
  Self.UserAgent.Locator.ResolveNameRecords(Host, IPAddresses);
end;

procedure TIdSipNameServerExtension.AssertAddressWellFormed(IPAddressOrHost: String);
const
  BadParameter     = '''%s'' is neither a domain name nor IP address';
begin
  if (IPAddressOrHost = '') then
    raise EBadParameter.Create(Format(BadParameter, [IPAddressOrHost]));

  if not TIdIPAddressParser.IsIPAddress(IPAddressOrHost) and not TIdSimpleParser.IsFQDN(IPAddressOrHost) then
    raise EBadParameter.Create(Format(BadParameter, [IPAddressOrHost]));
end;

function TIdSipNameServerExtension.ResolveAddress(IPAddressOrHost: String): String;
const
  UnresolvableName = 'Could not resolve name ''%s''';
var
  Names: TIdDomainNameRecords;
begin
  if TIdIPAddressParser.IsIPAddress(IPAddressOrHost) then begin
    Result := IPAddressOrHost
  end
  else if TIdSimpleParser.IsFQDN(IPAddressOrHost) then begin
    Names := TIdDomainNameRecords.Create;
    try
      Self.ResolveNamesFor(IPAddressOrHost, Names);
      if Names.IsEmpty then
        raise ENetworkError.Create(Format(UnresolvableName, [IPAddressOrHost]));

      Result := Names[0].IPAddress;
    finally
      Names.Free;
    end;
  end
end;

//******************************************************************************
//* TIdSipNetworkExtension                                                     *
//******************************************************************************
//* TIdSipNetworkExtension Public methods **************************************

procedure TIdSipNetworkExtension.GetBindings(Bindings: TIdSipLocations);
begin
  Self.UserAgent.Dispatcher.LocalBindings(Bindings);
end;

//******************************************************************************
//* TIdSipStatisticsExtension                                                  *
//******************************************************************************
//* TIdSipStatisticsExtension Public methods ***********************************

procedure TIdSipStatisticsExtension.CollectTransactionStatistics(Keys: TStringDictionary);
begin
  // TODO: Implement me!
end;

procedure TIdSipStatisticsExtension.CollectTransactionUserStatistics(Keys: TStringDictionary);
begin
  Self.UserAgent.Actions.Status(Keys);
end;

procedure TIdSipStatisticsExtension.CollectTransportStatistics(Keys: TStringDictionary);
begin
  // TODO: Implement me!
end;

procedure TIdSipStatisticsExtension.CollectAllStatistics(Keys: TStringDictionary);
begin
  Self.CollectTransactionStatistics(Keys);
  Self.CollectTransactionUserStatistics(Keys);
  Self.CollectTransportStatistics(Keys);
end;

//******************************************************************************
//* TIdEventData                                                               *
//******************************************************************************
//* TIdEventData Public methods ************************************************

constructor TIdEventData.Create;
begin
  inherited Create;
end;

procedure TIdEventData.Assign(Src: TPersistent);
var
  Other: TIdEventData;
begin
  if (Src is TIdEventData) then begin
    Other := Src as TIdEventData;
    Self.Handle := Other.Handle;
  end
  else
    inherited Assign(Src);
end;

function TIdEventData.AsString: String;
begin
  Result := Self.TimestampLine
          + Self.EventName + CRLF
          + Self.Data;
end;

function TIdEventData.Copy: TIdEventData;
begin
  Result := TIdEventDataClass(Self.ClassType).Create;
  Result.Assign(Self);
end;

//* TIdEventData Protected methods *********************************************

function TIdEventData.Data: String;
begin
  Result := '';
end;

function TIdEventData.EventName: String;
begin
  Result := '';
end;

//* TIdEventData Private methods ***********************************************

function TIdEventData.TimestampLine: String;
begin
  Result := FormatDateTime('yyyy/mm/dd hh:mm:ss', Now)
          + ' Handle: ' + IntToStr(Self.Handle) + CRLF;
end;

//******************************************************************************
//* TIdInformationalData                                                       *
//******************************************************************************
//* TIdInformationalData Public methods ****************************************

constructor TIdInformationalData.Create;
begin
  inherited Create;

  Self.ErrorCode := CallEndedSuccess;
end;

procedure TIdInformationalData.Assign(Src: TPersistent);
var
  Other: TIdInformationalData;
begin
  inherited Assign(Src);

  if (Src is TIdInformationalData) then begin
    Other := Src as TIdInformationalData;

    Self.ErrorCode := Other.ErrorCode;
    Self.Reason    := Other.Reason;
  end;
end;

function TIdInformationalData.NotAnError: Boolean;
begin
  Result := Self.ErrorCode = 0;
end;

//* TIdInformationalData Protected methods *************************************

function TIdInformationalData.Data: String;
begin
  Result := IntToStr(Self.ErrorCode) + ' ' + Self.Reason + CRLF;
end;

//* TIdInformationalData Private methods ***************************************

procedure TIdInformationalData.SetErrorCode(Value: Cardinal);
begin
  Self.fErrorCode := Value;

  // TODO: Look up the reason string corresponding to ErrorCode here.
end;

//******************************************************************************
//* TIdAuthenticationChallengeData                                             *
//******************************************************************************
//* TIdAuthenticationChallengeData Public methods ******************************

constructor TIdAuthenticationChallengeData.Create;
begin
  inherited Create;

  Self.fChallenge         := TIdSipResponse.Create;
  Self.fChallengedRequest := TIdSipRequest.Create;
end;

destructor TIdAuthenticationChallengeData.Destroy;
begin
  Self.fChallengedRequest.Free;
  Self.fChallenge.Free;

  inherited Destroy;
end;

procedure TIdAuthenticationChallengeData.Assign(Src: TPersistent);
var
  Other: TIdAuthenticationChallengeData;
begin
  inherited Assign(Src);

  if (Src is TIdAuthenticationChallengeData) then begin
    Other := Src as TIdAuthenticationChallengeData;

    Self.Challenge         := Other.Challenge;
    Self.ChallengedRequest := Other.ChallengedRequest;
  end;
end;

//* TIdAuthenticationChallengeData Protected methods ***************************

function TIdAuthenticationChallengeData.Data: String;
begin
  Result := 'CHALLENGED REQUEST' + CRLF
          + Self.ChallengedRequest.AsString
          + CRLF
          + 'CHALLENGE' + CRLF
          + Self.Challenge.AsString;
end;

function TIdAuthenticationChallengeData.EventName: String;
begin
  Result := EventNames(CM_AUTHENTICATION_CHALLENGE);
end;

//* TIdAuthenticationChallengeData Private methods *****************************

procedure TIdAuthenticationChallengeData.SetChallenge(Response: TIdSipResponse);
begin
  Self.fChallenge.Assign(Response);
end;

procedure TIdAuthenticationChallengeData.SetChallengedRequest(Request: TIdSipRequest);
begin
  Self.fChallengedRequest.Assign(Request);
end;

//******************************************************************************
//* TIdNetworkFailureData                                                      *
//******************************************************************************
//* TIdNetworkFailureData Protected methods ************************************

function TIdNetworkFailureData.EventName: String;
begin
  Result := EventNames(CM_NETWORK_FAILURE);
end;

//******************************************************************************
//* TIdCallEndedData                                                           *
//******************************************************************************
//* TIdCallEndedData Protected methods *****************************************

function TIdCallEndedData.EventName: String;
begin
  Result := EventNames(CM_CALL_ENDED);
end;

//******************************************************************************
//* TIdDebugData                                                               *
//******************************************************************************
//* TIdDebugData Public methods ************************************************

procedure TIdDebugData.Assign(Src: TPersistent);
var
  Other: TIdDebugData;
begin
  inherited Assign(Src);

  if (Src is TIdDebugData) then begin
    Other := Src as TIdDebugData;

    Self.Event := Other.Event;
  end;
end;

//* TIdDebugData Protected methods *********************************************

function TIdDebugData.EventName: String;
begin
  Result := EventNames(Self.Event);
end;

//******************************************************************************
//* TIdDebugMessageData                                                        *
//******************************************************************************
//* TIdDebugMessageData Public methods *****************************************

destructor TIdDebugMessageData.Destroy;
begin
  Self.fMessage.Free;

  inherited Destroy;
end;

procedure TIdDebugMessageData.Assign(Src: TPersistent);
var
  Other: TIdDebugMessageData;
begin
  inherited Assign(Src);

  if (Src is TIdDebugMessageData) then begin
    Other := Src as TIdDebugMessageData;
    
    Self.Message := Other.Message.Copy;
  end;
end;

//* TIdDebugMessageData Protected methods **************************************

function TIdDebugMessageData.Data: String;
begin
  Result := Self.Message.AsString;
end;

//******************************************************************************
//* TIdDebugDroppedMessageData                                                 *
//******************************************************************************
//* TIdDebugDroppedMessageData Public methods **********************************

destructor TIdDebugDroppedMessageData.Destroy;
begin
  Self.Binding.Free;

  inherited Destroy;
end;

procedure TIdDebugDroppedMessageData.Assign(Src: TPersistent);
var
  Other: TIdDebugDroppedMessageData;
begin
  inherited Assign(Src);

  if (Src is TIdDebugDroppedMessageData) then begin
    Other := Src as TIdDebugDroppedMessageData;

    Self.Binding := Other.Binding.Copy;
  end;
end;

//* TIdDebugDroppedMessageData Protected methods *******************************

function TIdDebugDroppedMessageData.Data: String;
begin
  Result := Self.Binding.AsString + CRLF
          + inherited Data;
end;

function TIdDebugDroppedMessageData.EventName;
begin
  Result := EventNames(CM_DEBUG_DROPPED_MSG);
end;

//******************************************************************************
//* TIdDebugReceiveMessageData                                                 *
//******************************************************************************
//* TIdDebugReceiveMessageData Public methods **********************************

destructor TIdDebugReceiveMessageData.Destroy;
begin
  Self.Binding.Free;

  inherited Destroy;
end;

procedure TIdDebugReceiveMessageData.Assign(Src: TPersistent);
var
  Other: TIdDebugReceiveMessageData;
begin
  inherited Assign(Src);

  if (Src is TIdDebugReceiveMessageData) then begin
    Other := Src as TIdDebugReceiveMessageData;

    Self.Binding := Other.Binding.Copy;
  end;
end;

//* TIdDebugReceiveMessageData Protected methods *******************************

function TIdDebugReceiveMessageData.Data: String;
begin
  Result := Self.Binding.AsString + CRLF
          + inherited Data;
end;

function TIdDebugReceiveMessageData.EventName: String;
begin
  Result := EventNames(CM_DEBUG_RECV_MSG);
end;

//******************************************************************************
//* TIdDebugSendMessageData                                                    *
//******************************************************************************
//* TIdDebugSendMessageData Public methods *************************************

destructor TIdDebugSendMessageData.Destroy;
begin
  Self.Binding.Free;

  inherited Destroy;
end;

procedure TIdDebugSendMessageData.Assign(Src: TPersistent);
var
  Other: TIdDebugSendMessageData;
begin
  inherited Assign(Src);

  if (Src is TIdDebugSendMessageData) then begin
    Other := Src as TIdDebugSendMessageData;

    Self.Binding := Other.Binding.Copy;
  end;
end;

//* TIdDebugSendMessageData Protected methods **********************************

function TIdDebugSendMessageData.Data: String;
begin
  Result := Self.Binding.AsString + CRLF
          + inherited Data;
end;

function TIdDebugSendMessageData.EventName: String;
begin
  Result := EventNames(CM_DEBUG_SEND_MSG);
end;

//******************************************************************************
//* TIdDebugExceptionData                                                      *
//******************************************************************************
//* TIdDebugExceptionData Public methods ***************************************

procedure TIdDebugExceptionData.Assign(Src: TPersistent);
var
  Other: TIdDebugExceptionData;
begin
  inherited Assign(Src);

  if (Src is TIdDebugExceptionData) then begin
    Other := Src as TIdDebugExceptionData;

    Self.Error  := Other.Error;
    Self.Reason := Other.Reason;
  end;
end;

//* TIdDebugExceptionData Protected methods ************************************

function TIdDebugExceptionData.Data: String;
begin
  Result := Self.Error + ': ' + Self.Reason + CRLF;
end;

function TIdDebugExceptionData.EventName: String;
begin
  Result := EventNames(CM_DEBUG_EXCEPTION);
end;

//******************************************************************************
//* TIdDebugTransportExceptionData                                             *
//******************************************************************************
//* TIdDebugTransportExceptionData Protected methods ***************************

function TIdDebugTransportExceptionData.EventName: String;
begin
  Result := EventNames(CM_DEBUG_TRANSPORT_EXCEPTION);
end;

//******************************************************************************
//* TIdDebugWaitExceptionData                                                  *
//******************************************************************************
//* TIdDebugWaitExceptionData Public methods ***********************************

destructor TIdDebugWaitExceptionData.Destroy;
begin
  Self.Wait.Free;

  inherited Destroy;
end;

procedure TIdDebugWaitExceptionData.Assign(Src: TPersistent);
var
  Other: TIdDebugWaitExceptionData;
begin
  inherited Assign(Src);

  if (Src is TIdDebugWaitExceptionData) then begin
    Other := Src as TIdDebugWaitExceptionData;

    Self.Wait   := Other.Wait;
  end;
end;

//* TIdDebugWaitExceptionData Protected methods ********************************

function TIdDebugWaitExceptionData.Data: String;
var
  WaitType: String;
begin
  if Assigned(Self.Wait) then
    WaitType := Self.Wait.ClassName
  else
    WaitType := 'nil';

  Result := inherited Data
          + 'Wait type: ' + WaitType + CRLF;
end;

function TIdDebugWaitExceptionData.EventName: String;
begin
  Result := EventNames(CM_DEBUG_WAIT_EXCEPTION);
end;

//* TIdDebugWaitExceptionData Private methods **********************************

procedure TIdDebugWaitExceptionData.SetWait(Value: TIdWait);
begin
  Self.fWait.Free;

  if Assigned(Value) then
    Self.fWait := Value.Copy
  else
    Self.fWait := nil;
end;

//******************************************************************************
//* TIdDebugTransportRejectedMessageData                                       *
//******************************************************************************
//* TIdDebugTransportRejectedMessageData Public methods ************************

destructor TIdDebugTransportRejectedMessageData.Destroy;
begin
  Self.Binding.Free;

  inherited Destroy;
end;

procedure TIdDebugTransportRejectedMessageData.Assign(Src: TPersistent);
var
  Other: TIdDebugTransportRejectedMessageData;
begin
  inherited Assign(Src);

  if (Src is TIdDebugTransportRejectedMessageData) then begin
    Other := Src as TIdDebugTransportRejectedMessageData;

    Self.Binding := Other.Binding.Copy;
    Self.Msg     := Other.Msg;
    Self.Reason  := Other.Reason;
  end;
end;

//* TIdDebugTransportRejectedMessageData Protected methods *********************

function TIdDebugTransportRejectedMessageData.Data: String;
begin
  Result := Self.Binding.AsString + CRLF
          + Self.Reason + CRLF
          + Self.Msg;
end;

function TIdDebugTransportRejectedMessageData.EventName: String;
begin
  Result := EventNames(CM_DEBUG_TRANSPORT_REJECTED_MSG);
end;

//******************************************************************************
//* TIdQueryOptionsData                                                        *
//******************************************************************************
//* TIdQueryOptionsData Public methods *****************************************

constructor TIdQueryOptionsData.Create;
begin
  inherited Create;

  Self.fResponse := TIdSipResponse.Create;
end;

destructor TIdQueryOptionsData.Destroy;
begin
  Self.Response.Free;

  inherited Destroy;
end;

procedure TIdQueryOptionsData.Assign(Src: TPersistent);
var
  Other: TIdQueryOptionsData;
begin
  inherited Assign(Src);

  if (Src is TIdQueryOptionsData) then begin
    Other := Src as TIdQueryOptionsData;

    Self.Response := Other.Response;
  end;
end;

//* TIdQueryOptionsData Protected methods **************************************

function TIdQueryOptionsData.Data: String;
begin
  Result := 'Response:' + CRLF
          + Self.Response.AsString;
end;

function TIdQueryOptionsData.EventName: String;
begin
  Result := EventNames(CM_QUERY_OPTIONS_RESPONSE);
end;

//* TIdQueryOptionsData Private methods ****************************************

procedure TIdQueryOptionsData.SetResponse(Value: TIdSipResponse);
begin
  Self.fResponse.Assign(Value);
end;

//******************************************************************************
//* TIdRegistrationData                                                        *
//******************************************************************************
//* TIdRegistrationData Public methods *****************************************

constructor TIdRegistrationData.Create;
begin
  inherited Create;

  Self.fContacts := TIdSipContacts.Create;
end;

destructor TIdRegistrationData.Destroy;
begin
  Self.fContacts.Free;

  inherited Destroy;
end;

procedure TIdRegistrationData.Assign(Src: TPersistent);
var
  Other: TIdRegistrationData;
begin
  inherited Assign(Src);

  if (Src is TIdRegistrationData) then begin
    Other := Src as TIdRegistrationData;

    Self.Contacts := Other.Contacts;
  end;
end;

//* TIdRegistrationData Protected methods **************************************

function TIdRegistrationData.Data: String;
begin
  Result := Self.Contacts.AsString;
end;

function TIdRegistrationData.EventName: String;
begin
  Result := EventNames(CM_SUCCESS) + ' Registration';
end;

//* TIdRegistrationData Private methods ****************************************

procedure TIdRegistrationData.SetContacts(Value: TIdSipContacts);
begin
  Self.Contacts.Clear;
  Self.Contacts.Add(Value);
end;

//******************************************************************************
//* TIdFailedRegistrationData                                                  *
//******************************************************************************
//* TIdFailedRegistrationData Public methods ***********************************

constructor TIdFailedRegistrationData.Create;
begin
  inherited Create;

  Self.RegistrationData := TIdRegistrationData.Create;
end;

destructor TIdFailedRegistrationData.Destroy;
begin
  Self.RegistrationData.Free;

  inherited Destroy;
end;

procedure TIdFailedRegistrationData.Assign(Src: TPersistent);
var
  Other: TIdFailedRegistrationData;
begin
  inherited Assign(Src);

  if (Src is TIdFailedRegistrationData) then begin
    Other := Src as TIdFailedRegistrationData;

    Self.ErrorCode := Other.ErrorCode;
    Self.Reason    := Other.Reason;
  end;
end;

//* TIdFailedRegistrationData Protected methods ********************************

function TIdFailedRegistrationData.EventName: String;
begin
  Result := EventNames(CM_FAIL) + ' Registration';
end;

//******************************************************************************
//* TIdSessionData                                                             *
//******************************************************************************
//* TIdSessionData Public methods **********************************************

constructor TIdSessionData.Create;
begin
  inherited Create;

  Self.fLocalContact  := TIdSipContactHeader.Create;
  Self.fLocalParty    := TIdSipAddressHeader.Create;
  Self.fRemoteContact := TIdSipContactHeader.Create;
  Self.fRemoteParty   := TIdSipAddressHeader.Create;
end;

destructor TIdSessionData.Destroy;
begin
  Self.fRemoteParty.Free;
  Self.fRemoteContact.Free;
  Self.fLocalParty.Free;
  Self.fLocalContact.Free;

 inherited Destroy;
end;

procedure TIdSessionData.Assign(Src: TPersistent);
var
  Other: TIdSessionData;
begin
  inherited Assign(Src);

  if (Src is TIdSessionData) then begin
    Other := Src as TIdSessionData;

    Self.LocalContact             := Other.LocalContact;
    Self.LocalMimeType            := Other.LocalMimeType;
    Self.LocalParty               := Other.LocalParty;    
    Self.LocalSessionDescription  := Other.LocalSessionDescription;
    Self.RemoteContact            := Other.RemoteContact;
    Self.RemoteMimeType           := Other.RemoteMimeType;
    Self.RemoteParty              := Other.RemoteParty;
    Self.RemoteSessionDescription := Other.RemoteSessionDescription;
  end;
end;

//* TIdSessionData Protected methods *******************************************

function TIdSessionData.Data: String;
begin
  Result := 'Local party: ' + Self.LocalParty.FullValue + CRLF
          + 'Local contact: ' + Self.LocalContact.FullValue + CRLF
          + 'Remote party: ' + Self.RemoteParty.FullValue + CRLF
          + 'Remote contact: ' + Self.RemoteContact.FullValue + CRLF
          + 'Local session description (' + Self.LocalMimeType + '):' + CRLF
          + Self.LocalSessionDescription + CRLF
          + 'Remote session description (' + Self.RemoteMimeType + '):' + CRLF
          + Self.RemoteSessionDescription + CRLF
end;

//* TIdSessionData Private methods *********************************************

procedure TIdSessionData.SetLocalContact(Value: TIdSipContactHeader);
begin
  Self.LocalContact.Assign(Value);
end;

procedure TIdSessionData.SetLocalParty(Value: TIdSipAddressHeader);
begin
  Self.LocalParty.Assign(Value);

  if Self.LocalParty.HasParameter(TagParam) then
    Self.LocalParty.RemoveParameter(TagParam);
end;

procedure TIdSessionData.SetRemoteContact(Value: TIdSipContactHeader);
begin
  Self.RemoteContact.Assign(Value);
end;

procedure TIdSessionData.SetRemoteParty(Value: TIdSipAddressHeader);
begin
  Self.RemoteParty.Assign(Value);

  if Self.RemoteParty.HasParameter(TagParam) then
    Self.RemoteParty.RemoveParameter(TagParam);
end;

//******************************************************************************
//* TIdEstablishedSessionData                                                  *
//******************************************************************************
//* TIdEstablishedSessionData Public methods ***********************************


constructor TIdEstablishedSessionData.Create;
begin
  inherited Create;

  Self.fID := TIdSipDialogID.Create;
end;

destructor TIdEstablishedSessionData.Destroy;
begin
  Self.fID.Free;

  inherited Destroy;
end;

procedure TIdEstablishedSessionData.Assign(Src: TPersistent);
var
  Other: TIdEstablishedSessionData;
begin
  inherited Assign(Src);

  if (Src is TIdEstablishedSessionData) then begin
    Other := Src as TIdEstablishedSessionData;

    Self.ID := Other.ID;
  end;
end;

//* TIdEstablishedSessionData Protected methods ********************************

function TIdEstablishedSessionData.EventName: String;
begin
  Result := EventNames(CM_CALL_ESTABLISHED);
end;

//* TIdEstablishedSessionData Private methods **********************************

procedure TIdEstablishedSessionData.SetID(Value: TIdSipDialogID);
begin
  Self.fID.CallID    := Value.CallID;
  Self.fID.LocalTag  := Value.LocalTag;
  Self.fID.RemoteTag := Value.RemoteTag;
end;

//******************************************************************************
//* TIdInboundCallData                                                         *
//******************************************************************************

constructor TIdInboundCallData.Create;
begin
  inherited Create;

  Self.fInvite := TIdSipRequest.Create; 
end;

destructor TIdInboundCallData.Destroy;
begin
  Self.Invite.Free;

  inherited Destroy;
end;

procedure TIdInboundCallData.Assign(Src: TPersistent);
var
  Other: TIdInboundCallData;
begin
  inherited Assign(Src);

  if (Src is TIdInboundCallData) then begin
    Other := Src as TIdInboundCallData;

    Self.Invite := Other.Invite;
  end;
end;

//* TIdInboundCallData Protected methods ***************************************

function TIdInboundCallData.EventName: String;
begin
  Result := EventNames(CM_CALL_REQUEST_NOTIFY);
end;

//* TIdInboundCallData Private methods *****************************************

procedure TIdInboundCallData.SetInvite(Value: TIdSipRequest);
begin
  Self.fInvite.Assign(Value);
end;

//******************************************************************************
//* TIdModifiedSessionData                                                     *
//******************************************************************************
//* TIdModifiedSessionData Protected methods ***********************************

function TIdModifiedSessionData.EventName: String;
begin
  Result := EventNames(CM_CALL_OUTBOUND_MODIFY_SUCCESS);
end;

//******************************************************************************
//* TIdModifySessionData                                                       *
//******************************************************************************
//* TIdModifySessionData Protected methods *************************************

function TIdModifySessionData.EventName: String;
begin
  Result := EventNames(CM_CALL_REMOTE_MODIFY_REQUEST);
end;

//******************************************************************************
//* TIdSessionProgressData                                                     *
//******************************************************************************
//* TIdSessionProgressData Public methods **************************************

procedure TIdSessionProgressData.Assign(Src: TPersistent);
var
  Other: TIdSessionProgressData;
begin
  inherited Assign(Src);

  if (Src is TIdSessionProgressData) then begin
    Other := Src as TIdSessionProgressData;

    Self.Banner       := Other.Banner;
    Self.ProgressCode := Other.ProgressCode;
  end;
end;

//* TIdSessionProgressData Protected methods ***********************************

function TIdSessionProgressData.Data: String;
begin
  Result := IntToStr(Self.ProgressCode) + ' ' + Self.Banner + CRLF
          + inherited Data;
end;

function TIdSessionProgressData.EventName: String;
begin
  Result := EventNames(CM_CALL_PROGRESS);
end;

//******************************************************************************
//* TIdSubscriptionData                                                        *
//******************************************************************************
//* TIdSubscriptionData Public methods *****************************************

procedure TIdSubscriptionData.Assign(Src: TPersistent);
var
  Other: TIdSubscriptionData;
begin
  inherited Assign(Src);

  if (Src is TIdSubscriptionData) then begin
    Other := Src as TIdSubscriptionData;

    Self.EventPackage  := Other.EventPackage;
  end;
end;

//* TIdSubscriptionData Protected methods **************************************

function TIdSubscriptionData.Data: String;
begin
  Result := 'Event: ' + Self.EventPackage + CRLF;
end;

function TIdSubscriptionData.GetEventPackage: String;
begin
  Result := Self.fEventPackage;
end;

procedure TIdSubscriptionData.SetEventPackage(Value: String);
begin
  Self.fEventPackage := Value;
end;

//******************************************************************************
//* TIdSubscriptionRequestData                                                 *
//******************************************************************************
//* TIdSubscriptionRequestData Public methods **********************************

constructor TIdSubscriptionRequestData.Create;
begin
  inherited Create;

  Self.fRequest := TIdSipRequest.Create;
end;

destructor TIdSubscriptionRequestData.Destroy;
begin
  Self.fRequest.Free;

  inherited Destroy;
end;

procedure TIdSubscriptionRequestData.Assign(Src: TPersistent);
var
  Other: TIdSubscriptionRequestData;
begin
  inherited Assign(Src);

  if (Src is TIdSubscriptionRequestData) then begin
    Other := Src as TIdSubscriptionRequestData;

    Self.Request := Other.Request;
  end;
end;

//* TIdSubscriptionRequestData Protected methods *******************************

function TIdSubscriptionRequestData.Data: String;
begin
  Result := inherited Data
          + Self.ReferTo.AsString + CRLF
          + Self.From.AsString + CRLF
          + Self.RemoteContact.AsString + CRLF;
end;

function TIdSubscriptionRequestData.EventName: String;
begin
  Result := EventNames(CM_SUBSCRIPTION_REQUEST_NOTIFY);
end;

function TIdSubscriptionRequestData.GetEventPackage: String;
begin
  Result := Self.Request.Event.EventType;
end;

procedure TIdSubscriptionRequestData.SetEventPackage(Value: String);
begin
  // Do nothing.
end;

//* TIdSubscriptionRequestData Private methods *********************************

function TIdSubscriptionRequestData.GetFrom: TIdSipFromHeader;
begin
  Result := Self.Request.From;
end;

function TIdSubscriptionRequestData.GetReferTo: TIdSipReferToHeader;
begin
  Result := Self.Request.ReferTo;
end;

function TIdSubscriptionRequestData.GetRemoteContact: TIdSipContactHeader;
begin
  Result := Self.Request.FirstContact;
end;

function TIdSubscriptionRequestData.GetTarget: TIdSipUri;
begin
  Result := Self.Request.RequestUri;
end;

procedure TIdSubscriptionRequestData.SetRequest(Value: TIdSipRequest);
begin
  Self.Request.Assign(Value);
end;

//******************************************************************************
//* TIdSessionReferralData                                                     *
//******************************************************************************
//* TIdSessionReferralData Public methods **************************************

procedure TIdSessionReferralData.Assign(Src: TPersistent);
var
  Other: TIdSessionReferralData;
begin
  inherited Assign(Src);

  if (Src is TIdSessionReferralData) then begin
    Other := Src as TIdSessionReferralData;
    Self.ReferAction := Other.ReferAction;
  end;
end;

//******************************************************************************
//* TIdSessionReferralData
//******************************************************************************
//* TIdSessionReferralData Protected methods ***********************************

function TIdSessionReferralData.Data: String;
begin
  Result := 'Refer action: ' + IntToStr(Self.ReferAction) + CRLF
          + inherited Data;
end;

function TIdSessionReferralData.EventName: String;
begin
  Result := EventNames(CM_CALL_REFERRAL);
end;

//******************************************************************************
//* TIdSubscriptionNotifyData                                                  *
//******************************************************************************
//* TIdSubscriptionNotifyData Public methods ***********************************

constructor TIdSubscriptionNotifyData.Create;
begin
  inherited Create;

  Self.fNotify := TIdSipRequest.Create;
end;

destructor TIdSubscriptionNotifyData.Destroy;
begin
  Self.fNotify.Free;

  inherited Destroy;
end;

procedure TIdSubscriptionNotifyData.Assign(Src: TPersistent);
var
  Other: TIdSubscriptionNotifyData;
begin
  inherited Assign(Src);

  if (Src is TIdSubscriptionNotifyData) then begin
    Other := Src as TIdSubscriptionNotifyData;

    Self.Notify := Other.Notify;
  end;
end;

//* TIdSubscriptionNotifyData Protected methods ********************************

function TIdSubscriptionNotifyData.Data: String;
begin
  Result := Self.Notify.AsString;
end;

function TIdSubscriptionNotifyData.EventName: String;
begin
  Result := EventNames(Self.Event);
end;

//* TIdSubscriptionNotifyData Private methods **********************************

procedure TIdSubscriptionNotifyData.SetNotify(Value: TIdSipRequest);
begin
  Self.fNotify.Assign(Value);
end;

//******************************************************************************
//* TIdFailedSubscriptionData                                                  *
//******************************************************************************
//* TIdFailedSubscriptionData Public methods ***********************************

constructor TIdFailedSubscriptionData.Create;
begin
  inherited Create;

  Self.fResponse := TIdSipResponse.Create;
end;

destructor TIdFailedSubscriptionData.Destroy;
begin
  inherited Destroy;
end;

procedure TIdFailedSubscriptionData.Assign(Src: TPersistent);
var
  Other: TIdFailedSubscriptionData;
begin
  inherited Assign(Src);

  if (Src is TIdFailedSubscriptionData) then begin
    Other := Src as TIdFailedSubscriptionData;

    Self.ErrorCode := Other.ErrorCode;
    Self.Reason    := Other.Reason;
    Self.Response  := Other.Response;
  end;
end;

//* TIdFailedSubscriptionData Protected methods ********************************

function TIdFailedSubscriptionData.Data: String;
begin
  Result := Self.Response.AsString;
end;

function TIdFailedSubscriptionData.EventName: String;
begin
  Result := EventNames(CM_FAIL) + ' Subscription';
end;

//* TIdFailedSubscriptionData Private methods **********************************

procedure TIdFailedSubscriptionData.SetResponse(Value: TIdSipResponse);
begin
  Self.Response.Assign(Value);
end;

//******************************************************************************
//* TIdStackReconfiguredData                                                   *
//******************************************************************************
//* TIdStackReconfiguredData Public methods ************************************

constructor TIdStackReconfiguredData.Create;
begin
  inherited Create;

  Self.fBindings         := TIdSipLocations.Create;
  Self.fRawConfiguration := TStringList.Create;
end;

destructor TIdStackReconfiguredData.Destroy;
begin
  Self.fRawConfiguration.Free;
  Self.fBindings.Free;

  inherited Destroy;
end;

procedure TIdStackReconfiguredData.Assign(Src: TPersistent);
var
  Other: TIdStackReconfiguredData;
begin
  inherited Assign(Src);

  if (Src is TIdStackReconfiguredData) then begin
    Other := Src as TIdStackReconfiguredData;

    Self.ActsAsRegistrar  := Other.ActsAsRegistrar;
    Self.Bindings         := Other.Bindings;
    Self.RawConfiguration := Other.RawConfiguration;
    Self.RoutingTableType := Other.RoutingTableType;
  end;
end;

//* TIdStackReconfiguredData Protected methods *********************************

function TIdStackReconfiguredData.Data: String;
begin
  Result := 'RawConfiguration:' + CRLF
          + Self.RawConfiguration.Text + CRLF
          + 'ActsAsRegistrar: ' + BoolToStr(Self.ActsAsRegistrar, true) + CRLF
          + Self.Bindings.AsStringWithPrefix('Binding: ') + CRLF
          + 'RoutingTableType: ' + Self.RoutingTableType + CRLF;
end;

function TIdStackReconfiguredData.EventName: String;
begin
  Result := EventNames(CM_STACK_RECONFIGURED);
end;

//* TIdStackReconfiguredData Private methods ***********************************

procedure TIdStackReconfiguredData.SetBindings(Value: TIdSipLocations);
begin
  Self.Bindings.Clear;
  Self.Bindings.AddLocations(Value);
end;

procedure TIdStackReconfiguredData.SetRawConfiguration(Value: TStrings);
begin
  Self.fRawConfiguration.Assign(Value);
end;

//******************************************************************************
//* TIdAsynchronousMessageResultData                                           *
//******************************************************************************
//* TIdAsynchronousMessageResultData Public methods ****************************

procedure TIdAsynchronousMessageResultData.Assign(Src: TPersistent);
var
  Other: TIdAsynchronousMessageResultData;
begin
  inherited Assign(Src);

  if (Src is TIdAsynchronousMessageResultData) then begin
    Other := Src as TIdAsynchronousMessageResultData;

    Self.ReferenceID := Other.ReferenceID;
  end;
end;

//* TIdAsynchronousMessageResultData Protected methods *************************

function TIdAsynchronousMessageResultData.Data: String;
begin
  Result := 'ReferenceID: ' + Self.ReferenceID + CRLF;
end;

function TIdAsynchronousMessageResultData.EventName: String;
begin
  Result := EventNames(CM_ASYNC_MSG_RESULT);
end;

//******************************************************************************
//* TIdBooleanResultData                                                       *
//******************************************************************************
//* TIdBooleanResultData Public methods ****************************************

procedure TIdBooleanResultData.Assign(Src: TPersistent);
var
  Other: TIdBooleanResultData;
begin
  inherited Assign(Src);

  if (Src is TIdBooleanResultData) then begin
    Other := Src as TIdBooleanResultData;

    Self.Result := Other.Result;
  end;
end;

//* TIdBooleanResultData Protected methods *************************************

function TIdBooleanResultData.Data: String;
begin
  Result := inherited Data
          + 'Result: ' + BoolToStr(Self.Result, true) + CRLF;
end;

//******************************************************************************
//* TIdDomainNameRecordsResultData                                             *
//******************************************************************************
//* TIdDomainNameRecordsResultData Public methods ******************************

constructor TIdDomainNameRecordsResultData.Create;
begin
  inherited Create;

  Self.fIPAddresses := TIdDomainNameRecords.Create;
end;

destructor TIdDomainNameRecordsResultData.Destroy;
begin
  Self.IPAddresses.Free;

  inherited Destroy;
end;

procedure TIdDomainNameRecordsResultData.Assign(Src: TPersistent);
var
  Other: TIdDomainNameRecordsResultData;
begin
  inherited Assign(Src);

  if (Src is TIdDomainNameRecordsResultData) then begin
    Other := Src as TIdDomainNameRecordsResultData;

    Self.IPAddresses := Other.IPAddresses;
  end;
end;

//* TIdDomainNameRecordsResultData Protected methods ***************************

function TIdDomainNameRecordsResultData.Data: String;
var
  I: Integer;
begin
  Result := inherited Data;

  for I := 0 to Self.IPAddresses.Count - 1 do
    Result := Result
            + 'IPAddress' + IntToStr(I) + ': ' + Self.IPAddresses[I].AsString + CRLF;
end;

//* TIdDomainNameRecordsResultData Private methods *****************************

procedure TIdDomainNameRecordsResultData.SetIPAddresses(Value: TIdDomainNameRecords);
var
  I: Integer;
begin
  for I := 0 to Value.Count - 1 do
    Self.fIPAddresses.Add(Value[I]);
end;

//******************************************************************************
//* TIdGetBindingsData                                                         *
//******************************************************************************
//* TIdGetBindingsData Public methods ******************************************

constructor TIdGetBindingsData.Create;
begin
  inherited Create;

  Self.fBindings := TIdSipLocations.Create;
end;

destructor TIdGetBindingsData.Destroy;
begin
  Self.Bindings.Free;

  inherited Destroy;
end;

procedure TIdGetBindingsData.Assign(Src: TPersistent);
var
  Other: TIdGetBindingsData;
begin
  inherited Assign(Src);

  if (Src is TIdGetBindingsData) then begin
    Other := Src as TIdGetBindingsData;

    Self.Bindings := Other.Bindings;
  end;
end;

//* TIdGetBindingsData Protected methods ***************************************

function TIdGetBindingsData.Data: String;
var
  I: Integer;
begin
  Result := inherited Data;

  for I := 0 to Self.Bindings.Count - 1 do
    Result := Result
            + 'Binding' + IntToStr(I) + ': ' + Self.Bindings[I].AsString + CRLF;
end;

//* TIdGetBindingsData Private methods *****************************************

procedure TIdGetBindingsData.SetBindings(Value: TIdSipLocations);
begin
  Self.fBindings.Clear;
  Self.fBindings.AddLocations(Value);
end;

//******************************************************************************
//* TIdStringResultData                                                        *
//******************************************************************************
//* TIdStringResultData Public methods *****************************************

procedure TIdStringResultData.Assign(Src: TPersistent);
var
  Other: TIdStringResultData;
begin
  inherited Assign(Src);

  if (Src is TIdStringResultData) then begin
    Other := Src as TIdStringResultData;

    Self.Result := Other.Result;
  end;
end;

//* TIdStringResultData Protected methods **************************************

function TIdStringResultData.Data: String;
begin
  Result := inherited Data
          + 'Result: ' + Self.Result + CRLF;
end;

//******************************************************************************
//* TIdStringDictionaryResultData                                              *
//******************************************************************************
//* TIdStringDictionaryResultData Public methods *******************************

constructor TIdStringDictionaryResultData.Create;
begin
  inherited Create;

  Self.fResult := TStringDictionary.Create;
end;

destructor TIdStringDictionaryResultData.Destroy;
begin
  Self.fResult.Free;

  inherited Create;
end;

procedure TIdStringDictionaryResultData.Assign(Src: TPersistent);
var
  Other: TIdStringDictionaryResultData;
begin
  inherited Assign(Src);

  if (Src is TIdStringDictionaryResultData) then begin
    Other := Src as TIdStringDictionaryResultData;

    Self.Result := Other.Result;
  end;
end;

//* TIdStringDictionaryResultData Protected methods ****************************

function TIdStringDictionaryResultData.Data: String;
begin
  Result := inherited Data
          + 'Result:' + CRLF
          + Self.DictionaryAsString(Self.Result) + CRLF;
end;

//* TIdStringDictionaryResultData Private methods ******************************

function TIdStringDictionaryResultData.DictionaryAsString(D: TStringDictionary): String;
var
  I:    Integer;
  Keys: TStrings;
begin
  Result := '';

  Keys := TStringList.Create;
  try
    D.CollectKeys(Keys);

    for I := 0 to Keys.Count - 1 do
      Result := Result + Format('  "%s": "%s"', [Keys[I], D.Find(Keys[I])]) + CRLF;
  finally
    Keys.Free;
  end;

  Result := System.Copy(Result, 1, Length(Result) - Length(CRLF));
end;

procedure TIdStringDictionaryResultData.SetResult(Value: TStringDictionary);
begin
  Self.Result.Clear;
  Self.Result.AddKeys(Value);
end;

//******************************************************************************
//* TIdSipStackInterfaceEventMethod                                            *
//******************************************************************************
//* TIdSipStackInterfaceEventMethod Public methods *****************************

procedure TIdSipStackInterfaceEventMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipStackListener).OnEvent(Self.Stack,
                                           Self.Event,
                                           Self.Data);
end;

//******************************************************************************
//* TIdStackWait                                                               *
//******************************************************************************
//* TIdStackWait Public methods ************************************************

procedure TIdStackWait.Trigger;
var
  O: TObject;
begin
  O := TIdObjectRegistry.FindObject(Self.StackID);

  if Assigned(O) and (O is TIdSipStackInterface) then
    Self.TriggerOnStack(O as TIdSipStackInterface);
end;

//* TIdStackWait Protected methods *********************************************

procedure TIdStackWait.TriggerOnStack(Stack: TIdSipStackInterface);
begin
  // By default, do nothing.
end;

//******************************************************************************
//* TIdStackShutdownWait                                                       *
//******************************************************************************
//* TIdStackShutdownWait Protected methods *************************************

procedure TIdStackShutdownWait.TriggerOnStack(Stack: TIdSipStackInterface);
begin
  Stack.Free;
end;

//******************************************************************************
//* TIdSipStackReconfigureStackInterfaceWait                                   *
//******************************************************************************
//* TIdSipStackReconfigureStackInterfaceWait Public methods ********************

constructor TIdSipStackReconfigureStackInterfaceWait.Create;
begin
  inherited Create;

  Self.fConfiguration := TStringList.Create;
end;

destructor TIdSipStackReconfigureStackInterfaceWait.Destroy;
begin
  Self.Configuration.Free;

  inherited Destroy;
end;

//* TIdSipStackReconfigureStackInterfaceWait Protected methods *****************

procedure TIdSipStackReconfigureStackInterfaceWait.TriggerOnStack(Stack: TIdSipStackInterface);
var
  SubMod:       TIdSipSubscribeModule;
  Configurator: TIdSipStackConfigurator;
begin
  // The configuration file can contain both configuration details defined by
  // TIdSipStackInterface and TIdSipUserAgent.

  Configurator := TIdSipStackConfigurator.Create;
  try
    Configurator.UpdateConfiguration(Stack.UserAgent, Self.Configuration);

    if Stack.UserAgent.UsesModule(TIdSipSubscribeModule) then begin
      SubMod := Stack.UserAgent.ModuleFor(TIdSipSubscribeModule) as TIdSipSubscribeModule;

      // "RemoveListener" first because SubMod may have existed before this
      // update. If it did, then Stack is already a Listener, and we wouldn't
      // want to re-add it. However, we've no way of knowing if Stack is
      // already a Listener.
      SubMod.RemoveListener(Stack);
      SubMod.AddListener(Stack);
    end;

    Stack.UserAgent.Dispatcher.StartAllTransports;
  finally
    Configurator.Free;
  end;

  Stack.Configure(Self.Configuration);
end;

//* TIdSipStackReconfigureStackInterfaceWait Private methods *******************

procedure TIdSipStackReconfigureStackInterfaceWait.SetConfiguration(Value: TStrings);
begin
  Self.Configuration.Assign(Value);
end;

//******************************************************************************
//* TIdAsynchronousMessageWait                                                 *
//******************************************************************************
//* TIdAsynchronousMessageWait Public methods **********************************

procedure TIdAsynchronousMessageWait.Trigger;
var
  Stack: TObject;
begin
  Stack := TIdObjectRegistry.FindObject(Self.StackID);

  if Assigned(Stack) and (Stack is TIdSipStackInterface) then
    Self.FireWait(Stack as TIdSipStackInterface);
end;

//* TIdAsynchronousMessageWait Protected methods *******************************

procedure TIdAsynchronousMessageWait.FireWait(Stack: TIdSipStackInterface);
begin
  // By default, do nothing.
end;

//******************************************************************************
//* TIdGetBindingsWait                                                         *
//******************************************************************************
//* TIdGetBindingsWait Protected methods ***************************************

procedure TIdGetBindingsWait.FireWait(Stack: TIdSipStackInterface);
var
  Data: TIdGetBindingsData;
  E:    TIdSipNetworkExtension;
begin
  Data := TIdGetBindingsData.Create;
  try
    Data.ReferenceID := Self.ID;

    E := Stack.AttachExtension(TIdSipNetworkExtension) as TIdSipNetworkExtension;
    try
      E.GetBindings(Data.Bindings);

      Stack.NotifyOfAsyncMessageResult(Data);
    finally
      E.Free;
    end;
  finally
    Data.Free;
  end;
end;

//******************************************************************************
//* TIdIsSourceOfWait                                                          *
//******************************************************************************
//* TIdIsSourceOfWait Public methods *******************************************

constructor TIdIsSourceOfWait.Create;
begin
  inherited Create;

  Self.fRequest := TIdSipRequest.Create;
end;

destructor TIdIsSourceOfWait.Destroy;
begin
  Self.fRequest.Free;

  inherited Destroy;
end;

//* TIdIsSourceOfWait Protected methods ****************************************

procedure TIdIsSourceOfWait.FireWait(Stack: TIdSipStackInterface);
var
  Result: TIdBooleanResultData;
begin
  Result := TIdBooleanResultData.Create;
  try
    Result.ReferenceID := Self.ID;
    Result.Result      := Stack.UserAgent.IsSourceOf(Self.Request);

    Stack.NotifyOfAsyncMessageResult(Result);
  finally
    Result.Free;
  end;
end;

//* TIdIsSourceOfWait Private methods ******************************************

procedure TIdIsSourceOfWait.SetRequest(Value: TIdSipRequest);
begin
  Self.fRequest.Assign(Value);
end;

//******************************************************************************
//* TIdNetworkingMessageWait                                                   *
//******************************************************************************
//* TIdNetworkingMessageWait Protected methods *********************************

procedure TIdNetworkingMessageWait.FireWait(Stack: TIdSipStackInterface);
var
  E: TIdSipNameServerExtension;
begin
  E := Stack.AttachExtension(TIdSipNameServerExtension) as TIdSipNameServerExtension;
  try
    Self.Ask(Stack, E);
  finally
    E.Free;
  end;
end;

procedure TIdNetworkingMessageWait.Ask(Stack: TIdSipStackInterface;
                                       E: TIdSipNameServerExtension);
begin
  // By default do nothing.
end;

//******************************************************************************
//* TIdLocalAddressForWait                                                     *
//******************************************************************************
//* TIdLocalAddressForWait Protected methods ***********************************

procedure TIdLocalAddressForWait.Ask(Stack: TIdSipStackInterface;
                                     E: TIdSipNameServerExtension);
var
  Result: TIdStringResultData;
begin
  Result := TIdStringResultData.Create;
  try
    Result.ReferenceID := Self.ID;
    Result.Result      := E.LocalAddressFor(Self.DestinationAddress);

    Stack.NotifyOfAsyncMessageResult(Result);
  finally
    Result.Free;
  end;
end;

//******************************************************************************
//* TIdLocalOrMappedAddressForWait                                             *
//******************************************************************************
//* TIdLocalOrMappedAddressForWait Protected methods ***************************

procedure TIdLocalOrMappedAddressForWait.Ask(Stack: TIdSipStackInterface;
                                             E: TIdSipNameServerExtension);
var
  Result: TIdStringResultData;
begin
  Result := TIdStringResultData.Create;
  try
    Result.ReferenceID := Self.ID;
    Result.Result      := E.LocalOrMappedAddressFor(Self.DestinationAddress);

    Stack.NotifyOfAsyncMessageResult(Result);
  finally
    Result.Free;
  end;
end;

//******************************************************************************
//* TIdResolveNamesForWait                                                     *
//******************************************************************************
//* TIdResolveNamesForWait Protected methods ***********************************

procedure TIdResolveNamesForWait.Ask(Stack: TIdSipStackInterface;
                                     E: TIdSipNameServerExtension);
var
  Result: TIdDomainNameRecordsResultData;
begin
  Result := TIdDomainNameRecordsResultData.Create;
  try
    Result.ReferenceID := Self.ID;

    E.ResolveNamesFor(Self.HostName, Result.IPAddresses);

    Stack.NotifyOfAsyncMessageResult(Result);
  finally
    Result.Free;
  end;
end;

//******************************************************************************
//* TIdCollectStatisticsWait                                                   *
//******************************************************************************
//* TIdCollectStatisticsWait Public methods ************************************

procedure TIdCollectStatisticsWait.FireWait(Stack: TIdSipStackInterface);
var
  E:      TIdSipStatisticsExtension;
  Result: TIdStringDictionaryResultData;
begin
  Result := TIdStringDictionaryResultData.Create;
  try
    Result.ReferenceID := Self.ID;

    E := Stack.AttachExtension(TIdSipStatisticsExtension) as TIdSipStatisticsExtension;
    try
      E.CollectAllStatistics(Result.Result);
    finally
      E.Free;
    end;

    Stack.NotifyOfAsyncMessageResult(Result);
  finally
    Result.Free;
  end;
end;

end.
