unit IdSipInviteModule;

interface

uses
  Classes, Contnrs, IdNotification, IdSipCore, IdSipDialog, IdSipDialogID,
  IdSipMessage, SyncObjs;

type
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

  TIdSipInboundSession = class;
  TIdSipInviteModule = class;

  IIdSipInviteModuleListener = interface(IIdSipMessageModuleListener)
    ['{E9D86376-16A4-4166-885C-03697B121F23}']
    procedure OnInboundCall(UserAgent: TIdSipInviteModule;
                            Session: TIdSipInboundSession);
  end;

  TIdSipOutboundSession = class;

  TIdSipInviteModule = class(TIdSipMessageModule)
  private
    fDoNotDisturb:           Boolean;
    fInitialResendInterval:  Cardinal; // in milliseconds
    fProgressResendInterval: Cardinal; // in milliseconds

    function  ConvertToHeader(ValueList: TStrings): String;
    function  HasTooManyReplaces(Request: TIdSipRequest): Boolean;
    function  MaybeAcceptReplaces(Request: TIdSipRequest;
                                  UsingSecureTransport: Boolean): TIdSipAction;
    procedure NotifyOfInboundCall(Session: TIdSipInboundSession);
    procedure TurnIntoInvite(OutboundRequest: TIdSipRequest;
                             const Offer: String;
                             const OfferMimeType: String);
  public
    constructor Create(UA: TIdSipAbstractCore); override;

    function  Accept(Request: TIdSipRequest;
                     UsingSecureTransport: Boolean): TIdSipAction; override;
    function  AddInboundInvite(Invite: TIdSipRequest;
                               UsingSecureTransport: Boolean): TIdSipInboundInvite;
    procedure AddListener(Listener: IIdSipInviteModuleListener);
    function  AddOutboundSession: TIdSipOutboundSession;
    function  AddOutboundSessionReplacer(Invite: TIdSipRequest): TIdSipOutboundSession;
    function  AllowedExtensions: String; override;
    function  Call(Dest: TIdSipAddressHeader;
                   const LocalSessionDescription: String;
                   const MimeType: String): TIdSipOutboundSession;
    function  CreateAck(Dialog: TIdSipDialog): TIdSipRequest;
    function  CreateBye(Dialog: TIdSipDialog): TIdSipRequest;
    function  CreateInvite(Dest: TIdSipAddressHeader;
                           const Body: String;
                           const MimeType: String): TIdSipRequest;
    function  CreateReInvite(Dialog: TIdSipDialog;
                             const Body: String;
                             const MimeType: String): TIdSipRequest;
    procedure RemoveListener(Listener: IIdSipInviteModuleListener);
    function  ReplaceCall(Invite: TIdSipRequest;
                          Dest: TIdSipAddressHeader;
                          const LocalSessionDescription: String;
                          const MimeType: String): TIdSipOutboundSession;

    property DoNotDisturb:           Boolean  read fDoNotDisturb write fDoNotDisturb;
    property InitialResendInterval:  Cardinal read fInitialResendInterval write fInitialResendInterval;
    property ProgressResendInterval: Cardinal read fProgressResendInterval write fProgressResendInterval;
  end;

  // I provide basic facilities for all Actions that need to handle INVITEs, BYEs, CANCELs,
  TIdSipInviteBase = class(TIdSipAction)
  protected
    Module: TIdSipInviteModule;

    procedure Initialise(UA: TIdSipAbstractCore;
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
    procedure Initialise(UA: TIdSipAbstractCore;
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
    procedure Initialise(UA: TIdSipAbstractCore;
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
    procedure RegisterFinalResponse(Response: TIdSipResponse);
    procedure SendAckFor(Response: TIdSipResponse;
                         UsingSecureTransport: Boolean);
    procedure SendBye(Response: TIdSipResponse;
                      UsingSecureTransport: Boolean);
    procedure SendCancel;
    procedure SetAckBody(Ack: TIdSipRequest);
  protected
    procedure ActionSucceeded(Response: TIdSipResponse); override;
    function  CreateInvite: TIdSipRequest; virtual; abstract;
    function  CreateNewAttempt: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    procedure NotifyOfFailure(Response: TIdSipResponse); override;
    function  ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionResult; override;
    function  ReceiveGlobalFailureResponse(Response: TIdSipResponse): TIdSipActionResult; override;
    function  ReceiveOKResponse(Response: TIdSipResponse;
                                UsingSecureTransport: Boolean): TIdSipActionResult; override;
    function  ReceiveProvisionalResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): TIdSipActionResult; override;
    function  ReceiveRedirectionResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): TIdSipActionResult; override;
    function  ReceiveServerFailureResponse(Response: TIdSipResponse): TIdSipActionResult; override;
  public
    destructor Destroy; override;

    procedure AddListener(const Listener: IIdSipInviteListener);
    procedure Cancel;
    function  Match(Msg: TIdSipMessage): Boolean; override;
    procedure RemoveListener(const Listener: IIdSipInviteListener);
    procedure Resend(AuthorizationCredentials: TIdSipAuthorizationHeader); override;
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

  // I implement the call flow around an out-of-dialog INVITE - an INVITE that
  // can create a dialog, in other words.
  TIdSipOutboundInitialInvite = class(TIdSipOutboundInvite)
  private
    fDestination: TIdSipAddressHeader;

    procedure SetDestination(Value: TIdSipAddressHeader);
  protected
    function  CreateInvite: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractCore;
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
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    destructor Destroy; override;

    property Contact:        TIdSipAddressHeader read fContact write SetContact;
    property OriginalInvite: TIdSipRequest       read fOriginalInvite write SetOriginalInvite;
  end;

  // I implement the call flow surrounding an in-dialog INVITE, that is, an
  // INVITE that changes the session description in some way.
  TIdSipOutboundReInvite = class(TIdSipOutboundInvite)
  private
    fOriginalInvite: TIdSipRequest;

    procedure SetOriginalInvite(Value: TIdSipRequest);
  protected
    function  CreateInvite: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractCore;
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
    procedure Initialise(UA: TIdSipAbstractCore;
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
    procedure Resend(AuthorizationCredentials: TIdSipAuthorizationHeader); override;

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
    procedure Initialise(UA: TIdSipAbstractCore;
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
    Cancelling:           Boolean;
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
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse); override;
    procedure OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                  NewDialog: TIdSipDialog); override;
    procedure OnFailure(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse;
                        const Reason: String); override;
    procedure OnRedirect(InviteAgent: TIdSipOutboundInvite;
                         Redirect: TIdSipResponse); override;
    procedure OnSuccess(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse); override;
  public
    constructor Create(UA: TIdSipAbstractCore); override;
    constructor CreateSessionReplacer(UA: TIdSipAbstractCore;
                                      Invite: TIdSipRequest);
    destructor Destroy; override;

    procedure Cancel;
    function  CanForkOn(Response: TIdSipResponse): Boolean;
    function  Match(Msg: TIdSipMessage): Boolean; override;
    function  ModifyWaitTime: Cardinal; override;
    procedure Resend(AuthorizationCredentials: TIdSipAuthorizationHeader); override;
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

  TIdSipInviteModuleInboundCallMethod = class(TIdNotification)
  private
    fSession:   TIdSipInboundSession;
    fUserAgent: TIdSipInviteModule;
  public
    procedure Run(const Subject: IInterface); override;

    property Session:   TIdSipInboundSession read fSession write fSession;
    property UserAgent: TIdSipInviteModule   read fUserAgent write fUserAgent;
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

implementation

uses
  IdRandom, IdSdp, IdSipSubscribeModule, IdSipUserAgent, IdSipTransaction, SysUtils;

// Exception messages
const
  CannotModifyBeforeEstablished  = 'Cannot modify a session before it''s fully established';
  CannotModifyDuringModification = 'Cannot modify a session while a modification is in progress';
  DoubleCancelSend               = 'SendCancel already invoked';
  PrematureInviteMessage         = 'Don''t attempt to modify the session before it''s fully established';

//******************************************************************************
//* TIdSipInviteModule                                                         *
//******************************************************************************
//* TIdSipInviteModule Public methods ******************************************

constructor TIdSipInviteModule.Create(UA: TIdSipAbstractCore);
begin
  inherited Create(UA);

  Self.AllowedContentTypeList.Add(SdpMimeType);
  Self.AcceptsMethodsList.Add(MethodAck);
  Self.AcceptsMethodsList.Add(MethodBye);
  Self.AcceptsMethodsList.Add(MethodCancel);
  Self.AcceptsMethodsList.Add(MethodInvite);

  Self.ProgressResendInterval := OneMinute*1000;
end;

function TIdSipInviteModule.Accept(Request: TIdSipRequest;
                                   UsingSecureTransport: Boolean): TIdSipAction;
var
  ExpectedStatusCode: Cardinal;
  Session:            TIdSipInboundSession;
begin
  Result := inherited Accept(Request, UsingSecureTransport);

  if Assigned(Result) then Exit;

  // BYEs, CANCELS, ACKS relate to an existing action - a Session - so we don't
  // make a new action for these types of messages.
  if not Request.IsInvite then begin
      if not Request.IsAck then
        Self.UserAgent.ReturnResponse(Request,
                                      SIPCallLegOrTransactionDoesNotExist);
      Exit;
  end;

  ExpectedStatusCode := Self.UserAgent.ResponseForInvite;
  if (ExpectedStatusCode <> SIPOK) then begin
    Result := nil;
    Self.UserAgent.ReturnResponse(Request, ExpectedStatusCode);
  end
  else begin
    if Request.HasReplaces then begin
      Result := Self.MaybeAcceptReplaces(Request, UsingSecureTransport);
    end
    else begin
      Session := TIdSipInboundSession.CreateInbound(Self.UserAgent,
                                                    Request,
                                                    UsingSecureTransport);

      // TODO: This is oh so wrong! Why? Because the Core knows about inbound
      // calls, even if the Core's a Registrar. And it shouldn't.
      Self.NotifyOfInboundCall(Session);

      if Request.HasHeader(ExpiresHeader) then
        Self.UserAgent.ScheduleEvent(TIdSipInboundInviteExpire,
                                     Request.Expires.NumericValue,
                                     Request);
      Result := Session;
    end;
  end;
end;

function TIdSipInviteModule.AddInboundInvite(Invite: TIdSipRequest;
                                             UsingSecureTransport: Boolean): TIdSipInboundInvite;
begin
  Result := Self.UserAgent.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.UserAgent, Invite, UsingSecureTransport)) as TIdSipInboundInvite;
end;

procedure TIdSipInviteModule.AddListener(Listener: IIdSipInviteModuleListener);
begin
  Self.Listeners.AddListener(Listener);
end;

function TIdSipInviteModule.AddOutboundSession: TIdSipOutboundSession;
begin
  Result := Self.UserAgent.AddOutboundAction(TIdSipOutboundSession) as TIdSipOutboundSession;
end;

function TIdSipInviteModule.AddOutboundSessionReplacer(Invite: TIdSipRequest): TIdSipOutboundSession;
begin
  Result := Self.UserAgent.Actions.Add(TIdSipOutboundSession.CreateSessionReplacer(Self.UserAgent, Invite)) as TIdSipOutboundSession;
end;

function TIdSipInviteModule.AllowedExtensions: String;
begin
  Result := ExtensionReplaces;
end;

function TIdSipInviteModule.Call(Dest: TIdSipAddressHeader;
                                 const LocalSessionDescription: String;
                                 const MimeType: String): TIdSipOutboundSession;
begin
  Result := Self.AddOutboundSession;
  Result.Destination             := Dest;
  Result.LocalSessionDescription := LocalSessionDescription;
  Result.LocalMimeType           := MimeType;
end;

function TIdSipInviteModule.CreateAck(Dialog: TIdSipDialog): TIdSipRequest;
begin
  try
    Result := Dialog.CreateAck;
    Self.UserAgent.AddLocalHeaders(Result);
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

procedure TIdSipInviteModule.RemoveListener(Listener: IIdSipInviteModuleListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

function TIdSipInviteModule.ReplaceCall(Invite: TIdSipRequest;
                                        Dest: TIdSipAddressHeader;
                                        const LocalSessionDescription: String;
                                        const MimeType: String): TIdSipOutboundSession;
begin
  // cf. RFC 3515

  Result := Self.AddOutboundSessionReplacer(Invite);
  Result.Destination             := Dest;
  Result.LocalSessionDescription := LocalSessionDescription;
  Result.LocalMimeType           := MimeType;
end;

//* TIdSipInviteModule Private methods *****************************************

function TIdSipInviteModule.ConvertToHeader(ValueList: TStrings): String;
begin
  Result := StringReplace(ValueList.CommaText, ',', ', ', [rfReplaceAll]);
end;

function TIdSipInviteModule.HasTooManyReplaces(Request: TIdSipRequest): Boolean;
var
  Replaces: TIdSipHeadersFilter;
begin
  Replaces := TIdSipHeadersFilter.Create(Request.Headers, ReplacesHeader);
  try
    Result := Replaces.Count > 1;
  finally
    Replaces.Free;
  end;
end;

function TIdSipInviteModule.MaybeAcceptReplaces(Request: TIdSipRequest;
                                                UsingSecureTransport: Boolean): TIdSipAction;
begin
  Result := nil;

  if Self.HasTooManyReplaces(Request) then
    Self.RejectBadRequest(Request, 'Too many Replaces headers');
end;

procedure TIdSipInviteModule.NotifyOfInboundCall(Session: TIdSipInboundSession);
var
  Notification: TIdSipInviteModuleInboundCallMethod;
begin
  Notification := TIdSipInviteModuleInboundCallMethod.Create;
  try
    Notification.Session   := Session;
    Notification.UserAgent := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

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
  OutboundRequest.Accept.Value    := Self.ConvertToHeader(Self.AllowedContentTypes);
  OutboundRequest.Supported.Value := Self.AllowedExtensions;
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

procedure TIdSipInviteBase.Initialise(UA: TIdSipAbstractCore;
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

procedure TIdSipInvite.Initialise(UA: TIdSipAbstractCore;
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

procedure TIdSipInboundInvite.Initialise(UA: TIdSipAbstractCore;
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

procedure TIdSipOutboundInvite.Resend(AuthorizationCredentials: TIdSipAuthorizationHeader);
begin
  Self.ReceivedFinalResponse := false;

  inherited Resend(AuthorizationCredentials);
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
  if not Self.ReceivedFinalResponse then
    Self.Cancel
  else
    Self.MarkAsTerminated;
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

function TIdSipOutboundInvite.CreateNewAttempt: TIdSipRequest;
begin
  Result := Self.CreateInvite;
end;

procedure TIdSipOutboundInvite.Initialise(UA: TIdSipAbstractCore;
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

function TIdSipOutboundInvite.ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionResult;
begin
  Result := inherited ReceiveFailureResponse(Response);

  if not Self.ReceivedFinalResponse then begin
    Self.ReceivedFinalResponse := true;
    Self.AnswerResponse.Assign(Response);
  end;
end;

function TIdSipOutboundInvite.ReceiveGlobalFailureResponse(Response: TIdSipResponse): TIdSipActionResult;
begin
  Result := inherited ReceiveGlobalFailureResponse(Response);

  Self.RegisterFinalResponse(Response);
end;

function TIdSipOutboundInvite.ReceiveOKResponse(Response: TIdSipResponse;
                                                UsingSecureTransport: Boolean): TIdSipActionResult;
begin
  // REMEMBER: A 2xx response to an INVITE DOES NOT take place in a transaction!
  // A 2xx response immediately terminates a client INVITE transaction so that
  // the ACK can get passed up to the UA (as an unhandled request).
  Result := arFailure;

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
      Result := arSuccess;

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
                                                         UsingSecureTransport: Boolean): TIdSipActionResult;
begin
  Result := arSuccess;

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
                                                         UsingSecureTransport: Boolean): TIdSipActionResult;
begin
  Result := inherited ReceiveRedirectionResponse(Response, UsingSecureTransport);

  Self.RegisterFinalResponse(Response);

  Self.NotifyOfRedirect(Response);
end;

function TIdSipOutboundInvite.ReceiveServerFailureResponse(Response: TIdSipResponse): TIdSipActionResult;
begin
  Result := inherited ReceiveServerFailureResponse(Response);

  Self.RegisterFinalResponse(Response);
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

procedure TIdSipOutboundInvite.RegisterFinalResponse(Response: TIdSipResponse);
begin
  if not Self.ReceivedFinalResponse then begin
    Self.ReceivedFinalResponse := true;
    Self.AnswerResponse.Assign(Response);
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
  // Note that sending a CANCEL does NOT terminate an outbound INVITE: we must
  // still wait for a final response from the network (either a 487 Request
  // Cancelled, or possibly a 200 OK, or whatever) that will actually terminate
  // this Action.

  Assert(not Self.SentCancel, DoubleCancelSend);
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

procedure TIdSipOutboundInitialInvite.Initialise(UA: TIdSipAbstractCore;
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
  // cf. RFC 3261, section 8.1.3.4

  Result := TIdSipRequest.Create;
  Result.Assign(Self.OriginalInvite);
  Result.CSeq.SequenceNo := Self.UA.NextInitialSequenceNo;
  Result.LastHop.Branch := Self.UA.NextBranch;
  Result.RequestUri := Self.Contact.Address;
end;

procedure TIdSipOutboundRedirectedInvite.Initialise(UA: TIdSipAbstractCore;
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

procedure TIdSipOutboundReInvite.Initialise(UA: TIdSipAbstractCore;
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
  Result.Replaces.CallID  := Self.CallID;
  Result.Replaces.FromTag := Self.FromTag;
  Result.Replaces.ToTag   := Self.ToTag;
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

procedure TIdSipSession.Resend(AuthorizationCredentials: TIdSipAuthorizationHeader);
begin
  Self.ModifyLock.Acquire;
  try
    if Assigned(Self.ModifyAttempt) then begin
      Self.ModifyAttempt.Resend(AuthorizationCredentials);
      Self.InitialRequest.Assign(Self.ModifyAttempt.InitialRequest);
    end;
  finally
    Self.ModifyLock.Release;
  end;
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

procedure TIdSipSession.Initialise(UA: TIdSipAbstractCore;
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
  // INVITEs that get challenged aren't visible outside this session, so we
  // re-notify of the authentication challenge.
  Self.NotifyOfAuthenticationChallenge(Response);
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
    Bye.CopyHeaders(Self.InitialRequest, AuthorizationHeader);
    Bye.CopyHeaders(Self.InitialRequest, ProxyAuthorizationHeader);

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

procedure TIdSipInboundSession.Initialise(UA: TIdSipAbstractCore;
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
    Self.InitialInvite := nil;
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

constructor TIdSipOutboundSession.Create(UA: TIdSipAbstractCore);
begin
  inherited Create(UA);

  Self.InitialiseUsing(TIdSipOutboundInitialInvite);
end;

constructor TIdSipOutboundSession.CreateSessionReplacer(UA: TIdSipAbstractCore;
                                                        Invite: TIdSipRequest);
var
  Replacer: TIdSipOutboundReplacingInvite;
begin
  inherited Create(UA);

  Self.InitialiseUsing(TIdSipOutboundReplacingInvite);

  Replacer := Self.InitialInvite as TIdSipOutboundReplacingInvite;
  Replacer.CallID  := Invite.CallID;
  Replacer.FromTag := Invite.From.Tag;
  Replacer.ToTag   := Invite.ToHeader.Tag;
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

  Self.Cancelling := true;
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

procedure TIdSipOutboundSession.Resend(AuthorizationCredentials: TIdSipAuthorizationHeader);
begin
  if Assigned(Self.InitialInvite) then begin
    Self.InitialInvite.Resend(AuthorizationCredentials);
    Self.InitialRequest.Assign(Self.InitialInvite.InitialRequest);
  end
  else
    inherited Resend(AuthorizationCredentials);
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
  //
  // If we send an INVITE we MUST NOT send a CANCEL until we've received at
  // least one response from the remote end. That means that while we have
  // started terminating, we have not finished, and cannot until we've
  // received a response.
  //
  // If we've sent an INVITE, the called party challenges our INVITE, and we
  // give up the attempt, we tell the initial INVITE to terminate, and terminate
  // ourselves, without sending any messages.
  if Self.FullyEstablished then begin
    Self.MarkAsTerminated;
    Self.SendBye;
    Self.NotifyOfEndedSession(LocalHangUp, RSNoReason);
  end
  else begin
    if (Self.InitialInvite.Result = arInterim) then begin
      Self.InitialInvite.Terminate;
      Self.MarkAsTerminated;
    end
    else
      Self.Cancel;
  end;
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

procedure TIdSipOutboundSession.Initialise(UA: TIdSipAbstractCore;
                                           Request: TIdSipRequest;
                                           UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.Cancelling := false;
end;

procedure TIdSipOutboundSession.OnAuthenticationChallenge(Action: TIdSipAction;
                                                          Response: TIdSipResponse);
begin
  inherited OnAuthenticationChallenge(Action, Response);

//  Self.SetResult(arInterim);
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

    if Self.Cancelling and (Self.RedirectedInvites.Count = 0) then
      Self.MarkAsTerminated;
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
//* TIdSipInviteModuleInboundCallMethod                                        *
//******************************************************************************
//* TIdSipInviteModuleInboundCallMethod Public methods *************************

procedure TIdSipInviteModuleInboundCallMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipInviteModuleListener).OnInboundCall(Self.UserAgent,
                                                        Self.Session);
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
//* TIdSipEndedSessionMethod                                                   *
//******************************************************************************
//* TIdSipEndedSessionMethod Public methods ************************************

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

end.
