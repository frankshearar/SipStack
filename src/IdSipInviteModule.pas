{
  (c) 2005 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipInviteModule;

interface

uses
  Classes, Contnrs, IdInterfacedObject, IdNotification, IdSipCore, IdSipDialog,
  IdSipDialogID, IdSipMessage, IdSipTransport, SyncObjs;

type
  TIdSipInboundInvite = class;

  IIdSipInboundInviteListener = interface
    ['{DE147123-E768-464A-924A-411BAA0C0B53}']
    procedure OnFailure(InviteAgent: TIdSipInboundInvite);
    procedure OnSuccess(InviteAgent: TIdSipInboundInvite;
                        Ack: TIdSipMessage);
  end;

  TIdSipOutboundInvite = class;

  IIdSipInviteListener = interface
    ['{8694DF86-3012-41AE-9854-A623A486743F}']
    procedure OnCallProgress(InviteAgent: TIdSipOutboundInvite;
                             Response: TIdSipResponse);
    procedure OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                  NewDialog: TIdSipDialog);
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
  // * OnReferral indicates I've received an in-dialog REFER.
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
    procedure OnReferral(Session: TIdSipSession;
                         Refer: TIdSipRequest;
                         UsingSecureTransport: Boolean);
  end;

  TIdSipInboundSession = class;
  TIdSipInviteModule = class;

  IIdSipInviteModuleListener = interface(IIdSipMessageModuleListener)
    ['{E9D86376-16A4-4166-885C-03697B121F23}']
    procedure OnInboundCall(UserAgent: TIdSipInviteModule;
                            Session: TIdSipInboundSession);
//    procedure OnReplacingInboundCall(Agent: TIdSipInviteModule);
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
  protected
    function AcceptRequest(Request: TIdSipRequest;
                           UsingSecureTransport: Boolean): TIdSipAction; override;
    function WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; override;
  public
    constructor Create(UA: TIdSipAbstractCore); override;

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

  // I provide basic facilities for all (owned) Actions that need to handle
  // INVITEs, BYEs, CANCELs.
  TIdSipInvite = class(TIdSipOwnedAction)
  protected
    Module: TIdSipInviteModule;

    function  CreateNewAttempt: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    procedure ReceiveAck(Ack: TIdSipRequest); virtual;
    procedure ReceiveBye(Bye: TIdSipRequest); virtual;
    procedure ReceiveCancel(Cancel: TIdSipRequest); virtual;
    procedure ReceiveInvite(Invite: TIdSipRequest); virtual;
  public
    class function Method: String; override;

    function IsInvite: Boolean; override;
    procedure ReceiveRequest(Request: TIdSipRequest); override;
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
    fLastResponse:            TIdSipResponse;
    fLocalMimeType:           String;
    fLocalSessionDescription: String;
    fLocalTag:                String;
    fMaxResendInterval:       Cardinal; // in milliseconds
    Grid:                     String;
    InviteListeners:          TIdNotificationList;
    InviteModule:             TIdSipInviteModule;
    ReceivedAck:              Boolean;
    ResendInterval:           Cardinal;
    SentFinalResponse:        Boolean;
    Terminating:              Boolean;

    function  GetInitialResendInterval: Cardinal;
    function  GetProgressResendInterval: Cardinal;
    procedure NotifyOfFailure; reintroduce; overload;
    procedure ScheduleResendOk(Interval: Cardinal);
    procedure SendSimpleResponse(StatusCode: Cardinal);
  protected
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    procedure NotifyOfSuccess(Ack: TIdSipMessage); override;
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

    property LastResponse:            TIdSipResponse read fLastResponse;
    property LocalSessionDescription: String         read fLocalSessionDescription;
    property LocalMimeType:           String         read fLocalMimeType;
    property LocalTag:                String         read fLocalTag write fLocalTag;
    property InitialResendInterval:   Cardinal       read GetInitialResendInterval;
    property MaxResendInterval:       Cardinal       read fMaxResendInterval write fMaxResendInterval;
    property ProgressResendInterval:  Cardinal       read GetProgressResendInterval;
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
    HasReceivedProvisionalResponse: Boolean;
    InviteListeners:                TIdNotificationList;
    ReceivedFinalResponse:          Boolean;
    SentCancel:                     Boolean;

    procedure NotifyOfCallProgress(Response: TIdSipResponse);
    procedure NotifyOfDialogEstablished(Response: TIdSipResponse;
                                        UsingSecureTransport: Boolean);
    procedure RegisterFinalResponse(Response: TIdSipResponse);
    procedure SendAckFor(Response: TIdSipResponse;
                         UsingSecureTransport: Boolean);
    procedure SendBye(Response: TIdSipResponse;
                      UsingSecureTransport: Boolean);
    procedure SendCancel;
    procedure SetAckBody(Ack: TIdSipRequest);
  protected
    procedure ActionSucceeded(Response: TIdSipResponse); override;
    function  CreateNewAttempt: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
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

    procedure AddInviteListener(const Listener: IIdSipInviteListener);
    procedure Cancel; override;
    function  Match(Msg: TIdSipMessage): Boolean; override;
    procedure RemoveInviteListener(const Listener: IIdSipInviteListener);
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
    function  CreateNewAttempt: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    destructor Destroy; override;

    property Destination: TIdSipAddressHeader read fDestination write SetDestination;
  end;

  TIdSipOutboundRedirectedInvite = class(TIdSipOutboundInvite)
  private
    fContact:         TIdSipAddressHeader;
    fOriginalRequest: TIdSipRequest;

    procedure SetContact(Value: TIdSipAddressHeader);
    procedure SetOriginalRequest(Value: TIdSipRequest);
  protected
    function  CreateNewAttempt: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    destructor Destroy; override;

    property Contact:         TIdSipAddressHeader read fContact write SetContact;
    property OriginalRequest: TIdSipRequest       read fOriginalRequest write SetOriginalRequest;
  end;

  // I implement the call flow surrounding an in-dialog INVITE, that is, an
  // INVITE that changes the session description in some way.
  TIdSipOutboundReInvite = class(TIdSipOutboundInvite)
  private
    fOriginalInvite: TIdSipRequest;

    procedure SetOriginalInvite(Value: TIdSipRequest);
  protected
    function  CreateNewAttempt: TIdSipRequest; override;
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
    function CreateNewAttempt: TIdSipRequest; override;
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
  TIdSipSession = class(TIdSipOwningAction,
                        IIdSipActionListener,
                        IIdSipOwnedActionListener,
                        IIdSipInviteListener,
                        IIdSipInboundInviteListener)
  private
    DialogLock:                TCriticalSection;
    fDialog:                   TIdSipDialog;
    fFullyEstablished:         Boolean;
    fLocalMimeType:            String;
    fLocalSessionDescription:  String;
    fReceivedAck:              Boolean;
    fRemoteContact:            TIdSipContactHeader;
    fRemoteMimeType:           String;
    fRemoteParty:              TIdSipAddressHeader;
    fRemoteSessionDescription: String;
    LastModifyDescription:     String;
    LastModifyMimeType:        String;
    UsingSecureTransport:      Boolean;

    procedure NotifyOfModifiedSession(Answer: TIdSipResponse);
    procedure RejectOutOfOrderRequest(Request: TIdSipRequest);
    procedure RejectPrematureInvite(Invite: TIdSipRequest);
    procedure RejectReInvite(Invite: TIdSipRequest);
    procedure RejectRequest(Request: TIdSipRequest);
    procedure RescheduleModify(InviteAgent: TIdSipAction);
    procedure SetRemoteContact(Value: TIdSipContactHeader);
    procedure SetRemoteParty(Value: TIdSipAddressHeader);
    procedure TerminateAnyPendingRequests;
  protected
    ChallengedAction: TIdSipAction;
    ModifyAttempt:    TIdSipInvite;
    ModifyLock:       TCriticalSection;
    Module:           TIdSipInviteModule;

    procedure ActionSucceeded(Response: TIdSipResponse); override;
    function  CreateDialogIDFrom(Msg: TIdSipMessage): TIdSipDialogID; virtual; abstract;
    function  CreateNewAttempt: TIdSipRequest; override;
    function  GetDialog: TIdSipDialog; virtual;
    function  GetFullyEstablished: Boolean; virtual;
    function  GetInvite: TIdSipRequest; virtual;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
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
    procedure OnFailure(Action: TIdSipAction;
                        Response: TIdSipResponse;
                        const Reason: String); overload; virtual;
    procedure OnFailure(InviteAgent: TIdSipInboundInvite); overload; virtual;
    procedure OnRedirect(Action: TIdSipAction;
                         Redirect: TIdSipResponse); virtual;
    procedure OnSuccess(InviteAgent: TIdSipInboundInvite;
                        Ack: TIdSipMessage); overload; virtual;
    procedure OnSuccess(Action: TIdSipAction;
                        Response: TIdSipMessage); overload; virtual;
    procedure ReceiveAck(Ack: TIdSipRequest);
    procedure ReceiveBye(Bye: TIdSipRequest);
    procedure ReceiveCancel(Cancel: TIdSipRequest);
    procedure ReceiveInitialInvite(Invite: TIdSipRequest); virtual;
    procedure ReceiveInvite(Invite: TIdSipRequest);
    procedure ReceiveRefer(Refer: TIdSipRequest); virtual;
    procedure SendBye; virtual;
    procedure SetFullyEstablished(Value: Boolean); virtual;

    property FullyEstablished: Boolean read GetFullyEstablished write SetFullyEstablished;
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
    function  SupportsExtension(const ExtensionName: String): Boolean;

    property Dialog:                   TIdSipDialog        read GetDialog;
    property LocalMimeType:            String              read fLocalMimeType write fLocalMimeType;
    property LocalSessionDescription:  String              read fLocalSessionDescription write fLocalSessionDescription;
    property ReceivedAck:              Boolean             read fReceivedAck;
    property RemoteContact:            TIdSipContactHeader read fRemoteContact write SetRemoteContact;
    property RemoteMimeType:           String              read fRemoteMimeType write fRemoteMimeType;
    property RemoteParty:              TIdSipAddressHeader read fRemoteParty write SetRemoteParty;
    property RemoteSessionDescription: String              read fRemoteSessionDescription write fRemoteSessionDescription;
  end;

  TIdSipInboundSession = class(TIdSipSession)
  private
    InitialInvite: TIdSipInboundInvite;
    Terminating:   Boolean;

    function CreateInboundDialog(Response: TIdSipResponse): TIdSipDialog;
    function MatchesLocalGruu(Msg: TIdSipMessage): Boolean;
    function MatchReplaces(Request: TIdSipRequest): Boolean;
  protected
    function  CreateDialogIDFrom(Msg: TIdSipMessage): TIdSipDialogID; override;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    procedure OnFailure(InviteAgent: TIdSipInboundInvite); override;
    procedure OnSuccess(InviteAgent: TIdSipInboundInvite;
                        Ack: TIdSipMessage); override;
    procedure OnSuccess(Action: TIdSipAction;
                        Response: TIdSipMessage); override;
    procedure ReceiveInitialInvite(Invite: TIdSipRequest); override;
    procedure ReceiveInvite(Invite: TIdSipRequest);
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
  TIdSipOutboundSession = class(TIdSipSession,
                                IIdSipActionRedirectorListener)
  private
    fDestination: TIdSipAddressHeader;
    Redirector:   TIdSipActionRedirector;

    function  GetCancelling: Boolean;
    procedure OnFailure(Redirector: TIdSipActionRedirector;
                        Response: TIdSipResponse); overload;
    procedure OnNewAction(Redirector: TIdSipActionRedirector;
                          NewAction: TIdSipAction);
    procedure OnRedirectFailure(Redirector: TIdSipActionRedirector;
                                ErrorCode: Cardinal;
                                const Reason: String); overload;
    procedure OnSuccess(Redirector: TIdSipActionRedirector;
                        SuccessfulAction: TIdSipAction;
                        Response: TIdSipResponse); overload;
    procedure SetCancelling(Value: Boolean);
    procedure SetDestination(Value: TIdSipAddressHeader);

    property Cancelling: Boolean read GetCancelling write SetCancelling;
  protected
    function  CreateDialogIDFrom(Msg: TIdSipMessage): TIdSipDialogID; override;
    function  GetFullyEstablished: Boolean; override;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse); override;
    procedure OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                  NewDialog: TIdSipDialog); override;
    procedure OnFailure(Action: TIdSipAction;
                        Response: TIdSipResponse;
                        const Reason: String); overload; override;
    procedure OnRedirect(Action: TIdSipAction;
                         Redirect: TIdSipResponse); override;
    procedure OnSuccess(Action: TIdSipAction;
                        Response: TIdSipMessage); overload; override;
    procedure SetFullyEstablished(Value: Boolean); override;
  public
    destructor Destroy; override;

    procedure Cancel;
    function  CanForkOn(Response: TIdSipResponse): Boolean;
    function  CreateInitialAction: TIdSipOwnedAction; override;
    function  CreateRedirectedAction(OriginalRequest: TIdSipRequest;
                                     Contact: TIdSipContactHeader): TIdSipOwnedAction; override;
    function  Match(Msg: TIdSipMessage): Boolean; override;
    function  ModifyWaitTime: Cardinal; override;
    procedure Resend(AuthorizationCredentials: TIdSipAuthorizationHeader); override;
    procedure Send; override;
    procedure Terminate; override;

    property Destination: TIdSipAddressHeader read fDestination write SetDestination;
  end;

  // the Invite property contains a copy of the session we wish to replace.
  TIdSipOutboundReplacingSession = class(TIdSipOutboundSession)
  private
    fInvite: TIdSipRequest;

    procedure SetInvite(Value: TIdSipRequest);
  protected
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    destructor Destroy; override;

    function CreateInitialAction: TIdSipOwnedAction; override;

    property Invite: TIdSipRequest read fInvite write SetInvite;
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
    fAck: TIdSipMessage;
  public
    procedure Run(const Subject: IInterface); override;

    property Ack: TIdSipMessage read fAck write fAck;
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

  TIdSipSessionReferralMethod = class(TIdSipSessionMethod)
  private
    fRefer:                TIdSipRequest;
    fUsingSecureTransport: Boolean;
  public
    procedure Run(const Subject: IInterface); override;

    property Refer:                TIdSipRequest read fRefer write fRefer;
    property UsingSecureTransport: Boolean       read fUsingSecureTransport write fUsingSecureTransport;
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

  Self.AddAllowedContentType(SdpMimeType);
  Self.AcceptsMethodsList.Add(MethodAck);
  Self.AcceptsMethodsList.Add(MethodBye);
  Self.AcceptsMethodsList.Add(MethodCancel);
  Self.AcceptsMethodsList.Add(MethodInvite);

  Self.ProgressResendInterval := OneMinute*1000;
end;

function TIdSipInviteModule.AddInboundInvite(Invite: TIdSipRequest;
                                             UsingSecureTransport: Boolean): TIdSipInboundInvite;
begin
  Result := Self.UserAgent.AddAction(TIdSipInboundInvite.CreateInbound(Self.UserAgent, Invite, UsingSecureTransport)) as TIdSipInboundInvite;
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
var
  Replacer: TIdSipOutboundReplacingSession;
begin
  Replacer := TIdSipOutboundReplacingSession.Create(Self.UserAgent);
  Self.UserAgent.Actions.Add(Replacer);

  Replacer.Invite := Invite;

  Result := Replacer;
end;

function TIdSipInviteModule.AllowedExtensions: String;
begin
  Result := ExtensionReplaces + ', '
          + ExtensionTargetDialog;

  if Self.UserAgent.UseGruu then
    Result := ExtensionGruu + ', ' + Result;          
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

//* TIdSipInviteModule Protected methods ***************************************

function TIdSipInviteModule.AcceptRequest(Request: TIdSipRequest;
                                          UsingSecureTransport: Boolean): TIdSipAction;
var
  ExpectedStatusCode: Cardinal;
  Session:            TIdSipInboundSession;
begin
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

      Self.NotifyOfInboundCall(Session);

      if Request.HasHeader(ExpiresHeader) then
        Self.UserAgent.ScheduleEvent(TIdSipInboundInviteExpire,
                                     Request.Expires.NumericValue,
                                     Request);
      Result := Session;
    end;
  end;
end;

function TIdSipInviteModule.WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  Result := inherited WillAcceptRequest(Request);

  if (Result = uarAccept) then begin
    // BYEs, CANCELS, ACKS relate to an existing action - a Session - so we don't
    // make a new action for these types of messages.
    if not Request.IsInvite then begin
        if not Request.IsAck then
          Result := uarNoSuchCall
        else
          Result := uarDoNothing;
    end
    else if Request.IsInvite then begin
      if Self.DoNotDisturb then
        Result := uarDoNotDisturb
      // RFC 3261, section 8.1.1.8 says that a request that can start a dialog
      // (like an INVITE), MUST contain a Contact.
      else if not Request.HasHeader(ContactHeaderFull) then
        Result := uarMissingContact;
    end;
  end;
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
//* TIdSipInvite                                                               *
//******************************************************************************
//* TIdSipInvite Public methods ************************************************

class function TIdSipInvite.Method: String;
begin
  Result := MethodInvite;
end;

function TIdSipInvite.IsInvite: Boolean;
begin
  Result := true;
end;

procedure TIdSipInvite.ReceiveRequest(Request: TIdSipRequest);
begin
       if Request.IsAck       then Self.ReceiveAck(Request)
  else if Request.IsBye       then Self.ReceiveBye(Request)
  else if Request.IsCancel    then Self.ReceiveCancel(Request)
  else if Request.IsInvite    then Self.ReceiveInvite(Request)
  else
    inherited ReceiveRequest(Request);
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

  Self.Module := Self.UA.ModuleFor(Self.Method) as TIdSipInviteModule;  

  // Invites are always owned by a Session
  Self.fIsOwned := true;
end;

procedure TIdSipInvite.ReceiveAck(Ack: TIdSipRequest);
begin
  Assert(Ack.IsAck,
         'TIdSipInvite.ReceiveAck must only receive ACKs');
  // By default do nothing
end;

procedure TIdSipInvite.ReceiveBye(Bye: TIdSipRequest);
begin
  Assert(Bye.IsBye,
         'TIdSipInvite.ReceiveBye must only receive BYEs');
  // By default do nothing
end;

procedure TIdSipInvite.ReceiveCancel(Cancel: TIdSipRequest);
var
  Ok: TIdSipResponse;
begin
  Assert(Cancel.IsCancel,
         'TIdSipInvite.ReceiveCancel must only receive CANCELs');

  Ok := Self.UA.CreateResponse(Cancel, SIPOK);
  try
    Self.SendResponse(Ok);
  finally
    Ok.Free;
  end;
end;

procedure TIdSipInvite.ReceiveInvite(Invite: TIdSipRequest);
begin
  Assert(Invite.IsInvite,
         'TIdSipInvite.ReceiveInvite must only receive INVITEs');
end;

//******************************************************************************
//* TIdSipInboundInvite                                                        *
//******************************************************************************
//* TIdSipInboundInvite Public methods *****************************************

destructor TIdSipInboundInvite.Destroy;
begin
  Self.LastResponse.Free;
  Self.InviteListeners.Free;

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

    if Self.UA.UseGruu then
      Ok.FirstContact.Grid := Self.Grid;

    // This works regardless of whether the UA supports GRUU: if the UA doesn't
    // then we simply make no USE of LocalGruu.
    Self.LocalGruu := Ok.FirstContact;

    Self.SendResponse(Ok);
  finally
    Ok.Free;
  end;

  Self.ScheduleResendOk(Self.ResendInterval);
end;

procedure TIdSipInboundInvite.AddListener(const Listener: IIdSipInboundInviteListener);
begin
  Self.InviteListeners.AddListener(Listener);
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
  Self.ActionListeners.RemoveListener(Listener);
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
  if not Self.SentFinalResponse then begin
    Self.SendSimpleResponse(SIPRequestTerminated);
    inherited Terminate;
  end
  else begin
    if not Self.ReceivedAck then begin
      // We sent a final response (say, 200) and we terminate the INVITE before
      // we receive the ACK. We need to wait until we receive the ACK and THEN
      // Terminate.
      Self.Terminating := true;
    end
    else begin
      // We can't reach here because the owning session's already killed Self.
    end;
  end;
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

  Self.InviteListeners := TIdNotificationList.Create;
  Self.InviteModule    := Self.UA.ModuleFor(MethodInvite) as TIdSipInviteModule;

  Self.fLastResponse     := TIdSipResponse.Create;
  Self.ReceivedAck       := false;
  Self.ResendInterval    := Self.InitialResendInterval;

  Self.LocalTag := Self.UA.NextTag;

  if Self.UA.UseGruu then
    Self.Grid := Self.UA.NextGrid;
end;

procedure TIdSipInboundInvite.NotifyOfSuccess(Ack: TIdSipMessage);
var
  Notification: TIdSipInboundInviteSuccessMethod;
begin
  Notification := TIdSipInboundInviteSuccessMethod.Create;
  try
    Notification.Ack    := Ack;
    Notification.Invite := Self;

    Self.InviteListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipInboundInvite.ReceiveAck(Ack: TIdSipRequest);
begin
  inherited ReceiveAck(Ack);

  if not Self.ReceivedAck then begin
    Self.ReceivedAck := true;

    // We always notify our listeners when we receive the ACK to our INVITE even
    // while terminating so that, for instance, our owning Session can terminate
    // correctly.
    Self.NotifyOfSuccess(Ack);

    if Self.Terminating then
      Self.MarkAsTerminated;
  end;
end;

procedure TIdSipInboundInvite.ReceiveCancel(Cancel: TIdSipRequest);
begin
  // This sends the 200 OK to the CANCEL.
  inherited ReceiveCancel(Cancel);

  // This tells the listeners we've failed. The owning InboundSession will
  // trigger sending the 487 Request Terminated.
  if not Self.SentFinalResponse then begin
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
  Self.LastResponse.Assign(Response);

  if Self.InitialRequest.Match(Response) then begin
    if not Self.SentFinalResponse then
      Self.SentFinalResponse := Response.IsFinal;

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

    Self.InviteListeners.Notify(Notification);
  finally
    Notification.Free;
  end;

  Self.MarkAsTerminated;
end;

procedure TIdSipInboundInvite.ScheduleResendOk(Interval: Cardinal);
begin
  Self.UA.ScheduleEvent(TIdSipInboundInviteResendOk,
                        Interval,
                        Self.InitialRequest,
                        Self.ID);
end;

procedure TIdSipInboundInvite.SendSimpleResponse(StatusCode: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.UA.CreateResponse(Self.InitialRequest,
                                     StatusCode);
  try
    if Self.UA.UseGruu then begin
      Response.FirstContact.Grid := Self.Grid;
      Self.LocalGruu := Response.FirstContact;
    end;

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
  Self.InviteListeners.Free;
  Self.CancelRequest.Free;
  Self.AnswerResponse.Free;

  inherited Destroy;
end;

procedure TIdSipOutboundInvite.AddInviteListener(const Listener: IIdSipInviteListener);
begin
  Self.InviteListeners.AddListener(Listener);
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

procedure TIdSipOutboundInvite.RemoveInviteListener(const Listener: IIdSipInviteListener);
begin
  Self.InviteListeners.RemoveListener(Listener);
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

  Invite := Self.CreateNewAttempt;
  try
    Self.InitialRequest.Assign(Invite);
    Self.LocalGruu := Invite.FirstContact;
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
  else begin
    Self.MarkAsTerminated;
    Self.NotifyOfFailure(Self.AnswerResponse);
  end;
end;

//* TIdSipOutboundInvite Protected methods *************************************

procedure TIdSipOutboundInvite.ActionSucceeded(Response: TIdSipResponse);
begin
  inherited ActionSucceeded(Response);

  // We only call SendAck here because we need to give the listeners (especially
  // the Session that created this Invite) time to process the message. The
  // Session especially needs to have its Dialog receive the response to set up
  // the Dialog, otherwise the ACK will not be well formed.
  if Response.IsOK then
    Self.SendAck(Self.Dialog, Response);
end;

function TIdSipOutboundInvite.CreateNewAttempt: TIdSipRequest;
begin
  raise Exception.Create('Override TIdSipOutboundInvite.CreateNewAttempt');
end;

procedure TIdSipOutboundInvite.Initialise(UA: TIdSipAbstractCore;
                                          Request: TIdSipRequest;
                                          UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.AnswerResponse                 := TIdSipResponse.Create;
  Self.Cancelling                     := false;
  Self.CancelRequest                  := TIdSipRequest.Create;
  Self.HasReceivedProvisionalResponse := false;
  Self.InviteListeners                := TIdNotificationList.Create;
  Self.ReceivedFinalResponse          := false;
  Self.SentCancel                     := false;
end;

function TIdSipOutboundInvite.ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionResult;
begin
  Result := inherited ReceiveFailureResponse(Response);

  Self.RegisterFinalResponse(Response);
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
      Self.RegisterFinalResponse(Response);

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

    Self.InviteListeners.Notify(Notification);
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

        Self.InviteListeners.Notify(Notification);
      finally
        Notification.Free;
      end;
    finally
      Dialog.Free;
    end;
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

function TIdSipOutboundInitialInvite.CreateNewAttempt: TIdSipRequest;
begin
  Result := Self.Module.CreateInvite(Self.Destination, Self.Offer, Self.MimeType);

  if Self.UA.UseGruu then begin
    Result.FirstContact.Grid := Self.UA.NextGrid;
  end;
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
  Self.fOriginalRequest.Free;
  Self.fContact.Free;

  inherited Destroy;
end;

//* TIdSipOutboundRedirectedInvite Protected methods ***************************

function TIdSipOutboundRedirectedInvite.CreateNewAttempt: TIdSipRequest;
begin
  // Use this method in the context of a redirect to an INVITE.
  // cf. RFC 3261, section 8.1.3.4

  Result := Self.UA.CreateRedirectedRequest(Self.OriginalRequest,
                                            Self.Contact);
end;

procedure TIdSipOutboundRedirectedInvite.Initialise(UA: TIdSipAbstractCore;
                                                    Request: TIdSipRequest;
                                                    UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.fContact         := TIdSipAddressHeader.Create;
  Self.fOriginalRequest := TIdSipRequest.Create;
end;

//* TIdSipOutboundRedirectedInvite Private methods *****************************

procedure TIdSipOutboundRedirectedInvite.SetContact(Value: TIdSipAddressHeader);
begin
  Self.fContact.Assign(Value);
end;

procedure TIdSipOutboundRedirectedInvite.SetOriginalRequest(Value: TIdSipRequest);
begin
  Self.OriginalRequest.Assign(Value);
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

function TIdSipOutboundReInvite.CreateNewAttempt: TIdSipRequest;
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

function TIdSipOutboundReplacingInvite.CreateNewAttempt: TIdSipRequest;
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
  Self.fRemoteParty.Free;
  Self.fRemoteContact.Free;

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
  Self.ActionListeners.AddListener(Listener);
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
    ReInvite.AddOwnedActionListener(Self);
    ReInvite.AddInviteListener(Self);
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
  end;

  if Request.IsAck         then Self.ReceiveAck(Request)
  else if Request.IsBye    then Self.ReceiveBye(Request)
  else if Request.IsCancel then Self.ReceiveCancel(Request)
  else if Request.IsInvite then Self.ReceiveInvite(Request)
  else if Request.IsRefer  then Self.ReceiveRefer(Request)
  else
    inherited ReceiveRequest(Request);
end;

procedure TIdSipSession.Remodify;
begin
  // Reattempt the previously-attempted modify. Don't call this; Notifications
  // (like TIdSipSessionResendReInvite) call this.

  Self.Modify(Self.LastModifyDescription, Self.LastModifyMimeType);
end;

procedure TIdSipSession.RemoveSessionListener(const Listener: IIdSipSessionListener);
begin
  Self.ActionListeners.RemoveListener(Listener);
end;

procedure TIdSipSession.Resend(AuthorizationCredentials: TIdSipAuthorizationHeader);
begin
  if (Self.State = asInitialised) then
    raise EIdSipTransactionUser.Create('You cannot REsend if you didn''t send'
                                     + ' in the first place');

  Self.ModifyLock.Acquire;
  try
    if Assigned(Self.ModifyAttempt)
      and (Self.ChallengedAction = Self.ModifyAttempt) then begin
      Self.ModifyAttempt.Resend(AuthorizationCredentials);
      Self.InitialRequest.Assign(Self.ModifyAttempt.InitialRequest);
    end;
  finally
    Self.ModifyLock.Release;
  end;
end;

function TIdSipSession.SupportsExtension(const ExtensionName: String): Boolean;
begin
  Self.DialogLock.Acquire;
  try
    if Self.DialogEstablished then
      Result := Self.Dialog.SupportsExtension(ExtensionName)
    else
      Result := false;
  finally
    Self.DialogLock.Release;
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

function TIdSipSession.GetDialog: TIdSipDialog;
begin
  Result := Self.fDialog;
end;

function TIdSipSession.GetFullyEstablished: Boolean;
begin
  Result := Self.fFullyEstablished;
end;

function TIdSipSession.GetInvite: TIdSipRequest;
begin
  Result := Self.InitialRequest;
end;

procedure TIdSipSession.Initialise(UA: TIdSipAbstractCore;
                                   Request: TIdSipRequest;
                                   UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.Module := Self.UA.ModuleFor(Self.Method) as TIdSipInviteModule;

  Self.DialogLock := TCriticalSection.Create;
  Self.ModifyLock := TCriticalSection.Create;

  Self.fReceivedAck := false;

  Self.fRemoteContact := TIdSipContactHeader.Create;
  Self.fRemoteParty   := TIdSipAddressHeader.Create;
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

    Self.ActionListeners.Notify(Notification);
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

    Self.ActionListeners.Notify(Notification);
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

    Self.ActionListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipSession.OnAuthenticationChallenge(Action: TIdSipAction;
                                                  Response: TIdSipResponse);
begin
  Self.ChallengedAction := Action;
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

    Self.ActionListeners.Notify(Notification);
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

procedure TIdSipSession.OnFailure(Action: TIdSipAction;
                                  Response: TIdSipResponse;
                                  const Reason: String);
begin
  Self.ModifyLock.Acquire;
  try
    if (Action = Self.ModifyAttempt) then begin
      Self.ModifyAttempt := nil;
      case Response.StatusCode of
        //  We attempted to modify the session. The remote end has also
        // attempted to do so, and sent an INVITE before our INVITE arrived.
        // Thus it rejects our attempt with a 491 Request Pending.
        SIPRequestPending: Self.RescheduleModify(Action);

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

procedure TIdSipSession.OnRedirect(Action: TIdSipAction;
                                   Redirect: TIdSipResponse);
begin
end;

procedure TIdSipSession.OnSuccess(InviteAgent: TIdSipInboundInvite;
                                  Ack: TIdSipMessage);
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

procedure TIdSipSession.OnSuccess(Action: TIdSipAction;
                                  Response: TIdSipMessage);
begin
  Self.DialogLock.Acquire;
  try
    if Self.DialogEstablished then
      Self.Dialog.ReceiveResponse(Response as TIdSipResponse);
  finally
    Self.DialogLock.Release;
  end;

  Self.ModifyLock.Acquire;
  try
    if (Action = Self.ModifyAttempt) then begin
      Self.ModifyAttempt := nil;

      Self.LocalSessionDescription  := Action.InitialRequest.Body;
      Self.LocalMimeType            := Action.InitialRequest.ContentType;
      Self.RemoteSessionDescription := Response.Body;
      Self.RemoteMimeType           := Response.ContentType;
    end;
  finally
    Self.ModifyLock.Release;
  end;
end;

procedure TIdSipSession.ReceiveAck(Ack: TIdSipRequest);
begin
  Assert(Ack.IsAck,
         'TIdSipSession.ReceiveAck must only receive ACKs');
  // By default do nothing
end;

procedure TIdSipSession.ReceiveBye(Bye: TIdSipRequest);
var
  OK: TIdSipResponse;
begin
  Assert(Bye.IsBye,
         'TIdSipSession.ReceiveBye must only receive BYEs');

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

procedure TIdSipSession.ReceiveCancel(Cancel: TIdSipRequest);
var
  Ok: TIdSipResponse;
begin
  Assert(Cancel.IsCancel,
         'TIdSipInvite.ReceiveCancel must only receive CANCELs');

  Ok := Self.UA.CreateResponse(Cancel, SIPOK);
  try
    Self.SendResponse(Ok);
  finally
    Ok.Free;
  end;
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
  Assert(Invite.IsInvite,
         'TIdSipSession.ReceiveInvite must only receive INVITEs');

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
          Modify.LocalTag := Invite.ToHeader.Tag;

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

procedure TIdSipSession.ReceiveRefer(Refer: TIdSipRequest);
var
  Module:       TIdSipSubscribeModule;
  Notification: TIdSipSessionReferralMethod;
begin
  // This may looks dangerous: what if the UA doesn't support REFER? In that
  // case, the UA will already have rejected the request, and we wouldn't be
  // executing this code.
  Module := Self.UA.ModuleFor(Refer) as TIdSipSubscribeModule;
  if Module.IsNull then begin
    // Something serious went wrong here: if there's no module for this message
    // then the UA should reject it with a 501 Not Implemented!
  end
  else begin
    Notification := TIdSipSessionReferralMethod.Create;
    try
      Notification.Refer   := Refer;
      Notification.Session := Self;

      Self.ActionListeners.Notify(Notification);
    finally
      Notification.Free;
    end;
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

procedure TIdSipSession.SetFullyEstablished(Value: Boolean);
begin
  Self.fFullyEstablished := Value;
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

    Self.ActionListeners.Notify(Notification);
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
    RetryAfter := Response.RetryAfter;

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

procedure TIdSipSession.RescheduleModify(InviteAgent: TIdSipAction);
begin
  // Precondition: You've acquired ModifyLock.
  Self.UA.ScheduleEvent(TIdSipSessionResendReInvite,
                        Self.ModifyWaitTime,
                        InviteAgent.InitialRequest,
                        Self.ID);
end;

procedure TIdSipSession.SetRemoteContact(Value: TIdSipContactHeader);
begin
  Self.fRemoteContact.Assign(Value);
end;

procedure TIdSipSession.SetRemoteParty(Value: TIdSipAddressHeader);
begin
  Self.fRemoteParty.Assign(Value);

  if Self.fRemoteParty.HasParam(TagParam) then
    Self.fRemoteParty.RemoveParameter(TagParam);
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
  if not Assigned(Self.InitialInvite) then
    raise EIdSipTransactionUser.Create('You have already invoked AcceptCall');

  Self.LocalSessionDescription := Offer;
  Self.LocalMimeType           := ContentType;

  Self.InitialInvite.Accept(Offer, ContentType);
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

  // Never match the ACKs: the InboundInvites will do that.
  if Msg.IsRequest and (Msg as TIdSipRequest).IsAck then
    Result := false
  // Leave modifying INVITE matches to the modifying Invite.
  else if MatchesReInvite then
    Result := false
  // Match CANCEL messages.
  else if Msg.IsRequest and (Msg as TIdSipRequest).IsCancel then
    Result := Self.InitialRequest.MatchCancel(Msg as TIdSipRequest)
  else if Msg.IsRequest and Msg.HasHeader(ReplacesHeader) then
    Result := Self.MatchReplaces(Msg as TIdSipRequest)
  else begin
    // Match anything directed at our LocalGRUU or shares our dialog
    Result := Self.MatchesLocalGruu(Msg)
          or (not Self.InitialRequest.Equals(Msg)
              and Self.DialogMatches(Msg));
  end;
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
      Self.InitialInvite.Ring;
      Self.fDialog := Self.CreateInboundDialog(Self.InitialInvite.LastResponse);
      Self.InitialRequest.Assign(Self.InitialInvite.InitialRequest);
      Self.InitialRequest.ToHeader.Tag := Self.Dialog.ID.LocalTag;
      Self.LocalGruu                   := Self.InitialInvite.LocalGruu;
    end;
  finally
    Self.DialogLock.Release;
  end;
end;

procedure TIdSipInboundSession.Terminate;
begin
  if Self.FullyEstablished then begin
    Self.SendBye;

    Self.NotifyOfEndedSession(LocalHangUp, RSNoReason);

    inherited Terminate;
  end
  else begin
    if Self.Terminating then begin
      // We shouldn't reach this point because we've already executed this
      // method. At any rate, it's senseless to continue, so just exit.     
      Exit;
    end;

    Self.InitialInvite.Terminate;

    Self.NotifyOfEndedSession(LocalHangUp, RSNoReason);

    if Self.InitialInvite.IsTerminated then begin
      // For instance, we received a CANCEL, so the InitialInvite sent the 487.
      Self.MarkAsTerminated
    end
    else begin
      // We issued the request to terminate, after we'd sent our 200 OK
      // accepting the call, but before we received the remote party's ACK.
      // We're waiting for that ACK before we terminate.
      Self.Terminating := true;
    end;
  end;
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

  Self.FullyEstablished := false;

  Self.InitialInvite := Self.Module.AddInboundInvite(Request, UsingSecureTransport);
  Self.InitialInvite.AddListener(Self);

  Self.RemoteContact            := Request.FirstContact;
  Self.RemoteMimeType           := Request.ContentType;
  Self.RemoteParty              := Request.From;
  Self.RemoteSessionDescription := Request.Body;
end;

procedure TIdSipInboundSession.OnFailure(InviteAgent: TIdSipInboundInvite);
begin
  if (Self.InitialInvite = InviteAgent) then
    Self.Terminate
  else
    inherited OnFailure(InviteAgent);
end;

procedure TIdSipInboundSession.OnSuccess(InviteAgent: TIdSipInboundInvite;
                                         Ack: TIdSipMessage);
begin
  inherited OnSuccess(InviteAgent, Ack);

  if (InviteAgent = Self.InitialInvite) then begin
    if Self.Terminating then begin
      // See the comment in Terminate.
      Self.SendBye;
      Self.MarkAsTerminated;
    end
    else begin
      if (Self.RemoteSessionDescription = '') then begin
        Self.RemoteSessionDescription := Ack.Body;
        Self.RemoteMimeType           := Ack.ContentType;
      end;

      Self.FullyEstablished := true;
      Self.NotifyOfEstablishedSession(Self.InitialInvite.InitialRequest.Body,
                                      Self.InitialInvite.InitialRequest.ContentType);

      Self.InitialInvite := nil;
    end;
  end;
end;

procedure TIdSipInboundSession.OnSuccess(Action: TIdSipAction;
                                         Response: TIdSipMessage);
begin
  inherited OnSuccess(Action, Response);

  Self.NotifyOfModifiedSession(Response as TIdSipResponse);
end;

procedure TIdSipInboundSession.ReceiveInitialInvite(Invite: TIdSipRequest);
begin
  Self.RemoteSessionDescription := Invite.Body;
  Self.RemoteMimeType           := Invite.ContentType;

  Self.Ring;
end;

procedure TIdSipInboundSession.ReceiveInvite(Invite: TIdSipRequest);
begin
  Assert(Invite.IsInvite,
         'TIdSipInboundSession.ReceiveInvite must only receive INVITEs');

  if Invite.HasReplaces then begin
    raise Exception.Create('TIdSipInboundSession.ReceiveInvite: Can''t yet challenge an INVITE with a Replaces header');
    // RFC 3891 section 3:
    //   If the Replaces header field matches an active dialog, the UA MUST
    //   verify that the initiator of the new INVITE is authorized to replace
    //   the matched dialog.  If the initiator of the new INVITE has been
    //   successfully authenticated as equivalent to the user who is being
    //   replaced, then the replacement is authorized.  For example, if the
    //   user being replaced and the initiator of the replacement dialog share
    //   the same credentials for Digest authentication [6], or they sign the
    //   replacement request with S/MIME [7] with the same private key and
    //   present the (same) corresponding certificate used in the original
    //   dialog, then the replacement is authorized.
    //
    //   Alternatively, the Referred-By mechanism [4] defines a mechanism that
    //   the UAS can use to verify that a replacement request was sent on
    //   behalf of the other participant in the matched dialog (in this case,
    //   triggered by a REFER request).  If the replacement request contains a
    //   Referred-By header that corresponds to the user being replaced, the
    //   UA SHOULD treat the replacement as if the replacement was authorized
    //   by the replaced party.  The Referred-By header SHOULD reference a
    //   corresponding, valid Refererred-By Authenticated Identity Body [5].
  end;
end;

//* TIdSipInboundSession Private methods ***************************************

function TIdSipInboundSession.CreateInboundDialog(Response: TIdSipResponse): TIdSipDialog;
begin
  Result := TIdSipDialog.CreateInboundDialog(Self.InitialRequest,
                                             Response,
                                             Self.UsingSecureTransport);
  Result.ReceiveResponse(Response);
end;

function TIdSipInboundSession.MatchesLocalGruu(Msg: TIdSipMessage): Boolean;
begin
  Result := Msg.IsRequest;

  if Result then begin
    Result := Result
          and (Msg as TIdSipRequest).RequestUri.HasGrid;

    if Result then begin
      Result := Result
          and (Self.LocalGruu.Grid = (Msg as TIdSipRequest).RequestUri.Grid);
    end;
  end;
end;

function TIdSipInboundSession.MatchReplaces(Request: TIdSipRequest): Boolean;
begin
  // We don't check for malformed requests like having multiple Replaces
  // headers and the like, because the InviteModule checks for that.

  Assert(Request.HasReplaces,
         'Request MUST have a Replaces header');

  Self.DialogLock.Acquire;
  try
    Result := (Self.Dialog.ID.CallID    = Request.Replaces.CallID)
          and (Self.Dialog.ID.LocalTag  = Request.Replaces.ToTag)
          and (Self.Dialog.ID.RemoteTag = Request.Replaces.FromTag);
  finally
    Self.DialogLock.Release;
  end;
end;

//******************************************************************************
//* TIdSipOutboundSession                                                      *
//******************************************************************************
//* TIdSipOutboundSession Public methods ***************************************

destructor TIdSipOutboundSession.Destroy;
begin
  Self.Redirector.Free;
  Self.fDestination.Free;

  inherited Destroy;
end;

procedure TIdSipOutboundSession.Cancel;
begin
  Self.Redirector.Cancel;
end;

function TIdSipOutboundSession.CanForkOn(Response: TIdSipResponse): Boolean;
begin
  Result := (Self.Redirector.InitialAction.InitialRequest.CallID = Response.CallID)
        and (Self.Redirector.InitialAction.InitialRequest.From.Tag = Response.From.Tag);
end;

function TIdSipOutboundSession.CreateInitialAction: TIdSipOwnedAction;
var
  Initial: TIdSipOutboundInitialInvite;
begin
  Initial := Self.UA.AddOutboundAction(TIdSipOutboundInitialInvite) as TIdSipOutboundInitialInvite;

  Initial.Destination := Self.Destination;
  Initial.Offer       := Self.LocalSessionDescription;
  Initial.MimeType    := Self.LocalMimeType;

  Result := Initial;
end;

function TIdSipOutboundSession.CreateRedirectedAction(OriginalRequest: TIdSipRequest;
                                                      Contact: TIdSipContactHeader): TIdSipOwnedAction;
var
  Redirect: TIdSipOutboundRedirectedInvite;
begin
  Redirect := Self.UA.AddOutboundAction(TIdSipOutboundRedirectedInvite) as TIdSipOutboundRedirectedInvite;
  Redirect.Contact         := Contact;
  Redirect.OriginalRequest := OriginalRequest;

  Result := Redirect;
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
  else if Msg.IsRequest and (Msg as TIdSipRequest).RequestUri.HasGrid then
    Result := Self.LocalGruu.Grid = (Msg as TIdSipRequest).RequestUri.Grid
  else begin
    // Any responses to our initial invite(s) must go to the OutboundInvite.
    // Otherwise, we match any in-dialog request that bears our dialog ID. 
    Result := Msg.IsRequest
          and not Self.InitialRequest.Equals(Msg)
          and Self.DialogMatches(Msg);
  end;
end;

function TIdSipOutboundSession.ModifyWaitTime: Cardinal;
begin
  // 2.1s <= WaitTime <= 4s, in 10ms units
  Result := GRandomNumber.NextCardinal(190)*10 + 2100
end;

procedure TIdSipOutboundSession.Resend(AuthorizationCredentials: TIdSipAuthorizationHeader);
begin
  if (Self.State = asInitialised) then
    raise EIdSipTransactionUser.Create('You cannot REsend if you didn''t send'
                                     + ' in the first place');

  if Self.Redirector.Contains(Self.ChallengedAction) then begin
    Self.Redirector.Resend(Self.ChallengedAction, AuthorizationCredentials);
    Self.InitialRequest.Assign(Self.ChallengedAction.InitialRequest);
  end
  else
    inherited Resend(AuthorizationCredentials);
end;

procedure TIdSipOutboundSession.Send;
begin
  inherited Send;

  Self.Redirector.Send;
  Self.InitialRequest.Assign(Self.Redirector.InitialAction.InitialRequest);
  Self.LocalGruu := Self.Redirector.InitialAction.LocalGruu;

  Self.RemoteParty := Self.InitialRequest.ToHeader;
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
  // received a response. When Redirector returns OnSuccess or OnFailure we will
  // act appropriately, terminating or sending a CANCEL and then terminating.
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
    Self.Redirector.Terminate;
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

function TIdSipOutboundSession.GetFullyEstablished: Boolean;
begin
  Result := Self.Redirector.FullyEstablished;
end;

procedure TIdSipOutboundSession.Initialise(UA: TIdSipAbstractCore;
                                           Request: TIdSipRequest;
                                           UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.fDestination := TIdSipAddressHeader.Create;

  Self.Redirector := TIdSipActionRedirector.Create(Self);
  Self.Redirector.AddListener(Self);
  
  Self.FullyEstablished := false;

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

procedure TIdSipOutboundSession.OnFailure(Action: TIdSipAction;
                                          Response: TIdSipResponse;
                                          const Reason: String);
begin
   if (Self.ModifyAttempt = Action) then
    inherited OnFailure(Action, Response, Reason)
end;

procedure TIdSipOutboundSession.OnRedirect(Action: TIdSipAction;
                                           Redirect: TIdSipResponse);
begin
  // Getting here means that the remote party redirected Action - an outbound
  // modifying INVITE! 
end;

procedure TIdSipOutboundSession.OnSuccess(Action: TIdSipAction;
                                          Response: TIdSipMessage);
begin
  inherited OnSuccess(Action, Response);

  Self.NotifyOfModifiedSession(Response as TIdSipResponse);
end;

procedure TIdSipOutboundSession.SetFullyEstablished(Value: Boolean);
begin
  Self.Redirector.FullyEstablished := Value;
end;

//* TIdSipOutboundSession Private methods **************************************

function TIdSipOutboundSession.GetCancelling: Boolean;
begin
  Result := Self.Redirector.Cancelling;
end;

procedure TIdSipOutboundSession.OnFailure(Redirector: TIdSipActionRedirector;
                                          Response: TIdSipResponse);
begin
  Self.NotifyOfFailure(Response);
end;

procedure TIdSipOutboundSession.OnNewAction(Redirector: TIdSipActionRedirector;
                                            NewAction: TIdSipAction);
begin
  NewAction.AddActionListener(Self);
  (NewAction as TIdSipOutboundInvite).AddInviteListener(Self);
end;

procedure TIdSipOutboundSession.OnRedirectFailure(Redirector: TIdSipActionRedirector;
                                                  ErrorCode: Cardinal;
                                                  const Reason: String);
begin
  // ErrorCode = NoError means that the Redirector found no more redirected
  // actions, and was busy cancelling. As such, we don't NotifyOfEndedSession
  // since the session never started.

  if (ErrorCode <> NoError) then
    Self.NotifyOfEndedSession(ErrorCode, Reason);

  Self.MarkAsTerminated;
end;

procedure TIdSipOutboundSession.OnSuccess(Redirector: TIdSipActionRedirector;
                                          SuccessfulAction: TIdSipAction;
                                          Response: TIdSipResponse);
begin
  Self.DialogLock.Acquire;
  try
    if Self.DialogEstablished then
      Self.Dialog.ReceiveResponse(Response);
  finally
    Self.DialogLock.Release;
  end;

  // This lets us store Authorization credentials for future use in things
  // like modifying INVITEs.
  Self.InitialRequest.Assign(SuccessfulAction.InitialRequest);

  Self.RemoteContact            := Response.FirstContact;
  Self.RemoteMimeType           := Response.ContentType;
  Self.RemoteParty              := Response.ToHeader;
  Self.RemoteSessionDescription := Response.Body;

  Self.NotifyOfEstablishedSession(Self.RemoteSessionDescription,
                                  Self.RemoteMimeType);

  (SuccessfulAction as TIdSipOutboundInvite).Offer    := Self.LocalSessionDescription;
  (SuccessfulAction as TIdSipOutboundInvite).MimeType := Self.LocalMimeType;
end;

procedure TIdSipOutboundSession.SetCancelling(Value: Boolean);
begin
  Self.Redirector.Cancelling := Value;
end;

procedure TIdSipOutboundSession.SetDestination(Value: TIdSipAddressHeader);
begin
  Self.fDestination.Assign(Value);
end;

//******************************************************************************
//* TIdSipOutboundReplacingSession                                             *
//******************************************************************************
//* TIdSipOutboundReplacingSession Public methods ******************************

destructor TIdSipOutboundReplacingSession.Destroy;
begin
  Self.fInvite.Free;

  inherited Destroy;
end;

function TIdSipOutboundReplacingSession.CreateInitialAction: TIdSipOwnedAction;
var
  Replacer: TIdSipOutboundReplacingInvite;
begin
  Replacer := Self.UA.AddOutboundAction(TIdSipOutboundReplacingInvite) as TIdSipOutboundReplacingInvite;
  Replacer.CallID      := Invite.CallID;
  Replacer.Destination := Self.Destination;
  Replacer.FromTag     := Invite.From.Tag;
  Replacer.ToTag       := Invite.ToHeader.Tag;

  Result := Replacer;
end;

procedure TIdSipOutboundReplacingSession.Initialise(UA: TIdSipAbstractCore;
                                                    Request: TIdSipRequest;
                                                    UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.fInvite := TIdSipRequest.Create;
end;

//* TIdSipOutboundReplacingSession Private methods *****************************

procedure TIdSipOutboundReplacingSession.SetInvite(Value: TIdSipRequest);
begin
  Self.fInvite.Assign(Value);
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

//******************************************************************************
//* TIdSipSessionReferralMethod                                                *
//******************************************************************************
//* TIdSipSessionReferralMethod Public methods *********************************

procedure TIdSipSessionReferralMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSessionListener).OnReferral(Self.Session,
                                                Self.Refer,
                                                Self.UsingSecureTransport);
end;

end.
