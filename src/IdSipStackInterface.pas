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
  Classes, Contnrs, IdInterfacedObject, IdNotification, IdSipCore,
  IdSipInviteModule, IdSipLocator, IdSipMessage, IdSipRegistration,
  IdSipSubscribeModule, IdSipTransaction, IdSipTransport,  IdSipUserAgent,
  IdTimerQueue, SyncObjs, SysUtils, Messages, Windows;

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
  TIdSipStackInterface = class(TIdThreadedTimerQueue,
                               IIdSipActionListener,
                               IIdSipInviteModuleListener,
                               IIdSipMessageModuleListener,
                               IIdSipRegistrationListener,
                               IIdSipSessionListener,
                               IIdSipSubscribeModuleListener,
                               IIdSipSubscriptionListener,
                               IIdSipTransactionUserListener,
                               IIdSipTransportListener,
                               IIdSipTransportSendingListener,
                               IIdSipUserAgentListener)
  private
    ActionLock:      TCriticalSection;
    Actions:         TObjectList;
    fUiHandle:       HWnd;
    fUserAgent:      TIdSipUserAgent;
    SubscribeModule: TIdSipSubscribeModule;

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function  ActionFor(Handle: TIdSipHandle): TIdSipAction;
    function  AddAction(Action: TIdSipAction): TIdSipHandle;
    function  AssociationAt(Index: Integer): TIdActionAssociation;
    function  GetAndCheckAction(Handle: TIdSipHandle;
                                ExpectedType: TIdSipActionClass): TIdSipAction;
    function  HandleFor(Action: TIdSipAction): TIdSipHandle;
    function  IndexOf(H: TIdSipHandle): Integer;
    function  HasHandle(H: TIdSipHandle): Boolean;
    function  NewHandle: TIdSipHandle;
    procedure NotifyEvent(Action: TIdSipAction;
                          Event: Cardinal;
                          Data: TIdEventData);
    procedure NotifySubscriptionEvent(Event: Cardinal;
                                      Subscription: TIdSipSubscription;
                                      Notify: TIdSipRequest);
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse); overload;
    procedure OnAuthenticationChallenge(UserAgent: TIdSipAbstractCore;
                                        Challenge: TIdSipResponse;
                                        var Username: String;
                                        var Password: String;
                                        var TryAgain: Boolean); overload;
    procedure OnAuthenticationChallenge(UserAgent: TIdSipAbstractCore;
                                        ChallengedRequest: TIdSipRequest;
                                        Challenge: TIdSipResponse); overload;
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                        Message: TIdSipMessage;
                                        Receiver: TIdSipTransport);
    procedure OnEndedSession(Session: TIdSipSession;
                             ErrorCode: Cardinal;
                             const Reason: String);
    procedure OnEstablishedSession(Session: TIdSipSession;
                                   const RemoteSessionDescription: String;
                                   const MimeType: String);
    procedure OnEstablishedSubscription(Subscription: TIdSipOutboundSubscription;
                                        Notify: TIdSipRequest);
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnExpiredSubscription(Subscription: TIdSipOutboundSubscription;
                                    Notify: TIdSipRequest);
    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts;
                        Response: TIdSipResponse); overload;
    procedure OnFailure(Subscription: TIdSipOutboundSubscription;
                        Response: TIdSipResponse); overload;
    procedure OnInboundCall(UserAgent: TIdSipAbstractCore;
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
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipOutboundSubscription);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Destination: TIdSipLocation);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Destination: TIdSipLocation);
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractCore;
                                    Subscription: TIdSipInboundSubscription);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts);

    procedure RemoveAction(Handle: TIdSipHandle);
    procedure SendAction(Action: TIdSipAction);
    procedure SynchronizedNotify(Notification: TIdSipStackInterfaceEventMethod);

    property UiHandle:  HWnd            read fUiHandle;
    property UserAgent: TIdSipUserAgent read fUserAgent;
  public
    constructor Create(UiHandle: HWnd;
                       Configuration: TStrings); reintroduce;
    destructor  Destroy; override;

    procedure AcceptCallModify(ActionHandle: TIdSipHandle;
                               const LocalSessionDescription: String;
                               const ContentType: String);
    procedure AnswerCall(ActionHandle: TIdSipHandle;
                         const Offer: String;
                         const ContentType: String);
    procedure Authenticate(ActionHandle: TIdSipHandle;
                           Credentials: TIdSipAuthorizationHeader);
    procedure HangUp(ActionHandle: TIdSipHandle);
//    function  MakeBlindTransfer(Call: TIdSipHandle;
//                                NewTarget: TIdSipAddressHeader): TIdSipHandle;
    function  MakeCall(Dest: TIdSipAddressHeader;
                       const LocalSessionDescription: String;
                       const MimeType: String): TIdSipHandle;
    function  MakeRefer(Target: TIdSipAddressHeader;
                        Resource: TIdSipAddressHeader): TIdSipHandle;
    function  MakeRegistration(Registrar: TIdSipUri): TIdSipHandle;
    procedure ModifyCall(ActionHandle: TIdSipHandle;
                         const Offer: String;
                         const ContentType: String);
    procedure RedirectCall(ActionHandle: TIdSipHandle;
                           NewTarget: TIdSipAddressHeader);
    procedure RejectCall(ActionHandle: TIdSipHandle);
    procedure Send(ActionHandle: TIdSipHandle);
  end;

  // I contain data relating to a particular event.
  TIdEventData = class(TPersistent)
  private
    fHandle: TIdSipHandle;
  public
    constructor Create; virtual;

    procedure Assign(Src: TPersistent); override;
    function  Copy: TIdEventData; virtual;

    property Handle: TIdSipHandle read fHandle write fHandle;
  end;

  TIdEventDataClass = class of TIdEventData;

  // An ErrorCode of 0 means "no error".
  // Usually the ErrorCode will map to a SIP response Status-Code.
  TIdInformationalData = class(TIdEventData)
  private
    fErrorCode: Cardinal;

    fReason: String;
    procedure SetErrorCode(Value: Cardinal);  private
  public
    constructor Create; override;

    procedure Assign(Src: TPersistent); override;

    property ErrorCode: Cardinal read fErrorCode write SetErrorCode;
    property Reason: String read fReason write fReason;
  end;

  TIdAuthenticationChallengeData = class(TIdEventData)
  private
    fChallenge:         TIdSipResponse;
    fChallengedRequest: TIdSipRequest;

    procedure SetChallenge(Response: TIdSipResponse);
    procedure SetChallengedRequest(Request: TIdSipRequest);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Challenge:         TIdSipResponse read fChallenge write SetChallenge;
    property ChallengedRequest: TIdSipRequest  read fChallengedRequest write SetChallengedRequest;
  end;

  TIdFailData = class(TIdInformationalData);
  TIdCallEndedData = class(TIdInformationalData);

  TIdDebugMessageData = class(TIdEventData)
  private
    fMessage: TIdSipMessage;
  public
    destructor Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Message: TIdSipMessage read fMessage write fMessage;
  end;

  TIdDebugSendMessageData = class(TIdDebugMessageData)
  private
    fDestination: TIdSipLocation;
  public
    destructor Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Destination: TIdSipLocation read fDestination write fDestination;
  end;

  TIdRegistrationData = class(TIdEventData)
  private
    fContacts: TIdSipContacts;

    procedure SetContacts(Value: TIdSipContacts);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Contacts: TIdSipContacts read fContacts write SetContacts;
  end;

  TIdFailedRegistrationData = class(TIdFailData)
  private
    RegistrationData: TIdRegistrationData;

    function  GetContacts: TIdSipContacts;
    procedure SetContacts(Value: TIdSipContacts);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Contacts: TIdSipContacts read GetContacts write SetContacts;
  end;

  TIdSessionData = class(TIdEventData)
  private
    fLocalSessionDescription:  String;
    fLocalMimeType:            String;
    fRemoteSessionDescription: String;
    fRemoteMimeType:           String;
  public
    procedure Assign(Src: TPersistent); override;

    property LocalSessionDescription:  String read fLocalSessionDescription write fLocalSessionDescription;
    property LocalMimeType:            String read fLocalMimeType write fLocalMimeType;
    property RemoteSessionDescription: String read fRemoteSessionDescription write fRemoteSessionDescription;
    property RemoteMimeType:           String read fRemoteMimeType write fRemoteMimeType;
  end;

  TIdSessionProgressData = class(TIdSessionData)
  private
    fBanner:       String;
    fProgressCode: Cardinal;
  public
    procedure Assign(Src: TPersistent); override;

    property Banner:       String   read fBanner write fBanner;
    property ProgressCode: Cardinal read fProgressCode write fProgressCode;
  end;

  TIdInboundCallData = class(TIdSessionData)
  private
    fContact: TIdSipContactHeader;
    fFrom:    TIdSipFromHeader;

    procedure SetContact(Value: TIdSipContactHeader);
    procedure SetFrom(Value: TIdSipFromHeader);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Contact: TIdSipContactHeader read fContact write SetContact;
    property From:    TIdSipFromHeader    read fFrom write SetFrom;
  end;

  TIdSubscriptionRequestData = class(TIdEventData)
  private
    fContact:      TIdSipContactHeader;
    fEventPackage: String;
    fFrom:         TIdSipFromHeader;
    fReferTo:      TIdSipReferToHeader;

    procedure SetContact(Value: TIdSipContactHeader);
    procedure SetFrom(Value: TIdSipFromHeader);
    procedure SetReferTo(Value: TIdSipReferToHeader);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Contact:      TIdSipContactHeader read fContact write SetContact;
    property EventPackage: String              read fEventPackage write fEventPackage;
    property From:         TIdSipFromHeader    read fFrom write SetFrom;
    property ReferTo:      TIdSipReferToHeader read fReferTo write SetReferTo;
  end;

  TIdSubscriptionData = class(TIdEventData)
  private
    fNotify: TIdSipRequest;

    procedure SetNotify(Value: TIdSipRequest);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property Notify: TIdSipRequest read fNotify write SetNotify;
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

    property Data:   TIdEventData         read fData write fData;
    property Event:  Cardinal             read fEvent write fEvent;
    property Stack:  TIdSipStackInterface read fStack write fStack;
  end;

  EInvalidHandle = class(Exception)
  public
    constructor Create(const Reason: String;
                       Handle: TIdSipHandle);
  end;

  // Raise me when the UserAgent doesn't support an action (e.g., it doesn't use
  // the SubscribeModule and the caller tried to MakeRefer).
  ENotSupported = class(Exception);

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
  CM_AUTHENTICATION_CHALLENGE     = CM_BASE + 9;
  CM_SUBSCRIPTION_ESTABLISHED     = CM_BASE + 10;
  CM_SUBSCRIPTION_RECV_NOTIFY     = CM_BASE + 11;
  CM_SUBSCRIPTION_EXPIRED         = CM_BASE + 12;
  CM_SUBSCRIPTION_REQUEST_NOTIFY  = CM_BASE + 13;

  CM_DEBUG = CM_BASE + 10000;

  CM_DEBUG_DROPPED_MSG         = CM_DEBUG + 0;
  CM_DEBUG_RECV_MSG            = CM_DEBUG + 1;
  CM_DEBUG_SEND_MSG            = CM_DEBUG + 2;
  CM_DEBUG_TRANSPORT_EXCEPTION = CM_DEBUG + 3;
  CM_LAST                      = CM_DEBUG_DROPPED_MSG;

// Constants for TIdCallEndedData
const
  CallEndedSuccess        = 0;
  CallEndedFailure        = 1;
  CallEndedNoSuchUser     = SIPNotFound;
  CallEndedRejected       = SIPBusyHere;
  CallServiceNotAvailable = SIPServiceUnavailable;

type
  TIdSipEventMessage = packed record
    Event:    Cardinal;
    Data:     TIdSipStackInterfaceEventMethod;
    Reserved: DWord;
  end;

function EventNames(Event: Cardinal): String;

implementation

uses
  IdGlobal, IdRandom, IdSimpleParser, IdSipAuthentication, IdSipIndyLocator,
  IdSipMockLocator, IdStack, IdUDPServer;

const
  ActionNotAllowedForHandle = 'You cannot perform that action on this handle (%d)';
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
    CM_SUCCESS:                      Result := 'CM_SUCCESS';

    CM_DEBUG_DROPPED_MSG:            Result := 'CM_DEBUG_DROPPED_MSG';
    CM_DEBUG_RECV_MSG:               Result := 'CM_DEBUG_RECV_MSG';
    CM_DEBUG_SEND_MSG:               Result := 'CM_DEBUG_SEND_MSG';
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
                                        Configuration: TStrings);
var
  Configurator: TIdSipStackConfigurator;
  I:            Integer;
begin
  inherited Create(true);

  Self.ActionLock := TCriticalSection.Create;
  Self.Actions    := TObjectList.Create(true);

  Self.fUiHandle := UiHandle;

  Configurator := TIdSipStackConfigurator.Create;
  try
    Self.fUserAgent := Configurator.CreateUserAgent(Configuration, Self);
    Self.UserAgent.AddUserAgentListener(Self);
    Self.UserAgent.InviteModule.AddListener(Self);
//    Self.UserAgent.AddTransportListener(Self);

    for I := 0 to Self.UserAgent.Dispatcher.TransportCount - 1 do
      Self.UserAgent.Dispatcher.Transports[I].AddTransportSendingListener(Self);

    Self.SubscribeModule := Self.UserAgent.ModuleFor(TIdSipSubscribeModule) as TIdSipSubscribeModule;

    if Assigned(Self.SubscribeModule) then
      Self.SubscribeModule.AddListener(Self);
  finally
    Configurator.Free;
  end;
end;

destructor TIdSipStackInterface.Destroy;
begin
//  Self.DebugUnregister;

  Self.UserAgent.Free;

  Self.Actions.Free;
  Self.ActionLock.Free;

  inherited Destroy;
end;

procedure TIdSipStackInterface.AcceptCallModify(ActionHandle: TIdSipHandle;
                                                const LocalSessionDescription: String;
                                                const ContentType: String);
var
  Action: TIdSipAction;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipSession);

    (Action as TIdSipSession).AcceptModify(LocalSessionDescription, ContentType);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.AnswerCall(ActionHandle: TIdSipHandle;
                                          const Offer: String;
                                          const ContentType: String);
var
  Action: TIdSipAction;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipInboundSession);

    (Action as TIdSipInboundSession).AcceptCall(Offer, ContentType)
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.Authenticate(ActionHandle: TIdSipHandle;
                                            Credentials: TIdSipAuthorizationHeader);
var
  Action: TIdSipAction;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipAction);

    Action.Resend(Credentials);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.HangUp(ActionHandle: TIdSipHandle);
var
  Action: TIdSipAction;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipSession);

    Action.Terminate;
  finally
    Self.ActionLock.Release;
  end;
end;
{
function TIdSipStackInterface.MakeBlindTransfer(Call: TIdSipHandle;
                                                NewTarget: TIdSipAddressHeader): TIdSipHandle;
var
  Action:   TIdSipAction;
  Session:  TIdSipSession;
  Transfer: TIdSipBlindTransferral;
begin
  // Remember, usually you want to put Call on hold before you invoke this
  // procedure.

  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(Call, TIdSipSession);
    Session := Action as TIdSipSession;

    Transfer := Self.SubscribeModule.BlindTransfer(Session, NewTarget);
    Result := Self.AddAction(Transfer);
    Transfer.AddListener(Self);
  finally
    Self.ActionLock.Release;
  end;
end;
}
function TIdSipStackInterface.MakeCall(Dest: TIdSipAddressHeader;
                                       const LocalSessionDescription: String;
                                       const MimeType: String): TIdSipHandle;
var
  Sess: TIdSipOutboundSession;
begin
  Sess := Self.UserAgent.InviteModule.Call(Dest, LocalSessionDescription, MimeType);
  Result := Self.AddAction(Sess);
  Sess.AddSessionListener(Self);
end;

function TIdSipStackInterface.MakeRefer(Target: TIdSipAddressHeader;
                                        Resource: TIdSipAddressHeader): TIdSipHandle;
var
  Ref: TIdSipOutboundReferral;
begin
  // Transfer a call (for instance) to someone else (Resource) by sending a
  // REFER message to the caller (Target).

  // Check that the UA even supports REFER!
  if not Assigned(Self.SubscribeModule) then
    raise ENotSupported.Create(MethodRefer);

  Ref := Self.SubscribeModule.Refer(Target, Resource);
  Result := Self.AddAction(Ref);
  Ref.AddListener(Self);
end;

function TIdSipStackInterface.MakeRegistration(Registrar: TIdSipUri): TIdSipHandle;
var
  Reg: TIdSipOutboundRegistration;
begin
  Reg := Self.UserAgent.RegisterModule.RegisterWith(Registrar);
  Result := Self.AddAction(Reg);
  Reg.AddListener(Self);
end;

procedure TIdSipStackInterface.ModifyCall(ActionHandle: TIdSipHandle;
                                          const Offer: String;
                                          const ContentType: String);
var
  Action: TIdSipAction;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipSession);

    (Action as TIdSipSession).Modify(Offer, ContentType);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.RedirectCall(ActionHandle: TIdSipHandle;
                                            NewTarget: TIdSipAddressHeader);
var
  Action: TIdSipAction;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipInboundSession);

    (Action as TIdSipInboundSession).RedirectCall(NewTarget);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.RejectCall(ActionHandle: TIdSipHandle);
var
  Action: TIdSipAction;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.GetAndCheckAction(ActionHandle, TIdSipInboundSession);

    (Action as TIdSipInboundSession).RejectCallBusy;
  finally
    Self.ActionLock.Release;
  end;
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

//* TIdSipStackInterface Private methods ***************************************

function TIdSipStackInterface.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  // Don't support reference counting.
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TIdSipStackInterface._AddRef: Integer;
begin
  // Don't support reference counting.
  Result := -1;
end;

function TIdSipStackInterface._Release: Integer;
begin
  // Don't support reference counting.
  Result := -1;
end;

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

function TIdSipStackInterface.AssociationAt(Index: Integer): TIdActionAssociation;
begin
  Result := Self.Actions[Index] as TIdActionAssociation;
end;

function TIdSipStackInterface.GetAndCheckAction(Handle: TIdSipHandle;
                                                ExpectedType: TIdSipActionClass): TIdSipAction;
begin
  Result := Self.ActionFor(Handle);

  if not Assigned(Result) then
    raise EInvalidHandle.Create(NoSuchHandle, Handle);

  if not (Result is ExpectedType) then
    raise EInvalidHandle.Create(ActionNotAllowedForHandle, Handle);
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

function TIdSipStackInterface.NewHandle: TIdSipHandle;
begin
  // Precondition: ActionLock acquired.
  // Postcondition: Result contains a handle that's not assigned to any ongoing
  // action.

  repeat
    Result := GRandomNumber.NextCardinal;
  until not Self.HasHandle(Result);
end;

procedure TIdSipStackInterface.NotifyEvent(Action: TIdSipAction;
                                           Event: Cardinal;
                                           Data: TIdEventData);
var
  Notification: TIdSipStackInterfaceEventMethod;
begin
  // We lock Actions before we notify so that we can guarantee that the handle
  // will be valid for the duration of the notification.
  Self.ActionLock.Acquire;
  try
    Notification := TIdSipStackInterfaceEventMethod.Create;
    Notification.Data   := Data.Copy;
    Notification.Event  := Event;
    Notification.Stack  := Self;

    // The receiver of this message must free the Notification.
    Self.SynchronizedNotify(Notification);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.NotifySubscriptionEvent(Event: Cardinal;
                                                       Subscription: TIdSipSubscription;
                                                       Notify: TIdSipRequest);
var
  Data: TIdSubscriptionData;
begin
  Data := TIdSubscriptionData.Create;
  try
    Data.Handle := Self.HandleFor(Subscription);

    if (Notify <> nil) then
      Data.Notify := Notify;

    Self.NotifyEvent(Subscription, Event, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnAuthenticationChallenge(Action: TIdSipAction;
                                                         Response: TIdSipResponse);
var
  Data: TIdAuthenticationChallengeData;
begin
  Data := TIdAuthenticationChallengeData.Create;
  try
    Data.Challenge         := Response;
    Data.ChallengedRequest := Action.InitialRequest;
    Data.Handle            := Self.HandleFor(Action);

    Self.NotifyEvent(Action, CM_AUTHENTICATION_CHALLENGE, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnAuthenticationChallenge(UserAgent: TIdSipAbstractCore;
                                                         Challenge: TIdSipResponse;
                                                         var Username: String;
                                                         var Password: String;
                                                         var TryAgain: Boolean);
begin
end;

procedure TIdSipStackInterface.OnAuthenticationChallenge(UserAgent: TIdSipAbstractCore;
                                                         ChallengedRequest: TIdSipRequest;
                                                         Challenge: TIdSipResponse);
begin
end;                                                         

procedure TIdSipStackInterface.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                                         Message: TIdSipMessage;
                                                         Receiver: TIdSipTransport);
var
  Data: TIdDebugMessageData;
begin
  Data := TIdDebugMessageData.Create;
  try
    Data.Handle := InvalidHandle;
    Data.Message := Message.Copy;

    Self.NotifyEvent(nil, CM_DEBUG_DROPPED_MSG, Data);
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
    Self.NotifyEvent(Session, CM_CALL_ENDED, Data);

    Self.RemoveAction(Data.Handle);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnEstablishedSession(Session: TIdSipSession;
                                                    const RemoteSessionDescription: String;
                                                    const MimeType: String);
var
  Data: TIdSessionData;
begin
  Data := TIdSessionData.Create;
  try
    Data.Handle                   := Self.HandleFor(Session);
    Data.LocalSessionDescription  := Session.LocalSessionDescription;
    Data.LocalMimeType            := Session.LocalMimeType;
    Data.RemoteSessionDescription := RemoteSessionDescription;
    Data.RemoteMimeType           := MimeType;

    Self.NotifyEvent(Session, CM_CALL_ESTABLISHED, Data);
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

procedure TIdSipStackInterface.OnException(E: Exception;
                                           const Reason: String);
var
  Data: TIdFailData;
begin
  Data := TIdFailData.Create;
  try
    Data.Handle    := InvalidHandle;
    Data.ErrorCode := CallEndedFailure; // This value is actually bogus, but it's at least non-zero
    Data.Reason    := Reason;

    Self.NotifyEvent(nil, CM_DEBUG_TRANSPORT_EXCEPTION, Data);
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

procedure TIdSipStackInterface.OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                                         CurrentBindings: TIdSipContacts;
                                         Response: TIdSipResponse);
var
  Data: TIdFailedRegistrationData;
begin
  Data := TIdFailedRegistrationData.Create;
  try
    Data.Handle    := Self.HandleFor(RegisterAgent);
    Data.Contacts  := CurrentBindings;
    Data.ErrorCode := Response.StatusCode;
    Data.Reason    := Response.Description;

    Self.NotifyEvent(RegisterAgent, CM_FAIL, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnFailure(Subscription: TIdSipOutboundSubscription;
                                         Response: TIdSipResponse);
begin
  raise Exception.Create('Implement TIdSipStackInterface.OnFailure');
end;

procedure TIdSipStackInterface.OnInboundCall(UserAgent: TIdSipAbstractCore;
                                             Session: TIdSipInboundSession);
var
  Data: TIdInboundCallData;
begin
  Session.AddSessionListener(Self);
  Self.AddAction(Session);

  Data := TIdInboundCallData.Create;
  try
    Data.Handle                   := Self.HandleFor(Session);
    Data.Contact                  := Session.InitialRequest.FirstContact;
    Data.From                     := Session.InitialRequest.From;
    Data.RemoteSessionDescription := Session.RemoteSessionDescription;
    Data.RemoteMimeType           := Session.RemoteMimeType;

    Self.NotifyEvent(Session, CM_CALL_REQUEST_NOTIFY, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnModifySession(Session: TIdSipSession;
                                               const RemoteSessionDescription: String;
                                               const MimeType: String);
var
  Data: TIdSessionData;
begin
  Data := TIdSessionData.Create;
  try
    Data.Handle := Self.HandleFor(Session);
    Data.RemoteSessionDescription := RemoteSessionDescription;
    Data.RemoteMimeType           := MimeType;

    Self.NotifyEvent(Session, CM_CALL_REMOTE_MODIFY_REQUEST, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnModifiedSession(Session: TIdSipSession;
                                                 Answer: TIdSipResponse);
var
  Data: TIdSessionData;
begin
  Data := TIdSessionData.Create;
  try
    Data.Handle := Self.HandleFor(Session);
    Data.LocalMimeType            := Session.LocalMimeType;
    Data.LocalSessionDescription  := Session.LocalSessionDescription;
    Data.RemoteMimeType           := Answer.ContentType;
    Data.RemoteSessionDescription := Answer.Body;

    Self.NotifyEvent(Session, CM_CALL_OUTBOUND_MODIFY_SUCCESS, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnNetworkFailure(Action: TIdSipAction;
                                                ErrorCode: Cardinal;
                                                const Reason: String);
var
  Data: TIdFailData;
begin
  Data := TIdFailData.Create;
  try
    Data.Handle    := Self.HandleFor(Action);
    Data.ErrorCode := ErrorCode;
    Data.Reason    := Reason;

    Self.NotifyEvent(Action, CM_NETWORK_FAILURE, Data);
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
    Data.Banner                   := TIdSipUri.Decode(Progress.StatusText);
    Data.Handle                   := Self.HandleFor(Session);
    Data.LocalSessionDescription  := Session.LocalSessionDescription;
    Data.LocalMimeType            := Session.LocalMimeType;
    Data.ProgressCode             := Progress.StatusCode;
    Data.RemoteSessionDescription := Progress.Body;
    Data.RemoteMimeType           := Progress.ContentType;

    Self.NotifyEvent(Session, CM_CALL_PROGRESS, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnReceiveRequest(Request: TIdSipRequest;
                                                Receiver: TIdSipTransport);
var
  Data: TIdDebugMessageData;
begin
  Data := TIdDebugMessageData.Create;
  try
    Data.Handle := InvalidHandle;
    Data.Message := Request;

    Self.NotifyEvent(nil, CM_DEBUG_RECV_MSG, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnReceiveResponse(Response: TIdSipResponse;
                                                 Receiver: TIdSipTransport);
var
  Data: TIdDebugMessageData;
begin
  Data := TIdDebugMessageData.Create;
  try
    Data.Handle := InvalidHandle;
    Data.Message := Response;

    Self.NotifyEvent(nil, CM_DEBUG_RECV_MSG, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnRejectedMessage(const Msg: String;
                                                 const Reason: String);
begin
  raise Exception.Create('TIdSipStackInterface.OnRejectedMessage');
end;

procedure TIdSipStackInterface.OnRenewedSubscription(UserAgent: TIdSipAbstractCore;
                                                     Subscription: TIdSipOutboundSubscription);
begin
  Subscription.AddListener(Self);
  Self.AddAction(Subscription);
  raise Exception.Create('TIdSipStackInterface.OnRenewedSubscription');
end;

procedure TIdSipStackInterface.OnSendRequest(Request: TIdSipRequest;
                                             Sender: TIdSipTransport;
                                             Destination: TIdSipLocation);
var
  Data: TIdDebugSendMessageData;
begin
  Data := TIdDebugSendMessageData.Create;
  try
    Data.Handle      := InvalidHandle;
    Data.Destination := Destination.Copy;
    Data.Message     := Request.Copy;

    Self.NotifyEvent(nil, CM_DEBUG_SEND_MSG, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnSendResponse(Response: TIdSipResponse;
                                              Sender: TIdSipTransport;
                                              Destination: TIdSipLocation);
var
  Data: TIdDebugSendMessageData;
begin
  // TODO Refactor this & OnSendRequest into NotifyOfSentMessage

  Data := TIdDebugSendMessageData.Create;
  try
    Data.Handle      := InvalidHandle;
    Data.Destination := Destination.Copy;
    Data.Message     := Response.Copy;

    Self.NotifyEvent(nil, CM_DEBUG_SEND_MSG, Data);
  finally
    Data.Free;
  end;
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
    Data.Contact      := Subscription.InitialRequest.FirstContact;
    Data.EventPackage := Subscription.EventPackage;
    Data.From         := Subscription.InitialRequest.From;
    Data.ReferTo      := Subscription.InitialRequest.ReferTo;

    Self.NotifyEvent(Subscription, CM_SUBSCRIPTION_REQUEST_NOTIFY, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                                         CurrentBindings: TIdSipContacts);
var
  Data: TIdRegistrationData;
begin
  Data := TIdRegistrationData.Create;
  try
    Data.Handle   := Self.HandleFor(RegisterAgent);
    Data.Contacts := CurrentBindings;

    Self.NotifyEvent(RegisterAgent, CM_SUCCESS, Data);
  finally
    Data.Free;
  end;
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
  Wait.Action := Action;
  Self.UserAgent.ScheduleEvent(TriggerImmediately, Wait);
end;

procedure TIdSipStackInterface.SynchronizedNotify(Notification: TIdSipStackInterfaceEventMethod);
begin
  PostMessage(Self.UiHandle, UINT(Notification.Event), WPARAM(Notification), 0)
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

function TIdEventData.Copy: TIdEventData;
begin
  Result := TIdEventDataClass(Self.ClassType).Create;
  Result.Assign(Self);
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

//******************************************************************************
//* TIdDebugSendMessageData                                                    *
//******************************************************************************
//* TIdDebugSendMessageData Public methods *************************************

destructor TIdDebugSendMessageData.Destroy;
begin
  Self.Destination.Free;

  inherited Destroy;
end;

procedure TIdDebugSendMessageData.Assign(Src: TPersistent);
var
  Other: TIdDebugSendMessageData;
begin
  inherited Assign(Src);

  if (Src is TIdDebugSendMessageData) then begin
    Other := Src as TIdDebugSendMessageData;

    Self.Destination := Other.Destination.Copy;
  end;
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

    Self.Contacts  := Other.Contacts;
    Self.ErrorCode := Other.ErrorCode;
    Self.Reason    := Other.Reason;
  end;
end;

//* TIdFailedRegistrationData Private methods **********************************

function TIdFailedRegistrationData.GetContacts: TIdSipContacts;
begin
  Result := Self.RegistrationData.Contacts;
end;

procedure TIdFailedRegistrationData.SetContacts(Value: TIdSipContacts);
begin
  Self.RegistrationData.Contacts := Value;
end;

//******************************************************************************
//* TIdSessionData                                                             *
//******************************************************************************
//* TIdSessionData Public methods **********************************************

procedure TIdSessionData.Assign(Src: TPersistent);
var
  Other: TIdSessionData;
begin
  inherited Assign(Src);

  if (Src is TIdSessionData) then begin
    Other := Src as TIdSessionData;

    Self.LocalMimeType            := Other.LocalMimeType;
    Self.LocalSessionDescription  := Other.LocalSessionDescription;
    Self.RemoteMimeType           := Other.RemoteMimeType;
    Self.RemoteSessionDescription := Other.RemoteSessionDescription;
  end;
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

//******************************************************************************
//* TIdInboundCallData                                                         *
//******************************************************************************
//* TIdInboundCallData Public methods ******************************************

constructor TIdInboundCallData.Create;
begin
  inherited Create;

  Self.fContact := TIdSipContactHeader.Create;
  Self.fFrom    := TIdSipFromHeader.Create;
end;

destructor TIdInboundCallData.Destroy;
begin
  Self.fFrom.Free;
  Self.fContact.Free;

 inherited Destroy;
end;

procedure TIdInboundCallData.Assign(Src: TPersistent);
var
  Other: TIdInboundCallData;
begin
  inherited Assign(Src);

  if (Src is TIdInboundCallData) then begin
    Other := Src as TIdInboundCallData;

    Self.Contact := Other.Contact;
    Self.From    := Other.From;
  end;
end;

//* TIdInboundCallData Private methods *****************************************

procedure TIdInboundCallData.SetContact(Value: TIdSipContactHeader);
begin
  Self.Contact.Assign(Value);
end;

procedure TIdInboundCallData.SetFrom(Value: TIdSipFromHeader);
begin
  Self.From.Assign(Value);
  Self.From.RemoveParameter(TagParam);
end;

//******************************************************************************
//* TIdSubscriptionRequestData                                                 *
//******************************************************************************
//* TIdSubscriptionRequestData Public methods **********************************

constructor TIdSubscriptionRequestData.Create;
begin
  inherited Create;

  Self.fContact := TIdSipContactHeader.Create;
  Self.fFrom    := TIdSipFromHeader.Create;
  Self.fReferTo := TIdSipReferToHeader.Create;
end;

destructor TIdSubscriptionRequestData.Destroy;
begin
  Self.fReferTo.Free;
  Self.fFrom.Free;
  Self.fContact.Free;

  inherited Destroy;
end;

procedure TIdSubscriptionRequestData.Assign(Src: TPersistent);
var
  Other: TIdSubscriptionRequestData;
begin
  inherited Assign(Src);

  if (Src is TIdSubscriptionRequestData) then begin
    Other := Src as TIdSubscriptionRequestData;

    Self.Contact      := Other.Contact;
    Self.EventPackage := Other.EventPackage;
    Self.From         := Other.From;
    Self.ReferTo      := Other.ReferTo;
  end;
end;

//* TIdSubscriptionRequestData Private methods *********************************

procedure TIdSubscriptionRequestData.SetContact(Value: TIdSipContactHeader);
begin
  Self.fContact.Assign(Value);
end;

procedure TIdSubscriptionRequestData.SetFrom(Value: TIdSipFromHeader);
begin
  Self.fFrom.Assign(Value);
end;

procedure TIdSubscriptionRequestData.SetReferTo(Value: TIdSipReferToHeader);
begin
  Self.fReferTo.Assign(Value);
end;

//******************************************************************************
//* TIdSubscriptionData                                                        *
//******************************************************************************
//* TIdSubscriptionData Public methods *****************************************

constructor TIdSubscriptionData.Create;
begin
  inherited Create;

  Self.fNotify := TIdSipRequest.Create;
end;

destructor TIdSubscriptionData.Destroy;
begin
  Self.fNotify.Free;

  inherited Destroy;
end;

procedure TIdSubscriptionData.Assign(Src: TPersistent);
var
  Other: TIdSubscriptionData;
begin
  inherited Assign(Src);

  if (Src is TIdSubscriptionData) then begin
    Other := Src as TIdSubscriptionData;

    Self.Notify := Other.Notify;
  end;
end;

//* TIdSubscriptionData Private methods ****************************************

procedure TIdSubscriptionData.SetNotify(Value: TIdSipRequest);
begin
  Self.fNotify.Assign(Value);
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
//* EInvalidHandle                                                             *
//******************************************************************************
//* EInvalidHandle Public methods **********************************************

constructor EInvalidHandle.Create(const Reason: String;
                                  Handle: TIdSipHandle);
begin
  inherited Create(Format(Reason, [Handle]));
end;

end.
