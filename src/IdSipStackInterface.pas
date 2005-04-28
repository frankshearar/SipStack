unit IdSipStackInterface;

interface

uses
  Classes, Contnrs, IdInterfacedObject, IdNotification, IdSipCore, IdSipMessage,
  IdSipTransport, IdTimerQueue, SyncObjs, SysUtils;

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
  TIdSipStackInterface = class(TIdInterfacedObject,
                               IIdSipActionListener,
                               IIdSipRegistrationListener,
                               IIdSipSessionListener,
                               IIdSipUserAgentListener)
  private
    ActionLock: TCriticalSection;
    Actions:    TObjectList;
    fUserAgent: TIdSipUserAgent;
    Listeners:  TIdNotificationList;

    function  ActionFor(Handle: TIdSipHandle): TIdSipAction;
    function  AddAction(Action: TIdSipAction): TIdSipHandle;
    function  AssociationAt(Index: Integer): TIdActionAssociation;
    function  HandleFor(Action: TIdSipAction): TIdSipHandle;
    function  IndexOf(H: TIdSipHandle): Integer;
    function  HasHandle(H: TIdSipHandle): Boolean;
    function  NewHandle: TIdSipHandle;
    procedure NotifyEvent(Action: TIdSipAction;
                          Event: Cardinal;
                          Data: TIdEventData);
    procedure OnAuthenticationChallenge(UserAgent: TIdSipAbstractUserAgent;
                                        Challenge: TIdSipResponse;
                                        var Username: String;
                                        var Password: String;
                                        var TryAgain: Boolean);
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractUserAgent;
                                        Message: TIdSipMessage;
                                        Receiver: TIdSipTransport);
    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String);
    procedure OnEstablishedSession(Session: TIdSipSession;
                                   const RemoteSessionDescription: String;
                                   const MimeType: String);
    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts;
                        const Reason: String);
    procedure OnInboundCall(UserAgent: TIdSipAbstractUserAgent;
                            Session: TIdSipInboundSession);
    procedure OnModifySession(Session: TIdSipSession;
                              const RemoteSessionDescription: String;
                              const MimeType: String);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Answer: TIdSipResponse);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               const Reason: String);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts);
    procedure RemoveAction(Handle: TIdSipHandle);
    procedure SendAction(Action: TIdSipAction);
    procedure SynchronizedNotify(Listeners: TIdNotificationList;
                                 Notification: TIdNotification);

    property UserAgent: TIdSipUserAgent read fUserAgent;
  public
    constructor Create(UserAgent: TIdSipUserAgent);
    destructor  Destroy; override;

    procedure AddListener(Listener: IIdSipStackListener);
    procedure AnswerCall(ActionHandle: TIdSipHandle;
                         const Offer: String;
                         const ContentType: String);
    procedure HangUp(ActionHandle: TIdSipHandle);
    function  MakeCall(Dest: TIdSipAddressHeader;
                       const LocalSessionDescription: String;
                       const MimeType: String): TIdSipHandle;
    function  MakeRegistration(Registrar: TIdSipUri): TIdSipHandle;
    procedure ModifyCall(ActionHandle: TIdSipHandle;
                         const Offer: String;
                         const ContentType: String);
    procedure RemoveListener(Listener: IIdSipStackListener);
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

  TIdFailEventData = class(TIdEventData)
  private
    fReason: String;
  public
    procedure Assign(Src: TPersistent); override;

    property Reason: String read fReason write fReason;
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

  TIdFailedRegistrationData = class(TIdRegistrationData)
  private
    fReason: String;
  public
    procedure Assign(Src: TPersistent); override;

    property Reason: String read fReason write fReason;
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

  // Use me when you want to run something in the context of the VCL/main
  // thread.
  TIdSipVclNotifier = class(TObject)
  private
    fListeners:    TIdNotificationList;
    fNotification: TIdNotification;
  public
    procedure Notify;

    property Listeners:    TIdNotificationList read fListeners write fListeners;
    property Notification: TIdNotification     read fNotification write fNotification;
  end;

  EInvalidHandle = class(Exception)
  public
    constructor Create(const Reason: String;
                       Handle: TIdSipHandle);
  end;

const
  InvalidHandle = 0;

const
  SipSuccess            = 0;
  SipFailure            = 1;
  SipNetworkFailure     = 2;
  SipSessionInbound     = 3;
  SipSessionEnded       = 4;
  SipSessionEstablished = 5;
  SipSessionModify      = 6;
  SipSessionModified    = 7;

implementation

uses
  IdRandom;

const
  NoSuchHandle = 'No such handle (%d)';
  ActionNotAllowedForHandle = 'You cannot perform that action on this handle (%d)';

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

constructor TIdSipStackInterface.Create(UserAgent: TIdSipUserAgent);
begin
  inherited Create;

  Self.ActionLock := TCriticalSection.Create;
  Self.Actions   := TObjectList.Create(true);
  Self.Listeners := TIdNotificationList.Create;

  Self.fUserAgent := UserAgent;

  Self.UserAgent.AddUserAgentListener(Self);
end;

destructor TIdSipStackInterface.Destroy;
begin
  Self.UserAgent.RemoveUserAgentListener(Self);

  Self.Listeners.Free;
  Self.Actions.Free;
  Self.ActionLock.Free;

  inherited Destroy;
end;

procedure TIdSipStackInterface.AddListener(Listener: IIdSipStackListener);
begin
  Self.Listeners.AddListener(Listener);
end;

procedure TIdSipStackInterface.AnswerCall(ActionHandle: TIdSipHandle;
                                          const Offer: String;
                                          const ContentType: String);
var
  Action: TIdSipAction;
begin
  Self.ActionLock.Acquire;
  try
    if not Self.HasHandle(ActionHandle) then
      raise EInvalidHandle.Create(NoSuchHandle, ActionHandle);

    Action := Self.ActionFor(ActionHandle);

    if not (Action is TIdSipInboundSession) then
      raise EInvalidHandle.Create(ActionNotAllowedForHandle, ActionHandle);

    (Action as TIdSipInboundSession).AcceptCall(Offer, ContentType)
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
    Action := Self.ActionFor(ActionHandle);

    if not Assigned(Action) then
      raise EInvalidHandle.Create(NoSuchHandle, ActionHandle);

    if not (Action is TIdSipSession) then
      raise EInvalidHandle.Create(ActionNotAllowedForHandle, ActionHandle);

    Action.Terminate;
  finally
    Self.ActionLock.Release;
  end;
end;

function TIdSipStackInterface.MakeCall(Dest: TIdSipAddressHeader;
                                       const LocalSessionDescription: String;
                                       const MimeType: String): TIdSipHandle;
var
  Sess: TIdSipOutboundSession;
begin
  Sess := Self.UserAgent.Call(Dest, LocalSessionDescription, MimeType);
  Result := Self.AddAction(Sess);
  Sess.AddSessionListener(Self);
end;

function TIdSipStackInterface.MakeRegistration(Registrar: TIdSipUri): TIdSipHandle;
var
  Reg: TIdSipOutboundRegistration;
begin
  Reg := Self.UserAgent.RegisterWith(Registrar);
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
    Action := Self.ActionFor(ActionHandle);

    if not Assigned(Action) then
      raise EInvalidHandle.Create(NoSuchHandle, ActionHandle);

    if not (Action is TIdSipSession) then
      raise EInvalidHandle.Create(ActionNotAllowedForHandle, ActionHandle);

    (Action as TIdSipSession).Modify(Offer, ContentType);
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.RemoveListener(Listener: IIdSipStackListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

procedure TIdSipStackInterface.Send(ActionHandle: TIdSipHandle);
var
  Action: TIdSipAction;
begin
  Self.ActionLock.Acquire;
  try
    Action := Self.ActionFor(ActionHandle);

    if not Assigned(Action) then
      raise EInvalidHandle.Create(NoSuchHandle, ActionHandle);

    Self.SendAction(Action);
  finally
    Self.ActionLock.Release;
  end;
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
    try
      Notification.Data   := Data;
      Notification.Event  := Event;
      Notification.Stack  := Self;

      Self.SynchronizedNotify(Self.Listeners, Notification);
    finally
      Notification.Free;
    end;
  finally
    Self.ActionLock.Release;
  end;
end;

procedure TIdSipStackInterface.OnAuthenticationChallenge(UserAgent: TIdSipAbstractUserAgent;
                                                         Challenge: TIdSipResponse;
                                                         var Username: String;
                                                         var Password: String;
                                                         var TryAgain: Boolean);
begin
end;

procedure TIdSipStackInterface.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractUserAgent;
                                                         Message: TIdSipMessage;
                                                         Receiver: TIdSipTransport);
begin
end;

procedure TIdSipStackInterface.OnEndedSession(Session: TIdSipSession;
                                              const Reason: String);
var
  Data: TIdEventData;
begin
  Data := TIdEventData.Create;
  try
    Data.Handle := Self.HandleFor(Session);
    Self.NotifyEvent(Session, SipSessionEnded, Data);

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

    Self.NotifyEvent(Session, SipSessionEstablished, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                                         CurrentBindings: TIdSipContacts;
                                         const Reason: String);
var
  Data: TIdFailedRegistrationData;
begin
  Data := TIdFailedRegistrationData.Create;
  try
    Data.Handle   := Self.HandleFor(RegisterAgent);
    Data.Contacts := CurrentBindings;
    Data.Reason   := Reason;

    Self.NotifyEvent(RegisterAgent, SipFailure, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnInboundCall(UserAgent: TIdSipAbstractUserAgent;
                                             Session: TIdSipInboundSession);
var
  Data: TIdEventData;
begin
  Session.AddSessionListener(Self);
  Self.AddAction(Session);

  Data := TIdEventData.Create;
  try
    Data.Handle := Self.HandleFor(Session);

    Self.NotifyEvent(Session, SipSessionInbound, Data);
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

    Self.NotifyEvent(Session, SipSessionModify, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnModifiedSession(Session: TIdSipSession;
                                                 Answer: TIdSipResponse);
begin
  raise Exception.Create('TIdSipStackInterface.OnModifiedSession not implemented');
end;

procedure TIdSipStackInterface.OnNetworkFailure(Action: TIdSipAction;
                                                const Reason: String);
var
  Data: TIdFailEventData;
begin
  Data := TIdFailEventData.Create;
  try
    Data.Handle := Self.HandleFor(Action);
    Data.Reason := Reason;

    Self.NotifyEvent(Action, SipNetworkFailure, Data);
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

    Self.NotifyEvent(RegisterAgent, SipSuccess, Data);
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

procedure TIdSipStackInterface.SynchronizedNotify(Listeners: TIdNotificationList;
                                                  Notification: TIdNotification);
var
  Notifier: TIdSipVclNotifier;                                                  
begin
  Notifier := TIdSipVclNotifier.Create;
  try
    Notifier.Listeners    := Listeners;
    Notifier.Notification := Notification;

    Self.UserAgent.Timer.Synchronize(Notifier.Notify);
  finally
    Notifier.Free;
  end;
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
//* TIdFailEventData                                                           *
//******************************************************************************
//* TIdFailEventData Public methods ********************************************

procedure TIdFailEventData.Assign(Src: TPersistent);
var
  Other: TIdFailEventData;
begin
  inherited Assign(Src);

  if (Src is TIdFailEventData) then begin
    Other := Src as TIdFailEventData;
    Self.Reason := Other.Reason;
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

procedure TIdFailedRegistrationData.Assign(Src: TPersistent);
var
  Other: TIdFailedRegistrationData;
begin
  inherited Assign(Src);

  if (Src is TIdFailedRegistrationData) then begin
    Other := Src as TIdFailedRegistrationData;

    Self.Reason := Other.Reason;
  end;
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
//* TIdSipVclNotifier                                                          *
//******************************************************************************
//* TIdSipVclNotifier Public methods *******************************************

procedure TIdSipVclNotifier.Notify;
begin
  Self.Listeners.Notify(Self.Notification);
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
