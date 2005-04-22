unit IdSipStackInterface;

interface

uses
  Contnrs, IdInterfacedObject, IdNotification, IdSipCore, IdSipMessage,
  IdSipTransport, IdTimerQueue, SyncObjs, SysUtils;

type
  TIdSipHandle = Cardinal;

  TIdSipStackInterface = class;
  IIdSipStackListener = interface
    ['{BBC8C7F4-4031-4258-93B3-8CA71C9F8733}']
    procedure OnEvent(Stack: TIdSipStackInterface;
                      Action: TIdSipHandle;
                      Event: Cardinal);
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
  // I make sure that messages are sent in the context of the stack's thread
  // (its Timer).
  //
  // You receive Handles to actions by calling methods with the prefix "Make".
  // You can perform actions using those Handles using the other methods. If you
  // call a method of mine with an invalid handle (a handle for an action that's
  // finished, a handle I never gave you) or try issue an inappropriate command
  // using an otherwise valid handle (calling AcceptCall on an outbound call,
  // for instance) I will raise an EInvalidHandle exception.  
  TIdSipStackInterface = class(TIdInterfacedObject,
                               IIdSipActionListener,
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
    function  HasHandle(H: Cardinal): Boolean;
    function  NewHandle: TIdSipHandle;
    procedure NotifyEvent(Action: TIdSipAction;
                          Event: Cardinal);
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
    procedure OnInboundCall(UserAgent: TIdSipAbstractUserAgent;
                            Session: TIdSipInboundSession);
    procedure OnModifySession(Modify: TIdSipInboundInvite);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Answer: TIdSipResponse);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               const Reason: String);
    procedure SendAction(Action: TIdSipAction);

    property UserAgent: TIdSipUserAgent read fUserAgent;
  public
    constructor Create(UserAgent: TIdSipUserAgent);
    destructor  Destroy; override;

    procedure AddListener(Listener: IIdSipStackListener);
    procedure AnswerCall(ActionHandle: TIdSipHandle;
                         const Offer: String;
                         const ContentType: String);
    function  MakeCall(Dest: TIdSipAddressHeader;
                       const LocalSessionDescription: String;
                       const MimeType: String): TIdSipHandle;
    function  MakeCallModification(ActionHandle: TIdSipHandle;
                                   const Offer: String;
                                   const ContentType: String): TIdSipHandle;
    procedure RemoveListener(Listener: IIdSipStackListener);
    procedure Send(ActionHandle: TIdSipHandle);
  end;

  // I represent a reified method call, like my ancestor, that a
  // SipStackInterface uses to signal that something interesting happened (an
  // inbound call has arrived, a network failure occured, an action succeeded,
  // tc.)
  TIdSipStackInterfaceEventMethod = class(TIdNotification)
  private
    fAction: TIdSipHandle;
    fEvent:  Cardinal;
    fStack:  TIdSipStackInterface;
  public
    procedure Run(const Subject: IInterface); override;

    property Action: TIdSipHandle         read fAction write fAction;
    property Event:  Cardinal             read fEvent write fEvent;
    property Stack:  TIdSipStackInterface read fStack write fStack;
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
  SipNetworkFailure     = 1;
  SipSessionInbound     = 2;
  SipSessionEnded       = 3;
  SipSessionEstablished = 4;
  SipSessionModify      = 5;
  SipSessionModified    = 6;

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

function TIdSipStackInterface.MakeCallModification(ActionHandle: TIdSipHandle;
                                                   const Offer: String;
                                                   const ContentType: String): TIdSipHandle;
begin
  raise Exception.Create('Implement TIdSipStackInterface.MakeCallModification');
end;

procedure TIdSipStackInterface.RemoveListener(Listener: IIdSipStackListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

procedure TIdSipStackInterface.Send(ActionHandle: TIdSipHandle);
begin
  Self.ActionLock.Acquire;
  try
    Self.SendAction(Self.ActionFor(ActionHandle));
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

function TIdSipStackInterface.HasHandle(H: Cardinal): Boolean;
var
  I: Integer;
begin
  // Precondition: ActionLock acquired.
  I      := 0;
  Result := false;

  while (I < Self.Actions.Count) and not Result do begin
    Result := Self.AssociationAt(I).Handle = H;
    Inc(I);
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

procedure TIdSipStackInterface.NotifyEvent(Action: TIdSipAction;
                                           Event: Cardinal);
var
  Notification: TIdSipStackInterfaceEventMethod;
begin
  // We lock Actions before we notify so that we can guarantee that the handle
  // will be valid for the duration of the notification.
  Self.ActionLock.Acquire;
  try
    Notification := TIdSipStackInterfaceEventMethod.Create;
    try
      Notification.Action := Self.HandleFor(Action);
      Notification.Event  := Event;
      Notification.Stack  := Self;

      Self.Listeners.Notify(Notification);
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
begin
  Self.NotifyEvent(Session, SipSessionEnded);
end;

procedure TIdSipStackInterface.OnEstablishedSession(Session: TIdSipSession;
                                                    const RemoteSessionDescription: String;
                                                    const MimeType: String);
begin
  Self.NotifyEvent(Session, SipSessionEstablished);
end;

procedure TIdSipStackInterface.OnInboundCall(UserAgent: TIdSipAbstractUserAgent;
                                             Session: TIdSipInboundSession);
begin
  Session.AddSessionListener(Self);
  Self.AddAction(Session);
  Self.NotifyEvent(Session, SipSessionInbound);
end;

procedure TIdSipStackInterface.OnModifySession(Modify: TIdSipInboundInvite);
begin
  raise Exception.Create('TIdSipStackInterface.OnModifySession not implemented');
end;

procedure TIdSipStackInterface.OnModifiedSession(Session: TIdSipSession;
                                                 Answer: TIdSipResponse);
begin
  raise Exception.Create('TIdSipStackInterface.OnModifiedSession not implemented');
end;

procedure TIdSipStackInterface.OnNetworkFailure(Action: TIdSipAction;
                                                const Reason: String);
begin
  Self.NotifyEvent(Action, SipNetworkFailure);
end;

procedure TIdSipStackInterface.SendAction(Action: TIdSipAction);
var
  Wait: TIdSipActionSendWait;
begin
  Wait := TIdSipActionSendWait.Create;
  Wait.Action := Action;
  Self.UserAgent.ScheduleEvent(TriggerImmediately, Wait);
end;

//******************************************************************************
//* TIdSipStackInterfaceEventMethod                                            *
//******************************************************************************
//* TIdSipStackInterfaceEventMethod Public methods *****************************

procedure TIdSipStackInterfaceEventMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipStackListener).OnEvent(Self.Stack,
                                           Self.Action,
                                           Self.Event);
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
