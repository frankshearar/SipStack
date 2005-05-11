unit IdSipStackInterface;

interface

uses
  Classes, Contnrs, IdInterfacedObject, IdNotification, IdSipCore, IdSipMessage,
  IdSipTransaction, IdSipTransport, IdTimerQueue, SyncObjs, SysUtils, Messages,
  Windows;

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
                               IIdSipRegistrationListener,
                               IIdSipSessionListener,
                               IIdSipTransportSendingListener,
                               IIdSipUserAgentListener)
  private
    ActionLock: TCriticalSection;
    Actions:    TObjectList;
    fUiHandle:  HWnd;
    fUserAgent: TIdSipUserAgent;

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function  ActionFor(Handle: TIdSipHandle): TIdSipAction;
    function  AddAction(Action: TIdSipAction): TIdSipHandle;
    function  AssociationAt(Index: Integer): TIdActionAssociation;
    procedure DebugUnregister;
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
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts);

    procedure RemoveAction(Handle: TIdSipHandle);
    procedure SendAction(Action: TIdSipAction);
    procedure SynchronizedNotify(Notification: TIdSipStackInterfaceEventMethod);

    property UiHandle:  HWnd            read fUiHandle;
    property UserAgent: TIdSipUserAgent read fUserAgent;
  public
    constructor Create(UiHandle: HWnd); reintroduce;
    destructor  Destroy; override;

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
    procedure Send(ActionHandle: TIdSipHandle);
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
    procedure EatDirective(var Line: String);
    procedure ParseLine(UserAgent: TIdSipUserAgent;
                        const ConfigurationLine: String;
                        PendingActions: TObjectList);
    procedure RegisterUA(UserAgent: TIdSipAbstractUserAgent;
                         const RegisterLine: String;
                         PendingActions: TObjectList);
  public
    function CreateUserAgent(Configuration: TStrings;
                             Context: TIdTimerQueue): TIdSipUserAgent; overload;
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

  TIdInformationalData = class(TIdEventData)
  private
    fReason: String;
  public
    procedure Assign(Src: TPersistent); override;

    property Reason: String read fReason write fReason;
  end;

  TIdFailData = class(TIdInformationalData);
  TIdCallEndedData = class(TIdInformationalData);

  TIdDebugMessageData = class(TIdEventData)
  private
    fMessage: TIdSipMessage;
  public
    property Message: TIdSipMessage read fMessage write fMessage;
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

  TIdInboundCallData = class(TIdSessionData)
  private
    fFrom: TIdSipFromHeader;

    procedure SetFrom(Value: TIdSipFromHeader);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;

    property From: TIdSipFromHeader read fFrom write SetFrom;
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

// Configuration file constants
const
  AutoKeyword         = 'AUTO';
  ContactDirective    = ContactHeaderFull;
  FromDirective       = FromHeaderFull;
  ListenDirective     = 'Listen';
  MockKeyword         = 'MOCK';
  NameServerDirective = 'NameServer';
  ProxyDirective      = 'Proxy';
  RegisterDirective   = 'Register';

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

  CM_DEBUG = CM_BASE + 10000;

  CM_DEBUG_SEND_MSG = CM_DEBUG + 0;

type
  TIdSipEventMessage = packed record
    Event:    Cardinal;
    Data:     TIdSipStackInterfaceEventMethod;
    Reserved: DWord;
  end;

function LocalAddress: String;

implementation

uses
  IdGlobal, IdRandom, IdSimpleParser, IdSipIndyLocator, IdSipMockLocator,
  IdStack, IdUDPServer;

const
  ActionNotAllowedForHandle = 'You cannot perform that action on this handle (%d)';
  NoSuchHandle              = 'No such handle (%d)';

const
  MalformedConfigurationLine = 'Malformed configuration line: %s';

//******************************************************************************
//* Unit public procedures & functions                                         *
//******************************************************************************

function LocalAddress: String;
var
  UnusedServer: TIdUDPServer;
begin
  if not Assigned(GStack) then begin
    UnusedServer := TIdUDPServer.Create(nil);
    try
      Result := GStack.LocalAddress;
    finally
      UnusedServer.Free;
    end;
  end
  else
    Result := GStack.LocalAddress;
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

constructor TIdSipStackInterface.Create(UiHandle: HWnd);
var
  Conf:         TStrings;
  Configurator: TIdSipStackConfigurator;
  I:            Integer;
begin
  inherited Create(true);

  Self.ActionLock := TCriticalSection.Create;
  Self.Actions   := TObjectList.Create(true);

  Self.fUiHandle := UiHandle;

  Conf := TStringList.Create;
  try
    Conf.Add('Listen: UDP AUTO:5060');
    Conf.Add('NameServer: 62.241.160.200:53');
    Conf.Add('Contact: sip:foo@' + LocalAddress + ':5060');
    Conf.Add('From: sip:foo@' + LocalAddress + ':5060');
    Conf.Add('Register: sip:192.168.1.132');

    Configurator := TIdSipStackConfigurator.Create;
    try
      Self.fUserAgent := Configurator.CreateUserAgent(Conf, Self);
      Self.UserAgent.AddUserAgentListener(Self);

      for I := 0 to Self.UserAgent.Dispatcher.TransportCount - 1 do
        Self.UserAgent.Dispatcher.Transports[I].AddTransportSendingListener(Self);
    finally
      Configurator.Free;
    end;
  finally
    Conf.Free;
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

function TIdSipStackInterface.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TIdSipStackInterface._AddRef: Integer;
begin
  Result := -1;
end;

function TIdSipStackInterface._Release: Integer;
begin
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

procedure TIdSipStackInterface.DebugUnregister;
var
  Reg: TIdSipUri;
begin
  Reg := TIdSipUri.Create('sip:192.168.1.132');
  try
    Self.UserAgent.UnregisterFrom(Reg).Send;
  finally
    Reg.Free;
  end;
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
  Data: TIdCallEndedData;
begin
  Data := TIdCallEndedData.Create;
  try
    Data.Handle := Self.HandleFor(Session);
    Data.Reason := Reason;
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

    Self.NotifyEvent(RegisterAgent, CM_FAIL, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnInboundCall(UserAgent: TIdSipAbstractUserAgent;
                                             Session: TIdSipInboundSession);
var
  Data: TIdInboundCallData;
begin
  Session.AddSessionListener(Self);
  Self.AddAction(Session);

  Data := TIdInboundCallData.Create;
  try
    Data.Handle := Self.HandleFor(Session);
    Data.From := Session.InitialRequest.From;
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
                                                const Reason: String);
var
  Data: TIdFailData;
begin
  Data := TIdFailData.Create;
  try
    Data.Handle := Self.HandleFor(Action);
    Data.Reason := Reason;

    Self.NotifyEvent(Action, CM_NETWORK_FAILURE, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnSendRequest(Request: TIdSipRequest;
                                             Sender: TIdSipTransport);
var
  Data: TIdDebugMessageData;
begin
  Data := TIdDebugMessageData.Create;
  try
    Data.Handle := InvalidHandle;
    Data.Message := Request.Copy;

    Self.NotifyEvent(nil, CM_DEBUG_SEND_MSG, Data);
  finally
    Data.Free;
  end;
end;

procedure TIdSipStackInterface.OnSendResponse(Response: TIdSipResponse;
                                              Sender: TIdSipTransport);
var
  Data: TIdDebugMessageData;
begin
  // TODO Refactor this & OnSendRequest into NotifyOfSentMessage

  Data := TIdDebugMessageData.Create;
  try
    Data.Handle  := InvalidHandle;
    Data.Message := Response.Copy;

    Self.NotifyEvent(nil, CM_DEBUG_SEND_MSG, Data);
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
//* TIdSipStackConfigurator                                                    *
//******************************************************************************
//* TIdSipStackConfigurator Public methods *************************************

function TIdSipStackConfigurator.CreateUserAgent(Configuration: TStrings;
                                                 Context: TIdTimerQueue): TIdSipUserAgent;
var
  I:              Integer;
  PendingActions: TObjectList;
begin
  PendingActions := TObjectList.Create(false);
  try
    Result := TIdSipUserAgent.Create;
    try
      Result.Timer := Context;
      Result.Dispatcher := TIdSipTransactionDispatcher.Create(Result.Timer, nil);

      for I := 0 to Configuration.Count - 1 do
        Self.ParseLine(Result, Configuration[I], PendingActions);

      for I := 0 to PendingActions.Count - 1 do
        (PendingActions[I] as TIdSipAction).Send;
    except
      FreeAndNil(Result);

      raise;
    end;
  finally
    PendingActions.Free;
  end;
end;

//* TIdSipStackConfigurator Private methods ************************************

procedure TIdSipStackConfigurator.AddContact(UserAgent: TIdSipAbstractUserAgent;
                                             const ContactLine: String);
var
  Line: String;
begin
  Line := ContactLine;
  Self.EatDirective(Line);

  UserAgent.Contact.Value := Line;

  if UserAgent.Contact.IsMalformed then
    raise EParserError.Create(Format(MalformedConfigurationLine, [ContactLine]));
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

procedure TIdSipStackConfigurator.EatDirective(var Line: String);
begin
  Fetch(Line, ':');
  Line := Trim(Line);
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

  if      IsEqual(FirstToken, ContactDirective) then
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

procedure TIdSipStackConfigurator.RegisterUA(UserAgent: TIdSipAbstractUserAgent;
                                             const RegisterLine: String;
                                             PendingActions: TObjectList);
var
  Line:      String;
  Registrar: TIdSipUri;
begin
  // See class comment for the format for this directive.
  Line := RegisterLine;
  Self.EatDirective(Line);

  Line := Trim(Line);

  Registrar := TIdSipUri.Create(Line);
  try
    UserAgent.AutoReRegister := true;
    PendingActions.Add(UserAgent.RegisterWith(Registrar));
  finally
    Registrar.Free;
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
//* TIdInformationalData                                                       *
//******************************************************************************
//* TIdInformationalData Public methods ****************************************

procedure TIdInformationalData.Assign(Src: TPersistent);
var
  Other: TIdFailData;
begin
  inherited Assign(Src);

  if (Src is TIdFailData) then begin
    Other := Src as TIdFailData;
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
//* TIdInboundCallData                                                         *
//******************************************************************************
//* TIdInboundCallData Public methods ******************************************

constructor TIdInboundCallData.Create;
begin
  inherited Create;

  Self.fFrom := TIdSipFromHeader.Create;
end;

destructor TIdInboundCallData.Destroy;
begin
  Self.fFrom.Free;

 inherited Destroy;
end;

procedure TIdInboundCallData.Assign(Src: TPersistent);
var
  Other: TIdInboundCallData;
begin
  inherited Assign(Src);

  if (Src is TIdInboundCallData) then begin
    Other := Src as TIdInboundCallData;

    Self.From := Other.From;
  end;
end;

//* TIdInboundCallData Private methods *****************************************

procedure TIdInboundCallData.SetFrom(Value: TIdSipFromHeader);
begin
  Self.From.Assign(Value);
  Self.From.RemoveParameter(TagParam);
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
