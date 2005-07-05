{
  (c) 2005 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipSubscribeModule;

interface

uses
  Contnrs, IdNotification, IdSipCore, IdSipDialog, IdSipMessage, IdTimerQueue,
  SyncObjs;

type
  TIdSipOutboundNotify = class;

  IIdSipNotifyListener = interface(IIdSipActionListener)
    ['{15BEA69F-16D0-46C8-BB60-75F1CD3EC4CC}']
    procedure OnFailure(NotifyAgent: TIdSipOutboundNotify;
                        Response: TIdSipResponse);
    procedure OnSuccess(NotifyAgent: TIdSipOutboundNotify;
                        Response: TIdSipResponse);
  end;

  TIdSipOutboundSubscribe = class;

  IIdSipSubscribeListener = interface(IIdSipActionListener)
    ['{15BEA69F-16D0-46C8-BB60-75F1CD3EC4CC}']
    procedure OnFailure(SubscribeAgent: TIdSipOutboundSubscribe;
                        Response: TIdSipResponse);
    procedure OnSuccess(SubscribeAgent: TIdSipOutboundSubscribe;
                        Response: TIdSipResponse);
  end;

  TIdSipOutboundSubscription = class;

  // I define the protocol for things that listen for a particular
  // Subscription's events.
  // * OnEstablishedSubscription tells you that the target returned a 202
  //   Accepted or a 200 OK. This means that the remote end is prepared to
  //   notify you, or has consulted the user for permission.
  // * OnExpiredSubscription tells you that the target just sent you a NOTIFY
  //   terminating the subscription. Clear your references to the subscription
  //   because the Transaction-User Core will destroy the subscription after
  //   this.
  // * OnNotify tells you that the target notified you of some state change.
  //   This event will trigger on ALL notifications, so for instance you'll
  //   see this event fire just before OnExpiredSubscription.
  IIdSipSubscriptionListener = interface(IIdSipActionListener)
    ['{6A6F6A2D-D987-47BE-BC70-83622FF99CDF}']
    procedure OnEstablishedSubscription(Subscription: TIdSipOutboundSubscription;
                                        Notify: TIdSipRequest);
    procedure OnExpiredSubscription(Subscription: TIdSipOutboundSubscription;
                                    Notify: TIdSipRequest);
    procedure OnNotify(Subscription: TIdSipOutboundSubscription;
                       Notify: TIdSipRequest);
  end;

  TIdSipInboundSubscription = class;

  // I define the protocol for things that are interested in subscriptions.
  // * OnRenewedSubscription fires whenever something creates a new outbound
  //   Subscription as part of an automatic process: when the target
  //   deactivates the subscription, when the retry-after time elapses for some
  //   scheduled event, etc.
  IIdSipSubscribeModuleListener = interface(IIdSipMessageModuleListener)
    ['{9BF47363-0182-4E6E-88E0-A1898B3B779B}']
    procedure OnRenewedSubscription(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipOutboundSubscription);
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipInboundSubscription);
  end;

  TIdSipEventPackage = class;
  TIdSipEventPackageClass = class of TIdSipEventPackage;

  TIdSipSubscribeModule = class(TIdSipMessageModule)
  private
    fMinimumExpiryTime: Cardinal;
    Listeners:          TIdNotificationList;
    Packages:           TObjectList;

    function  DefaultSubscriptionDuration: Cardinal;
    function  KnowsEvent(const EventPackage: String): Boolean;
    procedure NotifyOfRenewedSubscription(NewSub: TIdSipOutboundSubscription);
    procedure NotifyOfSubscriptionRequest(Subscription: TIdSipInboundSubscription);
    function  PackageAt(Index: Integer): TIdSipEventPackage;
    procedure RejectUnknownEvent(Request: TIdSipRequest);
  public
    constructor Create(UA: TIdSipAbstractUserAgent); override;
    destructor  Destroy; override;

    function  Accept(Request: TIdSipRequest;
                     UsingSecureTransport: Boolean): TIdSipAction; override;
    function  AcceptsMethods: String; override;
    procedure AddListener(Listener: IIdSipSubscribeModuleListener);
    procedure AddPackage(PackageType: TIdSipEventPackageClass);
    function  AllowedEvents: String;
    function  CreateNotify(Dialog: TIdSipDialog;
                           Subscribe: TIdSipRequest;
                           const SubscriptionState: String): TIdSipRequest;
    function  CreateSubscribe(Dest: TIdSipAddressHeader;
                              const EventPackage: String): TIdSipRequest;
    procedure RemoveAllPackages;
    procedure RetrySubscriptionAfter(Target: TIdSipAddressHeader;
                                     const EventPackage: String;
                                     WaitTime: Cardinal);
    function  Resubscribe(Target: TIdSipAddressHeader;
                          const EventPackage: String): TIdSipOutboundSubscription;
    procedure RemoveListener(Listener: IIdSipSubscribeModuleListener);
    function  Subscribe(Target: TIdSipAddressHeader;
                        const EventPackage: String): TIdSipOutboundSubscription;
    function  WillAccept(Request: TIdSipRequest): Boolean; override;

    property MinimumExpiryTime: Cardinal read fMinimumExpiryTime write fMinimumExpiryTime;
  end;

  TIdSipEventPackage = class(TObject)
  public
    class function EventPackage: String; virtual; abstract;
  end;

  TIdSipReferPackage = class(TIdSipEventPackage)
  public
    class function EventPackage: String; override;
  end;

  TIdSipNotify = class(TIdSipAction)
  protected
    function CreateNewAttempt: TIdSipRequest; override;
  public
    class function Method: String; override;
  end;

  TIdSipOutboundNotify = class(TIdSipNotify);

  TIdSipSubscribe = class(TIdSipAction)
  private
    fDuration:     Cardinal; // in seconds
    fEventPackage: String;
    fID:           String;
    Module:        TIdSipSubscribeModule;
  protected
    function CreateNewAttempt: TIdSipRequest; override;
  public
    class function Method: String; override;

    constructor Create(UA: TIdSipAbstractUserAgent); override;

    property EventPackage: String   read fEventPackage write fEventPackage;
    property Duration:     Cardinal read fDuration write fDuration;
    property ID:           String   read fID write fID;
  end;

  TIdSipInboundSubscribe = class(TIdSipSubscribe)
  public
    constructor Create(UA: TIdSipAbstractUserAgent;
                       Sub: TIdSipRequest); reintroduce;

    procedure Accept(MaximumDuration: Cardinal);
    function  IsInbound: Boolean; override;
    function  IsUnsubscribe: Boolean;
  end;

  TIdSipOutboundSubscribe = class(TIdSipSubscribe)
  private
    fDestination: TIdSipAddressHeader;

    procedure NotifyOfSuccess(Response: TIdSipResponse);
    procedure SetDestination(Value: TIdSipAddressHeader);
  protected
    procedure NotifyOfFailure(Response: TIdSipResponse); override;
    function  ReceiveOKResponse(Response: TIdSipResponse;
                                UsingSecureTransport: Boolean): TIdSipActionStatus; override;
  public
    constructor Create(UA: TIdSipAbstractUserAgent); overload; override;
    destructor  Destroy; override;

    procedure AddListener(Listener: IIdSipSubscribeListener);
    function  Match(Msg: TIdSipMessage): Boolean; override;
    procedure RemoveListener(Listener: IIdSipSubscribeListener);
    procedure Send; override;

    property Destination: TIdSipAddressHeader read fDestination write SetDestination;
  end;

  TIdSipOutboundUnsubscribe = class(TIdSipOutboundSubscribe)
  public
    procedure Send; override;
  end;

  // I represent a subscription to another entity's state of some kind.
  // The relationship between me, TIdSip(In|Out)boundSubscribe and
  // TIdSip(In|Out)boundNotify resembles that between TIdSipSession,
  // TIdSip(In|Out)boundInvite, etc.
  TIdSipSubscription = class(TIdSipAction,
                             IIdSipActionListener)
  private
    fEventPackage: String;
    fID:           String;
    fTarget:       TIdSipAddressHeader;
    fTerminating:  Boolean;
    Module:        TIdSipSubscribeModule;

    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Challenge: TIdSipResponse);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
  protected
    function  CreateNewAttempt: TIdSipRequest; override;
  public
    class function Method: String; override;

    constructor Create(UA: TIdSipAbstractUserAgent); override;

    property EventPackage: String              read fEventPackage write fEventPackage;
    property ID:           String              read fID write fID;
    property Target:       TIdSipAddressHeader read fTarget write fTarget;
    property Terminating:  Boolean             read fTerminating;
  end;

  TIdSipInboundSubscription = class(TIdSipSubscription)
  private
    procedure ReturnAccept(Subscribe: TIdSipRequest);
  protected
    procedure ReceiveSubscribe(Request: TIdSipRequest); override;
  public
    constructor Create(UA: TIdSipAbstractUserAgent;
                       Subscribe: TIdSipRequest); reintroduce;

    procedure Accept;
    function  IsInbound: Boolean; override;
  end;

  TIdSipOutboundSubscription = class(TIdSipSubscription,
                                     IIdSipSubscribeListener)
  private
    Dialog:           TIdSipDialog;
    DialogLock:       TCriticalSection;
    InitialSubscribe: TIdSipOutboundSubscribe;
    RefreshSubscribe: TIdSipOutboundSubscribe;
    Unsubscriber:     TIdSipOutboundUnsubscribe;

    procedure ConfigureRequest(Sub: TIdSipOutboundSubscribe);
    function  CreateOutboundSubscribe: TIdSipOutboundSubscribe;
    function  DialogEstablished: Boolean;
    procedure EstablishDialog(Response: TIdSipResponse);
    procedure NotifyOfExpiredSubscription(Notify: TIdSipRequest);
    procedure NotifyOfReceivedNotify(Notify: TIdSipRequest);
    procedure NotifyOfSuccess(Notify: TIdSipRequest);
    procedure OnFailure(SubscribeAgent: TIdSipOutboundSubscribe;
                        Response: TIdSipResponse);
    procedure OnSuccess(SubscribeAgent: TIdSipOutboundSubscribe;
                        Response: TIdSipResponse);
    procedure RejectUnauthorized(Notify: TIdSipRequest);
    procedure ScheduleTermination(Expires: Cardinal);
    procedure SendResponseFor(Notify: TIdSipRequest);
    procedure StartNewSubscription(Notify: TIdSipRequest);
  protected
    procedure NotifyOfFailure(Response: TIdSipResponse); override;
    procedure ReceiveNotify(Notify: TIdSipRequest); override;
  public
    constructor Create(UA: TIdSipAbstractUserAgent); override;
    destructor  Destroy; override;

    procedure AddListener(Listener: IIdSipSubscriptionListener);
    function  Match(Msg: TIdSipMessage): Boolean; override;
    procedure Refresh;
    procedure RemoveListener(Listener: IIdSipSubscriptionListener);
    procedure Send; override;
    procedure Terminate; override;
    procedure Unsubscribe;
  end;

  TIdSipSubscriptionExpiresWait = class(TIdWait)
  private
    fSubscription: TIdSipOutboundSubscription;
  public
    procedure Trigger; override;

    property Subscription: TIdSipOutboundSubscription read fSubscription write fSubscription;
  end;

  TIdSipSubscriptionRetryWait = class(TIdWait)
  private
    fEventPackage: String;
    fTarget:       TIdSipAddressHeader;
    fUserAgent:    TIdSipAbstractUserAgent;

    procedure SetTarget(Value: TIdSipAddressHeader);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Trigger; override;

    property EventPackage: String                  read fEventPackage write fEventPackage;
    property Target:       TIdSipAddressHeader     read fTarget write SetTarget;
    property UserAgent:    TIdSipAbstractUserAgent read fUserAgent write fUserAgent;
  end;

  TIdSipNotifyMethod = class(TIdNotification)
  private
    fResponse: TIdSipResponse;
    fNotify:   TIdSipOutboundNotify;
  public
    property Response: TIdSipResponse       read fResponse write fResponse;
    property Notify:   TIdSipOutboundNotify read fNotify write fNotify;
  end;

  TIdSipNotifyFailedMethod = class(TIdSipNotifyMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSipNotifySucceededMethod = class(TIdSipNotifyMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSipOutboundSubscribeMethod = class(TIdNotification)
  private
    fResponse:  TIdSipResponse;
    fSubscribe: TIdSipOutboundSubscribe;
  public
    property Response:  TIdSipResponse          read fResponse write fResponse;
    property Subscribe: TIdSipOutboundSubscribe read fSubscribe write fSubscribe;
  end;

  TIdSipOutboundSubscribeFailedMethod = class(TIdSipOutboundSubscribeMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSipOutboundSubscribeSucceededMethod = class(TIdSipOutboundSubscribeMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSipSubscriptionMethod = class(TIdNotification)
  private
    fSubscription: TIdSipOutboundSubscription;
  public
    property Subscription: TIdSipOutboundSubscription read fSubscription write fSubscription;
  end;

  TIdSipEstablishedSubscriptionMethod = class(TIdSipSubscriptionMethod)
  private
    fNotify: TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property Notify: TIdSipRequest read fNotify write fNotify;
  end;

  TIdSipExpiredSubscriptionMethod = class(TIdSipSubscriptionMethod)
  private
    fNotify: TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property Notify: TIdSipRequest read fNotify write fNotify;
  end;

  TIdSipSubscriptionNotifyMethod = class(TIdSipSubscriptionMethod)
  private
    fNotify: TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property Notify: TIdSipRequest read fNotify write fNotify;
  end;

  TIdSipRenewedSubscriptionMethod = class(TIdSipUserAgentMethod)
  private
    fSubscription: TIdSipOutboundSubscription;
  public
    procedure Run(const Subject: IInterface); override;

    property Subscription: TIdSipOutboundSubscription read fSubscription write fSubscription;
  end;

  TIdSipSubscriptionRequestMethod = class(TIdSipUserAgentMethod)
  private
    fSubscription: TIdSipInboundSubscription;
  public
    procedure Run(const Subject: IInterface); override;

    property Subscription: TIdSipInboundSubscription read fSubscription write fSubscription;
  end;

implementation

uses
  SysUtils;

//******************************************************************************
//* TIdSipSubscribeModule                                                      *
//******************************************************************************
//* TIdSipSubscribeModule Public methods ***************************************

constructor TIdSipSubscribeModule.Create(UA: TIdSipAbstractUserAgent);
begin
  inherited Create(UA);

  Self.Listeners := TIdNotificationList.Create;
  Self.Packages  := TObjectList.Create(true);
end;

destructor TIdSipSubscribeModule.Destroy;
begin
  Self.Packages.Free;
  Self.Listeners.Free;

  inherited Destroy;
end;

function TIdSipSubscribeModule.Accept(Request: TIdSipRequest;
                                      UsingSecureTransport: Boolean): TIdSipAction;
var
  Subscription: TIdSipInboundSubscription;
begin
  Result := nil;

  // While this module supports SUBSCRIBE and NOTIFY messages, only SUBSCRIBE
  // messages can set up subscriptions: NOTIFYs always occur within the context
  // of a subscription.
  if not Request.IsSubscribe then
    Exit;

  if Self.KnowsEvent(Request.FirstEvent.EventType) then begin
    Subscription := TIdSipInboundSubscription.Create(Self.UserAgent, Request);
    Self.NotifyOfSubscriptionRequest(Subscription);
    Result := Subscription;
  end
  else begin
    Result := nil;
    Self.RejectUnknownEvent(Request);
  end;
end;

function TIdSipSubscribeModule.AcceptsMethods: String;
begin
  Result := MethodSubscribe + ', '
          + MethodNotify;
end;

procedure TIdSipSubscribeModule.AddListener(Listener: IIdSipSubscribeModuleListener);
begin
  Self.Listeners.AddListener(Listener);
end;

procedure TIdSipSubscribeModule.AddPackage(PackageType: TIdSipEventPackageClass);
begin
  Self.Packages.Add(PackageType.Create);
end;

function TIdSipSubscribeModule.AllowedEvents: String;
var
  I: Integer;
begin
  Result := '';

  if (Self.Packages.Count > 0) then begin
    Result := Self.PackageAt(0).EventPackage;

    for I := 1 to Self.Packages.Count - 1 do
      Result := Result + ', ' + Self.PackageAt(I).EventPackage;
  end;
end;

function TIdSipSubscribeModule.CreateNotify(Dialog: TIdSipDialog;
                                            Subscribe: TIdSipRequest;
                                            const SubscriptionState: String): TIdSipRequest;
begin
  Result := Self.UserAgent.CreateRequest(MethodNotify, Dialog);
  try
    // cf RFC 3265, section 3.2.2

    Result.AddHeader(Subscribe.FirstEvent);
    Result.AddHeader(SubscriptionStateHeader).Value := SubscriptionState;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipSubscribeModule.CreateSubscribe(Dest: TIdSipAddressHeader;
                                                 const EventPackage: String): TIdSipRequest;
begin
  Result := Self.UserAgent.CreateRequest(MethodSubscribe, Dest);
  try
    Result.AddHeader(EventHeaderFull).Value := EventPackage;
    Result.AddHeader(ExpiresHeader);
    Result.FirstExpires.NumericValue := Self.DefaultSubscriptionDuration;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

procedure TIdSipSubscribeModule.RemoveAllPackages;
begin
  Self.Packages.Clear;
end;

procedure TIdSipSubscribeModule.RetrySubscriptionAfter(Target: TIdSipAddressHeader;
                                                       const EventPackage: String;
                                                       WaitTime: Cardinal);
var
  Wait: TIdSipSubscriptionRetryWait;
begin
  Wait := TIdSipSubscriptionRetryWait.Create;
  Wait.EventPackage := EventPackage;
  Wait.Target       := Target;
  Self.UserAgent.ScheduleEvent(WaitTime, Wait);
end;

function TIdSipSubscribeModule.Resubscribe(Target: TIdSipAddressHeader;
                                           const EventPackage: String): TIdSipOutboundSubscription;
begin
  Result := Self.Subscribe(Target, EventPackage);
  Self.NotifyOfRenewedSubscription(Result);
end;

procedure TIdSipSubscribeModule.RemoveListener(Listener: IIdSipSubscribeModuleListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

function TIdSipSubscribeModule.Subscribe(Target: TIdSipAddressHeader;
                                         const EventPackage: String): TIdSipOutboundSubscription;
var
  Sub: TIdSipOutboundSubscription;
begin
  Sub := Self.UserAgent.AddOutboundAction(TIdSipOutboundSubscription) as TIdSipOutboundSubscription;

  Sub.Target       := Target;
  Sub.EventPackage := EventPackage;

  Result := Sub;
end;

function TIdSipSubscribeModule.WillAccept(Request: TIdSipRequest): Boolean;
begin
  Result := Request.IsSubscribe or Request.IsNotify;
end;

//* TIdSipSubscribeModule Private methods **************************************

function TIdSipSubscribeModule.DefaultSubscriptionDuration: Cardinal;
begin
  Result := OneHour;
end;

function TIdSipSubscribeModule.KnowsEvent(const EventPackage: String): Boolean;
begin
  Result := Pos(EventPackage, Self.AllowedEvents) > 0;
end;

procedure TIdSipSubscribeModule.NotifyOfRenewedSubscription(NewSub: TIdSipOutboundSubscription);
var
  Notification: TIdSipRenewedSubscriptionMethod;
begin
  Notification := TIdSipRenewedSubscriptionMethod.Create;
  try
    Notification.Subscription := NewSub;
    Notification.UserAgent    := Self.UserAgent;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipSubscribeModule.NotifyOfSubscriptionRequest(Subscription: TIdSipInboundSubscription);
var
  Notification: TIdSipSubscriptionRequestMethod;
begin
  Notification := TIdSipSubscriptionRequestMethod.Create;
  try
  finally
    Notification.Subscription := Subscription;
    Notification.UserAgent    := Self.UserAgent;

    Self.Listeners.Notify(Notification);
  end;
end;

function TIdSipSubscribeModule.PackageAt(Index: Integer): TIdSipEventPackage;
begin
  Result := Self.Packages[Index] as TIdSipEventPackage;
end;

procedure TIdSipSubscribeModule.RejectUnknownEvent(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.UserAgent.CreateResponse(Request, SIPBadEvent);
  try
    Response.AddHeader(AllowEventsHeaderFull).Value := Self.AllowedEvents;

    Self.UserAgent.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

//******************************************************************************
//* TIdSipReferPackage                                                         *
//******************************************************************************
//* TIdSipReferPackage Public methods ******************************************

class function TIdSipReferPackage.EventPackage: String;
begin
  Result := PackageRefer;
end;

//******************************************************************************
//* TIdSipNotify                                                               *
//******************************************************************************
//* TIdSipNotify Public methods ************************************************

class function TIdSipNotify.Method: String;
begin
  Result := MethodNotify;
end;

//* TIdSipNotify Protected methods *********************************************

function TIdSipNotify.CreateNewAttempt: TIdSipRequest;
begin
  raise Exception.Create('Implement TIdSipNotify.CreateNewAttempt');
end;

//******************************************************************************
//* TIdSipSubscribe                                                            *
//******************************************************************************
//* TIdSipSubscribe Public methods *********************************************

class function TIdSipSubscribe.Method: String;
begin
  Result := MethodSubscribe;
end;

constructor TIdSipSubscribe.Create(UA: TIdSipAbstractUserAgent);
begin
  inherited Create(UA);

  Self.Module := Self.UA.ModuleFor(Self.Method) as TIdSipSubscribeModule;
end;

//* TIdSipSubscribe Protected methods ******************************************

function TIdSipSubscribe.CreateNewAttempt: TIdSipRequest;
var
  TempTo: TIdSipToHeader;
begin
  TempTo := TIdSipToHeader.Create;
  try
    TempTo.Address := Self.InitialRequest.RequestUri;

    Result := Self.Module.CreateSubscribe(TempTo, Self.EventPackage);
  finally
    TempTo.Free;
  end;
end;

//******************************************************************************
//* TIdSipInboundSubscribe                                                     *
//******************************************************************************
//* TIdSipInboundSubscribe Public methods **************************************

constructor TIdSipInboundSubscribe.Create(UA: TIdSipAbstractUserAgent;
                                          Sub: TIdSipRequest);
begin
  inherited Create(UA);

  Self.InitialRequest.Assign(Sub);
  Self.ReceiveRequest(Sub);
end;

procedure TIdSipInboundSubscribe.Accept(MaximumDuration: Cardinal);
var
  OK: TIdSipResponse;
begin
  OK := TIdSipResponse.InResponseTo(Self.InitialRequest, SIPAccepted);
  try
    OK.AddHeader(ExpiresHeader);
    OK.FirstExpires.NumericValue := MaximumDuration;

    if Self.InitialRequest.HasHeader(ExpiresHeader) and
      (OK.FirstExpires.NumericValue > Self.InitialRequest.FirstExpires.NumericValue) then
      OK.FirstExpires.NumericValue := Self.InitialRequest.FirstExpires.NumericValue;

    Self.SendResponse(OK);
  finally
    OK.Free;
  end;
end;

function TIdSipInboundSubscribe.IsInbound: Boolean;
begin
  Result := true;
end;

function TIdSipInboundSubscribe.IsUnsubscribe: Boolean;
begin
  Result := Self.InitialRequest.HasHeader(ExpiresHeader)
        and (Self.InitialRequest.FirstExpires.NumericValue = 0);
end;

//******************************************************************************
//* TIdSipOutboundSubscribe                                                    *
//******************************************************************************
//* TIdSipOutboundSubscribe Public methods *************************************

constructor TIdSipOutboundSubscribe.Create(UA: TIdSipAbstractUserAgent);
begin
  inherited Create(UA);

  Self.fDestination := TIdSipAddressHeader.Create;
end;

destructor TIdSipOutboundSubscribe.Destroy;
begin
  Self.fDestination.Free;

  inherited Destroy;
end;

procedure TIdSipOutboundSubscribe.AddListener(Listener: IIdSipSubscribeListener);
begin
  Self.Listeners.AddListener(Listener);
end;

function TIdSipOutboundSubscribe.Match(Msg: TIdSipMessage): Boolean;
begin
  Result := false;

  if Msg.IsResponse then begin
    Result := Self.InitialRequest.CSeq.Equals(Msg.CSeq)
          and (Self.InitialRequest.CallID = Msg.CallID)
          and (Self.InitialRequest.From.Tag = Msg.From.Tag);
  end;
end;

procedure TIdSipOutboundSubscribe.RemoveListener(Listener: IIdSipSubscribeListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

procedure TIdSipOutboundSubscribe.Send;
var
  Sub: TIdSipRequest;
begin
  inherited Send;

  Sub := Self.Module.CreateSubscribe(Self.Destination, Self.EventPackage);
  try
    if (Self.ID <> '') then
      Sub.FirstHeader(EventHeaderFull).Params[IdParam] := Self.ID;
      
    Sub.FirstExpires.NumericValue := Self.Duration;
    Self.InitialRequest.Assign(Sub);

    Self.SendRequest(Sub);
  finally
    Sub.Free;
  end;
end;

//* TIdSipOutboundSubscribe Protected methods **********************************

procedure TIdSipOutboundSubscribe.NotifyOfFailure(Response: TIdSipResponse);
var
  Notification: TIdSipOutboundSubscribeFailedMethod;
begin
  Notification := TIdSipOutboundSubscribeFailedMethod.Create;
  try
    Notification.Response  := Response;
    Notification.Subscribe := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

function TIdSipOutboundSubscribe.ReceiveOKResponse(Response: TIdSipResponse;
                                                   UsingSecureTransport: Boolean): TIdSipActionStatus;
begin
  Result := inherited ReceiveOKResponse(Response, UsingSecureTransport);

  Self.NotifyOfSuccess(Response);
end;

//* TIdSipOutboundSubscribe Private methods ************************************

procedure TIdSipOutboundSubscribe.NotifyOfSuccess(Response: TIdSipResponse);
var
  Notification: TIdSipOutboundSubscribeSucceededMethod;
begin
  Notification := TIdSipOutboundSubscribeSucceededMethod.Create;
  try
    Notification.Response  := Response;
    Notification.Subscribe := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipOutboundSubscribe.SetDestination(Value: TIdSipAddressHeader);
begin
  Self.Destination.Assign(Value);
end;

//******************************************************************************
//* TIdSipOutboundUnSubscribe                                                  *
//******************************************************************************
//* TIdSipOutboundUnSubscribe Public methods ***********************************

procedure TIdSipOutboundUnsubscribe.Send;
var
  Sub: TIdSipRequest;
begin
  inherited Send;

  Sub := Self.Module.CreateSubscribe(Self.Destination, Self.EventPackage);
  try
    Sub.FirstExpires.NumericValue := 0;
    Self.InitialRequest.Assign(Sub);

    Self.SendRequest(Sub);
  finally
    Sub.Free;
  end;
end;

//******************************************************************************
//* TIdSipSubscription                                                         *
//******************************************************************************
//* TIdSipSubscription Public methods ******************************************

class function TIdSipSubscription.Method: String;
begin
  Result := MethodSubscribe;
end;

constructor TIdSipSubscription.Create(UA: TIdSipAbstractUserAgent);
begin
  inherited Create(UA);

  Self.fTerminating := false;
  Self.Module := Self.UA.ModuleFor(Self.Method) as TIdSipSubscribeModule;
end;

//* TIdSipSubscription Protected methods ***************************************

function TIdSipSubscription.CreateNewAttempt: TIdSipRequest;
begin
  raise Exception.Create('Implement TIdSipSubscription.CreateNewAttempt');
end;

//* TIdSipSubscription Private methods *****************************************

procedure TIdSipSubscription.OnAuthenticationChallenge(Action: TIdSipAction;
                                                       Challenge: TIdSipResponse);
begin
  raise Exception.Create('implement TIdSipSubscription.OnAuthenticationChallenge');
end;

procedure TIdSipSubscription.OnNetworkFailure(Action: TIdSipAction;
                                              ErrorCode: Cardinal;
                                              const Reason: String);
begin
  Self.NotifyOfNetworkFailure(ErrorCode, Reason);
end;

//******************************************************************************
//* TIdSipInboundSubscription                                                  *
//******************************************************************************
//* TIdSipInboundSubscription Public methods ***********************************

constructor TIdSipInboundSubscription.Create(UA: TIdSipAbstractUserAgent;
                                             Subscribe: TIdSipRequest);
begin
  inherited Create(UA);

  Self.InitialRequest.Assign(Subscribe);
end;

procedure TIdSipInboundSubscription.Accept;
var
  ActiveNotify: TIdSipRequest;
begin
end;

function TIdSipInboundSubscription.IsInbound: Boolean;
begin
  Result := true;
end;

//* TIdSipInboundSubscription Protected methods ********************************

procedure TIdSipInboundSubscription.ReceiveSubscribe(Request: TIdSipRequest);
begin
  // If we get this far then the SubscribeModule's decided that we can
  // accept the SUBSCRIBE.

  inherited ReceiveSubscribe(Request);

  Self.ReturnAccept(Request);
end;

//* TIdSipInboundSubscription Private methods **********************************

procedure TIdSipInboundSubscription.ReturnAccept(Subscribe: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.UA.CreateResponse(Subscribe, SIPAccepted);
  try
    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

//******************************************************************************
//* TIdSipOutboundSubscription                                                 *
//******************************************************************************
//* TIdSipOutboundSubscription Public methods **********************************

constructor TIdSipOutboundSubscription.Create(UA: TIdSipAbstractUserAgent);
begin
  inherited Create(UA);

  Self.DialogLock       := TCriticalSection.Create;
  Self.InitialSubscribe := Self.CreateOutboundSubscribe;
end;

destructor TIdSipOutboundSubscription.Destroy;
begin
  Self.DialogLock.Acquire;
  try
    Self.Dialog.Free;
  finally
    Self.DialogLock.Release;
  end;
  Self.DialogLock.Free;

  inherited Destroy;
end;

procedure TIdSipOutboundSubscription.AddListener(Listener: IIdSipSubscriptionListener);
begin
  Self.Listeners.AddListener(Listener);
end;

function TIdSipOutboundSubscription.Match(Msg: TIdSipMessage): Boolean;
var
  Req: TIdSipRequest;
begin
  Result := false;
  if Msg.IsRequest then begin
    Req := Msg as TIdSipRequest;

    if Req.IsNotify then begin
      Result := (Self.InitialRequest.CallID = Req.CallID)
            and (Self.InitialRequest.From.Tag = Req.ToHeader.Tag)
            and Req.HasHeader(EventHeaderFull)
            and (Self.InitialRequest.FirstEvent.Equals(Req.FirstEvent));
    end
  end
    else
      Result := inherited Match(Msg);
end;

procedure TIdSipOutboundSubscription.Refresh;
begin
  // cf. RFC 3265, section 3.1.4.2.

  if not Assigned(Self.RefreshSubscribe) then begin
    Self.RefreshSubscribe := Self.CreateOutboundSubscribe;
    Self.ConfigureRequest(Self.RefreshSubscribe);
    Self.RefreshSubscribe.Send;
  end;
end;

procedure TIdSipOutboundSubscription.RemoveListener(Listener: IIdSipSubscriptionListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

procedure TIdSipOutboundSubscription.Send;
begin
  inherited Send;

  Self.ConfigureRequest(Self.InitialSubscribe);
  Self.InitialSubscribe.Send;
  Self.InitialRequest.Assign(Self.InitialSubscribe.InitialRequest);
end;

procedure TIdSipOutboundSubscription.Terminate;
begin
  Self.Unsubscriber := Self.UA.AddOutboundAction(TIdSipOutboundUnsubscribe) as TIdSipOutboundUnsubscribe;
  Self.Unsubscriber.AddListener(Self);
  Self.ConfigureRequest(Self.Unsubscriber);
  Self.Unsubscriber.Send;
  Self.fTerminating := true;
end;

procedure TIdSipOutboundSubscription.Unsubscribe;
begin
  Self.Terminate;
end;

//* TIdSipOutboundSubscription Protected methods *******************************

procedure TIdSipOutboundSubscription.NotifyOfFailure(Response: TIdSipResponse);
var
  Notification: TIdSipExpiredSubscriptionMethod;
begin
  Notification := TIdSipExpiredSubscriptionMethod.Create;
  try
    Notification.Subscription := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipOutboundSubscription.ReceiveNotify(Notify: TIdSipRequest);
var
  State: TIdSipSubscriptionStateHeader;
begin
  // Precondition: Request contains a NOTIFY.
  inherited ReceiveNotify(Notify);

  if not Notify.HasAuthorization then begin
    Self.RejectUnauthorized(Notify);
    Exit;
  end;

  // if not authenticated then
  //   issue challenge
  //   exit

  // cf. RFC 3265, section 3.2.4
  Self.NotifyOfReceivedNotify(Notify);

  State := Notify.FirstSubscriptionState;

  if State.IsActive then begin
    if (State.Expires > 0) then
      Self.ScheduleTermination(State.Expires);

    Self.NotifyOfSuccess(Notify);
  end
  else if State.IsPending then begin
    if (State.Expires > 0) then
      Self.ScheduleTermination(State.Expires);
  end
  else if State.IsTerminated then begin
    if not (State.IsRejected or State.IsNoResource) then begin
      Self.StartNewSubscription(Notify);
    end;

    Self.NotifyOfExpiredSubscription(Notify);
    Self.MarkAsTerminated;
  end;

  Self.SendResponseFor(Notify);
end;

//* TIdSipOutboundSubscription Private methods *********************************


procedure TIdSipOutboundSubscription.ConfigureRequest(Sub: TIdSipOutboundSubscribe);
begin
  Sub.Destination  := Self.Target;
  Sub.EventPackage := Self.EventPackage;
  Sub.ID           := Self.ID;
end;

function TIdSipOutboundSubscription.CreateOutboundSubscribe: TIdSipOutboundSubscribe;
begin
  Result := Self.UA.AddOutboundAction(TIdSipOutboundSubscribe) as TIdSipOutboundSubscribe;
  Result.AddListener(Self);
end;

function TIdSipOutboundSubscription.DialogEstablished: Boolean;
begin
  Result := Assigned(Self.Dialog);
end;

procedure TIdSipOutboundSubscription.EstablishDialog(Response: TIdSipResponse);
begin
  Self.DialogLock.Acquire;
  try
    if not Self.DialogEstablished then
      Self.Dialog := TIdSipDialog.CreateInboundDialog(Self.InitialRequest, Response, false);
  finally
    Self.DialogLock.Release;
  end;
end;

procedure TIdSipOutboundSubscription.NotifyOfExpiredSubscription(Notify: TIdSipRequest);
var
  Notification: TIdSipExpiredSubscriptionMethod;
begin
  Notification := TIdSipExpiredSubscriptionMethod.Create;
  try
    Notification.Notify       := Notify;
    Notification.Subscription := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipOutboundSubscription.NotifyOfReceivedNotify(Notify: TIdSipRequest);
var
  Notification: TIdSipSubscriptionNotifyMethod;
begin
  Notification := TIdSipSubscriptionNotifyMethod.Create;
  try
    Notification.Notify       := Notify;
    Notification.Subscription := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipOutboundSubscription.NotifyOfSuccess(Notify: TIdSipRequest);
var
  Notification: TIdSipEstablishedSubscriptionMethod;
begin
  Notification := TIdSipEstablishedSubscriptionMethod.Create;
  try
    Notification.Subscription := Self;
    Notification.Notify       := Notify;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipOutboundSubscription.OnFailure(SubscribeAgent: TIdSipOutboundSubscribe;
                                               Response: TIdSipResponse);
begin
  if (Self.InitialSubscribe = SubscribeAgent) then begin
    Self.NotifyOfFailure(Response);
    Self.InitialSubscribe := nil;
  end
  else if (Self.RefreshSubscribe = SubscribeAgent) then begin
    if (Response.StatusCode = SIPCallLegOrTransactionDoesNotExist) then
      Self.NotifyOfFailure(Response);
    Self.RefreshSubscribe := nil;
  end
  else if (Self.Unsubscriber = SubscribeAgent) then begin
    Assert(Self.Terminating,
           'Not flagged as Terminating but the Unsubscriber just failed.');
    Self.Unsubscriber := nil;
    Self.MarkAsTerminated;
  end;
end;

procedure TIdSipOutboundSubscription.OnSuccess(SubscribeAgent: TIdSipOutboundSubscribe;
                                               Response: TIdSipResponse);
begin
  if (Self.InitialSubscribe = SubscribeAgent) then begin
    Self.InitialSubscribe := nil;
    Self.EstablishDialog(Response);

    // Update expiry time;
    // reschedule a refresh
  end
  else if (Self.RefreshSubscribe = SubscribeAgent) then begin
    Self.RefreshSubscribe := nil;
  end
end;

procedure TIdSipOutboundSubscription.RejectUnauthorized(Notify: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.UA.CreateChallengeResponseAsUserAgent(Notify);
  try
    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipOutboundSubscription.ScheduleTermination(Expires: Cardinal);
var
  Wait: TIdSipSubscriptionExpiresWait;
begin
  Wait := TIdSipSubscriptionExpiresWait.Create;
  Wait.Subscription := Self;

  // Expires contains a value in seconds
  Self.UA.ScheduleEvent(Expires*1000, Wait);
end;

procedure TIdSipOutboundSubscription.SendResponseFor(Notify: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Notify, SIPOK);
  try
    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipOutboundSubscription.StartNewSubscription(Notify: TIdSipRequest);
var
  RetryAfter: Cardinal;
  State:      TIdSipSubscriptionStateHeader;
  Wait:       TIdSipSubscriptionRetryWait;
begin
  State := Notify.FirstSubscriptionState;

  // Subscription-State's retry-after is in seconds
  RetryAfter := State.RetryAfter * 1000;

  if (RetryAfter = 0) then
    Self.Module.Resubscribe(Self.Target, Self.EventPackage)
  else begin
    if not (State.IsDeactivated or State.IsTimedOut) then begin
      Wait := TIdSipSubscriptionRetryWait.Create;
      Wait.EventPackage := Self.EventPackage;
      Wait.Target       := Self.Target;
      Wait.UserAgent    := Self.UA;
      Self.UA.ScheduleEvent(RetryAfter, Wait);
    end;
  end;
end;

//******************************************************************************
//* TIdSipSubscriptionExpiresWait                                              *
//******************************************************************************
//* TIdSipSubscriptionExpiresWait Public methods *******************************

procedure TIdSipSubscriptionExpiresWait.Trigger;
begin
  Self.Subscription.Terminate;
end;

//******************************************************************************
//* TIdSipSubscriptionRetryWait                                                *
//******************************************************************************
//* TIdSipSubscriptionRetryWait Public methods *********************************

constructor TIdSipSubscriptionRetryWait.Create;
begin
  inherited Create;

  Self.fTarget := TIdSipAddressHeader.Create;
end;

destructor TIdSipSubscriptionRetryWait.Destroy;
begin
  Self.Target.Free;

  inherited Destroy;
end;

procedure TIdSipSubscriptionRetryWait.Trigger;
var
  Module: TIdSipSubscribeModule;
begin
  Module := Self.UserAgent.ModuleFor(MethodSubscribe) as TIdSipSubscribeModule;

  if Assigned(Module) then
    Module.Resubscribe(Self.Target, Self.EventPackage);
end;

//* TIdSipSubscriptionRetryWait Private methods ********************************

procedure TIdSipSubscriptionRetryWait.SetTarget(Value: TIdSipAddressHeader);
begin
  Self.fTarget.Value := Value.FullValue;
end;

//******************************************************************************
//* TIdSipNotifyFailedMethod                                                   *
//******************************************************************************
//* TIdSipNotifyFailedMethod Public methods ************************************

procedure TIdSipNotifyFailedMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipNotifyListener).OnFailure(Self.Notify,
                                              Self.Response);
end;

//******************************************************************************
//* TIdSipNotifySucceededMethod                                                *
//******************************************************************************
//* TIdSipNotifySucceededMethod Public methods *********************************

procedure TIdSipNotifySucceededMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipNotifyListener).OnSuccess(Self.Notify,
                                              Self.Response);
end;

//******************************************************************************
//* TIdSipOutboundSubscribeFailedMethod                                        *
//******************************************************************************
//* TIdSipOutboundSubscribeFailedMethod Public methods *************************

procedure TIdSipOutboundSubscribeFailedMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSubscribeListener).OnFailure(Self.Subscribe,
                                                 Self.Response);
end;

//******************************************************************************
//* TIdSipOutboundSubscribeSucceededMethod                                     *
//******************************************************************************
//* TIdSipOutboundSubscribeSucceededMethod Public methods **********************

procedure TIdSipOutboundSubscribeSucceededMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSubscribeListener).OnSuccess(Self.Subscribe,
                                                 Self.Response);
end;

//******************************************************************************
//* TIdSipEstablishedSubscriptionMethod                                        *
//******************************************************************************
//* TIdSipEstablishedSubscriptionMethod Public methods *************************

procedure TIdSipEstablishedSubscriptionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSubscriptionListener).OnEstablishedSubscription(Self.Subscription,
                                                                    Self.Notify);
end;

//******************************************************************************
//* TIdSipExpiredSubscriptionMethod                                            *
//******************************************************************************
//* TIdSipExpiredSubscriptionMethod Public methods *****************************

procedure TIdSipExpiredSubscriptionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSubscriptionListener).OnExpiredSubscription(Self.Subscription,
                                                                Self.Notify);
end;

//******************************************************************************
//* TIdSipSubscriptionNotifyMethod                                             *
//******************************************************************************
//* TIdSipSubscriptionNotifyMethod Public methods ******************************

procedure TIdSipSubscriptionNotifyMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSubscriptionListener).OnNotify(Self.Subscription,
                                                   Self.Notify);
end;

//******************************************************************************
//* TIdSipRenewedSubscriptionMethod                                            *
//******************************************************************************
//* TIdSipRenewedSubscriptionMethod Public methods *****************************

procedure TIdSipRenewedSubscriptionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSubscribeModuleListener).OnRenewedSubscription(Self.UserAgent,
                                                                   Self.Subscription);
end;

//******************************************************************************
//* TIdSipSubscriptionRequestMethod                                            *
//******************************************************************************
//* TIdSipSubscriptionRequestMethod Public methods *****************************

procedure TIdSipSubscriptionRequestMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSubscribeModuleListener).OnSubscriptionRequest(Self.UserAgent,
                                                                   Self.Subscription);
end;

end.
