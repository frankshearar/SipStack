unit IdSipSubscribeModule;

interface

uses
  Contnrs, IdNotification, IdSipCore, IdSipMessage;

type
  IIdSipSubscribeModuleListener = interface(IIdSipMessageModuleListener)
    ['{9BF47363-0182-4E6E-88E0-A1898B3B779B}']
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipInboundSubscription);
  end;

  TIdSipEventPackage = class;
  TIdSipEventPackageClass = class of TIdSipEventPackage;

  TIdSipSubscribeModule = class(TIdSipMessageModule)
  private
    Listeners: TIdNotificationList;
    Packages:  TObjectList;

    function  KnowsEvent(const EventPackage: String): Boolean;
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
    procedure RemoveAllPackages;
    procedure RemoveListener(Listener: IIdSipSubscribeModuleListener);
    function  WillAccept(Request: TIdSipRequest): Boolean; override;
  end;

  TIdSipEventPackage = class(TObject)
  public
    class function EventPackage: String; virtual; abstract;
  end;

  TIdSipReferPackage = class(TIdSipEventPackage)
  public
    class function EventPackage: String; override;
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
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Response: TIdSipResponse read fResponse write fResponse;
  end;

  TIdSipExpiredSubscriptionMethod = class(TIdSipSubscriptionMethod)
  private
    fNotify: TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property Notify: TIdSipRequest read fNotify write fNotify;
  end;

  TIdSipRenewedSubscriptionMethod = class(TIdSipSubscriptionMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSipSubscriptionNotifyMethod = class(TIdSipSubscriptionMethod)
  private
    fNotify: TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property Notify: TIdSipRequest read fNotify write fNotify;
  end;

  TIdSipSubscriptionRequestMethod = class(TIdSipUserAgentMethod)
  private
    fSubscription: TIdSipInboundSubscription;
  public
    procedure Run(const Subject: IInterface); override;

    property Subscription: TIdSipInboundSubscription read fSubscription write fSubscription;
  end;

implementation

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

procedure TIdSipSubscribeModule.RemoveAllPackages;
begin
  Self.Packages.Clear;
end;

procedure TIdSipSubscribeModule.RemoveListener(Listener: IIdSipSubscribeModuleListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

function TIdSipSubscribeModule.WillAccept(Request: TIdSipRequest): Boolean;
begin
  Result := Request.IsSubscribe or Request.IsNotify;
end;

//* TIdSipSubscribeModule Private methods **************************************

function TIdSipSubscribeModule.KnowsEvent(const EventPackage: String): Boolean;
begin
  Result := Pos(EventPackage, Self.AllowedEvents) > 0;
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
                                                                    Self.Response);
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
//* TIdSipRenewedSubscriptionMethod                                            *
//******************************************************************************
//* TIdSipRenewedSubscriptionMethod Public methods *****************************

procedure TIdSipRenewedSubscriptionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSubscriptionListener).OnRenewedSubscription(Self.Subscription);
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
//* TIdSipSubscriptionRequestMethod                                            *
//******************************************************************************
//* TIdSipSubscriptionRequestMethod Public methods *****************************

procedure TIdSipSubscriptionRequestMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipSubscribeModuleListener).OnSubscriptionRequest(Self.UserAgent,
                                                                   Self.Subscription);
end;

end.
