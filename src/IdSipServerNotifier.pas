{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipServerNotifier;

interface

uses
  IdSipMessage, IdNotification, SysUtils;

type
  // I provide a wrapper around a NotificationList and provide the
  // notifications that a SIP server needs to do its job.
  TIdSipServerNotifier = class(TObject)
  private
    Listeners: TIdNotificationList;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddMessageListener(const Listener: IIdSipMessageListener);
    procedure NotifyListenersOfException(E: Exception;
                                         const Reason: String);
    procedure NotifyListenersOfMalformedMessage(const Msg: String;
                                                const Reason: String);
    procedure NotifyListenersOfRequest(Request: TIdSipRequest;
                                       ReceivedFrom: TIdSipConnectionBindings); overload;
    procedure NotifyListenersOfResponse(Response: TIdSipResponse;
                                        ReceivedFrom: TIdSipConnectionBindings); overload;
    procedure RemoveMessageListener(const Listener: IIdSipMessageListener);
  end;

  TIdSipServerExceptionMethod = class(TIdMethod)
  private
    fException: Exception;
    fReason:    String;
  public
    procedure Run(const Subject: IInterface); override;

    property Exception: Exception read fException write fException;
    property Reason:    String    read fReason write fReason;
  end;

  TIdSipServerMalformedMessageMethod = class(TIdMethod)
  private
    fMsg:    String;
    fReason: String;
  public
    procedure Run(const Subject: IInterface); override;

    property Msg:    String read fMsg write fMsg;
    property Reason: String read fReason write fReason;
  end;

  TIdSipServerReceiveRequestMethod = class(TIdMethod)
  private
    fReceivedFrom: TIdSipConnectionBindings;
    fRequest:      TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property ReceivedFrom: TIdSipConnectionBindings read fReceivedFrom write fReceivedFrom;
    property Request:      TIdSipRequest            read fRequest write fRequest;
  end;

  TIdSipServerReceiveResponseMethod = class(TIdMethod)
  private
    fReceivedFrom: TIdSipConnectionBindings;
    fResponse:     TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property ReceivedFrom: TIdSipConnectionBindings read fReceivedFrom write fReceivedFrom;
    property Response:     TIdSipResponse           read fResponse write fResponse;
  end;

implementation

//******************************************************************************
//* TIdSipServerNotifier                                                       *
//******************************************************************************
//* TIdSipServerNotifier Public methods ****************************************

constructor TIdSipServerNotifier.Create;
begin
  inherited Create;

  Self.Listeners := TIdNotificationList.Create;
end;

destructor TIdSipServerNotifier.Destroy;
begin
  Self.Listeners.Free;

  inherited Destroy;
end;

procedure TIdSipServerNotifier.AddMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.Listeners.AddListener(Listener);
end;

procedure TIdSipServerNotifier.NotifyListenersOfException(E: Exception;
                                                          const Reason: String);
var
  Notification: TIdSipServerExceptionMethod;
begin
  Notification := TIdSipServerExceptionMethod.Create;
  try
    Notification.Exception := E;
    Notification.Reason    := Reason;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipServerNotifier.NotifyListenersOfMalformedMessage(const Msg: String;
                                                                 const Reason: String);
var
  Notification: TIdSipServerMalformedMessageMethod;
begin
  Notification := TIdSipServerMalformedMessageMethod.Create;
  try
    Notification.Msg    := Msg;
    Notification.Reason := Reason;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipServerNotifier.NotifyListenersOfRequest(Request: TIdSipRequest;
                                                        ReceivedFrom: TIdSipConnectionBindings);
var
  Notification: TIdSipServerReceiveRequestMethod;
begin
  Notification := TIdSipServerReceiveRequestMethod.Create;
  try
    Notification.ReceivedFrom := ReceivedFrom;
    Notification.Request      := Request;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipServerNotifier.NotifyListenersOfResponse(Response: TIdSipResponse;
                                                         ReceivedFrom: TIdSipConnectionBindings);
var
  Notification: TIdSipServerReceiveResponseMethod;
begin
  Notification := TIdSipServerReceiveResponseMethod.Create;
  try
    Notification.ReceivedFrom := ReceivedFrom;
    Notification.Response     := Response;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipServerNotifier.RemoveMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

//******************************************************************************
//* TIdSipServerExceptionMethod                                                *
//******************************************************************************
//* TIdSipServerExceptionMethod Public methods *********************************

procedure TIdSipServerExceptionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipMessageListener).OnException(Self.Exception,
                                                 Self.Reason);
end;

//******************************************************************************
//* TIdSipServerMalformedMessageMethod                                         *
//******************************************************************************
//* TIdSipServerMalformedMessageMethod Public methods **************************

procedure TIdSipServerMalformedMessageMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipMessageListener).OnMalformedMessage(Self.Msg,
                                                        Self.Reason);
end;

//******************************************************************************
//* TIdSipServerReceiveRequestMethod                                           *
//******************************************************************************
//* TIdSipServerReceiveRequestMethod Public methods ****************************

procedure TIdSipServerReceiveRequestMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipMessageListener).OnReceiveRequest(Self.Request,
                                                      Self.ReceivedFrom);
end;

//******************************************************************************
//* TIdSipServerReceiveResponseMethod                                          *
//******************************************************************************
//* TIdSipServerReceiveResponseMethod Public methods ***************************

procedure TIdSipServerReceiveResponseMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipMessageListener).OnReceiveResponse(Self.Response,
                                                       Self.ReceivedFrom);
end;

end.
