{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipTransport;

interface

uses
  Classes, Contnrs, IdException, IdInterfacedObject,
  IdNotification, IdSipLocation, IdSipMessage, IdSocketHandle, IdSSLOpenSSL,
  IdTCPConnection, IdTimerQueue, SyncObjs, SysUtils;

type
  TIdSipTransport = class;
  TIdSipTransportClass = class of TIdSipTransport;

  // I provide a protocol for objects that want tolisten for incoming messages.
  IIdSipTransportListener = interface
    ['{D3F0A0D5-A4E9-42BD-B337-D5B3C652F340}']
    procedure OnException(FailedMessage: TIdSipMessage;
                          E: Exception;
                          const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport;
                               Source: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdSipConnectionBindings);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
  end;

  // I listen for when messages are sent, rather than received. You could use
  // me as a logger/debugging tool, for instance.
  IIdSipTransportSendingListener = interface
    ['{2E451F5D-5053-4A2C-BE5F-BB68E5CB3A6D}']
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Destination: TIdSipLocation);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Destination: TIdSipLocation);
  end;

  // I provide functionality common to all transports.
  // Instances of my subclasses may bind to a single IP/port. (Of course,
  // UDP/localhost/5060 != TCP/localhost/5060.). I receive messages from
  // the network through means defined in my subclasses, process them
  // in various ways, and present them to my listeners. Together, all the
  // instances of my subclasses form the Transport layer of the SIP stack.
  TIdSipTransport = class(TIdInterfacedObject,
                          IIdSipMessageListener)
  private
    fAddress:                  String;
    fHostName:                 String;
    fID:                       String;
    fPort:                     Cardinal;
    fTimeout:                  Cardinal;
    fTimer:                    TIdTimerQueue;
    fUseRport:                 Boolean;
    TransportListeners:        TIdNotificationList;
    TransportSendingListeners: TIdNotificationList;

    procedure RewriteOwnVia(Msg: TIdSipMessage;
                            const SentBy: String); overload;
    procedure RewriteOwnVia(Msg: TIdSipMessage;
                            const SentBy: String;
                            Port: Cardinal); overload;
  protected
    procedure DestroyServer; virtual;
    function  GetAddress: String; virtual;
    function  GetBindings: TIdSocketHandles; virtual; abstract;
    function  GetPort: Cardinal; virtual;
    function  IndexOfBinding(const Address: String; Port: Cardinal): Integer;
    procedure InstantiateServer; virtual;
    procedure NotifyOfReceivedRequest(Request: TIdSipRequest;
                                      ReceivedFrom: TIdSipConnectionBindings);
    procedure NotifyOfReceivedResponse(Response: TIdSipResponse;
                                       ReceivedFrom: TIdSipConnectionBindings);
    procedure NotifyOfException(FailedMessage: TIdSipMessage;
                                E: Exception;
                                const Reason: String);
    procedure NotifyOfRejectedMessage(const Msg: String;
                                      const Reason: String);
    procedure NotifyOfSentRequest(Request: TIdSipRequest;
                                  Dest: TIdSipLocation);
    procedure NotifyOfSentResponse(Response: TIdSipResponse;
                                   Dest: TIdSipLocation);
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnMalformedMessage(const Msg: String;
                                 const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdSipConnectionBindings);
    procedure ReturnBadRequest(Request: TIdSipRequest;
                               Target: TIdSipConnectionBindings;
                               const StatusText: String);
    procedure SendRequest(R: TIdSipRequest;
                          Dest: TIdSipLocation); virtual;
    procedure SendResponse(R: TIdSipResponse;
                           Dest: TIdSipLocation); virtual;
    function  SentByIsRecognised(Via: TIdSipViaHeader): Boolean; virtual;
    procedure SetTimeout(Value: Cardinal); virtual;
    procedure SetTimer(Value: TIdTimerQueue); virtual;
  public
    class function  DefaultPort: Cardinal; virtual;
    class function  GetTransportType: String; virtual; abstract;
    class function  IsSecure: Boolean; virtual;
    class function  SrvPrefix: String; virtual;
    class function  SrvQuery(const Domain: String): String;
    class function  UriScheme: String;

    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddBinding(const Address: String; Port: Cardinal); virtual;
    procedure AddTransportListener(const Listener: IIdSipTransportListener);
    procedure AddTransportSendingListener(const Listener: IIdSipTransportSendingListener);
    procedure ClearBindings;
    function  DefaultTimeout: Cardinal; virtual;
    function  HasBinding(const Address: String; Port: Cardinal): Boolean;
    function  IsNull: Boolean; virtual;
    function  IsReliable: Boolean; virtual;
    function  IsRunning: Boolean; virtual;
    procedure ReceiveException(FailedMessage: TIdSipMessage;
                               E: Exception;
                               const Reason: String); virtual;
    procedure ReceiveRequest(Request: TIdSipRequest;
                             ReceivedFrom: TIdSipConnectionBindings); virtual;
    procedure ReceiveResponse(Response: TIdSipResponse;
                              ReceivedFrom: TIdSipConnectionBindings); virtual;
    procedure RemoveBinding(const Address: String; Port: Cardinal);
    procedure RemoveTransportListener(const Listener: IIdSipTransportListener);
    procedure RemoveTransportSendingListener(const Listener: IIdSipTransportSendingListener);
    procedure Send(Msg: TIdSipMessage;
                   Dest: TIdSipLocation);
    procedure Start; virtual;
    procedure Stop; virtual;

    property Bindings: TIdSocketHandles read GetBindings;
    property HostName: String           read fHostName write fHostName;
    property ID:       String           read fID;
    property Timeout:  Cardinal         read fTimeout write SetTimeout;
    property Timer:    TIdTimerQueue    read fTimer write SetTimer;
    property UseRport: Boolean          read fUseRport write fUseRport;
  end;

  // I supply methods for objects to find out what transports the stack knows
  // about, and information about those transports.
  TIdSipTransportRegistry = class(TObject)
  private
    class function TransportAt(Index: Integer): TIdSipTransport; virtual;
    class function TransportTypeAt(Index: Integer): TIdSipTransportClass;
    class function TransportRegistry: TStrings;
    class function TransportTypeRegistry: TStrings;
  public
    class function  DefaultPortFor(const Transport: String): Cardinal;
    class procedure InsecureTransports(Result: TStrings);
    class function  IsSecure(const Transport: String): Boolean;
    class function  RegisterTransport(Instance: TIdSipTransport): String;
    class procedure RegisterTransportType(const Name: String;
                                          const TransportType: TIdSipTransportClass);
    class procedure SecureTransports(Result: TStrings);
    class function  TransportFor(const TransportID: String): TIdSipTransport;
    class function  TransportTypeFor(const Transport: String): TIdSipTransportClass;
    class procedure UnregisterTransport(const TransportID: String);
    class procedure UnregisterTransportType(const Name: String);
    class function  UriSchemeFor(const Transport: String): String;
  end;

  // I give complete and arbitrary access to all transports. Use me at your
  // peril. I exist so that tests may access transports that are not visible to
  // production code.
  TIdSipDebugTransportRegistry = class(TIdSipTransportRegistry)
  public
    class function LastTransport: TIdSipTransport;
    class function SecondLastTransport: TIdSipTransport;
    class function TransportAt(Index: Integer): TIdSipTransport; override;
    class function TransportCount: Integer;
  end;

  // I represent the (possibly) deferred handling of an exception raised in the
  // process of sending or receiving a message.
  //
  // I store a COPY of the message that we were sending/processing when the
  // exception occured. I free this copy.
  TIdSipMessageExceptionWait = class(TIdWait)
  private
    fExceptionMessage: String;
    fExceptionType:    ExceptClass;
    fFailedMessage:    TIdSipMessage;
    fReason:           String;
    fTransportID:      String;

    procedure SetFailedMessage(Value: TIdSipMessage);
  public
    destructor Destroy; override;

    procedure Trigger; override;

    property ExceptionType:    ExceptClass   read fExceptionType write fExceptionType;
    property ExceptionMessage: String        read fExceptionMessage write fExceptionMessage;
    property FailedMessage:    TIdSipMessage read fFailedMessage write SetFailedMessage;
    property Reason:           String        read fReason write fReason;
    property TransportID:      String        read fTransportID write fTransportID;
  end;

  // I represent the (possibly) deferred handling of an inbound message.
  TIdSipReceiveMessageWait = class(TIdSipMessageWait)
  private
    fReceivedFrom: TIdSipConnectionBindings;
    fTransportID:  String;
  public
    destructor Destroy; override;

    procedure Trigger; override;

    property ReceivedFrom: TIdSipConnectionBindings read fReceivedFrom write fReceivedFrom;
    property TransportID:  String                   read fTransportID write fTransportID;
  end;

  // I represent a collection of Transports. I own, and hence manage the
  // lifetimes of, all transports given to me via Add.
  TIdSipTransports = class(TObject)
  private
    List: TObjectList;

    function GetTransports(Index: Integer): TIdSipTransport;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(T: TIdSipTransport);
    procedure Clear;
    function  Count: Integer;

    property Transports[Index: Integer]: TIdSipTransport read GetTransports; default;
  end;

  // Look at IIdSipTransportListener's declaration.
  TIdSipTransportExceptionMethod = class(TIdNotification)
  private
    fException:     Exception;
    fFailedMessage: TIdSipMessage;
    fReason:        String;

    procedure SetFailedMessage(Value: TIdSipMessage);
  public
    destructor Destroy; override;

    procedure Run(const Subject: IInterface); override;

    property Exception:     Exception     read fException write fException;
    property FailedMessage: TIdSipMessage read fFailedMessage write SetFailedMessage;
    property Reason:        String        read fReason write fReason;
  end;

  TIdSipTransportReceiveMethod = class(TIdNotification)
  private
    fReceiver: TIdSipTransport;
    fSource:   TIdSipConnectionBindings;
  public
    property Receiver: TIdSipTransport          read fReceiver write fReceiver;
    property Source:   TIdSipConnectionBindings read fSource write fSource;
  end;

  // Look at IIdSipTransportListener's declaration.
  TIdSipTransportReceiveRequestMethod = class(TIdSipTransportReceiveMethod)
  private
    fRequest: TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property Request: TIdSipRequest read fRequest write fRequest;
  end;

  // Look at IIdSipTransportListener's declaration.
  TIdSipTransportReceiveResponseMethod = class(TIdSipTransportReceiveMethod)
  private
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Response: TIdSipResponse  read fResponse write fResponse;
  end;

  // Look at IIdSipTransportListener's declaration.
  TIdSipTransportRejectedMessageMethod = class(TIdNotification)
  private
    fMsg:    String;
    fReason: String;
  public
    procedure Run(const Subject: IInterface); override;

    property Msg:    String read fMsg write fMsg;
    property Reason: String read fReason write fReason;
  end;

  TIdSipTransportSendingMethod = class(TIdNotification)
  private
    fDestination: TIdSipLocation;
    fSender:      TIdSipTransport;
  public
    property Destination: TIdSipLocation  read fDestination write fDestination;
    property Sender:      TIdSipTransport read fSender write fSender;
  end;

  // Look at IIdSipTransportSendingListener's declaration.
  TIdSipTransportSendingRequestMethod = class(TIdSipTransportSendingMethod)
  private
    fRequest: TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property Request: TIdSipRequest read fRequest write fRequest;
  end;

  // Look at IIdSipTransportSendingListener's declaration.
  TIdSipTransportSendingResponseMethod = class(TIdSipTransportSendingMethod)
  private
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Response: TIdSipResponse read fResponse write fResponse;
  end;

  EIdSipTransport = class(Exception)
  private
    fSipMessage: TIdSipMessage;
    fTransport:  TIdSipTransport;
  public
    constructor Create(Transport: TIdSipTransport;
                       SipMessage: TIdSipMessage;
                       const Msg: String);

    property SipMessage: TIdSipMessage   read fSipMessage;
    property Transport:  TIdSipTransport read fTransport;
  end;

  EUnknownTransport = class(EIdException);

const
  ExceptionDuringTcpClientRequestSend = 'Something went wrong sending a TCP '
                                      + 'request or receiving a response to one.';
  MustHaveAtLeastOneVia   = 'An outbound message must always have at least one '
                          + 'Via, namely, this stack.';
  RequestNotSentFromHere  = 'The request to which this response replies could '
                          + 'not have been sent from here.';
  ViaTransportMismatch    = 'Via transport mismatch';
  WrongTransport          = 'This transport only supports %s  messages but '
                          + 'received a %s message.';

const
  ItemNotFoundIndex = -1;

implementation

uses
  IdRandom, IdSipConsts, IdTCPServer, IdIOHandlerSocket;

var
  GTransports:     TStrings;
  GTransportTypes: TStrings;

//******************************************************************************
//* TIdSipTransport                                                            *
//******************************************************************************
//* TIdSipTransport Public methods *********************************************

class function TIdSipTransport.DefaultPort: Cardinal;
begin
  Result := IdPORT_SIP;
end;

class function TIdSipTransport.IsSecure: Boolean;
begin
  Result := false;
end;

class function TIdSipTransport.SrvPrefix: String;
begin
  Result := Self.ClassName + ' hasn''t overridden SrvPrefix';
end;

class function TIdSipTransport.SrvQuery(const Domain: String): String;
begin
  Result := Self.SrvPrefix + '.' + Domain;
end;

class function TIdSipTransport.UriScheme: String;
begin
  if Self.IsSecure then
    Result := SipsScheme
  else
    Result := SipScheme;
end;

constructor TIdSipTransport.Create;
begin
  inherited Create;

  Self.TransportListeners        := TIdNotificationList.Create;
  Self.TransportSendingListeners := TIdNotificationList.Create;

  Self.fID := TIdSipTransportRegistry.RegisterTransport(Self);

  Self.InstantiateServer;
    
  Self.Timeout  := Self.DefaultTimeout;
  Self.UseRport := false;
end;

destructor TIdSipTransport.Destroy;
begin
  Self.TransportSendingListeners.Free;
  Self.TransportListeners.Free;

  Self.DestroyServer;

  TIdSipTransportRegistry.UnregisterTransport(Self.ID);

  inherited Destroy;
end;

procedure TIdSipTransport.AddBinding(const Address: String; Port: Cardinal);
var
  Handle:     TIdSocketHandle;
  WasRunning: Boolean;
begin
  WasRunning := Self.IsRunning;
  Self.Stop;

  Handle := Self.Bindings.Add;
  Handle.IP   := Address;
  Handle.Port := Port;

  if WasRunning then
    Self.Start;
end;

procedure TIdSipTransport.AddTransportListener(const Listener: IIdSipTransportListener);
begin
  Self.TransportListeners.AddListener(Listener);
end;

procedure TIdSipTransport.AddTransportSendingListener(const Listener: IIdSipTransportSendingListener);
begin
  Self.TransportSendingListeners.AddListener(Listener);
end;

procedure TIdSipTransport.ClearBindings;
var
  WasRunning: Boolean;
begin
  // We DON'T use a try/finally block here because there's no sense in
  // restarting the stack if something went wrong clearing the bindings.

  WasRunning := Self.IsRunning;
  Self.Stop;

  Self.Bindings.Clear;

  if WasRunning then
    Self.Start;
end;

function TIdSipTransport.DefaultTimeout: Cardinal;
begin
  // 5 seconds seems reasonable.

  Result := 5000;
end;

function TIdSipTransport.HasBinding(const Address: String; Port: Cardinal): Boolean;
begin
  Result := Self.IndexOfBinding(Address, Port) <> ItemNotFoundIndex;
end;

function TIdSipTransport.IsNull: Boolean;
begin
  Result := false;
end;

function TIdSipTransport.IsReliable: Boolean;
begin
  Result := true;
end;

function TIdSipTransport.IsRunning: Boolean;
begin
  Result := false;
end;

procedure TIdSipTransport.ReceiveException(FailedMessage: TIdSipMessage;
                                           E: Exception;
                                           const Reason: String);
begin
  Self.NotifyOfException(FailedMessage, E, Reason);
end;

procedure TIdSipTransport.ReceiveRequest(Request: TIdSipRequest;
                                         ReceivedFrom: TIdSipConnectionBindings);
begin
  if Request.IsMalformed then begin
    Self.NotifyOfRejectedMessage(Request.AsString,
                                 Request.ParseFailReason);
    Self.ReturnBadRequest(Request, ReceivedFrom, Request.ParseFailReason);
    Exit;
  end;

  if (Request.LastHop.Transport <> Self.GetTransportType) then begin
    Self.NotifyOfRejectedMessage(Request.AsString,
                                 ViaTransportMismatch);
    Self.ReturnBadRequest(Request, ReceivedFrom, ViaTransportMismatch);
    Exit;
  end;

  // cf. RFC 3261 section 18.2.1
  if TIdSipParser.IsFQDN(Request.LastHop.SentBy)
    or (Request.LastHop.SentBy <> ReceivedFrom.PeerIP) then
    Request.LastHop.Received := ReceivedFrom.PeerIP;

  // We let the UA handle rejecting messages because of things like the UA
  // not supporting the SIP version or whatnot. This allows us to centralise
  // response generation.
    Self.NotifyOfReceivedRequest(Request, ReceivedFrom);
end;

procedure TIdSipTransport.ReceiveResponse(Response: TIdSipResponse;
                                          ReceivedFrom: TIdSipConnectionBindings);
begin
  if Response.IsMalformed then begin
    Self.NotifyOfRejectedMessage(Response.AsString,
                                 Response.ParseFailReason);
    // Drop the malformed response.
    Exit;
  end;

  if (Response.LastHop.Transport <> Self.GetTransportType) then begin
    Self.NotifyOfRejectedMessage(Response.AsString,
                                 ViaTransportMismatch);

    // Drop the malformed response.
    Exit;
  end;

  // cf. RFC 3261 section 18.1.2

  if Self.SentByIsRecognised(Response.LastHop) then begin
    Self.NotifyOfReceivedResponse(Response, ReceivedFrom);
  end
  else
    Self.NotifyOfRejectedMessage(Response.AsString,
                                 RequestNotSentFromHere);
end;


procedure TIdSipTransport.RemoveBinding(const Address: String; Port: Cardinal);
var
  Index:      Integer;
  WasRunning: Boolean;
begin
  WasRunning := Self.IsRunning;
  Self.Stop;

  Index := Self.IndexOfBinding(Address, Port);

  if (Index <> ItemNotFoundIndex) then
    Self.Bindings.Delete(Index);

  if WasRunning then
    Self.Start;  
end;

procedure TIdSipTransport.RemoveTransportListener(const Listener: IIdSipTransportListener);
begin
  Self.TransportListeners.RemoveListener(Listener);
end;

procedure TIdSipTransport.RemoveTransportSendingListener(const Listener: IIdSipTransportSendingListener);
begin
  Self.TransportSendingListeners.RemoveListener(Listener);
end;

procedure TIdSipTransport.Send(Msg: TIdSipMessage;
                               Dest: TIdSipLocation);
begin
  try
    Assert(not Msg.IsMalformed,
           'A Transport must NEVER send invalid messages onto the network ('
         + Msg.ParseFailReason + ')');
    if Msg.IsRequest then
      Self.SendRequest(Msg as TIdSipRequest, Dest)
    else
      Self.SendResponse(Msg as TIdSipResponse, Dest);
  except
    on E: EIdException do
      raise EIdSipTransport.Create(Self, Msg, E.Message);
  end;
end;

procedure TIdSipTransport.Start;
begin
end;

procedure TIdSipTransport.Stop;
begin
end;

//* TIdSipTransport Protected methods ******************************************

procedure TIdSipTransport.DestroyServer;
begin
end;

function TIdSipTransport.GetAddress: String;
begin
  Result := Self.fAddress;
end;

function TIdSipTransport.GetPort: Cardinal;
begin
  Result := Self.fPort;
end;

function TIdSipTransport.IndexOfBinding(const Address: String; Port: Cardinal): Integer;
begin
  Result := 0;

  // Indy uses an Integer to represent an unsigned value. We don't. Thus the
  // unnecessary typecast.
  while (Result < Self.Bindings.Count) do begin
    if (Self.Bindings[Result].IP = Address) and (Self.Bindings[Result].Port = Integer(Port)) then
      Break
    else
      Inc(Result);
  end;

  if (Result = Self.Bindings.Count) then
    Result := ItemNotFoundIndex;
end;

procedure TIdSipTransport.InstantiateServer;
begin
end;

procedure TIdSipTransport.NotifyOfReceivedRequest(Request: TIdSipRequest;
                                                  ReceivedFrom: TIdSipConnectionBindings);
var
  Notification: TIdSipTransportReceiveRequestMethod;
begin
  Assert(not Request.IsMalformed,
         'A Transport must NEVER send invalid requests up the stack ('
       + Request.ParseFailReason + ')');

  Notification := TIdSipTransportReceiveRequestMethod.Create;
  try
    Notification.Receiver := Self;
    Notification.Request  := Request;
    Notification.Source   := ReceivedFrom;

    Self.TransportListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransport.NotifyOfReceivedResponse(Response: TIdSipResponse;
                                                   ReceivedFrom: TIdSipConnectionBindings);
var
  Notification: TIdSipTransportReceiveResponseMethod;
begin
  Assert(not Response.IsMalformed,
         'A Transport must NEVER send invalid responses up the stack ('
       + Response.ParseFailReason + ')');

  Notification := TIdSipTransportReceiveResponseMethod.Create;
  try
    Notification.Receiver := Self;
    Notification.Response := Response;
    Notification.Source   := ReceivedFrom;

    Self.TransportListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransport.NotifyOfException(FailedMessage: TIdSipMessage;
                                            E: Exception;
                                            const Reason: String);
var
  Notification: TIdSipTransportExceptionMethod;
begin
  Notification := TIdSipTransportExceptionMethod.Create;
  try
    Notification.Exception     := E;
    Notification.FailedMessage := FailedMessage;
    Notification.Reason        := Reason;

    Self.TransportListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransport.NotifyOfRejectedMessage(const Msg: String;
                                                  const Reason: String);
var
  Notification: TIdSipTransportRejectedMessageMethod;
begin
  Notification := TIdSipTransportRejectedMessageMethod.Create;
  try
    Notification.Msg    := Msg;
    Notification.Reason := Reason;

    Self.TransportListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransport.NotifyOfSentRequest(Request: TIdSipRequest;
                                              Dest: TIdSipLocation);
var
  Notification: TIdSipTransportSendingRequestMethod;
begin
  Notification := TIdSipTransportSendingRequestMethod.Create;
  try
    Notification.Destination := Dest;
    Notification.Sender      := Self;
    Notification.Request     := Request;

    Self.TransportSendingListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransport.NotifyOfSentResponse(Response: TIdSipResponse;
                                               Dest: TIdSipLocation);
var
  Notification: TIdSipTransportSendingResponseMethod;
begin
  Notification := TIdSipTransportSendingResponseMethod.Create;
  try
    Notification.Destination := Dest;
    Notification.Sender      := Self;
    Notification.Response    := Response;

    Self.TransportSendingListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransport.OnException(E: Exception;
                                      const Reason: String);
begin
  Self.ReceiveException(nil, E, Reason);
end;

procedure TIdSipTransport.OnMalformedMessage(const Msg: String;
                                             const Reason: String);
begin
  Self.NotifyOfRejectedMessage(Msg, Reason);
end;

procedure TIdSipTransport.OnReceiveRequest(Request: TIdSipRequest;
                                           ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.ReceiveRequest(Request, ReceivedFrom);
end;

procedure TIdSipTransport.OnReceiveResponse(Response: TIdSipResponse;
                                            ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.ReceiveResponse(Response, ReceivedFrom);
end;

procedure TIdSipTransport.ReturnBadRequest(Request: TIdSipRequest;
                                           Target: TIdSipConnectionBindings;
                                           const StatusText: String);
var
  Destination: TIdSipLocation;
  Res:         TIdSipResponse;
begin
  Res := TIdSipResponse.InResponseTo(Request, SIPBadRequest);
  try
    Res.StatusText := StatusText;

    Destination := TIdSipLocation.Create(Self.GetTransportType,
                                         Target.PeerIP,
                                         Target.PeerPort);
    try
      Self.SendResponse(Res, Destination);
    finally
      Destination.Free;
    end;
  finally
    Res.Free;
  end;
end;

procedure TIdSipTransport.SendRequest(R: TIdSipRequest;
                                      Dest: TIdSipLocation);
begin
  Self.RewriteOwnVia(R, Self.Bindings[0].IP, Self.Bindings[0].Port);
  Self.NotifyOfSentRequest(R, Dest);
end;

procedure TIdSipTransport.SendResponse(R: TIdSipResponse;
                                       Dest: TIdSipLocation);
begin
  Self.NotifyOfSentResponse(R, Dest);
end;

function TIdSipTransport.SentByIsRecognised(Via: TIdSipViaHeader): Boolean;
var
  I: Integer;
begin
  Result := IsEqual(Via.SentBy, Self.HostName);

  I := 0;

  if not Result then begin
    while (I < Self.Bindings.Count) and not Result do begin
      Result := Result or (Self.Bindings[I].IP = Via.SentBy);

      Inc(I);
    end;
  end;
end;

procedure TIdSipTransport.SetTimeout(Value: Cardinal);
begin
  Self.fTimeout := Value;
end;

procedure TIdSipTransport.SetTimer(Value: TIdTimerQueue);
begin
  Self.fTimer := Value;
end;

//* TIdSipTransport Private methods ********************************************

procedure TIdSipTransport.RewriteOwnVia(Msg: TIdSipMessage;
                                        const SentBy: String);
begin
  // We have two separate RewriteOwnVias because there is a difference between
  // "Via: SIP/2.0/TCP foo:5060" and "Via: SIP/2.0/TCP foo" - the latter uses
  // SRV lookups to determine the actual address/port while the former uses only
  // A/AAAA lookups. See RFC 3263 for more detail. This method sets up a Via
  // without an explicit port:
  // Via: SIP/2.0/<this transport> <SentBy>
  Assert(Msg.Path.Length > 0,
         MustHaveAtLeastOneVia);

  Assert(Msg.LastHop.Transport = Self.GetTransportType,
         Format(WrongTransport, [Self.GetTransportType, Msg.LastHop.Transport]));

  Msg.LastHop.SentBy := SentBy;

  if Self.UseRport then
    Msg.LastHop.Params[RportParam] := '';
end;

procedure TIdSipTransport.RewriteOwnVia(Msg: TIdSipMessage;
                                        const SentBy: String;
                                        Port: Cardinal);
begin
  // This method rewrites a Via header in the form
  // Via: SIP/2.0/<this transport> <SentBy>:<Port>

  Self.RewriteOwnVia(Msg, SentBy);

  Msg.LastHop.Port := Port;
end;

//******************************************************************************
//* TIdSipTransportRegistry                                                    *
//******************************************************************************
//* TIdSipTransportRegistry Public methods *************************************

class function TIdSipTransportRegistry.DefaultPortFor(const Transport: String): Cardinal;
begin
  try
    Result := Self.TransportTypeFor(Transport).DefaultPort;
  except
    on EUnknownTransport do
      Result := TIdSipTransport.DefaultPort;
  end;
end;

class procedure TIdSipTransportRegistry.InsecureTransports(Result: TStrings);
var
  I: Integer;
begin
  for I := 0 to Self.TransportTypeRegistry.Count - 1 do begin
    if not Self.TransportTypeAt(I).IsSecure then
      Result.Add(Self.TransportTypeRegistry[I]);
  end;
end;

class function TIdSipTransportRegistry.IsSecure(const Transport: String): Boolean;
begin
  Result := Self.TransportTypeFor(Transport).IsSecure;
end;

class function TIdSipTransportRegistry.RegisterTransport(Instance: TIdSipTransport): String;
begin
  repeat
    Result := GRandomNumber.NextHexString;
  until (Self.TransportRegistry.IndexOf(Result) = ItemNotFoundIndex);

  Self.TransportRegistry.AddObject(Result, Instance);
end;

class procedure TIdSipTransportRegistry.RegisterTransportType(const Name: String;
                                                          const TransportType: TIdSipTransportClass);
begin
  if (Self.TransportTypeRegistry.IndexOf(Name) = ItemNotFoundIndex) then
    Self.TransportTypeRegistry.AddObject(Name, TObject(TransportType));
end;

class procedure TIdSipTransportRegistry.SecureTransports(Result: TStrings);
var
  I: Integer;
begin
  for I := 0 to Self.TransportTypeRegistry.Count - 1 do begin
    if Self.TransportTypeAt(I).IsSecure then
      Result.Add(Self.TransportTypeRegistry[I]);
  end;
end;

class function TIdSipTransportRegistry.TransportFor(const TransportID: String): TIdSipTransport;
var
  Index: Integer;
begin
  Index := Self.TransportRegistry.IndexOf(TransportID);

  // Unlike TransportTypeFor, we don't blow up if you request a transport we
  // don't know about.
  if (Index <> ItemNotFoundIndex) then
    Result := Self.TransportAt(Index)
  else
    Result := nil;
end;

class function TIdSipTransportRegistry.TransportTypeFor(const Transport: String): TIdSipTransportClass;
var
  Index: Integer;
begin
  Index := Self.TransportTypeRegistry.IndexOf(Transport);

  if (Index <> ItemNotFoundIndex) then
    Result := Self.TransportTypeAt(Index)
  else
    raise EUnknownTransport.Create('TIdSipTransportRegistry.TransportTypeFor: ' + Transport);
end;

class procedure TIdSipTransportRegistry.UnregisterTransport(const TransportID: String);
var
  Index: Integer;
begin
  Index := Self.TransportRegistry.IndexOf(TransportID);
  if (Index <> ItemNotFoundIndex) then
    Self.TransportRegistry.Delete(Index);
end;

class procedure TIdSipTransportRegistry.UnregisterTransportType(const Name: String);
var
  Index: Integer;
begin
  Index := Self.TransportTypeRegistry.IndexOf(Name);
  if (Index <> ItemNotFoundIndex) then
    Self.TransportTypeRegistry.Delete(Index);
end;

class function TIdSipTransportRegistry.UriSchemeFor(const Transport: String): String;
begin
  try
    Result := Self.TransportTypeFor(Transport).UriScheme;
  except
    on EUnknownTransport do
      Result := TIdSipTransport.UriScheme;
  end;
end;

//* TIdSipTransportRegistry Private methods ************************************

class function TIdSipTransportRegistry.TransportAt(Index: Integer): TIdSipTransport;
begin
  Result := TIdSipTransport(Self.TransportRegistry.Objects[Index]);
end;

class function TIdSipTransportRegistry.TransportTypeAt(Index: Integer): TIdSipTransportClass;
begin
  Result := TIdSipTransportClass(Self.TransportTypeRegistry.Objects[Index]);
end;

class function TIdSipTransportRegistry.TransportRegistry: TStrings;
begin
  Result := GTransports;
end;

class function TIdSipTransportRegistry.TransportTypeRegistry: TStrings;
begin
  Result := GTransportTypes;
end;

//******************************************************************************
//* TIdSipDebugTransportRegistry                                               *
//******************************************************************************
//* TIdSipDebugTransportRegistry Public methods ********************************

class function TIdSipDebugTransportRegistry.LastTransport: TIdSipTransport;
begin
  Result := Self.TransportAt(Self.TransportCount - 1);
end;

class function TIdSipDebugTransportRegistry.SecondLastTransport: TIdSipTransport;
begin
  Result := Self.TransportAt(Self.TransportCount - 1);
end;

class function TIdSipDebugTransportRegistry.TransportAt(Index: Integer): TIdSipTransport;
begin
  Result := inherited TransportAt(Index);
end;

class function TIdSipDebugTransportRegistry.TransportCount: Integer;
begin
  Result := Self.TransportRegistry.Count;
end;

//******************************************************************************
//* TIdSipMessageExceptionWait                                                 *
//******************************************************************************
//* TIdSipMessageExceptionWait Public methods **********************************

destructor TIdSipMessageExceptionWait.Destroy;
begin
  Self.fFailedMessage.Free;

  inherited Destroy;
end;

procedure TIdSipMessageExceptionWait.Trigger;
var
  FakeException: Exception;
  Receiver:      TIdSipTransport;
begin
  FakeException := Self.ExceptionType.Create(Self.ExceptionMessage);
  try
    Receiver := TIdSipTransportRegistry.TransportFor(Self.TransportID);

    if Assigned(Receiver) then
      Receiver.ReceiveException(Self.FailedMessage,
                                FakeException,
                                Self.Reason);
  finally
    FakeException.Free;
  end;
end;

//* TIdSipMessageExceptionWait Private methods *********************************

procedure TIdSipMessageExceptionWait.SetFailedMessage(Value: TIdSipMessage);
begin
  if Assigned(Self.fFailedMessage) then
    Self.fFailedMessage.Free;

  Self.fFailedMessage := Value.Copy;
end;

//******************************************************************************
//* TIdSipReceiveMessageWait                                                   *
//******************************************************************************
//* TIdSipReceiveMessageWait Public methods ************************************

destructor TIdSipReceiveMessageWait.Destroy;
begin
  Self.fReceivedFrom.Free;

  inherited Destroy;
end;

procedure TIdSipReceiveMessageWait.Trigger;
var
  Receiver: TIdSipTransport;
begin
  Receiver := TIdSipTransportRegistry.TransportFor(Self.TransportID);

  if Assigned(Receiver) then begin
    if Self.Message.IsRequest then
      Receiver.ReceiveRequest(Self.Message as TIdSipRequest,
                              Self.ReceivedFrom)
    else
      Receiver.ReceiveResponse(Self.Message as TIdSipResponse,
                               Self.ReceivedFrom);
  end;
end;

//******************************************************************************
//* TIdSipTransports                                                           *
//******************************************************************************
//* TIdSipTransports Public methods ********************************************

constructor TIdSipTransports.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdSipTransports.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

procedure TIdSipTransports.Add(T: TIdSipTransport);
begin
  Self.List.Add(T);
end;

procedure TIdSipTransports.Clear;
begin
  Self.List.Clear;
end;

function TIdSipTransports.Count: Integer;
begin
  Result := Self.List.Count;
end;

//* TIdSipTransports Private methods *******************************************

function TIdSipTransports.GetTransports(Index: Integer): TIdSipTransport;
begin
  Result := Self.List[Index] as TIdSipTransport;
end;

//******************************************************************************
//* TIdSipTransportExceptionMethod                                             *
//******************************************************************************
//* TIdSipTransportExceptionMethod Public methods ******************************

destructor TIdSipTransportExceptionMethod.Destroy;
begin
  Self.fFailedMessage.Free;

  inherited Destroy;
end;

procedure TIdSipTransportExceptionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransportListener).OnException(Self.FailedMessage,
                                                   Self.Exception,
                                                   Self.Reason);
end;

//* TIdSipTransportExceptionMethod Private methods *****************************

procedure TIdSipTransportExceptionMethod.SetFailedMessage(Value: TIdSipMessage);
begin
  if Assigned(Self.fFailedMessage) then
    Self.fFailedMessage.Free;

  Self.fFailedMessage := Value.Copy;
end;

//******************************************************************************
//* TIdSipTransportReceiveRequestMethod                                        *
//******************************************************************************
//* TIdSipTransportReceiveRequestMethod Public methods *************************

procedure TIdSipTransportReceiveRequestMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransportListener).OnReceiveRequest(Self.Request,
                                                        Self.Receiver,
                                                        Self.Source);
end;

//******************************************************************************
//* TIdSipTransportReceiveResponseMethod                                       *
//******************************************************************************
//* TIdSipTransportReceiveResponseMethod Public methods ************************

procedure TIdSipTransportReceiveResponseMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransportListener).OnReceiveResponse(Self.Response,
                                                         Self.Receiver,
                                                         Self.Source);
end;

//******************************************************************************
//* TIdSipTransportRejectedMessageMethod                                       *
//******************************************************************************
//* TIdSipTransportRejectedMessageMethod Public methods ************************

procedure TIdSipTransportRejectedMessageMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransportListener).OnRejectedMessage(Self.Msg,
                                                         Self.Reason);
end;

//******************************************************************************
//* TIdSipTransportSendingRequestMethod                                        *
//******************************************************************************
//* TIdSipTransportSendingRequestMethod Public methods *************************

procedure TIdSipTransportSendingRequestMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransportSendingListener).OnSendRequest(Self.Request,
                                                            Self.Sender,
                                                            Self.Destination);
end;

//******************************************************************************
//* TIdSipTransportSendingResponseMethod                                       *
//******************************************************************************
//* TIdSipTransportSendingResponseMethod Public methods ************************

procedure TIdSipTransportSendingResponseMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransportSendingListener).OnSendResponse(Self.Response,
                                                             Self.Sender,
                                                             Self.Destination);
end;

//******************************************************************************
//* EIdSipTransport                                                            *
//******************************************************************************
//* EIdSipTransport Public methods *********************************************

constructor EIdSipTransport.Create(Transport: TIdSipTransport;
                                   SipMessage: TIdSipMessage;
                                   const Msg: String);
begin
  inherited Create(Msg);

  Self.fSipMessage := SipMessage;
  Self.fTransport  := Transport;
end;

initialization
  GTransports     := TStringList.Create;
  GTransportTypes := TStringList.Create;
finalization
// These objects are purely memory-based, so it's safe not to free them here.
// Still, perhaps we need to review this methodology. How else do we get
// something like class variables?
//  GTransports.Free;
//  GTransportTypes.Free;
end.
