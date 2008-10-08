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
  Classes, Contnrs, IdConnectionBindings, IdException, IdInterfacedObject,
  IdNotification, IdRoutingTable, IdSipLocation, IdSipMessage, IdSocketHandle,
  IdSSLOpenSSL, IdTCPConnection, IdTimerQueue, PluggableLogging, SyncObjs,
  SysUtils;

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
                               Source: TIdConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdConnectionBindings);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String;
                                Source: TIdConnectionBindings);
  end;

  // I listen for when messages are sent, rather than received. You could use
  // me as a logger/debugging tool, for instance.
  IIdSipTransportSendingListener = interface
    ['{2E451F5D-5053-4A2C-BE5F-BB68E5CB3A6D}']
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Destination: TIdConnectionBindings);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Destination: TIdConnectionBindings);
  end;

  // I provide the protocol for objects that want to know when connection
  // oriented transports (TCP, TLS, SCTP, etc.) open or close connections.
  IIdSipConnectionListener = interface
    ['{D6E40048-731A-414E-B547-8D683FDD02F0}']
    procedure OnConnection(Transport: TIdSipTransport;
                           Connection: TIdConnectionBindings);
    procedure OnDisconnection(Transport: TIdSipTransport;
                              Connection: TIdConnectionBindings);
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
    ConnectionListeners:      TIdNotificationList;
    fAddress:                  String;
    fConserveConnections:      Boolean; // This is actually only used by connection-oriented transports.
    fHostName:                 String;
    fPort:                     Cardinal;
    fRoutingTable:             TIdRoutingTable;
    fTimeout:                  Cardinal;
    fTimer:                    TIdTimerQueue;
    fUseRport:                 Boolean;
    TransportListeners:        TIdNotificationList;
    TransportSendingListeners: TIdNotificationList;

  protected
    procedure Assert(Condition: Boolean; ProblemDescription: String);
    procedure AssertReceiveWellFormed(Msg: TIdSipMessage);
    procedure AssertSendWellFormed(Msg: TIdSipMessage);
    procedure AssertWellFormed(Msg: TIdSipMessage; LogMsgTemplate: String);
    procedure DestroyServer; virtual;
    function  GetAddress: String; virtual;
    function  GetBindings: TIdSocketHandles; virtual;
    function  GetConserveConnections: Boolean; virtual;
    function  GetPort: Cardinal; virtual;
    function  IndexOfBinding(const Address: String; Port: Cardinal): Integer;
    procedure InstantiateServer; virtual;
    procedure Log(Description: String;
                  Severity: TSeverityLevel;
                  EventRef: Cardinal;
                  DebugInfo: String);
    procedure LogException(FailedMessage: TIdSipMessage;
                           E: Exception;
                           Reason: String);
    procedure LogReceivedMessage(Msg: TIdSipMessage;
                                 ReceivedFrom: TIdConnectionBindings);
    procedure LogRejectedMessage(Msg: String;
                                 Reason: String;
                                 ReceivedFrom: TIdConnectionBindings);
    procedure LogSentMessage(Msg: TIdSipMessage; SentTo: TIdConnectionBindings);
    procedure NotifyOfConnection(Connection: TIdConnectionBindings);
    procedure NotifyOfDisconnection(Connection: TIdConnectionBindings);
    procedure NotifyOfReceivedRequest(Request: TIdSipRequest;
                                      ReceivedFrom: TIdConnectionBindings);
    procedure NotifyOfReceivedResponse(Response: TIdSipResponse;
                                       ReceivedFrom: TIdConnectionBindings);
    procedure NotifyOfException(FailedMessage: TIdSipMessage;
                                E: Exception;
                                const Reason: String);
    procedure NotifyOfRejectedMessage(const Msg: String;
                                      const Reason: String;
                                      ReceivedFrom: TIdConnectionBindings);
    procedure NotifyOfSentRequest(Request: TIdSipRequest;
                                  Binding: TIdConnectionBindings);
    procedure NotifyOfSentResponse(Response: TIdSipResponse;
                                   Binding: TIdConnectionBindings);
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnMalformedMessage(const Msg: String;
                                 const Reason: String;
                                 ReceivedFrom: TIdConnectionBindings);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdConnectionBindings);
    procedure ReturnBadRequest(Request: TIdSipRequest;
                               Target: TIdConnectionBindings;
                               const StatusText: String);
    procedure SendMessage(M: TIdSipMessage;
                          Dest: TIdConnectionBindings); virtual;
    procedure SendRequest(R: TIdSipRequest;
                          Dest: TIdSipLocation);
    procedure SendResponse(R: TIdSipResponse;
                           Dest: TIdSipLocation); overload;
    procedure SendResponse(R: TIdSipResponse;
                           Binding: TIdConnectionBindings); overload;
    function  SentByIsRecognised(Via: TIdSipViaHeader): Boolean; virtual;
    procedure SetConserveConnections(Value: Boolean); virtual;
    procedure SetTimeout(Value: Cardinal); virtual;
    procedure SetTimer(Value: TIdTimerQueue); virtual;

    property Bindings: TIdSocketHandles read GetBindings;
  public
    class function  DefaultPort: Cardinal; virtual;
    class function  GetTransportType: String; virtual;
    class function  IsSecure: Boolean; virtual;
    class function  SrvPrefix: String; virtual;
    class function  SrvQuery(const Domain: String): String;
    class function  UriScheme: String;

    constructor Create; override;
    destructor  Destroy; override;

    procedure AddBinding(const Address: String; Port: Cardinal); virtual;
    procedure AddConnectionListener(Listener: IIdSipConnectionListener);
    procedure AddTransportListener(const Listener: IIdSipTransportListener; Priority: Integer = 0);
    procedure AddTransportSendingListener(const Listener: IIdSipTransportSendingListener; Priority: Integer = 0);
    function  BindingCount: Integer;
    procedure ClearBindings;
    function  DefaultTimeout: Cardinal; virtual;
    function  FindBinding(Dest: TIdConnectionBindings): TIdSocketHandle;
    function  FirstIPBound: String;
    function  HasBinding(const Address: String; Port: Cardinal): Boolean;
    function  IsMock: Boolean; virtual;
    function  IsNull: Boolean; virtual;
    function  IsReliable: Boolean; virtual;
    function  IsRunning: Boolean; virtual;
    procedure LocalBindings(Bindings: TIdSipLocations);
    procedure ReceiveException(FailedMessage: TIdSipMessage;
                               E: Exception;
                               const Reason: String); virtual;
    procedure ReceiveRequest(Request: TIdSipRequest;
                             ReceivedFrom: TIdConnectionBindings); virtual;
    procedure ReceiveResponse(Response: TIdSipResponse;
                              ReceivedFrom: TIdConnectionBindings); virtual;
    procedure RemoveBinding(const Address: String; Port: Cardinal);
    procedure RemoveConnectionListener(Listener: IIdSipConnectionListener);
    procedure RemoveTransportListener(const Listener: IIdSipTransportListener);
    procedure RemoveTransportSendingListener(const Listener: IIdSipTransportSendingListener);
    procedure Send(Msg: TIdSipMessage;
                   Dest: TIdSipLocation);
    procedure SetFirstBinding(IPAddress: String; Port: Cardinal);
    procedure Start; virtual;
    procedure Stop; virtual;

    property ConserveConnections: Boolean         read GetConserveConnections write SetConserveConnections;
    property HostName:            String          read fHostName write fHostName;
    property RoutingTable:        TIdRoutingTable read fRoutingTable write fRoutingTable;
    property Timeout:             Cardinal        read fTimeout write SetTimeout;
    property Timer:               TIdTimerQueue   read fTimer write SetTimer;
    property UseRport:            Boolean         read fUseRport write fUseRport;
  end;

  // I supply methods for objects to find out what transports the stack knows
  // about, and information about those transports.
  TIdSipTransportRegistry = class(TObject)
  private
    class function TransportTypeAt(Index: Integer): TIdSipTransportClass;
    class function TransportTypeRegistry: TStrings;
  public
    class function  DefaultPortFor(const Transport: String): Cardinal;
    class procedure InsecureTransports(Result: TStrings);
    class function  IsSecure(const Transport: String): Boolean;
    class function  NonstandardPort(const Transport: String; Port: Cardinal): Boolean;
    class procedure RegisterTransportType(const Name: String;
                                          const TransportType: TIdSipTransportClass);
    class procedure SecureTransports(Result: TStrings);
    class function  TransportTypeFor(const Transport: String): TIdSipTransportClass;
    class procedure UnregisterTransportType(const Name: String);
    class function  UriSchemeFor(const Transport: String): String;
  end;

  // I give complete and arbitrary access to all transports. Use me at your
  // peril. I exist so that tests may access transports that are not visible to
  // production code.
  //
  // Please note that I make no attempt to prevent multiple TIdSipMockTransports
  // from running on the same Host:Port, even though that's not possible for
  // real transports.
  TIdSipDebugTransportRegistry = class(TIdSipTransportRegistry)
  private
    class function GetAllTransports: TStrings;
  public
    class function TransportRunningOn(Host: String; Port: Cardinal): TIdSipTransport; overload;
    class function TransportRunningOn(Transport, Host: String; Port: Cardinal): TIdSipTransport; overload;
    class function TransportCount: Integer;
  end;

  TIdSipTransportWait = class(TIdWait)
  private
    fTransportID: String;
  protected
    procedure TriggerOn(Transport: TIdSipTransport); virtual;
  public
    procedure Trigger; override;

    property TransportID: String read fTransportID write fTransportID;
  end;

  // I represent the (possibly) deferred handling of an exception raised in the
  // process of sending or receiving a message.
  //
  // I store a COPY of the message that we were sending/processing when the
  // exception occured. I free this copy.
  TIdSipMessageExceptionWait = class(TIdSipTransportWait)
  private
    fExceptionMessage: String;
    fExceptionType:    ExceptClass;
    fFailedMessage:    TIdSipMessage;
    fReason:           String;

    procedure SetFailedMessage(Value: TIdSipMessage);
  protected
    procedure TriggerOn(Transport: TIdSipTransport); override;
  public
    destructor Destroy; override;

    property ExceptionType:    ExceptClass   read fExceptionType write fExceptionType;
    property ExceptionMessage: String        read fExceptionMessage write fExceptionMessage;
    property FailedMessage:    TIdSipMessage read fFailedMessage write SetFailedMessage;
    property Reason:           String        read fReason write fReason;
  end;

  // I represent the (possibly) deferred handling of an inbound message.
  TIdSipReceiveMessageWait = class(TIdSipMessageWait)
  private
    fReceivedFrom: TIdConnectionBindings;
    fTransportID:  String;

    procedure SetReceivedFrom(Value: TIdConnectionBindings);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Trigger; override;

    property ReceivedFrom: TIdConnectionBindings read fReceivedFrom write SetReceivedFrom;
    property TransportID:  String                read fTransportID write fTransportID;
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
    fSource:   TIdConnectionBindings;
  public
    property Receiver: TIdSipTransport       read fReceiver write fReceiver;
    property Source:   TIdConnectionBindings read fSource write fSource;
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

    property Response: TIdSipResponse read fResponse write fResponse;
  end;

  // Look at IIdSipTransportListener's declaration.
  TIdSipTransportRejectedMessageMethod = class(TIdNotification)
  private
    fMsg:    String;
    fReason: String;
    fSource: TIdConnectionBindings;
  public
    procedure Run(const Subject: IInterface); override;

    property Msg:    String                read fMsg write fMsg;
    property Reason: String                read fReason write fReason;
    property Source: TIdConnectionBindings read fSource write fSource;
  end;

  TIdSipTransportSendingMethod = class(TIdNotification)
  private
    fBinding: TIdConnectionBindings;
    fSender:  TIdSipTransport;
  public
    property Binding: TIdConnectionBindings read fBinding write fBinding;
    property Sender:  TIdSipTransport       read fSender write fSender;
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

  TIdSipConnectionOrientedTransportMethod = class(TIdNotification)
  private
    fConnection: TIdConnectionBindings;
    fTransport:  TIdSipTransport;
  public
    property Connection: TIdConnectionBindings read fConnection write fConnection;
    property Transport:  TIdSipTransport       read fTransport write fTransport;
  end;

  TIdSipTransportConnectionMethod = class(TIdSipConnectionOrientedTransportMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSipTransportDisconnectionMethod = class(TIdSipConnectionOrientedTransportMethod)
  public
    procedure Run(const Subject: IInterface); override;
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
  NoBindings              = 'You can''t send messages through a transport with '
                          + 'no bindings';
  RequestNotSentFromHere  = 'The request to which this response replies could '
                          + 'not have been sent from here.';
  TransportMismatch       = 'You can''t use a %s transport to send a %s packet.';
  ViaTransportMismatch    = 'Via transport mismatch';
  WrongTransport          = 'This transport only supports %s  messages but '
                          + 'received a %s message.';

const
  ItemNotFoundIndex = -1;

implementation

uses
  IdRegisteredObject, IdTCPServer, IdIOHandlerSocket, RuntimeSafety;

var
  GTransportTypes: TStrings;

//******************************************************************************
//* TIdSipTransport                                                            *
//******************************************************************************
//* TIdSipTransport Public methods *********************************************

class function TIdSipTransport.DefaultPort: Cardinal;
begin
  Result := DefaultSipPort;
end;

class function TIdSipTransport.GetTransportType: String;
begin
  Result := '';
  RaiseAbstractError(Self.ClassName, 'GetTransportType');
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

  Self.ConnectionListeners       := TIdNotificationList.Create;
  Self.TransportListeners        := TIdNotificationList.Create;
  Self.TransportSendingListeners := TIdNotificationList.Create;

  Self.InstantiateServer;

  Self.Timeout  := Self.DefaultTimeout;
  Self.UseRport := false;
end;

destructor TIdSipTransport.Destroy;
begin
  Self.TransportSendingListeners.Free;
  Self.TransportListeners.Free;
  Self.ConnectionListeners.Free;

  Self.DestroyServer;

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

procedure TIdSipTransport.AddConnectionListener(Listener: IIdSipConnectionListener);
begin
  Self.ConnectionListeners.AddListener(Listener);
end;

procedure TIdSipTransport.AddTransportListener(const Listener: IIdSipTransportListener; Priority: Integer = 0);
begin
  Self.TransportListeners.AddListener(Listener, Priority);
end;

procedure TIdSipTransport.AddTransportSendingListener(const Listener: IIdSipTransportSendingListener; Priority: Integer = 0);
begin
  Self.TransportSendingListeners.AddListener(Listener, Priority);
end;

function TIdSipTransport.BindingCount: Integer;
begin
  Result := Self.Bindings.Count;
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

function TIdSipTransport.FindBinding(Dest: TIdConnectionBindings): TIdSocketHandle;
var
  DefaultPort:  Cardinal;
  I:            Integer;
  LocalAddress: String;
begin
  // In a multihomed environment, Indy does the wrong thing when you invoke
  // Send. It uses the first socket in its list of bindings, not the socket most
  // appropriate to use to send to a target.
  //
  // Now we might have several bindings on the same IP address (say, 192.168.1.1
  // on ports 5060, 15060 and 25060. All these bindings are equally appropriate
  // because port numbers don't exist in the network (i.e., IP) layer, so we
  // simply return the first one.

  Self.Assert(Dest.Transport = Self.GetTransportType,
              Format(TransportMismatch, [Self.GetTransportType, Dest.Transport]));
  Self.Assert(Self.Bindings.Count > 0, NoBindings);

  // A subtlety: this method will select the binding on the default port if
  // it can.
  DefaultPort  := TIdSipTransportRegistry.DefaultPortFor(Dest.Transport);
  LocalAddress := Self.RoutingTable.GetBestLocalAddress(Dest.PeerIP);

  // Try find the binding using the default port on the LocalAddress for this
  // transport.
  Result := nil;

  for I := 0 to Self.Bindings.Count - 1 do begin
    // Try use any address bound on LocalAddress.IPAddress
    if (Self.Bindings[I].IP = LocalAddress) then begin
      Result := Self.Bindings[I];
    end;

    // But if something's bound on LocalAddress.IPAddress AND uses the default
    // port for this transport, use that instead.
    if Assigned(Result) then begin
      if    (Self.Bindings[I].IP = LocalAddress)
        and (Cardinal(Self.Bindings[I].Port) = DefaultPort) then begin
        Result := Self.Bindings[I];
        Break;
      end;
    end;
  end;

  // Nothing appropriate found? Just use any old socket, and pray.
  if (Result = nil) then
    Result := Self.Bindings[0];

  Self.Assert(Result <> nil, 'No binding found for the destination ' + Dest.AsString);
end;


function TIdSipTransport.FirstIPBound: String;
begin
  if (Self.BindingCount = 0) then
    Result := ''
  else
    Result := Self.Bindings[0].IP;
end;

function TIdSipTransport.HasBinding(const Address: String; Port: Cardinal): Boolean;
begin
  Result := Self.IndexOfBinding(Address, Port) <> ItemNotFoundIndex;
end;

function TIdSipTransport.IsMock: Boolean;
begin
  // Return true if and only if I am a mock transport.
  Result := false;
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

procedure TIdSipTransport.LocalBindings(Bindings: TIdSipLocations);
var
  I: Integer;
begin
  for I := 0 to Self.Bindings.Count - 1 do
    Bindings.AddLocation(Self.GetTransportType, Self.Bindings[I].IP, Self.Bindings[I].Port);
end;

procedure TIdSipTransport.ReceiveException(FailedMessage: TIdSipMessage;
                                           E: Exception;
                                           const Reason: String);
begin
  Self.NotifyOfException(FailedMessage, E, Reason);
end;

procedure TIdSipTransport.ReceiveRequest(Request: TIdSipRequest;
                                         ReceivedFrom: TIdConnectionBindings);
begin
  if Request.IsMalformed then begin
    Self.NotifyOfRejectedMessage(Request.AsString,
                                 Request.ParseFailReason,
                                 ReceivedFrom);
    Self.ReturnBadRequest(Request, ReceivedFrom, Request.ParseFailReason);
    Exit;
  end;

  if (Request.LastHop.Transport <> Self.GetTransportType) then begin
    Self.NotifyOfRejectedMessage(Request.AsString,
                                 ViaTransportMismatch,
                                 ReceivedFrom);
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
                                          ReceivedFrom: TIdConnectionBindings);
begin
  if Response.IsMalformed then begin
    Self.NotifyOfRejectedMessage(Response.AsString,
                                 Response.ParseFailReason,
                                 ReceivedFrom);
    // Drop the malformed response.
    Exit;
  end;

  if (Response.LastHop.Transport <> Self.GetTransportType) then begin
    Self.NotifyOfRejectedMessage(Response.AsString,
                                 ViaTransportMismatch,
                                 ReceivedFrom);

    // Drop the malformed response.
    Exit;
  end;

  // cf. RFC 3261 section 18.1.2

  if Self.SentByIsRecognised(Response.LastHop) then begin
    Self.NotifyOfReceivedResponse(Response, ReceivedFrom);
  end
  else
    Self.NotifyOfRejectedMessage(Response.AsString,
                                 RequestNotSentFromHere,
                                 ReceivedFrom);
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

procedure TIdSipTransport.RemoveConnectionListener(Listener: IIdSipConnectionListener);
begin
  Self.ConnectionListeners.RemoveListener(Listener);
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
    Self.AssertSendWellFormed(Msg);
    
    if Msg.IsRequest then
      Self.SendRequest(Msg as TIdSipRequest, Dest)
    else
      Self.SendResponse(Msg as TIdSipResponse, Dest);
  except
    on E: EIdException do
      raise EIdSipTransport.Create(Self, Msg, E.Message);
  end;
end;

procedure TIdSipTransport.SetFirstBinding(IPAddress: String; Port: Cardinal);
begin
  Self.Bindings[0].IP   := IPAddress;
  Self.Bindings[0].Port := Port;
end;

procedure TIdSipTransport.Start;
begin
end;

procedure TIdSipTransport.Stop;
begin
end;

//* TIdSipTransport Protected methods ******************************************

procedure TIdSipTransport.Assert(Condition: Boolean; ProblemDescription: String);
begin
  if not Condition then
    Self.Log('Assertion violation', slError, LogEventException, ProblemDescription);

  System.Assert(Condition, ProblemDescription);
end;

procedure TIdSipTransport.AssertReceiveWellFormed(Msg: TIdSipMessage);
const
  LogMsg = 'A Transport must NEVER send invalid messages up the stack (%s): %s';
begin
  Self.AssertWellFormed(Msg, LogMsg);
end;

procedure TIdSipTransport.AssertSendWellFormed(Msg: TIdSipMessage);
const
  LogMsg = 'A Transport must NEVER send invalid messages onto the network (%s): %s';
begin
  Self.AssertWellFormed(Msg, LogMsg);
end;

procedure TIdSipTransport.AssertWellFormed(Msg: TIdSipMessage; LogMsgTemplate: String);
begin
  // PRECONDITION: There must be (at least) two %s occurences in LogMsgTemplate.

  Self.Assert(not Msg.IsMalformed, Format(LogMsgTemplate, [Msg.ParseFailReason, Msg.AsString]));
end;

procedure TIdSipTransport.DestroyServer;
begin
end;

function TIdSipTransport.GetAddress: String;
begin
  Result := Self.fAddress;
end;

function TIdSipTransport.GetBindings: TIdSocketHandles;
begin
  Result := nil;
  RaiseAbstractError(Self.ClassName, 'GetBindings');
end;

function TIdSipTransport.GetConserveConnections: Boolean;
begin
  Result := Self.fConserveConnections;
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

procedure TIdSipTransport.Log(Description: String;
                              Severity: TSeverityLevel;
                              EventRef: Cardinal;
                              DebugInfo: String);
begin
  LogEntry(Description, Self.ClassName, Severity, EventRef, DebugInfo);
end;

procedure TIdSipTransport.LogException(FailedMessage: TIdSipMessage;
                                       E: Exception;
                                       Reason: String);
const
  LogMsg = '%s sending %s: %s';
begin
  Self.Log(Format(LogMsg, [E.ClassName, FailedMessage.Description, Reason]),
           slError,
           LogEventException,
           FailedMessage.AsString);
end;

procedure TIdSipTransport.LogReceivedMessage(Msg: TIdSipMessage;
                                             ReceivedFrom: TIdConnectionBindings);
const
  LogMsg = 'Received %s from %s:%d on %s:%d';
begin
  Self.Log(Format(LogMsg, [Msg.Description, ReceivedFrom.PeerIP, ReceivedFrom.PeerPort, ReceivedFrom.LocalIP, ReceivedFrom.LocalPort]),
           slDebug,
           0,
           ReceivedFrom.AsString + CRLF + Msg.AsString);
end;

procedure TIdSipTransport.LogRejectedMessage(Msg: String;
                                             Reason: String;
                                             ReceivedFrom: TIdConnectionBindings);
const
  LogMsg = 'Rejected message starting "%s" from %s:%d on %s:%d: %s';
begin
  Self.Log(Format(LogMsg, [Copy(Msg, 1, 30), ReceivedFrom.PeerIP, ReceivedFrom.PeerPort, ReceivedFrom.LocalIP, ReceivedFrom.LocalPort, Reason]),
           slDebug,
           0,
           ReceivedFrom.AsString + CRLF + Reason + CRLF + Msg);
end;

procedure TIdSipTransport.LogSentMessage(Msg: TIdSipMessage; SentTo: TIdConnectionBindings);
const
  LogMsg = 'Sent %s to %s:%d from %s:%d';
begin
  Self.Log(Format(LogMsg, [Msg.Description, SentTo.PeerIP, SentTo.PeerPort, SentTo.LocalIP, SentTo.LocalPort]),
           slDebug,
           0,
           SentTo.AsString + CRLF + Msg.AsString);
end;

procedure TIdSipTransport.NotifyOfConnection(Connection: TIdConnectionBindings);
var
  Notification: TIdSipTransportConnectionMethod;
begin
  Notification := TIdSipTransportConnectionMethod.Create;
  try
    Notification.Connection := Connection;
    Notification.Transport  := Self;

    Self.ConnectionListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransport.NotifyOfDisconnection(Connection: TIdConnectionBindings);
var
  Notification: TIdSipTransportDisconnectionMethod;
begin
  Notification := TIdSipTransportDisconnectionMethod.Create;
  try
    Notification.Connection := Connection;
    Notification.Transport  := Self;

    Self.ConnectionListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransport.NotifyOfReceivedRequest(Request: TIdSipRequest;
                                                  ReceivedFrom: TIdConnectionBindings);
var
  Notification: TIdSipTransportReceiveRequestMethod;
begin
  Self.LogReceivedMessage(Request, ReceivedFrom);

  Self.AssertReceiveWellFormed(Request);

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
                                                   ReceivedFrom: TIdConnectionBindings);
var
  Notification: TIdSipTransportReceiveResponseMethod;
begin
  Self.LogReceivedMessage(Response, ReceivedFrom);

  Self.AssertReceiveWellFormed(Response);

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
  Self.LogException(FailedMessage, E, Reason);

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
                                                  const Reason: String;
                                                  ReceivedFrom: TIdConnectionBindings);
var
  Notification: TIdSipTransportRejectedMessageMethod;
begin
  Self.LogRejectedMessage(Msg, Reason, ReceivedFrom);

  Notification := TIdSipTransportRejectedMessageMethod.Create;
  try
    Notification.Msg    := Msg;
    Notification.Reason := Reason;
    Notification.Source := ReceivedFrom;

    Self.TransportListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransport.NotifyOfSentRequest(Request: TIdSipRequest;
                                              Binding: TIdConnectionBindings);
var
  Notification: TIdSipTransportSendingRequestMethod;
begin
  Self.LogSentMessage(Request, Binding);

  Notification := TIdSipTransportSendingRequestMethod.Create;
  try
    Notification.Binding := Binding;
    Notification.Sender  := Self;
    Notification.Request := Request;

    Self.TransportSendingListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransport.NotifyOfSentResponse(Response: TIdSipResponse;
                                               Binding: TIdConnectionBindings);
var
  Notification: TIdSipTransportSendingResponseMethod;
begin
  Self.LogSentMessage(Response, Binding);

  Notification := TIdSipTransportSendingResponseMethod.Create;
  try
    Notification.Binding  := Binding;
    Notification.Sender   := Self;
    Notification.Response := Response;

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
                                             const Reason: String;
                                             ReceivedFrom: TIdConnectionBindings);
begin
  Self.NotifyOfRejectedMessage(Msg, Reason, ReceivedFrom);
end;

procedure TIdSipTransport.OnReceiveRequest(Request: TIdSipRequest;
                                           ReceivedFrom: TIdConnectionBindings);
begin
  Self.ReceiveRequest(Request, ReceivedFrom);
end;

procedure TIdSipTransport.OnReceiveResponse(Response: TIdSipResponse;
                                            ReceivedFrom: TIdConnectionBindings);
begin
  Self.ReceiveResponse(Response, ReceivedFrom);
end;

procedure TIdSipTransport.ReturnBadRequest(Request: TIdSipRequest;
                                           Target: TIdConnectionBindings;
                                           const StatusText: String);
var
  Res: TIdSipResponse;
begin
  Res := TIdSipResponse.InResponseTo(Request, SIPBadRequest);
  try
    Res.StatusText := StatusText;

    Self.SendResponse(Res, Target);
  finally
    Res.Free;
  end;
end;

procedure TIdSipTransport.SendMessage(M: TIdSipMessage;
                                      Dest: TIdConnectionBindings);
begin
  RaiseAbstractError(Self.ClassName, 'SendMessage');
end;

procedure TIdSipTransport.SendRequest(R: TIdSipRequest;
                                      Dest: TIdSipLocation);
var
  LocalBinding: TIdConnectionBindings;
begin
  if Self.UseRport then
    R.LastHop.Params[RportParam] := '';

  LocalBinding := TIdConnectionBindings.Create;
  try
    LocalBinding.Assign(Dest);

    // We expect the subclass to fill in the local IP and port.
    Self.SendMessage(R, LocalBinding);

    Self.NotifyOfSentRequest(R, LocalBinding);
  finally
    LocalBinding.Free;
  end;
end;

procedure TIdSipTransport.SendResponse(R: TIdSipResponse;
                                       Dest: TIdSipLocation);
var
  LocalBinding: TIdConnectionBindings;
begin
  // Send a response to a machine identified by a transport/ip-address/port
  // triple.
  LocalBinding := TIdConnectionBindings.Create;
  try
    LocalBinding.Assign(Dest);

    Self.SendResponse(R, LocalBinding);
  finally
    LocalBinding.Free;
  end;
end;

procedure TIdSipTransport.SendResponse(R: TIdSipResponse;
                                       Binding: TIdConnectionBindings);
begin
  // Send a response to a machine when you already know exactly what binding to
  // use.

  // We expect the subclass to fill in the local IP and port.
  Self.SendMessage(R, Binding);

  Self.NotifyOfSentResponse(R, Binding);
end;

function TIdSipTransport.SentByIsRecognised(Via: TIdSipViaHeader): Boolean;
var
  I: Integer;
begin
  // Does the sent-by match our hostname?
  Result := IsEqual(Via.SentBy, Self.HostName);

  I := 0;

  if not Result then begin
    // Or does it match a binding?
    while (I < Self.Bindings.Count) and not Result do begin
      Result := Result or (Self.Bindings[I].IP = Via.SentBy);

      Inc(I);
    end;
  end;

  if not Result then begin
    // Or does it match a mapped route?
    Result := Self.RoutingTable.HasRouteThrough(Via.SentBy)
  end;
end;

procedure TIdSipTransport.SetConserveConnections(Value: Boolean);
begin
  Self.fConserveConnections := Value;
end;

procedure TIdSipTransport.SetTimeout(Value: Cardinal);
begin
  Self.fTimeout := Value;
end;

procedure TIdSipTransport.SetTimer(Value: TIdTimerQueue);
begin
  Self.fTimer := Value;
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

class function TIdSipTransportRegistry.NonstandardPort(const Transport: String; Port: Cardinal): Boolean;
begin
  Result := Self.TransportTypeFor(Transport).DefaultPort <> Port
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

class function TIdSipTransportRegistry.TransportTypeFor(const Transport: String): TIdSipTransportClass;
var
  Index: Integer;
begin
  Index := Self.TransportTypeRegistry.IndexOf(Transport);

  if (Index <> ItemNotFoundIndex) then
    Result := Self.TransportTypeAt(Index)
  else
    raise EUnknownTransport.Create(Format('TIdSipTransportRegistry.TransportTypeFor: "%s"', [Transport]));
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

class function TIdSipTransportRegistry.TransportTypeAt(Index: Integer): TIdSipTransportClass;
begin
  Result := TIdSipTransportClass(Self.TransportTypeRegistry.Objects[Index]);
end;

class function TIdSipTransportRegistry.TransportTypeRegistry: TStrings;
begin
  Result := GTransportTypes;
end;

//******************************************************************************
//* TIdSipDebugTransportRegistry                                               *
//******************************************************************************
//* TIdSipDebugTransportRegistry Public methods ********************************

class function TIdSipDebugTransportRegistry.TransportRunningOn(Host: String; Port: Cardinal): TIdSipTransport;
var
  I: Integer;
  L: TStrings;
  T: TIdSipTransport;
begin
  L := Self.GetAllTransports;
  try
    Result := nil;

    for I := 0 to L.Count - 1 do begin
      T := L.Objects[I] as TIdSipTransport;

      if T.HasBinding(Host, Port) then begin
        Result := T;
        Break;
      end;
    end;
  finally
    L.Free;
  end;
end;

class function TIdSipDebugTransportRegistry.TransportRunningOn(Transport, Host: String; Port: Cardinal): TIdSipTransport;
var
  I: Integer;
  L: TStrings;
  T: TIdSipTransport;
begin
  L := Self.GetAllTransports;
  try
    Result := nil;

    for I := 0 to L.Count - 1 do begin
      T := L.Objects[I] as TIdSipTransport;

      if T.HasBinding(Host, Port) and (T.GetTransportType = Transport) then begin
        Result := T;
        Break;
      end;
    end;
  finally
    L.Free;
  end;
end;

class function TIdSipDebugTransportRegistry.TransportCount: Integer;
var
  L: TStrings;
begin
  L := Self.GetAllTransports;
  try
    Result := L.Count;
  finally
    L.Free;
  end;
end;

//* TIdSipDebugTransportRegistry Private methods *******************************

class function TIdSipDebugTransportRegistry.GetAllTransports: TStrings;
begin
  Result := TStringList.Create;
  TIdObjectRegistry.CollectAllObjectsOfClass(TIdSipTransport, Result, true);
end;

//******************************************************************************
//* TIdSipTransportWait                                                        *
//******************************************************************************
//* TIdSipTransportWait Public methods *****************************************

procedure TIdSipTransportWait.TriggerOn(Transport: TIdSipTransport);
begin
  // By default, do nothing.
end;

procedure TIdSipTransportWait.Trigger;
var
  O: TObject;
begin
  O := TIdObjectRegistry.FindObject(Self.TransportID);

  if Assigned(O) and (O is TIdSipTransport) then
    Self.TriggerOn(O as TIdSipTransport);
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

//* TIdSipMessageExceptionWait Protected methods *******************************

procedure TIdSipMessageExceptionWait.TriggerOn(Transport: TIdSipTransport);
var
  FakeException: Exception;
begin
  FakeException := Self.ExceptionType.Create(Self.ExceptionMessage);
  try
    Transport.ReceiveException(Self.FailedMessage,
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

constructor TIdSipReceiveMessageWait.Create;
begin
  inherited Create;

  Self.fReceivedFrom := TIdConnectionBindings.Create;
end;

destructor TIdSipReceiveMessageWait.Destroy;
begin
  Self.fReceivedFrom.Free;

  inherited Destroy;
end;

procedure TIdSipReceiveMessageWait.Trigger;
var
  Receiver: TObject;
begin
  Receiver := TIdObjectRegistry.FindObject(Self.TransportID);

  if Assigned(Receiver) and (Receiver is TIdSipTransport) then begin
    if Self.Message.IsRequest then
      (Receiver as TIdSipTransport).ReceiveRequest(Self.Message as TIdSipRequest,
                                                   Self.ReceivedFrom)
    else
      (Receiver as TIdSipTransport).ReceiveResponse(Self.Message as TIdSipResponse,
                                                    Self.ReceivedFrom);
  end;
end;

procedure TIdSipReceiveMessageWait.SetReceivedFrom(Value: TIdConnectionBindings);
begin
  Self.fReceivedFrom.Assign(Value);
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
                                                         Self.Reason,
                                                         Self.Source);
end;

//******************************************************************************
//* TIdSipTransportSendingRequestMethod                                        *
//******************************************************************************
//* TIdSipTransportSendingRequestMethod Public methods *************************

procedure TIdSipTransportSendingRequestMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransportSendingListener).OnSendRequest(Self.Request,
                                                            Self.Sender,
                                                            Self.Binding);
end;

//******************************************************************************
//* TIdSipTransportSendingResponseMethod                                       *
//******************************************************************************
//* TIdSipTransportSendingResponseMethod Public methods ************************

procedure TIdSipTransportSendingResponseMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransportSendingListener).OnSendResponse(Self.Response,
                                                             Self.Sender,
                                                             Self.Binding);
end;

//******************************************************************************
//* TIdSipTransportConnectionMethod                                            *
//******************************************************************************
//* TIdSipTransportConnectionMethod Public methods *****************************

procedure TIdSipTransportConnectionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipConnectionListener).OnConnection(Self.Transport, Self.Connection);
end;

//******************************************************************************
//* TIdSipTransportDisconnectionMethod                                         *
//******************************************************************************
//* TIdSipTransportDisconnectionMethod Public methods **************************

procedure TIdSipTransportDisconnectionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipConnectionListener).OnDisconnection(Self.Transport, Self.Connection);
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
  GTransportTypes := TStringList.Create;
finalization
// These objects are purely memory-based, so it's safe not to free them here.
// Still, perhaps we need to review this methodology. How else do we get
// something like class variables?
//  GTransportTypes.Free;
end.
