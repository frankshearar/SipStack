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
  Contnrs, IdException, IdInterfacedObject, IdNotification, IdSipMessage,
  IdSipTcpClient, IdSipTcpServer, IdSipTlsServer, IdSipUdpServer,
  IdSocketHandle, IdSSLOpenSSL, SyncObjs, SysUtils;

type
  TIdSipTransport = class;
  TIdSipTransportClass = class of TIdSipTransport;

  // I listen for incoming messages.
  IIdSipTransportListener = interface
    ['{D3F0A0D5-A4E9-42BD-B337-D5B3C652F340}']
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
  end;

  // I listen for when messages are sent, rather than received. I'm most useful
  // as a logger/debugging tool.
  IIdSipTransportSendingListener = interface
    ['{2E451F5D-5053-4A2C-BE5F-BB68E5CB3A6D}']
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport);
  end;

  TIdSipTransport = class(TIdInterfacedObject,
                          IIdSipMessageVisitor,
                          IIdSipMessageListener)
  private
    fHostName:                 String;
    fTimeout:                  Cardinal;
    fUseRport:                 Boolean;
    TransportListeners:        TIdNotificationList;
    TransportSendingListeners: TIdNotificationList;

    procedure RewriteOwnVia(Msg: TIdSipMessage);
  protected
    procedure ChangeBinding(const Address: String; Port: Cardinal); virtual; abstract;
    function  GetAddress: String; virtual; abstract;
    function  GetBindings: TIdSocketHandles; virtual; abstract;
    function  GetPort: Cardinal; virtual; abstract;
    procedure NotifyTransportListeners(Request: TIdSipRequest); overload;
    procedure NotifyTransportListeners(Response: TIdSipResponse); overload;
    procedure NotifyTransportListenersOfException(E: Exception;
                                                  const Reason: String);
    procedure NotifyTransportListenersOfRejectedMessage(const Msg: String;
                                                        const Reason: String);
    procedure NotifyTransportSendingListeners(Request: TIdSipRequest); overload;
    procedure NotifyTransportSendingListeners(Response: TIdSipResponse); overload;
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnMalformedMessage(const Msg: String;
                                 const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdSipConnectionBindings); virtual;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdSipConnectionBindings);
    procedure ReturnBadRequest(Request: TIdSipRequest;
                               Target: TIdSipConnectionBindings); virtual; abstract;
    procedure SendRequest(R: TIdSipRequest); virtual;
    procedure SendResponse(R: TIdSipResponse); virtual;
    function  SentByIsRecognised(Via: TIdSipViaHeader): Boolean; virtual;
    procedure SetAddress(const Value: String); virtual;
    procedure SetPort(Value: Cardinal);

    property Bindings: TIdSocketHandles read GetBindings;
  public
    class function TransportFor(TT: TIdSipTransportType): TIdSipTransportClass;

    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddTransportListener(const Listener: IIdSipTransportListener);
    procedure AddTransportSendingListener(const Listener: IIdSipTransportSendingListener);
    function  DefaultPort: Cardinal; virtual;
    function  DefaultTimeout: Cardinal; virtual;
    function  GetTransportType: TIdSipTransportType; virtual; abstract;
    function  IsNull: Boolean; virtual;
    function  IsReliable: Boolean; virtual;
    function  IsSecure: Boolean; virtual;
    procedure RemoveTransportListener(const Listener: IIdSipTransportListener);
    procedure RemoveTransportSendingListener(const Listener: IIdSipTransportSendingListener);
    procedure Send(Msg: TIdSipMessage);
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure VisitRequest(Request: TIdSipRequest);
    procedure VisitResponse(Response: TIdSipResponse);

    property Address:  String           read GetAddress write SetAddress;
    property HostName: String           read fHostName write fHostName;
    property Port:     Cardinal         read GetPort write SetPort;
    property Timeout:  Cardinal         read fTimeout write fTimeout;
    property UseRport: Boolean          read fUseRport write fUseRport;
  end;

  TIdSipTCPTransport = class(TIdSipTransport)
  private
    Clients:    TObjectList;
    ClientLock: TCriticalSection;

    function  AddClient: TIdSipTcpClient;
    procedure DoOnClientFinished(Sender: TObject);
    procedure DoOnTcpResponse(Sender: TObject;
                              Response: TIdSipResponse;
                              ReceivedFrom: TIdSipConnectionBindings);
    procedure RemoveClient(Client: TIdSipTcpClient);
  protected
    Transport: TIdSipTcpServer;

    procedure ChangeBinding(const Address: String; Port: Cardinal); override;
    function  GetAddress: String; override;
    function  CreateClient: TIdSipTcpClient; virtual;
    procedure DestroyClient(Client: TIdSipTcpClient); virtual;
    function  GetBindings: TIdSocketHandles; override;
    function  GetPort: Cardinal; override;
    procedure SendRequest(R: TIdSipRequest); override;
    procedure SendResponse(R: TIdSipResponse); override;
    function  ServerType: TIdSipTcpServerClass; virtual;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function  GetTransportType: TIdSipTransportType; override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TIdSipTLSTransport = class(TIdSipTCPTransport)
  private
    function  GetOnGetPassword: TPasswordEvent;
    function  GetRootCertificate: TFileName;
    function  GetServerCertificate: TFileName;
    function  GetServerKey: TFileName;
    function  TLS: TIdSipTlsServer;
    procedure SetOnGetPassword(Value: TPasswordEvent);
    procedure SetRootCertificate(Value: TFileName);
    procedure SetServerCertificate(Value: TFileName);
    procedure SetServerKey(Value: TFileName);
  protected
    function  CreateClient: TIdSipTcpClient; override;
    procedure DestroyClient(Client: TIdSipTcpClient); override;
    function  ServerType: TIdSipTcpServerClass; override;
  public
    function  DefaultPort: Cardinal; override;
    function  GetTransportType: TIdSipTransportType; override;
    function  IsSecure: Boolean; override;

    property OnGetPassword:     TPasswordEvent read GetOnGetPassword write SetOnGetPassword;
    property RootCertificate:   TFileName read GetRootCertificate write SetRootCertificate;
    property ServerCertificate: TFileName read GetServerCertificate write SetServerCertificate;
    property ServerKey:         TFileName read GetServerKey write SetServerKey;
  end;

  TIdSipSCTPTransport = class(TIdSipTransport)
  public
    function GetTransportType: TIdSipTransportType; override;
  end;

  TIdSipUDPTransport = class(TIdSipTransport)
  private
    Transport: TIdSipUdpServer;

  protected
    procedure ChangeBinding(const Address: String; Port: Cardinal); override;
    function  GetAddress: String; override;
    function  GetBindings: TIdSocketHandles; override;
    function  GetPort: Cardinal; override;
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdSipConnectionBindings); override;
    procedure ReturnBadRequest(Request: TIdSipRequest;
                               Target: TIdSipConnectionBindings); override;
    procedure SendRequest(R: TIdSipRequest); override;
    procedure SendResponse(R: TIdSipResponse); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function  GetTransportType: TIdSipTransportType; override;
    function  IsReliable: Boolean; override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TIdSipNullTransport = class(TIdSipTransport)
  private
    FakeBindings: TIdSocketHandles;
  protected
    function  GetBindings: TIdSocketHandles; override;
    function  GetPort: Cardinal; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function GetTransportType: TIdSipTransportType; override;
    function IsNull: Boolean; override;
  end;

  TIdSipTransportExceptionMethod = class(TIdMethod)
  private
    fException: Exception;
    fReason:    String;
  public
    procedure Run(const Subject: IInterface); override;

    property Exception: Exception read fException write fException;
    property Reason:    String    read fReason write fReason;
  end;

  TIdSipTransportReceiveRequestMethod = class(TIdMethod)
  private
    fReceiver: TIdSipTransport;
    fRequest:  TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property Receiver: TIdSipTransport read fReceiver write fReceiver;
    property Request:  TIdSipRequest   read fRequest write fRequest;
  end;

  TIdSipTransportReceiveResponseMethod = class(TIdMethod)
  private
    fReceiver: TIdSipTransport;
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Receiver: TIdSipTransport read fReceiver write fReceiver;
    property Response: TIdSipResponse  read fResponse write fResponse;
  end;

  TIdSipTransportRejectedMessageMethod = class(TIdMethod)
  private
    fMsg:    String;
    fReason: String;
  public
    procedure Run(const Subject: IInterface); override;

    property Msg:    String read fMsg write fMsg;
    property Reason: String read fReason write fReason;
  end;

  TIdSipTransportSendingRequestMethod = class(TIdMethod)
  private
    fSender:  TIdSipTransport;
    fRequest: TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property Sender:  TIdSipTransport read fSender write fSender;
    property Request: TIdSipRequest   read fRequest write fRequest;
  end;

  TIdSipTransportSendingResponseMethod = class(TIdMethod)
  private
    fSender: TIdSipTransport;
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Sender:   TIdSipTransport read fSender write fSender;
    property Response: TIdSipResponse  read fResponse write fResponse;
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
  ResponseNotSentFromHere = 'Received response could not have been sent from here';

implementation

uses
  IdSipConsts, IdTCPServer;

//******************************************************************************
//* TIdSipTransport                                                            *
//******************************************************************************
//* TIdSipTransport Public methods *********************************************

class function TIdSipTransport.TransportFor(TT: TIdSipTransportType): TIdSipTransportClass;
begin
  case TT of
    sttSCTP: Result := TIdSipSCTPTransport;
    sttTCP:  Result := TIdSipTCPTransport;
    sttTLS:  Result := TIdSipTLSTransport;
    sttUDP:  Result := TIdSipUDPTransport;
    sttNULL: Result := TIdSipNullTransport;
  else
    raise EUnknownTransport.Create('TIdSipTransport.TransportFor');
  end;
end;

constructor TIdSipTransport.Create;
begin
  inherited Create;

  Self.TransportListeners        := TIdNotificationList.Create;
  Self.TransportSendingListeners := TIdNotificationList.Create;

  Self.Timeout  := Self.DefaultTimeout;
  Self.UseRport := false;
end;

destructor TIdSipTransport.Destroy;
begin
  Self.TransportSendingListeners.Free;
  Self.TransportListeners.Free;

  inherited Destroy;
end;

procedure TIdSipTransport.AddTransportListener(const Listener: IIdSipTransportListener);
begin
  Self.TransportListeners.AddListener(Listener);
end;

procedure TIdSipTransport.AddTransportSendingListener(const Listener: IIdSipTransportSendingListener);
begin
  Self.TransportSendingListeners.AddListener(Listener);
end;

function TIdSipTransport.DefaultPort: Cardinal;
begin
  Result := IdPORT_SIP;
end;

function TIdSipTransport.DefaultTimeout: Cardinal;
begin
  Result := 5000;
end;

function TIdSipTransport.IsNull: Boolean;
begin
  Result := false;
end;

function TIdSipTransport.IsReliable: Boolean;
begin
  Result := true;
end;

function TIdSipTransport.IsSecure: Boolean;
begin
  Result := false;
end;

procedure TIdSipTransport.RemoveTransportListener(const Listener: IIdSipTransportListener);
begin
  Self.TransportListeners.RemoveListener(Listener);
end;

procedure TIdSipTransport.RemoveTransportSendingListener(const Listener: IIdSipTransportSendingListener);
begin
  Self.TransportSendingListeners.RemoveListener(Listener);
end;

procedure TIdSipTransport.Send(Msg: TIdSipMessage);
begin
  try
    Assert(Msg.ContentLength = Length(Msg.Body),
           'Content-Length MUST equal length of body');
    Msg.Accept(Self);
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

procedure TIdSipTransport.VisitRequest(Request: TIdSipRequest);
begin
  Self.SendRequest(Request);
end;

procedure TIdSipTransport.VisitResponse(Response: TIdSipResponse);
begin
  Self.SendResponse(Response);
end;

//* TIdSipTransport Protected methods ******************************************

procedure TIdSipTransport.NotifyTransportListeners(Request: TIdSipRequest);
var
  Notification: TIdSipTransportReceiveRequestMethod;
begin
  Assert(not Request.IsMalformed,
         'A Transport must NEVER send invalid requests up the stack');

  Notification := TIdSipTransportReceiveRequestMethod.Create;
  try
    Notification.Receiver := Self;
    Notification.Request  := Request;

    Self.TransportListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransport.NotifyTransportListeners(Response: TIdSipResponse);
var
  Notification: TIdSipTransportReceiveResponseMethod;
begin
  Assert(not Response.IsMalformed,
         'A Transport must NEVER send invalid responses up the stack');

  Notification := TIdSipTransportReceiveResponseMethod.Create;
  try
    Notification.Receiver := Self;
    Notification.Response := Response;

    Self.TransportListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransport.NotifyTransportListenersOfException(E: Exception;
                                                              const Reason: String);
var
  Notification: TIdSipTransportExceptionMethod;
begin
  Notification := TIdSipTransportExceptionMethod.Create;
  try
    Notification.Exception := E;
    Notification.Reason    := Reason;

    Self.TransportListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransport.NotifyTransportListenersOfRejectedMessage(const Msg: String;
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

procedure TIdSipTransport.NotifyTransportSendingListeners(Request: TIdSipRequest);
var
  Notification: TIdSipTransportSendingRequestMethod;
begin
  Notification := TIdSipTransportSendingRequestMethod.Create;
  try
    Notification.Sender  := Self;
    Notification.Request := Request;

    Self.TransportSendingListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransport.NotifyTransportSendingListeners(Response: TIdSipResponse);
var
  Notification: TIdSipTransportSendingResponseMethod;
begin
  Notification := TIdSipTransportSendingResponseMethod.Create;
  try
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
  Self.NotifyTransportListenersOfException(E, Reason);
end;

procedure TIdSipTransport.OnMalformedMessage(const Msg: String;
                                             const Reason: String);
begin
  Self.NotifyTransportListenersOfRejectedMessage(Msg, Reason);
end;

procedure TIdSipTransport.OnReceiveRequest(Request: TIdSipRequest;
                                           ReceivedFrom: TIdSipConnectionBindings);
begin
  if Request.IsMalformed then begin
    Self.NotifyTransportListenersOfRejectedMessage(Request.AsString,
                                                   Request.ParseFailReason);
    Self.ReturnBadRequest(Request, ReceivedFrom);
    Exit;
  end;

  // cf. RFC 3261 section 18.2.1
  if TIdSipParser.IsFQDN(Request.LastHop.SentBy)
    or (Request.LastHop.SentBy <> ReceivedFrom.PeerIP) then
    Request.LastHop.Received := ReceivedFrom.PeerIP;

  // We let the UA handle rejecting messages because of things like the UA
  // not supporting the SIP version or whatnot. This allows us to centralise
  // response generation.
  Self.NotifyTransportListeners(Request);
end;

procedure TIdSipTransport.OnReceiveResponse(Response: TIdSipResponse;
                                            ReceivedFrom: TIdSipConnectionBindings);
begin
  if Response.IsMalformed then begin
    Self.NotifyTransportListenersOfRejectedMessage(Response.AsString,
                                                   Response.ParseFailReason);
    // Drop the malformed response.                                               
    Exit;
  end;

  // cf. RFC 3261 section 18.1.2

  if Self.SentByIsRecognised(Response.LastHop) then begin
    Self.NotifyTransportListeners(Response);
  end
  else
    Self.NotifyTransportListenersOfRejectedMessage(Response.AsString,
                                                   ResponseNotSentFromHere);
end;

procedure TIdSipTransport.SendRequest(R: TIdSipRequest);
begin
  Self.RewriteOwnVia(R);
  Self.NotifyTransportSendingListeners(R);
end;

procedure TIdSipTransport.SendResponse(R: TIdSipResponse);
begin
//  Self.RewriteOwnVia(R);
  Self.NotifyTransportSendingListeners(R);
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

procedure TIdSipTransport.SetAddress(const Value: String);
begin
  Self.ChangeBinding(Value, Self.Port);
end;

procedure TIdSipTransport.SetPort(Value: Cardinal);
begin
  Self.ChangeBinding(Self.Address, Value);
end;

//* TIdSipTransport Private methods ********************************************

procedure TIdSipTransport.RewriteOwnVia(Msg: TIdSipMessage);
begin
  Assert(Msg.Path.Length > 0,
         'An outbound message must always have at least one Via, '
       + 'namely, this stack.');

  Msg.LastHop.Transport := Self.GetTransportType;
  Msg.LastHop.SentBy    := Self.HostName;
  Msg.LastHop.Port      := Self.Port;

  if Self.UseRport then
    Msg.LastHop.Params[RportParam] := '';
end;

//******************************************************************************
//* TIdSipTCPTransport                                                         *
//******************************************************************************
//* TIdSipTCPTransport Public methods ******************************************

constructor TIdSipTCPTransport.Create;
begin
  inherited Create;

  Self.Clients    := TObjectList.Create(true);
  Self.ClientLock := TCriticalSection.Create;
  Self.Transport  := Self.ServerType.Create(nil);
  Self.Transport.AddMessageListener(Self);

  Self.Bindings.Add;
  Self.SetPort(Port);
end;

destructor TIdSipTCPTransport.Destroy;
begin
  Self.Transport.Free;
  Self.ClientLock.Free;
  Self.Clients.Free;

  inherited Destroy;
end;

function TIdSipTCPTransport.GetTransportType: TIdSipTransportType;
begin
  Result := sttTCP;
end;

procedure TIdSipTCPTransport.Start;
begin
  Self.Transport.Active := true;
end;

procedure TIdSipTCPTransport.Stop;
begin
  Self.Transport.Active := false;
end;

//* TIdSipTCPTransport Protected methods ***************************************

procedure TIdSipTCPTransport.ChangeBinding(const Address: String; Port: Cardinal);
var
  Binding: TIdSocketHandle;
begin
  Self.Stop;

  Self.Transport.Bindings.Clear;
  Self.Bindings.DefaultPort := Port;
  Binding := Self.Bindings.Add;
  Binding.IP   := Address;
  Binding.Port := Port;

  Self.Start;
end;

function TIdSipTCPTransport.GetAddress: String;
begin
  Result := Self.Transport.Bindings[0].IP;
end;

function TIdSipTCPTransport.CreateClient: TIdSipTcpClient;
begin
  // Precondition: Self.ClientLock has been acquired.
  Result := TIdSipTcpClient.Create(nil);
  Result.OnFinished := Self.DoOnClientFinished;
  Result.OnResponse := Self.DoOnTcpResponse;
  Result.Timeout    := Self.Timeout;
end;

function TIdSipTCPTransport.GetPort: Cardinal;
begin
  Result := Self.Transport.DefaultPort;
end;

procedure TIdSipTCPTransport.SendRequest(R: TIdSipRequest);
var
  Client: TIdSipTcpClient;
begin
  inherited SendRequest(R);

  Client := Self.AddClient;

  Client.Host    := R.RequestUri.Host;
  Client.Port    := R.RequestUri.Port;
  Client.Timeout := Self.Timeout;

  Client.Connect(Self.Timeout);
  Client.Send(R);
end;

procedure TIdSipTCPTransport.SendResponse(R: TIdSipResponse);
begin
  inherited SendResponse(R);

  Self.Transport.SendResponse(R);
end;

function TIdSipTCPTransport.ServerType: TIdSipTcpServerClass;
begin
  Result := TIdSipTcpServer;
end;

procedure TIdSipTCPTransport.DestroyClient(Client: TIdSipTcpClient);
begin
  // Precondition: Self.ClientLock has been acquired.
  if Client.Connected then
    Client.Disconnect;

  Self.Clients.Remove(Client);
end;

function TIdSipTCPTransport.GetBindings: TIdSocketHandles;
begin
  Result := Self.Transport.Bindings;
end;

//* TIdSipTCPTransport Private methods *****************************************

function TIdSipTCPTransport.AddClient: TIdSipTcpClient;
begin
  Self.ClientLock.Acquire;
  try
    Result := Self.CreateClient;
    try
      Self.Clients.Add(Result);
    except
      // Remember, Self.Clients owns the object and will free it.
      Self.Clients.Remove(Result);
      Result := nil;

      raise;
    end;
  finally
    Self.ClientLock.Release;
  end;
end;

procedure TIdSipTCPTransport.DoOnClientFinished(Sender: TObject);
begin
  Self.RemoveClient(Sender as TIdSipTcpClient);
end;

procedure TIdSipTCPTransport.DoOnTcpResponse(Sender: TObject;
                                             Response: TIdSipResponse;
                                             ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.OnReceiveResponse(Response, ReceivedFrom);
end;

procedure TIdSipTCPTransport.RemoveClient(Client: TIdSipTcpClient);
begin
  Self.ClientLock.Acquire;
  try
    Self.DestroyClient(Client);
  finally
    Self.ClientLock.Release;
  end;
end;

//******************************************************************************
//* TIdSipTLSTransport                                                         *
//******************************************************************************
//* TIdSipTLSTransport Public methods ******************************************

function TIdSipTLSTransport.DefaultPort: Cardinal;
begin
  Result := IdPORT_SIPS;
end;

function TIdSipTLSTransport.GetTransportType: TIdSipTransportType;
begin
  Result := sttTLS;
end;

function TIdSipTLSTransport.IsSecure: Boolean;
begin
  Result := true;
end;

//* TIdSipTLSTransport Protected methods ***************************************

function TIdSipTLSTransport.CreateClient: TIdSipTcpClient;
begin
  Result := inherited CreateClient;

  Result.IOHandler := TIdSSLIOHandlerSocket.Create(nil);
end;

procedure TIdSipTLSTransport.DestroyClient(Client: TIdSipTcpClient);
begin
  Client.IOHandler.Free;

  inherited DestroyClient(Client);
end;

function TIdSipTLSTransport.ServerType: TIdSipTcpServerClass;
begin
  Result := TIdSipTlsServer;
end;

//* TIdSipTLSTransport Private methods *****************************************

function TIdSipTLSTransport.GetOnGetPassword: TPasswordEvent;
begin
  Result := Self.TLS.OnGetPassword;
end;

function TIdSipTLSTransport.GetRootCertificate: TFileName;
begin
  Result := Self.TLS.RootCertificate;
end;

function TIdSipTLSTransport.GetServerCertificate: TFileName;
begin
  Result := Self.TLS.ServerCertificate;
end;

function TIdSipTLSTransport.GetServerKey: TFileName;
begin
  Result := Self.TLS.ServerKey;
end;

function TIdSipTLSTransport.TLS: TIdSipTlsServer;
begin
  Result := Self.Transport as TIdSipTlsServer;
end;

procedure TIdSipTLSTransport.SetOnGetPassword(Value: TPasswordEvent);
begin
  Self.TLS.OnGetPassword := Value;
end;

procedure TIdSipTLSTransport.SetRootCertificate(Value: TFileName);
begin
  Self.TLS.RootCertificate := Value;
end;

procedure TIdSipTLSTransport.SetServerCertificate(Value: TFileName);
begin
  Self.TLS.ServerCertificate := Value;
end;

procedure TIdSipTLSTransport.SetServerKey(Value: TFileName);
begin
  Self.TLS.ServerKey := Value;
end;

//******************************************************************************
//* TIdSipSCTPTransport                                                        *
//******************************************************************************
//* TIdSipSCTPTransport Public methods *****************************************

function TIdSipSCTPTransport.GetTransportType: TIdSipTransportType;
begin
  Result := sttSCTP;
end;

//******************************************************************************
//* TIdSipUDPTransport                                                         *
//******************************************************************************
//* TIdSipUDPTransport Public methods ******************************************

constructor TIdSipUDPTransport.Create;
begin
  inherited Create;

  Self.Transport := TIdSipUdpServer.Create(nil);
  Self.Transport.AddMessageListener(Self);
  Self.Transport.ThreadedEvent := true;

  Self.Bindings.Add;
end;

destructor TIdSipUDPTransport.Destroy;
begin
  Self.Transport.Free;

  inherited Destroy;
end;

function TIdSipUDPTransport.GetTransportType: TIdSipTransportType;
begin
  Result := sttUDP;
end;

function TIdSipUDPTransport.IsReliable: Boolean;
begin
  Result := false;
end;

procedure TIdSipUDPTransport.Start;
begin
  Self.Transport.Active := true;
end;

procedure TIdSipUDPTransport.Stop;
begin
  Self.Transport.Active := false;
end;

//* TIdSipUDPTransport Protected methods ***************************************

procedure TIdSipUDPTransport.ChangeBinding(const Address: String; Port: Cardinal);
var
  Binding: TIdSocketHandle;
begin
  Self.Stop;

  Self.Transport.Bindings.Clear;
  Self.Bindings.DefaultPort := Port;
  Binding := Self.Bindings.Add;
  Binding.IP   := Address;
  Binding.Port := Port;

  Self.Start;
end;

function TIdSipUDPTransport.GetAddress: String;
begin
  Result := Self.Bindings[0].IP;
end;

function TIdSipUDPTransport.GetBindings: TIdSocketHandles;
begin
  Result := Self.Transport.Bindings;
end;

function TIdSipUDPTransport.GetPort: Cardinal;
begin
  Result := Self.Transport.DefaultPort;
end;

procedure TIdSipUDPTransport.OnReceiveRequest(Request: TIdSipRequest;
                                              ReceivedFrom: TIdSipConnectionBindings);
begin
  // RFC 3581 section 4
  if Request.LastHop.HasRPort then begin
    if not Request.LastHop.HasReceived then
      Request.LastHop.Received := ReceivedFrom.PeerIP;

    Request.LastHop.RPort := ReceivedFrom.PeerPort;
  end;

  inherited OnReceiveRequest(Request, ReceivedFrom);
end;

procedure TIdSipUDPTransport.ReturnBadRequest(Request: TIdSipRequest;
                                              Target: TIdSipConnectionBindings);
var
  Res: TIdSipResponse;
begin
  Res := TIdSipResponse.InResponseTo(Request, SIPBadRequest);
  try
    Res.StatusText := Request.ParseFailReason;

    Self.Transport.Send(Target.PeerIP,
                        Target.PeerPort,
                        Res.AsString);
  finally
    Res.Free;
  end;
end;

procedure TIdSipUDPTransport.SendRequest(R: TIdSipRequest);
begin
  inherited SendRequest(R);

  Self.Transport.Send(R.RequestUri.Host,
                      R.RequestUri.Port,
                      R.AsString);
end;

procedure TIdSipUDPTransport.SendResponse(R: TIdSipResponse);
var
  Host: String;
  Port: Cardinal;
begin
  inherited SendResponse(R);

  if R.LastHop.HasMaddr then begin
    Host := R.LastHop.Maddr;
    Port := R.LastHop.Port;
  end
  else if R.LastHop.HasRport then begin
    // cf RFC 3581 section 4.
    // TODO: this isn't quite right. We have to send the response
    // from the ip/port that the request was received on.
    Host := R.LastHop.Received;
    Port := R.LastHop.RPort;
  end
  else if R.LastHop.HasReceived then begin
    Host := R.LastHop.Received;
    Port := R.LastHop.Port;
  end
  else begin
    Host := R.LastHop.SentBy;
    Port := R.LastHop.Port;
  end;

  Self.Transport.Send(Host, Port, R.AsString);
end;

//******************************************************************************
//* TIdSipNullTransport                                                        *
//******************************************************************************
//* TIdSipNullTransport Public methods *****************************************

constructor TIdSipNullTransport.Create;
begin
  inherited Create;

  Self.FakeBindings := TIdSocketHandles.Create(nil);
end;

destructor TIdSipNullTransport.Destroy;
begin
  Self.FakeBindings.Free;

  inherited Destroy;
end;

function TIdSipNullTransport.GetTransportType: TIdSipTransportType;
begin
  Result := sttNULL;
end;

function TIdSipNullTransport.IsNull: Boolean;
begin
  Result := true;
end;

//* TIdSipNullTransport Protected methods **************************************

function TIdSipNullTransport.GetBindings: TIdSocketHandles;
begin
  Result := Self.FakeBindings;
end;

function TIdSipNullTransport.GetPort: Cardinal;
begin
  Result := 0;
end;

//******************************************************************************
//* TIdSipTransportExceptionMethod                                             *
//******************************************************************************
//* TIdSipTransportExceptionMethod Public methods ******************************

procedure TIdSipTransportExceptionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransportListener).OnException(Self.Exception,
                                                   Self.Reason);
end;

//******************************************************************************
//* TIdSipTransportReceiveRequestMethod                                        *
//******************************************************************************
//* TIdSipTransportReceiveRequestMethod Public methods *************************

procedure TIdSipTransportReceiveRequestMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransportListener).OnReceiveRequest(Self.Request,
                                                        Self.Receiver);
end;

//******************************************************************************
//* TIdSipTransportReceiveResponseMethod                                       *
//******************************************************************************
//* TIdSipTransportReceiveResponseMethod Public methods ************************

procedure TIdSipTransportReceiveResponseMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransportListener).OnReceiveResponse(Self.Response,
                                                         Self.Receiver);
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
                                                            Self.Sender);
end;

//******************************************************************************
//* TIdSipTransportSendingResponseMethod                                       *
//******************************************************************************
//* TIdSipTransportSendingResponseMethod Public methods ************************

procedure TIdSipTransportSendingResponseMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransportSendingListener).OnSendResponse(Self.Response,
                                                             Self.Sender);
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

end.
