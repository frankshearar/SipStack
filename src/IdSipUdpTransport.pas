{
  (c) 2006 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipUdpTransport;

interface

uses
  Classes, IdSipLocator, IdSipMessage, IdSipServerNotifier, IdSipTransport,
  IdSocketHandle,IdTimerQueue, IdUDPServer, SysUtils;

type
  TIdSipUdpServer = class;

  // I implement the User Datagram Protocol (RFC 768) connections for the SIP
  // stack.
  TIdSipUDPTransport = class(TIdSipTransport)
  private
    Transport: TIdSipUdpServer;

  protected
    procedure DestroyServer; override;
    function  GetBindings: TIdSocketHandles; override;
    procedure InstantiateServer; override;
    procedure SendRequest(R: TIdSipRequest;
                          Dest: TIdSipLocation); override;
    procedure SendResponse(R: TIdSipResponse;
                           Dest: TIdSipLocation); override;
    procedure SetTimer(Value: TIdTimerQueue); override;
  public
    class function GetTransportType: String; override;
    class function SrvPrefix: String; override;

    constructor Create; override;

    function  IsReliable: Boolean; override;
    function  IsRunning: Boolean; override;
    procedure ReceiveRequest(Request: TIdSipRequest;
                             ReceivedFrom: TIdSipConnectionBindings); override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TIdSipReceiveUDPMessageWait = class;

  TIdSipUdpServer = class(TIdUDPServer)
  private
    fTimer:   TIdTimerQueue;
    Notifier: TIdSipServerNotifier;

    procedure DoOnException(Sender: TObject);
  protected
    procedure DoUDPRead(AData: TStream; ABinding: TIdSocketHandle); override;
    procedure NotifyListenersOfResponse(Response: TIdSipResponse;
                                        ReceivedFrom: TIdSipConnectionBindings); overload; virtual;
    procedure NotifyOfException(E: Exception);
    procedure ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                           Binding: TIdSocketHandle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure AddMessageListener(const Listener: IIdSipMessageListener);
    procedure RemoveMessageListener(const Listener: IIdSipMessageListener);

    property Timer: TIdTimerQueue read fTimer write fTimer;
  end;

  TIdSipUdpClient = class(TIdSipUdpServer)
  private
    fOnFinished: TNotifyEvent;

    procedure DoOnFinished;
  protected
    procedure NotifyListenersOfResponse(Response: TIdSipResponse;
                                        ReceivedFrom: TIdSipConnectionBindings); overload; override;
  public
    property OnFinished: TNotifyEvent read fOnFinished write fOnFinished;
  end;

  // I represent a (possibly) deferred receipt of a message.
  // Give me a COPY of a binding in ReceivedFrom, and I'll free it.
  TIdSipReceiveUDPMessageWait = class(TIdSipMessageWait)
  private
    fNotifier:     TIdSipServerNotifier;
    fReceivedFrom: TIdSipConnectionBindings;
  public
    destructor Destroy; override;

    procedure Trigger; override;

    property Notifier:     TIdSipServerNotifier     read fNotifier write fNotifier;
    property ReceivedFrom: TIdSipConnectionBindings read fReceivedFrom write fReceivedFrom;
  end;

implementation

uses
  IdSipDns;

//******************************************************************************
//* TIdSipUDPTransport                                                         *
//******************************************************************************
//* TIdSipUDPTransport Public methods ******************************************

class function TIdSipUDPTransport.GetTransportType: String;
begin
  Result := UdpTransport;
end;

class function TIdSipUDPTransport.SrvPrefix: String;
begin
  Result := SrvUdpPrefix;
end;

constructor TIdSipUDPTransport.Create;
begin
  inherited Create;

  Self.Bindings.Add;
end;

function TIdSipUDPTransport.IsReliable: Boolean;
begin
  Result := false;
end;

function TIdSipUDPTransport.IsRunning: Boolean;
begin
  Result := Self.Transport.Active;
end;

procedure TIdSipUDPTransport.ReceiveRequest(Request: TIdSipRequest;
                                            ReceivedFrom: TIdSipConnectionBindings);
begin
  // RFC 3581 section 4
  if Request.LastHop.HasRPort then begin
    if not Request.LastHop.HasReceived then
      Request.LastHop.Received := ReceivedFrom.PeerIP;

    Request.LastHop.RPort := ReceivedFrom.PeerPort;
  end;

  inherited ReceiveRequest(Request, ReceivedFrom);
end;

procedure TIdSipUDPTransport.Start;
begin
  inherited Start;

  Self.Transport.Active := true;
end;

procedure TIdSipUDPTransport.Stop;
begin
  Self.Transport.Active := false;
end;

//* TIdSipUDPTransport Protected methods ***************************************

procedure TIdSipUDPTransport.DestroyServer;
begin
  Self.Transport.Free;
end;

function TIdSipUDPTransport.GetBindings: TIdSocketHandles;
begin
  Result := Self.Transport.Bindings;
end;

procedure TIdSipUDPTransport.InstantiateServer;
begin
  Self.Transport := TIdSipUdpServer.Create(nil);
  Self.Transport.AddMessageListener(Self);
  Self.Transport.ThreadedEvent := true;
end;

procedure TIdSipUDPTransport.SendRequest(R: TIdSipRequest;
                                         Dest: TIdSipLocation);
begin
  inherited SendRequest(R, Dest);

  Self.Transport.Send(Dest.IPAddress,
                      Dest.Port,
                      R.AsString);
end;

procedure TIdSipUDPTransport.SendResponse(R: TIdSipResponse;
                                          Dest: TIdSipLocation);
begin
  inherited SendResponse(R, Dest);

  // cf RFC 3581 section 4.
  // TODO: this isn't quite right. We have to send the response (if that's what
  // the message is) from the ip/port that the request was received on.

  Self.Transport.Send(Dest.IPAddress,
                      Dest.Port,
                      R.AsString);
end;

procedure TIdSipUDPTransport.SetTimer(Value: TIdTimerQueue);
begin
  inherited SetTimer(Value);

  Self.Transport.Timer := Value;
end;

//******************************************************************************
//* TIdSipUdpServer                                                            *
//******************************************************************************
//* TIdSipUdpServer Public methods *********************************************

constructor TIdSipUdpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.DefaultPort   := TIdSipTransportRegistry.DefaultPortFor(UdpTransport);
  Self.Notifier      := TIdSipServerNotifier.Create;
  Self.ThreadedEvent := true;
end;

destructor TIdSipUdpServer.Destroy;
begin
  Self.Notifier.Free;

  inherited Destroy;
end;

procedure TIdSipUdpServer.AddMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.Notifier.AddMessageListener(Listener);
end;

procedure TIdSipUdpServer.RemoveMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.Notifier.RemoveMessageListener(Listener);
end;

//* TIdSipUdpServer Protected methods ******************************************

procedure TIdSipUdpServer.DoUDPRead(AData: TStream; ABinding: TIdSocketHandle);
var
  Msg: TIdSipMessage;
begin
  // Note that if AData contains a fragment of a message we don't care to
  // reassemble the packet. RFC 3261 section 18.3 tells us:

  //   If the transport packet ends before the end of the
  //   message body, this is considered an error.  If the message is a
  //   response, it MUST be discarded.  If the message is a request, the
  //   element SHOULD generate a 400 (Bad Request) response.  If the message
  //   has no Content-Length header field, the message body is assumed to
  //   end at the end of the transport packet.

  inherited DoUDPRead(AData, ABinding);

  try
    Msg := TIdSipMessage.ReadMessageFrom(AData);
    try
      Msg.ReadBody(AData);

      Self.ReceiveMessageInTimerContext(Msg, ABinding);
    finally
      Msg.Free;
    end;
  except
    on E: Exception do begin
      Self.NotifyOfException(E);
    end;
  end;
end;

procedure TIdSipUdpServer.NotifyListenersOfResponse(Response: TIdSipResponse;
                                                    ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.Notifier.NotifyListenersOfResponse(Response, ReceivedFrom);
end;

procedure TIdSipUdpServer.NotifyOfException(E: Exception);
var
  Ex: TIdSipExceptionWait;
begin
  Ex := TIdSipExceptionWait.Create;
  Ex.Event := Self.DoOnException;
  Ex.ExceptionType := ExceptClass(E.ClassType);
  Ex.Reason := E.Message;
  Self.Timer.AddEvent(TriggerImmediately, Ex);
end;

procedure TIdSipUdpServer.ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                                       Binding: TIdSocketHandle);
var
  Wait: TIdSipReceiveUDPMessageWait;
begin
  Wait := TIdSipReceiveUDPMessageWait.Create;
  Wait.Notifier := Self.Notifier;
  Wait.Message  := Msg.Copy;

  Wait.ReceivedFrom := TIdSipConnectionBindings.Create;
  Wait.ReceivedFrom.LocalIP   := Binding.IP;
  Wait.ReceivedFrom.LocalPort := Binding.Port;
  Wait.ReceivedFrom.PeerIP    := Binding.PeerIP;
  Wait.ReceivedFrom.PeerPort  := Binding.PeerPort;

  Self.Timer.AddEvent(TriggerImmediately, Wait);
end;

//* TIdSipUdpServer Private methods ********************************************

procedure TIdSipUdpServer.DoOnException(Sender: TObject);
var
  FakeException: Exception;
  Wait:          TIdSipExceptionWait;
begin
  Wait := Sender as TIdSipExceptionWait;

  FakeException := Wait.ExceptionType.Create(Wait.ExceptionMsg);
  try
    Self.Notifier.NotifyListenersOfException(FakeException,
                                             Wait.Reason);
  finally
    FakeException.Free;
  end;
end;

//******************************************************************************
//* TIdSipUdpClient                                                            *
//******************************************************************************
//* TIdSipUdpClient Protected methods ******************************************

procedure TIdSipUdpClient.NotifyListenersOfResponse(Response: TIdSipResponse;
                                                    ReceivedFrom: TIdSipConnectionBindings);
begin
  inherited NotifyListenersOfResponse(Response, ReceivedFrom);

  if Response.IsFinal then Self.DoOnFinished;
end;

//* TIdSipUdpClient Private methods ********************************************

procedure TIdSipUdpClient.DoOnFinished;
begin
  if Assigned(Self.OnFinished)
    then Self.OnFinished(Self);
end;

//******************************************************************************
//* TIdSipReceiveUDPMessageWait                                                *
//******************************************************************************
//* TIdSipReceiveUDPMessageWait Public methods *********************************

destructor TIdSipReceiveUDPMessageWait.Destroy;
begin
  Self.ReceivedFrom.Free;

  inherited Destroy;
end;

procedure TIdSipReceiveUDPMessageWait.Trigger;
begin
  if Self.Message.IsRequest then
    Self.Notifier.NotifyListenersOfRequest(Self.Message as TIdSipRequest,
                                           Self.ReceivedFrom)
  else
    Self.Notifier.NotifyListenersOfResponse(Self.Message as TIdSipResponse,
                                            Self.ReceivedFrom);
end;

end.
