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
  Classes, IdConnectionBindings, IdRegisteredObject, IdSipLocation,
  IdSipMessage, IdSipTransport, IdSocketHandle, IdTimerQueue, IdUDPServer,
  SysUtils;

type
  TIdSipUdpServer = class;

  // I implement the User Datagram Protocol (RFC 768) connections for the SIP
  // stack.
  TIdSipUDPTransport = class(TIdSipTransport)
  private
    Transport: TIdSipUdpServer;

    function  FindActualBinding(Dest: TIdConnectionBindings): TIdSocketHandle;
  protected
    procedure DestroyServer; override;
    procedure InstantiateServer; override;
    procedure SendMessage(M: TIdSipMessage;
                          Dest: TIdConnectionBindings); override;
    procedure SetTimer(Value: TIdTimerQueue); override;
  public
    class function GetTransportType: String; override;
    class function SrvPrefix: String; override;

    function  IsReliable: Boolean; override;
    function  IsRunning: Boolean; override;
    procedure ReceiveRequest(Request: TIdSipRequest;
                             ReceivedFrom: TIdConnectionBindings); override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TIdSipUdpServer = class(TIdUDPServer)
  private
    fTimer:         TIdTimerQueue;
    fTransportID:   TRegisteredObjectID;
    fTransportType: String;
  protected
    procedure DoUDPRead(AData: TStream; ABinding: TIdSocketHandle); override;
    procedure NotifyOfException(E: Exception);
    procedure ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                           Binding: TIdSocketHandle); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    property Timer:         TIdTimerQueue       read fTimer write fTimer;
    property TransportID:   TRegisteredObjectID read fTransportID write fTransportID;
    property TransportType: String              read fTransportType write fTransportType;
  end;

  TIdSipUdpClient = class(TIdSipUdpServer)
  private
    fOnFinished: TNotifyEvent;

    procedure DoOnFinished;
  protected
    procedure ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                           Binding: TIdSocketHandle); override;
  public
    property OnFinished: TNotifyEvent read fOnFinished write fOnFinished;
  end;

implementation

uses
  IdException, IdIndyUtils, IdSipDns;

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

function TIdSipUDPTransport.IsReliable: Boolean;
begin
  Result := false;
end;

function TIdSipUDPTransport.IsRunning: Boolean;
begin
  Result := Self.Transport.Active;
end;

procedure TIdSipUDPTransport.ReceiveRequest(Request: TIdSipRequest;
                                            ReceivedFrom: TIdConnectionBindings);
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
var
  B: TIdSocketHandle;
  I: Integer;
begin
  inherited Start;

  if Self.IsRunning then Exit;

  try
    Self.Transport.Bindings.Clear;
    for I := 0 to Self.Bindings.Count - 1 do begin
      B := Self.Transport.Bindings.Add;
      B.IP   := Self.Bindings[I].IPAddress;
      B.Port := Self.Bindings[I].Port;
    end;

    Self.Transport.Active := true;
  except
    on E: EIdException do
      RaiseSocketError(E, Self.Transport.Bindings);
  end;
end;

procedure TIdSipUDPTransport.Stop;
begin
  Self.Transport.Active := false;
end;

//* TIdSipUDPTransport Private methods *****************************************

function TIdSipUDPTransport.FindActualBinding(Dest: TIdConnectionBindings): TIdSocketHandle;
var
  DefaultPort:  TPortNum;
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

  for I := 0 to Self.Transport.Bindings.Count - 1 do begin
    // Try use any address bound on LocalAddress.IPAddress
    if (Self.Transport.Bindings[I].IP = LocalAddress) then begin
      Result := Self.Transport.Bindings[I];
    end;

    // But if something's bound on LocalAddress.IPAddress AND uses the default
    // port for this transport, use that instead.
    if Assigned(Result) then begin
      if    (Self.Transport.Bindings[I].IP = LocalAddress)
        and (Cardinal(Self.Transport.Bindings[I].Port) = DefaultPort) then begin
        Result := Self.Transport.Bindings[I];
        Break;
      end;
    end;
  end;

  // Nothing appropriate found? Just use any old socket, and pray.
  if (Result = nil) then
    Result := Self.Transport.Bindings[0];

  Self.Assert(Result <> nil, 'No binding found for the destination ' + Dest.AsString);
end;

//* TIdSipUDPTransport Protected methods ***************************************

procedure TIdSipUDPTransport.DestroyServer;
begin
  Self.Transport.Free;
end;

procedure TIdSipUDPTransport.InstantiateServer;
begin
  Self.Transport := TIdSipUdpServer.Create(nil);
  Self.Transport.ThreadedEvent := true;
  Self.Transport.TransportID := Self.ID;
  Self.Transport.TransportType := Self.GetTransportType;
end;

procedure TIdSipUDPTransport.SendMessage(M: TIdSipMessage;
                                         Dest: TIdConnectionBindings);
var
  B: TIdSocketHandle;
  S: String;
begin
  B := Self.FindActualBinding(Dest);

  if Assigned(B) then begin
    Dest.LocalIP   := B.IP;
    Dest.LocalPort := B.Port;

    if M.LastHop.IsUnset then
      M.RewriteLocationHeaders(Dest);

    S := M.AsString;
    B.SendTo(Dest.PeerIP, Dest.PeerPort, S[1], Length(S));

    Self.NotifyOfSentMessage(M, Dest);
  end;
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
  Self.ThreadedEvent := true;
  Self.TransportType := UdpTransport; // A sensible default.
end;

//* TIdSipUdpServer Protected methods ******************************************

procedure TIdSipUdpServer.DoUDPRead(AData: TStream; ABinding: TIdSocketHandle);
const
  MaxUdpPacketSize = 64*1024;
var
  Msg:             TIdSipMessage;
  NoContentLength: Boolean;
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
      // Msg might not have a Content-Length header. Cf. RFC 3261, section 18.3:
      //
      //   If the message [using a message-oriented transport] has no
      //   Content-Length header field, the message body is assumed to end at
      //   the end of the transport packet.

      // If Msg doesn't have a Content-Length header, we have to calculate the
      // message body length ourselves, and set Msg.ContentLength so that
      // Msg.ReadBody() knows how much data to read. The only data we have is
      // the unparsed message (in AData and in Msg.RawMessage), so the simplest,
      // reliable, way of reading the body is telling ReadBody "read as much as
      // you can".
      NoContentLength := not Msg.HasHeader(ContentLengthHeaderFull);

      if NoContentLength then
        Msg.ContentLength := MaxUdpPacketSize;

      Msg.ReadBody(AData);

      if NoContentLength then
        Msg.ContentLength := Length(Msg.Body);

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

procedure TIdSipUdpServer.NotifyOfException(E: Exception);
var
  Ex: TIdSipMessageExceptionWait;
begin
  Ex := TIdSipMessageExceptionWait.Create;
  Ex.ExceptionMessage := E.Message;
  Ex.Reason           := E.Message;
  Ex.TransportID      := Self.TransportID;

  Self.Timer.AddEvent(TriggerImmediately, Ex);
end;

procedure TIdSipUdpServer.ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                                       Binding: TIdSocketHandle);
var
  Wait: TIdSipReceiveMessageWait;
begin
  Wait := TIdSipReceiveMessageWait.Create;
  Wait.Message   := Msg.Copy;

  Wait.ReceivedFrom.LocalIP   := Binding.IP;
  Wait.ReceivedFrom.LocalPort := IntToPortNum(Binding.Port);
  Wait.ReceivedFrom.PeerIP    := Binding.PeerIP;
  Wait.ReceivedFrom.PeerPort  := IntToPortNum(Binding.PeerPort);
  Wait.ReceivedFrom.Transport := Self.TransportType;
  Wait.TransportID            := Self.TransportID;

  Self.Timer.AddEvent(TriggerImmediately, Wait);
end;

//******************************************************************************
//* TIdSipUdpClient                                                            *
//******************************************************************************
//* TIdSipUdpClient Protected methods ******************************************

procedure TIdSipUdpClient.ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                                       Binding: TIdSocketHandle);
begin
  inherited ReceiveMessageInTimerContext(Msg, Binding);

  if Msg.IsResponse and (Msg as TIdSipResponse).IsFinal then Self.DoOnFinished;
end;

//* TIdSipUdpClient Private methods ********************************************

procedure TIdSipUdpClient.DoOnFinished;
begin
  if Assigned(Self.OnFinished)
    then Self.OnFinished(Self);
end;

end.
