{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdRTPServer;

interface

uses
  Classes, IdConnectionBindings, IdInterfacedObject, IdRTP, IdTimerQueue,
  IdSocketHandle, IdUDPServer, SyncObjs;

type
  TIdRTCPReadEvent = procedure(Sender: TObject;
                               APacket: TIdRTCPPacket;
                               ABinding: TIdConnectionBindings) of object;
  TIdRTPReadEvent = procedure(Sender: TObject;
                              APacket: TIdRTPPacket;
                              ABinding: TIdConnectionBindings) of object;

  // I represent a peer in an RTP session. You may use me not only to receive
  // RTP/RTCP data, but also to send data.
  //
  // Typically, you create an instance of me, and then reference the Session.
  //
  // SendPacket provides a service to the Session. Don't use it directly.
  //
  // When I receive an RTCP packet, I do two things: first, I schedule a Wait
  // object so that higher layers of the stack run only in the context of my
  // Timer's thread. Second, I notify my listeners (as you might expect). THIS
  // NOTIFICATION HAPPENS IN THE CONTEXT OF MY LISTENER THREAD. USE IT FOR
  // TESTING, AND NOT FOR PRODUCTION CODE.
  // When I receive a compound RTCP packet, I present the contents of that
  // packet one at a time to my listeners. In other words, if I receive a
  // compound packet containing an SR, an RR and an SDES, then my listeners will
  // see first an SR, then an RR, then an SDES, with no apparent connection
  // between the packets other than them having arrived on the same binding. This
  // can work because the packets will all share the same SSRC. (Note that since
  // these Wait objects are scheduled to execute at the same time, and are
  // scheduled in a very short space of time, you cannot assume a particular
  // ordering of these RTCP Waits.)
  //
  // If NotifyListeners is true, we notify the Listeners of this class of
  // received packets as per usual. This is only useful as a debugging aid,
  // because usually you want to hand off the received packets to a TimerQueue.
  // (You want this because it's threadsafe - all the packet processing then
  // takes place in the context of the TimerQueue and not in the context of the
  // thread listening on the RTP or RTCP port.) In the typical production setup
  // then, if NotifyListeners is true you will receive every packet twice - once
  // because of the NotificationList, and once because of the TIdWait event.
  TIdRTPServer = class(TIdBaseRTPAbstractPeer)
  private
    fNotifyListeners: Boolean;
    ManuallySetRTCP:  Boolean;
    RTCP:             TIdUDPServer;
    RTP:              TIdUDPServer;

    function  CreateServer(const DefaultAddress: String; DefaultPort: Integer): TIdUDPServer;
    procedure ReceiveInTimerContext(Packet: TIdRTPBasePacket;
                                    Binding: TIdConnectionBindings);
    procedure ReceiveRTCPInTimerContext(Packet: TIdRTCPPacket;
                                       Binding: TIdConnectionBindings);
    procedure SetPort(Server: TIdUdpServer; Port: TPortNum);
  protected
    procedure DoUDPRead(Sender: TObject;
                        AData: TStream;
                        ABinding: TIdSocketHandle);
    function  GetActive: Boolean; override;
    function  GetAddress: String; override;
    function  GetDefaultPort: TPortNum; override;
    function  GetRTCPPort: TPortNum; override;
    function  GetRTPPort: TPortNum; override;
    procedure SetActive(Value: Boolean); override;
    procedure SetAddress(Value: String); override;
    procedure SetDefaultPort(Value: TPortNum); override;
    procedure SetRTCPPort(Value: TPortNum); override;
    procedure SetRTPPort(Value: TPortNum); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure ReceivePacket(Packet: TIdRTPBasePacket;
                            Binding: TIdConnectionBindings); override;
    procedure Send(const Host: String;
                   Port: Integer;
                   const Buffer: String); // This method only exists for some tests
    procedure SendPacket(const Host: String;
                         Port: TPortNum;
                         Packet: TIdRTPBasePacket); override;
  published
    property Active:          Boolean  read GetActive write SetActive;
    property Address:         String   read GetAddress write SetAddress;
    property DefaultPort:     TPortNum read GetDefaultPort write SetDefaultPort;
    property NotifyListeners: Boolean  read fNotifyListeners write fNotifyListeners;
    property RTCPPort:        TPortNum read GetRTCPPort write SetRTCPPort;
    property RTPPort:         TPortNum read GetRTPPort write SetRTPPort;
  end;

implementation

uses
  IdException, IdIndyUtils;

//******************************************************************************
//* TIdRTPServer                                                               *
//******************************************************************************
//* TIdRTPServer Public methods ************************************************

constructor TIdRTPServer.Create;
begin
  inherited Create;

  Self.ManuallySetRTCP := false;
  Self.NotifyListeners := false;

  Self.RTP  := Self.CreateServer('127.0.0.1', 8000);
  Self.RTCP := Self.CreateServer('127.0.0.1', Self.RTP.DefaultPort + 1);
end;

destructor TIdRTPServer.Destroy;
begin
  Self.RTCP.Free;
  Self.RTP.Free;

  inherited Destroy;
end;

procedure TIdRTPServer.ReceivePacket(Packet: TIdRTPBasePacket;
                                     Binding: TIdConnectionBindings);
begin
  if Self.NotifyListeners then
    inherited ReceivePacket(Packet, Binding);

  if Packet.IsRTP then
    Self.ReceiveInTimerContext(Packet, Binding)
  else
    Self.ReceiveRTCPInTimerContext(Packet as TIdRTCPPacket, Binding);
end;

procedure TIdRTPServer.Send(const Host: String;
                            Port: Integer;
                            const Buffer: String);
begin
  // This method only exists for some tests. Don't use it outside of tests.

  Self.RTP.Send(Host, Port, Buffer);
end;

procedure TIdRTPServer.SendPacket(const Host: String;
                                  Port: TPortNum;
                                  Packet: TIdRTPBasePacket);
var
  Binding: TIdConnectionBindings;
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Packet.PrintOn(S);

    Binding := TIdConnectionBindings.Create;
    try
      Binding.LocalIP   := Self.Address;
      Binding.PeerIP    := Host;
      Binding.PeerPort  := Port;
      Binding.Transport := UdpTransport;

      if Packet.IsRTCP then begin
        Binding.LocalPort := Self.RTCPPort;
        Self.NotifyListenersOfSentRTCP(Packet as TIdRTCPPacket, Binding);
        Self.RTCP.Send(Host, Integer(Port), S.DataString)
      end
      else begin
        Binding.LocalPort := Self.RTPPort;
        Self.NotifyListenersOfSentRTP(Packet as TIdRTPPacket, Binding);
        Self.RTP.Send(Host, Integer(Port), S.DataString);
      end;
    finally
      Binding.Free;
    end;
  finally
    S.Free;
  end;
end;

//* TIdRTPServer Protected methods *********************************************

procedure TIdRTPServer.DoUDPRead(Sender: TObject;
                                 AData: TStream;
                                 ABinding: TIdSocketHandle);
var
  Binding: TIdConnectionBindings;
  Pkt:     TIdRTPBasePacket;
begin
  AData.Seek(0, soFromBeginning);

  Binding := TIdConnectionBindings.Create;
  try
    Binding.LocalIP   := ABinding.IP;
    Binding.LocalPort := ABinding.Port;
    Binding.PeerIP    := ABinding.PeerIP;
    Binding.PeerPort  := ABinding.PeerPort;
    Binding.Transport := UdpTransport;

    Pkt := Self.RemoteProfile.CreatePacket(AData);
    try
      Pkt.ReadFrom(AData);

      Self.ReceivePacket(Pkt, Binding);
    finally
      Pkt.Free;
    end;
  finally
    Binding.Free;
  end;
end;

//* TIdRTPServer Private methods ***********************************************

function TIdRTPServer.CreateServer(const DefaultAddress: String; DefaultPort: Integer): TIdUDPServer;
var
  Binding: TIdSocketHandle;
begin
  Result  := TIdUDPServer.Create(nil);
  Result.DefaultPort   := DefaultPort;
  Result.OnUDPRead     := Self.DoUDPRead;
  Result.ThreadedEvent := true;

  Binding := Result.Bindings.Add;
  Binding.IP := DefaultAddress;
  Binding.Port := DefaultPort;
end;

function TIdRTPServer.GetActive: Boolean;
begin
  Result := Self.RTP.Active;
end;

function TIdRTPServer.GetAddress: String;
begin
  Result := Self.RTP.Bindings[0].IP;
end;

function TIdRTPServer.GetDefaultPort: TPortNum;
begin
  Result := Self.RTP.DefaultPort;
end;

function TIdRTPServer.GetRTCPPort: TPortNum;
begin
  Result := IntToPortNum(Self.RTCP.DefaultPort);
end;

function TIdRTPServer.GetRTPPort: TPortNum;
begin
  Result := IntToPortNum(Self.RTP.DefaultPort);
end;

procedure TIdRTPServer.ReceiveInTimerContext(Packet: TIdRTPBasePacket;
                                             Binding: TIdConnectionBindings);
var
  Wait: TIdRTPReceivePacketWait;
begin
  Wait := TIdRTPReceivePacketWait.Create;
  Wait.Packet := Packet.Copy;
  Wait.SessionID := Self.Session.ID;

  Wait.ReceivedFrom := Binding;

  Self.Timer.AddEvent(TriggerImmediately, Wait);
end;

procedure TIdRTPServer.ReceiveRTCPInTimerContext(Packet: TIdRTCPPacket;
                                                 Binding: TIdConnectionBindings);
var
  Compound: TIdCompoundRTCPPacket;
  I:        Integer;
begin
  if Packet is TIdCompoundRTCPPacket then begin
    Compound := Packet as TIdCompoundRTCPPacket;

    for I := 0 to Compound.PacketCount - 1 do begin
      Self.ReceiveInTimerContext(Compound.PacketAt(I), Binding);
    end;
  end
  else begin
    Self.ReceiveInTimerContext(Packet, Binding);
  end;
end;

procedure TIdRTPServer.SetActive(Value: Boolean);
begin
  try
    Self.RTCP.Active := Value;
  except
    on E: EIdException do
      RaiseSocketError(E, Self.RTCP.Bindings);
  end;

  try
    Self.RTP.Active := Value;
  except
    on E: EIdException do
      RaiseSocketError(E, Self.RTP.Bindings);
  end;
end;

procedure TIdRTPServer.SetAddress(Value: String);
begin
  Self.RTP.Bindings[0].IP  := Value;
  Self.RTCP.Bindings[0].IP := Value;
end;

procedure TIdRTPServer.SetDefaultPort(Value: TPortNum);
begin
  Self.RTP.DefaultPort := Value;
end;

procedure TIdRTPServer.SetPort(Server: TIdUdpServer; Port: TPortNum);
var
  ConvertedCardinal: Integer;
begin
  ConvertedCardinal := Port and $7fffffff;

  Server.DefaultPort      := ConvertedCardinal;
  Server.Bindings[0].Port := ConvertedCardinal;
end;

procedure TIdRTPServer.SetRTCPPort(Value: TPortNum);
begin
  Self.SetPort(Self.RTCP, Value);

  Self.ManuallySetRTCP := true;
end;

procedure TIdRTPServer.SetRTPPort(Value: TPortNum);
begin
  Self.SetPort(Self.RTP, Value);

  if not Self.ManuallySetRTCP then
    Self.RTCPPort := Value + 1;
end;

end.
