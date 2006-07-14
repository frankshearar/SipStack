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
  Classes, IdInterfacedObject, IdRTP, IdTimerQueue, IdSocketHandle, IdUDPServer,
  SyncObjs;

type
  TIdRTCPReadEvent = procedure(Sender: TObject;
                               APacket: TIdRTCPPacket;
                               ABinding: TIdConnection) of object;
  TIdRTPReadEvent = procedure(Sender: TObject;
                              APacket: TIdRTPPacket;
                              ABinding: TIdConnection) of object;

  // I represent a peer in an RTP session. You may use me not only to receive
  // RTP/RTCP data, but also to send data.
  //
  // Typically, you create an instance of me, and then reference the Session.
  //
  // SendPacket provides a service to the Session. Don't use it directly.
  //
  // When I receive an RTCP packet, I notify my listeners (as you might expect).
  // However, when I receive a compound RTCP packet, I present the contents of
  // that packet one at a time to my listeners. In other words, if I receive
  // a compound packet containing an SR, an RR and an SDES, then my listeners
  // will see first an SR, then an RR, then an SDES, with no apparent connection
  // between the packets other than them having arrived on the same binding. This
  // can work because the packets will all share the same SSRC.
  TIdRTPServer = class(TIdInterfacedObject,
                       IIdAbstractRTPPeer)
  private
    fOnRTCPRead:     TIdRTCPReadEvent;
    fOnRTPRead:      TIdRTPReadEvent;
    fSession:        TIdRTPSession;
    ManuallySetRTCP: Boolean;
    Peer:            TIdBaseRTPAbstractPeer; // We delegate to this to facilitate code reuse
    RTCP:            TIdUDPServer;
    RTP:             TIdUDPServer;

    function  CreateServer(const DefaultAddress: String; DefaultPort: Integer): TIdUDPServer;
    function  GetActive: Boolean;
    function  GetAddress: String;
    function  GetDefaultPort: Integer;
    function  GetOnUDPRead: TUDPReadEvent;
    function  GetProfile: TIdRTPProfile;
    function  GetRTCPPort: Integer;
    function  GetRTPPort: Integer;
    function  GetTimer: TIdTimerQueue;
    procedure NotifyListenersOfRTCP(Packet: TIdRTCPPacket;
                                    Binding: TIdConnection);
    procedure NotifyListenersOfRTP(Packet: TIdRTPPacket;
                                   Binding: TIdConnection);
    procedure SetActive(Value: Boolean);
    procedure SetAddress(Value: String);
    procedure SetDefaultPort(Value: Integer);
    procedure SetOnUDPRead(Value: TUDPReadEvent);
    procedure SetProfile(Value: TIdRTPProfile);
    procedure SetRTCPPort(Value: Integer);
    procedure SetRTPPort(Value: Integer);
    procedure SetTimer(Value: TIdTimerQueue);
  protected
    procedure DoUDPRead(Sender: TObject;
                        AData: TStream;
                        ABinding: TIdSocketHandle);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddListener(const Listener: IIdRTPListener);
    procedure RemoveListener(const Listener: IIdRTPListener);
    procedure Send(const Host: String;
                   Port: Integer;
                   const Buffer: String); // This method only exists for some tests
    procedure SendPacket(const Host: String;
                         Port: Cardinal;
                         Packet: TIdRTPBasePacket);
  published
    property Active:      Boolean          read GetActive write SetActive;
    property Address:     String           read GetAddress write SetAddress;
    property DefaultPort: Integer          read GetDefaultPort write SetDefaultPort;
    property OnRTCPRead:  TIdRTCPReadEvent read fOnRTCPRead write fOnRTCPRead;
    property OnRTPRead:   TIdRTPReadEvent  read fOnRTPRead write fOnRTPRead;
    property OnUDPRead:   TUDPReadEvent    read GetOnUDPRead write SetOnUDPRead;
    property Profile:     TIdRTPProfile    read GetProfile write SetProfile;
    property RTCPPort:    Integer          read GetRTCPPort write SetRTCPPort;
    property RTPPort:     Integer          read GetRTPPort write SetRTPPort;
    property Session:     TIdRTPSession    read fSession;
    property Timer:       TIdTimerQueue    read GetTimer write SetTimer;
  end;

implementation

//******************************************************************************
//* TIdRTPServer                                                               *
//******************************************************************************
//* TIdRTPServer Public methods ************************************************

constructor TIdRTPServer.Create;
begin
  inherited Create;

  Self.ManuallySetRTCP := false;

  Self.RTP  := Self.CreateServer('127.0.0.1', 8000);
  Self.RTCP := Self.CreateServer('127.0.0.1', Self.RTP.DefaultPort + 1);

  Self.Peer := TIdBaseRTPAbstractPeer.Create;

  Self.fSession := TIdRTPSession.Create(Self);
  Self.AddListener(Self.Session);
end;

destructor TIdRTPServer.Destroy;
begin
  Self.Session.Free;
  Self.Peer.Free;
  Self.RTCP.Free;
  Self.RTP.Free;  

  inherited Destroy;
end;

procedure TIdRTPServer.AddListener(const Listener: IIdRTPListener);
begin
  Self.Peer.AddListener(Listener);
end;

procedure TIdRTPServer.RemoveListener(const Listener: IIdRTPListener);
begin
  Self.Peer.RemoveListener(Listener);
end;

procedure TIdRTPServer.Send(const Host: String;
                            Port: Integer;
                            const Buffer: String);
begin
  // This method only exists for some tests. Don't use it outside of tests.

  Self.RTP.Send(Host, Port, Buffer);
end;

procedure TIdRTPServer.SendPacket(const Host: String;
                                  Port: Cardinal;
                                  Packet: TIdRTPBasePacket);
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Packet.PrintOn(S);

    if Packet.IsRTCP then
      Self.RTCP.Send(Host, Port, S.DataString)
    else
      Self.RTP.Send(Host, Port, S.DataString);
  finally
    S.Free;
  end;
end;

//* TIdRTPServer Protected methods *********************************************

procedure TIdRTPServer.DoUDPRead(Sender: TObject;
                                 AData: TStream;
                                 ABinding: TIdSocketHandle);
var
  Binding: TIdConnection;
  Pkt:     TIdRTPBasePacket;
begin
  AData.Seek(0, soFromBeginning);

  Binding.LocalIP   := ABinding.IP;
  Binding.LocalPort := ABinding.Port;
  Binding.PeerIP    := ABinding.PeerIP;
  Binding.PeerPort  := ABinding.PeerPort;

  Pkt := Self.Profile.CreatePacket(AData);
  try
    Pkt.ReadFrom(AData);

    if Pkt.IsRTP then
      Self.NotifyListenersOfRTP(Pkt as TIdRTPPacket, Binding)
    else
      Self.NotifyListenersOfRTCP(Pkt as TIdRTCPPacket, Binding);
  finally
    Pkt.Free;
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

function TIdRTPServer.GetDefaultPort: Integer;
begin
  Result := Self.RTP.DefaultPort;
end;

function TIdRTPServer.GetOnUDPRead: TUDPReadEvent;
begin
  Result := Self.RTP.OnUDPRead;
end;

function TIdRTPServer.GetProfile: TIdRTPProfile;
begin
  Result := Self.Peer.Profile;
end;

function TIdRTPServer.GetRTCPPort: Integer;
begin
  Result := Self.RTCP.DefaultPort;
end;

function TIdRTPServer.GetRTPPort: Integer;
begin
  Result := Self.RTP.DefaultPort;
end;

function TIdRTPServer.GetTimer: TIdTimerQueue;
begin
  Result := Self.Session.Timer;
end;

procedure TIdRTPServer.NotifyListenersOfRTCP(Packet: TIdRTCPPacket;
                                             Binding: TIdConnection);
var
  Compound: TIdCompoundRTCPPacket;
  I:        Integer;
begin
  if Packet is TIdCompoundRTCPPacket then begin
    Compound := Packet as TIdCompoundRTCPPacket;

    for I := 0 to Compound.PacketCount - 1 do begin
      Self.Peer.NotifyListenersOfRTCP(Compound.PacketAt(I), Binding);

      if Assigned(Self.OnRTCPRead) then
        Self.OnRTCPRead(Self, Compound.PacketAt(I), Binding);
    end;
  end
  else begin
    Self.Peer.NotifyListenersOfRTCP(Packet, Binding);

    if Assigned(Self.OnRTCPRead) then
      Self.OnRTCPRead(Self, Packet, Binding);
  end;
end;

procedure TIdRTPServer.NotifyListenersOfRTP(Packet: TIdRTPPacket;
                                            Binding: TIdConnection);
begin
  Self.Peer.NotifyListenersOfRTP(Packet, Binding);

  if Assigned(Self.OnRTPRead) then
    Self.OnRTPRead(Self, Packet, Binding);
end;

procedure TIdRTPServer.SetActive(Value: Boolean);
begin
  Self.RTCP.Active := Value;
  Self.RTP.Active  := Value;
end;

procedure TIdRTPServer.SetAddress(Value: String);
begin
  Self.RTP.Bindings[0].IP  := Value;
  Self.RTCP.Bindings[0].IP := Value;
end;

procedure TIdRTPServer.SetDefaultPort(Value: Integer);
begin
  Self.RTP.DefaultPort := Value;
end;

procedure TIdRTPServer.SetOnUDPRead(Value: TUDPReadEvent);
begin
  Self.RTP.OnUDPRead := Value;
end;

procedure TIdRTPServer.SetProfile(Value: TIdRTPProfile);
begin
  Self.Session.Profile := Value;
  Self.Peer.Profile    := Self.Session.Profile;
end;

procedure TIdRTPServer.SetRTCPPort(Value: Integer);
begin
  Self.RTCP.DefaultPort      := Value;
  Self.RTCP.Bindings[0].Port := Value;

  Self.ManuallySetRTCP := true;
end;

procedure TIdRTPServer.SetRTPPort(Value: Integer);
begin
  Self.RTP.DefaultPort      := Value;
  Self.RTP.Bindings[0].Port := Value;

  if not Self.ManuallySetRTCP then
    Self.RTCPPort := Value + 1;
end;

procedure TIdRTPServer.SetTimer(Value: TIdTimerQueue);
begin
  Self.Session.Timer := Value;
end;

end.
