unit IdRTPServer;

interface

uses
  Classes, IdRTP, IdSocketHandle, IdUDPServer, SyncObjs;

type
  TIdRTCPReadEvent = procedure(Sender: TObject;
                               APacket: TIdRTCPPacket;
                               ABinding: TIdSocketHandle) of object;
  TIdRTPReadEvent = procedure(Sender: TObject;
                              APacket: TIdRTPPacket;
                              ABinding: TIdSocketHandle) of object;

  // While I look like a Server, I really represent a peer in an RTP session.
  // You may use me not only to receive RTP/RTCP data, but also to send data.
  //
  // Typically, you create an instance of me, and then call JoinSession with
  // the session host/port you want to join. After this, you use the Session
  // property to send data/control packets, or leave the session.
  //
  // SendPacket provides a service to the Session. Don't use it directly.
  TIdRTPServer = class(TIdUDPServer,
                       IIdAbstractRTPPeer)
  private
    fControlPort: Cardinal;
    fOnRTCPRead:  TIdRTCPReadEvent;
    fOnRTPRead:   TIdRTPReadEvent;
    fSession:     TIdRTPSession;
    Peer:         TIdBaseRTPAbstractPeer;

    function  GetProfile: TIdRTPProfile;
    procedure NotifyListenersOfRTCP(Packet: TIdRTCPPacket;
                                    Binding: TIdSocketHandle);
    procedure NotifyListenersOfRTP(Packet: TIdRTPPacket;
                                   Binding: TIdSocketHandle);
    procedure SetProfile(const Value: TIdRTPProfile);
  protected
    procedure DoUDPRead(AData: TStream;
                        ABinding: TIdSocketHandle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure AddListener(const Listener: IIdRTPListener);
    procedure RemoveListener(const Listener: IIdRTPListener);
    procedure SendPacket(Host: String;
                         Port: Cardinal;
                         Packet: TIdRTPBasePacket);

  published
    property ControlPort: Cardinal         read fControlPort write fControlPort;
    property OnRTCPRead:  TIdRTCPReadEvent read fOnRTCPRead write fOnRTCPRead;
    property OnRTPRead:   TIdRTPReadEvent  read fOnRTPRead write fOnRTPRead;
    property Profile:     TIdRTPProfile    read GetProfile write SetProfile;
    property Session:     TIdRTPSession    read fSession;
  end;

implementation

//******************************************************************************
//* TIdRTPServer                                                               *
//******************************************************************************
//* TIdRTPServer Public methods ************************************************

constructor TIdRTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.Peer := TIdBaseRTPAbstractPeer.Create;

  Self.fSession := TIdRTPSession.Create(Self);
  Self.AddListener(Self.Session);

  Self.ThreadedEvent := true;

  Self.DefaultPort := 8000;
  Self.ControlPort := 8001;
end;

destructor TIdRTPServer.Destroy;
begin
  Self.Session.Free;
  Self.Peer.Free;

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

procedure TIdRTPServer.SendPacket(Host: String;
                                  Port: Cardinal;
                                  Packet: TIdRTPBasePacket);
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Packet.PrintOn(S);
    Self.Send(Host, Port, S.DataString);
  finally
    S.Free;
  end;
end;

//* TIdRTPServer Protected methods *********************************************

procedure TIdRTPServer.DoUDPRead(AData: TStream;
                                 ABinding: TIdSocketHandle);
var
  Pkt: TIdRTPBasePacket;
begin
  inherited DoUDPRead(AData, ABinding);
  AData.Seek(0, soFromBeginning);

  Pkt := Self.Profile.CreatePacket(AData);
  try
    Pkt.ReadFrom(AData);

    if Pkt.IsRTP then
      Self.NotifyListenersOfRTP(Pkt as TIdRTPPacket, ABinding)
    else
      Self.NotifyListenersOfRTCP(Pkt as TIdRTCPPacket, ABinding);
  finally
    Pkt.Free;
  end;
end;

//* TIdRTPServer Private methods ***********************************************

function TIdRTPServer.GetProfile: TIdRTPProfile;
begin
  Result := Self.Peer.Profile;
end;

procedure TIdRTPServer.NotifyListenersOfRTCP(Packet: TIdRTCPPacket;
                                             Binding: TIdSocketHandle);
begin
  Self.Peer.NotifyListenersOfRTCP(Packet, Binding);

  if Assigned(Self.OnRTCPRead) then
    Self.OnRTCPRead(Self, Packet, Binding);
end;

procedure TIdRTPServer.NotifyListenersOfRTP(Packet: TIdRTPPacket;
                                            Binding: TIdSocketHandle);
begin
  Self.Peer.NotifyListenersOfRTP(Packet, Binding);

  if Assigned(Self.OnRTPRead) then
    Self.OnRTPRead(Self, Packet, Binding);
end;

procedure TIdRTPServer.SetProfile(const Value: TIdRTPProfile);
begin
  Self.Session.Profile := Value;
  Self.Peer.Profile := Self.Session.Profile;
end;

end.
