unit IdRTPServer;

interface

uses
  Classes, IdRTP, IdSocketHandle, IdUDPServer;

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
  TIdRTPServer = class(TIdAbstractRTPPeer)
  private
    FCanonicalName: String;
    FControlPort:   Cardinal;
    FSession:       TIdRTPSession;
    FOnRTCPRead:    TIdRTCPReadEvent;
    FOnRTPRead:     TIdRTPReadEvent;
    FProfile:       TIdRTPProfile;

    procedure DoOnRTCPRead(APacket: TIdRTCPPacket;
                           ABinding: TIdSocketHandle);
    procedure DoOnRTPRead(APacket: TIdRTPPacket;
                          ABinding: TIdSocketHandle);
  protected
    procedure DoUDPRead(AData: TStream;
                        ABinding: TIdSocketHandle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  JoinSession(Host: String; Port: Cardinal): TIdRTPSession;
    procedure SendPacket(Host: String;
                         Port: Cardinal;
                         Packet: TIdRTPBasePacket); override;

    property Profile: TIdRTPProfile read FProfile;
  published
    property CanonicalName: String           read fCanonicalName write fCanonicalName;
    property ControlPort:   Cardinal         read FControlPort write FControlPort;
    property Session:       TIdRTPSession    read FSession;
    property OnRTCPRead:    TIdRTCPReadEvent read FOnRTCPRead write FOnRTCPRead;
    property OnRTPRead:     TIdRTPReadEvent  read FOnRTPRead write FOnRTPRead;
  end;

implementation

//******************************************************************************
//* TIdRTPServer                                                               *
//******************************************************************************
//* TIdRTPServer Public methods ************************************************

constructor TIdRTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.FProfile := TIdAudioVisualProfile.Create;
  Self.FSession := TIdRTPSession.Create(Self, Self.Profile);

  Self.ThreadedEvent := true;

  Self.DefaultPort := 8000;
  Self.ControlPort := 8001;
end;

destructor TIdRTPServer.Destroy;
begin
  Self.Profile.Free;
  Self.Session.Free;

  inherited Destroy;
end;

function TIdRTPServer.JoinSession(Host: String; Port: Cardinal): TIdRTPSession;
var
  Pkt: TIdRTCPSourceDescription;
  S:   TStringStream;
begin
  Pkt := TIdRTCPSourceDescription.Create;
  try
    Pkt.AddChunk.AddCanonicalName(Self.CanonicalName);
    Pkt.PrepareForTransmission(Self.Session);

    S := TStringStream.Create('');
    try
      Pkt.PrintOn(S);
      Self.Send(Host, Port, S.DataString);
    finally
      S.Free;
    end;
  finally
    Pkt.Free;
  end;

  Result := Self.Session;
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
      Self.DoOnRTPRead(Pkt as TIdRTPPacket, ABinding)
    else
      Self.DoOnRTCPRead(Pkt as TIdRTCPPacket, ABinding);
  finally
    Pkt.Free;
  end;
end;

//* TIdRTPServer Private methods ***********************************************

procedure TIdRTPServer.DoOnRTCPRead(APacket: TIdRTCPPacket;
                                   ABinding: TIdSocketHandle);
begin
  Self.Session.ReceiveControl(APacket, ABinding);

  if Assigned(Self.OnRTCPRead) then
    Self.OnRTCPRead(Self, APacket, ABinding);
end;

procedure TIdRTPServer.DoOnRTPRead(APacket: TIdRTPPacket;
                                   ABinding: TIdSocketHandle);
begin
  Self.Session.ReceiveData(APacket, ABinding);

  if Assigned(Self.OnRTPRead) then
    Self.OnRTPRead(Self, APacket, ABinding);
end;

end.
