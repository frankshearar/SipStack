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
  TIdRTPServer = class(TIdUDPServer,
                       IIdAbstractRTPPeer)
  private
    fControlPort: Cardinal;
    fOnRTCPRead:  TIdRTCPReadEvent;
    fOnRTPRead:   TIdRTPReadEvent;
    fProfile:     TIdRTPProfile;
    fSession:     TIdRTPSession;

    procedure DoOnRTCPRead(APacket: TIdRTCPPacket;
                           ABinding: TIdSocketHandle);
    procedure DoOnRTPRead(APacket: TIdRTPPacket;
                          ABinding: TIdSocketHandle);
    procedure SetProfile(const Value: TIdRTPProfile);
  protected
    procedure DoUDPRead(AData: TStream;
                        ABinding: TIdSocketHandle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure SendPacket(Host: String;
                         Port: Cardinal;
                         Packet: TIdRTPBasePacket); 

  published
    property ControlPort:   Cardinal         read fControlPort write fControlPort;
    property Profile:       TIdRTPProfile    read fProfile write SetProfile;
    property OnRTCPRead:    TIdRTCPReadEvent read fOnRTCPRead write fOnRTCPRead;
    property OnRTPRead:     TIdRTPReadEvent  read fOnRTPRead write fOnRTPRead;
    property Session:       TIdRTPSession    read fSession;
  end;

implementation

//******************************************************************************
//* TIdRTPServer                                                               *
//******************************************************************************
//* TIdRTPServer Public methods ************************************************

constructor TIdRTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.ThreadedEvent := true;

  Self.DefaultPort := 8000;
  Self.ControlPort := 8001;
end;

destructor TIdRTPServer.Destroy;
begin
  Self.Session.Free;

  inherited Destroy;
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

procedure TIdRTPServer.SetProfile(const Value: TIdRTPProfile);
begin
  Self.fProfile := Value;
  Self.fSession := TIdRTPSession.Create(Self, Value);
end;

end.
