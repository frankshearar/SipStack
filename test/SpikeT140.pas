{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit SpikeT140;

interface

uses
  Classes, Controls, ExtCtrls, Forms, IdRTP, IdRTPServer,
  IdSocketHandle, IdTimerQueue, IdUDPClient, IdUDPServer, StdCtrls, SyncObjs;

type
  TIdSpikeT140 = class(TForm,
                       IIdRTPListener)
    Sent: TMemo;
    Splitter1: TSplitter;
    Received: TMemo;
    Panel1: TPanel;
    Join: TButton;
    Label1: TLabel;
    ByteCount: TLabel;
    RemoteHostAndPort: TEdit;
    Label2: TLabel;
    PacketCount: TLabel;
    Timer1: TTimer;
    Leave: TButton;
    procedure LeaveClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SentKeyPress(Sender: TObject; var Key: Char);
    procedure JoinClick(Sender: TObject);
  private
    ByteCounter:   Cardinal;
    Client:        TIdRTPServer;
    Lock:          TCriticalSection;
    PacketCounter: Cardinal;
    Profile:       TIdRTPProfile;
    SendBuffer:    String;
    Server:        TIdRTPServer;
    Session:       TIdRTPSession;
    T140:          TIdRTPPayload;
    T140PT:        TIdRTPPayloadType;
    Timer:         TIdThreadedTimerQueue;

    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdConnection);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdConnection);
    procedure Reset;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  IdSpikeT140: TIdSpikeT140;

implementation

{$R *.dfm}

uses
  IdSimpleParser, IdSystem, IdUnicode, SysUtils;

//******************************************************************************
//* TIdSpikeT140                                                               *
//******************************************************************************
//* TIdSpikeT140 Public methods ************************************************

constructor TIdSpikeT140.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.Timer   := TIdThreadedTimerQueue.Create(false);
  Self.Profile := TIdAudioVisualProfile.Create;
  Self.Server := TIdRTPServer.Create;
  Self.Server.AddListener(Self);
  Self.Server.RTPPort       := 8002;
  Self.Server.RTCPPort      := 8003;
  Self.Server.LocalProfile  := Self.Profile;
  Self.Server.RemoteProfile := Self.Profile;
  Self.Server.Timer         := Self.Timer;
  Self.Server.Active        := true;

  Self.Session := Self.Server.Session;

  Self.Client := TIdRTPServer.Create;
  Self.Client.AddListener(Self);
  Self.Client.RTPPort       := Self.Server.RTPPort + 2;
  Self.Client.RTCPPort      := Self.Server.RTCPPort + 2;
  Self.Client.LocalProfile  := Self.Profile;
  Self.Client.RemoteProfile := Self.Profile;
  Self.Client.Timer         := Self.Timer;
  Self.Client.Active        := true;

  Self.T140   := TIdRTPT140Payload.Create;
  Self.T140PT := Self.Server.LocalProfile.FirstFreePayloadType;
  Self.Server.LocalProfile.AddEncoding(Self.T140, Self.T140PT);
  Self.Client.LocalProfile.AddEncoding(Self.T140, Self.T140PT);

  Self.Lock := TCriticalSection.Create;
  Self.SendBuffer := '';

  Self.Session.AddReceiver(Self.Client.Address,
                           Self.Client.RTPPort);
  Self.Reset;

  Self.RemoteHostAndPort.Text := Self.Client.Address + ':' + IntToStr(Self.Client.RTPPort);
end;

destructor TIdSpikeT140.Destroy;
begin
  Self.Timer1.Enabled := false;
  Self.Lock.Free;

  Self.Client.Active := false;
  Self.Server.Active := false;

  Self.T140.Free;
  Self.Client.Free;
  Self.Server.Free;
  Self.Profile.Free;
  Self.Timer.Terminate;

  inherited Destroy;
end;

//* TIdSpikeT140 Private methods ***********************************************

procedure TIdSpikeT140.OnRTCP(Packet: TIdRTCPPacket;
                              Binding: TIdConnection);
begin
end;

procedure TIdSpikeT140.OnRTP(Packet: TIdRTPPacket;
                             Binding: TIdConnection);
begin
  Self.Lock.Acquire;
  try
    Inc(Self.ByteCounter, Packet.RealLength);
    Inc(Self.PacketCounter);

    Self.ByteCount.Caption   := IntToStr(Self.ByteCounter);
    Self.PacketCount.Caption := IntToStr(Self.PacketCounter);

    if (Packet.Payload.EncodingName = T140Encoding)
      and (Binding.LocalIP   = Self.Server.Address)
      and (Binding.LocalPort = Self.Server.RTPPort) then
      Self.Received.Text := Self.Received.Text + (Packet.Payload as TIdRTPT140Payload).Block;
  finally
    Self.Lock.Release;
  end;

  if (Binding.LocalIP = Self.Client.Address)
    and (Binding.LocalPort = Self.Client.RTPPort) then
    Self.Client.Session.SendData(Packet.Payload);
end;

procedure TIdSpikeT140.Reset;
begin
  Self.Lock.Acquire;
  try
    Self.ByteCount.Caption   := '0';
    Self.ByteCounter         := 0;
    Self.PacketCount.Caption := '0';
    Self.Received.Text       := '';
    Self.PacketCounter       := 0;
    Self.SendBuffer          := '';
  finally
    Self.Lock.Release;
  end;
end;

//* TIdSpikeT140 Published methods *********************************************

procedure TIdSpikeT140.LeaveClick(Sender: TObject);
begin
  Self.Session.LeaveSession('Bye!');
end;

procedure TIdSpikeT140.Timer1Timer(Sender: TObject);
var
  Payload: TIdRTPT140Payload;
  Host:    String;
  Port:    String;
begin
  if (Self.SendBuffer <> '') then begin
    Port := Self.RemoteHostAndPort.Text;
    Host := Fetch(Port, ':');

    Payload := TIdRTPT140Payload.Create;
    try
      Payload.Block := Self.SendBuffer;
      Self.Session.SendData(Payload);
      Self.SendBuffer := '';
    finally
      Payload.Free;
    end;
  end;
end;

procedure TIdSpikeT140.SentKeyPress(Sender: TObject; var Key: Char);
begin
  Self.SendBuffer := Self.SendBuffer + Key;
end;

procedure TIdSpikeT140.JoinClick(Sender: TObject);
begin
  Self.Leave.Click;
  Self.Session.JoinSession;
end;

end.
