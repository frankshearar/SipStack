unit SpikeT140;

interface

uses
  Classes, Controls, ExtCtrls, Forms, IdRTP, IdRTPServer,
  IdSocketHandle, IdUDPClient, IdUDPServer, StdCtrls, SyncObjs;

type
  TIdSpikeT140 = class(TForm)
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

    procedure CountUDP(Sender: TObject;
                       Data: TStream;
                       Binding: TIdSocketHandle);
    procedure ReadRTP(Sender: TObject;
                      APacket: TIdRTPPacket;
                      ABinding: TIdSocketHandle);
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
  IdGlobal, IdSipConsts, SysUtils;

//******************************************************************************
//* TIdSpikeT140                                                               *
//******************************************************************************
//* TIdSpikeT140 Public methods ************************************************

constructor TIdSpikeT140.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.Profile := TIdAudioVisualProfile.Create;
  Self.Server := TIdRTPServer.Create(nil);
  Self.Server.DefaultPort := 8002;
  Self.Server.Profile     := Self.Profile;
  Self.Server.Active      := true;

  Self.Session := Self.Server.Session;

  Self.Client := TIdRTPServer.Create(nil);
  Self.Client.ControlPort := Self.Client.DefaultPort + 1;
  Self.Client.DefaultPort := Self.Server.DefaultPort + 2;
  Self.Client.OnRTPRead   := Self.ReadRTP;
  Self.Client.OnUDPRead   := Self.CountUDP;
  Self.Client.Profile     := Self.Profile;
  Self.Client.Active      := true;

  Self.T140   := TIdRTPT140Payload.Create(T140Encoding + '/' + IntToStr(T140ClockRate));
  Self.T140PT := Self.Server.Profile.FirstFreePayloadType;
  Self.Server.Profile.AddEncoding(Self.T140, Self.T140PT);
  Self.Client.Profile.AddEncoding(Self.T140, Self.T140PT);

  Self.Lock := TCriticalSection.Create;
  Self.SendBuffer := '';

  Self.Session.AddReceiver(IndyGetHostName,
                                  Self.Client.Bindings[0].Port);
  Self.Reset;
end;

destructor TIdSpikeT140.Destroy;
begin
  Self.Lock.Free;

  Self.Client.Active := false;
  Self.Server.Active := false;

  Self.T140.Free;
  Self.Client.Free;
  Self.Session.Free;
  Self.Server.Free;
  Self.Profile.Free;

  inherited Destroy;
end;

//* TIdSpikeT140 Private methods ***********************************************

procedure TIdSpikeT140.CountUDP(Sender: TObject;
                                Data: TStream;
                                Binding: TIdSocketHandle);
var
  S: TStringStream;
begin
  Self.Lock.Acquire;
  try
   S := TStringStream.Create('');
   try
     S.CopyFrom(Data, 0);
     Inc(Self.ByteCounter, S.Size);
   finally
     S.Free;
   end;

    Inc(Self.PacketCounter);
    Self.PacketCount.Caption := IntToStr(Self.PacketCounter);

    Self.ByteCount.Caption := IntToStr(Self.ByteCounter);
  finally
    Self.Lock.Release;
  end;
end;

procedure TIdSpikeT140.ReadRTP(Sender: TObject;
                               APacket: TIdRTPPacket;
                               ABinding: TIdSocketHandle);
begin
  Self.Lock.Acquire;
  try
    if (APacket.Payload.Name = T140Encoding) then
      Received.Text := Received.Text + (APacket.Payload as TIdRTPT140Payload).Block;
  finally
    Self.Lock.Release;
  end;
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

    Payload := TIdRTPT140Payload.Create(Self.Server.Profile.EncodingFor(Self.T140PT).Name);
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

end.
