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
    procedure JoinClick(Sender: TObject);
    procedure LeaveClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SentKeyPress(Sender: TObject; var Key: Char);
  private
    ByteCounter:   Cardinal;
//    Client:        TIdRTPServer;
    Lock:          TCriticalSection;
    PacketCounter: Cardinal;
    SendBuffer:    String;
    Server:        TIdRTPServer;
    T140:          TIdRTPEncoding;
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

  Self.Server := TIdRTPServer.Create(nil);
  Self.Server.OnRTPRead     := Self.ReadRTP;
  Self.Server.OnUDPRead     := Self.CountUDP;
  Self.Server.DefaultPort   := 8002;
  Self.Server.Active        := true;

//  Self.Client := TIdRTPServer.Create(nil);
//  Self.Client.DefaultPort := Self.Server.DefaultPort + 2;
//  Self.Client.ControlPort := Self.Client.DefaultPort + 1;
//  Self.Client.Active      := true;

  Self.T140   := TIdT140Encoding.Create(T140Encoding, T140ClockRate);
  Self.T140PT := Self.Server.Profile.FirstFreePayloadType;
  Self.Server.Profile.AddEncoding(Self.T140, Self.T140PT);
//  Self.Client.Profile.AddEncoding(Self.T140, Self.T140PT);

  Self.Lock := TCriticalSection.Create;
  Self.SendBuffer := '';
end;

destructor TIdSpikeT140.Destroy;
begin
  Self.Lock.Free;

//  Self.Client.Active := false;
  Self.Server.Active := false;
//  Self.Client.Free;
  Self.Server.Free;
  Self.T140.Free;

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
    if (APacket.Payload.Encoding.Name = T140Encoding) then
      Received.Text := Received.Text + (APacket.Payload as TIdT140Payload).Block;
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

procedure TIdSpikeT140.JoinClick(Sender: TObject);
begin
  Self.Reset;
//  Self.Client.JoinSession(IndyGetHostName, Self.Server.DefaultPort);
  Self.Server.JoinSession(IndyGetHostName, Self.Server.DefaultPort);
end;

procedure TIdSpikeT140.LeaveClick(Sender: TObject);
begin
//  Self.Client.Session.LeaveSession('Bye!');
  Self.Server.Session.LeaveSession('Bye!');
end;

procedure TIdSpikeT140.Timer1Timer(Sender: TObject);
var
  Payload: TIdT140Payload;
  Host:    String;
  Port:    String;
begin
  if (Self.SendBuffer <> '') then begin
    Port := Self.RemoteHostAndPort.Text;
    Host := Fetch(Port, ':');

    Payload := TIdT140Payload.Create(Self.Server.Profile.EncodingFor(Self.T140PT));
    try
      Payload.Block := Self.SendBuffer;
      Self.Server.Session.SendDataTo(Payload, Host, StrToInt(Port));
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
