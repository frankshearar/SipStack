unit SpikeT140;

interface

uses
  Classes, Controls, ExtCtrls, Forms, IdRTPBase, IdRTP, IdRTPClient,
  IdRTPServer, IdSocketHandle, IdUDPClient, IdUDPServer, StdCtrls, SyncObjs;

type
  TIdSpikeT140 = class(TForm)
    Sent: TMemo;
    Splitter1: TSplitter;
    Received: TMemo;
    Panel1: TPanel;
    Send: TButton;
    Label1: TLabel;
    ByteCount: TLabel;
    SendFile: TEdit;
    Save: TButton;
    Label2: TLabel;
    PacketCount: TLabel;
    procedure SendClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
  private
    Client:        TIdUDPClient;
    Lock:          TCriticalSection;
    PacketCounter: Cardinal;
    RecvText:      String;
    Server:        TIdUDPServer;
    T140Encoding:  TIdRTPEncoding;

    procedure CountUDP(Sender: TObject;
                       Data: TStream;
                       Binding: TIdSocketHandle);
    procedure ReadRTP(Sender: TObject;
                      APacket: TIdRTPPacket;
                      ABinding: TIdSocketHandle);
    procedure Reset;
    procedure SendData(Data: TStream);
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

  Self.T140Encoding := TIdRTPEncoding.Create(T140EncodingName, T140ClockRate);

  Self.Server := TIdUDPServer.Create(nil);
  Self.Server.DefaultPort   := 5004;
//  Self.Server.OnRTPRead     := Self.ReadRTP;
  Self.Server.OnUDPRead     := Self.CountUDP;
  Self.Server.ThreadedEvent := true;
  Self.Server.Active        := true;

//  Self.Server.Profile.AddEncoding(Self.T140Encoding,
//                                  Self.Server.Profile.FirstFreePayloadType);

  Self.Client := TIdUDPClient.Create(nil);
  Self.Client.Host := IndyGetHostName;
  Self.Client.Port := Server.DefaultPort;
  Self.Client.ReceiveTimeout := 100;

  Self.Lock := TCriticalSection.Create;
end;

destructor TIdSpikeT140.Destroy;
begin
  Self.Lock.Free;

  Self.Server.Active := false;
  Self.Client.Free;
  Self.Server.Free;
  Self.T140Encoding.Free;

  inherited Destroy;
end;

//* TIdSpikeT140 Private methods ***********************************************

procedure TIdSpikeT140.CountUDP(Sender: TObject;
                                Data: TStream;
                                Binding: TIdSocketHandle);
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    S.CopyFrom(Data, 0);
    Self.RecvText := Self.RecvText + S.DataString;
  finally
    S.Free;
  end;

//  Self.Lock.Acquire;
//  try
    Inc(Self.PacketCounter);
    Self.PacketCount.Caption := IntToStr(Self.PacketCounter);

    Self.ByteCount.Caption := IntToStr(Length(Self.RecvText));
//  finally
//    Self.Lock.Release;
//  end;
end;

procedure TIdSpikeT140.ReadRTP(Sender: TObject;
                               APacket: TIdRTPPacket;
                               ABinding: TIdSocketHandle);
begin
//  Self.Lock.Acquire;
//  try
//    Self.RecvText := Self.RecvText + TIdRawPayload(APacket.Payload).Data;
//  finally
//    Self.Lock.Release;
//  end;
end;

procedure TIdSpikeT140.Reset;
begin
  Self.Lock.Acquire;
  try
    Self.ByteCount.Caption   := '0';
    Self.PacketCount.Caption := '0';
    Self.RecvText            := '';
    Self.Received.Text       := '';
    Self.PacketCounter       := 0;
  finally
    Self.Lock.Release;
  end;
end;

procedure TIdSpikeT140.SendData(Data: TStream);
const
  BufLen = 100;
var
  S:         TStringStream;
  BytesRead: Cardinal;
  BytesLeft: Cardinal;
begin
  S := TStringStream.Create('');
  try
    BytesLeft := Data.Size;
    repeat
      S.Seek(0, soFromBeginning);
      BytesRead := S.CopyFrom(Data, Min(BufLen, BytesLeft));
      Dec(BytesLeft, BytesRead);
      Self.Client.Send(S.DataString);
      Sleep(100);
    until (BytesRead < BufLen);
  finally
    S.Free;
  end;
end;

//* TIdSpikeT140 Published methods *********************************************

procedure TIdSpikeT140.SendClick(Sender: TObject);
var
  FS: TFileStream;
begin
  Self.Reset;
  FS := TFileStream.Create(Self.SendFile.Text, fmOpenRead or fmShareDenyWrite);
  try
    Self.SendData(FS);
  finally
    FS.Free;
  end;
end;

procedure TIdSpikeT140.SaveClick(Sender: TObject);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create('..\etc\saved.wav', fmCreate or fmShareDenyWrite);
  try
    FS.Write(PChar(Self.RecvText)^, Length(Self.RecvText));
  finally
    FS.Free;
  end;
end;

end.
