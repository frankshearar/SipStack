unit SpikeT140;

interface

uses
  Classes, Controls, ExtCtrls, Forms, IdRTPClient, IdRTPServer, IdSocketHandle,
  StdCtrls;

type
  TIdSpikeT140 = class(TForm)
    Sent: TMemo;
    Splitter1: TSplitter;
    Received: TMemo;
    procedure SentKeyPress(Sender: TObject; var Key: Char);
  private
    Client:       TIdRTPClient;
    Server:       TIdRTPServer;
    T140Encoding: TIdRTPEncoding;

    procedure ReadRTP(Sender: TObject;
                      APacket: TIdRTPPacket;
                      ABinding: TIdSocketHandle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  IdSpikeT140: TIdSpikeT140;

implementation

{$R *.dfm}

uses
  IdGlobal, IdSipConsts;

//******************************************************************************
//* TIdSpikeT140                                                               *
//******************************************************************************
//* TIdSpikeT140 Public methods ************************************************

constructor TIdSpikeT140.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.T140Encoding := TIdRTPEncoding.Create(T140EncodingName, T140ClockRate);

  Self.Server := TIdRTPServer.Create(nil);
  Self.Server.DefaultPort := 5004;
  Self.Server.OnRTPRead := Self.ReadRTP;
  Self.Server.Active := true;

  Self.Server.Profile.AddEncoding(Self.T140Encoding,
                                  Self.Server.Profile.FirstFreePayloadType);

  Self.Client := TIdRTPClient.Create(nil);
  Self.Client.Host := IndyGetHostName;
  Self.Client.Port := Server.DefaultPort;
end;

destructor TIdSpikeT140.Destroy;
begin
  Self.Server.Active := false;
  Self.Client.Free;
  Self.Server.Free;
  Self.T140Encoding.Free;

  inherited Destroy;  
end;

//* TIdSpikeT140 Private methods ***********************************************

procedure TIdSpikeT140.ReadRTP(Sender: TObject;
                               APacket: TIdRTPPacket;
                               ABinding: TIdSocketHandle);
begin
  Received.Text := Received.Text + TIdT140Payload(APacket.Payload).Block;
end;

//* TIdSpikeT140 Published methods *********************************************

procedure TIdSpikeT140.SentKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key in [Chr(32)..Chr(127)]) then 
    Self.Client.Send(#$00#$00 + Key, Self.Server.Profile.PayloadTypeFor(Self.T140Encoding));
end;

end.
