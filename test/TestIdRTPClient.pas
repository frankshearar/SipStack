unit TestIdRTPClient;

interface

uses
  IdRTPClient, IdRTPServer, IdSocketHandle, TestIdRTPServer;

type
  TestTIdRTPClient = class(TTestRTP)
  private
    AVP:    TIdAudioVisualProfile;
    Client: TIdRTPClient;
    Packet: TIdRTPPacket;
    Server: TIdRTPServer;

    procedure CheckSend(Sender: TObject;
                        APacket: TIdRTPPacket;
                        ABinding: TIdSocketHandle);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSend;
  end;

implementation

uses
  TestFramework;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTPClient unit tests');
  Result.AddTest(TestTIdRTPClient.Suite);
end;

//******************************************************************************
//* TestTIdRTPClient                                                           *
//******************************************************************************
//* TestTIdRTPClient Public methods ********************************************

procedure TestTIdRTPClient.SetUp;
begin
  inherited SetUp;

  Self.AVP := TIdAudioVisualProfile.Create;

  Self.Packet := TIdRTPPacket.Create(Self.AVP);
  Self.Packet.Version      := 2;
  Self.Packet.HasPadding   := true;
  Self.Packet.HasExtension := false;
  Self.Packet.CsrcCount    := 2;
  Self.Packet.CsrcIDs[0]   := $CAFEBABE;
  Self.Packet.CsrcIDs[1]   := $DEADBEEF;
  Self.Packet.IsMarker     := true;
  Self.Packet.PayloadType  := $0D;
  Self.Packet.SequenceNo   := $0102;
  Self.Packet.Timestamp    := $0A0B0C0D;
  Self.Packet.SyncSrcID    := $10203040;

  Self.Client := TIdRTPClient.Create(nil);
  Self.Server := TIdRTPServer.Create(nil);

  Self.Server.DefaultPort := 5004;
  Self.Client.Host        := '127.0.0.1';
  Self.Client.Port        := Self.Server.DefaultPort;
end;

procedure TestTIdRTPClient.TearDown;
begin
  Self.Server.Free;
  Self.Client.Free;
  Self.AVP.Free;

  inherited TearDown;
end;

//* TestTIdRTPClient Private methods *******************************************

procedure TestTIdRTPClient.CheckSend(Sender: TObject;
                                     APacket: TIdRTPPacket;
                                     ABinding: TIdSocketHandle);
begin
  CheckHasEqualHeaders(Self.Packet, APacket);
end;

//* TestTIdRTPClient Published methods *****************************************

procedure TestTIdRTPClient.TestSend;
begin
  Self.Server.OnRTPRead := Self.CheckSend;

  Self.Server.Active := true;
  try
    Self.Client.Send(Self.Packet);
  finally
    Self.Server.Active := false;
  end;
end;

initialization
  RegisterTest('IdRTPClient', Suite);
end.
