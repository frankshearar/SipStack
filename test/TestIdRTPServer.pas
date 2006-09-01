{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdRTPServer;

interface

uses
  Classes, IdRTP, IdRTPServer, IdSocketHandle, IdUDPServer, SyncObjs,
  TestFramework, TestFrameworkEx, TestFrameworkRtp;

type
  TestTIdRTPServer = class(TTestRTP)
  private
    Client:           TIdRTPServer;
    Packet:           TIdRTPPacket;
    Profile:          TIdRTPProfile;
    ReceivingBinding: TIdConnection;
    Server:           TIdRTPServer;
    Session:          TIdRTPSession;

    procedure CheckReceivePacket(Sender: TObject;
                                 AData: TStream;
                                 ABinding: TIdSocketHandle);
    procedure CheckOnRTCPRead(Sender: TObject;
                              APacket: TIdRTCPPacket;
                              ABinding: TIdConnection);
    procedure CheckOnRTPRead(Sender: TObject;
                             APacket: TIdRTPPacket;
                             ABinding: TIdConnection);
    procedure SendRTCPToServer;
    procedure SendRTPToServer;
    procedure SetEvent;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckNoServerOnPort(const Host: String;
                                  Port: Cardinal;
                                  const Msg: String);
    procedure CheckServerOnPort(const Host: String;
                                Port: Cardinal;
                                const Msg: String);
  published
    procedure TestActiveStartsServers;
    procedure TestAddListener;
    procedure TestRemoveListener;
    procedure TestRTCPComesFromRTCPPort;
    procedure TestOnRTCPRead;
    procedure TestOnRTPRead;
    procedure TestOnUDPRead;
    procedure TestSessionGetsPackets;
    procedure TestSetRTCPPortOverridesDefaultSet;
    procedure TestSetRTPPortDefaultsRTCPPort;
    procedure TestSetRTPPortDoesntOverrideRTCPPort;
  end;

  TestT140 = class(TThreadingTestCase)
  private
    Client:    TIdRTPServer;
    Msg:       String;
    Profile:   TIdRTPProfile;
    RTCPEvent: TEvent;
    Server:    TIdRTPServer;
    T140PT:    TIdRTPPayloadType;

    procedure ReceiveAnyOldJunk(Sender: TObject;
                                Packet: TIdRTCPPacket;
                                Binding: TIdConnection);
    procedure StoreT140Data(Sender: TObject;
                            Packet: TIdRTPPacket;
                            Binding: TIdConnection);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTransmission;
  end;

implementation

uses
  DateUtils, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTPServer unit tests');
  Result.AddTest(TestTIdRTPServer.Suite);
  Result.AddTest(TestT140.Suite);
end;

//******************************************************************************
//* TestTIdRTPServer                                                           *
//******************************************************************************
//* TestTIdRTPServer Public methods ********************************************

procedure TestTIdRTPServer.SetUp;
var
  NoEncoding: TIdRTPPayload;
  T140:       TIdRTPPayload;
  PT:         TIdRTPPayloadType;
begin
  inherited SetUp;

  PT := 96;

  Self.Profile := TIdAudioVisualProfile.Create;
  Self.Server  := TIdRTPServer.Create;
  Self.Server.OnRTCPRead := Self.CheckOnRTCPRead;
  Self.Server.OnRTPRead := Self.CheckOnRTPRead;
  Self.Session := Self.Server.Session;

  NoEncoding := TIdRTPPayload.CreatePayload('No encoding/0');
  try
    Self.Profile.AddEncoding(NoEncoding, PT);
  finally
    NoEncoding.Free;
  end;

  T140 := TIdRTPPayload.CreatePayload(T140Encoding + '/' + IntToStr(T140ClockRate));
  try
    Self.Profile.AddEncoding(T140, PT + 1);
  finally
    T140.Free;
  end;

  Self.Server.LocalProfile  := Self.Profile;
  Self.Server.RemoteProfile := Self.Profile;
  Self.Server.RTPPort       := 5004;
  Self.Server.RTCPPort      := Self.Server.RTPPort + 1;

  Self.Client := TIdRTPServer.Create;
  Self.Client.LocalProfile  := Self.Profile;
  Self.Client.RemoteProfile := Self.Profile;
  Self.Server.RTPPort       := 6543; // arbitrary value
  Self.Server.RTCPPort      := Self.Server.RTPPort + 1;

  Self.Packet := TIdRTPPacket.Create(Self.Profile);
  Self.Packet.Version      := 2;
  Self.Packet.HasPadding   := false;
  Self.Packet.HasExtension := false;
  Self.Packet.CsrcCount    := 2;
  Self.Packet.CsrcIDs[0]   := $cafebabe;
  Self.Packet.CsrcIDs[1]   := $deadbeef;
  Self.Packet.IsMarker     := true;
  Self.Packet.PayloadType  := PT;
  Self.Packet.SequenceNo   := $0102;
  Self.Packet.Timestamp    := $0A0B0C0D;
  Self.Packet.SyncSrcID    := $decafbad;

  Self.Client.Active := true;
  Self.Server.Active := true;
end;

procedure TestTIdRTPServer.TearDown;
begin
  Self.Server.Active := false;
  Self.Client.Active := false;
  Self.Packet.Free;
  Self.Client.Free;
  Self.Server.Free;
  Self.Profile.Free;

  inherited TearDown;
end;

procedure TestTIdRTPServer.CheckNoServerOnPort(const Host: String;
                                               Port: Cardinal;
                                               const Msg: String);
var
  Binding: TIdSocketHandle;
  Server:  TIdUdpServer;
begin
  Server := TIdUdpServer.Create(nil);
  try
    Binding := Server.Bindings.Add;
    Binding.IP    := Host;
    Binding.Port  := Port;

    try
      Server.Active := true;
    except
      on EIdCouldNotBindSocket do
        Fail('Server running on ' + Host + ': ' + IntToStr(Port) + '; ' + Msg);
    end;
  finally
    Server.Free;
  end;
end;

procedure TestTIdRTPServer.CheckServerOnPort(const Host: String;
                                             Port: Cardinal;
                                             const Msg: String);
var
  Binding: TIdSocketHandle;
  Server:  TIdUdpServer;
begin
  try
    Server := TIdUdpServer.Create(nil);
    try
      Binding := Server.Bindings.Add;
      Binding.IP    := Host;
      Binding.Port  := Port;
      Server.Active := true;
      try
        // Do nothing
      finally
        Server.Active := false;
      end;
    finally
      Server.Free;
    end;
    Fail('No server running on ' + Host + ': ' + IntToStr(Port) + '; ' + Msg);
  except
    on EIdCouldNotBindSocket do;
  end;
end;

//* TestTIdRTPServer Private methods *******************************************

procedure TestTIdRTPServer.CheckReceivePacket(Sender: TObject;
                                              AData: TStream;
                                              ABinding: TIdSocketHandle);
var
  P: TIdRTPPacket;
begin
  try
    P := TIdRTPPacket.Create(Self.Profile);
    try
      P.ReadFrom(AData);
      Self.CheckHasEqualHeaders(Self.Packet, P);
    finally
      P.Free;
    end;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdRTPServer.CheckOnRTCPRead(Sender: TObject;
                                           APacket: TIdRTCPPacket;
                                           ABinding: TIdConnection);
begin
  Self.ReceivingBinding.LocalIP   := ABinding.LocalIP;
  Self.ReceivingBinding.LocalPort := ABinding.LocalPort;
  Self.ReceivingBinding.PeerIP    := ABinding.PeerIP;
  Self.ReceivingBinding.PeerPort  := ABinding.PeerPort;

  Self.SetEvent;
end;

procedure TestTIdRTPServer.CheckOnRTPRead(Sender: TObject;
                                          APacket: TIdRTPPacket;
                                          ABinding: TIdConnection);
begin
  Self.SetEvent;
end;

procedure TestTIdRTPServer.SendRTCPToServer;
var
  S:    TStringStream;
  RTCP: TIdRTCPApplicationDefined;
begin
  S := TStringStream.Create('');
  try
    RTCP := TIdRTCPApplicationDefined.Create;
    try
      RTCP.PrintOn(S);

      Self.Client.Send(Self.Server.Address,
                       Self.Server.RTCPPort,
                       S.DataString);
    finally
      RTCP.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPServer.SendRTPToServer;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Packet.PrintOn(S);

    Self.Client.Send(Self.Server.Address,
                     Self.Server.RTPPort,
                     S.DataString);
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPServer.SetEvent;
begin
  try
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

//* TestTIdRTPServer Published methods *****************************************

procedure TestTIdRTPServer.TestActiveStartsServers;
var
  RTCPPort: Cardinal;
  RTPPort:  Cardinal;
begin
  RTPPort  := Self.Server.RTPPort;
  RTCPPort := RTPPort + 1;

  CheckServerOnPort('127.0.0.1', RTPPort,  'No RTP server running');
  CheckServerOnPort('127.0.0.1', RTCPPort, 'No RTCP server running');
end;

procedure TestTIdRTPServer.TestAddListener;
var
  L1, L2: TIdRTPTestRTPListener;
begin
  L1 := TIdRTPTestRTPListener.Create;
  try
    Self.Server.AddListener(L1);

    L2 := TIdRTPTestRTPListener.Create;
    try
      Self.Server.AddListener(L2);

      Self.ExceptionMessage := 'Waiting for RTCP';
      Self.SendRTCPToServer;
      Self.WaitForSignaled;
      Check(L1.ReceivedRTCP, 'First listener didn''t receive RTCP');
      Check(L2.ReceivedRTCP, 'Second listener didn''t receive RTCP');

      Self.ExceptionMessage := 'Waiting for RTP';
      Self.SendRTPToServer;
      Self.WaitForSignaled;
      Check(L1.ReceivedRTP,  'First listener didn''t receive RTP');
      Check(L2.ReceivedRTCP, 'Second listener didn''t receive RTP');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdRTPServer.TestRemoveListener;
var
  Listener: TIdRTPTestRTPListener;
begin
  Listener := TIdRTPTestRTPListener.Create;
  try
    Self.Server.AddListener(Listener);
    Self.Server.RemoveListener(Listener);

    Self.ExceptionMessage := 'Waiting for RTCP';
    Self.SendRTCPToServer;
    Self.WaitForSignaled;
    Check(not Listener.ReceivedRTCP,
          'Listener received RTCP');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdRTPServer.TestRTCPComesFromRTCPPort;
var
  Bye: TIdRTCPBye;
begin
  Bye := TIdRTCPBye.Create;
  try
    Bye.PrepareForTransmission(Self.Client.Session);
    Self.Client.SendPacket(Self.Server.Address, Self.Server.RTCPPort, Bye);
    Self.WaitForSignaled;
    CheckEquals(Self.Client.Address,  Self.ReceivingBinding.PeerIP,   'IP of RTCP sender');
    CheckEquals(Self.Client.RTCPPort, Self.ReceivingBinding.PeerPort, 'port of RTCP sender');
  finally
    Bye.Free;
  end;
end;

procedure TestTIdRTPServer.TestOnRTCPRead;
begin
  Self.SendRTCPToServer;
  Self.WaitForSignaled;
end;

procedure TestTIdRTPServer.TestOnRTPRead;
begin
  Self.SendRTPToServer;
  Self.WaitForSignaled;
end;

procedure TestTIdRTPServer.TestOnUDPRead;
var
  S: TStringStream;
begin
  Self.Server.OnRTCPRead := nil;
  Self.Server.OnRTPRead  := nil;
  Self.Server.OnUDPRead  := Self.CheckReceivePacket;
  Self.Server.Active := true;
  try
    S := TStringStream.Create('');
    try
      Self.Packet.PrintOn(S);

      Self.Client.Send(Self.Server.Address,
                       Self.Server.RTPPort,
                       S.DataString);

      Self.WaitForSignaled;
    finally
      S.Free;
    end;
  finally
    Self.Server.Active := false;
  end;
end;

procedure TestTIdRTPServer.TestSessionGetsPackets;
var
  OriginalMemberCount: Cardinal;
begin
  Self.ExceptionMessage := 'Session waiting for RTCP';
  OriginalMemberCount := Self.Server.Session.MemberCount;

  Self.SendRTCPToServer;
  Self.WaitForSignaled;

  CheckEquals(OriginalMemberCount + 1,
              Self.Server.Session.MemberCount,
              'Session didn''t get RTCP');
end;

procedure TestTIdRTPServer.TestSetRTCPPortOverridesDefaultSet;
const
  Localhost = '127.0.0.1';
  Port      = 9000;
  RTCPPort  = Port + 5;
var
  Srv: TIdRTPServer;
begin
  // If you want a specific RTCP port, you can just set it after setting the RTP
  // port.

  Srv := TIdRTPServer.Create;
  try
    Srv.Address  := Localhost;
    Srv.RTPPort  := Port;
    Srv.RTCPPort := RTCPPort;

    Srv.Active := true;
    CheckServerOnPort(Localhost, Port,     'RTP not running');
    CheckServerOnPort(Localhost, RTCPPort, 'RTCP not running');
    CheckNoServerOnPort(Localhost, Port + 1, 'RTCP running on "default" port');
  finally
    Srv.Free;
  end;
end;

procedure TestTIdRTPServer.TestSetRTPPortDefaultsRTCPPort;
const
  Localhost = '127.0.0.1';
  Port      = 9000;
var
  Srv: TIdRTPServer;
begin
  // Setting just the RTP port sets the RTCP port to the next-port-up.

  Srv := TIdRTPServer.Create;
  try
    Srv.Address := Localhost;
    Srv.RTPPort := Port;

    Srv.Active := true;
    CheckServerOnPort(Localhost, Port,     'RTP not running');
    CheckServerOnPort(Localhost, Port + 1, 'RTCP not running');
  finally
    Srv.Free;
  end;
end;

procedure TestTIdRTPServer.TestSetRTPPortDoesntOverrideRTCPPort;
const
  Localhost = '127.0.0.1';
  Port      = 9000;
  RTCPPort  = Port + 5;
var
  Srv: TIdRTPServer;
begin
  // Setting the RTP port after you've set the RTCP port doesn't overwrite
  // the RTCP port value.

  Srv := TIdRTPServer.Create;
  try
    Srv.Address := Localhost;
    Srv.RTCPPort := RTCPPort;
    Srv.RTPPort  := Port;

    Srv.Active := true;
    CheckServerOnPort(Localhost, Port,     'RTP not running');
    CheckServerOnPort(Localhost, RTCPPort, 'RTCP not running');
    CheckNoServerOnPort(Localhost, Port + 1, 'RTCP running on "default" port');
  finally
    Srv.Free;
  end;
end;

//******************************************************************************
//* TestT140                                                                   *
//******************************************************************************
//* TestT140 Public methods ****************************************************

procedure TestT140.SetUp;
var
  T140: TIdRTPPayload;
begin
  inherited SetUp;

  Self.Profile := TIdAudioVisualProfile.Create;

  Self.DefaultTimeout := 5000;
  Self.RTCPEvent := TSimpleEvent.Create;

  Self.Msg := 'Goodbye, cruel world';
  Self.T140PT := 96;

  Self.Server := TIdRTPServer.Create;
  Self.Server.LocalProfile  := Self.Profile;
  Self.Server.RemoteProfile := Self.Profile;
  Self.Server.RTPPort       := 5004;
  Self.Server.RTCPPort      := Self.Server.RTPPort + 1;

  Self.Client  := TIdRTPServer.Create;
  Self.Client.LocalProfile  := Self.Profile;
  Self.Server.RemoteProfile := Self.Profile;
  Self.Client.RTPPort       := Self.Server.RTPPort + 2;
  Self.Client.RTCPPort      := Self.Server.RTPPort + 3;

  T140 := TIdRTPT140Payload.CreatePayload(T140Encoding + '/' + IntToStr(T140ClockRate));
  try
    Self.Profile.AddEncoding(T140, Self.T140PT);
  finally
    T140.Free;
  end;

  Self.Server.Active := true;
  Self.Client.Active := true;
end;

procedure TestT140.TearDown;
begin
  Self.Client.Active := false;
  Self.Server.Active := false;

  Self.Client.Free;
  Self.Server.Free;
  Self.RTCPEvent.Free;
  Self.Profile.Free;

  inherited TearDown;
end;

//* TestT140 Private methods ***************************************************

procedure TestT140.ReceiveAnyOldJunk(Sender: TObject;
                                     Packet: TIdRTCPPacket;
                                     Binding: TIdConnection);
begin
  Self.RTCPEvent.SetEvent;
end;

procedure TestT140.StoreT140Data(Sender: TObject;
                                 Packet: TIdRTPPacket;
                                 Binding: TIdConnection);
begin
  try
    CheckEquals(TIdRTPT140Payload.ClassName,
                Packet.Payload.ClassName,
                'Payload type');
    CheckEquals(Self.Msg,
                TIdRTPT140Payload(Packet.Payload).Block,
                'Payload');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

//* TestT140 Published methods *************************************************

procedure TestT140.TestTransmission;
var
  Payload: TIdRTPT140Payload;
  Session: TIdRTPSession;
begin
  Self.Server.OnRTCPRead := Self.ReceiveAnyOldJunk;
  Self.Client.OnRTCPRead := Self.ReceiveAnyOldJunk;
  Self.Server.OnRTPRead := Self.StoreT140Data;

  Session := Self.Client.Session;
  Session.AddReceiver(Self.Server.Address,
                      Self.Server.RTPPort);

  Self.ExceptionMessage := 'Waiting for RFC 4103 data';
  Payload := Self.Client.LocalProfile.EncodingFor(Self.T140PT).Clone as TIdRTPT140Payload;
  try
    Payload.Block := Self.Msg;
    Payload.StartTime := Now;
    Session.SendData(Payload);
  finally
    Payload.Free;
  end;

  Self.WaitForSignaled;
end;

initialization
  RegisterTest('IdRTPServer', Suite);
end.
