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
  Classes, IdConnectionBindings, IdRTP, IdRTPServer, IdSocketHandle, IdTimerQueue, IdUDPServer,
  SyncObjs, TestFramework, TestFrameworkEx, TestFrameworkRtp;

type
  TestTIdRTPServer = class(TTestRTP,
                           IIdRTPListener)
  private
    Client:           TIdRTPServer;
    EventCount:       Integer;
    Packet:           TIdRTPPacket;
    Profile:          TIdRTPProfile;
    ReceivingBinding: TIdConnectionBindings;
    RTCPPacket:       TIdRTCPPacket;
    Server:           TIdRTPServer;
    Session:          TIdRTPSession;
    Timer:            TIdDebugTimerQueue;

    procedure CheckEventScheduled(Msg: String);
    procedure CheckTwoEventsArrivedNonDeterministicOrder(ExpectedA, ExpectedB, ReceivedFirst, ReceivedSecond: String);
    procedure CheckWaitTriggered(Msg: String);
    procedure MarkEventCount;
    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdConnectionBindings);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdConnectionBindings);
    procedure SetBinding(Binding: TIdConnectionBindings; RTCP: Boolean);
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
    procedure TestReceiveCompoundRTCP;
    procedure TestReceiveRTCP;
    procedure TestReceiveRTP;
    procedure TestRTCPComesFromRTCPPort;
    procedure TestSessionGetsPackets;
    procedure TestSendRTCP;
    procedure TestSendRTP;
    procedure TestSetRTCPPortOverridesDefaultSet;
    procedure TestSetRTPPortDefaultsRTCPPort;
    procedure TestSetRTPPortDoesntOverrideRTCPPort;
  end;

  TestT140 = class(TThreadingTestCase,
                   IIdRTPDataListener)
  private
    Client:    TIdRTPServer;
    Msg:       String;
    Profile:   TIdRTPProfile;
    RTCPEvent: TEvent;
    Server:    TIdRTPServer;
    T140PT:    TIdRTPPayloadType;
    Timer:     TIdTimerQueue;

    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdConnectionBindings);
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

  Self.Timer := TIdDebugTimerQueue.Create(false);

  Self.Profile          := TIdAudioVisualProfile.Create;
  Self.ReceivingBinding := TIdConnectionBindings.Create;
  Self.Server           := TIdRTPServer.Create;
  Self.Session          := Self.Server.Session;

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
  Self.Server.Timer         := Self.Timer;

  Self.Client := TIdRTPServer.Create;
  Self.Client.LocalProfile  := Self.Profile;
  Self.Client.RemoteProfile := Self.Profile;
  Self.Server.RTPPort       := 6543; // arbitrary value
  Self.Server.RTCPPort      := Self.Server.RTPPort + 1;
  Self.Client.Timer         := Self.Timer;

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

  Self.RTCPPacket := TIdRTCPApplicationDefined.Create;
  Self.Packet.Version      := 2;
  Self.Packet.HasPadding   := false;
  Self.Packet.HasExtension := false;
  Self.RTCPPacket.SyncSrcID := Self.Packet.SyncSrcID;

  Self.Client.Active := true;
  Self.Server.Active := true;
end;

procedure TestTIdRTPServer.TearDown;
begin
  Self.Server.Active := false;
  Self.Client.Active := false;
  Self.RTCPPacket.Free;
  Self.Packet.Free;
  Self.Client.Free;
  Self.Server.Free;
  Self.ReceivingBinding.Free;
  Self.Profile.Free;

  Self.Timer.Terminate;

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

procedure TestTIdRTPServer.CheckEventScheduled(Msg: String);
begin
  Check(Self.EventCount < Self.Timer.EventCount, Msg);
end;

procedure TestTIdRTPServer.CheckTwoEventsArrivedNonDeterministicOrder(ExpectedA, ExpectedB, ReceivedFirst, ReceivedSecond: String);
begin
  if (ReceivedSecond = ExpectedA) then begin
    CheckEquals(ExpectedA,
                ReceivedSecond,
                'Unexpected last event');
    CheckEquals(ExpectedB,
                ReceivedFirst,
                'Unexpected second last event');
  end
  else if (ReceivedSecond = ExpectedB) then begin
    CheckEquals(ExpectedB,
                ReceivedSecond,
                'Unexpected last event');
    CheckEquals(ExpectedA,
                ReceivedFIrst,
                'Unexpected second last event');
  end
  else begin
    Fail('Unexpected last event: ' + ReceivedSecond);
  end;
end;

procedure TestTIdRTPServer.CheckWaitTriggered(Msg: String);
begin
  Check(Self.EventCount > Self.Timer.EventCount, Msg);
end;

procedure TestTIdRTPServer.MarkEventCount;
begin
  Self.EventCount := Self.Timer.EventCount;
end;

procedure TestTIdRTPServer.OnRTCP(Packet: TIdRTCPPacket;
                                  Binding: TIdConnectionBindings);
begin
  Self.ReceivingBinding.Assign(Binding);

  Self.SetEvent;
end;

procedure TestTIdRTPServer.OnRTP(Packet: TIdRTPPacket;
                                 Binding: TIdConnectionBindings);
begin
  Self.ReceivingBinding.Assign(Binding);

  Self.SetEvent;
end;

procedure TestTIdRTPServer.SetBinding(Binding: TIdConnectionBindings; RTCP: Boolean);
begin
  Binding.LocalIP   := Self.Server.Address;
  Binding.PeerIP    := Self.Client.Address;

  if RTCP then begin
    Binding.LocalPort := Self.Server.RTCPPort;
    Binding.PeerPort  := Self.Client.RTCPPort;
  end
  else begin
    Binding.LocalPort := Self.Server.RTPPort;
    Binding.PeerPort  := Self.Client.RTPPort;
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

procedure TestTIdRTPServer.TestReceiveCompoundRTCP;
var
  C:               TIdCompoundRTCPPacket;
  LastEvent:       TIdRTPReceivePacketWait;
  SecondLastEvent: TIdRTPReceivePacketWait;
begin
  C := TIdCompoundRTCPPacket.Create;
  C.AddSenderReport;
  C.AddReceiverReport;

  Self.MarkEventCount;
  Self.Server.ReceivePacket(C, Self.ReceivingBinding);
  CheckEventScheduled('No Wait scheduled for received RTCP');

  LastEvent       := Self.Timer.LastEventScheduled as TIdRTPReceivePacketWait;
  SecondLastEvent := Self.Timer.SecondLastEventScheduled as TIdRTPReceivePacketWait;

  Check(Assigned(SecondLastEvent),
        'There should be one Wait per packet in the compound packet');

  CheckTwoEventsArrivedNonDeterministicOrder(TIdRTCPReceiverReport.ClassName,
                                             TIdRTCPSenderReport.ClassName,
                                             SecondLastEvent.Packet.ClassName,
                                             LastEvent.Packet.ClassName);
end;

procedure TestTIdRTPServer.TestReceiveRTCP;
var
  LastEvent:      TIdRTPReceivePacketWait;
  ReceivedPacket: TIdRTCPApplicationDefined;
begin
  Self.SetBinding(Self.ReceivingBinding, true);

  Self.MarkEventCount;
  Self.Server.ReceivePacket(Self.RTCPPacket, Self.ReceivingBinding);
  CheckEventScheduled('No Wait scheduled for received RTCP');

  LastEvent := Self.Timer.LastEventScheduled(TIdRTPReceivePacketWait) as TIdRTPReceivePacketWait;

  CheckEquals(Self.Server.Address,  LastEvent.ReceivedFrom.LocalIP,   'Local IP address');
  CheckEquals(Self.Server.RTCPPort, LastEvent.ReceivedFrom.LocalPort, 'Local port');
  CheckEquals(Self.Client.Address,  LastEvent.ReceivedFrom.PeerIP,    'Peer IP address');
  CheckEquals(Self.Client.RTCPPort, LastEvent.ReceivedFrom.PeerPort,  'Peer port');

  Check(LastEvent.Packet.IsRTCP, 'Packet must be RTCP');
  CheckEquals(TIdRTCPApplicationDefined.ClassName,
              LastEvent.Packet.ClassName,
              'Unexpected packet type');

  ReceivedPacket := LastEvent.Packet as TIdRTCPApplicationDefined;

  CheckEquals(Self.RTCPPacket.Version,    ReceivedPacket.Version,    'Packet Version');
  CheckEquals(Self.RTCPPacket.HasPadding, ReceivedPacket.HasPadding, 'Packet HasPadding');
  CheckEquals(Self.RTCPPacket.PacketType, ReceivedPacket.PacketType, 'Packet PacketType');
  CheckEquals(IntToHex(Self.RTCPPacket.SyncSrcID, 8),
              IntToHex(ReceivedPacket.SyncSrcID, 8),                 'Packet SyncSrcID');
end;

procedure TestTIdRTPServer.TestReceiveRTP;
var
  LastEvent: TIdRTPReceivePacketWait;
  ReceivedPacket: TIdRTPPacket;
begin
  Self.SetBinding(Self.ReceivingBinding, false);

  Self.MarkEventCount;
  Self.Server.ReceivePacket(Self.Packet, Self.ReceivingBinding);
  CheckEventScheduled('No Wait scheduled for received RTP');

  LastEvent := Self.Timer.LastEventScheduled(TIdRTPReceivePacketWait) as TIdRTPReceivePacketWait;
  Check(Assigned(LastEvent), 'No TIdRTPReceivePacketWait scheduled');

  CheckEquals(Self.Server.Address, LastEvent.ReceivedFrom.LocalIP,   'Local IP address');
  CheckEquals(Self.Server.RTPPort, LastEvent.ReceivedFrom.LocalPort, 'Local port');
  CheckEquals(Self.Client.Address, LastEvent.ReceivedFrom.PeerIP,    'Peer IP address');
  CheckEquals(Self.Client.RTPPort, LastEvent.ReceivedFrom.PeerPort,  'Peer port');

  Check(LastEvent.Packet.IsRTP, 'Packet must be RTP');

  ReceivedPacket := LastEvent.Packet as TIdRTPPacket;

  CheckEquals(Self.Packet.Version,      ReceivedPacket.Version,      'Packet Version');
  CheckEquals(Self.Packet.HasPadding,   ReceivedPacket.HasPadding,   'Packet HasPadding');
  CheckEquals(Self.Packet.HasExtension, ReceivedPacket.HasExtension, 'Packet HasExtension');
  CheckEquals(Self.Packet.CsrcCount,    ReceivedPacket.CsrcCount,    'Packet CsrcCount');
  CheckEquals(IntToHex(Self.Packet.CsrcIDs[0], 8),
              IntToHex(ReceivedPacket.CsrcIDs[0], 8),                'Packet CsrcIDs[0]');
  CheckEquals(IntToHex(Self.Packet.CsrcIDs[1], 8),
              IntToHex(ReceivedPacket.CsrcIDs[1], 8),                'Packet CsrcIDs[1]');
  CheckEquals(Self.Packet.IsMarker,     ReceivedPacket.IsMarker,     'Packet IsMarker');
  CheckEquals(Self.Packet.PayloadType,  ReceivedPacket.PayloadType,  'Packet PayloadType');
  CheckEquals(Self.Packet.SequenceNo,   ReceivedPacket.SequenceNo,   'Packet SequenceNo');
  CheckEquals(IntToHex(Self.Packet.Timestamp, 8),
              IntToHex(ReceivedPacket.Timestamp, 8),                 'Packet Timestamp');
  CheckEquals(IntToHex(Self.Packet.SyncSrcID, 8),
              IntToHex(ReceivedPacket.SyncSrcID, 8),                 'Packet SyncSrcID');

  Self.MarkEventCount;
  Self.Timer.TriggerAllEventsUpToFirst(TIdRTPReceivePacketWait);
  CheckWaitTriggered('A receive event was rescheduled');
end;

procedure TestTIdRTPServer.TestRTCPComesFromRTCPPort;
var
  Bye: TIdRTCPBye;
  LiveTimer: TIdTimerQueue;
begin
  LiveTimer := TIdThreadedTimerQueue.Create(false);
  try
    Self.Client.Timer := LiveTimer;
    Self.Server.Timer := LiveTimer;

    Self.Server.NotifyListeners := true;
    Self.Server.AddListener(Self);

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
  finally
    Self.Server.RemoveListener(Self);
    LiveTimer.Terminate;
    Self.Server.Timer := Self.Timer;
    Self.Client.Timer := Self.Timer;
  end;
end;

procedure TestTIdRTPServer.TestSessionGetsPackets;
var
  OriginalMemberCount: Cardinal;
begin
  Self.SetBinding(Self.ReceivingBinding, true);

  Self.ExceptionMessage := 'Session waiting for RTCP';
  OriginalMemberCount := Self.Server.Session.MemberCount;

  Self.Server.ReceivePacket(Self.RTCPPacket, Self.ReceivingBinding);
  Self.Timer.TriggerAllEventsOfType(TIdRTPReceivePacketWait);

  CheckEquals(OriginalMemberCount + 1,
              Self.Server.Session.MemberCount,
              'Session didn''t get RTCP');
end;

procedure TestTIdRTPServer.TestSendRTCP;
var
  Bye:      TIdRTCPBye;
  Listener: TIdRTPTestRTPSendListener;
begin
  Listener := TIdRTPTestRTPSendListener.Create;
  try
    Self.Server.AddSendListener(Listener);

    Bye := TIdRTCPBye.Create;
    try
      Bye.PrepareForTransmission(Self.Server.Session);
      Self.Server.SendPacket(Self.Client.Address, Self.Client.RTCPPort, Bye);

      Check(Listener.SentRTCP, 'Listener not notified');
      CheckEquals(Self.Server.Address, Listener.BindingParam.LocalIP,    'Binding LocalIP');
      CheckEquals(Self.Server.RTCPPort, Listener.BindingParam.LocalPort, 'Binding LocalIP');
      CheckEquals(Self.Client.Address, Listener.BindingParam.PeerIP,     'Binding PeerIP');
      CheckEquals(Self.Client.RTCPPort, Listener.BindingParam.PeerPort,  'Binding PeerIP');
      Check(Listener.RTCPPacketParam.IsBye, 'Wrong packet sent');
    finally
      Bye.Free;
    end;
  finally
    Self.Server.RemoveSendListener(Listener);
    Listener.Free;
  end;
end;

procedure TestTIdRTPServer.TestSendRTP;
var
  Listener: TIdRTPTestRTPSendListener;
begin
  Listener := TIdRTPTestRTPSendListener.Create;
  try
    Self.Server.AddSendListener(Listener);

    Self.Server.SendPacket(Self.Client.Address, Self.Client.RTPPort, Self.Packet);

    Check(Listener.SentRTP, 'Listener not notified');
    CheckEquals(Self.Server.Address, Listener.BindingParam.LocalIP,    'Binding LocalIP');
    CheckEquals(Self.Server.RTPPort, Listener.BindingParam.LocalPort,  'Binding LocalIP');
    CheckEquals(Self.Client.Address, Listener.BindingParam.PeerIP,     'Binding PeerIP');
    CheckEquals(Self.Client.RTPPort, Listener.BindingParam.PeerPort,   'Binding PeerIP');
    CheckEquals(Self.Packet.Payload.EncodingName,
                Listener.RTPPacketParam.Payload.EncodingName,
                'Wrong packet sent');
  finally
    Self.Server.RemoveSendListener(Listener);
    Listener.Free;
  end;
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

  Self.Timer := TIdThreadedTimerQueue.Create(false);
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
  Self.Server.Timer         := Self.Timer;  

  Self.Client  := TIdRTPServer.Create;
  Self.Client.LocalProfile  := Self.Profile;
  Self.Server.RemoteProfile := Self.Profile;
  Self.Client.RTPPort       := Self.Server.RTPPort + 2;
  Self.Client.RTCPPort      := Self.Server.RTPPort + 3;
  Self.Client.Timer         := Self.Timer;

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
  Self.Timer.Terminate;

  inherited TearDown;
end;

//* TestT140 Private methods ***************************************************

procedure TestT140.OnNewData(Data: TIdRTPPayload;
                             Binding: TIdConnectionBindings);
begin
  try
    CheckEquals(TIdRTPT140Payload.ClassName,
                Data.ClassName,
                'Payload type');
    CheckEquals(Self.Msg,
                TIdRTPT140Payload(Data).Block,
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
  Session := Self.Client.Session;
  Session.AddReceiver(Self.Server.Address,
                      Self.Server.RTPPort);
  Self.Server.Session.AddListener(Self);

  Self.ExceptionMessage := 'Waiting for RFC 4103 data';
  Payload := Self.Client.LocalProfile.EncodingFor(Self.T140PT).Copy as TIdRTPT140Payload;
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
