unit TestIdRTPServer;

interface

uses
  Classes, IdRTP, IdRTPServer, IdSocketHandle, IdUDPServer,
  TestFramework, TestFrameworkEx, TestFrameworkRtp;

type
  TestTIdRTPServer = class(TTestRTP)
  private
    Encoding: TIdT140Encoding;
    Client:   TIdRTPServer;
    Packet:   TIdRTPPacket;
    Server:   TIdRTPServer;
    procedure CheckReceivePacket(Sender: TObject;
                                 AData: TStream;
                                 ABinding: TIdSocketHandle);
    procedure CheckOnRTCPRead(Sender: TObject;
                             APacket: TIdRTCPPacket;
                             ABinding: TIdSocketHandle);
    procedure CheckOnRTPRead(Sender: TObject;
                             APacket: TIdRTPPacket;
                             ABinding: TIdSocketHandle);
    procedure WaitForSignaled;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAcceptableSSRC;
    procedure TestOnRTCPRead;
    procedure TestOnRTPRead;
    procedure TestOnUDPRead;
    procedure TestSendPayload;
  end;

  TestT140 = class(TThreadingTestCase)
  private
    Client:   TIdRTPServer;
    Msg:      String;
    Server:   TIdRTPServer;
    T140PT:   TIdRTPPayloadType;

    procedure StoreT140Data(Sender: TObject;
                            APacket: TIdRTPPacket;
                            ABinding: TIdSocketHandle);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTransmission;
  end;

  TestTIdRTPSession = class(TTestCase)
  private
    Members: TIdRTPSession;
    Profile: TIdRTPProfile;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestContains;
    procedure TestPrepareRTP;
    procedure TestPrepareSR;
    procedure TestRemove;
  end;

const
  DefaultTimeout = 5000; // ms

implementation

uses
  SyncObjs, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTPServer unit tests');
  Result.AddTest(TestTIdRTPServer.Suite);
  Result.AddTest(TestT140.Suite);
  Result.AddTest(TestTIdRTPSession.Suite);
end;

//******************************************************************************
//* TestTIdRTPServer                                                           *
//******************************************************************************
//* TestTIdRTPServer Public methods ********************************************

procedure TestTIdRTPServer.SetUp;
var
  Binding:    TIdSocketHandle;
  NoEncoding: TIdRTPEncoding;
  PT:         TIdRTPPayloadType;
begin
  inherited SetUp;

  PT := 96;

  Self.Server := TIdRTPServer.Create(nil);
  Binding := Self.Server.Bindings.Add;
  Binding.IP   := '127.0.0.1';
  Binding.Port := 5004;

  Self.Packet := TIdRTPPacket.Create(Self.Server.Profile);
  Self.Packet.Version      := 2;
  Self.Packet.HasPadding   := true;
  Self.Packet.HasExtension := false;
  Self.Packet.CsrcCount    := 2;
  Self.Packet.CsrcIDs[0]   := $cafebabe;
  Self.Packet.CsrcIDs[1]   := $deadbeef;
  Self.Packet.IsMarker     := true;
  Self.Packet.PayloadType  := PT;
  Self.Packet.SequenceNo   := $0102;
  Self.Packet.Timestamp    := $0A0B0C0D;
  Self.Packet.SyncSrcID    := $decafbad;

  Self.Client := TIdRTPServer.Create(nil);
  Binding := Self.Client.Bindings.Add;
  Binding.IP   := '127.0.0.1';
  Binding.Port := 6543; // arbitrary value

  NoEncoding := TIdRTPEncoding.Create('No encoding', 0);
  try
    Self.Server.Profile.AddEncoding(NoEncoding, PT);
  finally
    NoEncoding.Free;
  end;

  Self.Encoding := TIdT140Encoding.Create(T140Encoding,
                                             T140ClockRate);
  Self.Server.Profile.AddEncoding(Self.Encoding, PT + 1);
  Self.Client.Profile.AddEncoding(Self.Encoding, PT + 1);

  Self.Client.Active := true;
  Self.Server.Active := true;
end;

procedure TestTIdRTPServer.TearDown;
begin
  Self.Server.Active := false;
  Self.Client.Active := false;

  Self.Encoding.Free;
  Self.Client.Free;
  Self.Packet.Free;
  Self.Server.Free;

  inherited TearDown;
end;

//* TestTIdRTPServer Private methods *******************************************

procedure TestTIdRTPServer.CheckReceivePacket(Sender: TObject;
                                              AData: TStream;
                                              ABinding: TIdSocketHandle);
var
  P: TIdRTPPacket;
begin
  try
    P := TIdRTPPacket.Create(Self.Server.Profile);
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
                                           ABinding: TIdSocketHandle);
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

procedure TestTIdRTPServer.CheckOnRTPRead(Sender: TObject;
                                          APacket: TIdRTPPacket;
                                          ABinding: TIdSocketHandle);
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

procedure TestTIdRTPServer.WaitForSignaled;
begin
  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
  Self.ThreadEvent.ResetEvent;
end;

//* TestTIdRTPServer Published methods *****************************************

procedure TestTIdRTPServer.TestAcceptableSSRC;
begin
  Check(not Self.Server.AcceptableSSRC(0), '0');
  Check(Self.Server.AcceptableSSRC(1), '1');

  Self.Server.Session.Add(1);
  Check(not Self.Server.AcceptableSSRC(1), '1 in the Members');
  Check(Self.Server.AcceptableSSRC(2), '2');
end;

procedure TestTIdRTPServer.TestOnRTCPRead;
var
  S:    TStringStream;
  RTCP: TIdRTCPApplicationDefinedPacket;
begin
  Self.Server.OnRTCPRead := Self.CheckOnRTCPRead;
  S := TStringStream.Create('');
  try
    RTCP := TIdRTCPApplicationDefinedPacket.Create;
    try
      RTCP.PrintOn(S);

      Self.Client.Send(Self.Server.Bindings[0].IP,
                       Self.Server.Bindings[0].Port,
                       S.DataString);

      Self.WaitForSignaled;

      Check(Self.Server.Session.Contains(RTCP.SyncSrcID),
            'Member not added');
      CheckEquals(Self.Client.Bindings[0].IP,
                  Self.Server.Session.Member(RTCP.SyncSrcID).ControlAddress,
                  'Control address');
      CheckEquals(Self.Client.Bindings[0].Port,
                  Self.Server.Session.Member(RTCP.SyncSrcID).ControlPort,
                  'Control port');
    finally
      RTCP.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPServer.TestOnRTPRead;
var
  S: TStringStream;
begin
  Self.Server.OnRTPRead := Self.CheckOnRTPRead;
  S := TStringStream.Create('');
  try
    Self.Packet.PrintOn(S);

    Self.Client.Send(Self.Server.Bindings[0].IP,
                     Self.Server.Bindings[0].Port,
                     S.DataString);

    Self.WaitForSignaled;

    Check(Self.Server.Session.Contains(Self.Packet.SyncSrcID),
          'Member not added');
    CheckEquals(Self.Client.Bindings[0].IP,
                Self.Server.Session.Member(Self.Packet.SyncSrcID).SourceAddress,
                'Source address');
    CheckEquals(Self.Client.Bindings[0].Port,
                Self.Server.Session.Member(Self.Packet.SyncSrcID).SourcePort,
                'Source port');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPServer.TestOnUDPRead;
var
  S: TStringStream;
begin
  Self.Server.OnUDPRead := Self.CheckReceivePacket;
  Self.Server.Active := true;
  try
    S := TStringStream.Create('');
    try
      Self.Packet.PrintOn(S);

      Self.Client.Send(Self.Server.Bindings[0].IP,
                       Self.Server.Bindings[0].Port,
                       S.DataString);

      Self.WaitForSignaled;
    finally
      S.Free;
    end;
  finally
    Self.Server.Active := false;
  end;
end;

procedure TestTIdRTPServer.TestSendPayload;
var
  Payload: TIdT140Payload;
begin
  Self.Server.OnRTPRead := CheckOnRTPRead;
  Self.Client.JoinSession(Self.Server.Bindings[0].IP,
                          Self.Server.Bindings[0].Port);

  Payload := TIdT140Payload.Create(Self.Encoding);
  try
    Payload.Block := '1';

    Self.Client.SendData(Payload);
    Self.WaitForSignaled;
  finally
    Payload.Free;
  end;
end;

//******************************************************************************
//* TestT140                                                                   *
//******************************************************************************
//* TestT140 Public methods ****************************************************

procedure TestT140.SetUp;
var
  Binding: TIdSocketHandle;
  T140:    TIdRTPEncoding;
begin
  inherited SetUp;

  Self.Msg := 'Goodbye, cruel world';
  Self.T140PT := 96;

  Self.Server := TIdRTPServer.Create(nil);
  Self.Server.DefaultPort := 5004;

  T140 := TIdRTPEncoding.Create(T140Encoding, T140ClockRate);
  try
    Self.Server.Profile.AddEncoding(T140, Self.T140PT);
  finally
    T140.Free;
  end;

  Self.Client  := TIdRTPServer.Create(nil);
  Binding      := Self.Client.Bindings.Add;
  Binding.IP   := '127.0.0.1';
  Binding.Port := Self.Server.DefaultPort;
end;

procedure TestT140.TearDown;
begin
  Self.Client.Free;
  Self.Server.Free;

  inherited TearDown;
end;

//* TestT140 Private methods ***************************************************

procedure TestT140.StoreT140Data(Sender: TObject;
                                 APacket: TIdRTPPacket;
                                 ABinding: TIdSocketHandle);
begin
  try
    CheckEquals(TIdT140Payload.ClassName,
                APacket.Payload.ClassName,
                'Payload type');
    CheckEquals(Self.Msg,
                TIdT140Payload(APacket.Payload).Block,
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
  Payload: TIdT140Payload;
begin
  Self.Server.OnRTPRead := Self.StoreT140Data;
  Self.Server.Active := true;
  try
    Payload := TIdT140Payload.Create(Self.Client.Profile.EncodingFor(Self.T140PT));
    try
      Payload.Block := Self.Msg;
      Client.SendData(Payload);
    finally
      Payload.Free;
    end;

    if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);
  finally
    Self.Server.Active := false;
  end;
end;

//******************************************************************************
//* TestTIdRTPSession                                                          *
//******************************************************************************
//* TestTIdRTPSession Public methods *******************************************

procedure TestTIdRTPSession.SetUp;
begin
  inherited SetUp;

  Self.Profile := TIdAudioVisualProfile.Create;
  Self.Members := TIdRTPSession.Create(nil, Self.Profile);
end;

procedure TestTIdRTPSession.TearDown;
begin
  Self.Members.Free;
  Self.Profile.Free;

  inherited TearDown;
end;

//* TestTIdRTPSession Published methods ****************************************

procedure TestTIdRTPSession.TestAdd;
begin
  CheckEquals(0, Self.Members.Count, 'Empty table');
  Self.Members.Add($decafbad);
  CheckEquals(1, Self.Members.Count, 'SSRC not added');
  Self.Members.Add($decafbad);
  CheckEquals(1, Self.Members.Count, 'SSRC re-added');
  Check(Self.Members.Member($decafbad) = Self.Members.Add($decafbad),
        'Different entry returned for same SSRC');
end;

procedure TestTIdRTPSession.TestContains;
begin
  Check(not Self.Members.Contains($decafbad), 'Empty table');
  Self.Members.Add($decafbad);
  Check(Self.Members.Contains($decafbad), 'SSRC not added?');
end;

procedure TestTIdRTPSession.TestPrepareRTP;
var
  Pkt: TIdRTPPacket;
begin
  Pkt := TIdRTPPacket.Create(Self.Profile);
  try
    Self.Members.PrepareData(Pkt);

    // Note that the session determines the initial sequence number and
    // timestamp by selecting random numbers. We can't really check for
    // that sort've thing. The tests below should fail with a probability
    // ~2^-32, if my maths is correct. In other words, while it's perfectly
    // legal to have a zero initial sequence number and/or timestamp, it's
    // not very likely. We mainly want to ensure that the session sets the
    // timestamp and sequence number.
    CheckNotEquals(IntToHex(0, 8),
                   IntToHex(Pkt.Timestamp, 8),
                   'Timestamp');
    CheckNotEquals(IntToHex(0, Sizeof(Pkt.SequenceNo)),
                   IntToHex(Pkt.SequenceNo, Sizeof(Pkt.SequenceNo)),
                   'SequenceNo');
  finally
    Pkt.Free;
  end;
end;

procedure TestTIdRTPSession.TestPrepareSR;
var
  SR: TIdRTCPSenderReportPacket;
  Now: TIdNTPTimestamp;
  Difference: Int64;
begin
  SR := TIdRTCPSenderReportPacket.Create;
  try
    Now := NowAsNTP;
    Self.Members.PrepareControl(SR);

    CheckEquals(IntToHex(Now.IntegerPart, 8),
                IntToHex(SR.NTPTimestamp.IntegerPart, 8),
                'Integer part of timestamp');

    // Because of timing, we can't really check equality between
    // Now.FractionalPart and SR.NTPTimestamp.FractionalPart. We
    // thus check for proximity.
    Difference := Abs(Int64(Now.FractionalPart) - SR.NTPTimestamp.FractionalPart);
    Check(Difference < DateTimeToNTPFractionsOfASecond(EncodeTime(0, 0, 0, 100)),
          'Timestamps differ by more than 100ms');
  finally
    SR.Free;
  end;
end;

procedure TestTIdRTPSession.TestRemove;
begin
  Self.Members.Add($decafbad);
  Check(Self.Members.Contains($decafbad), 'SSRC not added');
  Self.Members.Remove($decafbad);
  Check(not Self.Members.Contains($decafbad), 'SSRC not removed');
end;

initialization
  RegisterTest('IdRTPServer', Suite);
end.
