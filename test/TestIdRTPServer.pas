unit TestIdRTPServer;

interface

uses
  Classes, IdRTP, IdRTPServer, IdSocketHandle, IdUDPServer, SyncObjs,
  TestFramework, TestFrameworkEx, TestFrameworkRtp;

type
  TestTIdRTPMember = class(TTestCase)
  private
    Data:     TIdRTPPacket;
    Member:   TIdRTPMember;
    Notified: Boolean;
    Profile:  TIdRTPProfile;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitSequence;
  end;

  TestTIdRTPMembers = class(TTestCase)
  private
    Members: TIdRTPMemberTable;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestMembersContains;
    procedure TestRemove;
  end;

  TRTPSessionTestCase = class(TTestCase)
  protected
    Agent:   TIdNullRTPPeer;
    Session: TIdRTPSession;
    Profile: TIdRTPProfile;
    T140PT:  Cardinal;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestSessionDelegationMethods = class(TRTPSessionTestCase)
  published
    procedure TestAcceptableSSRC;
    procedure TestAddMember;
    procedure TestAddSender;
    procedure TestAddSenderAddsMember;
    procedure TestCantRemoveSelfFromSession;
    procedure TestIsSender;
    procedure TestMembersContains;
    procedure TestSendersContains;
    procedure TestPrepareRTP;
    procedure TestPrepareSR;
    procedure TestRemoveMember;
    procedure TestRemoveSender;
  end;

  TestSessionSequenceNumberRules = class(TRTPSessionTestCase)
  private
    Binding: TIdSocketHandle;
    Data:    TIdRTPPacket;
    Member:  TIdRTPMember;

    procedure ValidateSource;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFirstDataSetsProbation;
    procedure TestDuplicateRTPPacketDoesNothing;
    procedure TestLargeJumpInSequenceNoIgnored;
    procedure TestMemberSequenceInitialized;
    procedure TestMisOrderedPacketIgnored;
    procedure TestStaleSequenceNoAccepted;
    procedure TestSequentialPacketsDecrementProbationCounter;
    procedure TestSequentialPacketsIncrementReceivedCounter;
  end;

  TestSessionReportRules = class(TRTPSessionTestCase)
  published
    // still got to do a test for LARGE numbers of members - packet size > MTU (1500?)
    procedure TestReportWith2Senders;
    procedure TestReportWith32Senders;
    procedure TestSentOctetCount;
    procedure TestSentPacketCount;
    procedure TestSSRCChangeResetsSentOctetCount;
    procedure TestSSRCChangeResetsSentPacketCount;
  end;

  TestSessionSendReceiveRules = class(TRTPSessionTestCase)
  private
    Binding: TIdSocketHandle;
    Bye:     TIdRTCPBye;
    Data:    TIdRTPPacket;
    SR:      TIdRTCPSenderReport;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitialState;
    procedure TestInitialDeterministicSendInterval;
    procedure TestReceiveRTPAddsMembers;
    procedure TestReceiveByeMarksMembers;
    procedure TestReceiveByeOnNewSessionDoesNothing;
    procedure TestReceiveRTPIncreasesSenderCount;
    procedure TestReceiveRTCPAffectsAvgRTCPSize;
    procedure TestReceiveSrcDescAddsAllSources;
    procedure TestRemoveTimedOutMembers;
    procedure TestRemoveTimedOutSenders;
    procedure TestRTCPDoesntAddSender;
    procedure TestDeterministicSendInterval10MembersAndNotSender;
    procedure TestDeterministicSendInterval10MembersAndSender;
    procedure TestDeterministicSendIntervalMinimumInterval;
    procedure TestDeterministicSendIntervalWithZeroBandwidth;
  end;

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
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOnRTCPRead;
    procedure TestOnRTPRead;
    procedure TestOnUDPRead;
  end;

  TestT140 = class(TThreadingTestCase)
  private
    Client:    TIdRTPServer;
    Msg:       String;
    RTCPEvent: TEvent;
    Server:    TIdRTPServer;
    T140PT:    TIdRTPPayloadType;

    procedure ReceiveAnyOldJunk(Sender: TObject;
                                APacket: TIdRTCPPacket;
                                ABinding: TIdSocketHandle);
    procedure StoreT140Data(Sender: TObject;
                            APacket: TIdRTPPacket;
                            ABinding: TIdSocketHandle);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTransmission;
  end;

implementation

uses
  DateUtils, IdGlobal, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTPServer unit tests');

  Result.AddTest(TestTIdRTPMember.Suite);
  Result.AddTest(TestTIdRTPMembers.Suite);
  Result.AddTest(TestSessionDelegationMethods.Suite);
  Result.AddTest(TestSessionSequenceNumberRules.Suite);
  Result.AddTest(TestSessionReportRules.Suite);
  Result.AddTest(TestSessionSendReceiveRules.Suite);
  Result.AddTest(TestTIdRTPServer.Suite);
  Result.AddTest(TestT140.Suite);
end;

//******************************************************************************
//* TestTIdRTPMember                                                           *
//******************************************************************************
//* TestTIdRTPMember Public methods ********************************************

procedure TestTIdRTPMember.SetUp;
begin
  inherited SetUp;

  Self.Member := TIdRTPMember.Create;;
  Self.Member.SyncSrcID := $decafbad;
  Self.Notified := false;

  Self.Profile := TIdAudioVisualProfile.Create;
  Self.Data    := TIdRTPPacket.Create(Self.Profile);

  Self.Data.SyncSrcID  := Self.Member.SyncSrcID;
  Self.Data.SequenceNo := $f00d;
end;

procedure TestTIdRTPMember.TearDown;
begin
  Self.Data.Free;
  Self.Profile.Free;
  Self.Member.Free;

  inherited TearDown;
end;

//* TestTIdRTPMember Published methods *****************************************

procedure TestTIdRTPMember.TestInitSequence;
begin
  Self.Member.InitSequence(Self.Data);

  CheckEquals(IntToHex(Self.Data.SequenceNo, 8),
              IntToHex(Self.Member.BaseSeqNo, 8),
              'Base sequence no');
  CheckEquals(IntToHex(Self.Data.SequenceNo, 8),
              IntToHex(Self.Member.MaxSeqNo, 8),
              'Max sequence no');
  CheckEquals(IntToHex(Self.Member.SequenceNumberRange + 1, 8),
              IntToHex(Self.Member.BadSeqNo, 8),
              'Bad sequence no');
  CheckEquals(0,
              Self.Member.Cycles,
              'Cycles');
  CheckEquals(0,
              Self.Member.ExpectedPrior,
              'Expected Prior');
  CheckEquals(0,
              Self.Member.ReceivedPrior,
              'Received Prior');
end;

//******************************************************************************
//* TestTIdRTPMembers                                                          *
//******************************************************************************
//* TestTIdRTPMembers Public methods *******************************************

procedure TestTIdRTPMembers.SetUp;
begin
  inherited SetUp;

  Self.Members := TIdRTPMemberTable.Create;
end;

procedure TestTIdRTPMembers.TearDown;
begin
  Self.Members.Free;

  inherited TearDown;
end;

//* TestTIdRTPMembers Published methods ****************************************

procedure TestTIdRTPMembers.TestAdd;
begin
  CheckEquals(0, Self.Members.Count, 'Empty table');
  Self.Members.Add($decafbad);
  CheckEquals(1, Self.Members.Count, 'SSRC not added');
  Self.Members.Add($decafbad);
  CheckEquals(1, Self.Members.Count, 'SSRC re-added');
  Check(Self.Members.Member($decafbad) = Self.Members.Add($decafbad),
        'Different entry returned for same SSRC');
end;

procedure TestTIdRTPMembers.TestMembersContains;
begin
  Check(not Self.Members.Contains($decafbad), 'Empty table');
  Self.Members.Add($decafbad);
  Check(Self.Members.Contains($decafbad), 'SSRC not added?');
end;

procedure TestTIdRTPMembers.TestRemove;
begin
  Self.Members.Add($decafbad);
  Check(Self.Members.Contains($decafbad), 'SSRC not added');
  Self.Members.Remove($decafbad);
  Check(not Self.Members.Contains($decafbad), 'SSRC not removed');
end;

//******************************************************************************
//* TRTPSessionTestCase                                                        *
//******************************************************************************
//* TRTPSessionTestCase Public methods *****************************************

procedure TRTPSessionTestCase.SetUp;
var
  T140: TIdT140Encoding;
begin
  inherited SetUp;

  Self.Agent   := TIdNullRTPPeer.Create(nil);
  Self.Profile := TIdAudioVisualProfile.Create;
  Self.Session := TIdRTPSession.Create(Self.Agent, Self.Profile);

  Self.T140PT := Self.Profile.FirstFreePayloadType;
  T140 := TIdT140Encoding.Create;
  try
    Self.Profile.AddEncoding(T140, Self.T140PT);
  finally
    T140.Free;
  end;
end;

procedure TRTPSessionTestCase.TearDown;
begin
  Self.Session.Free;
  Self.Profile.Free;
  Self.Agent.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestSessionDelegationMethods                                               *
//******************************************************************************
//* TestSessionDelegationMethods Published methods *****************************

procedure TestSessionDelegationMethods.TestAcceptableSSRC;
begin
  Check(not Self.Session.AcceptableSSRC(0), '0');
  Check(Self.Session.AcceptableSSRC(1), '1');

  Self.Session.AddMember(1);
  Check(not Self.Session.AcceptableSSRC(1), '1 in the Session');
  Check(Self.Session.AcceptableSSRC(2), '2');
end;

procedure TestSessionDelegationMethods.TestAddMember;
var
  OriginalCount: Cardinal;
begin
  OriginalCount := Self.Session.MemberCount;
  CheckEquals(1, OriginalCount, 'Empty table, except for self');

  Self.Session.AddMember($decafbad);
  CheckEquals(OriginalCount + 1,
              Self.Session.MemberCount,
              'SSRC not added');

  Self.Session.AddMember($decafbad);
  CheckEquals(OriginalCount + 1,
              Self.Session.MemberCount,
              'SSRC re-added');

  Check(Self.Session.Member($decafbad) = Self.Session.AddMember($decafbad),
        'Different entry returned for same SSRC');
end;

procedure TestSessionDelegationMethods.TestAddSender;
begin
  CheckEquals(0, Self.Session.SenderCount, 'Empty table');
  Self.Session.AddSender($decafbad);
  CheckEquals(1, Self.Session.SenderCount, 'SSRC not added');
  Self.Session.AddSender($decafbad);
  CheckEquals(1, Self.Session.SenderCount, 'SSRC re-added');
  Check(Self.Session.Sender($decafbad) = Self.Session.AddSender($decafbad),
        'Different entry returned for same SSRC');
end;

procedure TestSessionDelegationMethods.TestAddSenderAddsMember;
var
  NewSSRC:             Cardinal;
  OriginalMemberCount: Cardinal;
begin
  NewSSRC             := $decafbad;
  OriginalMemberCount := Self.Session.MemberCount;

  CheckNotEquals(IntToHex(Self.Session.CurrentSSRC, 8),
                 IntToHex(NewSSRC, 8),
                 'Sanity check');

  Self.Session.AddSender(NewSSRC);
  CheckEquals(1,
              Self.Session.SenderCount,
              'SSRC not added to Senders');
  CheckEquals(OriginalMemberCount + 1,
              Self.Session.MemberCount,
              'SSRC not added to Members');
end;

procedure TestSessionDelegationMethods.TestCantRemoveSelfFromSession;
begin
  CheckEquals(1, Self.Session.MemberCount, 'Initially only self in session');
  Self.Session.RemoveMember(Self.Session.CurrentSSRC);
  CheckEquals(1, Self.Session.MemberCount, 'Self was removed from session');
end;

procedure TestSessionDelegationMethods.TestIsSender;
begin
  Check(not Self.Session.IsSender, 'New session');
  Self.Session.SendData(TIdNullPayload.NullPayload);
  Check(Self.Session.IsSender, 'Sent data');
end;

procedure TestSessionDelegationMethods.TestMembersContains;
begin
  Check(not Self.Session.MembersContains($decafbad), 'Empty table');
  Self.Session.AddMember($decafbad);
  Check(Self.Session.MembersContains($decafbad), 'SSRC not added?');
end;

procedure TestSessionDelegationMethods.TestSendersContains;
begin
  Check(not Self.Session.SendersContains($decafbad), 'Empty table');
  Self.Session.AddSender($decafbad);
  Check(Self.Session.SendersContains($decafbad), 'SSRC not added?');
end;

procedure TestSessionDelegationMethods.TestPrepareRTP;
var
  Pkt: TIdRTPPacket;
begin
  Pkt := TIdRTPPacket.Create(Self.Profile);
  try
    Self.Session.PrepareData(Pkt);

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

procedure TestSessionDelegationMethods.TestPrepareSR;
var
  Difference: Int64;
  Now:        TIdNTPTimestamp;
  SR:         TIdRTCPSenderReport;
begin
  SR := TIdRTCPSenderReport.Create;
  try
    Now := NowAsNTP;
    Self.Session.PrepareControl(SR);

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

procedure TestSessionDelegationMethods.TestRemoveMember;
begin
  Self.Session.AddMember($decafbad);
  Check(Self.Session.MembersContains($decafbad), 'SSRC not added');
  Self.Session.RemoveMember($decafbad);
  Check(not Self.Session.MembersContains($decafbad), 'SSRC not removed');
end;

procedure TestSessionDelegationMethods.TestRemoveSender;
begin
  Self.Session.AddSender($decafbad);
  Check(Self.Session.SendersContains($decafbad), 'SSRC not added');
  Self.Session.RemoveSender($decafbad);
  Check(not Self.Session.SendersContains($decafbad), 'SSRC not removed');
end;

//******************************************************************************
//* TestSessionSequenceNumberRules                                             *
//******************************************************************************
//* TestSessionSequenceNumberRules Public methods ******************************

procedure TestSessionSequenceNumberRules.SetUp;
begin
  inherited SetUp;

  Self.Binding := TIdSocketHandle.Create(nil);
  Self.Binding.IP   := '1.2.3.4';
  Self.Binding.Port := 4321;

  Self.Data := TIdRTPPacket.Create(Self.Profile);
  Self.Data.SequenceNo := $f00d;
  Self.Data.SyncSrcID  := $decafbad;

  Self.Session.ReceiveData(Self.Data, Self.Binding);

  Self.Member := Self.Session.Member(Self.Data.SyncSrcID);
end;

procedure TestSessionSequenceNumberRules.TearDown;
begin
  Self.Data.Free;
  Self.Binding.Free;

  inherited TearDown;
end;

//* TestSessionSequenceNumberRules Private methods *****************************

procedure TestSessionSequenceNumberRules.ValidateSource;
var
  I: Cardinal;
begin
  for I := 1 to Member.MinimumSequentialPackets - 1 do begin
    Self.Data.SequenceNo := Self.Data.SequenceNo + 1;
    Self.Session.ReceiveData(Self.Data, Self.Binding);
  end;
end;

//* TestSessionSequenceNumberRules Published methods ***************************

procedure TestSessionSequenceNumberRules.TestFirstDataSetsProbation;
begin
  CheckEquals(Self.Member.MinimumSequentialPackets - 1,
              Self.Member.Probation,
              'Probation counter not decremented');
  CheckEquals(0,
              Self.Member.ReceivedPackets,
              'Member should still be under probation');
end;

procedure TestSessionSequenceNumberRules.TestDuplicateRTPPacketDoesNothing;
begin
  Self.Session.ReceiveData(Self.Data, Self.Binding);
  CheckEquals(Self.Member.MinimumSequentialPackets - 1,
              Self.Member.Probation,
              'Received identical packet (i.e., not sequential), but '
            + 'probation counter decremented');
end;

procedure TestSessionSequenceNumberRules.TestLargeJumpInSequenceNoIgnored;
var
  PacketCount: Cardinal;
begin
  Self.ValidateSource;
  PacketCount := Self.Member.ReceivedPackets;

  Self.Data.SequenceNo := AddModuloWord(Self.Data.SequenceNo,
                                        Self.Member.MaxDropout);
  Self.Session.ReceiveData(Self.Data, Self.Binding);

  CheckEquals(PacketCount,
              Self.Member.ReceivedPackets,
              'Mis-ordered sequence number not ignored');
end;

procedure TestSessionSequenceNumberRules.TestMemberSequenceInitialized;
begin
  Self.Data.SequenceNo := Self.Data.SequenceNo + 1;
  Self.Session.ReceiveData(Self.Data, Self.Binding);

  CheckEquals(Self.Data.SequenceNo,
              Self.Member.BaseSeqNo,
              'BaseSeqNo');
  CheckEquals(Self.Member.SequenceNumberRange + 1,
              Self.Member.BadSeqNo,
              'BadSeqNo');
  CheckEquals(Self.Data.SequenceNo,
              Self.Member.MaxSeqNo,
              'MaxSeqNo');
  CheckEquals(0,
              Self.Member.Cycles,
              'Cycles');
  CheckEquals(0,
              Self.Member.ReceivedPrior,
              'ReceivedPrior');
  CheckEquals(0,
              Self.Member.ExpectedPrior,
              'ExpectedPrior');
end;

procedure TestSessionSequenceNumberRules.TestMisOrderedPacketIgnored;
var
  PacketCount: Cardinal;
begin
  Self.ValidateSource;
  PacketCount := Self.Member.ReceivedPackets;

  Self.Data.SequenceNo := Self.Data.SequenceNo - Self.Member.MaxMisOrder;
  Self.Session.ReceiveData(Self.Data, Self.Binding);

  CheckEquals(PacketCount,
              Self.Member.ReceivedPackets,
              'Mis-ordered sequence number not ignored');
end;

procedure TestSessionSequenceNumberRules.TestStaleSequenceNoAccepted;
var
  PacketCount: Cardinal;
begin
  Self.ValidateSource;
  PacketCount := Self.Member.ReceivedPackets;

  Self.Data.SequenceNo := Self.Data.SequenceNo - Self.Member.MaxMisOrder + 2;
  Self.Session.ReceiveData(Self.Data, Self.Binding);

  CheckEquals(PacketCount + 1,
              Self.Member.ReceivedPackets,
              'Mis-ordered sequence number not ignored');
end;

procedure TestSessionSequenceNumberRules.TestSequentialPacketsDecrementProbationCounter;
begin
  Self.Data.SequenceNo := Self.Data.SequenceNo + 1;
  Self.Session.ReceiveData(Self.Data, Self.Binding);
  CheckEquals(Self.Member.MinimumSequentialPackets - 2,
              Self.Member.Probation,
              'Received identical packet (i.e., not sequential), but '
            + 'probation counter decremented');
end;

procedure TestSessionSequenceNumberRules.TestSequentialPacketsIncrementReceivedCounter;
begin
  Self.ValidateSource;
  CheckEquals(0,
              Self.Member.Probation,
              'Probation period should have ended');
  CheckEquals(1,
              Self.Member.ReceivedPackets,
              'Received counter');

  Self.Data.SequenceNo := Self.Data.SequenceNo + 1;
  Self.Session.ReceiveData(Self.Data, Self.Binding);
  CheckEquals(2,
              Self.Member.ReceivedPackets,
              'Received counter increment');
end;

//******************************************************************************
//* TestSessionReportRules                                                     *
//******************************************************************************
//* TestSessionReportRules Published methods ***********************************

procedure TestSessionReportRules.TestReportWith2Senders;
var
  Pkt: TIdCompoundRTCPPacket;
begin
  Self.Session.AddSender(Self.Session.NewSSRC);

  Pkt := Self.Session.CreateNextReport;
  try
    // An RR and the obligatory SDES
    CheckEquals(2, Pkt.PacketCount, 'Packet count');
    CheckEquals(RTCPReceiverReport,
                Pkt.PacketAt(0).PacketType,
                '1st packet');
    CheckEquals(RTCPSourceDescription,
                Pkt.PacketAt(1).PacketType,
                '2nd packet');

    CheckEquals(1,
                (Pkt.PacketAt(0) as TIdRTCPReceiverReport).ReceptionReportCount,
                'Wrong number of report blocks');
  finally
    Pkt.Free;
  end;
end;

procedure TestSessionReportRules.TestReportWith32Senders;
var
  I:   Integer;
  Pkt: TIdCompoundRTCPPacket;
begin
  for I := 1 to 32 do
    Self.Session.AddSender(Self.Session.NewSSRC);

  Pkt := Self.Session.CreateNextReport;
  try
    // Two RRs and the obligatory SDES
    CheckEquals(3, Pkt.PacketCount, 'Packet count');
    CheckEquals(RTCPReceiverReport,
                Pkt.PacketAt(0).PacketType,
                '1st packet');
    CheckEquals(RTCPReceiverReport,
                Pkt.PacketAt(1).PacketType,
                '2nd packet');
    CheckEquals(RTCPSourceDescription,
                Pkt.PacketAt(2).PacketType,
                '3rd packet');

    CheckEquals(31,
                (Pkt.PacketAt(0) as TIdRTCPReceiverReport).ReceptionReportCount,
                'Wrong number of report blocks, 1st RR');
    CheckEquals(1,
                (Pkt.PacketAt(1) as TIdRTCPReceiverReport).ReceptionReportCount,
                'Wrong number of report blocks, 2nd RR');
  finally
    Pkt.Free;
  end;
end;

procedure TestSessionReportRules.TestSentOctetCount;
var
  DataLen: Cardinal;
  Payload: TIdT140Payload;
begin
  CheckEquals(0,
              Self.Session.SentOctetCount,
              'Initially we have sent no data');

  Payload := TIdT140Payload.Create(Self.Profile.EncodingFor(Self.T140PT));
  try
    Payload.Block := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';
    DataLen := Payload.Length;

    Self.Session.SendData(Payload);
    CheckEquals(DataLen,
                Self.Session.SentOctetCount,
                'SentOctetCount not updated');
  finally
    Payload.Free;
  end;
end;

procedure TestSessionReportRules.TestSentPacketCount;
var
  I:       Integer;
  Payload: TIdT140Payload;
begin
  CheckEquals(0,
              Self.Session.SentPacketCount,
              'Initially we have sent no data');

  Payload := TIdT140Payload.Create(Self.Profile.EncodingFor(Self.T140PT));
  try
    for I := 1 to 5 do begin
      Self.Session.SendData(Payload);
      CheckEquals(I,
                  Self.Session.SentPacketCount,
                  'SentPacketCount not updated, I=' + IntToStr(I));
    end;
  finally
    Payload.Free;
  end;
end;

procedure TestSessionReportRules.TestSSRCChangeResetsSentOctetCount;
var
  Payload: TIdT140Payload;
begin
  Payload := TIdT140Payload.Create(Self.Profile.EncodingFor(Self.T140PT));
  try
    Payload.Block := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';

    Self.Session.SendData(Payload);
  finally
    Payload.Free;
  end;

  Self.Session.Initialize;
  CheckEquals(0,
              Self.Session.SentOctetCount,
              'Changed SSRC');
end;

procedure TestSessionReportRules.TestSSRCChangeResetsSentPacketCount;
var
  Payload: TIdT140Payload;
begin
  Payload := TIdT140Payload.Create(Self.Profile.EncodingFor(Self.T140PT));
  try
    Self.Session.SendData(Payload);
  finally
    Payload.Free;
  end;

  Self.Session.Initialize;
  CheckEquals(0,
              Self.Session.SentPacketCount,
              'Changed SSRC');
end;

//******************************************************************************
//* TestSessionSendReceiveRules                                                *
//******************************************************************************
//* TestSessionSendReceiveRules Public methods *********************************

procedure TestSessionSendReceiveRules.SetUp;
begin
  inherited SetUp;

  Self.Binding := TIdSocketHandle.Create(nil);
  Self.Binding.IP   := '1.2.3.4';
  Self.Binding.Port := 4321;

  Self.Data := TIdRTPPacket.Create(Self.Profile);
  Self.Data.SyncSrcID  := $decafbad;
  Self.Data.CsrcCount  := 2;
  Self.Data.CsrcIDs[0] := $deadbeef;
  Self.Data.CsrcIDs[1] := $cafef00d;

  Self.Bye := TIdRTCPBye.Create;
  Self.Bye.SyncSrcID := $deadbeef;

  Self.SR := TIdRTCPSenderReport.Create;
  Self.SR.SyncSrcID := Self.Data.SyncSrcID;
  Self.SR.ReceptionReportCount := Self.Data.CsrcCount;
  Self.SR.Reports[0].SyncSrcID := Self.Data.CsrcIDs[0];
  Self.SR.Reports[1].SyncSrcID := Self.Data.CsrcIDs[1];
end;

procedure TestSessionSendReceiveRules.TearDown;
begin
  Self.SR.Free;
  Self.Bye.Free;
  Self.Data.Free;

  Self.Binding.Free;

  inherited TearDown;
end;

//* TestSessionSendReceiveRules Published methods ******************************

procedure TestSessionSendReceiveRules.TestInitialState;
begin
  CheckEquals(1, Self.Session.MemberCount, 'New initialised, session');

  CheckEquals(0, Self.Session.SenderCount,         'Senders');
  CheckEquals(1, Self.Session.PreviousMemberCount, 'PreviousMemberCount');
  CheckEquals(20, Self.Session.AvgRTCPSize,        'AvgRTCPSize'); // a small SDES
  Check(not Self.Session.IsSender,                 'IsSender');
  Check(Self.Session.NoControlSent,                'NoControlSent');
end;

procedure TestSessionSendReceiveRules.TestInitialDeterministicSendInterval;
begin
  Self.Session.MaxRTCPBandwidth := 100; // bytes per second
  CheckEquals(Self.Session.MinimumRTCPSendInterval / 2,
              Self.Session.DeterministicSendInterval(Self.Session.IsSender),
              OneMillisecond,
              'Initial send interval should be half the minimum');
end;

procedure TestSessionSendReceiveRules.TestReceiveRTPAddsMembers;
var
  I: Integer;
begin
  Self.Session.ReceiveData(Self.Data, Self.Binding);
  CheckEquals(4,
              Self.Session.MemberCount,
              'Session member count SSRC + 2 CSRCSs + self');

  Check(Self.Session.MembersContains(Self.Data.SyncSrcID), 'Data.SyncSrcID');

  for I := 0 to Self.Data.CsrcCount - 1 do
    Check(Self.Session.MembersContains(Self.Data.CsrcIDs[I]),
          'Data.CsrcIDs[' + IntToStr(I) + ']');
end;

procedure TestSessionSendReceiveRules.TestReceiveByeMarksMembers;
begin
  Self.Session.ReceiveData(Self.Data, Self.Binding);
  Self.Session.ReceiveControl(Self.Bye, Self.Binding);

  Check(not Self.Session.MembersContains(Self.Bye.SyncSrcID),
        'BYE didn''t remove member as leaving');
  Check(Self.Session.MembersContains(Self.Data.SyncSrcID),
        'Wrong member removed');

  Check(not Self.Session.SendersContains(Self.Bye.SyncSrcID),
        'BYE didn''t remove member (from senders)');
  Check(Self.Session.SendersContains(Self.Data.SyncSrcID),
        'Wrong sender removed');
end;

procedure TestSessionSendReceiveRules.TestReceiveByeOnNewSessionDoesNothing;
begin
  Self.Session.ReceiveControl(Self.Bye, Self.Binding);
  // sole member = Self.Session itself
  CheckEquals(1, Self.Session.MemberCount, 'Bye added members');
end;

procedure TestSessionSendReceiveRules.TestReceiveRTPIncreasesSenderCount;
begin
  CheckEquals(0, Self.Session.SenderCount, 'New session');
  Self.Session.ReceiveData(Self.Data, Self.Binding);
  CheckEquals(3, Self.Session.SenderCount, 'RTP has 3 SSRC');
end;

procedure TestSessionSendReceiveRules.TestReceiveRTCPAffectsAvgRTCPSize;
var
  InitialAvgSize: Cardinal;
begin
  InitialAvgSize := Self.Session.AvgRTCPSize;

  Self.Session.ReceiveControl(Self.SR, Self.Binding);

  CheckEquals(Self.SR.RealLength div 16 + (15 * InitialAvgSize) div 16,
              Self.Session.AvgRTCPSize,
              'Avg RTCP size not correctly adjusted');
end;

procedure TestSessionSendReceiveRules.TestReceiveSrcDescAddsAllSources;
var
  I: Integer;
begin
  Self.Session.ReceiveControl(Self.SR, Self.Binding);
  CheckEquals(Length(Self.SR.GetAllSrcIDs) + 1,
              Self.Session.MemberCount,
              IntToStr(Length(Self.SR.GetAllSrcIDs)) + ' sources + self');

  for I := 0 to Self.SR.ReceptionReportCount - 1 do
    Check(Self.Session.MembersContains(Self.SR.Reports[I].SyncSrcID),
          'SR.Reports[' + IntToStr(I) + '].SyncSrcID');
end;

procedure TestSessionSendReceiveRules.TestRemoveTimedOutMembers;
var
  FirstSSRC, SecondSSRC: Cardinal;
begin
  FirstSSRC  := 1;
  SecondSSRC := 2;
  Self.Session.AddMember(FirstSSRC).LastRTCPReceiptTime := Now - 1;
  Self.Session.AddMember(SecondSSRC).LastRTCPReceiptTime := Now;
  Self.Session.RemoveTimedOutMembers;
  CheckEquals(2,
              Self.Session.MemberCount,
              'Timed-out member not removed');
  // MemberAt(0) is Self.Session
  CheckEquals(SecondSSRC,
              Self.Session.MemberAt(1).SyncSrcID,
              'Wrong member removed');
end;

procedure TestSessionSendReceiveRules.TestRemoveTimedOutSenders;
var
  FirstSSRC, SecondSSRC: Cardinal;
begin
  FirstSSRC  := 1;
  SecondSSRC := 2;
  Self.Session.AddSender(FirstSSRC).LastRTCPReceiptTime := Now - 1;
  Self.Session.AddSender(SecondSSRC).LastRTCPReceiptTime := Now;
  Self.Session.RemoveTimedOutSenders;
  CheckEquals(1,
              Self.Session.SenderCount,
              'Timed-out sender not removed');
  // MemberAt(0) is not Self.Session since Self.Session's not sent any RTCP!
  CheckEquals(SecondSSRC,
              Self.Session.SenderAt(0).SyncSrcID,
              'Wrong member removed');
end;

procedure TestSessionSendReceiveRules.TestRTCPDoesntAddSender;
begin
  Self.Session.ReceiveControl(Self.SR, Self.Binding);
  CheckEquals(0, Self.Session.SenderCount, 'Sender added');
end;

procedure TestSessionSendReceiveRules.TestDeterministicSendInterval10MembersAndNotSender;
var
  I: Integer;
begin
  // See RFC 3550 section 6.2, 6.3, Appendix A.7 for details

  Self.Session.MaxRTCPBandwidth := 100; // bytes per second
  for I := 1 to 9 do
    Self.Session.AddMember(I);

  CheckEquals(2666*OneMillisecond,
              Self.Session.DeterministicSendInterval(Self.Session.IsSender),
              OneMillisecond,
              '10 members, session''s not a sender');
end;

procedure TestSessionSendReceiveRules.TestDeterministicSendInterval10MembersAndSender;
begin
  // See RFC 3550 section 6.2, 6.3, Appendix A.7 for details

  // Senders get much less bandwidth - 2666/0.75*0.25 = 2666/3 ~= 888ms;
  // 888 < minimum RTCP interval
  Self.Session.MaxRTCPBandwidth := 100; // bytes per second
  Self.Session.SendData(TIdNullPayload.NullPayload);
  CheckEquals(Self.Session.MinimumRTCPSendInterval / 2,
              Self.Session.DeterministicSendInterval(Self.Session.IsSender),
              OneMillisecond,
              '10 members, session''s a sender');
end;

procedure TestSessionSendReceiveRules.TestDeterministicSendIntervalMinimumInterval;
var
  I: Integer;
begin
  // See RFC 3550 section 6.2, 6.3, Appendix A.7 for details

  Self.Session.MaxRTCPBandwidth := 100; // bytes per second
  for I := 1 to 4 do
    Self.Session.AddMember(I);

  // Calculated interval is 1333ms, and 1333ms < minimum RTCP interval
  CheckEquals(Self.Session.MinimumRTCPSendInterval / 2,
              Self.Session.DeterministicSendInterval(Self.Session.IsSender),
              OneMillisecond,
              '5 members, session''s not a sender');
end;

procedure TestSessionSendReceiveRules.TestDeterministicSendIntervalWithZeroBandwidth;
begin
  CheckEquals(Self.Session.MinimumRTCPSendInterval / 2,
              Self.Session.DeterministicSendInterval(Self.Session.IsSender),
              OneMillisecond,
              'Zero bandwidth');
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

//* TestTIdRTPServer Published methods *****************************************

procedure TestTIdRTPServer.TestOnRTCPRead;
var
  S:    TStringStream;
  RTCP: TIdRTCPApplicationDefined;
begin
  Self.Server.OnRTCPRead := Self.CheckOnRTCPRead;
  S := TStringStream.Create('');
  try
    RTCP := TIdRTCPApplicationDefined.Create;
    try
      RTCP.PrintOn(S);

      Self.Client.Send(Self.Server.Bindings[0].IP,
                       Self.Server.Bindings[0].Port,
                       S.DataString);

      Self.WaitForSignaled;

      Check(Self.Server.Session.MembersContains(RTCP.SyncSrcID),
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

    Check(Self.Server.Session.MembersContains(Self.Packet.SyncSrcID),
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

  Self.DefaultTimeout := 5000;
  Self.RTCPEvent := TSimpleEvent.Create;

  Self.Msg := 'Goodbye, cruel world';
  Self.T140PT := 96;

  Self.Server := TIdRTPServer.Create(nil);
  Binding      := Self.Server.Bindings.Add;
  Binding.IP   := '127.0.0.1';
  Binding.Port := 5004;

  Self.Client  := TIdRTPServer.Create(nil);
  Binding      := Self.Client.Bindings.Add;
  Binding.IP   := '127.0.0.1';
  Binding.Port := Self.Server.DefaultPort + 2;

  T140 := TIdT140Encoding.Create(T140Encoding, T140ClockRate);
  try
    Self.Server.Profile.AddEncoding(T140, Self.T140PT);
    Self.Client.Profile.AddEncoding(T140, Self.T140PT);
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

  inherited TearDown;
end;

//* TestT140 Private methods ***************************************************

procedure TestT140.ReceiveAnyOldJunk(Sender: TObject;
                                     APacket: TIdRTCPPacket;
                                     ABinding: TIdSocketHandle);
begin
  Self.RTCPEvent.SetEvent;
end;

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
  Session: TIdRTPSession;
begin
  Self.ExceptionMessage := 'Waiting for RTCP to Server';
  Self.Server.OnRTCPRead := Self.ReceiveAnyOldJunk;
  Self.Client.OnRTCPRead := Self.ReceiveAnyOldJunk;
  Self.Server.OnRTPRead := Self.StoreT140Data;

  Self.Server.JoinSession(Self.Client.Bindings[0].IP,
                          Self.Client.Bindings[0].Port);
  Self.WaitForSignaled(Self.RTCPEvent);

  Self.ExceptionMessage := 'Waiting for RTCP to Client';
  Session := Self.Client.JoinSession(Self.Server.Bindings[0].IP,
                                     Self.Server.Bindings[0].Port);
  Self.WaitForSignaled(Self.RTCPEvent);

  Self.ExceptionMessage := 'Waiting for RFC 2793 data';
  Payload := TIdT140Payload.Create(Self.Client.Profile.EncodingFor(Self.T140PT));
  try
    Payload.Block := Self.Msg;
    Session.SendDataTo(Payload,
                       Self.Server.Bindings[0].IP,
                       Self.Server.Bindings[0].Port);
  finally
    Payload.Free;
  end;

  Self.WaitForSignaled;
end;

initialization
  RegisterTest('IdRTPServer', Suite);
end.
