unit IdRTPServer;

interface

uses
  Classes, Contnrs, IdRTP, IdRTPTimerQueue, IdSocketHandle, IdUDPServer,
  SyncObjs;

type
  TIdRTCPReadEvent = procedure(Sender: TObject;
                               APacket: TIdRTCPPacket;
                               ABinding: TIdSocketHandle) of object;
  TIdRTPReadEvent = procedure(Sender: TObject;
                              APacket: TIdRTPPacket;
                              ABinding: TIdSocketHandle) of object;

{
       u_int16 max_seq;        /* highest seq. number seen */
       u_int32 cycles;         /* shifted count of seq. number cycles */
       u_int32 base_seq;       /* base seq number */
       u_int32 bad_seq;        /* last 'bad' seq number + 1 */
       u_int32 probation;      /* sequ. packets till source is valid */
       u_int32 received;       /* packets received */
       u_int32 expected_prior; /* packet expected at last interval */
       u_int32 received_prior; /* packet received at last interval */
       u_int32 transit;        /* relative trans time for prev pkt */
       u_int32 jitter;         /* estimated jitter */
}

  TIdRTPMember = class(TObject)
  private
    fBadSeqNo:                 Cardinal;
    fBaseSeqNo:                Word;
    fCanonicalName:            String;
    fControlAddress:           String;
    fControlPort:              Cardinal;
    fCycles:                   Cardinal;
    fExpectedPrior:            Cardinal;
    fHasLeft:                  Boolean;
    fJitter:                   Cardinal;
    fLastRTCPReceiptTime:      TDateTime;
    fLastRTPReceiptTime:       TDateTime;
    fLocalAddress:             Boolean;
    fMaxDropout:               Word;
    fMaxMisOrder:              Word;
    fMaxSeqNo:                 Word;
    fMinimumSequentialPackets: Word;
    fProbation:                Cardinal;
    fReceivedPackets:          Cardinal;
    fReceivedPrior:            Cardinal;
    fSentControl:              Boolean;
    fSentData:                 Boolean;
    fSourceAddress:            String;
    fSourcePort:               Cardinal;
    fSyncSrcID:                Cardinal;
    fTransit:                  Cardinal;

    function DefaultMaxDropout: Cardinal;
    function DefaultMaxMisOrder: Word;
    function DefaultMinimumSequentialPackets: Cardinal;
  public
    constructor Create;

    procedure InitSequence(Data: TIdRTPPacket);
    function  IsInSequence(Data: TIdRTPPacket): Boolean;
    function  IsUnderProbation: Boolean;
    function  SequenceNumberRange: Cardinal;
    function  UpdateSequenceNo(Data: TIdRTPPacket): Boolean;

    property CanonicalName:         String    read fCanonicalName write fCanonicalName;
    property ControlAddress:        String    read fControlAddress write fControlAddress;
    property ControlPort:           Cardinal  read fControlPort write fControlPort;
    property HasLeft:               Boolean   read fHasLeft write fHasLeft;
    property LocalAddress:          Boolean   read fLocalAddress write fLocalAddress;
    property LastRTCPReceiptTime:   TDateTime read fLastRTCPReceiptTime write fLastRTCPReceiptTime;
    property LastRTPReceiptTime:    TDateTime read fLastRTPReceiptTime write fLastRTPReceiptTime;
    property SentControl:           Boolean   read fSentControl write fSentControl;
    property SentData:              Boolean   read fSentData write fSentData;
    property SourceAddress:         String    read fSourceAddress write fSourceAddress;
    property SourcePort:            Cardinal  read fSourcePort write fSourcePort;
    property SyncSrcID:             Cardinal  read fSyncSrcID write fSyncSrcID;

    // Sequence number validity, bytecounts, etc
    property MaxSeqNo:        Word     read fMaxSeqNo write fMaxSeqNo;
    property Cycles:          Cardinal read fCycles write fCycles;
    property BaseSeqNo:       Word     read fBaseSeqNo write fBaseSeqNo;
    property BadSeqNo:        Cardinal read fBadSeqNo write fBadSeqNo;
    property Probation:       Cardinal read fProbation write fProbation;
    property ReceivedPackets: Cardinal read fReceivedPackets write fReceivedPackets;
    property ExpectedPrior:   Cardinal read fExpectedPrior write fExpectedPrior;
    property ReceivedPrior:   Cardinal read fReceivedPrior write fReceivedPrior;
    property Transit:         Cardinal read fTransit write fTransit;
    property Jitter:          Cardinal read fJitter write fJitter;

    // Parameters for handling sequence validity
    property MaxDropout:               Word read fMaxDropout write fMaxDropout;
    property MinimumSequentialPackets: Word read fMinimumSequentialPackets write fMinimumSequentialPackets;
    property MaxMisOrder:              Word read fMaxMisOrder write fMaxMisOrder;
  end;

  TIdRTPMemberTable = class(TObject)
  private
    List: TObjectList;

    function Find(SSRC: Cardinal): TIdRTPMember;
    function GetMembers(Index: Cardinal): TIdRTPMember;
  public
    constructor Create;
    destructor  Destroy; override;

    function  Add(SSRC: Cardinal): TIdRTPMember;
    function  Contains(SSRC: Cardinal): Boolean;
    function  Count: Cardinal;
    function  Member(SSRC: Cardinal): TIdRTPMember;
    function  MemberAt(Index: Cardinal): TIdRTPMember;
    procedure Remove(SSRC: Cardinal);
    procedure RemoveAll;

    property Members[Index: Cardinal]: TIdRTPMember read GetMembers;
  end;

  TIdAbstractRTPPeer = class(TIdUDPServer)
  public
    procedure SendPacket(Host: String;
                         Port: Cardinal;
                         Packet: TIdRTPBasePacket); virtual; abstract;
  end;

  // I provide a self-contained SSRC space.
  // All values involving time represent milliseconds / ticks.

  // current responsibilities:
  // * Keep track of members/senders
  // * Keep track of timing stuff
  // * Keep track of session state
  TIdRTPSession = class(TObject)
  private
    Agent:                      TIdAbstractRTPPeer;
    fCurrentSSRC:               Cardinal;
    CurrentTime:                Cardinal;
    fAssumedMTU:                Cardinal;
    fAvgRTCPSize:               Cardinal;
    fCanonicalName:             String;
    fNoControlSent:             Boolean;
    fMaxRTCPBandwidth:          Cardinal; // octets per second
    fPreviousMemberCount:       Cardinal; // member count at last transmission time
    fReceiverBandwidthFraction: Double;
    fMissedReportTolerance:     Cardinal;
    fSenderBandwidthFraction:   Double;
    fSentOctetCount:            Cardinal;
    fSentPacketCount:           Cardinal;
    fSessionBandwidth:          Cardinal;
    Lock:                       TCriticalSection;
    Members:                    TIdRTPMemberTable;
    NextTransmissionTime:       TDateTime;
    NoDataSent:                 Boolean;
    PreviousTransmissionTime:   TDateTime;
    Profile:                    TIdRTPProfile;
    Senders:                    TIdRTPMemberTable;
    SequenceNo:                 TIdRTPSequenceNo;
    Timer:                      TIdRTPTimerQueue;

    function  AddAppropriateReport(Packet: TIdCompoundRTCPPacket): TIdRTCPReceiverReport;
    procedure AddControlSource(ID: Cardinal; Binding: TIdSocketHandle);
    procedure AddControlSources(RTCP: TIdRTCPMultiSSRCPacket;
                                Binding: TIdSocketHandle);
    procedure AddDataSource(ID: Cardinal; Binding: TIdSocketHandle);
    procedure AddReports(Packet: TIdCompoundRTCPPacket);
    procedure AddSourceDesc(Packet: TIdCompoundRTCPPacket);
    procedure AdjustAvgRTCPSize(Control: TIdRTCPPacket);
    procedure AdjustTransmissionTime;
    function  CompensationFactor: Double;
    function  DefaultAssumedMTU: Cardinal;
    function  DefaultMissedReportTolerance: Cardinal;
    function  DefaultNoControlSentAvgRTCPSize: Cardinal;
    function  DefaultReceiverBandwidthFraction: Double;
    function  DefaultSenderBandwidthFraction: Double;
    procedure IncSentOctetCount(N: Cardinal);
    procedure IncSentPacketCount;
    function  RandomTimeFactor: Double;
    procedure RemoveSource(ID: Cardinal);
    procedure RemoveSources(Bye: TIdRTCPBye);
    procedure ResetSentOctetCount;
    procedure ResetSentPacketCount;
    procedure SetCurrentSSRC(const Value: Cardinal);
    procedure TransmissionTimeExpire(Sender: TObject);
  public
    constructor Create(Agent: TIdAbstractRTPPeer;
                       Profile: TIdRTPProfile);
    destructor  Destroy; override;

    function  AcceptableSSRC(SSRC: Cardinal): Boolean;
    function  AddMember(SSRC: Cardinal): TIdRTPMember;
    function  AddSender(SSRC: Cardinal): TIdRTPMember;
    function  CreateNextReport: TIdCompoundRTCPPacket;
    function  DeterministicSendInterval(ForSender: Boolean): TDateTime;
    procedure Initialize;
    function  IsSender: Boolean;
    procedure LeaveSession;
    function  Member(SSRC: Cardinal): TIdRTPMember;
    function  MemberAt(Index: Cardinal): TIdRTPMember;
    function  MemberCount: Cardinal;
    function  MembersContains(SSRC: Cardinal): Boolean;
    function  MinimumRTCPSendInterval: TDateTime;
    function  NewSSRC: Cardinal;
    function  NextSequenceNo: TIdRTPSequenceNo;
    function  NothingSent: Boolean;
    procedure PrepareBasePacket(Pkt: TIdRTPBasePacket);
    procedure PrepareControl(Packet: TIdCompoundRTCPPacket); overload;
    procedure PrepareControl(RR: TIdRTCPReceiverReport); overload;
    procedure PrepareControl(SR: TIdRTCPSenderReport); overload;
    procedure PrepareControl(SDES: TIdRTCPSourceDescription); overload;
    procedure PrepareControl(Bye: TIdRTCPBye); overload;
    procedure PrepareData(Packet: TIdRTPPacket);
    procedure ReceiveControl(RTCP: TIdRTCPPacket;
                             Binding: TIdSocketHandle);
    procedure ReceiveData(RTP: TIdRTPPacket;
                          Binding: TIdSocketHandle);
    function  ReceiverCount: Cardinal;
    procedure RemoveMember(SSRC: Cardinal);
    procedure RemoveSender(SSRC: Cardinal);
    procedure RemoveTimedOutMembers;
    procedure RemoveTimedOutSenders;
    procedure SendReport;
    procedure SendControl(Packet: TIdRTCPPacket);
    procedure SendData(Data: TIdRTPPayload);
    procedure SendDataTo(Data: TIdRTPPayload;
                         Host: String;
                         Port: Cardinal);
    function  Sender(SSRC: Cardinal): TIdRTPMember;
    function  SenderAt(Index: Cardinal): TIdRTPMember;
    function  SenderCount: Cardinal;
    function  SendersContains(SSRC: Cardinal): Boolean;
    function  SendInterval: TDateTime;

    property AssumedMTU:                Cardinal read fAssumedMTU write fAssumedMTU;
    property AvgRTCPSize:               Cardinal read fAvgRTCPSize;
    property CanonicalName:             String   read fCanonicalName write fCanonicalName;
    property CurrentSSRC:               Cardinal read fCurrentSSRC;
    property NoControlSent:             Boolean  read fNoControlSent;
    property MaxRTCPBandwidth:          Cardinal read fMaxRTCPBandwidth write fMaxRTCPBandwidth;
    property PreviousMemberCount:       Cardinal read fPreviousMemberCount;
    property MissedReportTolerance:     Cardinal read fMissedReportTolerance write fMissedReportTolerance;
    property ReceiverBandwidthFraction: Double   read fReceiverBandwidthFraction write fReceiverBandwidthFraction;
    property SenderBandwidthFraction:   Double   read fSenderBandwidthFraction write fSenderBandwidthFraction;
    property SentOctetCount:            Cardinal read fSentOctetCount;
    property SentPacketCount:           Cardinal read fSentPacketCount;
    property SessionBandwith:           Cardinal read fSessionBandwidth write fSessionBandwidth;
  end;

  // While I look like a Server, I really represent a peer in an RTP session.
  // You may use me not only to receive RTP/RTCP data, but also to send data.
  //
  // As an implementation detail, the SSRC with value zero has a special
  // meaning. It represents the null SSRC. Equivalently, if CurrentSSRC has a
  // value of zero then I have not joined an RTP session.
  TIdRTPServer = class(TIdAbstractRTPPeer)
  private
    FControlPort: Cardinal;
    FSession:     TIdRTPSession;
    FOnRTCPRead:  TIdRTCPReadEvent;
    FOnRTPRead:   TIdRTPReadEvent;
    FProfile:     TIdRTPProfile;

    procedure DoOnRTCPRead(APacket: TIdRTCPPacket;
                           ABinding: TIdSocketHandle);
    procedure DoOnRTPRead(APacket: TIdRTPPacket;
                          ABinding: TIdSocketHandle);
  protected
    procedure DoUDPRead(AData: TStream;
                        ABinding: TIdSocketHandle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  JoinSession(Host: String; Port: Cardinal): TIdRTPSession;
    procedure SendPacket(Host: String;
                         Port: Cardinal;
                         Packet: TIdRTPBasePacket); override;

    property Profile: TIdRTPProfile read FProfile;
  published
    property ControlPort: Cardinal         read FControlPort write FControlPort;
    property Session:     TIdRTPSession    read FSession;
    property OnRTCPRead:  TIdRTCPReadEvent read FOnRTCPRead write FOnRTCPRead;
    property OnRTPRead:   TIdRTPReadEvent  read FOnRTPRead write FOnRTPRead;
  end;

  TIdNullRTPPeer = class(TIdAbstractRTPPeer)
  public
    procedure SendPacket(Host: String;
                         Port: Cardinal;
                         Packet: TIdRTPBasePacket); override;
  end;

implementation

uses
  DateUtils, IdGlobal, IdHash, IdHashMessageDigest, IdRandom, Math,
  SysUtils, Types;

const
  ZeroSSRC = 0;

//******************************************************************************
//* TIdRTPMember                                                               *
//******************************************************************************
//* TIdRTPMember Public methods ************************************************

constructor TIdRTPMember.Create;
begin
  inherited Create;

  Self.ControlAddress           := '';
  Self.ControlPort              := 0;
  Self.HasLeft                  := false;
  Self.LocalAddress             := false;
  Self.MaxDropout               := Self.DefaultMaxDropout;
  Self.MaxMisOrder              := Self.DefaultMaxMisOrder;
  Self.MinimumSequentialPackets := Self.DefaultMinimumSequentialPackets;
  Self.Probation                := Self.MinimumSequentialPackets;
  Self.SentData                 := false;
  Self.SentControl              := false;
  Self.SourceAddress            := '';
  Self.SourcePort               := 0;
end;

procedure TIdRTPMember.InitSequence(Data: TIdRTPPacket);
begin
  Assert(Self.SyncSrcID = Data.SyncSrcID,
         'Member received an RTP packet not meant for it');

  Self.BaseSeqNo := Data.SequenceNo;
  Self.BadSeqNo  := Self.SequenceNumberRange + 1;
  Self.MaxSeqNo  := Data.SequenceNo;

  // Data is the very first packet from the newly-validated source
  Self.ReceivedPackets := 1;
end;

function TIdRTPMember.IsInSequence(Data: TIdRTPPacket): Boolean;
begin
  Result := Data.SequenceNo = Self.MaxSeqNo + 1;
end;

function TIdRTPMember.IsUnderProbation: Boolean;
begin
  Result := Self.Probation > 0;
end;

function TIdRTPMember.SequenceNumberRange: Cardinal;
begin
  Result := 1 shl 16;
end;

function TIdRTPMember.UpdateSequenceNo(Data: TIdRTPPacket): Boolean;
var
  Delta: Cardinal;
begin
  if (Data.SequenceNo < Self.MaxSeqNo) then
    Delta := High(Data.SequenceNo) - (Self.MaxSeqNo - Data.SequenceNo)
  else
    Delta := Data.SequenceNo - Self.MaxSeqNo;

  if Self.IsUnderProbation then begin
    if Self.IsInSequence(Data) then begin
      Self.Probation := Self.Probation - 1;
      Self.MaxSeqNo  := Data.SequenceNo;

      if not Self.IsUnderProbation then begin
        Self.InitSequence(Data);
        Result := true;
        Exit;
      end;
    end
    else begin
      // First received packet - one down, Self.MinimumSequentialPackets - 1
      Self.Probation := Self.MinimumSequentialPackets - 1;
      Self.MaxSeqNo  := Data.SequenceNo;
    end;
    Result := false;
    Exit;
  end
  else if (Delta < Self.MaxDropout) then begin
    // In order, with permissible gap
    if (Data.SequenceNo < Self.MaxSeqNo) then begin
      // Sequence number wrapped - count another 64k cycle
      Self.Cycles := Self.Cycles + Self.SequenceNumberRange;
    end;
    Self.MaxSeqNo := Data.SequenceNo;
  end
  else if (Delta < Self.SequenceNumberRange - Self.MaxMisOrder) then begin
    // The sequence made a very large jump
    if (Data.SequenceNo = Self.BadSeqNo) then begin
      // Two sequential packets - assume the other side restarted without
      // telling us so just re-sync (i.e., pretend this was the first
      // packet).
      Self.InitSequence(Data);
    end
    else begin
      Self.BadSeqNo := (Data.SequenceNo + 1) and (Self.SequenceNumberRange - 1);
      Result := false;
      Exit;
    end;
  end
  else begin
    // duplicate or re-ordered packet
  end;
  Self.ReceivedPackets := Self.ReceivedPackets + 1;
  Result := true;
end;

//* TIdRTPMember Private methods ***********************************************

function TIdRTPMember.DefaultMaxDropout: Cardinal;
begin
  // This tells us how big a gap in the sequence numbers we will
  // accept before invalidating the source.
  Result := 3000;
end;

function TIdRTPMember.DefaultMaxMisOrder: Word;
begin
  // We accept packets as valid if their sequence numbers are no more
  // than MaxMisOrder behind MaxSeqNo.
  Result := 100;
end;

function TIdRTPMember.DefaultMinimumSequentialPackets: Cardinal;
begin
  // This tells us how many packets we must receive, _in_order_, before
  // we validate a source.
  Result := 2;
end;

//******************************************************************************
//* TIdRTPMemberTable                                                          *
//******************************************************************************
//* TIdRTPMemberTable Public methods *******************************************

constructor TIdRTPMemberTable.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdRTPMemberTable.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

function TIdRTPMemberTable.Add(SSRC: Cardinal): TIdRTPMember;
begin
  if Self.Contains(SSRC) then begin
    Result := Self.Find(SSRC);
    Exit;
  end;

  Result := TIdRTPMember.Create;
  try
    Self.List.Add(Result);

    Result.ControlAddress := '';
    Result.ControlPort    := 0;
    Result.SourceAddress  := '';
    Result.SourcePort     := 0;
    Result.SyncSrcID      := SSRC;
  except
    if (Self.List.IndexOf(Result) <> -1) then
      Self.List.Remove(Result)
    else
      FreeAndNil(Result);

    raise;
  end;
end;

function TIdRTPMemberTable.Contains(SSRC: Cardinal): Boolean;
begin
  Result := Assigned(Self.Find(SSRC));
end;

function TIdRTPMemberTable.Count: Cardinal;
begin
  Result := Self.List.Count;
end;

function TIdRTPMemberTable.Member(SSRC: Cardinal): TIdRTPMember;
begin
  Result := Self.Find(SSRC);
end;

function TIdRTPMemberTable.MemberAt(Index: Cardinal): TIdRTPMember;
begin
  Result := Self.List[Index] as TIdRTPMember;
end;

procedure TIdRTPMemberTable.Remove(SSRC: Cardinal);
begin
  Self.List.Remove(Self.Find(SSRC));
end;

procedure TIdRTPMemberTable.RemoveAll;
begin
  Self.List.Clear;
end;

//* TIdRTPMemberTable Private methods ******************************************

function TIdRTPMemberTable.Find(SSRC: Cardinal): TIdRTPMember;
var
  I: Cardinal;
begin
  Result := nil;
  I := 0;

  while (I < Self.Count) and not Assigned(Result) do
    if (Self.MemberAt(I).SyncSrcID = SSRC) then
      Result := Self.MemberAt(I)
    else
      Inc(I);
end;

function TIdRTPMemberTable.GetMembers(Index: Cardinal): TIdRTPMember;
begin
  // TODO: This will blow up when Index > High(Integer)
  Result := Self.Members[Index] as TIdRTPMember;
end;

//******************************************************************************
//* TIdRTPSession                                                              *
//******************************************************************************
//* TIdRTPSession Public methods ***********************************************

constructor TIdRTPSession.Create(Agent: TIdAbstractRTPPeer;
                                 Profile: TIdRTPProfile);
begin
  inherited Create;

  Self.Agent          := Agent;
  Self.fNoControlSent := true;
  Self.NoDataSent     := true;
  Self.Profile        := Profile;

  Self.Members := TIdRTPMemberTable.Create;
  Self.Senders := TIdRTPMemberTable.Create;

  Self.Lock              := TCriticalSection.Create;
  Self.Timer             := TIdRTPTimerQueue.Create;

  Self.AssumedMTU            := Self.DefaultAssumedMTU;
  Self.MissedReportTolerance := Self.DefaultMissedReportTolerance;

  Self.Initialize;
end;

destructor TIdRTPSession.Destroy;
begin
  Self.Members.Free;
  Self.Senders.Free;
  Self.Timer.Free;
  Self.Lock.Free;

  inherited Destroy;
end;

function TIdRTPSession.AcceptableSSRC(SSRC: Cardinal): Boolean;
begin
  Result := (SSRC <> 0) and not Self.MembersContains(SSRC);
end;

function TIdRTPSession.AddMember(SSRC: Cardinal): TIdRTPMember;
begin
  Result := Self.Members.Add(SSRC);
end;

function TIdRTPSession.AddSender(SSRC: Cardinal): TIdRTPMember;
begin
  Self.Members.Add(SSRC);
  Result := Self.Senders.Add(SSRC);
end;

function TIdRTPSession.CreateNextReport: TIdCompoundRTCPPacket;
begin
  // Either an RR or SR, plus an SDES
  Result := TIdCompoundRTCPPacket.Create;
  try
    Self.AddReports(Result);
    Self.AddSourceDesc(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TIdRTPSession.DeterministicSendInterval(ForSender: Boolean): TDateTime;
var
  N:           Cardinal;
  MinInterval: TDateTime;
begin
  MinInterval := Self.MinimumRTCPSendInterval;

  if (Self.NoControlSent) then
    MinInterval := MinInterval / 2;

  N := Self.MemberCount;
  if (Self.SenderCount <= Round(Self.MemberCount * Self.SenderBandwidthFraction)) then begin
    if ForSender then begin
      Self.MaxRTCPBandwidth := Round(Self.MaxRTCPBandwidth * Self.SenderBandwidthFraction);
      N := Self.SenderCount;
    end
    else begin
      Self.MaxRTCPBandwidth := Round(Self.MaxRTCPBandwidth * Self.ReceiverBandwidthFraction);
      N := Self.ReceiverCount;
    end;
  end;

  if (Self.MaxRTCPBandwidth > 0) then begin
    Result := OneSecond * Self.AvgRTCPSize * N / Self.MaxRTCPBandwidth;

    if (Result < MinInterval) then
      Result := MinInterval;
  end
  else
    Result := MinInterval;
end;

procedure TIdRTPSession.Initialize;
begin
  Self.SequenceNo := TIdRandomNumber.NextCardinal(High(SequenceNo));

  Self.Members.RemoveAll;

  Self.SetCurrentSSRC(Self.NewSSRC);
  Self.AddMember(Self.CurrentSSRC).LocalAddress := true;
  Self.SenderBandwidthFraction   := Self.DefaultSenderBandwidthFraction;
  Self.ReceiverBandwidthFraction := Self.DefaultReceiverBandwidthFraction;

  Self.fPreviousMemberCount     := 1;
  Self.PreviousTransmissionTime := 0;
  Self.fAvgRTCPSize             := Self.DefaultNoControlSentAvgRTCPSize;
end;

function TIdRTPSession.IsSender: Boolean;
begin
  Result := Self.SendersContains(Self.CurrentSSRC);
end;

procedure TIdRTPSession.LeaveSession;
var
  Bye: TIdRTCPBye;
begin
  Bye := TIdRTCPBye.Create;
  try
    Self.PrepareControl(Bye);

    Self.SendControl(Bye);
  finally
    Bye.Free;
  end;
end;

function TIdRTPSession.Member(SSRC: Cardinal): TIdRTPMember;
begin
  Result := Self.Members.Member(SSRC);
end;

function TIdRTPSession.MemberAt(Index: Cardinal): TIdRTPMember;
begin
  Result := Self.Members.MemberAt(Index);
end;

function TIdRTPSession.MemberCount: Cardinal;
begin
  Result := Self.Members.Count;
end;

function TIdRTPSession.MembersContains(SSRC: Cardinal): Boolean;
begin
  Result := Self.Members.Contains(SSRC);
end;

function TIdRTPSession.MinimumRTCPSendInterval: TDateTime;
begin
  Result := 5*OneSecond;
end;

function TIdRTPSession.NewSSRC: Cardinal;
var
  Hash:   T4x4LongWordRecord;
  Hasher: TIdHash128;
  I:      Integer;
begin
  Result := 0;
  // This implementation's largely stolen from Appendix A.6 of RFC 3550.
  Hasher := TIdHashMessageDigest5.Create;
  try
    while not Self.AcceptableSSRC(Result) do begin
      // TODO: We should add more stuff here. RFC 3550's uses: pid, uid, gid and
      // hostid (but hostid is deprecated according to FreeBSD's gethostid(3)
      // manpage).
      Hash := Hasher.HashValue(DateTimeToStr(Now)
                             + IndyGetHostName
                             + IntToHex(CurrentProcessId, 8)
                             + IntToHex(TIdRandomNumber.NextCardinal, 8));

      Result := 0;
      for I := Low(Hash) to High(Hash) do
        Result := Result xor Hash[I];
    end;
  finally
    Hasher.Free;
  end;
end;

function TIdRTPSession.NextSequenceNo: TIdRTPSequenceNo;
begin
  Result := SequenceNo;
  SequenceNo := AddModuloWord(SequenceNo, 1);
end;

function TIdRTPSession.NothingSent: Boolean;
begin
  Result := Self.NoDataSent and Self.NoControlSent;
end;

procedure TIdRTPSession.PrepareBasePacket(Pkt: TIdRTPBasePacket);
begin
  Pkt.SyncSrcID := Self.CurrentSSRC;

  if Pkt.IsRTCP then
    Self.fNoControlSent := false;
end;

procedure TIdRTPSession.PrepareControl(Packet: TIdCompoundRTCPPacket);
var
  I: Cardinal;
begin
  if (Packet.PacketCount > 0) then
    for I := 0 to Packet.PacketCount - 1 do
      Self.PrepareBasePacket(Packet.PacketAt(I));
end;

procedure TIdRTPSession.PrepareControl(RR: TIdRTCPReceiverReport);
begin
  Self.PrepareBasePacket(RR);
end;

procedure TIdRTPSession.PrepareControl(SR: TIdRTCPSenderReport);
begin
  Self.PrepareBasePacket(SR);
  SR.NTPTimestamp := NowAsNTP;
end;

procedure TIdRTPSession.PrepareControl(SDES: TIdRTCPSourceDescription);
begin
  Self.PrepareBasePacket(SDES);
end;

procedure TIdRTPSession.PrepareControl(Bye: TIdRTCPBye);
begin
  Self.PrepareBasePacket(Bye);
end;

procedure TIdRTPSession.PrepareData(Packet: TIdRTPPacket);
begin
  Self.PrepareBasePacket(Packet);

  if Self.NoDataSent then begin
    Self.SequenceNo   := TIdRandomNumber.NextCardinal(High(Self.SequenceNo));
    Self.CurrentTime  := TIdRandomNumber.NextCardinal;
    Packet.SequenceNo := Self.SequenceNo;

    Self.NoDataSent := false;
  end
  else begin
    Inc(Self.CurrentTime, Packet.Payload.NumberOfSamples);
    Packet.SequenceNo := Self.NextSequenceNo;
  end;

  Packet.Timestamp := Self.CurrentTime;
end;

procedure TIdRTPSession.ReceiveControl(RTCP: TIdRTCPPacket;
                                       Binding: TIdSocketHandle);
begin
  if RTCP.IsBye then begin
    Self.RemoveSources(RTCP as TIdRTCPBye);
  end
  else begin
    Self.AdjustAvgRTCPSize(RTCP);

    if RTCP is TIdRTCPMultiSSRCPacket then
      Self.AddControlSources(RTCP as TIdRTCPMultiSSRCPacket, Binding)
    else
      Self.AddControlSource(RTCP.SyncSrcID, Binding);
  end;
end;

procedure TIdRTPSession.ReceiveData(RTP: TIdRTPPacket;
                                    Binding: TIdSocketHandle);
var
  I: Integer;
begin
  if not Self.MembersContains(RTP.SyncSrcID) then
    Self.AddDataSource(RTP.SyncSrcID, Binding);

  for I := 0 to RTP.CsrcCount - 1 do
    Self.AddDataSource(RTP.CsrcIDs[I], Binding);

  if Self.Member(RTP.SyncSrcID).UpdateSequenceNo(RTP) then begin
    // Valid, in-sequence RTP can be sent up the stack
  end;
end;

function TIdRTPSession.ReceiverCount: Cardinal;
begin
  Result := Self.MemberCount - Self.SenderCount;
end;

procedure TIdRTPSession.RemoveMember(SSRC: Cardinal);
begin
  // We can't remove ourselves from a session - it makes no sense!
  if (SSRC <> Self.CurrentSSRC) then
    Self.Members.Remove(SSRC);
end;

procedure TIdRTPSession.RemoveSender(SSRC: Cardinal);
begin
  Self.Senders.Remove(SSRC);
end;

procedure TIdRTPSession.RemoveTimedOutMembers;
var
  I:       Cardinal;
  Timeout: TDateTime;
begin
  Timeout := Now - Self.MissedReportTolerance*Self.DeterministicSendInterval(false);

  // 1 because Self.MemberAt(0) represents Self, which won't ever time out.
  I := 1;
  while (I < Self.MemberCount) do begin
    if (Self.MemberAt(I).LastRTCPReceiptTime < Timeout) then
      Self.RemoveMember(Self.MemberAt(I).SyncSrcID)
    else
      Inc(I)
  end;
end;

procedure TIdRTPSession.RemoveTimedOutSenders;
var
  CurrentTime: TDateTime;
  I:           Cardinal;
begin
  CurrentTime := Now;

  // This implies that Self can itself be timed out as a sender. That's fine.
  I := 0;
  while (I < Self.SenderCount) do begin
    if (Self.SenderAt(I).LastRTCPReceiptTime < (CurrentTime - 2*Self.SendInterval)) then
      Self.RemoveSender(Self.SenderAt(I).SyncSrcID)
    else
      Inc(I)
  end;
end;

procedure TIdRTPSession.SendReport;
begin
end;

procedure TIdRTPSession.SendControl(Packet: TIdRTCPPacket);
var
  I: Integer;
begin
  Self.PrepareControl(Packet as TIdRTCPSenderReport);

  for I := 0 to Self.MemberCount - 1 do
    Agent.SendPacket(Self.MemberAt(I).ControlAddress,
                     Self.MemberAt(I).ControlPort,
                     Packet);
end;

procedure TIdRTPSession.SendData(Data: TIdRTPPayload);
var
  I:      Integer;
  Packet: TIdRTPPacket;
  S:      TMemoryStream;
begin
  Packet := TIdRTPPacket.Create(Self.Profile);
  try
    S := TMemoryStream.Create;
    try
      Data.PrintOn(S);
      S.Seek(0, soFromBeginning);
      Packet.PayloadType := Self.Profile.PayloadTypeFor(Data.Encoding);
      Packet.ReadPayload(S);

      Self.PrepareData(Packet);

      if not Self.SendersContains(Self.CurrentSSRC) then
        Self.AddSender(Self.CurrentSSRC);

      Self.IncSentOctetCount(Data.Length);
      Self.IncSentPacketCount;

      if (Self.MemberCount > 0) then
        for I := 0 to Self.MemberCount - 1 do
          Agent.SendPacket(Self.MemberAt(I).SourceAddress,
                           Self.MemberAt(I).SourcePort,
                           Packet);
    finally
      S.Free;
    end;
  finally
    Packet.Free;
  end;
end;

procedure TIdRTPSession.SendDataTo(Data: TIdRTPPayload;
                                   Host: String;
                                   Port: Cardinal);
var
  Packet: TIdRTPPacket;
  S:      TMemoryStream;
begin
  Packet := TIdRTPPacket.Create(Self.Profile);
  try
    S := TMemoryStream.Create;
    try
      Data.PrintOn(S);
      S.Seek(0, soFromBeginning);
      Packet.PayloadType := Self.Profile.PayloadTypeFor(Data.Encoding);
      Packet.ReadPayload(S);

      Self.PrepareData(Packet);

      Self.Agent.SendPacket(Host, Port, Packet);
    finally
      S.Free;
    end;
  finally
    Packet.Free;
  end;
end;

function TIdRTPSession.Sender(SSRC: Cardinal): TIdRTPMember;
begin
  Result := Self.Senders.Member(SSRC);
end;

function TIdRTPSession.SenderAt(Index: Cardinal): TIdRTPMember;
begin
  Result := Self.Senders.MemberAt(Index);
end;

function TIdRTPSession.SenderCount: Cardinal;
begin
  Result := Self.Senders.Count;
end;

function TIdRTPSession.SendersContains(SSRC: Cardinal): Boolean;
begin
  Result := Self.Senders.Contains(SSRC);
end;

// Return the number of milliseconds until we must send the next RTCP
// (i.e., SR/RR) packet to the members of this session.
function TIdRTPSession.SendInterval: TDateTime;
begin
  Result := Self.DeterministicSendInterval(Self.IsSender)
          * Self.RandomTimeFactor
          / Self.CompensationFactor;
end;

//* TIdRTPSession Private methods **********************************************

function TIdRTPSession.AddAppropriateReport(Packet: TIdCompoundRTCPPacket): TIdRTCPReceiverReport;
begin
  if Self.IsSender then
    Result := Packet.AddSenderReport
  else
    Result := Packet.AddReceiverReport;
end;

procedure TIdRTPSession.AddControlSource(ID: Cardinal; Binding: TIdSocketHandle);
begin
  if not Self.Members.Contains(ID) then begin
    Self.Members.Add(ID);
  end;

  if not Self.Members.Member(ID).SentControl then begin
    Self.Members.Member(ID).ControlAddress := Binding.PeerIP;
    Self.Members.Member(ID).ControlPort    := Binding.PeerPort;
    Self.Members.Member(ID).SentControl    := true;
  end;
end;

procedure TIdRTPSession.AddControlSources(RTCP: TIdRTCPMultiSSRCPacket;
                                          Binding: TIdSocketHandle);
var
  I: Integer;
  IDs: TCardinalDynArray;
begin
  IDs := RTCP.GetAllSrcIDs;

  for I := Low(IDs) to High(IDs) do
    Self.AddControlSource(IDs[I], Binding);
end;

procedure TIdRTPSession.AddDataSource(ID: Cardinal; Binding: TIdSocketHandle);
  procedure AddToTable(ID: Cardinal; Table: TIdRTPMemberTable);
  begin
    if not Table.Contains(ID) then begin
      Table.Add(ID);
    end;

    if not Table.Member(ID).SentControl then begin
      Table.Member(ID).SourceAddress := Binding.PeerIP;
      Table.Member(ID).SourcePort    := Binding.PeerPort;
      Table.Member(ID).SentControl   := true;
    end;
  end;
begin
  AddToTable(ID, Self.Members);
  AddToTable(ID, Self.Senders);
end;

procedure TIdRTPSession.AddReports(Packet: TIdCompoundRTCPPacket);
var
  I, J:    Cardinal;
  Report:  TIdRTCPReceiverReport;
  NumSrcs: Cardinal;
begin
  Report := Self.AddAppropriateReport(Packet);

  I := 0;
  while (I < Self.SenderCount) do begin
    J := 0;
    NumSrcs := Min(High(TIdRTCPReceptionCount), Self.SenderCount - I);
    Report.ReceptionReportCount := NumSrcs;
    while (J < NumSrcs) do begin
      Report.Reports[J].SyncSrcID := Self.SenderAt(I).SyncSrcID;
      Inc(J);
      Inc(I);
    end;

    if (I < Self.SenderCount) then
      Report := Self.AddAppropriateReport(Packet);
  end;
end;

procedure TIdRTPSession.AddSourceDesc(Packet: TIdCompoundRTCPPacket);
var
  Chunk: TIdRTCPSrcDescChunk;
  SDES:  TIdRTCPSourceDescription;
begin
  SDES := Packet.AddSourceDescription;
  Chunk := SDES.AddChunk;
  Chunk.SyncSrcID := Self.CurrentSSRC;
  Chunk.AddCanonicalName(Self.CanonicalName);
end;

procedure TIdRTPSession.AdjustAvgRTCPSize(Control: TIdRTCPPacket);
begin
  Self.fAvgRTCPSize := Control.RealLength div 16
                     + (15*Self.AvgRTCPSize) div 16;
end;

procedure TIdRTPSession.AdjustTransmissionTime;
var
  CurrentTime: TDateTime;
begin
  CurrentTime := Now;

  Self.NextTransmissionTime := CurrentTime
           + (Self.MemberCount/Self.PreviousMemberCount) * (Self.NextTransmissionTime - CurrentTime);

  Self.PreviousTransmissionTime := CurrentTime
           - (Self.MemberCount/Self.PreviousMemberCount) * (CurrentTime - Self.PreviousTransmissionTime);

  Self.fPreviousMemberCount := Self.MemberCount;
end;

function TIdRTPSession.CompensationFactor: Double;
begin
  // cf RFC 3550, section 6.3.1:
  // The resulting value of T is divided by e-3/2=1.21828 to compensate
  // for the fact that the timer reconsideration algorithm converges to
  // a value of the RTCP bandwidth below the intended average.

  Result := Exp(1) - 1.5;
end;

function TIdRTPSession.DefaultAssumedMTU: Cardinal;
begin
  Result := 1500;
end;

function TIdRTPSession.DefaultMissedReportTolerance: Cardinal;
begin
  Result := 5;
end;

function TIdRTPSession.DefaultNoControlSentAvgRTCPSize: Cardinal;
begin
  Result := 20; // a small SDES
end;

function TIdRTPSession.DefaultReceiverBandwidthFraction: Double;
begin
  Result := 1 - Self.DefaultSenderBandwidthFraction;
end;

function TIdRTPSession.DefaultSenderBandwidthFraction: Double;
begin
  Result := 0.25;
end;

procedure TIdRTPSession.IncSentOctetCount(N: Cardinal);
begin
  Inc(Self.fSentOctetCount, N);
end;

procedure TIdRTPSession.IncSentPacketCount;
begin
  Inc(Self.fSentPacketCount);
end;

function TIdRTPSession.RandomTimeFactor: Double;
begin
  // We want a factor in the range [0.5, 1.5]
  Result := TIdRandomNumber.NextDouble + 0.5;
end;

procedure TIdRTPSession.RemoveSource(ID: Cardinal);
begin
  if Self.MembersContains(ID) then
    Self.RemoveMember(ID);

  if Self.SendersContains(ID) then
    Self.RemoveSender(ID);
end;

procedure TIdRTPSession.RemoveSources(Bye: TIdRTCPBye);
var
  I:   Integer;
  IDs: TCardinalDynArray;
begin
  IDs := Bye.GetAllSrcIDs;

  for I := Low(IDs) to High(IDs) do
    Self.RemoveSource(IDs[I]);

  Self.AdjustTransmissionTime;
end;

procedure TIdRTPSession.ResetSentOctetCount;
begin
  Self.fSentOctetCount := 0;
end;

procedure TIdRTPSession.ResetSentPacketCount;
begin
  Self.fSentPacketCount := 0;
end;

procedure TIdRTPSession.SetCurrentSSRC(const Value: Cardinal);
begin
  Self.fCurrentSSRC := Value;
  Self.ResetSentOctetCount;
  Self.ResetSentPacketCount;
end;

procedure TIdRTPSession.TransmissionTimeExpire(Sender: TObject);
var
  PresumedNextTransmissionTime: TDateTime;
begin
  Self.Lock.Acquire;
  try
    Self.RemoveTimedOutSenders;
    Self.RemoveTimedOutMembers;
    Self.AdjustTransmissionTime;

    PresumedNextTransmissionTime := Self.PreviousTransmissionTime
                                  + OneMillisecond*Self.SendInterval;

    if (PresumedNextTransmissionTime < Now) then
      Self.SendReport
    else begin
      // We must redraw the interval.  Don't reuse the
      // one computed above, since its not actually
      // distributed the same, as we are conditioned
      // on it being small enough to cause a packet to
      // be sent.
      Self.Timer.AddEvent(MilliSecondOfTheDay(Self.SendInterval),
                          Self.TransmissionTimeExpire);
    end;
  finally
    Self.Lock.Release
  end;
end;

//******************************************************************************
//* TIdRTPServer                                                               *
//******************************************************************************
//* TIdRTPServer Public methods ************************************************

constructor TIdRTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.FProfile := TIdAudioVisualProfile.Create;
  Self.FSession := TIdRTPSession.Create(Self, Self.Profile);

  Self.ThreadedEvent := true;

  Self.DefaultPort := 8000;
  Self.ControlPort := 8001;
end;

destructor TIdRTPServer.Destroy;
begin
  Self.Profile.Free;
  Self.Session.Free;

  inherited Destroy;
end;

function TIdRTPServer.JoinSession(Host: String; Port: Cardinal): TIdRTPSession;
var
  Pkt: TIdRTCPSourceDescription;
  S:   TStringStream;
begin
  Pkt := TIdRTCPSourceDescription.Create;
  try
    Pkt.AddChunk.AddCanonicalName('The Black Goat of a Thousand Young');
    Self.Session.PrepareControl(Pkt);

    S := TStringStream.Create('');
    try
      Pkt.PrintOn(S);
      Self.Send(Host, Port, S.DataString);
    finally
      S.Free;
    end;
  finally
    Pkt.Free;
  end;

  Result := Self.Session;
end;

procedure TIdRTPServer.SendPacket(Host: String;
                                  Port: Cardinal;
                                  Packet: TIdRTPBasePacket);
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Packet.PrintOn(S);
    Self.Send(Host, Port, S.DataString);
  finally
    S.Free;
  end;
end;

//* TIdRTPServer Protected methods *********************************************

procedure TIdRTPServer.DoUDPRead(AData: TStream;
                                 ABinding: TIdSocketHandle);
var
  Pkt: TIdRTPBasePacket;
begin
  inherited DoUDPRead(AData, ABinding);
  AData.Seek(0, soFromBeginning);

  Pkt := TIdRTPBasePacket.CreateFrom(AData, Self.Profile);
  try
    Pkt.ReadFrom(AData);

    if Pkt.IsRTP then
      Self.DoOnRTPRead(Pkt as TIdRTPPacket, ABinding)
    else
      Self.DoOnRTCPRead(Pkt as TIdRTCPPacket, ABinding);
  finally
    Pkt.Free;
  end;
end;

//* TIdRTPServer Private methods ***********************************************

procedure TIdRTPServer.DoOnRTCPRead(APacket: TIdRTCPPacket;
                                   ABinding: TIdSocketHandle);
begin
  Self.Session.ReceiveControl(APacket, ABinding);

  if Assigned(Self.OnRTCPRead) then
    Self.OnRTCPRead(Self, APacket, ABinding);
end;

procedure TIdRTPServer.DoOnRTPRead(APacket: TIdRTPPacket;
                                   ABinding: TIdSocketHandle);
begin
  Self.Session.ReceiveData(APacket, ABinding);

  if Assigned(Self.OnRTPRead) then
    Self.OnRTPRead(Self, APacket, ABinding);
end;

//******************************************************************************
//* TIdNullRTPPeer                                                             *
//******************************************************************************
//* TIdNullRTPPeer Public methods **********************************************

procedure TIdNullRTPPeer.SendPacket(Host: String;
                                  Port: Cardinal;
                                  Packet: TIdRTPBasePacket);
begin
end;

end.
