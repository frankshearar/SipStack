{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdRTP;

interface

uses
  IdConnectionBindings, IdRTP, IdTimerQueue, TestFramework, TestFrameworkRtp;

type
  TestFunctions = class(TTestRTP)
  private
    OldShortDateFormat: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDateTimeToNTPFractionsOfASecond;
    procedure TestDateTimeToNTPSeconds;
    procedure TestDateTimeToNTPTimestampExceptionalCases;
    procedure TestDateTimeToNTPTimestampFractionalValues;
    procedure TestDateTimeToNTPTimestampIntegralValues;
    procedure TestDateTimeToRTPTimestamp;
    procedure TestEncodeAsStringCardinal;
    procedure TestEncodeAsStringWord;
    procedure TestEof;
    procedure TestHtoNL;
    procedure TestHtoNS;
    procedure TestMultiplyCardinal;
    procedure TestNtoHL;
    procedure TestNtoHS;
    procedure TestPeekByte;
    procedure TestPeekWord;
    procedure TestReadByte;
    procedure TestReadCardinal;
    procedure TestReadRemainderOfStream;
    procedure TestReadRemainderOfStreamAsWideString;
    procedure TestReadRemainderOfStreamLong;
    procedure TestReadTimestamp;
    procedure TestReadString;
    procedure TestReadWord;
    procedure TestRoundUpToMultipleOfFour;
    procedure TestTwosComplement;
    procedure TestWriteCardinal;
    procedure TestWriteNTPTimestamp;
    procedure TestWriteString;
    procedure TestWriteWideString;
    procedure TestWriteWord;
  end;

  TPayloadTestCase = class(TTestRTP)
  protected
    Payload:  TIdRTPPayload;

    function PayloadType: TIdRTPPayloadClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestCreateFromBadEncodingName;
    procedure TestCopy;
    procedure TestCopyCopiesData;
    procedure TestEncodingName;
    procedure TestIsNull; virtual;
    procedure TestIsReserved; virtual;
  end;

  TestTIdNullPayload = class(TPayloadTestCase)
  protected
    function PayloadType: TIdRTPPayloadClass; override;
  published
    procedure TestIsNull; override;
  end;

  TestTIdRTPReservedPayload = class(TPayloadTestCase)
  protected
    function PayloadType: TIdRTPPayloadClass; override;
  published
    procedure TestIsReserved; override;
  end;

  TestTIdRTPRawPayload = class(TPayloadTestCase)
  published
    procedure TestEncodingName;
    procedure TestName;
  end;

  TestTIdRTPT140Payload = class(TPayloadTestCase)
  private
    Packet: TIdRTPT140Payload;
  protected
    function PayloadType: TIdRTPPayloadClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestInitialClockRate;
    procedure TestLength;
    procedure TestName;
    procedure TestPrintOn;
    procedure TestPrintOnWithRealUnicode;
    procedure TestReadFrom;
    procedure TestReadFromWithRealUnicode;
  end;

  TestTIdRTPTelephoneEventPayload = class(TPayloadTestCase)
  private
    Packet:          TIdRTPTelephoneEventPayload;
    SampleDurations: array[0..5] of Word;
  protected
    function PayloadType: TIdRTPPayloadClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestName;
    procedure TestNumberOfSamples;
    procedure TestPrintOnDuration;
    procedure TestPrintOnEvent;
    procedure TestPrintOnIsEnd;
    procedure TestPrintOnReadFromSymmetry;
    procedure TestPrintOnReservedBit;
    procedure TestPrintOnVolume;
    procedure TestReadFromDuration;
    procedure TestReadFromEvent;
    procedure TestReadFromIsEnd;
    procedure TestReadFromPrintOnSymmetry;
    procedure TestReadFromReservedBit;
    procedure TestReadFromVolume;
  end;

  TestTIdRTPProfile = class(TTestCase)
  private
    ArbPT:                   TIdRTPPayloadType;
    Profile:                 TIdRTPProfile;
    T140Encoding:            TIdRTPPayload;
    InterleavedT140Encoding: TIdRTPPayload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddEncoding;
    procedure TestAddEncodingByName;
    procedure TestAssign;
    procedure TestClear;
    procedure TestCount;
    procedure TestCreatePacketRTCP;
    procedure TestCreatePacketRTP;
    procedure TestEncodingFor;
    procedure TestEncodingForName;
    procedure TestFirstFreePayloadType;
    procedure TestHasEncoding;
    procedure TestHasPayloadType;
    procedure TestIsFull;
    procedure TestIsRTCPPayloadType;
    procedure TestPayloadTypeFor;
    procedure TestReservedEncodingMustntOverwriteOthers;
    procedure TestStreamContainsEncoding;
    procedure TestStreamContainsPayloadType;
  end;

  TestTIdAudioVisualProfile = class(TTestCase)
  private
    Profile: TIdAudioVisualProfile;

    procedure CheckRange(StartType, EndType: TIdRTPPayloadType);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestDefinedPayloads;
    procedure TestDynamicPayloadTypes;
    procedure TestReservedAndUnassignedPayloadTypes;
    procedure TestTransportDesc;
  end;

  TestTIdRTPHeaderExtension = class(TTestCase)
  private
    HeaderExtension: TIdRTPHeaderExtension;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPrintOn;
    procedure TestReadFrom;
  end;

  TestTIdRTPBasePacket = class(TTestCase)
  private
    Profile: TIdRTPProfile;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdRTCPReportBlock = class(TTestCase)
  private
    Block: TIdRTCPReportBlock;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPrintOn;
    procedure TestReadFrom;
  end;

  TestTIdRTPPacket = class(TTestRTP)
  private
    AVP:    TIdAudioVisualProfile;
    Packet: TIdRTPPacket;
    T140PT: TIdRTPPayloadType;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestCollidesWith;
    procedure TestGetAllSrcIDsNoCsrcs;
    procedure TestGetAllSrcIDsWithCsrcs;
    procedure TestIsRTCP;
    procedure TestIsRTP;
    procedure TestIsValidBogusVersion;
    procedure TestIsValidKnownPayloadType;
    procedure TestIsValidProfileDoesntAllowHeaderExtensions;
    procedure TestIsValidRealVersion;
    procedure TestIsValidSenderOrReceiverReport;
    procedure TestIsValidUnknownPayloadType;
    procedure TestPrintOn;
    procedure TestPrintOnHasExtension;
    procedure TestReadFromContributingSourceIdentifiers;
    procedure TestReadFromCsrcCount;
    procedure TestReadFromHasExtension;
    procedure TestReadFromHasPadding;
    procedure TestReadFromIsMarker;
    procedure TestReadFromPayloadType;
    procedure TestReadFromSequenceNo;
    procedure TestReadFromSynchronizationSourceIdentifier;
    procedure TestReadFromTimestamp;
    procedure TestReadFromVersion;
    procedure TestReadFromWithPadding;
    procedure TestReadFromPrintOnInverse;
    procedure TestReadPayloadPayload;
    procedure TestReadPayloadStream;
    procedure TestReadPayloadString;
    procedure TestRealLength;
  end;

  TestTIdRTCPPacket = class(TTestCase)
  published
    procedure TestRTCPType;
    procedure TestIsValidVersionNumber;
  end;

  TestTIdSrcDescChunkItem = class(TTestCase)
  published
    procedure TestItemType;
  end;

  TSrcDescChunkItemTestCase = class(TTestCase)
  protected
    Item: TIdSrcDescChunkItem;

    function ItemType: TIdSrcDescChunkItemClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPrintOn;
    procedure TestReadFrom;
    procedure TestRealLength;
  end;

  TestTIdSDESCanonicalName = class(TSrcDescChunkItemTestCase)
  protected
    function ItemType: TIdSrcDescChunkItemClass; override;
  published
    procedure TestID;
  end;

  TestTIdSDESUserName = class(TSrcDescChunkItemTestCase)
  protected
    function ItemType: TIdSrcDescChunkItemClass; override;
  published
    procedure TestID;
  end;

  TestTIdSDESEmail = class(TSrcDescChunkItemTestCase)
  protected
    function ItemType: TIdSrcDescChunkItemClass; override;
  published
    procedure TestID;
  end;

  TestTIdSDESPhone = class(TSrcDescChunkItemTestCase)
  protected
    function ItemType: TIdSrcDescChunkItemClass; override;
  published
    procedure TestID;
  end;

  TestTIdSDESLocation = class(TSrcDescChunkItemTestCase)
  protected
    function ItemType: TIdSrcDescChunkItemClass; override;
  published
    procedure TestID;
  end;

  TestTIdSDESTool = class(TSrcDescChunkItemTestCase)
  protected
    function ItemType: TIdSrcDescChunkItemClass; override;
  published
    procedure TestID;
  end;

  TestTIdSDESNote = class(TSrcDescChunkItemTestCase)
  protected
    function ItemType: TIdSrcDescChunkItemClass; override;
  published
    procedure TestID;
  end;

  TestTIdSDESPriv = class(TTestCase)
  private
    Item: TIdSDESPriv;

    procedure CheckPrintOn(Prefix, Data: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestID;
    procedure TestPrintOn;
    procedure TestPrintOnEmpty;
    procedure TestPrintOnNoPrefix;
    procedure TestPrintOnNoData;
    procedure TestReadFromNoPrefixNoData;
    procedure TestReadFromNoPrefixWithData;
    procedure TestReadFromPrefixNoData;
    procedure TestReadFromPrefixWithData;
    procedure TestRealLength;
    procedure TestSetData;
  end;

  TestTIdRTCPSrcDescChunk = class(TTestCase)
  private
    Chunk: TIdRTCPSrcDescChunk;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPrintOn;
    procedure TestPrintOnByteAlign;
    procedure TestReadFrom;
    procedure TestReadFromMultipleChunks;
    procedure TestRealLength;
  end;

  TRTCPPacketTestCase = class(TTestCase)
  protected
    Packet: TIdRTCPPacket;

    function PacketType: Byte; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestIsBye; virtual;
    procedure TestIsReceiverReport; virtual;
    procedure TestIsSenderReport; virtual;
    procedure TestIsSourceDescription; virtual;
    procedure TestIsRTCP;
    procedure TestIsRTP;
    procedure TestPacketType;
    procedure TestPrintOnHasPadding;
    procedure TestPrintOnPacketType;
    procedure TestPrintOnSyncSrcId;
    procedure TestPrintOnVersion;
    procedure TestPrintOnWithPadding;
    procedure TestReadFromHasPadding;
    procedure TestReadFromLength;
    procedure TestReadFromPacketType;
    procedure TestReadFromVersion;
  end;

  TestTIdRTCPReceiverReport = class(TRTCPPacketTestCase)
  private
    ReceiverReport: TIdRTCPReceiverReport;

    function  MultipleReportBlockReceiverReport: String;
    procedure ReadFromSampleReceiverReport;
  protected
    function PacketType: Byte; override;
  public
    procedure SetUp; override;
  published
    procedure TestGetAllSrcIDs;
    procedure TestIsReceiverReport; override;
    procedure TestPrintOnSyncSrcId;
    procedure TestPrintOnReceptionReportCount;
    procedure TestPrintOnReportBlocks;
    procedure TestRealLength;
    procedure TestReadFromReceptionReportCount;
    procedure TestReadFromReport;
    procedure TestReadFromReports;
    procedure TestReportAt;
  end;

  TestTIdRTCPSenderReport = class(TRTCPPacketTestCase)
  private
    SenderReport: TIdRTCPSenderReport;

    function  MultipleReportBlockSenderReport: String;
    procedure ReadFromSampleSenderReport;
  protected
    function PacketType: Byte; override;
  public
    procedure SetUp; override;
  published
    procedure TestGetAllSrcIDs;
    procedure TestIsSenderReport; override;
    procedure TestPrintOnSyncSrcId;
    procedure TestPrintOnNTPTimestamp;
    procedure TestPrintOnOctetCount;
    procedure TestPrintOnPacketCount;
    procedure TestPrintOnReceptionReportCount;
    procedure TestPrintOnReportBlocks;
    procedure TestPrintOnRTPTimestamp;
    procedure TestPrintOnWord;
    procedure TestReadFromNTPTimestamp;
    procedure TestReadFromOctetCount;
    procedure TestReadFromPacketCount;
    procedure TestReadFromReceptionReportCount;
    procedure TestReadFromReport;
    procedure TestReadFromReports;
    procedure TestReadFromRTPTimestamp;
    procedure TestReadFromSyncSrcID;
    procedure TestReadFromTruncatedStream;
    procedure TestRealLength;
    procedure TestReportAt;
  end;

  TestTIdRTCPSourceDescription = class(TRTCPPacketTestCase)
  private
    SrcDesc: TIdRTCPSourceDescription;
  protected
    function PacketType: Byte; override;
  public
    procedure SetUp; override;
  published
    procedure TestAddChunkAndChunkCount;
    procedure TestGetAllSrcIDs;
    procedure TestIsSourceDescription; override;
    procedure TestPrintOn;
    procedure TestPrintOnLength;
    procedure TestReadFromWithSingleCname;
  end;

  TestTIdRTCPBye = class(TRTCPPacketTestCase)
  private
    Bye:   TIdRTCPBye;
    Timer: TIdTimerQueue;
  protected
    function PacketType: Byte; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetAllSrcIDs;
    procedure TestIsBye; override;
    procedure TestPrepareForTransmission;
    procedure TestPrintOnLength;
    procedure TestPrintOnLengthAutomaticallyCalculated;
    procedure TestPrintOnMultipleSources;
    procedure TestPrintOnReason;
    procedure TestPrintOnSyncSrcId;
    procedure TestReadFrom;
    procedure TestReadFromLongReason;
    procedure TestReadFromReason;
    procedure TestReadFromReasonLength;
    procedure TestReadFromSourceCount;
    procedure TestReadFromSources;
    procedure TestRealLength;
  end;

  TestTIdRTCPApplicationDefined = class(TRTCPPacketTestCase)
  private
    AppDef: TIdRTCPApplicationDefined;
  protected
    function PacketType: Byte; override;
  public
    procedure SetUp; override;
  published
    procedure TestPrintOnData;
    procedure TestPrintOnEmptyData;
    procedure TestPrintOnLength;
    procedure TestPrintOnName;
    procedure TestReadFromData;
    procedure TestReadFromName;
    procedure TestReadFromSyncSrcId;
    procedure TestRealLength;
    procedure TestSetData;
    procedure TestSetName;
  end;

  TestTIdCompoundRTCPPacket = class(TTestCase)
  private
    Packet: TIdCompoundRTCPPacket;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestCopy;
    procedure TestFirstPacket;
    procedure TestHasBye;
    procedure TestHasReceiverReport;
    procedure TestHasSourceDescription;
    procedure TestIsRTCP;
    procedure TestIsRTP;
    procedure TestIsValidFirstPacketAnAppDef;
    procedure TestIsValidFirstPacketAnRR;
    procedure TestIsValidFirstPacketAnSDES;
    procedure TestIsValidFirstPacketAnSR;
    procedure TestIsValidPacketFirstPacketPadded;
    procedure TestPrintOn;
    procedure TestReadFrom;
    procedure TestReadFromEmptyStream;
  end;

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
    procedure TestDelaySinceLastSenderReport;
    procedure TestInitSequence;
    procedure TestLastSenderReport;
    procedure TestPacketLossCount;
    procedure TestPacketLossFraction;
    procedure TestSetControlBinding;
    procedure TestSetDataBinding;
    procedure TestUpdateJitter;
  end;

  TestTIdRTPMemberTable = class(TTestCase)
  private
    Binding: TIdConnectionBindings;
    Host:    String;
    Members: TIdRTPMemberTable;
    Port:    Cardinal;
    SSRC:    Cardinal;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndFind;
    procedure TestAddReceiver;
    procedure TestContains;
    procedure TestContainsReceiver;
    procedure TestFind;
    procedure TestFindReceiver;
    procedure TestRemove;
    procedure TestRemoveNonMember;
    procedure TestRemoveAll;
    procedure TestReceiverCount;
    procedure TestRemoveTimedOutMembers;
    procedure TestRemoveTimedOutSendersExceptFor;
    procedure TestSenderCount;
    procedure TestSetControlBinding;
    procedure TestSetDataBinding;
  end;

  TestTIdRTPSenderTable = class(TTestCase)
  private
    Members: TIdRTPMemberTable;
    Senders: TIdRTPSenderTable;
    SSRC:    Cardinal;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndFind;
    procedure TestAddSetsIsSender;
    procedure TestContains;
    procedure TestMemberAt;
    procedure TestMemberAtAnotherTest;
    procedure TestRemove;
    procedure TestRemoveAll;
  end;

  TestTIdBaseRTPAbstractPeer = class(TTestCase)
  private
    Peer: TIdMockRTPPeer;
    RTCP: TIdRTCPPacket;
    RTP:  TIdRTPPacket;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddListener;
    procedure TestAddSendListener;
    procedure TestRemoveListener;
    procedure TestRemoveSendListener;
  end;

  TestTIdRTPPeerRegistry = class(TTestCase)
  private
    Agent: TIdMockRTPPeer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFindServer;
    procedure TestFindServerNoSuchRegisteredObject;
    procedure TestFindServerNotAnRTPPeer;
    procedure TestServerOn;
    procedure TestServerRunningOn;
  end;

  TRTPSessionTestCase = class(TTestCase)
  protected
    Agent:   TIdMockRTPPeer;
    Session: TIdRTPSession;
    Profile: TIdRTPProfile;
    T140PT:  Cardinal;
    Timer:   TIdDebugTimerQueue;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestSessionDelegationMethods = class(TRTPSessionTestCase)
  private
    SSRC: Cardinal;
  public
    procedure SetUp; override;
  published
    procedure TestAcceptableSSRC;
    procedure TestAddMember;
    procedure TestAddReceiver;
    procedure TestAddSender;
    procedure TestAddSenderAddsMember;
    procedure TestIsMember;
    procedure TestIsSender;
    procedure TestIsSenderSelf;
    procedure TestPrepareRTP;
    procedure TestPrepareSR;
  end;

  TSessionDataTestCase = class(TRTPSessionTestCase)
  protected
    Binding:     TIdConnectionBindings;
    Data:        TIdRTPPacket;
    RTCPBinding: TIdConnectionBindings;
    RR:          TIdRTCPReceiverReport;

    procedure ValidateSource(Member: TIdRTPMember);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdRTPSession = class(TSessionDataTestCase,
                            IIdRTPDataListener)
  private
    Member: TIdRTPMember;
    NewDataArrived: Boolean;

    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdConnectionBindings);
  public
    procedure SetUp; override;
  published
    procedure TestAddListener;
    procedure TestJoinSendsSenderReport;
    procedure TestReceiveRTPFromValidatedSourceNotifiesListeners;
    procedure TestRemoveListener;
    procedure TestSendReportSchedulesNextSend;
  end;

  TestSessionSequenceNumberRules = class(TSessionDataTestCase)
  private
    Member: TIdRTPMember;
  public
    procedure SetUp; override;
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

  TestSessionReportRules = class(TSessionDataTestCase)
  private
    function AddNewSender: TIdRTPMember;
  published
    // still got to do a test for LARGE numbers of members - packet size > MTU (1500?)
    procedure TestReportDetailsProperData;
    procedure TestReportDetailsUnvalidatedSource;
    procedure TestReportWith0Senders;
    procedure TestReportWith2Senders;
    procedure TestReportWith31Senders;
    procedure TestReportWith32Senders;
    procedure TestSentOctetCount;
    procedure TestSentPacketCount;
    procedure TestSSRCChangeResetsSentOctetCount;
    procedure TestSSRCChangeResetsSentPacketCount;
  end;

  TestSessionSendReceiveRules = class(TSessionDataTestCase)
  private
    Bye: TIdRTCPBye;
    SR:  TIdRTCPSenderReport;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddReceiverGetsItsSsrc;
    procedure TestCollisionTriggersBye;
    procedure TestInitialState;
    procedure TestInitialDeterministicSendInterval;
    procedure TestReceiveRTPAddsMembers;
    procedure TestReceiveByeMarksMembers;
    procedure TestReceiveByeOnNewSessionDoesNothing;
    procedure TestReceiveRTPIncreasesSenderCount;
    procedure TestReceiveRTCPAffectsAvgRTCPSize;
    procedure TestReceiveSrcDescAddsAllSources;
    procedure TestRTCPDoesntAddSender;
    procedure TestSendDataToInvalidSender;
    procedure TestSendDataWhenOtherMembersHaventSentData;
    procedure TestSendDataWhenNoOtherMembersHaveJoined;
    procedure TestDeterministicSendInterval10MembersAndNotSender;
    procedure TestDeterministicSendInterval10MembersAndSender;
    procedure TestDeterministicSendIntervalMinimumInterval;
    procedure TestDeterministicSendIntervalWithZeroBandwidth;
    procedure TestSendControlDoesntSendToSelf;
  end;

  TestTIdRTPPacketBuffer = class(TTestCase)
  private
    Profile: TIdRTPProfile;
    Q:       TIdRTPPacketBuffer;

    function CreateExamplePacket(const Timestamp: Cardinal): TIdRTPPacket;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBadlyOrderedStream;
    procedure TestOutOfOrderPacket;
    procedure TestWellOrderedStream;
  end;

  TRTPListenerTestCase = class(TTestCase)
  protected
    Binding: TIdConnectionBindings;

    procedure CheckEquals(Expected,
                          Received: TIdConnectionBindings;
                          Msg: String); overload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdRTPListenerReceiveRTCPMethod = class(TRTPListenerTestCase)
  private
    Method:  TIdRTPListenerReceiveRTCPMethod;
    Packet:  TIdRTCPPacket;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdRTPListenerReceiveRTPMethod = class(TRTPListenerTestCase)
  private
    Method:  TIdRTPListenerReceiveRTPMethod;
    Packet:  TIdRTPPacket;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdRTPDataListenerNewDataMethod = class(TRTPListenerTestCase)
  private
   Data:   TIdRTPPayload;
   Method: TIdRTPDataListenerNewDataMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TIdRTPWaitClass = class of TIdRTPWait;

  TTestCaseRTP = class(TTestCase)
  protected
    Agent:   TIdMockRTPPeer;
    Profile: TIdRTPProfile;
    Session: TIdRTPSession;
    Timer:   TIdDebugTimerQueue;
    Wait:    TIdRTPWait;

    procedure CheckTriggerDoesNothing(Msg: String); virtual;
    function  WaitType: TIdRTPWaitClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTriggerWithNonexistentSession;
    procedure TestTriggerWithWrongTypeOfObject;
  end;

  TestTIdRTPReceivePacketWait = class(TTestCaseRTP)
  private
    Binding:     TIdConnectionBindings;
    Control:     TIdRTCPPacket;
    Data:        TIdRTPPacket;
    Listener:    TIdRTPTestRTPDataListener;
    MemberCount: Cardinal;
    Payload:     TIdRTPT140Payload;
  protected
    procedure CheckTriggerDoesNothing(Msg: String); override;
    function  WaitType: TIdRTPWaitClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTriggerRTCP;
    procedure TestTriggerRTP;
  end;

  TSenderReportTestCase = class(TTestCaseRTP)
  protected
    procedure CheckTriggerDoesNothing(Msg: String); override;
  public
    procedure SetUp; override;
  published
    procedure TestTrigger;
  end;

  TestTIdRTPTransmissionTimeExpire = class(TSenderReportTestCase)
  protected
    function WaitType: TIdRTPWaitClass; override;
  end;

  TestTIdRTPSenderReportWait = class(TSenderReportTestCase)
  protected
    function WaitType: TIdRTPWaitClass; override;
  end;

  TestTIdRTPSendDataWait = class(TTestCaseRTP)
  private
    Payload: TIdRTPT140Payload;
  protected
    procedure CheckTriggerDoesNothing(Msg: String); override;
    function  WaitType: TIdRTPWaitClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestTrigger;
  end;

// Most of the values are thumb-sucked.
// We calculate Length by hand, and RFC3550 gives us the value for packet type
const
  SampleReportBlock    = #$DE#$AD#$BE#$EF  // SSRC_1
                       + #$0F#$00#$FE#$ED  // Fraction lost, Cumulative packet loss
                       + #$10#$11#$12#$13  // highest received sequence no
                       + #$00#$FE#$DF#$ED  // interarrival jitter
                       + #$EA#$7E#$A7#$EA  // last SR
                       + #$00#$00#$FF#$EF; // delay since last SR
  SampleReceiverReport = #$81#$C9#$00#$0C  // version, padding, packet type length
                       + #$DE#$CA#$FB#$AD  // SSRC
                       + SampleReportBlock;
  SampleSenderReport   = #$81#$C8#$00#$0C  // version, padding, packet type length
                       + #$DE#$CA#$FB#$AD  // SSRC
                       + #$A1#$A2#$A3#$A4  // NTP timestamp
                       + #$A5#$A6#$A7#$A8  // NTP timestamp
                       + #$A9#$AA#$AB#$AC  // RTP timestamp
                       + #$00#$00#$0F#$ED  // sender's packet count
                       + #$00#$00#$F0#$0D  // sender's octet count
                       + SampleReportBlock;

implementation

uses
  Classes, DateUtils, IdRandom, IdRegisteredObject, IdRtpServer, IdSimpleParser,
  IdSystem, SysUtils, Types;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTP unit tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdNullPayload.Suite);
  Result.AddTest(TestTIdRTPReservedPayload.Suite);
  Result.AddTest(TestTIdRTPTelephoneEventPayload.Suite);
  Result.AddTest(TestTIdRTPT140Payload.Suite);
  Result.AddTest(TestTIdRTPProfile.Suite);
  Result.AddTest(TestTIdAudioVisualProfile.Suite);
  Result.AddTest(TestTIdRTPHeaderExtension.Suite);
  Result.AddTest(TestTIdRTCPReportBlock.Suite);
  Result.AddTest(TestTIdRTPBasePacket.Suite);
  Result.AddTest(TestTIdRTPPacket.Suite);
  Result.AddTest(TestTIdRTCPPacket.Suite);
  Result.AddTest(TestTIdSrcDescChunkItem.Suite);
  Result.AddTest(TestTIdSDESCanonicalName.Suite);
  Result.AddTest(TestTIdSDESUserName.Suite);
  Result.AddTest(TestTIdSDESEmail.Suite);
  Result.AddTest(TestTIdSDESPhone.Suite);
  Result.AddTest(TestTIdSDESLocation.Suite);
  Result.AddTest(TestTIdSDESTool.Suite);
  Result.AddTest(TestTIdSDESNote.Suite);
  Result.AddTest(TestTIdSDESPriv.Suite);
  Result.AddTest(TestTIdRTCPSrcDescChunk.Suite);
  Result.AddTest(TestTIdRTCPReceiverReport.Suite);
  Result.AddTest(TestTIdRTCPSenderReport.Suite);
  Result.AddTest(TestTIdRTCPSourceDescription.Suite);
  Result.AddTest(TestTIdRTCPBye.Suite);
  Result.AddTest(TestTIdRTCPApplicationDefined.Suite);
  Result.AddTest(TestTIdCompoundRTCPPacket.Suite);
  Result.AddTest(TestTIdRTPMember.Suite);
  Result.AddTest(TestTIdRTPMemberTable.Suite);
  Result.AddTest(TestTIdRTPSenderTable.Suite);
  Result.AddTest(TestTIdBaseRTPAbstractPeer.Suite);
  Result.AddTest(TestTIdRTPPeerRegistry.Suite);
  Result.AddTest(TestSessionDelegationMethods.Suite);
  Result.AddTest(TestSessionSequenceNumberRules.Suite);
  Result.AddTest(TestTIdRTPSession.Suite);
  Result.AddTest(TestSessionReportRules.Suite);
  Result.AddTest(TestSessionSendReceiveRules.Suite);
  Result.AddTest(TestTIdRTPPacketBuffer.Suite);
  Result.AddTest(TestTIdRTPListenerReceiveRTCPMethod.Suite);
  Result.AddTest(TestTIdRTPListenerReceiveRTPMethod.Suite);
  Result.AddTest(TestTIdRTPDataListenerNewDataMethod.Suite);
  Result.AddTest(TestTIdRTPReceivePacketWait.Suite);
  Result.AddTest(TestTIdRTPTransmissionTimeExpire.Suite);
  Result.AddTest(TestTIdRTPSenderReportWait.Suite);
  Result.AddTest(TestTIdRTPSendDataWait.Suite);
end;

function ShowEncoded(S: String): String;
var
  I: Integer;
begin
  Result := '';

  for I := 1 to Length(S) do
    Result := Result + '#$' + IntToHex(Ord(S[I]), 2);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Public methods ***********************************************

procedure TestFunctions.SetUp;
begin
  inherited SetUp;

  Self.OldShortDateFormat := ShortDateFormat;

  ShortDateFormat := 'yyyy/mm/dd hh:mm:ss';
end;

procedure TestFunctions.TearDown;
begin
  ShortDateFormat := Self.OldShortDateFormat;

  inherited TearDown;
end;

//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestDateTimeToNTPFractionsOfASecond;
var
  JanOne1900: TDateTime;
begin
  JanOne1900 := 2;
  CheckEquals(0,
              DateTimeToNTPFractionsOfASecond(JanOne1900),
              '1900/01/01 00:00:00');
  CheckEquals(IntToHex($80000000, 8),
              IntToHex(DateTimeToNTPFractionsOfASecond(JanOne1900 + 0.5*OneSecond), 8),
              '1900/01/01 00:00:0.500');
  CheckEquals(IntToHex($80000000, 8),
              IntToHex(DateTimeToNTPFractionsOfASecond(EncodeDateTime(1999, 1, 1, 0, 0, 0, 500)), 8),
              '1999/01/01 00:00:0.500');
  CheckEquals(IntToHex($40000000, 8),
              IntToHex(DateTimeToNTPFractionsOfASecond(EncodeDateTime(1999, 1, 1, 0, 0, 0, 250)), 8),
              '1999/01/01 00:00:0.250');

  CheckEquals(IntToHex(429496728, 8),
              IntToHex(DateTimeToNTPFractionsOfASecond(EncodeDateTime(1999, 1, 1, 0, 0, 0, 100)), 8),
              '1999/01/01 00:00:0.1');

  // Squeak calculates the fractional part of 0.001 as 4294966. However, it
  // calculates 1/1000 exactly, not as a floating point.
  CheckEquals(IntToHex(4294964, 8),
              IntToHex(DateTimeToNTPFractionsOfASecond(EncodeDateTime(1999, 1, 1, 0, 0, 0, 1)), 8),
              '1999/01/01 00:00:0.001');
end;

procedure TestFunctions.TestDateTimeToNTPSeconds;
var
  AprTwelve2002: Cardinal;
  JanOne1900:    TDateTime;
  ExpectedTime:  Cardinal;
  ReceivedTime:  Cardinal;
begin
  JanOne1900    := 2;
  AprTwelve2002 := 37356;

  CheckEquals(0,
              DateTimeToNTPSeconds(JanOne1900),
              '1900/01/01 00:00:00');
  CheckEquals(1,
              DateTimeToNTPSeconds(JanOne1900 + OneSecond),
              '1900/01/01 00:00:01');
  CheckEquals(4*SecsPerDay + 3600 + 120 + 1,
              DateTimeToNTPSeconds(EncodeDateTime(1900, 1, 5, 1, 2, 1, 0)),
              '1900/01/05 01:02:01');

  ExpectedTime := MultiplyCardinal(AprTwelve2002, SecsPerDay) + 14*3600 + 59*60 + 59;

  ReceivedTime := DateTimeToNTPSeconds(EncodeDateTime(2002, 4, 12, 14, 59, 59, 999));
  CheckEquals(IntToHex(ExpectedTime, 8),
              IntToHex(ReceivedTime, 8),
              '2002/04/12 14:59:59.999');
end;

procedure TestFunctions.TestDateTimeToNTPTimestampExceptionalCases;
begin
  try
    DateTimeToNTPTimestamp(1);
  except
    on EConvertError do;
  end;

  try
    DateTimeToNTPTimestamp(0);
  except
    on EConvertError do;
  end;

  try
    DateTimeToNTPTimestamp(-1);
  except
    on EConvertError do;
  end;
end;

procedure TestFunctions.TestDateTimeToNTPTimestampFractionalValues;
var
  JanOne1900: TDateTime;
begin

  JanOne1900 := 2;

  CheckEquals(IntToHex($80000000, 8),
              IntToHex(DateTimeToNTPTimestamp(JanOne1900 + 0.5*OneSecond).FractionalPart, 8),
              '1900/01/01 00:00:00.5, FractionalPart');
  CheckEquals(IntToHex($40000000, 8),
              IntToHex(DateTimeToNTPTimestamp(JanOne1900 + 0.25*OneSecond).FractionalPart, 8),
              '1900/01/01 00:00:00.25, FractionalPart');

  CheckEquals(IntToHex($ffbe76c8, 8),
              IntToHex(DateTimeToNTPTimestamp(StrToDateTime('2002/04/12 14:59:59.999')).FractionalPart, 8),
              '2002/04/12 14:59:59.999, FractionalPart');
end;

procedure TestFunctions.TestDateTimeToNTPTimestampIntegralValues;
var
  AprTwelve2002: Cardinal;
  JanOne1900:    TDateTime;
  ExpectedTime:  Cardinal;
  ReceivedTime:  Cardinal;
begin
  JanOne1900    := 2;
  AprTwelve2002 := 37356;

  CheckEquals(0,
              DateTimeToNTPTimestamp(JanOne1900).IntegerPart,
              '1900/01/01 00:00:00, IntegerPart');
  CheckEquals(1,
              DateTimeToNTPTimestamp(JanOne1900 + OneSecond).IntegerPart,
              '1900/01/01 00:00:01, IntegerPart');
  CheckEquals(4*SecsPerDay + 3600 + 120 + 1,
              DateTimeToNTPTimestamp(StrToDateTime('1900/01/05 01:02:01')).IntegerPart,
              '1900/01/05 01:02:01, IntegerPart');
  CheckEquals(0,
              DateTimeToNTPTimestamp(StrToDateTime('1900/01/05 01:02:01')).FractionalPart,
              '1900/01/05 01:02:01, FractionalPart');

  ExpectedTime := MultiplyCardinal(AprTwelve2002, SecsPerDay) + 14*3600 + 59*60 + 59;
  ReceivedTime := DateTimeToNTPTimestamp(StrToDateTime('2002/04/12 14:59:59.999')).IntegerPart;
  CheckEquals(IntToHex(ExpectedTime, 8),
              IntToHex(ReceivedTime, 8),
              '2002/04/12 14:59:59.999, IntegerPart');
end;

procedure TestFunctions.TestDateTimeToRTPTimestamp;
begin
  CheckEquals(0,
              DateTimeToRTPTimestamp(0, 0),
              'Zero clock rate');

  try
    DateTimeToRTPTimestamp(-1, 0);
    Fail('Negative timestamp not rejected');
  except
    on EConvertError do;
  end;

  CheckEquals(0,
              DateTimeToRTPTimestamp(0, 100),
              'Time 0, Clock rate 100');
  CheckEquals(100,
              DateTimeToRTPTimestamp(OneSecond, 100),
              'Time 1s, Clock rate 100');
  CheckEquals(22000,
              DateTimeToRTPTimestamp(OneSecond, 22000),
              'Time 1s, Clock rate 22000');
  CheckEquals(2200000,
              DateTimeToRTPTimestamp(100*OneSecond, 22000),
              'Time 100s, Clock rate 22000');
end;

procedure TestFunctions.TestEncodeAsStringCardinal;
begin
  CheckEquals('#$00#$00#$00#$00',
              ShowEncoded(EncodeAsString(Cardinal(0))),
              '0');

  CheckEquals('#$01#$02#$03#$04',
              ShowEncoded(EncodeAsString($01020304)),
              '$01020304');
end;

procedure TestFunctions.TestEncodeAsStringWord;
begin
  CheckEquals('#$00#$00',
              ShowEncoded(EncodeAsString(Word(0))),
              '0');

  CheckEquals('#$01#$02',
              ShowEncoded(EncodeAsString($0102)),
              '$0102');
end;

procedure TestFunctions.TestEof;
var
  B: Byte;
  S: TStringStream;
begin
  S := TStringStream.Create(#0#0);
  try
    Check(not Eof(S), 'Beginning of stream');
    S.Read(B, 1);
    Check(not Eof(S), 'First byte');
    S.Read(B, 1);
    Check(Eof(S), 'Second byte');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestHtoNL;
begin
  CheckEquals(0,         HtoNL(0),         '0');
  CheckEquals($01020304, HtoNL($04030201), '$04030201');
end;

procedure TestFunctions.TestHtoNS;
begin
  CheckEquals(0,     HtoNS(0),     '0');
  CheckEquals($0102, HtoNS($0201), '$0201');
end;

procedure TestFunctions.TestMultiplyCardinal;
begin
  CheckEquals($FE,        MultiplyCardinal($7F, 2),       '$7F * $02');
  CheckEquals($17D,       MultiplyCardinal($7F, 3),       '$7F * $03');
  Check      ($FE000000 = MultiplyCardinal($7F000000, 2), '$7F000000 * $02');

  try
    MultiplyCardinal($7F000000, 3);
    Fail('Failed to produce an Integer Overflow');
  except
    on EIntOverflow do;
  end;
end;

procedure TestFunctions.TestNtoHL;
var
  I: Integer;
  N: Cardinal;
begin
  CheckEquals(0,         NtoHL(0),         '0');
  CheckEquals($01020304, NtoHL($04030201), '$04030201');

  for I := 1 to 1000 do begin
    N := Abs(Random(High(Integer))); // Random returns an Integer
    CheckEquals(N, HtoNL(NtoHL(N)), IntToStr(N));
  end;
end;

procedure TestFunctions.TestNtoHS;
var
  I: Integer;
  N: Word;
begin
  CheckEquals(0,     NtoHS(0),      '0');
  CheckEquals($0304, NtoHS($0403), '$0403');

  for I := 1 to 1000 do begin
    N := Abs(Random(High(Word))); // Random returns an Integer
    CheckEquals(N, HtoNS(NtoHS(N)), IntToStr(N));
  end;
end;

procedure TestFunctions.TestPeekByte;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$11);
  try
    CheckEquals($11,
                PeekByte(S),
                'Byte incorrectly peeked');
    CheckEquals($11,
                ReadByte(S),
                'Peek read from the stream, not just peeked');
  finally
    S.Free;
  end;

  S := TStringStream.Create('');
  try
    CheckEquals(0, PeekByte(S), 'Peeking on a truncated stream');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestPeekWord;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$11#$22);
  try
    CheckEquals($1122,
                PeekWord(S),
                'Word incorrectly peeked');
    CheckEquals($1122,
                ReadWord(S),
                'Peek read from the stream, not just peeked');
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$11);
  try
    CheckEquals($0000,
                PeekWord(S),
                'Word incorrectly peeked');
  finally
    S.Free;
  end;

  S := TStringStream.Create('');
  try
    CheckEquals(0, PeekByte(S), 'Peeking on a truncated stream');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestReadByte;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$11);
  try
    CheckEquals($11, ReadByte(S), 'Byte incorrectly read');
  finally
    S.Free;
  end;

  S := TStringStream.Create('');
  try
    try
      ReadByte(S);
      Fail('Failed to bail out on truncated stream');
    except
      on EStreamTooShort do;
    end;
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestReadCardinal;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$11#$22#$33#$44);
  try
    CheckEquals($11223344,
                ReadCardinal(S),
                'Cardinal incorrectly read - check byte order');
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$00#$00#$00);
  try
    try
      ReadCardinal(S);
      Fail('Failed to bail out on truncated stream');
    except
      on EStreamTooShort do;
    end;
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestReadRemainderOfStream;
var
  C: Char;
  S: TStringStream;
begin
  S := TStringStream.Create('1234567890');
  try
    S.Read(C, 1);
    S.Read(C, 1);
    S.Read(C, 1);

    CheckEquals('4567890', ReadRemainderOfStream(S), 'ReadRemainderOfStream');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestReadRemainderOfStreamAsWideString;
var
  C: Char;
  S: TStringStream;
  W: WideString;
begin
  S := TStringStream.Create(#0'1'#0'2'#0'3'#0'4'#0'5'#0'6'#0'7'#0'8'#0'9'#0'0');
  try
    S.Read(C, 1);
    S.Read(C, 1);

    W := ReadRemainderOfStreamAsWideString(S);

    CheckEqualsW('234567890', W, 'Buffer');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestReadRemainderOfStreamLong;
var
  C:   Char;
  S:   TStringStream;
  Src: String;
begin
  while (Length(Src) < 1000) do
    Src := Src + '0123456789';

  S := TStringStream.Create(Src);
  try
    S.Read(C, 1);
    S.Read(C, 1);
    S.Read(C, 1);

    CheckEquals(Length(Src) - 3,
                Length(ReadRemainderOfStream(S)),
                'ReadRemainderOfStream, length of remainder');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestReadTimestamp;
var
  S: TStringStream;
  T: TIdNTPTimestamp;
begin
  S := TStringStream.Create(#$11#$22#$33#$44#$55#$66#$77#$88);
  try
    ReadNTPTimestamp(S, T);
    CheckEquals($11223344, T.IntegerPart,    'IntegerPart');
    CheckEquals($55667788, T.FractionalPart, 'FractionalPart');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestReadString;
var
  Src: TStringStream;
begin
  Src := TStringStream.Create('onetwothree');
  try
    CheckEquals('one', ReadString(Src, 3), 'First read');
    CheckEquals('two', ReadString(Src, 3), 'Second read');

    try
      ReadString(Src, 1000);
    except
      on EStreamTooShort do;
    end;
  finally
    Src.Free;
  end;
end;

procedure TestFunctions.TestReadWord;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$11#$22);
  try
    CheckEquals($1122,
                ReadWord(S),
                'Word incorrectly read - check byte order');
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$00);
  try
    try
      ReadWord(S);
      Fail('Failed to bail out on truncated stream');
    except
      on EStreamTooShort do;
    end;
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestRoundUpToMultipleOfFour;
begin
  CheckEquals(0, RoundUpToMultipleOfFour(0), '0');
  CheckEquals(4, RoundUpToMultipleOfFour(1), '1');
  CheckEquals(4, RoundUpToMultipleOfFour(2), '2');
  CheckEquals(4, RoundUpToMultipleOfFour(3), '3');
  CheckEquals(4, RoundUpToMultipleOfFour(4), '4');
  CheckEquals(8, RoundUpToMultipleOfFour(5), '5');
end;

procedure TestFunctions.TestTwosComplement;
begin
  CheckEquals(IntToHex(0, 16),
              IntToHex(TwosComplement(0), 16),
              '0');
  CheckEquals(IntToHex(Low(Int64) + 1, 16),
              IntToHex(TwosComplement(High(Int64)), 16),
              'High(Int64)');
  CheckEquals(IntToHex($fffffffffffffffb, 16),
              IntToHex(TwosComplement(5), 16),
              '5');
end;

procedure TestFunctions.TestWriteCardinal;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    WriteCardinal(S, $11223344);

    CheckEquals(#$11#$22#$33#$44,
                S.DataString,
                'Cardinal incorrectly written - check byte order');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestWriteNTPTimestamp;
var
  T: TIdNTPTimestamp;
  S: TStringStream;
begin
  T.IntegerPart    := $decafbad;
  T.FractionalPart := $beeff00d;

  S := TStringStream.Create('');
  try
    WriteNTPTimestamp(S, T);

    CheckEquals(#$de#$ca#$fb#$ad#$be#$ef#$f0#$0d,
                S.DataString,
                'Timestamp incorrectly written - check byte order');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestWriteString;
const
  TestStrings: array[1..4] of String = ('',
                                        'Cthulhu',
                                        #0'Azathoth'#0,
                                        #$be#$ef#$ca'ke');
var
  I: Integer;
  S: TStringStream;
begin
  for I := Low(TestStrings) to High(TestStrings) do begin
    S := TStringStream.Create('');
    try
      WriteString(S, TestStrings[I]);

      CheckEquals(TestStrings[I],
                  S.DataString,
                  'String incorrectly written');
    finally
      S.Free;
    end;
  end;
end;

procedure TestFunctions.TestWriteWideString;
const
  TestStrings: array[1..4] of WideString = ('',
                                            'Cthulhu',
                                            #0'Azathoth'#0,
                                            #$be#$ef#$ca'ke');
var
  I: Integer;
  S: TStringStream;
begin
  for I := Low(TestStrings) to High(TestStrings) do begin
    S := TStringStream.Create('');
    try
      WriteWideString(S, TestStrings[I]);

      CheckUnicode(TestStrings[I],
                   S.DataString,
                   'String incorrectly written');
    finally
      S.Free;
    end;
  end;
end;

procedure TestFunctions.TestWriteWord;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    WriteWord(S, $1122);
    CheckEquals(#$11#$22,
                S.DataString,
                'Word incorrectly written - check byte order');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TPayloadTestCase                                                           *
//******************************************************************************
//* TPayloadTestCase Public methods ********************************************

procedure TPayloadTestCase.SetUp;
begin
  inherited SetUp;

  Self.Payload := Self.PayloadType.Create;
end;

procedure TPayloadTestCase.TearDown;
begin
  Self.Payload.Free;

  inherited TearDown;
end;

//* TPayloadTestCase Protected methods *****************************************

function TPayloadTestCase.PayloadType: TIdRTPPayloadClass;
begin
  Result := TIdRTPPayload;
  Fail(Self.ClassName + ' must override PayloadType');
end;

//* TPayloadTestCase Published methods *****************************************

procedure TPayloadTestCase.TestCreate;
var
  NewPayload: TIdRTPPayload;
begin
  NewPayload := TIdRTPPayload.CreatePayload(Self.Payload.EncodingName);
  try
    CheckEquals(Self.Payload.EncodingName,
                NewPayload.EncodingName,
                'EncodingName');
    CheckEquals(Self.Payload.ClockRate,
                NewPayload.ClockRate,
                'ClockRate');
    CheckEquals(Self.Payload.Parameters,
                NewPayload.Parameters,
                'Parameters');
  finally
    NewPayload.Free;
  end;
end;

procedure TPayloadTestCase.TestCreateFromBadEncodingName;
begin
  try
    TIdRTPPayload.CreatePayload(T140EncodingName);
    Fail('Failed to bail out on malformed encoding name (no clock rate)');
  except
    on EBadEncodingName do;
  end;
end;

procedure TPayloadTestCase.TestCopy;
var
  Copy: TIdRTPPayload;
begin
  Copy := Self.Payload.Copy;
  try
    Check(Copy <> nil,
          'Copy returned the nil pointer');
    Check(Copy <> Self.Payload,
          'Copy returned the original object, not a copy');
    Check(Copy.HasSameEncoding(Self.Payload),
          'Copy has different encoding to original');
  finally
    Copy.Free;
  end;
end;

procedure TPayloadTestCase.TestCopyCopiesData;
var
  Source: TIdRTPRawPayload;
  Copy:   TIdRTPRawPayload;
begin
  Source := TIdRTPRawPayload.Create;
  try
    Source.SetName('foo');
    Source.Data := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';

    Copy := Source.Copy as TIdRTPRawPayload;
    try
      CheckEquals(Source.Data,
                  Copy.Data,
                  'Payload data not copied');
    finally
      Copy.Free;
    end;
  finally
   Source.Free;
  end;
end;

procedure TPayloadTestCase.TestEncodingName;
begin
  CheckEquals('foo/0',     TIdRTPPayload.EncodingName('foo', 0),        'foo/0');
  CheckEquals('foo/0',     TIdRTPPayload.EncodingName('foo', 0, ''),    'foo/0');
  CheckEquals('foo/100',   TIdRTPPayload.EncodingName('foo', 100),      'foo/100');
  CheckEquals('foo/100/1', TIdRTPPayload.EncodingName('foo', 100, '1'), 'foo/100/1');
end;

procedure TPayloadTestCase.TestIsNull;
begin
  Check(not Self.Payload.IsNull, Self.Payload.ClassName + ' IsNull');
end;

procedure TPayloadTestCase.TestIsReserved;
begin
  Check(not Self.Payload.IsReserved, Self.Payload.ClassName + ' IsReserved');
end;

//******************************************************************************
//* TestTIdNullPayload                                                         *
//******************************************************************************
//* TestTIdNullPayload Protected methods ***************************************

function TestTIdNullPayload.PayloadType: TIdRTPPayloadClass;
begin
  Result := TIdNullPayload;
end;

//* TestTIdNullPayload Published methods ***************************************

procedure TestTIdNullPayload.TestIsNull;
begin
  Check(Self.Payload.IsNull, Self.Payload.ClassName + ' IsNull');
end;

//******************************************************************************
//* TestTIdRTPReservedPayload                                                  *
//******************************************************************************
//* TestTIdRTPReservedPayload Protected methods ********************************

function TestTIdRTPReservedPayload.PayloadType: TIdRTPPayloadClass;
begin
  Result := TIdRTPReservedPayload;
end;

//* TestTIdRTPReservedPayload Published methods ********************************

procedure TestTIdRTPReservedPayload.TestIsReserved;
begin
  Check(Self.Payload.IsReserved, Self.Payload.ClassName + ' IsReserved');
end;

//******************************************************************************
//* TestTIdRTPRawPayload                                                       *
//******************************************************************************
//* TestTIdRTPRawPayload Published methods *************************************

procedure TestTIdRTPRawPayload.TestEncodingName;
begin
  CheckEquals('/0', Self.Payload.EncodingName, 'Degenerate');
  (Self.Payload as TIdRTPRawPayload).SetName('foo');
  Self.Payload.ClockRate := 8000;
  CheckEquals('foo/8000', Self.Payload.EncodingName, 'No parameters');
  Self.Payload.Parameters := '2';
  CheckEquals('foo/8000/2', Self.Payload.EncodingName, 'Parameters');
end;

procedure TestTIdRTPRawPayload.TestName;
var
  NewName: String;
begin
  NewName := 'foo';
  CheckEquals('', Self.Payload.Name, 'New payload');
  (Self.Payload as TIdRTPRawPayload).SetName(NewName);
  CheckEquals(NewName, Self.Payload.Name, 'After SetName');
end;

//******************************************************************************
//* TestTIdRTPT140Payload                                                      *
//******************************************************************************
//* TestTIdRTPT140Payload Public methods ***************************************

procedure TestTIdRTPT140Payload.SetUp;
begin
  inherited SetUp;

  Self.Packet := Self.Payload as TIdRTPT140Payload;
end;

//* TestTIdRTPT140Payload Protected methods ************************************

function TestTIdRTPT140Payload.PayloadType: TIdRTPPayloadClass;
begin
  Result := TIdRTPT140Payload;
end;

//* TestTIdRTPT140Payload Published methods ************************************

procedure TestTIdRTPT140Payload.TestInitialClockRate;
begin
  CheckEquals(T140ClockRate, Self.Payload.ClockRate, 'Initial clock rate');
end;

procedure TestTIdRTPT140Payload.TestLength;
var
  S: String;
begin
  Self.Packet.Block := '';
  CheckEquals(0, Self.Packet.Length, 'Length of empty string');

  S := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';
  Self.Packet.Block := S;
  CheckEquals(Length(S)*SizeOf(WideChar),
              Self.Packet.Length,
              'Length');
end;

procedure TestTIdRTPT140Payload.TestName;
begin
  CheckEquals(T140EncodingName,
              Self.Payload.Name,
              Self.Payload.ClassName + ' Name');
end;

procedure TestTIdRTPT140Payload.TestPrintOn;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Packet.Block := 'fooing the bar';

    Self.Packet.PrintOn(S);

    CheckEquals('fooing the bar',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPT140Payload.TestPrintOnWithRealUnicode;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Packet.Block := T140ByteOrderMark;
    Self.Packet.Block := Self.Packet.Block + 'fooing the bar';

    Self.Packet.PrintOn(S);

    CheckEquals(#$ef#$bb#$bf'fooing the bar',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPT140Payload.TestReadFrom;
var
  S: TStringStream;
begin
  S := TStringStream.Create('ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn');
  try
    Self.Packet.ReadFrom(S);

    CheckEqualsW('ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn',
                Self.Packet.Block,
                'Block');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPT140Payload.TestReadFromWithRealUnicode;
var
  Data: WideString;
  S:    TStringStream;
begin
  S := TStringStream.Create(#$ef#$bb#$bf'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn');
  try
    Self.Packet.ReadFrom(S);

    Data := T140ByteOrderMark;
    Data := Data + 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';

    CheckEqualsW(Data,
                 Self.Packet.Block,
                 'Block');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdRTPTelephoneEventPayload                                            *
//******************************************************************************
//* TestTIdRTPTelephoneEventPayload Public methods *****************************

procedure TestTIdRTPTelephoneEventPayload.SetUp;
begin
  inherited SetUp;

  Self.Packet := Self.Payload as TIdRTPTelephoneEventPayload;

  SampleDurations[0] := $f00d;
  SampleDurations[1] := $beef;
  SampleDurations[2] := $cafe;
  SampleDurations[3] := $deaf;
  SampleDurations[4] := $deca;
  SampleDurations[5] := $fbad;
end;

//* TestTIdRTPTelephoneEventPayload Protected methods **************************

function TestTIdRTPTelephoneEventPayload.PayloadType: TIdRTPPayloadClass;
begin
  Result := TIdRTPTelephoneEventPayload;
end;

//* TestTIdRTPTelephoneEventPayload Published methods **************************

procedure TestTIdRTPTelephoneEventPayload.TestName;
begin
  CheckEquals(TelephoneEventEncodingName,
              Self.Payload.Name,
              Self.Payload.ClassName + ' Name');
end;

procedure TestTIdRTPTelephoneEventPayload.TestNumberOfSamples;
var
  I: Integer;
begin
  for I := Low(SampleDurations) to High(SampleDurations) do begin
    Self.Packet.Duration := Self.SampleDurations[I];
    CheckEquals(Self.SampleDurations[I],
                Self.Packet.NumberOfSamples,
                'NumberOfSamples for duration $'
              + IntToHex(Self.SampleDurations[I], 4));
  end;
end;

procedure TestTIdRTPTelephoneEventPayload.TestPrintOnDuration;
var
  I: Integer;
  S: TStringStream;
begin
  for I := Low(SampleDurations) to High(SampleDurations) do begin
    S := TStringStream.Create('');
    try
      Self.Packet.Duration := Self.SampleDurations[I];
      Self.Packet.PrintOn(S);

      Check(S.Size > 3, 'Output stream too short');
      CheckEquals(Self.Packet.Duration shr 8,
                  Ord(S.DataString[3]),
                  'Duration ' + IntToHex(Self.Packet.Duration, 4) + ' '
                + 'MSB');
      CheckEquals(Self.Packet.Duration and $00FF,
                  Ord(S.DataString[4]),
                  'Duration ' + IntToHex(Self.Packet.Duration, 4) + ' '
                + 'LSB');
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPTelephoneEventPayload.TestPrintOnEvent;
const
  SampleEvents: array[0..7] of Byte = ($f0, $0d, $be, $ef, $de, $ca, $fb, $ad);
var
  I: Integer;
  S: TStringStream;
begin
  for I := Low(SampleEvents) to High(SampleEvents) do begin
    S := TStringStream.Create('');
    try
      Self.Packet.Event := SampleEvents[I];
      Self.Packet.PrintOn(S);

      Check(S.Size > 3, 'Output stream too short');
      CheckEquals(Self.Packet.Event,
                  Ord(S.DataString[1]),
                  'Event ' + IntToHex(Self.Packet.Event, 2));
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPTelephoneEventPayload.TestPrintOnIsEnd;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Packet.IsEnd := false;
    Self.Packet.PrintOn(S);

    Check(S.Size > 3, 'Output stream too short, not end');
    CheckEquals($00, Ord(S.DataString[1]), 'First byte, not end');
    CheckEquals($00, Ord(S.DataString[2]), 'Second byte, not end');
    CheckEquals($00, Ord(S.DataString[3]), 'Third byte, not end');
    CheckEquals($00, Ord(S.DataString[4]), 'Fourth byte, not end');
  finally
    S.Free;
  end;

  S := TStringStream.Create('');
  try
    Self.Packet.IsEnd := true;
    Self.Packet.PrintOn(S);

    Check(S.Size > 3, 'Output stream too short, end');
    CheckEquals($00, Ord(S.DataString[1]), 'First byte, end');
    CheckEquals($80, Ord(S.DataString[2]), 'Second byte, end');
    CheckEquals($00, Ord(S.DataString[3]), 'Third byte, end');
    CheckEquals($00, Ord(S.DataString[4]), 'Fourth byte, end');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPTelephoneEventPayload.TestPrintOnReadFromSymmetry;
var
  NewPacket: TIdRTPTelephoneEventPayload;
  OutStream: TStringStream;
begin
  Self.Packet.Event     := DTMF5;
  Self.Packet.Duration  := 100;
  Self.Packet.StartTime := Now;

  OutStream := TStringStream.Create('');
  try
    Self.Packet.PrintOn(OutStream);
    OutStream.Seek(0, soFromBeginning);

    NewPacket := TIdRTPTelephoneEventPayload.Create;
    try
      NewPacket.ReadFrom(OutStream);

      CheckEquals(Self.Packet.Event, NewPacket.Event, 'Event');
      CheckEquals(Self.Packet.Duration, NewPacket.Duration, 'Duration');
      CheckEquals(Self.Packet.StartTime, NewPacket.StartTime, OneSecond, 'StartTime');
    finally
      NewPacket.Free;
    end;
  finally
    OutStream.Free;
  end;
end;

procedure TestTIdRTPTelephoneEventPayload.TestPrintOnReservedBit;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Packet.ReservedBit := true;
    Self.Packet.PrintOn(S);

    Check(S.Size > 3, 'Output stream too short');
    CheckEquals(0, Ord(S.DataString[1]) and $40, 'Reserved bit MUST be zero');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPTelephoneEventPayload.TestPrintOnVolume;
var
  V: TIdTelephoneEventVolume;
  S: TStringStream;
begin
  for V := Low(TIdTelephoneEventVolume) to High(TIdTelephoneEventVolume) do begin
    S := TStringStream.Create('');
    try
      Self.Packet.IsEnd  := V mod 2 = 0;
      Self.Packet.Volume := V;
      Self.Packet.PrintOn(S);

      Check(S.Size > 3, 'Output stream too short');
      CheckEquals(V,
                  Ord(S.DataString[2]) and $7F,
                  'Volume ' + IntToStr(V)
                + '; IsEnd = ' + BoolToStr(Self.Packet.IsEnd));
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPTelephoneEventPayload.TestReadFromDuration;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$00#$00#$CA#$FE);
  try
    Self.Packet.ReadFrom(S);
    CheckEquals($CAFE,
                Self.Packet.Duration, 'Duration');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPTelephoneEventPayload.TestReadFromEvent;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$BE#$EF#$00#$00);
  try
    Self.Packet.ReadFrom(S);
    CheckEquals($BE, Self.Packet.Event, 'Event');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPTelephoneEventPayload.TestReadFromIsEnd;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$00#$80#$00#$00);
  try
    Self.Packet.ReadFrom(S);
    Check(Self.Packet.IsEnd, 'IsEnd not set');
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$00#$00#$00#$00);
  try
    Self.Packet.ReadFrom(S);
    Check(not Self.Packet.IsEnd, 'IsEnd set');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPTelephoneEventPayload.TestReadFromPrintOnSymmetry;
var
  InStream:   TStringStream;
  OutStream: TStringStream;
begin
  InStream := TStringStream.Create(#$00#$20#$CA#$FE);
  try
    OutStream := TStringStream.Create('');
    try
      Self.Packet.ReadFrom(InStream);
      Self.Packet.PrintOn(OutStream);
      CheckEquals(InStream.DataString, OutStream.DataString, 'Stream');
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

procedure TestTIdRTPTelephoneEventPayload.TestReadFromReservedBit;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$00#$40#$00#$00);
  try
    Self.Packet.ReadFrom(S);
    Check(Self.Packet.ReservedBit, 'ReservedBit not set');
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$00#$00#$00#$00);
  try
    Self.Packet.ReadFrom(S);
    Check(not Self.Packet.ReservedBit, 'ReservedBit set');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPTelephoneEventPayload.TestReadFromVolume;
var
  S: TStringStream;
  V: TIdTelephoneEventVolume;
begin
  for V := Low(TIdTelephoneEventVolume) to High(TIdTelephoneEventVolume) do begin
    S := TStringStream.Create(#$00 + Chr($C0 or V) + #$00#$00);
    try
      Self.Packet.ReadFrom(S);
      CheckEquals(V,
                  Self.Packet.Volume,
                  'Volume at -' + IntToStr(V - 1) + ' dBm0');
    finally
      S.Free;
    end;
  end;
end;

//******************************************************************************
//* TestTIdRTPProfile                                                          *
//******************************************************************************
//* TestTIdRTPProfile Public methods *******************************************

procedure TestTIdRTPProfile.SetUp;
begin
  inherited SetUp;

  Self.ArbPT := 98;

  Self.InterleavedT140Encoding := TIdRTPPayload.CreatePayload('RED/8000');
  Self.Profile                 := TIdRTPProfile.Create;
  Self.T140Encoding            := TIdRTPPayload.CreatePayload('T140/1000');
end;

procedure TestTIdRTPProfile.TearDown;
begin
  Self.T140Encoding.Free;
  Self.Profile.Free;
  Self.InterleavedT140Encoding.Free;

  inherited TearDown;
end;

//* TestTIdRTPProfile Published methods ****************************************

procedure TestTIdRTPProfile.TestAddEncoding;
begin
  Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT);
  CheckEquals(1, Self.Profile.Count, 'New MIME type not added');
  CheckEquals(Self.T140Encoding.ClassType,
              Self.Profile.EncodingFor(Self.ArbPT).ClassType,
              'Incorrect class type used for new MIME type');

  Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT - 1);
  CheckEquals(1,
              Self.Profile.Count,
              'No duplication of MIME type allowed');

  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, Self.ArbPT);
  CheckEquals(1,
              Self.Profile.Count,
              'No duplication of Payload Type allowed');

  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, Self.ArbPT - 1);
  CheckEquals(2, Self.Profile.Count, 'New, different, MIME type not added');
end;

procedure TestTIdRTPProfile.TestAddEncodingByName;
begin
  Self.Profile.AddEncoding(Self.T140Encoding.Name,
                           Self.T140Encoding.ClockRate,
                           '',
                           Self.ArbPT);
  CheckEquals(Self.T140Encoding.ClassType,
              Self.Profile.EncodingFor(Self.ArbPT).ClassType,
              'Incorrect class type used for new MIME type');
end;

procedure TestTIdRTPProfile.TestAssign;
var
  Dvi4Vid:    TIdRTPRawPayload;
  GSM:        TIdRTPRawPayload;
  I:          Integer;
  NewProfile: TIdRTPProfile;
begin
  NewProfile := TIdRTPProfile.Create;
  try
    GSM := TIdRTPRawPayload.Create;
    try
      GSM.SetName(GSMEncoding + '/8000');

      Dvi4Vid := TIdRTPRawPayload.Create;
      try
        Dvi4Vid.SetName(DVI4Encoding + '/22050');
        NewProfile.AddEncoding(GSM, 5);
        NewProfile.AddEncoding(Dvi4Vid, 10);

        Self.Profile.Assign(NewProfile);

        CheckEquals(NewProfile.Count,
                    Self.Profile.Count,
                    'Encoding count');

        for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do begin
          CheckEquals(NewProfile.EncodingFor(I).ClassType,
                      Self.Profile.EncodingFor(I).ClassType,
                      IntToStr(I) + '''s type');

          CheckEquals(NewProfile.EncodingFor(I).Name,
                      Self.Profile.EncodingFor(I).Name,
                      IntToStr(I) + '''s name');

          CheckEquals(NewProfile.EncodingFor(I).ClockRate,
                      Self.Profile.EncodingFor(I).ClockRate,
                      IntToStr(I) + '''s clock rate');

          CheckEquals(NewProfile.EncodingFor(I).Parameters,
                      Self.Profile.EncodingFor(I).Parameters,
                      IntToStr(I) + '''s parameters');
        end;

      finally
        Dvi4Vid.Free;
      end;
    finally
      GSM.Free;
    end;
  finally
    NewProfile.Free;
  end;
end;

procedure TestTIdRTPProfile.TestClear;
begin
  Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT);
  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, Self.ArbPT + 1);

  Self.Profile.Clear;

  CheckEquals(0, Self.Profile.Count, 'Profile wasn''t cleared');
end;

procedure TestTIdRTPProfile.TestCount;
begin
  CheckEquals(0, Self.Profile.Count, 'Count on new profile');

  Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT);
  CheckEquals(1, Self.Profile.Count, 'Count after AddEncoding');
end;

procedure TestTIdRTPProfile.TestCreatePacketRTCP;
var
  Data: TStringStream;
  Pkt:  TIdRTPBasePacket;
begin
  Data := TStringStream.Create(#$00 + Chr(RTCPGoodbye));
  try
    Pkt := Self.Profile.CreatePacket(Data);
    try
      Check(Pkt.IsRTCP, 'RTCP packet not created');
    finally
      Pkt.Free;
    end;
  finally
    Data.Free;
  end;
end;

procedure TestTIdRTPProfile.TestCreatePacketRTP;
var
  Data: TStringStream;
  Pkt:  TIdRTPBasePacket;
begin
  Data := TStringStream.Create(#$00#$00);
  try
    Pkt := Self.Profile.CreatePacket(Data);
    try
      Check(Pkt.IsRTP, 'RTP packet not created');
    finally
      Pkt.Free;
    end;
  finally
    Data.Free;
  end;
end;

procedure TestTIdRTPProfile.TestEncodingFor;
begin
  CheckEquals(NullEncodingName + '/0',
              Self.Profile.EncodingFor(Self.ArbPT).EncodingName,
              '"MIME type" for unknown Payload Type');

  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, Self.ArbPT - 1);
  Self.Profile.AddEncoding(Self.T140Encoding,            Self.ArbPT);

  CheckEquals(Self.InterleavedT140Encoding.EncodingName,
              Self.Profile.EncodingFor(Self.ArbPT - 1).EncodingName,
              'MIME type for ' + InterleavedT140MimeType);

  CheckEquals(Self.T140Encoding.EncodingName,
              Self.Profile.EncodingFor(Self.ArbPT).EncodingName,
              'MIME type for ' + T140MimeType);
end;

procedure TestTIdRTPProfile.TestEncodingForName;
begin
  Check(Self.Profile.EncodingFor(Self.InterleavedT140Encoding.EncodingName).IsNull,
        'Non-null encoding returned for non-existent encoding');

  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, Self.ArbPT - 1);
  Self.Profile.AddEncoding(Self.T140Encoding,            Self.ArbPT);

  CheckEquals(Self.InterleavedT140Encoding.EncodingName,
              Self.Profile.EncodingFor(Self.InterleavedT140Encoding.EncodingName).EncodingName,
              'MIME type for ' + InterleavedT140MimeType);
  CheckEquals(Self.T140Encoding.EncodingName,
              Self.Profile.EncodingFor(Self.T140Encoding.EncodingName).EncodingName,
              'MIME type for ' + T140MimeType);
end;

procedure TestTIdRTPProfile.TestFirstFreePayloadType;
begin
  CheckEquals(0, Self.Profile.FirstFreePayloadType, 'New (empty) profile');

  Self.Profile.AddEncoding(Self.T140Encoding, 1);
  CheckEquals(0,
              Self.Profile.FirstFreePayloadType,
              'Empty spaces before first assigned payload type');

  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, 0);
  CheckEquals(2,
              Self.Profile.FirstFreePayloadType,
              'First couple of payload types assigned');
end;

procedure TestTIdRTPProfile.TestHasEncoding;
begin
  Check(not Self.Profile.HasEncoding(Self.T140Encoding), 'New profile');

  Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT);
  Check(Self.Profile.HasEncoding(Self.T140Encoding), 'After Add');
end;

procedure TestTIdRTPProfile.TestHasPayloadType;
begin
  Check(not Self.Profile.HasPayloadType(Self.ArbPT), 'New profile');

  Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT);
  Check(Self.Profile.HasPayloadType(Self.ArbPT), 'After Add');
end;

procedure TestTIdRTPProfile.TestIsFull;
var
  I:   TIdRTPPayloadType;
  Enc: TIdRTPPayload;
begin
  Check(not Self.Profile.IsFull, 'Empty profile');

  for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do begin
    Enc := TIdRTPPayload.CreatePayload('foo/' + IntToStr(I));
    try
      Self.Profile.AddEncoding(Enc, I);
    finally
      Enc.Free;
    end;
  end;
  Check(Self.Profile.IsFull, 'Full profile');
end;

procedure TestTIdRTPProfile.TestIsRTCPPayloadType;
var
  PT: Byte;
begin
  for PT := Low(Byte) to RTCPSenderReport - 1 do
    Check(not Self.Profile.IsRTCPPayloadType(PT),
          'Unknown RTCP payload type: ' + IntToStr(PT));

  Check(Self.Profile.IsRTCPPayloadType(RTCPSenderReport),
        'RTCPSenderReport');
  Check(Self.Profile.IsRTCPPayloadType(RTCPReceiverReport),
        'RTCPReceiverReport');
  Check(Self.Profile.IsRTCPPayloadType(RTCPSourceDescription),
        'RTCPSourceDescription');
  Check(Self.Profile.IsRTCPPayloadType(RTCPGoodbye),
        'RTCPGoodbye');
  Check(Self.Profile.IsRTCPPayloadType(RTCPApplicationDefined),
        'RTCPApplicationDefined');

  for PT := RTCPApplicationDefined + 1 to High(Byte) - 1 do
    Check(not Self.Profile.IsRTCPPayloadType(PT),
          'Unknown RTCP payload type: ' + IntToStr(PT));
end;

procedure TestTIdRTPProfile.TestPayloadTypeFor;
begin
  try
    Self.Profile.PayloadTypeFor(Self.T140Encoding);
    Fail('"Payload Type" for unknown MIME type');
  except
    on ENoPayloadTypeFound do;
  end;

  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, Self.ArbPT - 1);
  Self.Profile.AddEncoding(Self.T140Encoding,            Self.ArbPT);

  CheckEquals(Self.ArbPT - 1,
              Self.Profile.PayloadTypeFor(Self.InterleavedT140Encoding),
              'Payload Type for ' + InterleavedT140MimeType);

  CheckEquals(Self.ArbPT,
              Self.Profile.PayloadTypeFor(Self.T140Encoding),
              'Payload Type for ' + T140MimeType);
end;

procedure TestTIdRTPProfile.TestReservedEncodingMustntOverwriteOthers;
var
  Res: TIdRTPReservedPayload;
begin
  Res := TIdRTPReservedPayload.Create;
  try
    Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT);
    Self.Profile.AddEncoding(Res, Self.ArbPT);
    Self.Profile.PayloadTypeFor(Self.T140Encoding);
  finally
    Res.Free;
  end;
end;

procedure TestTIdRTPProfile.TestStreamContainsEncoding;
var
  PT: TIdRTPPayloadType;
  S:  TStringStream;
begin
  for PT := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do begin
    S := TStringStream.Create(#$00 + Chr(PT));
    try
      Check(Self.Profile.EncodingFor(PT) = Self.Profile.StreamContainsEncoding(S),
            'Wrong encoding, PT = ' + IntToStr(PT));
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPProfile.TestStreamContainsPayloadType;
var
  PT: TIdRTPPayloadType;
  S:  TStringStream;
begin
  for PT := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do begin
    S := TStringStream.Create(#$00 + Chr(PT));
    try
    CheckEquals(PT,
                Self.Profile.StreamContainsPayloadType(S),
                'Wrong payload type');
    finally
      S.Free;
    end;
  end;
end;

//******************************************************************************
//* TestTIdAudioVisualProfile                                                  *
//******************************************************************************
//* TestTIdAudioVisualProfile Public methods ***********************************

procedure TestTIdAudioVisualProfile.SetUp;
begin
  inherited SetUp;

  Self.Profile := TIdAudioVisualProfile.Create;
end;

procedure TestTIdAudioVisualProfile.TearDown;
begin
  Self.Profile.Free;

  inherited TearDown;
end;

//* TIdAudioVisualProfile Private methods **************************************

procedure TestTIdAudioVisualProfile.CheckRange(StartType, EndType: TIdRTPPayloadType);
var
  I:            TIdRTPPayloadType;
  PayloadCount: Integer;
  TestEncoding: TIdRTPPayload;
begin
  PayloadCount := Self.Profile.Count;

  TestEncoding := TIdRTPPayload.Create;
  try
    for I := StartType to EndType do begin
      Self.Profile.AddEncoding(TestEncoding, I);
      Check(PayloadCount = Self.Profile.Count,
            'New encoding was added for a reserved/unassigned Payload Type '
          + IntToStr(I));
    end;
  finally
    TestEncoding.Free
  end;
end;

//* TIdAudioVisualProfile Published methods ************************************

procedure TestTIdAudioVisualProfile.TestAssign;
var
  Dvi4Vid:       TIdRTPRawPayload;
  GSM:           TIdRTPRawPayload;
  NewProfile:    TIdRTPProfile;
  VirginProfile: TIdAudioVisualProfile;
begin
  VirginProfile := TIdAudioVisualProfile.Create;
  try
    NewProfile := TIdRTPProfile.Create;
    try
      GSM := TIdRTPRawPayload.Create;
      try
        GSM.SetName(GSMEncoding + '/44100');

        Dvi4Vid := TIdRTPRawPayload.Create;
        try
          Dvi4Vid.SetName(DVI4Encoding + '/666');

          NewProfile.AddEncoding(GSM, 0);
          NewProfile.AddEncoding(Dvi4Vid, 98);

          Self.Profile.Assign(NewProfile);

          CheckEquals(VirginProfile.EncodingFor(0).ClassType,
                      Self.Profile.EncodingFor(0).ClassType,
                      '0''s type');

          CheckEquals(VirginProfile.EncodingFor(0).EncodingName,
                      Self.Profile.EncodingFor(0).EncodingName,
                      '0''s details');

          CheckEquals(NewProfile.EncodingFor(98).ClassType,
                      Self.Profile.EncodingFor(98).ClassType,
                      '98''s type');

          CheckEquals(NewProfile.EncodingFor(98).EncodingName,
                      Self.Profile.EncodingFor(98).EncodingName,
                      '98''s details');
        finally
          Dvi4Vid.Free;
        end;
      finally
        GSM.Free;
      end;
    finally
      NewProfile.Free;
    end;
  finally
    VirginProfile.Free;
  end;
end;

procedure TestTIdAudioVisualProfile.TestDefinedPayloads;
begin
  CheckEquals('PCMU/8000',    Self.Profile.EncodingFor(0).EncodingName,   '0');
  CheckEquals('GSM/8000',     Self.Profile.EncodingFor(3).EncodingName,   '3');
  CheckEquals('G723/8000',    Self.Profile.EncodingFor(4).EncodingName,   '4');
  CheckEquals('DVI4/8000',    Self.Profile.EncodingFor(5).EncodingName,   '5');
  CheckEquals('DVI4/16000',   Self.Profile.EncodingFor(6).EncodingName,   '6');
  CheckEquals('LPC/8000',     Self.Profile.EncodingFor(7).EncodingName,   '7');
  CheckEquals('PCMA/8000',    Self.Profile.EncodingFor(8).EncodingName,   '8');
  CheckEquals('G722/8000',    Self.Profile.EncodingFor(9).EncodingName,   '9');
  CheckEquals('L16/44100/2',  Self.Profile.EncodingFor(10).EncodingName, '10');
  CheckEquals('L16/44100/1',  Self.Profile.EncodingFor(11).EncodingName, '11');
  CheckEquals('QCELP/8000',   Self.Profile.EncodingFor(12).EncodingName, '12');
  CheckEquals('CN/8000',      Self.Profile.EncodingFor(13).EncodingName, '13');
  CheckEquals('MPA/90000',    Self.Profile.EncodingFor(14).EncodingName, '14');
  CheckEquals('G728/8000',    Self.Profile.EncodingFor(15).EncodingName, '15');
  CheckEquals('DVI4/11025',   Self.Profile.EncodingFor(16).EncodingName, '16');
  CheckEquals('DVI4/22050',   Self.Profile.EncodingFor(17).EncodingName, '17');
  CheckEquals('G729/8000',    Self.Profile.EncodingFor(18).EncodingName, '18');

  CheckEquals('CelB/90000',   Self.Profile.EncodingFor(25).EncodingName, '25');
  CheckEquals('JPEG/90000',   Self.Profile.EncodingFor(26).EncodingName, '26');

  CheckEquals('nv/90000',     Self.Profile.EncodingFor(28).EncodingName, '28');

  CheckEquals('H261/90000',   Self.Profile.EncodingFor(31).EncodingName, '31');
  CheckEquals('MPV/90000',    Self.Profile.EncodingFor(32).EncodingName, '32');
  CheckEquals('MP2T/90000',   Self.Profile.EncodingFor(33).EncodingName, '33');
  CheckEquals('H263/90000',   Self.Profile.EncodingFor(34).EncodingName, '34');
end;

procedure TestTIdAudioVisualProfile.TestDynamicPayloadTypes;
var
  I:            TIdRTPPayloadType;
  PayloadCount: Integer;
  TestEncoding: TIdRTPPayload;
begin
  for I := 96 to High(TIdRTPPayloadType) do begin
    TestEncoding := TIdRTPPayload.CreatePayload('arb values/' + IntToStr(I));
    try
      PayloadCount := Self.Profile.Count;

      Self.Profile.AddEncoding(TestEncoding, I);
      Check(PayloadCount + 1 = Self.Profile.Count,
            'New encoding wasn''t added for a dynamic Payload Type '
          + IntToStr(I));
    finally
      TestEncoding.Free
    end;
  end;
end;

procedure TestTIdAudioVisualProfile.TestReservedAndUnassignedPayloadTypes;
begin
  Self.CheckRange(1,  2);
  Self.CheckRange(19, 24);
  Self.CheckRange(27, 27);
  Self.CheckRange(29, 30);
  Self.CheckRange(35, 95);
end;

procedure TestTIdAudioVisualProfile.TestTransportDesc;
begin
  CheckEquals(AudioVisualProfile,
              Self.Profile.TransportDesc,
              'TransportDesc');
end;

//******************************************************************************
//* TestTIdRTPHeaderExtension                                                  *
//******************************************************************************
//* TestTIdRTPHeaderExtension Public methods ***********************************

procedure TestTIdRTPHeaderExtension.SetUp;
begin
  inherited SetUp;

  Self.HeaderExtension := TIdRTPHeaderExtension.Create;
end;

procedure TestTIdRTPHeaderExtension.TearDown;
begin
  Self.HeaderExtension.Free;

  inherited TearDown;
end;

//* TestTIdRTPHeaderExtension Published methods ********************************

procedure TestTIdRTPHeaderExtension.TestPrintOn;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.HeaderExtension.ProfileDefinedValue := $CAFE;
    Self.HeaderExtension.Length              := 5;

    Self.HeaderExtension.Data[0] := $CAFEBABE;
    Self.HeaderExtension.Data[1] := $DEADBEEF;
    Self.HeaderExtension.Data[2] := $0DEFACED;
    Self.HeaderExtension.Data[3] := $F00DD00D;
    Self.HeaderExtension.Data[4] := $B1FFB0FF;

    Self.HeaderExtension.PrintOn(S);

    CheckEquals(#$CA#$FE#$00#$05
              + #$CA#$FE#$BA#$BE
              + #$DE#$AD#$BE#$EF
              + #$0D#$EF#$AC#$ED
              + #$F0#$0D#$D0#$0D
              + #$B1#$FF#$B0#$FF,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPHeaderExtension.TestReadFrom;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$CA#$FE#$00#$05
                          + #$CA#$FE#$BA#$BE
                          + #$DE#$AD#$BE#$EF
                          + #$0D#$EF#$AC#$ED
                          + #$F0#$0D#$D0#$0D
                          + #$B1#$FF#$B0#$FF);
  try
    Self.HeaderExtension.ReadFrom(S);

    CheckEquals($CAFE,
                Self.HeaderExtension.ProfileDefinedValue,
                'ProfileDefinedValue');
    CheckEquals(5,
                Self.HeaderExtension.Length,
                'Length');

    CheckEquals(Integer($CAFEBABE), Integer(Self.HeaderExtension.Data[0]), 'Data[0]');
    CheckEquals(Integer($DEADBEEF), Integer(Self.HeaderExtension.Data[1]), 'Data[1]');
    CheckEquals(Integer($0DEFACED), Integer(Self.HeaderExtension.Data[2]), 'Data[2]');
    CheckEquals(Integer($F00DD00D), Integer(Self.HeaderExtension.Data[3]), 'Data[3]');
    CheckEquals(Integer($B1FFB0FF), Integer(Self.HeaderExtension.Data[4]), 'Data[4]');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdRTPBasePacket                                                       *
//******************************************************************************
//* TestTIdRTPBasePacket Public methods ****************************************

procedure TestTIdRTPBasePacket.SetUp;
begin
  inherited SetUp;

  Self.Profile := TIdAudioVisualProfile.Create;
end;

procedure TestTIdRTPBasePacket.TearDown;
begin
  Self.Profile.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdRTCPReportBlock                                                     *
//******************************************************************************
//* TestTIdRTCPReportBlock Public methods **************************************

procedure TestTIdRTCPReportBlock.SetUp;
begin
  inherited SetUp;

  Self.Block := TIdRTCPReportBlock.Create;
end;

procedure TestTIdRTCPReportBlock.TearDown;
begin
  Self.Block.Free;

  inherited TearDown;
end;

//* TestTIdRTCPReportBlock Published methods ***********************************

procedure TestTIdRTCPReportBlock.TestPrintOn;
var
  S: TStringStream;
begin
  Block.CumulativeLoss     := $000102;
  Block.DelaySinceLastSR   := $03040506;
  Block.FractionLost       := $07;
  Block.HighestSeqNo       := $08090a0b;
  Block.InterArrivalJitter := $0c0d0e0f;
  Block.LastSenderReport   := $10111213;
  Block.SyncSrcID          := $14151617;

  S := TStringStream.Create('');
  try
    Block.PrintOn(S);
    Check(Length(S.DataString) > 23, 'Insufficient output');

    CheckEquals($14, Ord(S.DataString[ 1]), 'SSRC MSB');
    CheckEquals($15, Ord(S.DataString[ 2]), 'SSRC MSB - 1');
    CheckEquals($16, Ord(S.DataString[ 3]), 'SSRC LSB + 1');
    CheckEquals($17, Ord(S.DataString[ 4]), 'SSRC LSB');
    CheckEquals($07, Ord(S.DataString[ 5]), 'Fraction lost');
    CheckEquals($00, Ord(S.DataString[ 6]), 'Cumulative loss MSB');
    CheckEquals($01, Ord(S.DataString[ 7]), 'Cumulative loss MSB - 1');
    CheckEquals($02, Ord(S.DataString[ 8]), 'Cumulative loss LSB');
    CheckEquals($08, Ord(S.DataString[ 9]), 'Highest seq no MSB');
    CheckEquals($09, Ord(S.DataString[10]), 'Highest seq no MSB - 1');
    CheckEquals($0a, Ord(S.DataString[11]), 'Highest seq no LSB + 1');
    CheckEquals($0b, Ord(S.DataString[12]), 'Highest seq no LSB');
    CheckEquals($0c, Ord(S.DataString[13]), 'Inter-arrival jitter MSB');
    CheckEquals($0d, Ord(S.DataString[14]), 'Inter-arrival jitter MSB - 1');
    CheckEquals($0e, Ord(S.DataString[15]), 'Inter-arrival jitter LSB + 1');
    CheckEquals($0f, Ord(S.DataString[16]), 'Inter-arrival jitter LSB');
    CheckEquals($10, Ord(S.DataString[17]), 'Last SR MSB');
    CheckEquals($11, Ord(S.DataString[18]), 'Last SR MSB - 1');
    CheckEquals($12, Ord(S.DataString[19]), 'Last SR LSB + 1');
    CheckEquals($13, Ord(S.DataString[20]), 'Last SR LSB');
    CheckEquals($03, Ord(S.DataString[21]), 'Delay since last SR MSB');
    CheckEquals($04, Ord(S.DataString[22]), 'Delay since last SR MSB - 1');
    CheckEquals($05, Ord(S.DataString[23]), 'Delay since last SR LSB + 1');
    CheckEquals($06, Ord(S.DataString[24]), 'Delay since last SR LSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPReportBlock.TestReadFrom;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$00#$01#$02#$03
                          + #$04#$05#$06#$07
                          + #$08#$09#$0a#$0b
                          + #$0c#$0d#$0e#$0f
                          + #$10#$11#$12#$13
                          + #$14#$15#$16#$17);
  try
    Block.ReadFrom(S);
    CheckEquals($00010203, Block.SyncSrcID,          'SSRC');
    CheckEquals($04,       Block.FractionLost,       'Fraction lost');
    CheckEquals($050607,   Block.CumulativeLoss,     'Cumulative loss');
    CheckEquals($08090a0b, Block.HighestSeqNo,       'Highest seq no');
    CheckEquals($0c0d0e0f, Block.InterArrivalJitter, 'Inter-arrival jitter');
    CheckEquals($10111213, Block.LastSenderReport,   'Last SR');
    CheckEquals($14151617, Block.DelaySinceLastSR,   'Delay since last SR');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdRTPPacket                                                           *
//******************************************************************************
//* TestTIdRTPPacket Public methods ********************************************

procedure TestTIdRTPPacket.SetUp;
begin
  inherited SetUp;

  Self.AVP := TIdAudioVisualProfile.Create;

  Self.T140PT := 96;

  Self.Packet := TIdRTPPacket.Create(Self.AVP);
end;

procedure TestTIdRTPPacket.TearDown;
begin
  Self.Packet.Free;
  Self.AVP.Free;

  inherited TearDown;
end;

//* TestTIdRTPPacket Published methods *****************************************

procedure TestTIdRTPPacket.TestCopy;
var
  Copy:    TIdRTPBasePacket;
  Expected: TStringStream;
  Received: TStringStream;
begin
  Self.Packet.HasExtension    := true;
  Self.Packet.IsMarker        := true;
  Self.Packet.PayloadType     := Self.AVP.FirstFreePayloadType;
  Self.Packet.SequenceNo      := $dead;
  Self.Packet.SyncSrcID       := $decafbad;
  Self.Packet.Timestamp       := $f00df00d;
  Self.Packet.HeaderExtension.Length              := 1;
  Self.Packet.HeaderExtension.ProfileDefinedValue := $cafe;
  Self.Packet.HeaderExtension.Data[0]             := $fe;

  Expected := TStringStream.Create('');
  try
    Received := TStringStream.Create('');
    try
      Self.Packet.PrintOn(Expected);

      Copy := Self.Packet.Copy;
      try
        Copy.PrintOn(Received);

        CheckEquals(Expected.DataString,
                    Received.DataString,
                    'Copy');
      finally
        Copy.Free;
      end;
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdRTPPacket.TestCollidesWith;
var
  SSRC: Cardinal;
begin
  SSRC := $cafebeef;
  Self.Packet.SyncSrcID := $decafbad;
  Check(not Self.Packet.CollidesWith(SSRC), 'SSRCs different');

  Self.Packet.CsrcCount := Self.Packet.CsrcCount + 1;
  Self.Packet.CsrcIDs[Self.Packet.CsrcCount - 1] := SSRC;
  Check(Self.Packet.CollidesWith(SSRC), 'Session''s SSRC in CSRCs');

  Self.Packet.CsrcCount := Self.Packet.CsrcCount - 1;
  Check(Self.Packet.CollidesWith(Self.Packet.SyncSrcID), 'SSRCs identical');
end;

procedure TestTIdRTPPacket.TestGetAllSrcIDsNoCsrcs;
var
  IDs: TCardinalDynArray;
begin
  IDs := Self.Packet.GetAllSrcIDs;
  CheckEquals(1, Length(IDs), 'Number of identifiers');
  CheckEquals(IntToHex(Self.Packet.SyncSrcID, 8),
              IntToHex(IDs[0], 8),
              'ID');
end;

procedure TestTIdRTPPacket.TestGetAllSrcIDsWithCsrcs;
var
  IDs: TCardinalDynArray;
  SSRC1: Cardinal;
  SSRC2: Cardinal;
  SSRC3: Cardinal;
begin
  SSRC1 := $deafbead;
  SSRC2 := $decafbad;
  SSRC3 := $deadbea7;

  Self.Packet.CsrcCount := 3;
  Self.Packet.CsrcIDs[0] := SSRC1;
  Self.Packet.CsrcIDs[1] := SSRC2;
  Self.Packet.CsrcIDs[2] := SSRC3;

  IDs := Self.Packet.GetAllSrcIDs;
  CheckEquals(Self.Packet.CsrcCount + 1,
              Length(IDs),
              'Number of identifiers');
  CheckEquals(IntToHex(SSRC1, 8),
              IntToHex(IDs[1], 8),
              'CSRC 1');
  CheckEquals(IntToHex(SSRC2, 8),
              IntToHex(IDs[2], 8),
              'CSRC 2');
  CheckEquals(IntToHex(SSRC3, 8),
              IntToHex(IDs[3], 8),
              'CSRC 3');
end;

procedure TestTIdRTPPacket.TestIsRTCP;
begin
  Check(not Self.Packet.IsRTCP, 'IsRTCP');
end;

procedure TestTIdRTPPacket.TestIsRTP;
begin
  Check(Self.Packet.IsRTP, 'IsRTP');
end;

procedure TestTIdRTPPacket.TestIsValidBogusVersion;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$40#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    Self.Packet.ReadFrom(S);
    Check(not Self.Packet.IsValid, 'Bogus version (RFC 1889)');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestIsValidKnownPayloadType;
var
  S: TStringStream;
begin
  S := TStringStream.Create(Chr(RFC3550Version shl 6) + #$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    Self.Packet.ReadFrom(S);
    Check(Self.Packet.IsValid, 'Known payload type (PCM Mu-law; 8000 KHz)');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestIsValidProfileDoesntAllowHeaderExtensions;
var
  N: TIdMockProfile;
  P: TIdRTPPacket;
  S: TStringStream;
begin
  N := TIdMockProfile.Create;
  try
    N.SetAllowExtension(false);

    S := TStringStream.Create(Chr((RFC3550Version shl 6) or $10) + #$00#$00#$00
                            + #$00#$00#$00#$00
                            + #$00#$00#$00#$00
                            + #$00#$00#$00#$00); // profile-specific word + extension length
    try
      P := TIdRTPPacket.Create(N);
      try
        P.ReadFrom(S);
        Check(not P.IsValid, 'Profile forbids header extensions');
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  finally
    N.Free;
  end;
end;

procedure TestTIdRTPPacket.TestIsValidRealVersion;
var
  S: TStringStream;
begin
  S := TStringStream.Create(Chr(RFC3550Version shl 6) + #$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    Self.Packet.ReadFrom(S);
    Check(Self.Packet.IsValid, 'RFC 3550 version');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestIsValidSenderOrReceiverReport;
var
  S: TStringStream;
begin
  S := TStringStream.Create(Chr(RFC3550Version shl 6) + Chr(RTCPSenderReport) + #$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    Self.Packet.ReadFrom(S);
    Check(Self.Packet.IsValid, 'Sender report');
  finally
    S.Free;
  end;

  S := TStringStream.Create(Chr(RFC3550Version shl 6) + Chr(RTCPReceiverReport) + #$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    Self.Packet.ReadFrom(S);
    Check(Self.Packet.IsValid, 'Receiver report');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestIsValidUnknownPayloadType;
var
  S: TStringStream;
begin
  S := TStringStream.Create(Chr(RFC3550Version shl 6) + Chr(Self.AVP.FirstFreePayloadType) + #$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    Self.Packet.ReadFrom(S);
    Check(not Self.Packet.IsValid, 'Unknown payload type');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestPrintOn;
var
  P: TIdRTPPacket;
  S: TStringStream;
begin
  Self.Packet.Version      := 2;
  Self.Packet.HasPadding   := true;
  Self.Packet.HasExtension := false;
  Self.Packet.CsrcCount    := 2;
  Self.Packet.CsrcIDs[0]   := $CAFEBABE;
  Self.Packet.CsrcIDs[1]   := $DEADBEEF;
  Self.Packet.IsMarker     := true;
  Self.Packet.Length       := 32;
  Self.Packet.PayloadType  := $0D;
  Self.Packet.SequenceNo   := $0102;
  Self.Packet.Timestamp    := $0A0B0C0D;
  Self.Packet.SyncSrcID    := $10203040;

  S := TStringStream.Create('');
  try
    Self.Packet.PrintOn(S);
    S.Seek(0, soFromBeginning);

    P := TIdRTPPacket.Create(Self.AVP);
    try
      P.ReadFrom(S);
      Self.CheckHasEqualHeaders(Self.Packet, P);
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestPrintOnHasExtension;
var
  S: TStringStream;
begin
  Self.Packet.HasExtension := true;
  Self.Packet.HeaderExtension.ProfileDefinedValue := $dead;
  Self.Packet.HeaderExtension.Length              := 4;
  Self.Packet.HeaderExtension.Data[0]             := $deadbeef;
  Self.Packet.HeaderExtension.Data[1]             := $f00dcafe;
  Self.Packet.HeaderExtension.Data[2]             := $beeff00d;
  Self.Packet.HeaderExtension.Data[3]             := $cafebabe;

  S := TStringStream.Create('');
  try
    Self.Packet.PrintOn(S);
    Check(Length(S.DataString) > 31, 'Too little output');
    Check((Ord(S.DataString[1]) and $10) > 0, 'Header eXtension bit not set');

    CheckEquals($de, Ord(S.DataString[13]), 'ProfileDefinedValue MSB');
    CheckEquals($ad, Ord(S.DataString[14]), 'ProfileDefinedValue LSB');
    CheckEquals(0,   Ord(S.DataString[15]), 'Length MSB');
    CheckEquals(4,   Ord(S.DataString[16]), 'Length LSB');
    CheckEquals($de, Ord(S.DataString[17]), 'Data[0] MSB');
    CheckEquals($ad, Ord(S.DataString[18]), 'Data[0] MSB - 1');
    CheckEquals($be, Ord(S.DataString[19]), 'Data[0] LSB + 1');
    CheckEquals($ef, Ord(S.DataString[20]), 'Data[0] LSB');
    CheckEquals($f0, Ord(S.DataString[21]), 'Data[1] MSB');
    CheckEquals($0d, Ord(S.DataString[22]), 'Data[1] MSB - 1');
    CheckEquals($ca, Ord(S.DataString[23]), 'Data[1] LSB + 1');
    CheckEquals($fe, Ord(S.DataString[24]), 'Data[1] LSB');
    CheckEquals($be, Ord(S.DataString[25]), 'Data[2] MSB');
    CheckEquals($ef, Ord(S.DataString[26]), 'Data[2] MSB - 1');
    CheckEquals($f0, Ord(S.DataString[27]), 'Data[2] LSB + 1');
    CheckEquals($0d, Ord(S.DataString[28]), 'Data[2] LSB');
    CheckEquals($ca, Ord(S.DataString[29]), 'Data[3] MSB');
    CheckEquals($fe, Ord(S.DataString[30]), 'Data[3] MSB - 1');
    CheckEquals($ba, Ord(S.DataString[31]), 'Data[3] LSB + 1');
    CheckEquals($be, Ord(S.DataString[32]), 'Data[3] LSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromContributingSourceIdentifiers;
var
  I:      TIdRTPCsrcCount;
  IDs:    TIdCardinalArray;
  J:      Integer;
  Packet: String;
  P:      TIdRTPPacket;
  S:      TStream;
begin
  for I := Low(TIdRTPCsrcCount) to High(TIdRTPCsrcCount) do begin
    SetLength(IDs, I);
    Packet := Chr(I) + #$00#$00#$00
                 + #$00#$00#$00#$00  // timestamp
                 + #$00#$00#$00#$00; // SSRC ID


    for J := 0 to I - 1 do begin
      IDs[J] := Random(High(Integer));
      Packet := Packet + EncodeAsString(IDs[J]);
    end;

    S := TStringStream.Create(Packet);
    try
      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        CheckEquals(I, P.CsrcCount, 'CsrcCount');

        for J := 0 to P.CsrcCount - 1 do
          CheckEquals(IDs[J],
                      P.CsrcIDs[J],
                      'CSRC ID #' + IntToStr(J));
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromCsrcCount;
var
  I, J:   Integer;
  P:      TIdRTPPacket;
  Packet: String;
  S:      TStream;
begin
  for I := Low(TIdRTPCsrcCount) to High(TIdRTPCsrcCount) do begin
    Packet := Chr($c0 or I) + #$00#$00#$00
            + #$00#$00#$00#$00 // timestamp
            + #$00#$00#$00#$00; // SSRC
    for J := Low(TIdRTPCsrcCount) to I - 1 do
      Packet := Packet + #$00#$00#$00#$00;

    S := TStringStream.Create(Packet);
    try
      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        CheckEquals(I, P.CsrcCount, 'CsrcCount ' + IntToStr(I));
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromHasExtension;
var
  P: TIdRTPPacket;
  S: TStream;
begin
  S := TStringStream.Create(#$10#$00#$00#$04
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00); // header extension
  try
    P := TIdRTPPacket.Create(Self.AVP);
    try
      P.ReadFrom(S);
      Check(P.HasExtension, 'HasExtension set');
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$00#$00#$00#$04
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    P := TIdRTPPacket.Create(Self.AVP);
    try
      P.ReadFrom(S);
      Check(not P.HasExtension, 'HasExtension not set');
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromHasPadding;
var
  P: TIdRTPPacket;
  S: TStream;
begin
  S := TStringStream.Create(#$E0#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    P := TIdRTPPacket.Create(Self.AVP);
    try
      P.ReadFrom(S);
      Check(P.HasPadding, 'HasPadding set');
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$C0#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    P := TIdRTPPacket.Create(Self.AVP);
    try
      P.ReadFrom(S);
      Check(not P.HasPadding, 'HasPadding not set');
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromIsMarker;
var
  P: TIdRTPPacket;
  S: TStream;
begin
  S := TStringStream.Create(#$00#$80#$00#$02
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    P := TIdRTPPacket.Create(Self.AVP);
    try
      P.ReadFrom(S);
      Check(P.IsMarker, 'IsMarker set');
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$00#$00#$00#$02
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    P := TIdRTPPacket.Create(Self.AVP);
    try
      P.ReadFrom(S);
      Check(not P.IsMarker, 'IsMarker not set');
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromPayloadType;
var
  I: Integer;
  P: TIdRTPPacket;
  S: TStream;
begin
  for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do begin
    S := TStringStream.Create(#$00 + Chr(I) + #$00#$00
                            + #$00#$00#$00#$00
                            + #$00#$00#$00#$00);
    try
      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        CheckEquals(I and $7F,
                    P.PayloadType,
                    'PayloadType ' + IntToStr(I));
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromSequenceNo;
var
  I: Word;
  P: TIdRTPPacket;
  S: TStringStream;
begin
  for I := Low(TIdRTPSequenceNo) to High(TIdRTPSequenceNo) do begin
    S := TStringStream.Create(#$00#$00 + EncodeAsString(I)
                            + #$00#$00#$00#$00
                            + #$00#$00#$00#$00);
    try
      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        CheckEquals(I,
                    P.SequenceNo,
                    'SequenceNo ' + IntToStr(I));
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromSynchronizationSourceIdentifier;
var
  I:    Integer;
  P:    TIdRTPPacket;
  S:    TStream;
  SSRC: Cardinal;
begin
  for I := 1 to 1000 do begin
    SSRC := Abs(Random(High(Integer)));
    S := TStringStream.Create(#$00#$00#$00#$00
                            + #$00#$00#$00#$00
                            + EncodeAsString(SSRC));
    try
      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        CheckEquals(SSRC,
                    P.SyncSrcID,
                    'Synchronization SouRCe identifier, ' + IntToStr(I) + 'th test');
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromTimestamp;
var
  I:         Integer;
  P:         TIdRTPPacket;
  S:         TStream;
  Timestamp: Cardinal;
begin
  for I := 1 to 1000 do begin
    Timestamp := Abs(Random(High(Integer)));
    S := TStringStream.Create(#$00#$00#$00#$00
                            + EncodeAsString(Timestamp)
                            + #$00#$00#$00#$00);
    try
      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        CheckEquals(Timestamp,
                    P.Timestamp,
                    'Timestamp, ' + IntToStr(I) + 'th test');
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromVersion;
var
  I: TIdRTPVersion;
  P: TIdRTPPacket;
  S: TStream;
begin
  for I := Low(TIdRTPVersion) to High(TIdRTPVersion) do begin
    S := TStringStream.Create(Chr(I shl 6) + #$00#$00#$00
                            + #$00#$00#$00#$00
                            + #$00#$00#$00#$00);
    try
      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        CheckEquals(I, P.Version, 'Version ' + IntToStr(I));
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromWithPadding;
var
  S: TStringStream;
begin
  S := TStringStream.Create(Chr((RFC3550Version shl 6) or $20) + Chr(Self.T140PT) + #$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + 'This'
                          + ' is '
                          + 'blac'
                          + 'k su'
                          + 'nshi'
                          + 'ne' + #$00#$00 // padding starts here
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$0E); // 14 octets
  try
    Self.Packet.ReadFrom(S);
    CheckEquals(TIdRTPRawPayload,
                Self.Packet.Payload.ClassType,
                'Payload type');
    CheckEqualsW('This is black sunshine',
                 (Self.Packet.Payload as TIdRTPRawPayload).Data,
                 'Data');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromPrintOnInverse;
var
  Input:  TStringStream;
  Output: TStringStream;
begin
  Input := TStringStream.Create(#$80#$80#$04#$05
                              + #$01#$B8#$1A#$E0
                              + #$07#$21#$3F#$F5
                              + #$CA#$FE#$BA#$BE
                              + #$DE#$AD#$BE#$EF
                              + #$FE#$ED#$F0#$0D);
  try
    Output := TStringStream.Create('');
    try
      Self.Packet.ReadFrom(Input);
      Self.Packet.PrintOn(Output);

      CheckEquals(Input.DataString,
                  Output.DataString,
                  'PrintOn is not the inverse operation of ReadFrom');
    finally
      Output.Free;
    end;
  finally
    Input.Free;
  end;
end;

procedure TestTIdRTPPacket.TestReadPayloadStream;
var
  Data:     WideString;
  Expected: TIdRTPRawPayload;
  S:        TStringStream;
begin
  Data := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';
  Expected := TIdRTPRawPayload.Create;
  try
    Expected.SetName(Self.AVP.EncodingFor(Self.T140PT).Name);
    Expected.Data := Data;

    S := TStringStream.Create('');
    try
      Expected.PrintOn(S);
      S.Seek(0, soFromBeginning);

      Self.Packet.PayloadType := Self.T140PT;
      Self.Packet.ReadPayload(S, Self.AVP);

      CheckEquals(Expected.ClassType,
                  Self.Packet.Payload.ClassType,
                  'Payload type');
      CheckEquals(Expected.Data,
                  (Self.Packet.Payload as TIdRTPRawPayload).Data,
                  'Payload data');
    finally
      S.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdRTPPacket.TestReadPayloadPayload;
var
  Data:     WideString;
  Expected: TIdRTPRawPayload;
begin
  Data := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';
  Expected := Self.AVP.EncodingFor(Self.T140PT).Copy as TIdRTPRawPayload;
  try
    Expected.Data := Data;

    Self.Packet.ReadPayload(Expected);

    CheckEquals(Expected.ClassType,
                Self.Packet.Payload.ClassType,
                'Payload type');
    CheckEqualsW(Expected.Data,
                 (Self.Packet.Payload as TIdRTPRawPayload).Data,
                 'Payload data');
  finally
    Expected.Free;
  end;
end;

procedure TestTIdRTPPacket.TestReadPayloadString;
var
  Data:     String;
  Expected: TIdRTPRawPayload;
  S:        TStringStream;
begin
  Data := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';
  Expected := TIdRTPRawPayload.Create;
  try
    Expected.SetName(Self.AVP.EncodingFor(Self.T140PT).Name);
    Expected.Data := Data;

    S := TStringStream.Create('');
    try
      Expected.PrintOn(S);
      S.Seek(0, soFromBeginning);

      Self.Packet.PayloadType := Self.T140PT;
      Self.Packet.ReadPayload(S.DataString, Self.AVP);

      CheckEquals(Expected.ClassType,
                  Self.Packet.Payload.ClassType,
                  'Payload type');
      CheckEqualsW(Expected.Data,
                  (Self.Packet.Payload as TIdRTPRawPayload).Data,
                  'Payload data');
    finally
      S.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdRTPPacket.TestRealLength;
begin
  CheckEquals(12, Self.Packet.RealLength, 'Minimum RealLength');

  Self.Packet.CsrcCount := 4;
  CheckEquals(12 + 4*4,
              Self.Packet.RealLength, 'RealLength with 4 CSRCs');

  Self.Packet.CsrcCount              := 0;
  Self.Packet.HasExtension           := true;
  Self.Packet.HeaderExtension.Length := 4;
  CheckEquals(12 + 4*4 + 4,
              Self.Packet.RealLength, 'RealLength with a header extension');
end;

//******************************************************************************
//* TestTIdRTCPPacket                                                          *
//******************************************************************************
//* TestTIdRTCPPacket Public methods *******************************************

procedure TestTIdRTCPPacket.TestRTCPType;
var
  I: Byte;
begin
  CheckEquals(TIdRTCPSenderReport,
              TIdRTCPPacket.RTCPType(RTCPSenderReport),
              'Sender Report');
  CheckEquals(TIdRTCPReceiverReport,
              TIdRTCPPacket.RTCPType(RTCPReceiverReport),
              'Receiver Report');
  CheckEquals(TIdRTCPSourceDescription,
              TIdRTCPPacket.RTCPType(RTCPSourceDescription),
              'Source Description');
  CheckEquals(TIdRTCPBye,
              TIdRTCPPacket.RTCPType(RTCPGoodbye),
              'Goodbye');
  CheckEquals(TIdRTCPApplicationDefined,
              TIdRTCPPacket.RTCPType(RTCPApplicationDefined),
              'Application Defined');

  for I := Low(Byte) to RTCPSenderReport - 1 do
    Check(nil = TIdRTCPPacket.RTCPType(I), 'Packet Type ' + IntToStr(I));

  for I := RTCPApplicationDefined + 1 to High(Byte) do
    Check(nil = TIdRTCPPacket.RTCPType(I), 'Packet Type ' + IntToStr(I));
end;

procedure TestTIdRTCPPacket.TestIsValidVersionNumber;
var
  P: TIdRTCPBye;
begin
  P := TIdRTCPBye.Create;
  try
    P.Version := RFC3550Version - 1;
    Check(not P.IsValid, 'Bad version number');
    P.Version := RFC3550Version;
    Check(P.IsValid, 'RFC 3550 version number');
  finally
    P.Free;
  end;
end;

//******************************************************************************
//* TestTIdSrcDescChunkItem                                                    *
//******************************************************************************
//* TestTIdSrcDescChunkItem Public methods *************************************

procedure TestTIdSrcDescChunkItem.TestItemType;
begin
  CheckEquals(TIdSDESCanonicalName,
              TIdSrcDescChunkItem.ItemType(SDESCName),
              'SDESCName');
  CheckEquals(TIdSDESUserName,
              TIdSrcDescChunkItem.ItemType(SDESName),
              'SDESName');
  CheckEquals(TIdSDESEmail,
              TIdSrcDescChunkItem.ItemType(SDESEmail),
              'SDESEmail');
  CheckEquals(TIdSDESPhone,
              TIdSrcDescChunkItem.ItemType(SDESPhone),
              'SDESPhone');
  CheckEquals(TIdSDESLocation,
              TIdSrcDescChunkItem.ItemType(SDESLoc),
              'SDESLoc');
  CheckEquals(TIdSDESTool,
              TIdSrcDescChunkItem.ItemType(SDESTool),
              'SDESTool');
  CheckEquals(TIdSDESNote,
              TIdSrcDescChunkItem.ItemType(SDESNote),
              'SDESNote');
  CheckEquals(TIdSDESPriv,
              TIdSrcDescChunkItem.ItemType(SDESPriv),
              'SDESPriv');

  try
    TIdSrcDescChunkItem.ItemType(0);
    Fail('Failed to bail out with unknown SDES');
  except
    on EUnknownSDES do;
  end;
end;

//******************************************************************************
//* TSrcDescChunkItemTestCase                                                  *
//******************************************************************************
//* TSrcDescChunkItemTestCase Public methods ***********************************

procedure TSrcDescChunkItemTestCase.SetUp;
begin
  inherited SetUp;

  Self.Item := Self.ItemType.Create;
end;

procedure TSrcDescChunkItemTestCase.TearDown;
begin
  Self.Item.Free;

  inherited TearDown;
end;

//* TSrcDescChunkItemTestCase Protected methods ********************************

function TSrcDescChunkItemTestCase.ItemType: TIdSrcDescChunkItemClass;
begin
  Result := TIdSrcDescChunkItem;
  Fail(Self.ClassName + ' must override ItemType');
end;

//* TSrcDescChunkItemTestCase Published methods ********************************

procedure TSrcDescChunkItemTestCase.TestPrintOn;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Item.Data := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';

    Self.Item.PrintOn(S);

    Check(Length(S.DataString) > Length(Self.Item.Data) + 1,
          'Too little output');
    CheckEquals(Self.Item.ID,
                Ord(S.DataString[1]), 'ID');
    CheckEquals(Length(Self.Item.Data),
                Ord(S.DataString[2]), 'Length');

    CheckEquals(Self.Item.Data,
                Copy(S.DataString, 3, Length(S.DataString)),
                'Name');
  finally
    S.Free;
  end;
end;

procedure TSrcDescChunkItemTestCase.TestReadFrom;
var
  Name: String;
  S: TStringStream;
begin
  Name := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';

  S := TStringStream.Create(Chr(Self.Item.ID) + Chr(Length(Name))
                          + Name
                          + 'trailing garbage');
  try
    Self.Item.ReadFrom(S);
    CheckEquals(Name, Self.Item.Data, 'Name');
  finally
    S.Free;
  end;
end;

procedure TSrcDescChunkItemTestCase.TestRealLength;
begin
  CheckEquals(2, Self.Item.RealLength, 'Empty data');

  Self.Item.Data := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';
  CheckEquals(2 + Length(Self.Item.Data),
              Self.Item.RealLength,
              'Real data');
end;

//******************************************************************************
//* TestTIdSDESCanonicalName                                                   *
//******************************************************************************
//* TestTIdSDESCanonicalName Protected methods *********************************

function TestTIdSDESCanonicalName.ItemType: TIdSrcDescChunkItemClass;
begin
  Result := TIdSDESCanonicalName;
end;

//* TestTIdSDESCanonicalName Published methods *********************************

procedure TestTIdSDESCanonicalName.TestID;
begin
  CheckEquals(SDESCName,
              Self.Item.ID,
              'ID');
end;

//******************************************************************************
//* TestTIdSDESUserName                                                        *
//******************************************************************************
//* TestTIdSDESUserName Protected methods **************************************

function TestTIdSDESUserName.ItemType: TIdSrcDescChunkItemClass;
begin
  Result := TIdSDESUserName;
end;

//* TestTIdSDESUserName Published methods **************************************

procedure TestTIdSDESUserName.TestID;
begin
  CheckEquals(SDESName,
              Self.Item.ID,
              'ID');
end;

//******************************************************************************
//* TestTIdSDESEmail                                                           *
//******************************************************************************
//* TestTIdSDESEmail Protected methods *****************************************

function TestTIdSDESEmail.ItemType: TIdSrcDescChunkItemClass;
begin
  Result := TIdSDESEmail;
end;

//* TestTIdSDESEmail Published methods *****************************************

procedure TestTIdSDESEmail.TestID;
begin
  CheckEquals(SDESEmail,
              Self.Item.ID,
              'ID');
end;

//******************************************************************************
//* TestTIdSDESPhone                                                           *
//******************************************************************************
//* TestTIdSDESPhone Protected methods *****************************************

function TestTIdSDESPhone.ItemType: TIdSrcDescChunkItemClass;
begin
  Result := TIdSDESPhone;
end;

//* TestTIdSDESPhone Published methods *****************************************

procedure TestTIdSDESPhone.TestID;
begin
  CheckEquals(SDESPhone,
              Self.Item.ID,
              'ID');
end;

//******************************************************************************
//* TestTIdSDESLocation                                                        *
//******************************************************************************
//* TestTIdSDESLocation Protected methods **************************************

function TestTIdSDESLocation.ItemType: TIdSrcDescChunkItemClass;
begin
  Result := TIdSDESLocation;
end;

//* TestTIdSDESLocation Published methods **************************************

procedure TestTIdSDESLocation.TestID;
begin
  CheckEquals(SDESLoc,
              Self.Item.ID,
              'ID');
end;

//******************************************************************************
//* TestTIdSDESTool                                                            *
//******************************************************************************
//* TestTIdSDESTool Protected methods ******************************************

function TestTIdSDESTool.ItemType: TIdSrcDescChunkItemClass;
begin
  Result := TIdSDESTool;
end;

//* TestTIdSDESTool Published methods ******************************************

procedure TestTIdSDESTool.TestID;
begin
  CheckEquals(SDESTool,
              Self.Item.ID,
              'ID');
end;

//******************************************************************************
//* TestTIdSDESNote                                                            *
//******************************************************************************
//* TestTIdSDESNote Protected methods ******************************************

function TestTIdSDESNote.ItemType: TIdSrcDescChunkItemClass;
begin
  Result := TIdSDESNote;
end;

//* TestTIdSDESNote Published methods ******************************************

procedure TestTIdSDESNote.TestID;
begin
  CheckEquals(SDESNote,
              Self.Item.ID,
              'ID');
end;

//******************************************************************************
//* TestTIdSDESPriv                                                            *
//******************************************************************************
//* TestTIdSDESPriv Public methods *********************************************

procedure TestTIdSDESPriv.SetUp;
begin
  inherited SetUp;

  Self.Item := TIdSDESPriv.Create
end;

procedure TestTIdSDESPriv.TearDown;
begin
  Self.Item.Free;

  inherited TearDown;
end;

//* TestTIdSDESPriv Private methods ********************************************

procedure TestTIdSDESPriv.CheckPrintOn(Prefix, Data: String);
var
  Offset: Cardinal;
  S:      TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Item.Prefix := Prefix;
    Self.Item.Data   := Data;

    Self.Item.PrintOn(S);

    Check(Length(S.DataString) > 0, 'Too little output');
    CheckEquals(SDESPriv, Ord(S.DataString[1]), 'PRIV ID');
    CheckEquals(Length(Self.Item.Data) + Length(Self.Item.Prefix) + 1,
                Ord(S.DataString[2]),
                'Length');
    CheckEquals(Length(Self.Item.Prefix),
                Ord(S.DataString[3]),
                'Prefix length');

    Offset := 4;
    CheckEquals(Self.Item.Prefix,
                Copy(S.DataString, Offset, Length(Self.Item.Prefix)),
                'Prefix');
    Inc(Offset, Length(Self.Item.Prefix));
    CheckEquals(Self.Item.Data,
                Copy(S.DataString, Offset, Length(Self.Item.Data)),
                'Data');

  finally
    S.Free;
  end;
end;

//* TestTIdSDESPriv Published methods ******************************************

procedure TestTIdSDESPriv.TestID;
begin
  CheckEquals(SDESPriv,
              Self.Item.ID,
              'ID');
end;

procedure TestTIdSDESPriv.TestPrintOn;
begin
  Self.CheckPrintOn('foo', 'bar');
end;

procedure TestTIdSDESPriv.TestPrintOnEmpty;
begin
  Self.CheckPrintOn('', '');
end;

procedure TestTIdSDESPriv.TestPrintOnNoPrefix;
begin
  Self.CheckPrintOn('', 'bar');
end;

procedure TestTIdSDESPriv.TestPrintOnNoData;
begin
  Self.CheckPrintOn('foo', '');
end;

procedure TestTIdSDESPriv.TestReadFromNoPrefixNoData;
var
  S: TStringStream;
begin
  // The length must still be positive since the prefix length takes up one
  // byte.
  S := TStringStream.Create(Chr(SDESPriv) + #$01#$00);
  try
    Self.Item.ReadFrom(S);
    CheckEquals('', Self.Item.Prefix, 'Prefix');
    CheckEquals('', Self.Item.Data,   'Data');
  finally
    S.Free;
  end;
end;

procedure TestTIdSDESPriv.TestReadFromNoPrefixWithData;
var
  Data: String;
  Len:  Byte;
  S:    TStringStream;
begin
  Data := 'nyarlathotep';
  Len  := Length(Data) + Sizeof(Byte);

  S := TStringStream.Create(Chr(SDESPriv) + Chr(Len) + #$00 + Data);
  try
    Self.Item.ReadFrom(S);
    CheckEquals('', Self.Item.Prefix, 'Prefix');
    CheckEquals(Data, Self.Item.Data, 'Data');
  finally
    S.Free;
  end;
end;

procedure TestTIdSDESPriv.TestReadFromPrefixNoData;
var
  Len:    Byte;
  Prefix: String;
  S:      TStringStream;
begin
  Prefix := 'nyarlathotep';
  Len    := Length(Prefix);

  S := TStringStream.Create(Chr(SDESPriv) + Chr(Len + 1) + Chr(Len) + Prefix);
  try
    Self.Item.ReadFrom(S);
    CheckEquals(Prefix, Self.Item.Prefix, 'Prefix');
    CheckEquals('',     Self.Item.Data,   'Data');
  finally
    S.Free;
  end;
end;

procedure TestTIdSDESPriv.TestReadFromPrefixWithData;
var
  Data:      String;
  DataLen:   Byte;
  Prefix:    String;
  PrefixLen: Byte;
  S:         TStringStream;
begin
  Prefix    := 'Ia!';
  PrefixLen := Length(Prefix);
  Data      := 'Shub-Niggurath!';
  DataLen   := Length(Data) + Length(Prefix) + SizeOf(PrefixLen);

  S := TStringStream.Create(Chr(SDESPriv) + Chr(DataLen) + Chr(PrefixLen)
                          + Prefix + Data);
  try
    Self.Item.ReadFrom(S);
    CheckEquals(Prefix, Self.Item.Prefix, 'Prefix');
    CheckEquals(Data,   Self.Item.Data,   'Data');
  finally
    S.Free;
  end;
end;

procedure TestTIdSDESPriv.TestRealLength;
begin
  CheckEquals(3, Self.Item.RealLength, '"Empty" PRIV');

  Self.Item.Prefix := 'foo';
  CheckEquals(3 + Length(Self.Item.Prefix),
              Self.Item.RealLength,
              'PRIV with prefix');

  Self.Item.Prefix := '';
  Self.Item.Data   := 'bar';
  CheckEquals(3 + Length(Self.Item.Data),
              Self.Item.RealLength,
              'PRIV with data');

  Self.Item.Prefix := 'foo';
  CheckEquals(3 + Length(Self.Item.Prefix) + Length(Self.Item.Data),
              Self.Item.RealLength,
              'PRIV with prefix & data');
end;

procedure TestTIdSDESPriv.TestSetData;
var
  I: Integer;
  S: String;
begin
  S := '';
  for I := 1 to 300 do
    S := S + 'o';

  Self.Item.Data := S;
  CheckEquals(High(Byte) - 1,
              Length(Self.Item.Data),
              'Maximal data with no prefix');

  Self.Item.Prefix := 'ia! shub-niggurath!';
  CheckEquals(High(Byte) - Length(Self.Item.Prefix) - 1,
              Length(Self.Item.Data),
              'Prefix causes truncation of data');

  Self.Item.Prefix := S;
  CheckEquals(0,
              Length(Self.Item.Data),
              'Massive prefix wipes out all data');
  CheckEquals(High(Byte) - 1,
              Length(Self.Item.Prefix),
              'Prefix wasn''t truncated');

  Self.Item.Data := S;
  CheckEquals(0,
              Length(Self.Item.Data),
              'Prefix gets precedence over data');
end;

//******************************************************************************
//* TestTIdRTCPSrcDescChunk                                                    *
//******************************************************************************
//* TestTIdRTCPSrcDescChunk Public methods *************************************

procedure TestTIdRTCPSrcDescChunk.SetUp;
begin
  inherited SetUp;

  Self.Chunk := TIdRTCPSrcDescChunk.Create;
end;

procedure TestTIdRTCPSrcDescChunk.TearDown;
begin
  Self.Chunk.Free;

  inherited TearDown;
end;

//* TestTIdRTCPSrcDescChunk Published methods **********************************

procedure TestTIdRTCPSrcDescChunk.TestPrintOn;
var
  CName:  TStringStream;
  Name:   String;
  Offset: Cardinal;
  S:      TStringStream;
begin
  Name := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';

  Self.Chunk.SyncSrcID := $deadbeef;
  Self.Chunk.AddCanonicalName(Name);
  Self.Chunk.AddCanonicalName(Name);

  CName := TStringStream.Create('');
  try
    Self.Chunk.Items[0].PrintOn(CName);

    S := TStringStream.Create('');
    try
      Self.Chunk.PrintOn(S);

      CheckEquals($de, Ord(S.DataString[1]), 'MSB');
      CheckEquals($ad, Ord(S.DataString[2]), 'MSB - 1');
      CheckEquals($be, Ord(S.DataString[3]), 'LSB + 1');
      CheckEquals($ef, Ord(S.DataString[4]), 'LSB');
      Offset := 5;

      CheckEquals(CName.DataString,
                  Copy(S.DataString, Offset, Length(CName.DataString)),
                  'First CNAME');

      Inc(Offset, Self.Chunk.Items[0].RealLength);
      CheckEquals(CName.DataString,
                  Copy(S.DataString, Offset, Length(CName.DataString)),
                  'Second CNAME');
    finally
      S.Free;
    end;
  finally
    CName.Free;
  end;
end;

procedure TestTIdRTCPSrcDescChunk.TestPrintOnByteAlign;
var
  S: TStringStream;
begin
  Self.Chunk.AddCanonicalName('A');

  S := TStringStream.Create('');
  try
    Self.Chunk.PrintOn(S);

    Check(Length(S.DataString) > 7, 'Too little output');
    CheckEquals($00, Ord(S.DataString[8]), 'Zero padding');
  finally
    S.Free;
  end;

  (Self.Chunk.Items[0] as TIdSDESCanonicalName).Data := 'AAAA';
  S := TStringStream.Create('');
  try
    Self.Chunk.PrintOn(S);

    Check(Length(S.DataString) > 11, 'Too little output');
    CheckEquals('AAAA'#$00#$00,
                Copy(S.DataString, 7, Length(S.DataString)),
                'CNAME name');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSrcDescChunk.TestReadFrom;
var
  Name: String;
  S:    TStringStream;
begin
  Name := 'shub-niggurath';

  S := TStringStream.Create(#$de#$ad#$be#$ef
                          + Chr(SDESCName)
                          + Chr(Length(Name)) + Name);
  try
    Self.Chunk.ReadFrom(S);

    CheckEquals(IntToHex($deadbeef, 8),
                IntToHex(Self.Chunk.SyncSrcID, 8),
                'SSRC');
    CheckEquals(1,
                Self.Chunk.ItemCount,
                'ItemCount');
    CheckEquals(SDESCName,
                Self.Chunk.Items[0].ID, 'Item ID');
    CheckEquals(Name,
                (Self.Chunk.Items[0] as TIdSDESCanonicalName).Data,
                'CNAME');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSrcDescChunk.TestReadFromMultipleChunks;
var
  Garbage:   String;
  Name1:     String;
  Name2:     String;
  S:         TStringStream;
begin
  Garbage := 'trailing garbage';
  Name1   := 'ia!';
  Name2   := 'shub-niggurath!';

  S := TStringStream.Create(#$de#$ad#$be#$ef
                          + Chr(SDESCName)
                          + Chr(Length(Name1)) + Name1
                          + Chr(SDESCName)
                          + Chr(Length(Name2)) + Name2
                          + #$00#$00 + Garbage);
  try
    Self.Chunk.ReadFrom(S);

    CheckEquals(IntToHex($deadbeef, 8),
                IntToHex(Self.Chunk.SyncSrcID, 8),
                'SSRC');
    CheckEquals(2,
                Self.Chunk.ItemCount,
                'ItemCount');
    CheckEquals(SDESCName,
                Self.Chunk.Items[0].ID, 'Item 1 ID');
    CheckEquals(Name1,
                (Self.Chunk.Items[0] as TIdSDESCanonicalName).Data,
                'Item 1 CNAME');
    CheckEquals(SDESCName,
                Self.Chunk.Items[1].ID, 'Item 2 ID');
    CheckEquals(Name2,
                (Self.Chunk.Items[1] as TIdSDESCanonicalName).Data,
                'Item 2 CNAME');
    CheckEquals(Garbage,
                ReadString(S, Length(Garbage)),
                'Too much read');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSrcDescChunk.TestRealLength;
begin
  CheckEquals(4, Self.Chunk.RealLength, 'Empty chunk');

  Self.Chunk.AddCanonicalName('A');
  CheckEquals(4 + 2 + Length('A'),
              Self.Chunk.RealLength,
              'Chunk with a CNAME');

  Self.Chunk.AddCanonicalName('A');
  CheckEquals(4 + 2*(2 + Length('A')),
              Self.Chunk.RealLength,
              'Chunk with two CNAMEs');
end;

//******************************************************************************
//* TRTCPPacketTestCase                                                        *
//******************************************************************************
//* TRTCPPacketTestCase Public methods *****************************************

procedure TRTCPPacketTestCase.SetUp;
begin
  inherited SetUp;

  Self.Packet := TIdRTCPPacket.RTCPType(Self.PacketType).Create;
end;

procedure TRTCPPacketTestCase.TearDown;
begin
  Self.Packet.Free;

  inherited TearDown;
end;

//* TRTCPPacketTestCase Protected methods **************************************

function TRTCPPacketTestCase.PacketType: Byte;
begin
  Result := 0;
  Fail(Self.ClassName + ' must override PacketType');
end;

//* TRTCPPacketTestCase Published methods **************************************

procedure TRTCPPacketTestCase.TestCopy;
var
  Copy:    TIdRTPBasePacket;
  Expected: TStringStream;
  Received: TStringStream;
begin
  Expected := TStringStream.Create('');
  try
    Received := TStringStream.Create('');
    try
      Self.Packet.PrintOn(Expected);

      Copy := Self.Packet.Copy;
      try
        Copy.PrintOn(Received);

        CheckEquals(Expected.DataString,
                    Received.DataString,
                    Self.Packet.ClassName + ' Copy');
      finally
        Copy.Free;
      end;
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TRTCPPacketTestCase.TestIsBye;
begin
  Check(not Self.Packet.IsBye,
        Self.Packet.ClassName  + ' marked as being a bye');
end;

procedure TRTCPPacketTestCase.TestIsReceiverReport;
begin
  Check(not Self.Packet.IsReceiverReport,
        Self.Packet.ClassName  + ' marked as being a receiver report');
end;

procedure TRTCPPacketTestCase.TestIsSenderReport;
begin
  Check(not Self.Packet.IsSenderReport,
        Self.Packet.ClassName  + ' marked as being a sender report');
end;

procedure TRTCPPacketTestCase.TestIsSourceDescription;
begin
  Check(not Self.Packet.IsSourceDescription,
        Self.Packet.ClassName  + ' marked as being a source description');
end;

procedure TRTCPPacketTestCase.TestIsRTCP;
begin
  Check(Self.Packet.IsRTCP, 'IsRTCP');
end;

procedure TRTCPPacketTestCase.TestIsRTP;
begin
  Check(not Self.Packet.IsRTP, 'IsRTP');
end;

procedure TRTCPPacketTestCase.TestPacketType;
begin
  CheckEquals(Self.PacketType,
              Self.Packet.PacketType,
              'PacketType');
end;

procedure TRTCPPacketTestCase.TestPrintOnHasPadding;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Packet.PrintOn(S);
    Check(S.DataString <> '', 'Too little input');
    CheckEquals(RFC3550Version shl 6,
                Ord(S.DataString[1]) and $c0,
                'Has no padding');
  finally
    S.Free;
  end;

  S := TStringStream.Create('');
  try
    Self.Packet.HasPadding := true;
    Self.Packet.Length := 100;
    Self.Packet.PrintOn(S);
    Check(S.DataString <> '', 'Too little input');
    CheckEquals((RFC3550Version shl 6) or $20,
                Ord(S.DataString[1]) and $e0,
                'Has padding');
  finally
    S.Free;
  end;
end;

procedure TRTCPPacketTestCase.TestPrintOnPacketType;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Packet.PrintOn(S);

    Check(Length(S.DataString) > 1, 'Too little output');

    // This looks odd, but remember that Self.Packet.PacketType returns
    // a constant - Packet doesn't set PacketType based on the source
    // stream.
    CheckEquals(Self.Packet.PacketType,
                Ord(S.DataString[2]),
                'Packet type');
  finally
    S.Free;
  end;
end;

procedure TRTCPPacketTestCase.TestPrintOnSyncSrcId;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Packet.SyncSrcID := $decafbad;
    Self.Packet.PrintOn(S);

    Check(Length(S.DataString) > 7, 'Too little output');
    CheckEquals($de, Ord(S.DataString[5]), 'LSB');
    CheckEquals($ca, Ord(S.DataString[6]), 'LSB + 1');
    CheckEquals($fb, Ord(S.DataString[7]), 'MSB - 1');
    CheckEquals($ad, Ord(S.DataString[8]), 'MSB');
  finally
    S.Free;
  end;
end;

procedure TRTCPPacketTestCase.TestPrintOnVersion;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Packet.PrintOn(S);
    Check(S.DataString <> '', 'Too little input');
    CheckEquals(RFC3550Version shl 6,
                Ord(S.DataString[1]) and $c0,
                'Version');
  finally
    S.Free;
  end;
end;

procedure TRTCPPacketTestCase.TestPrintOnWithPadding;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Packet.HasPadding := true;
    Self.Packet.Length := 200;
    Self.Packet.PrintOn(S);

    CheckEquals(Self.Packet.Length,
                Length(S.DataString),
                'Too little or too much output');
    CheckEquals(Self.Packet.Length - Self.Packet.RealLength,
                Ord(S.DataString[Length(S.DataString)]),
                'Pad length');
  finally
    S.Free;
  end;
end;

procedure TRTCPPacketTestCase.TestReadFromHasPadding;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$00 + Chr(Self.PacketType)+ #$00#$06
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    Self.Packet.ReadFrom(S);

    Check(not Self.Packet.HasPadding, 'HasPadding set');
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$20 + Chr(Self.PacketType)+ #$00#$06
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    Self.Packet.ReadFrom(S);

    Check(Self.Packet.HasPadding, 'HasPadding not set');
  finally
    S.Free;
  end;
end;

procedure TRTCPPacketTestCase.TestReadFromLength;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$00 + Chr(Self.PacketType) + #$00#$06
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    Self.Packet.ReadFrom(S);

    CheckEquals(IntToHex($0006, 4),
                IntToHex(Self.Packet.Length, 4),
                'Length');
  finally
    S.Free;
  end;
end;

procedure TRTCPPacketTestCase.TestReadFromPacketType;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$00 + Chr(Self.PacketType)+ #$00#$06
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    Self.Packet.ReadFrom(S);

    CheckEquals(Self.PacketType,
                Self.Packet.PacketType,
                'PacketType');
  finally
    S.Free;
  end;
end;

procedure TRTCPPacketTestCase.TestReadFromVersion;
var
  I: TIdRTPVersion;
  S: TStream;
begin
  for I := Low(TIdRTPVersion) to High(TIdRTPVersion) do begin
    S := TStringStream.Create(Chr(I shl 6) + Chr(Self.PacketType)+ #$00#$06
                            + #$00#$00#$00#$00
                            + #$00#$00#$00#$00
                            + #$00#$00#$00#$00
                            + #$00#$00#$00#$00
                            + #$00#$00#$00#$00
                            + #$00#$00#$00#$00);
    try
      Self.Packet.ReadFrom(S);
      CheckEquals(I, Self.Packet.Version, 'Version ' + IntToStr(I));
    finally
      S.Free;
    end;
  end;
end;

//******************************************************************************
//* TestTIdRTCPReceiverReport                                                  *
//******************************************************************************
//* TestTIdRTCPReceiverReport Public methods ***********************************

procedure TestTIdRTCPReceiverReport.SetUp;
begin
  inherited SetUp;

  Self.ReceiverReport := Self.Packet as TIdRTCPReceiverReport;
end;

//* TestTIdRTCPReceiverReport Protected methods ********************************

function TestTIdRTCPReceiverReport.PacketType: Byte;
begin
  Result := RTCPReceiverReport;
end;

//* TestTIdRTCPReceiverReport Private methods ********************************

function TestTIdRTCPReceiverReport.MultipleReportBlockReceiverReport: String;
begin
  Result     := SampleReceiverReport + SampleReportBlock;
  Result[1]  := Chr($82); // 2 reports
  Result[40] := Chr(Ord(Result[40]) + 1); // Cumulative packet loss of 2nd report
end;

procedure TestTIdRTCPReceiverReport.ReadFromSampleReceiverReport;
var
  S: TStringStream;
begin
  S := TStringStream.Create(SampleReceiverReport);
  try
    Self.ReceiverReport.ReadFrom(S);
  finally
    S.Free;
  end;
end;

//* TestTIdRTCPReceiverReport Published methods ********************************

procedure TestTIdRTCPReceiverReport.TestGetAllSrcIDs;
var
  I, J: Integer;
  IDs:  TCardinalDynArray;
begin
  Self.ReceiverReport.ReceptionReportCount := 3;
  Self.ReceiverReport.Reports[0].SyncSrcID := $decafbad;
  Self.ReceiverReport.Reports[1].SyncSrcID := $deadbeef;
  Self.ReceiverReport.Reports[2].SyncSrcID := $f00dd00d;

  IDs := Self.ReceiverReport.GetAllSrcIDs;
  CheckEquals(Self.ReceiverReport.ReceptionReportCount + 1,
              Length(IDs),
              'Number of IDs');

  CheckEquals(IntToHex(Self.ReceiverReport.SyncSrcID, 8),
              IntToHex(IDs[Low(IDs)], 8),
              'Source ID the First');

  J := 0;
  for I := Low(IDs) + 1 to High(IDs) do begin
    CheckEquals(IntToHex(Self.ReceiverReport.Reports[J].SyncSrcID, 8),
                IntToHex(IDs[I], 8),
                'Source ID ' + IntToStr(I));
    Inc(J);
  end;
end;

procedure TestTIdRTCPReceiverReport.TestIsReceiverReport;
begin
  Check(Self.Packet.IsReceiverReport,
        Self.Packet.ClassName  + ' marked as being a receiver report');
end;

procedure TestTIdRTCPReceiverReport.TestPrintOnSyncSrcId;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.ReceiverReport.SyncSrcID := $decafbad;
    Self.ReceiverReport.PrintOn(S);

    Check(Length(S.DataString) > 7, 'Too little output');
    CheckEquals($de, Ord(S.DataString[5]), 'LSB');
    CheckEquals($ca, Ord(S.DataString[6]), 'LSB + 1');
    CheckEquals($fb, Ord(S.DataString[7]), 'MSB - 1');
    CheckEquals($ad, Ord(S.DataString[8]), 'MSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPReceiverReport.TestPrintOnReceptionReportCount;
var
  I: TIdFiveBitInt;
  S: TStringStream;
begin
  for I := Low(TIdFiveBitInt) to High(TIdFiveBitInt) do begin
    S := TStringStream.Create('');
    try
      Self.ReceiverReport.ReceptionReportCount := I;
      Self.ReceiverReport.PrintOn(S);

      Check(Length(S.DataString) > 0, 'Too little output, ' + IntToStr(I));
      CheckEquals($80 or I, Ord(S.DataString[1]), 'RC, ' + IntToStr(I));
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTCPReceiverReport.TestPrintOnReportBlocks;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.ReceiverReport.ReceptionReportCount          := 1;
    Self.ReceiverReport.Reports[0].SyncSrcID          := $deadbeef;
    Self.ReceiverReport.Reports[0].FractionLost       := $0f;
    Self.ReceiverReport.Reports[0].CumulativeLoss     := $feed;
    Self.ReceiverReport.Reports[0].HighestSeqNo       := $10111213;
    Self.ReceiverReport.Reports[0].InterArrivalJitter := $00fedfed;
    Self.ReceiverReport.Reports[0].LastSenderReport   := $ea7ea7ea;
    Self.ReceiverReport.Reports[0].DelaySinceLastSR   := $0000ffef;

    Self.ReceiverReport.PrintOn(S);
    Check(Length(S.DataString) > 31, 'Too little output');
    CheckEquals($de, Ord(S.DataString[9]), 'SSRC MSB');
    CheckEquals($ad, Ord(S.DataString[10]), 'SSRC MSB - 1');
    CheckEquals($be, Ord(S.DataString[11]), 'SSRC LSB + 1');
    CheckEquals($ef, Ord(S.DataString[12]), 'SSRC LSB');
    CheckEquals($0f, Ord(S.DataString[13]), 'Fraction lost');
    CheckEquals($00, Ord(S.DataString[14]), 'Cumulative loss MSB');
    CheckEquals($fe, Ord(S.DataString[15]), 'Cumulative loss MSB - 1');
    CheckEquals($ed, Ord(S.DataString[16]), 'Cumulative loss LSB');
    CheckEquals($10, Ord(S.DataString[17]), 'Highest sequence no MSB');
    CheckEquals($11, Ord(S.DataString[18]), 'Highest sequence no MSB - 1');
    CheckEquals($12, Ord(S.DataString[19]), 'Highest sequence no LSB + 1');
    CheckEquals($13, Ord(S.DataString[20]), 'Highest sequence no LSB');
    CheckEquals($00, Ord(S.DataString[21]), 'Interarrival jitter MSB');
    CheckEquals($fe, Ord(S.DataString[22]), 'Interarrival jitter MSB - 1');
    CheckEquals($df, Ord(S.DataString[23]), 'Interarrival jitter LSB + 1');
    CheckEquals($ed, Ord(S.DataString[24]), 'Interarrival jitter LSB');
    CheckEquals($ea, Ord(S.DataString[25]), 'Last SR MSB');
    CheckEquals($7e, Ord(S.DataString[26]), 'Last SR MSB - 1');
    CheckEquals($a7, Ord(S.DataString[27]), 'Last SR LSB + 1');
    CheckEquals($ea, Ord(S.DataString[28]), 'Last SR LSB');
    CheckEquals($00, Ord(S.DataString[29]), 'Delay since last SR MSB');
    CheckEquals($00, Ord(S.DataString[30]), 'Delay since last SR MSB - 1');
    CheckEquals($ff, Ord(S.DataString[31]), 'Delay since last SR LSB + 1');
    CheckEquals($ef, Ord(S.DataString[32]), 'Delay since last SR LSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPReceiverReport.TestRealLength;
const
  MinimumLength     = 2*4;
  ReportBlockLength = 6*4;
begin
  CheckEquals(MinimumLength,
              Self.ReceiverReport.RealLength,
              'Minimum RealLength');

  Self.ReceiverReport.ReceptionReportCount := 1;
  CheckEquals(MinimumLength + ReportBlockLength,
              Self.ReceiverReport.RealLength,
              'RealLength with one report block');

  Self.ReceiverReport.ReceptionReportCount := 5;
  CheckEquals(MinimumLength + 5*ReportBlockLength,
              Self.ReceiverReport.RealLength,
              'RealLength with five report blocks');

  Self.ReceiverReport.Extension := '12345';
  CheckEquals(MinimumLength + 5*ReportBlockLength + 5,
              Self.ReceiverReport.RealLength,
              'RealLength with five report blocks and a 5-octet extension');
end;

procedure TestTIdRTCPReceiverReport.TestReadFromReceptionReportCount;
begin
  Self.ReadFromSampleReceiverReport;
  CheckEquals(1,
              Self.ReceiverReport.ReceptionReportCount,
              'Reception report count');
end;

procedure TestTIdRTCPReceiverReport.TestReadFromReport;
var
  Report: TIdRTCPReportBlock;
begin
  Self.ReadFromSampleReceiverReport;

  Report := Self.ReceiverReport.Reports[0];

  CheckEquals(IntToHex($deadbeef, 8),
              IntToHex(Report.SyncSrcID, 8),
              'SSRC_1');
  CheckEquals($0f,
              Report.FractionLost,
              'Fraction lost');
  CheckEquals($feed,
              Report.CumulativeLoss,
              'Cumulative packet loss');
  CheckEquals(IntToHex($10111213, 8),
              IntToHex(Report.HighestSeqNo, 8),
              'Highest received sequence no');
  CheckEquals(IntToHex($00fedfed, 8),
              IntToHex(Report.InterArrivalJitter, 8),
              'Interarrival jitter');
  CheckEquals(IntToHex($ea7ea7ea, 8),
              IntToHex(Report.LastSenderReport, 8),
              'Last SR');
  CheckEquals(IntToHex($0000ffef, 8),
              IntToHex(Report.DelaySinceLastSR, 8),
              'Delay since last SR');
end;

procedure TestTIdRTCPReceiverReport.TestReadFromReports;
var
  S: TStringStream;
begin
  S := TStringStream.Create(Self.MultipleReportBlockReceiverReport);
  try
    Self.ReceiverReport.ReadFrom(S);
    CheckEquals(2,
                Self.ReceiverReport.ReceptionReportCount,
                'Reception report count');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPReceiverReport.TestReportAt;
var
  I: Integer;
  S: TStringStream;
begin
  try
    Self.ReceiverReport.Reports[0];
    Fail('Failed to raise error accessing first element of an empty list');
  except
    on EListError do;
  end;

  try
    Self.ReceiverReport.Reports[-1];
    Fail('Failed to raise error on bad index');
  except
    on EListError do;
  end;

  S := TStringStream.Create(Self.MultipleReportBlockReceiverReport);
  try
    Self.ReceiverReport.ReadFrom(S);
  finally
    S.Free;
  end;

  for I := 0 to 1 do
    CheckEquals($feed + I,
                Self.ReceiverReport.Reports[I].CumulativeLoss,
                'Wrong report off index ' + IntToStr(I));

  try
    Self.ReceiverReport.Reports[3];
    Fail('Failed to raise error accessing out-of-range index');
  except
    on EListError do;
  end;
end;

//******************************************************************************
//* TestTIdRTCPSenderReport                                                    *
//******************************************************************************
//* TestTIdRTCPSenderReport Public methods *************************************

procedure TestTIdRTCPSenderReport.SetUp;
begin
  inherited SetUp;

  Self.SenderReport := Self.Packet as TIdRTCPSenderReport;
end;

//* TestTIdRTCPSenderReport Protected methods **********************************

function TestTIdRTCPSenderReport.PacketType: Byte;
begin
  Result := RTCPSenderReport;
end;

//* TestTIdRTCPSenderReport Private methods ******************************

function TestTIdRTCPSenderReport.MultipleReportBlockSenderReport: String;
begin
  Result     := SampleSenderReport + SampleReportBlock;
  Result[1]  := Chr($82); // 2 reports
  Result[60] := Chr(Ord(Result[60]) + 1); // Cumulative packet loss of 2nd report
end;

procedure TestTIdRTCPSenderReport.ReadFromSampleSenderReport;
var
  S: TStringStream;
begin
  S := TStringStream.Create(SampleSenderReport);
  try
    Self.SenderReport.ReadFrom(S);
  finally
    S.Free;
  end;
end;

//* TestTIdRTCPSenderReport Published methods **********************************

procedure TestTIdRTCPSenderReport.TestGetAllSrcIDs;
var
  I, J: Integer;
  IDs:  TCardinalDynArray;
begin
  Self.SenderReport.ReceptionReportCount := 3;
  Self.SenderReport.Reports[0].SyncSrcID := $decafbad;
  Self.SenderReport.Reports[1].SyncSrcID := $deadbeef;
  Self.SenderReport.Reports[2].SyncSrcID := $f00dd00d;

  IDs := Self.SenderReport.GetAllSrcIDs;
  CheckEquals(Self.SenderReport.ReceptionReportCount + 1,
              Length(IDs),
              'Number of IDs');

  CheckEquals(IntToHex(Self.SenderReport.SyncSrcID, 8),
              IntToHex(IDs[Low(IDs)], 8),
              'Source ID the First');

  J := 0;
  for I := Low(IDs) + 1 to High(IDs) do begin
    CheckEquals(IntToHex(Self.SenderReport.Reports[J].SyncSrcID, 8),
                IntToHex(IDs[I], 8),
                'Source ID ' + IntToStr(I));
    Inc(J);
  end;
end;

procedure TestTIdRTCPSenderReport.TestIsSenderReport;
begin
  Check(Self.Packet.IsSenderReport,
        Self.Packet.ClassName  + ' marked as being a sender report');
end;

procedure TestTIdRTCPSenderReport.TestPrintOnSyncSrcId;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SenderReport.SyncSrcID := $decafbad;
    Self.SenderReport.PrintOn(S);

    Check(Length(S.DataString) > 7, 'Too little output');
    CheckEquals($de, Ord(S.DataString[5]), 'LSB');
    CheckEquals($ca, Ord(S.DataString[6]), 'LSB + 1');
    CheckEquals($fb, Ord(S.DataString[7]), 'MSB - 1');
    CheckEquals($ad, Ord(S.DataString[8]), 'MSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSenderReport.TestPrintOnNTPTimestamp;
var
  S: TStringStream;
  T: TIdNTPTimestamp;
begin
  S := TStringStream.Create('');
  try
    T.IntegerPart    := $decafbad;
    T.FractionalPart := $beeff00d;

    Self.SenderReport.NTPTimestamp := T;
    Self.SenderReport.PrintOn(S);

    Check(Length(S.DataString) > 15, 'Too little output');
    CheckEquals($de, Ord(S.DataString[9]),  'Integer part, LSB');
    CheckEquals($ca, Ord(S.DataString[10]), 'Integer part, LSB + 1');
    CheckEquals($fb, Ord(S.DataString[11]), 'Integer part, MSB - 1');
    CheckEquals($ad, Ord(S.DataString[12]), 'Integer part, MSB');

    CheckEquals($be, Ord(S.DataString[13]), 'Fraction part, LSB');
    CheckEquals($ef, Ord(S.DataString[14]), 'Fraction part, LSB + 1');
    CheckEquals($f0, Ord(S.DataString[15]), 'Fraction part, MSB - 1');
    CheckEquals($0d, Ord(S.DataString[16]), 'Fraction part, MSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSenderReport.TestPrintOnOctetCount;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SenderReport.OctetCount := $decafbad;
    Self.SenderReport.PrintOn(S);

    Check(Length(S.DataString) > 27, 'Too little output');
    CheckEquals($de, Ord(S.DataString[25]), 'Integer part, LSB');
    CheckEquals($ca, Ord(S.DataString[26]), 'Integer part, LSB + 1');
    CheckEquals($fb, Ord(S.DataString[27]), 'Integer part, MSB - 1');
    CheckEquals($ad, Ord(S.DataString[28]), 'Integer part, MSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSenderReport.TestPrintOnPacketCount;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SenderReport.PacketCount := $decafbad;
    Self.SenderReport.PrintOn(S);

    Check(Length(S.DataString) > 23, 'Too little output');
    CheckEquals($de, Ord(S.DataString[21]), 'Integer part, LSB');
    CheckEquals($ca, Ord(S.DataString[22]), 'Integer part, LSB + 1');
    CheckEquals($fb, Ord(S.DataString[23]), 'Integer part, MSB - 1');
    CheckEquals($ad, Ord(S.DataString[24]), 'Integer part, MSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSenderReport.TestPrintOnReceptionReportCount;
var
  I: TIdFiveBitInt;
  S: TStringStream;
begin
  for I := Low(TIdFiveBitInt) to High(TIdFiveBitInt) do begin
    S := TStringStream.Create('');
    try
      Self.SenderReport.ReceptionReportCount := I;
      Self.SenderReport.PrintOn(S);

      Check(Length(S.DataString) > 0, 'Too little output, ' + IntToStr(I));
      CheckEquals($80 or I, Ord(S.DataString[1]), 'RC, ' + IntToStr(I));
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTCPSenderReport.TestPrintOnReportBlocks;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SenderReport.ReceptionReportCount          := 1;
    Self.SenderReport.Reports[0].SyncSrcID          := $deadbeef;
    Self.SenderReport.Reports[0].FractionLost       := $0f;
    Self.SenderReport.Reports[0].CumulativeLoss     := $feed;
    Self.SenderReport.Reports[0].HighestSeqNo       := $10111213;
    Self.SenderReport.Reports[0].InterArrivalJitter := $00fedfed;
    Self.SenderReport.Reports[0].LastSenderReport   := $ea7ea7ea;
    Self.SenderReport.Reports[0].DelaySinceLastSR   := $0000ffef;

    Self.SenderReport.PrintOn(S);
    Check(Length(S.DataString) > 51, 'Too little output');
    CheckEquals($de, Ord(S.DataString[29]), 'SSRC MSB');
    CheckEquals($ad, Ord(S.DataString[30]), 'SSRC MSB - 1');
    CheckEquals($be, Ord(S.DataString[31]), 'SSRC LSB + 1');
    CheckEquals($ef, Ord(S.DataString[32]), 'SSRC LSB');
    CheckEquals($0f, Ord(S.DataString[33]), 'Fraction lost');
    CheckEquals($00, Ord(S.DataString[34]), 'Cumulative loss MSB');
    CheckEquals($fe, Ord(S.DataString[35]), 'Cumulative loss MSB - 1');
    CheckEquals($ed, Ord(S.DataString[36]), 'Cumulative loss LSB');
    CheckEquals($10, Ord(S.DataString[37]), 'Highest sequence no MSB');
    CheckEquals($11, Ord(S.DataString[38]), 'Highest sequence no MSB - 1');
    CheckEquals($12, Ord(S.DataString[39]), 'Highest sequence no LSB + 1');
    CheckEquals($13, Ord(S.DataString[40]), 'Highest sequence no LSB');
    CheckEquals($00, Ord(S.DataString[41]), 'Interarrival jitter MSB');
    CheckEquals($fe, Ord(S.DataString[42]), 'Interarrival jitter MSB - 1');
    CheckEquals($df, Ord(S.DataString[43]), 'Interarrival jitter LSB + 1');
    CheckEquals($ed, Ord(S.DataString[44]), 'Interarrival jitter LSB');
    CheckEquals($ea, Ord(S.DataString[45]), 'Last SR MSB');
    CheckEquals($7e, Ord(S.DataString[46]), 'Last SR MSB - 1');
    CheckEquals($a7, Ord(S.DataString[47]), 'Last SR LSB + 1');
    CheckEquals($ea, Ord(S.DataString[48]), 'Last SR LSB');
    CheckEquals($00, Ord(S.DataString[49]), 'Delay since last SR MSB');
    CheckEquals($00, Ord(S.DataString[50]), 'Delay since last SR MSB - 1');
    CheckEquals($ff, Ord(S.DataString[51]), 'Delay since last SR LSB + 1');
    CheckEquals($ef, Ord(S.DataString[52]), 'Delay since last SR LSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSenderReport.TestPrintOnRTPTimestamp;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SenderReport.RTPTimestamp := $decafbad;
    Self.SenderReport.PrintOn(S);

    Check(Length(S.DataString) > 19, 'Too little output');
    CheckEquals($de, Ord(S.DataString[17]), 'Integer part, LSB');
    CheckEquals($ca, Ord(S.DataString[18]), 'Integer part, LSB + 1');
    CheckEquals($fb, Ord(S.DataString[19]), 'Integer part, MSB - 1');
    CheckEquals($ad, Ord(S.DataString[20]), 'Integer part, MSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSenderReport.TestPrintOnWord;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SenderReport.Length := $beef;
    Self.SenderReport.PrintOn(S);

    Check(Length(S.DataString) > 3, 'Too little output');
    CheckEquals($be, Ord(S.DataString[3]), 'Length MSB');
    CheckEquals($ef, Ord(S.DataString[4]), 'Length LSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSenderReport.TestReadFromNTPTimestamp;
begin
  Self.ReadFromSampleSenderReport;
  CheckEquals(IntToHex($a1a2a3a4, 8),
              IntToHex(Self.SenderReport.NTPTimestamp.IntegerPart, 8),
              'NTP timestamp, Integer');
  CheckEquals(IntToHex($a5a6a7a8, 8),
              IntToHex(Self.SenderReport.NTPTimestamp.FractionalPart, 8),
              'NTP timestamp, Integer');
end;

procedure TestTIdRTCPSenderReport.TestReadFromOctetCount;
begin
  Self.ReadFromSampleSenderReport;
  CheckEquals(IntToHex($0000f00d, 8),
              IntToHex(Self.SenderReport.OctetCount, 8),
              'Octet count');
end;

procedure TestTIdRTCPSenderReport.TestReadFromPacketCount;
begin
  Self.ReadFromSampleSenderReport;
  CheckEquals(IntToHex($00000fed, 8),
              IntToHex(Self.SenderReport.PacketCount, 8),
              'Packet count');
end;

procedure TestTIdRTCPSenderReport.TestReadFromReceptionReportCount;
begin
  Self.ReadFromSampleSenderReport;
  CheckEquals(1,
              Self.SenderReport.ReceptionReportCount,
              'Reception report count');
end;

procedure TestTIdRTCPSenderReport.TestReadFromReport;
var
  Report: TIdRTCPReportBlock;
begin
  Self.ReadFromSampleSenderReport;

  Report := Self.SenderReport.Reports[0];

  CheckEquals(IntToHex($deadbeef, 8),
              IntToHex(Report.SyncSrcID, 8),
              'SSRC_1');
  CheckEquals($0f,
              Report.FractionLost,
              'Fraction lost');
  CheckEquals($feed,
              Report.CumulativeLoss,
              'Cumulative packet loss');
  CheckEquals(IntToHex($10111213, 8),
              IntToHex(Report.HighestSeqNo, 8),
              'Highest received sequence no');
  CheckEquals(IntToHex($00fedfed, 8),
              IntToHex(Report.InterArrivalJitter, 8),
              'Interarrival jitter');
  CheckEquals(IntToHex($ea7ea7ea, 8),
              IntToHex(Report.LastSenderReport, 8),
              'Last SR');
  CheckEquals(IntToHex($0000ffef, 8),
              IntToHex(Report.DelaySinceLastSR, 8),
              'Delay since last SR');
end;

procedure TestTIdRTCPSenderReport.TestReadFromReports;
var
  S: TStringStream;
begin
  S := TStringStream.Create(Self.MultipleReportBlockSenderReport);
  try
    Self.SenderReport.ReadFrom(S);
    CheckEquals(2,
                Self.SenderReport.ReceptionReportCount,
                'Reception report count');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSenderReport.TestReadFromRTPTimestamp;
begin
  Self.ReadFromSampleSenderReport;
  CheckEquals(IntToHex($a9aaabac, 8),
              IntToHex(Self.SenderReport.RTPTimestamp, 8),
              'RTP timestamp');
end;

procedure TestTIdRTCPSenderReport.TestReadFromSyncSrcID;
begin
  Self.ReadFromSampleSenderReport;
  CheckEquals(IntToHex($decafbad, 8),
              IntToHex(Self.SenderReport.SyncSrcID, 8),
              'SSRC');
end;

procedure TestTIdRTCPSenderReport.TestReadFromTruncatedStream;
var
  Report: String;
  S: TStringStream;
begin
  Report := Self.MultipleReportBlockSenderReport;
  Report := Copy(Report, 1, Length(Report) - 5);

  S := TStringStream.Create(Report);
  try
    try
      Self.SenderReport.ReadFrom(S);
      Fail('Failed to raise exception from truncated stream');
    except
      on EStreamTooShort do;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSenderReport.TestRealLength;
const
  MinimumLength     = 7*4;
  ReportBlockLength = 6*4;
begin
  CheckEquals(MinimumLength,
              Self.SenderReport.RealLength,
              'Minimum RealLength');

  Self.SenderReport.ReceptionReportCount := 1;
  CheckEquals(MinimumLength + ReportBlockLength,
              Self.SenderReport.RealLength,
              'RealLength with one report block');

  Self.SenderReport.ReceptionReportCount := 5;
  CheckEquals(MinimumLength + 5*ReportBlockLength,
              Self.SenderReport.RealLength,
              'RealLength with five report blocks');

  Self.SenderReport.Extension := '12345';
  CheckEquals(MinimumLength + 5*ReportBlockLength + 5,
              Self.SenderReport.RealLength,
              'RealLength with five report blocks and a 5-octet extension');
end;

procedure TestTIdRTCPSenderReport.TestReportAt;
var
  I: Integer;
  S: TStringStream;
begin
  try
    Self.SenderReport.Reports[0];
    Fail('Failed to raise error accessing first element of an empty list');
  except
    on EListError do;
  end;

  try
    Self.SenderReport.Reports[-1];
    Fail('Failed to raise error on bad index');
  except
    on EListError do;
  end;

  S := TStringStream.Create(Self.MultipleReportBlockSenderReport);
  try
    Self.SenderReport.ReadFrom(S);
  finally
    S.Free;
  end;

  for I := 0 to 1 do
    CheckEquals($feed + I,
                Self.SenderReport.Reports[I].CumulativeLoss,
                'Wrong report off index ' + IntToStr(I));

  try
    Self.SenderReport.Reports[3];
    Fail('Failed to raise error accessing out-of-range index');
  except
    on EListError do;
  end;
end;

//******************************************************************************
//* TestTIdRTCPSourceDescription                                               *
//******************************************************************************
//* TestTIdRTCPSourceDescription Public methods ********************************

procedure TestTIdRTCPSourceDescription.SetUp;
begin
  inherited SetUp;

  Self.SrcDesc := Self.Packet as TIdRTCPSourceDescription;
end;

//* TestTIdRTCPSourceDescription Protected methods *****************************

function TestTIdRTCPSourceDescription.PacketType: Byte;
begin
  Result := RTCPSourceDescription;
end;

//* TestTIdRTCPSourceDescription Published methods *****************************

procedure TestTIdRTCPSourceDescription.TestAddChunkAndChunkCount;
begin
  CheckEquals(0, Self.SrcDesc.ChunkCount, 'Empty list');
  CheckNotNull(Self.SrcDesc.AddChunk, 'AddChunk');
  CheckEquals(1, Self.SrcDesc.ChunkCount, 'Chunk not added');
end;

procedure TestTIdRTCPSourceDescription.TestGetAllSrcIDs;
var
  I, J: Integer;
  IDs:  TCardinalDynArray;
begin
  Self.SrcDesc.AddChunk.SyncSrcID := $decafbad;
  Self.SrcDesc.AddChunk.SyncSrcID := $deadbeef;
  Self.SrcDesc.AddChunk.SyncSrcID := $f00dd00d;

  IDs := Self.SrcDesc.GetAllSrcIDs;
  CheckEquals(Self.SrcDesc.ChunkCount,
              Length(IDs),
              'Number of IDs');

  J := 0;
  for I := Low(IDs) to High(IDs) do begin
    CheckEquals(IntToHex(Self.SrcDesc.Chunks[J].SyncSrcID, 8),
                IntToHex(IDs[I], 8),
                'Source ID ' + IntToStr(I));
    Inc(J);
  end;
end;

procedure TestTIdRTCPSourceDescription.TestIsSourceDescription;
begin
  Check(Self.Packet.IsSourceDescription,
        Self.Packet.ClassName  + ' not marked as being a source description');
end;

procedure TestTIdRTCPSourceDescription.TestPrintOn;
var
  Chunk: TIdRTCPSrcDescChunk;
  Name:  String;
  S:     TStringStream;
begin
  Name := 'A';

  S := TStringStream.Create('');
  try
    Self.SrcDesc.HasPadding := false;

    Chunk := Self.SrcDesc.AddChunk;
    Chunk.SyncSrcID := $deadbeef;
    Chunk.AddCanonicalName(Name);
    Self.SrcDesc.Length := 12;
    Self.SrcDesc.PrintOn(S);

    Check(Length(S.DataString) > 11, 'Too little output');

    CheckEquals($81, Ord(S.DataString[1]),  'Version, padding & SC');
    CheckEquals(RTCPSourceDescription,
                     Ord(S.DataString[2]),  'Packet type');
    CheckEquals($00, Ord(S.DataString[3]),  'Length MSB');
    CheckEquals($0C, Ord(S.DataString[4]),  'Length LSB');

    CheckEquals($de, Ord(S.DataString[5]),  'Chunk 1 SSRC MSB');
    CheckEquals($ad, Ord(S.DataString[6]),  'Chunk 1 SSRC MSB - 1');
    CheckEquals($be, Ord(S.DataString[7]),  'Chunk 1 SSRC LSB + 1');
    CheckEquals($ef, Ord(S.DataString[8]),  'Chunk 1 SSRC LSB');
    CheckEquals(SDESCName,
                     Ord(S.DataString[9]),  'CNAME');
    CheckEquals($01, Ord(S.DataString[10]), 'CNAME length');
    CheckEquals('A',     S.DataString[11],  'CNAME name');
    CheckEquals($00, Ord(S.DataString[12]), 'One byte of zero padding');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSourceDescription.TestPrintOnLength;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SrcDesc.Length := $f00d;
    Self.SrcDesc.PrintOn(S);

    Check(Length(S.DataString) > 3, 'Too little output');
    CheckEquals($f0, Ord(S.DataString[3]), 'MSB');
    CheckEquals($0d, Ord(S.DataString[4]), 'LSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSourceDescription.TestReadFromWithSingleCname;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$c1 + Chr(Self.PacketType) + #$00#$04
                          + #$de#$ca#$fb#$ad
                          + #$01#$08'Az'
                          + 'atho'
                          + 'th'#$00#$00);
  try
    Self.SrcDesc.ReadFrom(S);

    CheckEquals(1,
                Self.SrcDesc.ChunkCount,
                'Chunk count');
    CheckEquals(1,
                Self.SrcDesc.Chunks[0].ItemCount,
                'Chunk item count');
    CheckEquals(IntToHex($decafbad, 8),
                IntToHex(Self.SrcDesc.Chunks[0].SyncSrcID, 8),
                'Chunk item SSRC');
    CheckEquals(1,
                Self.SrcDesc.Chunks[0].ItemCount,
                'Chunk item count');
     CheckEquals(SDESCName,
                 Self.SrcDesc.Chunks[0].Items[0].ID,
                 'Chunk item id');
    CheckEquals('Azathoth',
                (Self.SrcDesc.Chunks[0].Items[0] as TIdSDESCanonicalName).Data,
                'CNAME');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdRTCPBye                                                             *
//******************************************************************************
//* TestTIdRTCPBye Public methods **********************************************

procedure TestTIdRTCPBye.SetUp;
begin
  inherited SetUp;

  Self.Bye := Self.Packet as TIdRTCPBye;
  Self.Timer := TIdDebugTimerQueue.Create(false);
end;

procedure TestTIdRTCPBye.TearDown;
begin
  Self.Timer.Terminate;

  inherited TearDown;
end;

//* TestTIdRTCPBye Protected methods *******************************************

function TestTIdRTCPBye.PacketType: Byte;
begin
  Result := RTCPGoodBye;
end;

//* TestTIdRTCPBye Published methods *******************************************

procedure TestTIdRTCPBye.TestGetAllSrcIDs;
var
  I, J: Integer;
  IDs:  TCardinalDynArray;
begin
  Self.Bye.SourceCount := 3;
  Self.Bye.Sources[0] := $decafbad;
  Self.Bye.Sources[1] := $deadbeef;
  Self.Bye.Sources[2] := $f00dd00d;

  IDs := Self.Bye.GetAllSrcIDs;
  CheckEquals(Self.Bye.SourceCount,
              Length(IDs),
              'Number of IDs');

  J := 0;
  for I := Low(IDs) to High(IDs) do begin
    CheckEquals(IntToHex(Self.Bye.Sources[J], 8),
                IntToHex(IDs[I], 8),
                'Source ID ' + IntToStr(I));
    Inc(J);
  end;
end;

procedure TestTIdRTCPBye.TestIsBye;
begin
  Check(Self.Packet.IsBye,
        Self.Packet.ClassName  + ' not marked as being a bye');
end;

procedure TestTIdRTCPBye.TestPrepareForTransmission;
var
  MockAgent: TIdMockRTPPeer;
  Profile:   TIdRTPProfile;
  Session:   TIdRTPSession;
begin
  MockAgent := TIdMockRTPPeer.Create;
  try
    Profile := TIdAudioVisualProfile.Create;
    try
      Session := MockAgent.Session;

      Session.LocalProfile := Profile;
      Session.Timer   := Self.Timer;
      Self.Bye.PrepareForTransmission(Session);
      Check(Self.Bye.SourceCount > 0,
            'BYE must have source counts');
      CheckEquals(IntToHex(Session.SyncSrcID, 8),
                  IntToHex(Self.Bye.Sources[0], 8),
                  'We''re not BYEing ourselves');
    finally
      Profile.Free;
    end;
  finally
    MockAgent.Free;
  end;
end;

procedure TestTIdRTCPBye.TestPrintOnLength;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Bye.Length := $f00d;
    Self.Bye.PrintOn(S);

    Check(Length(S.DataString) > 7, 'Too little output');
    CheckEquals($f0, Ord(S.DataString[3]), 'MSB');
    CheckEquals($0d, Ord(S.DataString[4]), 'LSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPBye.TestPrintOnLengthAutomaticallyCalculated;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Bye.Reason := 'haha';
    Self.Bye.PrintOn(S);

    Check(Length(S.DataString) > 2, 'Too little output');
    Check(Ord(S.DataString[3]) = 0,
          'MSB of length');

    CheckEquals(3,
                Ord(S.DataString[4]),
                'LSB of length');
  finally
    S.Free;
  end;
end;


procedure TestTIdRTCPBye.TestPrintOnMultipleSources;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Bye.Sources[0] := $decafbad;
    Self.Bye.Sources[1] := $beeffeed;
    Self.Bye.PrintOn(S);

    Check(Length(S.DataString) > 11, 'Too little output');
    CheckEquals(2,
                Ord(S.DataString[1]) and $1f,
                'Source count');

    CheckEquals($de, Ord(S.DataString[5]), '0: MSB');
    CheckEquals($ca, Ord(S.DataString[6]), '0: MSB - 1');
    CheckEquals($fb, Ord(S.DataString[7]), '0: LSB + 1');
    CheckEquals($ad, Ord(S.DataString[8]), '0: LSB');

    CheckEquals($be, Ord(S.DataString[ 9]), '1: MSB');
    CheckEquals($ef, Ord(S.DataString[10]), '1: MSB - 1');
    CheckEquals($fe, Ord(S.DataString[11]), '1: LSB + 1');
    CheckEquals($ed, Ord(S.DataString[12]), '1: LSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPBye.TestPrintOnReason;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Bye.Reason := 'No reason';
    Self.Bye.PrintOn(S);

    // We typecast to shut the compiler up - anyone seen a String with
    // length < 0 lately?
    Check(Cardinal(Length(S.DataString)) > (7 + SizeOf(Byte)
                                + RoundUpToMultipleOfFour(Self.Bye.ReasonLength)),
          'Too little output');

    CheckEquals(Self.Bye.ReasonLength,
                Ord(S.DataString[9]),
                'Reason length');
    CheckEquals(Self.Bye.Reason,
                Copy(S.DataString,
                     10,
                     Self.Bye.ReasonLength),
                'Reason');
    CheckEquals(0,
                Ord(S.DataString[Length(S.DataString) - 1]),
                'Reason padding, first octet');
    CheckEquals(0,
                Ord(S.DataString[Length(S.DataString)]),
                'Reason padding, second octet');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPBye.TestPrintOnSyncSrcId;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Bye.Sources[0] := $decafbad;
    Self.Bye.PrintOn(S);

    Check(Length(S.DataString) > 7, 'Too little output');
    CheckEquals($de, Ord(S.DataString[5]), 'LSB');
    CheckEquals($ca, Ord(S.DataString[6]), 'LSB + 1');
    CheckEquals($fb, Ord(S.DataString[7]), 'MSB - 1');
    CheckEquals($ad, Ord(S.DataString[8]), 'MSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPBye.TestReadFrom;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$80 + Chr(RTCPGoodbye) + #$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    Self.Bye.ReadFrom(S);

    CheckEquals(2, Self.Bye.Version, 'Version');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPBye.TestReadFromLongReason;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$80 + Chr(RTCPGoodbye) + #$00#$00
                          + #$00#$00#$00#$00
                          + #$00#$00#$00#$00);
  try
    Self.Bye.ReadFrom(S);

    CheckEquals(2, Self.Bye.Version, 'Version');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPBye.TestReadFromReason;
var
  PacketWordLength: Word;
  Reason:           String;
  ReasonLength:     Word;
  S:                TStringStream;
  TrailingGarbage:  String;
begin
  // 1 word for header
  // 1 word for single SSRC
  // 6 words for 22 + 1 octets (ReasonLength + Reason + zero padding)
  // -1 for size indication
  PacketWordLength := 7;

  // This needs 1 byte of zero padding
  Reason := 'Burn an X in your head';
  ReasonLength := Length(Reason);
  TrailingGarbage := 'xxxx';

  S  := TStringStream.Create(#$01 + Chr(RTCPGoodbye) + EncodeAsString(PacketWordLength)
                           + #$CA#$FE#$BA#$BE
                           + Chr(ReasonLength)
                           + Reason + #$00 // zero pad to 32-bit boundary
                           + TrailingGarbage);
  try
    Self.Bye.ReadFrom(S);

    CheckEquals(ReasonLength,
                Self.Bye.ReasonLength,
                'ReasonLength');
    CheckEquals(Reason,
                Self.Bye.Reason,
                'Reason');

    CheckEquals(TrailingGarbage,
                ReadRemainderOfStream(S),
                'Bye consumed the trailing garbage');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPBye.TestReadFromReasonLength;
var
  S: TStringStream;
begin
  S  := TStringStream.Create(#$01 + Chr(RTCPGoodbye) + #$00#$00
                           + #$CA#$FE#$BA#$BE
                           + #$F0#$0D);
  try
    Self.Bye.ReadFrom(S);

    // Zero, because the Reason's empty. This is a malformed packet anyway.
    CheckEquals(0,
                Self.Bye.ReasonLength,
                'ReasonLength');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPBye.TestReadFromSourceCount;
var
  I: TIdRTCPSourceCount;
  J: Integer;
  Packet: String;
  S: TStringStream;
begin
  for I := Low(TIdRTCPSourceCount) to High(TIdRTCPSourceCount) do begin
    Packet := Chr(I) + Chr(RTCPGoodbye) + #$00 + Chr(I);

    for J := Low(TIdRTCPSourceCount) to I - 1 do
      Packet := Packet + #$00#$00#$00#$00;

    S  := TStringStream.Create(Packet);
    try
      Self.Bye.ReadFrom(S);

      CheckEquals(I, Self.Bye.SourceCount, 'SourceCount ' + IntToStr(I));
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTCPBye.TestReadFromSources;
var
  I:      TIdRTCPSourceCount;
  IDs:    TIdCardinalArray;
  J:      Integer;
  Packet: String;
  S:      TStream;
begin
  for I := Low(TIdRTCPSourceCount) to High(TIdRTCPSourceCount) do begin
    SetLength(IDs, I);
    Packet := Chr($c0 or I) + Chr(RTCPGoodBye) + #$00 + Chr(I);

    for J := 0 to I - 1 do begin
      IDs[J] := Random(High(Integer));
      Packet := Packet + EncodeAsString(IDs[J]);
    end;

    S := TStringStream.Create(Packet);
    try
      Self.Bye.ReadFrom(S);
      CheckEquals(I, Self.Bye.SourceCount, 'SourceCount');

      for J := 0 to Self.Bye.SourceCount - 1 do
        CheckEquals(IDs[J],
                    Self.Bye.Sources[J],
                    'SSRC #' + IntToStr(J));
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTCPBye.TestRealLength;
var
  I: Integer;
begin
  for I := 0 to 4 do begin
    Self.Bye.SourceCount := I;
    CheckEquals(4 + I*4,
                Self.Bye.RealLength,
                'Length with ' + IntToStr(I) + ' SSRCs');
  end;

  Self.Bye.SourceCount := 1;
  Self.Bye.Reason := '12345';
  CheckEquals(4 + 4 + (Sizeof(Byte) + Length(Self.Bye.Reason)),
              Self.Bye.RealLength,
              'RealLength with reason');
end;

//******************************************************************************
//* TestTIdRTCPApplicationDefined                                              *
//******************************************************************************
//* TestTIdRTCPApplicationDefined Public methods *******************************

procedure TestTIdRTCPApplicationDefined.SetUp;
begin
  inherited SetUp;

  Self.AppDef := Self.Packet as TIdRTCPApplicationDefined;
end;

//* TestTIdRTCPApplicationDefined Protected methods ****************************

function TestTIdRTCPApplicationDefined.PacketType: Byte;
begin
  Result := RTCPApplicationDefined;
end;

//* TestTIdRTCPApplicationDefined Published methods ****************************

procedure TestTIdRTCPApplicationDefined.TestPrintOnData;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.AppDef.Data := 'abcd';
    Self.AppDef.PrintOn(S);

    Check(Length(S.DataString) > 15, 'Too little output');
    CheckEquals('a', S.DataString[13], 'Data 1');
    CheckEquals('b', S.DataString[14], 'Data 2');
    CheckEquals('c', S.DataString[15], 'Data 3');
    CheckEquals('d', S.DataString[16], 'Data 4');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPApplicationDefined.TestPrintOnEmptyData;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.AppDef.PrintOn(S);

    CheckEquals(12, Length(S.DataString), 'Too little output');
    CheckEquals(0, Ord(S.DataString[3]), 'Length MSB');
    CheckEquals(2, Ord(S.DataString[4]), 'Length LSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPApplicationDefined.TestPrintOnLength;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.AppDef.Length := $f00d;
    Self.AppDef.PrintOn(S);

    Check(Length(S.DataString) > 11, 'Too little output');
    CheckEquals($f0, Ord(S.DataString[3]), 'MSB');
    CheckEquals($0d, Ord(S.DataString[4]), 'LSB');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPApplicationDefined.TestPrintOnName;
var
  S: TStringStream;
begin
  S  := TStringStream.Create('');
  try
    Self.AppDef.Name := 'grue';
    Self.AppDef.PrintOn(S);

    Check(Length(S.DataString) > 11, 'Too little output');
    CheckEquals(Self.AppDef.Name,
                Copy(S.DataString, 9, 4),
                'Name');
  finally
    S.Free;
  end;

  S  := TStringStream.Create('');
  try
    Self.AppDef.Name := 'x';
    Self.AppDef.PrintOn(S);

    Check(Length(S.DataString) > 11, 'Too little output');
    CheckEquals(Self.AppDef.Name + #$00#$00#$00,
                Copy(S.DataString, 9, 4),
                'Name');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPApplicationDefined.TestReadFromData;
var
  S: TStringStream;
begin
  S  := TStringStream.Create(#$00 + Chr(RTCPApplicationDefined) + #$00#$09
                           + #$de#$ca#$fb#$ad
                           + 'dead'
                           + 'toom'
                           + 'anyc'
                           + 'ooks'
                           + 'spoi'
                           + 'lthe'
                           + 'brot'
                           + 'h'#0#0#0);
  try
    Self.AppDef.ReadFrom(S);

    CheckEquals('toomanycooksspoilthebroth'#0#0#0, Self.AppDef.Data, 'Data');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPApplicationDefined.TestReadFromName;
var
  S: TStringStream;
begin
  S  := TStringStream.Create(#$00 + Chr(RTCPApplicationDefined) + #$00#$02
                           + #$de#$ca#$fb#$ad
                           + 'dead');
  try
    Self.Packet.ReadFrom(S);

    CheckEquals('dead',
                Self.AppDef.Name,
                'Name');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPApplicationDefined.TestReadFromSyncSrcId;
var
  S: TStringStream;
begin
  S  := TStringStream.Create(#$00 + Chr(RTCPApplicationDefined) + #$00#$02
                           + #$de#$ca#$fb#$ad
                           + #$00#$00#$00#$00);
  try
    Self.Packet.ReadFrom(S);

    CheckEquals(IntToHex($decafbad, 4),
                IntToHex(Self.AppDef.SyncSrcID, 4),
                'SyncSrcId');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPApplicationDefined.TestRealLength;
begin
  // Name is ALWAYS present, and always has a multiple of 4 octets
  CheckEquals(8 + 4, Self.AppDef.RealLength, 'Minimum');

  Self.AppDef.Name := 'foo';
  Self.AppDef.Data := '';
  CheckEquals(8 + 4,
              Self.AppDef.RealLength,
              'RealLength with Name and no Data');

  Self.AppDef.Name := '';
  Self.AppDef.Data := 'bar';
  CheckEquals(8 + 4 + Length(Self.AppDef.Data),
              Self.AppDef.RealLength,
              'RealLength with Data and no Name; only of course we always have '
            + 'a Name');

  Self.AppDef.Name := 'foobaring';
  Self.AppDef.Data := 'bar';
  CheckEquals(8 + 4 + Length(Self.AppDef.Data),
              Self.AppDef.RealLength,
              'RealLength with Name and Data');
end;

procedure TestTIdRTCPApplicationDefined.TestSetData;
begin
  Self.AppDef.Data := '';
  CheckEquals('', Self.AppDef.Data, 'No data');

  Self.AppDef.Data := '1';
  CheckEquals('1'#$00#$00#$00, Self.AppDef.Data, '1 byte');

  Self.AppDef.Data := '1234';
  CheckEquals('1234', Self.AppDef.Data, '4 bytes');

  Self.AppDef.Data := '123456';
  CheckEquals('123456'#$00#$00, Self.AppDef.Data, '6 bytes');
end;

procedure TestTIdRTCPApplicationDefined.TestSetName;
begin
  Self.AppDef.Name := '12345678';
  CheckEquals('1234', Self.AppDef.Name, 'Long name');

  Self.AppDef.Name := '12';
  CheckEquals('12', Self.AppDef.Name, 'Short name');

  Self.AppDef.Name := '1234';
  CheckEquals('1234', Self.AppDef.Name, 'Just Right name');

  Self.AppDef.Name := '';
  CheckEquals('', Self.AppDef.Name, 'Null name');
end;

//******************************************************************************
//* TestTIdCompoundRTCPPacket                                                  *
//******************************************************************************
//* TestTIdCompoundRTCPPacket Public methods ***********************************

procedure TestTIdCompoundRTCPPacket.SetUp;
begin
  inherited SetUp;

  Self.Packet := TIdCompoundRTCPPacket.Create;
end;

procedure TestTIdCompoundRTCPPacket.TearDown;
begin
  Self.Packet.Free;

  inherited TearDown;
end;

//* TestTIdCompoundRTCPPacket Published methods ********************************

procedure TestTIdCompoundRTCPPacket.TestAdd;
begin
  CheckEquals(0, Self.Packet.PacketCount, 'Empty packet');
  Self.Packet.AddApplicationDefined;
  CheckEquals(1, Self.Packet.PacketCount, 'APPDEF');
  Self.Packet.AddBye;
  CheckEquals(2, Self.Packet.PacketCount, 'APPDEF + BYE');
  Self.Packet.AddReceiverReport;
  CheckEquals(3, Self.Packet.PacketCount, 'APPDEF + BYE + RR');
  Self.Packet.AddSenderReport;
  CheckEquals(4, Self.Packet.PacketCount, 'APPDEF + BYE + RR + SR');
  Self.Packet.AddSourceDescription;
  CheckEquals(5, Self.Packet.PacketCount, 'APPDEF + BYE + RR + SR + SDES');
end;

procedure TestTIdCompoundRTCPPacket.TestCopy;
var
  Copy: TIdCompoundRTCPPacket;
  I:     Integer;
begin
  Self.Packet.AddReceiverReport;
  Self.Packet.AddReceiverReport;
  Self.Packet.AddApplicationDefined;

  Copy := Self.Packet.Copy as TIdCompoundRTCPPacket;
  try
    CheckEquals(Self.Packet.ClassType,
                Copy.ClassType,
                'Unexpected type for a Copy');

    CheckEquals(Self.Packet.PacketCount,
                Copy.PacketCount,
                'Packet count');

    for I := 0 to Self.Packet.PacketCount - 1 do
      CheckEquals(Self.Packet.PacketAt(I).ClassType,
                  Copy.PacketAt(I).ClassType,
                  'Report #' + IntToStr(I + 1));
  finally
    Copy.Free;
  end;
end;

procedure TestTIdCompoundRTCPPacket.TestFirstPacket;
var
  RR: TIdRTCPReceiverReport;
begin
  Check(Self.Packet.FirstPacket = nil,
        'FirstPacket didn''t return nil on an empty compound packet');
  RR := Self.Packet.AddReceiverReport;
  Check(RR = Self.Packet.FirstPacket, 'FirstPacket returned unknown packet');
end;

procedure TestTIdCompoundRTCPPacket.TestHasBye;
begin
  Check(not Self.Packet.HasBye, 'Empty packet');
  Self.Packet.AddSenderReport;
  Check(not Self.Packet.HasBye, 'One SR');
  Self.Packet.AddBye;
  Check(Self.Packet.HasBye, 'One SR, one BYE');
end;

procedure TestTIdCompoundRTCPPacket.TestHasReceiverReport;
begin
  Check(not Self.Packet.HasReceiverReport, 'Empty packet');
  Self.Packet.AddBye;
  Check(not Self.Packet.HasReceiverReport, 'One BYE');
  Self.Packet.AddReceiverReport;
  Check(Self.Packet.HasReceiverReport, 'One BYE, one RR');
end;

procedure TestTIdCompoundRTCPPacket.TestHasSourceDescription;
begin
  Check(not Self.Packet.HasSourceDescription, 'Empty packet');
  Self.Packet.AddSenderReport;
  Check(not Self.Packet.HasSourceDescription, 'One SR');
  Self.Packet.AddSourceDescription;
  Check(Self.Packet.HasSourceDescription, 'One SR, one SDES');
end;

procedure TestTIdCompoundRTCPPacket.TestIsRTCP;
begin
  Check(Self.Packet.IsRTCP, 'IsRTCP');
end;

procedure TestTIdCompoundRTCPPacket.TestIsRTP;
begin
  Check(not Self.Packet.IsRTP, 'IsRTP');
end;

procedure TestTIdCompoundRTCPPacket.TestIsValidFirstPacketAnAppDef;
begin
  Self.Packet.AddApplicationDefined;
  Check(not Self.Packet.IsValid, 'First report an APPDEF');
end;

procedure TestTIdCompoundRTCPPacket.TestIsValidFirstPacketAnRR;
begin
  Self.Packet.AddReceiverReport;
  Check(Self.Packet.IsValid, 'First report an RR');
end;

procedure TestTIdCompoundRTCPPacket.TestIsValidFirstPacketAnSDES;
begin
  Self.Packet.AddSourceDescription;
  Check(not Self.Packet.IsValid, 'First report an SDES');
end;

procedure TestTIdCompoundRTCPPacket.TestIsValidFirstPacketAnSR;
begin
  Self.Packet.AddSenderReport;
  Check(Self.Packet.IsValid, 'First report an SR');
end;

procedure TestTIdCompoundRTCPPacket.TestIsValidPacketFirstPacketPadded;
var
  PaddedReport: String;
  S:            TStringStream;
begin
  PaddedReport := SampleReceiverReport;
  PaddedReport[1] := Chr(Ord(PaddedReport[1]) or $20);
  S := TStringStream.Create(PaddedReport);
  try
    Self.Packet.ReadFrom(S);
    Check(Self.Packet.FirstPacket.HasPadding,
          'Padding bit not set');
    Check(not Self.Packet.IsValid,
          'First packet shouldn''t have padding');
  finally
    S.Free;
  end;
end;

procedure TestTIdCompoundRTCPPacket.TestPrintOn;
var
  Expected: TStringStream;
  Received: TStringStream;
  SDES:     TIdRTCPSourceDescription;
  SR:       TIdRTCPSenderReport;
begin
  Expected := TStringStream.Create('');
  try
    Received := TStringStream.Create('');
    try
      SR := TIdRTCPSenderReport.Create;
      try
        SDES := TIdRTCPSourceDescription.Create;
        try
          SR.PrintOn(Expected);
          SDES.PrintOn(Expected);
        finally
          SDES.Free;
        end;
      finally
        SR.Free;
      end;

      Self.Packet.AddSenderReport;
      Self.Packet.AddSourceDescription;

      Self.Packet.PrintOn(Received);

      CheckEquals(Expected.DataString, Received.DataString, 'SR+SDES PrintOn');
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdCompoundRTCPPacket.TestReadFrom;
var
  SDES: TIdRTCPSourceDescription;
  Src:  TStringStream;
  SR:   TIdRTCPSenderReport;
begin
  Src := TStringStream.Create('');
  try
    SR := TIdRTCPSenderReport.Create;
    try
      SDES := TIdRTCPSourceDescription.Create;
      try
        SR.PrintOn(Src);
        SDES.PrintOn(Src);
      finally
        SDES.Free;
      end;
    finally
      SR.Free;
    end;
    Src.Seek(0, soFromBeginning);

    Self.Packet.ReadFrom(Src);
    CheckEquals(2, Self.Packet.PacketCount, 'PacketCount');
    CheckEquals(RTCPSenderReport,
                Self.Packet.PacketAt(0).PacketType,
                'SR packet type');
    CheckEquals(RTCPSourceDescription,
                Self.Packet.PacketAt(1).PacketType,
                'SDES packet type');
  finally
    Src.Free;
  end;
end;

procedure TestTIdCompoundRTCPPacket.TestReadFromEmptyStream;
var
  Src: TStringStream;
begin
  Src := TStringStream.Create('');
  try
    Self.Packet.ReadFrom(Src);
    CheckEquals(0, Self.Packet.PacketCount, 'Empty stream');
  finally
    Src.Free;
  end;
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

procedure TestTIdRTPMember.TestDelaySinceLastSenderReport;
var
  DLSR: Cardinal;
begin
  CheckEquals(0,
              Self.Member.DelaySinceLastSenderReport,
              'New source');

  // Don't place too much faith in the following tests.
  // DelaySinceLastSenderReport uses Now(), so we can't be too sure of the
  // _exact_ delay. Too, doubles lack precision, so there might be roundoff
  // errors. They shouldn't be more than a handful of units out though.
  Self.Member.LastSenderReportReceiptTime := Now - OneSecond;
  DLSR := Self.Member.DelaySinceLastSenderReport;
  CheckEquals(65536, DLSR, 'Delay of one second');

  Self.Member.LastSenderReportReceiptTime := Now - 100*OneSecond;
  DLSR := Self.Member.DelaySinceLastSenderReport;
  CheckEquals(6553600, DLSR, 'Delay of 100 seconds');
end;

procedure TestTIdRTPMember.TestInitSequence;
begin
  Self.Member.InitSequence(Self.Data);

  CheckEquals(IntToHex(Self.Data.SequenceNo, 8),
              IntToHex(Self.Member.BaseSeqNo, 8),
              'Base sequence no');
  CheckEquals(IntToHex(Self.Data.SequenceNo, 8),
              IntToHex(Self.Member.HighestSeqNo, 8),
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

procedure TestTIdRTPMember.TestLastSenderReport;
var
  DT:  TDateTime;
  NTP: TIdNTPTimestamp;
begin
  Self.Member.LastSenderReportReceiptTime := 0;
  CheckEquals(0,
              Self.Member.LastSenderReport,
              'Time = 0');

  DT := Now;
  NTP := DateTimeToNTPTimestamp(DT);
  Self.Member.LastSenderReportReceiptTime := DT;
  CheckEquals(IntToHex((NTP.IntegerPart and $0000ffff) shl 16, 8),
              IntToHex(Self.Member.LastSenderReport and $ffff0000, 8),
              'High 16 bits');
  CheckEquals(IntToHex((NTP.FractionalPart and $ffff0000) shr 16, 8),
              IntToHex(Self.Member.LastSenderReport and $0000ffff, 8),
              'Low 16 bits');
end;

procedure TestTIdRTPMember.TestPacketLossCount;
begin
  Self.Member.BaseSeqNo       := 0;
  Self.Member.HighestSeqNo    := 0;
  Self.Member.Cycles          := 0;
  Self.Member.ReceivedPackets := 0;

  // This may seem odd, but consider that BaseSeqNo et al can't even be set
  // without receiving a packet. PacketLossCount returns 1 on a new member
  // because we've "lost" the initial packet that starts the source off in
  // the first place.
  CheckEquals(1, Self.Member.PacketLossCount, '0 packets; no cycle');

  Self.Member.HighestSeqNo := 10;
  CheckEquals(Self.Member.HighestSeqNo + 1,
              Self.Member.PacketLossCount,
              '10 packets; no cycle');

  Self.Member.Cycles := 1;
  CheckEquals(Self.Member.SequenceNumberRange + Self.Member.HighestSeqNo + 1,
              Self.Member.PacketLossCount,
              '10 packets; one cycle');

  Self.Member.ReceivedPackets := 888;
  CheckEquals(Self.Member.SequenceNumberRange + Self.Member.HighestSeqNo + 1
            - Self.Member.ReceivedPackets,
              Self.Member.PacketLossCount,
              '10 packets; one cycle; 888 packets received');

  Self.Member.Cycles          := 128; // $800000
  Self.Member.HighestSeqNo    := 0;
  Self.Member.ReceivedPackets := 0;
  CheckEquals($7fffff,
              Self.Member.PacketLossCount,
              'Packet loss wasn''t capped on the positive side');

  Self.Member.Cycles := 0;
  Self.Member.ReceivedPackets := $ffffff;
  CheckEquals($800000,
              Self.Member.PacketLossCount,
              'Packet loss wasn''t capped on the negative side');

  Self.Member.HighestSeqNo    := $ff;
  Self.Member.ReceivedPackets := $ffff;
  CheckEquals($00feff,
              Self.Member.PacketLossCount,
              'Negative packet loss');
end;

procedure TestTIdRTPMember.TestPacketLossFraction;
begin
  Self.Member.BaseSeqNo       := 0;
  Self.Member.HighestSeqNo    := 19;
  Self.Member.ReceivedPackets := 10;
  CheckEquals($80, Self.Member.PacketLossFraction, '1/2');

  Self.Member.HighestSeqNo := 20;
  CheckEquals($86, Self.Member.PacketLossFraction, '11/21');
end;

procedure TestTIdRTPMember.TestSetControlBinding;
var
  Binding: TIdConnectionBindings;
begin
  Self.Member.SentControl := false;

  Binding := TIdConnectionBindings.Create;
  try
    Binding.LocalIP   := '127.0.0.1';
    Binding.LocalPort := 8001;
    Binding.PeerIP    := '127.0.0.2';
    Binding.PeerPort  := 9001;
    Binding.Transport := UdpTransport;

    Self.Member.SetControlBinding(Binding);
    CheckEquals(Binding.PeerPort, Self.Member.ControlPort,    'ControlPort');
    CheckEquals(Binding.PeerIP,   Self.Member.ControlAddress, 'ControlAddress');
    Check(Self.Member.SentControl, 'SentControl');
  finally
    Binding.Free;
  end;
end;

procedure TestTIdRTPMember.TestSetDataBinding;
var
  Binding: TIdConnectionBindings;
begin
  Self.Member.SentData := false;

  Binding := TIdConnectionBindings.Create;
  try
    Binding.LocalIP   := '127.0.0.1';
    Binding.LocalPort := 8001;
    Binding.PeerIP    := '127.0.0.2';
    Binding.PeerPort  := 9001;
    Binding.Transport := UdpTransport;

    Self.Member.SetDataBinding(Binding);
    CheckEquals(Binding.PeerPort, Self.Member.SourcePort,    'SourcePort');
    CheckEquals(Binding.PeerIP,   Self.Member.SourceAddress, 'SourceAddress');
    Check(Self.Member.IsSender, 'IsSender');
  finally
    Binding.Free;
  end;
end;

procedure TestTIdRTPMember.TestUpdateJitter;
  function CurrentTime(Timestamp, Transit: Cardinal): Cardinal;
  begin
    Result := Timestamp + Transit;
  end;
var
  Transit: Cardinal;
begin
  CheckEquals(0,
              Self.Member.Jitter,
              'Initial jitter');
  CheckEquals(0,
              Self.Member.PreviousPacketTransit,
              'Initial PreviousPacketTransit');

  Self.Data.Timestamp := $beefcace;
  Transit             := 100;
  Self.Member.UpdateStatistics(Self.Data, CurrentTime(Self.Data.Timestamp, Transit));
  CheckEquals(100,
              Self.Member.Jitter,
              'First packet');
  CheckEquals(100,
              Self.Member.PreviousPacketTransit,
              'First packet; PreviousPacketTransit');

  Self.Data.Timestamp := $cafebabe;
  Transit             := 80;
  Self.Member.UpdateStatistics(Self.Data, CurrentTime(Self.Data.Timestamp, Transit));
  CheckEquals(114,
              Self.Member.Jitter,
              'Second packet');
  CheckEquals(80,
              Self.Member.PreviousPacketTransit,
              'Second packet; PreviousPacketTransit');

  Self.Data.Timestamp := $deadbeef;
  Transit             := 110;
  Self.Member.UpdateStatistics(Self.Data, CurrentTime(Self.Data.Timestamp, Transit));
  CheckEquals(137,
              Self.Member.Jitter,
              'Third packet');
  CheckEquals(110,
              Self.Member.PreviousPacketTransit,
              'Third packet; PreviousPacketTransit');
end;

//******************************************************************************
//* TestTIdRTPMemberTable                                                      *
//******************************************************************************
//* TestTIdRTPMemberTable Public methods ***************************************

procedure TestTIdRTPMemberTable.SetUp;
begin
  inherited SetUp;

  Self.Binding := TIdConnectionBindings.Create;
  Self.Binding.LocalIP   := '127.0.0.1';
  Self.Binding.LocalPort := 5000;
  Self.Binding.PeerIP    := '127.0.0.1';
  Self.Binding.PeerPort  := 5002;
  Self.Binding.Transport := UdpTransport;

  Self.Host    := '127.0.0.1';
  Self.Members := TIdRTPMemberTable.Create;
  Self.Port    := 6060;
  Self.SSRC    := $decafbad;
end;

procedure TestTIdRTPMemberTable.TearDown;
begin
  Self.Members.Free;
  Self.Binding.Free;

  inherited TearDown;
end;

//* TestTIdRTPMemberTable Published methods ************************************

procedure TestTIdRTPMemberTable.TestAddAndFind;
var
  Member: TIdRTPMember;
begin
  CheckEquals(0, Self.Members.Count, 'Empty table');

  Member := Self.Members.Add(Self.SSRC);
  CheckEquals(1, Self.Members.Count, 'SSRC not added');
  Check(not Member.IsSender, 'New member marked as a sender');
  Check(Member.HasSyncSrcID, 'New member not marked as having an SSRC');

  Self.Members.Add(Self.SSRC);
  CheckEquals(1, Self.Members.Count, 'SSRC re-added');

  Check(Self.Members.Find(Self.SSRC) = Self.Members.Add(Self.SSRC),
        'Different entry returned for same SSRC');
end;

procedure TestTIdRTPMemberTable.TestAddReceiver;
var
  Member: TIdRTPMember;
begin
  CheckEquals(0, Self.Members.Count, 'Empty table');

  Member := Self.Members.AddReceiver(Host, Port);

  CheckEquals(1, Self.Members.Count, 'Receiver not added');
  Check(Member <> nil, 'Nil returned');
  Check(not Member.HasSyncSrcID,
        'Receiver can''t have an SSRC - we don''t know it yet');
  CheckEquals(Host, Member.SourceAddress, 'SourceAddress');
  CheckEquals(Port, Member.SourcePort,    'SourcePort');

  // A bug didn't initialise these two values, with the result that the first
  // time we checked for timed out SSRCs (pretty soon after the session is
  // first established - about 500ms or so - we removed the remote parties.
  // Now we set the time to Now. We don't _check_ that because checking against
  // Now introduces transient failures during testing. (Consider if we had a
  // delta of OneSecond, and you took longer than that to step through the code.
  // While the code might be correct, you'd see a failure that you wouldn't see
  // if you just let the test run.)
  CheckNotEquals(0, Member.LastRTCPReceiptTime, OneSecond, 'LastRTCPReceiptTime not initialised');
  CheckNotEquals(0, Member.LastRTPReceiptTime,  OneSecond, 'LastRTPReceiptTime not initialised');

  Self.Members.AddReceiver(Host, Port);
  CheckEquals(1, Self.Members.Count, 'Receiver erroneously re-added');
end;

procedure TestTIdRTPMemberTable.TestContains;
begin
  Check(not Self.Members.Contains(Self.SSRC), 'Empty table');
  Self.Members.Add(Self.SSRC);
  Check(Self.Members.Contains(Self.SSRC), 'SSRC not added?');
end;

procedure TestTIdRTPMemberTable.TestContainsReceiver;
begin
  Check(not Self.Members.ContainsReceiver(Self.Host, Self.Port),
        'Empty table');
  Self.Members.AddReceiver(Self.Host, Self.Port);
  Check(Self.Members.ContainsReceiver(Self.Host, Self.Port),
        'Receiver not added?');
end;

procedure TestTIdRTPMemberTable.TestFind;
var
  Member: TIdRTPMember;
begin
  Check(nil = Self.Members.Find(Self.SSRC),
        'Found non-existent member');

  Self.Members.AddReceiver(Self.Host, Self.Port);
  Check(nil = Self.Members.Find(Self.SSRC),
        'Found wrong member');
        
  Member := Self.Members.Add(Self.SSRC);
  Check(Member = Self.Members.Find(Self.SSRC),
        'Unexpected member found');
end;

procedure TestTIdRTPMemberTable.TestFindReceiver;
var
  Member: TIdRTPMember;
begin
  Check(nil = Self.Members.FindReceiver(Self.Host, Self.Port),
        'Found non-existent receiver');
  Member := Self.Members.AddReceiver(Self.Host, Self.Port);
  Check(Member = Self.Members.FindReceiver(Self.Host, Self.Port),
        'Unexpected receiver found');
end;

procedure TestTIdRTPMemberTable.TestRemove;
var
  Member: TIdRTPMember;
begin
  Self.Members.Add(Self.SSRC);
  Check(Self.Members.Contains(Self.SSRC), 'SSRC not added');
  Self.Members.Remove(Self.SSRC);
  Check(not Self.Members.Contains(Self.SSRC), 'SSRC not removed');

  Member := Self.Members.Add(Self.SSRC);
  Check(Self.Members.Contains(Self.SSRC), 'Member not added');
  Self.Members.Remove(Member);
  Check(not Self.Members.Contains(Self.SSRC), 'Member not removed');
end;

procedure TestTIdRTPMemberTable.TestRemoveNonMember;
var
  MemberCount: Cardinal;
begin
  Self.Members.Add(Self.SSRC);
  MemberCount := Self.Members.Count;
  Self.Members.Remove($deadbeef);
  CheckEquals(MemberCount, Self.Members.Count, 'Some member was removed');
end;

procedure TestTIdRTPMemberTable.TestRemoveAll;
begin
  Self.Members.Add(Self.SSRC);
  Self.Members.Add($deadbeef);
  Self.Members.RemoveAll;
  CheckEquals(0, Self.Members.Count, 'RemoveAll didn''t');
end;

procedure TestTIdRTPMemberTable.TestReceiverCount;
var
  NewSSRC: Cardinal;
begin
  NewSSRC := $deadbeef;
  CheckEquals(0, Self.Members.ReceiverCount, 'Empty list');

  Self.Members.Add(Self.SSRC);
  CheckEquals(1, Self.Members.ReceiverCount, 'One member');

  Self.Members.Add(NewSSRC);
  CheckEquals(2, Self.Members.ReceiverCount, 'Two members');

  Self.Members.Find(NewSSRC).IsSender := true;
  CheckEquals(1, Self.Members.ReceiverCount, 'Two members; one a sender');

  Self.Members.Find(Self.SSRC).IsSender := true;
  CheckEquals(0, Self.Members.ReceiverCount, 'Two members; both senders');
end;

procedure TestTIdRTPMemberTable.TestRemoveTimedOutMembers;
var
  FirstSSRC:   Cardinal;
  SecondSSRC:  Cardinal;
  SessionSSRC: Cardinal;
  Timestamp:   TDateTime;
begin
  Timestamp   := Now;
  SessionSSRC := $deadbeef;
  FirstSSRC   := 1;
  SecondSSRC  := 2;

  Self.Members.Add(SessionSSRC).LastRTCPReceiptTime := Timestamp - 1;
  Self.Members.Add(FirstSSRC).LastRTCPReceiptTime   := Timestamp - 1;
  Self.Members.Add(SecondSSRC).LastRTCPReceiptTime  := Timestamp;
  
  Self.Members.AddReceiver('CthulhuPort', 666).LastRTCPReceiptTime := Timestamp;

  Self.Members.RemoveTimedOutMembersExceptFor(Timestamp, SessionSSRC);
  CheckEquals(3,
              Self.Members.Count,
              'Timed-out members not removed');

  CheckEquals(SecondSSRC,
              Self.Members.MemberAt(1).SyncSrcID,
              'Protected member removed');
end;

procedure TestTIdRTPMemberTable.TestRemoveTimedOutSendersExceptFor;
var
  FirstSSRC:  Cardinal;
  SecondSSRC: Cardinal;
  Timestamp:  TDateTime;
begin
  FirstSSRC  := 1;
  SecondSSRC := 2;
  Timestamp  := Now;

  Self.Members.AddSender(FirstSSRC).LastRTCPReceiptTime  := Timestamp - 1;
  Self.Members.AddSender(SecondSSRC).LastRTCPReceiptTime := Timestamp;
  Self.Members.RemoveTimedOutSendersExceptFor(Timestamp,
                                              0);
  CheckEquals(1,
              Self.Members.SenderCount,
              'Timed-out sender not removed');

  CheckEquals(SecondSSRC,
              Self.Members.MemberAt(0).SyncSrcID,
              'Wrong member removed');

  Self.Members.RemoveTimedOutSendersExceptFor(Timestamp,
                                              SecondSSRC);
  CheckEquals(1,
              Self.Members.SenderCount,
              'Protected sender removed');
end;

procedure TestTIdRTPMemberTable.TestSenderCount;
var
  NewSSRC: Cardinal;
begin
  NewSSRC := $deadbeef;
  CheckEquals(0, Self.Members.SenderCount, 'Empty list');

  Self.Members.Add(Self.SSRC);
  CheckEquals(0, Self.Members.SenderCount, 'One member');

  Self.Members.Add(NewSSRC);
  CheckEquals(0, Self.Members.SenderCount, 'Two members');

  Self.Members.Find(NewSSRC).IsSender := true;
  CheckEquals(1, Self.Members.SenderCount, 'Two members; one a sender');

  Self.Members.Find(Self.SSRC).IsSender := true;
  CheckEquals(2, Self.Members.SenderCount, 'Two members; both senders');
end;

procedure TestTIdRTPMemberTable.TestSetControlBinding;
var
  Member: TIdRTPMember;
begin
  Member := Self.Members.Add(Self.SSRC);
  Check(not Member.SentControl, 'A new member can''t have sent control');

  Self.Members.SetControlBinding(Self.SSRC, Self.Binding);
  Check(Member.SentControl, 'Control binding not set');

  // We can't check the binding itself becausewe can't change the Peer(IP|Port)
  // of the TIdConnectionBindings
end;

procedure TestTIdRTPMemberTable.TestSetDataBinding;
var
  Member: TIdRTPMember;
begin
  Member := Self.Members.Add(Self.SSRC);
  Check(not Member.SentData, 'A new member can''t have sent Data');

  Self.Members.SetDataBinding(Self.SSRC, Self.Binding);
  Check(Member.SentData, 'Data binding not set');

  // We can't check the binding itself becausewe can't change the Peer(IP|Port)
  // of the TIdConnectionBindings
end;

//******************************************************************************
//* TestTIdRTPSenderTable                                                      *
//******************************************************************************
//* TestTIdRTPSenderTable Public methods ***************************************

procedure TestTIdRTPSenderTable.SetUp;
begin
  inherited SetUp;

  Self.Members := TIdRTPMemberTable.Create;
  Self.Senders := TIdRTPSenderTable.Create(Self.Members);
  Self.SSRC    := $decafbad;
end;

procedure TestTIdRTPSenderTable.TearDown;
begin
  Self.Senders.Free;
  Self.Members.Free;

  inherited TearDown;
end;

//* TestTIdRTPSenderTable Published methods ************************************

procedure TestTIdRTPSenderTable.TestAddAndFind;
var
  Member: TIdRTPMember;
begin
  CheckEquals(0, Self.Senders.Count, 'Empty table');

  Member := Self.Senders.Add(SSRC);
  CheckEquals(1, Self.Senders.Count, 'SSRC not added');
  Check(Member.IsSender, 'New member not marked as a sender');

  Self.Senders.Add(SSRC);
  CheckEquals(1, Self.Senders.Count, 'SSRC re-added');

  Check(Self.Senders.Find(SSRC) = Self.Senders.Add(SSRC),
        'Different entry returned for same SSRC');
  Check(Self.Senders.Find(SSRC) = Self.Members.Find(SSRC),
        'Senders table doesn''t simply reference Members table');
end;

procedure TestTIdRTPSenderTable.TestAddSetsIsSender;
var
  Member: TIdRTPMember;
begin
  Member := Self.Members.Add(SSRC);
  Self.Senders.Add(Self.SSRC);
  CheckEquals(1, Self.Senders.Count, 'SSRC added to senders');
  CheckEquals(1, Self.Members.Count, 'SSRC re-added to underlying table');
  Check(Member.IsSender, 'Member not set as IsSender');
end;

procedure TestTIdRTPSenderTable.TestContains;
var
  NonSenderSSRC: Cardinal;
begin
  NonSenderSSRC := $cafebabe;
  Self.Members.Add(NonSenderSSRC);

  Check(not Self.Senders.Contains(Self.SSRC), 'Empty table');
  Self.Senders.Add(Self.SSRC);
  Check(Self.Senders.Contains(Self.SSRC), 'SSRC not added?');

  Check(not Self.Senders.Contains(NonSenderSSRC), 'Table contains a non-sender');
end;

procedure TestTIdRTPSenderTable.TestMemberAt;
var
  NewSSRC: Cardinal;
begin
  NewSSRC := $deadbeef;
  Self.Senders.Add(Self.SSRC);
  Self.Senders.Add(NewSSRC);

  Check(Self.Senders.Count > 1, 'Both members weren''t added');
  CheckEquals(IntToHex(Self.SSRC, 8),
              IntToHex(Self.Senders.SenderAt(0).SyncSrcID, 8),
              'Index 0');
  CheckEquals(IntToHex(NewSSRC, 8),
              IntToHex(Self.Senders.SenderAt(1).SyncSrcID, 8),
              'Index 1');
end;

procedure TestTIdRTPSenderTable.TestMemberAtAnotherTest;
var
  NewSSRC: Cardinal;
begin
  NewSSRC := $deadbeef;
  Self.Members.Add(Self.SSRC);
  Self.Senders.Add(NewSSRC);

  CheckEquals(1, Self.Senders.Count, 'Non-sender was added');
  CheckEquals(IntToHex(NewSSRC, 8),
              IntToHex(Self.Senders.SenderAt(0).SyncSrcID, 8),
              'Index 0');
end;

procedure TestTIdRTPSenderTable.TestRemove;
var
  NewSSRC: Cardinal;
begin
  NewSSRC := $deadbeef;
  Self.Senders.Add(Self.SSRC);
  Self.Senders.Add(NewSSRC);

  Self.Senders.Remove(Self.SSRC);

  CheckEquals(1, Self.Senders.Count, 'Sender not removed');
  Check(Self.Senders.Contains(NewSSRC), 'Wrong sender removed');

  Self.Senders.Remove(Self.Senders.Add(Self.SSRC));
  CheckEquals(1, Self.Senders.Count, 'Sender not removed (2)');
end;

procedure TestTIdRTPSenderTable.TestRemoveAll;
begin
  Self.Senders.Add(Self.SSRC);
  Self.Members.Add($cafebabe);
  Self.Senders.Add($deadbeef);
  Self.Senders.RemoveAll;
  CheckEquals(0, Self.Senders.Count, 'RemoveAll didn''t');
  CheckEquals(1,
              Self.Members.Count,
              'Non-senders removed from underlying table');
end;

//******************************************************************************
//* TestTIdBaseRTPAbstractPeer                                                 *
//******************************************************************************
//* TestTIdBaseRTPAbstractPeer Public methods **********************************

procedure TestTIdBaseRTPAbstractPeer.SetUp;
begin
  inherited SetUp;

  Self.Peer := TIdMockRTPPeer.Create;
  Self.RTCP := TIdRTCPSenderReport.Create;
  Self.RTP  := TIdRTPPacket.Create(Self.Peer.LocalProfile);
end;

procedure TestTIdBaseRTPAbstractPeer.TearDown;
begin
  Self.RTP.Free;
  Self.RTCP.Free;
  Self.Peer.Free;

  inherited TearDown;
end;

//* TestTIdBaseRTPAbstractPeer Published methods *******************************

procedure TestTIdBaseRTPAbstractPeer.TestAddListener;
var
  L1, L2:        TIdRTPTestRTPListener;
  UnusedBinding: TIdConnectionBindings;
begin
  L1 := TIdRTPTestRTPListener.Create;
  try
    Self.Peer.AddListener(L1);

    L2 := TIdRTPTestRTPListener.Create;
    try
      Self.Peer.AddListener(L2);

      UnusedBinding := TIdConnectionBindings.Create;
      try
        Self.Peer.NotifyListenersOfRTCP(Self.RTCP, UnusedBinding);
        Check(L1.ReceivedRTCP, 'First listener didn''t receive RTCP');
        Check(L2.ReceivedRTCP, 'Second listener didn''t receive RTCP');

        Self.Peer.NotifyListenersOfRTP(Self.RTP, UnusedBinding);
        Check(L1.ReceivedRTP,  'First listener didn''t receive RTP');
        Check(L2.ReceivedRTCP, 'Second listener didn''t receive RTP');
      finally
        UnusedBinding.Free;
      end;
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdBaseRTPAbstractPeer.TestAddSendListener;
var
  L1, L2:        TIdRTPTestRTPSendListener;
  UnusedBinding: TIdConnectionBindings;
begin
  L1 := TIdRTPTestRTPSendListener.Create;
  try
    Self.Peer.AddSendListener(L1);

    L2 := TIdRTPTestRTPSendListener.Create;
    try
      Self.Peer.AddSendListener(L2);

      UnusedBinding := TIdConnectionBindings.Create;
      try
        Self.Peer.NotifyListenersOfSentRTCP(Self.RTCP, UnusedBinding);
        Check(L1.SentRTCP, 'First listener didn''t receive notification of sent RTCP');
        Check(L2.SentRTCP, 'Second listener didn''t receive notification of sent RTCP');

        Self.Peer.NotifyListenersOfSentRTP(Self.RTP, UnusedBinding);
        Check(L1.SentRTP,  'First listener didn''t receive notification of sent RTP');
        Check(L2.SentRTCP, 'Second listener didn''t receive notification of sent RTP');
      finally
        UnusedBinding.Free;
      end;
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdBaseRTPAbstractPeer.TestRemoveListener;
var
  Listener:      TIdRTPTestRTPListener;
  UnusedBinding: TIdConnectionBindings;
begin
  Listener := TIdRTPTestRTPListener.Create;
  try
    Self.Peer.AddListener(Listener);
    Self.Peer.RemoveListener(Listener);

    UnusedBinding := TIdConnectionBindings.Create;
    try
      Self.Peer.NotifyListenersOfRTCP(Self.RTCP, UnusedBinding);
      Check(not Listener.ReceivedRTCP,
            'Listener received RTCP');

      Self.Peer.NotifyListenersOfRTP(Self.RTP, UnusedBinding);
      Check(not Listener.ReceivedRTP,
            'Listener received RTP');
    finally
      UnusedBinding.Free;
    end;
  finally
    Listener.Free;
  end;
end;

procedure TestTIdBaseRTPAbstractPeer.TestRemoveSendListener;
var
  Listener:      TIdRTPTestRTPSendListener;
  UnusedBinding: TIdConnectionBindings;
begin
  Listener := TIdRTPTestRTPSendListener.Create;
  try
    Self.Peer.AddSendListener(Listener);
    Self.Peer.RemoveSendListener(Listener);

    UnusedBinding := TIdConnectionBindings.Create;
    try
      Self.Peer.NotifyListenersOfSentRTCP(Self.RTCP, UnusedBinding);
      Check(not Listener.SentRTCP,
            'Listener received sent RTCP notification');

      Self.Peer.NotifyListenersOfSentRTP(Self.RTP, UnusedBinding);
      Check(not Listener.SentRTP,
            'Listener received sent RTP notification');
    finally
      UnusedBinding.Free;
    end;
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdRTPPeerRegistry                                                     *
//******************************************************************************
//* TestTIdRTPPeerRegistry Public methods **************************************

procedure TestTIdRTPPeerRegistry.SetUp;
begin
  inherited SetUp;

  Self.Agent := TIdMockRTPPeer.Create;
end;

procedure TestTIdRTPPeerRegistry.TearDown;
begin
  Self.Agent.Free;

  inherited TearDown;
end;

//* TestTIdRTPPeerRegistry Published methods ***********************************

procedure TestTIdRTPPeerRegistry.TestFindServer;
begin
  Check(Self.Agent = TIdRTPPeerRegistry.FindServer(Self.Agent.ID),
        'FindServer: registered TcpConnection');
end;

procedure TestTIdRTPPeerRegistry.TestFindServerNoSuchRegisteredObject;
const
  NotARegisteredObject = 'Registered objects don''t have an ID like this';
begin
  Self.ExpectedException := ERegistry;
  TIdRTPPeerRegistry.FindServer(NotARegisteredObject);
end;

procedure TestTIdRTPPeerRegistry.TestFindServerNotAnRTPPeer;
var
  RandomRegisteredObject: TIdRegisteredObject;
begin
  RandomRegisteredObject := TIdRegisteredObject.Create;
  try
    Self.ExpectedException := ERegistry;
    TIdRTPPeerRegistry.FindServer(RandomRegisteredObject.ID);
  finally
    RandomRegisteredObject.Free;
  end;
end;

procedure TestTIdRTPPeerRegistry.TestServerOn;
const
  ArbitraryAddress = '1.2.3.4';
  ArbitraryPort    = 8000;
  AnotherPort      = 9000;
begin
  Self.Agent.Address := ArbitraryAddress;
  Self.Agent.RTPPort := ArbitraryPort;

  Check(Self.Agent = TIdRTPPeerRegistry.ServerOn(ArbitraryAddress, ArbitraryPort),
        'Server not found in registry');

  Check(nil = TIdRTPPeerRegistry.ServerOn(TIdIPAddressParser.IncIPAddress(ArbitraryAddress), ArbitraryPort),
        'Server found in registry (no such host');
  Check(nil = TIdRTPPeerRegistry.ServerOn(ArbitraryAddress, ArbitraryPort + 1),
        'Server found in registry (no such port');

  Self.Agent.RTPPort := AnotherPort;
  Check(nil = TIdRTPPeerRegistry.ServerOn(ArbitraryAddress, ArbitraryPort),
        'Server found in registry after moving');
  Check(Self.Agent = TIdRTPPeerRegistry.ServerOn(ArbitraryAddress, AnotherPort),
        'Server not found in registry after moving');
end;

procedure TestTIdRTPPeerRegistry.TestServerRunningOn;
const
  Address = '127.0.0.1';
  Port    = 8000;
begin
  Check(not TIdRTPPeerRegistry.ServerRunningOn(Address, Port),
        'There''s a server running where there should be no server');

  Self.Agent.Address := Address;
  Self.Agent.RTPPort := Port;

  Check(not TIdRTPPeerRegistry.ServerRunningOn(Address, Port),
        'While there''s a server on this address:port, it isn''t running');

  // Then we make start running the server.
  Self.Agent.Active := true;

  Check(TIdRTPPeerRegistry.ServerRunningOn(Address, Port),
        'There''s no running server');
end;

//******************************************************************************
//* TRTPSessionTestCase                                                        *
//******************************************************************************
//* TRTPSessionTestCase Public methods *****************************************

procedure TRTPSessionTestCase.SetUp;
begin
  inherited SetUp;

  Self.Timer   := TIdDebugTimerQueue.Create(false);
  Self.Profile := TIdAudioVisualProfile.Create;

  Self.T140PT := Self.Profile.FirstFreePayloadType;
  Self.Profile.AddEncoding('T140', 1000, '', Self.T140PT);

  Self.Agent   := TIdMockRTPPeer.Create;
  Self.Agent.LocalProfile  := Self.Profile;
  Self.Agent.RemoteProfile := Self.Profile;
  Self.Agent.Timer         := Self.Timer;

  Self.Session := Self.Agent.Session;
end;

procedure TRTPSessionTestCase.TearDown;
begin
  Self.Agent.Free;
  Self.Profile.Free;
  Self.Timer.Terminate;

  inherited TearDown;
end;

//******************************************************************************
//* TestSessionDelegationMethods                                               *
//******************************************************************************
//* TestSessionDelegationMethods Public methods ********************************

procedure TestSessionDelegationMethods.SetUp;
begin
  inherited SetUp;

  Self.SSRC := $decafbad;
end;

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

  Self.Session.AddMember(Self.SSRC);
  CheckEquals(OriginalCount + 1,
              Self.Session.MemberCount,
              'SSRC not added');

  Self.Session.AddMember(Self.SSRC);
  CheckEquals(OriginalCount + 1,
              Self.Session.MemberCount,
              'SSRC re-added');

  Check(Self.Session.Member(Self.SSRC) = Self.Session.AddMember(Self.SSRC),
        'Different entry returned for same SSRC');
end;

procedure TestSessionDelegationMethods.TestAddReceiver;
var
  Host:          String;
  OriginalCount: Cardinal;
  Port:          Cardinal;
begin
  Host := '127.0.0.1';
  Port := 5050;

  OriginalCount := Self.Session.MemberCount;
  CheckEquals(1, OriginalCount, 'Empty table, except for self');

  Self.Session.AddReceiver(Host, Port);
  CheckEquals(OriginalCount + 1,
              Self.Session.MemberCount,
              'Host/port not added');

  Self.Session.AddReceiver(Host, Port);
  CheckEquals(OriginalCount + 1,
              Self.Session.MemberCount,
              'Host/port re-added');

  Check(Self.Session.Member(Host, Port) = Self.Session.AddReceiver(Host, Port),
        'Different entry returned for same Host/port');
end;

procedure TestSessionDelegationMethods.TestAddSender;
begin
  CheckEquals(0, Self.Session.SenderCount, 'Empty table');
  Self.Session.AddSender(Self.SSRC);
  CheckEquals(1, Self.Session.SenderCount, 'SSRC not added');
  Self.Session.AddSender(Self.SSRC);
  CheckEquals(1, Self.Session.SenderCount, 'SSRC re-added');
  Check(Self.Session.Sender(Self.SSRC) = Self.Session.AddSender(Self.SSRC),
        'Different entry returned for same SSRC');
end;

procedure TestSessionDelegationMethods.TestAddSenderAddsMember;
var
  OriginalMemberCount: Cardinal;
begin
  OriginalMemberCount := Self.Session.MemberCount;

  CheckNotEquals(IntToHex(Self.Session.SyncSrcID, 8),
                 IntToHex(Self.SSRC, 8),
                 'Sanity check');

  Self.Session.AddSender(Self.SSRC);
  CheckEquals(1,
              Self.Session.SenderCount,
              'SSRC not added to Senders');
  CheckEquals(OriginalMemberCount + 1,
              Self.Session.MemberCount,
              'SSRC not added to Members');
end;

procedure TestSessionDelegationMethods.TestIsMember;
begin
  Check(not Self.Session.IsMember(Self.SSRC), 'Empty table');
  Self.Session.AddMember(Self.SSRC);
  Check(Self.Session.IsMember(Self.SSRC), 'SSRC not added?');
end;

procedure TestSessionDelegationMethods.TestIsSender;
begin
  Check(not Self.Session.IsSender(Self.SSRC),
        'Empty table; SSRC not found');
  Self.Session.AddMember(Self.SSRC);
  Check(not Self.Session.IsSender(Self.SSRC),
        'SSRC not meant to be a sender');
  Self.Session.Member(Self.SSRC).IsSender := true;
  Check(Self.Session.IsSender(Self.SSRC),
        'SSRC meant to be a sender');
end;

procedure TestSessionDelegationMethods.TestIsSenderSelf;
var
  Data: TIdRTPPayload;
begin
  Data := Self.Profile.EncodingFor(Self.T140PT).Copy;
  try
    Check(not Self.Session.IsSender, 'New session');

    // This seems silly, but remember that the session expects you to set the
    // start time of the payload - it comes from the time the data was
    // generated/stored, not the time the session sends the packet.
    Data.StartTime := Now;

    Self.Session.SendData(Data);
    Check(Self.Session.IsSender, 'Sent data');
  finally
    Data.Free;
  end;
end;

procedure TestSessionDelegationMethods.TestPrepareRTP;
var
  Data:           TIdRTPPayload;
  Encoding:       TIdRTPPayload;
  StartTimestamp: Cardinal;
begin
  // This has nothing really to do with the tests: we don't send data to
  // ourselves, and we need at least one other member in the session for the
  // tests to mean something. The values here might be anything - it doesn't
  // matter.
  Self.Session.AddReceiver('127.0.0.1', 8000);

  Encoding := TIdRTPPayload.CreatePayload('foo/8000');
  try
    Self.Profile.AddEncoding(Encoding, Self.Profile.FirstFreePayloadType);

    Data := Encoding.Copy;
    try
      Data.ClockRate := Encoding.ClockRate;
      Data.StartTime := Now + 5*OneSecond;
      Self.Session.SendData(Data);

      // Note that the session determines the initial sequence number and
      // timestamp by selecting random numbers. We can't really check for         
      // that sort've thing. The tests below should fail with a probability
      // ~2^-32, if my maths is correct. In other words, while it's perfectly
      // legal to have a zero initial sequence number and/or timestamp, it's
      // not very likely. We mainly want to ensure that the session sets the
      // timestamp and sequence number.
      CheckNotEquals(IntToHex(0,
                              Sizeof(Self.Agent.LastRTP.Timestamp)),
                     IntToHex(Self.Agent.LastRTP.Timestamp,
                              Sizeof(Self.Agent.LastRTP.Timestamp)),
                     'Timestamp');
      CheckNotEquals(IntToHex(0,
                              Sizeof(Self.Agent.LastRTP.SequenceNo)),
                     IntToHex(Self.Agent.LastRTP.SequenceNo,
                              Sizeof(Self.Agent.LastRTP.SequenceNo)),
                     'SequenceNo');

      StartTimestamp := Self.Agent.LastRTP.Timestamp;
      Data.StartTime := Data.StartTime + OneSecond;
      Self.Session.SendData(Data);

      CheckEquals(Encoding.ClockRate,
                  GetTickDiff(StartTimestamp, Self.Agent.LastRTP.Timestamp),
                  'Timestamp increment');
    finally
      Data.Free;
    end;
  finally
    Encoding.Free;
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
    SR.PrepareForTransmission(Self.Session);

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

//******************************************************************************
//* TSessionDataTestCase                                                       *
//******************************************************************************
//* TSessionDataTestCase Public methods ****************************************

procedure TSessionDataTestCase.SetUp;
begin
  inherited SetUp;

  Self.Binding := TIdConnectionBindings.Create;
  Self.Binding.LocalIP   := '127.0.0.1';
  Self.Binding.LocalPort := 4321;
  Self.Binding.PeerIP    := '1.2.3.4';
  Self.Binding.PeerPort  := 4321;
  Self.Binding.Transport := UdpTransport;

  Self.RTCPBinding := TIdConnectionBindings.Create;
  Self.RTCPBinding.LocalIP   := Self.Binding.LocalIP;
  Self.RTCPBinding.LocalPort := Self.Binding.LocalPort + 1;
  Self.RTCPBinding.PeerIP    := Self.Binding.PeerIP;
  Self.RTCPBinding.PeerPort  := Self.Binding.PeerPort + 1;

  Self.Data := TIdRTPPacket.Create(Self.Profile);
  Self.Data.SequenceNo := $f00d;
  Self.Data.SyncSrcID  := $decafbad;

  Self.RR := TIdRTCPReceiverReport.Create;
  Self.RR.SyncSrcID := Self.Data.SyncSrcID;
  Self.RR.ReceptionReportCount := 1;
end;

procedure TSessionDataTestCase.TearDown;
begin
  Self.RR.Free;
  Self.Data.Free;
  Self.RTCPBinding.Free;
  Self.Binding.Free;

  inherited TearDown;
end;

//* TSessionDataTestCase Protected methods *************************************

procedure TSessionDataTestCase.ValidateSource(Member: TIdRTPMember);
var
  I: Cardinal;
begin
  for I := 1 to Member.MinimumSequentialPackets - 1 do begin
    Self.Data.SequenceNo := Self.Data.SequenceNo + 1;
    Self.Session.ReceiveData(Self.Data, Self.Binding);
  end;
end;

//******************************************************************************
//* TestTIdRTPSession                                                          *
//******************************************************************************
//* TestTIdRTPSession Public methods *******************************************

procedure TestTIdRTPSession.SetUp;
begin
  inherited SetUp;

  // Receive an RTP packet from the remote party
  Self.Session.ReceiveData(Self.Data, Self.Binding);
  Self.Session.ReceiveControl(Self.RR, Self.Binding);

  Self.Member := Self.Session.Member(Self.Data.SyncSrcID);
  Self.ValidateSource(Self.Member);
end;

//* TestTIdRTPSession Private methods ******************************************

procedure TestTIdRTPSession.OnNewData(Data: TIdRTPPayload;
                                      Binding: TIdConnectionBindings);
begin
  Self.NewDataArrived := true;
end;

//* TestTIdRTPSession Published methods ****************************************

procedure TestTIdRTPSession.TestAddListener;
var
  L1, L2: TIdRTPTestRTPDataListener;
begin
  L1 := TIdRTPTestRTPDataListener.Create;
  try
    L2 := TIdRTPTestRTPDataListener.Create;
    try
      Self.Session.AddListener(L1);
      Self.Session.AddListener(L2);

      Self.Session.ReceiveData(Self.Data, Self.Binding);

      Check(L1.NewData, 'L1 didn''t get notified');
      Check(L2.NewData, 'L2 didn''t get notified');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdRTPSession.TestJoinSendsSenderReport;
var
  OldRTCPCount: Cardinal;
begin
  OldRTCPCount := Self.Agent.RTCPCount;
  Self.Session.JoinSession;
  Self.Timer.TriggerEarliestEvent;
  Check(OldRTCPCount < Self.Agent.RTCPCount, 'No RTCP sent; no event scheduled');
end;

procedure TestTIdRTPSession.TestReceiveRTPFromValidatedSourceNotifiesListeners;
begin
  Self.Session.AddListener(Self);
  Self.ValidateSource(Self.Member);
  Self.Session.ReceiveData(Self.Data, Self.Binding);
  Check(Self.NewDataArrived, 'RTP data from a validated source didn''t notify us');
end;

procedure TestTIdRTPSession.TestRemoveListener;
var
  L1, L2: TIdRTPTestRTPDataListener;
begin
  L1 := TIdRTPTestRTPDataListener.Create;
  try
    L2 := TIdRTPTestRTPDataListener.Create;
    try
      Self.Session.AddListener(L1);
      Self.Session.AddListener(L2);
      Self.Session.RemoveListener(L1);
      Self.Session.ReceiveData(Self.Data, Self.Binding);

      Check(not L1.NewData, 'L1 wasn''t removed as a listener');
      Check(L2.NewData, 'L2 didn''t get notified');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdRTPSession.TestSendReportSchedulesNextSend;
var
  OldRTCPCount: Cardinal;
begin
  Self.Session.JoinSession;
  Self.Timer.TriggerEarliestEvent;

  OldRTCPCount := Self.Agent.RTCPCount;
  Self.Timer.TriggerEarliestEvent;
  Check(OldRTCPCount < Self.Agent.RTCPCount, 'No RTCP sent; no event scheduled');
end;

//******************************************************************************
//* TestSessionSequenceNumberRules                                             *
//******************************************************************************
//* TestSessionSequenceNumberRules Public methods ******************************

procedure TestSessionSequenceNumberRules.SetUp;
begin
  inherited SetUp;

  Self.Session.ReceiveData(Self.Data, Self.Binding);

  Self.Member := Self.Session.Member(Self.Data.SyncSrcID);
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
  Self.ValidateSource(Self.Member);
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
              Self.Member.HighestSeqNo,
              'HighestSeqNo');
  CheckEquals(0,
              Self.Member.Cycles,
              'Cycles');
  CheckEquals(1,
              Self.Member.ReceivedPrior,
              'ReceivedPrior');
  CheckEquals(1,
              Self.Member.ExpectedPrior,
              'ExpectedPrior');
end;

procedure TestSessionSequenceNumberRules.TestMisOrderedPacketIgnored;
var
  PacketCount: Cardinal;
begin
  Self.ValidateSource(Self.Member);
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
  Self.ValidateSource(Self.Member);
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
  Self.ValidateSource(Self.Member);
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
//* TestSessionReportRules Private methods *************************************

function TestSessionReportRules.AddNewSender: TIdRTPMember;
begin
  Self.Session.ReceiveData(Self.Data, Self.Binding);
  Result := Self.Session.Member(Self.Data.SyncSrcID);
  Self.ValidateSource(Result);
end;

//* TestSessionReportRules Published methods ***********************************

procedure TestSessionReportRules.TestReportDetailsProperData;
var
  Member: TIdRTPMember;
  Pkt:    TIdCompoundRTCPPacket;
  RR:     TIdRTCPReceiverReport;
begin
  Member := Self.AddNewSender;

  Pkt := Self.Session.CreateNextReport;
  try
    CheckEquals(IntToHex(Self.Session.SyncSrcID, 8),
                IntToHex(Pkt.SyncSrcID, 8),
                'SSRC');

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
                'Wrong number of report blocks (for senders)');
    RR := Pkt.PacketAt(0) as TIdRTCPReceiverReport;

    CheckEquals(IntToHex(Member.SyncSrcID, 8),
                IntToHex(RR.Reports[0].SyncSrcID, 8),
                'Report SSRC');

    CheckEquals(Member.HighestSeqNo,
                RR.Reports[0].HighestSeqNo,
                'Highest sequence no');
    CheckEquals(Member.Jitter,
                RR.Reports[0].InterArrivalJitter,
                'Interarrival jitter');

    CheckEquals(Member.PacketLossCount,
                RR.Reports[0].CumulativeLoss,
                'Cumulative loss');
    CheckEquals(0, RR.Reports[0].DelaySinceLastSR, 'Delay since last SR');
    CheckEquals(Member.PacketLossFraction,
                RR.Reports[0].FractionLost,
                'Fraction lost');
    CheckEquals(0, RR.Reports[0].LastSenderReport, 'Last SR');
  finally
    Pkt.Free;
  end;
end;

procedure TestSessionReportRules.TestReportDetailsUnvalidatedSource;
var
  Pkt: TIdCompoundRTCPPacket;
  RR:  TIdRTCPReceiverReport;
begin
  Self.Session.ReceiveData(Self.Data, Self.Binding);

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
    RR := Pkt.PacketAt(0) as TIdRTCPReceiverReport;

    CheckEquals(IntToHex(Self.Data.SyncSrcID, 8),
                IntToHex(RR.Reports[0].SyncSrcID, 8),
                'Report SSRC');

    // We don't check some of the RTCP statistics because it makes no
    // sense if we haven't validated the source.
    // CumulativeLoss in particular depends on a base sequence number,
    // which we only store upon source validation.
    CheckEquals(0, RR.Reports[0].DelaySinceLastSR, 'Delay since last SR');
    CheckEquals(0, RR.Reports[0].FractionLost,     'Fraction lost');
    CheckEquals(Self.Data.SequenceNo,
                RR.Reports[0].HighestSeqNo,
                'Highest sequence no of UNVALIDATED sender');
    CheckEquals(0, RR.Reports[0].InterArrivalJitter, 'Interarrival jitter');
    CheckEquals(0, RR.Reports[0].LastSenderReport,   'Last SR');
  finally
    Pkt.Free;
  end;
end;

procedure TestSessionReportRules.TestReportWith0Senders;
var
  Pkt: TIdCompoundRTCPPacket;
begin
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

    CheckEquals(0,
                (Pkt.PacketAt(0) as TIdRTCPReceiverReport).ReceptionReportCount,
                'Wrong number of report blocks');
  finally
    Pkt.Free;
  end;
end;

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

procedure TestSessionReportRules.TestReportWith31Senders;
var
  I:   Integer;
  Pkt: TIdCompoundRTCPPacket;
begin
  for I := 1 to 31 do
    Self.Session.AddSender(Self.Session.NewSSRC);

  Pkt := Self.Session.CreateNextReport;
  try
    // One (full) RR and the obligatory SDES
    CheckEquals(2, Pkt.PacketCount, 'Packet count');
    CheckEquals(TIdRTCPReceiverReport,
                Pkt.PacketAt(0).ClassType,
                '1st packet');
    CheckEquals(TIdRTCPSourceDescription,
                Pkt.PacketAt(1).ClassType,
                '2nd packet');

    CheckEquals(31,
                (Pkt.PacketAt(0) as TIdRTCPReceiverReport).ReceptionReportCount,
                'Wrong number of report blocks, 1st RR');
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
    CheckEquals(TIdRTCPReceiverReport,
                Pkt.PacketAt(0).ClassType,
                '1st packet');
    CheckEquals(TIdRTCPReceiverReport,
                Pkt.PacketAt(1).ClassType,
                '2nd packet');
    CheckEquals(TIdRTCPSourceDescription,
                Pkt.PacketAt(2).ClassType,
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
  Payload: TIdRTPT140Payload;
begin
  CheckEquals(0,
              Self.Session.SentOctetCount,
              'Initially we have sent no data');

  Payload := TIdRTPT140Payload.Create;
  try
    Payload.Block := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';
    DataLen := Payload.Length;

    Self.Session.SendData(Payload);
    CheckEquals(DataLen,
                Self.Session.SentOctetCount,
                'SentOctetCount not updated');

    Self.Session.SendData(Payload);
    CheckEquals(2*DataLen,
                Self.Session.SentOctetCount,
                'SentOctetCount not updated; 2nd packet');
  finally
    Payload.Free;
  end;
end;

procedure TestSessionReportRules.TestSentPacketCount;
var
  I:       Integer;
  Payload: TIdRTPT140Payload;
begin
  CheckEquals(0,
              Self.Session.SentPacketCount,
              'Initially we have sent no data');

  Payload := TIdRTPT140Payload.Create;
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
  Payload: TIdRTPT140Payload;
begin
  Payload := TIdRTPT140Payload.Create;
  try
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
  Payload: TIdRTPT140Payload;
begin
  Payload := TIdRTPT140Payload.Create;
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

  Self.Binding.LocalIP   := '127.0.0.1';
  Self.Binding.LocalPort := 4321;
  Self.Binding.PeerIP    := '1.2.3.4';
  Self.Binding.PeerPort  := 4321;

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

  inherited TearDown;
end;

//* TestSessionSendReceiveRules Published methods ******************************

procedure TestSessionSendReceiveRules.TestAddReceiverGetsItsSsrc;
var
  OriginalMemberCount: Cardinal;
begin
  // You want to enter an RTP session. You know the IP/port of the remote end.
  // Since you've received no data yet, you don't know its SSRC. You cunningly
  // use Session.AddReceiver to tell the session about the IP/port. But when
  // you receive RTP/RTCP from the remote end, you must update the existing
  // entry in the member table, not add a new one.

  Self.Data.CsrcCount := 0;

  OriginalMemberCount := Self.Session.MemberCount;
  Self.Session.AddReceiver(Self.Binding.PeerIP,
                           Self.Binding.PeerPort);
  CheckEquals(1 + OriginalMemberCount,
              Self.Session.MemberCount,
              'AddReceiver should add member');
  Self.Session.ReceiveData(Self.Data, Self.Binding);
  CheckEquals(1 + OriginalMemberCount,
              Self.Session.MemberCount,
              'Received data should update existing member');
end;

procedure TestSessionSendReceiveRules.TestCollisionTriggersBye;
begin
  Self.SR.ReceptionReportCount := 0;
  Self.Data.CsrcCount          := 0;

  // We need at least one other person in the session
  Self.Session.ReceiveData(Self.Data,  Self.Binding);
  Self.Session.ReceiveControl(Self.SR, Self.RTCPBinding);

  // We induce an SSRC collision: the remote party changes its SSRC to our SSRC.
  Self.Data.CsrcCount  := 1;
  Self.Data.CsrcIDs[0] := Self.Session.SyncSrcID;
  Self.Session.ReceiveData(Self.Data, Self.Binding);

  // And then we make sure that we've sent an RTCP BYE and an RR (to indicate
  // that we've rejoined the session, and advertise our new SSRC).
  Self.Timer.TriggerEarliestEvent;

  Check(Self.Agent.RTCPCount > 1,
        'Not enough control stuff sent');

  Check(Self.Agent.SecondLastRTCP.IsBye,
        'No BYE sent');
  CheckEquals(TIdCompoundRTCPPacket.ClassName,
              Self.Agent.LastRTCP.ClassName,
             'No RR sent, so we didn''t rejoin session');
  Check(TIdCompoundRTCPPacket(Self.Agent.LastRTCP).HasReceiverReport,
        'No RR sent in the compound packet');
end;

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
var
  Table: TIdRTPMemberTable;
begin
  Table := Self.Session.LockMembers;
  try
    Self.Session.MaxRTCPBandwidth := 100; // bytes per second
    CheckEquals(Self.Session.MinimumRTCPSendInterval / 2,
                Self.Session.DeterministicSendInterval(Self.Session.IsSender, Table),
                OneMillisecond,
                'Initial send interval should be half the minimum');
  finally
    Self.Session.UnlockMembers;
  end;
end;

procedure TestSessionSendReceiveRules.TestReceiveRTPAddsMembers;
var
  I: Integer;
begin
  Self.Session.ReceiveData(Self.Data, Self.Binding);
  CheckEquals(4,
              Self.Session.MemberCount,
              'Session member count: SSRC + 2 CSRCSs + self');

  Check(Self.Session.IsMember(Self.Data.SyncSrcID), 'Data.SyncSrcID');

  for I := 0 to Self.Data.CsrcCount - 1 do
    Check(Self.Session.IsMember(Self.Data.CsrcIDs[I]),
          'Data.CsrcIDs[' + IntToStr(I) + ']');
end;

procedure TestSessionSendReceiveRules.TestReceiveByeMarksMembers;
begin
  Self.Session.ReceiveData(Self.Data, Self.Binding);
  Self.Session.ReceiveControl(Self.Bye, Self.Binding);

  Check(not Self.Session.IsMember(Self.Bye.SyncSrcID),
        'BYE didn''t remove member as leaving');
  Check(Self.Session.IsMember(Self.Data.SyncSrcID),
        'Wrong member removed');

  Check(not Self.Session.IsSender(Self.Bye.SyncSrcID),
        'BYE didn''t remove member (from senders)');
  Check(Self.Session.IsSender(Self.Data.SyncSrcID),
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
    Check(Self.Session.IsMember(Self.SR.Reports[I].SyncSrcID),
          'SR.Reports[' + IntToStr(I) + '].SyncSrcID');
end;

procedure TestSessionSendReceiveRules.TestRTCPDoesntAddSender;
begin
  Self.Session.ReceiveControl(Self.SR, Self.Binding);
  CheckEquals(0, Self.Session.SenderCount, 'Sender added');
end;

procedure TestSessionSendReceiveRules.TestSendDataToInvalidSender;
var
  L:        TIdRTPTestRTPDataListener;
  Member:   TIdRTPMember;
  OldCount: Cardinal;
begin
  Self.Data.SequenceNo := 0;
  Self.Data.Timestamp  := 0;
  Self.Data.CsrcCount  := 0;

  L := TIdRTPTestRTPDataListener.Create;
  try
    Self.Session.AddListener(L);

    Self.Session.ReceiveData(Self.Data, Self.Binding);
    Check(L.NewData, 'Listener didn''t receive data');

    // Receive some more (invalid) data from the network - the sequence number
    // isn't incrementing!
    Self.Session.ReceiveData(Self.Data, Self.Binding);
    Self.Session.ReceiveData(Self.Data, Self.Binding);

    Member := Self.Session.Member(Self.Data.SyncSrcID);
    Check(Assigned(Member), 'Member not added');
    Check(Member.IsUnderProbation, 'Member not under probation');

    OldCount := Self.Agent.RTPCount;
    Self.Session.SendData(Self.Data.Payload);
    Check(OldCount < Self.Agent.RTPCount, 'No data sent');
  finally
    Self.Session.RemoveListener(L);
    L.Free;
  end;
end;

procedure TestSessionSendReceiveRules.TestSendDataWhenOtherMembersHaventSentData;
var
  OldRTPCount:    Cardinal;
  Payload:        TIdRTPT140Payload;
  SingleMemberSR: TIdRTCPSenderReport;
begin
  SingleMemberSR := TIdRTCPSenderReport.Create;
  try
    SingleMemberSR.SyncSrcID := Self.Data.SyncSrcID;
    Self.Session.ReceiveControl(SingleMemberSR, Self.Binding);

    OldRTPCount := Agent.RTPCount;

    // Note that we have to send data to those members from whom we've not
    // received data. Otherwise we find ourselves in a chicken-and-egg problem,
    // because they won't send data to US (because we've not sent data to them).
    // This implies that when you add a member you know their data port from
    // _somewhere_ (like, say, an SDP message).

    Payload := TIdRTPT140Payload.Create;
    try
      Payload.Block := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';

      Self.Session.SendData(Payload);
      CheckEquals(OldRTPCount + 1,
                  Self.Agent.RTPCount,
                  'Data sent despite not knowing remote member''s data port');
    finally
      Payload.Free;
    end;
  finally
    SingleMemberSR.Free;
  end;
end;

procedure TestSessionSendReceiveRules.TestSendDataWhenNoOtherMembersHaveJoined;
var
  Payload: TIdRTPT140Payload;
begin
  Payload := TIdRTPT140Payload.Create;
  try
    Payload.Block := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';

    Self.Session.SendData(Payload);
    CheckEquals(0,
                Self.Agent.RTPCount,
                'RTP sent to ' + Self.Agent.LastPacketHostTarget + ':' + IntToStr(Self.Agent.LastPacketPortTarget));
  finally
    Payload.Free;
  end;
end;

procedure TestSessionSendReceiveRules.TestDeterministicSendInterval10MembersAndNotSender;
var
  I:     Integer;
  Table: TIdRTPMemberTable;
begin
  // See RFC 3550 section 6.2, 6.3, Appendix A.7 for details

  Self.Session.MaxRTCPBandwidth := 100; // bytes per second
  for I := 1 to 9 do
    Self.Session.AddMember(I);

  Table := Self.Session.LockMembers;
  try
    CheckEquals(2666*OneMillisecond,
                Self.Session.DeterministicSendInterval(Self.Session.IsSender, Table),
                OneMillisecond,
                '10 members, session''s not a sender');
  finally
    Self.Session.UnlockMembers;
  end;
end;

procedure TestSessionSendReceiveRules.TestDeterministicSendInterval10MembersAndSender;
var
  Data:  TIdRTPPayload;
  Table: TIdRTPMemberTable;
begin
  // See RFC 3550 section 6.2, 6.3, Appendix A.7 for details

  // Senders get much less bandwidth - 2666/0.75*0.25 = 2666/3 ~= 888ms;
  // 888 < minimum RTCP interval
  Self.Session.MaxRTCPBandwidth := 100; // bytes per second

  Data := TIdRTPPayload.CreatePayload(T140Encoding);
  try
    Self.Session.SendData(Data);
  finally
    Data.Free;
  end;

  Table := Self.Session.LockMembers;
  try
    CheckEquals(Self.Session.MinimumRTCPSendInterval / 2,
                Self.Session.DeterministicSendInterval(Self.Session.IsSender, Table),
                OneMillisecond,
                '10 members, session''s a sender');
  finally
    Self.Session.UnlockMembers;
  end;
end;

procedure TestSessionSendReceiveRules.TestDeterministicSendIntervalMinimumInterval;
var
  I:     Integer;
  Table: TIdRTPMemberTable;
begin
  // See RFC 3550 section 6.2, 6.3, Appendix A.7 for details

  Self.Session.MaxRTCPBandwidth := 100; // bytes per second
  for I := 1 to 4 do
    Self.Session.AddMember(I);

  Table := Self.Session.LockMembers;
  try
    // Calculated interval is 1333ms, and 1333ms < minimum RTCP interval
    CheckEquals(Self.Session.MinimumRTCPSendInterval / 2,
                Self.Session.DeterministicSendInterval(Self.Session.IsSender, Table),
                OneMillisecond,
                '5 members, session''s not a sender');
  finally
    Self.Session.UnlockMembers;
  end;
end;

procedure TestSessionSendReceiveRules.TestDeterministicSendIntervalWithZeroBandwidth;
var
  Table: TIdRTPMemberTable;
begin
  Table := Self.Session.LockMembers;
  try
    CheckEquals(Self.Session.MinimumRTCPSendInterval / 2,
                Self.Session.DeterministicSendInterval(Self.Session.IsSender, Table),
                OneMillisecond,
                'Zero bandwidth');
  finally
    Self.Session.UnlockMembers;
  end;
end;

procedure TestSessionSendReceiveRules.TestSendControlDoesntSendToSelf;
var
  RTCPCount: Cardinal;
begin
  // At SetUp, there's only one member in the session, namely Self.Session.
  // Thus, if any RTCP messages make it onto the network (as revealed by
  // Agent.RTCPCount), then Self.Session send RTCP data to itself.

  RTCPCount := Self.Agent.RTCPCount;
  Self.Session.SendControl(Self.SR);
  CheckEquals(RTCPCount, Self.Agent.RTCPCount, 'Session sent RTCP to itself');
end;

//******************************************************************************
//* TestTIdRTPPacketBuffer                                                     *
//******************************************************************************
//* TestTIdRTPPacketBuffer Public methods **************************************

procedure TestTIdRTPPacketBuffer.SetUp;
begin
  inherited SetUp;

  Self.Profile := TIdAudioVisualProfile.Create;
  Self.Q := TIdRTPPacketBuffer.Create;
end;

procedure TestTIdRTPPacketBuffer.TearDown;
begin
  Self.Q.Free;
  Self.Profile.Free;

  inherited TearDown;
end;

//* TestTIdRTPPacketBuffer Private methods *************************************

function TestTIdRTPPacketBuffer.CreateExamplePacket(const Timestamp: Cardinal): TIdRTPPacket;
begin
 Result := TIdRTPPacket.Create(Self.Profile);
 try
   Result.SyncSrcID := $decafbad;
   Result.Timestamp := Timestamp;
 except
   FreeAndNil(Result);
   raise;
 end;
end;

//* TestTIdRTPPacketBuffer Published methods ***********************************

procedure TestTIdRTPPacketBuffer.TestBadlyOrderedStream;
var
  I: Integer;
begin
  Self.Q.Add(Self.CreateExamplePacket(3));
  Self.Q.Add(Self.CreateExamplePacket(1));
  Self.Q.Add(Self.CreateExamplePacket(2));

  for I := 1 to 3 do begin
    CheckEquals(I, Self.Q.Last.Timestamp, 'Last #' + IntToStr(I));
    Self.Q.RemoveLast;
  end;
end;

procedure TestTIdRTPPacketBuffer.TestOutOfOrderPacket;
var
  I: Integer;
begin
  Self.Q.Add(Self.CreateExamplePacket(1));
  Self.Q.Add(Self.CreateExamplePacket(3));
  Self.Q.Add(Self.CreateExamplePacket(2));

  for I := 1 to 3 do begin
    CheckEquals(I, Self.Q.Last.Timestamp, 'Last #' + IntToStr(I));
    Self.Q.RemoveLast;
  end;
end;

procedure TestTIdRTPPacketBuffer.TestWellOrderedStream;
var
  I: Integer;
begin
  for I := 1 to 3 do
    Self.Q.Add(Self.CreateExamplePacket(I));

  for I := 1 to 3 do begin
    CheckEquals(I, Self.Q.Last.Timestamp, 'Last #' + IntToStr(I));
    Self.Q.RemoveLast;
  end;
end;

//******************************************************************************
//* TRTPListenerTestCase                                                       *
//******************************************************************************
//* TRTPListenerTestCase Public methods ****************************************

procedure TRTPListenerTestCase.SetUp;
begin
  inherited SetUp;

  Self.Binding := TIdConnectionBindings.Create;
end;

procedure TRTPListenerTestCase.TearDown;
begin
  Self.Binding.Free;

  inherited TearDown;
end;

procedure TRTPListenerTestCase.CheckEquals(Expected,
                                           Received: TIdConnectionBindings;
                                           Msg: String);
begin
  CheckEquals(Expected.LocalIP,   Received.LocalIP,   Msg + ' (LocalIP)');
  CheckEquals(Expected.LocalPort, Received.LocalPort, Msg + ' (LocalPort)');
  CheckEquals(Expected.PeerIP,    Received.PeerIP,    Msg + ' (PeerIP)');
  CheckEquals(Expected.PeerPort,  Received.PeerPort,  Msg + ' (PeerPort)');
  CheckEquals(Expected.Transport, Received.Transport, Msg + ' (Transport)');
end;

//******************************************************************************
//* TestTIdRTPListenerReceiveRTCPMethod                                        *
//******************************************************************************
//* TestTIdRTPListenerReceiveRTCPMethod Public methods *************************

procedure TestTIdRTPListenerReceiveRTCPMethod.SetUp;
begin
  inherited SetUp;

  Self.Packet := TIdRTCPReceiverReport.Create;

  Self.Method := TIdRTPListenerReceiveRTCPMethod.Create;
  Self.Method.Binding := Self.Binding;
  Self.Method.Packet  := Self.Packet;
end;

procedure TestTIdRTPListenerReceiveRTCPMethod.TearDown;
begin
  Self.Method.Free;
  Self.Packet.Free;

  inherited TearDown;
end;

//* TestTIdRTPListenerReceiveRTCPMethod Published methods **********************

procedure TestTIdRTPListenerReceiveRTCPMethod.TestRun;
var
  Listener: TIdRTPTestRTPListener;
begin
  Listener := TIdRTPTestRTPListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.ReceivedRTCP,
          Self.ClassName + ': Listener not notified');
    CheckEquals(Self.Method.Binding,
                Listener.BindingParam,
                Self.ClassName + ': Binding param');
    Check(Self.Method.Packet = Listener.RTCPPacketParam,
          Self.ClassName + ': Packet param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdRTPListenerReceiveRTPMethod                                         *
//******************************************************************************
//* TestTIdRTPListenerReceiveRTPMethod Public methods **************************

procedure TestTIdRTPListenerReceiveRTPMethod.SetUp;
begin
  inherited SetUp;

  Self.Packet  := TIdRTPPacket.Create(nil);

  Self.Method := TIdRTPListenerReceiveRTPMethod.Create;
  Self.Method.Binding := Self.Binding;
  Self.Method.Packet  := Self.Packet;
end;

procedure TestTIdRTPListenerReceiveRTPMethod.TearDown;
begin
  Self.Method.Free;
  Self.Packet.Free;

  inherited TearDown;
end;

//* TestTIdRTPListenerReceiveRTPMethod Published methods ***********************

procedure TestTIdRTPListenerReceiveRTPMethod.TestRun;
var
  Listener: TIdRTPTestRTPListener;
begin
  Listener := TIdRTPTestRTPListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.ReceivedRTP,
          Self.ClassName + ': Listener not notified');
    CheckEquals(Self.Method.Binding,
                Listener.BindingParam,
                Self.ClassName + ': Binding param');
    Check(Self.Method.Packet = Listener.RTPPacketParam,
          Self.ClassName + ': Packet param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdRTPDataListenerNewDataMethod                                        *
//******************************************************************************
//* TestTIdRTPDataListenerNewDataMethod Public methods *************************

procedure TestTIdRTPDataListenerNewDataMethod.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdRTPRawPayload.Create;
  Self.Method := TIdRTPDataListenerNewDataMethod.Create;
  Self.Method.Binding := Self.Binding;
  Self.Method.Data    := Self.Data;
end;

procedure TestTIdRTPDataListenerNewDataMethod.TearDown;
begin
  Self.Method.Free;
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdRTPDataListenerNewDataMethod Published methods **********************

procedure TestTIdRTPDataListenerNewDataMethod.TestRun;
var
  Listener: TIdRTPTestRTPDataListener;
begin
  Listener := TIdRTPTestRTPDataListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.NewData,
          Self.ClassName + ': Listener not notified');
    Check(Self.Method.Data = Listener.DataParam,
          Self.ClassName + ': Data param');
    CheckEquals(Self.Method.Binding,
                Listener.BindingParam,
                Self.ClassName + ': Binding param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TTestCaseRTP                                                               *
//******************************************************************************
//* TTestCaseRTP Public methods ************************************************

procedure TTestCaseRTP.SetUp;
begin
  inherited SetUp;

  Self.Profile := TIdAudioVisualProfile.Create;
  Self.Timer   := TIdDebugTimerQueue.Create(false);

  Self.Agent := TIdMockRTPPeer.Create;
  Self.Agent.LocalProfile  := Self.Profile;
  Self.Agent.RemoteProfile := Self.Profile;
  Self.Agent.Timer         := Self.Timer;

  Self.Session := Self.Agent.Session;

  Self.Wait := Self.WaitType.Create;
  Self.Wait.SessionID := Self.Session.ID;
end;

procedure TTestCaseRTP.TearDown;
begin
  Self.Wait.Free;
  Self.Agent.Free;
  Self.Timer.Terminate;
  Self.Profile.Free;

  inherited TearDown;
end;

//* TTestCaseRTP Protected methods *********************************************

procedure TTestCaseRTP.CheckTriggerDoesNothing(Msg: String);
begin
  Fail(Self.ClassName + ' must override CheckTriggerDoesNothing');
end;

function TTestCaseRTP.WaitType: TIdRTPWaitClass;
begin
  Result := nil;
  Fail(Self.ClassName + ' must override WaitType');
end;

//* TTestCaseRTP Published methods *********************************************

procedure TTestCaseRTP.TestTriggerWithNonexistentSession;
begin
  Self.Wait.SessionID := 'fake ID';
  Self.Wait.Trigger;
  CheckTriggerDoesNothing('Triggered using a nonexistent object ID');
end;

procedure TTestCaseRTP.TestTriggerWithWrongTypeOfObject;
var
  R: TIdRegisteredObject;
begin
  R := TIdRegisteredObject.Create;
  try
    Self.Wait.SessionID := R.ID;
    Self.Wait.Trigger;
    CheckTriggerDoesNothing('Triggered using the ID of some inappropriate object');
  finally
    R.Free;
  end;
end;

//******************************************************************************
//* TestTIdRTPReceivePacketWait                                                *
//******************************************************************************
//* TestTIdRTPReceivePacketWait Public methods *********************************

procedure TestTIdRTPReceivePacketWait.SetUp;
const
  ArbitraryHost = '127.0.0.1';
  ArbitraryPort = 9000;
  T140PT        = 98;
var
  M: TIdRTPMember;
  W: TIdRTPReceivePacketWait;
begin
  inherited SetUp;

  Self.Payload := TIdRTPT140Payload.Create;
  Self.Payload.Block := 'test';

  Self.Profile.AddEncoding(Self.Payload, T140PT);

  Self.Binding := TIdConnectionBindings.Create;
  Self.Binding.LocalIP   := '127.0.0.1';
  Self.Binding.LocalPort := 8000;
  Self.Binding.PeerIP    := '127.0.0.2';
  Self.Binding.PeerPort  := 9000;

  Self.Control := TIdRTCPBye.Create;
  Self.Control.SyncSrcID := $decafbad;

  Self.Data := TIdRTPPacket.Create(Self.Profile);
  Self.Data.SyncSrcID := Self.Control.SyncSrcID;

  Self.Data.Payload := Self.Payload;
  Self.Data.PayloadType := T140PT;

  Self.Listener := TIdRTPTestRTPDataListener.Create;

  Self.Session.AddListener(Self.Listener);

  M := Self.Session.AddMember(Self.Control.SyncSrcID);
  M.SourceAddress := ArbitraryHost;
  M.SourcePort    := ArbitraryPort;

  W := Self.Wait as TIdRTPReceivePacketWait;
  W.ReceivedFrom := Self.Binding;
  W.SessionID    := Self.Session.ID;

  Self.MemberCount := Self.Session.MemberCount;
end;

procedure TestTIdRTPReceivePacketWait.TearDown;
begin
  Self.Listener.Free;
  Self.Data.Free;
  Self.Control.Free;
  Self.Binding.Free;
  Self.Payload.Free;

  inherited TearDown;
end;

//* TestTIdRTPReceivePacketWait Protected methods ******************************

procedure TestTIdRTPReceivePacketWait.CheckTriggerDoesNothing(Msg: String);
begin
  CheckEquals(Self.MemberCount, Self.Session.MemberCount, Msg);
end;

function TestTIdRTPReceivePacketWait.WaitType: TIdRTPWaitClass;
begin
  Result := TIdRTPReceivePacketWait;
end;

//* TestTIdRTPReceivePacketWait Published methods ******************************

procedure TestTIdRTPReceivePacketWait.TestTriggerRTCP;
var
  MemberCount: Cardinal;
begin
  MemberCount := Self.Session.MemberCount;

  (Self.Wait as TIdRTPReceivePacketWait).Packet := Self.Control.Copy;
  Self.Wait.Trigger;

  Check(Self.Session.MemberCount < MemberCount, 'Bye not acted upon, so Wait not triggered');
end;

procedure TestTIdRTPReceivePacketWait.TestTriggerRTP;
begin
  (Self.Wait as TIdRTPReceivePacketWait).Packet := Self.Data.Copy;

  Self.Wait.Trigger;

  Check(Self.Listener.NewData, 'Listener not notified, so Wait not triggered');
  CheckEquals(Self.Payload.ClassName,
              Self.Listener.DataParam.ClassName,
              'Packet param');
  CheckEquals(Self.Payload.Block,
              (Self.Listener.DataParam as TIdRTPT140Payload).Block,
              'Packet param (block)');
  CheckEquals(Self.Binding.LocalIP,   Self.Listener.BindingParam.LocalIP,   'Binding local IP');
  CheckEquals(Self.Binding.LocalPort, Self.Listener.BindingParam.LocalPort, 'Binding local port');
  CheckEquals(Self.Binding.PeerIP,    Self.Listener.BindingParam.PeerIP,    'Binding peer IP');
  CheckEquals(Self.Binding.PeerPort,  Self.Listener.BindingParam.PeerPort,  'Binding peer port');
end;

//******************************************************************************
//* TSenderReportTestCase                                                      *
//******************************************************************************
//* TSenderReportTestCase Public methods ***************************************

procedure TSenderReportTestCase.SetUp;
const
  ArbitraryHost = '127.0.0.1';
  ArbitraryPort = 9000;
begin
  inherited SetUp;

  Self.Session.AddReceiver(ArbitraryHost, ArbitraryPort);
end;

//* TSenderReportTestCase Protected methods ************************************

procedure TSenderReportTestCase.CheckTriggerDoesNothing(Msg: String);
begin
  CheckEquals(0, Self.Agent.RTCPCount, Msg);
end;

//* TSenderReportTestCase Published methods ************************************

procedure TSenderReportTestCase.TestTrigger;
begin
  CheckEquals(0, Self.Agent.RTCPCount, 'Sanity check: RTCP sent before it should have');

  Self.Wait.Trigger;

  Check(Self.Agent.RTCPCount > 0, 'No RTCP sent, ergo Trigger didn''t happen');
end;

//******************************************************************************
//* TestTIdRTPTransmissionTimeExpire                                           *
//******************************************************************************
//* TestTIdRTPTransmissionTimeExpire Protected methods *************************

function TestTIdRTPTransmissionTimeExpire.WaitType: TIdRTPWaitClass;
begin
  Result := TIdRTPTransmissionTimeExpire;
end;

//******************************************************************************
//* TestTIdRTPSenderReportWait                                                 *
//******************************************************************************
//* TestTIdRTPSenderReportWait Protected methods *******************************

function TestTIdRTPSenderReportWait.WaitType: TIdRTPWaitClass;
begin
  Result := TIdRTPSenderReportWait;
end;

//******************************************************************************
//* TestTIdRTPSendDataWait                                                     *
//******************************************************************************
//* TestTIdRTPSendDataWait Public methods **************************************

procedure TestTIdRTPSendDataWait.SetUp;
const
  ArbitraryHost = '127.0.0.1';
  ArbitraryPort = 9000;
  T140PT        = 98;
begin
  inherited SetUp;

  Self.Payload := TIdRTPT140Payload.Create;
  Self.Payload.Block := 'test';

  Self.Profile.AddEncoding(Self.Payload, T140PT);

  Self.Session.AddReceiver(ArbitraryHost, ArbitraryPort);

  (Self.Wait as TIdRTPSendDataWait).Data := Self.Payload.Copy;
end;

//* TestTIdRTPSendDataWait Protected methods ***********************************

procedure TestTIdRTPSendDataWait.CheckTriggerDoesNothing(Msg: String);
begin
  CheckEquals(0, Self.Agent.RTPCount, Msg);
end;

function TestTIdRTPSendDataWait.WaitType: TIdRTPWaitClass;
begin
  Result := TIdRTPSendDataWait;
end;

//* TestTIdRTPSendDataWait Published methods ***********************************

procedure TestTIdRTPSendDataWait.TestTrigger;
begin
  CheckEquals(0, Self.Agent.RTPCount, 'Sanity check: RTP sent before it should have');

  Self.Wait.Trigger;

  Check(Self.Agent.RTPCount > 0, 'No RTP sent, ergo Trigger didn''t happen');

  CheckEquals(T140Encoding,
              Self.Agent.LastRTP.Payload.EncodingName,
              'Payload data type');
  CheckEquals(Self.Payload.Block,
              (Self.Agent.LastRTP.Payload as TIdRTPT140Payload).Block,
              'Payload');
end;

initialization
  RegisterTest('RTP', Suite);
end.

