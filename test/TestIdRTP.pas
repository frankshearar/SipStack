unit TestIdRTP;

interface

uses
  IdRTP, TestFramework, TestFrameworkRtp;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestAddModulo;
    procedure TestAddModuloWord;
    procedure TestDateTimeToNTPFractionsOfASecond;
    procedure TestDateTimeToNTPSeconds;
    procedure TestDateTimeToNTPTimestampExceptionalCases;
    procedure TestDateTimeToNTPTimestampFractionalValues;
    procedure TestDateTimeToNTPTimestampIntegralValues;
    procedure TestEncodeAsStringCardinal;
    procedure TestEncodeAsStringWord;
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
    procedure TestReadRemainderOfStreamLong;
    procedure TestReadTimestamp;
    procedure TestReadString;
    procedure TestReadWord;
    procedure TestWriteCardinal;
    procedure TestWriteTimestamp;
    procedure TestWriteWord;
  end;

  TestTIdRTPEncoding = class(TTestCase)
  published
    procedure TestAsString;
    procedure TestCreate;
    procedure TestCreateEncoding;
    procedure TestCreateEncodingT140;
    procedure TestCreateEncodingTelephoneEvent;
    procedure TestCreateFromEncoding;
    procedure TestClone;
    procedure TestIsNull;
  end;

  TTestCaseEncoding = class(TTestCase)
  private
    Encoding: TIdRTPEncoding;

    function EncodingType: TIdRTPEncodingClass; virtual; abstract;
    function EncodingName: String; virtual; abstract;
    function EncodingClockRate: Cardinal; virtual;
    function EncodingParameters: String; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClone;
    procedure TestCreatePayload;
  end;

  TestTIdT140Encoding = class(TTestCaseEncoding)
  private
    function EncodingType: TIdRTPEncodingClass; override;
    function EncodingName: String; override;
    function EncodingClockRate: Cardinal; override;
  end;

  TestTIdTelephoneEventEncoding = class(TTestCaseEncoding)
  private
    function EncodingType: TIdRTPEncodingClass; override;
    function EncodingName: String; override;
  end;

  TestTIdRTPNullEncoding = class(TTestCase)
  private
    Encoding: TIdRTPNullEncoding;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCreate;
    procedure TestCreateFromEncoding;
    procedure TestClone;
    procedure TestIsNull;
  end;

  TestTIdT140Payload = class(TTestCase)
  private
    Encoding: TIdRTPEncoding;
    Packet:   TIdT140Payload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLength;
    procedure TestPrintOn;
    procedure TestReadFrom;
  end;

  TestTIdTelephoneEventPayload = class(TTestCase)
  private
    Encoding:        TIdRTPEncoding;
    Packet:          TIdTelephoneEventPayload;
    SampleDurations: array[0..5] of Word;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNumberOfSamples;
    procedure TestPrintOnDuration;
    procedure TestPrintOnEvent;
    procedure TestPrintOnIsEnd;
    procedure TestPrintOnReservedBit;
    procedure TestPrintOnVolume;
    procedure TestReadFromDuration;
    procedure TestReadFromEvent;
    procedure TestReadFromIsEnd;
    procedure TestReadFromReservedBit;
    procedure TestReadFromVolume;
  end;

  TestTIdRTPProfile = class(TTestCase)
  private
    ArbPT:                   TIdRTPPayloadType;
    Profile:                 TIdRTPProfile;
    T140Encoding:            TIdRTPEncoding;
    InterleavedT140Encoding: TIdRTPEncoding;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddEncoding;
    procedure TestAssign;
    procedure TestClear;
    procedure TestCount;
    procedure TestEncodingFor;
    procedure TestFirstFreePayloadType;
    procedure TestHasEncoding;
    procedure TestHasPayloadType;
    procedure TestIsFull;
    procedure TestPayloadTypeFor;
    procedure TestReservedEncodingMustntOverwriteOthers;
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
  published
    procedure TestIsRTCPPayloadType;
    procedure TestCreatePacketRTCP;
    procedure TestCreatePacketRTP;
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
    procedure TestIsRTCP;
    procedure TestIsRTP;
    procedure TestIsValidBogusVersion;
    procedure TestIsValidKnownPayloadType;
    procedure TestIsValidProfileDoesntAllowHeaderExtensions;
    procedure TestIsValidRealVersion;
    procedure TestIsValidSenderOrReceiverReport;
    procedure TestIsValidUnknownPayloadType;
    procedure TestPrintOn;
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
    procedure TestRealLength;
    procedure TestT140Payload;
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

    function ItemType: TIdSrcDescChunkItemClass; virtual; abstract;
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

    function PacketType: Byte; virtual; abstract;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsBye; virtual;
    procedure TestIsReceiverReport; virtual;
    procedure TestIsSenderReport; virtual;
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
    procedure TestPrintOn;
    procedure TestPrintOnLength;
    procedure TestReadFromWithSingleCname;
  end;

  TestTIdRTCPBye = class(TRTCPPacketTestCase)
  private
    Bye: TIdRTCPBye;
  protected
    function PacketType: Byte; override;
  public
    procedure SetUp; override;
  published
    procedure TestGetAllSrcIDs;
    procedure TestIsBye; override;
    procedure TestPrintOnLength;
    procedure TestPrintOnMultipleSources;
    procedure TestPrintOnReason;
    procedure TestPrintOnSyncSrcId;
    procedure TestReadFrom;
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
    procedure TestFirstPacket;
    procedure TestHasBye;
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

  TIdMockEncoding = class(TIdRTPEncoding)
  public
    function PayloadType: TIdRTPPayloadClass; override;
  end;

  TIdMockPayload = class(TIdRTPPayload)
  private
    fHasKnownLength: Boolean;
    fLength:         Cardinal;
  public
    function  HasKnownLength: Boolean; override;
    function  Length: Cardinal; override;
    procedure SetHasKnownLength(const Yes: Boolean);
    procedure SetLength(const Length: Cardinal);
  end;

  TIdMockProfile = class(TIdAudioVisualProfile)
  private
    fAllowExtension: Boolean;
  public
    function  AllowsHeaderExtensions: Boolean; override;
    procedure SetAllowExtension(const Allow: Boolean);
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
  Classes, DateUtils, IdRtpServer, SysUtils, Types;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTP unit tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdRTPEncoding.Suite);
  Result.AddTest(TestTIdRTPNullEncoding.Suite);
  Result.AddTest(TestTIdT140Encoding.Suite);
  Result.AddTest(TestTIdTelephoneEventEncoding.Suite);
  Result.AddTest(TestTIdTelephoneEventPayload.Suite);
  Result.AddTest(TestTIdT140Payload.Suite);
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
  Result.AddTest(TestTIdRTPPacketBuffer.Suite);
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
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestAddModulo;
begin
  CheckEquals(8, AddModulo(7, 1, 10),   'AddModulo(7, 1, 10)');
  CheckEquals(8, AddModulo(7, 11, 10),  'AddModulo(7, 11, 10)');
  CheckEquals(8, AddModulo(7, 101, 10), 'AddModulo(7, 101, 10)');
  CheckEquals(0, AddModulo(7, 1, 8),    'AddModulo(7, 1, 8)');

  CheckEquals(0,
              AddModulo($fffffffe, 1, $ffffffff),
              'AddModulo($fffffffe, 1, $ffffffff)');

  CheckEquals(1,
              AddModulo($ffffffff, 1, $ffffffff),
              'AddModulo($ffffffff, 1, $ffffffff)');
end;

procedure TestFunctions.TestAddModuloWord;
begin
  CheckEquals(8,  AddModuloWord(7, 1),   'AddModuloWord(7, 1)');
  CheckEquals(18, AddModuloWord(7, 11),  'AddModuloWord(7, 11)');

  CheckEquals(0,
              AddModuloWord($fffe, 1),
              'AddModuloWord($fffe, 1)');

  CheckEquals(1,
              AddModuloWord($ffff, 1),
              'AddModuloWord($ffff, 1)');
end;

procedure TestFunctions.TestDateTimeToNTPFractionsOfASecond;
var
  JanOne1900: TDateTime;
begin
  JanOne1900    := 2;
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
              IntToHex(DateTimeToNTPTimestamp(StrToDateTime('12/04/2002 14:59:59.999')).FractionalPart, 8),
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
              DateTimeToNTPTimestamp(StrToDateTime('05/01/1900 01:02:01')).IntegerPart,
              '1900/01/05 01:02:01, IntegerPart');
  CheckEquals(0,
              DateTimeToNTPTimestamp(StrToDateTime('05/01/1900 01:02:01')).FractionalPart,
              '1900/01/05 01:02:01, FractionalPart');

  ExpectedTime := MultiplyCardinal(AprTwelve2002, SecsPerDay) + 14*3600 + 59*60 + 59;
  ReceivedTime := DateTimeToNTPTimestamp(StrToDateTime('12/04/2002 14:59:59.999')).IntegerPart;
  CheckEquals(IntToHex(ExpectedTime, 8),
              IntToHex(ReceivedTime, 8),
              '2002/04/12 14:59:59.999, IntegerPart');
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

procedure TestFunctions.TestWriteTimestamp;
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
//* TestTIdRTPEncoding                                                         *
//******************************************************************************
//* TestTIdRTPEncoding Published methods ***************************************

procedure TestTIdRTPEncoding.TestAsString;
var
  Enc: TIdRTPEncoding;
begin
  Enc := TIdRTPEncoding.Create('red', 8000);
  try
    CheckEquals('red/8000', Enc.AsString, 'AsString without Parameters');
  finally
    Enc.Free;
  end;

  Enc := TIdRTPEncoding.Create('foo', 8000, '2');
  try
    CheckEquals('foo/8000/2', Enc.AsString, 'AsString with Parameters');
  finally
    Enc.Free;
  end;
end;

procedure TestTIdRTPEncoding.TestCreate;
var
  Enc: TIdRTPEncoding;
begin
  Enc := TIdRTPEncoding.Create(T140Encoding, T140ClockRate, '1');
  try
    CheckEquals(T140ClockRate, Enc.ClockRate,  'ClockRate');
    CheckEquals(T140Encoding,  Enc.Name,       'Name');
    CheckEquals('1',           Enc.Parameters, 'Parameters');
  finally
    Enc.Free;
  end;
end;

procedure TestTIdRTPEncoding.TestCreateEncoding;
var
  Enc: TIdRTPEncoding;
begin
  Enc := TIdRTPEncoding.CreateEncoding('1015/8000/1');
  try
    CheckEquals(TIdRTPEncoding.ClassName,
                Enc.ClassName,
                'Encoding type');
    CheckEquals('1015', Enc.Name,       'Name');
    CheckEquals(8000,   Enc.ClockRate,  'Clock rate');
    CheckEquals('1',    Enc.Parameters, 'Parameters');
  finally
    Enc.Free;
  end;
end;

procedure TestTIdRTPEncoding.TestCreateEncodingT140;
var
  Enc: TIdRTPEncoding;
begin
  Enc := TIdRTPEncoding.CreateEncoding(Format('%s/%d/%s', [T140Encoding, T140ClockRate, '1']));
  try
    CheckEquals(TIdT140Encoding.ClassName,
                Enc.ClassName,
                'Encoding type');
    CheckEquals(T140Encoding,  Enc.Name,       'Name');
    CheckEquals(T140ClockRate, Enc.ClockRate,  'Clock rate');
    CheckEquals('1',           Enc.Parameters, 'Parameters');
  finally
    Enc.Free;
  end;
end;

procedure TestTIdRTPEncoding.TestCreateEncodingTelephoneEvent;
var
  Enc: TIdRTPEncoding;
begin
  Enc := TIdRTPEncoding.CreateEncoding('telephone-event/8000');
  try
    CheckEquals(TIdTelephoneEventEncoding.ClassName,
                Enc.ClassName,
                'Encoding type');
    CheckEquals(TelephoneEventEncoding, Enc.Name,       'Name');
    CheckEquals(8000,                   Enc.ClockRate,  'Clock rate');
    CheckEquals('',                     Enc.Parameters, 'Parameters');
  finally
    Enc.Free;
  end;
end;

procedure TestTIdRTPEncoding.TestCreateFromEncoding;
var
  Enc1: TIdRTPEncoding;
  Enc2: TIdRTPEncoding;
begin
  Enc1 := TIdRTPEncoding.Create(T140Encoding, T140ClockRate, 'foo');
  try
    Enc2 := TIdRTPEncoding.Create(Enc1);
    try
      Check(Enc2.IsEqualTo(Enc1), 'Properties not copied correctly');
    finally
      Enc2.Free;
    end;
  finally
    Enc1.Free;
  end;
end;

procedure TestTIdRTPEncoding.TestClone;
var
  Enc1: TIdRTPEncoding;
  Enc2: TIdRTPEncoding;
begin
  Enc1 := TIdRTPEncoding.Create(T140Encoding, T140ClockRate, 'foo');
  try
    Enc2 := Enc1.Clone;
    try
      CheckEquals(Enc1.ClassName,
                  Enc2.ClassName,
                  'Incorrect type used for cloning');
    finally
      Enc2.Free;
    end;
  finally
    Enc1.Free;
  end;
end;

procedure TestTIdRTPEncoding.TestIsNull;
var
  Enc: TIdRTPEncoding;
begin
  Enc := TIdRTPEncoding.Create(T140Encoding, T140ClockRate, '1');
  try
    Check(not Enc.IsNull, 'non-Null Encoding marked as null');
  finally
    Enc.Free;
  end;
end;

//******************************************************************************
//* TTestCaseEncoding                                                          *
//******************************************************************************
//* TTestCaseEncoding Public methods *******************************************

procedure TTestCaseEncoding.SetUp;
begin
  inherited SetUp;

  Self.Encoding := Self.EncodingType.Create(Self.EncodingName,
                                            Self.EncodingClockRate,
                                            Self.EncodingParameters);
end;

procedure TTestCaseEncoding.TearDown;
begin
  Self.Encoding.Free;

  inherited TearDown;
end;

//* TTestCaseEncoding Public methods *******************************************

function TTestCaseEncoding.EncodingClockRate: Cardinal;
begin
  Result := 8000;
end;

function TTestCaseEncoding.EncodingParameters: String;
begin
  Result := '';
end;

//* TTestCaseEncoding Published methods ****************************************

procedure TTestCaseEncoding.TestClone;
var
  Enc: TIdRTPEncoding;
begin
  Enc := Self.Encoding.Clone;
  try
    CheckEquals(Self.EncodingType.ClassName,
                Enc.ClassName,
                'Type');
    CheckEquals(Self.Encoding.Name,
                Enc.Name,
                'Name');
    CheckEquals(Self.Encoding.ClockRate,
                Enc.ClockRate,
                'Clock Rate');
    CheckEquals(Self.Encoding.Parameters,
                Enc.Parameters,
                'Parameters');
  finally
    Enc.Free;
  end;
end;

procedure TTestCaseEncoding.TestCreatePayload;
var
  Payload: TIdRTPPayload;
begin
  Payload := Self.Encoding.CreatePayload;
  try
    CheckEquals(Self.Encoding.PayloadType.ClassName,
                Payload.ClassName,
                'Payload type');
  finally
    Payload.Free
  end;
end;

//******************************************************************************
//* TestTIdT140Encoding                                                        *
//******************************************************************************
//* TestTIdT140Encoding Private methods ****************************************

function TestTIdT140Encoding.EncodingType: TIdRTPEncodingClass;
begin
  Result := TIdT140Encoding;
end;

function TestTIdT140Encoding.EncodingName: String;
begin
  Result := T140Encoding;
end;

function TestTIdT140Encoding.EncodingClockRate: Cardinal;
begin
  Result := T140ClockRate;
end;

//******************************************************************************
//* TestTIdTelephoneEventEncoding                                           *
//******************************************************************************
//* TestTIdTelephoneEventEncoding Private methods ***************************

function TestTIdTelephoneEventEncoding.EncodingType: TIdRTPEncodingClass;
begin
  Result := TIdTelephoneEventEncoding;
end;

function TestTIdTelephoneEventEncoding.EncodingName: String;
begin
  Result := TelephoneEventEncoding;
end;

//******************************************************************************
//* TestTIdRTPNullEncoding                                                     *
//******************************************************************************
//* TestTIdRTPNullEncoding Public methods **************************************

procedure TestTIdRTPNullEncoding.SetUp;
begin
  inherited SetUp;

  Self.Encoding := TIdRTPNullEncoding.Create('t140', 1000, '1');
end;

procedure TestTIdRTPNullEncoding.TearDown;
begin
  Self.Encoding.Free;

  inherited TearDown;
end;

//* TestTIdRTPNullEncoding Published methods ***********************************

procedure TestTIdRTPNullEncoding.TestAsString;
begin
  CheckEquals('', Self.Encoding.AsString, 'AsString for Null Encoding');
end;

procedure TestTIdRTPNullEncoding.TestCreate;
begin
  CheckEquals(0,  Self.Encoding.ClockRate,  'ClockRate');
  CheckEquals('', Self.Encoding.Name,       'Name');
  CheckEquals('', Self.Encoding.Parameters, 'Parameters');
end;

procedure TestTIdRTPNullEncoding.TestCreateFromEncoding;
var
  Enc:  TIdRTPEncoding;
  Null: TIdRTPNullEncoding;
begin
  Enc := TIdRTPEncoding.Create('t140', 1000, '1');
  try
    Null := TIdRTPNullEncoding.Create(Enc);
    try
      Check(Null.IsEqualTo(Self.Encoding), 'Properties not copied correctly');
    finally
      Null.Free;
    end;
  finally
    Enc.Free;
  end;
end;

procedure TestTIdRTPNullEncoding.TestClone;
var
  Null: TIdRTPEncoding;
begin
  Null := Self.Encoding.Clone;
  try
    CheckEquals(TIdRTPNullEncoding.ClassName,
                Null.ClassName,
                'Incorrect type used for cloning');
  finally
    Null.Free;
  end;
end;

procedure TestTIdRTPNullEncoding.TestIsNull;
begin
  Check(Self.Encoding.IsNull, 'Null Encoding not marked as null');
end;

//******************************************************************************
//* TestTIdT140Payload                                                         *
//******************************************************************************
//* TestTIdT140Payload Public methods ******************************************

procedure TestTIdT140Payload.SetUp;
begin
  inherited SetUp;

  Self.Encoding := TIdT140Encoding.Create;
  Self.Packet   := TIdT140Payload.Create(Self.Encoding);
end;

procedure TestTIdT140Payload.TearDown;
begin
  Self.Packet.Free;
  Self.Encoding.Free;

  inherited TearDown;
end;

//* TestTIdT140Payload Published methods ***************************************

procedure TestTIdT140Payload.TestLength;
var
  S: String;
begin
  Self.Packet.Block := '';
  CheckEquals(0, Self.Packet.Length, 'Length of empty string');

  S := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';
  Self.Packet.Block := S;
  CheckEquals(Length(S), Self.Packet.Length, 'Length');
end;

procedure TestTIdT140Payload.TestPrintOn;
var
  Data: String;
  S:    TStringStream;
begin
  Data := 'fooing the bar';
  S := TStringStream.Create('');
  try
    Self.Packet.Block := Data;

    Self.Packet.PrintOn(S);

    CheckEquals(Data,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdT140Payload.TestReadFrom;
var
  Data: String;
  S:    TStringStream;
begin
  Data := 'fooing the bar';
  S := TStringStream.Create(Data);
  try
    Self.Packet.ReadFrom(S);

    CheckEquals(Data,
                Self.Packet.Block,
                'Block');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdTelephoneEventPayload                                               *
//******************************************************************************
//* TestTIdTelephoneEventPayload Public methods ********************************

procedure TestTIdTelephoneEventPayload.SetUp;
begin
  inherited SetUp;

  Self.Encoding := TIdTelephoneEventEncoding.Create;
  Self.Packet := TIdTelephoneEventPayload.Create(Self.Encoding);

  SampleDurations[0] := $f00d;
  SampleDurations[1] := $beef;
  SampleDurations[2] := $cafe;
  SampleDurations[3] := $deaf;
  SampleDurations[4] := $deca;
  SampleDurations[5] := $fbad;
end;

procedure TestTIdTelephoneEventPayload.TearDown;
begin
  Self.Packet.Free;
  Self.Encoding.Free;

  inherited TearDown;
end;

//* TestTIdTelephoneEventPayload Published methods *****************************

procedure TestTIdTelephoneEventPayload.TestNumberOfSamples;
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

procedure TestTIdTelephoneEventPayload.TestPrintOnDuration;
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

procedure TestTIdTelephoneEventPayload.TestPrintOnEvent;
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

procedure TestTIdTelephoneEventPayload.TestPrintOnIsEnd;
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

procedure TestTIdTelephoneEventPayload.TestPrintOnReservedBit;
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

procedure TestTIdTelephoneEventPayload.TestPrintOnVolume;
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

procedure TestTIdTelephoneEventPayload.TestReadFromDuration;
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

procedure TestTIdTelephoneEventPayload.TestReadFromEvent;
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

procedure TestTIdTelephoneEventPayload.TestReadFromIsEnd;
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

procedure TestTIdTelephoneEventPayload.TestReadFromReservedBit;
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

procedure TestTIdTelephoneEventPayload.TestReadFromVolume;
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

  Self.InterleavedT140Encoding := TIdRTPEncoding.Create('red', 8000);
  Self.Profile                 := TIdRTPProfile.Create;
  Self.T140Encoding            := TIdRTPEncoding.Create('t140', 1000);
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

procedure TestTIdRTPProfile.TestAssign;
var
  Dvi4Vid:    TIdRTPEncoding;
  GSM:        TIdRTPEncoding;
  I:          Integer;
  NewProfile: TIdRTPProfile;
begin
  NewProfile := TIdRTPProfile.Create;
  try
    GSM := TIdRTPEncoding.Create(GSMEncoding, 8000);
    try
      Dvi4Vid := TIdRTPEncoding.Create(DVI4Encoding, 22050);
      try
        NewProfile.AddEncoding(GSM, 5);
        NewProfile.AddEncoding(Dvi4Vid, 10);

        Self.Profile.Assign(NewProfile);

        CheckEquals(NewProfile.Count,
                    Self.Profile.Count,
                    'Encoding count');

        for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do begin
          CheckEquals(NewProfile.EncodingFor(I).ClassName,
                      Self.Profile.EncodingFor(I).ClassName,
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

procedure TestTIdRTPProfile.TestEncodingFor;
begin
  CheckEquals('',
              Self.Profile.EncodingFor(Self.ArbPT).AsString,
              '"MIME type" for unknown Payload Type');

  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, Self.ArbPT - 1);
  Self.Profile.AddEncoding(Self.T140Encoding,            Self.ArbPT);

  CheckEquals(Self.InterleavedT140Encoding.AsString,
              Self.Profile.EncodingFor(Self.ArbPT - 1).AsString,
              'MIME type for ' + InterleavedT140MimeType);

  CheckEquals(Self.T140Encoding.AsString,
              Self.Profile.EncodingFor(Self.ArbPT).AsString,
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
  Enc: TIdRTPEncoding;
begin
  Check(not Self.Profile.IsFull, 'Empty profile');

  for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do begin
    Enc := TIdRTPEncoding.Create('foo', I);
    try
      Self.Profile.AddEncoding(Enc, I);
    finally
      Enc.Free;
    end;
  end;
  Check(Self.Profile.IsFull, 'Full profile');
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
  Res: TIdRTPReservedEncoding;
begin
  Res := TIdRTPReservedEncoding.Create('', 0);
  try
    Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT);
    Self.Profile.AddEncoding(Res, Self.ArbPT);
    Self.Profile.PayloadTypeFor(Self.T140Encoding);
  finally
    Res.Free;
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
  TestEncoding: TIdRTPEncoding;
begin
  PayloadCount := Self.Profile.Count;

  TestEncoding := TIdRTPEncoding.Create('arb values', 0);
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
  Dvi4Vid:       TIdRTPEncoding;
  GSM:           TIdRTPEncoding;
  NewProfile:    TIdRTPProfile;
  VirginProfile: TIdAudioVisualProfile;
begin
  VirginProfile := TIdAudioVisualProfile.Create;
  try
    NewProfile := TIdRTPProfile.Create;
    try
      GSM := TIdRTPEncoding.Create(GSMEncoding, 44100);
      try
        Dvi4Vid := TIdRTPEncoding.Create(DVI4Encoding, 666);
        try
          NewProfile.AddEncoding(GSM, 0);
          NewProfile.AddEncoding(Dvi4Vid, 98);

          Self.Profile.Assign(NewProfile);

          CheckEquals(VirginProfile.EncodingFor(0).ClassName,
                      Self.Profile.EncodingFor(0).ClassName,
                      '0''s type');

          CheckEquals(VirginProfile.EncodingFor(0).AsString,
                      Self.Profile.EncodingFor(0).AsString,
                      '0''s details');

          CheckEquals(NewProfile.EncodingFor(98).ClassName,
                      Self.Profile.EncodingFor(98).ClassName,
                      '98''s type');

          CheckEquals(NewProfile.EncodingFor(98).AsString,
                      Self.Profile.EncodingFor(98).AsString,
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
  CheckEquals('PCMU/8000',    Self.Profile.EncodingFor(0).AsString,   '0');
  CheckEquals('GSM/8000',     Self.Profile.EncodingFor(3).AsString,   '3');
  CheckEquals('G723/8000',    Self.Profile.EncodingFor(4).AsString,   '4');
  CheckEquals('DVI4/8000',    Self.Profile.EncodingFor(5).AsString,   '5');
  CheckEquals('DVI4/16000',   Self.Profile.EncodingFor(6).AsString,   '6');
  CheckEquals('LPC/8000',     Self.Profile.EncodingFor(7).AsString,   '7');
  CheckEquals('PCMA/8000',    Self.Profile.EncodingFor(8).AsString,   '8');
  CheckEquals('G722/8000',    Self.Profile.EncodingFor(9).AsString,   '9');
  CheckEquals('L16/44100/2',  Self.Profile.EncodingFor(10).AsString, '10');
  CheckEquals('L16/44100/1',  Self.Profile.EncodingFor(11).AsString, '11');
  CheckEquals('QCELP/8000',   Self.Profile.EncodingFor(12).AsString, '12');
  CheckEquals('CN/8000',      Self.Profile.EncodingFor(13).AsString, '13');
  CheckEquals('MPA/90000',    Self.Profile.EncodingFor(14).AsString, '14');
  CheckEquals('G728/8000',    Self.Profile.EncodingFor(15).AsString, '15');
  CheckEquals('DVI4/11025',   Self.Profile.EncodingFor(16).AsString, '16');
  CheckEquals('DVI4/22050',   Self.Profile.EncodingFor(17).AsString, '17');
  CheckEquals('G729/8000',    Self.Profile.EncodingFor(18).AsString, '18');

  CheckEquals('CelB/90000',   Self.Profile.EncodingFor(25).AsString, '25');
  CheckEquals('JPEG/90000',   Self.Profile.EncodingFor(26).AsString, '26');

  CheckEquals('nv/90000',     Self.Profile.EncodingFor(28).AsString, '28');

  CheckEquals('H261/90000',   Self.Profile.EncodingFor(31).AsString, '31');
  CheckEquals('MPV/90000',    Self.Profile.EncodingFor(32).AsString, '32');
  CheckEquals('MP2T/90000',   Self.Profile.EncodingFor(33).AsString, '33');
  CheckEquals('H263/90000',   Self.Profile.EncodingFor(34).AsString, '34');
end;

procedure TestTIdAudioVisualProfile.TestDynamicPayloadTypes;
var
  I:            TIdRTPPayloadType;
  PayloadCount: Integer;
  TestEncoding: TIdRTPEncoding;
begin
  for I := 96 to High(TIdRTPPayloadType) do begin
    TestEncoding := TIdRTPEncoding.Create('arb values', I);
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

//* TestTIdRTPBasePacket Published methods *************************************

procedure TestTIdRTPBasePacket.TestIsRTCPPayloadType;
var
  PT: Byte;
begin
  for PT := Low(Byte) to RTCPSenderReport - 1 do
    Check(not TIdRTPBasePacket.IsRTCPPayloadType(PT),
          'Unknown RTCP payload type: ' + IntToStr(PT));

  Check(TIdRTPBasePacket.IsRTCPPayloadType(RTCPSenderReport),
        'RTCPSenderReport');
  Check(TIdRTPBasePacket.IsRTCPPayloadType(RTCPReceiverReport),
        'RTCPReceiverReport');
  Check(TIdRTPBasePacket.IsRTCPPayloadType(RTCPSourceDescription),
        'RTCPSourceDescription');
  Check(TIdRTPBasePacket.IsRTCPPayloadType(RTCPGoodbye),
        'RTCPGoodbye');
  Check(TIdRTPBasePacket.IsRTCPPayloadType(RTCPApplicationDefined),
        'RTCPApplicationDefined');

  for PT := RTCPApplicationDefined + 1 to High(Byte) - 1 do
    Check(not TIdRTPBasePacket.IsRTCPPayloadType(PT),
          'Unknown RTCP payload type: ' + IntToStr(PT));
end;

procedure TestTIdRTPBasePacket.TestCreatePacketRTCP;
var
  Data: TStringStream;
  Pkt:  TIdRTPBasePacket;
begin
  Data := TStringStream.Create(#$00 + Chr(RTCPGoodbye));
  try
    Pkt := TIdRTPBasePacket.CreateFrom(Data, Self.Profile);
    try
      Check(Pkt.IsRTCP, 'RTCP packet not created');
    finally
      Pkt.Free;
    end;
  finally
    Data.Free;
  end;
end;

procedure TestTIdRTPBasePacket.TestCreatePacketRTP;
var
  Data: TStringStream;
  Pkt:  TIdRTPBasePacket;
begin
  Data := TStringStream.Create(#$00#$00);
  try
    Pkt := TIdRTPBasePacket.CreateFrom(Data, Self.Profile);
    try
      Check(Pkt.IsRTP, 'RTP packet not created');
    finally
      Pkt.Free;
    end;
  finally
    Data.Free;
  end;
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
var
  Encoding: TIdT140Encoding;
begin
  inherited SetUp;

  Self.AVP := TIdAudioVisualProfile.Create;

  Self.T140PT := 96;

  Encoding := TIdT140Encoding.Create(T140Encoding, T140ClockRate);
  try
    Self.AVP.AddEncoding(Encoding, Self.T140PT);
  finally
    Encoding.Free;
  end;

  Self.Packet := TIdRTPPacket.Create(Self.AVP);
end;

procedure TestTIdRTPPacket.TearDown;
begin
  Self.Packet.Free;
  Self.AVP.Free;

  inherited TearDown;
end;

//* TestTIdRTPPacket Published methods *****************************************

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
  P:   TIdRTPPacket;
  S:   TStringStream;
  Srv: TIdRTPServer;
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
    Srv := TIdRTPServer.Create(nil);
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
      Srv.Free;
    end;
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
        CheckEquals(I, P.PayloadType, 'PayloadType ' + IntToStr(I));
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
    CheckEquals(TIdT140Payload.ClassName,
                Self.Packet.Payload.ClassName,
                'Payload type');
    CheckEquals('This is black sunshine',
                (Self.Packet.Payload as TIdT140Payload).Block,
                'T140block');
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

procedure TestTIdRTPPacket.TestT140Payload;
var
  Data:     String;
  Expected: TIdT140Payload;
  S:        TStringStream;
begin
  Data := 'ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';
  Expected := TIdT140Payload.Create(Self.AVP.EncodingFor(Self.T140PT));
  try
    Expected.Block := Data;

    S := TStringStream.Create('');
    try
      Expected.PrintOn(S);
      S.Seek(0, soFromBeginning);

      Self.Packet.PayloadType := Self.T140PT;
      Self.Packet.ReadPayload(S);

      CheckEquals(Expected.ClassName,
                  Self.Packet.Payload.ClassName,
                  'Payload type');
      CheckEquals(Expected.Block,
                  (Self.Packet.Payload as TIdT140Payload).Block,
                  'Payload data');
    finally
      S.Free;
    end;
  finally
    Expected.Free;
  end;
end;

//******************************************************************************
//* TestTIdRTCPPacket                                                          *
//******************************************************************************
//* TestTIdRTCPPacket Public methods *******************************************

procedure TestTIdRTCPPacket.TestRTCPType;
var
  I: Byte;
begin
  CheckEquals(TIdRTCPSenderReport.ClassName,
              TIdRTCPPacket.RTCPType(RTCPSenderReport).ClassName,
              'Sender Report');
  CheckEquals(TIdRTCPReceiverReport.ClassName,
              TIdRTCPPacket.RTCPType(RTCPReceiverReport).ClassName,
              'Receiver Report');
  CheckEquals(TIdRTCPSourceDescription.ClassName,
              TIdRTCPPacket.RTCPType(RTCPSourceDescription).ClassName,
              'Source Description');
  CheckEquals(TIdRTCPBye.ClassName,
              TIdRTCPPacket.RTCPType(RTCPGoodbye).ClassName,
              'Goodbye');
  CheckEquals(TIdRTCPApplicationDefined.ClassName,
              TIdRTCPPacket.RTCPType(RTCPApplicationDefined).ClassName,
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
  CheckEquals(TIdSDESCanonicalName.ClassName,
              TIdSrcDescChunkItem.ItemType(SDESCName).ClassName,
              'SDESCName');
  CheckEquals(TIdSDESUserName.ClassName,
              TIdSrcDescChunkItem.ItemType(SDESName).ClassName,
              'SDESName');
  CheckEquals(TIdSDESEmail.ClassName,
              TIdSrcDescChunkItem.ItemType(SDESEmail).ClassName,
              'SDESEmail');
  CheckEquals(TIdSDESPhone.ClassName,
              TIdSrcDescChunkItem.ItemType(SDESPhone).ClassName,
              'SDESPhone');
  CheckEquals(TIdSDESLocation.ClassName,
              TIdSrcDescChunkItem.ItemType(SDESLoc).ClassName,
              'SDESLoc');
  CheckEquals(TIdSDESTool.ClassName,
              TIdSrcDescChunkItem.ItemType(SDESTool).ClassName,
              'SDESTool');
  CheckEquals(TIdSDESNote.ClassName,
              TIdSrcDescChunkItem.ItemType(SDESNote).ClassName,
              'SDESNote');
  CheckEquals(TIdSDESPriv.ClassName,
              TIdSrcDescChunkItem.ItemType(SDESPriv).ClassName,
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

//* TRTCPPacketTestCase Published methods **************************************

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
                Ord(S.DataString[1]),
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
                Ord(S.DataString[1]),
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
                Ord(S.DataString[1]),
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

    Check(Length(S.DataString) > (7 + SizeOf(Word) + Length(Self.Bye.Reason)),
          'Too little output');

    CheckEquals(Self.Bye.ReasonLength shr 8,
                Ord(S.DataString[9]),
                'Reason length MSB');
    CheckEquals(Self.Bye.ReasonLength and $FF,
                Ord(S.DataString[10]),
                'Reason length LSB');
    CheckEquals(Self.Bye.Reason,
                Copy(S.DataString,
                     Length(S.DataString) - Length(Self.Bye.Reason) + 1,
                     Length(S.DataString)),
                'Reason');
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
  CheckEquals(4 + 4 + (2 + 5),
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
//* TIdMockEncoding                                                            *
//******************************************************************************
//* TIdMockEncoding Public methods *********************************************

function TIdMockEncoding.PayloadType: TIdRTPPayloadClass;
begin
  Result := TIdMockPayload;
end;

//******************************************************************************
//* TIdMockPayload                                                             *
//******************************************************************************
//* TIdMockPayload Public methods **********************************************

function TIdMockPayload.HasKnownLength: Boolean;
begin
  Result := fHasKnownLength;
end;

function TIdMockPayload.Length: Cardinal;
begin
  Result := fLength;
end;

procedure TIdMockPayload.SetHasKnownLength(const Yes: Boolean);
begin
  fHasKnownLength := Yes;
end;

procedure TIdMockPayload.SetLength(const Length: Cardinal);
begin
  fLength := Length;
end;

//******************************************************************************
//* TIdMockProfile                                                             *
//******************************************************************************
//* TIdMockProfile Public methods **********************************************

function TIdMockProfile.AllowsHeaderExtensions: Boolean;
begin
  Result := fAllowExtension;
end;

procedure TIdMockProfile.SetAllowExtension(const Allow: Boolean);
begin
  fAllowExtension := Allow;
end;

initialization
  RegisterTest('RTP', Suite);
end.
