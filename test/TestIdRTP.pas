unit TestIdRTP;

interface

uses
  IdRTP, TestFramework, TestFrameworkRtp;

type
  TestFunctions = class(TTestCase)
  published
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

  TestTIdRTPT140Encoding = class(TTestCaseEncoding)
  private
    function EncodingType: TIdRTPEncodingClass; override;
    function EncodingName: String; override;
    function EncodingClockRate: Cardinal; override;
  end;

  TestTIdRTPTelephoneEventEncoding = class(TTestCaseEncoding)
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
    Packet: TIdT140Payload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPrintOn;
    procedure TestReadFrom;
  end;

  TestTIdTelephoneEventPayload = class(TTestCase)
  private
    Packet: TIdTelephoneEventPayload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
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
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsRTCP;
    procedure TestIsRTP;
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
    procedure TestReadFromPrintOnInverse;
    procedure TestRealLength;
  end;

  TestTIdRTCPPacket = class(TTestCase)
  published
    procedure TestRTCPType;
  end;

  TRTCPPacketTestCase = class(TTestCase)
  protected
    Packet: TIdRTCPPacket;

    function PacketType: TIdRTCPPacketClass; virtual; abstract;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsRTCP;
    procedure TestIsRTP;
    procedure TestPrintOnHasPadding;
    procedure TestPrintOnSyncSrcId;
    procedure TestPrintOnVersion;
    procedure TestPrintOnWithPadding;
  end;

  TestTIdRTCPSenderReportPacket = class(TRTCPPacketTestCase)
  private
    SenderReport: TIdRTCPSenderReportPacket;

    function  MultipleReportBlockSenderReport: String;
    procedure ReadFromSampleSenderReport;
  protected
    function PacketType: TIdRTCPPacketClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestPacketType;
    procedure TestPrintOnSyncSrcId;
    procedure TestPrintOnNTPTimestamp;
    procedure TestPrintOnOctetCount;
    procedure TestPrintOnPacketCount;
    procedure TestPrintOnPacketType;
    procedure TestPrintOnReceptionReportCount;
    procedure TestPrintOnReportBlocks;
    procedure TestPrintOnRTPTimestamp;
    procedure TestPrintOnWord;
    procedure TestReadFromLength;
    procedure TestReadFromNTPTimestamp;
    procedure TestReadFromOctetCount;
    procedure TestReadFromPacketCount;
    procedure TestReadFromPadding;
    procedure TestReadFromReceptionReportCount;
    procedure TestReadFromReport;
    procedure TestReadFromReports;
    procedure TestReadFromRTPTimestamp;
    procedure TestReadFromSyncSrcID;
    procedure TestReadFromVersion;
    procedure TestRealLength;
    procedure TestReportAt;
  end;

  TestTIdRTCPByePacket = class(TRTCPPacketTestCase)
  private
    Bye: TIdRTCPByePacket;
  protected
    function PacketType: TIdRTCPPacketClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestPacketType;
    procedure TestPrintOnLength;
    procedure TestPrintOnMultipleSources;
    procedure TestPrintOnPacketType;
    procedure TestPrintOnReason;
    procedure TestPrintOnSyncSrcId;
    procedure TestReadFrom;
    procedure TestReadFromHasPadding;
    procedure TestReadFromLength;
    procedure TestReadFromPacketType;
    procedure TestReadFromReason;
    procedure TestReadFromReasonLength;
    procedure TestReadFromSourceCount;
    procedure TestReadFromSources;
    procedure TestReadFromVersion;
    procedure TestRealLength;
  end;

  TestTIdRTCPApplicationDefinedPacket = class(TRTCPPacketTestCase)
  private
    AppDef: TIdRTCPApplicationDefinedPacket;
  protected
    function PacketType: TIdRTCPPacketClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestPacketType;
    procedure TestPrintOnData;
    procedure TestPrintOnLength;
    procedure TestPrintOnName;
    procedure TestPrintOnPacketType;
    procedure TestReadFromData;
    procedure TestReadFromHasPadding;
    procedure TestReadFromLength;
    procedure TestReadFromName;
    procedure TestReadFromPacketType;
    procedure TestReadFromSyncSrcId;
    procedure TestReadFromVersion;
    procedure TestRealLength;
    procedure TestSetData;
    procedure TestSetName;
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

// Most of the values are thumb-sucked.
// We calculate Length by hand, and RFC3550 gives us the value for packet type
const
  SampleReportBlock  = #$DE#$AD#$BE#$EF  // SSRC_1
                     + #$0F#$00#$FE#$ED  // Fraction lost, Cumulative packet loss
                     + #$10#$11#$12#$13  // highest received sequence no
                     + #$00#$FE#$DF#$ED  // interarrival jitter
                     + #$EA#$7E#$A7#$EA  // last SR
                     + #$00#$00#$FF#$EF; // delay since last SR
  SampleSenderReport = #$81#$C8#$00#$0C  // version, padding, packet type length
                     + #$DE#$CA#$FB#$AD  // SSRC
                     + #$A1#$A2#$A3#$A4  // NTP timestamp
                     + #$A5#$A6#$A7#$A8  // NTP timestamp
                     + #$A9#$AA#$AB#$AC  // RTP timestamp
                     + #$00#$00#$0F#$ED  // sender's packet count
                     + #$00#$00#$F0#$0D  // sender's octet count
                     + SampleReportBlock;

implementation

uses
  Classes, DateUtils, IdRtpServer, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTP unit tests');

  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdRTPEncoding.Suite);
  Result.AddTest(TestTIdRTPNullEncoding.Suite);
  Result.AddTest(TestTIdRTPT140Encoding.Suite);
  Result.AddTest(TestTIdRTPTelephoneEventEncoding.Suite);
  Result.AddTest(TestTIdTelephoneEventPayload.Suite);
  Result.AddTest(TestTIdT140Payload.Suite);
  Result.AddTest(TestTIdRTPProfile.Suite);
  Result.AddTest(TestTIdAudioVisualProfile.Suite);
  Result.AddTest(TestTIdRTPHeaderExtension.Suite);
  Result.AddTest(TestTIdRTCPReportBlock.Suite);
  Result.AddTest(TestTIdRTPBasePacket.Suite);
  Result.AddTest(TestTIdRTPPacket.Suite);
  Result.AddTest(TestTIdRTCPSenderReportPacket.Suite);
  Result.AddTest(TestTIdRTCPByePacket.Suite);
  Result.AddTest(TestTIdRTCPApplicationDefinedPacket.Suite);
  Result.AddTest(TestTIdRTPPacketBuffer.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

function ShowEncoded(S: String): String;
var
  I: Integer;
begin
  Result := '';

  for I := 1 to Length(S) do
    Result := Result + '#$' + IntToHex(Ord(S[I]), 2);
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
              DateTimeToNTPSeconds(StrToDateTime('05/01/1900 01:02:01')),
              '1900/01/05 01:02:01');

  ExpectedTime := MultiplyCardinal(AprTwelve2002, SecsPerDay) + 14*3600 + 59*60 + 59;
  ReceivedTime := DateTimeToNTPSeconds(StrToDateTime('12/04/2002 14:59:59.999'));
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

procedure TestFunctions.TestReadCardinal;
var
  S: TStringStream;
  C: Cardinal;
begin
  S := TStringStream.Create(#$11#$22#$33#$44);
  try
    C := ReadCardinal(S);
    CheckEquals($11223344, C, 'Cardinal incorrectly read - check byte order');
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
  S:   String;
  Src: TStringStream;
begin
  Src := TStringStream.Create('onetwothree');
  try
    S := ReadString(Src, 3);
    CheckEquals('one', S, 'First read');
    S := ReadString(Src, 3);
    CheckEquals('two', S, 'Second read');
    S := ReadString(Src, 1000);
    CheckEquals('three', S, 'Third read');
  finally
    Src.Free;
  end;
end;

procedure TestFunctions.TestReadWord;
var
  S: TStringStream;
  W: Word;
begin
  S := TStringStream.Create(#$11#$22);
  try
    W := ReadWord(S);
    CheckEquals($1122, W, 'Word incorrectly read - check byte order');
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
    CheckEquals(TIdRTPT140Encoding.ClassName,
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
    CheckEquals(TIdRTPTelephoneEventEncoding.ClassName,
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
//* TestTIdRTPT140Encoding                                                     *
//******************************************************************************
//* TestTIdRTPT140Encoding Private methods *************************************

function TestTIdRTPT140Encoding.EncodingType: TIdRTPEncodingClass;
begin
  Result := TIdRTPT140Encoding;
end;

function TestTIdRTPT140Encoding.EncodingName: String;
begin
  Result := T140Encoding;
end;

function TestTIdRTPT140Encoding.EncodingClockRate: Cardinal;
begin
  Result := T140ClockRate;
end;

//******************************************************************************
//* TestTIdRTPTelephoneEventEncoding                                           *
//******************************************************************************
//* TestTIdRTPTelephoneEventEncoding Private methods ***************************

function TestTIdRTPTelephoneEventEncoding.EncodingType: TIdRTPEncodingClass;
begin
  Result := TIdRTPTelephoneEventEncoding;
end;

function TestTIdRTPTelephoneEventEncoding.EncodingName: String;
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

  Self.Packet := TIdT140Payload.Create;
end;

procedure TestTIdT140Payload.TearDown;
begin
  Self.Packet.Free;

  inherited TearDown;
end;

//* TestTIdT140Payload Published methods ***************************************

procedure TestTIdT140Payload.TestPrintOn;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Packet.BlockCount := $BEEF;
    Self.Packet.Block      := 'fooing the bar';

    Self.Packet.PrintOn(S);

    CheckEquals(#$BE#$EF'fooing the bar',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdT140Payload.TestReadFrom;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$BE#$EF'fooing the bar');
  try
    Self.Packet.ReadFrom(S);

    CheckEquals($BEEF,            Self.Packet.BlockCount, 'BlockCount');
    CheckEquals('fooing the bar', Self.Packet.Block,      'Block');
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

  Self.Packet := TIdTelephoneEventPayload.Create;
end;

procedure TestTIdTelephoneEventPayload.TearDown;
begin
  Self.Packet.Free;

  inherited TearDown;
end;

//* TestTIdTelephoneEventPayload Published methods *****************************

procedure TestTIdTelephoneEventPayload.TestPrintOnDuration;
const
  SampleDurations: array[0..5] of Word = ($f00d, $beef, $cafe, $deaf, $deca, $fbad);
var
  I: Integer;
  S: TStringStream;
begin
  for I := Low(SampleDurations) to High(SampleDurations) do begin
    S := TStringStream.Create('');
    try
      Self.Packet.Duration := SampleDurations[I];
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
  S := TStringStream.Create(#$BE#$EF);
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
  S := TStringStream.Create(#$00#$80);
  try
    Self.Packet.ReadFrom(S);
    Check(Self.Packet.IsEnd, 'IsEnd not set');
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$00#$00);
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
  S := TStringStream.Create(#$00#$40);
  try
    Self.Packet.ReadFrom(S);
    Check(Self.Packet.ReservedBit, 'ReservedBit not set');
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$00#$00);
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
    S := TStringStream.Create(#$00 + Chr($C0 or V));
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
  Data := TStringStream.Create('');
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
begin
  inherited SetUp;

  Self.AVP := TIdAudioVisualProfile.Create;
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
  I: Integer;
  P: TIdRTPPacket;
  S: TStream;
begin
  for I := Low(TIdRTPCsrcCount) to High(TIdRTPCsrcCount) do begin
    S := TStringStream.Create(Chr(I));
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
  S := TStringStream.Create(#$10#$00#$00#$00
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

  S := TStringStream.Create(#$00);
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
  S := TStringStream.Create(#$20);
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

  S := TStringStream.Create(#$00);
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
  S := TStringStream.Create(#$00#$80);
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

  S := TStringStream.Create(#$00#$00);
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
    S := TStringStream.Create(#$00 + Chr(I));
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
    S := TStringStream.Create(#$00#$00 + EncodeAsString(I));
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
                            + EncodeAsString(Timestamp));
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
    S := TStringStream.Create(Chr(I shl 6));
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

//******************************************************************************
//* TestTIdRTCPPacket                                                          *
//******************************************************************************
//* TestTIdRTCPPacket Public methods *******************************************

procedure TestTIdRTCPPacket.TestRTCPType;
var
  I: Byte;
begin
  CheckEquals(TIdRTCPSenderReportPacket.ClassName,
              TIdRTCPPacket.RTCPType(RTCPSenderReport).ClassName,
              'Sender Report');
  CheckEquals(TIdRTCPReceiverReportPacket.ClassName,
              TIdRTCPPacket.RTCPType(RTCPReceiverReport).ClassName,
              'Receiver Report');
  CheckEquals(TIdRTCPSourceDescriptionPacket.ClassName,
              TIdRTCPPacket.RTCPType(RTCPSourceDescription).ClassName,
              'Source Description');
  CheckEquals(TIdRTCPByePacket.ClassName,
              TIdRTCPPacket.RTCPType(RTCPGoodbye).ClassName,
              'Goodbye');
  CheckEquals(TIdRTCPApplicationDefinedPacket.ClassName,
              TIdRTCPPacket.RTCPType(RTCPApplicationDefined).ClassName,
              'Application Defined');

  for I := Low(Byte) to RTCPSenderReport - 1 do
    Check(nil = TIdRTCPPacket.RTCPType(I), 'Packet Type ' + IntToStr(I));

  for I := RTCPApplicationDefined + 1 to High(Byte) do
    Check(nil = TIdRTCPPacket.RTCPType(I), 'Packet Type ' + IntToStr(I));
end;

//******************************************************************************
//* TRTCPPacketTestCase                                                        *
//******************************************************************************
//* TRTCPPacketTestCase Public methods *****************************************

procedure TRTCPPacketTestCase.SetUp;
begin
  inherited SetUp;

  Self.Packet := Self.PacketType.Create;
end;

procedure TRTCPPacketTestCase.TearDown;
begin
  Self.Packet.Free;

  inherited TearDown;
end;

//* TRTCPPacketTestCase Published methods **************************************

procedure TRTCPPacketTestCase.TestIsRTCP;
begin
  Check(Self.Packet.IsRTCP, 'IsRTCP');
end;

procedure TRTCPPacketTestCase.TestIsRTP;
begin
  Check(not Self.Packet.IsRTP, 'IsRTP');
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

procedure TRTCPPacketTestCase.TestPrintOnSyncSrcId;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Packet.SyncSrcID := $decafbad;
    Self.Packet.PrintOn(S);

    Check(Length(S.DataString) > 11, 'Too little output');
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

//******************************************************************************
//* TestTIdRTCPSenderReportPacket                                              *
//******************************************************************************
//* TestTIdRTCPSenderReportPacket Public methods *******************************

procedure TestTIdRTCPSenderReportPacket.SetUp;
begin
  inherited SetUp;

  Self.SenderReport := Self.Packet as TIdRTCPSenderReportPacket;
end;

//* TestTIdRTCPSenderReportPacket Protected methods ****************************

function TestTIdRTCPSenderReportPacket.PacketType: TIdRTCPPacketClass;
begin
  Result := TIdRTCPSenderReportPacket;
end;

//* TestTIdRTCPSenderReportPacket Private methods ******************************

function TestTIdRTCPSenderReportPacket.MultipleReportBlockSenderReport: String;
begin
  Result     := SampleSenderReport + SampleReportBlock;
  Result[1]  := Chr($82); // 2 reports
  Result[60] := Chr(Ord(Result[60]) + 1); // Cumulative packet loss of 2nd report
end;

procedure TestTIdRTCPSenderReportPacket.ReadFromSampleSenderReport;
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

//* TestTIdRTCPSenderReportPacket Published methods ****************************

procedure TestTIdRTCPSenderReportPacket.TestPacketType;
begin
  CheckEquals(RTCPSenderReport, Self.Packet.PacketType, 'PacketType');
end;

procedure TestTIdRTCPSenderReportPacket.TestPrintOnSyncSrcId;
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

procedure TestTIdRTCPSenderReportPacket.TestPrintOnNTPTimestamp;
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

procedure TestTIdRTCPSenderReportPacket.TestPrintOnOctetCount;
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

procedure TestTIdRTCPSenderReportPacket.TestPrintOnPacketCount;
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

procedure TestTIdRTCPSenderReportPacket.TestPrintOnPacketType;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SenderReport.PrintOn(S);

    Check(Length(S.DataString) > 2, 'Too little output');
    CheckEquals(RTCPSenderReport, Ord(S.DataString[2]), 'Packet type');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSenderReportPacket.TestPrintOnReceptionReportCount;
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

procedure TestTIdRTCPSenderReportPacket.TestPrintOnReportBlocks;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SenderReport.ReceptionReportCount           := 1;
    Self.SenderReport.ReportAt(0).SyncSrcID          := $deadbeef;
    Self.SenderReport.ReportAt(0).FractionLost       := $0f;
    Self.SenderReport.ReportAt(0).CumulativeLoss     := $feed;
    Self.SenderReport.ReportAt(0).HighestSeqNo       := $10111213;
    Self.SenderReport.ReportAt(0).InterArrivalJitter := $00fedfed;
    Self.SenderReport.ReportAt(0).LastSenderReport   := $ea7ea7ea;
    Self.SenderReport.ReportAt(0).DelaySinceLastSR   := $0000ffef;

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

procedure TestTIdRTCPSenderReportPacket.TestPrintOnRTPTimestamp;
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

procedure TestTIdRTCPSenderReportPacket.TestPrintOnWord;
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

procedure TestTIdRTCPSenderReportPacket.TestReadFromLength;
begin
  Self.ReadFromSampleSenderReport;
  CheckEquals(12, Self.SenderReport.Length, 'Length');
end;

procedure TestTIdRTCPSenderReportPacket.TestReadFromNTPTimestamp;
begin
  Self.ReadFromSampleSenderReport;
  CheckEquals(IntToHex($a1a2a3a4, 8),
              IntToHex(Self.SenderReport.NTPTimestamp.IntegerPart, 8),
              'NTP timestamp, Integer');
  CheckEquals(IntToHex($a5a6a7a8, 8),
              IntToHex(Self.SenderReport.NTPTimestamp.FractionalPart, 8),
              'NTP timestamp, Integer');
end;

procedure TestTIdRTCPSenderReportPacket.TestReadFromOctetCount;
begin
  Self.ReadFromSampleSenderReport;
  CheckEquals(IntToHex($0000f00d, 8),
              IntToHex(Self.SenderReport.OctetCount, 8),
              'Octet count');
end;

procedure TestTIdRTCPSenderReportPacket.TestReadFromPacketCount;
begin
  Self.ReadFromSampleSenderReport;
  CheckEquals(IntToHex($00000fed, 8),
              IntToHex(Self.SenderReport.PacketCount, 8),
              'Packet count');
end;

procedure TestTIdRTCPSenderReportPacket.TestReadFromPadding;
var
  PaddedStr: String;
  S:         TStringStream;
begin
  Self.ReadFromSampleSenderReport;
  Check(not Self.SenderReport.HasPadding, 'No padding');

  PaddedStr := SampleSenderReport;
  PaddedStr[1] := Chr($20 or Ord(PaddedStr[1]));
  S := TStringStream.Create(PaddedStr);
  try
    Self.SenderReport.ReadFrom(S);
    Check(Self.SenderReport.HasPadding, 'Has padding');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPSenderReportPacket.TestReadFromReceptionReportCount;
begin
  Self.ReadFromSampleSenderReport;
  CheckEquals(1,
              Self.SenderReport.ReceptionReportCount,
              'Reception report count');
end;

procedure TestTIdRTCPSenderReportPacket.TestReadFromReport;
begin
  Self.ReadFromSampleSenderReport;

  CheckEquals(IntToHex($deadbeef, 8),
              IntToHex(Self.SenderReport.ReportAt(0).SyncSrcID, 8),
              'SSRC_1');
  CheckEquals($0f,
              Self.SenderReport.ReportAt(0).FractionLost,
              'Fraction lost');
  CheckEquals($feed,
              Self.SenderReport.ReportAt(0).CumulativeLoss,
              'Cumulative packet loss');
  CheckEquals(IntToHex($10111213, 8),
              IntToHex(Self.SenderReport.ReportAt(0).HighestSeqNo, 8),
              'Highest received sequence no');
  CheckEquals(IntToHex($00fedfed, 8),
              IntToHex(Self.SenderReport.ReportAt(0).InterArrivalJitter, 8),
              'Interarrival jitter');
  CheckEquals(IntToHex($ea7ea7ea, 8),
              IntToHex(Self.SenderReport.ReportAt(0).LastSenderReport, 8),
              'Last SR');
  CheckEquals(IntToHex($0000ffef, 8),
              IntToHex(Self.SenderReport.ReportAt(0).DelaySinceLastSR, 8),
              'Delay since last SR');
end;

procedure TestTIdRTCPSenderReportPacket.TestReadFromReports;
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

procedure TestTIdRTCPSenderReportPacket.TestReadFromRTPTimestamp;
begin
  Self.ReadFromSampleSenderReport;
  CheckEquals(IntToHex($a9aaabac, 8),
              IntToHex(Self.SenderReport.RTPTimestamp, 8),
              'RTP timestamp');
end;

procedure TestTIdRTCPSenderReportPacket.TestReadFromSyncSrcID;
begin
  Self.ReadFromSampleSenderReport;
  CheckEquals(IntToHex($decafbad, 8),
              IntToHex(Self.SenderReport.SyncSrcID, 8),
              'SSRC');
end;

procedure TestTIdRTCPSenderReportPacket.TestReadFromVersion;
begin
  Self.ReadFromSampleSenderReport;
  CheckEquals(RFC3550Version, Self.SenderReport.Version, 'Version');
end;

procedure TestTIdRTCPSenderReportPacket.TestRealLength;
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

procedure TestTIdRTCPSenderReportPacket.TestReportAt;
var
  I: Integer;
  S: TStringStream;
begin
  try
    Self.SenderReport.ReportAt(0);
    Fail('Failed to raise error accessing first element of an empty list');
  except
    on EListError do;
  end;

  try
    Self.SenderReport.ReportAt(-1);
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
                Self.SenderReport.ReportAt(I).CumulativeLoss,
                'Wrong report off index ' + IntToStr(I));

  try
    Self.SenderReport.ReportAt(3);
    Fail('Failed to raise error accessing out-of-range index');
  except
    on EListError do;
  end;
end;

//******************************************************************************
//* TestTIdRTCPByePacket                                                       *
//******************************************************************************
//* TestTIdRTCPByePacket Public methods ****************************************

procedure TestTIdRTCPByePacket.SetUp;
begin
  inherited SetUp;

  Self.Bye := Self.Packet as TIdRTCPByePacket;
end;

//* TestTIdRTCPByePacket Protected methods *************************************

function TestTIdRTCPByePacket.PacketType: TIdRTCPPacketClass;
begin
  Result := TIdRTCPByePacket;
end;

//* TestTIdRTCPByePacket Published methods *************************************

procedure TestTIdRTCPByePacket.TestPacketType;
begin
  CheckEquals(RTCPGoodbye, Self.Packet.PacketType, 'PacketType');
end;

procedure TestTIdRTCPByePacket.TestPrintOnLength;
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

procedure TestTIdRTCPByePacket.TestPrintOnMultipleSources;
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

procedure TestTIdRTCPByePacket.TestPrintOnPacketType;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Bye.PrintOn(S);

    Check(Length(S.DataString) > 7, 'Too little output');
    CheckEquals(RTCPGoodbye, Ord(S.DataString[2]), 'Packet type');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPByePacket.TestPrintOnReason;
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

procedure TestTIdRTCPByePacket.TestPrintOnSyncSrcId;
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

procedure TestTIdRTCPByePacket.TestReadFrom;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$80 + Chr(RTCPGoodbye));
  try
    Self.Bye.ReadFrom(S);

    CheckEquals(2, Self.Bye.Version, 'Version');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPByePacket.TestReadFromHasPadding;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$00 + Chr(RTCPGoodbye));
  try
    Self.Bye.ReadFrom(S);

    Check(not Self.Bye.HasPadding, 'HasPadding set');
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$20 + Chr(RTCPGoodbye));
  try
    Self.Bye.ReadFrom(S);

    Check(Self.Bye.HasPadding, 'HasPadding not set');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPByePacket.TestReadFromLength;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$00 + Chr(RTCPGoodbye) + #$DE#$AD);
  try
    Self.Bye.ReadFrom(S);

    CheckEquals($DEAD, Self.Bye.Length, 'Length');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPByePacket.TestReadFromPacketType;
var
  S: TStringStream;
begin
  S  := TStringStream.Create(#$00 + Chr(RTCPGoodbye));
  try
    Self.Packet.ReadFrom(S);

    CheckEquals(RTCPGoodbye,
                Self.Bye.PacketType,
                'PacketType');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPByePacket.TestReadFromReason;
var
  Reason:       String;
  ReasonLength: Word;
  S:            TStringStream;
begin
  Reason := 'I''m already dead';
  ReasonLength := Length(Reason);

  S  := TStringStream.Create(#$01 + Chr(RTCPGoodbye) + #$00#$00
                           + #$CA#$FE#$BA#$BE
                           + Chr(ReasonLength shr 8) + Chr(ReasonLength and $FF)
                           + Reason);
  try
    Self.Bye.ReadFrom(S);

    CheckEquals(ReasonLength,
                Self.Bye.ReasonLength,
                'ReasonLength');
    CheckEquals(Reason,
                Self.Bye.Reason,
                'Reason');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPByePacket.TestReadFromReasonLength;
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

procedure TestTIdRTCPByePacket.TestReadFromSourceCount;
var
  I: TIdRTCPSourceCount;
  S: TStringStream;
begin
  for I := Low(TIdRTCPSourceCount) to High(TIdRTCPSourceCount) do begin
    S  := TStringStream.Create(Chr(I) + Chr(RTCPGoodbye));
    try
      Self.Bye.ReadFrom(S);

      CheckEquals(I, Self.Bye.SourceCount, 'SourceCount ' + IntToStr(I));
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTCPByePacket.TestReadFromSources;
var
  I:      TIdRTCPSourceCount;
  IDs:    TIdCardinalArray;
  J:      Integer;
  Packet: String;
  S:      TStream;
begin
  for I := Low(TIdRTCPSourceCount) to High(TIdRTCPSourceCount) do begin
    SetLength(IDs, I);
    Packet := Chr(I) + Chr(RTCPGoodBye) + #$00#$00;

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

procedure TestTIdRTCPByePacket.TestReadFromVersion;
var
  I: TIdRTPVersion;
  S: TStream;
begin
  for I := Low(TIdRTPVersion) to High(TIdRTPVersion) do begin
    S := TStringStream.Create(Chr(I shl 6) + Chr(RTCPGoodbye));
    try
      Self.Bye.ReadFrom(S);
      CheckEquals(I, Self.Bye.Version, 'Version ' + IntToStr(I));
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTCPByePacket.TestRealLength;
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
//* TestTIdRTCPApplicationDefinedPacket                                        *
//******************************************************************************
//* TestTIdRTCPApplicationDefinedPacket Public methods *************************

procedure TestTIdRTCPApplicationDefinedPacket.SetUp;
begin
  inherited SetUp;

  Self.AppDef := Self.Packet as TIdRTCPApplicationDefinedPacket;
end;

//* TestTIdRTCPApplicationDefinedPacket Protected methods **********************

function TestTIdRTCPApplicationDefinedPacket.PacketType: TIdRTCPPacketClass;
begin
  Result := TIdRTCPApplicationDefinedPacket;
end;

//* TestTIdRTCPApplicationDefinedPacket Published methods **********************

procedure TestTIdRTCPApplicationDefinedPacket.TestPacketType;
begin
  CheckEquals(RTCPApplicationDefined, Self.Packet.PacketType, 'PacketType');
end;

procedure TestTIdRTCPApplicationDefinedPacket.TestPrintOnData;
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

procedure TestTIdRTCPApplicationDefinedPacket.TestPrintOnLength;
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

procedure TestTIdRTCPApplicationDefinedPacket.TestPrintOnName;
begin
end;

procedure TestTIdRTCPApplicationDefinedPacket.TestPrintOnPacketType;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.AppDef.PrintOn(S);

    Check(Length(S.DataString) > 11, 'Too little output');
    CheckEquals(RTCPApplicationDefined, Ord(S.DataString[2]), 'Packet type');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPApplicationDefinedPacket.TestReadFromData;
var
  S: TStringStream;
begin
  S  := TStringStream.Create(#$00 + Chr(RTCPApplicationDefined) + #$00#$28
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
    Self.Packet.ReadFrom(S);

    CheckEquals('toomanycooksspoilthebroth'#0#0#0, Self.AppDef.Data, 'Data');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPApplicationDefinedPacket.TestReadFromHasPadding;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$00 + Chr(RTCPApplicationDefined));
  try
    Self.AppDef.ReadFrom(S);

    Check(not Self.AppDef.HasPadding, 'HasPadding set');
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$20 + Chr(RTCPApplicationDefined));
  try
    Self.AppDef.ReadFrom(S);

    Check(Self.AppDef.HasPadding, 'HasPadding not set');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPApplicationDefinedPacket.TestReadFromLength;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$00 + Chr(RTCPApplicationDefined) + #$DE#$AD);
  try
    Self.AppDef.ReadFrom(S);

    CheckEquals(IntToHex($dead, 4),
                IntToHex(Self.AppDef.Length, 4),
                'Length');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPApplicationDefinedPacket.TestReadFromName;
var
  S: TStringStream;
begin
  S  := TStringStream.Create(#$00 + Chr(RTCPApplicationDefined) + #$00#$0C
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

procedure TestTIdRTCPApplicationDefinedPacket.TestReadFromPacketType;
var
  S: TStringStream;
begin
  S  := TStringStream.Create(#$00 + Chr(RTCPApplicationDefined));
  try
    Self.Packet.ReadFrom(S);

    CheckEquals(RTCPApplicationDefined,
                Self.AppDef.PacketType,
                'PacketType');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPApplicationDefinedPacket.TestReadFromSyncSrcId;
var
  S: TStringStream;
begin
  S  := TStringStream.Create(#$00 + Chr(RTCPApplicationDefined) + #$00#$0C
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

procedure TestTIdRTCPApplicationDefinedPacket.TestReadFromVersion;
var
  I: TIdRTPVersion;
  S: TStream;
begin
  for I := Low(TIdRTPVersion) to High(TIdRTPVersion) do begin
    S := TStringStream.Create(Chr(I shl 6) + Chr(RTCPApplicationDefined));
    try
      Self.AppDef.ReadFrom(S);
      CheckEquals(I, Self.AppDef.Version, 'Version ' + IntToStr(I));
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTCPApplicationDefinedPacket.TestRealLength;
begin
  // Name is ALWAYS present, and always has a multiple of 4 octets
  CheckEquals(8 + 4, Self.AppDef.RealLength, 'Minimum');

  Self.AppDef.Name := 'foo';
  Self.AppDef.Data := '';
  CheckEquals(8 + Length(Self.AppDef.Name),
              Self.AppDef.RealLength,
              'RealLength with Name and no Data');

  Self.AppDef.Name := '';
  Self.AppDef.Data := 'bar';
  CheckEquals(12 + Length(Self.AppDef.Data),
              Self.AppDef.RealLength,
              'RealLength with Data and no Name; only of course we always have '
            + 'a Name');

  Self.AppDef.Name := 'foobaring';
  Self.AppDef.Data := 'bar';
  CheckEquals(8 + Length(Self.AppDef.Name) + Length(Self.AppDef.Data),
              Self.AppDef.RealLength,
              'RealLength with Name and Data');
end;

procedure TestTIdRTCPApplicationDefinedPacket.TestSetData;
begin
  Self.AppDef.Data := '';
  CheckEquals('', Self.AppDef.Data, 'No data');

  Self.AppDef.Data := '1';
  CheckEquals('1'#0#0#0, Self.AppDef.Data, '1 byte');

  Self.AppDef.Data := '1234';
  CheckEquals('1234', Self.AppDef.Data, '4 bytes');

  Self.AppDef.Data := '123456';
  CheckEquals('123456'#0#0, Self.AppDef.Data, '6 bytes');
end;

procedure TestTIdRTCPApplicationDefinedPacket.TestSetName;
begin
  Self.AppDef.Name := '12345678';
  CheckEquals('1234', Self.AppDef.Name, 'Long name');

  Self.AppDef.Name := '12';
  CheckEquals('12'#0#0, Self.AppDef.Name, 'Short name');

  Self.AppDef.Name := '1234';
  CheckEquals('1234', Self.AppDef.Name, 'Just Right name');

  Self.AppDef.Name := '';
  CheckEquals(#0#0#0#0, Self.AppDef.Name, 'Null name');
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

initialization
  RegisterTest('RTP', Suite);
end.
