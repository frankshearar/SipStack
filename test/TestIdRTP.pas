unit TestIdRTP;

interface

uses
  IdRTP, TestFramework, TestFrameworkRtp;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestEncodeAsStringCardinal;
    procedure TestEncodeAsStringWord;
    procedure TestHtoNL;
    procedure TestHtoNS;
    procedure TestNtoHL;
    procedure TestNtoHS;
    procedure TestReadCardinal;
    procedure TestReadRemainderOfStream;
    procedure TestReadRemainderOfStreamLong;
    procedure TestReadString;
    procedure TestReadWord;
    procedure TestWriteCardinal;
    procedure TestWriteWord;
  end;

  TestTIdRTPEncoding = class(TTestCase)
  private
    Encoding: TIdRTPEncoding;
  public
    procedure SetUp; override;
    procedure TearDown; override;
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
    procedure TestReadFrom;
    procedure TestPrintOn;
  end;

  TestTIdTelephoneEventPayload = class(TTestCase)
  private
    Packet: TIdTelephoneEventPayload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
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

  TestTIdRTPPacket = class(TTestRTP)
  private
    AVP:    TIdAudioVisualProfile;
    Packet: TIdRTPPacket;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
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
  end;

  TestTIdRTCPByePacket = class(TTestCase)
  private
    Packet: TIdRTCPByePacket;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPacketType;
    procedure TestPrintOn;
    procedure TestReadFrom;
    procedure TestReadFromHasPadding;
    procedure TestReadFromLength;
    procedure TestReadFromPacketType;
    procedure TestReadFromReason;
    procedure TestReadFromReasonLength;
    procedure TestReadFromSourceCount;
    procedure TestReadFromSources;
    procedure TestReadFromVersion;
  end;

implementation

uses
  Classes, IdRtpServer, IdSdp, SysUtils;

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
  Result.AddTest(TestTIdRTPPacket.Suite);
  Result.AddTest(TestTIdRTCPByePacket.Suite);
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
//* TestTIdRTPEncoding Public methods ******************************************

procedure TestTIdRTPEncoding.SetUp;
begin
  inherited SetUp;

  Self.Encoding := TIdRTPEncoding.Create('t140', 1000, '1');
end;

procedure TestTIdRTPEncoding.TearDown;
begin
  Self.Encoding.Free;

  inherited TearDown;
end;

//* TestTIdRTPEncoding Published methods ***************************************

procedure TestTIdRTPEncoding.TestAsString;
var
  Enc: TIdRTPEncoding;
begin
  CheckEquals('t140/1000/1',
              Self.Encoding.AsString,
              'AsString with Parameters');

  Enc := TIdRTPEncoding.Create('red', 8000);
  try
    CheckEquals('red/8000', Enc.AsString, 'AsString without Parameters');
  finally
    Enc.Free;
  end;
end;

procedure TestTIdRTPEncoding.TestCreate;
begin
  CheckEquals(1000,   Self.Encoding.ClockRate,  'ClockRate');
  CheckEquals('t140', Self.Encoding.Name,       'Name');
  CheckEquals('1',    Self.Encoding.Parameters, 'Parameters');
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
  Enc := TIdRTPEncoding.CreateEncoding('t140/1000/1');
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
  Enc: TIdRTPEncoding;
begin
  Enc := TIdRTPEncoding.Create(Self.Encoding);
  try
    Check(Enc.IsEqualTo(Self.Encoding), 'Properties not copied correctly');
  finally
    Enc.Free;
  end;
end;

procedure TestTIdRTPEncoding.TestClone;
var
  Enc: TIdRTPEncoding;
begin
  Enc := Self.Encoding.Clone;
  try
    CheckEquals(TIdRTPEncoding.ClassName,
                Enc.ClassName,
                'Incorrect type used for cloning');
  finally
    Enc.Free;
  end;
end;

procedure TestTIdRTPEncoding.TestIsNull;
begin
  Check(not Self.Encoding.IsNull, 'non-Null Encoding marked as null');
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

//******************************************************************************
//* TestTIdRTCPByePacket                                                       *
//******************************************************************************
//* TestTIdRTCPByePacket Public methods ****************************************

procedure TestTIdRTCPByePacket.SetUp;
begin
  inherited SetUp;

  Self.Packet := TIdRTCPByePacket.Create;
end;

procedure TestTIdRTCPByePacket.TearDown;
begin
  Self.Packet.Free;

  inherited TearDown;
end;

//* TestTIdRTCPByePacket Published methods *************************************

procedure TestTIdRTCPByePacket.TestPacketType;
begin
  CheckEquals(RTCPGoodbye, Self.Packet.PacketType, 'PacketType');
end;

procedure TestTIdRTCPByePacket.TestPrintOn;
begin
end;

procedure TestTIdRTCPByePacket.TestReadFrom;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$80 + Chr(RTCPGoodbye));
  try
    Self.Packet.ReadFrom(S);

    CheckEquals(2, Self.Packet.Version, 'Version');
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
    Self.Packet.ReadFrom(S);

    Check(not Self.Packet.HasPadding, 'HasPadding set');
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$20 + Chr(RTCPGoodbye));
  try
    Self.Packet.ReadFrom(S);

    Check(Self.Packet.HasPadding, 'HasPadding not set');
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
    Self.Packet.ReadFrom(S);

    CheckEquals($DEAD, Self.Packet.Length, 'Length');
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
                Self.Packet.PacketType,
                'PacketType');
  finally
    S.Free;
  end;
end;

procedure TestTIdRTCPByePacket.TestReadFromReason;
var
  S: TStringStream;
  Reason: String;
  ReasonLength: Word;
begin
  Reason := 'I''m already dead';
  ReasonLength := Length(Reason);

  S  := TStringStream.Create(#$01 + Chr(RTCPGoodbye) + #$00#$00
                           + #$CA#$FE#$BA#$BE
                           + #$00#$10
                           + Reason);
  try
    Self.Packet.ReadFrom(S);

    CheckEquals(ReasonLength,
                Self.Packet.ReasonLength,
                'ReasonLength');
    CheckEquals(Reason,
                Self.Packet.Reason,
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
    Self.Packet.ReadFrom(S);

    CheckEquals($F00D,
                Self.Packet.ReasonLength,
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
      Self.Packet.ReadFrom(S);

      CheckEquals(I, Self.Packet.SourceCount, 'SourceCount ' + IntToStr(I));
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
      Self.Packet.ReadFrom(S);
      CheckEquals(I, Self.Packet.SourceCount, 'SourceCount');

      for J := 0 to Self.Packet.SourceCount - 1 do
        CheckEquals(IDs[J],
                    Self.Packet.Sources[J],
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
      Self.Packet.ReadFrom(S);
      CheckEquals(I, Self.Packet.Version, 'Version ' + IntToStr(I));
    finally
      S.Free;
    end;
  end;
end;

initialization
  RegisterTest('RTP', Suite);
end.
