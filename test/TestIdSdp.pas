unit TestIdSdp;

interface

uses
  Classes, IdRTP, IdRTPServer, IdSdp, IdSocketHandle, TestFramework,
  TestFrameworkEx, TestFrameworkRtp;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestAddressTypeToStr;
    procedure TestBandwidthTypeToStr;
    procedure TestKeyTypeToStr;
    procedure TestMediaTypeToStr;
    procedure TestStrToAddressType;
    procedure TestStrToBandwidthType;
    procedure TestStrToKeyType;
    procedure TestStrToMediaType;
  end;

  TestTIdSdpAttribute = class(TTestCase)
  private
    A: TIdSdpAttribute;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateAttribute;
    procedure TestClone;
    procedure TestIsRTPMap;
    procedure TestPrintOnNoValue;
    procedure TestPrintOnWithValue;
  end;

  TestTIdSdpRTPMapAttribute = class(TTestCase)
  private
    A: TIdSdpRTPMapAttribute;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClone;
    procedure TestGetName;
    procedure TestIsRTPMap;
    procedure TestSetValue;
    procedure TestSetValueWithParameters;
  end;

  TestTIdSdpBandwidth = class(TTestCase)
  private
    B: TIdSdpBandwidth;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPrintOn;
  end;

  TestTIdSdpConnection = class(TTestCase)
  private
    C: TIdSdpConnection;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestPrintOnMultiCast;
    procedure TestPrintOnMultiCastWithNumberNoTtl;
    procedure TestPrintOnMultiCastWithTtl;
    procedure TestPrintOnMultiCastWithTtlAndNumber;
    procedure TestPrintOnUnicast;
  end;

  TestTIdSdpKey = class(TTestCase)
  private
    K: TIdSdpKey;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPrintOnJustMethod;
    procedure TestPrintOnMethodPlusValue;
    procedure TestPrintOnPromptWithKeyData;
    procedure TestPrintOnUriWithNoKeyData;
  end;

  TestTIdSdpMediaDescription = class(TTestCase)
  private
    M: TIdSdpMediaDescription;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAttributeAndCount;
    procedure TestAddFormatAndFormatCount;
    procedure TestAttributeAt;
    procedure TestFormatClear;
    procedure TestHasFormat;
    procedure TestGetFormat;
    procedure TestInitialState;
    procedure TestPrintOnBasic;
    procedure TestPrintOnFull;
    procedure TestPrintOnWithPortCount;
  end;

  TestTIdSdpOrigin = class(TTestCase)
  private
    O: TIdSdpOrigin;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPrintOn;
  end;

  TestTIdSdpTime = class(TTestCase)
  private
    T: TIdSdpTime;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPrintOn;
    procedure TestPrintOnWithRepeats;
    procedure TestPrintOnWithZoneAdjustments;
  end;

  TestTIdSdpAttributes = class(TTestCase)
  private
    A: TIdSdpAttributes;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAddUsingString;
    procedure TestClear;
    procedure TestContains;
    procedure TestPrintOn;
  end;

  TestTIdSdpRTPMapAttributes = class(TTestCase)
  private
    A: TIdSdpRTPMapAttributes;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestClear;
    procedure TestContains;
    procedure TestPrintOn;
  end;

  TestTIdSdpBandwidths = class(TTestCase)
  private
    B: TIdSdpBandwidths;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestClear;
    procedure TestContains;
  end;

  TestTIdSdpConnections = class(TTestCase)
  private
    C: TIdSdpConnections;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAddMultipleConnections;
    procedure TestClear;
    procedure TestContains;
  end;

  TestTIdSdpMediaDescriptions = class(TTestCase)
  private
    M: TIdSdpMediaDescriptions;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAllDescriptionsHaveConnections;
    procedure TestClear;
    procedure TestContains;
  end;

  TestTIdSdpRepeats = class(TTestCase)
  private
    R: TIdSdpRepeats;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestClear;
    procedure TestContains;
    procedure TestPrintOn;
  end;

  TestTIdSdpTimes = class(TTestCase)
  private
    T: TIdSdpTimes;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestClear;
    procedure TestContains;
    procedure TestPrintOn;
  end;

  TestTIdSdpZoneAdjustments = class(TTestCase)
  private
    Z: TIdSdpZoneAdjustments;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestClear;
    procedure TestContains;
    procedure TestPrintOn;
  end;

  TestTIdSdpPayload = class(TTestCase)
  private
    Payload: TIdSdpPayload;

    procedure SetToMinimumPayload(P: TIdSdpPayload);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAttribute;
    procedure TestAddConnection;
    procedure TestAsString;
    procedure TestAttributeCount;
    procedure TestConnectionAt;
    procedure TestConnectionCount;
    procedure TestCreateFromStream;
    procedure TestCreateFromStreamString;
    procedure TestGetRtpMapAttributes;
    procedure TestInitializeOnEmptySdpPayload;
    procedure TestInitializeOnSingleMediaSdp;
    procedure TestMediaDescriptionAt;
    procedure TestMediaDescriptionCount;
    procedure TestMediaDescriptionGetsSessionConnections;
    procedure TestNewSessionConnectionGetsCopiedToMediaDescriptions;
    procedure TestPrintOnBasic;
    procedure TestPrintOnWithAttributes;
    procedure TestPrintOnWithBandwidth;
    procedure TestPrintOnWithEmail;
    procedure TestPrintOnWithInfo;
    procedure TestPrintOnWithKey;
    procedure TestPrintOnWithMediaDescriptions;
    procedure TestPrintOnWithPhoneNumber;
    procedure TestPrintOnWithTimes;
    procedure TestPrintOnWithUri;
  end;

  TestTIdSdpParser = class(TTestCase)
  private
    P:       TIdSdpParser;
    Payload: TIdSdpPayload;

    procedure CheckMalformedAttribute(const Value: String);
    procedure CheckMalformedConnection(const Value: String);
    procedure CheckMalformedOrigin(const OriginValue: String);
    procedure CheckMalformedPhoneNumber(const Value: String);
    procedure CheckMalformedKey(const KeyValue: String);
    procedure CheckMalformedMediaDescription(const Value: String);
    procedure CheckMalformedOptionalSessionHeader(const Name, Value: String);
    procedure CheckMalformedTimeWithRepeat(const RepeatValue: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsAddressType;
    procedure TestIsBandwidthType;
    procedure TestIsByteString;
    procedure TestIsKeyData;
    procedure TestIsKeyType;
    procedure TestIsMediaType;
    procedure TestIsMulticastAddress;
    procedure TestIsNetType;
    procedure TestIsPhone;
    procedure TestIsPhoneNumber;
    procedure TestIsPort;
    procedure TestIsText;
    procedure TestIsTime;
    procedure TestIsTransport;
    procedure TestParseAttribute;
    procedure TestParseAttributeWithValue;
    procedure TestParseAttributeMalformedName;
    procedure TestParseAttributeMalformedValue;
    procedure TestParseBandwidth;
    procedure TestParseBandwidthMalformed;
    procedure TestParseBandwidthMultipleHeaders;
    procedure TestParseConnectionInSessionAndMediaDescription;
    procedure TestParseConnectionMalformed;
    procedure TestParseConnectionMalformedMulticastAddress;
    procedure TestParseConnectionMalformedUnicastAddress;
    procedure TestParseConnectionMulticastAddress;
    procedure TestParseConnectionMulticastAddressWithMultiple;
    procedure TestParseConnectionMulticastIPv6AddressWithMultiple;
    procedure TestParseConnectionUnknownAddrType;
    procedure TestParseConnectionUnknownNetType;
    procedure TestParseConnectionUnicastAddress;
    procedure TestParseEmail;
    procedure TestParseEmailBracketedName;
    procedure TestParseEmailRfc822;
    procedure TestParseEmptyStream;
    procedure TestParseHeaderMissingName;
    procedure TestParseHeaderMissingValue;
    procedure TestParseHeadersInvalidOrder;
    procedure TestParseInfo;
    procedure TestParseInfoIllegalCharacters;
    procedure TestParseKey;
    procedure TestParseKeyMalformedPrompt;
    procedure TestParseKeyMalformedClear;
    procedure TestParseKeyMalformedBase64;
    procedure TestParseKeyMalformedUri;
    procedure TestParseKeyUnknownKeyType;
    procedure TestParseLinphoneSessionDescription;
    procedure TestParseMediaDescription;
    procedure TestParseMediaDescriptionWithSessionConnections;
    procedure TestParseMediaDescriptionMalformedFormatList;
    procedure TestParseMediaDescriptionMalformedPort;
    procedure TestParseMediaDescriptionMissingFormatList;
    procedure TestParseMediaDescriptionMissingInformation;
    procedure TestParseMediaDescriptionMissingKey;
    procedure TestParseMediaDescriptionMissingPort;
    procedure TestParseMediaDescriptionsMissingSessionConnection;
    procedure TestParseMediaDescriptions;
    procedure TestParseMediaDescriptionUnknownMediaType;
    procedure TestParseMediaDescriptionUnknownTransport;
    procedure TestParseMediaDescriptionWithAttributes;
    procedure TestParseMediaDescriptionWithBandwidth;
    procedure TestParseMediaDescriptionWithConnection;
    procedure TestParseMediaDescriptionWithKey;
    procedure TestParseMediaDescriptionWithMultipleFormats;
    procedure TestParseMediaDescriptionWithPortCount;
    procedure TestParseMinimumPayload;
    procedure TestParseMissingOrigin;
    procedure TestParseMissingSession;
    procedure TestParseMissingSessionConnection;
    procedure TestParseMissingVersion;
    procedure TestParseOriginIPv6Address;
    procedure TestParseOriginMalformed;
    procedure TestParseOriginMalformedUserName;
    procedure TestParseOriginMalformedSessionID;
    procedure TestParseOriginMalformedSessionVersion;
    procedure TestParseOriginMalformedNetType;
    procedure TestParsePhoneNumber;
    procedure TestParsePhoneNumberMultipleHeaders;
    procedure TestParsePhoneNumberWithAngleBracketsButNoName;
    procedure TestParsePhoneNumberWithStuffOutsideComment;
    procedure TestParsePhoneNumberWithUnsafeChars;
    procedure TestParseRedefinedPayloadType;
    procedure TestParseSessionIllegalCharacters;
    procedure TestParseSomeMediaDescriptionsLackConnectionAndNoSessionConnection;
    procedure TestParseTimeMultipleHeaders;
    procedure TestParseTimeSingleBounded;
    procedure TestParseTimeSingleUnbounded;
    procedure TestParseTimeSingleUnboundedEndTime;
    procedure TestParseTimeWithMalformedMultipleRepeat;
    procedure TestParseTimeWithMalformedSingleRepeat;
    procedure TestParseTimeWithRepeatAndZoneAdjustment;
    procedure TestParseTimeWithRepeatBeforeZoneAdjustment;
    procedure TestParseTimeWithSingleRepeat;
    procedure TestParseTimeWithSingleTypedRepeat;
    procedure TestParseTimeWithSingleZoneAdjustment;
    procedure TestParseUri;
    procedure TestParseVersionMalformed;
    procedure TestParseVersionMultipleHeaders;
  end;

  TestTIdFilteredRTPPeer = class(TTestCase,
                                 IIdRTPListener)
  private
    Binding:          TIdSocketHandle;
    NormalPacket:     TIdRTPPacket;
    LocalDesc:        TIdSdpMediaDescription;
    ReceivedRTCP:     Boolean;
    ReceivedRTP:      Boolean;
    Profile:          TIdRTPProfile;
    RemoteDesc:       TIdSdpMediaDescription;
    Peer:             TIdFilteredRTPPeer;
    Server:           TIdMockRTPPeer;
    T140PT:           TIdRTPPayloadType;
    UnexpectedPacket: TIdRTPPacket;

    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdSocketHandle);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdSocketHandle);
    procedure Send(RTP: TIdRTPPacket);
    procedure SendNormalData;
    procedure SendUnexpectedData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFilter;
    procedure TestNormalOperation;
    procedure TestSendPacket;
  end;

  TestTIdSdpPayloadProcessor = class(TTestCase)
  private
    Proc: TIdSdpPayloadProcessor;

    procedure CheckServerActiveOn(Port: Cardinal);
    procedure CheckServerNotActiveOn(Port: Cardinal);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsListening;
    procedure TestMediaDescriptionWithMultiplePorts;
    procedure TestMultipleMediaDescriptions;
    procedure TestSessionCount;
    procedure TestSingleMediaDescription;
    procedure TestStopListening;
  end;

const
  MinimumPayloadSansConnection = 'v=0'#13#10
                 + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                 + 's=Minimum Session Info';
  DefaultConnection = 'c=IN IP4 224.2.17.12/127';

  MinimumPayload = MinimumPayloadSansConnection + #13#10
                 + DefaultConnection;

implementation

uses
  IdSimpleParser, IdUDPServer, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSdpParser unit tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSdpAttribute.Suite);
  Result.AddTest(TestTIdSdpRTPMapAttribute.Suite);
  Result.AddTest(TestTIdSdpBandwidth.Suite);
  Result.AddTest(TestTIdSdpConnection.Suite);
  Result.AddTest(TestTIdSdpKey.Suite);
  Result.AddTest(TestTIdSdpMediaDescription.Suite);
  Result.AddTest(TestTIdSdpOrigin.Suite);
  Result.AddTest(TestTIdSdpTime.Suite);
  Result.AddTest(TestTIdSdpAttributes.Suite);
  Result.AddTest(TestTIdSdpRTPMapAttributes.Suite);
  Result.AddTest(TestTIdSdpBandwidths.Suite);
  Result.AddTest(TestTIdSdpConnections.Suite);
  Result.AddTest(TestTIdSdpMediaDescriptions.Suite);
  Result.AddTest(TestTIdSdpRepeats.Suite);
  Result.AddTest(TestTIdSdpTimes.Suite);
  Result.AddTest(TestTIdSdpZoneAdjustments.Suite);
  Result.AddTest(TestTIdSdpParser.Suite);
  Result.AddTest(TestTIdSdpPayload.Suite);
  Result.AddTest(TestTIdFilteredRTPPeer.Suite);
  Result.AddTest(TestTIdSdpPayloadProcessor.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestAddressTypeToStr;
var
  A: TIdIPVersion;
begin
  CheckEquals(Id_SDP_IP4, AddressTypeToStr(Id_IPv4), 'Id_IPv4');
  CheckEquals(Id_SDP_IP6, AddressTypeToStr(Id_IPv6), 'Id_IPv6');

  // To check that ALL TIdIPVersions can be converted
  for A := Low(TIdIPVersion) to High(TIdIPVersion) do
    AddressTypeToStr(A);
end;

procedure TestFunctions.TestBandwidthTypeToStr;
var
  B: TIdSdpBandwidthType;
begin
  CheckEquals(Id_SDP_ConferenceTotal,     BandwidthTypeToStr(btConferenceTotal),     'btConferenceTotal');
  CheckEquals(Id_SDP_ApplicationSpecific, BandwidthTypeToStr(btApplicationSpecific), 'btApplicationSpecific');
  CheckEquals(Id_SDP_RS,                  BandwidthTypeToStr(btRS),                  'btRS');
  CheckEquals(Id_SDP_RR,                  BandwidthTypeToStr(btRR),                  'btRR');

  // To check that ALL TIdSdpBandwidthTypes can be converted
  for B := Low(TIdSdpBandwidthType) to High(TIdSdpBandwidthType) do
    BandwidthTypeToStr(B);
end;

procedure TestFunctions.TestKeyTypeToStr;
var
  K: TIdSdpKeyType;
begin
  CheckEquals(Id_SDP_Clear,  KeyTypeToStr(ktClear),  'ktClear');
  CheckEquals(Id_SDP_Base64, KeyTypeToStr(ktBase64), 'ktBase64');
  CheckEquals(Id_SDP_URI,    KeyTypeToStr(ktURI),    'ktURI');
  CheckEquals(Id_SDP_Prompt, KeyTypeToStr(ktPrompt), 'ktPrompt');

  // To check that ALL TIdSdpKeyTypes can be converted
  for K := Low(TIdSdpKeyType) to High(TIdSdpKeyType) do
    KeyTypeToStr(K);
end;

procedure TestFunctions.TestMediaTypeToStr;
var
  M: TIdSdpMediaType;
begin
  CheckEquals(RSSDPMediaTypeAudio,       MediaTypeToStr(mtAudio),       'mtAudio');
  CheckEquals(RSSDPMediaTypeVideo,       MediaTypeToStr(mtVideo),       'mtVideo');
  CheckEquals(RSSDPMediaTypeApplication, MediaTypeToStr(mtApplication), 'mtApplication');
  CheckEquals(RSSDPMediaTypeData,        MediaTypeToStr(mtData),        'mtData');
  CheckEquals(RSSDPMediaTypeControl,     MediaTypeToStr(mtControl),     'mtControl');
  CheckEquals(RSSDPMediaTypeText,        MediaTypeToStr(mtText),        'mtText');

  // To check that ALL TIdSdpMediaTypes can be converted
  for M := Low(TIdSdpMediaType) to High(TIdSdpMediaType) do
    MediaTypeToStr(M);
end;

procedure TestFunctions.TestStrToAddressType;
var
  AT: TIdIPVersion;
begin
  for AT := Low(TIdIPVersion) to High(TIdIPVersion) do
    StrToAddressType(AddressTypeToStr(AT));

  try
    StrToAddressType('');
    Fail('Failed to bail out: ''''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert '''' to type TIdIPVersion',
                  E.Message,
                  'Unexpected exception: ''''');
  end;

  try
    StrToAddressType('ip4');
    Fail('Failed to bail out: ''ip4''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert ''ip4'' to type TIdIPVersion',
                  E.Message,
                  'Unexpected exception: ''ip4''');
  end;

  try
    StrToAddressType('IP5');
    Fail('Failed to bail out: ''IP5''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert ''IP5'' to type TIdIPVersion',
                  E.Message,
                  'Unexpected exception: ''IP5''');
  end;

  try
    StrToAddressType('halloo');
    Fail('Failed to bail out: ''halloo''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert ''halloo'' to type TIdIPVersion',
                  E.Message,
                  'Unexpected exception: ''halloo''');
  end;

  try
    StrToAddressType(' ');
    Fail('Failed to bail out: '' ''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert '' '' to type TIdIPVersion',
                  E.Message,
                  'Unexpected exception: '' ''');
  end;
end;

procedure TestFunctions.TestStrToBandwidthType;
var
  BT: TIdSdpBandwidthType;
begin
  for BT := Low(TIdSdpBandwidthType) to High(TIdSdpBandwidthType) do
    StrToBandwidthType(BandwidthTypeToStr(BT));

  try
    StrToBandwidthType('halloo');
    Fail('Failed to bail out: ''halloo''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert ''halloo'' to type TIdSdpBandwidthType',
                  E.Message,
                  'Unexpected exception: ''halloo''');
  end;

  try
    StrToBandwidthType(' ');
    Fail('Failed to bail out: '' ''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert '' '' to type TIdSdpBandwidthType',
                  E.Message,
                  'Unexpected exception: '' ''');
  end;
end;

procedure TestFunctions.TestStrToKeyType;
var
  KT: TIdSdpKeyType;
begin
  for KT := Low(TIdSdpKeyType) to High(TIdSdpKeyType) do
    StrToKeyType(KeyTypeToStr(KT));

  try
    StrToKeyType('halloo');
    Fail('Failed to bail out: ''halloo''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert ''halloo'' to type TIdSdpKeyType',
                  E.Message,
                  'Unexpected exception: ''halloo''');
  end;

  try
    StrToKeyType(' ');
    Fail('Failed to bail out: '' ''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert '' '' to type TIdSdpKeyType',
                  E.Message,
                  'Unexpected exception: '' ''');
  end;
end;

procedure TestFunctions.TestStrToMediaType;
var
  MT: TIdSdpMediaType;
begin
  for MT := Low(TIdSdpMediaType) to High(TIdSdpMediaType) do
    StrToMediaType(MediaTypeToStr(MT));

  try
    StrToMediaType('halloo');
    Fail('Failed to bail out: ''halloo''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert ''halloo'' to type TIdSdpMediaType',
                  E.Message,
                  'Unexpected exception: ''halloo''');
  end;

  try
    StrToMediaType(' ');
    Fail('Failed to bail out: '' ''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert '' '' to type TIdSdpMediaType',
                  E.Message,
                  'Unexpected exception: '' ''');
  end;
end;

//******************************************************************************
//* TestTIdSdpAttribute                                                        *
//******************************************************************************
//* TestTIdSdpAttribute Public methods *****************************************

procedure TestTIdSdpAttribute.SetUp;
begin
  inherited SetUp;

  Self.A := TIdSdpAttribute.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpAttribute.TearDown;
begin
  Self.S.Free;
  Self.A.Free;

  inherited TearDown;
end;

//* TestTIdSdpAttribute Published methods **************************************

procedure TestTIdSdpAttribute.TestCreateAttribute;
var
  Att: TIdSdpAttribute;
begin
  Att := TIdSdpAttribute.CreateAttribute('rtpmap:98 t140/1000');
  try
    CheckEquals(TIdSdpRTPMapAttribute.ClassName,
                Att.ClassName,
                'Incorrect class for rtpmap attribute');
    CheckEquals('rtpmap',       Att.Name,  'rtpmap attribute name');
    CheckEquals('98 t140/1000', Att.Value, 'rtpmap attribute value');
  finally
    Att.Free;
  end;

  Att := TIdSdpAttribute.CreateAttribute('98 t140/1000');
  try
    CheckEquals(TIdSdpAttribute.ClassName,
                Att.ClassName,
                'Incorrect class for "unspecial" attribute');
    CheckEquals('98 t140/1000', Att.Name,  'attribute name');
    CheckEquals('',             Att.Value, 'attribute value');
  finally
    Att.Free;
  end;
end;

procedure TestTIdSdpAttribute.TestClone;
var
  Clone: TIdSdpAttribute;
begin
  Self.A.Name  := 'abcd';
  Self.A.Value := 'efgh';

  Clone := Self.A.Copy;
  try
    CheckEquals(Self.A.Name,  Clone.Name,  'Name');
    CheckEquals(Self.A.Value, Clone.Value, 'Value');
  finally
    Clone.Free;
  end;
end;

procedure TestTIdSdpAttribute.TestIsRTPMap;
begin
  Check(not Self.A.IsRTPMap, 'Shouldn''t be an RTP map');
end;

procedure TestTIdSdpAttribute.TestPrintOnNoValue;
begin
  Self.A.Name := 'rtpmap';

  Self.A.PrintOn(Self.S);

  CheckEquals(#13#10'a=rtpmap', Self.S.DataString, 'PrintOn');
end;

procedure TestTIdSdpAttribute.TestPrintOnWithValue;
begin
  Self.A.Name  := 'rtpmap';
  Self.A.Value := '98 t140';

  Self.A.PrintOn(Self.S);

  CheckEquals(#13#10'a=rtpmap:98 t140', Self.S.DataString, 'PrintOn');
end;

//******************************************************************************
//* TestTIdSdpRTPMapAttribute                                                  *
//******************************************************************************
//* TestTIdSdpRTPMapAttribute Public methods ***********************************

procedure TestTIdSdpRTPMapAttribute.SetUp;
begin
  inherited SetUp;

  Self.A := TIdSdpRTPMapAttribute.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpRTPMapAttribute.TearDown;
begin
  Self.S.Free;
  Self.A.Free;

  inherited TearDown;
end;

//* TestTIdSdpRTPMapAttribute Published methods ********************************

procedure TestTIdSdpRTPMapAttribute.TestClone;
var
  Clone: TIdSdpAttribute;
begin
  Self.A.Name  := 'rtpmap';
  Self.A.Value := '98 t140/8000';

  Clone := Self.A.Copy;
  try
    CheckEquals(Self.A.ClassName,
                Clone.ClassName,
                'Type');
    CheckEquals(Self.A.Name,  Clone.Name,  'Name');
    CheckEquals(Self.A.Value, Clone.Value, 'Value');
  finally
    Clone.Free;
  end;
end;

procedure TestTIdSdpRTPMapAttribute.TestGetName;
begin
  CheckEquals(RTPMapAttribute, Self.A.Name, 'New rtpmap attribute');

  Self.A.Name := 'foo';

  CheckEquals(RTPMapAttribute, Self.A.Name, 'After trying to change its name');
end;

procedure TestTIdSdpRTPMapAttribute.TestIsRTPMap;
begin
  Check(Self.A.IsRTPMap, 'Should be an RTP map');
end;

procedure TestTIdSdpRTPMapAttribute.TestSetValue;
begin
  Self.A.Value := '98 T140/1000';

  CheckEquals(TIdRTPT140Payload.ClassName,
              Self.A.Encoding.ClassName,
              'Encoding');
  CheckEquals(T140Encoding,
              Self.A.Encoding.Name,
              'Encoding name');
  CheckEquals(T140ClockRate,
              Self.A.Encoding.ClockRate,
              'Encoding clock rate');
  CheckEquals('',
              Self.A.Encoding.Parameters,
              'Encoding parameters');
  CheckEquals(98,
              Self.A.PayloadType,
              'Payload type');
end;

procedure TestTIdSdpRTPMapAttribute.TestSetValueWithParameters;
begin
  Self.A.Value := '98 t140/1000/1';

  CheckEquals('1', Self.A.Encoding.Parameters, 'Encoding parameters');
end;

//******************************************************************************
//* TestTIdSdpBandwidth                                                        *
//******************************************************************************
//* TestTIdSdpBandwidth Public methods *****************************************

procedure TestTIdSdpBandwidth.SetUp;
begin
  inherited SetUp;

  Self.B := TIdSdpBandwidth.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpBandwidth.TearDown;
begin
  Self.S.Free;
  Self.B.Free;

  inherited TearDown;
end;

//* TestTIdSdpBandwidth Published methods **************************************

procedure TestTIdSdpBandwidth.TestPrintOn;
begin
  Self.B.BandwidthType := btConferenceTotal;
  Self.B.Bandwidth     := 42;

  Self.B.PrintOn(Self.S);

  CheckEquals(#13#10'b=CT:42', S.DataString, 'PrintOn');
end;

//******************************************************************************
//* TestTIdSdpConnection                                                       *
//******************************************************************************
//* TestTIdSdpConnection Public methods ****************************************

procedure TestTIdSdpConnection.SetUp;
begin
  inherited SetUp;

  Self.C := TIdSdpConnection.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpConnection.TearDown;
begin
  Self.S.Free;
  Self.C.Free;

  inherited TearDown;
end;

//* TestTIdSdpConnection Published methods *************************************

procedure TestTIdSdpConnection.TestCopy;
var
  Copy: TIdSdpConnection;
begin
  Self.C.Address           := 'FF80::1';
  Self.C.AddressType       := Id_IPv6;
  Self.C.NetType           := Id_SDP_IN;
  Self.C.NumberOfAddresses := 2;
  Self.C.TTL               := 255;

  Copy := Self.C.Copy;
  try
    CheckEquals(Self.C.Address,           Copy.Address,           'Address');
    Check      (Self.C.AddressType      = Copy.AddressType,       'AddressType');
    CheckEquals(Self.C.NetType,           Copy.NetType,           'NetType');
    CheckEquals(Self.C.NumberOfAddresses, Copy.NumberOfAddresses, 'NumberOfAddresses');
    CheckEquals(Self.C.TTL,               Copy.TTL,               'TTL');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdSdpConnection.TestPrintOnMultiCast;
begin
  Self.C.Address     := '224.0.0.0';
  Self.C.AddressType := Id_IPv4;
  Self.C.NetType     := Id_SDP_IN;

  Self.C.PrintOn(Self.S);

  CheckEquals(#13#10'c=IN IP4 224.0.0.0', S.DataString, 'PrintOn');
end;

procedure TestTIdSdpConnection.TestPrintOnMultiCastWithNumberNoTtl;
begin
  Self.C.Address           := '224.0.0.0';
  Self.C.AddressType       := Id_IPv4;
  Self.C.NetType           := Id_SDP_IN;
  Self.C.NumberOfAddresses := 127;

  Self.C.PrintOn(Self.S);

  CheckEquals(#13#10'c=IN IP4 224.0.0.0', S.DataString, 'PrintOn');
end;

procedure TestTIdSdpConnection.TestPrintOnMultiCastWithTtl;
begin
  Self.C.Address     := '224.0.0.0';
  Self.C.AddressType := Id_IPv4;
  Self.C.NetType     := Id_SDP_IN;
  Self.C.TTL         := 127;

  Self.C.PrintOn(Self.S);

  CheckEquals(#13#10'c=IN IP4 224.0.0.0/127', S.DataString, 'PrintOn');
end;

procedure TestTIdSdpConnection.TestPrintOnMultiCastWithTtlAndNumber;
begin
  Self.C.Address           := '224.0.0.0';
  Self.C.AddressType       := Id_IPv4;
  Self.C.NetType           := Id_SDP_IN;
  Self.C.NumberOfAddresses := 4;
  Self.C.TTL               := 127;

  Self.C.PrintOn(Self.S);

  CheckEquals(#13#10'c=IN IP4 224.0.0.0/127/4', S.DataString, 'PrintOn');
end;

procedure TestTIdSdpConnection.TestPrintOnUnicast;
begin
  Self.C.Address     := '0.0.0.255';
  Self.C.AddressType := Id_IPv4;
  Self.C.NetType     := Id_SDP_IN;

  Self.C.PrintOn(Self.S);

  CheckEquals(#13#10'c=IN IP4 0.0.0.255', S.DataString, 'PrintOn');
end;

//******************************************************************************
//* TestTIdSdpKey                                                              *
//******************************************************************************
//* TestTIdSdpKey Public methods ***********************************************

procedure TestTIdSdpKey.SetUp;
begin
  inherited SetUp;

  Self.K := TIdSdpKey.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpKey.TearDown;
begin
  Self.S.Free;
  Self.K.Free;

  inherited TearDown;
end;

//* TestTIdSdpKey Published methods ********************************************

procedure TestTIdSdpKey.TestPrintOnJustMethod;
begin
  Self.K.KeyType := ktPrompt;

  Self.K.PrintOn(Self.S);

  CheckEquals(#13#10'k=prompt', S.DataString, 'PrintOn');
end;

procedure TestTIdSdpKey.TestPrintOnMethodPlusValue;
begin
  Self.K.KeyType := ktUri;
  Self.K.Value   := 'tel://42';

  Self.K.PrintOn(Self.S);

  CheckEquals(#13#10'k=uri:tel://42', S.DataString, 'PrintOn');
end;

procedure TestTIdSdpKey.TestPrintOnPromptWithKeyData;
begin
  Self.K.KeyType := ktPrompt;
  Self.K.Value   := 'tel://42';

  Self.K.PrintOn(Self.S);

  CheckEquals(#13#10'k=prompt', S.DataString, 'PrintOn');
end;

procedure TestTIdSdpKey.TestPrintOnUriWithNoKeyData;
begin
  Self.K.KeyType := ktUri;

  Self.K.PrintOn(Self.S);

  CheckEquals(#13#10'k=uri:', S.DataString, 'PrintOn');
end;

//******************************************************************************
//* TestTIdSdpMediaDescription                                                 *
//******************************************************************************
//* TestTIdSdpMediaDescription Public methods **********************************

procedure TestTIdSdpMediaDescription.SetUp;
begin
  inherited SetUp;

  Self.M := TIdSdpMediaDescription.Create;
end;

procedure TestTIdSdpMediaDescription.TearDown;
begin
  Self.M.Free;

  inherited TearDown;
end;

//* TestTIdSdpMediaDescription Published methods *******************************

procedure TestTIdSdpMediaDescription.TestAddAttributeAndCount;
var
  I: Integer;
begin
  for I := 1 to 10 do begin
    Self.M.AddAttribute('rtpmap', '98 t140/1000');
    CheckEquals(I,
                Self.M.AttributeCount,
                'Attribute not added, ' + IntToStr(I));
  end;
end;

procedure TestTIdSdpMediaDescription.TestAddFormatAndFormatCount;
var
  I: Integer;
begin
  CheckEquals(0, Self.M.FormatCount, 'FormatCount on new media-description');

  for I := 1 to 1000 do begin
    Self.M.AddFormat(IntToStr(I));
    CheckEquals(I,
                Self.M.FormatCount,
                'FormatCount after ' + IntToStr(I) + ' AddFormat/s');
  end;
end;

procedure TestTIdSdpMediaDescription.TestAttributeAt;
begin
  Self.M.AddAttribute('rtpmap', '98 t140/1000');
  Self.M.AddAttribute('rtpmap', '99 gsm/8000');

  CheckEquals('98 t140/1000',
              Self.M.AttributeAt(0).Value,
              'AttributeAt(0)');
  CheckEquals('99 gsm/8000',
              Self.M.AttributeAt(1).Value,
              'AttributeAt(1)');
end;

procedure TestTIdSdpMediaDescription.TestFormatClear;
var
  I: Integer;
begin
  for I := 1 to 1000 do
    Self.M.AddFormat(IntToStr(I));

  Self.M.ClearFormats;
  CheckEquals(0, Self.M.FormatCount, 'FormatCount after ClearFormats');
end;

procedure TestTIdSdpMediaDescription.TestHasFormat;
var
  Fmt: String;
begin
  Self.M.ClearFormats;
  Fmt := 'foo';
  Check(not Self.M.HasFormat(Fmt), 'Empty format list');
  Self.M.AddFormat(Fmt);
  Check(Self.M.HasFormat(Fmt), 'Format not added?');
end;

procedure TestTIdSdpMediaDescription.TestGetFormat;
var
  I: Integer;
begin
  for I := 0 to 999 do begin
    Self.M.AddFormat(IntToStr(I));
    CheckEquals(IntToStr(I), Self.M.Formats[I], 'Formats[' + IntToStr(I) + ']');
  end;
end;

procedure TestTIdSdpMediaDescription.TestInitialState;
begin
  CheckEquals(1, Self.M.PortCount, 'PortCount');
end;

procedure TestTIdSdpMediaDescription.TestPrintOnBasic;
var
  S: TStringStream;
begin
  Self.M.MediaType := mtAudio;
  Self.M.Port      := 49230;
  Self.M.Transport := 'RTP/AVP';

  S := TStringStream.Create('');
  try
    Self.M.PrintOn(S);
    CheckEquals(#13#10
              + 'm=audio 49230 RTP/AVP',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpMediaDescription.TestPrintOnFull;
var
  Conn: TIdSdpConnection;
  S: TStringStream;
begin
  Self.M.AddAttribute(RTPMapAttribute, '98 L16/16000/2');

  Self.M.AddAttribute(RTPMapAttribute, '100 t140/1000');

  Self.M.Bandwidths.Add(TIdSdpBandwidth.Create);
  Self.M.Bandwidths[0].Bandwidth := 666;
  Self.M.Bandwidths[0].BandwidthType := btRS;

  Self.M.Bandwidths.Add(TIdSdpBandwidth.Create);
  Self.M.Bandwidths[1].Bandwidth := 42;
  Self.M.Bandwidths[1].BandwidthType := btConferenceTotal;

  Self.M.Connections.Add(TIdSdpConnection.Create);
  Conn := Self.M.Connections[0];
  Conn.Address           := '127.0.0.1';
  Conn.AddressType       := Id_IPv4;
  Conn.NetType           := Id_SDP_IN;
  Conn.TTL               := 5;
  Conn.NumberOfAddresses := 5;
  Self.M.Info            := 'Cthulhu Speaks';
  Self.M.Key.KeyType     := ktBase64;
  Self.M.Key.Value       := 'DEADBEEF';
  Self.M.MediaType       := mtAudio;
  Self.M.Port            := 49230;
  Self.M.Transport       := 'RTP/AVP';

  S := TStringStream.Create('');
  try
    Self.M.PrintOn(S);
    CheckEquals(#13#10
              + 'm=audio 49230 RTP/AVP'#13#10
              + 'i=Cthulhu Speaks'#13#10
              + 'c=IN IP4 127.0.0.1/5/5'#13#10
              + 'b=RS:666'#13#10
              + 'b=CT:42'#13#10
              + 'k=base64:DEADBEEF'#13#10
              + 'a=rtpmap:98 L16/16000/2'#13#10
              + 'a=rtpmap:100 t140/1000',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpMediaDescription.TestPrintOnWithPortCount;
var
  S: TStringStream;
begin
  Self.M.AddFormat('0');
  Self.M.MediaType := mtAudio;
  Self.M.Port      := 49230;
  Self.M.PortCount := 4;
  Self.M.Transport := 'RTP/AVP';

  S := TStringStream.Create('');
  try
    Self.M.PrintOn(S);

    CheckEquals(#13#10'm=audio 49230/4 RTP/AVP 0',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpOrigin                                                           *
//******************************************************************************
//* TestTIdSdpOrigin Public methods ********************************************

procedure TestTIdSdpOrigin.SetUp;
begin
  inherited SetUp;

  Self.O := TIdSdpOrigin.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpOrigin.TearDown;
begin
  Self.S.Free;
  Self.O.Free;

  inherited TearDown;
end;

//* TestTIdSdpOrigin Published methods *****************************************

procedure TestTIdSdpOrigin.TestPrintOn;
begin
  Self.O.Address        := 'www.example.com';
  Self.O.AddressType    := Id_IPv6;
  Self.O.NetType        := Id_SDP_IN;
  Self.O.SessionID      := 'side0f';
  Self.O.SessionVersion := 'beef';
  Self.O.UserName       := 'Holy_Cow';

  Self.O.PrintOn(S);

  CheckEquals(#13#10'o=Holy_Cow side0f beef IN IP6 www.example.com',
              S.DataString,
              'PrintOn');
end;

//******************************************************************************
//* TestTIdSdpTime                                                             *
//******************************************************************************
//* TestTIdSdpTime Public methods **********************************************

procedure TestTIdSdpTime.SetUp;
begin
  inherited SetUp;

  Self.T := TIdSdpTime.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpTime.TearDown;
begin
  Self.S.Free;
  Self.T.Free;

  inherited TearDown;
end;

//* TestTIdSdpTime Published methods *******************************************

procedure TestTIdSdpTime.TestPrintOn;
begin
  Self.T.EndTime   := $deadbeef;
  Self.T.StartTime := $cafebabe;

  Self.T.PrintOn(S);

  CheckEquals(#13#10't=3405691582 3735928559',
              S.DataString,
              'PrintOn');
end;

procedure TestTIdSdpTime.TestPrintOnWithRepeats;
begin
  Self.T.EndTime   := $deadbeef;
  Self.T.StartTime := $cafebabe;
  Self.T.Repeats.Add(TIdSdpRepeat.Create);
  Self.T.Repeats[0].Value := '1d';

  Self.T.PrintOn(S);

  CheckEquals(#13#10
            + 't=3405691582 3735928559'#13#10
            + 'r=1d',
              S.DataString,
              'PrintOn');
end;

procedure TestTIdSdpTime.TestPrintOnWithZoneAdjustments;
begin
  Self.T.EndTime   := $deadbeef;
  Self.T.StartTime := $cafebabe;
  Self.T.ZoneAdjustments.Add(TIdSdpZoneAdjustment.Create);
  Self.T.ZoneAdjustments[0].Value := '3735928559 -2h';

  Self.T.PrintOn(S);

  CheckEquals(#13#10
            + 't=3405691582 3735928559'#13#10
            + 'z=3735928559 -2h',
              S.DataString,
              'PrintOn');
end;

//******************************************************************************
//* TestTIdSdpAttributes                                                       *
//******************************************************************************
//* TestTIdSdpAttributes Public methods ****************************************

procedure TestTIdSdpAttributes.SetUp;
begin
  inherited SetUp;

  Self.A := TIdSdpAttributes.Create;
end;

procedure TestTIdSdpAttributes.TearDown;
begin
  Self.A.Free;

  inherited TearDown;
end;

//* TestTIdSdpAttributes Published methods *************************************

procedure TestTIdSdpAttributes.TestAddAndCount;
begin
  CheckEquals(0, Self.A.Count, 'Count on new list');
  Self.A.Add(TIdSdpAttribute.Create);
  CheckEquals(1, Self.A.Count, 'Count after Add()');
  Self.A.Add(TIdSdpAttribute.Create);
  CheckEquals(2, Self.A.Count, 'Count after 2nd Add()');
end;

procedure TestTIdSdpAttributes.TestAddUsingString;
begin
  Self.A.Add('foo:bar');
  CheckEquals(TIdSdpAttribute.ClassName,
              Self.A[0].ClassName,
              'Incorrect class added from ''foo:bar''');

  Self.A.Add('rtpmap:98 t140/1000');
  CheckEquals(TIdSdpRTPMapAttribute.ClassName,
              Self.A[1].ClassName,
              'Incorrect class added from ''rtpmap:98 t140/1000''');
end;

procedure TestTIdSdpAttributes.TestClear;
begin
  Self.A.Add(TIdSdpAttribute.Create);
  Self.A.Add(TIdSdpAttribute.Create);
  Self.A.Add(TIdSdpAttribute.Create);

  Self.A.Clear;
  CheckEquals(0, Self.A.Count, 'Count after clear');
end;

procedure TestTIdSdpAttributes.TestContains;
var
  O: TIdSdpAttribute;
begin
  O := TIdSdpAttribute.Create;
  Check(not Self.A.Contains(O), 'Contains object when it shouldn''t');
  Self.A.Add(O);
  Check(Self.A.Contains(O), 'Doesn''t contain object when it should');
end;

procedure TestTIdSdpAttributes.TestPrintOn;
var
  S: TStringStream;
begin
  Self.A.Add(TIdSdpAttribute.Create);
  Self.A.Add(TIdSdpAttribute.Create);

  Self.A[0].Name  := 'rtpmap';
  Self.A[0].Value := '98 t140/1000';
  Self.A[1].Name  := 'dead';
  Self.A[1].Value := 'beef';

  S := TStringStream.Create('');
  try
    Self.A.PrintOn(S);
    CheckEquals(#13#10
              + 'a=rtpmap:98 t140/1000'#13#10
              + 'a=dead:beef',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpRTPMapAttributes                                                 *
//******************************************************************************
//* TestTIdSdpRTPMapAttributes Public methods **********************************

procedure TestTIdSdpRTPMapAttributes.SetUp;
begin
  inherited SetUp;

  Self.A := TIdSdpRTPMapAttributes.Create;
end;

procedure TestTIdSdpRTPMapAttributes.TearDown;
begin
  Self.A.Free;

  inherited TearDown;
end;

//* TestTIdSdpRTPMapAttributes Published methods *******************************

procedure TestTIdSdpRTPMapAttributes.TestAddAndCount;
begin
  CheckEquals(0, Self.A.Count, 'Count on new list');
  Self.A.Add(TIdSdpRTPMapAttribute.Create);
  CheckEquals(1, Self.A.Count, 'Count after Add()');
  Self.A.Add(TIdSdpRTPMapAttribute.Create);
  CheckEquals(2, Self.A.Count, 'Count after 2nd Add()');
end;

procedure TestTIdSdpRTPMapAttributes.TestClear;
begin
  Self.A.Add(TIdSdpRTPMapAttribute.Create);
  Self.A.Add(TIdSdpRTPMapAttribute.Create);
  Self.A.Add(TIdSdpRTPMapAttribute.Create);

  Self.A.Clear;
  CheckEquals(0, Self.A.Count, 'Count after clear');
end;

procedure TestTIdSdpRTPMapAttributes.TestContains;
var
  O: TIdSdpRTPMapAttribute;
begin
  O := TIdSdpRTPMapAttribute.Create;
  Check(not Self.A.Contains(O), 'Contains object when it shouldn''t');
  Self.A.Add(O);
  Check(Self.A.Contains(O), 'Doesn''t contain object when it should');
end;

procedure TestTIdSdpRTPMapAttributes.TestPrintOn;
var
  S: TStringStream;
begin
  Self.A.Add(TIdSdpRTPMapAttribute.Create);
  Self.A.Add(TIdSdpRTPMapAttribute.Create);

  Self.A[0].Name  := 'rtpmap';
  Self.A[0].Value := '98 t140/1000';
  Self.A[1].Name  := 'rtpmap';
  Self.A[1].Value := '99 dead/8000';

  S := TStringStream.Create('');
  try
    Self.A.PrintOn(S);
    CheckEquals(#13#10
              + 'a=rtpmap:98 t140/1000'#13#10
              + 'a=rtpmap:99 dead/8000',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpBandwidths                                                       *
//******************************************************************************
//* TestTIdSdpBandwidths Public methods ****************************************

procedure TestTIdSdpBandwidths.SetUp;
begin
  inherited SetUp;

  Self.B := TIdSdpBandwidths.Create;
end;

procedure TestTIdSdpBandwidths.TearDown;
begin
  Self.B.Free;

  inherited TearDown;
end;

//* TestTIdSdpBandwidths Published methods *************************************

procedure TestTIdSdpBandwidths.TestAddAndCount;
begin
  CheckEquals(0, Self.B.Count, 'Count on new list');
  Self.B.Add(TIdSdpBandwidth.Create);
  CheckEquals(1, Self.B.Count, 'Count after Add()');
  Self.B.Add(TIdSdpBandwidth.Create);
  CheckEquals(2, Self.B.Count, 'Count after 2nd Add()');
end;

procedure TestTIdSdpBandwidths.TestClear;
begin
  Self.B.Add(TIdSdpBandwidth.Create);
  Self.B.Add(TIdSdpBandwidth.Create);
  Self.B.Add(TIdSdpBandwidth.Create);

  Self.B.Clear;
  CheckEquals(0, Self.B.Count, 'Count after clear');
end;

procedure TestTIdSdpBandwidths.TestContains;
var
  O: TIdSdpBandwidth;
begin
  O := TIdSdpBandwidth.Create;
  Check(not Self.B.Contains(O), 'Contains object when it shouldn''t');
  Self.B.Add(O);
  Check(Self.B.Contains(O), 'Doesn''t contain object when it should');
end;

//******************************************************************************
//* TestTIdSdpConnections                                                      *
//******************************************************************************
//* TestTIdSdpConnections Public methods ***************************************

procedure TestTIdSdpConnections.SetUp;
begin
  inherited SetUp;

  Self.C := TIdSdpConnections.Create;
end;

procedure TestTIdSdpConnections.TearDown;
begin
  Self.C.Free;

  inherited TearDown;
end;

//* TestTIdSdpConnections Published methods ************************************

procedure TestTIdSdpConnections.TestAddAndCount;
begin
  CheckEquals(0, Self.C.Count, 'Count on new list');
  Self.C.Add(TIdSdpConnection.Create);
  CheckEquals(1, Self.C.Count, 'Count after Add()');
  Self.C.Add(TIdSdpConnection.Create);
  CheckEquals(2, Self.C.Count, 'Count after 2nd Add()');
end;

procedure TestTIdSdpConnections.TestAddMultipleConnections;
var
  I:              Integer;
  NewConnections: TIdSdpConnections;
begin
  NewConnections := TIdSdpConnections.Create;
  try
    NewConnections.Add(TIdSdpConnection.Create);
    NewConnections.Add(TIdSdpConnection.Create);
    NewConnections.Add(TIdSdpConnection.Create);

    for I := 0 to NewConnections.Count - 1 do
      NewConnections[I].TTL := I;

    Self.C.Add(NewConnections);

    CheckEquals(NewConnections.Count,
                Self.C.Count,
                'Not all connections added');

    for I := 0 to Self.C.Count - 1 do
      CheckEquals(NewConnections[I].TTL,
                  Self.C[I].TTL,
                  'TTL on connection ' + IntToStr(I));
  finally
    NewConnections.Free;
  end;
end;

procedure TestTIdSdpConnections.TestClear;
begin
  Self.C.Add(TIdSdpConnection.Create);
  Self.C.Add(TIdSdpConnection.Create);
  Self.C.Add(TIdSdpConnection.Create);

  Self.C.Clear;
  CheckEquals(0, Self.C.Count, 'Count after clear');
end;

procedure TestTIdSdpConnections.TestContains;
var
  O: TIdSdpConnection;
begin
  O := TIdSdpConnection.Create;
  Check(not Self.C.Contains(O), 'Contains object when it shouldn''t');
  Self.C.Add(O);
  Check(Self.C.Contains(O), 'Doesn''t contain object when it should');
end;

//******************************************************************************
//* TestTIdSdpMediaDescriptions                                                *
//******************************************************************************
//* TestTIdSdpMediaDescriptions Public methods *********************************

procedure TestTIdSdpMediaDescriptions.SetUp;
begin
  inherited SetUp;

  Self.M := TIdSdpMediaDescriptions.Create;
end;

procedure TestTIdSdpMediaDescriptions.TearDown;
begin
  Self.M.Free;

  inherited TearDown;
end;

//* TestTIdSdpMediaDescriptions Published methods ******************************

procedure TestTIdSdpMediaDescriptions.TestAddAndCount;
begin
  CheckEquals(0, Self.M.Count, 'Count on new list');
  Self.M.Add(TIdSdpMediaDescription.Create);
  CheckEquals(1, Self.M.Count, 'Count after Add()');
  Self.M.Add(TIdSdpMediaDescription.Create);
  CheckEquals(2, Self.M.Count, 'Count after 2nd Add()');
end;

procedure TestTIdSdpMediaDescriptions.TestAllDescriptionsHaveConnections;
begin
  Check(Self.M.AllDescriptionsHaveConnections, 'Trivial case - empty list');

  Self.M.Add(TIdSdpMediaDescription.Create);
  Check(not Self.M.AllDescriptionsHaveConnections,
        'One item with no connection');

  Self.M[0].Connections.Add(TIdSdpConnection.Create);
  Check(Self.M.AllDescriptionsHaveConnections,
        'One item now has a connection');

  Self.M.Add(TIdSdpMediaDescription.Create);
  Check(not Self.M.AllDescriptionsHaveConnections,
        'A second item, with no connection');

  Self.M[1].Connections.Add(TIdSdpConnection.Create);
  Check(Self.M.AllDescriptionsHaveConnections,
        'Both items now have connections');
end;

procedure TestTIdSdpMediaDescriptions.TestClear;
begin
  Self.M.Add(TIdSdpMediaDescription.Create);
  Self.M.Add(TIdSdpMediaDescription.Create);
  Self.M.Add(TIdSdpMediaDescription.Create);

  Self.M.Clear;
  CheckEquals(0, Self.M.Count, 'Count after clear');
end;

procedure TestTIdSdpMediaDescriptions.TestContains;
var
  O: TIdSdpMediaDescription;
begin
  O := TIdSdpMediaDescription.Create;
  Check(not Self.M.Contains(O), 'Contains object when it shouldn''t');
  Self.M.Add(O);
  Check(Self.M.Contains(O), 'Doesn''t contain object when it should');
end;

//******************************************************************************
//* TestTIdSdpRepeats                                                          *
//******************************************************************************
//* TestTIdSdpRepeats Public methods *******************************************

procedure TestTIdSdpRepeats.SetUp;
begin
  inherited SetUp;

  Self.R := TIdSdpRepeats.Create;
end;

procedure TestTIdSdpRepeats.TearDown;
begin
  Self.R.Free;

  inherited TearDown;
end;

//* TestTIdSdpRepeats Published methods ****************************************

procedure TestTIdSdpRepeats.TestAddAndCount;
begin
  CheckEquals(0, Self.R.Count, 'Count on new list');
  Self.R.Add(TIdSdpRepeat.Create);
  CheckEquals(1, Self.R.Count, 'Count after Add()');
  Self.R.Add(TIdSdpRepeat.Create);
  CheckEquals(2, Self.R.Count, 'Count after 2nd Add()');
end;

procedure TestTIdSdpRepeats.TestClear;
begin
  Self.R.Add(TIdSdpRepeat.Create);
  Self.R.Add(TIdSdpRepeat.Create);
  Self.R.Add(TIdSdpRepeat.Create);

  Self.R.Clear;
  CheckEquals(0, Self.R.Count, 'Count after clear');
end;

procedure TestTIdSdpRepeats.TestContains;
var
  O: TIdSdpRepeat;
begin
  O := TIdSdpRepeat.Create;
  Check(not Self.R.Contains(O), 'Contains object when it shouldn''t');
  Self.R.Add(O);
  Check(Self.R.Contains(O), 'Doesn''t contain object when it should');
end;

procedure TestTIdSdpRepeats.TestPrintOn;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.R.Add(TIdSdpRepeat.Create);
    Self.R.Add(TIdSdpRepeat.Create);

    Self.R[0].Value := '1w';
    Self.R[1].Value := '1h 1d 1w';

    Self.R.PrintOn(S);

    CheckEquals(#13#10
              + 'r=1w'#13#10
              + 'r=1h 1d 1w',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpTimes                                                            *
//******************************************************************************
//* TestTIdSdpTimes Public methods *********************************************

procedure TestTIdSdpTimes.SetUp;
begin
  inherited SetUp;

  Self.T := TIdSdpTimes.Create;
end;

procedure TestTIdSdpTimes.TearDown;
begin
  Self.T.Free;

  inherited TearDown;
end;

//* TestTIdSdpTimes Published methods ******************************************

procedure TestTIdSdpTimes.TestAddAndCount;
begin
  CheckEquals(0, Self.T.Count, 'Count on new list');
  Self.T.Add(TIdSdpTime.Create);
  CheckEquals(1, Self.T.Count, 'Count after Add()');
  Self.T.Add(TIdSdpTime.Create);
  CheckEquals(2, Self.T.Count, 'Count after 2nd Add()');
end;

procedure TestTIdSdpTimes.TestClear;
begin
  Self.T.Add(TIdSdpTime.Create);
  Self.T.Add(TIdSdpTime.Create);
  Self.T.Add(TIdSdpTime.Create);

  Self.T.Clear;
  CheckEquals(0, Self.T.Count, 'Count after clear');
end;

procedure TestTIdSdpTimes.TestContains;
var
  O: TIdSdpTime;
begin
  O := TIdSdpTime.Create;
  Check(not Self.T.Contains(O), 'Contains object when it shouldn''t');
  Self.T.Add(O);
  Check(Self.T.Contains(O), 'Doesn''t contain object when it should');
end;

procedure TestTIdSdpTimes.TestPrintOn;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.T.Add(TIdSdpTime.Create);
    Self.T.Add(TIdSdpTime.Create);
    Self.T[0].StartTime := $cafebabe;
    Self.T[0].EndTime   := $deadbeef;
    Self.T[1].StartTime := 1000000000;
    Self.T[1].EndTime   := 1000000001;

    Self.T.PrintOn(S);

    CheckEquals(#13#10
              + 't=3405691582 3735928559'#13#10
              + 't=1000000000 1000000001',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpZoneAdjustments                                                  *
//******************************************************************************
//* TestTIdSdpZoneAdjustments Public methods ***********************************

procedure TestTIdSdpZoneAdjustments.SetUp;
begin
  inherited SetUp;

  Self.Z := TIdSdpZoneAdjustments.Create;
end;

procedure TestTIdSdpZoneAdjustments.TearDown;
begin
  Self.Z.Free;

  inherited TearDown;
end;

//* TestTIdSdpZoneAdjustments Published methods ********************************

procedure TestTIdSdpZoneAdjustments.TestAddAndCount;
begin
  CheckEquals(0, Self.Z.Count, 'Count on new list');
  Self.Z.Add(TIdSdpZoneAdjustment.Create);
  CheckEquals(1, Self.Z.Count, 'Count after Add()');
  Self.Z.Add(TIdSdpZoneAdjustment.Create);
  CheckEquals(2, Self.Z.Count, 'Count after 2nd Add()');
end;

procedure TestTIdSdpZoneAdjustments.TestClear;
begin
  Self.Z.Add(TIdSdpZoneAdjustment.Create);
  Self.Z.Add(TIdSdpZoneAdjustment.Create);
  Self.Z.Add(TIdSdpZoneAdjustment.Create);

  Self.Z.Clear;
  CheckEquals(0, Self.Z.Count, 'Count after clear');
end;

procedure TestTIdSdpZoneAdjustments.TestContains;
var
  O: TIdSdpZoneAdjustment;
begin
  O := TIdSdpZoneAdjustment.Create;
  Check(not Self.Z.Contains(O), 'Contains object when it shouldn''t');
  Self.Z.Add(O);
  Check(Self.Z.Contains(O), 'Doesn''t contain object when it should');
end;

procedure TestTIdSdpZoneAdjustments.TestPrintOn;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Z.Add(TIdSdpZoneAdjustment.Create);
    Self.Z.Add(TIdSdpZoneAdjustment.Create);
    Self.Z[0].Value := '3405691582 -2s';
    Self.Z[1].Value := '3735928559 5d';

    Self.Z.PrintOn(S);

    CheckEquals(#13#10
              + 'z=3405691582 -2s'#13#10
              + 'z=3735928559 5d',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpPayload                                                          *
//******************************************************************************
//* TestTIdSdpPayload Public methods *******************************************

procedure TestTIdSdpPayload.SetUp;
begin
  inherited SetUp;

  Self.Payload := TIdSdpPayload.Create;
end;

procedure TestTIdSdpPayload.TearDown;
begin
  Self.Payload.Free;

  inherited TearDown;
end;

//* TestTIdSdpPayload Private methods ******************************************

procedure TestTIdSdpPayload.SetToMinimumPayload(P: TIdSdpPayload);
var
  Parser: TIdSdpParser;
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload);
  try
    Parser := TIdSdpParser.Create;
    try
      Parser.Source := S;
      Parser.Parse(P);
    finally
      Parser.Free;
    end;
  finally
    S.Free;
  end;
end;

//* TestTIdSdpPayload Published methods ****************************************

procedure TestTIdSdpPayload.TestAddAttribute;
begin
  Self.Payload.AddAttribute('foo', 'bar');
  CheckEquals(1, Self.Payload.AttributeCount, 'Attribute not added');
//  Self.Payload.AddAttribute('foo', 'bar');
//  CheckEquals(1, Self.Payload.Attributes.Count, 'Existing attribute re-added');
  Self.Payload.AddAttribute('bar', 'foo');
  CheckEquals(2, Self.Payload.AttributeCount, 'Next attribute not added');
end;

procedure TestTIdSdpPayload.TestAddConnection;
var
  I: Integer;
begin
  Self.Payload.AddMediaDescription;
  Self.Payload.AddMediaDescription;

  Self.Payload.AddConnection;

  for I := 0 to Self.Payload.MediaDescriptionCount - 1 do
    CheckEquals(1,
                Self.Payload.MediaDescriptionAt(I).Connections.Count,
                'Media description(' + IntToStr(I)
              + ') didn''t get a connection');
end;

procedure TestTIdSdpPayload.TestAsString;
begin
  Self.SetToMinimumPayload(Self.Payload);

  CheckEquals(MinimumPayload, Self.Payload.AsString, 'AsString');
end;

procedure TestTIdSdpPayload.TestAttributeCount;
begin
  Self.Payload.AddAttribute('foo', 'bar');
  CheckEquals(1, Self.Payload.AttributeCount, 'Attribute not added');
end;

procedure TestTIdSdpPayload.TestConnectionAt;
var
  I: Integer;
begin
  CheckNull(Self.Payload.ConnectionAt(-1),
            '-1 should never return a meaningful connection');
  CheckNull(Self.Payload.ConnectionAt(0),
            'First connection of empty set');

  Self.Payload.AddConnection.TTL := 0;
  Self.Payload.AddConnection.TTL := 1;

  for I := 0 to 1 do
    CheckEquals(I,
                Self.Payload.ConnectionAt(I).TTL,
                'TTL on ' + IntToStr(I) + 'th connection');

  CheckNull(Self.Payload.ConnectionAt(2),
            'Out-of-bounds index');
end;

procedure TestTIdSdpPayload.TestConnectionCount;
var
  I: Integer;
begin
  for I := 0 to 9 do begin
    CheckEquals(I,
                Self.Payload.ConnectionCount,
                'ConnectionCount ' + IntToStr(I));
    Self.Payload.AddConnection;
  end;
end;

procedure TestTIdSdpPayload.TestCreateFromStream;
var
  Dest:    TStringStream;
  Payload: TIdSdpPayload;
  Src:     TStringStream;
begin
  Src := TStringStream.Create(MinimumPayload);
  try
    Payload := TIdSdpPayload.CreateFrom(Src);
    try
      Dest := TStringStream.Create('');
      try
        Payload.PrintOn(Dest);

        CheckEquals(MinimumPayload, Dest.DataString, 'ReadFrom');
      finally
        Dest.Free;
      end;
    finally
      Payload.Free;
    end;
  finally
    Src.Free;
  end;
end;

procedure TestTIdSdpPayload.TestCreateFromStreamString;
var
  Dest:    TStringStream;
  Payload: TIdSdpPayload;
begin
  Payload := TIdSdpPayload.CreateFrom(MinimumPayload);
  try
    Dest := TStringStream.Create('');
    try
      Payload.PrintOn(Dest);

      CheckEquals(MinimumPayload, Dest.DataString, 'ReadFrom');
    finally
      Dest.Free;
    end;
  finally
    Payload.Free;
  end;
end;

procedure TestTIdSdpPayload.TestGetRtpMapAttributes;
var
  Attributes: TIdSdpRTPMapAttributes;
  P:          TIdSdpParser;
  S:          TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'a=rtpmap:96 t140/8000'#13#10
                          + 'm=text 11000 RTP/AVP 98'#13#10
                          + 'a=rtpmap:98 t140/1000'#13#10
                          + 'm=text 12000 RTP/AVP 97 100'#13#10
                          + 'a=rtpmap:97 t140/1000'#13#10
                          + 'a=rtpmap:100 red/1000'#13#10
                          + 'a=fmtp:100 98/98');
  try
    P := TIdSdpParser.Create;
    try
      P.Source := S;
      P.Parse(Self.Payload);

      Attributes := TIdSdpRTPMapAttributes.Create;
      try
        Self.Payload.GetRtpMapAttributes(Attributes);
        CheckEquals(4, Attributes.Count, 'Number of attributes');
        CheckEquals('96 t140/8000', Attributes[0].Value, '1st attribute value');
        CheckEquals('98 t140/1000', Attributes[1].Value, '2nd attribute value');
        CheckEquals('97 t140/1000', Attributes[2].Value, '3rd attribute value');
        CheckEquals('100 red/1000', Attributes[3].Value, '4th attribute value');
      finally
        Attributes.Free;
      end;
    finally
      P.Free
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestInitializeOnEmptySdpPayload;
var
  PT:      TIdRTPPayloadType;
  Profile: TIdRTPProfile;
begin
  Profile := TIdRTPProfile.Create;
  try
    Self.Payload.InitializeProfile(Profile);

    for PT := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do
      CheckEquals(TIdNullPayload.ClassName,
                  Profile.EncodingFor(PT).ClassName,
                  'Encoding for payload type ' + IntToStr(PT));
  finally
    Profile.Free;
  end;
end;

procedure TestTIdSdpPayload.TestInitializeOnSingleMediaSdp;
var
  MD:      TIdSdpMediaDescription;
  Profile: TIdRTPProfile;
  PT:      TIdRTPPayloadType;
begin
  MD := Self.Payload.AddMediaDescription;
  MD.AddAttribute(RTPMapAttribute, '98 t140/1000');

  Profile := TIdRTPProfile.Create;
  try
    Self.Payload.InitializeProfile(Profile);

    for PT := Low(TIdRTPPayloadType) to 97 do
      CheckEquals(TIdNullPayload.ClassName,
                  Profile.EncodingFor(PT).ClassName,
                  'Encoding for payload type ' + IntToStr(PT));

    CheckNotEquals(TIdNullPayload.ClassName,
                   Profile.EncodingFor(98).ClassName,
                   'Encoding for payload type 98');

    for PT := 99 to High(TIdRTPPayloadType) do
      CheckEquals(TIdNullPayload.ClassName,
                  Profile.EncodingFor(PT).ClassName,
                  'Encoding for payload type ' + IntToStr(PT));
  finally
    Profile.Free;
  end;
end;

procedure TestTIdSdpPayload.TestMediaDescriptionAt;
var
  I: Integer;
begin
  CheckNull(Self.Payload.MediaDescriptionAt(-1),
            '-1 should never return a meaningful media description');
  CheckNull(Self.Payload.MediaDescriptionAt(0),
            'First media description of empty set');

  Self.Payload.AddMediaDescription.Port := 0;
  Self.Payload.AddMediaDescription.Port := 1;

  for I := 0 to 1 do
    CheckEquals(I,
                Self.Payload.MediaDescriptionAt(I).Port,
                'Port on ' + IntToStr(I) + 'th media description');

  CheckNull(Self.Payload.MediaDescriptionAt(2),
            'Out-of-bounds index');
end;

procedure TestTIdSdpPayload.TestMediaDescriptionCount;
var
  I: Integer;
begin
  for I := 0 to 9 do begin
    CheckEquals(I,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount ' + IntToStr(I));
    Self.Payload.AddMediaDescription;
  end;
end;

procedure TestTIdSdpPayload.TestMediaDescriptionGetsSessionConnections;
begin
  Self.Payload.AddConnection;
  Self.Payload.AddMediaDescription;

  CheckEquals(1,
              Self.Payload.MediaDescriptionAt(0).Connections.Count,
              'Media description didn''t "inherit" session connection');
end;

procedure TestTIdSdpPayload.TestNewSessionConnectionGetsCopiedToMediaDescriptions;
var
  MD: TIdSdpMediaDescription;
begin
  MD := Self.Payload.AddMediaDescription;
  Self.Payload.AddConnection(TIdSdpConnection.Create);

  CheckEquals(1,
              MD.Connections.Count,
              'No connection added to an existing media description');
end;

procedure TestTIdSdpPayload.TestPrintOnBasic;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);

    Self.Payload.PrintOn(S);

    CheckEquals(MinimumPayload, S.DataString, 'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithAttributes;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.AddAttribute('dead', 'beef');

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10
              + 'a=dead:beef',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithBandwidth;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.Bandwidths.Add(TIdSdpBandwidth.Create);
    Self.Payload.Bandwidths[0].Bandwidth := 13;
    Self.Payload.Bandwidths[0].BandwidthType := btApplicationSpecific;

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10
              + 'b=AS:13',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithEmail;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.EmailAddress.Address := 'azathoth@centre.of.chaos.net';

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'e=azathoth@centre.of.chaos.net'#13#10
              + 'c=IN IP4 224.2.17.12/127',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithInfo;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.Info := 'Like a murder of ravens in fugue';

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'i=Like a murder of ravens in fugue'#13#10
              + 'c=IN IP4 224.2.17.12/127',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithKey;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.Key.KeyType := ktBase64;
    Self.Payload.Key.Value   := '5ideofbeef';

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10
              + 'k=base64:5ideofbeef',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithMediaDescriptions;
var
  MD: TIdSdpMediaDescription;
  S:  TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    MD := Self.Payload.AddMediaDescription;
    MD.AddFormat('98');
    MD.AddAttribute(RTPMapAttribute, '98 t140/1000');

    MD.MediaType := mtText;
    MD.Transport := Id_SDP_RTPAVP;
    MD.Port      := 8000;

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'm=text 8000 RTP/AVP 98'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10
              + 'a=rtpmap:98 t140/1000',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithPhoneNumber;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.PhoneNumber := '+44 404 0000';

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'p=+44 404 0000'#13#10
              + 'c=IN IP4 224.2.17.12/127',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithTimes;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.Times.Add(TIdSdpTime.Create);
    Self.Payload.Times[0].StartTime := 1000000000;
    Self.Payload.Times[0].EndTime   := 1000000001;

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10
              + 't=1000000000 1000000001',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithUri;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.URI := 'mailto:nyarlathotep@crawling.chaos.net';

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'u=mailto:nyarlathotep@crawling.chaos.net'#13#10
              + 'c=IN IP4 224.2.17.12/127',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpParser                                                           *
//******************************************************************************
//* TestTIdSdpParser Public methods ********************************************

procedure TestTIdSdpParser.SetUp;
begin
  inherited SetUp;

  Self.P       := TIdSdpParser.Create;
  Self.Payload := TIdSdpPayload.Create;
end;

procedure TestTIdSdpParser.TearDown;
begin
  Self.Payload.Free;
  Self.P.Free;

  inherited TearDown;
end;

//* TestTIdSdpParser Private methods *******************************************

procedure TestTIdSdpParser.CheckMalformedAttribute(const Value: String);
begin
  Self.CheckMalformedOptionalSessionHeader(RSSDPAttributeName, Value);
end;

procedure TestTIdSdpParser.CheckMalformedConnection(const Value: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'c=' + Value);
  try
    try
      Self.P.Source := S;
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPConnectionName, 'c=' + Value]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.CheckMalformedOrigin(const OriginValue: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=' + OriginValue + #13#10
                          + 's=Minimum Session Info');
  try
    try
      Self.P.Source := S;
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPOriginName, 'o=' + OriginValue]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.CheckMalformedPhoneNumber(const Value: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'p=' + Value);
  try
    try
      Self.P.Source := S;
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPPhoneName, 'p=' + Value]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.CheckMalformedKey(const KeyValue: String);
begin
  Self.CheckMalformedOptionalSessionHeader(RSSDPKeyName, KeyValue);
end;

procedure TestTIdSdpParser.CheckMalformedMediaDescription(const Value: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'm=' + Value + #13#10
                          + 'i=Information');
  try
    Self.P.Source := S;

    try
    Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPMediaDescriptionName, 'm=' + Value]),
                    E.Message,
                    'Unexpected exception');
    end;

  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.CheckMalformedOptionalSessionHeader(const Name, Value: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + Name + '=' + Value);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [Name, Name + '=' + Value]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.CheckMalformedTimeWithRepeat(const RepeatValue: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 't=3034423619 3042462419'#13#10
                          + 'r=' + RepeatValue);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPRepeatName, 'r=' + RepeatValue]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

//* TestTIdSdpParser Published methods *****************************************

procedure TestTIdSdpParser.TestIsAddressType;
begin
  Check(not TIdSdpParser.IsAddressType(''), '''''');
  Check(not TIdSdpParser.IsAddressType('ip4'), 'ip4');
  Check(not TIdSdpParser.IsAddressType(' IP4'), ' IP4');
  Check(    TIdSdpParser.IsAddressType('IP4'), 'IP4');
  Check(    TIdSdpParser.IsAddressType(Id_SDP_IP4), 'Id_SDP_IP4 constant');
  Check(    TIdSdpParser.IsAddressType('IP4'), 'IP6');
  Check(    TIdSdpParser.IsAddressType(Id_SDP_IP6), 'Id_SDP_IP6 constant');
end;

procedure TestTIdSdpParser.TestIsBandwidthType;
begin
  Check(not TIdSdpParser.IsBandwidthType(''),                         '''''');
  Check(not TIdSdpParser.IsBandwidthType('ct'),                       'ct');
  Check(    TIdSdpParser.IsBandwidthType('CT'),                       'CT');
  Check(    TIdSdpParser.IsBandwidthType(Id_SDP_ConferenceTotal),     'Id_SDP_ConferenceTotal constant');
  Check(    TIdSdpParser.IsBandwidthType('AS'),                       'AS');
  Check(    TIdSdpParser.IsBandwidthType(Id_SDP_ApplicationSpecific), 'Id_SDP_ApplicationSpecific constant');
  Check(    TIdSdpParser.IsBandwidthType('RS'),                       'RS');
  Check(    TIdSdpParser.IsBandwidthType(Id_SDP_RS),                  'Id_SDP_RS constant');
  Check(    TIdSdpParser.IsBandwidthType('RR'),                       'RR');
  Check(    TIdSdpParser.IsBandwidthType(Id_SDP_RR),                  'Id_SDP_RR constant');
end;

procedure TestTIdSdpParser.TestIsByteString;
var
  I: Integer;
  S: String;
begin
  Check(not TIdSdpParser.IsByteString(''),  '''''');
  Check(not TIdSdpParser.IsByteString(#13), #13);
  Check(not TIdSdpParser.IsByteString(#10), #10);
  Check(not TIdSdpParser.IsByteString(#0),  #0);

  Check(TIdSdpParser.IsByteString('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'),
        'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789');

  for I := 1 to 9 do
    Check(TIdSdpParser.IsByteString(Chr(I)), '#' + IntToHex(I, 2));

  Check(TIdSdpParser.IsByteString(#11), '#11');
  Check(TIdSdpParser.IsByteString(#12), '#12');

  for I := 14 to 255 do
    Check(TIdSdpParser.IsByteString(Chr(I)), '#' + IntToHex(I, 2));

  S := '';
  for I := 1 to 1000000 do
    S := S + 'a';
  Check(TIdSdpParser.IsByteString(S), '1 million a''s');
end;

procedure TestTIdSdpParser.TestIsKeyData;
begin
  Check(not TIdSdpParser.IsKeyData(''), '''''');
  Check(not TIdSdpParser.IsKeyData('abc'#0), 'abc#0');
  Check(    TIdSdpParser.IsKeyData(' '), 'SP');
  Check(    TIdSdpParser.IsKeyData('~'), '~');
  Check(    TIdSdpParser.IsKeyData('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 '#9'''"-./:?"#$&*;=@[]^_`{|}+~'),
            '''abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 '#9'''"-./:?"#$&*;=@[]^_`{|}+~');
end;

procedure TestTIdSdpParser.TestIsKeyType;
begin
  Check(not TIdSdpParser.IsKeyType(''),            '''''');
  Check(not TIdSdpParser.IsKeyType('PROMPT'),      'PROMPT');
  Check(    TIdSdpParser.IsKeyType('clear'),       'clear');
  Check(    TIdSdpParser.IsKeyType(Id_SDP_Clear),  'Id_SDP_Clear constant');
  Check(    TIdSdpParser.IsKeyType('base64'),      'base64');
  Check(    TIdSdpParser.IsKeyType(Id_SDP_Base64), 'Id_SDP_Base64 constant');
  Check(    TIdSdpParser.IsKeyType('uri'),         'uri');
  Check(    TIdSdpParser.IsKeyType(Id_SDP_URI),    'Id_SDP_URI constant');
  Check(    TIdSdpParser.IsKeyType('prompt'),      'prompt');
  Check(    TIdSdpParser.IsKeyType(Id_SDP_Prompt), 'Id_SDP_Prompt constant');
end;

procedure TestTIdSdpParser.TestIsMediaType;
begin
  Check(not TIdSdpParser.IsMediaType(''),                        '''''');
  Check(not TIdSdpParser.IsMediaType('Audio'),                   'Audio');
  Check(    TIdSdpParser.IsMediaType('audio'),                   'audio');
  Check(    TIdSdpParser.IsMediaType(RSSDPMediaTypeAudio),       'RSSDPMediaTypeAudio constant');
  Check(    TIdSdpParser.IsMediaType('video'),                   'video');
  Check(    TIdSdpParser.IsMediaType(RSSDPMediaTypeVideo),       'RSSDPMediaTypeVideo constant');
  Check(    TIdSdpParser.IsMediaType('application'),             'application');
  Check(    TIdSdpParser.IsMediaType(RSSDPMediaTypeApplication), 'RSSDPMediaTypeApplication constant');
  Check(    TIdSdpParser.IsMediaType('data'),                    'data');
  Check(    TIdSdpParser.IsMediaType(RSSDPMediaTypeData),        'RSSDPMediaTypeData constant');
  Check(    TIdSdpParser.IsMediaType('control'),                 'control');
  Check(    TIdSdpParser.IsMediaType(RSSDPMediaTypeControl),     'RSSDPMediaTypeControl constant');
end;

procedure TestTIdSdpParser.TestIsMulticastAddress;
begin
  // ipv4
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, ''),                'IPv4: ''''');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, '1'),               'IPv4: 1');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, 'abcd'),            'IPv4: abcd');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.'),            'IPv4: 224.');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, '127.2.17.12'),     'IPv4: 127.2.17.12');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.2.17.12'),     'IPv4: 224.2.17.12');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.0.0.0'),       'IPv4: 224.0.0.0');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.255.255.255'), 'IPv4: 224.255.255.255');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.255.255.256'), 'IPv4: 224.255.255.256');

  // ipv6
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv6, ''), 'IPv6: ''''');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv6, '1'),                          'IPv6: 1');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv6, '224.'),                       'IPv6: 224.');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv6, '224.2.17.12'),                'IPv6: 224.2.17.12');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv6, '2002:c058:6301::'),           'IPv6: 2002:c058:6301::');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv6, '1080:0:0:0:8:800:200C:417A'), 'IPv6: 1080:0:0:0:8:800:200C:417A');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv6, 'FF02:c058:6301::'),           'IPv6: FF02:c058:6301::');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv6, 'Ff00:0:0:0:0:0:0:101'),       'IPv6: Ff00:0:0:0:0:0:0:101');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv6, 'ff01:0:0:0:0:0:0:101'),       'IPv6: ff01:0:0:0:0:0:0:101');
end;

procedure TestTIdSdpParser.TestIsNetType;
begin
  Check(not TIdSdpParser.IsNetType(''),        '''''');
  Check(not TIdSdpParser.IsNetType('in'),      'in');
  Check(    TIdSdpParser.IsNetType('IN'),      'IN');
  Check(    TIdSdpParser.IsNetType(Id_SDP_IN), 'Id_SDP_IN constant');

end;

procedure TestTIdSdpParser.TestIsPhone;
begin
  Check(not TIdSdpParser.IsPhone(''),                   '''''');
  Check(not TIdSdpParser.IsPhone('+-1'),                '+-1');
  Check(not TIdSdpParser.IsPhone('+1'),                 '+1');
  Check(    TIdSdpParser.IsPhone('+1 '),                '+1 ');
  Check(    TIdSdpParser.IsPhone('+1-'),                '+1-');
  Check(    TIdSdpParser.IsPhone('+1-2-3'),             '+1-2-3');
  Check(    TIdSdpParser.IsPhone('+1-2 3 '),            '+1-2 3 SP');
  Check(    TIdSdpParser.IsPhone('+44-870  154 0 154'), '+44-870  154 0 154');
end;

procedure TestTIdSdpParser.TestIsPhoneNumber;
begin
  Check(not TIdSdpParser.IsPhoneNumber(''),                   '''''');
  Check(not TIdSdpParser.IsPhoneNumber('+-1'),                '+-1');
  Check(not TIdSdpParser.IsPhoneNumber('+1'),                 '+1');
  Check(    TIdSdpParser.IsPhoneNumber('+1 '),                '+1 SP');
  Check(    TIdSdpParser.IsPhoneNumber('+1-'),                '+1-');
  Check(    TIdSdpParser.IsPhoneNumber('+1-2-3'),             '+1-2-3');
  Check(    TIdSdpParser.IsPhoneNumber('+1-2 3 '),            '+1-2 3 SP');
  Check(    TIdSdpParser.IsPhoneNumber('+44-870  154 0 154'), '+44-870  154 0 154');

  Check(TIdSdpParser.IsPhoneNumber('"Quoted name" <+44 870 154 0 154>'),
        '"Quoted name" <+44 870 154 0 154>');
  Check(TIdSdpParser.IsPhoneNumber('aheh "Quoted name" <+44 870 154 0 154>'),
        '"aheh Quoted name" <+44 870 154 0 154>');
  Check(TIdSdpParser.IsPhoneNumber('+44 870 154 0 154'),
        '+44 870 154 0 154');
  Check(TIdSdpParser.IsPhoneNumber('+44 870 154 0 154 (Quoted name)'),
        '+44 870 154 0 154 (Quoted name)');
  Check(not TIdSdpParser.IsPhoneNumber('+44 870 154 0 154 (Quoted name) fink'),
        '+44 870 154 0 154 (Quoted name) fink');
end;

procedure TestTIdSdpParser.TestIsPort;
begin
  Check(not TIdSdpParser.IsPort(''), '''''');
  Check(not TIdSdpParser.IsPort('a'), 'a');
  Check(not TIdSdpParser.IsPort('1a'), '1a');
  Check(not TIdSdpParser.IsPort(#0), '#0');
  Check(not TIdSdpParser.IsPort('1 '), '1 SP');
  Check(not TIdSdpParser.IsPort(''), '65536');
  Check(not TIdSdpParser.IsPort('1'#0), '1#0');
  Check(    TIdSdpParser.IsPort('1'), '1');
  Check(    TIdSdpParser.IsPort('65535'), '65535');
  Check(    TIdSdpParser.IsPort('666'), '666');
end;

procedure TestTIdSdpParser.TestIsText;
var
  C: Char;
begin
  Check(not TIdSdpParser.IsText(''),         '''''');
  Check(    TIdSdpParser.IsText('abc'),      'abc');
  Check(not TIdSdpParser.IsText('hello'#0),  'hello#0');
  Check(not TIdSdpParser.IsText('hello'#10), 'hello#10');
  Check(not TIdSdpParser.IsText('hello'#13), 'hello#13');

  for C := Low(Char) to High(Char) do
    if not (C in [#0, #10, #13]) then
      Check(TIdSdpParser.IsText(C), 'Checking Ord(C) = ' + IntToStr(Ord(C)));
end;

procedure TestTIdSdpParser.TestIsTime;
begin
  Check(not TIdSdpParser.IsTime(''),                     '''''');
  Check(not TIdSdpParser.IsTime('a'),                    'a');
  Check(not TIdSdpParser.IsTime('d'),                    'd');
  Check(not TIdSdpParser.IsTime('99a'),                  '99a');
  Check(not TIdSdpParser.IsTime('1d1'),                  '1d1');
  Check(not TIdSdpParser.IsTime('1dd'),                  '1dd');
  Check(    TIdSdpParser.IsTime('98765432109876543210'), '98765432109876543210');
  Check(    TIdSdpParser.IsTime('1d'),                   '1d');
  Check(    TIdSdpParser.IsTime('1000h'),                '1000h');
  Check(    TIdSdpParser.IsTime('1000m'),                '1000m');
  Check(    TIdSdpParser.IsTime('1000s'),                '1000s');
end;

procedure TestTIdSdpParser.TestIsTransport;
begin
  Check(not TIdSdpParser.IsTransport(''),            '''''');
  Check(not TIdSdpParser.IsTransport('vAt'),         'vAt');
  Check(not TIdSdpParser.IsTransport('rapunzel'),    'rapunzel');
  Check(    TIdSdpParser.IsTransport('RTP/AVP'),     'RTP/AVP');
  Check(    TIdSdpParser.IsTransport(Id_SDP_RTPAVP), 'Id_SDP_RTPAVP constant');
  Check(    TIdSdpParser.IsTransport('vat'),         'vat');
  Check(    TIdSdpParser.IsTransport(Id_SDP_vat),    'Id_SDP_vat constant');
  Check(    TIdSdpParser.IsTransport('rtp'),         'rtp');
  Check(    TIdSdpParser.IsTransport(Id_SDP_rtp),    'Id_SDP_rtp constant');
  Check(    TIdSdpParser.IsTransport('UDPTL'),       'UDPTL');
  Check(    TIdSdpParser.IsTransport(Id_SDP_UDPTL),  'Id_SDP_UDPTL constant');
  Check(    TIdSdpParser.IsTransport('TCP'),         'TCP');
  Check(    TIdSdpParser.IsTransport(Id_SDP_TCP),    'Id_SDP_TCP constant');
end;

procedure TestTIdSdpParser.TestParseAttribute;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'a=recvonly');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,          Self.Payload.AttributeCount,    'AttributeCount');
    CheckEquals('recvonly', Self.Payload.AttributeAt(0).Name,  'Attribute.Name');
    CheckEquals('',         Self.Payload.AttributeAt(0).Value, 'Attribute.Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseAttributeWithValue;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'a=orient:landscape');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,           Self.Payload.AttributeCount,    'AttributeCount');
    CheckEquals('orient',    Self.Payload.AttributeAt(0).Name,  'Attribute.Name');
    CheckEquals('landscape', Self.Payload.AttributeAt(0).Value, 'Attribute.Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseAttributeMalformedName;
begin
  Self.CheckMalformedAttribute('a=real-dash');
end;

procedure TestTIdSdpParser.TestParseAttributeMalformedValue;
begin
  Self.CheckMalformedAttribute('a=bytestring:it is not#0one at all');
end;

procedure TestTIdSdpParser.TestParseBandwidth;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'b=RR:666');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,     Self.Payload.Bandwidths.Count,            'Bandwidths.Count');
    Check      (btRR = Self.Payload.Bandwidths[0].BandwidthType, 'BandwidthType');
    CheckEquals(666,   Self.Payload.Bandwidths[0].Bandwidth,     'Bandwidth');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseBandwidthMalformed;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'b=:');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out on a malformed bandwidth');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPBandwidthName, 'b=:']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseBandwidthMultipleHeaders;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'b=RR:666'#13#10
                          + 'b=CT:123');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(2,                  Self.Payload.Bandwidths.Count, 'Bandwidths.Count');
    Check      (btRR =              Self.Payload.Bandwidths[0].BandwidthType, '[0].BandwidthType');
    CheckEquals(666,                Self.Payload.Bandwidths[0].Bandwidth,     '[0].Bandwidth');
    Check      (btConferenceTotal = Self.Payload.Bandwidths[1].BandwidthType, '[1].BandwidthType');
    CheckEquals(123,                Self.Payload.Bandwidths[1].Bandwidth,     '[1].Bandwidth');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseConnectionInSessionAndMediaDescription;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.11/127'); // note the difference compared with the session c.
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('IN',          Self.Payload.ConnectionAt(0).NetType,     'NetType');
    Check      (Id_IPv4 =      Self.Payload.ConnectionAt(0).AddressType, 'AddressType');
    CheckEquals('224.2.17.12', Self.Payload.ConnectionAt(0).Address,     'Address');
    CheckEquals(127,           Self.Payload.ConnectionAt(0).TTL,         'TTL');

    CheckEquals(1,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount');
    CheckEquals(2,
                Self.Payload.MediaDescriptionAt(0).Connections.Count,
                'MediaDescriptionAt(0).Connections.Count');
    CheckEquals('IN',
                Self.Payload.MediaDescriptionAt(0).Connections[0].NetType,
                'Connection.NetType');
    Check(Id_IPv4 = Self.Payload.MediaDescriptionAt(0).Connections[0].AddressType,
          'Connection.AddressType');
    CheckEquals('224.2.17.11',
                Self.Payload.MediaDescriptionAt(0).Connections[0].Address,
                'Connection.Address');
    CheckEquals(127,
                Self.Payload.MediaDescriptionAt(0).Connections[0].TTL,
                'Connection.TTL');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseConnectionMalformed;
begin
  Self.CheckMalformedConnection('c=IN');
end;

procedure TestTIdSdpParser.TestParseConnectionMalformedMulticastAddress;
begin
  Self.CheckMalformedConnection('c=IN IP4 224.2.17.12/a');
  Self.CheckMalformedConnection('c=IN IP4 224.2.17.12');
  Self.CheckMalformedConnection('c=IN IP4 127.2.17.12/127');
  Self.CheckMalformedConnection('c=IN IP6 224.2.17.12/127');
end;

procedure TestTIdSdpParser.TestParseConnectionMalformedUnicastAddress;
begin
  Self.CheckMalformedConnection('c=IN IP4 127.0.0.1/127');
  Self.CheckMalformedConnection('c=IN IP4 127.0.0.1/127/2');
end;

procedure TestTIdSdpParser.TestParseConnectionMulticastAddress;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'c=IN IP4 224.2.17.12/127');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('IN',          Self.Payload.ConnectionAt(0).NetType,     'NetType');
    Check      (Id_IPv4 =      Self.Payload.ConnectionAt(0).AddressType, 'AddressType');
    CheckEquals('224.2.17.12', Self.Payload.ConnectionAt(0).Address,     'Address');
    CheckEquals(127,           Self.Payload.ConnectionAt(0).TTL,         'TTL');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseConnectionMulticastAddressWithMultiple;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'c=IN IP4 224.2.17.12/127/3');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(3,
                Self.Payload.ConnectionCount,
                'Connections.Count');
    CheckEquals('IN',
                Self.Payload.ConnectionAt(0).NetType,
                'NetType');
    Check(Id_IPv4 = Self.Payload.ConnectionAt(0).AddressType,
          'AddressType');
    CheckEquals('224.2.17.12',
                Self.Payload.ConnectionAt(0).Address,
                'Address 1');
    CheckEquals('224.2.17.13',
                Self.Payload.ConnectionAt(1).Address,
                'Address 2');
    CheckEquals('224.2.17.14',
                Self.Payload.ConnectionAt(2).Address,
                'Address 3');
    CheckEquals(127,
                Self.Payload.ConnectionAt(0).TTL,
                'TTL');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseConnectionMulticastIPv6AddressWithMultiple;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'c=IN IP6 FF02:5156:4019:2::FFFF/127/3');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(3,
                Self.Payload.ConnectionCount,
                'Connections.Count');
    CheckEquals('IN',
                Self.Payload.ConnectionAt(0).NetType,
                'NetType');
    Check(Id_IPv6 = Self.Payload.ConnectionAt(0).AddressType,
          'AddressType');
    CheckEquals('FF02:5156:4019:2:0:0:0:FFFF',
                Self.Payload.ConnectionAt(0).Address,
                'Address 1');
    CheckEquals('FF02:5156:4019:2:0:0:1:0',
                Self.Payload.ConnectionAt(1).Address,
                'Address 2');
    CheckEquals('FF02:5156:4019:2:0:0:1:1',
                Self.Payload.ConnectionAt(2).Address,
                'Address 3');
    CheckEquals(127,
                Self.Payload.ConnectionAt(0).TTL,
                'TTL');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseConnectionUnknownAddrType;
begin
  Self.CheckMalformedConnection('c=IN IP5 224.2.17.12/127');
end;

procedure TestTIdSdpParser.TestParseConnectionUnknownNetType;
begin
  Self.CheckMalformedConnection('c=ON IP4 224.2.17.12/127');
end;

procedure TestTIdSdpParser.TestParseConnectionUnicastAddress;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'c=IN IP4 127.2.17.12');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('IN',          Self.Payload.ConnectionAt(0).NetType,     'NetType');
    Check      (Id_IPv4 =      Self.Payload.ConnectionAt(0).AddressType, 'AddressType');
    CheckEquals('127.2.17.12', Self.Payload.ConnectionAt(0).Address,     'Address');
    CheckEquals(0,             Self.Payload.ConnectionAt(0).TTL,         'TTL');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseEmail;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'e=nihilistic@mystics.net'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('nihilistic@mystics.net', Self.Payload.EmailAddress.Text, 'EmailAddress');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseEmailBracketedName;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'e=nihilistic@mystics.net (Apostolic alcoholics)'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    // note that "Apostolic alcoholics" is a COMMENT, not a display name.
    CheckEquals('nihilistic@mystics.net', Self.Payload.EmailAddress.Text, 'EmailAddress.Text');
    CheckEquals('',                       Self.Payload.EmailAddress.Name, 'EmailAddress.Name');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseEmailRfc822;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'e="Apostolic alcoholics" <nihilistic@mystics.net>'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    CheckEquals('Apostolic alcoholics <nihilistic@mystics.net>',
                Self.Payload.EmailAddress.Text,
                'EmailAddress.Text');

    CheckEquals('Apostolic alcoholics',
                Self.Payload.EmailAddress.Name,
                'EmailAddress.Name');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseEmptyStream;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with empty input stream');
    except
      on E: EParser do
        CheckEquals(EmptyInputStream, E.Message, 'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseHeaderMissingName;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + '=Missing name');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(Format(UnknownOptionalHeader, ['=Missing name']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseHeaderMissingValue;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'b=');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, ['b', 'b=']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseHeadersInvalidOrder;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=Minimum Session Info'#13#10
                          + 'u=http://127.0.0.1/'#13#10
                          + 'i=My u & i headers are swopped round');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out: vosui');
    except
      on E: EParser do
        CheckEquals(Format(BadHeaderOrder, [RSSDPInformationName, RSSDPUriName]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;

  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'b=RR:666'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'b=CT:666');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out: vosbcb');
    except
      on E: EParser do
        CheckEquals(Format(BadHeaderOrder, [RSSDPConnectionName, RSSDPBandwidthName]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseInfo;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'i=An optional header'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('An optional header', Self.Payload.Info, 'Info');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseInfoIllegalCharacters;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=f00'#13#10
                          + 'i=haha'#0'haha');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out on malformed information-field');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPInformationName, 'i=haha'#0'haha']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseKey;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'k=uri:sip:wintermute@tessier-ashpool.co.luna');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    Check(ktUri =                                       Self.Payload.Key.KeyType, 'KeyType');
    CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.Payload.Key.Value,   'Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseKeyMalformedPrompt;
begin
  Self.CheckMalformedKey('prompt:sip:wintermute@tessier-ashpool.co.luna');
end;

procedure TestTIdSdpParser.TestParseKeyMalformedBase64;
begin
  Self.CheckMalformedKey('base64');
end;

procedure TestTIdSdpParser.TestParseKeyMalformedClear;
begin
  Self.CheckMalformedKey('clear');
end;

procedure TestTIdSdpParser.TestParseKeyMalformedUri;
begin
  Self.CheckMalformedKey('uri');
end;

procedure TestTIdSdpParser.TestParseKeyUnknownKeyType;
begin
  Self.CheckMalformedKey('base46');
end;

procedure TestTIdSdpParser.TestParseLinphoneSessionDescription;
var
  S: TStringStream;
begin
  // Note the space in the bandwidth header. We just rip out the first token
  // after the colon and ignore the rest.
  S := TStringStream.Create('v=0'#13#10
                          + 'o=frank 123456 654321 IN IP4 192.168.0.2'#13#10
                          + 's=A conversation'#13#10
                          + 'c=IN IP4 192.168.0.2'#13#10
                          + 't=0 0'#13#10
                          + 'm=audio 7078 RTP/AVP 110 115 101'#13#10
                          + 'b=AS:110 8'#13#10
                          + 'a=rtpmap:110 speex/8000/1'#13#10
                          + 'a=rtpmap:115 1015/8000/1'#13#10
                          + 'a=rtpmap:101 telephone-event/8000'#13#10
                          + 'a=fmtp:101 0-11');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    Check(Self.Payload.MediaDescriptionCount > 0,
          'Insuffient media descriptions');
    Check(Self.Payload.MediaDescriptionAt(0).Bandwidths.Count > 0,
          'Insuffient bandwidths');
    CheckEquals(110,
                Self.Payload.MediaDescriptionAt(0).Bandwidths[0].Bandwidth,
                'Bandwidth');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescription;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'm=data 6666 RTP/AVP 1'#13#10
                          + 'i=Information');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1, Self.Payload.MediaDescriptionCount, 'MediaDescriptionCount');

    Check(mtData = Self.Payload.MediaDescriptionAt(0).MediaType,
          'MediaDescriptionAt(0).MediaType');
    CheckEquals(6666,
                Self.Payload.MediaDescriptionAt(0).Port,
                'MediaDescriptionAt(0).Port');
    CheckEquals(1,
                Self.Payload.MediaDescriptionAt(0).PortCount,
                'MediaDescriptionAt(0).PortCount');
    CheckEquals('RTP/AVP',
                Self.Payload.MediaDescriptionAt(0).Transport,
                'MediaDescriptionAt(0).Transport');
    CheckEquals('Information',
                Self.Payload.MediaDescriptionAt(0).Info,
                'MediaDescriptionAt(0).Info');
    CheckEquals(1,
                Self.Payload.MediaDescriptionAt(0).FormatCount,
                'MediaDescriptionAt(0).FormatCount');
    CheckEquals('1',
                Self.Payload.MediaDescriptionAt(0).Formats[0],
                'MediaDescriptionAt(0).Formats[0]');
    Check(Self.Payload.MediaDescriptionAt(0).HasConnection,
         'MediaDescriptionAt(0).HasConnection');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithSessionConnections;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'c=IN IP4 127.0.0.1'#13#10
                          + 'm=data 6666 RTP/AVP 1'#13#10
                          + 'i=Information');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    CheckEquals(2,
                Self.Payload.MediaDescriptionAt(0).Connections.Count,
                'Media description connection count');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionMalformedFormatList;
begin
  Self.CheckMalformedMediaDescription('data 65536 RTP/AVP 1 2 3 -1');
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionMissingInformation;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'c=IN IP4 224.2.17.12/127');
  try
    Self.P.Source := S;
    Self.P.Parse(Self.Payload);
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionMissingKey;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'c=IN IP4 224.2.17.12/127');
  try
    Self.P.Source := S;
    Self.P.Parse(Self.Payload);

    Check(Self.Payload.MediaDescriptionCount > 0,
          'No media descriptions');
    Check(not Self.Payload.MediaDescriptionAt(0).HasKey, 'HasKey');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionMalformedPort;
begin
  Self.CheckMalformedMediaDescription('data 65536 RTP/AVP 1');
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionMissingFormatList;
begin
  Self.CheckMalformedMediaDescription('data 65535 RTP/AVP');
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionMissingPort;
begin
  Self.CheckMalformedMediaDescription('data RTP/AVP 1');
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionsMissingSessionConnection;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'b=RR:666'#13#10
                          + 'a=recvonly'#13#10
                          + 'a=T38FaxTranscodingJBIG'#13#10
                          + 'a=type:broadcast');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1, Self.Payload.MediaDescriptionCount, 'MediaDescriptionCount');
    Check(Self.Payload.MediaDescriptionAt(0).HasConnection, 'MediaDescriptionAt(0) connection');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptions;
var
  I: Integer;
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'm=video 49170/1 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'm=video 49171/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'm=video 49172/3 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'm=video 49173/4 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(4,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount');

    for I := 0 to 3 do begin
      Check(Self.Payload.MediaDescriptionAt(I).HasConnection,
            'MediaDescriptions[' + IntToStr(I) + '] connection');

      Check(mtVideo = Self.Payload.MediaDescriptionAt(I).MediaType,
            'MediaDescriptions[' + IntToStr(I) + '].MediaType');



    CheckEquals(49170 + I,
                Self.Payload.MediaDescriptionAt(I).Port,
                'MediaDescriptions[' + IntToStr(I) + '].Port');
    CheckEquals(1 + I,
                Self.Payload.MediaDescriptionAt(I).PortCount,
                'MediaDescriptions[' + IntToStr(I) + '].PortCount');
    CheckEquals('RTP/AVP',
                Self.Payload.MediaDescriptionAt(I).Transport,
                'MediaDescriptions[' + IntToStr(I) + '].Transport');
    CheckEquals('More information than you can shake a stick at',
                Self.Payload.MediaDescriptionAt(I).Info,
                'MediaDescriptions[' + IntToStr(I) + '].Info');
    CheckEquals(1,
                Self.Payload.MediaDescriptionAt(I).FormatCount,
                'MediaDescriptions[' + IntToStr(I) + '].FormatCount');
    CheckEquals('31',
                Self.Payload.MediaDescriptionAt(I).Formats[0],
                'MediaDescriptions[' + IntToStr(I) + '].Formats[0]');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionUnknownMediaType;
begin
  Self.CheckMalformedMediaDescription('date 65535 RTP/AVP 1');
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionUnknownTransport;
begin
  Self.CheckMalformedMediaDescription('data 6666 RtP/AVP 1');
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithAttributes;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'b=RR:666'#13#10
                          + 'a=recvonly'#13#10
                          + 'a=T38FaxTranscodingJBIG'#13#10
                          + 'a=type:broadcast');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(0, Self.Payload.AttributeCount,                       'Session attributes');
    CheckEquals(1, Self.Payload.MediaDescriptionCount,                'MediaDescriptionCount');
    CheckEquals(3, Self.Payload.MediaDescriptionAt(0).AttributeCount, 'AttributeCount');

    CheckEquals('recvonly',
                Self.Payload.MediaDescriptionAt(0).AttributeAt(0).Name,
                'AttributeAt(0).Name');
    CheckEquals('',
                Self.Payload.MediaDescriptionAt(0).AttributeAt(0).Value,
                'AttributeAt(0).Value');
    CheckEquals('T38FaxTranscodingJBIG',
                Self.Payload.MediaDescriptionAt(0).AttributeAt(1).Name,
                'AttributeAt(1).Name');
    CheckEquals('',
                Self.Payload.MediaDescriptionAt(0).AttributeAt(1).Value,
                'AttributeAt(1).Value');
    CheckEquals('type',
                Self.Payload.MediaDescriptionAt(0).AttributeAt(2).Name,
                'AttributeAt(2).Name');
    CheckEquals('broadcast',
                Self.Payload.MediaDescriptionAt(0).AttributeAt(2).Value,
                'AttributeAt(2).Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithBandwidth;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'b=RR:666');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(0,     Self.Payload.Bandwidths.Count,                                 'Bandwidths.Count');
    CheckEquals(1,     Self.Payload.MediaDescriptionCount,                          'MediaDescriptionCount');
    CheckEquals(1,     Self.Payload.MediaDescriptionAt(0).Bandwidths.Count,            'MediaDescriptions.Bandwidths.Count');
    Check      (btRR = Self.Payload.MediaDescriptionAt(0).Bandwidths[0].BandwidthType, 'BandwidthType');
    CheckEquals(666,   Self.Payload.MediaDescriptionAt(0).Bandwidths[0].Bandwidth,     'Bandwidth');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithConnection;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.14/127/2');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    CheckEquals(1,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount');
    Check(Self.Payload.MediaDescriptionAt(0).HasConnection,
          'HasConnection');
    CheckEquals(3,
                Self.Payload.MediaDescriptionAt(0).Connections.Count,
                'MediaDescriptionAt(0).Connections.Count');
    CheckEquals('IN',
                Self.Payload.MediaDescriptionAt(0).Connections[1].NetType,
                'Connection[1].NetType');
    Check(Id_IPv4 = Self.Payload.MediaDescriptionAt(0).Connections[1].AddressType,
          'Connection[1].AddressType');
    CheckEquals('224.2.17.14',
                Self.Payload.MediaDescriptionAt(0).Connections[0].Address,
                'Connection[0].Address');
    CheckEquals('224.2.17.15',
                Self.Payload.MediaDescriptionAt(0).Connections[1].Address,
                'Connection[1].Address');
    CheckEquals('224.2.17.12',
                Self.Payload.MediaDescriptionAt(0).Connections[2].Address,
                'Connection[2].Address');
    CheckEquals(127,
                Self.Payload.MediaDescriptionAt(0).Connections[1].TTL,
                'Connection[1].TTL');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithKey;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'k=uri:sip:wintermute@tessier-ashpool.co.luna');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    CheckEquals(1,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount');
    Check(Self.Payload.MediaDescriptionAt(0).HasKey,
          'MediaDescriptions.HasKey');
    Check(ktUri = Self.Payload.MediaDescriptionAt(0).Key.KeyType,
          'KeyType');

    CheckEquals('sip:wintermute@tessier-ashpool.co.luna',
                Self.Payload.MediaDescriptionAt(0).Key.Value,
                'Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithMultipleFormats;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'm=video 49170/2 RTP/AVP 31 23 ape convolution 3vilution'#13#10
                          + 'i=Information');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,             Self.Payload.MediaDescriptionCount,          'MediaDescriptionCount');
    CheckEquals(5,             Self.Payload.MediaDescriptionAt(0).FormatCount, 'MediaDescriptionAt(0).FormatCount');
    CheckEquals('31',          Self.Payload.MediaDescriptionAt(0).Formats[0],  'MediaDescriptionAt(0).Formats[0]');
    CheckEquals('23',          Self.Payload.MediaDescriptionAt(0).Formats[1],  'MediaDescriptionAt(0).Formats[1]');
    CheckEquals('ape',         Self.Payload.MediaDescriptionAt(0).Formats[2],  'MediaDescriptionAt(0).Formats[2]');
    CheckEquals('convolution', Self.Payload.MediaDescriptionAt(0).Formats[3],  'MediaDescriptionAt(0).Formats[3]');
    CheckEquals('3vilution',   Self.Payload.MediaDescriptionAt(0).Formats[4],  'MediaDescriptionAt(0).Formats[4]');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithPortCount;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'm=video 49170/2 RTP/AVP 2 2'#13#10
                          + 'i=Information');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1, Self.Payload.MediaDescriptionCount, 'MediaDescriptionCount');

    Check      (mtVideo      = Self.Payload.MediaDescriptionAt(0).MediaType, 'MediaDescriptionAt(0).MediaType');
    CheckEquals(49170,         Self.Payload.MediaDescriptionAt(0).Port,      'MediaDescriptionAt(0).Port');
    CheckEquals(2,             Self.Payload.MediaDescriptionAt(0).PortCount, 'MediaDescriptionAt(0).PortCount');
    CheckEquals('RTP/AVP',     Self.Payload.MediaDescriptionAt(0).Transport, 'MediaDescriptionAt(0).Transport');
    CheckEquals('Information', Self.Payload.MediaDescriptionAt(0).Info,      'MediaDescriptionAt(0).Info');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMinimumPayload;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(0,                      Self.Payload.Version,     'Version');
    CheckEquals('Minimum Session Info', Self.Payload.SessionName, 'SessionName');

    CheckEquals('mhandley',    Self.Payload.Origin.UserName,       'Origin.Username');
    CheckEquals('2890844526',  Self.Payload.Origin.SessionID,      'Origin.SessionID');
    CheckEquals('2890842807',  Self.Payload.Origin.SessionVersion, 'Origin.SessionVersion');
    CheckEquals('IN',          Self.Payload.Origin.NetType,        'Origin.NetType');
    Check      (Id_IPV4 =      Self.Payload.Origin.AddressType,    'Origin.AddressType');
    CheckEquals('126.16.64.4', Self.Payload.Origin.Address,        'Origin.Address');

    CheckEquals('IN',          Self.Payload.ConnectionAt(0).NetType,     'Connection.NetType');
    Check      (Id_IPv4      = Self.Payload.ConnectionAt(0).AddressType, 'Connection.AddressType');
    CheckEquals('224.2.17.12', Self.Payload.ConnectionAt(0).Address,     'Connection.Address');
    CheckEquals(127,           Self.Payload.ConnectionAt(0).TTL,         'Connection.TTL');

    CheckEquals('', Self.Payload.Info, 'Info');
    CheckEquals(0, Self.Payload.Times.Count, 'Times.Count');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMissingOrigin;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=1'#13#10
                          + 's=Missing Origin. Like We Bad Syntax');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with missing origin-field');
    except
      on E: EParser do
        CheckEquals(MissingOrigin, E.Message, 'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMissingSession;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=1'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 'i=Session Information');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with missing session-name-field');
    except
      on E: EParser do
        CheckEquals(MissingSessionName, E.Message, 'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMissingSessionConnection;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(MissingConnection,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMissingVersion;
var
  S: TStringStream;
begin
  S := TStringStream.Create('o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with missing proto-version');
    except
      on E: EParser do
        CheckEquals(MissingVersion, E.Message, 'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseOriginIPv6Address;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP6 2002:5156:4019:1::1'#13#10
                          + 's=Minimum Session Info'#13#10
                          + 'c=IN IP6 2002:5156:4019:1::1');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('2002:5156:4019:1::1',
                Self.Payload.Origin.Address,
                'Address');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseOriginMalformed;
begin
  Self.CheckMalformedOrigin('mhandley');
end;

procedure TestTIdSdpParser.TestParseOriginMalformedUserName;
begin
  Self.CheckMalformedOrigin('mhand ley 2890844526 2890842807 IN IP4 126.16.64.4');
end;

procedure TestTIdSdpParser.TestParseOriginMalformedSessionID;
begin
  Self.CheckMalformedOrigin('mhandley 28a90844526 2890842807 IN IP4 126.16.64.4');
end;

procedure TestTIdSdpParser.TestParseOriginMalformedSessionVersion;
begin
  Self.CheckMalformedOrigin('mhandley 2890844526 28a90842807 IN IP4 126.16.64.4');
end;

procedure TestTIdSdpParser.TestParseOriginMalformedNetType;
begin
  Self.CheckMalformedOrigin('mhandley 2890844526 2890842807 in IP4 126.16.64.4');
end;

procedure TestTIdSdpParser.TestParsePhoneNumber;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'p=Ziggy Stardust <+99 666 0942-3>'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('Ziggy Stardust <+99 666 0942-3>',
                Self.Payload.PhoneNumber,
                'PhoneNumber');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParsePhoneNumberMultipleHeaders;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'p=Ziggy Stardust <+99 666 0942-3>'#13#10
                          + 'p=Ziggy Stardust <+99 666 0942-3>'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with multiple Phone headers');
    except
      on E: EParser do
        CheckEquals(Format(TooManyHeaders, [RSSDPPhoneName]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParsePhoneNumberWithAngleBracketsButNoName;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'p=<+99 666 0942-3>'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('<+99 666 0942-3>',
                Self.Payload.PhoneNumber,
                'PhoneNumber');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParsePhoneNumberWithStuffOutsideComment;
begin
  Self.CheckMalformedPhoneNumber('+99 666 0942-3 (Ziggy Stardust) oh no! it''s garbage!');
end;

procedure TestTIdSdpParser.TestParsePhoneNumberWithUnsafeChars;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'p=+99 666 0942-3'#0#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPPhoneName, 'p=+99 666 0942-3'#0]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseRedefinedPayloadType;
var
  Profile: TIdRTPProfile;
  S:       TStringStream;
begin
  // RFC 3388 says
  //   An SDP session description typically contains one or more media lines
  //   - they are commonly known as "m" lines.  When a session description
  //   contains more than one "m" line, SDP does not provide any means to
  //   express a particular relationship between two or more of them.  When
  //   an application receives an SDP session description with more than one
  //   "m" line, it is up to the application what to do with them.  SDP does
  //   not carry any information about grouping media streams.
  //
  // We assume therefore that the application (us) can treat the SDP below
  // as we see fit. Note that the payload type is being defined and then
  // redefined. We see several interpretations - (a) format type 98
  // gets its first definition overwritten and becomes a t140/1000, or (b)
  // each media description uses a separate RTP profile (and hence needs a
  // separate RTP server), or (c) ignore the second definition. We choose
  // interpretation (c) because it seems simplest.
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + DefaultConnection + #13#10
                          + 'm=audio 4000 RTP/AVP 98'#13#10
                          + 'a=rtpmap:98 pcm/8000'#13#10
                          + 'm=text 8000 RTP/AVP 98'#13#10
                          + 'a=rtpmap:98 t140/1000');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    Profile := TIdRTPProfile.Create;
    try
      Self.Payload.InitializeProfile(Profile);
      CheckEquals('pcm',
                  Profile.EncodingFor(98).Name,
                  'Format type wasn''t redefined');
    finally
      Profile.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseSessionIllegalCharacters;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's='#0);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out on Session with #0');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPSessionName, 's='#0]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseSomeMediaDescriptionsLackConnectionAndNoSessionConnection;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.11/127'#13#10
                          + 'm=audio 5060/2 RTP/AVP 31'#13#10
                          + 'i=Oh no, we have no media-level connection!'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(MissingConnection,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeMultipleHeaders;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 't=3034423619 3042462419'#13#10
                          + 't=0 0'#13#10
                          + 't=3034423619 0');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(3,            Self.Payload.Times.Count,                    'Times.Count');
    CheckEquals('3042462419', IntToStr(Self.Payload.Times[0].EndTime),     'Times[0].EndTime');
    CheckEquals('3034423619', IntToStr(Self.Payload.Times[0].StartTime),   'Times[0].StartTime');
    CheckEquals(0,            Self.Payload.Times[0].Repeats.Count,         'Times[0].Repeats.Count');
    CheckEquals(0,            Self.Payload.Times[0].ZoneAdjustments.Count, 'Times[0].ZoneAdjustments.Count');
    CheckEquals('0',          IntToStr(Self.Payload.Times[1].EndTime),     'Times[1].EndTime');
    CheckEquals('0',          IntToStr(Self.Payload.Times[1].StartTime),   'Times[1].StartTime');
    CheckEquals(0,            Self.Payload.Times[1].Repeats.Count,         'Times[1].Repeats.Count');
    CheckEquals(0,            Self.Payload.Times[1].ZoneAdjustments.Count, 'Times[1].ZoneAdjustments.Count');
    CheckEquals('0',          IntToStr(Self.Payload.Times[2].EndTime),     'Times[2].EndTime');
    CheckEquals('3034423619', IntToStr(Self.Payload.Times[2].StartTime),   'Times[2].StartTime');
    CheckEquals(0,            Self.Payload.Times[2].Repeats.Count,         'Times[2].Repeats.Count');
    CheckEquals(0,            Self.Payload.Times[2].ZoneAdjustments.Count, 'Times[2].ZoneAdjustments.Count');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeSingleBounded;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 't=3034423619 3042462419');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,            Self.Payload.Times.Count,                   'Times.Count');
    // Yes, it looks lame, but CheckEquals takes two integers and the compiler
    // sees that 3042462419 is too big for an integer. This way we still get
    // to test, and see <expected> <actual> values instead of just a pass/fail
    CheckEquals('3042462419', IntToStr(Self.Payload.Times[0].EndTime),   'Times[0].EndTime');
    CheckEquals('3034423619', IntToStr(Self.Payload.Times[0].StartTime), 'Times[0].StartTime');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeSingleUnbounded;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 't=0 0');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1, Self.Payload.Times.Count,        'Times.Count');
    CheckEquals(0, Self.Payload.Times[0].EndTime,   'Times[0].EndTime');
    CheckEquals(0, Self.Payload.Times[0].StartTime, 'Times[0].StartTime');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeSingleUnboundedEndTime;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 't=3034423619 0');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,            Self.Payload.Times.Count,                  'Times.Count');
    CheckEquals('0',          IntToStr(Self.Payload.Times[0].EndTime),   'Times[0].EndTime');
    CheckEquals('3034423619', IntToStr(Self.Payload.Times[0].StartTime), 'Times[0].StartTime');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeWithMalformedMultipleRepeat;
begin
  Self.CheckMalformedTimeWithRepeat('1 1 1 1 1x');
end;

procedure TestTIdSdpParser.TestParseTimeWithMalformedSingleRepeat;
begin
  Self.CheckMalformedTimeWithRepeat('1x');
end;

procedure TestTIdSdpParser.TestParseTimeWithRepeatAndZoneAdjustment;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 't=3034423619 3042462419'#13#10
                          + 'z=1 -2 3 4'#13#10
                          + 'r=1');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,   Self.Payload.Times.Count,                    'Times.Count');
    CheckEquals(1,   Self.Payload.Times[0].Repeats.Count,         'Times[0].Repeats.Count');
    CheckEquals(1,   Self.Payload.Times[0].ZoneAdjustments.Count, 'Times[0].ZoneAdjustments.Count');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeWithRepeatBeforeZoneAdjustment;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 't=3034423619 3042462419'#13#10
                          + 'r=1'#13#10
                          + 'z=1 -2 3 4');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(Format(UnknownOptionalHeader, ['z=1 -2 3 4']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeWithSingleRepeat;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 't=3034423619 3042462419'#13#10
                          + 'r=1');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,   Self.Payload.Times.Count,              'Times.Count');
    CheckEquals(1,   Self.Payload.Times[0].Repeats.Count,   'Times[0].Repeats.Count');
    CheckEquals('1', Self.Payload.Times[0].Repeats[0].Value, 'Repeats[0].Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeWithSingleTypedRepeat;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 't=3034423619 3042462419'#13#10
                          + 'r=1d');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,    Self.Payload.Times.Count,               'Times.Count');
    CheckEquals(1,    Self.Payload.Times[0].Repeats.Count,    'Times[0].Repeats.Count');
    CheckEquals('1d', Self.Payload.Times[0].Repeats[0].Value, 'Repeats[0].Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeWithSingleZoneAdjustment;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 't=3034423619 3042462419'#13#10
                          + 'z=0 -1');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,      Self.Payload.Times.Count,                       'Times.Count');
    CheckEquals(0,      Self.Payload.Times[0].Repeats.Count,            'Times[0].Repeats.Count');
    CheckEquals(1,      Self.Payload.Times[0].ZoneAdjustments.Count,    'Times[0].ZoneAdjustments.Count');
    CheckEquals('0 -1', Self.Payload.Times[0].ZoneAdjustments[0].Value, 'ZoneAdjustments[0].Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseUri;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + 'u=http://127.0.0.1/'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('http://127.0.0.1/', Self.Payload.URI, 'URI');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseVersionMalformed;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=a'#13#10 // v must be a number
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=Minimum Session Info');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out malformed version');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPVersionName, 'v=a']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseVersionMultipleHeaders;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'v=1');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with a duplicate version header');
    except
      on E: EParser do
        CheckEquals(Format(UnknownOptionalHeader, ['v=1']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdFilteredRTPPeer                                                     *
//******************************************************************************
//* TestTIdFilteredRTPPeer Public methods **************************************

procedure TestTIdFilteredRTPPeer.SetUp;
var
  Encoding: TIdRTPT140Payload;
begin
  inherited SetUp;

  Self.Binding := TIdSocketHandle.Create(nil);
  Self.Profile := TIdAudioVisualProfile.Create;

  Self.T140PT := Self.Profile.FirstFreePayloadType;
  Encoding := TIdRTPT140Payload.Create(T140Encoding + '/' + IntToStr(T140ClockRate));
  try
    Self.Profile.AddEncoding(Encoding, Self.T140PT);
  finally
    Encoding.Free;
  end;

  Self.Server := TIdMockRTPPeer.Create;
  Self.Server.Profile := Self.Profile;

  Self.LocalDesc  := TIdSdpMediaDescription.Create;
  Self.LocalDesc.AddFormat(IntToStr(Self.T140PT));
  Self.RemoteDesc := TIdSdpMediaDescription.Create;

  Self.Peer := TIdFilteredRTPPeer.Create(Self.Server,
                                         Self.LocalDesc,
                                         Self.RemoteDesc);
  Self.Peer.AddListener(Self);

  Self.NormalPacket := TIdRTPPacket.Create(Self.Profile);
  Self.NormalPacket.PayloadType := Self.T140PT;
  Self.NormalPacket.Payload := Self.Profile.EncodingFor(Self.NormalPacket.PayloadType).Clone;

  Self.UnexpectedPacket := TIdRTPPacket.Create(Self.Profile);
  Self.UnexpectedPacket.PayloadType := 0;
  Self.UnexpectedPacket.Payload := Self.Profile.EncodingFor(Self.UnexpectedPacket.PayloadType).Clone;

  Self.ReceivedRTCP := false;
  Self.ReceivedRTCP := false;
end;

procedure TestTIdFilteredRTPPeer.TearDown;
begin
  Self.UnexpectedPacket.Free;
  Self.NormalPacket.Free;
  Self.Peer.Free;
  Self.RemoteDesc.Free;
  Self.LocalDesc.Free;
  Self.Server.Free;
  Self.Profile.Free;
  Self.Binding.Free;

  inherited TearDown;
end;

//* TestTIdFilteredRTPPeer Private methods *************************************

procedure TestTIdFilteredRTPPeer.OnRTCP(Packet: TIdRTCPPacket;
                                        Binding: TIdSocketHandle);
begin
  Self.ReceivedRTCP := true;
end;

procedure TestTIdFilteredRTPPeer.OnRTP(Packet: TIdRTPPacket;
                                       Binding: TIdSocketHandle);
begin
  Self.ReceivedRTP := true;
end;

procedure TestTIdFilteredRTPPeer.Send(RTP: TIdRTPPacket);
begin
  (Self.Server as IIdAbstractRTPPeer).NotifyListenersOfRTP(RTP,
                                                           Self.Binding);
end;

procedure TestTIdFilteredRTPPeer.SendNormalData;
begin
  Self.Send(Self.NormalPacket);
end;

procedure TestTIdFilteredRTPPeer.SendUnexpectedData;
begin
  Self.Send(Self.UnexpectedPacket);
end;

//* TestTIdFilteredRTPPeer Published methods ***********************************

procedure TestTIdFilteredRTPPeer.TestFilter;
begin
  Self.SendUnexpectedData;
  Check(not Self.ReceivedRTP, 'RTP traffic wasn''t filtered');
end;

procedure TestTIdFilteredRTPPeer.TestNormalOperation;
begin
  Self.SendNormalData;
  Check(Self.ReceivedRTP, 'RTP traffic was lost');
end;

procedure TestTIdFilteredRTPPeer.TestSendPacket;
begin
  Self.Peer.SendPacket('127.0.0.1', 8000, Self.NormalPacket);
  CheckEquals(Self.NormalPacket.PayloadType,
              Self.Server.LastRTP.PayloadType,
              'Normal packet');

  Self.Peer.SendPacket('127.0.0.1', 8000, Self.UnexpectedPacket);
  CheckEquals(Self.UnexpectedPacket.PayloadType,
              Self.Server.LastRTP.PayloadType,
              'Unexpected packet');
end;

//******************************************************************************
//* TestTIdSdpPayloadProcessor                                                 *
//******************************************************************************
//* TestTIdSdpPayloadProcessor Public methods **********************************

procedure TestTIdSdpPayloadProcessor.SetUp;
begin
  inherited SetUp;

  Self.Proc := TIdSdpPayloadProcessor.Create;
end;

procedure TestTIdSdpPayloadProcessor.TearDown;
begin
  Self.Proc.Free;

  inherited TearDown;
end;

//* TestTIdSdpPayloadProcessor Private methods *********************************

procedure TestTIdSdpPayloadProcessor.CheckServerActiveOn(Port: Cardinal);
var
  Server: TIdUDPServer;
begin
  Server := TIdUDPServer.Create(nil);
  try
    Server.DefaultPort := Port;

    try
      Server.Active := true;
      Fail('No server started on ' + IntToStr(Port));
    except
      on EIdCouldNotBindSocket do;
    end;
  finally
    Server.Free;
  end;
end;

procedure TestTIdSdpPayloadProcessor.CheckServerNotActiveOn(Port: Cardinal);
var
  Binding: TIdSocketHandle;
  Server:  TIdUDPServer;
begin
  Server := TIdUDPServer.Create(nil);
  try
    Binding := Server.Bindings.Add;
    try
      Binding.Port  := Port;
      Server.Active := true;
      Server.Active := false;
    except
      on EIdCouldNotBindSocket do
        Fail('Port ' + IntToStr(Port) + ' not closed');
    end;
  finally
    Server.Free;
  end;
end;

//* TestTIdSdpPayloadProcessor Published methods *******************************

procedure TestTIdSdpPayloadProcessor.TestIsListening;
begin
  Check(not Self.Proc.IsListening, 'Initial');

  Self.Proc.StartListening('v=0'#13#10
                         + 'o=wintermute 1 1 IN IP4 127.0.0.1'#13#10
                         + 's=-'#13#10
                         + 'c=IN IP4 127.0.0.1'#13#10
                         + 'm=audio 8000 RTP/AVP 0'#13#10);
  Check(Self.Proc.IsListening, 'After StartListening');

  Self.Proc.StopListening;
  Check(not Self.Proc.IsListening, 'After StopListening');
end;

procedure TestTIdSdpPayloadProcessor.TestMediaDescriptionWithMultiplePorts;
var
  Description: TIdSdpPayload;
begin
  Self.Proc.StartListening('v=0'#13#10
                         + 'o=wintermute 1 1 IN IP4 127.0.0.1'#13#10
                         + 's=-'#13#10
                         + 'c=IN IP4 127.0.0.1'#13#10
                         + 'm=audio 8000/2 RTP/AVP 0'#13#10);

  Self.CheckServerActiveOn(8000);
  Self.CheckServerActiveOn(8002);

  Description := TIdSdpPayload.CreateFrom(Self.Proc.LocalSessionDescription);
  try
    CheckEquals(1,
                Description.MediaDescriptionCount,
                'Number of media descriptions');
    CheckEquals(8000,
                Description.MediaDescriptionAt(0).Port,
                'First media description port');
  finally
    Description.Free;
  end;
end;

procedure TestTIdSdpPayloadProcessor.TestMultipleMediaDescriptions;
var
  Description: TIdSdpPayload;
begin
  Self.Proc.StartListening('v=0'#13#10
                         + 'o=wintermute 1 1 IN IP4 127.0.0.1'#13#10
                         + 's=-'#13#10
                         + 'c=IN IP4 127.0.0.1'#13#10
                         + 'm=audio 8000 RTP/AVP 0'#13#10
                         + 'm=text 8002 RTP/AVP 100'#13#10
                         + 'a=rtpmap:100 t140/1000'#13#10);

  Self.CheckServerActiveOn(8000);
  Self.CheckServerActiveOn(8002);

  Description := TIdSdpPayload.CreateFrom(Self.Proc.LocalSessionDescription);
  try
    CheckEquals(2,
                Description.MediaDescriptionCount,
                'Number of media descriptions');
    CheckEquals(8000,
                Description.MediaDescriptionAt(0).Port,
                'First media description port');
    CheckEquals(8002,
                Description.MediaDescriptionAt(1).Port,
                'Second media description port');
  finally
    Description.Free;
  end;
end;

procedure TestTIdSdpPayloadProcessor.TestSessionCount;
var
  Description: TIdSdpPayload;
begin
  Self.Proc.StartListening('v=0'#13#10
                         + 'o=wintermute 1 1 IN IP4 127.0.0.1'#13#10
                         + 's=-'#13#10
                         + 'c=IN IP4 127.0.0.1'#13#10
                         + 'm=audio 7000/4 RTP/AVP 0'#13#10
                         + 'm=audio 8000 RTP/AVP 0'#13#10
                         + 'm=text 8002 RTP/AVP 100'#13#10
                         + 'a=rtpmap:100 t140/1000'#13#10);

  CheckEquals(3, Self.Proc.SessionCount, 'SessionCount');

  Description := TIdSdpPayload.CreateFrom(Self.Proc.LocalSessionDescription);
  try
    CheckEquals(3,
                Description.MediaDescriptionCount,
                'Number of media descriptions');
    CheckEquals(7000,
                Description.MediaDescriptionAt(0).Port,
                'First media description port');
    CheckEquals(8000,
                Description.MediaDescriptionAt(1).Port,
                'Second media description port');
    CheckEquals(8002,
                Description.MediaDescriptionAt(2).Port,
                'Third media description port');
  finally
    Description.Free;
  end;
end;

procedure TestTIdSdpPayloadProcessor.TestSingleMediaDescription;
var
  Description: TIdSdpPayload;
begin
  Self.Proc.StartListening('v=0'#13#10
                         + 'o=wintermute 1 1 IN IP4 127.0.0.1'#13#10
                         + 's=-'#13#10
                         + 'c=IN IP4 127.0.0.1'#13#10
                         + 'm=audio 8000 RTP/AVP 0'#13#10);

  Self.CheckServerActiveOn(8000); // RTP
  Self.CheckServerActiveOn(8001); // RTCP

  Description := TIdSdpPayload.CreateFrom(Self.Proc.LocalSessionDescription);
  try
    CheckEquals(1,
                Description.MediaDescriptionCount,
                'Number of media descriptions');
    CheckEquals(8000,
                Description.MediaDescriptionAt(0).Port,
                'Media description port');
  finally
    Description.Free;
  end;
end;

procedure TestTIdSdpPayloadProcessor.TestStopListening;
var
  I: Integer;
begin
  Self.Proc.StartListening('v=0'#13#10
                         + 'o=wintermute 1 1 IN IP4 127.0.0.1'#13#10
                         + 's=-'#13#10
                         + 'c=IN IP4 127.0.0.1'#13#10
                         + 'm=audio 8000 RTP/AVP 0'#13#10
                         + 'm=audio 8002 RTP/AVP 8'#13#10);
  Self.Proc.StopListening;

  for I := 8000 to 8003 do
    Self.CheckServerNotActiveOn(I);

  CheckEquals(0, Self.Proc.SessionCount, 'RTP servers not destroyed');
end;

initialization
  RegisterTest('IdSdpParser', Suite);
end.
