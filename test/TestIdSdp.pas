{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSdp;

interface

uses
  Classes, IdRTP, IdRTPServer, IdSdp, IdSimpleParser, IdTimerQueue, IdUDPServer,
  SyncObjs, TestFramework, TestFrameworkEx, TestFrameworkRtp;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestAddressTypeToStr;
    procedure TestDirectionToStr;
    procedure TestBandwidthTypeToStr;
    procedure TestKeyTypeToStr;
    procedure TestMediaTypeToStr;
    procedure TestStrToAddressType;
    procedure TestStrToDirection;
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
    procedure TestAssign;
    procedure TestCopy;
    procedure TestCreateAttribute;
    procedure TestEquals;
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
    procedure TestCopy;
    procedure TestGetName;
    procedure TestGetValue;
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
    procedure TestAssign;
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
    procedure TestAddressRoutableAddress;
    procedure TestAssign;
    procedure TestCopy;
    procedure TestPrintOnMulticast;
    procedure TestPrintOnMulticastWithNumberNoTtl;
    procedure TestPrintOnMulticastWithTtl;
    procedure TestPrintOnMulticastWithTtlAndNumber;
    procedure TestPrintOnUnicast;
    procedure TestPrintOnUsesRoutableAddress;
  end;

  TestTIdSdpKey = class(TTestCase)
  private
    K: TIdSdpKey;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestPrintOnJustMethod;
    procedure TestPrintOnMethodPlusValue;
    procedure TestPrintOnPromptWithKeyData;
    procedure TestPrintOnUriWithNoKeyData;
  end;

  TestTIdSdpMediaDescription = class(TTestCase)
  private
    M: TIdSdpMediaDescription;

    procedure ConfigureComplicatedly(Desc: TIdSdpMediaDescription);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAttributeAndCount;
    procedure TestAddRTPMapAttribute;
    procedure TestAddFormatAndFormatCount;
    procedure TestAssign;
    procedure TestEquals;
    procedure TestFormatClear;
    procedure TestGetFormat;
    procedure TestHasFormat;
    procedure TestInitialState;
    procedure TestIsRefusedStream;
    procedure TestIsText;
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
    procedure TestAssign;
    procedure TestPrintOn;
    procedure TestUsernameEncode;
    procedure TestUsernameWithSpaces;
  end;

  TestTIdSdpRepeat = class(TTestCase)
  private
    R: TIdSdpRepeat;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
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
    procedure TestAssign;
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
    procedure TestAddMultipleAttributes;
    procedure TestAddUsingString;
    procedure TestAssign;
    procedure TestClear;
    procedure TestDirection;
    procedure TestEquals;
    procedure TestHasAttribute;
    procedure TestPrintOn;
    procedure TestSetDirection;
    procedure TestInitialSetDirection;
  end;

  TestTIdSdpRTPMapAttributes = class(TTestCase)
  private
    A: TIdSdpRTPMapAttributes;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAddMultipleAttributes;
    procedure TestAssign;
    procedure TestClear;
    procedure TestEquals;
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
    procedure TestAddMultipleBandwidths;
    procedure TestAssign;
    procedure TestClear;
    procedure TestPrintOn;
  end;

  TestTIdSdpConnections = class(TTestCase)
  private
    C: TIdSdpConnections;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAddConnection;
    procedure TestAddMultipleConnections;
    procedure TestAssign;
    procedure TestClear;
    procedure TestPrintOn;
  end;

  TestTIdSdpMediaDescriptions = class(TTestCase)
  private
    M: TIdSdpMediaDescriptions;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAssign;
    procedure TestAllDescriptionsHaveConnections;
    procedure TestClear;
    procedure TestPrintOn;
  end;

  TestTIdSdpRepeats = class(TTestCase)
  private
    R: TIdSdpRepeats;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAssign;
    procedure TestClear;
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
    procedure TestAssign;
    procedure TestClear;
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
    procedure TestAssign;
    procedure TestClear;
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
    procedure TestAddConnection;
    procedure TestAsString;
    procedure TestConnectionAt;
    procedure TestConnectionCount;
    procedure TestCreateFromStream;
    procedure TestCreateFromStreamString;
    procedure TestEquals;
    procedure TestEqualsDifferentAttributes;
    procedure TestEqualsDifferentBandwidths;
    procedure TestEqualsDifferentEmailAddress;
    procedure TestEqualsDifferentInfo;
    procedure TestEqualsDifferentKey;
    procedure TestEqualsDifferentOrigin;
    procedure TestEqualsDifferentPhoneNumber;
    procedure TestEqualsDifferentRTPMapAttributes;
    procedure TestEqualsDifferentSessionName;
    procedure TestEqualsDifferentTimes;
    procedure TestEqualsDifferentUri;
    procedure TestEqualsDifferentVersion;
    procedure TestGetRtpMapAttributes;
    procedure TestInitializeOnEmptySdpPayload;
    procedure TestInitializeOnSingleMediaSdp;
    procedure TestMediaDescriptionAt;
    procedure TestMediaDescriptionCount;
    procedure TestMediaDescriptionGetsSessionConnections;
    procedure TestMimeType;
    procedure TestNewSessionConnectionGetsCopiedToMediaDescriptions;
    procedure TestNoSessionNamePrintsDash;
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
    procedure TestReadFromString;
    procedure TestReadFromStringEmpty;
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
    procedure TestIsDirection;
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
    procedure TestParseMediaDescriptionWithSessionAttributes;
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
    procedure TestParseOriginMalformedUsername;
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

  TIdSdpTestCase = class(TThreadingTestCase)
  public
    procedure CheckPortActive(Address: String;
                              Port: Cardinal;
                              Msg: String);
    procedure CheckPortFree(Address: String;
                              Port: Cardinal;
                              Msg: String);
  end;

  TestTIdSDPMediaStream = class(TIdSdpTestCase,
                                IIdRTPDataListener,
                                IIdRTPListener)
  private
    AVP:              TIdRTPProfile;
    Media:            TIdSDPMediaStream;
    ReceivingBinding: TIdConnection;
    RTCPEvent:        TEvent;
    RTPEvent:         TEvent;
    Sender:           TIdSDPMediaStream;
    SentBye:          Boolean;
    Timer:            TIdDebugTimerQueue;

    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdConnection);
    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdConnection);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdConnection);
    procedure SendRTCP;
    procedure SendRTP(LayerID: Cardinal = 0);
    procedure SetLocalMediaDesc(Stream: TIdSDPMediaStream;
                                const MediaDesc: String);
    procedure ValidateSender(LayerID: Cardinal = 0);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddDataListener;
    procedure TestAddRTPListener;
    procedure TestHierarchicallyEncodedStream;
    procedure TestIsReceiver;
    procedure TestIsSender;
    procedure TestLayeredCodecAddressesAndPorts;
    procedure TestPutOnHoldRecvOnly;
    procedure TestPutOnHoldSendRecv;
    procedure TestPutOnHoldWhileOnHold;
    procedure TestReceiveDataWhenNotReceiver;
    procedure TestRemoteMembersControlAddressAndPortSet;
    procedure TestRTPListenersGetRTCP;
    procedure TestRTPListenersGetRTP;
    procedure TestSendData;
    procedure TestSendDataWhenNotSender;
    procedure TestSetRemoteDescriptionSendsNoPackets;
    procedure TestStartListening;
    procedure TestStopListeningStopsListening;
    procedure TestTakeOffHold;
  end;

  TestTIdSDPMultimediaSession = class(TIdSdpTestCase,
                                      IIdRTPListener)
  private
    Payload:    TClass;
    LocalPort:  Cardinal;
    MS:         TIdSDPMultimediaSession;
    Profile:    TIdRTPProfile;
    RemotePort: Cardinal;
    Server:     TIdUDPServer;

    procedure CheckOrigin(ExpectedNetType: String;
                          ExpectedAddressType: TIdIPVersion;
                          ExpectedAddress: String;
                          ExpectedSessionDescription: String);
    procedure CheckSessionName(ExpectedName: String;
                               SessionDescription: String;
                               Msg: String);
    procedure CheckOriginUsername(ExpectedUsername: String;
                                  SessionDescription: String);
    function  MultiStreamSDP(LowPort, HighPort: Cardinal): String;
    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdConnection);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdConnection);
    procedure ReceiveDataOfType(PayloadType: Cardinal);
    function  RefusedStreamSDP(Port: Cardinal): String;
    function  SingleStreamSDP(Port: Cardinal;
                              PayloadType: Cardinal = 96): String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddressTypeFor;
    procedure TestDifferentPayloadTypesForSameEncoding;
    procedure TestInitialize;
    procedure TestIsListening;
    procedure TestLocalSessionDescription;
    procedure TestLocalSessionDescriptionWithRefusedStream;
    procedure TestLocalSessionVersionIncrements;
    procedure TestMimeType;
    procedure TestNetTypeFor;
    procedure TestOnHold;
    procedure TestPortAboveHighestAllowedPort;
    procedure TestPortAsHighestAllowedPort;
    procedure TestPortAsLowestAllowedPort;
    procedure TestPortBelowLowestAllowedPort;
    procedure TestPortThreePortRange;
    procedure TestPutOnHold;
    procedure TestSetLocalMachineName;
    procedure TestSetLocalSessionName;
    procedure TestSetRemoteDescription;
    procedure TestSetRemoteDescriptionMalformedSdp;
    procedure TestSetRemoteDescriptionWithSdpPayload;
    procedure TestSetUsername;
    procedure TestStartListeningSingleStream;
    procedure TestStartListeningMalformedSdp;
    procedure TestStartListeningMultipleStreams;
    procedure TestStartListeningPortsOutsideAllowedRange;
    procedure TestStartListeningRegistersLocalRtpMaps;
    procedure TestStartListeningRegistersRemoteRtpMaps;
    procedure TestStartListeningTriesConsecutivePorts;
    procedure TestStopListening;
    procedure TestTakeOffHold;
  end;

  TestTIdSdpNatMasquerader = class(TTestCase)
  private
    Payload: TIdSdpPayload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNatify;
  end;

const
  MinimumPayloadSansConnection = 'v=0'#13#10
                 + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                 + 's=Minimum Session Info';
  DefaultConnection = 'c=IN IP4 224.2.17.12/127'#13#10;

  MinimumPayload = MinimumPayloadSansConnection + #13#10
                 + DefaultConnection;

implementation

uses
  IdSocketHandle, IdStack, IdUnicode, SysUtils;

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
  Result.AddTest(TestTIdSdpRepeat.Suite);
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
  Result.AddTest(TestTIdSDPMediaStream.Suite);
  Result.AddTest(TestTIdSDPMultimediaSession.Suite);
//  Result.AddTest(TestTIdSdpNatMasquerader.Suite);
end;

const
  ArbitraryPort = 8000;
  IPv4LocalHost = '127.0.0.1';
  IPv6LocalHost = '::1';

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

procedure TestFunctions.TestDirectionToStr;
var
  D: TIdSdpDirection;
begin
  CheckEquals('inactive', DirectionToStr(sdInactive), 'sdInactive');
  CheckEquals('recvonly', DirectionToStr(sdRecvOnly), 'sdRecvOnly');
  CheckEquals('sendonly', DirectionToStr(sdSendOnly), 'sdSendOnly');
  CheckEquals('sendrecv', DirectionToStr(sdSendRecv), 'sdSendRecv');

  // To check that ALL TIdSdpDirections can be converted
  for D := Low(TIdSdpDirection) to High(TIdSdpDirection) do
    DirectionToStr(D);
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

procedure TestFunctions.TestStrToDirection;
begin
  Check(sdInactive = StrToDirection('inactive'), 'inactive');
  Check(sdRecvOnly = StrToDirection('recvonly'), 'recvonly');
  Check(sdSendOnly = StrToDirection('sendonly'), 'sendonly');
  Check(sdSendRecv = StrToDirection('sendrecv'), 'sendrecv');

  try
    StrToDirection('');
    Fail('Failed to bail out on empty string');
  except
    on EConvertError do;
  end;

  try
    StrToDirection('foo');
    Fail('Failed to bail out on ''foo''');
  except
    on EConvertError do;
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

procedure TestTIdSdpAttribute.TestAssign;
var
  Other: TIdSdpAttribute;
begin
  Other := TIdSdpAttribute.Create;
  try
    Other.Name := 'foo';
    Other.Value := 'bar';

    Self.A.Assign(Other);
    CheckEquals(Other.Name,  Self.A.Name,  'Name');
    CheckEquals(Other.Value, Self.A.Value, 'Value');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpAttribute.TestCopy;
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

procedure TestTIdSdpAttribute.TestEquals;
var
  Att: TIdSdpAttribute;
begin
  Att := TIdSdpAttribute.CreateAttribute('foo:bar');
  try
    Check(not Self.A.Equals(Att), 'A <> Att; 1');
    Check(not Att.Equals(Self.A), 'Att <> A; 1');

    Self.A.Name := Att.Name;
    Check(not Self.A.Equals(Att), 'A <> Att; 2');
    Check(not Att.Equals(Self.A), 'Att <> A; 2');

    Self.A.Value := Att.Value;
    Check(Self.A.Equals(Att), 'A = Att');
    Check(Att.Equals(Self.A), 'Att = A');

    Self.A.Value := Att.Value + '1';
    Check(not Self.A.Equals(Att), 'A <> Att; 3');
    Check(not Att.Equals(Self.A), 'Att <> A; 3');
  finally
    Att.Free;
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

  CheckEquals('a=rtpmap'#13#10, Self.S.DataString, 'PrintOn');
end;

procedure TestTIdSdpAttribute.TestPrintOnWithValue;
begin
  Self.A.Name  := 'rtpmap';
  Self.A.Value := '98 T140';

  Self.A.PrintOn(Self.S);

  CheckEquals('a=rtpmap:98 T140'#13#10, Self.S.DataString, 'PrintOn');
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

procedure TestTIdSdpRTPMapAttribute.TestCopy;
var
  Clone: TIdSdpAttribute;
begin
  Self.A.Name  := 'rtpmap';
  Self.A.Value := '98 T140/8000';

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

procedure TestTIdSdpRTPMapAttribute.TestGetValue;
begin
  Self.A.Value := '98 T140/1000';
  Self.A.PayloadType := 99;

  CheckEquals(IntToStr(Self.A.PayloadType) + ' ' + Self.A.Encoding.EncodingName,
              Self.A.Value,
              'Value');
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
  CheckEquals(T140EncodingName,
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
  Self.A.Value := '98 T140/1000/1';

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

procedure TestTIdSdpBandwidth.TestAssign;
var
  Other: TIdSdpBandwidth;
begin
  Other := TIdSdpBandwidth.Create;
  try
    Other.BandwidthType := btConferenceTotal;
    Other.Bandwidth     := 42;

    Self.B.Assign(Other);
    Check(Other.BandwidthType = Self.B.BandwidthType, 'BandwidthType');
    CheckEquals(Other.Bandwidth, Self.B.Bandwidth, 'Bandwidth');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpBandwidth.TestPrintOn;
begin
  Self.B.BandwidthType := btConferenceTotal;
  Self.B.Bandwidth     := 42;

  Self.B.PrintOn(Self.S);

  CheckEquals('b=CT:42'#13#10, S.DataString, 'PrintOn');
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

procedure TestTIdSdpConnection.TestAddressRoutableAddress;
const
  Binding   = '10.0.0.1';
  Localhost = '127.0.0.1';
begin
  Self.C.Address := Localhost;

  CheckEquals(Localhost,      Self.C.Address,         '1: Address not set');
  CheckEquals(Self.C.Address, Self.C.RoutableAddress, '1: RoutableAddress not defaulted to Address');

  Self.C.RoutableAddress := Binding;
  CheckEquals(Binding,   Self.C.RoutableAddress, '2: RoutableAddress value not overwritten');
  CheckEquals(Localhost, Self.C.Address,         '2: Address altered');

  Self.C.Address := '';
  Self.C.RoutableAddress := '';

  Self.C.RoutableAddress := Binding;
  CheckEquals(Binding,                Self.C.RoutableAddress, '3: RoutableAddress not set');
  CheckEquals(Self.C.RoutableAddress, Self.C.Address,         '3: Address not defaulted to RoutableAddress');

  Self.C.Address := Localhost;
  CheckEquals(Binding,   Self.C.RoutableAddress, '4: RoutableAddress altered');
  CheckEquals(Localhost, Self.C.Address,         '4: Address value not overwritten');
end;

procedure TestTIdSdpConnection.TestAssign;
var
  Other: TIdSdpConnection;
begin
  Other := TIdSdpConnection.Create;
  try
    Other.Address           := 'FF80::1';
    Other.AddressType       := Id_IPv6;
    Other.NetType           := Id_SDP_IN;
    Other.NumberOfAddresses := 2;
    Other.RoutableAddress   := 'F00F::1';
    Other.TTL               := 255;

    Self.C.Assign(Other);

    CheckEquals(Other.Address,           Self.C.Address,           'Address');
    Check      (Other.AddressType      = Self.C.AddressType,       'AddressType');
    CheckEquals(Other.NetType,           Self.C.NetType,           'NetType');
    CheckEquals(Other.NumberOfAddresses, Self.C.NumberOfAddresses, 'NumberOfAddresses');
    CheckEquals(Other.RoutableAddress,   Self.C.RoutableAddress,   'RoutableAddress');
    CheckEquals(Other.TTL,               Self.C.TTL,               'TTL');
  finally
    Other.Free;
  end;
end;

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

procedure TestTIdSdpConnection.TestPrintOnMulticast;
begin
  Self.C.Address     := '224.0.0.0';
  Self.C.AddressType := Id_IPv4;
  Self.C.NetType     := Id_SDP_IN;

  Self.C.PrintOn(Self.S);

  CheckEquals('c=IN IP4 224.0.0.0'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpConnection.TestPrintOnMulticastWithNumberNoTtl;
begin
  Self.C.Address           := '224.0.0.0';
  Self.C.AddressType       := Id_IPv4;
  Self.C.NetType           := Id_SDP_IN;
  Self.C.NumberOfAddresses := 127;

  Self.C.PrintOn(Self.S);

  CheckEquals('c=IN IP4 224.0.0.0'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpConnection.TestPrintOnMulticastWithTtl;
begin
  Self.C.Address     := '224.0.0.0';
  Self.C.AddressType := Id_IPv4;
  Self.C.NetType     := Id_SDP_IN;
  Self.C.TTL         := 127;

  Self.C.PrintOn(Self.S);

  CheckEquals('c=IN IP4 224.0.0.0/127'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpConnection.TestPrintOnMulticastWithTtlAndNumber;
begin
  Self.C.Address           := '224.0.0.0';
  Self.C.AddressType       := Id_IPv4;
  Self.C.NetType           := Id_SDP_IN;
  Self.C.NumberOfAddresses := 4;
  Self.C.TTL               := 127;

  Self.C.PrintOn(Self.S);

  CheckEquals('c=IN IP4 224.0.0.0/127/4'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpConnection.TestPrintOnUnicast;
begin
  Self.C.Address     := '0.0.0.255';
  Self.C.AddressType := Id_IPv4;
  Self.C.NetType     := Id_SDP_IN;

  Self.C.PrintOn(Self.S);

  CheckEquals('c=IN IP4 0.0.0.255'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpConnection.TestPrintOnUsesRoutableAddress;
const
  Binding   = '10.0.0.1';
  LocalHost = '127.0.0.1';
begin
  Self.C.Address         := Localhost;
  Self.C.AddressType     := Id_IPv4;
  Self.C.NetType         := Id_SDP_IN;
  Self.C.RoutableAddress := Binding;

  Self.C.PrintOn(Self.S);

  CheckEquals('c=IN IP4 ' + Self.C.RoutableAddress + #13#10, S.DataString, 'PrintOn didn''t use the routable address');
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

procedure TestTIdSdpKey.TestAssign;
var
  Other: TIdSdpKey;
begin
  Other := TIdSdpKey.Create;
  try
    Other.KeyType := ktURI;
    Other.Value := 'http://heehee.heehee/';

    Self.K.Assign(Other);
    Check(Other.KeyType = Self.K.KeyType, 'KeyType');
    CheckEquals(Other.Value, Self.K.Value, 'Value');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpKey.TestPrintOnJustMethod;
begin
  Self.K.KeyType := ktPrompt;

  Self.K.PrintOn(Self.S);

  CheckEquals('k=prompt'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpKey.TestPrintOnMethodPlusValue;
begin
  Self.K.KeyType := ktUri;
  Self.K.Value   := 'tel://42';

  Self.K.PrintOn(Self.S);

  CheckEquals('k=uri:tel://42'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpKey.TestPrintOnPromptWithKeyData;
begin
  Self.K.KeyType := ktPrompt;
  Self.K.Value   := 'tel://42';

  Self.K.PrintOn(Self.S);

  CheckEquals('k=prompt'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpKey.TestPrintOnUriWithNoKeyData;
begin
  Self.K.KeyType := ktUri;

  Self.K.PrintOn(Self.S);

  CheckEquals('k=uri:'#13#10, S.DataString, 'PrintOn');
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

procedure TestTIdSdpMediaDescription.ConfigureComplicatedly(Desc: TIdSdpMediaDescription);
begin
  Desc.AddRTPMapAttribute('foo/42', 1);
  Desc.Bandwidths.Add;
  Desc.Bandwidths[0].Bandwidth := 666;
  Desc.Bandwidths[0].BandwidthType := btConferenceTotal;
  Desc.Connections.Add;
  Desc.Connections[0].AddressType := Id_IPv4;
  Desc.Connections[0].Address := '127.0.0.1';
  Desc.Connections[0].NetType := Id_SDP_IN;
  Desc.Connections[0].NumberOfAddresses := 2;
  Desc.Connections.Add;
  Desc.Connections[1].AddressType := Id_IPv4;
  Desc.Connections[1].Address := '::1';
  Desc.Connections[1].NetType := Id_SDP_IN;
  Desc.Connections[0].TTL := 1;
  Desc.AddFormat('2');
  Desc.AddFormat('4');
  Desc.Info := 'info';
  Desc.Key.KeyType := ktBase64;
  Desc.Key.Value := 'foo';
  Desc.MediaType := mtText;
  Desc.Port := 666;
  Desc.PortCount := 6;
  Desc.Transport := Id_SDP_RTPAVP;
end;

//* TestTIdSdpMediaDescription Published methods *******************************

procedure TestTIdSdpMediaDescription.TestAddAttributeAndCount;
var
  I: Integer;
begin
  for I := 1 to 10 do begin
    Self.M.AddAttribute('foo', IntToStr(I));
    CheckEquals(I,
                Self.M.Attributes.Count,
                'Attribute not added, ' + IntToStr(I));
  end;
end;

procedure TestTIdSdpMediaDescription.TestAddRTPMapAttribute;
var
  A: TIdSdpRTPMapAttribute;
begin
  A := TIdSdpRTPMapAttribute.Create;
  try
    A.Value := '66 foo/42';

    Self.M.AddRTPMapAttribute(A.Encoding.EncodingName, A.PayloadType);

    CheckEquals(1, Self.M.RTPMapAttributes.Count, 'Attribute not added');
    CheckEquals(A.AsString,
                Self.M.RTPMapAttributes[0].AsString,
                'RTP map attribute not added correctly');
  finally
    A.Free;
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

procedure TestTIdSdpMediaDescription.TestAssign;
var
  NewDesc: TIdSdpMediaDescription;
begin
  Self.ConfigureComplicatedly(Self.M);

  NewDesc := TIdSdpMediaDescription.Create;
  try
    NewDesc.Assign(Self.M);
    Check(NewDesc.Equals(Self.M), 'NewDesc = M');
    Check(Self.M.Equals(NewDesc), 'M = NewDesc');
  finally
    NewDesc.Free;
  end;
end;

procedure TestTIdSdpMediaDescription.TestEquals;
var
  I:         Integer;
  MediaDesc: TIdSdpMediaDescription;
begin
  Self.ConfigureComplicatedly(Self.M);

  MediaDesc := TIdSdpMediaDescription.Create;
  try
    Check(not Self.M.Equals(MediaDesc), 'Empty description');

    for I := 0 to Self.M.Attributes.Count - 1 do
      MediaDesc.AddAttribute(Self.M.Attributes[I].Name, Self.M.Attributes[I].Value);
    Check(not Self.M.Equals(MediaDesc), 'Attributes added');

    MediaDesc.Bandwidths.Add(Self.M.Bandwidths);
    Check(not Self.M.Equals(MediaDesc), 'Bandwidths added');

    MediaDesc.Connections.Add(Self.M.Connections);
    Check(not Self.M.Equals(MediaDesc), 'Connections added');

    for I := 0 to Self.M.FormatCount - 1 do
      MediaDesc.AddFormat(Self.M.Formats[I]);
    Check(not Self.M.Equals(MediaDesc), 'Formats added');

    MediaDesc.Info := Self.M.Info;
    Check(not Self.M.Equals(MediaDesc), 'Info added');

    MediaDesc.Key.Assign(Self.M.Key);
    Check(not Self.M.Equals(MediaDesc), 'Key added');

    MediaDesc.MediaType := Self.M.MediaType;
    Check(not Self.M.Equals(MediaDesc), 'MediaType added');

    MediaDesc.Port := Self.M.Port;
    Check(not Self.M.Equals(MediaDesc), 'Port added');

    MediaDesc.PortCount := Self.M.PortCount;
    Check(not Self.M.Equals(MediaDesc), 'Info added');

    MediaDesc.Transport := Self.M.Transport;
    Check(not Self.M.Equals(MediaDesc), 'Transport added');
  finally
    MediaDesc.Free;
  end;
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

procedure TestTIdSdpMediaDescription.TestGetFormat;
var
  I: Integer;
begin
  for I := 0 to 999 do begin
    Self.M.AddFormat(IntToStr(I));
    CheckEquals(IntToStr(I), Self.M.Formats[I], 'Formats[' + IntToStr(I) + ']');
  end;
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

procedure TestTIdSdpMediaDescription.TestInitialState;
begin
  CheckEquals(1, Self.M.PortCount, 'PortCount');
end;

procedure TestTIdSdpMediaDescription.TestIsRefusedStream;
begin
  Self.M.Port := 8000;
  Check(not Self.M.IsRefusedStream, 'Port <> 0 but stream looks refused');

  Self.M.Port := 0;
  Check(Self.M.IsRefusedStream, 'Port = 0 but stream doesn''t look refused');
end;

procedure TestTIdSdpMediaDescription.TestIsText;
var
  MT: TIdSdpMediaType;
begin
  for MT := Low(TIdSdpMediaType) to High(TIdSdpMediaType) do begin
    Self.M.MediaType := MT;

    if (Self.M.MediaType = mtText) then
      Check(Self.M.IsText, 'Not marked as text when media type is ' + MediaTypeToStr(MT))
    else
      Check(not Self.M.IsText, 'Marked as text when media type is ' + MediaTypeToStr(MT))
  end;
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
    CheckEquals('m=audio 49230 RTP/AVP'#13#10,
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

  Self.M.AddAttribute(RTPMapAttribute, '100 ' + T140Encoding);

  Self.M.Bandwidths.Add;
  Self.M.Bandwidths[0].Bandwidth := 666;
  Self.M.Bandwidths[0].BandwidthType := btRS;

  Self.M.Bandwidths.Add;
  Self.M.Bandwidths[1].Bandwidth := 42;
  Self.M.Bandwidths[1].BandwidthType := btConferenceTotal;

  Self.M.Connections.Add;
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
    CheckEquals('m=audio 49230 RTP/AVP'#13#10
              + 'i=Cthulhu Speaks'#13#10
              + 'c=IN IP4 127.0.0.1/5/5'#13#10
              + 'b=RS:666'#13#10
              + 'b=CT:42'#13#10
              + 'k=base64:DEADBEEF'#13#10
              + 'a=rtpmap:98 L16/16000/2'#13#10
              + 'a=rtpmap:100 ' + T140Encoding + #13#10,
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

    CheckEquals('m=audio 49230/4 RTP/AVP 0'#13#10,
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

procedure TestTIdSdpOrigin.TestAssign;
var
  NewO: TIdSdpOrigin;
begin
  Self.O.Address := 'tessier-ashpool.co.luna';
  Self.O.AddressType := Id_IPv6;
  Self.O.NetType := 'IN';
  Self.O.SessionID := 'rock-the-casbah';
  Self.O.SessionVersion := '1';
  Self.O.Username := 'wintermute';

  NewO := TIdSdpOrigin.Create;
  try
    NewO.Assign(Self.O);
    CheckEquals(Self.O.Address,        NewO.Address,        'Address');
    Check(      Self.O.AddressType =   NewO.AddressType,    'AddressType');
    CheckEquals(Self.O.NetType,        NewO.NetType,        'NetType');
    CheckEquals(Self.O.SessionID,      NewO.SessionID,      'SessionID');
    CheckEquals(Self.O.SessionVersion, NewO.SessionVersion, 'SessionVersion');
    CheckEquals(Self.O.Username,       NewO.Username,       'Username');
  finally
    NewO.Free;
  end;
end;

procedure TestTIdSdpOrigin.TestPrintOn;
begin
  Self.O.Address        := 'www.example.com';
  Self.O.AddressType    := Id_IPv6;
  Self.O.NetType        := Id_SDP_IN;
  Self.O.SessionID      := 'side0f';
  Self.O.SessionVersion := 'beef';
  Self.O.Username       := 'Holy_Cow';

  Self.O.PrintOn(S);

  CheckEquals('o=Holy_Cow side0f beef IN IP6 www.example.com'#13#10,
              S.DataString,
              'PrintOn');
end;

procedure TestTIdSdpOrigin.TestUsernameEncode;
var
  NihongoSpace: WideString;
  UnicodeUsername: String;
begin
  // The kanji for what, romanised, reads "NihongoSpace".
  NihongoSpace := WideChar($65E5);
  NihongoSpace := NihongoSpace + WideChar($672C) + WideChar($8A9E) + WideChar(' ');

  UnicodeUsername := UTF16LEToUTF8(NihongoSpace);

  CheckEquals('one',     Self.O.UsernameEncode('one'),     'one');
  CheckEquals('one_two', Self.O.UsernameEncode('one_two'), 'one_two');

  CheckEquals(UTF16LEToUTF8(StringReplaceW(NihongoSpace, ' ', '_', true)),
              Self.O.UsernameEncode(UnicodeUsername), '"nihongo" + space');
end;

procedure TestTIdSdpOrigin.TestUsernameWithSpaces;
const
  SpacedUsername = 'Holy Cow';
begin
  Self.O.Address        := 'www.example.com';
  Self.O.AddressType    := Id_IPv6;
  Self.O.NetType        := Id_SDP_IN;
  Self.O.SessionID      := 'side0f';
  Self.O.SessionVersion := 'beef';
  Self.O.Username       := SpacedUsername;

  Self.O.PrintOn(S);

  CheckEquals('o=Holy_Cow side0f beef IN IP6 www.example.com'#13#10,
              S.DataString,
              'PrintOn didn''t "escape" spaces in username');

  CheckEquals(SpacedUsername, Self.O.Username, 'Username not encoded');
end;

//******************************************************************************
//* TestTIdSdpRepeat                                                           *
//******************************************************************************
//* TestTIdSdpRepeat Public methods ********************************************

procedure TestTIdSdpRepeat.SetUp;
begin
  inherited SetUp;

  Self.R := TIdSdpRepeat.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpRepeat.TearDown;
begin
  Self.S.Free;
  Self.R.Free;

  inherited TearDown;
end;

//* TestTIdSdpRepeat Published methods *****************************************

procedure TestTIdSdpRepeat.TestAssign;
var
  OrigValue: String;
  NewR:      TIdSdpRepeat;
begin
  OrigValue := '604800 3600 0 90000';
  Self.R.Value := OrigValue;

  NewR := TIdSdpRepeat.Create;
  try
    NewR.Assign(Self.R);
    CheckEquals(Self.R.Value,
                NewR.Value,
                'Assign');
  finally
    NewR.Free;
  end;
end;

procedure TestTIdSdpRepeat.TestPrintOn;
var
  OrigValue: String;
begin
  OrigValue := '604800 3600 0 90000';
  Self.R.Value := OrigValue;
  Self.R.PrintOn(Self.S);

  CheckEquals('r=' + OrigValue + #13#10,
              Self.S.DataString,
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

procedure TestTIdSdpTime.TestAssign;
var
  Other: TIdSdpTime;
begin
  Other := TIdSdpTime.Create;
  try
    Self.T.EndTime := $cafebabe;
    Self.T.StartTime := $deadbeef;
    Self.T.Repeats.Add.Value := '604800 3600 0 90000';
    Self.T.ZoneAdjustments.Add;

    Other.Assign(Self.T);

    CheckEquals(IntToHex(Self.T.EndTime, 16),
                IntToHex(Other.EndTime, 16),
                'EndTime');
    Check(Other.Repeats.Equals(Self.T.Repeats), 'Repeats');
    CheckEquals(IntToHex(Self.T.StartTime, 16),
                IntToHex(Other.StartTime, 16),
                'StartTime');
    Check(Other.ZoneAdjustments.Equals(Self.T.ZoneAdjustments),
          'ZoneAdjustments');
  finally

    Other.Free;
  end;
end;

procedure TestTIdSdpTime.TestPrintOn;
begin
  Self.T.EndTime   := $deadbeef;
  Self.T.StartTime := $cafebabe;

  Self.T.PrintOn(S);

  CheckEquals('t=3405691582 3735928559'#13#10,
              S.DataString,
              'PrintOn');
end;

procedure TestTIdSdpTime.TestPrintOnWithRepeats;
begin
  Self.T.EndTime   := $deadbeef;
  Self.T.StartTime := $cafebabe;
  Self.T.Repeats.Add;
  Self.T.Repeats[0].Value := '1d';

  Self.T.PrintOn(S);

  CheckEquals('t=3405691582 3735928559'#13#10
            + 'r=1d'#13#10,
              S.DataString,
              'PrintOn');
end;

procedure TestTIdSdpTime.TestPrintOnWithZoneAdjustments;
begin
  Self.T.EndTime   := $deadbeef;
  Self.T.StartTime := $cafebabe;
  Self.T.ZoneAdjustments.Add.Value := '3735928559 -2h';

  Self.T.PrintOn(S);

  CheckEquals('t=3405691582 3735928559'#13#10
            + 'z=3735928559 -2h'#13#10,
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
var
  Att: TIdSdpAttribute;
begin
  CheckEquals(0, Self.A.Count, 'Count on new list');
  Self.A.Add;
  CheckEquals(1, Self.A.Count, 'Count after Add');

  Att := TIdSdpAttribute.Create;
  try
    Self.A.Add(Att);
    CheckEquals(2, Self.A.Count, 'Count after Add(TIdSdpAttribute)');
  finally
    Att.Free;
  end;
end;

procedure TestTIdSdpAttributes.TestAddMultipleAttributes;
var
  Atts: TIdSdpAttributes;
  I:    Integer;
begin
  Atts := TIdSdpAttributes.Create;
  try
    Atts.Add;
    Atts.Add;
    Atts.Add;

    for I := 0 to Atts.Count - 1 do
      Atts[I].Name := IntToStr(I);

    Self.A.Add(Atts);
    CheckEquals(Atts.Count,
                Self.A.Count,
                'Not all attributes added');

    for I := 0 to Self.A.Count - 1 do
      CheckEquals(IntToStr(I),
                  Self.A[I].Name,
                  'Name of attribute ' + IntToStr(I));
  finally
    Atts.Free;
  end;
end;

procedure TestTIdSdpAttributes.TestAddUsingString;
begin
  Self.A.Add('foo:bar');
  CheckEquals(TIdSdpAttribute.ClassName,
              Self.A[0].ClassName,
              'Incorrect class added from ''foo:bar''');

  Self.A.Add('rtpmap:98 T140/1000');
  CheckEquals(TIdSdpRTPMapAttribute.ClassName,
              Self.A[1].ClassName,
              'Incorrect class added from ''rtpmap:98 T140/1000''');
end;

procedure TestTIdSdpAttributes.TestAssign;
var
  Other: TIdSdpAttributes;
begin
  Self.A.Add.Name := '1';

  Other := TIdSdpAttributes.Create;
  try
    Other.Add.Name := '4';
    Other.Add.Name := '5';
    Other.Add.Name := '6';

    Self.A.Assign(Other);

    CheckEquals(3,   Self.A.Count,   'Not all Attributes copied across');
    CheckEquals('4', Self.A[0].Name, 'First Attribute');
    CheckEquals('5', Self.A[1].Name, 'Second Attribute');
    CheckEquals('6', Self.A[2].Name, 'Third Attribute');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpAttributes.TestClear;
begin
  Self.A.Add;
  Self.A.Add;
  Self.A.Add;

  Self.A.Clear;
  CheckEquals(0, Self.A.Count, 'Count after clear');
end;

procedure TestTIdSdpAttributes.TestDirection;
var
  Att: TIdSdpAttribute;
begin
  Check(sdSendRecv = Self.A.Direction,
       'default');

  Att := Self.A.Add;

  Att.Name := 'sendrecv';
  Check(sdSendRecv = Self.A.Direction,
       'sendrecv');

  Att.Name := 'recvonly';
  Check(sdrecvonly = Self.A.Direction,
       'recvonly');

  Att.Name := 'sendonly';
  Check(sdsendonly = Self.A.Direction,
       'sendonly');

  Att.Name := 'inactive';
  Check(sdinactive = Self.A.Direction,
       'inactive');
end;

procedure TestTIdSdpAttributes.TestEquals;
var
  Other: TIdSdpAttributes;
begin
  Other := TIdSdpAttributes.Create;
  try
    Check(Self.A.Equals(Other), 'Self.A = Other; empty list');
    Check(Self.A.Equals(Other), 'Other = Self.A; empty list');

    Self.A.Add.Name := '2';
    Self.A.Add.Name := '4';

    Other.Add(Self.A[1]);
    Check(not Self.A.Equals(Other), 'Self.A <> Other');
    Check(not Self.A.Equals(Other), 'Other <> Self.A');

    Other.Add(Self.A[0]);

    Check(Self.A.Equals(Other), 'Self.A = Other; non-empty list');
    Check(Self.A.Equals(Other), 'Other = Self.A; non-empty list');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpAttributes.TestHasAttribute;
var
  Att: TIdSdpAttribute;
begin
  Att := TIdSdpAttribute.Create;
  try
    Att.Name  := 'foo';
    Att.Value := 'bar';

    Check(not Self.A.HasAttribute(Att), 'Empty list');

    Self.A.Add('foo=baz');
    Check(not Self.A.HasAttribute(Att), 'Non-empty list without Att');

    Self.A.Add(Att);
    Check(Self.A.HasAttribute(Att), 'Non-empty list with an equivalent attribute to Att');
  finally
    Att.Free;
  end;
end;

procedure TestTIdSdpAttributes.TestPrintOn;
var
  S: TStringStream;
begin
  Self.A.Add;
  Self.A.Add;

  Self.A[0].Name  := 'rtpmap';
  Self.A[0].Value := '98 T140/1000';
  Self.A[1].Name  := 'dead';
  Self.A[1].Value := 'beef';

  S := TStringStream.Create('');
  try
    Self.A.PrintOn(S);
    CheckEquals('a=rtpmap:98 T140/1000'#13#10
              + 'a=dead:beef'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpAttributes.TestSetDirection;
begin
  Self.A.Direction := sdRecvOnly;
  Self.A.Direction := sdInactive;
  CheckEquals(DirectionToStr(sdInactive),
              DirectionToStr(Self.A.Direction),
              'Direction attribute not changed');
end;

procedure TestTIdSdpAttributes.TestInitialSetDirection;
begin
  CheckEquals(0, Self.A.Count, 'New Attributes not empty');

  Self.A.Direction := sdInactive;
  CheckEquals(1, Self.A.Count, 'Direction attribute not added');
  
  CheckEquals(DirectionToStr(sdInactive),
              DirectionToStr(Self.A.Direction),
              'Direction attribute incorrect');
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
var
  Att: TIdSdpRTPMapAttribute;
begin
  CheckEquals(0, Self.A.Count, 'Count on new list');
  Self.A.Add;
  CheckEquals(1, Self.A.Count, 'Count after Add');

  Att := TIdSdpRTPMapAttribute.Create;
  try
    Self.A.Add(Att);
    CheckEquals(2, Self.A.Count, 'Count after Add(TIdSdpRTPMapAttribute)');
  finally
    Att.Free;
  end;
end;

procedure TestTIdSdpRTPMapAttributes.TestAddMultipleAttributes;
var
  Atts: TIdSdpRTPMapAttributes;
  I:    Integer;
begin
  Atts := TIdSdpRTPMapAttributes.Create;
  try
    Atts.Add;
    Atts.Add;
    Atts.Add;

    for I := 0 to Atts.Count - 1 do
      Atts[I].PayloadType := I;

    Self.A.Add(Atts);
    CheckEquals(Atts.Count,
                Self.A.Count,
                'Not all RTPMapAttributes added');

    for I := 0 to Self.A.Count - 1 do
      CheckEquals(I,
                  Self.A[I].PayloadType,
                  'PayloadType of RTPMapAttribute ' + IntToStr(I));
  finally
    Atts.Free;
  end;
end;

procedure TestTIdSdpRTPMapAttributes.TestAssign;
var
  Other: TIdSdpRTPMapAttributes;
begin
  Self.A.Add.Value := '1 PCMU/1';

  Other := TIdSdpRTPMapAttributes.Create;
  try
    Other.Add.Value := '4 PCMU/4';
    Other.Add.Value := '5 PCMU/5';
    Other.Add.Value := '6 PCMU/6';

    Self.A.Assign(Other);

    CheckEquals(3,          Self.A.Count,    'Not all rtpmap attributes copied across');
    CheckEquals('4 PCMU/4', Self.A[0].Value, 'First rtpmap attribute');
    CheckEquals('5 PCMU/5', Self.A[1].Value, 'Second rtpmap attribute');
    CheckEquals('6 PCMU/6', Self.A[2].Value, 'Third rtpmap attribute');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpRTPMapAttributes.TestClear;
begin
  Self.A.Add;
  Self.A.Add;
  Self.A.Add;

  Self.A.Clear;
  CheckEquals(0, Self.A.Count, 'Count after clear');
end;

procedure TestTIdSdpRTPMapAttributes.TestEquals;
var
  Other: TIdSdpRTPMapAttributes;
begin
  Other := TIdSdpRTPMapAttributes.Create;
  try
    Check(Self.A.Equals(Other), 'Self.A = Other; empty list');
    Check(Self.A.Equals(Other), 'Other = Self.A; empty list');

    Self.A.Add.PayloadType := 2;
    Self.A.Add.PayloadType := 4;

    Other.Add(Self.A[1]);
    Check(not Self.A.Equals(Other), 'Self.A <> Other');
    Check(not Self.A.Equals(Other), 'Other <> Self.A');

    Other.Add(Self.A[0]);

    Check(Self.A.Equals(Other), 'Self.A = Other; non-empty list');
    Check(Self.A.Equals(Other), 'Other = Self.A; non-empty list');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpRTPMapAttributes.TestPrintOn;
var
  S: TStringStream;
begin
  Self.A.Add;
  Self.A.Add;

  Self.A[0].Name  := 'rtpmap';
  Self.A[0].Value := '98 t140/1000';
  Self.A[1].Name  := 'rtpmap';
  Self.A[1].Value := '99 dead/8000';

  S := TStringStream.Create('');
  try
    Self.A.PrintOn(S);
    CheckEquals('a=rtpmap:98 t140/1000'#13#10
              + 'a=rtpmap:99 dead/8000'#13#10,
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
var
  BW: TIdSdpBandwidth;
begin
  CheckEquals(0, Self.B.Count, 'Count on new list');
  Self.B.Add;
  CheckEquals(1, Self.B.Count, 'Count after Add');

  BW := TIdSdpBandwidth.Create;
  try
    Self.B.Add(BW);
    CheckEquals(2, Self.B.Count, 'Count after Add(TIdSdpBandwidth)');
  finally
    BW.Free;
  end;
end;

procedure TestTIdSdpBandwidths.TestAddMultipleBandwidths;
var
  Bs: TIdSdpBandwidths;
  I:    Integer;
begin
  Bs := TIdSdpBandwidths.Create;
  try
    Bs.Add;
    Bs.Add;
    Bs.Add;

    for I := 0 to Bs.Count - 1 do
      Bs[I].Bandwidth := I;

    Self.B.Add(Bs);
    CheckEquals(Bs.Count,
                Self.B.Count,
                'Not all bandwidths added');

    for I := 0 to Self.B.Count - 1 do
      CheckEquals(I,
                  Self.B[I].Bandwidth,
                  'Bandwidth of Bandwidth ' + IntToStr(I));
  finally
    Bs.Free;
  end;
end;

procedure TestTIdSdpBandwidths.TestAssign;
var
  Other: TIdSdpBandwidths;
begin
  Self.B.Add.Bandwidth := 1;

  Other := TIdSdpBandwidths.Create;
  try
    Other.Add.Bandwidth := 4;
    Other.Add.Bandwidth := 5;
    Other.Add.Bandwidth := 6;

    Self.B.Assign(Other);

    CheckEquals(3, Self.B.Count,        'Not all bandwidths copied across');
    CheckEquals(4, Self.B[0].Bandwidth, 'First bandwidth');
    CheckEquals(5, Self.B[1].Bandwidth, 'Second bandwidth');
    CheckEquals(6, Self.B[2].Bandwidth, 'Third bandwidth');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpBandwidths.TestClear;
begin
  Self.B.Add;
  Self.B.Add;
  Self.B.Add;

  Self.B.Clear;
  CheckEquals(0, Self.B.Count, 'Count after clear');
end;

procedure TestTIdSdpBandwidths.TestPrintOn;
var
  S: TStringStream;
begin
  Self.B.Add;
  Self.B.Add;

  Self.B[0].Bandwidth  := 666;
  Self.B[0].BandwidthType := btConferenceTotal;
  Self.B[1].Bandwidth  := 13;
  Self.B[1].BandwidthType := btApplicationSpecific;

  S := TStringStream.Create('');
  try
    Self.B.PrintOn(S);
    CheckEquals('b=CT:666'#13#10
              + 'b=AS:13'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
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
var
  Conn: TIdSdpConnection;
begin
  CheckEquals(0, Self.C.Count, 'Count on new list');
  Self.C.Add;
  CheckEquals(1, Self.C.Count, 'Count after Add');

  Conn := TIdSdpConnection.Create;
  try
    Self.C.Add(Conn);
    CheckEquals(2, Self.C.Count, 'Count after Add(TIdSdpConnection)');
  finally
    Conn.Free;
  end;
end;

procedure TestTIdSdpConnections.TestAddConnection;
begin
  Self.C.AddConnection('IP', Id_IPv6, '2002:5156:4019:2::1', 69);
  CheckEquals(1, Self.C.Count, 'Count after AddConnection()');

  CheckEquals('IP',                  Self.C[0].NetType,     'NetType');
  Check(      Id_IPv6 =              Self.C[0].AddressType, 'AddressType');
  CheckEquals('2002:5156:4019:2::1', Self.C[0].Address,     'Address');
  CheckEquals(69,                    Self.C[0].TTL,         'TTL');
end;

procedure TestTIdSdpConnections.TestAddMultipleConnections;
var
  I:              Integer;
  NewConnections: TIdSdpConnections;
begin
  NewConnections := TIdSdpConnections.Create;
  try
    NewConnections.Add;
    NewConnections.Add;
    NewConnections.Add;

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

procedure TestTIdSdpConnections.TestAssign;
var
  Other: TIdSdpConnections;
begin
  Self.C.Add;
  Self.C.Add;
  Self.C.Add;

  Other := TIdSdpConnections.Create;
  try
    Other.Add;
    Other[0].Address           := 'FF80::1';
    Other[0].AddressType       := Id_IPv6;
    Other[0].NetType           := Id_SDP_IN;
    Other[0].NumberOfAddresses := 2;
    Other[0].TTL               := 255;

    Self.C.Assign(Other);

    CheckEquals(1, Self.C.Count, 'Not all Connections copied across');

    CheckEquals(Other[0].Address,
                Self.C[0].Address,
                'Address');
    Check(      Other[0].AddressType = Self.C[0].AddressType,
                'AddressType');
    CheckEquals(Other[0].NetType,
                Self.C[0].NetType,
                'NetType');
    CheckEquals(Other[0].NumberOfAddresses,
                Self.C[0].NumberOfAddresses,
                'NumberOfAddresses');
    CheckEquals(Other[0].TTL,
                Self.C[0].TTL,
                'TTL');

  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpConnections.TestClear;
begin
  Self.C.Add;
  Self.C.Add;
  Self.C.Add;

  Self.C.Clear;
  CheckEquals(0, Self.C.Count, 'Count after clear');
end;

procedure TestTIdSdpConnections.TestPrintOn;
var
  S: TStringStream;
begin
  Self.C.Add;
  Self.C.Add;

  Self.C[0].Address := '127.0.0.1';
  Self.C[0].AddressType := Id_IPv4;
  Self.C[0].NetType := 'IN';
  Self.C[0].NumberOfAddresses := 1;
  Self.C[0].TTL := 0;
  Self.C[1].Address := '::1';
  Self.C[1].AddressType := Id_IPv6;
  Self.C[1].NetType := 'IN';
  Self.C[1].NumberOfAddresses := 2;
  Self.C[1].TTL := 1;

  S := TStringStream.Create('');
  try
    Self.C.PrintOn(S);
    CheckEquals('c=IN IP4 127.0.0.1'#13#10
              + 'c=IN IP6 ::1/1/2'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
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
var
  Desc: TIdSdpMediaDescription;
begin
  CheckEquals(0, Self.M.Count, 'Count on new list');
  Self.M.Add;
  CheckEquals(1, Self.M.Count, 'Count after Add');

  Desc := TIdSdpMediaDescription.Create;
  try
    Self.M.Add(Desc);
    CheckEquals(2, Self.M.Count, 'Count after Add(TIdSdpMediaDescription)');
  finally
    Desc.Free;
  end;
end;

procedure TestTIdSdpMediaDescriptions.TestAssign;
var
  Other: TIdSdpMediaDescriptions;
begin
  Self.M.Add.Info := '1';

  Other := TIdSdpMediaDescriptions.Create;
  try
    Other.Add.Info := '4';
    Other.Add.Info := '5';
    Other.Add.Info := '6';

    Self.M.Assign(Other);

    CheckEquals(3,   Self.M.Count,   'Not all media descriptions copied across');
    CheckEquals('4', Self.M[0].Info, 'First media description');
    CheckEquals('5', Self.M[1].Info, 'Second media description');
    CheckEquals('6', Self.M[2].info, 'Third media description');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpMediaDescriptions.TestAllDescriptionsHaveConnections;
begin
  Check(Self.M.AllDescriptionsHaveConnections, 'Trivial case - empty list');

  Self.M.Add;
  Check(not Self.M.AllDescriptionsHaveConnections,
        'One item with no connection');

  Self.M[0].Connections.Add;
  Check(Self.M.AllDescriptionsHaveConnections,
        'One item now has a connection');

  Self.M.Add;
  Check(not Self.M.AllDescriptionsHaveConnections,
        'A second item, with no connection');

  Self.M[1].Connections.Add;
  Check(Self.M.AllDescriptionsHaveConnections,
        'Both items now have connections');
end;

procedure TestTIdSdpMediaDescriptions.TestClear;
begin
  Self.M.Add;
  Self.M.Add;
  Self.M.Add;

  Self.M.Clear;
  CheckEquals(0, Self.M.Count, 'Count after clear');
end;

procedure TestTIdSdpMediaDescriptions.TestPrintOn;
var
  S: TStringStream;
begin
  Self.M.Add;
  Self.M.Add;

  Self.M[0].MediaType := mtAudio;
  Self.M[0].AddFormat('0');
  Self.M[0].Port := 0;
  Self.M[0].Transport := 'TCP';
  Self.M[1].MediaType := mtText;
  Self.M[1].Port := 1;
  Self.M[1].Transport := 'RTP/AVP';
  Self.M[1].AddFormat('1');
  Self.M[1].AddFormat('2');
  Self.M[1].AddFormat('3');

  S := TStringStream.Create('');
  try
    Self.M.PrintOn(S);
    CheckEquals('m=audio 0 TCP 0'#13#10
              + 'm=text 1 RTP/AVP 1 2 3'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
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
var
  Rpt: TIdSdpRepeat;
begin
  CheckEquals(0, Self.R.Count, 'Count on new list');
  Self.R.Add;
  CheckEquals(1, Self.R.Count, 'Count after Add');

  Rpt := TIdSdpRepeat.Create;
  try
    Self.R.Add(Rpt);
    CheckEquals(2, Self.R.Count, 'Count after Add(TIdSdpBandwidth)');
  finally
    Rpt.Free;
  end;
end;

procedure TestTIdSdpRepeats.TestAssign;
var
  Other: TIdSdpRepeats;
begin
  Self.R.Add.Value := '1';

  Other := TIdSdpRepeats.Create;
  try
    Other.Add.Value := '4';
    Other.Add.Value := '5';
    Other.Add.Value := '6';

    Self.R.Assign(Other);

    CheckEquals(3,   Self.R.Count,    'Not all repeats copied across');
    CheckEquals('4', Self.R[0].Value, 'First repeat');
    CheckEquals('5', Self.R[1].Value, 'Second repeat');
    CheckEquals('6', Self.R[2].Value, 'Third repeat');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpRepeats.TestClear;
begin
  Self.R.Add;
  Self.R.Add;
  Self.R.Add;

  Self.R.Clear;
  CheckEquals(0, Self.R.Count, 'Count after clear');
end;

procedure TestTIdSdpRepeats.TestPrintOn;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.R.Add;
    Self.R.Add;

    Self.R[0].Value := '1w';
    Self.R[1].Value := '1h 1d 1w';

    Self.R.PrintOn(S);

    CheckEquals('r=1w'#13#10
              + 'r=1h 1d 1w'#13#10,
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
var
  Time: TIdSdpTime;
begin
  CheckEquals(0, Self.T.Count, 'Count on new list');
  Self.T.Add;
  CheckEquals(1, Self.T.Count, 'Count after Add');

  Time := TIdSdpTime.Create;
  try
    Self.T.Add(Time);
    CheckEquals(2, Self.T.Count, 'Count after Add(TIdSdpTime)');
  finally
    Time.Free;
  end;
end;

procedure TestTIdSdpTimes.TestAssign;
var
  Other: TIdSdpTimes;
begin
  Self.T.Add.StartTime := 1;

  Other := TIdSdpTimes.Create;
  try
    Other.Add.StartTime := 4;
    Other.Add.StartTime := 5;
    Other.Add.StartTime := 6;

    Self.T.Assign(Other);

    CheckEquals(3, Self.T.Count, 'Not all times copied across');
    CheckEquals(IntToHex(4, Sizeof(Int64)*2),
                IntToHex(Self.T[0].StartTime, Sizeof(Int64)*2),
                'First time');
    CheckEquals(IntToHex(5, Sizeof(Int64)*2),
                IntToHex(Self.T[1].StartTime,
                Sizeof(Int64)*2), 'Second time');
    CheckEquals(IntToHex(6, Sizeof(Int64)*2),
                IntToHex(Self.T[2].StartTime,
                Sizeof(Int64)*2), 'Third time');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpTimes.TestClear;
begin
  Self.T.Add;
  Self.T.Add;
  Self.T.Add;

  Self.T.Clear;
  CheckEquals(0, Self.T.Count, 'Count after clear');
end;

procedure TestTIdSdpTimes.TestPrintOn;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.T.Add;
    Self.T.Add;
    Self.T[0].StartTime := $cafebabe;
    Self.T[0].EndTime   := $deadbeef;
    Self.T[1].StartTime := 1000000000;
    Self.T[1].EndTime   := 1000000001;

    Self.T.PrintOn(S);

    CheckEquals('t=3405691582 3735928559'#13#10
              + 't=1000000000 1000000001'#13#10,
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
var
  Zone: TIdSdpZoneAdjustment;
begin
  CheckEquals(0, Self.Z.Count, 'Count on new list');
  Self.Z.Add;
  CheckEquals(1, Self.Z.Count, 'Count after Add');

  Zone := TIdSdpZoneAdjustment.Create;
  try
    Self.Z.Add(Zone);
    CheckEquals(2, Self.Z.Count, 'Count after Add(TIdSdpZoneAdjustment)');
  finally
    Zone.Free;
  end;
end;

procedure TestTIdSdpZoneAdjustments.TestAssign;
var
  Other: TIdSdpZoneAdjustments;
begin
  Self.Z.Add.Value := '1';

  Other := TIdSdpZoneAdjustments.Create;
  try
    Other.Add.Value := '4';
    Other.Add.Value := '5';
    Other.Add.Value := '6';

    Self.Z.Assign(Other);

    CheckEquals(3,   Self.Z.Count,    'Not all zone adjustments copied across');
    CheckEquals('4', Self.Z[0].Value, 'First zone adjustment');
    CheckEquals('5', Self.Z[1].Value, 'Second zone adjustment');
    CheckEquals('6', Self.Z[2].Value, 'Third zone adjustment');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpZoneAdjustments.TestClear;
begin
  Self.Z.Add;
  Self.Z.Add;
  Self.Z.Add;

  Self.Z.Clear;
  CheckEquals(0, Self.Z.Count, 'Count after clear');
end;

procedure TestTIdSdpZoneAdjustments.TestPrintOn;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Z.Add.Value := '3405691582 -2s';
    Self.Z.Add.Value := '3735928559 5d';

    Self.Z.PrintOn(S);

    CheckEquals('z=3405691582 -2s'#13#10
              + 'z=3735928559 5d'#13#10,
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

procedure TestTIdSdpPayload.TestEquals;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Check(Self.Payload.Equals(Other), 'Empty payloads, Payload = Other');
    Check(Other.Equals(Self.Payload), 'Empty payloads, Other = Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentAttributes;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Other.Attributes.Add;

    Check(not Self.Payload.Equals(Other), 'Different attributes, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different attributes, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentBandwidths;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Other.Bandwidths.Add;

    Check(not Self.Payload.Equals(Other), 'Different Bandwidths, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different Bandwidths, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentEmailAddress;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.EmailAddress.Address := 'foo@bar';
    Other.EmailAddress.Address := Self.Payload.EmailAddress.Address + '1';

    Check(not Self.Payload.Equals(Other), 'Different EmailAddress, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different EmailAddress, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentInfo;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.Info := 'foo@bar';
    Other.Info := Self.Payload.Info + '1';

    Check(not Self.Payload.Equals(Other), 'Different Info, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different Info, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentKey;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.Key.Value := 'foo@bar';
    Other.Key.Value := Self.Payload.Key.Value + '1';

    Check(not Self.Payload.Equals(Other), 'Different Key, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different Key, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentOrigin;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.Origin.Address := 'foo@bar';
    Other.Origin.Address := Self.Payload.Origin.Address + '1';

    Check(not Self.Payload.Equals(Other), 'Different Origin, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different Origin, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentPhoneNumber;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.PhoneNumber := 'foo@bar';
    Other.PhoneNumber := Self.Payload.PhoneNumber + '1';

    Check(not Self.Payload.Equals(Other), 'Different PhoneNumber, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different PhoneNumber, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentRTPMapAttributes;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Other.RTPMapAttributes.Add;

    Check(not Self.Payload.Equals(Other), 'Different RTPMapAttributes, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different RTPMapAttributes, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentSessionName;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.SessionName := 'foo@bar';
    Other.SessionName := Self.Payload.SessionName + '1';

    Check(not Self.Payload.Equals(Other), 'Different SessionName, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different SessionName, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentTimes;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Other.Times.Add;

    Check(not Self.Payload.Equals(Other), 'Different Times, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different Times, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentUri;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.Uri := 'foo@bar';
    Other.Uri := Self.Payload.Uri + '1';

    Check(not Self.Payload.Equals(Other), 'Different Uri, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different Uri, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentVersion;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.Version := 1;
    Other.Version := Self.Payload.Version + 1;

    Check(not Self.Payload.Equals(Other), 'Different Version, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different Version, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestGetRtpMapAttributes;
var
  Attributes: TIdSdpRTPMapAttributes;
  P:          TIdSdpParser;
  S:          TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'a=rtpmap:96 t140/8000'#13#10
                          + 'a=fmtp:96 98/98'#13#10
                          + 'm=text 11000 RTP/AVP 98'#13#10
                          + 'a=rtpmap:98 t140/1000'#13#10
                          + 'm=text 12000 RTP/AVP 97 100'#13#10
                          + 'a=rtpmap:97 t140/1000'#13#10
                          + 'a=rtpmap:100 red/1000'#13#10
                          + 'a=fmtp:100 98/98'#13#10);
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
  MD.AddAttribute(RTPMapAttribute, '98 T140/1000');

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

procedure TestTIdSdpPayload.TestMimeType;
begin
  CheckEquals(SdpMimeType, Self.Payload.MimeType, 'SDP MIME type');
end;

procedure TestTIdSdpPayload.TestNewSessionConnectionGetsCopiedToMediaDescriptions;
var
  MD: TIdSdpMediaDescription;
begin
  MD := Self.Payload.AddMediaDescription;
  Self.Payload.AddConnection;

  CheckEquals(1,
              MD.Connections.Count,
              'No connection added to an existing media description');
end;

procedure TestTIdSdpPayload.TestNoSessionNamePrintsDash;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Payload.PrintOn(S);

    Check(Pos('s=-', S.DataString) > 0,
          '''s=-'' not present in payload');
  finally
    S.Free;
  end;
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
    Self.Payload.Attributes.Add('dead:beef');

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10
              + 'a=dead:beef'#13#10,
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
    Self.Payload.Bandwidths.Add;
    Self.Payload.Bandwidths[0].Bandwidth := 13;
    Self.Payload.Bandwidths[0].BandwidthType := btApplicationSpecific;

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10
              + 'b=AS:13'#13#10,
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
              + 'c=IN IP4 224.2.17.12/127'#13#10,
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
              + 'c=IN IP4 224.2.17.12/127'#13#10,
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
              + 'k=base64:5ideofbeef'#13#10,
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
              + 'a=rtpmap:98 t140/1000'#13#10,
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
              + 'c=IN IP4 224.2.17.12/127'#13#10,
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
    Self.Payload.Times.Add;
    Self.Payload.Times[0].StartTime := 1000000000;
    Self.Payload.Times[0].EndTime   := 1000000001;

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10
              + 't=1000000000 1000000001'#13#10,
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
              + 'c=IN IP4 224.2.17.12/127'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestReadFromString;
var
  Other: TIdSdpPayload;
  Src:   TStringStream;
begin
  Src := TStringStream.Create(MinimumPayload);
  try
    Other := TIdSdpPayload.CreateFrom(Src);
    try
      Self.Payload.ReadFrom(Src.DataString);
      Check(Self.Payload.Equals(Other),
            'ReadFrom(String) gives different result to ReadFrom(Stream)');
    finally
      Other.Free;
    end;
  finally
    Src.Free;
  end;
end;

procedure TestTIdSdpPayload.TestReadFromStringEmpty;
begin
  try
    Self.Payload.ReadFrom('');
    Fail('Failed to bail out on empty input stream');
  except
    on EParserError do;
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
      on E: EParserError do
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
      on E: EParserError do
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
      on E: EParserError do
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
  S := TStringStream.Create(MinimumPayload
                          + 'm=' + Value + #13#10
                          + 'i=Information');
  try
    Self.P.Source := S;

    try
    Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
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
  S := TStringStream.Create(MinimumPayload
                          + Name + '=' + Value + #13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
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
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10
                          + 'r=' + RepeatValue);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
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

procedure TestTIdSdpParser.TestIsDirection;
begin
  Check(TIdSdpParser.IsDirection('inactive'), 'inactive');
  Check(TIdSdpParser.IsDirection('recvonly'), 'recvonly');
  Check(TIdSdpParser.IsDirection('sendonly'), 'sendonly');
  Check(TIdSdpParser.IsDirection('sendrecv'), 'sendrecv');

  Check(not TIdSdpParser.IsDirection(''),    'Empty string');
  Check(not TIdSdpParser.IsDirection('foo'), 'foo');
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
var
  I: Integer;
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

  // 224.0.0.0 to 239.255.255.255 are all valid multicast addresses
  for I := 224 to 239 do
    Check(TIdSdpParser.IsMulticastAddress(Id_IPv4, IntToStr(I) + '.255.255.255'),
          'IPv4: ' + IntToStr(I) + '.255.255.255');

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
  S := TStringStream.Create(MinimumPayload
                          + 'a=recvonly'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,          Self.Payload.Attributes.Count,    'AttributeCount');
    CheckEquals('recvonly', Self.Payload.Attributes[0].Name,  'Attribute.Name');
    CheckEquals('',         Self.Payload.Attributes[0].Value, 'Attribute.Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseAttributeWithValue;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'a=orient:landscape'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,           Self.Payload.Attributes.Count,    'AttributeCount');
    CheckEquals('orient',    Self.Payload.Attributes[0].Name,  'Attribute.Name');
    CheckEquals('landscape', Self.Payload.Attributes[0].Value, 'Attribute.Value');
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
  S := TStringStream.Create(MinimumPayload
                          + 'b=RR:666'#13#10);
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
  S := TStringStream.Create(MinimumPayload
                          + 'b=:'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out on a malformed bandwidth');
    except
      on E: EParserError do
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
  S := TStringStream.Create(MinimumPayload
                          + 'b=RR:666'#13#10
                          + 'b=CT:123'#13#10);
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
                          + 'c=IN IP4 224.2.17.11/127'#13#10); // note the difference compared with the session c.
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
  S := TStringStream.Create(MinimumPayload
                          + 'c=IN IP4 224.2.17.12/127'#13#10);
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
                          + 'c=IN IP4 224.2.17.12/127/3'#13#10);
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
                          + 'c=IN IP6 FF02:5156:4019:2::FFFF/127/3'#13#10);
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
                          + 'c=IN IP4 127.2.17.12'#13#10);
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
      on E: EParserError do
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
  S := TStringStream.Create(MinimumPayload
                          + '=Missing name'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
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
  S := TStringStream.Create(MinimumPayload
                          + 'b='#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
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
                          + 'i=My u & i headers are swopped round'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out: vosui');
    except
      on E: EParserError do
        CheckEquals(Format(BadHeaderOrder, [RSSDPInformationName, RSSDPUriName]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;

  S := TStringStream.Create(MinimumPayload
                          + 'b=RR:666'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'b=CT:666'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out: vosbcb');
    except
      on E: EParserError do
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
      on E: EParserError do
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
  S := TStringStream.Create(MinimumPayload
                          + 'k=uri:sip:wintermute@tessier-ashpool.co.luna'#13#10);
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
                          + 'a=fmtp:101 0-11'#13#10);
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
  S := TStringStream.Create(MinimumPayload
                          + 'm=data 6666 RTP/AVP 1'#13#10
                          + 'i=Information'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount');

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

procedure TestTIdSdpParser.TestParseMediaDescriptionWithSessionAttributes;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'c=IN IP4 127.0.0.1'#13#10
                          + 'a=recvonly'#13#10
                          + 'a=tool:RNID SDP'#13#10
                          + 'm=data 6666 RTP/AVP 1'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    CheckEquals(Self.Payload.Attributes.Count,
                Self.Payload.MediaDescriptionAt(0).Attributes.Count,
                'Media description attribute count');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithSessionConnections;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'c=IN IP4 127.0.0.1'#13#10
                          + 'm=data 6666 RTP/AVP 1'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    CheckEquals(Self.Payload.ConnectionCount,
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
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10);
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
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10);
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
var
  S: TStringStream;
begin
  // A port number of 65536 makes no sense for UDP or TCP, but SDP doesn't
  // care - a port consists of a sequence of digits.
  S := TStringStream.Create(MinimumPayload
                          + 'm=data 65536 RTP/AVP 1'#13#10 // Note the port number
                          + 'i=Information'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(65536,
                Self.Payload.MediaDescriptionAt(0).Port,
                'High port (portnum > 65535)');

  finally
    S.Free;
  end;
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
                          + 'a=type:broadcast'#13#10);
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
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'b=RR:666'#13#10
                          + 'a=recvonly'#13#10
                          + 'a=T38FaxTranscodingJBIG'#13#10
                          + 'a=type:broadcast'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(0, Self.Payload.Attributes.Count,                       'Session attributes');
    CheckEquals(1, Self.Payload.MediaDescriptionCount,                  'MediaDescriptionCount');
    CheckEquals(3, Self.Payload.MediaDescriptionAt(0).Attributes.Count, 'AttributeCount');

    CheckEquals('recvonly',
                Self.Payload.MediaDescriptionAt(0).Attributes[0].Name,
                'Attributes[0].Name');
    CheckEquals('',
                Self.Payload.MediaDescriptionAt(0).Attributes[0].Value,
                'Attributes[0].Value');
    CheckEquals('T38FaxTranscodingJBIG',
                Self.Payload.MediaDescriptionAt(0).Attributes[1].Name,
                'Attributes[1].Name');
    CheckEquals('',
                Self.Payload.MediaDescriptionAt(0).Attributes[1].Value,
                'Attributes[1].Value');
    CheckEquals('type',
                Self.Payload.MediaDescriptionAt(0).Attributes[2].Name,
                'Attributes[2].Name');
    CheckEquals('broadcast',
                Self.Payload.MediaDescriptionAt(0).Attributes[2].Value,
                'Attributes[2].Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithBandwidth;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'b=RR:666'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(0,
                Self.Payload.Bandwidths.Count,
                'Bandwidths.Count');
    CheckEquals(1,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount');
    CheckEquals(1,
                Self.Payload.MediaDescriptionAt(0).Bandwidths.Count,
                'MediaDescriptions.Bandwidths.Count');
    Check(btRR = Self.Payload.MediaDescriptionAt(0).Bandwidths[0].BandwidthType,
                'BandwidthType');
    CheckEquals(666,
                Self.Payload.MediaDescriptionAt(0).Bandwidths[0].Bandwidth,
                'Bandwidth');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithConnection;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.14/127/2'#13#10);
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
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'k=uri:sip:wintermute@tessier-ashpool.co.luna'#13#10);
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
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 31 23 ape convolution 3vilution'#13#10
                          + 'i=Information'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount');
    CheckEquals(5,
                Self.Payload.MediaDescriptionAt(0).FormatCount,
                'MediaDescriptionAt(0).FormatCount');
    CheckEquals('31',
                Self.Payload.MediaDescriptionAt(0).Formats[0],
                'MediaDescriptionAt(0).Formats[0]');
    CheckEquals('23',
                Self.Payload.MediaDescriptionAt(0).Formats[1],
                'MediaDescriptionAt(0).Formats[1]');
    CheckEquals('ape',
                Self.Payload.MediaDescriptionAt(0).Formats[2],
                'MediaDescriptionAt(0).Formats[2]');
    CheckEquals('convolution',
                Self.Payload.MediaDescriptionAt(0).Formats[3],
                'MediaDescriptionAt(0).Formats[3]');
    CheckEquals('3vilution',
                Self.Payload.MediaDescriptionAt(0).Formats[4],
                'MediaDescriptionAt(0).Formats[4]');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithPortCount;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 2 2'#13#10
                          + 'i=Information'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1, Self.Payload.MediaDescriptionCount, 'MediaDescriptionCount');

    Check(mtVideo = Self.Payload.MediaDescriptionAt(0).MediaType,
                'MediaDescriptionAt(0).MediaType');
    CheckEquals(49170,
                Self.Payload.MediaDescriptionAt(0).Port,
                'MediaDescriptionAt(0).Port');
    CheckEquals(2,
                Self.Payload.MediaDescriptionAt(0).PortCount,
                'MediaDescriptionAt(0).PortCount');
    CheckEquals('RTP/AVP',
                Self.Payload.MediaDescriptionAt(0).Transport,
                'MediaDescriptionAt(0).Transport');
    CheckEquals('Information',
                Self.Payload.MediaDescriptionAt(0).Info,
                'MediaDescriptionAt(0).Info');
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

    CheckEquals('mhandley',    Self.Payload.Origin.Username,       'Origin.Username');
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
                          + 's=Missing Origin. Like We Bad Syntax'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with missing origin-field');
    except
      on E: EParserError do
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
                          + 'i=Session Information'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with missing session-name-field');
    except
      on E: EParserError do
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
      on E: EParserError do
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
  S := TStringStream.Create('o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with missing proto-version');
    except
      on E: EParserError do
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
                          + 'c=IN IP6 2002:5156:4019:1::1'#13#10);
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

procedure TestTIdSdpParser.TestParseOriginMalformedUsername;
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
      on E: EParserError do
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
      on E: EParserError do
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
  // gets its first definition overwritten and becomes a T140/1000, or (b)
  // each media description uses a separate RTP profile (and hence needs a
  // separate RTP server), or (c) ignore the second definition. We choose
  // interpretation (c) because it seems simplest.
  S := TStringStream.Create(MinimumPayloadSansConnection + #13#10
                          + DefaultConnection
                          + 'm=audio 4000 RTP/AVP 98'#13#10
                          + 'a=rtpmap:98 pcm/8000'#13#10
                          + 'm=text 8000 RTP/AVP 98'#13#10
                          + 'a=rtpmap:98 T140/1000'#13#10);
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
                          + 's='#0#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out on Session with #0');
    except
      on E: EParserError do
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
      on E: EParserError do
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
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10
                          + 't=0 0'#13#10
                          + 't=3034423619 0'#13#10);
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
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10);
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
  S := TStringStream.Create(MinimumPayload
                          + 't=0 0'#13#10);
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
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 0'#13#10);
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
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10
                          + 'z=1 -2 3 4'#13#10
                          + 'r=1'#13#10);
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
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10
                          + 'r=1'#13#10
                          + 'z=1 -2 3 4'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
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
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10
                          + 'r=1'#13#10);
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
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10
                          + 'r=1d'#13#10);
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
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10
                          + 'z=0 -1'#13#10);
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
                          + 's=Minimum Session Info'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out malformed version');
    except
      on E: EParserError do
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
  S := TStringStream.Create(MinimumPayload
                          + 'v=1'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with a duplicate version header');
    except
      on E: EParserError do
        CheckEquals(Format(UnknownOptionalHeader, ['v=1']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TIdSdpTestCase                                                             *
//******************************************************************************
//* TIdSdpTestCase Public methods **********************************************

procedure TIdSdpTestCase.CheckPortActive(Address: String;
                                         Port: Cardinal;
                                         Msg: String);
var
  Binding: TIdSocketHandle;
  Server:  TIdUDPServer;
begin
  Server := TIdUDPServer.Create(nil);
  try
    Binding := Server.Bindings.Add;
    Binding.IP   := Address;
    Binding.Port := Port;
    try
      Server.Active := true;
      Fail(Msg);
    except
      on EIdCouldNotBindSocket do;
    end;
  finally
    Server.Free;
  end;
end;

procedure TIdSdpTestCase.CheckPortFree(Address: String;
                                       Port: Cardinal;
                                       Msg: String);
var
  Binding: TIdSocketHandle;
  Server:  TIdUDPServer;
begin
  Server := TIdUDPServer.Create(nil);
  try
    Binding := Server.Bindings.Add;
    Binding.IP   := Address;
    Binding.Port := Port;

    try
      Server.Active := true;
    except
      on EIdCouldNotBindSocket do
        Fail('Port ' + Address + ':' + IntToStr(Port) + ' is not free');
    end;
  finally
    Server.Free;
  end;
end;

//******************************************************************************
//* TestTIdSDPMediaStream                                                      *
//******************************************************************************
//* TestTIdSDPMediaStream Public methods ***************************************

procedure TestTIdSDPMediaStream.SetUp;
const
  OneSecond = 1000; // milliseconds
var
  SDP:    TIdSDPPayload;
  T140PT: TIdRTPPayloadType;
begin
  inherited SetUp;

  Self.DefaultTimeout := OneSecond;

  T140PT := 96;

  Self.AVP := TIdRTPProfile.Create;
  Self.AVP.AddEncoding(T140EncodingName, T140ClockRate, '', T140PT);

  Self.RTCPEvent := TSimpleEvent.Create;
  Self.RTPEvent  := TSimpleEvent.Create;
  Self.Timer     := TIdDebugTimerQueue.Create(false);

  Self.Media := TIdSDPMediaStream.Create;
  Self.Media.LocalProfile  := Self.AVP;
  Self.Media.RemoteProfile := Self.AVP;
  Self.Media.Timer         := Self.Timer;

  Self.Sender := TIdSDPMediaStream.Create;
  Self.Sender.LocalProfile  := Self.AVP;
  Self.Sender.RemoteProfile := Self.AVP;
  Self.Sender.Timer         := Self.Timer;

  // Sender has a nice high port number because some tests use ports just above
  // Self.Media's port (8000).
  SDP := TIdSdpPayload.CreateFrom('v=0'#13#10
                                + 'o=foo 1 2 IN IP4 127.0.0.1'#13#10
                                + 's=-'#13#10
                                + 'c=IN IP4 127.0.0.1'#13#10
                                + 'm=text 8000 RTP/AVP 96'#13#10
                                + 'a=rtpmap:' + IntToStr(T140PT) + ' T140/8000'#13#10
                                + 'm=text 9000 RTP/AVP 96'#13#10
                                + 'a=rtpmap:' + IntToStr(T140PT) + ' T140/8000'#13#10);
  try
    Self.Media.LocalDescription  := SDP.MediaDescriptionAt(0);
    Self.Sender.LocalDescription := SDP.MediaDescriptionAt(1);

    Self.Media.RemoteDescription  := Self.Sender.LocalDescription;
    Self.Sender.RemoteDescription := Self.Media.LocalDescription;
  finally
    SDP.Free;
  end;

  Self.Media.Initialize;
  Self.Sender.Initialize;

  Self.Media.StartListening;
  Self.Sender.StartListening;

  Self.ValidateSender;

  Self.SentBye := false;
end;

procedure TestTIdSDPMediaStream.TearDown;
begin
  Self.Sender.Free;
  Self.Media.Free;
  Self.RTPEvent.Free;
  Self.RTCPEvent.Free;
  Self.AVP.Free;

  Self.Timer.Terminate;

  inherited TearDown;
end;

//* TestTIdSDPMediaStream Private methods **************************************

procedure TestTIdSDPMediaStream.OnNewData(Data: TIdRTPPayload;
                                          Binding: TIdConnection);
begin
  Self.ReceivingBinding.LocalIP   := Binding.LocalIP;
  Self.ReceivingBinding.LocalPort := Binding.LocalPort;
  Self.ReceivingBinding.PeerIP    := Binding.PeerIP;
  Self.ReceivingBinding.PeerPort  := Binding.PeerPort;

  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSDPMediaStream.OnRTCP(Packet: TIdRTCPPacket;
                                       Binding: TIdConnection);
begin
  Self.SentBye := Packet.IsBye;

  Self.ReceivingBinding.LocalIP   := Binding.LocalIP;
  Self.ReceivingBinding.LocalPort := Binding.LocalPort;
  Self.ReceivingBinding.PeerIP    := Binding.PeerIP;
  Self.ReceivingBinding.PeerPort  := Binding.PeerPort;  

  Self.RTCPEvent.SetEvent;
end;

procedure TestTIdSDPMediaStream.OnRTP(Packet: TIdRTPPacket;
                                      Binding: TIdConnection);
begin
  Self.RTPEvent.SetEvent;
end;

procedure TestTIdSDPMediaStream.SendRTCP;
var
  Svr:  TIdUDPServer;
  RTCP: TIdRTCPBye;
  S:    TStringStream;
begin
  Svr := TIdUDPServer.Create(nil);
  try
    RTCP := TIdRTCPBye.Create;
    try
      RTCP.Reason := 'foo';

      S := TStringStream.Create('');
      try
        RTCP.PrintOn(S);

        Svr.Send(Self.Media.LocalDescription.Connections[0].Address,
                 Self.Media.LocalDescription.Port,
                 S.DataString);
      finally
        S.Free;
      end;
    finally
      RTCP.Free;
    end;
  finally
    Svr.Free;
  end;
end;

procedure TestTIdSDPMediaStream.SendRTP(LayerID: Cardinal = 0);
var
  Text: TIdRTPT140Payload;
begin
  Text := TIdRTPT140Payload.Create;
  try
    Text.Block := '1234';
    Self.Sender.SendData(Text, LayerID);
  finally
    Text.Free;
  end;
end;

procedure TestTIdSDPMediaStream.SetLocalMediaDesc(Stream: TIdSDPMediaStream;
                                                  const MediaDesc: String);
var
  SDP: TIdSdpPayload;
begin
  SDP := TIdSdpPayload.CreateFrom('v=0'#13#10
                                + 'o=foo 1 2 IN IP4 127.0.0.1'#13#10
                                + 's=-'#13#10
                                + 'c=IN IP4 127.0.0.1'#13#10
                                + MediaDesc);
  try
    Stream.LocalDescription := SDP.MediaDescriptionAt(0);
  finally
    SDP.Free;
  end;
end;

procedure TestTIdSDPMediaStream.ValidateSender(LayerID: Cardinal = 0);
begin
  Self.Media.AddRTPListener(Self);
  try
    // To authenticate Self.Sender as a real RTP member
    Self.SendRTP(LayerID);
    Self.WaitForSignaled(Self.RTPEvent, 'Waiting to validate sender');
  finally
    Self.Media.RemoveRTPListener(Self);
  end;
  Self.RTPEvent.ResetEvent;
end;

//* TestTIdSDPMediaStream Published methods ************************************

procedure TestTIdSDPMediaStream.TestAddDataListener;
var
  L1: TIdRTPTestRTPDataListener;
  L2: TIdRTPTestRTPDataListener;
begin
  L1 := TIdRTPTestRTPDataListener.Create;
  try
    L2 := TIdRTPTestRTPDataListener.Create;
    try
      Self.Media.AddDataListener(L1);
      Self.Media.AddDataListener(L2);
      Self.Media.AddDataListener(Self);

      Self.ExceptionMessage := 'Waiting for RTP data';
      Self.SendRTP;
      Self.WaitForSignaled;

      Check(L1.NewData, 'L1 not notified');
      Check(L2.NewData, 'L2 not notified');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSDPMediaStream.TestAddRTPListener;
var
  L1: TIdRTPTestRTPListener;
  L2: TIdRTPTestRTPListener;
begin
  L1 := TIdRTPTestRTPListener.Create;
  try
    L2 := TIdRTPTestRTPListener.Create;
    try
      Self.Media.AddRTPListener(L1);
      Self.Media.AddRTPListener(L2);
      Self.Media.AddRTPListener(Self);

      Self.ExceptionMessage := 'Waiting for RTP';
      Self.SendRTP;
      Self.WaitForSignaled(Self.RTPEvent);

      Check(L1.ReceivedRTP, 'L1 not notified');
      Check(L2.ReceivedRTP, 'L2 not notified');
    finally
      Self.Media.RemoveRTPListener(L2);
      L2.Free;
    end;
  finally
    Self.Media.RemoveRTPListener(L1);
    L1.Free;
  end;
end;

procedure TestTIdSDPMediaStream.TestHierarchicallyEncodedStream;
const
  PortCount = 1;
var
  Offset:          Cardinal;
  ReceiverLayerID: Cardinal;
  SenderLayerID:   Cardinal;
begin
  // We change Self.Sender's description because both ends must have the same
  // number of ports open.
  Self.SetLocalMediaDesc(Self.Sender,
                         'm=audio 9000/' + IntToStr(PortCount) + ' RTP/AVP 0'#13#10);
  Self.SetLocalMediaDesc(Self.Media,
                         'm=audio 8000/' + IntToStr(PortCount) + ' RTP/AVP 0'#13#10);
  Self.Sender.RemoteDescription := Self.Media.LocalDescription;
  Self.Media.RemoteDescription  := Self.Sender.LocalDescription;

  Self.Media.Initialize;
  Self.Sender.Initialize;

  Self.Media.AddDataListener(Self);

  for Offset := 0 to PortCount - 1 do begin
    ReceiverLayerID := Self.Media.LocalDescription.Port + 2*Offset;
    SenderLayerID   := Self.Sender.LocalDescription.Port + 2*Offset;
    CheckPortActive(Self.Media.LocalDescription.Connections[0].Address,
                    ReceiverLayerID,
                    IntToStr(Offset + 1) + 'th port not active');

    Self.ValidateSender(SenderLayerID);
    Self.SendRTP(SenderLayerID);
    Self.WaitForSignaled(IntToStr(Offset + 1) + 'th port of sender didn''t send data');

    CheckEquals(ReceiverLayerID,
                Self.ReceivingBinding.LocalPort,
                'Wrong layer received the data');
    CheckEquals(SenderLayerID,
                Self.ReceivingBinding.PeerPort,
                'Sender used wrong port (hence layer) to send data');
  end;
end;

procedure TestTIdSDPMediaStream.TestIsReceiver;
begin
  Self.SetLocalMediaDesc(Self.Media,
                    'm=audio 8000 RTP/AVP 0'#13#10);
  Check(Self.Media.IsReceiver,
        'Not Receiver by default');

  Self.SetLocalMediaDesc(Self.Media,
                    'm=audio 8000 RTP/AVP 0'#13#10
                  + 'a=recvonly');
  Check(Self.Media.IsReceiver,
        'Not Receiver when recvonly');

  Self.SetLocalMediaDesc(Self.Media,
                    'm=audio 8000 RTP/AVP 0'#13#10
                  + 'a=sendrecv');
  Check(Self.Media.IsReceiver,
        'Not Receiver when sendrecv');

  Self.SetLocalMediaDesc(Self.Media,
                    'm=audio 8000 RTP/AVP 0'#13#10
                  + 'a=sendonly');
  Check(not Self.Media.IsReceiver,
        'Receiver when sendonly');

  Self.SetLocalMediaDesc(Self.Media,
                    'm=audio 8000 RTP/AVP 0'#13#10
                  + 'a=inactive');
  Check(not Self.Media.IsReceiver,
        'Receiver when inactive');
end;

procedure TestTIdSDPMediaStream.TestIsSender;
begin
  Self.SetLocalMediaDesc(Self.Media,
                    'm=audio 8000 RTP/AVP 0'#13#10);
  Check(Self.Media.IsSender,
        'Not Sender by default');

  Self.SetLocalMediaDesc(Self.Media,
                    'm=audio 8000 RTP/AVP 0'#13#10
                  + 'a=sendrecv');
  Check(Self.Media.IsSender,
        'Not Sender when sendrecv');

  Self.SetLocalMediaDesc(Self.Media,
                    'm=audio 8000 RTP/AVP 0'#13#10
                  + 'a=sendonly');
  Check(Self.Media.IsSender,
        'Not Sender when sendonly');

  Self.SetLocalMediaDesc(Self.Media,
                    'm=audio 8000 RTP/AVP 0'#13#10
                  + 'a=recvonly');
  Check(not Self.Media.IsSender,
        'Sender when recvonly');

  Self.SetLocalMediaDesc(Self.Media,
                    'm=audio 8000 RTP/AVP 0'#13#10
                  + 'a=inactive');
  Check(not Self.Media.IsSender,
        'Sender when inactive');
end;

procedure TestTIdSDPMediaStream.TestLayeredCodecAddressesAndPorts;
var
  FirstPort:               Cardinal;
  SecondExpectedLocalPort: Cardinal;
  SecondExpectedPeerPort:  Cardinal;
begin
  // We check that the SDP sets the RTCP address/ports of the remote party's
  // various layers by "joining a session" - that will send a Receiver Report to
  // the control ports of the remote party's different layers.

  Self.Media.StopListening;
  Self.Sender.StopListening;

  Self.SetLocalMediaDesc(Self.Media,
                         'm=text 8000/2 RTP/AVP 96'#13#10
                       + 'a=rtpmap:96 t140/1000'#13#10);
  Self.SetLocalMediaDesc(Self.Sender,
                         'm=text 9000/2 RTP/AVP 96'#13#10
                       + 'a=rtpmap:96 t140/1000'#13#10);
  Self.Media.RemoteDescription := Self.Sender.LocalDescription;
  Self.Sender.RemoteDescription := Self.Media.LocalDescription;

  Self.Media.Initialize;

  // Self.Sender sends off two RTCP packets to Self.Media (one per layer).
  // We don't care about those two sending events, so we just trigger them
  // to get them unqueued.
  Self.Media.StartListening;
  Self.Media.JoinSession;
  Self.Timer.RemoveAllEvents;

  Self.Sender.Initialize;

  Self.Media.AddRTPListener(Self);
  Self.Sender.StartListening;
  Self.Sender.JoinSession;

  // We use a debug timer, so we trigger the event manually.
  Self.Timer.TriggerEarliestEvent;

  // Now we check that we received two RTCP packets. We don't necessarily know
  // the order in which the packets will be sent.

  // Check the first: it could come from one of two ports, and could arrive at
  // one of two ports.
  Self.WaitForSignaled(Self.RTCPEvent, 'No RTCP sent');
  Check((Self.ReceivingBinding.PeerPort = Self.Sender.LocalDescription.Port + 1)
     or (Self.ReceivingBinding.PeerPort = Self.Sender.LocalDescription.Port + 3),
        'The RTCP came from an unexpected port: ' + IntToStr(Self.ReceivingBinding.PeerPort));
  Check((Self.ReceivingBinding.LocalPort = Self.Sender.RemoteDescription.Port + 1)
     or (Self.ReceivingBinding.LocalPort = Self.Sender.RemoteDescription.Port + 3),
        'The RTCP arrived at an unexpected port: ' + IntToStr(Self.ReceivingBinding.LocalPort));

  // Obviously, the second packet must come from the OTHER source and arrive at
  // the OTHER destination.
  FirstPort := Self.ReceivingBinding.PeerPort;
  if (FirstPort = Self.Sender.LocalDescription.Port + 1) then begin
    SecondExpectedLocalPort := Self.Sender.RemoteDescription.Port + 3;
    SecondExpectedPeerPort  := Self.Sender.LocalDescription.Port + 3;
  end
  else begin
    SecondExpectedLocalPort := Self.Sender.RemoteDescription.Port + 1;
    SecondExpectedPeerPort  := Self.Sender.LocalDescription.Port + 1;
  end;

  Self.Timer.TriggerEarliestEvent;
  Self.WaitForSignaled(Self.RTCPEvent, 'No RTCP sent; second packet');

  CheckEquals(SecondExpectedPeerPort,
              Self.ReceivingBinding.PeerPort,
              'The RTCP came from an unexpected port');
  CheckEquals(SecondExpectedLocalPort,
              Self.ReceivingBinding.LocalPort,
              'The RTCP arrived at an unexpected port');
end;

procedure TestTIdSDPMediaStream.TestPutOnHoldRecvOnly;
var
  LocalMediaType: TIdSdpMediaType;
begin
  Check(not Self.Media.OnHold,
        'OnHold set before PutOnHold');

  Self.SetLocalMediaDesc(Self.Media,
                         'm=text 8000 RTP/AVP 96'#13#10
                       + 'a=rtpmap:96 t140/1000'#13#10
                       + 'a=recvonly');
  LocalMediaType := Self.Media.LocalDescription.MediaType;
  Self.Media.PutOnHold;

  CheckEquals(DirectionToStr(sdInactive),
              DirectionToStr(Self.Media.Direction),
              'Stream not put on hold');
  Check(Self.Media.OnHold,
        'OnHold not set');
  CheckEquals(MediaTypeToStr(LocalMediaType),
              MediaTypeToStr(Self.Media.LocalDescription.MediaType),
        'Stream''s MediaType changed');
end;

procedure TestTIdSDPMediaStream.TestPutOnHoldSendRecv;
var
  LocalMediaType: TIdSdpMediaType;
begin
  Check(not Self.Media.OnHold,
        'OnHold set before PutOnHold');

  Self.SetLocalMediaDesc(Self.Media,
                         'm=text 8000 RTP/AVP 96'#13#10
                       + 'a=rtpmap:96 t140/1000'#13#10
                       + 'a=sendrecv');
  LocalMediaType := Self.Media.LocalDescription.MediaType;
  Self.Media.PutOnHold;

  CheckEquals(DirectionToStr(sdSendOnly),
              DirectionToStr(Self.Media.Direction),
              'Stream not put on hold');
  Check(Self.Media.OnHold,
        'OnHold not set');
  CheckEquals(MediaTypeToStr(LocalMediaType),
              MediaTypeToStr(Self.Media.LocalDescription.MediaType),
        'Stream''s MediaType changed');
end;

procedure TestTIdSDPMediaStream.TestPutOnHoldWhileOnHold;
begin
  Self.Media.PutOnHold;
  Self.Media.PutOnHold;
  Check(Self.Media.OnHold,
        'OnHold not set');
end;

procedure TestTIdSDPMediaStream.TestReceiveDataWhenNotReceiver;
begin
  Self.Media.AddDataListener(Self);
  Self.SetLocalMediaDesc(Self.Media,
                         'm=text 8000 RTP/AVP 96'#13#10
                       + 'a=rtpmap:96 t140/1000'#13#10
                       + 'a=inactive');

  Self.SendRTP;

  Self.WaitForTimeout(Self.ThreadEvent, 'Received data when not a receiver');
end;

procedure TestTIdSDPMediaStream.TestRemoteMembersControlAddressAndPortSet;
begin
  // We check that the SDP sets the RTCP address/port of the remote party
  // by "joining a session" - that will send a Receiver Report to the
  // control port of the remote party.
  Self.Media.Initialize;
  Self.Timer.RemoveAllEvents;

  // This schedules the joining of the RTP session.
  Self.Media.JoinSession;
  Self.Sender.AddRTPListener(Self);

  // We use a debug timer, so we trigger the event manually.
  Self.Timer.TriggerEarliestEvent;

  Self.WaitForSignaled(Self.RTCPEvent, 'No RTCP sent');

  CheckEquals(Self.Media.LocalDescription.Port + 1, Self.ReceivingBinding.PeerPort, 'The RTCP came from an unexpected port');
  CheckEquals(Self.Sender.LocalDescription.Port + 1, Self.ReceivingBinding.LocalPort, 'The RTCP arrived at an unexpected port');
end;

procedure TestTIdSDPMediaStream.TestRTPListenersGetRTCP;
var
  L1: TIdRTPTestRTPListener;
  L2: TIdRTPTestRTPListener;
begin
  L1 := TIdRTPTestRTPListener.Create;
  try
    L2 := TIdRTPTestRTPListener.Create;
    try
      Self.Media.AddRTPListener(L1);
      Self.Media.AddRTPListener(L2);
      Self.Media.AddRTPListener(Self);

      Self.ExceptionMessage := 'Waiting for RTCP';
      Self.SendRTCP;
      Self.WaitForSignaled(Self.RTCPEvent);

      Check(L1.ReceivedRTCP, 'L1 not notified');
      Check(L2.ReceivedRTCP, 'L2 not notified');
    finally
      Self.Media.RemoveRTPListener(L2);
      L2.Free;
    end;
  finally
    Self.Media.RemoveRTPListener(L1);
    L1.Free;
  end;
end;

procedure TestTIdSDPMediaStream.TestRTPListenersGetRTP;
var
  L1: TIdRTPTestRTPListener;
  L2: TIdRTPTestRTPListener;
begin
  L1 := TIdRTPTestRTPListener.Create;
  try
    L2 := TIdRTPTestRTPListener.Create;
    try
      Self.Media.AddRTPListener(L1);
      Self.Media.AddRTPListener(L2);
      Self.Media.AddRTPListener(Self);

      Self.ExceptionMessage := 'Waiting for RTP';
      Self.SendRTP;
      Self.WaitForSignaled(Self.RTPEvent);

      Check(L1.ReceivedRTP, 'L1 not notified');
      Check(L2.ReceivedRTP, 'L2 not notified');
    finally
      Self.Media.RemoveRTPListener(L2);
      L2.Free;
    end;
  finally
    Self.Media.RemoveRTPListener(L1);
    L1.Free;
  end;
end;

procedure TestTIdSDPMediaStream.TestSendData;
begin
  Self.Media.AddRTPListener(Self);

  Self.SendRTP;

  Self.WaitForSignaled(Self.RTPEvent);
end;

procedure TestTIdSDPMediaStream.TestSendDataWhenNotSender;
begin
  Self.Media.AddRTPListener(Self);
  Self.SetLocalMediaDesc(Self.Sender,
                         'm=text 8002 RTP/AVP 96'#13#10
                       + 'a=rtpmap:96 t140/1000'#13#10
                       + 'a=inactive');
  Self.SendRTP;

  Self.WaitForTimeout(Self.RTPEvent,
                      'Server sent data when not a sender');
end;

procedure TestTIdSDPMediaStream.TestSetRemoteDescriptionSendsNoPackets;
begin
  Self.Media.AddRTPListener(Self);
  Self.Media.RemoteDescription := Self.Media.RemoteDescription;

  Self.WaitForTimeout(Self.RTCPEvent,  'SetRemoteDescription sent RTCP packets');
end;

procedure TestTIdSDPMediaStream.TestStartListening;
begin
  Self.Media.StopListening;
  Self.Sender.StopListening;

  Self.SetLocalMediaDesc(Self.Sender,
                         'm=audio 9000 RTP/AVP 0'#13#10);
  Self.SetLocalMediaDesc(Self.Media,
                         'm=audio 8000 RTP/AVP 0'#13#10);
  Self.Sender.RemoteDescription := Self.Media.LocalDescription;
  Self.Media.RemoteDescription  := Self.Sender.LocalDescription;

  CheckPortFree(Self.Sender.LocalDescription.Connections[0].Address,
                Self.Sender.LocalDescription.Port,
                'Local port active before Initialize');

  Self.Media.AddRTPListener(Self);
  Self.Media.StartListening;

  Self.Sender.Initialize;

  CheckPortFree(Self.Sender.LocalDescription.Connections[0].Address,
                Self.Sender.LocalDescription.Port,
                'Local port active after Initialize, before StartListening');

  Self.Timer.RemoveAllEvents;
  Self.Sender.StartListening;

  CheckPortActive(Self.Sender.LocalDescription.Connections[0].Address,
                  Self.Sender.LocalDescription.Port,
                  'Local port not active after StartListening');

  Self.Sender.JoinSession;

  // We're using a debug timer, so we fire the event manually.
  Self.Timer.TriggerEarliestEvent;

  Self.WaitForSignaled(Self.RTCPEvent, 'No RTCP sent');

  Self.SendRTP;

  Self.WaitForSignaled(Self.RTPEvent, 'No RTP sent');
end;

procedure TestTIdSDPMediaStream.TestStopListeningStopsListening;
begin
  // Commented out stuff: an idea for how to test that the server
  // you stop sends an RTCP BYE to its RTP session.

//  Self.Sender.AddRTPListener(Self);
  Self.Media.AddRTPListener(Self);
  Self.Media.StopListening;
  Self.SendRTP;

  Self.WaitForTimeout(Self.RTPEvent,  'Server didn''t stop listening');
{
  // How do we test that the server behaved nicely and sent an RTCP BYE?
  Self.WaitForSignaled(Self.RTCPEvent, 'Server didn''t send any RTCP');
  Check(Self.SentBye, 'Server didn''t send an RTCP BYE');
}
end;

procedure TestTIdSDPMediaStream.TestTakeOffHold;
var
  PreHoldDirection: TIdSdpDirection;
begin
  PreHoldDirection := sdRecvOnly;
  Self.SetLocalMediaDesc(Self.Media,
                         'm=text 8000 RTP/AVP 96'#13#10
                       + 'a=rtpmap:96 t140/1000'#13#10
                       + 'a=' + DirectionToStr(PreHoldDirection));
  Self.Media.PutOnHold;
  Self.Media.TakeOffHold;

  CheckEquals(DirectionToStr(PreHoldDirection),
              DirectionToStr(Self.Media.Direction),
              'Stream not taken off hold');
  Check(not Self.Media.OnHold,
        'OnHold not unset');
end;

//******************************************************************************
//* TestTIdSDPMultimediaSession                                                *
//******************************************************************************
//* TestTIdSDPMultimediaSession Public methods *********************************

procedure TestTIdSDPMultimediaSession.SetUp;
begin
  inherited SetUp;

  Self.Profile := TIdAudioVisualProfile.Create;

  Self.MS := TIdSDPMultimediaSession.Create(Self.Profile);

  // We only instantiate Server so that we know that GStack points to an
  // instantiated stack.
  Self.Server := TIdUdpServer.Create(nil);
end;

procedure TestTIdSDPMultimediaSession.TearDown;
begin
  Self.Server.Free;
  Self.MS.Free;
  Self.Profile.Free;

  inherited TearDown;
end;

//* TestTIdSDPMultimediaSession Private methods ********************************

procedure TestTIdSDPMultimediaSession.CheckOrigin(ExpectedNetType: String;
                                                  ExpectedAddressType: TIdIPVersion;
                                                  ExpectedAddress: String;
                                                  ExpectedSessionDescription: String);
var
  SDP: TIdSDPPayload;
begin
  SDP := TIdSdpPayload.CreateFrom(Self.MS.LocalSessionDescription);
  try
    CheckEquals(ExpectedNetType,
                SDP.Origin.NetType,
                ExpectedAddress + ': Setting local address didn''t set the NetType');
    CheckEquals(AddressTypeToStr(ExpectedAddressType),
                AddressTypeToStr(SDP.Origin.AddressType),
                ExpectedAddress + ': Setting local address didn''t set the AddressType');
    CheckEquals(ExpectedAddress,
                SDP.Origin.Address,
                ExpectedAddress + ': Setting local address didn''t set the Address');
  finally
    SDP.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.CheckSessionName(ExpectedName: String;
                                                       SessionDescription: String;
                                                       Msg: String);
var
  SDP: TIdSDPPayload;
begin
  SDP := TIdSdpPayload.CreateFrom(Self.MS.LocalSessionDescription);
  try
    CheckEquals(ExpectedName,
                SDP.SessionName,
                Msg);
  finally
    SDP.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.CheckOriginUsername(ExpectedUsername: String;
                                                          SessionDescription: String);
var
  SDP: TIdSdpPayload;
begin
  SDP := TIdSdpPayload.CreateFrom(SessionDescription);
  try
    CheckEquals(ExpectedUsername, SDP.Origin.Username, 'Username not set');
  finally
    SDP.Free;
  end;
end;

function TestTIdSDPMultimediaSession.MultiStreamSDP(LowPort, HighPort: Cardinal): String;
begin
  // One stream on loopback:LowPort; one stream on NIC:HighPort
  Result := 'v=0'#13#10
          + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
          + 's=-'#13#10
          + 'm=text ' + IntToStr(LowPort) + ' RTP/AVP 96'#13#10
          + 'c=IN IP4 127.0.0.1'#13#10
          + 'a=rtpmap:96 t140/1000'#13#10
          + 'm=text ' + IntToStr(HighPort) + ' RTP/AVP 96'#13#10
          + 'c=IN IP4 ' + GStack.LocalAddress + #13#10
          + 'a=rtpmap:96 t140/1000'#13#10
end;

procedure TestTIdSDPMultimediaSession.OnRTCP(Packet: TIdRTCPPacket;
                                             Binding: TIdConnection);
begin
end;

procedure TestTIdSDPMultimediaSession.OnRTP(Packet: TIdRTPPacket;
                                            Binding: TIdConnection);
begin
  Self.Payload := Packet.Payload.ClassType;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSDPMultimediaSession.ReceiveDataOfType(PayloadType: Cardinal);
var
  Client: TIdRTPServer;
  NoData: TIdRTPPacket;
begin
  Client := TIdRTPServer.Create;
  try
    Client.LocalProfile  := Self.Profile;
    Client.RemoteProfile := Self.Profile;
    Client.RTPPort       := Self.RemotePort + 1000;

    NoData := TIdRTPPacket.Create(Client.RemoteProfile);
    try
      NoData.PayloadType := PayloadType;
      Client.SendPacket('127.0.0.1', Self.LocalPort, NoData);
    finally
      NoData.Free;
    end;
  finally
    Client.Free;
  end;
end;

function TestTIdSDPMultimediaSession.RefusedStreamSDP(Port: Cardinal): String;
begin
  // One stream refused, one accepted.
  Result := 'v=0'#13#10
          + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
          + 's=-'#13#10
          + 'c=IN IP4 127.0.0.1'#13#10
          + 'm=text 0 RTP/AVP 0'#13#10
          + 'm=text ' + IntToStr(Port) + ' RTP/AVP 96'#13#10
          + 'a=rtpmap:96 t140/1000'#13#10
end;

function TestTIdSDPMultimediaSession.SingleStreamSDP(Port: Cardinal;
                                                     PayloadType: Cardinal = 96): String;
begin
  Result := 'v=0'#13#10
          + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
          + 's=-'#13#10
          + 'c=IN IP4 127.0.0.1'#13#10
          + 'm=text ' + IntToStr(Port) + ' RTP/AVP ' + IntToStr(PayloadType) + #13#10
          + 'a=rtpmap:' + IntToStr(PayloadType) + ' t140/1000'#13#10
end;

//* TestTIdSDPMultimediaSession Published methods ******************************

procedure TestTIdSDPMultimediaSession.TestAddressTypeFor;
begin
  CheckEquals(AddressTypeToStr(Id_IPv4),
              AddressTypeToStr(Self.MS.AddressTypeFor(IPv4LocalHost)),
              IPv4LocalHost);

  CheckEquals(AddressTypeToStr(Id_IPv6),
              AddressTypeToStr(Self.MS.AddressTypeFor(IPv6LocalHost)),
              IPv6LocalHost);

  Check(Id_IPUnknown = Self.MS.AddressTypeFor(''),
        'Unknown address (the empty string)');
end;

procedure TestTIdSDPMultimediaSession.TestDifferentPayloadTypesForSameEncoding;
const
  LocalPT  = 96;
  RemotePT = 98;
begin
  Self.LocalPort  := 8000;
  Self.RemotePort := 9000;

  Self.MS.StartListening(Self.SingleStreamSDP(Self.LocalPort, LocalPT));
  Self.MS.SetRemoteDescription(Self.SingleStreamSDP(Self.RemotePort, RemotePT));

  Check(Self.MS.StreamCount > 0, 'Sanity check: no ports open!');
  Self.MS.Streams[0].AddRTPListener(Self);

  Self.ReceiveDataOfType(RemotePT);

  Self.WaitForSignaled('Waiting for RTP data');

  CheckEquals(TIdRTPT140Payload.ClassName,
              Self.Payload.ClassName,
              'Remote profile not used to determine payload');
end;

procedure TestTIdSDPMultimediaSession.TestInitialize;
begin
end;

procedure TestTIdSDPMultimediaSession.TestIsListening;
begin
  Check(not Self.MS.IsListening, 'Initial IsListening must be false');

  Self.MS.StartListening(Self.SingleStreamSDP(Self.MS.LowestAllowedPort));

  Check(Self.MS.IsListening, 'IsListening after StartListening');

  Self.MS.StopListening;

  Check(not Self.MS.IsListening, 'IsListening after StopListening');
end;

procedure TestTIdSDPMultimediaSession.TestLocalSessionDescription;
var
  CurrentDesc: String;
begin
  CurrentDesc := Self.MS.LocalSessionDescription;
  Self.MS.StartListening(Self.MultiStreamSDP(8000, 9000));
  CheckNotEquals(CurrentDesc,
                 Self.MS.LocalSessionDescription,
                 'After StartListening: session description not updated');

  CurrentDesc := Self.MS.LocalSessionDescription;
  Self.MS.PutOnHold;
  CheckNotEquals(CurrentDesc,
                 Self.MS.LocalSessionDescription,
                 'After PutOnHold: session description not updated');

  CurrentDesc := Self.MS.LocalSessionDescription;
  Self.MS.StopListening;
  CheckNotEquals(CurrentDesc,
                 Self.MS.LocalSessionDescription,
              'After StopListening: session description not updated');
end;

procedure TestTIdSDPMultimediaSession.TestLocalSessionDescriptionWithRefusedStream;
const
  NormalPort          = 8000;
  RefusedPortSentinel = 0;
var
  Desc: TIdSdpPayload;
  S:    String;
begin
  Self.MS.LowestAllowedPort := NormalPort;

  S := Self.MS.StartListening(Self.RefusedStreamSDP(NormalPort));
  Desc := TIdSdpPayload.CreateFrom(S);
  try
    CheckEquals(2, Desc.MediaDescriptionCount, 'Incorrect number of media descriptions');

    CheckEquals('RTP/AVP', Desc.MediaDescriptionAt(0).Transport, 'Transport changed: 1st desc');
    CheckEquals(RefusedPortSentinel, Desc.MediaDescriptionAt(0).Port, 'Port changed: 1st desc');

    CheckEquals('RTP/AVP', Desc.MediaDescriptionAt(1).Transport, 'Transport changed: 2nd desc');
    CheckEquals(NormalPort, Desc.MediaDescriptionAt(1).Port, 'Port changed: 2nd desc');
  finally
    Desc.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestLocalSessionVersionIncrements;
begin
  CheckEquals(0, Self.MS.LocalSessionVersion, 'Before setting the local description');

  Self.MS.StartListening(Self.SingleStreamSDP(ArbitraryPort));
  CheckEquals(0, Self.MS.LocalSessionVersion, 'First sess-version');

  Self.MS.StopListening;
  Self.MS.StartListening(Self.SingleStreamSDP(ArbitraryPort));
  CheckEquals(1, Self.MS.LocalSessionVersion, 'Second sess-version');

  Self.MS.StopListening;
  Self.MS.StartListening(Self.SingleStreamSDP(ArbitraryPort));
  CheckEquals(2, Self.MS.LocalSessionVersion, 'Third sess-version');
end;

procedure TestTIdSDPMultimediaSession.TestMimeType;
begin
  CheckEquals(SdpMimeType, Self.MS.MimeType, 'SDP MIME type');
end;

procedure TestTIdSDPMultimediaSession.TestNetTypeFor;
begin
  CheckEquals(Id_SDP_IN, Self.MS.NetTypeFor(IPv4LocalHost), 'IPv4 address (' + IPv4LocalHost + ')');
  CheckEquals(Id_SDP_IN, Self.MS.NetTypeFor(IPv6LocalHost), 'IPv6 address (' + IPv6LocalHost + ')');
  CheckEquals('UNKNOWN', Self.MS.NetTypeFor(''), 'Unknown net type (the empty string)');
end;

procedure TestTIdSDPMultimediaSession.TestOnHold;
begin
  Self.MS.StartListening(Self.MultiStreamSDP(8000, 9000));
  Check(not Self.MS.OnHold, 'Session shouldn''t be on hold at first');
  Self.MS.PutOnHold;
  Check(Self.MS.OnHold, 'OnHold after PutOnHold');

  Self.MS.TakeOffHold;
  Check(not Self.MS.OnHold, 'OnHold after TakeOffHold');

  CheckEquals(2,
              Self.MS.StreamCount,
              'Sanity check: MultiStreamSDP made the wrong number of streams');
  Self.MS.PutOnHold;
  Self.MS.Streams[0].TakeOffHold;
  Check(Self.MS.OnHold, 'The Session can''t know you manually took any streams off hold');
  Self.MS.Streams[1].TakeOffHold;
  Check(Self.MS.OnHold, 'The Session can''t know you manually took all streams off hold');

  Self.MS.TakeOffHold;
  Check(not Self.MS.OnHold,
        'OnHold after TakeOffHold, having manually taken all streams off hold');
end;

procedure TestTIdSDPMultimediaSession.TestPortAboveHighestAllowedPort;
var
  Desc:        String;
  HighestPort: Cardinal;
begin
  Self.MS.LowestAllowedPort  := 8000;
  Self.MS.HighestAllowedPort := 8100;
  HighestPort := Self.MS.HighestAllowedPort + 2;
  Desc := Self.MS.StartListening(Self.SingleStreamSDP(HighestPort));

  Check(Self.MS.StreamCount > 0,
        'Not enough streams instantiated');
  CheckEquals(0,
              Self.MS.Streams[0].LocalDescription.Port,
              'Port value');
end;

procedure TestTIdSDPMultimediaSession.TestPortAsHighestAllowedPort;
var
  Desc:        String;
  HighestPort: Cardinal;
begin
  Self.MS.LowestAllowedPort  := 8000;
  Self.MS.HighestAllowedPort := 8100;
  HighestPort := Self.MS.HighestAllowedPort;
  Desc := Self.MS.StartListening(Self.SingleStreamSDP(HighestPort));

  Check(Self.MS.StreamCount > 0,
        'Not enough streams instantiated');
  CheckEquals(0,
              Self.MS.Streams[0].LocalDescription.Port,
              'Port value');
end;

procedure TestTIdSDPMultimediaSession.TestPortAsLowestAllowedPort;
var
  Desc:        String;
  LowestPort: Cardinal;
begin
  Self.MS.LowestAllowedPort  := 8000;
  Self.MS.HighestAllowedPort := 8100;
  LowestPort := Self.MS.LowestAllowedPort;
  Desc := Self.MS.StartListening(Self.SingleStreamSDP(LowestPort));

  Check(Self.MS.StreamCount > 0,
        'Not enough streams instantiated');
  CheckEquals(LowestPort,
              Self.MS.Streams[0].LocalDescription.Port,
              'Port value');
end;

procedure TestTIdSDPMultimediaSession.TestPortBelowLowestAllowedPort;
var
  Desc:       String;
  LowestPort: Cardinal;
begin
  Self.MS.LowestAllowedPort  := 8000;
  Self.MS.HighestAllowedPort := 8100;
  LowestPort := Self.MS.LowestAllowedPort - 2;
  Desc := Self.MS.StartListening(Self.SingleStreamSDP(LowestPort));

  Check(Self.MS.StreamCount > 0,
        'Not enough streams instantiated');
  CheckEquals(0,
              Self.MS.Streams[0].LocalDescription.Port,
              'Port value');
end;

procedure TestTIdSDPMultimediaSession.TestPortThreePortRange;
const
  LowPort = 8000;
begin
  // This gives us three allowed ports, and we're trying to open two media
  // streams (which requires four ports).
  Self.MS.LowestAllowedPort  := LowPort;
  Self.MS.HighestAllowedPort := LowPort + 2;

  Self.MS.StartListening(Self.MultiStreamSDP(LowPort, LowPort + 2));

  CheckEquals(2, Self.MS.StreamCount, 'Wrong number of streams');
  CheckEquals(LowPort,
              Self.MS.Streams[0].LocalDescription.Port,
              'First stream port value');
  CheckEquals(0,
              Self.MS.Streams[1].LocalDescription.Port,
              'Second stream port value');
end;

procedure TestTIdSDPMultimediaSession.TestPutOnHold;
var
  Stream1MediaType:  TIdSdpMediaType;
  Stream2MediaType:  TIdSdpMediaType;
  OldSessionVersion: Int64;
begin
  Self.MS.StartListening(Self.MultiStreamSDP(8000, 9000));
  Self.MS.Streams[0].Direction := sdRecvOnly;
  Self.MS.Streams[1].Direction := sdSendRecv;

  Stream1MediaType := Self.MS.Streams[0].LocalDescription.MediaType;
  Stream2MediaType := Self.MS.Streams[1].LocalDescription.MediaType;

  OldSessionVersion := Self.MS.LocalSessionVersion;
  Self.MS.PutOnHold;

  Check(Self.MS.Streams[0].OnHold,
        'Stream #0 not put on hold');
  Check(Self.MS.Streams[1].OnHold,
        'Stream #1 not put on hold');

  CheckEquals(MediaTypeToStr(Stream1MediaType),
              MediaTypeToStr(Self.MS.Streams[0].LocalDescription.MediaType),
              'Stream #1 changed its media type');
  CheckEquals(MediaTypeToStr(Stream2MediaType),
              MediaTypeToStr(Self.MS.Streams[1].LocalDescription.MediaType),
              'Stream #2 changed its media type');

  CheckEquals(OldSessionVersion + 1, Self.MS.LocalSessionVersion, 'sess-version not incremented');
end;

procedure TestTIdSDPMultimediaSession.TestSetLocalMachineName;
begin
  Self.MS.StartListening(Self.SingleStreamSDP(ArbitraryPort));
  Self.MS.LocalMachineName := IPv4LocalHost;

  CheckOrigin(Id_SDP_IN,
              Id_IPv4,
              IPv4LocalHost,
              Self.MS.LocalSessionDescription);

  Self.MS.LocalMachineName := IPv6LocalHost;

  CheckOrigin(Id_SDP_IN,
              Id_IPv6,
              IPv6LocalHost,
              Self.MS.LocalSessionDescription);
end;

procedure TestTIdSDPMultimediaSession.TestSetLocalSessionName;
const
  OldSessionName = 'old';
  NewSessionName = 'new';
begin
  Self.MS.StartListening(Self.SingleStreamSDP(ArbitraryPort));

  CheckSessionName(BlankSessionName,
                   Self.MS.LocalSessionDescription,
                   'Default SessionName value');

  Self.MS.LocalSessionName := OldSessionName;

  CheckSessionName(OldSessionName,
                   Self.MS.LocalSessionDescription,
                  'SessionName not set');

  Self.MS.LocalSessionName := NewSessionName;
  CheckSessionName(NewSessionName,
                   Self.MS.LocalSessionDescription,
                   'SessionName not reset');
end;

procedure TestTIdSDPMultimediaSession.TestSetRemoteDescription;
var
  LowPort:  Cardinal;
  HighPort: Cardinal;
begin
  LowPort  := 8900;
  HighPort := 9900;

  Self.MS.StartListening(Self.MultiStreamSDP(8000, 9000));

  Self.MS.SetRemoteDescription(Self.MultiStreamSDP(LowPort, HighPort));

  CheckEquals(LowPort,
              Self.MS.Streams[0].RemoteDescription.Port,
              'Remote description of first stream not set');
  CheckEquals(HighPort,
              Self.MS.Streams[1].RemoteDescription.Port,
              'Remote description of second stream not set');
end;

procedure TestTIdSDPMultimediaSession.TestSetRemoteDescriptionWithSdpPayload;
var
  LowPort:  Cardinal;
  HighPort: Cardinal;
  SDP:      TIdSdpPayload;
begin
  LowPort  := 8900;
  HighPort := 9900;

  Self.MS.StartListening(Self.MultiStreamSDP(8000, 9000));

  SDP := TIdSdpPayload.CreateFrom(Self.MultiStreamSDP(LowPort, HighPort));
  try
    Self.MS.StartListening(SDP.AsString);
    Self.MS.SetRemoteDescription(SDP);

    CheckEquals(LowPort,
                Self.MS.Streams[0].RemoteDescription.Port,
                'Remote description of first stream not set');
    CheckEquals(HighPort,
                Self.MS.Streams[1].RemoteDescription.Port,
                'Remote description of second stream not set');
  finally
    SDP.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestSetUsername;
const
  UsernameCase       = 'Case';
  UsernameWintermute = 'Wintermute';
begin
  CheckEquals(BlankUsername, Self.MS.Username, 'Default Username value');

  Self.MS.StartListening(Self.SingleStreamSDP(ArbitraryPort));

  Self.MS.Username := UsernameCase;
  CheckOriginUsername(Self.MS.Username, Self.MS.LocalSessionDescription);

  Self.MS.Username := UsernameWintermute;
  CheckOriginUsername(Self.MS.Username, Self.MS.LocalSessionDescription);
end;

procedure TestTIdSDPMultimediaSession.TestSetRemoteDescriptionMalformedSdp;
begin
  try
    Self.MS.SetRemoteDescription('');
    Fail('Set remote description on a malformed session description');
  except
    on EParserError do;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestStartListeningSingleStream;
var
  Port: Cardinal;
begin
  Port := 8000;
  Self.MS.StartListening(Self.SingleStreamSDP(Port));

  Self.CheckPortActive('127.0.0.1',
                       Port,
                       'Server not listening on port 8000');
  CheckEquals(1, Self.MS.StreamCount, 'StreamCount');
end;

procedure TestTIdSDPMultimediaSession.TestStartListeningMalformedSdp;
begin
  try
    Self.MS.StartListening('');
    Fail('Started listening on malformed SDP');
  except
    on EParserError do;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestStartListeningMultipleStreams;
var
  LowPort:  Cardinal;
  HighPort: Cardinal;
begin
  LowPort  := 8000;
  HighPort := 9000;

  Self.MS.StartListening(Self.MultiStreamSDP(LowPort, HighPort));

  CheckEquals(2, Self.MS.StreamCount, 'StreamCount');
  Self.CheckPortActive('127.0.0.1',
                       LowPort,
                       'Server not listening on 127.0.0.7:' + IntToStr(LowPort));
  Self.CheckPortActive(GStack.LocalAddress,
                       HighPort,
                       'Server not listening on '
                     + GStack.LocalAddress + ':' + IntToStr(HighPort));
end;

procedure TestTIdSDPMultimediaSession.TestStartListeningPortsOutsideAllowedRange;
var
  I:   Integer;
  SDP: TIdSdpPayload;
begin
  // We create an abnormal setup: there's no legal ports to use!
  Self.MS.LowestAllowedPort := 10000;
  Self.MS.HighestAllowedPort := 9000;

  SDP := TIdSdpPayload.CreateFrom(Self.MS.StartListening(Self.MultiStreamSDP(Self.MS.HighestAllowedPort, Self.MS.LowestAllowedPort)));
  try
    for I := 0 to SDP.MediaDescriptionCount - 1 do
      CheckEquals(0,
                  SDP.MediaDescriptionAt(0).Port,
                  IntToStr(I) + 'th description''s port');
  finally
    SDP.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestStartListeningRegistersLocalRtpMaps;
const
  EncodingName   = T140EncodingName + '/1000';
  PayloadType    = 96;
  TEEncodingName = TelephoneEventEncoding;
  TEPayloadType  = 97;
var
  SDP: String;
begin
  SDP :=  'v=0'#13#10
        + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
        + 's=-'#13#10
        + 'c=IN IP4 127.0.0.1'#13#10
        + 'm=text 8000 RTP/AVP ' + IntToStr(PayloadType) + #13#10
        + 'a=rtpmap:' + IntToStr(PayloadType) + ' ' + EncodingName + #13#10
        + 'm=audio 8002 RTP/AVP ' + IntToStr(TEPayloadType) + #13#10
        + 'a=rtpmap:' + IntToStr(TEPayloadType) + ' ' + TEEncodingName + #13#10;

  Check(not Self.MS.LocalProfile.HasPayloadType(PayloadType),
        'Sanity check: profile already knows about ' + EncodingName + '!');
  Check(not Self.MS.LocalProfile.HasPayloadType(TEPayloadType),
        'Sanity check: profile already knows about ' + TEEncodingName + '!');

  Self.MS.StartListening(SDP);

  Check(Self.MS.LocalProfile.HasPayloadType(PayloadType),
        EncodingName + ' not registered');
  Check(Self.MS.LocalProfile.HasPayloadType(TEPayloadType),
        TEEncodingName + ' not registered');
end;

procedure TestTIdSDPMultimediaSession.TestStartListeningRegistersRemoteRtpMaps;
const
  EncodingName   = T140EncodingName + '/1000';
  PayloadType    = 96;
  TEEncodingName = TelephoneEventEncoding;
  TEPayloadType  = 97;
var
  LocalSDP: String;
  SDP:      String;
begin
  SDP :=  'v=0'#13#10
        + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
        + 's=-'#13#10
        + 'c=IN IP4 127.0.0.1'#13#10
        + 'm=text 8000 RTP/AVP ' + IntToStr(PayloadType) + #13#10
        + 'a=rtpmap:' + IntToStr(PayloadType) + ' ' + EncodingName + #13#10
        + 'm=audio 8002 RTP/AVP ' + IntToStr(TEPayloadType) + #13#10
        + 'a=rtpmap:' + IntToStr(TEPayloadType) + ' ' + TEEncodingName + #13#10;

  // We need two streams to match the number of streams in SDP
  LocalSDP := 'v=0'#13#10
            + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
            + 's=-'#13#10
            + 'c=IN IP4 127.0.0.1'#13#10
            + 'm=audio 9000 RTP/AVP 0'#13#10
            + 'm=audio 9002 RTP/AVP 0'#13#10;

  Check(not Self.MS.RemoteProfile.HasPayloadType(PayloadType),
        'Sanity check: profile already knows about ' + EncodingName + '!');
  Check(not Self.MS.RemoteProfile.HasPayloadType(TEPayloadType),
        'Sanity check: profile already knows about ' + TEEncodingName + '!');

  Self.MS.StartListening(LocalSDP);
  Self.MS.SetRemoteDescription(SDP);

  Check(Self.MS.RemoteProfile.HasPayloadType(PayloadType),
        EncodingName + ' not registered');
  Check(Self.MS.RemoteProfile.HasPayloadType(TEPayloadType),
        TEEncodingName + ' not registered');
end;

procedure TestTIdSDPMultimediaSession.TestStartListeningTriesConsecutivePorts;
const
  BlockedPort = 8000;
var
  PortBlocker: TIdUdpServer;
  SDP:         String;
begin
  PortBlocker := TIdUDPServer.Create(nil);
  try
    PortBlocker.DefaultPort := BlockedPort;
    PortBlocker.Active := true;

    SDP :=  'v=0'#13#10
          + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
          + 's=-'#13#10
          + 'c=IN IP4 127.0.0.1'#13#10
          + 'm=text ' + IntToStr(BlockedPort) + ' RTP/AVP 0'#13#10;

    SDP := Self.MS.StartListening(SDP);
    CheckPortActive('127.0.0.1', BlockedPort + 2, 'Next available RTP port not used');

    Check(Pos(IntToStr(BlockedPort + 2), SDP) > 0,
          'Expected actual port (' + IntToStr(BlockedPort + 2)
        + ') not present in resultant SDP');

    Self.MS.StopListening;

    CheckPortFree('127.0.0.1', BlockedPort + 2, 'Port not closed');
  finally
    PortBlocker.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestStopListening;
var
  LowPort:  Cardinal;
  HighPort: Cardinal;
begin
  LowPort  := 8000;
  HighPort := 9000;

  Self.MS.StartListening(Self.MultiStreamSDP(LowPort, HighPort));

  Self.MS.StopListening;

  Self.CheckPortFree('127.0.0.1',
                     LowPort,
                     'Server still listening on 127.0.0.7:' + IntToStr(LowPort));
  Self.CheckPortFree(GStack.LocalAddress,
                     HighPort,
                     'Server still listening on '
                   + GStack.LocalAddress + ':' + IntToStr(HighPort));
end;

procedure TestTIdSDPMultimediaSession.TestTakeOffHold;
var
  OldSessionVersion: Int64;
begin
  Self.MS.StartListening(Self.MultiStreamSDP(8000, 9000));
  Self.MS.Streams[0].Direction := sdRecvOnly;
  Self.MS.Streams[1].Direction := sdSendRecv;

  Self.MS.PutOnHold;

  OldSessionVersion := Self.MS.LocalSessionVersion;
  Self.MS.TakeOffHold;

  Check(not Self.MS.Streams[0].OnHold,
        'Stream #0 not taken off hold');
  Check(not Self.MS.Streams[1].OnHold,
        'Stream #1 not taken off hold');

  CheckEquals(OldSessionVersion + 1, Self.MS.LocalSessionVersion, 'sess-version not incremented');
end;

//******************************************************************************
//* TestTIdSdpNatMasquerader                                                   *
//******************************************************************************
//* TestTIdSdpNatMasquerader Public methods ************************************

procedure TestTIdSdpNatMasquerader.SetUp;
begin
end;

procedure TestTIdSdpNatMasquerader.TearDown;
begin
end;

procedure TestTIdSdpNatMasquerader.TestNatify;
begin
end;

initialization
  RegisterTest('IdSdpParser', Suite);
end.
