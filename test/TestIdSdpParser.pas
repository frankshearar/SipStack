unit TestIdSdpParser;

interface

uses
  IdSdpParser, TestFramework;

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

  TestTIdSdpMediaDescription = class(TTestCase)
  private
    M: TIdSdpMediaDescription;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddFormatAndFormatCount;
    procedure TestFormatClear;
    procedure TestGetFormat;
  end;

  TestTIdSdpAttributes = class(TTestCase)
  private
    B: TIdSdpAttributes;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestClear;
    procedure TestContains;
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

  TestTIdSdpMediaDescriptions = class(TTestCase)
  private
    R: TIdSdpMediaDescriptions;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
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
  end;

  TestTIdSdpParser = class(TTestCase)
  private
    P:       TIdSdpParser;
    Payload: TIdSdpPayload;

    procedure CheckMalformedAttribute(const Value: String);
    procedure CheckMalformedConnection(const Value: String);
    procedure CheckMalformedOrigin(const OriginValue: String);
    procedure CheckMalformedPhoneNumber(const PhoneNumberValue: String);
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
    procedure TestIsDecimalUchar;
    procedure TestIsFQDN;
    procedure TestIsIpv4Address;
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
    procedure TestParseBandwidthMultipleHeaders;
    procedure TestParseConnectionInSessionAndMediaDescription;
    procedure TestParseConnectionMalformed;
    procedure TestParseConnectionMalformedMulticastAddress;
    procedure TestParseConnectionMalformedUnicastAddress;
    procedure TestParseConnectionMulticastAddress;
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
    procedure TestParseMediaDescription;
    procedure TestParseMediaDescriptionMalformedFormatList;
    procedure TestParseMediaDescriptionMalformedPort;
    procedure TestParseMediaDescriptionMissingFormatList;
    procedure TestParseMediaDescriptionMissingInformation;
    procedure TestParseMediaDescriptionMissingPort;
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
    procedure TestParseMissingVersion;
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
    procedure TestParseSessionIllegalCharacters;
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

const
  MinimumPayload = 'v=0'#13#10
                 + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                 + 's=Minimum Session Info';

implementation

uses
  Classes, IdSimpleParser, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSdpParser unit tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSdpMediaDescription.Suite);
  Result.AddTest(TestTIdSdpAttributes.Suite);
  Result.AddTest(TestTIdSdpBandwidths.Suite);
  Result.AddTest(TestTIdSdpRepeats.Suite);
  Result.AddTest(TestTIdSdpTimes.Suite);
  Result.AddTest(TestTIdSdpZoneAdjustments.Suite);
  Result.AddTest(TestTIdSdpParser.Suite);
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

  // To check that ALL TIdSdpMediaTypes can be converted
  for M := Low(TIdSdpMediaType) to High(TIdSdpMediaType) do
    MediaTypeToStr(M);
end;

procedure TestFunctions.TestStrToAddressType;
begin
  Check(Id_IPv4 = StrToAddressType('IP4'), 'IP4');
  Check(Id_IPv6 = StrToAddressType('IP6'), 'IP6');

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
begin
  Check(btConferenceTotal     = StrToBandwidthType('CT'),                       'CT');
  Check(btConferenceTotal     = StrToBandwidthType(Id_SDP_ConferenceTotal),     'Id_SDP_ConferenceTotal constant');
  Check(btApplicationSpecific = StrToBandwidthType('AS'),                       'AS');
  Check(btApplicationSpecific = StrToBandwidthType(Id_SDP_ApplicationSpecific), 'Id_SDP_ApplicationSpecific constant');
  Check(btRS                  = StrToBandwidthType('RS'),                       'RS');
  Check(btRS                  = StrToBandwidthType(Id_SDP_RS),                  'Id_SDP_RS constant');
  Check(btRR                  = StrToBandwidthType('RR'),                       'RR');
  Check(btRR                  = StrToBandwidthType(Id_SDP_RR),                  'Id_SDP_RR constant');

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
begin
  Check(ktClear  = StrToKeyType(Id_SDP_Clear),  'Id_SDP_Clear constant');
  Check(ktBase64 = StrToKeyType(Id_SDP_Base64), 'Id_SDP_Base64 constant');
  Check(ktURI    = StrToKeyType(Id_SDP_URI),    'Id_SDP_URI constant');
  Check(ktPrompt = StrToKeyType(Id_SDP_Prompt), 'Id_SDP_Prompt constant');

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
begin
  Check(mtAudio       = StrToMediaType('audio'),       'audio');
  Check(mtAudio       = StrToMediaType('audio'),       'RSSDPMediaTypeAudio constant');
  Check(mtVideo       = StrToMediaType('video'),       'video');
  Check(mtVideo       = StrToMediaType('video'),       'RSSDPMediaTypeVideo constant');
  Check(mtApplication = StrToMediaType('application'), 'application');
  Check(mtApplication = StrToMediaType('application'), 'RSSDPMediaTypeApplication constant');
  Check(mtData        = StrToMediaType('data'),        'data');
  Check(mtData        = StrToMediaType('data'),        'RSSDPMediaTypeData constant');
  Check(mtControl     = StrToMediaType('control'),     'control');
  Check(mtControl     = StrToMediaType('control'),     'RSSDPMediaTypeControl constant');

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

//******************************************************************************
//* TestTIdSdpAttributes                                                       *
//******************************************************************************
//* TestTIdSdpAttributes Public methods ****************************************

procedure TestTIdSdpAttributes.SetUp;
begin
  inherited SetUp;

  Self.B := TIdSdpAttributes.Create;
end;

procedure TestTIdSdpAttributes.TearDown;
begin
  Self.B.Free;

  inherited TearDown;
end;

//* TestTIdSdpAttributes Published methods *************************************

procedure TestTIdSdpAttributes.TestAddAndCount;
begin
  CheckEquals(0, Self.B.Count, 'Count on new list');
  Self.B.Add(TIdSdpAttribute.Create);
  CheckEquals(1, Self.B.Count, 'Count after Add()');
  Self.B.Add(TIdSdpAttribute.Create);
  CheckEquals(2, Self.B.Count, 'Count after 2nd Add()');
end;

procedure TestTIdSdpAttributes.TestClear;
begin
  Self.B.Add(TIdSdpAttribute.Create);
  Self.B.Add(TIdSdpAttribute.Create);
  Self.B.Add(TIdSdpAttribute.Create);

  Self.B.Clear;
  CheckEquals(0, Self.B.Count, 'Count after clear');
end;

procedure TestTIdSdpAttributes.TestContains;
var
  O: TIdSdpAttribute;
begin
  O := TIdSdpAttribute.Create;
  Check(not Self.B.Contains(O), 'Contains object when it shouldn''t');
  Self.B.Add(O);
  Check(Self.B.Contains(O), 'Doesn''t contain object when it should');
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
//* TestTIdSdpMediaDescriptions                                                *
//******************************************************************************
//* TestTIdSdpMediaDescriptions Public methods *********************************

procedure TestTIdSdpMediaDescriptions.SetUp;
begin
  inherited SetUp;

  Self.R := TIdSdpMediaDescriptions.Create;
end;

procedure TestTIdSdpMediaDescriptions.TearDown;
begin
  Self.R.Free;

  inherited TearDown;
end;

//* TestTIdSdpMediaDescriptions Published methods ******************************

procedure TestTIdSdpMediaDescriptions.TestAddAndCount;
begin
  CheckEquals(0, Self.R.Count, 'Count on new list');
  Self.R.Add(TIdSdpMediaDescription.Create);
  CheckEquals(1, Self.R.Count, 'Count after Add()');
  Self.R.Add(TIdSdpMediaDescription.Create);
  CheckEquals(2, Self.R.Count, 'Count after 2nd Add()');
end;

procedure TestTIdSdpMediaDescriptions.TestClear;
begin
  Self.R.Add(TIdSdpMediaDescription.Create);
  Self.R.Add(TIdSdpMediaDescription.Create);
  Self.R.Add(TIdSdpMediaDescription.Create);

  Self.R.Clear;
  CheckEquals(0, Self.R.Count, 'Count after clear');
end;

procedure TestTIdSdpMediaDescriptions.TestContains;
var
  O: TIdSdpMediaDescription;
begin
  O := TIdSdpMediaDescription.Create;
  Check(not Self.R.Contains(O), 'Contains object when it shouldn''t');
  Self.R.Add(O);
  Check(Self.R.Contains(O), 'Doesn''t contain object when it should');
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
begin
  Self.CheckMalformedOptionalSessionHeader(RSSDPConnectionName, Value);
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
        CheckEquals(Format(MalformedToken, [RSSDPOriginName, OriginValue]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.CheckMalformedPhoneNumber(const PhoneNumberValue: String);
begin
  Self.CheckMalformedOptionalSessionHeader(RSSDPPhoneName, PhoneNumberValue);
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
        CheckEquals(Format(MalformedToken, [RSSDPMediaDescriptionName, Value]),
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
        CheckEquals(Format(MalformedToken, [Name, Value]),
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
        CheckEquals(Format(MalformedToken, [RSSDPRepeatName, RepeatValue]),
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

procedure TestTIdSdpParser.TestIsDecimalUchar;
begin
  Check(not TIdSdpParser.IsDecimalUchar(''),      '''''');
  Check(not TIdSdpParser.IsDecimalUchar('ct'),    'ct');
  Check(not TIdSdpParser.IsDecimalUchar('abc'#0), 'abc#0');
  Check(    TIdSdpParser.IsDecimalUchar('0'),     '0');
  Check(    TIdSdpParser.IsDecimalUchar('13'),    '13');
  Check(    TIdSdpParser.IsDecimalUchar('255'),   '255');
  Check(not TIdSdpParser.IsDecimalUchar('256'),   '256');
end;

procedure TestTIdSdpParser.TestIsFQDN;
begin
  Check(not TIdSdpParser.IsFQDN(''),        '''''');
  Check(not TIdSdpParser.IsFQDN('a.b.c-'),  'No trailing hyphens');
  Check(not TIdSdpParser.IsFQDN('a.-b.c'),  'No leading hyphens');
  Check(not TIdSdpParser.IsFQDN('a.1b.c'),  'No leading digits');
  Check(not TIdSdpParser.IsFQDN('a..1b.c'), 'Empty label');
  Check(not TIdSdpParser.IsFQDN('a.b.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz'),
        'Length(<label>) >= 64');

  Check(TIdSdpParser.IsFQDN('abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijk'),
        'Length(<label>) = 63');

  Check(    TIdSdpParser.IsFQDN('a'),        'a');
  Check(    TIdSdpParser.IsFQDN('a.b.c'),    'a.b.c');
  Check(    TIdSdpParser.IsFQDN('a-a.b.c'),  'a-a.b.c');
  Check(    TIdSdpParser.IsFQDN('a1.b2.c3'), 'a1.b2.c3');
end;

procedure TestTIdSdpParser.TestIsIpv4Address;
begin
  Check(not TIdSdpParser.IsIPv4Address(''),                '''''');
  Check(not TIdSdpParser.IsIPv4Address('1'),               '1');
  Check(not TIdSdpParser.IsIPv4Address('abcd'),            'abcd');
  Check(not TIdSdpParser.IsIPv4Address('224.'),            '224.');
  Check(    TIdSdpParser.IsIPv4Address('127.2.17.12'),     '127.2.17.12');
  Check(    TIdSdpParser.IsIPv4Address('224.2.17.12'),     '224.2.17.12');
  Check(    TIdSdpParser.IsIPv4Address('0.0.0.0'),         '0.0.0.0');
  Check(not TIdSdpParser.IsIPv4Address('-1.0.0.0'),        '-1.0.0.0');
  Check(    TIdSdpParser.IsIPv4Address('224.0.0.0'),       '224.0.0.0');
  Check(    TIdSdpParser.IsIPv4Address('224.255.255.255'), '224.255.255.255');
  Check(not TIdSdpParser.IsIPv4Address('224.255.255.256'), '224.255.255.256');
  Check(    TIdSdpParser.IsIPv4Address('255.255.255.255'), '255.255.255.255');
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
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, ''),                '''''');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, '1'),               '1');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, 'abcd'),            'abcd');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.'),            '224.');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, '127.2.17.12'),     '127.2.17.12');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.2.17.12'),     '224.2.17.12');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.0.0.0'),       '224.0.0.0');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.255.255.255'), '224.255.255.255');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.255.255.256'), '224.255.255.256');

  // ipv6
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
    CheckEquals(1,          Self.Payload.Attributes.Count,    'Attributes.Count');
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
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'a=orient:landscape');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,           Self.Payload.Attributes.Count,    'Attributes.Count');
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
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.11/127'); // note the difference compared with the session c.
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('IN',          Self.Payload.Connection.NetType,     'NetType');
    Check      (Id_IPv4 =      Self.Payload.Connection.AddressType, 'AddressType');
    CheckEquals('224.2.17.12', Self.Payload.Connection.Address,     'Address');
    CheckEquals(127,           Self.Payload.Connection.TTL,         'TTL');

    CheckEquals(1,                 Self.Payload.MediaDescriptions.Count,                     'MediaDescriptions.Count');
    CheckEquals('IN',              Self.Payload.MediaDescriptions[0].Connection.NetType,     'Connection.NetType');
    Check      (Id_IPv4 =          Self.Payload.MediaDescriptions[0].Connection.AddressType, 'Connection.AddressType');
    CheckEquals('224.2.17.11',     Self.Payload.MediaDescriptions[0].Connection.Address,     'Connection.Address');
    CheckEquals(127,               Self.Payload.MediaDescriptions[0].Connection.TTL,         'Connection.TTL');
    Check      (                   Self.Payload.MediaDescriptions[0].HasConnection,          'HasConnection');
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
    CheckEquals('IN',          Self.Payload.Connection.NetType,     'NetType');
    Check      (Id_IPv4 =      Self.Payload.Connection.AddressType, 'AddressType');
    CheckEquals('224.2.17.12', Self.Payload.Connection.Address,     'Address');
    CheckEquals(127,           Self.Payload.Connection.TTL,         'TTL');
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
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'c=IN IP4 127.2.17.12');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('IN',          Self.Payload.Connection.NetType,     'NetType');
    Check      (Id_IPv4 =      Self.Payload.Connection.AddressType, 'AddressType');
    CheckEquals('127.2.17.12', Self.Payload.Connection.Address,     'Address');
    CheckEquals(0,             Self.Payload.Connection.TTL,         'TTL');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseEmail;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'e=nihilistic@mystics.net');
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
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'e=nihilistic@mystics.net (Apostolic alcoholics)');
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
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'e="Apostolic alcoholics" <nihilistic@mystics.net>');
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
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'i=An optional header');
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
        CheckEquals(Format(MalformedToken, [RSSDPInformationName, 'haha'#0'haha']),
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
                          + 'k=uri:sip:wintermute@tessier-ashpool.co.lu');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    Check(ktUri =                                       Self.Payload.Key.KeyType, 'KeyType');
    CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Self.Payload.Key.Value,   'Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseKeyMalformedPrompt;
begin
  Self.CheckMalformedKey('prompt:sip:wintermute@tessier-ashpool.co.lu');
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
    CheckEquals(1, Self.Payload.MediaDescriptions.Count, 'MediaDescriptions.Count');

    Check      (mtData       = Self.Payload.MediaDescriptions[0].MediaType,     'MediaDescriptions[0].MediaType');
    CheckEquals(6666,          Self.Payload.MediaDescriptions[0].Port,          'MediaDescriptions[0].Port');
    CheckEquals(0,             Self.Payload.MediaDescriptions[0].PortCount,     'MediaDescriptions[0].PortCount');
    CheckEquals('RTP/AVP',     Self.Payload.MediaDescriptions[0].Transport,     'MediaDescriptions[0].Transport');
    CheckEquals('Information', Self.Payload.MediaDescriptions[0].Info,          'MediaDescriptions[0].Info');
    CheckEquals(1,             Self.Payload.MediaDescriptions[0].FormatCount,   'MediaDescriptions[0].FormatCount');
    CheckEquals('1',           Self.Payload.MediaDescriptions[0].Formats[0],    'MediaDescriptions[0].Formats[0]');
    Check      (           not Self.Payload.MediaDescriptions[0].HasConnection, 'MediaDescriptions[0].Connection');
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

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(BadHeaderOrder,
                    E.Message,
                    'Unexpected exception');
    end;
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
    CheckEquals(0, Self.Payload.Attributes.Count,                      'Session attributes');
    CheckEquals(1, Self.Payload.MediaDescriptions.Count,               'MediaDescriptions.Count');
    CheckEquals(3, Self.Payload.MediaDescriptions[0].Attributes.Count, 'Attributes.Count');

    CheckEquals('recvonly',              Self.Payload.MediaDescriptions[0].Attributes[0].Name,  'Attributes[0].Name');
    CheckEquals('',                      Self.Payload.MediaDescriptions[0].Attributes[0].Value, 'Attributes[0].Value');
    CheckEquals('T38FaxTranscodingJBIG', Self.Payload.MediaDescriptions[0].Attributes[1].Name,  'Attributes[1].Name');
    CheckEquals('',                      Self.Payload.MediaDescriptions[0].Attributes[1].Value, 'Attributes[1].Value');
    CheckEquals('type',                  Self.Payload.MediaDescriptions[0].Attributes[2].Name,  'Attributes[2].Name');
    CheckEquals('broadcast',             Self.Payload.MediaDescriptions[0].Attributes[2].Value, 'Attributes[2].Value');
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
    CheckEquals(1,     Self.Payload.MediaDescriptions.Count,                          'MediaDescriptions.Count');
    CheckEquals(1,     Self.Payload.MediaDescriptions[0].Bandwidths.Count,            'MediaDescriptions.Bandwidths.Count');
    Check      (btRR = Self.Payload.MediaDescriptions[0].Bandwidths[0].BandwidthType, 'BandwidthType');
    CheckEquals(666,   Self.Payload.MediaDescriptions[0].Bandwidths[0].Bandwidth,     'Bandwidth');
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
                          + 'c=IN IP4 224.2.17.12/127');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,                 Self.Payload.MediaDescriptions.Count,                     'MediaDescriptions.Count');
    CheckEquals('IN',              Self.Payload.MediaDescriptions[0].Connection.NetType,     'Connection.NetType');
    Check      (Id_IPv4 =          Self.Payload.MediaDescriptions[0].Connection.AddressType, 'Connection.AddressType');
    CheckEquals('224.2.17.12',     Self.Payload.MediaDescriptions[0].Connection.Address,     'Connection.Address');
    CheckEquals(127,               Self.Payload.MediaDescriptions[0].Connection.TTL,         'Connection.TTL');
    Check      (                   Self.Payload.MediaDescriptions[0].HasConnection,          'HasConnection');
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
                          + 'k=uri:sip:wintermute@tessier-ashpool.co.lu');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    CheckEquals(1, Self.Payload.MediaDescriptions.Count, 'MediaDescriptions.Count');

    Check(ktUri = Self.Payload.MediaDescriptions[0].Key.KeyType, 'KeyType');

    CheckEquals('sip:wintermute@tessier-ashpool.co.lu',
                Self.Payload.MediaDescriptions[0].Key.Value,
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
    CheckEquals(1,             Self.Payload.MediaDescriptions.Count,          'MediaDescriptions.Count');
    CheckEquals(5,             Self.Payload.MediaDescriptions[0].FormatCount, 'MediaDescriptions[0].FormatCount');
    CheckEquals('31',          Self.Payload.MediaDescriptions[0].Formats[0],  'MediaDescriptions[0].Formats[0]');
    CheckEquals('23',          Self.Payload.MediaDescriptions[0].Formats[1],  'MediaDescriptions[0].Formats[1]');
    CheckEquals('ape',         Self.Payload.MediaDescriptions[0].Formats[2],  'MediaDescriptions[0].Formats[2]');
    CheckEquals('convolution', Self.Payload.MediaDescriptions[0].Formats[3],  'MediaDescriptions[0].Formats[3]');
    CheckEquals('3vilution',   Self.Payload.MediaDescriptions[0].Formats[4],  'MediaDescriptions[0].Formats[4]');
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
    CheckEquals(1, Self.Payload.MediaDescriptions.Count, 'MediaDescriptions.Count');

    Check      (mtVideo      = Self.Payload.MediaDescriptions[0].MediaType, 'MediaDescriptions[0].MediaType');
    CheckEquals(49170,         Self.Payload.MediaDescriptions[0].Port,      'MediaDescriptions[0].Port');
    CheckEquals(2,             Self.Payload.MediaDescriptions[0].PortCount, 'MediaDescriptions[0].PortCount');
    CheckEquals('RTP/AVP',     Self.Payload.MediaDescriptions[0].Transport, 'MediaDescriptions[0].Transport');
    CheckEquals('Information', Self.Payload.MediaDescriptions[0].Info,      'MediaDescriptions[0].Info');
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

    CheckEquals('', Self.Payload.Info, 'Info');
    CheckEquals(0, Self.Payload.Times.Count, 'Times.Count');

    Check(not Self.Payload.HasConnection, 'Connection');
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
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'p=Ziggy Stardust <+99 666 0942-3>');
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
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'p=Ziggy Stardust <+99 666 0942-3>'#13#10
                          + 'p=Ziggy Stardust <+99 666 0942-3>');
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
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'p=<+99 666 0942-3>');
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
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'p=+99 666 0942-3'#0);
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
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'u=http://127.0.0.1/');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('http://127.0.0.1/', Self.Payload.URI.GetFullURI, 'URI');
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
        CheckEquals(Format(MalformedToken, [RSSDPVersionName, 'a']),
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

initialization
  RegisterTest('IdSdpParser', Suite);
end.