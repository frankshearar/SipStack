unit TestIdSdpParser;

interface

uses
  IdSdpParser, TestFramework;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestAddressTypeToStr;
    procedure TestBandwidthTypeToStr;
    procedure TestStrToAddressType;
    procedure TestStrToBandwidthType;
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
  end;

  TestTIdSdpParser = class(TTestCase)
  private
    P:       TIdSdpParser;
    Payload: TIdSdpPayload;

    procedure CheckMalformedOrigin(const OriginValue: String);
    procedure CheckMalformedPhoneNumber(const PhoneNumberValue: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsAddressType;
    procedure TestIsBandwidthType;
    procedure TestIsNetType;
    procedure TestIsNumber;
    procedure TestIsPhone;
    procedure TestIsPhoneNumber;
    procedure TestIsText;
    procedure TestParseBandwidth;
    procedure TestParseBandwidthMultipleHeaders;
    procedure TestParseConnection;
    procedure TestParseConnectionMalformed;
    procedure TestParseEmail;
    procedure TestParseEmailBracketedName;
    procedure TestParseEmailRfc822;
    procedure TestParseEmptyStream;
    procedure TestParseHeaderMissingName;
    procedure TestParseHeaderMissingValue;
    procedure TestParseHeadersInvalidOrder;
    procedure TestParseInfo;
    procedure TestParseInfoIllegalCharacters;
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
  Result.AddTest(TestTIdSdpBandwidths.Suite);
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
  CheckEquals(Id_SDP_CT, BandwidthTypeToStr(btCT), 'btCT');
  CheckEquals(Id_SDP_AS, BandwidthTypeToStr(btAS), 'btAS');
  CheckEquals(Id_SDP_RS, BandwidthTypeToStr(btRS), 'btRS');
  CheckEquals(Id_SDP_RR, BandwidthTypeToStr(btRR), 'btRR');

  // To check that ALL TIdSdpBandwidthTypes can be converted
  for B := Low(TIdSdpBandwidthType) to High(TIdSdpBandwidthType) do
    BandwidthTypeToStr(B);
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
  Check(btCT = StrToBandwidthType('CT'),      'CT');
  Check(btCT = StrToBandwidthType(Id_SDP_CT), 'Id_SDP_CT constant');
  Check(btAS = StrToBandwidthType('AS'),      'AS');
  Check(btAS = StrToBandwidthType(Id_SDP_AS), 'Id_SDP_AS constant');
  Check(btRS = StrToBandwidthType('RS'),      'RS');
  Check(btRS = StrToBandwidthType(Id_SDP_RS), 'Id_SDP_RS constant');
  Check(btRR = StrToBandwidthType('RR'),      'RR');
  Check(btRR = StrToBandwidthType(Id_SDP_RR), 'Id_SDP_RR constant');

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
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'p=' + PhoneNumberValue);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPPhoneName, PhoneNumberValue]),
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
  Check(not TIdSdpParser.IsBandwidthType(''),        '''''');
  Check(not TIdSdpParser.IsBandwidthType('ct'),      'ct');
  Check(    TIdSdpParser.IsBandwidthType('CT'),      'CT');
  Check(    TIdSdpParser.IsBandwidthType(Id_SDP_CT), 'Id_SDP_CT constant');
  Check(    TIdSdpParser.IsBandwidthType('AS'),      'AS');
  Check(    TIdSdpParser.IsBandwidthType(Id_SDP_AS), 'Id_SDP_AS constant');
  Check(    TIdSdpParser.IsBandwidthType('RS'),      'RS');
  Check(    TIdSdpParser.IsBandwidthType(Id_SDP_RS), 'Id_SDP_RS constant');
  Check(    TIdSdpParser.IsBandwidthType('RR'),      'RR');
  Check(    TIdSdpParser.IsBandwidthType(Id_SDP_RR), 'Id_SDP_RR constant');
end;

procedure TestTIdSdpParser.TestIsNetType;
begin
  Check(not TIdSdpParser.IsNetType(''),        '''''');
  Check(not TIdSdpParser.IsNetType('in'),      'in');
  Check(    TIdSdpParser.IsNetType('IN'),      'IN');
  Check(    TIdSdpParser.IsNetType(Id_SDP_IN), 'Id_SDP_IN constant');

end;

procedure TestTIdSdpParser.TestIsNumber;
begin
  Check(not TIdSdpParser.IsNumber(''),                     '''''');
  Check(not TIdSdpParser.IsNumber('1 '),                   '1 SP');
  Check(not TIdSdpParser.IsNumber(' 1'),                   'SP 1');
  Check(not TIdSdpParser.IsNumber('a'),                    'a');
  Check(    TIdSdpParser.IsNumber('1'),                    '1');
  Check(    TIdSdpParser.IsNumber('98765432109876543210'), '98765432109876543210');
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

procedure TestTIdSdpParser.TestParseBandwidth;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'b=RR:666');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,     Self.Payload.Bandwidths.Count, 'Bandwidths.Count');
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
    CheckEquals(2,     Self.Payload.Bandwidths.Count, 'Bandwidths.Count');
    Check      (btRR = Self.Payload.Bandwidths[0].BandwidthType, '[0].BandwidthType');
    CheckEquals(666,   Self.Payload.Bandwidths[0].Bandwidth,     '[0].Bandwidth');
    Check      (btCT = Self.Payload.Bandwidths[1].BandwidthType, '[1].BandwidthType');
    CheckEquals(123,   Self.Payload.Bandwidths[1].Bandwidth,     '[1].Bandwidth');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseConnection;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'c=IN IP4 224.2.17.12/127');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('IN',             Self.Payload.Connection.NetType,     'NetType');
    Check      (Id_IPv4 =         Self.Payload.Connection.AddressType, 'AddressType');
    CheckEquals('224.2.17.12/127', Self.Payload.Connection.Address,    'Address');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseConnectionMalformed;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'c=IN');
  try
    Self.P.Source := S;
    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPConnectionName, 'IN']),
                    E.Message,
                    'Unexpected exception');
    end;
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
        CheckEquals(BadHeaderOrder,
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
        CheckEquals(BadHeaderOrder,
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