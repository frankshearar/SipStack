unit TestIdSipParser;

interface

uses
  Classes, IdSipParser, TestFramework, TestFrameworkEx;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestIsEqual;
    procedure TestShortMonthToInt;
    procedure TestStrToTransport;
    procedure TestTransportToStr;
  end;

  TestTIdSipHeader = class(TTestCase)
  private
    H: TIdSipHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestIndexOfParam;
    procedure TestParamCount;
    procedure TestParamsAsString;
    procedure TestValue;
    procedure TestValueParameterClearing;
    procedure TestValueWithNewParams;
  end;

  TestTIdSipAddressHeader = class(TTestCase)
  private
    A: TIdSipAddressHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestDecodeQuotedStr;
    procedure TestEncodeQuotedStr;
    procedure TestValue;
    procedure TestValueEmptyDisplayName;
    procedure TestValueFolded;
    procedure TestValueWithBlankQuotedName;
    procedure TestValueWithEncodings;
    procedure TestValueWithMalformedQuotedName;
    procedure TestValueWithNormalName;
    procedure TestValueWithNoWhitespaceBetweenDisplayNameAndUri;
    procedure TestValueWithParam;
    procedure TestValueWithQuotedName;
    procedure TestValueWithSpace;
    procedure TestValueWithSpecialChars;
    procedure TestValueWithTrailingWhitespacePlusParam;
    procedure TestValueWithUnquotedNonTokensPlusParam;
  end;

  TestTIdSipCSeqHeader = class(TTestCase)
  private
    C: TIdSipCSeqHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetValue;
    procedure TestSetValue;
  end;

  TestTIdSipDateHeader = class(TTestCase)
  private
    D: TIdSipDateHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestValueAbsoluteTime;
    procedure TestValueMalformedAbsoluteTime;
    procedure TestValueRelativeTime;
  end;

  TestTIdSipNumericHeader = class(TTestCase)
  private
    N: TIdSipNumericHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestValue;
    procedure TestValueWithMultipleTokens;
    procedure TestValueWithNegativeNumber;
    procedure TestValueWithString;
  end;

  TestTIdSipMaxForwardsHeader = class(TTestCase)
  private
    M: TIdSipMaxForwardsHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestValueNormal;
    procedure TestValueNormalWithParam;
    procedure TestValueNonNumber;
    procedure TestValueTooBig;
  end;

  TestTIdSipViaHeader = class(TTestCase)
  private
    V: TIdSipViaHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsEqualTo;
    procedure TestValue;
  end;

  TestTIdSipHeaders = class(TTestCase)
  private
    H: TIdSipHeaders;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAddResultTypes;
    procedure TestAsString;
    procedure TestClear;
    procedure TestHasHeader;
    procedure TestHeaders;
    procedure TestItems;
    procedure TestIsCallID;
    procedure TestIsContact;
    procedure TestIsContentLength;
    procedure TestIsCSeq;
    procedure TestIsFrom;
    procedure TestIsMaxForwards;
    procedure TestIsTo;
    procedure TestIsVia;
    procedure TestSetMaxForwards;
    procedure TestValues;
  end;

  TestTIdSipPath = class(TTestCase)
  private
    Headers: TIdSipHeaders;
    Path:    TIdSipPath;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndLastHop;
    procedure TestFirstHop;
  end;

  TestTIdSipRequest = class(TExtendedTestCase)
  private
    Request: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestReadBody;
  end;

  TestTIdSipResponse = class(TExtendedTestCase)
  private
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestReadBody;
  end;


  TestTIdSipParser = class(TTestCase)
  private
    P:        TIdSipParser;
    Request:  TIdSipRequest;
    Response: TIdSipResponse;

    procedure CheckBasicMessage(const Msg: TIdSipMessage);
    procedure CheckBasicRequest(const Msg: TIdSipMessage);
    procedure CheckBasicResponse(const Msg: TIdSipMessage);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanonicaliseName;
    procedure TestCaseInsensitivityOfContentLengthHeader;
    procedure TestGetHeaderName;
    procedure TestGetHeaderNumberValue;
    procedure TestGetHeaderValue;
    procedure TestIsMethod;
    procedure TestIsNumber;
    procedure TestIsSipVersion;
    procedure TestIsToken;
    procedure TestParseAndMakeMessageEmptyString;
    procedure TestParseAndMakeMessageMalformedRequest;
    procedure TestParseAndMakeMessageRequest;
    procedure TestParseAndMakeMessageResponse;
    procedure TestParseExtensiveRequest;
    procedure TestParseReallyLongViaHeader;
    procedure TestParseRequest;
    procedure TestParseRequestEmptyString;
    procedure TestParseRequestFoldedHeader;
    procedure TestParseRequestMalformedMaxForwards;
    procedure TestParseRequestMalformedMethod;
    procedure TestParseRequestMalformedRequestLine;
    procedure TestParseRequestMessageBodyLongerThanContentLength;
    procedure TestParseRequestMultipleVias;
    procedure TestParseRequestRequestUriHasSpaces;
    procedure TestParseRequestRequestUriInAngleBrackets;
    procedure TestParseRequestWithLeadingCrLfs;
    procedure TestParseResponse;
    procedure TestParseResponseEmptyString;
    procedure TestParseResponseFoldedHeader;
    procedure TestParseResponseInvalidStatusCode;
    procedure TestParseResponseWithLeadingCrLfs;
    procedure TestParseShortFormContentLength;
    procedure TestTortureTest1;
    procedure TestTortureTest13;
    procedure TestTortureTest19;
    procedure TestTortureTest21;
    procedure TestTortureTest22;
    procedure TestTortureTest23;
    procedure TestTortureTest35;
    procedure TestTortureTest40;
  end;

const
  BasicRequest = 'INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
               + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10
               + 'Max-Forwards: 70'#13#10
               + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
               + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
               + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
               + 'CSeq: 314159 INVITE'#13#10
               + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
               + 'Content-Length: 29'#13#10
               + #13#10
               + 'I am a message. Hear me roar!';
  BasicResponse = 'SIP/2.0 486 Busy Here'#13#10
                + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10
                + 'Max-Forwards: 70'#13#10
                + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
                + 'CSeq: 314159 INVITE'#13#10
                + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                + 'Content-Length: 29'#13#10
                + #13#10
                + 'I am a message. Hear me roar!';
  ExhaustiveRequest = 'INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                    + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
                    + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                    + 'Content-Length: 29'#13#10
                    + 'Content-Type: text/plain'#13#10
                    + 'CSeq: 314159 INVITE'#13#10
                    + 'Date: Thu, 1 Jan 1970 00:00:00 GMT'#13#10
                    + 'Expires: 1000'#13#10
                    + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                    + 'Max-Forwards: 70'#13#10
                    + 'Subject: I am a SIP request with every legal header (even an extension)'#13#10
                    + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                    + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10
                    + 'X-Not-A-Header: I am not defined in RFC 3261'#13#10
                    + #13#10
                    + 'I am a message. Hear me roar!';

implementation

uses
  DateUtils, IdSimpleParser, SysUtils, TortureTests;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('SipParser tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSipHeader.Suite);
  Result.AddTest(TestTIdSipAddressHeader.Suite);
  Result.AddTest(TestTIdSipCSeqHeader.Suite);
  Result.AddTest(TestTIdSipDateHeader.Suite);
  Result.AddTest(TestTIdSipMaxForwardsHeader.Suite);
  Result.AddTest(TestTIdSipNumericHeader.Suite);
  Result.AddTest(TestTIdSipViaHeader.Suite);
  Result.AddTest(TestTIdSipHeaders.Suite);
  Result.AddTest(TestTIdSipPath.Suite);
  Result.AddTest(TestTIdSipRequest.Suite);
  Result.AddTest(TestTIdSipResponse.Suite);
  Result.AddTest(TestTIdSipParser.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestIsEqual;
begin
  Check(    IsEqual('', ''),         ''''' & ''''');
  Check(not IsEqual(' ', ''),        ''' '' & ''''');
  Check(    IsEqual('abcd', 'AbCd'), '''abcd'', ''AbCd''');
  Check(not IsEqual('absd', 'Abcd'), '''absd'', ''Abcd''');
end;

procedure TestFunctions.TestShortMonthToInt;
begin
  CheckEquals(1, ShortMonthToInt('JAN'), 'JAN');
  CheckEquals(1, ShortMonthToInt('jan'), 'jan');

  CheckEquals(1,  ShortMonthToInt('Jan'), 'Jan');
  CheckEquals(2,  ShortMonthToInt('Feb'), 'Feb');
  CheckEquals(3,  ShortMonthToInt('Mar'), 'Mar');
  CheckEquals(4,  ShortMonthToInt('Apr'), 'Apr');
  CheckEquals(5,  ShortMonthToInt('May'), 'May');
  CheckEquals(6,  ShortMonthToInt('Jun'), 'Jun');
  CheckEquals(7,  ShortMonthToInt('Jul'), 'Jul');
  CheckEquals(8,  ShortMonthToInt('Aug'), 'Aug');
  CheckEquals(9,  ShortMonthToInt('Sep'), 'Sep');
  CheckEquals(10, ShortMonthToInt('Oct'), 'Oct');
  CheckEquals(11, ShortMonthToInt('Nov'), 'Nov');
  CheckEquals(12, ShortMonthToInt('Dec'), 'Dec');

  try
    ShortMonthToInt('xxx');
    Fail('Failed to bail out on malformed short month name');
  except
    on E: EConvertError do
      CheckEquals('Failed to convert ''xxx'' to type Integer', E.Message, 'Unexpected error');
  end;
end;

procedure TestFunctions.TestStrToTransport;
begin
  Check(sttSCTP = StrToTransport('SCTP'), 'SCTP');
  Check(sttTCP  = StrToTransport('TCP'),  'TCP');
  Check(sttTLS  = StrToTransport('TLS'),  'TLS');
  Check(sttUDP  = StrToTransport('UDP'),  'UDP');

  try
    StrToTransport('not a transport');
    Fail('Failed to bail out on an unknown transport type');
  except
    on EConvertError do;
  end;
end;

procedure TestFunctions.TestTransportToStr;
var
  T: TIdSipTransportType;
begin
  for T := Low(TIdSipTransportType) to High(TIdSipTransportType) do
    Check(T = StrToTransport(TransportToStr(T)), 'Ord(T) = ' + IntToStr(Ord(T)));
end;

//******************************************************************************
//* TestTIdSipHeader                                                           *
//******************************************************************************
//* TestTIdSipHeader Public methods ********************************************

procedure TestTIdSipHeader.SetUp;
begin
  inherited SetUp;

  Self.H := TIdSipHeader.Create;
end;

procedure TestTIdSipHeader.TearDown;
begin
  Self.H.Free;

  inherited TearDown;
end;

//* TestTIdSipHeader Published methods *****************************************

procedure TestTIdSipHeader.TestAsString;
begin
  CheckEquals(': ', Self.H.AsString, 'AsString with no set properties');

  Self.H.Name := 'Foo';
  Self.H.Value := 'Fighters';
  CheckEquals('Foo: Fighters', Self.H.AsString, 'Foo: Fighters');

  Self.H.Params['tag'] := 'haha';
  CheckEquals('Foo: Fighters;tag=haha',
              Self.H.AsString,
              '''Foo: Fighters'' with tag');

  Self.H.Params['hidden'] := '';
  CheckEquals('Foo: Fighters;tag=haha;hidden',
              Self.H.AsString,
              '''Foo: Fighters'' with tag & hidden');
end;

procedure TestTIdSipHeader.TestIndexOfParam;
begin
  CheckEquals(-1, Self.H.IndexOfParam('branch'), 'Index of non-existent param');

  Self.H.Params['branch'] := 'z9hG4bK776asdhds';
  CheckEquals(0, Self.H.IndexOfParam('branch'), 'Index of 1st param');

  Self.H.Params['ttl']    := '5';
  CheckEquals(0, Self.H.IndexOfParam('branch'), 'Index of 1st param; paranoia check');
  CheckEquals(1, Self.H.IndexOfParam('ttl'),    'Index of 2nd param');
end;

procedure TestTIdSipHeader.TestParamCount;
begin
  CheckEquals(0, Self.H.ParamCount, 'ParamCount of an empty list');

  Self.H.Params['branch'] := 'z9hG4bK776asdhds';
  CheckEquals(1, Self.H.ParamCount, 'ParamCount, 1 param');

  Self.H.Params['ttl'] := '5';
  CheckEquals(2, Self.H.ParamCount, 'ParamCount, 2 params');
end;

procedure TestTIdSipHeader.TestParamsAsString;
begin
  Self.H.Params['branch'] := 'z9hG4bK776asdhds';
  Self.H.Params['ttl']    := '5';

  CheckEquals(';branch=z9hG4bK776asdhds;ttl=5',
              Self.H.ParamsAsString,
              'ParamsAsString');
end;

procedure TestTIdSipHeader.TestValue;
begin
  CheckEquals('', Self.H.Value, 'Value-less header');

  Self.H.Name := 'Foo';
  CheckEquals('', Self.H.Value, 'Value-less header after name''s set');

  Self.H.Value := 'Fighters';
  CheckEquals('Fighters', Self.H.Value, 'Value-ful header');

  Self.H.Params['branch'] := 'haha';
  CheckEquals('Fighters', Self.H.Value, 'Value-ful header with a param');

  Self.H.Params['ttl'] := 'eheh';
  CheckEquals('Fighters', Self.H.Value, 'Value-ful header with multiple params');

  Self.H.Value := 'Fluffy';
  CheckEquals(0, Self.H.ParamCount, 'Didn''t clear out old params');
end;

procedure TestTIdSipHeader.TestValueParameterClearing;
begin
  Self.H.Value := 'Fighters;branch=haha';
  Self.H.Value := 'Fighters';
  CheckEquals('', Self.H.ParamsAsString, 'Parameters not cleared');
end;

procedure TestTIdSipHeader.TestValueWithNewParams;
begin
  Self.H.Value := 'Fighters;branch=haha';
  Self.H.Value := 'Fighters;tickle=feather';
  CheckEquals(';tickle=feather', Self.H.ParamsAsString, 'Parameters not cleared');
end;

//******************************************************************************
//* TestTIdSipAddressHeader                                                    *
//******************************************************************************
//* TestTIdSipAddressHeader Public methods *************************************

procedure TestTIdSipAddressHeader.SetUp;
begin
  inherited SetUp;

  Self.A := TIdSipAddressHeader.Create;
end;

procedure TestTIdSipAddressHeader.TearDown;
begin
  Self.A.Free;

  inherited TearDown;
end;

//* TestTIdSipAddressHeader Published methods **********************************

procedure TestTIdSipAddressHeader.TestAsString;
begin
  Self.A.Name := ToHeaderFull;

  Self.A.Value := 'sips:countzero@jacks-bar.com';
  CheckEquals(ToHeaderFull + ': sips:countzero@jacks-bar.com',
              Self.A.AsString,
              'AsString, plain URI');

  Self.A.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.lu>';
  CheckEquals(ToHeaderFull + ': Wintermute <sip:wintermute@tessier-ashpool.co.lu>',
              Self.A.AsString,
              'AsString, display-name');

  Self.A.Value := '"Count Zero\"" <sips:countzero@jacks-bar.com>';
  CheckEquals(ToHeaderFull + ': "Count Zero\"" <sips:countzero@jacks-bar.com>',
              Self.A.AsString,
              'AsString, display-name with quoted-pair');

  Self.A.Value := '"Count Zero\"" <sips:countzero@jacks-bar.com>;paranoid';
  CheckEquals(ToHeaderFull + ': "Count Zero\"" <sips:countzero@jacks-bar.com>;paranoid',
              Self.A.AsString,
              'AsString, display-name with quoted-pair + parameters');

  Self.A.Value := 'Count Zero <sips:countzero@jacks-bar.com;paranoid>;very';
  CheckEquals(ToHeaderFull + ': Count Zero <sips:countzero@jacks-bar.com;paranoid>;very',
              Self.A.AsString,
              'AsString, display-name URI and header have parameters');

  Self.A.Value         := '';
  Self.A.DisplayName   := 'Bell, Alexander';
  Self.A.Address.URI   := 'sip:a.g.bell@bell-tel.com';
  Self.A.Params['tag'] := '43';
  CheckEquals(ToHeaderFull + ': "Bell, Alexander" <sip:a.g.bell@bell-tel.com>;tag=43',
              Self.A.AsString,
              'AsString, display-name with comma');
end;

procedure TestTIdSipAddressHeader.TestDecodeQuotedStr;
begin
  CheckEquals('',       Self.A.DecodeQuotedStr(''),         '''''');
  CheckEquals('abcd',   Self.A.DecodeQuotedStr('abcd'),     'abcd');
  CheckEquals('"',      Self.A.DecodeQuotedStr('\"'),       '\"');
  CheckEquals('\',      Self.A.DecodeQuotedStr('\\'),       '\\');
  CheckEquals(' ',      Self.A.DecodeQuotedStr('\ '),       '''\ ''');
  CheckEquals('abcd',   Self.A.DecodeQuotedStr('\a\b\c\d'), '\a\b\c\d');
  CheckEquals(#0,       Self.A.DecodeQuotedStr('\'#0),      '\#0');
  CheckEquals('hello\', Self.A.DecodeQuotedStr('hello\\'),  'hello\\');

  try
    Self.A.DecodeQuotedStr('\');
    Fail('Failed to bail out on a malformed string, ''\''');
  except
    on E: EBadHeader do;
  end;

  try
    Self.A.DecodeQuotedStr('hello\');
    Fail('Failed to bail out on a malformed string, ''hello\''');
  except
    on E: EBadHeader do;
  end;
end;

procedure TestTIdSipAddressHeader.TestEncodeQuotedStr;
begin
  CheckEquals('I am a ''normal'' string',
              Self.A.EncodeQuotedStr('I am a ''normal'' string'),
              '''I am a ''''normal'''' string''');
  CheckEquals('',
              Self.A.EncodeQuotedStr(''),
              '''''');
  CheckEquals('\\',
              Self.A.EncodeQuotedStr('\'),
              '\');
  CheckEquals('\"',
              Self.A.EncodeQuotedStr('"'),
              '"');
  CheckEquals('\\\"',
              Self.A.EncodeQuotedStr('\"'),
              '\"');
  CheckEquals('\"I am a ''normal'' string\"',
              Self.A.EncodeQuotedStr('"I am a ''normal'' string"'),
              '''"I am a ''normal'' string"''');
end;

procedure TestTIdSipAddressHeader.TestValue;
begin
  A.Value := 'sip:wintermute@tessier-ashpool.co.lu';

  CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Self.A.Address.GetFullURI, 'Address');
  CheckEquals('',                                     Self.A.DisplayName,        'DisplayName');
  CheckEquals('',                                     Self.A.ParamsAsString,     'Params');
  CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Self.A.Value,              'Value');
end;

procedure TestTIdSipAddressHeader.TestValueEmptyDisplayName;
begin
  A.Value := '<sip:wintermute@tessier-ashpool.co.lu>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Self.A.Address.GetFullURI, 'Address');
  CheckEquals('',                                     Self.A.DisplayName,        'DisplayName');
  CheckEquals('',                                     Self.A.ParamsAsString,     'Params');
  CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Self.A.Value,              'Value');
end;

procedure TestTIdSipAddressHeader.TestValueFolded;
begin
  Self.A.Value := 'Wintermute'#13#10' <sip:wintermute@tessier-ashpool.co.lu>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.lu',              Self.A.Address.GetFullURI, 'Address');
  CheckEquals('Wintermute',                                        Self.A.DisplayName,        'DisplayName');
  CheckEquals('',                                                  Self.A.ParamsAsString,     'Params');
  CheckEquals('Wintermute <sip:wintermute@tessier-ashpool.co.lu>', Self.A.Value,              'Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithBlankQuotedName;
begin
  Self.A.Value := '"" <sip:wintermute@tessier-ashpool.co.lu>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.lu',  Self.A.Address.GetFullURI, 'Address');
  CheckEquals('',                                      Self.A.DisplayName,        'DisplayName');
  CheckEquals('',                                      Self.A.ParamsAsString,     'Params');
  CheckEquals('sip:wintermute@tessier-ashpool.co.lu',  Self.A.Value,              'Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithEncodings;
begin
  Self.A.Value := '"Count Zero\"" <sips:countzero@jacks-bar.com>';
  CheckEquals('sips:countzero@jacks-bar.com',                  Self.A.Address.GetFullURI, '1: Address');
  CheckEquals('Count Zero"',                                   Self.A.DisplayName,        '1: DisplayName');
  CheckEquals('',                                              Self.A.ParamsAsString,     '1: Params');
  CheckEquals('"Count Zero\"" <sips:countzero@jacks-bar.com>', Self.A.Value,              '1: Value');

  Self.A.Value := '"Count\\\" Zero\"\"" <sips:countzero@jacks-bar.com>';
  CheckEquals('sips:countzero@jacks-bar.com', Self.A.Address.GetFullURI, '2: Address');
  CheckEquals('Count\" Zero""',               Self.A.DisplayName,        '2: DisplayName');
  CheckEquals('',                             Self.A.ParamsAsString,     '2: Params');
  CheckEquals('"Count\\\" Zero\"\"" <sips:countzero@jacks-bar.com>',
              Self.A.Value,
              '2: Value');

  Self.A.Value := '"\C\o\u\n\t\\\"\ \Z\e\r\o\"\"" <sips:countzero@jacks-bar.com>';
  CheckEquals('sips:countzero@jacks-bar.com', Self.A.Address.GetFullURI,  '3: Address');
  CheckEquals('Count\" Zero""',               Self.A.DisplayName,         '3: Name');
  CheckEquals('',                             Self.A.ParamsAsString,      '3: Params');
  CheckEquals('"Count\\\" Zero\"\"" <sips:countzero@jacks-bar.com>',
              Self.A.Value,
              '3: Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithMalformedQuotedName;
begin
  try
    // missing close quote
    Self.A.Value := '"Count Zero <sips:countzero@jacks-bar.com>';
    Fail('Failed to bail out because of unmatched quotes');
  except
    on E: EBadHeader do;
  end;

  try
    // missing close quote
    Self.A.Value := '"Count Zero \" <sips:countzero@jacks-bar.com>';
    Fail('Failed to bail out because of unmatched quotes');
  except
    on E: EBadHeader do;
  end;
end;

procedure TestTIdSipAddressHeader.TestValueWithNormalName;
begin
  Self.A.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.lu>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.lu',              Self.A.Address.GetFullURI, 'Address');
  CheckEquals('Wintermute',                                        Self.A.DisplayName,        'DisplayName');
  CheckEquals('',                                                  Self.A.ParamsAsString,     'Params');
  CheckEquals('Wintermute <sip:wintermute@tessier-ashpool.co.lu>', Self.A.Value,              'Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithNoWhitespaceBetweenDisplayNameAndUri;
begin
  Self.A.Value := '"caller"<sip:caller@example.com>';
  CheckEquals('sip:caller@example.com', Self.A.Address.GetFullURI, 'Address');
  CheckEquals('caller',                 Self.A.DisplayName,        'Name');
  CheckEquals('',                       Self.A.ParamsAsString,     'Params');
end;

procedure TestTIdSipAddressHeader.TestValueWithParam;
begin
  Self.A.Value := 'sip:wintermute@tessier-ashpool.co.lu;hidden';

  CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Self.A.Address.GetFullURI, 'Address');
  CheckEquals('',                                     Self.A.DisplayName,        'Name');
  CheckEquals(';hidden',                              Self.A.ParamsAsString,     'Params');
end;

procedure TestTIdSipAddressHeader.TestValueWithQuotedName;
begin
  Self.A.Value := '"Wintermute" <sip:wintermute@tessier-ashpool.co.lu>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.lu',              Self.A.Address.GetFullURI, '1: Address');
  CheckEquals('Wintermute',                                        Self.A.DisplayName,        '1: Name');
  CheckEquals('',                                                  Self.A.ParamsAsString,     '1: Params');
  CheckEquals('Wintermute <sip:wintermute@tessier-ashpool.co.lu>', Self.A.Value,              '1: Value');

  Self.A.Value := '"Count Zero" <sips:countzero@jacks-bar.com>';

  CheckEquals('sips:countzero@jacks-bar.com',                Self.A.Address.GetFullURI, '2: Address');
  CheckEquals('Count Zero',                                  Self.A.DisplayName,        '2: Name');
  CheckEquals('',                                            Self.A.ParamsAsString,     '2: Params');
  CheckEquals('Count Zero <sips:countzero@jacks-bar.com>',   Self.A.Value,              '2: Value');

end;

procedure TestTIdSipAddressHeader.TestValueWithSpace;
begin
  Self.A.Value := 'Count Zero <sips:countzero@jacks-bar.com>';
  CheckEquals('sips:countzero@jacks-bar.com',              Self.A.Address.GetFullURI, 'Address');
  CheckEquals('Count Zero',                                Self.A.DisplayName,        'Name');
  CheckEquals('',                                          Self.A.ParamsAsString,     'Params');
  CheckEquals('Count Zero <sips:countzero@jacks-bar.com>', Self.A.Value,              'Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithSpecialChars;
begin
  Self.A.Address.URI := 'sip:wintermute@tessier-ashpool.co.lu;tag=f00';
  CheckEquals('sip:wintermute@tessier-ashpool.co.lu;tag=f00',   Self.A.Address.GetFullURI, ';:Address');
  CheckEquals('',                                               Self.A.DisplayName,        ';: Name');
  CheckEquals('',                                               Self.A.ParamsAsString,     ';: Params');
  CheckEquals('<sip:wintermute@tessier-ashpool.co.lu;tag=f00>', Self.A.Value,              ';: Value');

  Self.A.Address.URI := 'sip:wintermute@tessier-ashpool.co.lu,tag=f00';
  CheckEquals('sip:wintermute@tessier-ashpool.co.lu,tag=f00',   Self.A.Address.GetFullURI, ',:Address');
  CheckEquals('',                                               Self.A.DisplayName,        ',: Name');
  CheckEquals('',                                               Self.A.ParamsAsString,     ',: Params');
  CheckEquals('<sip:wintermute@tessier-ashpool.co.lu,tag=f00>', Self.A.Value,              ',: Value');

  Self.A.Address.URI := 'sip:wintermute@tessier-ashpool.co.lu?tag=f00';
  CheckEquals('sip:wintermute@tessier-ashpool.co.lu?tag=f00',   Self.A.Address.GetFullURI, '?:Address');
  CheckEquals('',                                               Self.A.DisplayName,        '?: Name');
  CheckEquals('',                                               Self.A.ParamsAsString,     '?: Params');
  CheckEquals('<sip:wintermute@tessier-ashpool.co.lu?tag=f00>', Self.A.Value,              '?: Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithTrailingWhitespacePlusParam;
begin
  Self.A.Value := 'sip:vivekg@chair.dnrc.bell-labs.com ; haha=heehee';
  CheckEquals('sip:vivekg@chair.dnrc.bell-labs.com', Self.A.Address.GetFullURI, 'Address');
  CheckEquals('',                                    Self.A.DisplayName,        'DisplayName');
  CheckEquals(';haha=heehee',                        Self.A.ParamsAsString,     'Params');
  CheckEquals('sip:vivekg@chair.dnrc.bell-labs.com', Self.A.Value,              'Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithUnquotedNonTokensPlusParam;
begin
  try
    Self.A.Value := 'Bell, Alexander <sip:a.g.bell@bell-tel.com>;tag=43';
    Fail('Failed to bail out with unquoted non-tokens');
  except
    on EBadHeader do;
  end;
end;

//******************************************************************************
//* TestTIdSipCSeqHeader                                                       *
//******************************************************************************
//* TestTIdSipCSeqHeader Public methods ****************************************

procedure TestTIdSipCSeqHeader.SetUp;
begin
  inherited SetUp;

  Self.C := TIdSipCSeqHeader.Create;
end;

procedure TestTIdSipCSeqHeader.TearDown;
begin
  Self.C.Free;

  inherited TearDown;
end;

//* TestTIdSipCSeqHeader Published methods *************************************

procedure TestTIdSipCSeqHeader.TestGetValue;
begin
  Self.C.Value := '1 INVITE';
  CheckEquals(1,        Self.C.SequenceNo, '1: SequenceNo');
  CheckEquals('INVITE', Self.C.Method,     '1: Method');

  Self.C.Value := '1  INVITE';
  CheckEquals(1,        Self.C.SequenceNo, '2: SequenceNo');
  CheckEquals('INVITE', Self.C.Method,     '2: Method');

  Self.C.Value := '1'#13#10'  INVITE';
  CheckEquals(1,        Self.C.SequenceNo, '3: SequenceNo');
  CheckEquals('INVITE', Self.C.Method,     '3: Method');
end;

procedure TestTIdSipCSeqHeader.TestSetValue;
begin
  Self.C.Value := '1 INVITE';
  CheckEquals(1,        Self.C.SequenceNo, 'SequenceNo');
  CheckEquals('INVITE', Self.C.Method,     'Method');

  try
    Self.C.Value := 'a';
    Fail('Failed to bail out with a non-numeric sequence number, ''a''');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := 'cafebabe INVITE';
    Fail('Failed to bail out with a non-numeric sequence number, ''cafebabe INVITE''');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := '42 ';
    Fail('Failed to bail out with a non-method, ''42 ''');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := '42 "INVITE"';
    Fail('Failed to bail out with a non-method, ''42 "INVITE"');
  except
    on EBadHeader do;
  end;
end;

//******************************************************************************
//* TestTIdSipDateHeader                                                       *
//******************************************************************************
//* TestTIdSipDateHeader Public methods ****************************************

procedure TestTIdSipDateHeader.SetUp;
begin
  inherited SetUp;

  Self.D := TIdSipDateHeader.Create;
end;

procedure TestTIdSipDateHeader.TearDown;
begin
   Self.D.Free;

  inherited TearDown;
end;

//* TestTIdSipDateHeader Published methods *************************************

procedure TestTIdSipDateHeader.TestValueAbsoluteTime;
begin
  Self.D.Value := 'Fri, 18 Jul 2003 16:00:00 GMT';

  CheckEquals('2003/07/18 16:00:00',
              FormatDateTime('yyyy/mm/dd hh:mm:ss', Self.D.Time.AsTDateTime),
              'AbsoluteTime');
end;

procedure TestTIdSipDateHeader.TestValueMalformedAbsoluteTime;
begin
  try
    Self.D.Value := 'Thu, 44 Dec 19999 16:00:00 EDT';
    Fail('Failed to bail out');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipDateHeader.TestValueRelativeTime;
begin
  try
    Self.D.Value := '1';
    Fail('Failed to bail out');
  except
    on EBadHeader do;
  end;
end;

//******************************************************************************
//* TestTIdSipNumericHeader                                                    *
//******************************************************************************
//* TestTIdSipNumericHeader Public methods *************************************

procedure TestTIdSipNumericHeader.SetUp;
begin
  inherited SetUp;

  Self.N := TIdSipNumericHeader.Create;
end;

procedure TestTIdSipNumericHeader.TearDown;
begin
  Self.N.Free;

  inherited TearDown;
end;

//* TestTIdSipNumericHeader Published methods **********************************

procedure TestTIdSipNumericHeader.TestValue;
begin
  Self.N.Value := '0';
  CheckEquals(0,   Self.N.NumericValue, 'NumericValue (0)');

  Self.N.Value := '666';
  CheckEquals(666, Self.N.NumericValue, 'NumericValue (666)');
end;


procedure TestTIdSipNumericHeader.TestValueWithMultipleTokens;
begin
  try
    Self.N.Value := '1 1';
    Fail('Failed to bail out with multiple tokens');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipNumericHeader.TestValueWithNegativeNumber;
begin
  try
    Self.N.Value := '-1';
    Fail('Failed to bail out with negative integer');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipNumericHeader.TestValueWithString;
begin
  try
    Self.N.Value := 'one';
    Fail('Failed to bail out with string value');
  except
    on EBadHeader do;
  end;
end;

//******************************************************************************
//* TestTIdSipMaxForwardsHeader                                                *
//******************************************************************************
//* TestTIdSipMaxForwardsHeader Public methods *********************************

procedure TestTIdSipMaxForwardsHeader.SetUp;
begin
  inherited SetUp;

  Self.M := TIdSipMaxForwardsHeader.Create;
end;

procedure TestTIdSipMaxForwardsHeader.TearDown;
begin
  Self.M.Free;

  inherited TearDown;
end;

//* TestTIdSipMaxForwardsHeader Published methods ******************************

procedure TestTIdSipMaxForwardsHeader.TestValueNormal;
begin
  Self.M.Value := '42';
  CheckEquals(42, Self.M.NumericValue, 'NumericValue, 42');

  Self.M.Value := '0';
  CheckEquals(0, Self.M.NumericValue, 'NumericValue, 0');

  Self.M.Value := '255';
  CheckEquals(255, Self.M.NumericValue, 'NumericValue, 255');
end;

procedure TestTIdSipMaxForwardsHeader.TestValueNormalWithParam;
begin
  try
    Self.M.Value := '13;tag=f00';
    Fail('Failed to bail out on non-numeric value for Max-Forwards (no params allowed)');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipMaxForwardsHeader.TestValueNonNumber;
begin
  try
    Self.M.Value := 'alpha';
    Fail('Failed to bail out on non-numeric value for Max-Forwards');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipMaxForwardsHeader.TestValueTooBig;
begin
  try
    Self.M.Value := '256';
    Fail('Failed to bail out on numeric value > 255 for Max-Forwards');
  except
    on EBadHeader do;
  end;
end;

//******************************************************************************
//* TestTIdSipViaHeader                                                        *
//******************************************************************************
//* TestTIdSipViaHeader Public methods *****************************************

procedure TestTIdSipViaHeader.SetUp;
begin
  inherited SetUp;

  Self.V := TIdSipViaHeader.Create;
end;

procedure TestTIdSipViaHeader.TearDown;
begin
  Self.V.Free;

  inherited TearDown;
end;

//* TestTIdSipViaHeader Published methods **************************************

procedure TestTIdSipViaHeader.TestIsEqualTo;
var
  Hop2: TIdSipViaHeader;
begin
  Hop2 := TIdSipViaHeader.Create;
  try
    Self.V.Host       := '127.0.0.1';
    Self.V.Port       := 5060;
    Self.V.SipVersion := 'SIP/2.0';
    Self.V.Transport  := sttSCTP;

    Hop2.Host       := '127.0.0.1';
    Hop2.Port       := 5060;
    Hop2.SipVersion := 'SIP/2.0';
    Hop2.Transport  := sttSCTP;

    Check(V.IsEqualTo(Hop2), 'V.IsEqualTo(Hop2)');
    Check(Hop2.IsEqualTo(V), 'Hop2.IsEqualTo(V)');

    Self.V.Host := '127.0.0.2';
    Check(not Self.V.IsEqualTo(Hop2), 'V.IsEqualTo(Hop2); Host');
    Check(not Hop2.IsEqualTo(Self.V), 'Hop2.IsEqualTo(V); Host');
    Self.V.Host := '127.0.0.1';
    Check(Self.V.IsEqualTo(Hop2), 'V.IsEqualTo(Hop2); Host reset');
    Check(Hop2.IsEqualTo(Self.V), 'Hop2.IsEqualTo(V); Host reset');

    Self.V.Port := 111;
    Check(not Self.V.IsEqualTo(Hop2), 'V.IsEqualTo(Hop2); Port');
    Check(not Hop2.IsEqualTo(Self.V), 'Hop2.IsEqualTo(V); Port');
    Self.V.Port := 5060;
    Check(Self.V.IsEqualTo(Hop2), 'V.IsEqualTo(Hop2); Port reset');
    Check(Hop2.IsEqualTo(Self.V), 'Hop2.IsEqualTo(V); Port reset');

    Self.V.SipVersion := 'xxx';
    Check(not Self.V.IsEqualTo(Hop2), 'V.IsEqualTo(Hop2); SipVersion');
    Check(not Hop2.IsEqualTo(Self.V), 'Hop2.IsEqualTo(V); SipVersion');
    Self.V.SipVersion := 'SIP/2.0';
    Check(Self.V.IsEqualTo(Hop2), 'V.IsEqualTo(Hop2); SipVersion reset');
    Check(Hop2.IsEqualTo(Self.V), 'Hop2.IsEqualTo(V); SipVersion reset');

    Self.V.Transport := sttTCP;
    Check(not Self.V.IsEqualTo(Hop2), 'V.IsEqualTo(Hop2); Transport');
    Check(not Hop2.IsEqualTo(Self.V), 'Hop2.IsEqualTo(V); Transport');
  finally
    Hop2.Free;
  end;
end;

procedure TestTIdSipViaHeader.TestValue;
begin
  V.Value := 'SIP/1.5/UDP 127.0.0.1;tag=heehee';
  CheckEquals('127.0.0.1',   V.Host,           '1: Host');
  CheckEquals(';tag=heehee', V.ParamsAsString, '1: Parameters');
  CheckEquals(IdPORT_SIP,    V.Port,           '1: Port');
  CheckEquals('SIP/1.5',     V.SipVersion,     '1: SipVersion');
  Check      (sttUDP =       V.Transport,      '1: Transport');

  V.Value := 'SIP/1.5/TLS 127.0.0.1';
  CheckEquals('127.0.0.1',     V.Host,           '2: Host');
  CheckEquals('',              V.ParamsAsString, '2: Parameters');
  CheckEquals(IdPORT_SIP_TLS,  V.Port,           '2: Port');
  CheckEquals('SIP/1.5',       V.SipVersion,     '2: SipVersion');
  Check      (sttTLS =         V.Transport,      '2: Transport');

  V.Value := 'SIP/1.5/UDP 127.0.0.1:666;tag=heehee';
  CheckEquals('127.0.0.1',   V.Host,           '3: Host');
  CheckEquals(';tag=heehee', V.ParamsAsString, '3: Parameters');
  CheckEquals(666,           V.Port,           '3: Port');
  CheckEquals('SIP/1.5',     V.SipVersion,     '3: SipVersion');
  Check      (sttUDP =       V.Transport,      '3: Transport');

  V.Value := 'SIP/1.5/TLS 127.0.0.1:666;haha=heehee';
  CheckEquals('127.0.0.1',     V.Host,           '4: Host');
  CheckEquals(';haha=heehee',  V.ParamsAsString, '4: Parameters');
  CheckEquals(666,             V.Port,           '4: Port');
  CheckEquals('SIP/1.5',       V.SipVersion,     '4: SipVersion');
  Check      (sttTLS =         V.Transport,      '4: Transport');

  V.Value := 'SIP/1.5/TLS 127.0.0.1:666 '#13#10' ; haha=heehee';
  CheckEquals('127.0.0.1',     V.Host,           '5: Host');
  CheckEquals(';haha=heehee',  V.ParamsAsString, '5: Parameters');
  CheckEquals(666,             V.Port,           '5: Port');
  CheckEquals('SIP/1.5',       V.SipVersion,     '5: SipVersion');
  Check      (sttTLS =         V.Transport,      '5: Transport');
end;

//******************************************************************************
//* TestTIdSipHeaders                                                          *
//******************************************************************************
//* TestTIdSipHeaders Public methods *******************************************

procedure TestTIdSipHeaders.SetUp;
begin
  inherited SetUp;

  Self.H := TIdSipHeaders.Create;
end;

procedure TestTIdSipHeaders.TearDown;
begin
  Self.H.Free;

  inherited TearDown;
end;

//* TestTIdSipHeaders Published methods ****************************************

procedure TestTIdSipHeaders.TestAddAndCount;
begin
  CheckEquals(0, Self.H.Count, 'Supposedly an empty set of headers');
  CheckEquals(TIdSipHeader.ClassName,
              Self.H.Add(CallIdHeaderFull).ClassName,
              'Incorrect return type');
  CheckEquals(1, Self.H.Count, 'Failed to add new header');
end;

procedure TestTIdSipHeaders.TestAddResultTypes;
begin
  CheckEquals(TIdSipAddressHeader.ClassName,     Self.H.Add(ContactHeaderFull).ClassName,       ContactHeaderFull);
  CheckEquals(TIdSipNumericHeader.ClassName,     Self.H.Add(ContentLengthHeaderFull).ClassName, ContentLengthHeaderFull);
  CheckEquals(TIdSipCSeqHeader.ClassName,        Self.H.Add(CSeqHeader).ClassName,              CSeqHeader);
  CheckEquals(TIdSipDateHeader.ClassName,        Self.H.Add(DateHeader).ClassName,              DateHeader);
  CheckEquals(TIdSipNumericHeader.ClassName,     Self.H.Add(ExpiresHeader).ClassName,           ExpiresHeader);
  CheckEquals(TIdSipAddressHeader.ClassName,     Self.H.Add(FromHeaderFull).ClassName,          FromHeaderFull);
  CheckEquals(TIdSipMaxForwardsHeader.ClassName, Self.H.Add(MaxForwardsHeader).ClassName,       MaxForwardsHeader);
  CheckEquals(TIdSipAddressHeader.ClassName,     Self.H.Add(ToHeaderFull).ClassName,            ToHeaderFull);
  CheckEquals(TIdSipViaHeader.ClassName,         Self.H.Add(ViaHeaderFull).ClassName,           ViaHeaderFull);
end;

procedure TestTIdSipHeaders.TestAsString;
begin
  CheckEquals('',
              Self.H.AsString,
              'AsString with zero headers');

  Self.H.Add('Content-Length').Value := '28';
  H['Content-Length'].Params['bogus'] := 'true';

  CheckEquals('Content-Length: 28;bogus=true'#13#10,
              Self.H.AsString,
              'AsString with one header');

  Self.H.Add('Content-Type').Value := 'text/xml';
  H['Content-Type'].Params['kallisti'] := 'eris';

  CheckEquals('Content-Length: 28;bogus=true'#13#10
            + 'Content-Type: text/xml;kallisti=eris'#13#10,
              Self.H.AsString,
              'AsString with two headers');
end;

procedure TestTIdSipHeaders.TestClear;
begin
  Self.H.Clear;
  CheckEquals(0, Self.H.Count, 'Count after Clearing an empty list');

  Self.H.Add('Content-Length');
  Self.H.Add('Via');
  Self.H.Clear;
  CheckEquals(0, Self.H.Count, 'Count after Clearing a non-empty list');
end;

procedure TestTIdSipHeaders.TestHasHeader;
begin
  Check(not Self.H.HasHeader(''), '''''');
  Check(not Self.H.HasHeader('Content-Length'), 'Content-Length');

  Self.H.Add('Content-Length');
  Check(Self.H.HasHeader('Content-Length'), 'Content-Length not added');
end;

procedure TestTIdSipHeaders.TestHeaders;
var
  Header: TIdSipHeader;
begin
  CheckEquals(CallIdHeaderFull,
              Self.H.Headers[CallIdHeaderFull].Name,
              'Returned newly created header: Name');
  CheckEquals('',
              Self.H.Headers[CallIdHeaderFull].Value,
              'Returned newly created header: Value');
  CheckEquals(1, Self.H.Count, 'Newly created header wasn''t added though');

  Header := Self.H.Add('Via');
  Check(Header = Self.H.Headers['Via'],
        'Incorrect header returned');
end;

procedure TestTIdSipHeaders.TestItems;
begin
  try
    Self.H.Items[0];
    Fail('Failed to bail out accessing the 1st header in an empty collection');
  except
    on EListError do;
  end;

  Self.H.Add('Content-Length');
  CheckEquals('Content-Length', Self.H.Items[0].Name, 'Name of 1st header');
end;

procedure TestTIdSipHeaders.TestIsCallID;
begin
  Check(    TIdSipHeaders.IsCallID('Call-ID'),         'Call-ID');
  Check(    TIdSipHeaders.IsCallID('i'),               'i');
  Check(    TIdSipHeaders.IsCallID(CallIDHeaderFull),  'CallIDHeaderFull constant');
  Check(    TIdSipHeaders.IsCallID(CallIDHeaderShort), 'CallIDHeaderShort constant');
  Check(not TIdSipHeaders.IsCallID(''),                '''''');
  Check(not TIdSipHeaders.IsCallID(#0),                '#0');
  Check(not TIdSipHeaders.IsCallID(#$FF),              '#$FF');
  Check(not TIdSipHeaders.IsCallID('Content-Length'),  'Content-Length');
end;

procedure TestTIdSipHeaders.TestIsContact;
begin
  Check(    TIdSipHeaders.IsContact('Contact'),          'Contact');
  Check(    TIdSipHeaders.IsContact(ContactHeaderFull),  'ContactFull constant');
  Check(    TIdSipHeaders.IsContact('m'),                'm');
  Check(    TIdSipHeaders.IsContact(ContactHeaderShort), 'ContactShort constant');
  Check(not TIdSipHeaders.IsContact(''),                 '''''');
  Check(not TIdSipHeaders.IsContact('Via'),              'Via');
  Check(    TIdSipHeaders.IsContact('CoNTaCt'),          'CoNTaCt');
end;

procedure TestTIdSipHeaders.TestIsContentLength;
begin
  Check(    TIdSipHeaders.IsContentLength('Content-Length'),         'Content-Length');
  Check(    TIdSipHeaders.IsContentLength(ContentLengthHeaderFull),  'ContentLengthHeaderFull constant');
  Check(    TIdSipHeaders.IsContentLength('l'),                      'l');
  Check(    TIdSipHeaders.IsContentLength(ContentLengthHeaderShort), 'ContentLengthHeaderShort constant');
  Check(not TIdSipHeaders.IsContentLength(''),                       '''''');
  Check(not TIdSipHeaders.IsContentLength('Via'),                    'Via');
  Check(    TIdSipHeaders.IsContentLength('content-LeNgTh'),         'content-LeNgTh');
end;

procedure TestTIdSipHeaders.TestIsCSeq;
begin
  Check(not TIdSipHeaders.IsCSeq(''),         '''''');
  Check(    TIdSipHeaders.IsCSeq('cSeQ'),     'cSeQ');
  Check(    TIdSipHeaders.IsCSeq(CSeqHeader), 'CSeqHeader constant');
end;

procedure TestTIdSipHeaders.TestIsFrom;
begin
  Check(    TIdSipHeaders.IsFrom('From'),          'From');
  Check(    TIdSipHeaders.IsFrom(FromHeaderFull),  'FromHeaderFull constant');
  Check(    TIdSipHeaders.IsFrom('f'),             'f');
  Check(    TIdSipHeaders.IsFrom(FromHeaderShort), 'FromShort constant');
  Check(not TIdSipHeaders.IsFrom(''),              '''''');
  Check(not TIdSipHeaders.IsFrom('Via'),           'Via');
  Check(    TIdSipHeaders.IsFrom('fRoM'),          'fRoM');
end;

procedure TestTIdSipHeaders.TestIsMaxForwards;
begin
  Check(not TIdSipHeaders.IsMaxForwards(''),                '''''');
  Check(    TIdSipHeaders.IsMaxForwards('max-FORWARDS'),    'max-FORWARDS');
  Check(    TIdSipHeaders.IsMaxForwards(MaxForwardsHeader), 'MaxForwardsHeader constant');
end;

procedure TestTIdSipHeaders.TestIsTo;
begin
  Check(not TIdSipHeaders.IsTo(''),            '''''');
  Check(not TIdSipHeaders.IsTo('Tot'),         'Tot');
  Check(    TIdSipHeaders.IsTo('To'),          'To');
  Check(    TIdSipHeaders.IsTo('t'),           't');
  Check(    TIdSipHeaders.IsTo(ToHeaderFull),  'ToHeaderFull constant');
  Check(    TIdSipHeaders.IsTo(ToHeaderShort), 'ToHeaderShort constant');
end;

procedure TestTIdSipHeaders.TestIsVia;
begin
  Check(    TIdSipHeaders.IsVia('Via'),                  'Via');
  Check(    TIdSipHeaders.IsVia(ViaHeaderFull),          'ViaFull constant');
  Check(    TIdSipHeaders.IsVia('v'),                    'v');
  Check(    TIdSipHeaders.IsVia(ViaHeaderShort),         'ViaShort constant');
  Check(not TIdSipHeaders.IsVia(''),                     '''''');
  Check(not TIdSipHeaders.IsVia('Content-Length'),       'Content-Length');
  Check(    TIdSipHeaders.IsVia('Via:'),                 'Via:');
  Check(    TIdSipHeaders.IsVia('ViA'),                  'ViA');
end;

procedure TestTIdSipHeaders.TestSetMaxForwards;
begin
  Self.H.Headers[MaxForwardsHeader].Value := '1';
  CheckEquals('1', Self.H.Headers[MaxForwardsHeader].Value, '1');

  try
    Self.H.Headers[MaxForwardsHeader].Value := 'a';
    Fail('Failed to bail out setting value to ''a''');
  except
    on E: EBadHeader do;
  end;
end;

procedure TestTIdSipHeaders.TestValues;
begin
  CheckEquals('',
              Self.H.Headers[CallIdHeaderFull].Value,
              'Newly created header');

  Self.H.Values[CallIdHeaderFull] := 'b';
  CheckEquals('b',
              Self.H.Headers[CallIdHeaderFull].Value,
              'Header value not set');

  Self.H.Values[ContentTypeHeaderFull] := 'Content-Type';
  CheckEquals('Content-Type',
              Self.H.Headers[ContentTypeHeaderFull].Value,
              'New header value not set');
end;

//******************************************************************************
//* TestTIdSipPath                                                             *
//******************************************************************************
//* TestTIdSipPath Public methods **********************************************

procedure TestTIdSipPath.SetUp;
begin
  inherited SetUp;

  Self.Headers := TIdSipHeaders.Create;
  Self.Path := TIdSipPath.Create(Self.Headers);
end;

procedure TestTIdSipPath.TearDown;
begin
  Self.Path.Free;
  Self.Headers.Free;

  inherited TearDown;
end;

//* TestTIdSipPath Published methods *******************************************

procedure TestTIdSipPath.TestAddAndLastHop;
var
  Hop: TIdSipViaHeader;
begin
  CheckEquals(0, Self.Path.Length, 'Has hops, but is newly created');

  Hop := Headers.Add(ViaHeaderFull) as TIdSipViaHeader;

  Hop.Host       := '127.0.0.1';
  Hop.Port       := 5060;
  Hop.SipVersion := 'SIP/2.0';
  Hop.Transport  := sttSCTP;

  CheckEquals(1, Self.Path.Length, 'Has no hops after Add');

  CheckEquals('127.0.0.1', Self.Path.LastHop.Host,       'Host');
  CheckEquals(5060,        Self.Path.LastHop.Port,       'Port');
  CheckEquals('SIP/2.0',   Self.Path.LastHop.SipVersion, 'SipVersion');
  Check      (sttSCTP =    Self.Path.LastHop.Transport,  'Transport');
end;

procedure TestTIdSipPath.TestFirstHop;
var
  Hop: TIdSipViaHeader;
begin
  Hop := Headers.Add(ViaHeaderFull) as TIdSipViaHeader;

  Hop.Host       := '127.0.0.1';
  Hop.Port       := 5060;
  Hop.SipVersion := 'SIP/2.0';
  Hop.Transport  := sttSCTP;

  CheckEquals('127.0.0.1', Self.Path.FirstHop.Host,       'Host');
  CheckEquals(5060,        Self.Path.FirstHop.Port,       'Port');
  CheckEquals('SIP/2.0',   Self.Path.FirstHop.SipVersion, 'SipVersion');
  Check      (sttSCTP =    Self.Path.FirstHop.Transport,  'Transport');

  Check(Self.Path.FirstHop = Self.Path.LastHop, 'Sanity check on single-node Path');

  Hop := Headers.Add(ViaHeaderFull) as TIdSipViaHeader;
  Hop.Host       := '192.168.0.1';
  Hop.Port       := 5061;
  Hop.SipVersion := 'SIP/2.0';
  Hop.Transport  := sttTLS;

  CheckEquals('192.168.0.1', Self.Path.FirstHop.Host,       'Host');
  CheckEquals(5061,          Self.Path.FirstHop.Port,       'Port');
  CheckEquals('SIP/2.0',     Self.Path.FirstHop.SipVersion, 'SipVersion');
  Check      (sttTLS =       Self.Path.FirstHop.Transport,  'Transport');

  Check(Self.Path.FirstHop <> Self.Path.LastHop, 'Sanity check on two-node Path');
end;

//******************************************************************************
//* TestTIdSipRequest                                                          *
//******************************************************************************
//* TestTIdSipRequest Public methods *******************************************

procedure TestTIdSipRequest.SetUp;
begin
  inherited SetUp;

  Request := TIdSipRequest.Create;  
end;

procedure TestTIdSipRequest.TearDown;
begin
  Request.Free;

  inherited TearDown;
end;

//* TestTIdSipRequest Published methods ****************************************

procedure TestTIdSipRequest.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
  Parser:   TIdSipParser;
  Str:      TStringStream;
  Hop:      TIdSipViaHeader;
begin
  Request.Method                       := 'INVITE';
  Request.Request                      := 'sip:wintermute@tessier-ashpool.co.lu';
  Request.SIPVersion                   := SIPVersion;
  Hop := Request.Headers.Add(ViaHeaderFull) as TIdSipViaHeader;
  Hop.Value                            := 'SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds';
  Request.MaxForwards                  := 70;

  Request.Headers.Add('To').Value      := 'Wintermute <sip:wintermute@tessier-ashpool.co.lu>';
  Request.Headers.Add('From').Value    := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
  Request.CallID                       := 'a84b4c76e66710@gw1.leo_ix.org';
  Request.Headers.Add('CSeq').Value    := '314159 INVITE';
  Request.Headers.Add('Contact').Value := '<sip:wintermute@tessier-ashpool.co.lu>';

  Request.ContentLength                := 29;
  Request.Body                         := 'I am a message. Hear me roar!';

  Expected := TStringList.Create;
  try
    Expected.Text := BasicRequest;

    Received := TStringList.Create;
    try
      Received.Text := Request.AsString;

      CheckEquals(Expected, Received, '');

      Parser := TIdSipParser.Create;
      try
        Str := TStringStream.Create(Received.Text);
        try
          Parser.Source := Str;

          Parser.ParseRequest(Request);
        finally
          Str.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipRequest.TestReadBody;
var
  Len: Integer;
  S:   String;
  Str: TStringStream;
begin
  Request.ContentLength := 8;

  Str := TStringStream.Create('Negotium perambuians in tenebris');
  try
    Request.ReadBody(Str);
    CheckEquals('Negotium', Request.Body, 'Body');

    Len := Length(' perambuians in tenebris');
    SetLength(S, Len);
    Str.Read(S[1], Len);
    CheckEquals(' perambuians in tenebris', S, 'Unread bits of the stream');
  finally
    Str.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipResponse                                                         *
//******************************************************************************
//* TestTIdSipResponse Public methods ******************************************

procedure TestTIdSipResponse.SetUp;
begin
  inherited SetUp;

  Response := TIdSipResponse.Create;
end;

procedure TestTIdSipResponse.TearDown;
begin
  Response.Free;

  inherited TearDown;
end;

//* TestTIdSipResponse Published methods ***************************************

procedure TestTIdSipResponse.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
  Parser:   TIdSipParser;
  Str:      TStringStream;
begin
  Response.StatusCode                   := 486;
  Response.StatusText                   := 'Busy Here';
  Response.SIPVersion                   := SIPVersion;
  Response.Headers.Add('Via').Value     := 'SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds';
  Response.MaxForwards                  := 70;
  Response.Headers.Add('To').Value      := 'Wintermute <sip:wintermute@tessier-ashpool.co.lu>';
  Response.Headers.Add('From').Value    := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
  Response.CallID                       := 'a84b4c76e66710@gw1.leo_ix.org';
  Response.Headers.Add('CSeq').Value    := '314159 INVITE';
  Response.Headers.Add('Contact').Value := '<sip:wintermute@tessier-ashpool.co.lu>';
  Response.ContentLength                := 29;
  Response.Body                         := 'I am a message. Hear me roar!';

  Expected := TStringList.Create;
  try
    Expected.Text := BasicResponse;

    Received := TStringList.Create;
    try
      Received.Text := Response.AsString;

      CheckEquals(Expected, Received, '');

      Parser := TIdSipParser.Create;
      try
        Str := TStringStream.Create(Received.Text);
        try
          Parser.Source := Str;

          Parser.ParseResponse(Response);
        finally
          Str.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipResponse.TestReadBody;
var
  Len: Integer;
  S:   String;
  Str: TStringStream;
begin
  Response.ContentLength := 8;

  Str := TStringStream.Create('Negotium perambuians in tenebris');
  try
    Response.ReadBody(Str);
    CheckEquals('Negotium', Response.Body, 'Body');

    Len := Length(' perambuians in tenebris');
    SetLength(S, Len);
    Str.Read(S[1], Len);
    CheckEquals(' perambuians in tenebris', S, 'Unread bits of the stream');
  finally
    Str.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipParser                                                           *
//******************************************************************************
//* TestTIdSipParser Public methods ********************************************

procedure TestTIdSipParser.SetUp;
begin
  P := TIdSipParser.Create;

  Request := TIdSipRequest.Create;
  Response := TIdSipResponse.Create;
end;

procedure TestTIdSipParser.TearDown;
begin
  Request.Free;
  Response.Free;
  P.Free;
end;

//* TestTIdSipParser Published methods *****************************************

procedure TestTIdSipParser.TestCanonicaliseName;
begin
  CheckEquals(CSeqHeader, P.CanonicaliseName('cseq'),     'cseq');
  CheckEquals(CSeqHeader, P.CanonicaliseName('CSeq'),     'CSeq');
  CheckEquals(CSeqHeader, P.CanonicaliseName(CSeqHeader), 'CSeqHeader constant');

  CheckEquals(ContactHeaderFull, P.CanonicaliseName('contact'),          'contact');
  CheckEquals(ContactHeaderFull, P.CanonicaliseName('Contact'),          'Contact');
  CheckEquals(ContactHeaderFull, P.CanonicaliseName('m'),                'm');
  CheckEquals(ContactHeaderFull, P.CanonicaliseName('M'),                'M');
  CheckEquals(ContactHeaderFull, P.CanonicaliseName(ContactHeaderFull),  'ContactHeaderFull constant');
  CheckEquals(ContactHeaderFull, P.CanonicaliseName(ContactHeaderShort), 'ContactHeaderShort constant');

  CheckEquals(ContentLengthHeaderFull, P.CanonicaliseName('Content-Length'),         'Content-Length');
  CheckEquals(ContentLengthHeaderFull, P.CanonicaliseName('Content-Length'),         'Content-Length');
  CheckEquals(ContentLengthHeaderFull, P.CanonicaliseName('l'),                      'l');
  CheckEquals(ContentLengthHeaderFull, P.CanonicaliseName('L'),                      'L');
  CheckEquals(ContentLengthHeaderFull, P.CanonicaliseName(ContentLengthHeaderFull),  'ContentLengthHeaderFull constant');
  CheckEquals(ContentLengthHeaderFull, P.CanonicaliseName(ContentLengthHeaderShort), 'ContentLengthHeaderShort constant');

  CheckEquals(FromHeaderFull, P.CanonicaliseName('from'),          'from');
  CheckEquals(FromHeaderFull, P.CanonicaliseName('From'),          'From');
  CheckEquals(FromHeaderFull, P.CanonicaliseName('f'),             'f');
  CheckEquals(FromHeaderFull, P.CanonicaliseName('F'),             'F');
  CheckEquals(FromHeaderFull, P.CanonicaliseName(FromHeaderFull),  'FromHeaderFull constant');
  CheckEquals(FromHeaderFull, P.CanonicaliseName(FromHeaderShort), 'FromHeaderShort constant');

  CheckEquals(MaxForwardsHeader, P.CanonicaliseName('max-forwards'),    'max-forwards');
  CheckEquals(MaxForwardsHeader, P.CanonicaliseName('Max-Forwards'),    'Max-Forwards');
  CheckEquals(MaxForwardsHeader, P.CanonicaliseName(MaxForwardsHeader), 'MaxForwardsHeader constant');

  CheckEquals(SubjectHeader, P.CanonicaliseName('subject'),     'subject');
  CheckEquals(SubjectHeader, P.CanonicaliseName('Subject'),     'Subject');
  CheckEquals(SubjectHeader, P.CanonicaliseName(SubjectHeader), 'SubjectHeader constant');

  CheckEquals(ToHeaderFull, P.CanonicaliseName('to'),          'to');
  CheckEquals(ToHeaderFull, P.CanonicaliseName('To'),          'To');
  CheckEquals(ToHeaderFull, P.CanonicaliseName('t'),           't');
  CheckEquals(ToHeaderFull, P.CanonicaliseName('T'),           'T');
  CheckEquals(ToHeaderFull, P.CanonicaliseName(ToHeaderFull),  'ToHeaderFull constant');
  CheckEquals(ToHeaderFull, P.CanonicaliseName(ToHeaderShort), 'ToHeaderShort constant');

  CheckEquals(ViaHeaderFull, P.CanonicaliseName('via'),          'via');
  CheckEquals(ViaHeaderFull, P.CanonicaliseName('Via'),          'Via');
  CheckEquals(ViaHeaderFull, P.CanonicaliseName('v'),            'v');
  CheckEquals(ViaHeaderFull, P.CanonicaliseName('V'),            'V');
  CheckEquals(ViaHeaderFull, P.CanonicaliseName(ViaHeaderFull),  'ViaHeaderFull constant');
  CheckEquals(ViaHeaderFull, P.CanonicaliseName(ViaHeaderShort), 'ViaHeaderShort constant');

  CheckEquals('', P.CanonicaliseName(''), '''''');
end;

procedure TestTIdSipParser.TestCaseInsensitivityOfContentLengthHeader;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Content-LENGTH: 29'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    P.Source := Str;

    P.ParseRequest(Request);

    CheckEquals(29, Request.ContentLength, 'ContentLength');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestGetHeaderName;
begin
  CheckEquals('haha', P.GetHeaderName('haha'),        'haha');
  CheckEquals('haha', P.GetHeaderName('haha: kief'),  'haha: kief');
  CheckEquals('haha', P.GetHeaderName('haha:kief'),   'haha:kief');
  CheckEquals('haha', P.GetHeaderName('haha :kief'),  'haha :kief');
  CheckEquals('haha', P.GetHeaderName('haha : kief'), 'haha : kief');
  CheckEquals('haha', P.GetHeaderName(' haha'),       ' haha');
  CheckEquals('',     P.GetHeaderName(''),            '''''');
  CheckEquals('',     P.GetHeaderName(#0),            '#0');
end;

procedure TestTIdSipParser.TestGetHeaderNumberValue;
begin
  CheckEquals(12, P.GetHeaderNumberValue(Request, 'one :12'), 'one :12');
  CheckEquals(13, P.GetHeaderNumberValue(Request, 'one:13'), 'one:13');
  CheckEquals(14, P.GetHeaderNumberValue(Request, 'one : 14'), 'one : 14');

  try
    P.GetHeaderNumberValue(Request, '');
    Fail('Failed to bail getting numeric value of '''' (request)');
  except
    on EBadRequest do;
  end;

  try
    P.GetHeaderNumberValue(Response, '');
    Fail('Failed to bail getting numeric value of '''' (response)');
  except
    on EBadResponse do;
  end;

  try
    P.GetHeaderNumberValue(Response, 'haha: one');
    Fail('Failed to bail getting numeric value of ''haha: one''');
  except
    on EBadResponse do;
  end;
end;

procedure TestTIdSipParser.TestGetHeaderValue;
begin
  CheckEquals('',     P.GetHeaderValue('haha'),        'haha');
  CheckEquals('kief', P.GetHeaderValue('haha: kief'),  'haha: kief');
  CheckEquals('kief', P.GetHeaderValue('haha:kief'),   'haha:kief');
  CheckEquals('kief', P.GetHeaderValue('haha :kief'),  'haha :kief');
  CheckEquals('kief', P.GetHeaderValue('haha : kief'), 'haha : kief');
  CheckEquals('kief', P.GetHeaderValue(' : kief'),  ' : kief');
  CheckEquals('kief', P.GetHeaderValue(': kief'),  ': kief');
  CheckEquals('',     P.GetHeaderValue(' haha'),       ' haha');
  CheckEquals('',     P.GetHeaderValue(''),            '''''');
  CheckEquals('',     P.GetHeaderValue(#0),            '#0');
end;

procedure TestTIdSipParser.TestIsMethod;
begin
  Check(    P.IsMethod('INVITE'),                       'INVITE');
  Check(    P.IsMethod('X-INVITE'),                     'X-INVITE');
  Check(not P.IsMethod(''),                             '''''');
  Check(    P.IsMethod('1'),                            '1');
  Check(    P.IsMethod('a'),                            'a');
  Check(    P.IsMethod('---'),                          '---');
  Check(    P.IsMethod('X_CITE'),                       'X_CITE');
  Check(not P.IsMethod('Cra.-zy''+prea"cher%20man~`!'), 'Cra.-zy''+prea"cher%20man~`!'); // no "'s
  Check(    P.IsMethod('Cra.-zy''+preacher%20man~`!'),  'Cra.-zy''+preacher%20man~`!');
  Check(not P.IsMethod('LastChar"'),                    'LastChar"'); // no "'s
end;

procedure TestTIdSipParser.TestIsNumber;
begin
  Check(not P.IsNumber(''),                     '''''');
  Check(not P.IsNumber('a'),                    'a');
  Check(not P.IsNumber(#0),                     '#0');
  Check(    P.IsNumber('13'),                   '13');
  Check(    P.IsNumber('98765432109876543210'), '98765432109876543210');
end;

procedure TestTIdSipParser.TestIsSipVersion;
begin
  Check(not P.IsSipVersion(''),         '''''');
  Check(    P.IsSipVersion('SIP/2.0'),  'SIP/2.0');
  Check(    P.IsSipVersion('sip/2.0'),  'sip/2.0');
  Check(    P.IsSipVersion(SIPVersion), 'SIPVersion constant');
  Check(not P.IsSipVersion('SIP/X.Y'),  'SIP/X.Y');
end;

procedure TestTIdSipParser.TestIsToken;
begin
  Check(not P.IsToken(''),         '''''');
  Check(    P.IsToken('one'),      'one');
  Check(    P.IsToken('1two'),     '1two');
  Check(    P.IsToken('1-two'),    '1-two');
  Check(    P.IsToken('.'),        '.');
  Check(    P.IsToken('!'),        '!');
  Check(    P.IsToken('%'),        '%');
  Check(    P.IsToken('*'),        '*');
  Check(    P.IsToken('_'),        '_');
  Check(    P.IsToken('+'),        '+');
  Check(    P.IsToken('`'),        '`');
  Check(    P.IsToken(''''),        '''');
  Check(    P.IsToken('~'),        '~');
  Check(    P.IsToken('''baka'''), '''baka''');
end;

procedure TestTIdSipParser.TestParseAndMakeMessageEmptyString;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('');
  try
    P.Source := Str;

    try
      P.ParseAndMakeMessage.Free;
    except
      on E: EParser do
        CheckEquals(EmptyInputStream, E.Message, 'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeMessageMalformedRequest;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/;2.0'#13#10
                            + #13#10);
  try
    P.Source := Str;

    try
      P.ParseAndMakeMessage;
      Fail('Failed to bail out on parsing a malformed message');
    except
      on E: EBadRequest do;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeMessageRequest;
var
  Msg: TIdSipMessage;
  Str: TStringStream;
begin
  Str := TStringStream.Create(BasicRequest);
  try
    P.Source := Str;

    Msg := P.ParseAndMakeMessage;
    try
      Self.CheckBasicRequest(Msg);
    finally
      Msg.Free;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeMessageResponse;
var
  Msg: TIdSipMessage;
  Str: TStringStream;
begin
  Str := TStringStream.Create(BasicResponse);
  try
    P.Source := Str;

    Msg := P.ParseAndMakeMessage;
    try
      Self.CheckBasicResponse(Msg);
    finally
      Msg.Free;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseExtensiveRequest;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(ExhaustiveRequest);
  try
    P.Source := Str;

    P.ParseRequest(Request);
    CheckEquals('a84b4c76e66710@gw1.leo_ix.org',
                Self.Request.Headers[CallIdHeaderFull].Value,
                'Call-ID');
    CheckEquals(TIdSipAddressHeader.ClassName,
                Self.Request.Headers[ContactHeaderFull].ClassName,
                'Contact class');
    CheckEquals('sip:wintermute@tessier-ashpool.co.lu',
                Self.Request.Headers[ContactHeaderFull].Value,
                'Contact');
    CheckEquals('',
                (Self.Request.Headers[ContactHeaderFull] as TIdSipAddressHeader).DisplayName,
                'Contact DisplayName');
    CheckEquals('sip:wintermute@tessier-ashpool.co.lu',
                (Self.Request.Headers[ContactHeaderFull] as TIdSipAddressHeader).Address.GetFullURI,
                'Contact Address');
    CheckEquals(TIdSipNumericHeader.ClassName,
                Self.Request.Headers[ContentLengthHeaderFull].ClassName,
                'Content-Length class');
    CheckEquals(29,
                (Self.Request.Headers[ContentLengthHeaderFull] as TIdSipNumericHeader).NumericValue,
                'Content-Length');
    CheckEquals('text/plain',
                Self.Request.Headers[ContentTypeHeaderFull].Value,
                'Content-Type');
    CheckEquals(TIdSipCSeqHeader.ClassName,
                Self.Request.Headers[CSeqHeader].ClassName,
                'CSeq class');
    CheckEquals(314159,
                (Self.Request.Headers[CSeqHeader] as TIdSipCSeqHeader).SequenceNo,
                'CSeq SequenceNo');
    CheckEquals('INVITE',
                (Self.Request.Headers[CSeqHeader] as TIdSipCSeqHeader).Method,
                'CSeq Method');
    CheckEquals(TIdSipDateHeader.ClassName,
                Self.Request.Headers[DateHeader].ClassName,
                'Date class');
    CheckEquals('Thu, 1 Jan 1970 00:00:00 +0000',
                (Self.Request.Headers[DateHeader] as TIdSipDateHeader).Time.GetAsRFC822,
                'Date');
    CheckEquals(TIdSipNumericHeader.ClassName,
                Self.Request.Headers[ExpiresHeader].ClassName,
                'Expires class');
    CheckEquals(1000,
                (Self.Request.Headers[ExpiresHeader] as TIdSipNumericHeader).NumericValue,
                'Expires');
    CheckEquals(TIdSipAddressHeader.ClassName,
                Self.Request.Headers[FromHeaderFull].ClassName,
                'From class');
    CheckEquals('Case',
                (Self.Request.Headers[FromHeaderFull] as TIdSipAddressHeader).DisplayName,
                'From DisplayName');
    CheckEquals('sip:case@fried.neurons.org',
                (Self.Request.Headers[FromHeaderFull] as TIdSipAddressHeader).Address.GetFullURI,
                'From Address');
    CheckEquals(';tag=1928301774',
                Self.Request.Headers[FromHeaderFull].ParamsAsString,
                'From parameters');
    CheckEquals(TIdSipMaxForwardsHeader.ClassName,
                Self.Request.Headers[MaxForwardsHeader].ClassName,
                'Max-Forwards class');
    CheckEquals(70,
                (Self.Request.Headers[MaxForwardsHeader] as TIdSipMaxForwardsHeader).NumericValue,
                'Max-Forwards');
    CheckEquals(TIdSipAddressHeader.ClassName,
                Self.Request.Headers[ToHeaderFull].ClassName,
                'To class');
    CheckEquals('Wintermute',
                (Self.Request.Headers[ToHeaderFull] as TIdSipAddressHeader).DisplayName,
                ' DisplayName');
    CheckEquals('sip:wintermute@tessier-ashpool.co.lu',
                (Self.Request.Headers[ToHeaderFull] as TIdSipAddressHeader).Address.GetFullURI,
                'To Address');
    CheckEquals(TIdSipViaHeader.ClassName,
                Self.Request.Headers[ViaHeaderFull].ClassName,
                'Via class');
    CheckEquals('gw1.leo_ix.org',
                (Self.Request.Headers[ViaHeaderFull] as TIdSipViaHeader).Host,
                'Via Host');
    CheckEquals(5060,
                (Self.Request.Headers[ViaHeaderFull] as TIdSipViaHeader).Port,
                'Via Port');
    CheckEquals('SIP/2.0',
                (Self.Request.Headers[ViaHeaderFull] as TIdSipViaHeader).SipVersion,
                'Via SipVersion');
    Check       (sttTCP =
                (Self.Request.Headers[ViaHeaderFull] as TIdSipViaHeader).Transport,
                'Via Transport');
    CheckEquals(';branch=z9hG4bK776asdhds',
                (Self.Request.Headers[ViaHeaderFull] as TIdSipViaHeader).ParamsAsString,
                'Via Parameters');
    CheckEquals('I am not defined in RFC 3261',
                Self.Request.Headers['X-Not-A-Header'].Value,
                'X-Not-A-Header');
    CheckEquals(13, Self.Request.Headers.Count, 'Headers.Count');
{
  ExhaustiveRequest = 'INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                    + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
                    + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                    + 'Content-Length: 29'#13#10
                    + 'Content-Type: text/plain'#13#10
                    + 'CSeq: 314159 INVITE'#13#10
                    + 'Date: Thu, 1 January 1970 00:00:00 GMT'#13#10
                    + 'Expires: 1000'#13#10
                    + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                    + 'Max-Forwards: 70'#13#10
                    + 'Subject: I am a SIP request with every legal header (even an extension)'#13#10
                    + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                    + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10
                    + 'X-Not-A-Header: I am not defined in RFC 3261'#13#10
                    + #13#10
                    + 'I am a message. Hear me roar!';
}

  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseReallyLongViaHeader;
const
  IterCount = 49;
var
  Msg: TIdSipMessage;
  Str: TStringStream;
  S:   String;
  I, J: Integer;
begin
  S := 'Via: ';
  for I := 0 to IterCount do
    for J := 0 to IterCount do
      S := S + 'SIP/2.0/UDP 127.0.' + IntToStr(I) + '.' + IntToStr(J) + ',';
  Delete(S, Length(S), 1);

  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
               + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10
               + 'Max-Forwards: 70'#13#10
               + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
               + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
               + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
               + 'CSeq: 314159 INVITE'#13#10
               + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
               + 'Content-Length: 29'#13#10
               + S + #13#10
               + #13#10
               + 'I am a message. Hear me roar!');
  try
    P.Source := Str;

    Msg := P.ParseAndMakeMessage;
    try
      Self.CheckEquals((IterCount+1)*(IterCount+1) + 1, Msg.Path.Length, 'Length');
    finally
      Msg.Free;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequest;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(BasicRequest);
  try
    P.Source := Str;

    P.ParseRequest(Request);
    Self.CheckBasicRequest(Request);
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestEmptyString;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('');
  try
    P.Source := Str;
    P.ParseRequest(Request);

    CheckEquals('', Request.Method,     'Method');
    CheckEquals('', Request.Request,    'Request');
    CheckEquals('', Request.SipVersion, 'SipVersion');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestFoldedHeader;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                          + 'From: Case'#13#10
                          + ' <sip:case@fried.neurons.org>'#13#10
                          + #9';tag=1928301774'#13#10
                          + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                          + 'CSeq: 8'#13#10
                          + '  INVITE'#13#10
                          + #13#10);
  try
    P.Source := Str;
    P.ParseRequest(Request);
    CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',
                Request.Headers['from'].AsString,
                'From header');
    CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',
                Request.Headers['to'].AsString,
                'To header');
    CheckEquals('CSeq: 8 INVITE',
                Request.Headers['cseq'].AsString,
                'CSeq header');
    CheckEquals(3, Request.Headers.Count, 'Header count');
//    CheckEquals(0, Request.ContentLength, 'Content-Length');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMalformedMaxForwards;
var
  Str: TStringStream;
begin
  // Section 20.22 states that 0 <= Max-Forwards <= 255
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Max-Forwards: 666'#13#10
                            + #13#10);
  try
    P.Source := Str;

    try
      P.ParseRequest(Request);
      Fail('Failed to bail out on a Bad Request');
    except
      on E: EBadRequest do
        CheckEquals(Format(MalformedToken, [MaxForwardsHeader,
                                            'Max-Forwards: 666']),
                    E.Message,
                    'Exception type');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMalformedMethod;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('Bad"method sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                          + 'From: Case'#13#10
                          + ' <sip:case@fried.neurons.org>'#13#10
                          + #9';tag=1928301774'#13#10
                          + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                          + #13#10);
  try
    P.Source := Str;

    try
      P.ParseRequest(Request);
      Fail('Failed to bail out on a Bad Request');
    except
      on E: EBadRequest do
        CheckEquals(Format(MalformedToken, ['Method', 'Bad"method']), E.Message, 'Exception type');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMalformedRequestLine;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE  sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10);
  try
    P.Source := Str;
    try
      P.ParseRequest(Request);
      Fail('Malformed start line (too many spaces between Method and Request-URI) parsed without error');
    except
      on E: EBadRequest do
        CheckEquals(RequestUriNoSpaces, E.Message, 'Too many spaces');
    end;
  finally
    Str.Free;
  end;

  Str := TStringStream.Create('INVITEsip:wintermute@tessier-ashpool.co.luSIP/2.0'#13#10);
  try
    P.Source := Str;
    try
      P.ParseRequest(Request);
      Fail('Malformed start line (no spaces between Method and Request-URI) parsed without error');
    except
      on E: EBadRequest do
        CheckEquals(Format(MalformedToken, ['Request-Line', 'INVITEsip:wintermute@tessier-ashpool.co.luSIP/2.0']),
                    E.Message,
                    'Missing spaces');
    end;
  finally
    Str.Free;
  end;

  Str := TStringStream.Create('sip:wintermute@tessier-ashpool.co.lu SIP/2.0');
  try
    P.Source := Str;
    try
      P.ParseRequest(Request);
      Fail('Malformed start line (no Method) parsed without error');
    except
      on E: EBadRequest do
        CheckEquals(Format(MalformedToken, ['Request-Line', 'sip:wintermute@tessier-ashpool.co.lu SIP/2.0']),
                    E.Message,
                    'Missing Method');
    end;
  finally
    Str.Free;
  end;

  Str := TStringStream.Create('INVITE'#13#10);
  try
    P.Source := Str;
    try
      P.ParseRequest(Request);
      Fail('Malformed start line (no Request-URI, no SIP-Version) parsed without error');
    except
      on E: EBadRequest do
        CheckEquals(Format(MalformedToken, ['Request-Line', 'INVITE']),
                    E.Message,
                    'Missing Request & SIP Version');
    end;
  finally
    Str.Free;
  end;

  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/;2.0'#13#10);
  try
    P.Source := Str;
    try
      P.ParseRequest(Request);
      Fail('Malformed start line (malformed SIP-Version) parsed without error');
    except
      on E: EBadRequest do
        CheckEquals(Format(InvalidSipVersion, ['SIP/;2.0']), E.Message, 'Malformed SIP-Version');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMessageBodyLongerThanContentLength;
var
  Str: TStringStream;
  Leftovers: array[0..99] of Char;
  S: String;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Content-Length: 4'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    P.Source := Str;

    P.ParseRequest(Request);
    CheckEquals(4,  Request.ContentLength, 'ContentLength');
    CheckEquals('', Request.Body,          'Body');

    Str.Read(LeftOvers, 100);
    S := LeftOvers;
    CheckEquals('I am a message. Hear me roar!', S, 'Remaining (extra) bits in the message');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMultipleVias;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo_ix.org:5061;branch=z9hG4bK776asdhds'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Via: SIP/3.0/TLS gw5.cust1.leo_ix.org;branch=z9hG4bK776aheh'#13#10
                            + 'Content-Length: 4'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    P.Source := Str;
    P.ParseRequest(Request);

    CheckEquals(2,                       Request.Path.Length,                   'Path.Length');
    Check      (Request.Path.FirstHop <> Request.Path.LastHop,                  'Sanity check on Path');
    CheckEquals('Via',                   Request.Path.LastHop.Name,             'LastHop.Name');
    CheckEquals('SIP/2.0',               Request.Path.LastHop.SipVersion,       'LastHop.SipVersion');
    Check      (sttTCP =                 Request.Path.LastHop.Transport,        'LastHop.Transport');
    CheckEquals('gw1.leo_ix.org',        Request.Path.LastHop.Host,             'LastHop.Host');
    CheckEquals(5061,                    Request.Path.LastHop.Port,             'LastHop.Port');
    CheckEquals('z9hG4bK776asdhds',      Request.Path.LastHop.Params['branch'], 'LastHop.Params[''branch'']');
    CheckEquals('SIP/2.0/TCP gw1.leo_ix.org:5061',
                Request.Path.LastHop.Value,
                'LastHop.Value');
    CheckEquals('Via: SIP/2.0/TCP gw1.leo_ix.org:5061;branch=z9hG4bK776asdhds',
                Request.Path.LastHop.AsString,
                'LastHop.AsString');

    CheckEquals('Via',                   Request.Path.FirstHop.Name,             'FirstHop.Name');
    CheckEquals('SIP/3.0',               Request.Path.FirstHop.SipVersion,       'FirstHop.SipVersion');
    Check      (sttTLS =                 Request.Path.FirstHop.Transport,        'FirstHop.Transport');
    CheckEquals('gw5.cust1.leo_ix.org',  Request.Path.FirstHop.Host,             'FirstHop.Host');
    CheckEquals(IdPORT_SIP_TLS,          Request.Path.FirstHop.Port,             'FirstHop.Port');
    CheckEquals('z9hG4bK776aheh',        Request.Path.FirstHop.Params['branch'], 'FirstHop.Params[''branch'']');
    CheckEquals('SIP/3.0/TLS gw5.cust1.leo_ix.org',
                Request.Path.FirstHop.Value,
                'FirstHop.Value');
    CheckEquals('Via: SIP/3.0/TLS gw5.cust1.leo_ix.org;branch=z9hG4bK776aheh',
                Request.Path.FirstHop.AsString,
                'FirstHop.AsString');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestRequestUriHasSpaces;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier ashpool.co.lu SIP/2.0'#13#10);
  try
    P.Source := Str;
    try
      P.ParseRequest(Request);
      Fail('Malformed start line (Request-URI has spaces) parsed without error');
    except
      on E: EBadRequest do
        CheckEquals(RequestUriNoSpaces,
                    E.Message,
                    '<>');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestRequestUriInAngleBrackets;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE <sip:wintermute@tessier-ashpool.co.lu> SIP/2.0'#13#10);
  try
    P.Source := Str;
    try
      P.ParseRequest(Request);
      Fail('Malformed start line (Request-URI enclosed in angle brackets) parsed without error');
    except
      on E: EBadRequest do
        CheckEquals(RequestUriNoAngleBrackets,
                    E.Message,
                    '<>');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestWithLeadingCrLfs;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(#13#10#13#10'INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10);
  try
    P.Source := Str;
    P.ParseRequest(Request);

    CheckEquals('INVITE',                               Request.Method,     'Method');
    CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Request.Request,    'Request');
    CheckEquals('SIP/2.0',                              Request.SipVersion, 'SipVersion');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseResponse;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(BasicResponse);
  try
    P.Source := Str;

    P.ParseResponse(Response);
    Self.CheckBasicResponse(Response);
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseResponseEmptyString;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('');
  try
    P.Source := Str;
    P.ParseResponse(Response);

    CheckEquals(0,  Response.StatusCode, 'StatusCode');
    CheckEquals('', Response.StatusText, 'StatusText');
    CheckEquals('', Response.SipVersion, 'SipVersion');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseResponseFoldedHeader;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('SIP/1.0 200 OK'#13#10
                          + 'From: Case'#13#10
                          + ' <sip:case@fried.neurons.org>'#13#10
                          + #9';tag=1928301774'#13#10
                          + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                          + #13#10);
  try
    P.Source := Str;
    P.ParseResponse(Response);

    CheckEquals('SIP/1.0', Response.SipVersion, 'SipVersion');
    CheckEquals(200,       Response.StatusCode, 'StatusCode');
    CheckEquals('OK',      Response.StatusText, 'StatusTest');

    CheckEquals(2, Response.Headers.Count, 'Header count');
    CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',
                Response.Headers['from'].AsString,
                'From header');
    CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',
                Response.Headers['to'].AsString,
                'To header');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseResponseInvalidStatusCode;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('SIP/1.0 Aheh OK'#13#10);
  try
    P.Source := Str;
    try
      P.ParseResponse(Response);
      Fail('Failed to reject a non-numeric Status-Code');
    except
      on E: EBadResponse do
        CheckEquals(Format(InvalidStatusCode, ['Aheh']),
                    E.Message,
                    '<>');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseResponseWithLeadingCrLfs;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(#13#10#13#10'SIP/2.0 200 OK'#13#10);
  try
    P.Source := Str;
    P.ParseResponse(Response);

    CheckEquals('SIP/2.0', Response.SipVersion, 'SipVersion');
    CheckEquals(200,       Response.StatusCode, 'StatusCode');
    CheckEquals('OK',      Response.StatusText, 'StatusText');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseShortFormContentLength;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'l: 29'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    P.Source := Str;
    P.ParseRequest(Request);
    CheckEquals(29, Request.ContentLength, 'ContentLength');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestTortureTest1;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(TortureTest1);
  try
    P.Source := Str;
    P.ParseRequest(Request);
    CheckEquals('INVITE',                              Request.Method,      'Method');
    CheckEquals('SIP/2.0',                             Request.SipVersion,  'SipVersion');
    CheckEquals('sip:vivekg@chair.dnrc.bell-labs.com', Request.Request,     'Request');
    CheckEquals(6,                                     Request.MaxForwards, 'MaxForwards');
    CheckEquals('0ha0isndaksdj@10.1.1.1',              Request.CallID,      'CallID');

    CheckEquals('',
                Request.ToHeader.DisplayName,
                'ToHeader.DisplayName');
    CheckEquals('sip:vivekg@chair.dnrc.bell-labs.com',
                Request.ToHeader.Address.GetFullURI,
                'ToHeader.Address.GetFullURI');
    CheckEquals(';tag=1918181833n',
                Request.ToHeader.ParamsAsString,
                'ToHeader.ParamsAsString');

    CheckEquals('J Rosenberg \"',
                Request.From.DisplayName,
                'From.DisplayName');
    CheckEquals('sip:jdrosen@lucent.com',
                Request.From.Address.GetFullURI,
                'From.Address.GetFullURI');
    CheckEquals(';tag=98asjd8',
                Request.From.ParamsAsString,
                'From.ParamsAsString');

    CheckEquals(3, Request.Path.Length, 'Path.Length');

    CheckEquals('To: sip:vivekg@chair.dnrc.bell-labs.com;tag=1918181833n',
                Request.Headers.Items[0].AsString,
                'To header');
    CheckEquals('From: "J Rosenberg \\\"" <sip:jdrosen@lucent.com>;tag=98asjd8',
                Request.Headers.Items[1].AsString,
                'From header');
    CheckEquals('Max-Forwards: 6',
                Request.Headers.Items[2].AsString,
                'Max-Forwards header');
    CheckEquals('Call-ID: 0ha0isndaksdj@10.1.1.1',
                Request.Headers.Items[3].AsString,
                'Call-ID header');
    CheckEquals('CSeq: 8 INVITE',
                Request.Headers.Items[4].AsString,
                'CSeq header');
    CheckEquals('Via: SIP/2.0/UDP 135.180.130.133;branch=z9hG4bKkdjuw',
                Request.Headers.Items[5].AsString,
                'Via header #1');
    CheckEquals('Subject: ',
                Request.Headers.Items[6].AsString,
                'Subject header');
    CheckEquals('NewFangledHeader: newfangled value more newfangled value',
                Request.Headers.Items[7].AsString,
                'NewFangledHeader');
    CheckEquals('Content-Type: application/sdp',
                Request.Headers.Items[8].AsString,
                'Content-Type');
    CheckEquals('Via: SIP/2.0/TCP 1192.168.156.222;branch=9ikj8',
                Request.Headers.Items[9].AsString,
                'Via header #2');
    CheckEquals('Via: SIP/2.0/UDP 192.168.255.111;hidden',
                Request.Headers.Items[10].AsString,
                'Via header #3');
    CheckEquals('Contact: "Quoted string \"\"" <sip:jdrosen@bell-labs.com>;newparam=newvalue;secondparam=secondvalue;q=0.33',
                Request.Headers.Items[11].AsString,
                'Contact header #1');
    CheckEquals('Contact: tel:4443322',
                Request.Headers.Items[12].AsString,
                'Contact header #2');
    CheckEquals(13, Request.Headers.Count, 'Header count');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestTortureTest13;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(TortureTest13);
  try
    P.Source := Str;

    try
      P.ParseRequest(Request);
      Fail('Failed to bail out of a bad request');
    except
      on E: EBadRequest do
        CheckEquals(Format(MalformedToken, [ExpiresHeader, 'Expires: Thu, 44 Dec 19999 16:00:00 EDT']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestTortureTest19;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(TortureTest19);
  try
    P.Source := Str;

    try
      P.ParseRequest(Request);
      Fail('Failed to bail out of a bad request');
    except
      on E: EBadRequest do
        CheckEquals(Format(MalformedToken, [ToHeaderFull, 'To: "Mr. J. User <sip:j.user@company.com>']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestTortureTest21;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(TortureTest21);
  try
    P.Source := Str;

    try
      P.ParseRequest(Request);
      Fail('Failed to bail out of a bad request');
    except
      on E: EBadRequest do
        CheckEquals(RequestUriNoAngleBrackets,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestTortureTest22;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(TortureTest22);
  try
    P.Source := Str;

    try
      P.ParseRequest(Request);
      Fail('Failed to bail out of a bad request');
    except
      on E: EBadRequest do
        CheckEquals(RequestUriNoSpaces,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestTortureTest23;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(TortureTest23);
  try
    P.Source := Str;

    try
      P.ParseRequest(Request);
      Fail('Failed to bail out of a bad request');
    except
      on E: EBadRequest do
        CheckEquals(RequestUriNoSpaces,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestTortureTest35;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(TortureTest35);
  try
    P.Source := Str;

    try
      P.ParseRequest(Request);
      Fail('Failed to bail out with malformed Expires header');
    except
      on E: EBadRequest do
        CheckEquals(Format(MalformedToken, [ExpiresHeader, 'Expires: 0 0l@company.com']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestTortureTest40;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(TortureTest40);
  try
    P.Source := Str;

    try
      P.ParseRequest(Request);
      Fail('Failed to bail out of a bad request');
    except
      on E: EBadRequest do
        CheckEquals(Format(MalformedToken, [FromHeaderFull, 'From:    Bell, Alexander <sip:a.g.bell@bell-tel.com>;tag=43']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

//* TestTIdSipParser Private methods *******************************************

procedure TestTIdSipParser.CheckBasicMessage(const Msg: TIdSipMessage);
begin
  CheckEquals('SIP/2.0',                              Msg.SIPVersion,                   'SipVersion');
  CheckEquals(29,                                     Msg.ContentLength,                'ContentLength');
  CheckEquals(70,                                     Msg.MaxForwards,                  'MaxForwards');
  CheckEquals('a84b4c76e66710@gw1.leo_ix.org',        Msg.CallID,                       'CallID');
  CheckEquals('Wintermute',                           Msg.ToHeader.DisplayName,         'ToHeader.DisplayName');
  CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Msg.ToHeader.Address.GetFullURI,  'ToHeader.Address.GetFullURI');
  CheckEquals('',                                     Msg.ToHeader.ParamsAsString,      'Msg.ToHeader.ParamsAsString');
  CheckEquals('Case',                                 Msg.From.DisplayName,             'From.DisplayName');
  CheckEquals('sip:case@fried.neurons.org',           Msg.From.Address.GetFullURI,      'From.Address.GetFullURI');
  CheckEquals(';tag=1928301774',                      Msg.From.ParamsAsString,          'Msg.From.ParamsAsString');
  CheckEquals(314159,                                 Msg.CSeq.SequenceNo,              'Msg.CSeq.SequenceNo');
  CheckEquals('INVITE',                               Msg.CSeq.Method,                  'Msg.CSeq.Method');

  CheckEquals(TIdSipAddressHeader.ClassName,
              Msg.Headers[ContactHeaderFull].ClassName,
              'Contact header type');

  CheckEquals(1,                  Msg.Path.Length,                   'Path.Length');
  Check      (Msg.Path.FirstHop = Msg.Path.LastHop,                  'Sanity check on Path');
  CheckEquals('SIP/2.0',          Msg.Path.LastHop.SipVersion,       'LastHop.SipVersion');
  Check      (sttTCP =            Msg.Path.LastHop.Transport,        'LastHop.Transport');
  CheckEquals('gw1.leo_ix.org',   Msg.Path.LastHop.Host,             'LastHop.Host');
  CheckEquals(IdPORT_SIP,         Msg.Path.LastHop.Port,             'LastHop.Port');
  CheckEquals('z9hG4bK776asdhds', Msg.Path.LastHop.Params['branch'], 'LastHop.Params[''branch'']');

  CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',  Msg.Headers['to'].AsString,      'To');
  CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774', Msg.Headers['from'].AsString,    'From');
  CheckEquals('CSeq: 314159 INVITE',                                    Msg.Headers['cseq'].AsString,    'CSeq');
  CheckEquals('Contact: sip:wintermute@tessier-ashpool.co.lu',          Msg.Headers['contact'].AsString, 'Contact');
  CheckEquals(8, Msg.Headers.Count, 'Header count');

  CheckEquals('', Msg.Body, 'message-body');
end;

procedure TestTIdSipParser.CheckBasicRequest(const Msg: TIdSipMessage);
begin
  CheckEquals(TIdSipRequest.Classname, Msg.ClassName, 'Class type');

  CheckEquals('INVITE',                               TIdSipRequest(Msg).Method,  'Method');
  CheckEquals('sip:wintermute@tessier-ashpool.co.lu', TIdSipRequest(Msg).Request, 'Request');

  Self.CheckBasicMessage(Msg);
end;

procedure TestTIdSipParser.CheckBasicResponse(const Msg: TIdSipMessage);
begin
  CheckEquals(TIdSipResponse.Classname, Msg.ClassName, 'Class type');

  CheckEquals(486,         TIdSipResponse(Msg).StatusCode, 'StatusCode');
  CheckEquals('Busy Here', TIdSipResponse(Msg).StatusText, 'StatusText');

  Self.CheckBasicMessage(Msg);
end;

initialization
  RegisterTest('SIP Request Parsing', Suite);
end.
