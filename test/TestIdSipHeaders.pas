unit TestIdSipHeaders;

interface

uses
  IdSipHeaders, TestFramework;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestNeedsQuotes;
    procedure TestQuoteStringIfNecessary;
    procedure TestQValueToStr;
    procedure TestStrToQValue;
    procedure TestStrToQValueDef;
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
    procedure TestEncodeQuotedStr;
    procedure TestGetSetParam;
    procedure TestHasParam;
    procedure TestIndexOfParam;
    procedure TestIsEqualTo;
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
    procedure TestHasSipsUri;
    procedure TestSetAddress;
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

  TestTIdSipCallIDHeader = class(TTestCase)
  private
    C: TIdSipCallIDHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsEqualTo;
    procedure TestValue;
    procedure TestValueWithParams;
  end;

  TestTIdSipCommaSeparatedHeader = class(TTestCase)
  private
    C: TIdSipCommaSeparatedHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestValue;
  end;

  TestTIdSipContactHeader = class(TTestCase)
  private
    C: TIdSipContactHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestName;
    procedure TestGetSetExpires;
    procedure TestGetSetQ;
    procedure TestValueWithExpires;
    procedure TestValueWithQ;
    procedure TestValueWithStar;
  end;

  TestTIdSipContentDispositionHeader = class(TTestCase)
  private
    C: TIdSipContentDispositionHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestName;
    procedure TestGetSetHandling;
  end;

  TestTIdSipCSeqHeader = class(TTestCase)
  private
    C: TIdSipCSeqHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestValue;
  end;

  TestTIdSipDateHeader = class(TTestCase)
  private
    D: TIdSipDateHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestName;
    procedure TestValueAbsoluteTime;
    procedure TestValueMalformedAbsoluteTime;
    procedure TestValueRelativeTime;
  end;

  TestTIdSipFromToHeader = class(TTestCase)
  private
    F: TIdSipFromToHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHasTag;
    procedure TestIsEqualDifferentURI;
    procedure TestIsEqualSameURINoParams;
    procedure TestIsEqualSameURIWithParams;
    procedure TestValueWithTag;
    procedure TestValueResettingTag;
    procedure TestGetSetTag;
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
    procedure TestName;
    procedure TestValueNormal;
    procedure TestValueNormalWithParam;
    procedure TestValueNonNumber;
    procedure TestValueTooBig;
  end;

  TestTIdSipRouteHeader = class(TTestCase)
  private
    R: TIdSipRouteHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsLooseRoutable;
    procedure TestName;
    procedure TestValue;
    procedure TestValueWithParamsAndHeaderParams;
  end;

  TestTIdSipRecordRouteHeader = class(TTestCase)
  private
    R: TIdSipRecordRouteHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestName;
    procedure TestValue;
    procedure TestValueWithParamsAndHeaderParams;
  end;

  TestTIdSipTimestampHeader = class(TTestCase)
  private
    T: TIdSipTimestampHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestName;
    procedure TestNormalizeLWS;
    procedure TestReadNumber;
    procedure TestValue;
    procedure TestValueMalformed;
    procedure TestValueWithDelay;
  end;

  TestTIdSipUriHeader = class(TTestCase)
  private
    U: TIdSipUriHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestValue;
    procedure TestValueWithParams;
    procedure TestValueWithUriParams;
  end;

  TestTIdSipViaHeader = class(TTestCase)
  private
    V: TIdSipViaHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestAssignFromBadlyFormedVia;
    procedure TestBranch;
    procedure TestHasBranch;
    procedure TestHasMaddr;
    procedure TestHasReceived;
    procedure TestHasRport;
    procedure TestIsRFC3261Branch;
    procedure TestIsEqualTo;
    procedure TestIsEqualToBranchIsCaseInsensitive;
    procedure TestMaddr;
    procedure TestName;
    procedure TestReceived;
    procedure TestTTL;
    procedure TestValue;
    procedure TestValueWithBranch;
    procedure TestValueWithMaddr;
    procedure TestValueWithReceived;
    procedure TestValueWithTTL;
  end;

  TestTIdSipWarningHeader = class(TTestCase)
  private
    W: TIdSipWarningHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetValue;
    procedure TestName;
    procedure TestSetValue;
    procedure TestSetValueMalformed;
  end;

  TestTIdSipWeightedCommaSeparatedHeader = class(TTestCase)
  private
    W: TIdSipWeightedCommaSeparatedHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddValue;
    procedure TestClearValues;
    procedure TestGetValue;
    procedure TestSetValue;
    procedure TestValueMalformed;
  end;

  TestTIdSipHeaders = class(TTestCase)
  private
    H: TIdSipHeaders;

    procedure CheckType(ExpectedClassType: TClass;
                        ReceivedObject: TObject;
                        Message: String = '');
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAddHeader;
    procedure TestAddHeaderName;
    procedure TestAddHeaders;
    procedure TestAddHeadersFilter;
    procedure TestAddInReverseOrder;
    procedure TestAddResultTypes;
    procedure TestAsString;
    procedure TestCanonicaliseName;
    procedure TestClear;
    procedure TestDelete;
    procedure TestFirst;
    procedure TestHasHeader;
    procedure TestHeaders;
    procedure TestItems;
    procedure TestIsCallID;
    procedure TestIsCompoundHeader;
    procedure TestIsContact;
    procedure TestIsContentLength;
    procedure TestIsCSeq;
    procedure TestIsEmpty;
    procedure TestIsErrorInfo;
    procedure TestIsFrom;
    procedure TestIsMaxForwards;
    procedure TestIsRecordRoute;
    procedure TestIsRoute;
    procedure TestIsTo;
    procedure TestIsVia;
    procedure TestIsWarning;
    procedure TestIteratorVisitsAllHeaders;
    procedure TestRemove;
    procedure TestRemoveAll;
    procedure TestSetMaxForwards;
  end;

  TestTIdSipHeadersFilter = class(TTestCase)
  private
    Headers: TIdSipHeaders;
    Filter:  TIdSipHeadersFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestCount;
    procedure TestFirst;
    procedure TestIsEmpty;
    procedure TestIsEqualToFilter;
    procedure TestIsEqualToHeaders;
    procedure TestItems;
    procedure TestIteratorVisitsAllHeaders;
    procedure TestRemove;
    procedure TestRemoveAll;
  end;

  TestTIdSipViaPath = class(TTestCase)
  private
    Headers: TIdSipHeaders;
    Path:    TIdSipViaPath;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndLastHop;
    procedure TestClear;
    procedure TestRemoveLastHop;
  end;

implementation

uses
  Classes, IdSipConsts, IdSipMessage, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipHeaders unit tests');

  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSipHeader.Suite);
  Result.AddTest(TestTIdSipAddressHeader.Suite);
  Result.AddTest(TestTIdSipCallIDHeader.Suite);
  Result.AddTest(TestTIdSipCommaSeparatedHeader.Suite);
  Result.AddTest(TestTIdSipContactHeader.Suite);
  Result.AddTest(TestTIdSipContentDispositionHeader.Suite);
  Result.AddTest(TestTIdSipCSeqHeader.Suite);
  Result.AddTest(TestTIdSipDateHeader.Suite);
  Result.AddTest(TestTIdSipFromToHeader.Suite);
  Result.AddTest(TestTIdSipMaxForwardsHeader.Suite);
  Result.AddTest(TestTIdSipRouteHeader.Suite);
  Result.AddTest(TestTIdSipRecordRouteHeader.Suite);
  Result.AddTest(TestTIdSipNumericHeader.Suite);
  Result.AddTest(TestTIdSipTimestampHeader.Suite);
  Result.AddTest(TestTIdSipViaHeader.Suite);
  Result.AddTest(TestTIdSipUriHeader.Suite);
  Result.AddTest(TestTIdSipWarningHeader.Suite);
  Result.AddTest(TestTIdSipWeightedCommaSeparatedHeader.Suite);
  Result.AddTest(TestTIdSipHeaders.Suite);
  Result.AddTest(TestTIdSipHeadersFilter.Suite);
  Result.AddTest(TestTIdSipViaPath.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestNeedsQuotes;
begin
  Check(    NeedsQuotes(' '),          'SP');
  Check(    NeedsQuotes('"'),          '"');
  Check(    NeedsQuotes('\'),          '\');
  Check(    NeedsQuotes('"hello\"'),   '"hello\"');
  Check(not NeedsQuotes(''),           '''''');
  Check(not NeedsQuotes('hail eris!'), 'hail eris!');
end;

procedure TestFunctions.TestQuoteStringIfNecessary;
var
  C: Char;
begin
  CheckEquals('',     QuoteStringIfNecessary(''),     '''''');
  CheckEquals('" "',  QuoteStringIfNecessary(' '),    'SP');
  CheckEquals('abcd', QuoteStringIfNecessary('abcd'), 'abcd');

  for C := '!' to Chr($7E) do
    if (C in LegalTokenChars) then
      CheckEquals('ab' + C + 'cd',
                  QuoteStringIfNecessary('ab' + C + 'cd'),
                  'ab' + C + 'cd')
    else
      CheckEquals('"ab' + C + 'cd"',
                  QuoteStringIfNecessary('ab' + C + 'cd'),
                  'ab' + C + 'cd');
end;

procedure TestFunctions.TestQValueToStr;
begin
  CheckEquals('0',     QValueToStr(0),    'QValueToStr(0)');
  CheckEquals('0.001', QValueToStr(1),    'QValueToStr(1)');
  CheckEquals('0.01',  QValueToStr(10),   'QValueToStr(10)');
  CheckEquals('0.1',   QValueToStr(100),  'QValueToStr(100)');
  CheckEquals('0.666', QValueToStr(666),  'QValueToStr(666)');
  CheckEquals('1',     QValueToStr(1000), 'QValueToStr(1000)');
end;

procedure TestFunctions.TestStrToQValue;
begin
  CheckEquals(0,    StrToQValue('0'),     'StrToQValue(''0'')');
  CheckEquals(0,    StrToQValue('0.0'),   'StrToQValue(''0.0'')');
  CheckEquals(0,    StrToQValue('0.00'),  'StrToQValue(''0.00'')');
  CheckEquals(0,    StrToQValue('0.000'), 'StrToQValue(''0.000'')');
  CheckEquals(666,  StrToQValue('0.666'), 'StrToQValue(''0.666'')');
  CheckEquals(700,  StrToQValue('0.7'),   'StrToQValue(''0.7'')');
  CheckEquals(1000, StrToQValue('1'),     'StrToQValue(''1'')');
  CheckEquals(1000, StrToQValue('1.0'),   'StrToQValue(''1.0'')');
  CheckEquals(1000, StrToQValue('1.00'),  'StrToQValue(''1.00'')');
  CheckEquals(1000, StrToQValue('1.000'), 'StrToQValue(''1.000'')');

  try
    StrToQValue('.');
    Fail('Failed to bail out on malformed q (.');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['.', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.');
    Fail('Failed to bail out on malformed q (0.');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['0.', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0. 0');
    Fail('Failed to bail out on malformed q (0. 0)');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['0. 0', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('1.');
    Fail('Failed to bail out on malformed q (1.');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['1.', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.0000');
    Fail('Failed to bail out on too many digits (0.0000)');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['0.0000', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.0123');
    Fail('Failed to bail out on too many digits (0.0123)');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['0.0123', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.1234');
    Fail('Failed to bail out on too many digits (0.1234)');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['0.1234', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.a');
    Fail('Failed to bail out on letters');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['0.a', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('1.1');
    Fail('Failed to bail out on number too big (1.1)');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['1.1', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('3');
    Fail('Failed to bail out on number too big (3)');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['3', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('');
    Fail('Failed to bail out on empty string');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;
end;

procedure TestFunctions.TestStrToQValueDef;
begin
  CheckEquals(666, StrToQValueDef('', 666), '''''');
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

procedure TestTIdSipHeader.TestEncodeQuotedStr;
begin
  CheckEquals('I am a ''normal'' string',
              Self.H.EncodeQuotedStr('I am a ''normal'' string'),
              '''I am a ''''normal'''' string''');
  CheckEquals('',
              Self.H.EncodeQuotedStr(''),
              '''''');
  CheckEquals('\\',
              Self.H.EncodeQuotedStr('\'),
              '\');
  CheckEquals('\"',
              Self.H.EncodeQuotedStr('"'),
              '"');
  CheckEquals('\\\"',
              Self.H.EncodeQuotedStr('\"'),
              '\"');
  CheckEquals('\"I am a ''normal'' string\"',
              Self.H.EncodeQuotedStr('"I am a ''normal'' string"'),
              '''"I am a ''normal'' string"''');
end;

procedure TestTIdSipHeader.TestGetSetParam;
begin
  CheckEquals('', Self.H.Params['branch'], 'Value of non-existent param');

  Self.H.Params['branch'] := '';
  CheckEquals('', Self.H.Params['branch'], 'Value of param with empty string value');

  Self.H.Params['branch'] := 'f00';
  CheckEquals('f00', Self.H.Params['branch'], 'Value of param with non-empty string value');
end;

procedure TestTIdSipHeader.TestHasParam;
begin
  Check(not Self.H.HasParam('foo'), 'New header, no params');

  Self.H.Params['branch'] := 'z9hG4bK776asdhds';
  Check(not Self.H.HasParam('foo'), 'Some non-foo params');

  Self.H.Params['foo'] := 'bar';
  Check(Self.H.HasParam('foo'), 'Some non-foo and foo params');
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

procedure TestTIdSipHeader.TestIsEqualTo;
var
  Header: TIdSipHeader;
begin
  Self.H.Name := 'X-New-Header';
  Self.H.Value := 'secure;foo=bar';
  Header := TIdSipHeader.Create;
  try
    Header.Name  := Self.H.Name;
    Header.Value := Self.H.Value + Self.H.ParamsAsString;
    Check(Self.H.IsEqualTo(Header), 'H = Header');
    Check(Header.IsEqualTo(Self.H), 'Header = H');

    Header.Name  := Self.H.Name;
    Header.Value := Uppercase(Self.H.Value + Self.H.ParamsAsString);
    Check(Self.H.IsEqualTo(Header), 'H = Header, uppercase(value)');
    Check(Header.IsEqualTo(Self.H), 'Header = H, uppercase(value)');

    Header.Name  := 'X-Different-Header';
    Header.Value := Self.H.Value + Self.H.ParamsAsString;
    Check(not Self.H.IsEqualTo(Header), 'H <> Header, name');
    Check(not Header.IsEqualTo(Self.H), 'Header <> H, name');

    Header.Name  := Self.H.Name;
    Header.Value := 'wombat' + Self.H.ParamsAsString;
    Check(not Self.H.IsEqualTo(Header), 'H <> Header, value');
    Check(not Header.IsEqualTo(Self.H), 'Header <> H, value');
  finally
    Header.Free;
  end;
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

  Self.A.Value := 'sip:countzero@jacks-bar.com';
  CheckEquals(ToHeaderFull + ': sip:countzero@jacks-bar.com',
              Self.A.AsString,
              'AsString, plain URI');

  Self.A.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';
  CheckEquals(ToHeaderFull + ': Wintermute <sip:wintermute@tessier-ashpool.co.luna>',
              Self.A.AsString,
              'AsString, display-name');

  Self.A.Value := '"Count Zero\"" <sip:countzero@jacks-bar.com>';
  CheckEquals(ToHeaderFull + ': "Count Zero\"" <sip:countzero@jacks-bar.com>',
              Self.A.AsString,
              'AsString, display-name with quoted-pair');

  Self.A.Value := '"Count Zero\"" <sip:countzero@jacks-bar.com>;paranoid';
  CheckEquals(ToHeaderFull + ': "Count Zero\"" <sip:countzero@jacks-bar.com>;paranoid',
              Self.A.AsString,
              'AsString, display-name with quoted-pair + parameters');

  Self.A.Value := 'Count Zero <sip:countzero@jacks-bar.com;paranoid>;very';
  CheckEquals(ToHeaderFull + ': Count Zero <sip:countzero@jacks-bar.com;paranoid>;very',
              Self.A.AsString,
              'AsString, display-name, and URI and header have parameters');

  Self.A.Value         := '';
  Self.A.DisplayName   := 'Bell, Alexander';
  Self.A.Address.URI   := 'sip:a.g.bell@bell-tel.com';
  Self.A.Params['tag'] := '43';
  CheckEquals(ToHeaderFull + ': "Bell, Alexander" <sip:a.g.bell@bell-tel.com>;tag=43',
              Self.A.AsString,
              'AsString, display-name with comma');
end;

procedure TestTIdSipAddressHeader.TestHasSipsUri;
begin
  Self.A.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Self.A.HasSipsUri, 'SIP');

  Self.A.Address.URI := 'sips:wintermute@tessier-ashpool.co.luna';
  Check(Self.A.HasSipsUri, 'SIPS');
end;

procedure TestTIdSipAddressHeader.TestSetAddress;
var
  Addy: TIdSipAddressHeader;
begin
  Addy := TIdSipAddressHeader.Create;
  try
    Addy.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna';
    Self.A.Address := Addy.Address;

    CheckEquals(Addy.Address.URI, Self.A.Address.URI, 'SetAddress');
  finally
    Addy.Free;
  end;
end;

procedure TestTIdSipAddressHeader.TestValue;
begin
  Self.A.Value := 'sip:wintermute@tessier-ashpool.co.luna';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.A.Address.URI,    'Address');
  CheckEquals('',                                     Self.A.DisplayName,    'DisplayName');
  CheckEquals('',                                     Self.A.ParamsAsString, 'Params');
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.A.Value,          'Value');
end;

procedure TestTIdSipAddressHeader.TestValueEmptyDisplayName;
begin
  A.Value := '<sip:wintermute@tessier-ashpool.co.luna>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.A.Address.URI,    'Address');
  CheckEquals('',                                     Self.A.DisplayName,    'DisplayName');
  CheckEquals('',                                     Self.A.ParamsAsString, 'Params');
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.A.Value,          'Value');
end;

procedure TestTIdSipAddressHeader.TestValueFolded;
begin
  Self.A.Value := 'Wintermute'#13#10' <sip:wintermute@tessier-ashpool.co.luna>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',              Self.A.Address.URI,    'Address');
  CheckEquals('Wintermute',                                        Self.A.DisplayName,    'DisplayName');
  CheckEquals('',                                                  Self.A.ParamsAsString, 'Params');
  CheckEquals('Wintermute <sip:wintermute@tessier-ashpool.co.luna>', Self.A.Value,          'Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithBlankQuotedName;
begin
  Self.A.Value := '"" <sip:wintermute@tessier-ashpool.co.luna>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',  Self.A.Address.URI,    'Address');
  CheckEquals('',                                      Self.A.DisplayName,    'DisplayName');
  CheckEquals('',                                      Self.A.ParamsAsString, 'Params');
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',  Self.A.Value,          'Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithEncodings;
begin
  Self.A.Value := '"Count Zero\"" <sip:countzero@jacks-bar.com>';
  CheckEquals('sip:countzero@jacks-bar.com',                  Self.A.Address.URI,    '1: Address');
  CheckEquals('Count Zero"',                                  Self.A.DisplayName,    '1: DisplayName');
  CheckEquals('',                                             Self.A.ParamsAsString, '1: Params');
  CheckEquals('"Count Zero\"" <sip:countzero@jacks-bar.com>', Self.A.Value,          '1: Value');

  Self.A.Value := '"Count\\\" Zero\"\"" <sip:countzero@jacks-bar.com>';
  CheckEquals('sip:countzero@jacks-bar.com', Self.A.Address.URI,    '2: Address');
  CheckEquals('Count\" Zero""',              Self.A.DisplayName,    '2: DisplayName');
  CheckEquals('',                            Self.A.ParamsAsString, '2: Params');
  CheckEquals('"Count\\\" Zero\"\"" <sip:countzero@jacks-bar.com>',
              Self.A.Value,
              '2: Value');

  Self.A.Value := '"\C\o\u\n\t\\\"\ \Z\e\r\o\"\"" <sip:countzero@jacks-bar.com>';
  CheckEquals('sip:countzero@jacks-bar.com', Self.A.Address.URI,    '3: Address');
  CheckEquals('Count\" Zero""',              Self.A.DisplayName,    '3: Name');
  CheckEquals('',                            Self.A.ParamsAsString, '3: Params');
  CheckEquals('"Count\\\" Zero\"\"" <sip:countzero@jacks-bar.com>',
              Self.A.Value,
              '3: Value');

  Self.A.Value := '"Count Zero \\\\\\\"" <sip:countzero@jacks-bar.com>';
  CheckEquals('sip:countzero@jacks-bar.com', Self.A.Address.URI,    '4: Address');
  CheckEquals('Count Zero \\\"',             Self.A.DisplayName,    '4: Name');
  CheckEquals('',                            Self.A.ParamsAsString, '4: Params');
  CheckEquals('"Count Zero \\\\\\\"" <sip:countzero@jacks-bar.com>',
              Self.A.Value,
              '4: Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithMalformedQuotedName;
begin
  try
    // missing close quote
    Self.A.Value := '"Count Zero <sip:countzero@jacks-bar.com>';
    Fail('Failed to bail out because of unmatched quotes #1');
  except
    on EBadHeader do;
  end;

  try
    // missing close quote
    Self.A.Value := '"Count Zero \" <sip:countzero@jacks-bar.com>';
    Fail('Failed to bail out because of unmatched quotes #2');
  except
    on EBadHeader do;
  end;

  try
    // missing close quote
    Self.A.Value := '"Count Zero \\\\\\\" <sip:countzero@jacks-bar.com>';
    Fail('Failed to bail out because of unmatched quotes #3');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipAddressHeader.TestValueWithNormalName;
begin
  Self.A.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',              Self.A.Address.URI,    'Address');
  CheckEquals('Wintermute',                                        Self.A.DisplayName,    'DisplayName');
  CheckEquals('',                                                  Self.A.ParamsAsString, 'Params');
  CheckEquals('Wintermute <sip:wintermute@tessier-ashpool.co.luna>', Self.A.Value,          'Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithNoWhitespaceBetweenDisplayNameAndUri;
begin
  Self.A.Value := '"caller"<sip:caller@example.com>';
  CheckEquals('sip:caller@example.com', Self.A.Address.URI,    'Address');
  CheckEquals('caller',                 Self.A.DisplayName,    'Name');
  CheckEquals('',                       Self.A.ParamsAsString, 'Params');
end;

procedure TestTIdSipAddressHeader.TestValueWithParam;
begin
  Self.A.Value := 'sip:wintermute@tessier-ashpool.co.luna;hidden';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.A.Address.URI,    'Address');
  CheckEquals('',                                     Self.A.DisplayName,    'Name');
  CheckEquals(';hidden',                              Self.A.ParamsAsString, 'Params');
end;

procedure TestTIdSipAddressHeader.TestValueWithQuotedName;
begin
  Self.A.Value := '"Wintermute" <sip:wintermute@tessier-ashpool.co.luna>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',              Self.A.Address.URI,    '1: Address');
  CheckEquals('Wintermute',                                        Self.A.DisplayName,    '1: Name');
  CheckEquals('',                                                  Self.A.ParamsAsString, '1: Params');
  CheckEquals('Wintermute <sip:wintermute@tessier-ashpool.co.luna>', Self.A.Value,          '1: Value');

  Self.A.Value := '"Count Zero" <sip:countzero@jacks-bar.com>';

  CheckEquals('sip:countzero@jacks-bar.com',                Self.A.Address.URI,    '2: Address');
  CheckEquals('Count Zero',                                 Self.A.DisplayName,    '2: Name');
  CheckEquals('',                                           Self.A.ParamsAsString, '2: Params');
  CheckEquals('Count Zero <sip:countzero@jacks-bar.com>',   Self.A.Value,          '2: Value');

end;

procedure TestTIdSipAddressHeader.TestValueWithSpace;
begin
  Self.A.Value := 'Count Zero <sip:countzero@jacks-bar.com>';
  CheckEquals('sip:countzero@jacks-bar.com',              Self.A.Address.URI,    'Address');
  CheckEquals('Count Zero',                               Self.A.DisplayName,    'Name');
  CheckEquals('',                                         Self.A.ParamsAsString, 'Params');
  CheckEquals('Count Zero <sip:countzero@jacks-bar.com>', Self.A.Value,          'Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithSpecialChars;
begin
  Self.A.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna;tag=f00';
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna;tag=f00',   Self.A.Address.URI,    ';:Address');
  CheckEquals('',                                               Self.A.DisplayName,    ';: Name');
  CheckEquals('',                                               Self.A.ParamsAsString, ';: Params');
  CheckEquals('<sip:wintermute@tessier-ashpool.co.luna;tag=f00>', Self.A.Value,          ';: Value');

  Self.A.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna,tag=f00';
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna,tag=f00',   Self.A.Address.URI,    ',:Address');
  CheckEquals('',                                               Self.A.DisplayName,    ',: Name');
  CheckEquals('',                                               Self.A.ParamsAsString, ',: Params');
  CheckEquals('<sip:wintermute@tessier-ashpool.co.luna,tag=f00>', Self.A.Value,          ',: Value');

  Self.A.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna?tag=f00';
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna?tag=f00',   Self.A.Address.URI,    '?:Address');
  CheckEquals('',                                               Self.A.DisplayName,    '?: Name');
  CheckEquals('',                                               Self.A.ParamsAsString, '?: Params');
  CheckEquals('<sip:wintermute@tessier-ashpool.co.luna?tag=f00>', Self.A.Value,          '?: Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithTrailingWhitespacePlusParam;
begin
  Self.A.Value := 'sip:vivekg@chair.dnrc.bell-labs.com ; haha=heehee';
  CheckEquals('sip:vivekg@chair.dnrc.bell-labs.com', Self.A.Address.URI,    'Address');
  CheckEquals('',                                    Self.A.DisplayName,    'DisplayName');
  CheckEquals(';haha=heehee',                        Self.A.ParamsAsString, 'Params');
  CheckEquals('sip:vivekg@chair.dnrc.bell-labs.com', Self.A.Value,          'Value');
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
//* TestTIdSipCallIDHeader                                                     *
//******************************************************************************
//* TestTIdSipCallIDHeader Public methods **************************************

procedure TestTIdSipCallIDHeader.SetUp;
begin
  inherited SetUp;

  C := TIdSipCallIDHeader.Create;
end;

procedure TestTIdSipCallIDHeader.TearDown;
begin
  C.Free;

  inherited TearDown;
end;

//* TestTIdSipCallIDHeader Published methods ***********************************

procedure TestTIdSipCallIDHeader.TestIsEqualTo;
var
  CallID: TIdSipCallIDHeader;
  H:      TIdSipHeader;
begin
  CallID := TIdSipCallIDHeader.Create;
  try
    Self.C.Value := 'fdjhasdfa';

    CallID.Value := Self.C.Value;
    Check(Self.C.IsEqualTo(CallID), 'C = CallID');
    Check(CallID.IsEqualTo(Self.C), 'CallID = C');

    CallID.Value := Uppercase(Self.C.Value);
    Check(not Self.C.IsEqualTo(CallID), 'C <> CallID, case-sensitive');
    Check(not CallID.IsEqualTo(Self.C), 'CallID <> C, case-sensitive');

    CallID.Value := Self.C.Value + Self.C.Value;
    Check(not Self.C.IsEqualTo(CallID), 'C <> CallID, different value');
    Check(not CallID.IsEqualTo(Self.C), 'CallID <> C, different value');
  finally
    CallID.Free;
  end;

  H := TIdSipHeader.Create;
  try
    H.Name := Self.C.Name;
    H.Value := Self.C.Value;

    Check(H.IsEqualTo(Self.C), 'H = C');
    Check(H.IsEqualTo(Self.C), 'C = H');
  finally
    H.Free;
  end;
end;

procedure TestTIdSipCallIDHeader.TestValue;
begin
  Self.C.Value := 'fdjhasdfa';
  CheckEquals('fdjhasdfa', Self.C.Value, 'fdjhasdfa');
  Self.C.Value := 'fdjhasdfa@sda';
  CheckEquals('fdjhasdfa@sda', Self.C.Value, 'fdjhasdfa@sda');

  try
    Self.C.Value := '';
    Fail('Failed to bail out on empty string');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := 'aaaaaaaaaaaaaaaa;';
    Fail('Failed to bail out on non-word');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := 'aaaaaaaa@@bbbbb';
    Fail('Failed to bail out optional non-word');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipCallIDHeader.TestValueWithParams;
begin
  try
    Self.C.Value := 'one@two;tag=f00';
    Fail('Failed to bail out with params - semicolon is an invalid character');
  except
    on EBadHeader do;
  end;
end;

//******************************************************************************
//* TestTIdSipCommaSeparatedHeader                                             *
//******************************************************************************
//* TestTIdSipCommaSeparatedHeader Public methods ******************************

procedure TestTIdSipCommaSeparatedHeader.SetUp;
begin
  inherited SetUp;

  Self.C := TIdSipCommaSeparatedHeader.Create;
end;

procedure TestTIdSipCommaSeparatedHeader.TearDown;
begin
  Self.C.Free;

  inherited TearDown;
end;

//* TestTIdSipCommaSeparatedHeader Published methods ***************************

procedure TestTIdSipCommaSeparatedHeader.TestValue;
begin
  Self.C.Name := ContentLanguageHeader;

  Self.C.Value := '';
  CheckEquals(0, Self.C.Values.Count, 'Empty value');
  CheckEquals(Self.C.Name + ': ',
              Self.C.AsString,
              'Empty value AsString');

  Self.C.Value := 'a';
  CheckEquals(1, Self.C.Values.Count, 'a');
  CheckEquals('a', Self.C.Values[0], '1: First value');
  CheckEquals(Self.C.Name + ': a',
              Self.C.AsString,
              '''a'' AsString');

  Self.C.Value := 'a b';
  CheckEquals(1, Self.C.Values.Count, 'a b');
  CheckEquals('a b', Self.C.Values[0], '2: First value');
  CheckEquals(Self.C.Name + ': a b',
              Self.C.AsString,
              '''a b'' AsString');

  Self.C.Value := 'a,b';
  CheckEquals(2, Self.C.Values.Count, 'a,b');
  CheckEquals('a', Self.C.Values[0], '3: First value');
  CheckEquals('b', Self.C.Values[1], '3: Second value');
  CheckEquals(Self.C.Name + ': a, b',
              Self.C.AsString,
              '''a,b'' AsString');

  Self.C.Value := 'a, b';
  CheckEquals(2, Self.C.Values.Count, 'a, b');
  CheckEquals('a', Self.C.Values[0], '4: First value');
  CheckEquals('b', Self.C.Values[1], '4: Second value');
  CheckEquals(Self.C.Name + ': a, b',
              Self.C.AsString,
              '''a, b'' AsString');

  Self.C.Value := 'a;q=0.1, b;q=1';
  CheckEquals(2, Self.C.Values.Count, 'a;q=0.1, b;q=1');
  CheckEquals('a;q=0.1', Self.C.Values[0], '5: First value');
  CheckEquals('b;q=1', Self.C.Values[1],   '5: Second value');
  CheckEquals(Self.C.Name + ': a;q=0.1, b;q=1',
              Self.C.AsString,
              '''a;q=0.1, b;q=1'' AsString');
end;

//******************************************************************************
//* TestTIdSipContactHeader                                                    *
//******************************************************************************
//* TestTIdSipContactHeader Public methods *************************************

procedure TestTIdSipContactHeader.SetUp;
begin
  inherited SetUp;

  Self.C := TIdSipContactHeader.Create;
end;

procedure TestTIdSipContactHeader.TearDown;
begin
  Self.C.Free;

  inherited TearDown;
end;

//* TestTIdSipContactHeader Published methods **********************************

procedure TestTIdSipContactHeader.TestName;
begin
  CheckEquals(ContactHeaderFull, Self.C.Name, 'Name');

  Self.C.Name := 'foo';
  CheckEquals(ContactHeaderFull, Self.C.Name, 'Name after set');
end;

procedure TestTIdSipContactHeader.TestGetSetExpires;
begin
  Self.C.Expires := 0;
  CheckEquals(0, Self.C.Expires, '0');

  Self.C.Expires := 666;
  CheckEquals(666, Self.C.Expires, '666');
end;

procedure TestTIdSipContactHeader.TestGetSetQ;
begin
  Self.C.Q := 0;
  CheckEquals(0, Self.C.Q, '0');

  Self.C.Q := 666;
  CheckEquals(666, Self.C.Q, '666');
end;

procedure TestTIdSipContactHeader.TestValueWithExpires;
begin
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;expires=0';
  CheckEquals(0, C.Expires, 'expires=0');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;expires=666';
  CheckEquals(666, C.Expires, 'expires=666');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;expires=65536';
  CheckEquals(65536, C.Expires, 'expires=65536');

  try
    Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;expires=a';
    Fail('Failed to bail out with letters');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;expires=-1';
    Fail('Failed to bail out with negative number');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;expires=';
    Fail('Failed to bail out with empty string');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipContactHeader.TestValueWithQ;
begin
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.C.Address.URI,    'Address');
  CheckEquals('',                                     Self.C.DisplayName,    'DisplayName');
  CheckEquals('',                                     Self.C.ParamsAsString, 'Params');
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.C.Value,          'Value');
  Check(                                              not Self.C.IsWildCard, 'IsWild');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=0';
  CheckEquals(0, C.Q, 'q=0');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=0.0';
  CheckEquals(0, C.Q, 'q=0.0');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=0.00';
  CheckEquals(0, C.Q, 'q=0.00');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=0.000';
  CheckEquals(0, C.Q, 'q=0.000');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=0.123';
  CheckEquals(123, C.Q, 'q=0.123');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=0.666';
  CheckEquals(666, C.Q, 'q=0.666');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=1';
  CheckEquals(1000, C.Q, 'q=1');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=1.0';
  CheckEquals(1000, C.Q, 'q=1.0');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=1.00';
  CheckEquals(1000, C.Q, 'q=1.00');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=1.000';
  CheckEquals(1000, C.Q, 'q=1.000');

  try
    Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=';
    Fail('Failed to bail out on empty string');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=a';
    Fail('Failed to bail out on letters');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=0.1234';
    Fail('Failed to bail out on too many digits');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=1.1';
    Fail('Failed to bail out on number too big');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipContactHeader.TestValueWithStar;
begin
  Self.C.Value := '*';
  Check(Self.C.IsWildCard, 'IsWildCard');

  Self.C.Value := '*;q=0.1';
  Check(Self.C.IsWildCard, 'IsWildCard');
  CheckEquals(100, Self.C.Q, 'QValue');
end;

//******************************************************************************
//* TestTIdSipContentDispositionHeader                                         *
//******************************************************************************
//* TestTIdSipContentDispositionHeader Public methods **************************

procedure TestTIdSipContentDispositionHeader.SetUp;
begin
  inherited SetUp;

  Self.C := TIdSipContentDispositionHeader.Create;
end;

procedure TestTIdSipContentDispositionHeader.TearDown;
begin
  Self.C.Free;

  inherited TearDown;
end;

//* TestTIdSipContentDispositionHeader Published methods ***********************

procedure TestTIdSipContentDispositionHeader.TestName;
begin
  CheckEquals(ContentDispositionHeader, Self.C.Name, 'Name');

  Self.C.Name := 'foo';
  CheckEquals(ContentDispositionHeader, Self.C.Name, 'Name after set');
end;

procedure TestTIdSipContentDispositionHeader.TestGetSetHandling;
begin
  Self.C.Handling := HandlingRequired;
  CheckEquals(HandlingRequired, Self.C.Handling, HandlingRequired);

  Self.C.Handling := HandlingOptional;
  CheckEquals(HandlingOptional, Self.C.Handling, HandlingOptional);
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

procedure TestTIdSipCSeqHeader.TestValue;
begin
  Self.C.Value := '1 INVITE';
  CheckEquals(1,        Self.C.SequenceNo, 'SequenceNo');
  CheckEquals('INVITE', Self.C.Method,     'Method');

  Self.C.Value := '1  INVITE';
  CheckEquals(1,        Self.C.SequenceNo, '2: SequenceNo');
  CheckEquals('INVITE', Self.C.Method,     '2: Method');

  Self.C.Value := '1'#13#10'  INVITE';
  CheckEquals(1,        Self.C.SequenceNo, '3: SequenceNo');
  CheckEquals('INVITE', Self.C.Method,     '3: Method');

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

procedure TestTIdSipDateHeader.TestName;
begin
  CheckEquals(DateHeader, Self.D.Name, 'Name');

  Self.D.Name := 'foo';
  CheckEquals(DateHeader, Self.D.Name, 'Name after set');
end;

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
//* TestTIdSipFromToHeader                                                     *
//******************************************************************************
//* TestTIdSipFromToHeader Public methods **************************************

procedure TestTIdSipFromToHeader.SetUp;
begin
  inherited SetUp;

  Self.F := TIdSipFromToHeader.Create;
end;

procedure TestTIdSipFromToHeader.TearDown;
begin
  Self.F.Free;

  inherited TearDown;
end;

//* TestTIdSipFromToHeader Published methods ***********************************

procedure TestTIdSipFromToHeader.TestHasTag;
begin
  Self.F.Value := 'Case <sip:case@fried.neurons.org>';
  Check(not Self.F.HasTag, 'No tag');

  Self.F.Tag := BranchMagicCookie + 'f00';
  Check(Self.F.HasTag, 'Tag added via Tag property');

  Self.F.Value := 'Case <sip:case@fried.neurons.org>';
  Check(not Self.F.HasTag, 'No tag sanity check');
  Self.F.Value := 'Case <sip:case@fried.neurons.org>;' + TagParam + '=f00';
  Check(Self.F.HasTag, 'Tag added via SetValue');
end;

procedure TestTIdSipFromToHeader.TestIsEqualDifferentURI;
var
  From: TIdSipFromToHeader;
begin
  Self.F.Value := 'Case <sip:case@fried.neurons.org>';

  From := TIdSipFromToHeader.Create;
  try
    From.Value := 'sip:wintermute@tessier-ashpool.co.luna';
    Check(not Self.F.IsEqualTo(From), 'different URI');

    From.Value := 'sips:case@fried.neurons.org';
    Check(not Self.F.IsEqualTo(From), 'same user but different scheme; hence different URI');
  finally
    From.Free;
  end;
end;

procedure TestTIdSipFromToHeader.TestIsEqualSameURINoParams;
var
  From: TIdSipFromToHeader;
begin
  Self.F.Value := 'Case <sip:case@fried.neurons.org>';

  From := TIdSipFromToHeader.Create;
  try
    From.Value := 'Case <sip:case@fried.neurons.org>';
    Check(Self.F.IsEqualTo(From), 'Identical headers');

    From.Value := '"Caseless Ammo" <sip:case@fried.neurons.org>';
    Check(Self.F.IsEqualTo(From), 'Different display names');

    From.Value := 'sip:case@fried.neurons.org';
    Check(Self.F.IsEqualTo(From), 'No display name');

    From.Value := 'sip:case@fried.neurons.org;tag=1';
    Check(not Self.F.IsEqualTo(From), 'One has a tag, the other not');
  finally
    From.Free;
  end;
end;

procedure TestTIdSipFromToHeader.TestIsEqualSameURIWithParams;
var
  From: TIdSipFromToHeader;
begin
  Self.F.Value := 'sip:case@fried.neurons.org;tag=1234';

  From := TIdSipFromToHeader.Create;
  try
    From.Value := 'sip:case@fried.neurons.org;x-extend=00;tag=1234';
    Check(Self.F.IsEqualTo(From), 'No display name, extension param');

    From.Value := 'sip:case@fried.neurons.org;tag=1234;x-extend=00';
    Check(Self.F.IsEqualTo(From),
          'No display name, extension param; order is irrelevant');

    From.Value := 'sip:case@fried.neurons.org;tag=1235';
    Check(not Self.F.IsEqualTo(From), 'different tags');
  finally
    From.Free;
  end;
end;

procedure TestTIdSipFromToHeader.TestValueWithTag;
begin
  Self.F.Value := 'Case <sip:case@fried.neurons.org>';
  CheckEquals('', Self.F.Tag, '''''');

  Self.F.Value := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
  CheckEquals('1928301774', Self.F.Tag, '1928301774');

  try
    Self.F.Value := 'Case <sip:case@fried.neurons.org>;tag=19283@01774';
    Fail('Failed to bail out with malformed token');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipFromToHeader.TestValueResettingTag;
begin
  Self.F.Value := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
  CheckEquals('1928301774', Self.F.Tag, '1928301774');

  Self.F.Value := 'Case <sip:case@fried.neurons.org>';
  CheckEquals('', Self.F.Tag, '''''');
end;

procedure TestTIdSipFromToHeader.TestGetSetTag;
begin
  Self.F.Tag := '123';
  CheckEquals('123', Self.F.Tag, '123');

  Self.F.Tag := '123abc';
  CheckEquals('123abc', Self.F.Tag, '123abc');

  Self.F.Tag := '';
  Check(not Self.F.HasTag, 'Tag wasn''t removed');

  try
    Self.F.Tag := '19283@01774';
    Fail('Failed to bail out with malformed token');
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

procedure TestTIdSipMaxForwardsHeader.TestName;
begin
  CheckEquals(MaxForwardsHeader, Self.M.Name, 'Name');

  Self.M.Name := 'foo';
  CheckEquals(MaxForwardsHeader, Self.M.Name, 'Name after set');
end;

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
//* TestTIdSipRouteHeader                                                      *
//******************************************************************************
//* TestTIdSipRouteHeader Public methods ***************************************

procedure TestTIdSipRouteHeader.SetUp;
begin
  inherited SetUp;

  Self.R := TIdSipRouteHeader.Create;
end;

procedure TestTIdSipRouteHeader.TearDown;
begin
  Self.R.Free;

  inherited TearDown;
end;

//* TestTIdSipRouteHeader Published methods ************************************

procedure TestTIdSipRouteHeader.TestIsLooseRoutable;
begin
  Self.R.Value := '<sip:127.0.0.1>';
  Check(not Self.R.IsLooseRoutable, 'With no lr param');

  Self.R.Value := '<sip:127.0.0.1;lr>';
  Check(Self.R.IsLooseRoutable, 'With lr param');

  Self.R.Value := '<sip:127.0.0.1>;lr';
  Check(not Self.R.IsLooseRoutable, 'With no lr param for header, not URI');
end;

procedure TestTIdSipRouteHeader.TestName;
begin
  CheckEquals(RouteHeader, Self.R.Name, 'Name');

  Self.R.Name := 'foo';
  CheckEquals(RouteHeader, Self.R.Name, 'Name after set');
end;

procedure TestTIdSipRouteHeader.TestValue;
begin
  Self.R.Value := '<sip:127.0.0.1>';
  CheckEquals('sip:127.0.0.1', Self.R.Address.URI, 'Address');
  CheckEquals('',              Self.R.DisplayName, 'DisplayName');

  Self.R.Value := 'localhost <sip:127.0.0.1>';
  CheckEquals('sip:127.0.0.1', Self.R.Address.URI, 'Address');
  CheckEquals('localhost',     Self.R.DisplayName, 'DisplayName');

  try
    Self.R.Value := '';
    Fail('Failed to bail on empty string');
  except
    on EBadHeader do;
  end;

  try
    Self.R.Value := 'sip:127.0.0.1';
    Fail('Failed to bail on lack of angle brackets');
  except
    on EBadHeader do;
  end;

  try
    Self.R.Value := '<127.0.0.1>';
    Fail('Failed to bail on no scheme');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipRouteHeader.TestValueWithParamsAndHeaderParams;
begin
  Self.R.Value := 'Count Zero <sip:countzero@jacks-bar.com;paranoid>;very';

  CheckEquals('Count Zero', Self.R.DisplayName, 'DisplayName');
  CheckEquals('sip:countzero@jacks-bar.com;paranoid',
              Self.R.Address.URI,
              'Address');
  CheckEquals(';very', Self.R.ParamsAsString, 'Header parameters');
end;

//******************************************************************************
//* TestTIdSipRecordRouteHeader                                                *
//******************************************************************************
//* TestTIdSipRecordRouteHeader Public methods *********************************

procedure TestTIdSipRecordRouteHeader.SetUp;
begin
  inherited SetUp;

  Self.R := TIdSipRecordRouteHeader.Create;
end;

procedure TestTIdSipRecordRouteHeader.TearDown;
begin
  Self.R.Free;

  inherited TearDown;
end;

//* TestTIdSipRecordRouteHeader Published methods ******************************

procedure TestTIdSipRecordRouteHeader.TestName;
begin
  CheckEquals(RecordRouteHeader, Self.R.Name, 'Name');

  Self.R.Name := 'foo';
  CheckEquals(RecordRouteHeader, Self.R.Name, 'Name after set');
end;

procedure TestTIdSipRecordRouteHeader.TestValue;
begin
  Self.R.Value := '<sip:127.0.0.1>';
  CheckEquals('sip:127.0.0.1', Self.R.Address.URI, 'Address');
  CheckEquals('',              Self.R.DisplayName, 'DisplayName');

  Self.R.Value := 'localhost <sip:127.0.0.1>';
  CheckEquals('sip:127.0.0.1', Self.R.Address.URI, 'Address');
  CheckEquals('localhost',     Self.R.DisplayName, 'DisplayName');

  try
    Self.R.Value := '';
    Fail('Failed to bail on empty string');
  except
    on EBadHeader do;
  end;

  try
    Self.R.Value := '127.0.0.1';
    Fail('Failed to bail on lack of angle brackets');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipRecordRouteHeader.TestValueWithParamsAndHeaderParams;
begin
  Self.R.Value := 'Count Zero <sip:countzero@jacks-bar.com;paranoid>;very';

  CheckEquals('Count Zero', Self.R.DisplayName, 'DisplayName');
  CheckEquals('sip:countzero@jacks-bar.com;paranoid',
              Self.R.Address.URI,
              'Address');
  CheckEquals(';very', Self.R.ParamsAsString, 'Header parameters');
end;

//******************************************************************************
//* TestTIdSipTimestampHeader                                                  *
//******************************************************************************
//* TestTIdSipTimestampHeader Public methods ***********************************

procedure TestTIdSipTimestampHeader.SetUp;
begin
  inherited SetUp;

  Self.T := TIdSipTimestampHeader.Create;
end;

procedure TestTIdSipTimestampHeader.TearDown;
begin
  Self.T.Free;

  inherited TearDown;
end;

//* TestTIdSipTimestampHeader Published methods ********************************

procedure TestTIdSipTimestampHeader.TestName;
begin
  CheckEquals(TimestampHeader, Self.T.Name, 'Name');

  Self.T.Name := 'foo';
  CheckEquals(TimestampHeader, Self.T.Name, 'Name after set');
end;

procedure TestTIdSipTimestampHeader.TestNormalizeLWS;
begin
  CheckEquals('',       Self.T.NormalizeLWS(''),             '''''');
  CheckEquals('hello',  Self.T.NormalizeLWS('hello'),        'hello');
  CheckEquals('a b',    Self.T.NormalizeLWS('a b'),          'a b');
  CheckEquals('a b c',  Self.T.NormalizeLWS('a b  c'),       'a b  c');
  CheckEquals('a b',    Self.T.NormalizeLWS('a'#9'b'),       'a TAB b');
  CheckEquals('a b',    Self.T.NormalizeLWS('a'#13#10'  b'), 'a CRLF SP SP b');
end;

procedure TestTIdSipTimestampHeader.TestReadNumber;
var
  Src: String;
begin
  Src := '1';
  CheckEquals(1, Self.T.ReadNumber(Src), '1');
  CheckEquals('', Src, 'Src after ''1''');

  Src := '123 ';
  CheckEquals(123, Self.T.ReadNumber(Src), '123 SP');
  CheckEquals(' ', Src, 'Src after ''123 ''');

  Src := '456.';
  CheckEquals(456, Self.T.ReadNumber(Src), '456.');
  CheckEquals('.', Src, 'Src after ''456.''');

  try
    Src := '';
    Self.T.ReadNumber(Src);
    Fail('Failed to bail out on empty string');
  except
    on EBadHeader do;
  end;

  try
    Src := 'a';
    Self.T.ReadNumber(Src);
    Fail('Failed to bail out on non-number');
  except
    on EBadHeader do;
  end;

  try
    Src := 'a 1';
    Self.T.ReadNumber(Src);
    Fail('Failed to bail out on non-number, SP, number');
  except
    on EBadHeader do;
  end;

  try
    Src := '-66';
    Self.T.ReadNumber(Src);
    Fail('Failed to bail out on negative number (hence non-number)');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipTimestampHeader.TestValue;
begin
  Self.T.Value := '1';
  CheckEquals(1, Self.T.Timestamp.IntegerPart,    '1: Timestamp.IntegerPart');
  CheckEquals(0, Self.T.Timestamp.FractionalPart, '1: Timestamp.FractionalPart');
  CheckEquals(0, Self.T.Delay.IntegerPart,        '1: Delay.IntegerPart');
  CheckEquals(0, Self.T.Delay.FractionalPart,     '1: Delay.FractionalPart');

  Self.T.Value := '99.';
  CheckEquals(99, Self.T.Timestamp.IntegerPart,    '2: Timestamp.IntegerPart');
  CheckEquals(0,  Self.T.Timestamp.FractionalPart, '2: Timestamp.FractionalPart');
  CheckEquals(0,  Self.T.Delay.IntegerPart,        '2: Delay.IntegerPart');
  CheckEquals(0,  Self.T.Delay.FractionalPart,     '2: Delay.FractionalPart');

  Self.T.Value := '2.2';
  CheckEquals(2, Self.T.Timestamp.IntegerPart,    '3: Timestamp.IntegerPart');
  CheckEquals(2, Self.T.Timestamp.FractionalPart, '3: Timestamp.FractionalPart');
  CheckEquals(0, Self.T.Delay.IntegerPart,        '3: Delay.IntegerPart');
  CheckEquals(0, Self.T.Delay.FractionalPart,     '3: Delay.FractionalPart');
end;

procedure TestTIdSipTimestampHeader.TestValueMalformed;
begin
  try
    Self.T.Value := 'a';
    Fail('Failed to bail out on non-integer');
  except
    on EBadHeader do;
  end;

  try
    Self.T.Value := '1..1';
    Fail('Failed to bail out on too many periods');
  except
    on EBadHeader do;
  end;

  try
    Self.T.Value := '.1';
    Fail('Failed to bail out on no digits before period');
  except
    on EBadHeader do;
  end;

  try
    Self.T.Value := '1 a';
    Fail('Failed to bail out on malformed delay');
  except
    on EBadHeader do;
  end;

  try
    Self.T.Value := '1 1.1;tag';
    Fail('Failed to bail out on params');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipTimestampHeader.TestValueWithDelay;
begin
  Self.T.Value := '5.1 3';
  CheckEquals(5, Self.T.Timestamp.IntegerPart,    '1: Timestamp.IntegerPart');
  CheckEquals(1, Self.T.Timestamp.FractionalPart, '1: Timestamp.FractionalPart');
  CheckEquals(3, Self.T.Delay.IntegerPart,        '1: Delay.IntegerPart');
  CheckEquals(0, Self.T.Delay.FractionalPart,     '1: Delay.FractionalPart');

  Self.T.Value := '1.2 3.4';
  CheckEquals(1, Self.T.Timestamp.IntegerPart,    '2: Timestamp.IntegerPart');
  CheckEquals(2, Self.T.Timestamp.FractionalPart, '2: Timestamp.FractionalPart');
  CheckEquals(3, Self.T.Delay.IntegerPart,        '2: Delay.IntegerPart');
  CheckEquals(4, Self.T.Delay.FractionalPart,     '2: Delay.FractionalPart');

  Self.T.Value := '6.5 .4';
  CheckEquals(6, Self.T.Timestamp.IntegerPart,    '3: Timestamp.IntegerPart');
  CheckEquals(5, Self.T.Timestamp.FractionalPart, '3: Timestamp.FractionalPart');
  CheckEquals(0, Self.T.Delay.IntegerPart,        '3: Delay.IntegerPart');
  CheckEquals(4, Self.T.Delay.FractionalPart,     '3: Delay.FractionalPart');
end;

//******************************************************************************
//* TestTIdSipUriHeader                                                        *
//******************************************************************************
//* TestTIdSipUriHeader Public methods *****************************************

procedure TestTIdSipUriHeader.SetUp;
begin
  inherited SetUp;

  Self.U := TIdSipUriHeader.Create;
end;

procedure TestTIdSipUriHeader.TearDown;
begin
  Self.U.Free;

  inherited TearDown;
end;

//* TestTIdSipUriHeader Published methods **************************************

procedure TestTIdSipUriHeader.TestValue;
begin
  Self.U.Value := '<sip:case@jacks-bar.com>';
  CheckEquals('sip:case@jacks-bar.com',
              Self.U.Address.URI,
              'Address.GetFullURI');

  try
    Self.U.Value := '';
    Fail('Empty string');
  except
    on EBadHeader do;
  end;

  try
    Self.U.Value := 'sip:case@jacks-bar.com';
    Fail('un <>''d URI');
  except
    on EBadHeader do;
  end;

  try
    Self.U.Value := 'Case <sip:case@jacks-bar.com>';
    Fail('No display names allowed');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipUriHeader.TestValueWithParams;
begin
  Self.U.Value := '<sip:case@jacks-bar.com>;value=name';
  CheckEquals(1,             Self.U.ParamCount,     'ParamCount');
  CheckEquals(';value=name', Self.U.ParamsAsString, 'ParamsAsString');
end;

procedure TestTIdSipUriHeader.TestValueWithUriParams;
begin
  Self.U.Value := '<sip:case@jacks-bar.com;value=name>';
  CheckEquals(0, Self.U.ParamCount, 'ParamCount');
  CheckEquals('sip:case@jacks-bar.com;value=name',
              Self.U.Address.URI,
              'Address');
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

procedure TestTIdSipViaHeader.TestAssign;
var
  V2: TIdSipViaHeader;
begin
  V2 := TIdSipViaHeader.Create;
  try
    Self.V.Value := 'SIP/7.0/SCTP localhost;branch=' + BranchMagicCookie + 'f00';
    V2.Assign(Self.V);
    Check(V2.IsEqualTo(Self.V), 'V2 not properly assigned to');
  finally
    V2.Free;
  end;
end;

procedure TestTIdSipViaHeader.TestAssignFromBadlyFormedVia;
var
  V2: TIdSipViaHeader;
begin
  V2 := TIdSipViaHeader.Create;
  try
    Self.V.Branch := BranchMagicCookie + 'f00';
    V2.Assign(Self.V);
    Check(V2.IsEqualTo(Self.V), 'V2 not properly assigned to');
    CheckEquals(Self.V.Branch, V2.Branch, 'Branch');
  finally
    V2.Free;
  end;
end;

procedure TestTIdSipViaHeader.TestBranch;
begin
  Self.V.Branch := BranchMagicCookie;
  CheckEquals(BranchMagicCookie, Self.V.Branch, BranchMagicCookie);

  Self.V.Branch := BranchMagicCookie + 'abcdef';
  CheckEquals(BranchMagicCookie + 'abcdef', Self.V.Branch, BranchMagicCookie + 'abcdef');

  try
    Self.V.Branch := '';
    Fail('Failed to bail out on an invalid branch - empty string');
  except
    on EBadHeader do;
  end;

  try
    Self.V.Branch := 'I am not a valid branch';
    Fail('Failed to bail out on an invalid branch - multiple tokens');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipViaHeader.TestHasBranch;
begin
  Check(not Self.V.HasBranch, 'No branch should be assigned on creation');

  Self.V.Branch := BranchMagicCookie + 'f00';
  Check(Self.V.HasBranch, 'Branch should have been set');
end;

procedure TestTIdSipViaHeader.TestHasMaddr;
begin
  Check(not Self.V.HasMaddr, 'No maddr should be assigned on creation');

  Self.V.Maddr := '127.0.0.1';
  Check(Self.V.HasMaddr, 'Maddr should have been set');
end;

procedure TestTIdSipViaHeader.TestHasReceived;
begin
  Check(not Self.V.HasReceived, 'No Received should be assigned on creation');

  Self.V.Received := '127.0.0.1';
  Check(Self.V.HasReceived, 'Received should have been set');
end;

procedure TestTIdSipViaHeader.TestHasRport;
begin
  Check(not Self.V.HasRport, 'No rport should be assigned on creation');

  Self.V.Rport := 1024;
  Check(Self.V.HasRport, 'Rport should have been set');
end;

procedure TestTIdSipViaHeader.TestIsRFC3261Branch;
begin
  Self.V.Value := 'SIP/2.0/TCP gw1.leo-ix.org';
  Check(not Self.V.IsRFC3261Branch, 'No branch');

  Self.V.Branch := 'z9hG4b';
  Check(not Self.V.IsRFC3261Branch, 'z9hG4b');

  Self.V.Branch := BranchMagicCookie;
  Check(Self.V.IsRFC3261Branch, 'RFC 3261 magic cookie');

  Self.V.Branch := BranchMagicCookie + '1234';
  Check(Self.V.IsRFC3261Branch, 'RFC 3261 magic cookie + ''1234''');
end;

procedure TestTIdSipViaHeader.TestIsEqualTo;
var
  Hop2: TIdSipViaHeader;
begin
  Hop2 := TIdSipViaHeader.Create;
  try
    Self.V.SentBy     := '127.0.0.1';
    Self.V.Port       := 5060;
    Self.V.SipVersion := 'SIP/2.0';
    Self.V.Transport  := sttSCTP;

    Hop2.SentBy     := '127.0.0.1';
    Hop2.Port       := 5060;
    Hop2.SipVersion := 'SIP/2.0';
    Hop2.Transport  := sttSCTP;

    Check(V.IsEqualTo(Hop2), 'V.IsEqualTo(Hop2)');
    Check(Hop2.IsEqualTo(V), 'Hop2.IsEqualTo(V)');

    Self.V.SentBy := '127.0.0.2';
    Check(not Self.V.IsEqualTo(Hop2), 'V.IsEqualTo(Hop2); Host');
    Check(not Hop2.IsEqualTo(Self.V), 'Hop2.IsEqualTo(V); Host');
    Self.V.SentBy := '127.0.0.1';
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

procedure TestTIdSipViaHeader.TestIsEqualToBranchIsCaseInsensitive;
var
  Via: TIdSipViaHeader;
begin
  Via := TIdSipViaHeader.Create;
  try
    Self.V.Value := 'SIP/2.0/TCP gw1.leo-ix.net;branch=z9hG4bKdecafbad';
    Via.Value := Self.V.Value + Self.V.ParamsAsString;

    Via.Branch := Uppercase(Via.Branch);

    Check(Self.V.IsEqualTo(Via), 'V = Via');
    Check(Via.IsEqualTo(Self.V), 'Via = V');
  finally
    Via.Free;
  end;
end;

procedure TestTIdSipViaHeader.TestMaddr;
begin
  Self.V.Maddr := '1.2.3.4';
  CheckEquals('1.2.3.4', Self.V.Maddr, 'IPv4 address');

  Self.V.Maddr := '[fe80::201:2ff:fef0]';
  CheckEquals('[fe80::201:2ff:fef0]', Self.V.Maddr, 'IPv6 reference');

  Self.V.Maddr := 'one.two.three.four';
  CheckEquals('one.two.three.four', Self.V.Maddr, 'FQDN');

  try
    Self.V.Maddr := 'fe80::201:2ff:fef0';
    Fail('Failed to bail out on IPv6 address');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipViaHeader.TestName;
begin
  CheckEquals(ViaHeaderFull, Self.V.Name, 'Name');

  Self.V.Name := 'foo';
  CheckEquals(ViaHeaderFull, Self.V.Name, 'Name after set');
end;

procedure TestTIdSipViaHeader.TestReceived;
begin
  Self.V.Received := '1.2.3.4';
  CheckEquals('1.2.3.4', Self.V.Received, 'IPv4 address');

  Self.V.Received := 'fe80::201:2ff:fef0';
  CheckEquals('fe80::201:2ff:fef0', Self.V.Received, 'IPv6 address');

  try
    Self.V.Received := 'one.two.three.four';
    Fail('Failed to bail out on FQDN');
  except
    on EBadHeader do;
  end;

  try
    Self.V.Received := 'fjksahflkh fasdf';
    Fail('Failed to bail out on nonsense');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipViaHeader.TestTTL;
begin
  Self.V.TTL := 0;
  CheckEquals(0, V.TTL, 'TTL set to 0');

  Self.V.TTL := 255;
  CheckEquals(255, V.TTL, 'TTL set to 255');
end;

procedure TestTIdSipViaHeader.TestValue;
begin
  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;tag=heehee';
  CheckEquals('127.0.0.1',   Self.V.SentBy,         '1: SentBy');
  CheckEquals(';tag=heehee', Self.V.ParamsAsString, '1: Parameters');
  CheckEquals(IdPORT_SIP,    Self.V.Port,           '1: Port');
  CheckEquals('SIP/1.5',     Self.V.SipVersion,     '1: SipVersion');
  Check      (sttUDP =       Self.V.Transport,      '1: Transport');

  Self.V.Value := 'SIP/1.5/TLS 127.0.0.1';
  CheckEquals('127.0.0.1',     Self.V.SentBy,         '2: SentBy');
  CheckEquals('',              Self.V.ParamsAsString, '2: Parameters');
  CheckEquals(IdPORT_SIPS,     Self.V.Port,           '2: Port');
  CheckEquals('SIP/1.5',       Self.V.SipVersion,     '2: SipVersion');
  Check      (sttTLS =         Self.V.Transport,      '2: Transport');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1:666;tag=heehee';
  CheckEquals('127.0.0.1',   Self.V.SentBy,         '3: SentBy');
  CheckEquals(';tag=heehee', Self.V.ParamsAsString, '3: Parameters');
  CheckEquals(666,           Self.V.Port,           '3: Port');
  CheckEquals('SIP/1.5',     Self.V.SipVersion,     '3: SipVersion');
  Check      (sttUDP =       Self.V.Transport,      '3: Transport');

  Self.V.Value := 'SIP/1.5/TLS 127.0.0.1:666;haha=heehee';
  CheckEquals('127.0.0.1',     Self.V.SentBy,         '4: SentBy');
  CheckEquals(';haha=heehee',  Self.V.ParamsAsString, '4: Parameters');
  CheckEquals(666,             Self.V.Port,           '4: Port');
  CheckEquals('SIP/1.5',       Self.V.SipVersion,     '4: SipVersion');
  Check      (sttTLS =         Self.V.Transport,      '4: Transport');

  Self.V.Value := 'SIP/1.5/TLS 127.0.0.1:666 '#13#10' ; haha=heehee';
  CheckEquals('127.0.0.1',     Self.V.SentBy,         '5: SentBy');
  CheckEquals(';haha=heehee',  Self.V.ParamsAsString, '5: Parameters');
  CheckEquals(666,             Self.V.Port,           '5: Port');
  CheckEquals('SIP/1.5',       Self.V.SipVersion,     '5: SipVersion');
  Check      (sttTLS =         Self.V.Transport,      '5: Transport');
end;

procedure TestTIdSipViaHeader.TestValueWithBranch;
begin
  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;tag=heehee';
  CheckEquals('', Self.V.Branch, 'No Branch parameter');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;branch=1.2.3.4';
  CheckEquals('1.2.3.4', Self.V.Branch, 'IPv4 address');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;branch=' + BranchMagicCookie + '1234';
  CheckEquals(BranchMagicCookie + '1234', Self.V.Branch, BranchMagicCookie + '1234');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;branch=www.google.com';
  CheckEquals('www.google.com', Self.V.Branch, 'FQDN');

  try
    Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;branch=';
    Fail('Failed to bail out with empty string');
  except
    on EBadHeader do;
  end;

  try
    Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;branch=one two';
    Fail('Failed to bail out with multiple tokens');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipViaHeader.TestValueWithMaddr;
begin
  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;tag=heehee';
  CheckEquals('', Self.V.Maddr, 'No maddr parameter');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;maddr=1.2.3.4';
  CheckEquals('1.2.3.4', Self.V.Maddr, 'IPv4 address');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;maddr=[fe80::201:2ff:fef0]';
  CheckEquals('[fe80::201:2ff:fef0]', Self.V.Maddr, 'IPv6 reference');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;maddr=www.google.com';
  CheckEquals('www.google.com', Self.V.Maddr, 'FQDN');

  try
    Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;maddr=fe80::201:2ff:fef0';
    Fail('Failed to bail out with IPv6 address');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipViaHeader.TestValueWithReceived;
begin
  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;tag=heehee';
  CheckEquals('', Self.V.Received,  'Received value when not present');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;received=4.3.2.1';
  CheckEquals('4.3.2.1', Self.V.Received, 'IPv4 address');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;received=fe80::201:2ff:fef0';
  CheckEquals('fe80::201:2ff:fef0', Self.V.Received, 'IPv6 address');

  try
    Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;received=';
    Fail('Failed to bail out with empty string');
  except
    on EBadHeader do;
  end;

  try
    Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;received=www.google.com';
    Fail('Failed to bail out with FQDN');
  except
    on EBadHeader do;
  end;

  try
    Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;received=256.0.0.0';
    Fail('Failed to bail out with malformed IPv4 address');
  except
    on EBadHeader do;
  end;

  try
    Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;received=skjfhdlskfhsdfshdfhs';
    Fail('Failed to bail out with nonsense');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipViaHeader.TestValueWithTTL;
begin
  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;tag=heehee';
  CheckEquals(0, Self.V.TTL, 'TTL value when not present');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;ttl=255';
  CheckEquals(255, Self.V.TTL, 'TTL of 255');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;ttl=0';
  CheckEquals(0, Self.V.TTL, 'TTL of 0');

  try
    Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;ttl=256';
    Fail('Failed to bail on invalid TTL (''256'') via SetValue');
  except
    on EBadHeader do;
  end;

  try
    Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;ttl=a';
    Fail('Failed to bail on invalid TTL (''a'') via SetValue');
  except
    on EBadHeader do;
  end;
end;

//******************************************************************************
//* TestTIdSipWarningHeader                                                    *
//******************************************************************************
//* TestTIdSipWarningHeader Public methods *************************************

procedure TestTIdSipWarningHeader.SetUp;
begin
  inherited SetUp;

  Self.W := TIdSipWarningHeader.Create;
end;

procedure TestTIdSipWarningHeader.TearDown;
begin
  Self.W.Free;

  inherited TearDown;
end;

//* TestTIdSipWarningHeader Published methods **********************************

procedure TestTIdSipWarningHeader.TestGetValue;
begin
  Self.W.Code := 302;
  Self.W.Agent := 'gw1.leo-ix.net';
  Self.W.Text := 'Case is not home';

  CheckEquals('302 gw1.leo-ix.net "Case is not home"',
              Self.W.Value,
              'Value');
end;

procedure TestTIdSipWarningHeader.TestName;
begin
  CheckEquals(WarningHeader, Self.W.Name, 'Name');

  Self.W.Name := 'foo';
  CheckEquals(WarningHeader, Self.W.Name, 'Name after set');
end;

procedure TestTIdSipWarningHeader.TestSetValue;
begin
  Self.W.Value := '301 wsfrank "I dont know what message goes here"';
  CheckEquals(301,       Self.W.Code, 'Code');
  CheckEquals('wsfrank', Self.W.Agent, 'Agent');

  CheckEquals('I dont know what message goes here',
              Self.W.Text,
              'Text');
end;

procedure TestTIdSipWarningHeader.TestSetValueMalformed;
begin
  try
    Self.W.Value := 'a bad "header"';
    Fail('Failed to bail on a non-numeric warn-code');
  except
    on EBadHeader do;
  end;

  try
    Self.W.Value := '22 a "bad header"';
    Fail('Failed to bail on a too-short warn-code');
  except
    on EBadHeader do;
  end;

  try
    Self.W.Value := '2222 a "bad header"';
    Fail('Failed to bail on a too-long warn-code');
  except
    on EBadHeader do;
  end;

  try
    Self.W.Value := '301 a bad header';
    Fail('Failed to bail on an unquoted warn-text');
  except
    on EBadHeader do;
  end;

  try
    Self.W.Value := '302 a "bad header';
    Fail('Failed to bail on a malquoted warn-text');
  except
    on EBadHeader do;
  end;

  try
    Self.W.Value := '301 a "bad header";tag=xyz';
    Fail('Failed to bail on a malquoted warn-text (parameters)');
  except
    on EBadHeader do;
  end;
end;

//******************************************************************************
//* TestTIdSipWeightedCommaSeparatedHeader                                     *
//******************************************************************************
//* TestTIdSipWeightedCommaSeparatedHeader Public methods **********************

procedure TestTIdSipWeightedCommaSeparatedHeader.SetUp;
begin
  inherited SetUp;

  Self.W := TIdSipWeightedCommaSeparatedHeader.Create;
end;

procedure TestTIdSipWeightedCommaSeparatedHeader.TearDown;
begin
  Self.W.Free;

  inherited TearDown;
end;

//* TestTIdSipWeightedCommaSeparatedHeader Published methods *******************

procedure TestTIdSipWeightedCommaSeparatedHeader.TestAddValue;
begin
  CheckEquals(0, Self.W.ValueCount, 'Empty string');

  Self.W.AddValue('text/plain');
  CheckEquals(1,            Self.W.ValueCount, 'One Add');
  CheckEquals('text/plain', Self.W.Values[0].Value,  '[0].Value');
  CheckEquals(1000,         Self.W.Values[0].Weight, '[0].Weight');

  Self.W.AddValue('text/xml', 700);
  CheckEquals(2,          Self.W.ValueCount, 'Two Adds');
  CheckEquals('text/xml', Self.W.Values[1].Value,  '[1].Value');
  CheckEquals(700,        Self.W.Values[1].Weight, '[1].Weight');
end;

procedure TestTIdSipWeightedCommaSeparatedHeader.TestClearValues;
begin
  Self.W.AddValue('text/plain');
  Self.W.AddValue('text/plain');
  Self.W.AddValue('text/plain');

  Self.W.ClearValues;

  CheckEquals(0, Self.W.ValueCount, 'ClearValues didn''t');
end;

procedure TestTIdSipWeightedCommaSeparatedHeader.TestGetValue;
begin
  Self.W.AddValue('text/plain', 700);
  Self.W.Values[0].Parameters.Values['foo'] := 'bar';
  Self.W.AddValue('text/t140');

  CheckEquals('text/plain;q=0.7;foo=bar, text/t140', Self.W.Value, 'GetValue');
end;

procedure TestTIdSipWeightedCommaSeparatedHeader.TestSetValue;
begin
  Self.W.Value := '';
  CheckEquals(0, Self.W.ValueCount, 'Empty string');

  Self.W.Value := 'text/plain';
  CheckEquals(1,            Self.W.ValueCount,                 '1: Count');
  CheckEquals(0,            Self.W.Values[0].Parameters.Count, '1: Parameter count');
  CheckEquals('text/plain', Self.W.Values[0].Value,            '1: Value');
  CheckEquals(1000,         Self.W.Values[0].Weight,           '1: Weight');

  Self.W.Value := 'text/plain;q=0.7';
  CheckEquals(1,            Self.W.ValueCount,                 '2: Count');
  CheckEquals(0,            Self.W.Values[0].Parameters.Count, '2: Parameter count');
  CheckEquals('text/plain', Self.W.Values[0].Value,            '2: Value');
  CheckEquals(700,          Self.W.Values[0].Weight,           '2: Weight');

  Self.W.Value := 'text/plain;q=0.7;foo=bar';
  CheckEquals(1,            Self.W.ValueCount,                 '3: Count');
  CheckEquals(1,            Self.W.Values[0].Parameters.Count, '3: Parameter count');
  CheckEquals('foo=bar',    Self.W.Values[0].Parameters[0],    '3: Parameters[0]');
  CheckEquals('text/plain', Self.W.Values[0].Value,            '3: Value');
  CheckEquals(700,          Self.W.Values[0].Weight,           '3: Weight');

  Self.W.Value := 'text/plain;q=0.7;foo=bar, text/t140';
  CheckEquals(2,            Self.W.ValueCount,                 '4: Count');
  CheckEquals(1,            Self.W.Values[0].Parameters.Count, '4: [0].Parameter count');
  CheckEquals('foo=bar',    Self.W.Values[0].Parameters[0],    '4: [0].Parameters[0]');
  CheckEquals('text/plain', Self.W.Values[0].Value,            '4: [0].Value');
  CheckEquals(700,          Self.W.Values[0].Weight,           '4: [0].Weight');

  CheckEquals(0,            Self.W.Values[1].Parameters.Count, '4: [0].Parameter count');
  CheckEquals('text/t140',  Self.W.Values[1].Value,            '4: [1].Value');
  CheckEquals(1000,         Self.W.Values[1].Weight,           '4: [1].Weight');

  Self.W.Value := 'text/plain;q=0.0';
  CheckEquals(1,            Self.W.ValueCount,                 '5: Count');
  CheckEquals(0,            Self.W.Values[0].Parameters.Count, '5: Parameter count');
  CheckEquals(0,            Self.W.Values[0].Weight,           '5: Weight');
end;

procedure TestTIdSipWeightedCommaSeparatedHeader.TestValueMalformed;
begin
  try
    Self.W.Value := 'text/plain;q=1.7';
    Fail('Failed to bail out on malformed qvalue (q > 1)');
  except
    on EBadHeader do;
  end;

  try
    Self.W.Value := 'text/plain;q=a';
    Fail('Failed to bail out on malformed qvalue (q not numeric)');
  except
    on EBadHeader do;
  end;
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

//* TestTIdSipHeaders Private methods ******************************************

procedure TestTIdSipHeaders.CheckType(ExpectedClassType: TClass;
                                      ReceivedObject: TObject;
                                      Message: String = '');
begin
  Self.CheckEquals(ExpectedClassType.ClassName,
                   ReceivedObject.ClassName,
                   Message);
end;

//* TestTIdSipHeaders Published methods ****************************************

procedure TestTIdSipHeaders.TestAddAndCount;
begin
  CheckEquals(0, Self.H.Count, 'Supposedly an empty set of headers');
  CheckEquals(TIdSipHeader.ClassName,
              Self.H.Add(OrganizationHeader).ClassName,
              'Incorrect return type');
  CheckEquals(1, Self.H.Count, 'Failed to add new header');
end;

procedure TestTIdSipHeaders.TestAddHeader;
var
  NewHeader: TIdSipHeader;
begin
  NewHeader := TIdSipHeader.Create;
  try
    NewHeader.Name  := 'X-Header';
    NewHeader.Value := 'boogaloo;foo=bar';

    CheckEquals(0, Self.H.Count, 'Count before Add(Header)');
    Self.H.Add(NewHeader);
    CheckEquals(1, Self.H.Count, 'Count after Add(Header)');

    Self.H.First;
    CheckEquals(NewHeader.AsString,
                Self.H.CurrentHeader.AsString,
                'Data not copied');
  finally
    NewHeader.Free;
  end;
end;

procedure TestTIdSipHeaders.TestAddHeaderName;
begin
  CheckEquals(0, Self.H.Count, 'Count before Add(HeaderName)');
  Self.H.Add('NewHeader').Value := 'FooBar';
  CheckEquals(1, Self.H.Count, 'Count after Add(HeaderName)');

  Self.H.First;
  CheckEquals('NewHeader: FooBar',
              Self.H.CurrentHeader.AsString,
              'AsString');
end;

procedure TestTIdSipHeaders.TestAddHeaders;
var
  NewHeaders: TIdSipHeaders;
begin
  NewHeaders := TIdSipHeaders.Create;
  try
    NewHeaders.Add(ContentLanguageHeader).Value := 'en';
    NewHeaders.Add(ContentLanguageHeader).Value := 'es';
    NewHeaders.Add(ContentLanguageHeader).Value := 'fr';

    CheckEquals(0, Self.H.Count, 'Count before Add(Headers)');
    Self.H.Add(NewHeaders);
    CheckEquals(NewHeaders.Count, Self.H.Count, 'Count after Add(Headers)');

    Check(Self.H.IsEqualTo(NewHeaders), 'Headers weren''t properly added');
  finally
    NewHeaders.Free;
  end;
end;

procedure TestTIdSipHeaders.TestAddHeadersFilter;
var
  NewHeaders: TIdSipHeaders;
  Filter:     TIdSipHeadersFilter;
  Expected:   TIdSipHeaders;
begin
  NewHeaders := TIdSipHeaders.Create;
  try
    NewHeaders.Add(ContentLengthHeaderFull).Value := '22';
    NewHeaders.Add(ContentLanguageHeader).Value := 'en';
    NewHeaders.Add(ContentLanguageHeader).Value := 'es';
    NewHeaders.Add(ContentLanguageHeader).Value := 'fr';
    NewHeaders.Add(RouteHeader).Value := '<sip:127.0.0.1>';

    Filter := TIdSipHeadersFilter.Create(NewHeaders, ContentLanguageHeader);
    try
      CheckEquals(0, Self.H.Count, 'Count before Add(Filter)');

      Self.H.Add(Filter);
      CheckEquals(Filter.Count, Self.H.Count, 'Count after Add(Filter)');

      Expected := TIdSipHeaders.Create;
      try
        Expected.Add(ContentLanguageHeader).Value := 'en';
        Expected.Add(ContentLanguageHeader).Value := 'es';
        Expected.Add(ContentLanguageHeader).Value := 'fr';

        Check(Self.H.IsEqualTo(Expected), 'Filter doesn''t filter properly');
      finally
        Expected.Free;
      end;
    finally
      Filter.Free;
    end;
  finally
    NewHeaders.Free;
  end;
end;

procedure TestTIdSipHeaders.TestAddInReverseOrder;
var
  NewHeaders: TIdSipHeaders;
  Filter:     TIdSipHeadersFilter;
begin
  NewHeaders := TIdSipHeaders.Create;
  try
    NewHeaders.Add(ContentLengthHeaderFull).Value := '22';
    NewHeaders.Add(ContentLanguageHeader).Value := 'en';
    NewHeaders.Add(ContentLanguageHeader).Value := 'es';
    NewHeaders.Add(ContentLanguageHeader).Value := 'fr';
    NewHeaders.Add(RouteHeader).Value := '<sip:127.0.0.1>';

    Filter := TIdSipHeadersFilter.Create(NewHeaders, ContentLanguageHeader);
    try
      CheckEquals(0, Self.H.Count, 'Count before Add(Filter)');

      Self.H.AddInReverseOrder(Filter);
      CheckEquals(Filter.Count, Self.H.Count, 'Count after Add(Filter)');

      CheckEquals('fr', Self.H.Items[0].Value, '1st header');
      CheckEquals('es', Self.H.Items[1].Value, '2nd header');
      CheckEquals('en', Self.H.Items[2].Value, '3rd header');
    finally
      Filter.Free;
    end;
  finally
    NewHeaders.Free;
  end;
end;

procedure TestTIdSipHeaders.TestAddResultTypes;
begin
  CheckType(TIdSipWeightedCommaSeparatedHeader, Self.H.Add(AcceptHeader),               AcceptHeader);
  CheckType(TIdSipCommaSeparatedHeader,         Self.H.Add(AcceptEncodingHeader),       AcceptEncodingHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(AcceptLanguageHeader),       AcceptLanguageHeader);
  CheckType(TIdSipUriHeader,                    Self.H.Add(AlertInfoHeader),            AlertInfoHeader);
  CheckType(TIdSipCommaSeparatedHeader,         Self.H.Add(AllowHeader),                AllowHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(AuthenticationInfoHeader),   AuthenticationInfoHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(AuthorizationHeader),        AuthorizationHeader);
  CheckType(TIdSipCallIdHeader,                 Self.H.Add(CallIDHeaderFull),           CallIDHeaderFull);
  CheckType(TIdSipCallIdHeader,                 Self.H.Add(CallIDHeaderShort),          CallIDHeaderShort);
  CheckType(TIdSipHeader,                       Self.H.Add(CallInfoHeader),             CallInfoHeader);
  CheckType(TIdSipContactHeader,                Self.H.Add(ContactHeaderFull),          ContactHeaderFull);
  CheckType(TIdSipContactHeader,                Self.H.Add(ContactHeaderShort),         ContactHeaderShort);
  CheckType(TIdSipContentDispositionHeader,     Self.H.Add(ContentDispositionHeader),   ContentDispositionHeader);
  CheckType(TIdSipCommaSeparatedHeader,         Self.H.Add(ContentEncodingHeaderFull),  ContentEncodingHeaderFull);
  CheckType(TIdSipCommaSeparatedHeader,         Self.H.Add(ContentEncodingHeaderShort), ContentEncodingHeaderShort);
  CheckType(TIdSipCommaSeparatedHeader,         Self.H.Add(ContentLanguageHeader),      ContentLanguageHeader);
  CheckType(TIdSipNumericHeader,                Self.H.Add(ContentLengthHeaderFull),    ContentLengthHeaderFull);
  CheckType(TIdSipNumericHeader,                Self.H.Add(ContentLengthHeaderShort),   ContentLengthHeaderShort);
  CheckType(TIdSipHeader,                       Self.H.Add(ContentTypeHeaderFull),      ContentTypeHeaderFull);
  CheckType(TIdSipHeader,                       Self.H.Add(ContentTypeHeaderShort),     ContentTypeHeaderShort);
  CheckType(TIdSipCSeqHeader,                   Self.H.Add(CSeqHeader),                 CSeqHeader);
  CheckType(TIdSipDateHeader,                   Self.H.Add(DateHeader),                 DateHeader);
  CheckType(TIdSipUriHeader,                    Self.H.Add(ErrorInfoHeader),            ErrorInfoHeader);
  CheckType(TIdSipNumericHeader,                Self.H.Add(ExpiresHeader),              ExpiresHeader);
  CheckType(TIdSipFromHeader,                   Self.H.Add(FromHeaderFull),             FromHeaderFull);
  CheckType(TIdSipFromHeader,                   Self.H.Add(FromHeaderShort),            FromHeaderShort);
  CheckType(TIdSipCallIdHeader,                 Self.H.Add(InReplyToHeader),            InReplyToHeader);
  CheckType(TIdSipMaxForwardsHeader,            Self.H.Add(MaxForwardsHeader),          MaxForwardsHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(MIMEVersionHeader),          MIMEVersionHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(MinExpiresHeader),           MinExpiresHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(OrganizationHeader),         OrganizationHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(PriorityHeader),             PriorityHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(ProxyAuthenticateHeader),    ProxyAuthenticateHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(ProxyAuthorizationHeader),   ProxyAuthorizationHeader);
  CheckType(TIdSipCommaSeparatedHeader,         Self.H.Add(ProxyRequireHeader),         ProxyRequireHeader);
  CheckType(TIdSipRecordRouteHeader,            Self.H.Add(RecordRouteHeader),          RecordRouteHeader);
  CheckType(TIdSipCommaSeparatedHeader,         Self.H.Add(RequireHeader),              RequireHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(ReplyToHeader),              ReplyToHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(RetryAfterHeader),           RetryAfterHeader);
  CheckType(TIdSipRouteHeader,                  Self.H.Add(RouteHeader),                RouteHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(ServerHeader),               ServerHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(SubjectHeaderFull),          SubjectHeaderFull);
  CheckType(TIdSipHeader,                       Self.H.Add(SubjectHeaderShort),         SubjectHeaderShort);
  CheckType(TIdSipCommaSeparatedHeader,         Self.H.Add(SupportedHeaderFull),        SupportedHeaderFull);
  CheckType(TIdSipCommaSeparatedHeader,         Self.H.Add(SupportedHeaderShort),       SupportedHeaderShort);
  CheckType(TIdSipTimestampHeader,              Self.H.Add(TimestampHeader),            TimestampHeader);
  CheckType(TIdSipToHeader,                     Self.H.Add(ToHeaderFull),               ToHeaderFull);
  CheckType(TIdSipToHeader,                     Self.H.Add(ToHeaderShort),              ToHeaderShort);
  CheckType(TIdSipCommaSeparatedHeader,         Self.H.Add(UnsupportedHeader),          UnsupportedHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(UserAgentHeader),            UserAgentHeader);
  CheckType(TIdSipViaHeader,                    Self.H.Add(ViaHeaderFull),              ViaHeaderFull);
  CheckType(TIdSipViaHeader,                    Self.H.Add(ViaHeaderShort),             ViaHeaderShort);
  CheckType(TIdSipWarningHeader,                Self.H.Add(WarningHeader),              WarningHeader);
  CheckType(TIdSipHeader,                       Self.H.Add(WWWAuthenticateHeader),      WWWAuthenticateHeader);
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

procedure TestTIdSipHeaders.TestCanonicaliseName;
begin
  CheckEquals('', TIdSipHeaders.CanonicaliseName(''), 'Empty string');
  CheckEquals('New-Header', TIdSipHeaders.CanonicaliseName('New-Header'), 'New-Header');
  CheckEquals('new-header', TIdSipHeaders.CanonicaliseName('new-header'), 'new-header');

  CheckEquals(AcceptHeader, TIdSipHeaders.CanonicaliseName('accept'),     'accept');
  CheckEquals(AcceptHeader, TIdSipHeaders.CanonicaliseName('Accept'),     'Accept');
  CheckEquals(AcceptHeader, TIdSipHeaders.CanonicaliseName(AcceptHeader), 'AcceptHeader constant');

  CheckEquals(AcceptEncodingHeader, TIdSipHeaders.CanonicaliseName('accept-encoding'),    'accept-encoding');
  CheckEquals(AcceptEncodingHeader, TIdSipHeaders.CanonicaliseName('Accept-Encoding'),    'Accept-Encoding');
  CheckEquals(AcceptEncodingHeader, TIdSipHeaders.CanonicaliseName(AcceptEncodingHeader), 'AcceptEncodingHeader constant');

  CheckEquals(AcceptLanguageHeader, TIdSipHeaders.CanonicaliseName('accept-language'),    'accept-language');
  CheckEquals(AcceptLanguageHeader, TIdSipHeaders.CanonicaliseName('Accept-Language'),    'Accept-Language');
  CheckEquals(AcceptLanguageHeader, TIdSipHeaders.CanonicaliseName(AcceptLanguageHeader), 'AcceptLanguageHeader constant');

  CheckEquals(AlertInfoHeader, TIdSipHeaders.CanonicaliseName('alert-info'),    'alert-info');
  CheckEquals(AlertInfoHeader, TIdSipHeaders.CanonicaliseName('Alert-Info'),    'Alert-Info');
  CheckEquals(AlertInfoHeader, TIdSipHeaders.CanonicaliseName(AlertInfoHeader), 'AlertInfoHeader constant');

  CheckEquals(AllowHeader, TIdSipHeaders.CanonicaliseName('allow'),     'allow');
  CheckEquals(AllowHeader, TIdSipHeaders.CanonicaliseName('Allow'),     'Allow');
  CheckEquals(AllowHeader, TIdSipHeaders.CanonicaliseName(AllowHeader), 'AllowHeader constant');

  CheckEquals(AuthenticationInfoHeader, TIdSipHeaders.CanonicaliseName('authentication-info'),    'authentication-info');
  CheckEquals(AuthenticationInfoHeader, TIdSipHeaders.CanonicaliseName('Authentication-Info'),    'Authentication-Info');
  CheckEquals(AuthenticationInfoHeader, TIdSipHeaders.CanonicaliseName(AuthenticationInfoHeader), 'AuthenticationInfoHeader constant');

  CheckEquals(AuthorizationHeader, TIdSipHeaders.CanonicaliseName('authorization'),     'authorization');
  CheckEquals(AuthorizationHeader, TIdSipHeaders.CanonicaliseName('Authorization'),     'Authorization');
  CheckEquals(AuthorizationHeader, TIdSipHeaders.CanonicaliseName(AuthorizationHeader), 'AuthorizationHeader constant');

  CheckEquals(CallIDHeaderFull, TIdSipHeaders.CanonicaliseName('call-ID'),         'call-ID');
  CheckEquals(CallIDHeaderFull, TIdSipHeaders.CanonicaliseName('Call-ID'),         'Call-ID');
  CheckEquals(CallIDHeaderFull, TIdSipHeaders.CanonicaliseName('i'),               'i');
  CheckEquals(CallIDHeaderFull, TIdSipHeaders.CanonicaliseName('I'),               'I');
  CheckEquals(CallIDHeaderFull, TIdSipHeaders.CanonicaliseName(CallIDHeaderFull),  'CallIDHeaderFull constant');
  CheckEquals(CallIDHeaderFull, TIdSipHeaders.CanonicaliseName(CallIDHeaderShort), 'CallIDHeaderShort constant');

  CheckEquals(CallInfoHeader, TIdSipHeaders.CanonicaliseName('call-info'),     'call-info');
  CheckEquals(CallInfoHeader, TIdSipHeaders.CanonicaliseName('Call-Info'),     'Call-Info');
  CheckEquals(CallInfoHeader, TIdSipHeaders.CanonicaliseName(CallInfoHeader), 'CallInfoHeader constant');

  CheckEquals(ContactHeaderFull, TIdSipHeaders.CanonicaliseName('contact'),          'contact');
  CheckEquals(ContactHeaderFull, TIdSipHeaders.CanonicaliseName('Contact'),          'Contact');
  CheckEquals(ContactHeaderFull, TIdSipHeaders.CanonicaliseName('m'),                'm');
  CheckEquals(ContactHeaderFull, TIdSipHeaders.CanonicaliseName('M'),                'M');
  CheckEquals(ContactHeaderFull, TIdSipHeaders.CanonicaliseName(ContactHeaderFull),  'ContactHeaderFull constant');
  CheckEquals(ContactHeaderFull, TIdSipHeaders.CanonicaliseName(ContactHeaderShort), 'ContactHeaderShort constant');

  CheckEquals(ContentDispositionHeader, TIdSipHeaders.CanonicaliseName('content-disposition'),    'content-disposition');
  CheckEquals(ContentDispositionHeader, TIdSipHeaders.CanonicaliseName('Content-Disposition'),    'Content-Disposition');
  CheckEquals(ContentDispositionHeader, TIdSipHeaders.CanonicaliseName(ContentDispositionHeader), 'ContentDispositionHeader constant');

  CheckEquals(ContentEncodingHeaderFull, TIdSipHeaders.CanonicaliseName('content-encoding'),         'content-encoding');
  CheckEquals(ContentEncodingHeaderFull, TIdSipHeaders.CanonicaliseName('Content-Encoding'),         'Content-Encoding');
  CheckEquals(ContentEncodingHeaderFull, TIdSipHeaders.CanonicaliseName('e'),                        'e');
  CheckEquals(ContentEncodingHeaderFull, TIdSipHeaders.CanonicaliseName('E'),                        'E');
  CheckEquals(ContentEncodingHeaderFull, TIdSipHeaders.CanonicaliseName(ContentEncodingHeaderFull),  'ContentEncodingHeaderFull constant');
  CheckEquals(ContentEncodingHeaderFull, TIdSipHeaders.CanonicaliseName(ContentEncodingHeaderShort), 'ContentEncodingHeaderShort constant');

  CheckEquals(ContentLanguageHeader, TIdSipHeaders.CanonicaliseName('content-language'),    'content-language');
  CheckEquals(ContentLanguageHeader, TIdSipHeaders.CanonicaliseName('Content-Language'),    'Content-Language');
  CheckEquals(ContentLanguageHeader, TIdSipHeaders.CanonicaliseName(ContentLanguageHeader), 'ContentLanguageHeader constant');

  CheckEquals(ContentLengthHeaderFull, TIdSipHeaders.CanonicaliseName('Content-Length'),         'Content-Length');
  CheckEquals(ContentLengthHeaderFull, TIdSipHeaders.CanonicaliseName('Content-Length'),         'Content-Length');
  CheckEquals(ContentLengthHeaderFull, TIdSipHeaders.CanonicaliseName('l'),                      'l');
  CheckEquals(ContentLengthHeaderFull, TIdSipHeaders.CanonicaliseName('L'),                      'L');
  CheckEquals(ContentLengthHeaderFull, TIdSipHeaders.CanonicaliseName(ContentLengthHeaderFull),  'ContentLengthHeaderFull constant');
  CheckEquals(ContentLengthHeaderFull, TIdSipHeaders.CanonicaliseName(ContentLengthHeaderShort), 'ContentLengthHeaderShort constant');

  CheckEquals(ContentTypeHeaderFull, TIdSipHeaders.CanonicaliseName('content-type'),         'content-type');
  CheckEquals(ContentTypeHeaderFull, TIdSipHeaders.CanonicaliseName('Content-Type'),         'Content-Type');
  CheckEquals(ContentTypeHeaderFull, TIdSipHeaders.CanonicaliseName('c'),                    'c');
  CheckEquals(ContentTypeHeaderFull, TIdSipHeaders.CanonicaliseName('C'),                    'C');
  CheckEquals(ContentTypeHeaderFull, TIdSipHeaders.CanonicaliseName(ContentTypeHeaderFull),  'ContentTypeHeaderFull constant');
  CheckEquals(ContentTypeHeaderFull, TIdSipHeaders.CanonicaliseName(ContentTypeHeaderShort), 'ContentTypeHeaderShort constant');

  CheckEquals(CSeqHeader, TIdSipHeaders.CanonicaliseName('cseq'),     'cseq');
  CheckEquals(CSeqHeader, TIdSipHeaders.CanonicaliseName('CSeq'),     'CSeq');
  CheckEquals(CSeqHeader, TIdSipHeaders.CanonicaliseName(CSeqHeader), 'CSeqHeader constant');

  CheckEquals(DateHeader, TIdSipHeaders.CanonicaliseName('date'),     'date');
  CheckEquals(DateHeader, TIdSipHeaders.CanonicaliseName('Date'),     'Date');
  CheckEquals(DateHeader, TIdSipHeaders.CanonicaliseName(DateHeader), 'DateHeader constant');

  CheckEquals(ErrorInfoHeader, TIdSipHeaders.CanonicaliseName('error-info'),     'irror-info');
  CheckEquals(ErrorInfoHeader, TIdSipHeaders.CanonicaliseName('Error-Info'),     'Error-Info');
  CheckEquals(ErrorInfoHeader, TIdSipHeaders.CanonicaliseName(ErrorInfoHeader), 'ErrorInfoHeader constant');

  CheckEquals(ExpiresHeader, TIdSipHeaders.CanonicaliseName('expires'),     'expires');
  CheckEquals(ExpiresHeader, TIdSipHeaders.CanonicaliseName('Expires'),     'Expires');
  CheckEquals(ExpiresHeader, TIdSipHeaders.CanonicaliseName(ExpiresHeader), 'ExpiresHeader constant');

  CheckEquals(FromHeaderFull, TIdSipHeaders.CanonicaliseName('from'),          'from');
  CheckEquals(FromHeaderFull, TIdSipHeaders.CanonicaliseName('From'),          'From');
  CheckEquals(FromHeaderFull, TIdSipHeaders.CanonicaliseName('f'),             'f');
  CheckEquals(FromHeaderFull, TIdSipHeaders.CanonicaliseName('F'),             'F');
  CheckEquals(FromHeaderFull, TIdSipHeaders.CanonicaliseName(FromHeaderFull),  'FromHeaderFull constant');
  CheckEquals(FromHeaderFull, TIdSipHeaders.CanonicaliseName(FromHeaderShort), 'FromHeaderShort constant');

  CheckEquals(InReplyToHeader, TIdSipHeaders.CanonicaliseName('in-reply-to'),   'in-reply-to');
  CheckEquals(InReplyToHeader, TIdSipHeaders.CanonicaliseName('In-Reply-To'),   'In-Reply-To');
  CheckEquals(InReplyToHeader, TIdSipHeaders.CanonicaliseName(InReplyToHeader), 'InReplyToHeader constant');

  CheckEquals(MaxForwardsHeader, TIdSipHeaders.CanonicaliseName('max-forwards'),    'max-forwards');
  CheckEquals(MaxForwardsHeader, TIdSipHeaders.CanonicaliseName('Max-Forwards'),    'Max-Forwards');
  CheckEquals(MaxForwardsHeader, TIdSipHeaders.CanonicaliseName(MaxForwardsHeader), 'MaxForwardsHeader constant');

  CheckEquals(MIMEVersionHeader, TIdSipHeaders.CanonicaliseName('mime-version'),    'mime-version');
  CheckEquals(MIMEVersionHeader, TIdSipHeaders.CanonicaliseName('MIME-Version'),    'MIME-Version');
  CheckEquals(MIMEVersionHeader, TIdSipHeaders.CanonicaliseName(MIMEVersionHeader), 'MIMEVersionHeader constant');

  CheckEquals(MinExpiresHeader, TIdSipHeaders.CanonicaliseName('min-expires'),    'min-expires');
  CheckEquals(MinExpiresHeader, TIdSipHeaders.CanonicaliseName('Min-Expires'),    'Min-Expires');
  CheckEquals(MinExpiresHeader, TIdSipHeaders.CanonicaliseName(MinExpiresHeader), 'MinExpiresHeader constant');

  CheckEquals(OrganizationHeader, TIdSipHeaders.CanonicaliseName('organization'),     'organization');
  CheckEquals(OrganizationHeader, TIdSipHeaders.CanonicaliseName('Organization'),     'Organization');
  CheckEquals(OrganizationHeader, TIdSipHeaders.CanonicaliseName(OrganizationHeader), 'OrganizationHeader constant');

  CheckEquals(PriorityHeader, TIdSipHeaders.CanonicaliseName('priority'),     'priority');
  CheckEquals(PriorityHeader, TIdSipHeaders.CanonicaliseName('Priority'),     'Priority');
  CheckEquals(PriorityHeader, TIdSipHeaders.CanonicaliseName(PriorityHeader), 'PriorityHeader constant');

  CheckEquals(ProxyAuthenticateHeader, TIdSipHeaders.CanonicaliseName('proxy-authenticate'),    'proxy-authenticate');
  CheckEquals(ProxyAuthenticateHeader, TIdSipHeaders.CanonicaliseName('Proxy-Authenticate'),    'Proxy-Authenticate');
  CheckEquals(ProxyAuthenticateHeader, TIdSipHeaders.CanonicaliseName(ProxyAuthenticateHeader), 'ProxyAuthenticateHeader constant');

  CheckEquals(ProxyAuthorizationHeader, TIdSipHeaders.CanonicaliseName('proxy-authorization'),    'proxy-authorization');
  CheckEquals(ProxyAuthorizationHeader, TIdSipHeaders.CanonicaliseName('Proxy-Authorization'),    'Proxy-Authorization');
  CheckEquals(ProxyAuthorizationHeader, TIdSipHeaders.CanonicaliseName(ProxyAuthorizationHeader), 'ProxyAuthorizationHeader constant');

  CheckEquals(ProxyRequireHeader, TIdSipHeaders.CanonicaliseName('proxy-require'),    'proxy-require');
  CheckEquals(ProxyRequireHeader, TIdSipHeaders.CanonicaliseName('Proxy-Require'),    'Proxy-Require');
  CheckEquals(ProxyRequireHeader, TIdSipHeaders.CanonicaliseName(ProxyRequireHeader), 'ProxyRequireHeader constant');

  CheckEquals(RecordRouteHeader, TIdSipHeaders.CanonicaliseName('record-route'),    'record-route');
  CheckEquals(RecordRouteHeader, TIdSipHeaders.CanonicaliseName('Record-Route'),    'Record-Route');
  CheckEquals(RecordRouteHeader, TIdSipHeaders.CanonicaliseName(RecordRouteHeader), 'RecordRouteHeader constant');

  CheckEquals(ReplyToHeader, TIdSipHeaders.CanonicaliseName('reply-to'),    'reply-to');
  CheckEquals(ReplyToHeader, TIdSipHeaders.CanonicaliseName('Reply-To'),    'Reply-To');
  CheckEquals(ReplyToHeader, TIdSipHeaders.CanonicaliseName(ReplyToHeader), 'ReplyToHeader constant');

  CheckEquals(RequireHeader, TIdSipHeaders.CanonicaliseName('require'),     'require');
  CheckEquals(RequireHeader, TIdSipHeaders.CanonicaliseName('Require'),     'Require');
  CheckEquals(RequireHeader, TIdSipHeaders.CanonicaliseName(RequireHeader), 'RequireHeader constant');

  CheckEquals(RetryAfterHeader, TIdSipHeaders.CanonicaliseName('retry-after'),    'retry-after');
  CheckEquals(RetryAfterHeader, TIdSipHeaders.CanonicaliseName('Retry-After'),    'Retry-After');
  CheckEquals(RetryAfterHeader, TIdSipHeaders.CanonicaliseName(RetryAfterHeader), 'RetryAfterHeader constant');

  CheckEquals(RouteHeader, TIdSipHeaders.CanonicaliseName('route'),     'route');
  CheckEquals(RouteHeader, TIdSipHeaders.CanonicaliseName('Route'),     'Route');
  CheckEquals(RouteHeader, TIdSipHeaders.CanonicaliseName(RouteHeader), 'RouteHeader constant');

  CheckEquals(ServerHeader, TIdSipHeaders.CanonicaliseName('server'),     'server');
  CheckEquals(ServerHeader, TIdSipHeaders.CanonicaliseName('Server'),     'Server');
  CheckEquals(ServerHeader, TIdSipHeaders.CanonicaliseName(ServerHeader), 'ServerHeader constant');

  CheckEquals(SubjectHeaderFull, TIdSipHeaders.CanonicaliseName('subject'),          'subject');
  CheckEquals(SubjectHeaderFull, TIdSipHeaders.CanonicaliseName('Subject'),          'Subject');
  CheckEquals(SubjectHeaderFull, TIdSipHeaders.CanonicaliseName('s'),                's');
  CheckEquals(SubjectHeaderFull, TIdSipHeaders.CanonicaliseName('S'),                'S');
  CheckEquals(SubjectHeaderFull, TIdSipHeaders.CanonicaliseName(SubjectHeaderFull),  'SubjectHeaderFull constant');
  CheckEquals(SubjectHeaderFull, TIdSipHeaders.CanonicaliseName(SubjectHeaderShort), 'SubjectHeaderShort constant');

  CheckEquals(SupportedHeaderFull, TIdSipHeaders.CanonicaliseName('supported'),          'supported');
  CheckEquals(SupportedHeaderFull, TIdSipHeaders.CanonicaliseName('Supported'),          'Supported');
  CheckEquals(SupportedHeaderFull, TIdSipHeaders.CanonicaliseName('k'),                  'k');
  CheckEquals(SupportedHeaderFull, TIdSipHeaders.CanonicaliseName('K'),                  'K');
  CheckEquals(SupportedHeaderFull, TIdSipHeaders.CanonicaliseName(SupportedHeaderFull),  'SupportedHeaderFull constant');
  CheckEquals(SupportedHeaderFull, TIdSipHeaders.CanonicaliseName(SupportedHeaderShort), 'SupportedHeaderShort constant');

  CheckEquals(TimestampHeader, TIdSipHeaders.CanonicaliseName('timestamp'),     'timestamp');
  CheckEquals(TimestampHeader, TIdSipHeaders.CanonicaliseName('Timestamp'),     'Timestamp');
  CheckEquals(TimestampHeader, TIdSipHeaders.CanonicaliseName(TimestampHeader), 'TimestampHeader constant');

  CheckEquals(ToHeaderFull, TIdSipHeaders.CanonicaliseName('to'),          'to');
  CheckEquals(ToHeaderFull, TIdSipHeaders.CanonicaliseName('To'),          'To');
  CheckEquals(ToHeaderFull, TIdSipHeaders.CanonicaliseName('t'),           't');
  CheckEquals(ToHeaderFull, TIdSipHeaders.CanonicaliseName('T'),           'T');
  CheckEquals(ToHeaderFull, TIdSipHeaders.CanonicaliseName(ToHeaderFull),  'ToHeaderFull constant');
  CheckEquals(ToHeaderFull, TIdSipHeaders.CanonicaliseName(ToHeaderShort), 'ToHeaderShort constant');

  CheckEquals(UnsupportedHeader, TIdSipHeaders.CanonicaliseName('unsupported'),     'unsupported');
  CheckEquals(UnsupportedHeader, TIdSipHeaders.CanonicaliseName('Unsupported'),     'Unsupported');
  CheckEquals(UnsupportedHeader, TIdSipHeaders.CanonicaliseName(UnsupportedHeader), 'UnsupportedHeader constant');

  CheckEquals(UserAgentHeader, TIdSipHeaders.CanonicaliseName('user-agent'),    'user-agent');
  CheckEquals(UserAgentHeader, TIdSipHeaders.CanonicaliseName('User-Agent'),    'User-Agent');
  CheckEquals(UserAgentHeader, TIdSipHeaders.CanonicaliseName(UserAgentHeader), 'UserAgentHeader constant');

  CheckEquals(ViaHeaderFull, TIdSipHeaders.CanonicaliseName('via'),          'via');
  CheckEquals(ViaHeaderFull, TIdSipHeaders.CanonicaliseName('Via'),          'Via');
  CheckEquals(ViaHeaderFull, TIdSipHeaders.CanonicaliseName('v'),            'v');
  CheckEquals(ViaHeaderFull, TIdSipHeaders.CanonicaliseName('V'),            'V');
  CheckEquals(ViaHeaderFull, TIdSipHeaders.CanonicaliseName(ViaHeaderFull),  'ViaHeaderFull constant');
  CheckEquals(ViaHeaderFull, TIdSipHeaders.CanonicaliseName(ViaHeaderShort), 'ViaHeaderShort constant');

  CheckEquals(WarningHeader, TIdSipHeaders.CanonicaliseName('warning'),     'warning');
  CheckEquals(WarningHeader, TIdSipHeaders.CanonicaliseName('Warning'),     'Warning');
  CheckEquals(WarningHeader, TIdSipHeaders.CanonicaliseName(WarningHeader), 'WarningHeader constant');

  CheckEquals(WWWAuthenticateHeader, TIdSipHeaders.CanonicaliseName('www-authenticate'),    'www-authenticate');
  CheckEquals(WWWAuthenticateHeader, TIdSipHeaders.CanonicaliseName('WWW-Authenticate'),    'WWW-Authenticate');
  CheckEquals(WWWAuthenticateHeader, TIdSipHeaders.CanonicaliseName(WWWAuthenticateHeader), 'WWWAuthenticateHeader constant');
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

procedure TestTIdSipHeaders.TestDelete;
begin
  Self.H.Add('foo');
  Self.H.Add('bar');
  Self.H.Add('baz');
  Self.H.Add('quaax');

  Self.H.Delete(1);
  CheckEquals(3, Self.H.Count, 'Count after 1st Delete');
  CheckEquals('foo',   Self.H.Items[0].Name, '1: 1st header');
  CheckEquals('baz',   Self.H.Items[1].Name, '1: 2nd header');
  CheckEquals('quaax', Self.H.Items[2].Name, '1: 3rd header');

  Self.H.Delete(2);
  CheckEquals(2, Self.H.Count, 'Count after 2nd Delete');
  CheckEquals('foo',   Self.H.Items[0].Name, '2: 1st header');
  CheckEquals('baz',   Self.H.Items[1].Name, '2: 2nd header');

  Self.H.Delete(0);
  CheckEquals(1, Self.H.Count, 'Count after 3rd Delete');
  CheckEquals('baz', Self.H.Items[0].Name, '3: 1st header');

  Self.H.Delete(0);
  CheckEquals(0, Self.H.Count, 'Count after 4th Delete');
end;

procedure TestTIdSipHeaders.TestFirst;
begin
  Self.H.First;
  CheckNull(Self.H.CurrentHeader, 'First element of an empty collection');

  Self.H.Add('foo');
  Self.H.First;
  CheckNotNull(Self.H.CurrentHeader,
               'First element of a non-empty collection');
  CheckEquals('foo',
              Self.H.CurrentHeader.Name,
              'Name of first element');
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

procedure TestTIdSipHeaders.TestIsCompoundHeader;
begin
  Check(not TIdSipHeaders.IsCompoundHeader(''),                         '''''');
  Check(not TIdSipHeaders.IsCompoundHeader('New-Header'),               'New-Header');
  Check(not TIdSipHeaders.IsCompoundHeader(AcceptHeader),               AcceptHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(AcceptEncodingHeader),       AcceptEncodingHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(AcceptLanguageHeader),       AcceptLanguageHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(AlertInfoHeader),            AlertInfoHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(AllowHeader),                AllowHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(AuthenticationInfoHeader),   AuthenticationInfoHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(AuthorizationHeader),        AuthorizationHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(CallIDHeaderFull),           CallIDHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(CallIDHeaderShort),          CallIDHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(CallInfoHeader),             CallInfoHeader);
  Check(    TIdSipHeaders.IsCompoundHeader(ContactHeaderFull),          ContactHeaderFull);
  Check(    TIdSipHeaders.IsCompoundHeader(ContactHeaderShort),         ContactHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentDispositionHeader),   ContentDispositionHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentEncodingHeaderFull),  ContentEncodingHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentEncodingHeaderShort), ContentEncodingHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentLanguageHeader),      ContentLanguageHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentLengthHeaderFull),    ContentLengthHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentLengthHeaderShort),   ContentLengthHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentTypeHeaderFull),      ContentTypeHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentTypeHeaderShort),     ContentTypeHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(CSeqHeader),                 CSeqHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(DateHeader),                 DateHeader);
  Check(    TIdSipHeaders.IsCompoundHeader(ErrorInfoHeader),            ErrorInfoHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ExpiresHeader),              ExpiresHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(FromHeaderFull),             FromHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(FromHeaderShort),            FromHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(InReplyToHeader),            InReplyToHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(MaxForwardsHeader),          MaxForwardsHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(MIMEVersionHeader),          MIMEVersionHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(MinExpiresHeader),           MinExpiresHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(OrganizationHeader),         OrganizationHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(PriorityHeader),             PriorityHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ProxyAuthenticateHeader),    ProxyAuthenticateHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ProxyAuthorizationHeader),   ProxyAuthorizationHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ProxyRequireHeader),         ProxyRequireHeader);
  Check(    TIdSipHeaders.IsCompoundHeader(RecordRouteHeader),          RecordRouteHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ReplyToHeader),              ReplyToHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(RequireHeader),              RequireHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(RetryAfterHeader),           RetryAfterHeader);
  Check(    TIdSipHeaders.IsCompoundHeader(RouteHeader),                RouteHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ServerHeader),               ServerHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(SubjectHeaderFull),          SubjectHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(SubjectHeaderShort),         SubjectHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(SupportedHeaderFull),        SupportedHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(SupportedHeaderShort),       SupportedHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(TimestampHeader),            TimestampHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ToHeaderFull),               ToHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(ToHeaderShort),              ToHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(UnsupportedHeader),          UnsupportedHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(UserAgentHeader),            UserAgentHeader);
  Check(    TIdSipHeaders.IsCompoundHeader(ViaHeaderFull),              ViaHeaderFull);
  Check(    TIdSipHeaders.IsCompoundHeader(ViaHeaderShort),             ViaHeaderShort);
  Check(    TIdSipHeaders.IsCompoundHeader(WarningHeader),              WarningHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(WWWAuthenticateHeader),      WWWAuthenticateHeader);
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

procedure TestTIdSipHeaders.TestIsEmpty;
begin
  CheckEquals(0, Self.H.Count, 'Sanity check on test entry');
  Check(Self.H.IsEmpty, 'IsEmpty with 0 headers');
  Self.H.Add(RouteHeader).Value := '<sip:127.0.0.2>';
  Check(not Self.H.IsEmpty, 'IsEmpty after Add');
end;

procedure TestTIdSipHeaders.TestIsErrorInfo;
begin
  Check(not TIdSipHeaders.IsErrorInfo(''),              '''''');
  Check(    TIdSipHeaders.IsErrorInfo('Error-INFO'),    'Error-INFO');
  Check(    TIdSipHeaders.IsErrorInfo(ErrorInfoHeader), 'ErrorInfoHeader constant');
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

procedure TestTIdSipHeaders.TestIsRecordRoute;
begin
  Check(not TIdSipHeaders.IsRecordRoute(''),                '''''');
  Check(    TIdSipHeaders.IsRecordRoute('record-rOuTe'),    'record-rOuTe');
  Check(    TIdSipHeaders.IsRecordRoute(RecordRouteHeader), 'RecordRouteHeader constant');
end;

procedure TestTIdSipHeaders.TestIsRoute;
begin
  Check(not TIdSipHeaders.IsRoute(''),                '''''');
  Check(    TIdSipHeaders.IsRoute('rOuTe'),    'rOuTe');
  Check(    TIdSipHeaders.IsRoute(RouteHeader), 'RouteHeader constant');
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

procedure TestTIdSipHeaders.TestIsWarning;
begin
  Check(not TIdSipHeaders.IsWarning(''),            '''''');
  Check(    TIdSipHeaders.IsWarning('warnING'),     'warnING');
  Check(    TIdSipHeaders.IsWarning(WarningHeader), 'WarningHeader constant');
end;

procedure TestTIdSipHeaders.TestIteratorVisitsAllHeaders;
var
  X, Y, Z: TIdSipHeader;
begin
  X := Self.H.Add('X-X-X');
  Y := Self.H.Add('X-X-Y');
  Z := Self.H.Add('X-X-Z');

  Self.H.First;
  while Self.H.HasNext do begin
    Self.H.CurrentHeader.Params['foo'] := 'bar';
    Self.H.Next;
  end;

  CheckEquals('bar', X.Params['foo'], 'X wasn''t visited by iterator');
  CheckEquals('bar', Y.Params['foo'], 'Y wasn''t visited by iterator');
  CheckEquals('bar', Z.Params['foo'], 'Z wasn''t visited by iterator');
end;

procedure TestTIdSipHeaders.TestRemove;
var
  X, Y, Z: TIdSipHeader;
begin
  X := Self.H.Add('X-X-X');
  Y := Self.H.Add('X-X-Y');
  Z := Self.H.Add('X-X-Z');

  Self.H.Remove(X);
  CheckEquals(Self.H.Items[0].AsString, Y.AsString, 'Wrong header removed (0)');
  CheckEquals(Self.H.Items[1].AsString, Z.AsString, 'Wrong header removed (1)');
end;

procedure TestTIdSipHeaders.TestRemoveAll;
begin
  Self.H.Add('Foo');
  Self.H.Add('Bar');
  Self.H.Add('Foo');

  Self.H.RemoveAll('foo');
  CheckEquals(1, Self.H.Count, 'Header count');
  CheckEquals('Bar', Self.H.Items[0].Name, 'Wrong headers removed');
end;

procedure TestTIdSipHeaders.TestSetMaxForwards;
begin
  Self.H.Headers[MaxForwardsHeader].Value := '1';
  CheckEquals('1', Self.H.Headers[MaxForwardsHeader].Value, '1');

  try
    Self.H.Headers[MaxForwardsHeader].Value := 'a';
    Fail('Failed to bail out setting value to ''a''');
  except
    on EBadHeader do;
  end;
end;

//******************************************************************************
//* TestTIdSipHeadersFilter                                                    *
//******************************************************************************
//* TestTIdSipHeadersFilter Public methods *************************************

procedure TestTIdSipHeadersFilter.SetUp;
begin
  inherited SetUp;

  Self.Headers := TIdSipHeaders.Create;

  Self.Headers.Add(MaxForwardsHeader).Value       := '70';
  Self.Headers.Add(RouteHeader).Value             := 'wsfrank <sip:192.168.1.43>';
  Self.Headers.Add(ContentLengthHeaderFull).Value := '29';
  Self.Headers.Add(RouteHeader).Value             := 'localhost <sip:127.0.0.1>';

  Self.Filter := TIdSipHeadersFilter.Create(Self.Headers, RouteHeader);
end;

procedure TestTIdSipHeadersFilter.TearDown;
begin
  Self.Filter.Free;
  Self.Headers.Free;

  inherited TearDown;
end;

//* TestTIdSipHeadersFilter Published methods **********************************

procedure TestTIdSipHeadersFilter.TestAdd;
var
  Route: TIdSipRouteHeader;
  OriginalCount: Cardinal;
begin
  OriginalCount := Self.Filter.Count;

  Route := TIdSipRouteHeader.Create;
  try
    Route.Value := '<sip:127.0.0.1>';

    Self.Filter.Add(Route);
    CheckEquals(OriginalCount + 1, Self.Filter.Count, 'Count after Add');
  finally
    Route.Free;
  end;
end;

procedure TestTIdSipHeadersFilter.TestCount;
begin
  CheckEquals(2, Self.Filter.Count, 'Count with two headers');

  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.2>';
  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.3>';
  CheckEquals(4, Self.Filter.Count, 'Count with newly added headers');
end;

procedure TestTIdSipHeadersFilter.TestFirst;
begin
  Self.Headers.Clear;
  Self.Headers.First;
  CheckNull(Self.Filter.CurrentHeader,
            'First element of an empty collection');

  Self.Headers.Add(Self.Filter.HeaderName);
  Self.Headers.First;
  Self.Filter.First;
  Check(Self.Headers.CurrentHeader = Self.Filter.CurrentHeader,
  'First element of a non-empty collection');
end;

procedure TestTIdSipHeadersFilter.TestIsEmpty;
begin
  Check(not Self.Filter.IsEmpty, 'IsEmpty with 2 Route headers');
  Self.Headers.Remove(Self.Headers[RouteHeader]);
  Self.Headers.Remove(Self.Headers[RouteHeader]);
  Check(Self.Filter.IsEmpty, 'IsEmpty with no Route headers');

  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.2>';
  Check(not Self.Filter.IsEmpty, 'IsEmpty after Headers.Add(RouteHeader)');

  Self.Headers.Clear;
  Check(Self.Filter.IsEmpty, 'IsEmpty after Headers.Clear');
end;

procedure TestTIdSipHeadersFilter.TestIsEqualToFilter;
var
  H: TIdSipHeaders;
  F: TIdSipHeadersFilter;
begin
  Self.Filter.RemoveAll;

  H := TIdSipHeaders.Create;
  try
    F := TIdSipHeadersFilter.Create(H, RouteHeader);
    try
      Check(Self.Filter.IsEqualTo(F), 'Empty path = Empty path');

      H.Add(F.HeaderName).Value := '<127.0.0.1:1>';

      Check(not Self.Filter.IsEqualTo(F), 'Empty path <> non-empty path');

      H.Add(RouteHeader).Value := '<127.0.0.1:2>';

      Self.Filter.Add(F);
      Check(Self.Filter.IsEqualTo(F), 'Identical paths');

      Self.Filter.Items[Self.Filter.Count - 1].Value := '<127.0.0.1:1>';
      Check(not Self.Filter.IsEqualTo(F), 'Last header differs');
    finally
      F.Free;
    end;
  finally
    H.Free;
  end;
end;

procedure TestTIdSipHeadersFilter.TestIsEqualToHeaders;
var
  H: TIdSipHeaders;
begin
  Self.Filter.RemoveAll;

  H := TIdSipHeaders.Create;
  try
    Check(Self.Filter.IsEqualTo(H), 'Empty set = Empty set');

    H.Add(RouteHeader).Value := '<127.0.0.1:1>';

    Check(not Self.Filter.IsEqualTo(H), 'Empty set <> non-empty set');

    H.Add(RouteHeader).Value := '<127.0.0.1:2>';

    Self.Filter.Add(H);
    Check(Self.Filter.IsEqualTo(H), 'Identical sets');

    Self.Filter.Items[Self.Filter.Count - 1].Value := '<127.0.0.1:1>';
    Check(not Self.Filter.IsEqualTo(H), 'Last header differs');
  finally
    H.Free;
  end;
end;

procedure TestTIdSipHeadersFilter.TestItems;
begin
  CheckEquals('wsfrank <sip:192.168.1.43>', Self.Filter.Items[0].Value, '1st Route');
  CheckEquals('localhost <sip:127.0.0.1>',  Self.Filter.Items[1].Value, '2nd Route');

  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.2>';
  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.3>';
  CheckEquals('<sip:127.0.0.2>',  Self.Filter.Items[2].Value, '3rd Route');
  CheckEquals('<sip:127.0.0.3>',  Self.Filter.Items[3].Value, '4h Route');
end;

procedure TestTIdSipHeadersFilter.TestIteratorVisitsAllHeaders;
var
  Expected: TIdSipHeaders;
begin
  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.2>';
  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.3>';

  Self.Filter.First;
  while Self.Filter.HasNext do begin
    Self.Filter.CurrentHeader.Params['foo'] := 'bar';
    Self.Filter.Next;
  end;

  Expected := TIdSipHeaders.Create;
  try
    Expected.Add(MaxForwardsHeader).Value := '70';
    Expected.Add(RouteHeader).Value := 'wsfrank <sip:192.168.1.43>;foo=bar';
    Expected.Add(ContentLengthHeaderFull).Value := '29';
    Expected.Add(RouteHeader).Value := 'localhost <sip:127.0.0.1>;foo=bar';
    Expected.Add(RouteHeader).Value := '<sip:127.0.0.2>;foo=bar';
    Expected.Add(RouteHeader).Value := '<sip:127.0.0.3>;foo=bar';
    Check(Expected.IsEqualTo(Self.Headers), 'Not all (Route) headers visited');
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipHeadersFilter.TestRemove;
var
  Route: TIdSipRouteHeader;
begin
  Route := TIdSipRouteHeader.Create;
  try
    Route.Value := '<sip:127.0.0.1>';
    CheckEquals(2, Self.Filter.Count, 'Count with two headers');

    Self.Filter.Add(Route);
    CheckEquals(3, Self.Filter.Count, 'Count after Add');

    Self.Filter.Remove(Self.Headers[Self.Filter.HeaderName]);
    CheckEquals(2, Self.Filter.Count, 'Count after Remove');

    Self.Filter.Remove(Self.Headers[ContentLengthHeaderFull]);
    CheckEquals(2, Self.Filter.Count, 'Count after Remove''ing a non-Route header');
  finally
    Route.Free;
  end;
end;

procedure TestTIdSipHeadersFilter.TestRemoveAll;
begin
  Self.Filter.RemoveAll;
  CheckEquals(0, Self.Filter.Count, 'Route headers not removed');
end;

//******************************************************************************
//* TestTIdSipViaPath                                                          *
//******************************************************************************
//* TestTIdSipViaPath Public methods *******************************************

procedure TestTIdSipViaPath.SetUp;
begin
  inherited SetUp;

  Self.Headers := TIdSipHeaders.Create;
  Self.Path := TIdSipViaPath.Create(Self.Headers);
end;

procedure TestTIdSipViaPath.TearDown;
begin
  Self.Path.Free;
  Self.Headers.Free;

  inherited TearDown;
end;

//* TestTIdSipViaPath Published methods ****************************************

procedure TestTIdSipViaPath.TestAddAndLastHop;
var
  Hop: TIdSipViaHeader;
begin
  CheckEquals(0, Self.Path.Length, 'Has hops, but is newly created');

  Hop := Self.Headers.Add(ViaHeaderFull) as TIdSipViaHeader;

  Hop.SentBy       := '127.0.0.1';
  Hop.Port       := 5060;
  Hop.SipVersion := 'SIP/2.0';
  Hop.Transport  := sttSCTP;

  CheckEquals(1, Self.Path.Length, 'Has no hops after Add');

  CheckEquals(ViaHeaderFull, Self.Path.LastHop.Name,       'Name');
  CheckEquals('127.0.0.1',   Self.Path.LastHop.SentBy,     'SentBy');
  CheckEquals(5060,          Self.Path.LastHop.Port,       'Port');
  CheckEquals('SIP/2.0',     Self.Path.LastHop.SipVersion, 'SipVersion');
  Check      (sttSCTP =      Self.Path.LastHop.Transport,  'Transport');
end;

procedure TestTIdSipViaPath.TestClear;
begin
  CheckEquals(0, Self.Headers.Count, 'Unexpected headers');
  Self.Headers.Add(ViaHeaderFull);
  Self.Headers.Add(ViaHeaderFull);
  Self.Headers.Add(ViaHeaderFull);
  CheckEquals(3, Self.Headers.Count, 'After 3 Add()s');

  Self.Path.Clear;
  CheckEquals(0, Self.Headers.Count, 'After Clear()');
end;

procedure TestTIdSipViaPath.TestRemoveLastHop;
begin
  (Self.Headers.Add(ViaHeaderFull) as TIdSipViaHeader).SentBy := '1';
  (Self.Headers.Add(ViaHeaderFull) as TIdSipViaHeader).SentBy := '2';
  (Self.Headers.Add(ViaHeaderFull) as TIdSipViaHeader).SentBy := '3';

  CheckEquals('1', Self.Path.LastHop.SentBy, 'Sanity check');
  Self.Path.RemoveLastHop;
  CheckEquals('2', (Self.Headers.Items[0] as TIdSipViaHeader).SentBy, 'new LastHop');
  CheckEquals('3', (Self.Headers.Items[1] as TIdSipViaHeader).SentBy, 'First Hop');
end;

initialization
  RegisterTest('SIP Message Headers', Suite);
end.
