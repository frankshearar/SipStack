unit TestIdSipMessage;

interface

uses
  IdSipMessage, TestFramework, TestFrameworkEx;

type
  TestTIdSipHeader = class(TTestCase)
  private
    H: TIdSipHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestGetSetParam;
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

  TestTIdSipContactHeader = class(TTestCase)
  private
    C: TIdSipContactHeader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetSetExpires;
    procedure TestGetSetQ;
    procedure TestValueWithExpires;
    procedure TestValueWithQ;
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
    procedure TestValueWithTag;
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
    procedure TestBranch;
    procedure TestIsEqualTo;
    procedure TestMaddr;
    procedure TestReceived;
    procedure TestTTL;
    procedure TestValue;
    procedure TestValueWithBranch;
    procedure TestValueWithMaddr;
    procedure TestValueWithReceived;
    procedure TestValueWithTTL;
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

implementation

uses
  Classes, IdSipParser, SysUtils, TestIdSipParser;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipMessage tests');
  Result.AddTest(TestTIdSipHeader.Suite);
  Result.AddTest(TestTIdSipAddressHeader.Suite);
  Result.AddTest(TestTIdSipContactHeader.Suite);
  Result.AddTest(TestTIdSipCSeqHeader.Suite);
  Result.AddTest(TestTIdSipDateHeader.Suite);
  Result.AddTest(TestTIdSipFromToHeader.Suite);
  Result.AddTest(TestTIdSipMaxForwardsHeader.Suite);
  Result.AddTest(TestTIdSipNumericHeader.Suite);
  Result.AddTest(TestTIdSipViaHeader.Suite);
  Result.AddTest(TestTIdSipHeaders.Suite);
  Result.AddTest(TestTIdSipPath.Suite);
  Result.AddTest(TestTIdSipRequest.Suite);
  Result.AddTest(TestTIdSipResponse.Suite);
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

procedure TestTIdSipHeader.TestGetSetParam;
begin
  CheckEquals('', Self.H.Params['branch'], 'Value of non-existent param');

  Self.H.Params['branch'] := '';
  CheckEquals('', Self.H.Params['branch'], 'Value of param with empty string value');

  Self.H.Params['branch'] := 'f00';
  CheckEquals('f00', Self.H.Params['branch'], 'Value of param with non-empty string value');
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
    on EBadHeader do;
  end;

  try
    Self.A.DecodeQuotedStr('hello\');
    Fail('Failed to bail out on a malformed string, ''hello\''');
  except
    on EBadHeader do;
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
  Self.A.Value := 'sip:wintermute@tessier-ashpool.co.lu';

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

  Self.A.Value := '"Count Zero \\\\\\\"" <sips:countzero@jacks-bar.com>';
  CheckEquals('sips:countzero@jacks-bar.com', Self.A.Address.GetFullURI,  '4: Address');
  CheckEquals('Count Zero \\\"',              Self.A.DisplayName,         '4: Name');
  CheckEquals('',                             Self.A.ParamsAsString,      '4: Params');
  CheckEquals('"Count Zero \\\\\\\"" <sips:countzero@jacks-bar.com>',
              Self.A.Value,
              '4: Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithMalformedQuotedName;
begin
  try
    // missing close quote
    Self.A.Value := '"Count Zero <sips:countzero@jacks-bar.com>';
    Fail('Failed to bail out because of unmatched quotes #1');
  except
    on EBadHeader do;
  end;

  try
    // missing close quote
    Self.A.Value := '"Count Zero \" <sips:countzero@jacks-bar.com>';
    Fail('Failed to bail out because of unmatched quotes #2');
  except
    on EBadHeader do;
  end;

  try
    // missing close quote
    Self.A.Value := '"Count Zero \\\\\\\" <sips:countzero@jacks-bar.com>';
    Fail('Failed to bail out because of unmatched quotes #3');
  except
    on EBadHeader do;
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
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;expires=0';
  CheckEquals(0, C.Expires, 'expires=0');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;expires=666';
  CheckEquals(666, C.Expires, 'expires=666');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;expires=65536';
  CheckEquals(65536, C.Expires, 'expires=65536');

  try
    Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;expires=a';
    Fail('Failed to bail out with letters');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;expires=-1';
    Fail('Failed to bail out with negative number');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;expires=';
    Fail('Failed to bail out with empty string');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipContactHeader.TestValueWithQ;
begin
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu';

  CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Self.C.Address.GetFullURI, 'Address');
  CheckEquals('',                                     Self.C.DisplayName,        'DisplayName');
  CheckEquals('',                                     Self.C.ParamsAsString,     'Params');
  CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Self.C.Value,              'Value');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;q=0';
  CheckEquals(0, C.Q, 'q=0');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;q=0.0';
  CheckEquals(0, C.Q, 'q=0.0');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;q=0.00';
  CheckEquals(0, C.Q, 'q=0.00');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;q=0.000';
  CheckEquals(0, C.Q, 'q=0.000');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;q=0.123';
  CheckEquals(123, C.Q, 'q=0.123');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;q=0.666';
  CheckEquals(666, C.Q, 'q=0.666');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;q=1';
  CheckEquals(1000, C.Q, 'q=1');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;q=1.0';
  CheckEquals(1000, C.Q, 'q=1.0');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;q=1.00';
  CheckEquals(1000, C.Q, 'q=1.00');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;q=1.000';
  CheckEquals(1000, C.Q, 'q=1.000');

  try
    Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;q=';
    Fail('Failed to bail out on empty string');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;q=a';
    Fail('Failed to bail out on letters');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;q=0.1234';
    Fail('Failed to bail out on too many digits');
  except
    on EBadHeader do;
  end;

  try
    Self.C.Value := 'sip:wintermute@tessier-ashpool.co.lu;q=1.1';
    Fail('Failed to bail out on number too big');
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

procedure TestTIdSipFromToHeader.TestGetSetTag;
begin
  Self.F.Tag := '123';
  CheckEquals('123', Self.F.Tag, '123');

  Self.F.Tag := '123abc';
  CheckEquals('123abc', Self.F.Tag, '123abc');

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
  CheckEquals('127.0.0.1',   Self.V.Host,           '1: Host');
  CheckEquals(';tag=heehee', Self.V.ParamsAsString, '1: Parameters');
  CheckEquals(IdPORT_SIP,    Self.V.Port,           '1: Port');
  CheckEquals('SIP/1.5',     Self.V.SipVersion,     '1: SipVersion');
  Check      (sttUDP =       Self.V.Transport,      '1: Transport');

  Self.V.Value := 'SIP/1.5/TLS 127.0.0.1';
  CheckEquals('127.0.0.1',     Self.V.Host,           '2: Host');
  CheckEquals('',              Self.V.ParamsAsString, '2: Parameters');
  CheckEquals(IdPORT_SIP_TLS,  Self.V.Port,           '2: Port');
  CheckEquals('SIP/1.5',       Self.V.SipVersion,     '2: SipVersion');
  Check      (sttTLS =         Self.V.Transport,      '2: Transport');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1:666;tag=heehee';
  CheckEquals('127.0.0.1',   Self.V.Host,           '3: Host');
  CheckEquals(';tag=heehee', Self.V.ParamsAsString, '3: Parameters');
  CheckEquals(666,           Self.V.Port,           '3: Port');
  CheckEquals('SIP/1.5',     Self.V.SipVersion,     '3: SipVersion');
  Check      (sttUDP =       Self.V.Transport,      '3: Transport');

  Self.V.Value := 'SIP/1.5/TLS 127.0.0.1:666;haha=heehee';
  CheckEquals('127.0.0.1',     Self.V.Host,           '4: Host');
  CheckEquals(';haha=heehee',  Self.V.ParamsAsString, '4: Parameters');
  CheckEquals(666,             Self.V.Port,           '4: Port');
  CheckEquals('SIP/1.5',       Self.V.SipVersion,     '4: SipVersion');
  Check      (sttTLS =         Self.V.Transport,      '4: Transport');

  Self.V.Value := 'SIP/1.5/TLS 127.0.0.1:666 '#13#10' ; haha=heehee';
  CheckEquals('127.0.0.1',     Self.V.Host,           '5: Host');
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
  CheckEquals(TIdSipContactHeader.ClassName,     Self.H.Add(ContactHeaderFull).ClassName,       ContactHeaderFull);
  CheckEquals(TIdSipNumericHeader.ClassName,     Self.H.Add(ContentLengthHeaderFull).ClassName, ContentLengthHeaderFull);
  CheckEquals(TIdSipCSeqHeader.ClassName,        Self.H.Add(CSeqHeader).ClassName,              CSeqHeader);
  CheckEquals(TIdSipDateHeader.ClassName,        Self.H.Add(DateHeader).ClassName,              DateHeader);
  CheckEquals(TIdSipNumericHeader.ClassName,     Self.H.Add(ExpiresHeader).ClassName,           ExpiresHeader);
  CheckEquals(TIdSipFromToHeader.ClassName,      Self.H.Add(FromHeaderFull).ClassName,          FromHeaderFull);
  CheckEquals(TIdSipMaxForwardsHeader.ClassName, Self.H.Add(MaxForwardsHeader).ClassName,       MaxForwardsHeader);
  CheckEquals(TIdSipFromToHeader.ClassName,      Self.H.Add(ToHeaderFull).ClassName,            ToHeaderFull);
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
    on EBadHeader do;
  end;
end;

procedure TestTIdSipHeaders.TestValues;
begin
  CheckEquals(0, Self.H.Count, 'Header count of newly created headers');
  CheckEquals('',
              Self.H.Values[CallIdHeaderFull],
              'Newly created header');
  CheckEquals(0, Self.H.Count, 'Requesting a value doesn''t create a new header');

  Self.H.Values[CallIdHeaderFull] := 'b';
  CheckEquals('b',
              Self.H.Values[CallIdHeaderFull],
              'Header value not set');

  Self.H.Values[ContentTypeHeaderFull] := 'Content-Type';
  CheckEquals('Content-Type',
              Self.H.Values[ContentTypeHeaderFull],
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
  Hop.Value                            := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
  Request.MaxForwards                  := 70;

  Request.Headers.Add('To').Value           := 'Wintermute <sip:wintermute@tessier-ashpool.co.lu>';
  Request.Headers.Add('From').Value         := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
  Request.CallID                            := 'a84b4c76e66710@gw1.leo-ix.org';
  Request.Headers.Add('CSeq').Value         := '314159 INVITE';
  Request.Headers.Add('Contact').Value      := '<sip:wintermute@tessier-ashpool.co.lu>';
  Request.Headers.Add('Content-Type').Value := 'text/plain';

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
  Response.StatusCode                        := 486;
  Response.StatusText                        := 'Busy Here';
  Response.SIPVersion                        := SIPVersion;
  Response.Headers.Add('Via').Value          := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
  Response.MaxForwards                       := 70;
  Response.Headers.Add('To').Value           := 'Wintermute <sip:wintermute@tessier-ashpool.co.lu>';
  Response.Headers.Add('From').Value         := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
  Response.CallID                            := 'a84b4c76e66710@gw1.leo-ix.org';
  Response.Headers.Add('CSeq').Value         := '314159 INVITE';
  Response.Headers.Add('Contact').Value      := '<sip:wintermute@tessier-ashpool.co.lu>';
  Response.Headers.Add('Content-Type').Value := 'text/plain';
  Response.ContentLength                     := 29;
  Response.Body                              := 'I am a message. Hear me roar!';

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

initialization
  RegisterTest('SIP Messages', Suite);
end.
