{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipUri;

interface

uses
  IdSipMessage, TestFramework, TestFrameworkSip;

type
  TestTIdSipHostAndPort = class(TTestCase)
  private
    HP:   TIdSipHostAndPort;
    Port: Cardinal;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetDefaultPort;
    procedure TestSetValueHost;
    procedure TestSetValueHostWithColonAndNoPort;
    procedure TestSetValueHostWithDefaultPort;
    procedure TestSetValueHostWithMalformedPort;
    procedure TestSetValueHostWithPort;
    procedure TestSetValueIPv4Address;
    procedure TestSetValueIPv4AddressWithDefaultPort;
    procedure TestSetValueIPv4AddressWithPort;
    procedure TestSetValueIPv6Address;
    procedure TestSetValueIPv6AddressWithDefaultPort;
    procedure TestSetValueIPv6AddressWithPort;
  end;

  TestTIdUri = class(TTestCase)
  private
    DirectoryPath:     String;
    EmptySegmentPath:  String;
    FilePath:          String;
    NoPath:            String;
    RootPath:          String;
    Uri:               TIdUri;
    UTF8EncodedNihongo: String;

    procedure CheckPath(BaseUri: String; Path: String);
    procedure CheckPaths(BaseUri: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateUri;
    procedure TestDecode;
    procedure TestEncode;
    procedure TestEraseUserInfo;
    procedure TestHasValidSyntax;
    procedure TestHasValidSyntaxAddressChecks;
    procedure TestHasValidSyntaxUserPasswordChecks;
    procedure TestHasValidSyntaxSchemeChecks;
    procedure TestIsFragment;
    procedure TestIsPChar;
    procedure TestIsQuery;
    procedure TestIsScheme;
    procedure TestIsSipUri;
    procedure TestIsSipsUri;
    procedure TestSetSchemeNormalisesScheme;
    procedure TestSetUriNormalisesScheme;
    procedure TestSetUriIllegalScheme;
    procedure TestUriWithAuthority;
    procedure TestUriWithFragment;
    procedure TestUriWithNoAuthority;
    procedure TestUriWithQuery;
    procedure TestUriWithQueryAndFragment;
    procedure TestUriWithSipUri;
    procedure TestUriWithUserInfoInAuthority;
    procedure TestWeirdUris;
    procedure TestWellFormedPercentEncoding;
  end;

  TestTIdSipUri = class(TTestCaseSip)
  private
    EqualityA: TIdSipUri;
    EqualityB: TIdSipUri;
    Uri:       TIdSipUri;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsRouteHeader;
    procedure TestAsStringFancyParameters;
    procedure TestAsStringHeadersWithNoParameters;
    procedure TestAsStringHeadersWithParameters;
    procedure TestAsStringNormalParameters;
    procedure TestAsStringNoUsername;
    procedure TestAsStringUsernameNeedsEscaping;
    procedure TestAsStringWithDefaultPort;
    procedure TestAsStringWithPassword;
    procedure TestAsStringWithSpecialPort;
    procedure TestBasicGetUri;
    procedure TestBasicSetUri;
    procedure TestCanonicaliseAsAddress;
    procedure TestCanonicaliseAsAddressOfRecord;
    procedure TestCanonicaliseAsAddressOfRecordSips;
    procedure TestClearHeaders;
    procedure TestClearParameters;
    procedure TestCreateRequest;
    procedure TestCreateRequestWithBody;
    procedure TestCreateRequestWithDangerousHeaders;
    procedure TestCreateRequestWithMethodParam;
    procedure TestCreateRequestWithUnknownParams;
    procedure TestCreateSetsUri;
    procedure TestEqualsBasic;
    procedure TestEqualsDefaultPortVersusSpecified;
    procedure TestEqualsDefaultSecureTransportVersusSpecified;
    procedure TestEqualsDefaultTransportVersusSpecified;
    procedure TestEqualsDisjointParameterSets;
    procedure TestEqualsEscapedCharsInUsername;
    procedure TestEqualsHeaderOrderIrrelevant;
    procedure TestEqualsHeadersDiffer;
    procedure TestEqualsHostDiffers;
    procedure TestEqualsHostIsCaseInsensitive;
    procedure TestEqualsIsNotTransitive;
    procedure TestEqualsMethodParamIsCaseSensitive;
    procedure TestEqualsParamOrderIrrelevant;
    procedure TestEqualsParamsDiffer;
    procedure TestEqualsPasswordDiffers;
    procedure TestEqualsPortDiffers;
    procedure TestEqualsSchemeDiffers;
    procedure TestEqualsSchemeIsCaseInsensitive;
    procedure TestEqualsSecureTransportVersusSips;
    procedure TestEqualsUsernameDiffers;
    procedure TestEqualsUsernameParameter;
    procedure TestEraseUserInfo;
    procedure TestGetSetMaddr;
    procedure TestGetSetMethod;
    procedure TestGetSetTransport;
    procedure TestGetSetTTL;
    procedure TestGetSetUserParam;
    procedure TestGetUriWithDefaultPortSpecified;
    procedure TestGrid;
    procedure TestHasGrid;
    procedure TestHasHeaders;
    procedure TestHasMaddr;
    procedure TestHasMethod;
    procedure TestHasParameter;
    procedure TestIsGruu;
    procedure TestIsLooseRoutable;
    procedure TestIsMalformedBadHeader;
    procedure TestIsPassword;
    procedure TestIsParamNameOrValue;
    procedure TestIsUser;
    procedure TestIsSecure;
    procedure TestMaddr;
    procedure TestMalformedUris;
    procedure TestOpaque;
    procedure TestPasswordIsUriEncoded;
    procedure TestPasswordEncode;
    procedure TestPortIsSpecified;
    procedure TestPortWithSipScheme;
    procedure TestPortWithSipsScheme;
    procedure TestRemoveParameter;
    procedure TestSchemeChangeChangesPort;
    procedure TestSchemeChangeDoesntChangeSpecifiedPort;
    procedure TestSetUriBasicUri;
    procedure TestSetUriMalformedUserinfo;
    procedure TestSetUriMultipleHeaders;
    procedure TestSetUriMultipleParameters;
    procedure TestSetUriOneHeader;
    procedure TestSetUriOneParameter;
    procedure TestSetUriTortureParameters;
    procedure TestSetUriUserHasEscapedChars;
    procedure TestSetUriValuelessParameter;
    procedure TestSetUriWithEscapedCharacters;
    procedure TestSetUriWithIPv6;
    procedure TestSetUriWithNoUser;
    procedure TestSetUriWithPassword;
    procedure TestSetUriWithPort;
    procedure TestSetUriWithTrailingSpaces;
    procedure TestSetUriWithWeirdUser;
    procedure TestSetUriClearsOldValues;
    procedure TestTransport;
    procedure TestUnparsedValue;
    procedure TestUserInfoWithEscapedCharacters;
    procedure TestUserIsIP;
    procedure TestUserIsPhone;
  end;
{
  TestTIdSipsUri = class(TTestCase)
  published
    procedure TestDefaultPort;
    procedure TestDefaultTransport;
    procedure TestMalformedUris;
    procedure TestIsSecure;
  end;
}
implementation

uses
  Classes, IdSimpleParser, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipUri unit tests');
  Result.AddTest(TestTIdSipHostAndPort.Suite);
  Result.AddTest(TestTIdUri.Suite);
  Result.AddTest(TestTIdSipUri.Suite);
//  Result.AddTest(TestTIdSipsUri.Suite);
end;

//******************************************************************************
//* TestTIdSipHostAndPort                                                      *
//******************************************************************************
//* TestTIdSipHostAndPort Public methods ***************************************

procedure TestTIdSipHostAndPort.SetUp;
begin
  inherited SetUp;

  Self.HP := TIdSipHostAndPort.Create;
  Self.HP.DefaultPort := 1234;

  Self.Port := Self.HP.DefaultPort + 100;
end;

procedure TestTIdSipHostAndPort.TearDown;
begin
  Self.HP.Free;

  inherited TearDown;
end;

//* TestTIdSipHostAndPort Published methods ************************************

procedure TestTIdSipHostAndPort.TestSetDefaultPort;
const
  HostOnly = 'foo.com';
begin
  Self.HP.Value := HostOnly;
  Self.HP.DefaultPort := 1;
  CheckEquals(HostOnly, Self.HP.Value, 'Default port set after value');

  Self.HP.DefaultPort := 2;
  CheckEquals(HostOnly, Self.HP.Value, 'Default port re-set');
end;

procedure TestTIdSipHostAndPort.TestSetValueHost;
const
  FooComHost = 'foo.com';
begin
  Self.HP.Value := FooComHost;

  CheckEquals(FooComHost,
              Self.HP.Host,
              'Host');
  CheckEquals(Self.HP.DefaultPort,
              Self.HP.Port,
              'Port');
  Check(not Self.HP.PortIsSpecified,
              'PortIsSpecified');
  CheckEquals(FooComHost,
              Self.HP.Value,
              'GetValue');
end;

procedure TestTIdSipHostAndPort.TestSetValueHostWithColonAndNoPort;
const
  MissingPortString = 'foo.com:';
begin
  try
    Self.HP.Value := MissingPortString;
    Fail('Failed to bail out on malformed port');
  except
    on E: EParserError do begin
      Check(Pos(MissingPortString, E.Message) > 0,
            'Uninformative error message');
    end;
  end;
end;

procedure TestTIdSipHostAndPort.TestSetValueHostWithDefaultPort;
begin
  Self.HP.Value := 'foo.com:' + IntToStr(Self.HP.DefaultPort);

  CheckEquals('foo.com',
              Self.HP.Host,
              'Host');
  CheckEquals(Self.HP.DefaultPort,
              Self.HP.Port,
              'Port');
  Check(Self.HP.PortIsSpecified,
              'PortIsSpecified');
  CheckEquals('foo.com:' + IntToStr(Self.HP.Port),
              Self.HP.Value,
              'GetValue');
end;

procedure TestTIdSipHostAndPort.TestSetValueHostWithMalformedPort;
const
  MalformedPortString = 'foo.com:a';
begin
  try
    Self.HP.Value := MalformedPortString;
    Fail('Failed to bail out on malformed port');
  except
    on E: EParserError do begin
      Check(Pos(MalformedPortString, E.Message) > 0,
            'Uninformative error message');
    end;
  end;
end;

procedure TestTIdSipHostAndPort.TestSetValueHostWithPort;
begin
  Self.HP.Value := 'foo.com:' + IntToStr(Self.Port);

  CheckEquals('foo.com',
              Self.HP.Host,
              'Host');
  CheckEquals(Self.Port,
              Self.HP.Port,
              'Port');
  Check(Self.HP.PortIsSpecified,
              'PortIsSpecified');
  CheckEquals('foo.com:' + IntToStr(Self.HP.Port),
              Self.HP.Value,
              'GetValue');
end;

procedure TestTIdSipHostAndPort.TestSetValueIPv4Address;
begin
  Self.HP.Value := '127.0.0.1';

  CheckEquals('127.0.0.1',
              Self.HP.Host,
              'Host');
  CheckEquals(Self.HP.DefaultPort,
              Self.HP.Port,
              'Port');
  Check(not Self.HP.PortIsSpecified,
              'PortIsSpecified');
  CheckEquals('127.0.0.1',
              Self.HP.Value,
              'GetValue');
end;

procedure TestTIdSipHostAndPort.TestSetValueIPv4AddressWithDefaultPort;
begin
  Self.HP.Value := '127.0.0.1:' + IntToStr(Self.HP.DefaultPort);

  CheckEquals('127.0.0.1',
              Self.HP.Host,
              'Host');
  CheckEquals(Self.HP.DefaultPort,
              Self.HP.Port,
              'Port');
  Check(Self.HP.PortIsSpecified,
              'PortIsSpecified');
  CheckEquals('127.0.0.1:' + IntToStr(Self.HP.Port),
              Self.HP.Value,
              'GetValue');
end;

procedure TestTIdSipHostAndPort.TestSetValueIPv4AddressWithPort;
begin
  Self.HP.Value := '127.0.0.1:' + IntToStr(Self.Port);

  CheckEquals('127.0.0.1',
              Self.HP.Host,
              'Host');
  CheckEquals(Self.Port,
              Self.HP.Port,
              'Port');
  Check(Self.HP.PortIsSpecified,
              'PortIsSpecified');
  CheckEquals('127.0.0.1:' + IntToStr(Self.HP.Port),
              Self.HP.Value,
              'GetValue');
end;

procedure TestTIdSipHostAndPort.TestSetValueIPv6Address;
begin
  Self.HP.Value := '[1::127.0.0.1]';

  CheckEquals('[1::127.0.0.1]',
              Self.HP.Host,
              'Host');
  CheckEquals(Self.HP.DefaultPort,
              Self.HP.Port,
              'Port');
  Check(not Self.HP.PortIsSpecified,
              'PortIsSpecified');
  CheckEquals('[1::127.0.0.1]',
              Self.HP.Value,
              'GetValue');
end;

procedure TestTIdSipHostAndPort.TestSetValueIPv6AddressWithDefaultPort;
begin
  Self.HP.Value := '[1::127.0.0.1]:' + IntToStr(Self.HP.DefaultPort);

  CheckEquals('[1::127.0.0.1]',
              Self.HP.Host,
              'Host');
  CheckEquals(Self.HP.DefaultPort,
              Self.HP.Port,
              'Port');
  Check(Self.HP.PortIsSpecified,
              'PortIsSpecified');
  CheckEquals('[1::127.0.0.1]:' + IntToStr(Self.HP.Port),
              Self.HP.Value,
              'GetValue');
end;

procedure TestTIdSipHostAndPort.TestSetValueIPv6AddressWithPort;
begin
  Self.HP.Value := '[1::127.0.0.1]:' + IntToStr(Self.Port);

  CheckEquals('[1::127.0.0.1]',
              Self.HP.Host,
              'Host');
  CheckEquals(Self.Port,
              Self.HP.Port,
              'Port');
  Check(Self.HP.PortIsSpecified,
              'PortIsSpecified');
  CheckEquals('[1::127.0.0.1]:' + IntToStr(Self.HP.Port),
              Self.HP.Value,
              'GetValue');
end;

//******************************************************************************
//* TestTIdUri                                                                 *
//******************************************************************************
//* TestTIdUri Public methods **************************************************

procedure TestTIdUri.SetUp;
begin
  inherited SetUp;

  Self.Uri := TIdUri.Create('');

  Self.EmptySegmentPath   := '/path//nowhere/';
  Self.NoPath             := '';
  Self.RootPath           := '/';
  Self.DirectoryPath      := '/path/to/nowhere/';
  Self.FilePath           := Self.DirectoryPath + 'index.html';
  Self.UTF8EncodedNihongo := #$E6#$97#$A5#$E6#$9C#$AC#$E8#$AA#$9E;
end;

procedure TestTIdUri.TearDown;
begin
  Self.Uri.Free;

  inherited TearDown;
end;

//* TestTIdUri Private methods *************************************************

procedure TestTIdUri.CheckPath(BaseUri: String; Path: String);
var
  TestUri: String;
begin
  TestUri := BaseUri + Path;
  Self.Uri.Uri := TestUri;

  CheckEquals(Path, Self.Uri.Path, 'Path: ' + TestUri);
end;

procedure TestTIdUri.CheckPaths(BaseUri: String);
begin
  CheckPath(BaseUri, Self.NoPath);
  CheckPath(BaseUri, Self.RootPath);
  CheckPath(BaseUri, Self.DirectoryPath);
  CheckPath(BaseUri, Self.FilePath);
  CheckPath(BaseUri, Self.EmptySegmentPath)
end;

//* TestTIdUri Published methods ***********************************************

procedure TestTIdUri.TestCreateUri;
var
  Uri: TIdUri;
begin
  Uri := TIdUri.CreateUri('sip:wintermute@tessier-ashpool.co.luna');
  try
    CheckEquals(TIdSipUri.ClassName,
                Uri.ClassName,
                'SIP URI');
  finally
    Uri.Free;
  end;

  Uri := TIdUri.CreateUri('sipx:wintermute@tessier-ashpool.co.luna');
  CheckEquals('sipx', Uri.Scheme, 'Incorrect scheme for unknown URI scheme');
  Check(not Uri.IsSipUri, 'URI thinks it''s a SIP URI');
end;

procedure TestTIdUri.TestDecode;
var
  C:               Char;
  InvalidEncoding: String;
begin
  for C := #$00 to #$FF do
    CheckEquals(C, TIdUri.Decode('%' + IntToHex(Ord(C), 2)), 'Decoding %' + IntToHex(Ord(C), 2));

  CheckEquals('abc',
              TIdUri.Decode('%61%62%63'),
              'Decode multi-character string');

  InvalidEncoding := '%XX';
  try
    TIdUri.Decode(InvalidEncoding);
    Fail('Failed to bail on decoding "' + InvalidEncoding + '"');
  except
    on EParserError do;
  end;
end;

procedure TestTIdUri.TestEncode;
begin
  CheckEquals('%61%62%63',
              TIdUri.Encode('abc', []),
              'Encode all characters');
  CheckEquals('%61%62%63',
              TIdUri.Encode('abc', Digits),
              'Leave out digits');
  CheckEquals('%61b%63',
              TIdUri.Encode('abc', ['b']),
              'Leave out the b');
  CheckEquals('%61bc',
              TIdUri.Encode('abc', ['b', 'c']),
              'Leave out the b and the c');
  CheckEquals('abc',
              TIdUri.Encode('abc', Alphabet),
              'Leave out all alphabet characters');
end;

procedure TestTIdUri.TestEraseUserInfo;
begin
  Self.Uri.Uri := 'http://foo:bar@baz';
  Self.Uri.EraseUserInfo;

  CheckEquals('', Self.Uri.UserInfo, 'UserInfo not erased');
end;

procedure TestTIdUri.TestHasValidSyntax;
begin
  Check(not TIdUri.HasValidSyntax(''), 'The empty string is not a URI');
  Check(not TIdUri.HasValidSyntax('x'), 'Just a scheme, no colon');
end;

procedure TestTIdUri.TestHasValidSyntaxAddressChecks;
begin
  Check(TIdUri.HasValidSyntax('sip:127.0.0.1'), 'IPv4 address');
  Check(TIdUri.HasValidSyntax('sip:[::1]'), 'IPv6 reference');
  Check(TIdUri.HasValidSyntax('sip:example.com'), 'Fully qualified domain name');

  Check(not TIdUri.HasValidSyntax('sip:::1'),
        'Only IPv6 REFERENCES are valid as a host.');

  Check(not TIdUri.HasValidSyntax('sip:foo.1example.com'),
        Self.Uri.Uri + ': Tokens in a FQDN may not start with a digit.');
end;

procedure TestTIdUri.TestHasValidSyntaxUserPasswordChecks;
begin
  Check(TIdUri.HasValidSyntax('sip:foo@bar'), 'Well-formed URI');

  Check(not TIdUri.HasValidSyntax('sip:foo@@bar'),
        'User token cannot contain an @');

  Check(not TIdUri.HasValidSyntax('sip:localhostjoe127.0.0.1'),
        'The user likely left out an @. The remaining URI is invalid because '
      + 'some of the FQDN''s labels start with digits.');

  Check(TIdUri.HasValidSyntax('sip:b:@bar'),
        'A colon indicates the presence of a password, and it''s the empty password');

  Check(not TIdUri.HasValidSyntax('sip:b"@bar'),
        'DQUOT is an illegal character in a user token.');

  Check(not TIdUri.HasValidSyntax('sip:b:"@bar'),
        'A DQUOT is an illegal character in a password token.');
end;

procedure TestTIdUri.TestHasValidSyntaxSchemeChecks;
begin
  Check(TIdUri.HasValidSyntax('sip:127.0.0.1'), 'SIP scheme');
  Check(TIdUri.HasValidSyntax('sips:127.0.0.1'), 'SIPS scheme');
  Check(TIdUri.HasValidSyntax('sipp:127.0.0.1'), 'Unknown scheme: "sipp"');

  Check(not TIdUri.HasValidSyntax('sip127.0.0.1'),
        'The user likely left out the colon terminating the scheme.');
end;

procedure TestTIdUri.TestIsFragment;
begin
  Check(not TIdUri.IsFragment(''), '''''');
  Check(not TIdUri.IsFragment('%'), '%');
  Check(TIdUri.IsFragment('~'), '~');

  Check(TIdUri.IsFragment('%20'), '%20');
  Check(TIdUri.IsFragment('fragment'), 'fragment');
  Check(TIdUri.IsFragment('/fragment?'), '/fragment?');
  Check(TIdUri.IsFragment(':@!$&''()*+,;=-._~'), ':@!$&''()*+,;=-._~');
end;

procedure TestTIdUri.TestIsPChar;
begin
  Check(not TIdUri.IsPChar(''), '''''');
  Check(not TIdUri.IsPChar('%'), '%');

  Check(TIdUri.IsPChar('%20'), '%20');
  Check(TIdUri.IsPChar('word'), 'word');
  Check(TIdUri.IsPChar(':@!$&''()*+,;=-._~'), ':@!$&''()*+,;=-._~');
end;

procedure TestTIdUri.TestIsQuery;
begin
  Check(not TIdUri.IsQuery(''), '''''');
  Check(not TIdUri.IsQuery('%'), '%');
  Check(TIdUri.IsQuery('~'), '~');

  Check(TIdUri.IsQuery('%20'), '%20');
  Check(TIdUri.IsQuery('query'), 'query');
  Check(TIdUri.IsQuery('/query?'), '/query?');
  Check(TIdUri.IsQuery(':@!$&''()*+,;=-._~'), ':@!$&''()*+,;=-._~');
end;

procedure TestTIdUri.TestIsScheme;
begin
  Check(not TIdUri.IsScheme(''),          '''''');
  Check(not TIdUri.IsScheme('%'),         '%');
  Check(not TIdUri.IsScheme('1sip'),      '1sip');
  Check(    TIdUri.IsScheme('sip-2.0+3'), 'sip-2.0+3');
  Check(    TIdUri.IsScheme('http'),      'http');
end;

procedure TestTIdUri.TestIsSipUri;
begin
  Self.Uri.Scheme := 'http';
  Check(not Self.Uri.IsSipUri, 'http');

  Self.Uri.Scheme := SipScheme;
  Check(Self.Uri.IsSipUri, SipScheme);

  Self.Uri.Scheme := SipsScheme;
  Check(Self.Uri.IsSipUri, SipsScheme);
end;

procedure TestTIdUri.TestIsSipsUri;
begin
  Self.Uri.Scheme := 'http';
  Check(not Self.Uri.IsSipsUri, 'http');

  Self.Uri.Scheme := SipScheme;
  Check(not Self.Uri.IsSipsUri, SipScheme);

  Self.Uri.Scheme := SipsScheme;
  Check(Self.Uri.IsSipsUri, SipsScheme);
end;

procedure TestTIdUri.TestSetSchemeNormalisesScheme;
const
  NormalisedScheme = 'http';
begin
  Self.Uri.Scheme := NormalisedScheme;
  CheckEquals(NormalisedScheme, Self.Uri.Scheme, NormalisedScheme);

  Self.Uri.Scheme := Uppercase(NormalisedScheme);
  CheckEquals(NormalisedScheme, Self.Uri.Scheme, Uppercase(NormalisedScheme));
end;

procedure TestTIdUri.TestSetUriNormalisesScheme;
begin
  Self.Uri.Uri := 'HTTP://foo';
  CheckEquals('http', Self.Uri.Scheme, 'Scheme not normalised');
end;

procedure TestTIdUri.TestSetUriIllegalScheme;
begin
  Self.Uri.Uri := '%%:illegalUri';
  Check(Self.Uri.IsMalformed, 'URI not marked as malformed');
  CheckEquals(InvalidScheme, Self.Uri.ParseFailReason, 'URI has unexpected malformed reason');
end;

procedure TestTIdUri.TestUriWithAuthority;
const
  WeirdAuthority = 'abc-._~!$&''()*+,;=';
begin
  Self.Uri.Uri := 'http://foo/';
  CheckEquals('foo', Self.Uri.Host, 'Host foo');

  Self.Uri.Uri := 'http://foo.bar/';
  CheckEquals('foo.bar', Self.Uri.Host, 'Host foo.bar');

  Self.Uri.Uri := 'http://127.0.0.1/';
  CheckEquals('127.0.0.1', Self.Uri.Host, 'Host 127.0.0.1');

  Self.Uri.Uri := 'http://[::1]/';
  CheckEquals('[::1]', Self.Uri.Host, 'Host [::1]');

  Self.Uri.Uri := 'name://' + TIdUri.Encode(Self.UTF8EncodedNihongo, []) + '/foo';
  CheckEquals(EncodeNonLineUnprintableChars(Self.UTF8EncodedNihongo),
              EncodeNonLineUnprintableChars(Self.Uri.Host),
              'Percent-encoded UTF-8 hostname');

  Self.Uri.Uri := 'name://' + WeirdAuthority;
  CheckEquals(WeirdAuthority, Self.Uri.Host, 'Weird authority');

  CheckPaths('http://foo');
end;

procedure TestTIdUri.TestUriWithFragment;
const
  BaseUri           = 'http://foo/';
  NormalFragment    = 'bar';
  MalformedFragment = '[bar]';
begin
  Self.Uri.Uri := BaseUri + '#' + NormalFragment;
  CheckEquals(NormalFragment, Self.Uri.Fragment, 'Normal fragment');
  Check(not Self.Uri.IsMalformed, 'URI with normal fragment marked as malformed');

  Self.Uri.Uri := BaseUri + '#' + MalformedFragment;
  CheckEquals(MalformedFragment, Self.Uri.Fragment, 'Malformed fragment');
  Check(Self.Uri.IsMalformed, 'URI with malformed fragment not marked as malformed');
end;

procedure TestTIdUri.TestUriWithNoAuthority;
begin
  CheckPaths('x:');
end;

procedure TestTIdUri.TestUriWithQuery;
const
  BaseUri        = 'http://foo/';
  NormalQuery    = 'bar';
  MalformedQuery = '[bar]';
begin
  Self.Uri.Uri := BaseUri + '?' + NormalQuery;
  CheckEquals(NormalQuery, Self.Uri.Query, 'Normal query');
  Check(not Self.Uri.IsMalformed, 'URI with normal query marked as malformed');

  Self.Uri.Uri := BaseUri + '?' + MalformedQuery;
  CheckEquals(MalformedQuery, Self.Uri.Query, 'Malformed query');
  Check(Self.Uri.IsMalformed, 'URI with malformed query not marked as malformed');
end;

procedure TestTIdUri.TestUriWithQueryAndFragment;
const
  BaseUri  = 'http://foo/';
  Query    = 'bar';
  Fragment = 'baz';
begin
  Self.Uri.Uri := BaseUri + '?' + Query + '#' + Fragment;
  CheckEquals(Query,    Self.Uri.Query,    'Query');
  CheckEquals(Fragment, Self.Uri.Fragment, 'Fragment');
end;

procedure TestTIdUri.TestUriWithSipUri;
begin
  Self.Uri.Uri := 'sip:case@fried-neurons.org';
  CheckEquals('sip', Self.Uri.Scheme, 'Scheme of simple SIP URI');
  CheckEquals('case@fried-neurons.org', Self.Uri.Path, 'Path of simple SIP URI');

  Self.Uri.Uri := 'sip:case@fried-neurons.org;gruu';
  CheckEquals('sip', Self.Uri.Scheme, 'Scheme of SIP URI with parameter');
  CheckEquals('case@fried-neurons.org;gruu', Self.Uri.Path, 'Path of SIP URI with parameter');

  Self.Uri.Uri := 'sip:case@fried-neurons.org;gruu?Subject=foo';
  CheckEquals('sip', Self.Uri.Scheme, 'Scheme of SIP URI with parameter & header');
  CheckEquals('case@fried-neurons.org;gruu', Self.Uri.Path, 'Path of SIP URI with parameter & header');
  CheckEquals('Subject=foo', Self.Uri.Query, 'Query of SIP URI with parameter & header');
end;

procedure TestTIdUri.TestUriWithUserInfoInAuthority;
const
  WeirdUserInfo = '::~!&''(*)+,;=abc'#$E6;
begin
  Self.Uri.Uri := 'http://foo@bar';
  CheckEquals('foo', Self.Uri.UserInfo, 'User "foo"');

  Self.Uri.Uri := 'http://' + TIdSipUri.UsernameEncode(Self.UTF8EncodedNihongo) + '@bar';
  CheckEquals(EncodeNonLineUnprintableChars(Self.UTF8EncodedNihongo),
              EncodeNonLineUnprintableChars(Self.Uri.UserInfo),
              'User "nihongo"');

  Self.Uri.Uri := 'http://foo:bar@baz';
  CheckEquals('foo:bar', Self.Uri.UserInfo, 'User "foo" with password "bar"');

  Self.Uri.Uri := 'http://' + TIdUri.UsernameEncode(WeirdUserInfo) + '@bar';
  CheckEquals(WeirdUserInfo, Self.Uri.UserInfo, 'User "' + WeirdUserInfo + '"');

  CheckPaths('http://foo@bar');
end;

procedure TestTIdUri.TestWeirdUris;
begin
  Self.Uri.Uri := 'x:';
  CheckEquals('x', Self.Uri.Scheme, 'Scheme of "x:"');
  CheckEquals('',  Self.Uri.Path,   'Path of "x:"');
  Check(not Self.Uri.HasAuthority, 'Authority of "x:"');
end;

procedure TestTIdUri.TestWellFormedPercentEncoding;
var
  I: Integer;
begin
  Check(not TIdUri.WellFormedPercentEncoding('%'), '%');
  Check(not TIdUri.WellFormedPercentEncoding('%1'), '%1');
  Check(not TIdUri.WellFormedPercentEncoding('%fx'), '%fx');
  Check(not TIdUri.WellFormedPercentEncoding('%%00'), '%%00');

  Check(TIdUri.WellFormedPercentEncoding(''), 'The empty string');

  for I := 0 to $ff do
    Check(TIdUri.WellFormedPercentEncoding('%' + IntToHex(I, 2)), '%' + IntToHex(I, 2));

  Check(TIdUri.WellFormedPercentEncoding('%00%01%02%03'), '%00%01%02%03');
end;

//******************************************************************************
//* TestTIdSipUri                                                              *
//******************************************************************************
//* TestTIdSipUri Public methods ***********************************************

procedure TestTIdSipUri.SetUp;
begin
  inherited SetUp;

  Self.EqualityA := TIdSipUri.Create('');
  Self.EqualityB := TIdSipUri.Create('');
  Self.Uri       := TIdSipUri.Create('');
end;

procedure TestTIdSipUri.TearDown;
begin
  Self.EqualityB.Free;
  Self.EqualityA.Free;
  Self.Uri.Free;

  inherited TearDown;
end;

//* TestTIdSipUri Published methods ********************************************

procedure TestTIdSipUri.TestAsRouteHeader;
var
  Route: TIdSipRouteHeader;
begin
  Self.Uri.Uri := 'sip:proxy.tessier-ashpool.co.luna';
  Route := Self.Uri.AsRouteHeader;
  try
    CheckEquals(Self.Uri.Uri,
                Route.Address.Uri,
                'URI of route');
  finally
    Route.Free;
  end;
end;

procedure TestTIdSipUri.TestAsStringFancyParameters;
begin
  Self.Uri.Scheme   := 'sip';
  Self.Uri.Username := 'wintermute';
  Self.Uri.Host     := 'tessier-ashpool.co.luna';
  Self.Uri.AddParameter('foo', '<b%61r>');

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna;foo=%3Cbar%3E',
              Self.Uri.AsString,
              'AsString');
  CheckEquals(Self.Uri.AsString,
              Self.Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringHeadersWithNoParameters;
begin
  Self.Uri.Scheme   := 'sip';
  Self.Uri.Username := 'wintermute';
  Self.Uri.Host     := 'tessier-ashpool.co.luna';
  Self.Uri.Headers.Add(RouteHeader).Value := '<sip:127.0.0.1>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna?Route=%3Csip:127.0.0.1%3E',
              Self.Uri.AsString,
              'AsString');
  CheckEquals(Self.Uri.AsString,
              Self.Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringHeadersWithParameters;
begin
  Self.Uri.Scheme   := 'sip';
  Self.Uri.Username := 'wintermute';
  Self.Uri.Host     := 'tessier-ashpool.co.luna';
  Self.Uri.Headers.Add(RouteHeader).Value := '<sip:127.0.0.1>';
  Self.Uri.AddParameter('foo', '<bar>');
  Self.Uri.AddParameter('lr');
  Self.Uri.AddParameter('baz', 'quaax');

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna'
            + ';foo=%3Cbar%3E;lr;baz=quaax?Route=%3Csip:127.0.0.1%3E',
              Self.Uri.AsString,
              'AsString');
  CheckEquals(Self.Uri.AsString,
              Self.Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringNormalParameters;
begin
  Self.Uri.Scheme   := 'sip';
  Self.Uri.Username := 'wintermute';
  Self.Uri.Host     := 'tessier-ashpool.co.luna';
  Self.Uri.AddParameter('foo', 'bar');

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna;foo=bar',
              Self.Uri.AsString,
              'AsString');
  CheckEquals(Self.Uri.AsString,
              Self.Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringNoUsername;
begin
  Self.Uri.Scheme   := 'sip';
  Self.Uri.Username := '';
  Self.Uri.Host     := 'tessier-ashpool.co.luna';

  CheckEquals('sip:tessier-ashpool.co.luna',
              Self.Uri.AsString,
              'AsString');
  CheckEquals(Self.Uri.AsString,
              Self.Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringUsernameNeedsEscaping;
begin
  Self.Uri.Uri      := 'sip:company.com';
  Self.Uri.Username := 'sip:user@example.com';

  CheckEquals('sip:sip%3Auser%40example.com@company.com',
              Self.Uri.AsString,
              'AsString');
  CheckEquals(Self.Uri.AsString,
              Self.Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringWithDefaultPort;
begin
  Self.Uri.Scheme   := 'sip';
  Self.Uri.Username := 'wintermute';
  Self.Uri.Host     := 'tessier-ashpool.co.luna';
  Self.Uri.Port     := DefaultSipPort;

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna:5060',
              Self.Uri.AsString,
              'AsString');
  CheckEquals(Self.Uri.AsString,
              Self.Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringWithPassword;
begin
  Self.Uri.Scheme   := 'sip';
  Self.Uri.Username := 'wintermute';
  Self.Uri.Password := 'song';
  Self.Uri.Host     := 'tessier-ashpool.co.luna';

  CheckEquals('sip:wintermute:song@tessier-ashpool.co.luna',
              Self.Uri.AsString,
              'AsString');
  CheckEquals(Self.Uri.AsString,
              Self.Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringWithSpecialPort;
begin
  Self.Uri.Scheme   := 'sip';
  Self.Uri.Username := 'wintermute';
  Self.Uri.Port     := DefaultSipPort + 10000;
  Self.Uri.Host     := 'tessier-ashpool.co.luna';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna:15060',
              Self.Uri.AsString,
              'AsString');
  CheckEquals(Self.Uri.AsString,
              Self.Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestBasicGetUri;
begin
  Self.Uri.Scheme   := 'sip';
  Self.Uri.Username := 'wintermute';
  Self.Uri.Host     := 'tessier-ashpool.co.luna';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',
              Self.Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestBasicSetUri;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  CheckEquals('sip',                     Self.Uri.Scheme,        'Scheme');
  CheckEquals('wintermute',              Self.Uri.Username,      'User');
  CheckEquals('tessier-ashpool.co.luna', Self.Uri.Host,          'Host');
  CheckEquals(DefaultSipPort,                Self.Uri.Port,          'Port');
  CheckEquals(0,                         Self.Uri.ParamCount,    'Parameters');
  CheckEquals('',                        Self.Uri.Password,      'Password');
  CheckEquals(0,                         Self.Uri.Headers.Count, 'Headers');
end;

procedure TestTIdSipUri.TestCanonicaliseAsAddress;
const
  UnknownParam = 'unknown';
var
  AOR: String;
begin
  AOR := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.Uri.Uri := AOR;
  CheckEquals(AOR,
              Self.Uri.CanonicaliseAsAddress,
              'Normal URI');

  Self.Uri.Uri  := AOR;
  Self.Uri.Port := 5060;
  CheckEquals(AOR,
              Self.Uri.CanonicaliseAsAddress,
              'URI with port');

  Self.Uri.Uri := AOR;
  Self.Uri.Password := 'FooingTheBar';
  CheckEquals(AOR,
              Self.Uri.CanonicaliseAsAddress,
              'URI with password');

  Self.Uri.Uri := AOR;
  Self.Uri.AddParameter(MethodParam);
  Self.Uri.AddParameter(MaddrParam);
  Self.Uri.AddParameter(TTLParam);
  Self.Uri.AddParameter(TransportParam);
  Self.Uri.AddParameter(LooseRoutableParam);
  Self.Uri.AddParameter(UnknownParam);
  CheckEquals(AOR + ';' + UnknownParam,
              Self.Uri.CanonicaliseAsAddress,
              'URI with forbidden params and one unknown param');

  Self.Uri.Uri := AOR;
  Self.Uri.Headers.Add(ExpiresHeader);
  CheckEquals(AOR,
              Self.Uri.CanonicaliseAsAddress,
              'URI with header');

  AOR := 'sip:<wintermute>@tessier-ashpool.co.luna';
  Self.Uri.Uri := 'sip:%3c%77intermute%3e@tessier-ashpool.co.luna';
  CheckEquals(AOR,
              Self.Uri.CanonicaliseAsAddress,
              'URI with escaped characters in user name');
end;

procedure TestTIdSipUri.TestCanonicaliseAsAddressOfRecord;
var
  AOR: String;
begin
  AOR := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.Uri.Uri := AOR;
  CheckEquals(AOR,
              Self.Uri.CanonicaliseAsAddressOfRecord,
              'Normal URI');

  Self.Uri.Uri := AOR;
  Self.Uri.Password := 'FooingTheBar';
  CheckEquals(AOR,
              Self.Uri.CanonicaliseAsAddressOfRecord,
              'URI with password');

  Self.Uri.AddParameter('Foo', 'Bar');
  CheckEquals(AOR,
              Self.Uri.CanonicaliseAsAddressOfRecord,
              'URI with parameter');

  Self.Uri.Headers.Add(ExpiresHeader);
  CheckEquals(AOR,
              Self.Uri.CanonicaliseAsAddressOfRecord,
              'URI with header');

  AOR := 'sip:<wintermute>@tessier-ashpool.co.luna';
  Self.Uri.Uri := 'sip:%3c%77intermute%3e@tessier-ashpool.co.luna';
  CheckEquals(AOR,
              Self.Uri.CanonicaliseAsAddressOfRecord,
              'URI with escaped characters in user name');
end;

procedure TestTIdSipUri.TestCanonicaliseAsAddressOfRecordSips;
var
  AOR: String;
begin
  AOR := 'sips:wintermute@tessier-ashpool.co.luna';
  Self.Uri.Uri := AOR;

  CheckEquals(AOR,
              Self.Uri.CanonicaliseAsAddressOfRecord,
              'SIPS URI');
end;

procedure TestTIdSipUri.TestClearHeaders;
begin
  Self.Uri.Headers.Add(ExpiresHeader);
  Self.Uri.Headers.Add(RouteHeader);
  Self.Uri.ClearHeaders;
  CheckEquals(0,
              Self.Uri.Headers.Count,
              'Headers not cleared');
end;

procedure TestTIdSipUri.TestClearParameters;
begin
  Self.Uri.AddParameter(ExpiresHeader, '0');
  Self.Uri.AddParameter(RouteHeader, '<sip:127.0.0.1>');
  Self.Uri.ClearParameters;
  CheckEquals(0,
              Self.Uri.ParamCount,
              'Parameters not cleared');
end;

procedure TestTIdSipUri.TestCreateRequest;
var
  Request: TIdSipRequest;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  Request := Self.Uri.CreateRequest;
  try
    CheckEquals(Self.Uri.AsString,
                Request.RequestUri.AsString,
                'Request-URI');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipUri.TestCreateRequestWithBody;
var
  Body:    String;
  Request: TIdSipRequest;
begin
  Body := 'I am a plain text body';

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna?'
                + BodyHeaderFake + '=' + TIdSipUri.HeaderEncode(Body);

  Request := Self.Uri.CreateRequest;
  try
    CheckEquals(Body,
                Request.Body,
                'Body not set');
    Check(Request.IsMalformed,
          'An INVITE with a body, but no Content-Length or Content-Type '
        + 'header, must be malformed');
    Check(not Request.HasHeader(BodyHeaderFake),
          'Fake body header included in the request');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipUri.TestCreateRequestWithDangerousHeaders;
var
  Request: TIdSipRequest;
begin
  // We add the headers in the order they occur in section 19.1.5 of RFC 3261
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.Uri.Headers.Add(FromHeaderFull).Value       := 'sip:case@fried.neurons.org';
  Self.Uri.Headers.Add(CallIDHeaderFull).Value     := '1';
  Self.Uri.Headers.Add(CSeqHeader).Value           := '0 INVITE';
  Self.Uri.Headers.Add(ViaHeaderFull).Value        := 'SIP/2.0/TCP gw.tessier-ashpool.co.luna';
  Self.Uri.Headers.Add(RecordRouteHeader).Value    := '<sip:gw2.tessier-ashpool.co.luna>';
  Self.Uri.Headers.Add(RouteHeader).Value          := '<sip:gw2.tessier-ashpool.co.luna>';
  Self.Uri.Headers.Add(AcceptHeader).Value         := 'application/sdp';
  Self.Uri.Headers.Add(AcceptEncodingHeader).Value := 'gzip';
  Self.Uri.Headers.Add(AcceptLanguageHeader).Value := 'en';
  Self.Uri.Headers.Add(AllowHeader).Value          := 'REGISTER';
  Self.Uri.Headers.Add(ContactHeaderFull).Value    := 'sip:case@fried.neurons.org';
  Self.Uri.Headers.Add(OrganizationHeader).Value   := 'Foo-Using Bars';
  Self.Uri.Headers.Add(SupportedHeaderFull).Value  := '100rel';
  Self.Uri.Headers.Add(UserAgentHeader).Value      := 'Foo/1.1';

  Request := Self.Uri.CreateRequest;
  try
    CheckNotEquals(Self.Uri.Headers.Headers[FromHeaderFull].AsString,
                   Request.From.AsString,
                   'From header');
    CheckNotEquals(Self.Uri.Headers.Headers[CallIDHeaderFull].AsString,
                   Request.CallID,
                   'Call-ID header');
    CheckNotEquals(Self.Uri.Headers.Headers[CSeqHeader].AsString,
                   Request.CSeq.AsString,
                   'CSeq header');
    CheckEquals(0, Request.Path.Count, 'Via header');
    CheckEquals(0, Request.RecordRoute.Count, 'Record-Route header');
    CheckEquals(0, Request.Route.Count, 'Route header');
    Check(not Request.HasHeader(AcceptHeader),
          'Accept header');
    Check(not Request.HasHeader(AcceptEncodingHeader),
          'Accept-Encoding header');
    Check(not Request.HasHeader(AcceptLanguageHeader),
          'Accept-Language header');
    Check(not Request.HasHeader(AllowHeader),
          'Allow header');
    Check(not Request.HasHeader(ContactHeaderFull),
          'Contact header');
    Check(not Request.HasHeader(OrganizationHeader),
          'Organization header');
    Check(not Request.HasHeader(SupportedHeaderFull),
          'Supported header');
    Check(not Request.HasHeader(UserAgentHeader),
          'User-Agent header');

    Check(not Request.RequestUri.Headers.HasHeader(FromHeaderFull),
          'From header in Request-URI');
    Check(not Request.RequestUri.Headers.HasHeader(CallIDHeaderFull),
          'Call-ID header in Request-URI');
    Check(not Request.RequestUri.Headers.HasHeader(CSeqHeader),
          'CSeq header in Request-URI');
    Check(not Request.RequestUri.Headers.HasHeader(ViaHeaderFull),
          'Via header in Request-URI');
    Check(not Request.RequestUri.Headers.HasHeader(RecordRouteHeader),
          'Record-Route header in Request-URI');
    Check(not Request.RequestUri.Headers.HasHeader(RouteHeader),
          'Route header in Request-URI');
    Check(not Request.RequestUri.Headers.HasHeader(AcceptHeader),
          'Accept header in Request-URI');
    Check(not Request.RequestUri.Headers.HasHeader(AcceptEncodingHeader),
          'Accept-Encoding header in Request-URI');
    Check(not Request.RequestUri.Headers.HasHeader(AcceptLanguageHeader),
          'Accept-Language header in Request-URI');
    Check(not Request.RequestUri.Headers.HasHeader(AllowHeader),
          'Allow header in Request-URI');
    Check(not Request.RequestUri.Headers.HasHeader(ContactHeaderFull),
          'Contact header in Request-URI');
    Check(not Request.RequestUri.Headers.HasHeader(OrganizationHeader),
          'Organization header in Request-URI');
    Check(not Request.RequestUri.Headers.HasHeader(SupportedHeaderFull),
          'Supported header in Request-URI');
    Check(not Request.RequestUri.Headers.HasHeader(UserAgentHeader),
          'User-Agent header in Request-URI');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipUri.TestCreateRequestWithMethodParam;
var
  Request: TIdSipRequest;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;method=REGISTER';

  Request := Self.Uri.CreateRequest;
  try
    Check(Request.IsRegister,
                'Method');
    Check(not Request.RequestUri.HasParameter(MethodParam),
          'Method param not removed from Request-URI');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipUri.TestCreateRequestWithUnknownParams;
var
  Request: TIdSipRequest;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;foo=bar;baz';

  Request := Self.Uri.CreateRequest;
  try
    Check(Request.RequestUri.HasParameter('foo'),
          'foo param removed from Request-URI');
    Check(Request.RequestUri.HasParameter('baz'),
          'baz param removed from Request-URI');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipUri.TestCreateSetsUri;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('sip:wintermute@tessier-ashpool.co.luna');
  try
    CheckEquals('sip',                     Uri.Scheme,   'Scheme');
    CheckEquals('wintermute',              Uri.Username, 'User');
    CheckEquals('tessier-ashpool.co.luna', Uri.Host,     'Host');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestEqualsBasic;
begin
  Check(Self.EqualityA.Equals(Self.EqualityB), 'A = B, Vacuous');
  Check(Self.EqualityB.Equals(Self.EqualityA), 'B = A, Vacuous');
end;

procedure TestTIdSipUri.TestEqualsDefaultPortVersusSpecified;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna:5060';
  Self.EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  Check(not Self.EqualityA.Equals(Self.EqualityB), 'A <> B, Default port vs specified 5060');
  Check(not Self.EqualityB.Equals(Self.EqualityA), 'B <> A, Default port vs specified 5060');
end;

procedure TestTIdSipUri.TestEqualsDefaultSecureTransportVersusSpecified;
begin
  Self.EqualityA.Uri := 'sips:wintermute@tessier-ashpool.co.luna;transport=tls';
  Self.EqualityB.Uri := 'sips:wintermute@tessier-ashpool.co.luna';

  Check(not Self.EqualityA.Equals(Self.EqualityB),
        'A <> B, Default secure transport vs specified TLS');
  Check(not Self.EqualityB.Equals(Self.EqualityA),
        'B <> A, Default secure transport vs specified TLS');
end;

procedure TestTIdSipUri.TestEqualsDefaultTransportVersusSpecified;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp';
  Self.EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  Check(not Self.EqualityA.Equals(Self.EqualityB), 'A <> B, Default transport vs specified UDP');
  Check(not Self.EqualityB.Equals(Self.EqualityA), 'B <> A, Default transport vs specified UDP');
end;

procedure TestTIdSipUri.TestEqualsDisjointParameterSets;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna;newparam=5';
  Self.EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna;security=on';

  Check(Self.EqualityA.Equals(Self.EqualityB), 'A = B, Disjoint parameter sets');
  Check(Self.EqualityB.Equals(Self.EqualityA), 'B = A, Disjoint parameter sets');
end;

procedure TestTIdSipUri.TestEqualsEscapedCharsInUsername;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.EqualityB.Uri := 'sip:%77intermute@tessier-ashpool.co.luna';

  Check(Self.EqualityA.Equals(Self.EqualityB), 'A = B, Username with escaped characters');
  Check(Self.EqualityB.Equals(Self.EqualityA), 'B = A, Username with escaped characters');
end;

procedure TestTIdSipUri.TestEqualsHeaderOrderIrrelevant;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna?priority=urgent&subject=burn';
  Self.EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna?subject=burn&priority=urgent';

  Check(Self.EqualityA.Equals(Self.EqualityB), 'A = B, Header order');
  Check(Self.EqualityB.Equals(Self.EqualityA), 'B = A, Header order');
end;

procedure TestTIdSipUri.TestEqualsHeadersDiffer;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna?subject=burn';
  Self.EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna?subject=burningchrome';

  Check(not Self.EqualityA.Equals(Self.EqualityB), 'A <> B, Headers differ');
  Check(not Self.EqualityB.Equals(Self.EqualityA), 'B <> A, Header differ');
end;

procedure TestTIdSipUri.TestEqualsHostDiffers;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier.ashpool.co.luna';
  Self.EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  Check(not Self.EqualityA.Equals(Self.EqualityB), 'A <> B, Host');
  Check(not Self.EqualityB.Equals(Self.EqualityA), 'B <> A, Host');
end;

procedure TestTIdSipUri.TestEqualsHostIsCaseInsensitive;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.LUNA';

  Check(Self.EqualityA.Equals(Self.EqualityB), 'A = B, Host, Case differs');
  Check(Self.EqualityB.Equals(Self.EqualityA), 'B = A, Host, Case differs');
end;

procedure TestTIdSipUri.TestEqualsIsNotTransitive;
var
  EqualityC: TIdSipUri;
begin
  EqualityC := TIdSipUri.Create('');
  try
    Self.EqualityA.Uri := 'sip:carol@chicago.com';
    Self.EqualityB.Uri := 'sip:carol@chicago.com;security=on';
    EqualityC.Uri := 'sip:carol@chicago.com;security=off';

    Check(Self.EqualityA.Equals(Self.EqualityB), 'A <> B');
    Check(Self.EqualityB.Equals(Self.EqualityA), 'B <> A');

    Check(Self.EqualityA.Equals(EqualityC), 'A <> C');
    Check(EqualityC.Equals(Self.EqualityA), 'C <> A');

    Check(not Self.EqualityB.Equals(EqualityC), 'B = C');
    Check(not EqualityC.Equals(Self.EqualityB), 'C = B');
  finally
    EqualityC.Free;
  end;
end;

procedure TestTIdSipUri.TestEqualsMethodParamIsCaseSensitive;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna;method=REGISTER';
  Self.EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna;method=register';

  Check(not Self.EqualityA.Equals(Self.EqualityB), 'A <> B, Method param, Case differs');
  Check(not Self.EqualityB.Equals(Self.EqualityA), 'B <> A, Method param, Case differs');
end;

procedure TestTIdSipUri.TestEqualsParamOrderIrrelevant;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=tcp;ttl=1';
  Self.EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna;ttl=1;transport=tcp';

  Check(Self.EqualityA.Equals(Self.EqualityB), 'A = B, Params, different order');
  Check(Self.EqualityB.Equals(Self.EqualityA), 'B = A, Params, different order');
end;

procedure TestTIdSipUri.TestEqualsParamsDiffer;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=tcp;ttl=1';
  Self.EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna;ttl=1';

  Check(not Self.EqualityA.Equals(Self.EqualityB), 'A <> B, Params');
  Check(not Self.EqualityB.Equals(Self.EqualityA), 'B <> A, Params');

  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna;ttl=1';
  Self.EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  Check(not Self.EqualityA.Equals(Self.EqualityB), 'A <> B, Params, one has none');
  Check(not Self.EqualityB.Equals(Self.EqualityA), 'B <> A, Params, one has none');
end;

procedure TestTIdSipUri.TestEqualsPasswordDiffers;
begin
  Self.EqualityA.Uri := 'sip:wintermute:palace@tessier-ashpool.co.luna';
  Self.EqualityB.Uri := 'sip:wintermute:PALACE@tessier-ashpool.co.luna';

  Check(not Self.EqualityA.Equals(Self.EqualityB), 'A <> B, Password');
  Check(not Self.EqualityB.Equals(Self.EqualityA), 'B <> A, Password');
end;

procedure TestTIdSipUri.TestEqualsPortDiffers;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna:5060';
  Self.EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna:5061';

  Check(not Self.EqualityA.Equals(Self.EqualityB), 'A <> B, Port');
  Check(not Self.EqualityB.Equals(Self.EqualityA), 'B <> A, Port');
end;

procedure TestTIdSipUri.TestEqualsSchemeDiffers;
begin
  // Self.EqualityA has a port because SIP's default port is 5060 and SIPS' is 5061
  Self.EqualityA.Uri :=  'sip:wintermute@tessier-ashpool.co.luna:5061';
  Self.EqualityB.Uri := 'sips:wintermute@tessier-ashpool.co.luna';

  Check(not Self.EqualityA.Equals(Self.EqualityB), 'A <> B, Scheme');
  Check(not Self.EqualityB.Equals(Self.EqualityA), 'B <> A, Scheme');
end;

procedure TestTIdSipUri.TestEqualsSchemeIsCaseInsensitive;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.EqualityB.Uri := 'SIP:wintermute@tessier-ashpool.co.luna';

  Check(Self.EqualityA.Equals(Self.EqualityB), 'A = B, Scheme, Case differs');
  Check(Self.EqualityB.Equals(Self.EqualityA), 'B = A, Scheme, Case differs');
end;

procedure TestTIdSipUri.TestEqualsSecureTransportVersusSips;
begin
  Self.EqualityA.Uri :=  'sip:wintermute@tessier-ashpool.co.luna:5061;transport=tls';
  Self.EqualityB.Uri := 'sips:wintermute@tessier-ashpool.co.luna';

  Check(not Self.EqualityA.Equals(Self.EqualityB), 'A <> B, Secure transport vs. SIPS');
  Check(not Self.EqualityB.Equals(Self.EqualityA), 'B <> A, Secure transport vs. SIPS');
end;

procedure TestTIdSipUri.TestEqualsUsernameDiffers;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.EqualityB.Uri := 'sip:Wintermute@tessier-ashpool.co.luna';

  Check(not Self.EqualityA.Equals(Self.EqualityB), 'A <> B, Username');
  Check(not Self.EqualityB.Equals(Self.EqualityA), 'B <> A, Username');
end;

procedure TestTIdSipUri.TestEqualsUsernameParameter;
begin
  Self.EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.EqualityB.Uri := 'sip:tessier-ashpool.co.luna;user=wintermute';

  Check(not Self.EqualityA.Equals(Self.EqualityB), 'A <> B, Username parameter');
  Check(not Self.EqualityB.Equals(Self.EqualityA), 'B <> A, Username parameter');
end;

procedure TestTIdSipUri.TestEraseUserInfo;
begin
  Self.Uri.Username := 'hiro';
  Self.Uri.Password := 'protagonist';

  Self.Uri.EraseUserInfo;

  CheckEquals('', Self.Uri.Username, 'Username not erased');
  CheckEquals('', Self.Uri.Password, 'Password not erased');
end;

procedure TestTIdSipUri.TestGetSetMaddr;
begin
  Self.Uri.AddParameter(MaddrParam, '127.0.0.1');
  CheckEquals('127.0.0.1', Self.Uri.Maddr, 'Get Maddr');

  Self.Uri.Maddr := '0.0.0.0';
  CheckEquals('0.0.0.0', Self.Uri.ParamValue(MaddrParam), 'Set Maddr');
end;

procedure TestTIdSipUri.TestGetSetMethod;
begin
  Self.Uri.AddParameter(MethodParam, MethodInvite);
  CheckEquals(MethodInvite, Self.Uri.Method, 'Get Method');

  Self.Uri.Method := MethodRegister;
  CheckEquals(MethodRegister, Self.Uri.ParamValue(MethodParam), 'Set Method');
end;

procedure TestTIdSipUri.TestGetSetTransport;
begin
  Self.Uri.AddParameter(TransportParam, TransportParamTCP);
  CheckEquals(TransportParamTCP,
              Self.Uri.Transport,
              'Get Transport');

  Self.Uri.Transport := TransportParamSCTP;
  CheckEquals(TransportParamSCTP,
              Self.Uri.ParamValue(TransportParam),
              'Set Transport');
end;

procedure TestTIdSipUri.TestGetSetTTL;
begin
  Self.Uri.AddParameter(TTLParam, '42');
  CheckEquals(42, Self.Uri.TTL, 'Get TTL');

  Self.Uri.TTL := 13;
  CheckEquals('13', Self.Uri.ParamValue(TTLParam), 'Set TTL');
end;

procedure TestTIdSipUri.TestGetSetUserParam;
begin
  Self.Uri.AddParameter(UserParam, UserParamPhone);
  CheckEquals(UserParamPhone, Self.Uri.UserParameter, 'Get User');

  Self.Uri.UserParameter := UserParamIp;
  CheckEquals(UserParamIp, Self.Uri.ParamValue(UserParam), 'Set User');
end;

procedure TestTIdSipUri.TestGetUriWithDefaultPortSpecified;
const
  U = 'sip:wintermute@tessier-ashpool.co.luna:5060';
begin
  Self.Uri.Uri := U;
  CheckEquals(U,
              Self.Uri.Uri,
              'PortIsSpecified got lost');
end;

procedure TestTIdSipUri.TestGrid;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  CheckEquals('', Self.Uri.Grid, 'No grid present');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;grid=1234';
  CheckEquals('1234', Self.Uri.Grid, 'grid specified');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;grid=5678';
  CheckEquals('5678', Self.Uri.Grid, 'grid re-specified');
end;

procedure TestTIdSipUri.TestHasGrid;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Self.Uri.HasGrid, 'no parameters');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;grid=127.0.0.1';
  Check(Uri.HasGrid, 'grid parameter');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp;grid=127.0.0.1;lr';
  Check(Uri.HasGrid, 'grid parameter amongst others');
end;

procedure TestTIdSipUri.TestHasHeaders;
begin
  Check(not Self.Uri.HasHeaders, 'No headers');

  Self.Uri.Headers.Add(RouteHeader);
  Check(Uri.HasHeaders, 'One header');
end;

procedure TestTIdSipUri.TestHasMaddr;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Self.Uri.HasMaddr, 'no parameters');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;maddr=127.0.0.1';
  Check(Uri.HasMaddr, 'maddr parameter');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp;maddr=127.0.0.1;lr';
  Check(Uri.HasMaddr, 'maddr parameter amongst others');
end;

procedure TestTIdSipUri.TestHasMethod;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Self.Uri.Hasmethod, 'no parameters');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;method=INVITE';
  Check(Uri.Hasmethod, 'method parameter');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp;method=INVITE;lr';
  Check(Uri.Hasmethod, 'method parameter amongst others');
end;

procedure TestTIdSipUri.TestHasParameter;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Self.Uri.HasParameter('foo'), 'no parameters');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;foo';
  Check(Uri.HasParameter('foo'), 'valueless parameter');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;foo=bar';
  Check(Uri.HasParameter('foo'), 'valued parameter');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp;foo=bar;lr';
  Check(Uri.HasParameter('foo'), 'parameter amongst others');
end;

procedure TestTIdSipUri.TestIsGruu;
const
  BaseUri = 'sip:wintermute@tessier-ashpool.co.luna';
begin
  Self.Uri.Uri := BaseUri;
  Check(not Self.Uri.IsGruu, 'Looks like a GRUU, but no "gruu" parameter');

  Self.Uri.IsGruu := true;
  Check(Self.Uri.IsGruu, 'IsGruu not set');
  CheckEquals(BaseUri + ';' + GruuParam,
              Self.Uri.Uri,
              '"gruu" parameter doesn''t show up in the URI');

  Self.Uri.IsGruu := false;
  Check(not Self.Uri.IsGruu, 'IsGruu not unset');
  CheckEquals(BaseUri,
              Self.Uri.Uri,
              '"gruu" parameter not removed');

  Self.Uri.Uri := BaseUri + ';' + GruuParam;
  Check(Self.Uri.IsGruu,
        'IsGruu not set after setting Uri with a GRUU');
end;

procedure TestTIdSipUri.TestIsLooseRoutable;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Self.Uri.IsLooseRoutable, 'Non-loose-routable SIP URI marked as loose routable');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;lr';
  Check(Uri.IsLooseRoutable, 'Loose routable URI not marked as loose routable');
end;

procedure TestTIdSipUri.TestIsMalformedBadHeader;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.Uri.Headers.Add(ContactHeaderFull).Value := 'sipp:127.0.0.1';

  Self.Uri.Headers.First;
  Check(Self.Uri.Headers.CurrentHeader.IsMalformed, 'Sanity check: the first header''s not malformed!');

  Check(Self.Uri.IsMalformed, 'The URI contains a malformed header, but thinks it''s well-formed');

  Self.Uri.Headers.First;
  Self.Uri.Headers.CurrentHeader.Value := 'sip:127.0.0.1';
  Check(not Self.Uri.Headers.CurrentHeader.IsMalformed, 'Sanity check: the first header''s not well-formed!');

  Check(not Self.Uri.IsMalformed, 'The URI contains only well-formed headers, but thinks it''s malformed');
end;

procedure TestTIdSipUri.TestIsPassword;
begin
  Check(not TIdSipUri.IsPassword(';'),  ';');
  Check(not TIdSipUri.IsPassword('%'),  '%');
  Check(not TIdSipUri.IsPassword('%1'), '%1');

  Check(TIdSipUri.IsPassword('&'),          '&');
  Check(TIdSipUri.IsPassword('='),          '=');
  Check(TIdSipUri.IsPassword('+'),          '+');
  Check(TIdSipUri.IsPassword('$'),          '$');
  Check(TIdSipUri.IsPassword(','),          ',');
  Check(TIdSipUri.IsPassword('-'),          '-');
  Check(TIdSipUri.IsPassword('_'),          '_');
  Check(TIdSipUri.IsPassword('.'),          '.');
  Check(TIdSipUri.IsPassword('!'),          '!');
  Check(TIdSipUri.IsPassword('~'),          '~');
  Check(TIdSipUri.IsPassword('*'),          '*');
  Check(TIdSipUri.IsPassword(''''),         '''');
  Check(TIdSipUri.IsPassword('('),          '(');
  Check(TIdSipUri.IsPassword(')'),          ')');
  Check(TIdSipUri.IsPassword('~case101'),   '~case101');
  Check(TIdSipUri.IsPassword('~c%61se101'), '~c%61se101');
end;

procedure TestTIdSipUri.TestIsParamNameOrValue;
begin
  Check(TIdSipUri.IsParamNameOrValue('['),         '[');
  Check(TIdSipUri.IsParamNameOrValue(']'),         ']');
  Check(TIdSipUri.IsParamNameOrValue('/'),         '/');
  Check(TIdSipUri.IsParamNameOrValue(':'),         ':');
  Check(TIdSipUri.IsParamNameOrValue('&'),         '&');
  Check(TIdSipUri.IsParamNameOrValue('+'),         '+');
  Check(TIdSipUri.IsParamNameOrValue('$'),         '$');
  Check(TIdSipUri.IsParamNameOrValue('-'),         '-');
  Check(TIdSipUri.IsParamNameOrValue('_'),         '_');
  Check(TIdSipUri.IsParamNameOrValue('.'),         '.');
  Check(TIdSipUri.IsParamNameOrValue('!'),         '!');
  Check(TIdSipUri.IsParamNameOrValue('~'),         '~');
  Check(TIdSipUri.IsParamNameOrValue('*'),         '*');
  Check(TIdSipUri.IsParamNameOrValue(''''),        '''');
  Check(TIdSipUri.IsParamNameOrValue('('),         '(');
  Check(TIdSipUri.IsParamNameOrValue(')'),         ')');
  Check(TIdSipUri.IsParamNameOrValue('c%61se101'), 'c%61se101');

  Check(not TIdSipUri.IsParamNameOrValue('%'), '%');
end;

procedure TestTIdSipUri.TestIsUser;
begin
  Check(TIdSipUri.IsUser('&'),          '&');
  Check(TIdSipUri.IsUser('='),          '=');
  Check(TIdSipUri.IsUser('+'),          '+');
  Check(TIdSipUri.IsUser('$'),          '$');
  Check(TIdSipUri.IsUser(','),          ',');
  Check(TIdSipUri.IsUser(';'),          ';');
  Check(TIdSipUri.IsUser('?'),          '?');
  Check(TIdSipUri.IsUser('/'),          '/');
  Check(TIdSipUri.IsUser('-'),          '-');
  Check(TIdSipUri.IsUser('_'),          '_');
  Check(TIdSipUri.IsUser('.'),          '.');
  Check(TIdSipUri.IsUser('!'),          '!');
  Check(TIdSipUri.IsUser('~'),          '~');
  Check(TIdSipUri.IsUser('*'),          '*');
  Check(TIdSipUri.IsUser(''''),         '''');
  Check(TIdSipUri.IsUser('('),          '(');
  Check(TIdSipUri.IsUser(')'),          ')');
  Check(TIdSipUri.IsUser('~case101'),   '~case101');
  Check(TIdSipUri.IsUser('~c%61se101'), '~c%61se101');
end;

procedure TestTIdSipUri.TestIsSecure;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Self.Uri.IsSecure, 'SIP URI marked as secure');

  Self.Uri.Uri := 'sips:wintermute@tessier-ashpool.co.luna';
  Check(Uri.IsSecure, 'SIPS URI not marked as secure');
end;

procedure TestTIdSipUri.TestMaddr;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;user=ip;method=INVITE;ttl=2;maddr=127.0.0.255;lr';
  CheckEquals('127.0.0.255', Self.Uri.Maddr, 'maddr');
end;

procedure TestTIdSipUri.TestMalformedUris;
begin
  Self.Uri.Uri := ':wintermute@tessier-ashpool.co.luna';
  Check(Self.Uri.IsMalformed,
        'URI with missing scheme not marked as malformed');
  Check(Pos(InvalidScheme, Self.Uri.ParseFailReason) > 0,
        'Insufficiently informative exception (missing scheme)');

  Self.Uri.Uri := 'sip:';
  Check(Self.Uri.IsMalformed,
        'URI with missing host not marked as malformed');
  Check(Pos(MissingHost, Self.Uri.ParseFailReason) > 0,
        'Insufficiently informative exception (missing host)');

  Self.Uri.Uri := 'sip:1host';
  Check(Self.Uri.IsMalformed,
        'URI with invalid host info not marked as malformed');
  Check(Pos(InvalidHost, Self.Uri.ParseFailReason) > 0,
        'Insufficiently informative exception (invalid host info)');

  Self.Uri.Uri := 'sip:<@host';
  Check(Self.Uri.IsMalformed,
        'URI with invalid user info not marked as malformed');
  Check(Pos(InvalidUserInfo, Self.Uri.ParseFailReason) > 0,
        'Insufficiently informative exception (invalid user info)');

  Self.Uri.Uri := 'sip:case:%1@host';
  Check(Self.Uri.IsMalformed,
        'URI with invalid password info not marked as malformed');
  Check(Pos(InvalidUserInfo, Self.Uri.ParseFailReason) > 0,
        'Insufficiently informative exception (invalid password info)');

  Self.Uri.Uri := 'sip:case@friedneurons.org;f%%=tls';
  Check(Self.Uri.IsMalformed,
        'URI with malformed parameter name not marked as malformed');
end;

procedure TestTIdSipUri.TestOpaque;
const
  Opaque    = 'foo';
  NewOpaque = 'bar';
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;opaque=' + Opaque;
  CheckEquals(Opaque,
              Self.Uri.Opaque,
              'After SetValue');

  Self.Uri.Opaque := NewOpaque;
  CheckEquals(NewOpaque,
              Self.Uri.Opaque,
              'After SetOpaque');
end;

procedure TestTIdSipUri.TestPasswordIsUriEncoded;
const
  Host            = 'bar';
  IllegalPassword = 'contains+illegal+characters:@@@';
  User            = 'foo';
begin
  Self.Uri.Password := IllegalPassword;
  Self.Uri.Host     := Host;
  Self.Uri.Scheme   := SipScheme;
  Self.Uri.Username := User;

  CheckEquals('sip:' + User + ':' + TIdSipUri.PasswordEncode(IllegalPassword) + '@' + Host,
              Self.Uri.AsString,
              'Password not properly encoded');
end;

procedure TestTIdSipUri.TestPasswordEncode;
begin
  CheckEquals('',      TIdSipUri.PasswordEncode(''),      'The empty string');
  CheckEquals('foo',   TIdSipUri.PasswordEncode('foo'),   'foo');
  CheckEquals('%20',   TIdSipUri.PasswordEncode(' '),     'The space character');
  CheckEquals('&=+$,', TIdSipUri.PasswordEncode('&=+$,'), '&=+$,');
end;

procedure TestTIdSipUri.TestPortIsSpecified;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Self.Uri.PortIsSpecified, 'No port specified (default)');

  Self.Uri.Port := Self.Uri.DefaultPort;
  Check(Uri.PortIsSpecified, 'Port set to default value');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna:5060';

  Check(Uri.PortIsSpecified, 'Port specified');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna:5060';
  Check(Uri.PortIsSpecified, 'Port specified');
end;

procedure TestTIdSipUri.TestPortWithSipScheme;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  CheckEquals(DefaultSipPort, Self.Uri.Port, 'SIP URI default port');
end;

procedure TestTIdSipUri.TestPortWithSipsScheme;
begin
  Self.Uri.Uri := 'sips:wintermute@tessier-ashpool.co.luna';
  CheckEquals(DefaultSipsPort, Self.Uri.Port, 'SIPS URI default port');
end;

procedure TestTIdSipUri.TestRemoveParameter;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;security=on;transport=tcp';
  Self.Uri.RemoveParameter('foo');
  CheckEquals(2, Self.Uri.ParamCount, 'Wrong parameter was removed (removing foo)');

  Self.Uri.RemoveParameter('security');
  Check(Uri.HasParameter('transport'),
        'transport parameter was removed, not security');
  Check(not Self.Uri.HasParameter('security'),
        'security parameter was not removed');

  Self.Uri.RemoveParameter('transport');
  Check(not Self.Uri.HasParameter('transport'),
        'transport parameter was not removed');
end;

procedure TestTIdSipUri.TestSchemeChangeChangesPort;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  CheckEquals(DefaultSipPort, Self.Uri.Port, 'Sanity check on SIP scheme port');

  Self.Uri.Scheme := SipsScheme;
  CheckEquals(DefaultSipsPort,
              Self.Uri.Port,
              'Changing to SIPS scheme didn''t affect port');
  Check(not Self.Uri.PortIsSpecified,
        'Changing the scheme doesn''t mean you''ve specified a port');

  Self.Uri.Scheme := SipScheme;
  CheckEquals(DefaultSipPort,
              Self.Uri.Port,
              'Changing back to SIP scheme didn''t affect port');
  Check(not Self.Uri.PortIsSpecified,
        'Changing the scheme back to SIP doesn''t mean you''ve specified a port');
end;

procedure TestTIdSipUri.TestSchemeChangeDoesntChangeSpecifiedPort;
var
  Port: Integer;
begin
  Port := 8888;

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna:' + IntToStr(Port);

  Self.Uri.Scheme := SipsScheme;
  CheckEquals(Port,
              Self.Uri.Port,
              'Changing to SIPS scheme changed the specified port');
  Check(Self.Uri.PortIsSpecified,
        'You did specify the port');
end;

procedure TestTIdSipUri.TestSetUriBasicUri;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  CheckEquals('sip',                     Self.Uri.Scheme,     'Scheme');
  CheckEquals('wintermute',              Self.Uri.Username,   'User');
  CheckEquals('tessier-ashpool.co.luna', Self.Uri.Host,       'Host');
  CheckEquals(0,                         Self.Uri.ParamCount, 'Parameter count');
end;

procedure TestTIdSipUri.TestSetUriMalformedUserinfo;
begin
 Self.Uri.Uri := 'sip:foo @bar';
 Check(Self.Uri.IsMalformed, 'URI not marked as malformed');
end;

procedure TestTIdSipUri.TestSetUriMultipleHeaders;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna'
           + '?Route=%3Csip:127.0.0.1%3E&Route=%3Csip:127.0.0.2%3E';

  CheckEquals(2,                 Self.Uri.Headers.Count,          'Header count');
  CheckEquals('Route',           Self.Uri.Headers.Items[0].Name,  'Header 1 name');
  CheckEquals('<sip:127.0.0.1>', Self.Uri.Headers.Items[0].Value, 'Header 1 value');
  CheckEquals(TIdSipRouteHeader.ClassName,
              Self.Uri.Headers.Items[0].ClassName,
              'Header 1 class type');
  CheckEquals('Route',           Self.Uri.Headers.Items[1].Name,  'Header 2 name');
  CheckEquals('<sip:127.0.0.2>', Self.Uri.Headers.Items[1].Value, 'Header 2 value');
  CheckEquals(TIdSipRouteHeader.ClassName,
              Self.Uri.Headers.Items[1].ClassName,
              'Header 2 class type');
end;

procedure TestTIdSipUri.TestSetUriMultipleParameters;
const
  Param1 = 'foo';
  Param2 = 'baz';
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;foo=bar;baz=quaax';

  CheckEquals(2, Self.Uri.ParamCount, 'Parameter count');

  Check(Self.Uri.HasParameter(Param1), 'Parameter 1 not added');
  Check(Self.Uri.HasParameter(Param2), 'Parameter 2 not added');

  CheckEquals('bar',   Self.Uri.ParamValue(Param1), 'Parameter 1 value');
  CheckEquals('quaax', Self.Uri.ParamValue(Param2), 'Parameter 2 value');
end;

procedure TestTIdSipUri.TestSetUriOneHeader;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna?Route=%3Csip:127.0.0.1%3E';

  CheckEquals(1,                 Self.Uri.Headers.Count,          'Header count');
  CheckEquals('Route',           Self.Uri.Headers.Items[0].Name,  'Header name');
  CheckEquals('<sip:127.0.0.1>', Self.Uri.Headers.Items[0].Value, 'Header value');
  CheckEquals(TIdSipRouteHeader.ClassName,
              Self.Uri.Headers.Items[0].ClassName,
              'Header class type');
end;

procedure TestTIdSipUri.TestSetUriOneParameter;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;foo=bar';

  CheckEquals(1, Self.Uri.ParamCount, 'Parameter count');
  Check(Self.Uri.HasParameter('foo'),  'Parameter not added');
  CheckEquals('bar', Self.Uri.ParamValue('foo'), 'Parameter value');
end;

procedure TestTIdSipUri.TestSetUriTortureParameters;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;lr;foo=bar;baz=qu%61ax';

  CheckEquals(3, Self.Uri.ParamCount, 'Parameter count');

  Check(Self.Uri.HasParameter('lr'), 'Parameter 1 not added');
  CheckEquals('', Self.Uri.ParamValue('lr'), 'Parameter 1 value');

  Check(Self.Uri.HasParameter('foo'),  'Parameter 2 not added');
  CheckEquals('bar', Self.Uri.ParamValue('foo'), 'Parameter 2 value');

  Check(Self.Uri.HasParameter('baz'),  'Parameter 3 not added');
  CheckEquals('quaax', Self.Uri.ParamValue('baz'), 'Parameter 3 value');
end;

procedure TestTIdSipUri.TestSetUriUserHasEscapedChars;
begin
  Self.Uri.Uri := 'sip:sip%3Auser%40example.com@company.com;other-param=summit';

  CheckEquals('sip:user@example.com', Self.Uri.Username, 'Username');
end;

procedure TestTIdSipUri.TestSetUriValuelessParameter;
const
  Name = 'lr';
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;' + Name;

  CheckEquals(1,  Self.Uri.ParamCount,         'Parameter count');
  Check(          Self.Uri.HasParameter(Name), 'Parameter not added');
  CheckEquals('', Self.Uri.ParamValue(Name),   'Parameter value');
end;

procedure TestTIdSipUri.TestSetUriWithEscapedCharacters;
begin
  Self.Uri.Uri := 'sip:%77intermute@tessier-ashpool.co.luna';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',
              Self.Uri.Uri,
              '(Needlessly) escaped characters in username');
end;

procedure TestTIdSipUri.TestSetUriWithIPv6;
begin
  Self.Uri.Uri := 'sip:[2002:DEAD:BEEF:1::1]:15060';

  CheckEquals('[2002:DEAD:BEEF:1::1]',
              Self.Uri.Host,
              'Host');
  CheckEquals(15060,
              Self.Uri.Port,
              'Port');
  CheckEquals('sip:[2002:DEAD:BEEF:1::1]:15060',
              Self.Uri.Uri,
              'IPv6 host with a port');
end;

procedure TestTIdSipUri.TestSetUriWithNoUser;
begin
  Self.Uri.Uri := 'sip:tessier-ashpool.co.luna';

  CheckEquals('tessier-ashpool.co.luna', Self.Uri.Host,     'Host');
  CheckEquals('',                        Self.Uri.Username, 'User');
end;

procedure TestTIdSipUri.TestSetUriWithPassword;
begin
  Self.Uri.Uri := 'sip:wintermute:foo@tessier-ashpool.co.lu';

  CheckEquals('foo', Self.Uri.Password, 'Password');

  Self.Uri.Uri := 'sip:wintermute:%20@tessier-ashpool.co.lu';
  CheckEquals(' ', Self.Uri.Password, 'Encoded characters not decoded');
end;

procedure TestTIdSipUri.TestSetUriWithPort;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna:100';

  CheckEquals(100, Self.Uri.Port, 'Port');
end;

procedure TestTIdSipUri.TestSetUriWithTrailingSpaces;
const
  WintermutesUri = 'sip:wintermute@tessier-ashpool.co.luna';
begin
  Self.Uri.Uri := WintermutesUri + '   ';

  CheckEquals(WintermutesUri,
              Self.Uri.Uri,
              'Trailing whitespace wasn''t ignored');
end;

procedure TestTIdSipUri.TestSetUriWithWeirdUser;
begin
  Self.Uri.Uri := 'sip:;cic=0333@pstngateway';

  CheckEquals(';cic=0333', Self.Uri.Username, 'Username');
end;

procedure TestTIdSipUri.TestSetUriClearsOldValues;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp';
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  CheckEquals(Uri.DefaultTransport,
              Self.Uri.Transport,
              'Transport not cleared after SetUri');
end;

procedure TestTIdSipUri.TestTransport;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  CheckEquals('udp', Self.Uri.Transport, 'Default transport');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp';
  CheckEquals('udp', Self.Uri.Transport, 'UDP specified');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=tcp';
  CheckEquals('tcp', Self.Uri.Transport, 'TCP specified');
end;

procedure TestTIdSipUri.TestUnparsedValue;
const
  InvalidUri = ':foo';
begin
  Self.Uri.Uri := InvalidUri;
  CheckEquals(InvalidUri,
              Self.Uri.UnparsedValue,
              'UnparsedValue');
  Check(Self.Uri.IsMalformed,
        'URI not marked as being malformed');
  Check(Pos(InvalidScheme, Self.Uri.ParseFailReason) > 0,
        'ParseFailReason not explanatory enough');
end;

procedure TestTIdSipUri.TestUserInfoWithEscapedCharacters;
const
  UserInfo = 'sip:christer2@192.168.0.221';
begin
  Self.Uri.Uri := 'sip:217.13.240.136';
  Self.Uri.Username := UserInfo;

  Check(not Self.Uri.IsMalformed,
        'Setting Username means the actual userinfo may have escaped '
      + 'characters, so should NEVER trigger IsMalformed.');
  CheckEquals(UserInfo, Self.Uri.Username, 'Username not set properly');
end;

procedure TestTIdSipUri.TestUserIsIP;
begin
  Check(not Self.Uri.UserIsIP, 'no user parameter');

  Self.Uri.UserParameter := UserParamPhone;
  Check(not Self.Uri.UserIsIP, UserParamPhone);

  Self.Uri.UserParameter := UserParamIp;
  Check(Uri.UserIsIP, UserParamIp);

  Self.Uri.UserParameter := Uppercase(UserParamIp);
  Check(Uri.UserIsIP, Uppercase(UserParamIp));
end;

procedure TestTIdSipUri.TestUserIsPhone;
begin
  Check(not Self.Uri.UserIsPhoneNumber, 'no user parameter');

  Self.Uri.UserParameter := UserParamIp;
  Check(not Self.Uri.UserIsPhoneNumber, UserParamIp);

  Self.Uri.UserParameter := UserParamPhone;
  Check(Uri.UserIsPhoneNumber, UserParamPhone);

  Self.Uri.UserParameter := Uppercase(UserParamPhone);
  Check(Uri.UserIsPhoneNumber, Uppercase(UserParamPhone));
end;
{
//******************************************************************************
//* TestTIdSipsUri                                                             *
//******************************************************************************
//* TestTIdSipsUri Published methods *******************************************

procedure TestTIdSipsUri.TestDefaultPort;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipsUri.Create('sips:wintermute@tessier-ashpool.co.luna');
  try
    CheckEquals(DefaultSipsPort, Uri.Port, 'Port not specified');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipsUri.TestDefaultTransport;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipsUri.Create('sips:wintermute@tessier-ashpool.co.luna');
  try
    CheckEquals('tls', Uri.Transport, 'Transport not specified');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipsUri.TestIsSecure;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipsUri.Create('sips:wintermute@tessier-ashpool.co.luna');
  try
    Check(Uri.IsSecure, 'SIPS URI not secure');
  finally
    Uri.Free;
  end;
end;
}
initialization
  RegisterTest('SIP URIs', Suite);
end.
