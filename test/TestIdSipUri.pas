unit TestIdSipUri;

interface

uses
  IdSipMessage, TestFramework;

type
  TestTIdSipUri = class(TTestCase)
  private
    EqualityA: TIdSipUri;
    EqualityB: TIdSipUri;
    Uri:       TIdSipUri;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
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
    procedure TestCanonicaliseAsAddressOfRecord;
    procedure TestCanonicaliseAsAddressOfRecordSips;
    procedure TestClearHeaders;
    procedure TestClearParameters;
    procedure TestCreateRequest;
    procedure TestCreateRequestWithDangerousHeaders;
    procedure TestCreateRequestWithMethodParam;
    procedure TestCreateRequestWithUnknownParams;
    procedure TestCreateSetsUri;
    procedure TestCreateUri;
    procedure TestDecode;
    procedure TestEncode;
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
    procedure TestHasHeaders;
    procedure TestHasParameter;
    procedure TestHasValidSyntax;
    procedure TestIsLooseRoutable;
    procedure TestIsPassword;
    procedure TestIsParamNameOrValue;
    procedure TestIsUser;
    procedure TestIsSecure;
    procedure TestMaddr;
    procedure TestPortIsSpecified;
    procedure TestPortWithSipScheme;
    procedure TestPortWithSipsScheme;
    procedure TestRemoveParameter;
    procedure TestSetUriBasicUri;
    procedure TestSetUriMultipleHeaders;
    procedure TestSetUriMultipleParameters;
    procedure TestSetUriOneHeader;
    procedure TestSetUriOneParameter;
    procedure TestSetUriTortureParameters;
    procedure TestSetUriUserHasEscapedChars;
    procedure TestSetUriValuelessParameter;
    procedure TestSetUriWithEscapedCharacters;
    procedure TestSetUriWithNoUser;
    procedure TestSetUriWithPassword;
    procedure TestSetUriWithPort;
    procedure TestSetUriClearsOldValues;
    procedure TestTransport;
    procedure TestUserIsIP;
    procedure TestUserIsPhone;
  end;
{
  TestTIdSipsUri = class(TTestCase)
  published
    procedure TestDefaultPort;
    procedure TestDefaultTransport;
    procedure TestHasValidSyntax;
    procedure TestIsSecure;
  end;
}
implementation

uses
  IdSimpleParser, IdSipConsts, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipUri unit tests');
  Result.AddTest(TestTIdSipUri.Suite);
//  Result.AddTest(TestTIdSipsUri.Suite);
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
  Self.Uri.Port     := IdPORT_SIP;

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',
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
  Self.Uri.Port     := IdPORT_SIP + 10000;
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
  CheckEquals(IdPORT_SIP,                Self.Uri.Port,          'Port');
  CheckEquals(0,                         Self.Uri.ParamCount,    'Parameters');
  CheckEquals('',                        Self.Uri.Password,      'Password');
  CheckEquals(0,                         Self.Uri.Headers.Count, 'Headers');
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

procedure TestTIdSipUri.TestCreateUri;
var
  Uri: TIdUri;
begin
  Uri := TIdSipUri.CreateUri('sip:wintermute@tessier-ashpool.co.luna');
  try
    CheckEquals(TIdSipUri.ClassName,
                Uri.ClassName,
                'SIP URI');
  finally
    Uri.Free;
  end;
{
  Uri := TIdSipUri.CreateUri('sips:wintermute@tessier-ashpool.co.luna');
  try
    CheckEquals(TIdSipsUri.ClassName,
                Uri.ClassName,
                'SIPS URI');
  finally
    Uri.Free;
  end;
}
  try
    TIdSipUri.CreateUri('sipx:wintermute@tessier-ashpool.co.luna');
    Fail('Failed to bail out on unknown/unsupported scheme');
  except
    on ESchemeNotSupported do begin
      Check(true);
    end;
  end;
end;

procedure TestTIdSipUri.TestDecode;
begin
  CheckEquals('abc',
              TIdSipUri.Decode('%61%62%63'),
              'Decode');
end;

procedure TestTIdSipUri.TestEncode;
begin
  CheckEquals('%61%62%63',
              TIdSipUri.Encode('abc', []),
              'Encode all characters');
  CheckEquals('%61%62%63',
              TIdSipUri.Encode('abc', Digits),
              'Leave out digits');
  CheckEquals('%61b%63',
              TIdSipUri.Encode('abc', ['b']),
              'Leave out the b');
  CheckEquals('%61bc',
              TIdSipUri.Encode('abc', ['b', 'c']),
              'Leave out the b and the c');
  CheckEquals('abc',
              TIdSipUri.Encode('abc', Alphabet),
              'Leave out all alphabet characters');
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

    Check(Self.EqualityA.Equals(Self.EqualityB), 'A = B');
    Check(Self.EqualityB.Equals(Self.EqualityA), 'B = A');

    Check(Self.EqualityA.Equals(EqualityC), 'A = C');
    Check(EqualityC.Equals(Self.EqualityA), 'C = A');

    Check(not Self.EqualityB.Equals(EqualityC), 'B <> C');
    Check(not EqualityC.Equals(Self.EqualityB), 'C <> B');
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

procedure TestTIdSipUri.TestHasHeaders;
begin
  Check(not Self.Uri.HasHeaders, 'No headers');

  Self.Uri.Headers.Add(RouteHeader);
  Check(Uri.HasHeaders, 'One header');
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

procedure TestTIdSipUri.TestHasValidSyntax;
begin
  Self.Uri.Uri := ':wintermute@tessier-ashpool.co.luna';
  Check(not Self.Uri.HasValidSyntax, 'Missing scheme');

  Self.Uri.Uri := 'sipx:a';
  Check(not Self.Uri.HasValidSyntax, 'Invalid scheme');

  Self.Uri.Uri := 'sip:';
  Check(not Self.Uri.HasValidSyntax, 'Missing host');

  Self.Uri.Uri := 'sip:host';
  Check(Uri.HasValidSyntax, 'Very simple but valid SIP URI');

  Self.Uri.Uri := 'sip:1host';
  Check(not Self.Uri.HasValidSyntax, 'Invalid host info');

  Self.Uri.Uri := 'sip:<@host';
  Check(not Self.Uri.HasValidSyntax, 'Invalid user info');

  Self.Uri.Uri := 'sip:case:%1@host';
  Check(not Self.Uri.HasValidSyntax, 'Invalid password info');

  Self.Uri.Uri := 'sip:case@friedneurons.org;f%%=tls';
  Check(not Self.Uri.HasValidSyntax,
        'Malformed parameter name');
end;

procedure TestTIdSipUri.TestIsLooseRoutable;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Self.Uri.IsLooseRoutable, 'Non-loose-routable SIP URI marked as loose routable');

  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;lr';
  Check(Uri.IsLooseRoutable, 'Loose routable URI not marked as loose routable');
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
  CheckEquals(IdPORT_SIP, Self.Uri.Port, 'SIP URI default port');
end;

procedure TestTIdSipUri.TestPortWithSipsScheme;
begin
  Self.Uri.Uri := 'sips:wintermute@tessier-ashpool.co.luna';
  CheckEquals(IdPORT_SIPS, Self.Uri.Port, 'SIPS URI default port');
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

procedure TestTIdSipUri.TestSetUriBasicUri;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  CheckEquals('sip',                     Self.Uri.Scheme,     'Scheme');
  CheckEquals('wintermute',              Self.Uri.Username,   'User');
  CheckEquals('tessier-ashpool.co.luna', Self.Uri.Host,       'Host');
  CheckEquals(0,                         Self.Uri.ParamCount, 'Parameter count');
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
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;foo=bar;baz=quaax';

  CheckEquals(2,       Self.Uri.ParamCount,    'Parameter count');
  CheckEquals('foo',   Self.Uri.ParamName(0),  'Parameter 1 name');
  CheckEquals('baz',   Self.Uri.ParamName(1),  'Parameter 2 name');
  CheckEquals('bar',   Self.Uri.ParamValue(0), 'Parameter 1 value');
  CheckEquals('quaax', Self.Uri.ParamValue(1), 'Parameter 2 value');
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

  CheckEquals(1,     Self.Uri.ParamCount,    'Parameter count');
  CheckEquals('foo', Self.Uri.ParamName(0),  'Parameter name');
  CheckEquals('bar', Self.Uri.ParamValue(0), 'Parameter value');
end;

procedure TestTIdSipUri.TestSetUriTortureParameters;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;lr;foo=bar;baz=qu%61ax';

  CheckEquals(3,       Self.Uri.ParamCount,    'Parameter count');
  CheckEquals('lr',    Self.Uri.ParamName(0),  'Parameter 1 name');
  CheckEquals('',      Self.Uri.ParamValue(0), 'Parameter 1 value');
  CheckEquals('foo',   Self.Uri.ParamName(1),  'Parameter 2 name');
  CheckEquals('bar',   Self.Uri.ParamValue(1), 'Parameter 2 value');
  CheckEquals('baz',   Self.Uri.ParamName(2),  'Parameter 3 name');
  CheckEquals('quaax', Self.Uri.ParamValue(2), 'Parameter 3 value');
end;

procedure TestTIdSipUri.TestSetUriUserHasEscapedChars;
begin
  Self.Uri.Uri := 'sip:sip%3Auser%40example.com@company.com;other-param=summit';

  CheckEquals('sip:user@example.com', Self.Uri.Username, 'Username');
end;

procedure TestTIdSipUri.TestSetUriValuelessParameter;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;lr';

  CheckEquals(1,    Self.Uri.ParamCount,    'Parameter count');
  CheckEquals('lr', Self.Uri.ParamName(0),  'Parameter name');
  CheckEquals('',   Self.Uri.ParamValue(0), 'Parameter value');
end;

procedure TestTIdSipUri.TestSetUriWithEscapedCharacters;
begin
  Self.Uri.Uri := 'sip:%77intermute@tessier-ashpool.co.luna';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',
              Self.Uri.Uri,
              '(Needlessly) escaped characters in username');
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
end;

procedure TestTIdSipUri.TestSetUriWithPort;
begin
  Self.Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna:100';

  CheckEquals(100, Self.Uri.Port, 'Port');
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
    CheckEquals(IdPORT_SIPS, Uri.Port, 'Port not specified');
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

procedure TestTIdSipsUri.TestHasValidSyntax;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipsUri.Create('');
  try
    Uri.Uri := 'sips:case@friedneurons.org';
    Check(Uri.HasValidSyntax,
          'SIPS URI with no specified transport');
    Uri.Uri := 'sips:case@friedneurons.org;transport=udp';
    Check(not Uri.HasValidSyntax,
          'SIPS URI may not have an unreliable transport');
    Uri.Uri := 'sips:case@friedneurons.org;transport=sctp';
    Check(not Uri.HasValidSyntax,
          'SIPS URI must have a secure transport, if any (sctp)');
    Uri.Uri := 'sips:case@friedneurons.org;transport=tcp';
    Check(not Uri.HasValidSyntax,
          'SIPS URI must have a secure transport, if any (tcp)');
    Uri.Uri := 'sips:case@friedneurons.org;transport=tls';
    Check(Uri.HasValidSyntax,
          'SIPS URI must have a reliable transport, if any (tls)');
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
