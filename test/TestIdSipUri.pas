unit TestIdSipUri;

interface

uses
  IdSipHeaders, TestFramework;

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
//    procedure TestEqualsHeaderOrderIrrelevant;
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
    procedure TestSetUriWithNoUser;
    procedure TestSetUriWithPassword;
    procedure TestSetUriWithPort;
    procedure TestSetUriClearsOldValues;
    procedure TestTransport;
    procedure TestUserIsIP;
    procedure TestUserIsPhone;
  end;

  TestTIdSipsUri = class(TTestCase)
  published
    procedure TestDefaultPort;
    procedure TestDefaultTransport;
    procedure TestHasValidSyntax;
    procedure TestIsSecure;
  end;

implementation

uses
  IdSimpleParser, IdSipConsts, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipUri unit tests');
  Result.AddTest(TestTIdSipUri.Suite);
  Result.AddTest(TestTIdSipsUri.Suite);
end;

//******************************************************************************
//* TestTIdSipUri                                                              *
//******************************************************************************
//* TestTIdSipUri Public methods ***********************************************

procedure TestTIdSipUri.SetUp;
begin
  inherited SetUp;

  EqualityA := TIdSipUri.Create('');
  EqualityB := TIdSipUri.Create('');
  Uri       := TIdSipUri.Create('');
end;

procedure TestTIdSipUri.TearDown;
begin
  EqualityB.Free;
  EqualityA.Free;
  Uri.Free;

  inherited TearDown;
end;

//* TestTIdSipUri Published methods ********************************************

procedure TestTIdSipUri.TestAsStringFancyParameters;
begin
  Uri.Scheme   := 'sip';
  Uri.Username := 'wintermute';
  Uri.Host     := 'tessier-ashpool.co.luna';
  Uri.AddParameter('foo', '<b%61r>');

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna;foo=%3Cbar%3E',
              Uri.AsString,
              'AsString');
  CheckEquals(Uri.AsString,
              Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringHeadersWithNoParameters;
begin
  Uri.Scheme   := 'sip';
  Uri.Username := 'wintermute';
  Uri.Host     := 'tessier-ashpool.co.luna';
  Uri.Headers.Add(RouteHeader).Value := '<sip:127.0.0.1>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna?Route=%3Csip:127.0.0.1%3E',
              Uri.AsString,
              'AsString');
  CheckEquals(Uri.AsString,
              Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringHeadersWithParameters;
begin
  Uri.Scheme   := 'sip';
  Uri.Username := 'wintermute';
  Uri.Host     := 'tessier-ashpool.co.luna';
  Uri.Headers.Add(RouteHeader).Value := '<sip:127.0.0.1>';
  Uri.AddParameter('foo', '<bar>');
  Uri.AddParameter('lr');
  Uri.AddParameter('baz', 'quaax');

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna'
            + ';foo=%3Cbar%3E;lr;baz=quaax?Route=%3Csip:127.0.0.1%3E',
              Uri.AsString,
              'AsString');
  CheckEquals(Uri.AsString,
              Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringNormalParameters;
begin
  Uri.Scheme   := 'sip';
  Uri.Username := 'wintermute';
  Uri.Host     := 'tessier-ashpool.co.luna';
  Uri.AddParameter('foo', 'bar');

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna;foo=bar',
              Uri.AsString,
              'AsString');
  CheckEquals(Uri.AsString,
              Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringNoUsername;
begin
  Uri.Scheme   := 'sip';
  Uri.Username := '';
  Uri.Host     := 'tessier-ashpool.co.luna';

  CheckEquals('sip:tessier-ashpool.co.luna',
              Uri.AsString,
              'AsString');
  CheckEquals(Uri.AsString,
              Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringUsernameNeedsEscaping;
begin
  Uri.Uri      := 'sip:company.com';
  Uri.Username := 'sip:user@example.com';

  CheckEquals('sip:sip%3Auser%40example.com@company.com',
              Uri.AsString,
              'AsString');
  CheckEquals(Uri.AsString,
              Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringWithDefaultPort;
begin
  Uri.Scheme   := 'sip';
  Uri.Username := 'wintermute';
  Uri.Host     := 'tessier-ashpool.co.luna';
  Uri.Port     := IdPORT_SIP;

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',
              Uri.AsString,
              'AsString');
  CheckEquals(Uri.AsString,
              Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringWithPassword;
begin
  Uri.Scheme   := 'sip';
  Uri.Username := 'wintermute';
  Uri.Password := 'song';
  Uri.Host     := 'tessier-ashpool.co.luna';

  CheckEquals('sip:wintermute:song@tessier-ashpool.co.luna',
              Uri.AsString,
              'AsString');
  CheckEquals(Uri.AsString,
              Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestAsStringWithSpecialPort;
begin
  Uri.Scheme   := 'sip';
  Uri.Username := 'wintermute';
  Uri.Port     := IdPORT_SIP + 10000;
  Uri.Host     := 'tessier-ashpool.co.luna';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna:15060',
              Uri.AsString,
              'AsString');
  CheckEquals(Uri.AsString,
              Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestBasicGetUri;
begin
  Uri.Scheme   := 'sip';
  Uri.Username := 'wintermute';
  Uri.Host     := 'tessier-ashpool.co.luna';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',
              Uri.Uri,
              'URI');
end;

procedure TestTIdSipUri.TestBasicSetUri;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  CheckEquals('sip',                     Uri.Scheme,        'Scheme');
  CheckEquals('wintermute',              Uri.Username,      'User');
  CheckEquals('tessier-ashpool.co.luna', Uri.Host,          'Host');
  CheckEquals(IdPORT_SIP,                Uri.Port,          'Port');
  CheckEquals(0,                         Uri.ParamCount,    'Parameters');
  CheckEquals('',                        Uri.Password,      'Password');
  CheckEquals(0,                         Uri.Headers.Count, 'Headers');
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

  Uri := TIdSipUri.CreateUri('sips:wintermute@tessier-ashpool.co.luna');
  try
    CheckEquals(TIdSipsUri.ClassName,
                Uri.ClassName,
                'SIPS URI');
  finally
    Uri.Free;
  end;

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
  Check(EqualityA.Equals(EqualityB), 'A = B, Vacuous');
  Check(EqualityB.Equals(EqualityA), 'B = A, Vacuous');
end;

procedure TestTIdSipUri.TestEqualsDefaultPortVersusSpecified;
begin
  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna:5060';
  EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  Check(not EqualityA.Equals(EqualityB), 'A <> B, Default port vs specified 5060');
  Check(not EqualityB.Equals(EqualityA), 'B <> A, Default port vs specified 5060');
end;

procedure TestTIdSipUri.TestEqualsDefaultSecureTransportVersusSpecified;
begin
  EqualityA.Uri := 'sips:wintermute@tessier-ashpool.co.luna;transport=tls';
  EqualityB.Uri := 'sips:wintermute@tessier-ashpool.co.luna';

  Check(not EqualityA.Equals(EqualityB),
        'A <> B, Default secure transport vs specified TLS');
  Check(not EqualityB.Equals(EqualityA),
        'B <> A, Default secure transport vs specified TLS');
end;

procedure TestTIdSipUri.TestEqualsDefaultTransportVersusSpecified;
begin
  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp';
  EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  Check(not EqualityA.Equals(EqualityB), 'A <> B, Default transport vs specified UDP');
  Check(not EqualityB.Equals(EqualityA), 'B <> A, Default transport vs specified UDP');
end;

procedure TestTIdSipUri.TestEqualsDisjointParameterSets;
begin
  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna;newparam=5';
  EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna;security=on';

  Check(EqualityA.Equals(EqualityB), 'A = B, Disjoint parameter sets');
  Check(EqualityB.Equals(EqualityA), 'B = A, Disjoint parameter sets');
end;

procedure TestTIdSipUri.TestEqualsEscapedCharsInUsername;
begin
  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  EqualityB.Uri := 'sip:%77intermute@tessier-ashpool.co.luna';

  Check(EqualityA.Equals(EqualityB), 'A = B, Username with escaped characters');
  Check(EqualityB.Equals(EqualityA), 'B = A, Username with escaped characters');
end;
{
procedure TestTIdSipUri.TestEqualsHeaderOrderIrrelevant;
begin
  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna?priority=urgent&subject=burn';
  EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna?subject=burn&priority=urgent';

  Check(EqualityA.Equals(EqualityB), 'A = B, Header order');
  Check(EqualityB.Equals(EqualityA), 'B = A, Header order');
end;
}
procedure TestTIdSipUri.TestEqualsHeadersDiffer;
begin
  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna?subject=burn';
  EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna?subject=burningchrome';

  Check(not EqualityA.Equals(EqualityB), 'A <> B, Headers differ');
  Check(not EqualityB.Equals(EqualityA), 'B <> A, Header differ');
end;

procedure TestTIdSipUri.TestEqualsHostDiffers;
begin
  EqualityA.Uri := 'sip:wintermute@tessier.ashpool.co.luna';
  EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  Check(not EqualityA.Equals(EqualityB), 'A <> B, Host');
  Check(not EqualityB.Equals(EqualityA), 'B <> A, Host');
end;

procedure TestTIdSipUri.TestEqualsHostIsCaseInsensitive;
begin
  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.LUNA';

  Check(EqualityA.Equals(EqualityB), 'A = B, Host, Case differs');
  Check(EqualityB.Equals(EqualityA), 'B = A, Host, Case differs');
end;

procedure TestTIdSipUri.TestEqualsIsNotTransitive;
var
  EqualityC: TIdSipUri;
begin
  EqualityC := TIdSipUri.Create('');
  try
    EqualityA.Uri := 'sip:carol@chicago.com';
    EqualityB.Uri := 'sip:carol@chicago.com;security=on';
    EqualityC.Uri := 'sip:carol@chicago.com;security=off';

    Check(EqualityA.Equals(EqualityB), 'A = B');
    Check(EqualityB.Equals(EqualityA), 'B = A');

    Check(EqualityA.Equals(EqualityC), 'A = C');
    Check(EqualityC.Equals(EqualityA), 'C = A');

    Check(not EqualityB.Equals(EqualityC), 'B <> C');
    Check(not EqualityC.Equals(EqualityB), 'C <> B');
  finally
    EqualityC.Free;
  end;
end;

procedure TestTIdSipUri.TestEqualsMethodParamIsCaseSensitive;
begin
  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna;method=REGISTER';
  EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna;method=register';

  Check(not EqualityA.Equals(EqualityB), 'A <> B, Method param, Case differs');
  Check(not EqualityB.Equals(EqualityA), 'B <> A, Method param, Case differs');
end;

procedure TestTIdSipUri.TestEqualsParamOrderIrrelevant;
begin
  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=tcp;ttl=1';
  EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna;ttl=1;transport=tcp';

  Check(EqualityA.Equals(EqualityB), 'A = B, Params, different order');
  Check(EqualityB.Equals(EqualityA), 'B = A, Params, different order');
end;

procedure TestTIdSipUri.TestEqualsParamsDiffer;
begin
  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=tcp;ttl=1';
  EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna;ttl=1';

  Check(not EqualityA.Equals(EqualityB), 'A <> B, Params');
  Check(not EqualityB.Equals(EqualityA), 'B <> A, Params');

  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna;ttl=1';
  EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  Check(not EqualityA.Equals(EqualityB), 'A <> B, Params, one has none');
  Check(not EqualityB.Equals(EqualityA), 'B <> A, Params, one has none');
end;

procedure TestTIdSipUri.TestEqualsPasswordDiffers;
begin
  EqualityA.Uri := 'sip:wintermute:palace@tessier-ashpool.co.luna';
  EqualityB.Uri := 'sip:wintermute:PALACE@tessier-ashpool.co.luna';

  Check(not EqualityA.Equals(EqualityB), 'A <> B, Password');
  Check(not EqualityB.Equals(EqualityA), 'B <> A, Password');
end;

procedure TestTIdSipUri.TestEqualsPortDiffers;
begin
  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna:5060';
  EqualityB.Uri := 'sip:wintermute@tessier-ashpool.co.luna:5061';

  Check(not EqualityA.Equals(EqualityB), 'A <> B, Port');
  Check(not EqualityB.Equals(EqualityA), 'B <> A, Port');
end;

procedure TestTIdSipUri.TestEqualsSchemeDiffers;
begin
  // EqualityA has a port because SIP's default port is 5060 and SIPS' is 5061
  EqualityA.Uri :=  'sip:wintermute@tessier-ashpool.co.luna:5061';
  EqualityB.Uri := 'sips:wintermute@tessier-ashpool.co.luna';

  Check(not EqualityA.Equals(EqualityB), 'A <> B, Scheme');
  Check(not EqualityB.Equals(EqualityA), 'B <> A, Scheme');
end;

procedure TestTIdSipUri.TestEqualsSchemeIsCaseInsensitive;
begin
  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  EqualityB.Uri := 'SIP:wintermute@tessier-ashpool.co.luna';

  Check(EqualityA.Equals(EqualityB), 'A = B, Scheme, Case differs');
  Check(EqualityB.Equals(EqualityA), 'B = A, Scheme, Case differs');
end;

procedure TestTIdSipUri.TestEqualsSecureTransportVersusSips;
begin
  EqualityA.Uri :=  'sip:wintermute@tessier-ashpool.co.luna:5061;transport=tls';
  EqualityB.Uri := 'sips:wintermute@tessier-ashpool.co.luna';

  Check(not EqualityA.Equals(EqualityB), 'A <> B, Secure transport vs. SIPS');
  Check(not EqualityB.Equals(EqualityA), 'B <> A, Secure transport vs. SIPS');
end;

procedure TestTIdSipUri.TestEqualsUsernameDiffers;
begin
  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  EqualityB.Uri := 'sip:Wintermute@tessier-ashpool.co.luna';

  Check(not EqualityA.Equals(EqualityB), 'A <> B, Username');
  Check(not EqualityB.Equals(EqualityA), 'B <> A, Username');
end;

procedure TestTIdSipUri.TestEqualsUsernameParameter;
begin
  EqualityA.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  EqualityB.Uri := 'sip:tessier-ashpool.co.luna;user=wintermute';

  Check(not EqualityA.Equals(EqualityB), 'A <> B, Username parameter');
  Check(not EqualityB.Equals(EqualityA), 'B <> A, Username parameter');
end;

procedure TestTIdSipUri.TestGetSetMaddr;
begin
  Uri.AddParameter(MaddrParam, '127.0.0.1');
  CheckEquals('127.0.0.1', Uri.Maddr, 'Get Maddr');

  Uri.Maddr := '0.0.0.0';
  CheckEquals('0.0.0.0', Uri.ParamValue(MaddrParam), 'Set Maddr');
end;

procedure TestTIdSipUri.TestGetSetMethod;
begin
  Uri.AddParameter(MethodParam, MethodInvite);
  CheckEquals(MethodInvite, Uri.Method, 'Get Method');

  Uri.Method := MethodRegister;
  CheckEquals(MethodRegister, Uri.ParamValue(MethodParam), 'Set Method');
end;

procedure TestTIdSipUri.TestGetSetTransport;
begin
  Uri.AddParameter(TransportParam, TransportParamTCP);
  CheckEquals(TransportParamTCP,
              Uri.Transport,
              'Get Transport');

  Uri.Transport := TransportParamSCTP;
  CheckEquals(TransportParamSCTP,
              Uri.ParamValue(TransportParam),
              'Set Transport');
end;

procedure TestTIdSipUri.TestGetSetTTL;
begin
  Uri.AddParameter(TTLParam, '42');
  CheckEquals(42, Uri.TTL, 'Get TTL');

  Uri.TTL := 13;
  CheckEquals('13', Uri.ParamValue(TTLParam), 'Set TTL');
end;

procedure TestTIdSipUri.TestGetSetUserParam;
begin
  Uri.AddParameter(UserParam, UserParamPhone);
  CheckEquals(UserParamPhone, Uri.UserParameter, 'Get User');

  Uri.UserParameter := UserParamIp;
  CheckEquals(UserParamIp, Uri.ParamValue(UserParam), 'Set User');
end;

procedure TestTIdSipUri.TestHasHeaders;
begin
  Check(not Uri.HasHeaders, 'No headers');

  Uri.Headers.Add(RouteHeader);
  Check(Uri.HasHeaders, 'One header');
end;

procedure TestTIdSipUri.TestHasParameter;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Uri.HasParameter('foo'), 'no parameters');

  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;foo';
  Check(Uri.HasParameter('foo'), 'valueless parameter');

  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;foo=bar';
  Check(Uri.HasParameter('foo'), 'valued parameter');

  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp;foo=bar;lr';
  Check(Uri.HasParameter('foo'), 'parameter amongst others');
end;

procedure TestTIdSipUri.TestHasValidSyntax;
begin
  Uri.Uri := ':wintermute@tessier-ashpool.co.luna';
  Check(not Uri.HasValidSyntax, 'Missing scheme');

  Uri.Uri := 'sipx:a';
  Check(not Uri.HasValidSyntax, 'Invalid scheme');

  Uri.Uri := 'sip:';
  Check(not Uri.HasValidSyntax, 'Missing host');

  Uri.Uri := 'sip:host';
  Check(Uri.HasValidSyntax, 'Very simple but valid SIP URI');

  Uri.Uri := 'sip:1host';
  Check(not Uri.HasValidSyntax, 'Invalid host info');

  Uri.Uri := 'sip:<@host';
  Check(not Uri.HasValidSyntax, 'Invalid user info');

  Uri.Uri := 'sip:case:%1@host';
  Check(not Uri.HasValidSyntax, 'Invalid password info');

  Uri.Uri := 'sip:case@friedneurons.org;f%%=tls';
  Check(not Uri.HasValidSyntax,
        'Malformed parameter name');
end;

procedure TestTIdSipUri.TestIsLooseRoutable;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Uri.IsLooseRoutable, 'Non-loose-routable SIP URI marked as loose routable');

  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;lr';
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
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Uri.IsSecure, 'SIP URI marked as secure');

  Uri.Uri := 'sips:wintermute@tessier-ashpool.co.luna';
  Check(Uri.IsSecure, 'SIPS URI not marked as secure');
end;

procedure TestTIdSipUri.TestMaddr;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;user=ip;method=INVITE;ttl=2;maddr=127.0.0.255;lr';
  CheckEquals('127.0.0.255', Uri.Maddr, 'maddr');
end;

procedure TestTIdSipUri.TestPortIsSpecified;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Uri.PortIsSpecified, 'No port specified (default)');

  Uri.Port := Uri.DefaultPort;
  Check(Uri.PortIsSpecified, 'Port set to default value');

  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna:5060';

  Check(Uri.PortIsSpecified, 'Port specified');

  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna:5060';
  Check(Uri.PortIsSpecified, 'Port specified');
end;

procedure TestTIdSipUri.TestPortWithSipScheme;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  CheckEquals(IdPORT_SIP, Uri.Port, 'SIP URI default port');
end;

procedure TestTIdSipUri.TestPortWithSipsScheme;
begin
  Uri.Uri := 'sips:wintermute@tessier-ashpool.co.luna';
  CheckEquals(IdPORT_SIPS, Uri.Port, 'SIPS URI default port');
end;

procedure TestTIdSipUri.TestRemoveParameter;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;security=on;transport=tcp';
  Uri.RemoveParameter('foo');
  CheckEquals(2, Uri.ParamCount, 'Wrong parameter was removed (removing foo)');

  Uri.RemoveParameter('security');
  Check(Uri.HasParameter('transport'),
        'transport parameter was removed, not security');
  Check(not Uri.HasParameter('security'),
        'security parameter was not removed');

  Uri.RemoveParameter('transport');
  Check(not Uri.HasParameter('transport'),
        'transport parameter was not removed');
end;

procedure TestTIdSipUri.TestSetUriBasicUri;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  CheckEquals('sip',                     Uri.Scheme,     'Scheme');
  CheckEquals('wintermute',              Uri.Username,   'User');
  CheckEquals('tessier-ashpool.co.luna', Uri.Host,       'Host');
  CheckEquals(0,                         Uri.ParamCount, 'Parameter count');
end;

procedure TestTIdSipUri.TestSetUriMultipleHeaders;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna'
           + '?Route=%3Csip:127.0.0.1%3E&Route=%3Csip:127.0.0.2%3E';

  CheckEquals(2,                 Uri.Headers.Count,          'Header count');
  CheckEquals('Route',           Uri.Headers.Items[0].Name,  'Header 1 name');
  CheckEquals('<sip:127.0.0.1>', Uri.Headers.Items[0].Value, 'Header 1 value');
  CheckEquals(TIdSipRouteHeader.ClassName,
              Uri.Headers.Items[0].ClassName,
              'Header 1 class type');
  CheckEquals('Route',           Uri.Headers.Items[1].Name,  'Header 2 name');
  CheckEquals('<sip:127.0.0.2>', Uri.Headers.Items[1].Value, 'Header 2 value');
  CheckEquals(TIdSipRouteHeader.ClassName,
              Uri.Headers.Items[1].ClassName,
              'Header 2 class type');
end;

procedure TestTIdSipUri.TestSetUriMultipleParameters;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;foo=bar;baz=quaax';

  CheckEquals(2,       Uri.ParamCount,    'Parameter count');
  CheckEquals('foo',   Uri.ParamName(0),  'Parameter 1 name');
  CheckEquals('baz',   Uri.ParamName(1),  'Parameter 2 name');
  CheckEquals('bar',   Uri.ParamValue(0), 'Parameter 1 value');
  CheckEquals('quaax', Uri.ParamValue(1), 'Parameter 2 value');
end;

procedure TestTIdSipUri.TestSetUriOneHeader;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna?Route=%3Csip:127.0.0.1%3E';

  CheckEquals(1,                 Uri.Headers.Count,          'Header count');
  CheckEquals('Route',           Uri.Headers.Items[0].Name,  'Header name');
  CheckEquals('<sip:127.0.0.1>', Uri.Headers.Items[0].Value, 'Header value');
  CheckEquals(TIdSipRouteHeader.ClassName,
              Uri.Headers.Items[0].ClassName,
              'Header class type');
end;

procedure TestTIdSipUri.TestSetUriOneParameter;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;foo=bar';

  CheckEquals(1,     Uri.ParamCount,    'Parameter count');
  CheckEquals('foo', Uri.ParamName(0),  'Parameter name');
  CheckEquals('bar', Uri.ParamValue(0), 'Parameter value');
end;

procedure TestTIdSipUri.TestSetUriTortureParameters;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;lr;foo=bar;baz=qu%61ax';

  CheckEquals(3,       Uri.ParamCount,    'Parameter count');
  CheckEquals('lr',    Uri.ParamName(0),  'Parameter 1 name');
  CheckEquals('',      Uri.ParamValue(0), 'Parameter 1 value');
  CheckEquals('foo',   Uri.ParamName(1),  'Parameter 2 name');
  CheckEquals('bar',   Uri.ParamValue(1), 'Parameter 2 value');
  CheckEquals('baz',   Uri.ParamName(2),  'Parameter 3 name');
  CheckEquals('quaax', Uri.ParamValue(2), 'Parameter 3 value');
end;

procedure TestTIdSipUri.TestSetUriUserHasEscapedChars;
begin
  Uri.Uri := 'sip:sip%3Auser%40example.com@company.com;other-param=summit';

  CheckEquals('sip:user@example.com', Uri.Username, 'Username');
end;

procedure TestTIdSipUri.TestSetUriValuelessParameter;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;lr';

  CheckEquals(1,    Uri.ParamCount,    'Parameter count');
  CheckEquals('lr', Uri.ParamName(0),  'Parameter name');
  CheckEquals('',   Uri.ParamValue(0), 'Parameter value');
end;

procedure TestTIdSipUri.TestSetUriWithNoUser;
begin
  Uri.Uri := 'sip:tessier-ashpool.co.luna';

  CheckEquals('tessier-ashpool.co.luna', Uri.Host,     'Host');
  CheckEquals('',                        Uri.Username, 'User');
end;

procedure TestTIdSipUri.TestSetUriWithPassword;
begin
  Uri.Uri := 'sip:wintermute:foo@tessier-ashpool.co.lu';

  CheckEquals('foo', Uri.Password, 'Password');
end;

procedure TestTIdSipUri.TestSetUriWithPort;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna:100';

  CheckEquals(100, Uri.Port, 'Port');
end;

procedure TestTIdSipUri.TestSetUriClearsOldValues;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp';
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';

  CheckEquals(Uri.DefaultTransport,
              Uri.Transport,
              'Transport not cleared after SetUri');
end;

procedure TestTIdSipUri.TestTransport;
begin
  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  CheckEquals('udp', Uri.Transport, 'Default transport');

  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp';
  CheckEquals('udp', Uri.Transport, 'UDP specified');

  Uri.Uri := 'sip:wintermute@tessier-ashpool.co.luna;transport=tcp';
  CheckEquals('tcp', Uri.Transport, 'TCP specified');
end;

procedure TestTIdSipUri.TestUserIsIP;
begin
  Check(not Uri.UserIsIP, 'no user parameter');

  Uri.UserParameter := UserParamPhone;
  Check(not Uri.UserIsIP, UserParamPhone);

  Uri.UserParameter := UserParamIp;
  Check(Uri.UserIsIP, UserParamIp);

  Uri.UserParameter := Uppercase(UserParamIp);
  Check(Uri.UserIsIP, Uppercase(UserParamIp));
end;

procedure TestTIdSipUri.TestUserIsPhone;
begin
  Check(not Uri.UserIsPhoneNumber, 'no user parameter');

  Uri.UserParameter := UserParamIp;
  Check(not Uri.UserIsPhoneNumber, UserParamIp);

  Uri.UserParameter := UserParamPhone;
  Check(Uri.UserIsPhoneNumber, UserParamPhone);

  Uri.UserParameter := Uppercase(UserParamPhone);
  Check(Uri.UserIsPhoneNumber, Uppercase(UserParamPhone));
end;

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

initialization
  RegisterTest('SIP URIs', Suite);
end.
