unit TestIdSipUri;

interface

uses
  IdSipHeaders, TestFramework;

type
  TestTIdSipUri = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBasicGetUri;
    procedure TestBasicSetUri;
    procedure TestCreateSetsUri;
    procedure TestGetSetMaddr;
    procedure TestGetSetMethod;
    procedure TestGetSetTransport;
    procedure TestGetSetTTL;
    procedure TestGetSetUserParam;
    procedure TestGetUriFancyParameters;
    procedure TestGetUriHeadersWithNoParameters;
    procedure TestGetUriHeadersWithParameters;
    procedure TestGetUriNormalParameters;
    procedure TestGetUriNoUsername;
    procedure TestGetUriWithDefaultPort;
    procedure TestGetUriWithPassword;
    procedure TestGetUriWithSpecialPort;
    procedure TestHasHeaders;
    procedure TestHasValidSyntax;
    procedure TestIsLooseRoutable;
    procedure TestIsPassword;
    procedure TestIsParamNameOrValue;
    procedure TestIsUser;
    procedure TestIsSecure;
    procedure TestPortWithSipScheme;
    procedure TestPortWithSipsScheme;
    procedure TestSpecialParams;
    procedure TestSetUriBasicUri;
    procedure TestSetUriMultipleHeaders;
    procedure TestSetUriMultipleParameters;
    procedure TestSetUriOneHeader;
    procedure TestSetUriOneParameter;
    procedure TestSetUriTortureParameters;
    procedure TestSetUriValuelessParameter;
    procedure TestSetUriWithNoUser;
    procedure TestSetUriWithPassword;
    procedure TestSetUriWithPort;
    procedure TestSetUriClearsOldValues;
    procedure TestUserIsIP;
    procedure TestUserIsPhone;
  end;

implementation

uses
  IdSipConsts, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipUri unit tests');
  Result.AddTest(TestTIdSipUri.Suite);
end;

//******************************************************************************
//* TestTIdSipUri                                                              *
//******************************************************************************
//* TestTIdSipUri Public methods ***********************************************

procedure TestTIdSipUri.SetUp;
begin
  inherited SetUp;
end;

procedure TestTIdSipUri.TearDown;
begin
  inherited TearDown;
end;

//* TestTIdSipUri Published methods ********************************************

procedure TestTIdSipUri.TestBasicGetUri;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Scheme   := 'sip';
    Uri.Username := 'wintermute';
    Uri.Host     := 'tessier-ashpool.co.lu';

    CheckEquals('sip:wintermute@tessier-ashpool.co.lu',
                Uri.Uri,
                'URI');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestBasicSetUri;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu';

    CheckEquals('sip',                   Uri.Scheme,        'Scheme');
    CheckEquals('wintermute',            Uri.Username,      'User');
    CheckEquals('tessier-ashpool.co.lu', Uri.Host,          'Host');
    CheckEquals(IdPORT_SIP,              Uri.Port,          'Port');
    CheckEquals(0,                       Uri.ParamCount,    'Parameters');
    CheckEquals('',                      Uri.Password,      'Password');
    CheckEquals(0,                       Uri.Headers.Count, 'Headers');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestCreateSetsUri;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('sip:wintermute@tessier-ashpool.co.lu');
  try
    CheckEquals('sip',                   Uri.Scheme,   'Scheme');
    CheckEquals('wintermute',            Uri.Username, 'User');
    CheckEquals('tessier-ashpool.co.lu', Uri.Host,     'Host');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestGetSetMaddr;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.AddParameter(MaddrParam, '127.0.0.1');
    CheckEquals('127.0.0.1', Uri.Maddr, 'Get Maddr');

    Uri.Maddr := '0.0.0.0';
    CheckEquals('0.0.0.0', Uri.ParamValue(MaddrParam), 'Set Maddr');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestGetSetMethod;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.AddParameter(MethodParam, MethodInvite);
    CheckEquals(MethodInvite, Uri.Method, 'Get Method');

    Uri.Method := MethodRegister;
    CheckEquals(MethodRegister, Uri.ParamValue(MethodParam), 'Set Method');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestGetSetTransport;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.AddParameter(TransportParam, TransportParamTCP);
    CheckEquals(TransportParamTCP,
                Uri.Transport,
                'Get Transport');

    Uri.Transport := TransportParamSCTP;
    CheckEquals(TransportParamSCTP,
                Uri.ParamValue(TransportParam),
                'Set Transport');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestGetSetTTL;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.AddParameter(TTLParam, '42');
    CheckEquals(42, Uri.TTL, 'Get TTL');

    Uri.TTL := 13;
    CheckEquals('13', Uri.ParamValue(TTLParam), 'Set TTL');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestGetSetUserParam;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.AddParameter(UserParam, UserParamPhone);
    CheckEquals(UserParamPhone, Uri.UserParameter, 'Get User');

    Uri.UserParameter := UserParamIp;
    CheckEquals(UserParamIp, Uri.ParamValue(UserParam), 'Set User');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestGetUriFancyParameters;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Scheme   := 'sip';
    Uri.Username := 'wintermute';
    Uri.Host     := 'tessier-ashpool.co.lu';
    Uri.AddParameter('foo', '<b%61r>');

    CheckEquals('sip:wintermute@tessier-ashpool.co.lu;foo=%3Cbar%3E',
                Uri.Uri,
                'URI');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestGetUriHeadersWithNoParameters;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Scheme   := 'sip';
    Uri.Username := 'wintermute';
    Uri.Host     := 'tessier-ashpool.co.lu';
    Uri.Headers.Add(RouteHeader).Value := '<sip:127.0.0.1>';

    CheckEquals('sip:wintermute@tessier-ashpool.co.lu?Route=%3Csip:127.0.0.1%3E',
                Uri.Uri,
                'URI');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestGetUriHeadersWithParameters;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Scheme   := 'sip';
    Uri.Username := 'wintermute';
    Uri.Host     := 'tessier-ashpool.co.lu';
    Uri.Headers.Add(RouteHeader).Value := '<sip:127.0.0.1>';
    Uri.AddParameter('foo', '<bar>');
    Uri.AddParameter('lr');
    Uri.AddParameter('baz', 'quaax');

    CheckEquals('sip:wintermute@tessier-ashpool.co.lu'
              + ';foo=%3Cbar%3E;lr;baz=quaax?Route=%3Csip:127.0.0.1%3E',
                Uri.Uri,
                'URI');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestGetUriNormalParameters;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Scheme   := 'sip';
    Uri.Username := 'wintermute';
    Uri.Host     := 'tessier-ashpool.co.lu';
    Uri.AddParameter('foo', 'bar');

    CheckEquals('sip:wintermute@tessier-ashpool.co.lu;foo=bar',
                Uri.Uri,
                'URI');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestGetUriNoUsername;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Scheme   := 'sip';
    Uri.Username := '';
    Uri.Host     := 'tessier-ashpool.co.lu';

    CheckEquals('sip:tessier-ashpool.co.lu',
                Uri.Uri,
                'URI');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestGetUriWithDefaultPort;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Scheme   := 'sip';
    Uri.Username := 'wintermute';
    Uri.Host     := 'tessier-ashpool.co.lu';
    Uri.Port     := IdPORT_SIP;

    CheckEquals('sip:wintermute@tessier-ashpool.co.lu',
                Uri.Uri,
                'URI');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestGetUriWithPassword;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Scheme   := 'sip';
    Uri.Username := 'wintermute';
    Uri.Password := 'song';
    Uri.Host     := 'tessier-ashpool.co.lu';

    CheckEquals('sip:wintermute:song@tessier-ashpool.co.lu',
                Uri.Uri,
                'URI');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestGetUriWithSpecialPort;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Scheme   := 'sip';
    Uri.Username := 'wintermute';
    Uri.Port     := IdPORT_SIP + 10000;
    Uri.Host     := 'tessier-ashpool.co.lu';

    CheckEquals('sip:wintermute@tessier-ashpool.co.lu:15060',
                Uri.Uri,
                'URI');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestHasHeaders;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Check(not Uri.HasHeaders, 'No headers');

    Uri.Headers.Add(RouteHeader);
    Check(Uri.HasHeaders, 'One header');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestHasValidSyntax;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := ':wintermute@tessier-ashpool.co.lu';
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

    Uri.Uri := 'sips:case@friedneurons.org';
    Check(Uri.HasValidSyntax,
          'SIPS URI with no specified transport');
    Uri.Uri := 'sips:case@friedneurons.org;transport=udp';
    Check(not Uri.HasValidSyntax,
          'SIPS URI may not have an unreliable transport');
    Uri.Uri := 'sips:case@friedneurons.org;transport=sctp';
    Check(Uri.HasValidSyntax,
          'SIPS URI must have a reliable transport, if any (sctp)');
    Uri.Uri := 'sips:case@friedneurons.org;transport=tcp';
    Check(Uri.HasValidSyntax,
          'SIPS URI must have a reliable transport, if any (tcp)');
    Uri.Uri := 'sips:case@friedneurons.org;transport=tls';
    Check(Uri.HasValidSyntax,
          'SIPS URI must have a reliable transport, if any (tls)');

    Uri.Uri := 'sip:case@friedneurons.org;f%%=tls';
    Check(not Uri.HasValidSyntax,
          'Malformed parameter name');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestIsLooseRoutable;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu';
    Check(not Uri.IsLooseRoutable, 'Non-loose-routable SIP URI marked as loose routable');

    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu;lr';
    Check(Uri.IsLooseRoutable, 'Loose routable URI not marked as loose routable');
  finally
    Uri.Free;
  end;
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
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu';
    Check(not Uri.IsSecure, 'SIP URI marked as secure');

    Uri.Uri := 'sips:wintermute@tessier-ashpool.co.lu';
    Check(Uri.IsSecure, 'SIPS URI not marked as secure');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestPortWithSipScheme;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu';
    CheckEquals(IdPORT_SIP, Uri.Port, 'SIP URI default port');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestPortWithSipsScheme;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sips:wintermute@tessier-ashpool.co.lu';
    CheckEquals(IdPORT_SIPS, Uri.Port, 'SIPS URI default port');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestSpecialParams;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu;user=ip&method=INVITE&ttl=2&maddr=127.0.0.255;lr';
{
    CheckEquals('ip',          Uri.User,            'user');
    CheckEquals('127.0.0.255', Uri.Maddr,           'maddr');
}
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestSetUriBasicUri;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu';

    CheckEquals('sip',                   Uri.Scheme,     'Scheme');
    CheckEquals('wintermute',            Uri.Username,   'User');
    CheckEquals('tessier-ashpool.co.lu', Uri.Host,       'Host');
    CheckEquals(0,                       Uri.ParamCount, 'Parameter count');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestSetUriMultipleHeaders;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu'
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
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestSetUriMultipleParameters;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu;foo=bar;baz=quaax';

    CheckEquals(2,       Uri.ParamCount,    'Parameter count');
    CheckEquals('foo',   Uri.ParamName(0),  'Parameter 1 name');
    CheckEquals('baz',   Uri.ParamName(1),  'Parameter 2 name');
    CheckEquals('bar',   Uri.ParamValue(0), 'Parameter 1 value');
    CheckEquals('quaax', Uri.ParamValue(1), 'Parameter 2 value');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestSetUriOneHeader;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu?Route=%3Csip:127.0.0.1%3E';

    CheckEquals(1,                 Uri.Headers.Count,          'Header count');
    CheckEquals('Route',           Uri.Headers.Items[0].Name,  'Header name');
    CheckEquals('<sip:127.0.0.1>', Uri.Headers.Items[0].Value, 'Header value');
    CheckEquals(TIdSipRouteHeader.ClassName,
                Uri.Headers.Items[0].ClassName,
                'Header class type');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestSetUriOneParameter;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu;foo=bar';

    CheckEquals(1,     Uri.ParamCount,    'Parameter count');
    CheckEquals('foo', Uri.ParamName(0),  'Parameter name');
    CheckEquals('bar', Uri.ParamValue(0), 'Parameter value');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestSetUriTortureParameters;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu;lr;foo=bar;baz=qu%61ax';

    CheckEquals(3,       Uri.ParamCount,    'Parameter count');
    CheckEquals('lr',    Uri.ParamName(0),  'Parameter 1 name');
    CheckEquals('',      Uri.ParamValue(0), 'Parameter 1 value');
    CheckEquals('foo',   Uri.ParamName(1),  'Parameter 2 name');
    CheckEquals('bar',   Uri.ParamValue(1), 'Parameter 2 value');
    CheckEquals('baz',   Uri.ParamName(2),  'Parameter 3 name');
    CheckEquals('quaax', Uri.ParamValue(2), 'Parameter 3 value');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestSetUriValuelessParameter;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu;lr';

    CheckEquals(1,    Uri.ParamCount,    'Parameter count');
    CheckEquals('lr', Uri.ParamName(0),  'Parameter name');
    CheckEquals('',   Uri.ParamValue(0), 'Parameter value');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestSetUriWithNoUser;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:tessier-ashpool.co.lu';

    CheckEquals('tessier-ashpool.co.lu', Uri.Host,     'Host');
    CheckEquals('',                      Uri.Username, 'User');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestSetUriWithPassword;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute:foo@tessier-ashpool.co.lu';

    CheckEquals('foo', Uri.Password, 'Password');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestSetUriWithPort;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu:100';

    CheckEquals(100, Uri.Port, 'Port');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestSetUriClearsOldValues;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu;transport=udp';
    Uri.Uri := 'sip:wintermute@tessier-ashpool.co.lu';

    CheckEquals('', Uri.Transport, 'Transport not cleared after SetUri');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestUserIsIP;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Check(not Uri.UserIsIP, 'no user parameter');

    Uri.UserParameter := UserParamPhone;
    Check(not Uri.UserIsIP, UserParamPhone);

    Uri.UserParameter := UserParamIp;
    Check(Uri.UserIsIP, UserParamIp);

    Uri.UserParameter := Uppercase(UserParamIp);
    Check(Uri.UserIsIP, Uppercase(UserParamIp));
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipUri.TestUserIsPhone;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create('');
  try
    Check(not Uri.UserIsPhoneNumber, 'no user parameter');

    Uri.UserParameter := UserParamIp;
    Check(not Uri.UserIsPhoneNumber, UserParamIp);

    Uri.UserParameter := UserParamPhone;
    Check(Uri.UserIsPhoneNumber, UserParamPhone);

    Uri.UserParameter := Uppercase(UserParamPhone);
    Check(Uri.UserIsPhoneNumber, Uppercase(UserParamPhone));
  finally
    Uri.Free;
  end;
end;

initialization
  RegisterTest('SIP URIs', Suite);
end.
