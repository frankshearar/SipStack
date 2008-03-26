{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipProxyDescription;

interface

uses
  IdSipMessage, IdSipProxyDescription, TestFramework;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestNumberOfOccurences;
    procedure TestSuffix;
  end;

  TestTIdAddressSpace = class(TTestCase)
  private
    Space: TIdAddressSpace;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateAddressSpace;
    procedure TestIdentifyAddressSpaceType;
  end;

  TestTIdIPv4SubnetAddressSpace = class(TTestCase)
  private
    Space: TIdIPv4SubnetAddressSpace;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestContains;
    procedure MoreSpecificThan;
  end;

  TestTIdIPv6SubnetAddressSpace = class(TTestCase)
  private
    Space: TIdIPv6SubnetAddressSpace;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestContains;
    procedure MoreSpecificThan;
  end;

  TestTIdDomainAddressSpace = class(TTestCase)
  private
    Space: TIdDomainAddressSpace;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestContains;
    procedure TestEmptyStringContainsEveryDomain;
    procedure MoreSpecificThan;
  end;

  TestTIdProxyDescription = class(TTestCase)
  private
    Desc: TIdProxyDescription;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddressSpaceAndProxyFor;
    procedure TestContainsBeforeSettingDescription;
    procedure TestNumBitsAndNetmaskIdentical;
  end;

  TestTIdProxyDescriptions = class(TTestCase)
  private
    DefaultRoute:          TIdSipRoutePath;
    EmptyRoute:            TIdSipRoutePath;
    LanAddressSpace:       String;
    LanRoute:              TIdSipRoutePath;
    LanTarget:             String;
    LocalhostAddressSpace: String;
    LocalhostRoute:        TIdSipRoutePath;
    LocalhostTarget:       String;
    Proxies:               TIdProxyDescriptions;

    procedure CheckEquals(Expected, Received: TIdSipRoutePath; Msg: String); overload;
    procedure CheckExceptionAddedFirst;
    procedure CheckExceptionAddedLast;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddDescriptionAndRoutePathFor;
    procedure TestAddRoute;
    procedure TestAddRouteNoProxyPresent;
    procedure TestAddRouteCanonicalisesNetmask;
    procedure TestClearAllDescriptions;
    procedure TestContainedRoutesOrderIndependent;
    procedure TestRemoveDescription;
    procedure TestRoutePathForDefaultRoute;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdProxyDescription unit tests');
  Result.AddSuite(TestFunctions.Suite);
  Result.AddSuite(TestTIdAddressSpace.Suite);
  Result.AddSuite(TestTIdIPv4SubnetAddressSpace.Suite);
  Result.AddSuite(TestTIdIPv6SubnetAddressSpace.Suite);
  Result.AddSuite(TestTIdDomainAddressSpace.Suite);
  Result.AddSuite(TestTIdProxyDescription.Suite);
  Result.AddSuite(TestTIdProxyDescriptions.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestNumberOfOccurences;
begin
  CheckEquals(0, NumberOfOccurences('.', ''), 'Dots in the empty string');
  CheckEquals(0, NumberOfOccurences(#0, ''), 'NULs in the empty string');

  CheckEquals(1, NumberOfOccurences('.', '.'), 'Dots in ''.''');
  CheckEquals(2, NumberOfOccurences('.', '.a.'), 'Dots in ''.a.''');

  CheckEquals(0, NumberOfOccurences(';', '.'), 'Commas in ''.''');
end;

procedure TestFunctions.TestSuffix;
begin
  CheckEquals('', Suffix('', 10), '"", 10');

  CheckEquals('',    Suffix('foo', 0), '"foo", 0');
  CheckEquals('o',   Suffix('foo', 1), '"foo", 1');
  CheckEquals('oo',  Suffix('foo', 2), '"foo", 2');
  CheckEquals('foo', Suffix('foo', 3), '"foo", 3');
  CheckEquals('foo', Suffix('foo', 4), '"foo", 4');
end;

//******************************************************************************
//* TestTIdAddressSpace                                                        *
//******************************************************************************
//* TestTIdAddressSpace Public methods *****************************************

procedure TestTIdAddressSpace.SetUp;
begin
  inherited SetUp;

  Self.Space := TIdAddressSpace.Create;
end;

procedure TestTIdAddressSpace.TearDown;
begin
  Self.Space.Free;

  inherited TearDown;
end;

//* TestTIdAddressSpace Published methods **************************************

procedure TestTIdAddressSpace.TestCreateAddressSpace;
var
  A: TIdAddressSpace;
begin
  A := TIdAddressSpace.CreateAddressSpace(asIPv4Subnet);
  try
    CheckEquals(TIdIPv4SubnetAddressSpace, A.ClassType, 'IPv4 subnet');
  finally
    A.Free;
  end;

  A := TIdAddressSpace.CreateAddressSpace(asIPv6Subnet);
  try
    CheckEquals(TIdIPv6SubnetAddressSpace, A.ClassType, 'IPv6 subnet');
  finally
    A.Free;
  end;

  A := TIdAddressSpace.CreateAddressSpace(asDomain);
  try
    CheckEquals(TIdDomainAddressSpace, A.ClassType, 'Domain');
  finally
    A.Free;
  end;

  A := TIdAddressSpace.CreateAddressSpace(asUnknown);
  try
    CheckEquals(TIdAddressSpace, A.ClassType, 'Unknown');
  finally
    A.Free;
  end;
end;

procedure TestTIdAddressSpace.TestIdentifyAddressSpaceType;
begin
  Check(asIPv4Subnet = Self.Space.IdentifySpaceType('10.0.0.1'),           'IPv4 address');
  Check(asIPv4Subnet = Self.Space.IdentifySpaceType('10.0.0.0/8'),         'IPv4 Subnet/numbits');
  Check(asIPv4Subnet = Self.Space.IdentifySpaceType('10.0.0.0/255.0.0.0'), 'IPv4 Subnet/mask');
  Check(asIPv4Subnet = Self.Space.IdentifySpaceType('0.0.0.0/0'),          'The IPv4 zero address');

  Check(asIPv6Subnet = Self.Space.IdentifySpaceType('2002::1'),       'IPv6 address');
  Check(asIPv6Subnet = Self.Space.IdentifySpaceType('2002::/8'),      'IPv6 Subnet/numbits');
  Check(asIPv6Subnet = Self.Space.IdentifySpaceType('2002::/ffff::'), 'IPv6 Subnet/mask');
  Check(asIPv6Subnet = Self.Space.IdentifySpaceType('::/::'),         'The IPv6 zero address');

  Check(asDomain = Self.Space.IdentifySpaceType('com'),         'Top Level Domain');
  Check(asDomain = Self.Space.IdentifySpaceType('example.com'), 'Domain');
  Check(asDomain = Self.Space.IdentifySpaceType(''),            'The empty string');

  Check(asUnknown = Self.Space.IdentifySpaceType('0..0.0.0'),    'Malformed IPv4 address');
  Check(asUnknown = Self.Space.IdentifySpaceType('0..0.0.0/0'),  'Malformed IPv4 subnet');
  Check(asUnknown = Self.Space.IdentifySpaceType(':::'),         'Malformed IPv6 address');
  Check(asUnknown = Self.Space.IdentifySpaceType('::://ffff::'), 'Malformed IPv6 subnet');
  Check(asUnknown = Self.Space.IdentifySpaceType('0.0.0.0/::'),  'IPv4 subnet but IPv6 mask');
  Check(asUnknown = Self.Space.IdentifySpaceType('::/0.0.0.0'),  'IPv6 subnet but IPv4 mask');
  Check(asUnknown = Self.Space.IdentifySpaceType('0.0.0.0/foo'),  'IPv4 subnet but string for mask');
  Check(asUnknown = Self.Space.IdentifySpaceType('::/foo'),       'IPv6 subnet but string for mask');
  Check(asUnknown = Self.Space.IdentifySpaceType('foo/foo'),      'Domain with string for mask');
  Check(asUnknown = Self.Space.IdentifySpaceType('foo/foo'),      'Domain with number-of-significant-bits for mask');
end;

//******************************************************************************
//* TestTIdIPv4SubnetAddressSpace                                              *
//******************************************************************************
//* TestTIdIPv4SubnetAddressSpace Public methods *******************************

procedure TestTIdIPv4SubnetAddressSpace.SetUp;
begin
  inherited SetUp;

  Self.Space := TIdIPv4SubnetAddressSpace.Create;
  Self.Space.Description := '10.0.0.0/8';
end;

procedure TestTIdIPv4SubnetAddressSpace.TearDown;
begin
  Self.Space.Free;

  inherited TearDown;
end;

//* TestTIdIPv4SubnetAddressSpace Published methods ****************************

procedure TestTIdIPv4SubnetAddressSpace.TestContains;
begin
  Check(    Self.Space.Contains('10.0.0.1'),    '10.0.0.1');
  Check(    Self.Space.Contains('10.0.1.0'),    '10.0.1.0');
  Check(    Self.Space.Contains('10.1.0.0'),    '10.1.0.0');
  Check(not Self.Space.Contains('11.0.0.1'),    '11.0.0.1');
  Check(not Self.Space.Contains('10..0.0.1'),   'Malformed IPv4 10..0.0.1');
  Check(not Self.Space.Contains('::1'),         '::1');
  Check(not Self.Space.Contains('example.com'), 'example.com');
  Check(not Self.Space.Contains(''),            'The empty string');
end;

procedure TestTIdIPv4SubnetAddressSpace.MoreSpecificThan;
var
  OtherSpace: TIdAddressSpace;
begin
  Check(not Self.Space.MoreSpecificThan(Self.Space), 'Not more specific than self!');

  OtherSpace := TIdIPv4SubnetAddressSpace.Create;
  try
    OtherSpace.Description := '10.0.0.0/16';
    Check(not Self.Space.MoreSpecificThan(OtherSpace), '/16 more specific than /8');
    Check(    OtherSpace.MoreSpecificThan(Self.Space), '/8 less specific than /16');

    OtherSpace.Description := '192.168.0.0/16';
    Check(not Self.Space.MoreSpecificThan(OtherSpace), '/16 more specific than /8, even for different subnet');
    Check(    OtherSpace.MoreSpecificThan(Self.Space), '/8 less specific than /16, even for different subnet');
  finally
    OtherSpace.Free;
  end;

  OtherSpace := TIdIPv6SubnetAddressSpace.Create;
  try
    OtherSpace.Description := '2002:1234:5678::/16';
    Check(not Self.Space.MoreSpecificThan(OtherSpace), 'IPv6 subnet is not more specific than an IPv4 subnet');
    Check(not OtherSpace.MoreSpecificThan(Self.Space), 'IPv4 subnet is not more specific than an IPv6 subnet');
  finally
    OtherSpace.Free;
  end;

  OtherSpace := TIdDomainAddressSpace.Create;
  try
    OtherSpace.Description := 'foo.bar';
    Check(not Self.Space.MoreSpecificThan(OtherSpace), 'Domain is not more specific than an IPv4 subnet');
    Check(not OtherSpace.MoreSpecificThan(Self.Space), 'IPv4 subnet is not more specific than a domain');
  finally
    OtherSpace.Free;
  end;
end;

//******************************************************************************
//* TestTIdIPv6SubnetAddressSpace                                              *
//******************************************************************************
//* TestTIdIPv6SubnetAddressSpace Public methods *******************************

procedure TestTIdIPv6SubnetAddressSpace.SetUp;
begin
  inherited SetUp;

  Self.Space := TIdIPv6SubnetAddressSpace.Create;
  Self.Space.Description := '2002:1234:5678::/16';
end;

procedure TestTIdIPv6SubnetAddressSpace.TearDown;
begin
  Self.Space.Free;

  inherited TearDown;
end;

//* TestTIdIPv6SubnetAddressSpace Published methods ****************************

procedure TestTIdIPv6SubnetAddressSpace.TestContains;
begin
  Check(    Self.Space.Contains('2002::1'),     '2002::1');
  Check(    Self.Space.Contains('2002::1:0'),   '2002::1:0');
  Check(    Self.Space.Contains('2002::1:0:0'), '2002::1:0:0');
  Check(not Self.Space.Contains('2003::1'),     '2003::1');
  Check(not Self.Space.Contains(':::'),         'Malformed IPv6 :::');
  Check(not Self.Space.Contains('127.0.0.1'),   '127.0.0.1');
  Check(not Self.Space.Contains('example.com'), 'example.com');
  Check(not Self.Space.Contains(''),            'The empty string');
end;

procedure TestTIdIPv6SubnetAddressSpace.MoreSpecificThan;
var
  OtherSpace: TIdAddressSpace;
begin
  Check(not Self.Space.MoreSpecificThan(Self.Space), 'Not more specific than self!');

  OtherSpace := TIdIPv6SubnetAddressSpace.Create;
  try
    OtherSpace.Description := '2002:1234:5678::/24';
    Check(    OtherSpace.MoreSpecificThan(Self.Space), '/8 less specific than /24');
    Check(not Self.Space.MoreSpecificThan(OtherSpace), '/24 more specific than /16');

    OtherSpace.Description := '2001:1234:5678::/8';
    Check(    Self.Space.MoreSpecificThan(OtherSpace), '/16 more specific than /8, even for different subnet');
    Check(not OtherSpace.MoreSpecificThan(Self.Space), '/8 less specific than /16, even for different subnet');
  finally
    OtherSpace.Free;
  end;

  OtherSpace := TIdIPv4SubnetAddressSpace.Create;
  try
    OtherSpace.Description := '10.0.0.0/16';
    Check(not Self.Space.MoreSpecificThan(OtherSpace), 'IPv6 subnet is not more specific than an IPv4 subnet');
    Check(not OtherSpace.MoreSpecificThan(Self.Space), 'IPv4 subnet is not more specific than an IPv6 subnet');
  finally
    OtherSpace.Free;
  end;

  OtherSpace := TIdDomainAddressSpace.Create;
  try
    OtherSpace.Description := 'foo.bar';
    Check(not Self.Space.MoreSpecificThan(OtherSpace), 'Domain is not more specific than an IPv6 subnet');
    Check(not OtherSpace.MoreSpecificThan(Self.Space), 'IPv6 subnet is not more specific than a domain');
  finally
    OtherSpace.Free;
  end;
end;

//******************************************************************************
//* TestTIdDomainAddressSpace                                                  *
//******************************************************************************
//* TestTIdDomainAddressSpace Public methods ***********************************

procedure TestTIdDomainAddressSpace.SetUp;
begin
  inherited SetUp;

  Self.Space := TIdDomainAddressSpace.Create;
  Self.Space.Description := 'local';
end;

procedure TestTIdDomainAddressSpace.TearDown;
begin
  Self.Space.Free;

  inherited TearDown;
end;

procedure TestTIdDomainAddressSpace.TestContains;
begin
  Check(    Self.Space.Contains('local'),          'local');
  Check(    Self.Space.Contains('roke.local'),     'roke.local');
  Check(    Self.Space.Contains('sub.roke.local'), 'sub.roke.local');
  Check(not Self.Space.Contains('example.com'),    'example.com');
  Check(not Self.Space.Contains('roke.local.com'), 'roke.local.com');
  Check(not Self.Space.Contains(''),               'The empty string');
  Check(not Self.Space.Contains('.local'),         'Malformed FQDN: .local');

  Check(not Self.Space.Contains('2002::1'),     '2002::1');
  Check(not Self.Space.Contains(':::'),         'Malformed IPv6 :::');
  Check(not Self.Space.Contains('127.0.0.1'),   '127.0.0.1');
  Check(not Self.Space.Contains('127.0.0..1'),  'Malformed IPv4 127.0.0..1');
  Check(not Self.Space.Contains('example.com'), 'example.com');
end;

procedure TestTIdDomainAddressSpace.TestEmptyStringContainsEveryDomain;
begin
  Self.Space.Description := '';

  Check(    Self.Space.Contains('local'),          'local');
  Check(    Self.Space.Contains('roke.local'),     'roke.local');
  Check(    Self.Space.Contains('sub.roke.local'), 'sub.roke.local');
  Check(    Self.Space.Contains('example.com'),    'example.com');
  Check(    Self.Space.Contains('roke.local.com'), 'roke.local.com');
  Check(    Self.Space.Contains(''),               'The empty string');
  Check(not Self.Space.Contains('.local'),         'Malformed FQDN: .local');
end;

procedure TestTIdDomainAddressSpace.MoreSpecificThan;
var
  OtherSpace: TIdAddressSpace;
begin
  Check(not Self.Space.MoreSpecificThan(Self.Space), 'Not more specific than self!');


  OtherSpace := TIdDomainAddressSpace.Create;
  try
    OtherSpace.Description := 'roke.local';
    Check(not Self.Space.MoreSpecificThan(OtherSpace), 'subdomain is more specific than containing domain');
    Check(    OtherSpace.MoreSpecificThan(Self.Space), 'Containing domain is less specific than subdomain');

    OtherSpace.Description := 'foo.bar';
    Check(not Self.Space.MoreSpecificThan(OtherSpace), 'Subdomain is more specific than NON-containing domain');
    Check(    OtherSpace.MoreSpecificThan(Self.Space), 'NON-containing domain is more specific than subdomain');

    OtherSpace.Description := 'localdomain';
    Check(not Self.Space.MoreSpecificThan(OtherSpace), 'Top level domain is not more specific than another top level domain');
    Check(not OtherSpace.MoreSpecificThan(Self.Space), 'Another top level domain is not more specific than top level domain');

    OtherSpace.Description := '';
    Check(    Self.Space.MoreSpecificThan(OtherSpace), 'Top level domain is more specific than the null domain');
    Check(not OtherSpace.MoreSpecificThan(Self.Space), 'The null domain is more specific than top level domain');
  finally
    OtherSpace.Free;
  end;

  OtherSpace := TIdIPv4SubnetAddressSpace.Create;
  try
    OtherSpace.Description := '0.0.0.0/0';
    Check(not Self.Space.MoreSpecificThan(OtherSpace), 'A domain is not more specific than an IPv4 address space');
    Check(not OtherSpace.MoreSpecificThan(Self.Space), 'An IPv4 address space is not more specific than a domain');
  finally
    OtherSpace.Free;
  end;

  OtherSpace := TIdIPv6SubnetAddressSpace.Create;
  try
    OtherSpace.Description := '::/0';
    Check(not Self.Space.MoreSpecificThan(OtherSpace), 'A domain is not more specific than an IPv6 address space');
    Check(not OtherSpace.MoreSpecificThan(Self.Space), 'An IPv6 address space is not more specific than a domain');
  finally
    OtherSpace.Free;
  end;
end;

//******************************************************************************
//* TestTIdProxyDescription                                                    *
//******************************************************************************
//* TestTIdProxyDescription Public methods *************************************

procedure TestTIdProxyDescription.SetUp;
begin
  inherited SetUp;

  Self.Desc := TIdProxyDescription.Create;
end;

procedure TestTIdProxyDescription.TearDown;
begin
  Self.Desc.Free;

  inherited TearDown;
end;

procedure TestTIdProxyDescription.TestAddressSpaceAndProxyFor;
const
  LanClient        = 'roke.local';
  Localhost        = '127.0.0.1';
  SixToFourAddress = '2002:5156:4278::1';
begin
  Self.Desc.AddressSpace := '127.0.0.0/8';
  Check(    Self.Desc.ProxyFor(Localhost),        'Proxying for the local loop subnet: localhost');
  Check(not Self.Desc.ProxyFor(SixToFourAddress), 'Proxying for the local loop subnet: IPv6 address');
  Check(not Self.Desc.ProxyFor(LanClient),        'Proxying for the local loop subnet: FQDN');

  Self.Desc.AddressSpace := '::/0';
  Check(not Self.Desc.ProxyFor(Localhost),        'Proxying for IPv6: localhost');
  Check(    Self.Desc.ProxyFor(SixToFourAddress), 'Proxying for IPv6: IPv6 address');
  Check(not Self.Desc.ProxyFor(LanClient),        'Proxying for IPv6: FQDN');

  Self.Desc.AddressSpace := 'local';
  Check(not Self.Desc.ProxyFor(Localhost),        'Proxying for a domain: localhost');
  Check(not Self.Desc.ProxyFor(SixToFourAddress), 'Proxying for a domain: IPv6 address');
  Check(    Self.Desc.ProxyFor(LanClient),        'Proxying for a domain: FQDN');
end;

procedure TestTIdProxyDescription.TestContainsBeforeSettingDescription;
const
  LanClient        = 'roke.local';
  Localhost        = '127.0.0.1';
  SixToFourAddress = '2002:5156:4278::1';
begin
  Check(not Self.Desc.ProxyFor(Localhost),        'No description set: localhost');
  Check(not Self.Desc.ProxyFor(SixToFourAddress), 'No description set: IPv6 address');
  Check(not Self.Desc.ProxyFor(LanClient),        'No description set: FQDN');
end;

procedure TestTIdProxyDescription.TestNumBitsAndNetmaskIdentical;
const
  LanAddress = '10.0.0.1';
  LanNetwork = '10.0.0.0';
begin
  Self.Desc.AddressSpace := LanNetwork + '/8';
  Check(Self.Desc.ProxyFor(LanAddress), 'LAN address, Description a number of significant bits');

  Self.Desc.AddressSpace := LanNetwork + '/255.0.0.0';
  Check(Self.Desc.ProxyFor(LanAddress), 'LAN address, Description a netmask');
end;

//******************************************************************************
//* TestTIdProxyDescriptions                                                   *
//******************************************************************************
//* TestTIdProxyDescriptions Public methods ************************************

procedure TestTIdProxyDescriptions.SetUp;
begin
  inherited SetUp;

  Self.Proxies := TIdProxyDescriptions.Create;

  Self.DefaultRoute := TIdSipRoutePath.Create;
  Self.DefaultRoute.Add(RouteHeader).Value := '<sip:default_gateway>';

  Self.EmptyRoute := TIdSipRoutePath.Create;

  Self.LanAddressSpace := 'local';
  Self.LanRoute := TIdSipRoutePath.Create;
  Self.LanRoute.Add(RouteHeader).Value := '<sip:roke.local;lr>';
  Self.LanRoute.Add(RouteHeader).Value := '<sip:vpn.local;lr>';
  Self.LanTarget := 'gont.local';

  Self.LocalhostAddressSpace := '127.0.0.0/8';
  Self.LocalhostRoute := TIdSipRoutePath.Create;
  Self.LocalhostRoute.Add(RouteHeader).Value := '<sip:127.0.0.1:35060;lr>';
  Self.LocalhostTarget := '127.0.0.2';
end;

procedure TestTIdProxyDescriptions.TearDown;
begin
  Self.LocalhostRoute.Free;
  Self.LanRoute.Free;
  Self.EmptyRoute.Free;
  Self.DefaultRoute.Free;
  Self.Proxies.Free;

  inherited TearDown;
end;

//* TestTIdProxyDescriptions Private methods ***********************************

procedure TestTIdProxyDescriptions.CheckEquals(Expected, Received: TIdSipRoutePath; Msg: String);
begin
  CheckEquals(Expected.Count, Received.Count, Msg + ': route path length');

  Expected.First;
  Received.First;
  while (Expected.HasNext) do begin
    CheckEquals(Expected.CurrentRoute.FullValue, Received.CurrentRoute.FullValue, Msg + ': unexpected Route');

    Expected.Next;
    Received.Next;
  end;
end;

procedure TestTIdProxyDescriptions.CheckExceptionAddedFirst;
const
  MainAddressSpace      = '10.0.0.0/8';
  ExceptionalAddress    = '10.0.0.0/24';
  MainSpaceAddress      = '10.0.1.1';
  ExceptionSpaceAddress = '10.0.0.1';
var
  ExceptionRoute: TIdSipRoutePath;
  MainRoute:      TIdSipRoutePath;
begin
  // Route: <null> 10.0.0.0/8
  // Route: <sip:proxy;lr> 10.0.0.0/24
  ExceptionRoute := TIdSipRoutePath.Create;
  try
    MainRoute := TIdSipRoutePath.Create;
    try
      MainRoute.Add(RouteHeader).Value := '<sip:proxy;lr>';

      Self.Proxies.AddDescription(ExceptionalAddress, ExceptionRoute);
      Self.Proxies.AddDescription(MainAddressSpace, MainRoute);

      Check(not Self.Proxies.RoutePathFor(MainSpaceAddress).IsEmpty,   'Wrong route path for main address space; exception added first');
      Check(    Self.Proxies.RoutePathFor(ExceptionSpaceAddress).IsEmpty, 'Wrong route path for exceptional address; exception added first');
    finally
      MainRoute.Free;
    end;
  finally
    ExceptionRoute.Free;
  end;
end;

procedure TestTIdProxyDescriptions.CheckExceptionAddedLast;
const
  MainAddressSpace      = 'tessier-ashpool.co.luna';
  ExceptionalAddress    = 'proxy.' + MainAddressSpace;
  MainSpaceAddress      = 'foo.' + MainAddressSpace;
  ExceptionSpaceAddress = 'foo.' + ExceptionalAddress;
var
  ExceptionRoute: TIdSipRoutePath;
  MainRoute:      TIdSipRoutePath;
begin
  // Route: <sip:proxy;lr> 10.0.0.0/24
  // Route: <null> 10.0.0.0/8
  ExceptionRoute := TIdSipRoutePath.Create;
  try
    MainRoute := TIdSipRoutePath.Create;
    try
      MainRoute.Add(RouteHeader).Value := '<sip:proxy;lr>';

      Self.Proxies.AddDescription(MainAddressSpace, MainRoute);
      Self.Proxies.AddDescription(ExceptionalAddress, ExceptionRoute);

      Check(not Self.Proxies.RoutePathFor(MainSpaceAddress).IsEmpty,   'Wrong route path for main address space; exception added last');
      Check(    Self.Proxies.RoutePathFor(ExceptionSpaceAddress).IsEmpty, 'Wrong route path for exceptional address; exception added last');
    finally
      MainRoute.Free;
    end;
  finally
    ExceptionRoute.Free;
  end;
end;

//* TestTIdProxyDescriptions Published methods *********************************

procedure TestTIdProxyDescriptions.TestAddDescriptionAndRoutePathFor;
begin
  Self.Proxies.AddDescription(Self.LanAddressSpace, Self.LanRoute);
  Self.Proxies.AddDescription(Self.LocalhostAddressSpace, Self.LocalhostRoute);

  CheckEquals(Self.LanRoute,       Self.Proxies.RoutePathFor(Self.LanTarget),       'LAN address space');
  CheckEquals(Self.LocalhostRoute, Self.Proxies.RoutePathFor(Self.LocalhostTarget), 'Localhost address space');
end;

procedure TestTIdProxyDescriptions.TestAddRoute;
var
  EmptyPath: TIdSipRoutePath;
begin
  EmptyPath := TIdSipRoutePath.Create;
  try
    Self.Proxies.AddDescription(Self.LanAddressSpace, EmptyPath);
  finally
    EmptyPath.Free;
  end;

  Self.LanRoute.First;
  while Self.LanRoute.HasNext do begin
    Self.Proxies.AddRouteFor(Self.LanAddressSpace, Self.LanRoute.CurrentRoute);
    Self.LanRoute.Next;
  end;

  CheckEquals(Self.LanRoute, Self.Proxies.RoutePathFor(Self.LanTarget), 'LAN routes not all added');
end;

procedure TestTIdProxyDescriptions.TestAddRouteNoProxyPresent;
begin
  Self.LanRoute.First;
  while Self.LanRoute.HasNext do begin
    Self.Proxies.AddRouteFor(Self.LanAddressSpace, Self.LanRoute.CurrentRoute);
    Self.LanRoute.Next;
  end;

  CheckEquals(Self.LanRoute, Self.Proxies.RoutePathFor(Self.LanTarget), 'LAN routes not all added');
end;

procedure TestTIdProxyDescriptions.TestAddRouteCanonicalisesNetmask;
begin
  Self.Proxies.AddDescription('10.0.0.0/8', Self.LanRoute);
  Self.Proxies.AddDescription('10.0.0.0/255.0.0.0', Self.LocalhostRoute);

  CheckEquals(1, Self.Proxies.Count, '10.0.0.0/8 and 10.0.0.0/255.0.0.0 are the same address space, but the ProxyDescriptions doesn''t recognise this');
end;

procedure TestTIdProxyDescriptions.TestClearAllDescriptions;
begin
  Self.Proxies.DefaultRoutePath := Self.DefaultRoute;
  Self.Proxies.AddDescription(Self.LanAddressSpace, Self.LanRoute);
  Self.Proxies.AddDescription(Self.LocalhostAddressSpace, Self.LocalhostRoute);

  Self.Proxies.ClearAllDescriptions;

  CheckEquals(Self.EmptyRoute, Self.Proxies.RoutePathFor(Self.LanTarget),       'LAN address space');
  CheckEquals(Self.EmptyRoute, Self.Proxies.RoutePathFor(Self.LocalhostTarget), 'Localhost address space');
  CheckEquals(Self.EmptyRoute, Self.Proxies.RoutePathFor('example.com'),        'Default address space');
end;

procedure TestTIdProxyDescriptions.TestContainedRoutesOrderIndependent;
begin
  // Previously, an address space for a destination was found on a first come,
  // first served basis. That means that if one wanted to have a description
  // like "all addresses in this address space, EXCEPT that address" one had to
  // be very careful about the order of the address spaces.

  Self.CheckExceptionAddedFirst;
  Self.CheckExceptionAddedLast;
end;

procedure TestTIdProxyDescriptions.TestRemoveDescription;
begin
  Self.Proxies.DefaultRoutePath := Self.DefaultRoute;
  Self.Proxies.AddDescription(Self.LanAddressSpace, Self.LanRoute);
  Self.Proxies.AddDescription(Self.LocalhostAddressSpace, Self.LocalhostRoute);

  Self.Proxies.RemoveDescription('unknown');
  CheckEquals(Self.LanRoute,       Self.Proxies.RoutePathFor(Self.LanTarget),       'LAN address space removed instead of "unknown"');
  CheckEquals(Self.LocalhostRoute, Self.Proxies.RoutePathFor(Self.LocalhostTarget), 'Localhost address space removed instead of "unknown"');

  Self.Proxies.RemoveDescription(Self.LanAddressSpace);
  CheckEquals(Self.DefaultRoute,   Self.Proxies.RoutePathFor(Self.LanTarget),       'LAN address space not removed');
  CheckEquals(Self.LocalhostRoute, Self.Proxies.RoutePathFor(Self.LocalhostTarget), 'Localhost address space removed instead of "unknown"');
end;

procedure TestTIdProxyDescriptions.TestRoutePathForDefaultRoute;
begin
  Self.Proxies.DefaultRoutePath := Self.DefaultRoute;
  CheckEquals(Self.DefaultRoute, Self.Proxies.RoutePathFor(Self.LanTarget), 'Default route, LAN target');
  CheckEquals(Self.DefaultRoute, Self.Proxies.RoutePathFor(Self.LocalhostTarget), 'Default route, Localhost target');

  Self.Proxies.AddDescription(Self.LanAddressSpace, Self.LanRoute);
  CheckEquals(Self.LanRoute, Self.Proxies.RoutePathFor(Self.LanTarget), 'LAN route added, LAN target');
  CheckEquals(Self.DefaultRoute, Self.Proxies.RoutePathFor(Self.LocalhostTarget), 'LAN route added, Localhost target');

  Self.Proxies.AddDescription(Self.LocalhostAddressSpace, Self.LocalhostRoute);
  CheckEquals(Self.LanRoute, Self.Proxies.RoutePathFor(Self.LanTarget), 'LAN & localhost routes added, LAN target');
  CheckEquals(Self.LocalhostRoute, Self.Proxies.RoutePathFor(Self.LocalhostTarget), 'LAN & localhost routes added, Localhost target');
end;

initialization
  RegisterTest('Proxy description tests', Suite);
end.
