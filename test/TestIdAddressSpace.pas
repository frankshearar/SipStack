{
  (c) 2008 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdAddressSpace;

interface

uses
  IdAddressSpace, TestFramework;

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

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdAddressSpace unit tests');
  Result.AddSuite(TestFunctions.Suite);
  Result.AddSuite(TestTIdAddressSpace.Suite);
  Result.AddSuite(TestTIdIPv4SubnetAddressSpace.Suite);
  Result.AddSuite(TestTIdIPv6SubnetAddressSpace.Suite);
  Result.AddSuite(TestTIdDomainAddressSpace.Suite);
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

initialization
  RegisterTest('Address space tests', Suite);
end.
