{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipLocator;

interface

uses
  Classes, IdSipLocator, IdSipMessage, IdSipMockLocator, TestFramework;

type
  TestTIdSipLocation = class(TTestCase)
  private
    Address:   String;
    Loc:       TIdSipLocation;
    Port:      Cardinal;
    Transport: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestCreate;
    procedure TestCreateFromVia;
  end;

  TestTIdSipLocations = class(TTestCase)
  private
    Locs: TIdSipLocations;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddLocation;
    procedure TestAddLocationsFromNames;
    procedure TestCount;
    procedure TestIsEmpty;
  end;

  TestTIdSipAbstractLocator = class(TTestCase)
  private
    ARecord:        String;
    AAAARecord:     String;
    Domain:         String;
    IP:             String;
    Loc:            TIdSipMockLocator;
    NameRecs:       TIdDomainNameRecords;
    Naptr:          TIdNaptrRecords;
    Port:           Cardinal;
    Srv:            TIdSrvRecords;
    Target:         TIdSipUri;
    TransportParam: String;

    procedure AddNameRecords(const Domain: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckAorAAAARecords(Locations: TIdSipLocations;
                                  const ExpectedTransport: String;
                                  const MsgPrefix: String);
  published
    procedure TestFindServersForResponseWithNameAndPort;
    procedure TestFindServersForResponseWithNameNoSrv;
    procedure TestFindServersForResponseWithNumericSentBy;
    procedure TestFindServersForResponseWithNumericSentByAndPort;
    procedure TestFindServersForResponseWithReceivedParam;
    procedure TestFindServersForResponseWithReceivedParamAndRport;
    procedure TestFindServersForResponseWithReceivedParamAndNumericSentBy;
    procedure TestFindServersForResponseWithReceivedParamAndIPv6NumericSentBy;
    procedure TestFindServersForResponseWithRport;
    procedure TestFindServersForResponseWithSrv;
    procedure TestNameAndPortWithTransportParam;
    procedure TestNameNoNaptrNoSrv;
    procedure TestNameNaptrSomeSrv;
    procedure TestNumericAddressNonStandardPort;
    procedure TestNumericAddressUsesUdp;
    procedure TestNumericAddressSipsUriUsesTls;
    procedure TestNumericAddressSipsUriNonStandardPort;
    procedure TestNumericMaddr;
    procedure TestNumericMaddrIPv6;
    procedure TestNumericMaddrSips;
    procedure TestNumericMaddrSipsIPv6;
    procedure TestSrvNoNameRecords;
    procedure TestSrvNotAvailable;
    procedure TestSrvTarget;
    procedure TestTransportParamTakesPrecedence;
    procedure TestTransportFor;
    procedure TestTransportForNameAndExplicitPort;
    procedure TestTransportForNumericIPv4;
    procedure TestTransportForNumericIPv6;
    procedure TestTransportForWithNaptr;
    procedure TestTransportForWithoutNaptrAndNoSrv;
    procedure TestTransportForWithoutNaptrWithSrv;
    procedure TestWithoutNaptrWithSrv;
    procedure TestFindServersForNameNaptrNoSrv;
    procedure TestFindServersForNameNaptrSrv;
    procedure TestFindServersForNameNoNaptrManualTransportNoSrv;
    procedure TestFindServersForNameNoNaptrManualTransportSrv;
    procedure TestFindServersForNameNoNaptrNoManualTransportNoSrv;
    procedure TestFindServersForNameNoNaptrNoManualTransportSrv;
    procedure TestFindServersForNameWithPort;
    procedure TestFindServersForNumericAddress;
    procedure TestFindServersForNumericAddressWithPort;
  end;

  TestTIdSipMockLocator = class(TTestCase)
  private
    AOR: TIdUri;
    Loc: TIdSipMockLocator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestResolveNameRecords;
    procedure TestResolveNAPTRSip;
    procedure TestResolveNAPTRSips;
    procedure TestResolveSRV;
    procedure TestResolveSRVWithNameRecords;
  end;

  TestTIdDomainNameRecord = class(TTestCase)
  private
    Domain:     String;
    IPAddress:  String;
    Rec:        TIdDomainNameRecord;
    RecordType: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestInstantiation;
  end;

  TestTIdDomainNameRecords = class(TTestCase)
  private
    List: TIdDomainNameRecords;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAddWithParameters;
    procedure TestClear;
    procedure TestCopy;
    procedure TestIsEmpty;
  end;

  TestTIdNaptrRecord = class(TTestCase)
  private
    Flags:      String;
    Key:        String;
    Order:      Word;
    Preference: Word;
    Rec:        TIdNaptrRecord;
    Regex:      String;
    Service:    String;
    Value:      String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsSipTransport;
    procedure TestCopy;
    procedure TestInstantiation;
    procedure TestIsSecureService;
  end;

  TestTIdNaptrRecords = class(TTestCase)
  private
    List: TIdNaptrRecords;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAddWithParameters;
    procedure TestAnyAppropriateRecord;
    procedure TestClear;
    procedure TestDelete;
    procedure TestIsEmpty;
    procedure TestSort;
  end;

  TestTIdSrvRecord = class(TTestCase)
  private
    Domain:      String;
    NameRecords: TIdDomainNameRecords;
    Port:        Cardinal;
    Priority:    Word;
    Rec:         TIdSrvRecord;
    Service:     String;
    Target:      String;
    Weight:      Word;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestInstantiation;
    procedure TestQueryName;
    procedure TestSipTransport;
  end;

  TestTIdSrvRecords = class(TTestCase)
  private
    List: TIdSrvRecords;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAddWithParameters;
    procedure TestAddNameRecord;
    procedure TestClear;
    procedure TestLast;
    procedure TestIsEmpty;
  end;

implementation

uses
  IdSipConsts, Math, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipLocator unit tests');
  Result.AddTest(TestTIdSipLocation.Suite);
  Result.AddTest(TestTIdSipLocations.Suite);
  Result.AddTest(TestTIdSipAbstractLocator.Suite);
  Result.AddTest(TestTIdSipMockLocator.Suite);
  Result.AddTest(TestTIdDomainNameRecord.Suite);
  Result.AddTest(TestTIdDomainNameRecords.Suite);
  Result.AddTest(TestTIdNaptrRecord.Suite);
  Result.AddTest(TestTIdNaptrRecords.Suite);
  Result.AddTest(TestTIdSrvRecord.Suite);
  Result.AddTest(TestTIdSrvRecords.Suite);
end;

//******************************************************************************
//* TestTIdSipLocation                                                         *
//******************************************************************************
//* TestTIdSipLocation Public methods ******************************************

procedure TestTIdSipLocation.SetUp;
begin
  inherited SetUp;

  Self.Address   := '127.0.0.1';
  Self.Port      := 9999;
  Self.Transport := TcpTransport;

  Self.Loc := TIdSipLocation.Create(Self.Transport, Self.Address, Self.Port);
end;

procedure TestTIdSipLocation.TearDown;
begin
  Self.Loc.Free;

  inherited TearDown;
end;

//* TestTIdSipLocation Published methods ***************************************

procedure TestTIdSipLocation.TestCopy;
var
  Copy: TIdSipLocation;
begin
  Copy := Self.Loc.Copy;
  try
    CheckEquals(Self.Loc.IPAddress, Copy.IPAddress, 'IPAddress');
    CheckEquals(Self.Loc.Port,      Copy.Port,      'Port');
    CheckEquals(Self.Loc.Transport, Copy.Transport, 'Transport');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdSipLocation.TestCreate;
begin
  CheckEquals(Self.Address,   Self.Loc.IPAddress, 'IPAddress');
  CheckEquals(Self.Port,      Self.Loc.Port,      'Port');
  CheckEquals(Self.Transport, Self.Loc.Transport, 'Transport');
end;

procedure TestTIdSipLocation.TestCreateFromVia;
var
  Loc: TIdSipLocation;
  Via: TIdSipViaHeader;
begin
  Via := TIdSipViaHeader.Create;
  try
    Via.Port      := Self.Port;
    Via.SentBy    := Self.Address;
    Via.Transport := Self.Transport;

    Loc := TIdSipLocation.Create(Via);
    try
      CheckEquals(Via.Port,      Loc.Port,      'Port');
      CheckEquals(Via.SentBy,    Loc.IPAddress, 'IPAddress');
      CheckEquals(Via.Transport, Loc.Transport, 'Transport');
    finally
      Loc.Free;
    end;
  finally
    Via.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipLocations                                                        *
//******************************************************************************
//* TestTIdSipLocations Public methods *****************************************

procedure TestTIdSipLocations.SetUp;
begin
  inherited SetUp;

  Self.Locs := TIdSipLocations.Create;
end;

procedure TestTIdSipLocations.TearDown;
begin
  Self.Locs.Free;

  inherited TearDown;
end;

//* TestTIdSipLocations Published methods **************************************

procedure TestTIdSipLocations.TestAddLocation;
const
  Transport = TcpTransport;
  Address   = 'foo.com';
  Port      = IdPORT_SIP;
begin
  Self.Locs.AddLocation(Transport, Address, Port);

  CheckEquals(Address,   Self.Locs.First.IPAddress, 'IPAddress');
  CheckEquals(Port,      Self.Locs.First.Port,      'Port');
  CheckEquals(Transport, Self.Locs.First.Transport, 'Transport');
end;

procedure TestTIdSipLocations.TestAddLocationsFromNames;
const
  Transport = TcpTransport;
  Port      = IdPORT_SIP;
var
  I:     Integer;
  Names: TIdDomainNameRecords;
begin
  Names := TIdDomainNameRecords.Create;
  try
    Names.Add(DnsAAAARecord, 'foo.com', '::1');
    Names.Add(DnsARecord,    'bar.com', '127.0.0.1');

    for I := 0 to Min(Names.Count, Self.Locs.Count) - 1 do begin
      CheckEquals(Transport,
                  Self.Locs[I].Transport,
                  IntToStr(I) + 'th location transport');
      CheckEquals(Names[I].IPAddress,
                  Self.Locs[I].IPAddress,
                  IntToStr(I) + 'th location address');
      CheckEquals(Port,
                  Self.Locs[I].Port,
                  IntToStr(I) + 'th location port');
    end;

    Self.Locs.AddLocationsFromNames(Transport, Port, Names);

    CheckEquals(Names.Count,
                Self.Locs.Count,
                'Number of records');
  finally
    Names.Free;
  end;
end;

procedure TestTIdSipLocations.TestCount;
var
  I: Integer;
begin
  CheckEquals(0, Self.Locs.Count, 'Empty list');

  for I := 1 to 5 do begin
    Self.Locs.AddLocation(TcpTransport, 'foo.com', I);
    CheckEquals(I,
                Self.Locs.Count,
                'Added ' + IntToStr(I) + ' item(s)');
  end;
end;

procedure TestTIdSipLocations.TestIsEmpty;
var
  I: Integer;
begin
  Check(Self.Locs.IsEmpty, 'Empty list');

  for I := 1 to 5 do begin
    Self.Locs.AddLocation(TcpTransport, 'foo.com', I);
    Check(not Self.Locs.IsEmpty,
          'IsEmpty after ' + IntToStr(I) + ' item(s)');
  end;
end;

//******************************************************************************
//* TestTIdSipAbstractLocator                                                  *
//******************************************************************************
//* TestTIdSipAbstractLocator Public methods ***********************************

procedure TestTIdSipAbstractLocator.SetUp;
begin
  inherited SetUp;

  Self.ARecord    := '127.0.0.1';
  Self.AAAARecord := '::1';
  Self.Domain     := 'foo.com';
  Self.IP         := '127.0.0.1';
  Self.Loc        := TIdSipMockLocator.Create;
  Self.NameRecs   := TIdDomainNameRecords.Create;
  Self.Naptr      := TIdNaptrRecords.Create;
  Self.Port       := IdPORT_SIP;
  Self.Srv        := TIdSrvRecords.Create;
  Self.Target     := TIdSipUri.Create;
end;

procedure TestTIdSipAbstractLocator.TearDown;
begin
  Self.Target.Free;
  Self.Srv.Free;
  Self.Naptr.Free;
  Self.NameRecs.Free;
  Self.Loc.Free;

  inherited Destroy;
end;

procedure TestTIdSipAbstractLocator.CheckAorAAAARecords(Locations: TIdSipLocations;
                                                        const ExpectedTransport: String;
                                                        const MsgPrefix: String);
begin
  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    CheckEquals(Self.Loc.NameRecords.Count,
                Locations.Count, MsgPrefix + ': Location count');

    CheckEquals(ExpectedTransport,
                Locations[0].Transport,
                MsgPrefix + ': 1st record Transport');
    CheckEquals(Self.ARecord,
                Locations[0].IPAddress,
                MsgPrefix + ': 1st record IPAddress');
    CheckEquals(Self.Port,
                Locations[0].Port,
                MsgPrefix + ': 1st record Port');

    CheckEquals(ExpectedTransport,
                Locations[1].Transport,
                MsgPrefix + ': 2nd record Transport');
    CheckEquals(Self.AAAARecord,
                Locations[1].IPAddress,
                MsgPrefix + ': 2nd record IPAddress');
    CheckEquals(Self.Port,
                Locations[1].Port,
                MsgPrefix + ': 2nd record Port');
  finally
    Locations.Free;
  end;
end;

//* TestTIdSipAbstractLocator Private methods **********************************

procedure TestTIdSipAbstractLocator.AddNameRecords(const Domain: String);
begin
  Self.Loc.AddA(   Self.Domain, Self.ARecord);
  Self.Loc.AddAAAA(Self.Domain, Self.AAAARecord);
end;

//* TestTIdSipAbstractLocator Published methods ********************************

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithNameAndPort;
var
  Locations: TIdSipLocations;
  Response:  TIdSipResponse;
begin
  Self.Loc.AddAAAA(Self.Domain, Self.AAAARecord);
  Self.Loc.AddA(   Self.Domain, Self.ARecord);

  Response := TIdSipResponse.Create;
  try
    Response.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/UDP ' + Domain + ':6666';

    Locations := Self.Loc.FindServersFor(Response);
    try
      CheckEquals(Response.LastHop.Transport,
                  Locations[0].Transport,
                  'First location transport');
      CheckEquals(Self.Loc.NameRecords[0].IPAddress,
                  Locations[0].IPAddress,
                  'First location address');
      CheckEquals(Response.LastHop.Port,
                  Locations[0].Port,
                  'First location port');

      CheckEquals(Response.LastHop.Transport,
                  Locations[1].Transport,
                  'Second location transport');
      CheckEquals(Self.Loc.NameRecords[1].IPAddress,
                  Locations[1].IPAddress,
                  'Second location address');
      CheckEquals(Response.LastHop.Port,
                  Locations[1].Port,
                  'Second location port');
    finally
      Locations.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithNameNoSrv;
var
  Locations: TIdSipLocations;
  Response:  TIdSipResponse;
begin
  Self.Loc.AddAAAA(Self.Domain, Self.AAAARecord);
  Self.Loc.AddA(   Self.Domain, Self.ARecord);

  Response := TIdSipResponse.Create;
  try
    Response.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/UDP ' + Domain;

    Locations := Self.Loc.FindServersFor(Response);
    try
      CheckEquals(Response.LastHop.Transport,
                  Locations[0].Transport,
                  'First location transport');
      CheckEquals(Self.Loc.NameRecords[0].IPAddress,
                  Locations[0].IPAddress,
                  'First location address');
      CheckEquals(Response.LastHop.Port,
                  Locations[0].Port,
                  'First location port');

      CheckEquals(Response.LastHop.Transport,
                  Locations[1].Transport,
                  'Second location transport');
      CheckEquals(Self.Loc.NameRecords[1].IPAddress,
                  Locations[1].IPAddress,
                  'Second location address');
      CheckEquals(Response.LastHop.Port,
                  Locations[1].Port,
                  'Second location port');
    finally
      Locations.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithNumericSentBy;
var
  Locations: TIdSipLocations;
  Response:  TIdSipResponse;
begin
  Response := TIdSipResponse.Create;
  try
    Response.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/UDP 127.0.0.1';

    Locations := Self.Loc.FindServersFor(Response);
    try
      CheckEquals(Response.LastHop.Transport,
                  Locations[0].Transport,
                  'First location transport');
      CheckEquals(Self.IP,
                  Locations[0].IPAddress,
                  'First location address');
      CheckEquals(Response.LastHop.Port,
                  Locations[0].Port,
                  'First location port');
    finally
      Locations.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithNumericSentByAndPort;
var
  Locations: TIdSipLocations;
  Response:  TIdSipResponse;
begin
  Response := TIdSipResponse.Create;
  try
    Response.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/UDP 127.0.0.1:666';

    Locations := Self.Loc.FindServersFor(Response);
    try
      CheckEquals(Response.LastHop.Transport,
                  Locations[0].Transport,
                  'First location transport');
      CheckEquals(Self.IP,
                  Locations[0].IPAddress,
                  'First location address');
      CheckEquals(Response.LastHop.Port,
                  Locations[0].Port,
                  'First location port');
    finally
      Locations.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithReceivedParam;
var
  Locations: TIdSipLocations;
  Response:  TIdSipResponse;
begin
  Response := TIdSipResponse.Create;
  try
    Response.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/UDP gw1.leo-ix.net;received=' + Self.IP;

    Locations := Self.Loc.FindServersFor(Response);
    try
      CheckEquals(Response.LastHop.Transport,
                  Locations[0].Transport,
                  'First location transport');
      CheckEquals(Response.LastHop.Received,
                  Locations[0].IPAddress,
                  'First location address');
      CheckEquals(Response.LastHop.Port,
                  Locations[0].Port,
                  'First location port');
    finally
      Locations.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithReceivedParamAndRport;
var
  Locations: TIdSipLocations;
  Response:  TIdSipResponse;
begin
  Response := TIdSipResponse.Create;
  try
    Self.Port := 6666;
    Response.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/UDP gw1.leo-ix.net'
                                             + ';received=' + Self.IP
                                             + ';rport=' + IntToStr(Self.Port);

    Locations := Self.Loc.FindServersFor(Response);
    try
      CheckEquals(Response.LastHop.Transport,
                  Locations[0].Transport,
                  'First location transport');
      CheckEquals(Response.LastHop.Received,
                  Locations[0].IPAddress,
                  'First location address');
      CheckEquals(Response.LastHop.Rport,
                  Locations[0].Port,
                  'First location port');
    finally
      Locations.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithReceivedParamAndNumericSentBy;
const
  SentByIP = '6.6.6.6';
var
  Locations: TIdSipLocations;
  Response:  TIdSipResponse;
begin
  Response := TIdSipResponse.Create;
  try
    Response.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/UDP ' + SentByIP + ';received=' + Self.IP;

    Locations := Self.Loc.FindServersFor(Response);
    try
      CheckEquals(Response.LastHop.Transport,
                  Locations[1].Transport,
                  'First location transport');
      CheckEquals(SentByIP,
                  Locations[1].IPAddress,
                  'First location address');
      CheckEquals(Response.LastHop.Port,
                  Locations[1].Port,
                  'First location port');
    finally
      Locations.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithReceivedParamAndIpv6NumericSentBy;
const
  SentByIP = '[2002:dead:beef:1::1]';
var
  Locations: TIdSipLocations;
  Response:  TIdSipResponse;
begin
  Response := TIdSipResponse.Create;
  try
    Response.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/UDP ' + SentByIP + ';received=' + Self.IP;

    Locations := Self.Loc.FindServersFor(Response);
    try
      CheckEquals(Response.LastHop.Transport,
                  Locations[1].Transport,
                  'First location transport');
      CheckEquals(SentByIP,
                  Locations[1].IPAddress,
                  'First location address');
      CheckEquals(Response.LastHop.Port,
                  Locations[1].Port,
                  'First location port');
    finally
      Locations.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithRport;
var
  Locations: TIdSipLocations;
  Response:  TIdSipResponse;
begin
  Response := TIdSipResponse.Create;
  try
    Response.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/UDP 127.0.0.1;rport=666';

    Locations := Self.Loc.FindServersFor(Response);
    try
      CheckEquals(Response.LastHop.Transport,
                  Locations[0].Transport,
                  'First location transport');
      CheckEquals(Self.IP,
                  Locations[0].IPAddress,
                  'First location address');
      CheckEquals(Response.LastHop.Port,
                  Locations[0].Port,
                  'First location port: must ignore rport');
    finally
      Locations.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithSrv;
const
  SecondRecord = '::2';
  ThirdRecord  = '::3';
var
  Locations: TIdSipLocations;
  Response:  TIdSipResponse;
  TlsDomain: String;
begin
  TlsDomain := 'sips.' + Self.Domain;

  Self.Loc.AddSRV(Self.Domain, SrvTlsPrefix, 0, 0, IdPORT_SIPS, TlsDomain);
  Self.Loc.AddSRV(Self.Domain, SrvTcpPrefix, 0, 0, IdPORT_SIP,  Self.Domain);

  Self.Loc.AddAAAA(Self.Domain, Self.AAAARecord);
  Self.Loc.AddAAAA(TlsDomain, SecondRecord);
  Self.Loc.AddAAAA(TlsDomain, ThirdRecord);

  Response := TIdSipResponse.Create;
  try
    Response.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TLS ' + Self.Domain;
    Locations := Self.Loc.FindServersFor(Response);
    try
      CheckEquals(2,
                  Locations.Count,
                  'Wrong number of records');

      CheckEquals(TlsTransport,
                  Locations[0].Transport,
                  '1st record transport');
      CheckEquals(SecondRecord,
                  Locations[0].IPAddress,
                  '1st record address');
      CheckEquals(IdPORT_SIPS,
                  Locations[0].Port,
                  '1st record port');
      CheckEquals(TlsTransport,
                  Locations[1].Transport,
                  '2nd record transport');
      CheckEquals(ThirdRecord,
                  Locations[1].IPAddress,
                  '2nd record address');
      CheckEquals(IdPORT_SIPS,
                  Locations[1].Port,
                  '2nd record port');
    finally
      Locations.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNameAndPortWithTransportParam;
var
  Locations: TIdSipLocations;
begin
  // Iterate over the SRV RRs for _sip._udp (despite the remote side preferring
  // TLS).
  Self.Target.Uri := 'sip:example.com;transport=udp';

  Self.Loc.AddNAPTR(Self.IP,  50, 50, 's', NaptrTlsService, '_sips._tcp.example.com');
  Self.Loc.AddNAPTR(Self.IP,  90, 50, 's', NaptrTcpService, '_sip._tcp.example.com');
  Self.Loc.AddNAPTR(Self.IP, 100, 50, 's', NaptrUdpService, '_sip._udp.example.com');
  Self.Loc.AddSRV('example.com', '_sips._tcp', 0, 0, 5061, 'paranoid.example.com');
  Self.Loc.AddSRV('example.com', '_sip._tcp', 0, 0, 5061,  'reliable.example.com');
  Self.Loc.AddSRV('example.com', '_sip._udp', 0, 0, 5061,  'unreliable.example.com');
  Self.Loc.AddA('paranoid.example.com',   '127.0.0.1');
  Self.Loc.AddA('reliable.example.com',   '127.0.0.2');
  Self.Loc.AddA('unreliable.example.com', '127.0.0.3');

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    CheckEquals(1, Locations.Count, 'Number of locations');
    CheckEquals('127.0.0.3', Locations[0].IPAddress, 'Wrong location');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNameNoNaptrNoSrv;
var
  Locations: TIdSipLocations;
begin
  // Use all A/AAAA RRs for the host
  Self.Target.Uri := 'sip:' + Self.Domain;

  Self.AddNameRecords(Self.Target.Host);

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Self.CheckAorAAAARecords(Locations, UdpTransport, 'No NAPTR no SRV');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNameNaptrSomeSrv;
var
  Locations: TIdSipLocations;
begin
  // Iterate over SRV records
  Self.Target.Uri := 'sip:' + Self.Domain;

  // We have two NAPTR records. Probably as a result of an admin slip-up,
  // there're no SRV records for the first NAPTR. This test shows that we look
  // up SRV stuff for NAPTR records at least until we find some SRVs.
  Self.Loc.AddNAPTR(Self.Domain, 0, 0, NaptrDefaultFlags, NaptrTlsService, SrvTlsPrefix + '.' + Self.Domain);
  Self.Loc.AddNAPTR(Self.Domain, 0, 0, NaptrDefaultFlags, NaptrTcpService, SrvTcpPrefix + '.' + Self.Domain);
  Self.Loc.AddSRV(Self.Domain, SrvTcpPrefix, 0, 0, IdPORT_SIP, 'sip.' + Self.Domain);
  Self.Loc.AddAAAA('sip.' + Self.Domain, Self.AAAARecord);

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    CheckEquals(Self.Loc.NameRecords.Count,
                Locations.Count,
                'No SRV lookup for NAPTR records beyond the first?');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNumericAddressNonStandardPort;
var
  Locations: TIdSipLocations;
begin
  Self.Port       := 3000;
  Self.Target.Uri := 'sip:' + Self.IP + ':' + IntToStr(Self.Port);

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(UdpTransport, Locations.First.Transport, 'Transport');
    CheckEquals(Self.IP,      Locations.First.IPAddress, 'IPAddress');
    CheckEquals(Self.Port,    Locations.First.Port,      'Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNumericAddressUsesUdp;
var
  Locations: TIdSipLocations;
begin
  Self.Target.Uri := 'sip:' + Self.IP;

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(UdpTransport, Locations.First.Transport, 'Transport');
    CheckEquals(Self.IP,      Locations.First.IPAddress, 'IPAddress');
    CheckEquals(Self.Port,    Locations.First.Port,      'Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNumericAddressSipsUriUsesTls;
var
  Locations: TIdSipLocations;
begin
  Self.Port       := IdPORT_SIPS;
  Self.Target.Uri := 'sips:' + Self.IP;

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(TlsTransport, Locations.First.Transport, 'Transport');
    CheckEquals(Self.IP,      Locations.First.IPAddress, 'IPAddress');
    CheckEquals(Self.Port,    Locations.First.Port,      'Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNumericAddressSipsUriNonStandardPort;
var
  Locations: TIdSipLocations;
begin
  Self.Port       := 3000;
  Self.Target.Uri := 'sips:' + Self.IP + ':' + IntToStr(Self.Port);

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(TlsTransport, Locations.First.Transport, 'Transport');
    CheckEquals(Self.IP,      Locations.First.IPAddress, 'IPAddress');
    CheckEquals(Self.Port,    Locations.First.Port,      'Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNumericMaddr;
var
  Locations: TIdSipLocations;
begin
  Self.Target.Uri := 'sip:foo.com;maddr=' + Self.IP;

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(UdpTransport, Locations.First.Transport, 'Transport');
    CheckEquals(Self.IP,      Locations.First.IPAddress, 'IPAddress');
    CheckEquals(Self.Port,    Locations.First.Port,      'Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNumericMaddrIPv6;
var
  Locations: TIdSipLocations;
begin
  Self.Target.Uri := 'sip:foo.com;maddr=' + Self.AAAARecord;

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(UdpTransport,    Locations.First.Transport, 'Transport');
    CheckEquals(Self.AAAARecord, Locations.First.IPAddress, 'IPAddress');
    CheckEquals(Self.Port,       Locations.First.Port,      'Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNumericMaddrSips;
var
  Locations: TIdSipLocations;
begin
  Self.Target.Uri := 'sips:foo.com;maddr=' + Self.ARecord;

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(TlsTransport, Locations.First.Transport, 'Transport');
    CheckEquals(Self.ARecord, Locations.First.IPAddress, 'IPAddress');
    CheckEquals(IdPORT_SIPS,  Locations.First.Port,      'Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNumericMaddrSipsIPv6;
var
  Locations: TIdSipLocations;
begin
  Self.Target.Uri := 'sips:foo.com;maddr=' + Self.AAAARecord;

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(TlsTransport,    Locations.First.Transport, 'Transport');
    CheckEquals(Self.AAAARecord, Locations.First.IPAddress, 'IPAddress');
    CheckEquals(IdPORT_SIPS,     Locations.First.Port,      'Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestSrvNoNameRecords;
var
  Locations: TIdSipLocations;
begin
  Self.Target.Uri :='sip:' + Self.Domain;

  Self.Loc.AddSRV(Self.Domain, SrvTcpPrefix,  0, 0, 0, Self.Domain);
  Self.Loc.AddSRV(Self.Domain, SrvSctpPrefix, 0, 0, 0, Self.Domain);

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.IsEmpty,
          'The locator added locations that don''t exist');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestSrvNotAvailable;
var
  Locations: TIdSipLocations;
begin
  // SRV targets can sometimes be '.' - the root name of all domain names.
  // We ignore them (they mean "we don't support the service you're looking
  // for"). Once we have the SRV records though we need A/AAAA records to
  // get the actual IP addresses we want to contact.

  Self.Target.Uri :='sip:' + Self.Domain;

  Self.Loc.AddSRV(Self.Domain, SrvTcpPrefix,  0, 0, 0, SrvNotAvailableTarget);
  Self.Loc.AddSRV(Self.Domain, SrvSctpPrefix, 0, 0, 0, Self.Domain);

  Self.Loc.AddA(Self.Domain, Self.ARecord);

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    CheckEquals(1,
                Locations.Count,
                'The locator didn''t filter out the "unavailable" SRV');
    CheckEquals(SctpTransport, Locations[0].Transport, 'Wrong location found');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestSrvTarget;
begin
  CheckEquals('_sip._tcp.leo-ix.net',
              Self.Loc.SrvTarget(false, 'tcp', 'leo-ix.net'),
              'SIP/TCP lookup');

  CheckEquals('_sips._tcp.leo-ix.net',
              Self.Loc.SrvTarget(true, 'tcp', 'leo-ix.net'),
              'SIP/TLS lookup');

  CheckEquals('_sip._tcp.leo-ix.net',
              Self.Loc.SrvTarget(false, 'TCP', 'leo-ix.net'),
              'Transports all lowercase');

  CheckEquals('_sip._.',
              Self.Loc.SrvTarget(false, '', ''),
              'Insane lookup: GIGO');
end;

procedure TestTIdSipAbstractLocator.TestTransportParamTakesPrecedence;
var
  Locations: TIdSipLocations;
begin
  Self.TransportParam := TransportParamSCTP;
  Self.Target.Uri := 'sip:127.0.0.1;transport=' + Self.TransportParam;

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(ParamToTransport(Self.TransportParam),
                Locations.First.Transport,
                'Transport');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestTransportFor;
begin
  Self.TransportParam := TransportParamSCTP;
  Self.Target.Uri := 'sip:foo.com;transport=' + Self.TransportParam;

  CheckEquals(ParamToTransport(Self.TransportParam),
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'Transport parameter must take precedence');
end;

procedure TestTIdSipAbstractLocator.TestTransportForNameAndExplicitPort;
begin
  Self.Target.Uri := 'sip:' + Self.Domain + ':' + IntToStr(Self.Port);

  CheckEquals(UdpTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Name, explicit port');

  Self.Target.Uri := 'sips:' + Self.Domain + ':' + IntToStr(Self.Port);

  CheckEquals(TlsTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIPS, Name, explicit port');
end;

procedure TestTIdSipAbstractLocator.TestTransportForNumericIPv4;
begin
  Self.Target.Uri := 'sip:' + Self.IP;

  CheckEquals(UdpTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Numeric IPv4 address');

  Self.Target.Uri := 'sip:' + Self.IP + ':' + IntToStr(Self.Port);
  CheckEquals(UdpTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Numeric IPv4 address, explicit port');

  Self.Target.Scheme := SipsScheme;
  CheckEquals(TlsTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIPS, Numeric IPv4 address');

  Self.Target.Uri := 'sips:' + Self.IP + ':' + IntToStr(Self.Port);
  CheckEquals(TlsTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIPS, Numeric IPv4 address, explicit port');
end;

procedure TestTIdSipAbstractLocator.TestTransportForNumericIPv6;
begin
  Self.IP := '[::1]';
  Self.Target.Uri := 'sip:' + Self.IP;

  CheckEquals(UdpTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Numeric IPv6 address');

  Self.Target.Uri := 'sip:' + Self.IP + ':' + IntToStr(Self.Port);
  CheckEquals(UdpTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Numeric IPv6 address, port');

  Self.Target.Scheme := SipsScheme;
  CheckEquals(TlsTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIPS, Numeric IPv6 address');

  Self.Target.Uri := 'sips:' + Self.IP + ':' + IntToStr(Self.Port);
  CheckEquals(TlsTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIPS, Numeric IPv6 address, explicit port');
end;

procedure TestTIdSipAbstractLocator.TestTransportForWithNaptr;
begin
  Self.IP         := 'example.com';
  Self.Target.Uri := 'sip:' + Self.IP;

  // Values shamelessly stolen from RFC 3263, section 4.1
  // ;           order pref flags service      regexp  replacement
  //    IN NAPTR 50   50  "s"  "SIPS+D2T"     ""  _sips._tcp.example.com.
  //    IN NAPTR 90   50  "s"  "SIP+D2T"      ""  _sip._tcp.example.com
  //    IN NAPTR 100  50  "s"  "SIP+D2U"      ""  _sip._udp.example.com.
  Self.Loc.AddNAPTR(Self.IP,  50, 50, 's', NaptrTlsService, '_sips._tcp.example.com');
  Self.Loc.AddNAPTR(Self.IP,  90, 50, 's', NaptrTcpService, '_sip._tcp.example.com');
  Self.Loc.AddNAPTR(Self.IP, 100, 50, 's', NaptrUdpService, '_sip._udp.example.com');

  CheckEquals(Self.Loc.NAPTR[0].AsSipTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'Name, NAPTR records');
end;

procedure TestTIdSipAbstractLocator.TestTransportForWithoutNaptrAndNoSrv;
begin
  Self.IP         := 'example.com';
  Self.Target.Uri := 'sip:' + Self.IP;

  CheckEquals(UdpTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Name, no NAPTR records, no SRV records');

  Self.Target.Scheme := SipsScheme;

  CheckEquals(TlsTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIPS, Name, no NAPTR records, no SRV records');
end;

procedure TestTIdSipAbstractLocator.TestTransportForWithoutNaptrWithSrv;
begin
  Self.IP         := 'example.com';
  Self.Target.Uri := 'sip:' + Self.IP;

  // Values shamelessly stolen from RFC 3263, section 4.1
  // ;;          Priority Weight Port   Target
  //     IN SRV  0        1      5060   server1.example.com
  //     IN SRV  0        2      5060   server2.example.com
  Self.Loc.AddSRV('example.com', '_sip._tcp', 0, 1, 5060, 'server1.example.com');
  Self.Loc.AddSRV('example.com', '_sip._tcp', 0, 2, 5060, 'server2.example.com');

  CheckEquals(TcpTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Name, no NAPTR records, but SRV records');

  Self.Target.Scheme := SipsScheme;
  CheckEquals(TcpTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIPS, Name, no NAPTR records, but SRV records (none acceptable)');
end;

procedure TestTIdSipAbstractLocator.TestWithoutNaptrWithSrv;
var
  Locations: TIdSipLocations;
begin
  Self.IP         := 'example.com';
  Self.Target.Uri := 'sip:' + Self.IP;

  // Values shamelessly stolen from RFC 3263, section 4.1
  // ;;          Priority Weight Port   Target
  //     IN SRV  0        1      5060   server1.example.com
  //     IN SRV  0        2      5060   server2.example.com
  Self.Loc.AddSRV('example.com', '_sip._tcp', 0, 2, 5060, 'server1.example.com');
  Self.Loc.AddSRV('example.com', '_sip._tcp', 0, 1, 5060, 'server2.example.com');

  Self.Loc.AddA('server1.example.com', '127.0.0.1');
  Self.Loc.AddA('server2.example.com', '127.0.0.2');

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 1, 'Too few locations');
    CheckEquals('127.0.0.1', Locations[0].IPAddress, '1st record address');
    CheckEquals('127.0.0.2', Locations[1].IPAddress, '2nd record address');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameNaptrNoSrv;
var
  Locations: TIdSipLocations;
begin
  // The target's a domain name, we've NAPTR records from the transport lookup,
  // but no SRV RRs.
  Self.Target.Uri := 'sip:' + Self.Domain;
  Self.Loc.AddNAPTR(Self.Domain, 0, 0, NaptrDefaultFlags, NaptrTcpService,
                    '_sip._tcp.' + Self.Domain);
  Self.AddNameRecords(Self.Target.Host);

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Self.CheckAorAAAARecords(Locations,
                             Self.Loc.NAPTR[0].AsSipTransport,
                             'NAPTR, no SRV');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameNaptrSrv;
var
  Locations: TIdSipLocations;
begin
  // The target's a domain name, we've NAPTR records from the transport lookup,
  // and we've SRV RRs.
  Self.Target.Uri := 'sip:' + Self.Domain;
  Self.Loc.AddNAPTR(Self.Domain, 0, 0, NaptrDefaultFlags, NaptrTlsService,
                    SrvTlsPrefix + '.' + Self.Domain);
  Self.Loc.AddNAPTR(Self.Domain, 1, 0, NaptrDefaultFlags, NaptrTcpService,
                    SrvTcpPrefix + '.' + Self.Domain);
  Self.Loc.AddSRV(Self.Domain, SrvTlsPrefix, 0, 0, 0, Self.Domain);
  Self.Loc.AddSRV(Self.Domain, SrvTcpPrefix, 1, 0, 0, Self.Domain);

  Self.AddNameRecords(Self.Target.Host);

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    CheckEquals(4,                    Locations.Count,        'Location count');
    CheckEquals(TlsTransport,         Locations[0].Transport, '1st record Transport');
    CheckEquals(Self.ARecord,         Locations[0].IPAddress, '1st record IPAddress');
    CheckEquals(Self.Loc.SRV[0].Port, Locations[0].Port,      '1st record Port');
    CheckEquals(TlsTransport,         Locations[1].Transport, '2nd record Transport');
    CheckEquals(Self.AAAARecord,      Locations[1].IPAddress, '2nd record IPAddress');
    CheckEquals(Self.Loc.SRV[0].Port, Locations[1].Port,      '2nd record Port');

    CheckEquals(TcpTransport,         Locations[2].Transport, '3rd record Transport');
    CheckEquals(Self.ARecord,         Locations[2].IPAddress, '3rd record IPAddress');
    CheckEquals(Self.Loc.SRV[1].Port, Locations[2].Port,      '3rd record Port');
    CheckEquals(TcpTransport,         Locations[3].Transport, '4th record Transport');
    CheckEquals(Self.AAAARecord,      Locations[3].IPAddress, '4th record IPAddress');
    CheckEquals(Self.Loc.SRV[1].Port, Locations[3].Port,      '4th record Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameNoNaptrManualTransportNoSrv;
var
  Locations: TIdSipLocations;
begin
  // The target's a domain name, we've no NAPTR records from the transport lookup,
  // we've no SRV RRs, but we've a manually-specified transport.
  Self.Target.Uri := 'sip:' + Self.Domain + ';transport=' + TransportParamTLS_SCTP;
  Self.AddNameRecords(Self.Target.Host);

  Locations := TIdSipLocations.Create;
  try
    Self.CheckAorAAAARecords(Locations,
                             ParamToTransport(Self.Target.Transport),
                             'No NAPTR, no SRV, transport param');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameNoNaptrManualTransportSrv;
var
  Locations: TIdSipLocations;
begin
  // The target's a domain name, we've no NAPTR records from the transport lookup,
  // we've SRV RRs, and the transport's specified.

  Self.Target.Uri := 'sip:' + Self.Domain + ';transport=tls';

  Self.Loc.AddSRV(Self.Domain, SrvTlsPrefix, 0, 0, 0, Self.Domain);
  Self.Loc.AddSRV(Self.Domain, SrvTcpPrefix, 1, 0, 0, Self.Domain);

  Self.AddNameRecords(Self.Target.Host);

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    CheckEquals(2,                    Locations.Count,        'Location count');
    CheckEquals(TlsTransport,         Locations[0].Transport, '1st record Transport');
    CheckEquals(Self.ARecord,         Locations[0].IPAddress, '1st record IPAddress');
    CheckEquals(Self.Loc.SRV[0].Port, Locations[0].Port,      '1st record Port');
    CheckEquals(TlsTransport,         Locations[1].Transport, '2nd record Transport');
    CheckEquals(Self.AAAARecord,      Locations[1].IPAddress, '2nd record IPAddress');
    CheckEquals(Self.Loc.SRV[0].Port, Locations[1].Port,      '2nd record Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameNoNaptrNoManualTransportNoSrv;
var
  Locations: TIdSipLocations;
begin
  // A/AAAA lookup
  Self.Target.Uri := 'sips:' + Self.Domain;

  Self.AddNameRecords(Self.Target.Host);

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Self.CheckAorAAAARecords(Locations,
                             TlsTransport,
                             'No NAPTR, no SRV, no transport param');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameNoNaptrNoManualTransportSrv;
begin
  // iterate over SRV
  Fail('not yet implemented');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameWithPort;
var
  Locations: TIdSipLocations;
begin
  Self.AddNameRecords(Self.Target.Host);

  Self.Target.Uri := 'sip:' + Self.Domain + ':' + IntToStr(Self.Port);

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Self.CheckAorAAAARecords(Locations, UdpTransport, 'SIP URI');
  finally
    Locations.Free;
  end;

  Self.Target.Scheme := SipsScheme;

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Self.CheckAorAAAARecords(Locations, TlsTransport, 'SIPS URI');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNumericAddress;
var
  Locations: TIdSipLocations;
begin
  Self.Target.Uri := 'sip:' + Self.IP;

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    CheckEquals(1,            Locations.Count,        'SIP Location count');
    CheckEquals(UdpTransport, Locations[0].Transport, 'SIP Transport');
    CheckEquals(Self.IP,      Locations[0].IPAddress, 'SIP IPAddress');
    CheckEquals(IdPORT_SIP,   Locations[0].Port,      'SIP Port');
  finally
    Locations.Free;
  end;

  Self.Target.Scheme := SipsScheme;

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    CheckEquals(1,            Locations.Count,        'SIPS Location count');
    CheckEquals(TlsTransport, Locations[0].Transport, 'SIPS Transport');
    CheckEquals(Self.IP,      Locations[0].IPAddress, 'SIPS IPAddress');
    CheckEquals(IdPORT_SIPS,  Locations[0].Port,      'SIPS Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNumericAddressWithPort;
var
  Locations: TIdSipLocations;
begin
  Self.Target.Uri := 'sip:' + Self.IP + ':' + IntToStr(Self.Port);

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    CheckEquals(1,            Locations.Count,        'SIP Location count');
    CheckEquals(UdpTransport, Locations[0].Transport, 'SIP Transport');
    CheckEquals(Self.IP,      Locations[0].IPAddress, 'SIP IPAddress');
    CheckEquals(Self.Port,    Locations[0].Port,      'SIP Port');
  finally
    Locations.Free;
  end;

  Self.Target.Scheme := SipsScheme;

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    CheckEquals(1,            Locations.Count,        'SIPS Location count');
    CheckEquals(TlsTransport, Locations[0].Transport, 'SIPS Transport');
    CheckEquals(Self.IP,      Locations[0].IPAddress, 'SIPS IPAddress');
    CheckEquals(Self.Port,    Locations[0].Port,      'SIPS Port');
  finally
    Locations.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipMockLocator                                                      *
//******************************************************************************
//* TestTIdSipMockLocator Public methods ***************************************

procedure TestTIdSipMockLocator.SetUp;
begin
  inherited SetUp;

  Self.AOR := TIdUri.Create('sip:bar');
  Self.Loc := TIdSipMockLocator.Create;
end;

procedure TestTIdSipMockLocator.TearDown;
begin
  Self.Loc.Free;

  inherited TearDown;
end;

//* TestTIdSipMockLocator Published methods ************************************

procedure TestTIdSipMockLocator.TestResolveNameRecords;
var
  Results: TIdDomainNameRecords;
begin
  // All mixed up records
  Self.Loc.AddA('foo',            '127.0.0.3');
  Self.Loc.AddA(Self.AOR.Host,    '127.0.0.1');
  Self.Loc.AddAAAA(Self.AOR.Host, '::1');
  Self.Loc.AddAAAA(Self.AOR.Host, '::2');
  Self.Loc.AddA(Self.AOR.Host,    '127.0.0.2');
  Self.Loc.AddAAAA('foo',         '::3');

  Results := TIdDomainNameRecords.Create;
  try
    Self.Loc.ResolveNameRecords(Self.AOR.Host, Results);

    CheckEquals(4,
                Results.Count,
                'Incorrect number of results: unwanted records added?');

    CheckEquals('127.0.0.1', Results[0].IPAddress, '1st record');
    CheckEquals('::1',       Results[1].IPAddress, '2nd record');
    CheckEquals('::2',       Results[2].IPAddress, '3rd record');
    CheckEquals('127.0.0.2', Results[3].IPAddress, '4th record');
  finally
    Results.Free;
  end;
end;

procedure TestTIdSipMockLocator.TestResolveNAPTRSip;
var
  Results: TIdNaptrRecords;
begin
  Self.Loc.AddNAPTR(AOR.Host, 10, 10, 's', 'http+foo', 'foo.bar');
  Self.Loc.AddNAPTR(AOR.Host, 20, 10, 's', 'SIP+D2T',  '_sip._tcp.bar');
  Self.Loc.AddNAPTR(AOR.Host, 10, 10, 's', 'SIPS+D2T', '_sips._tcp.bar');
  Self.Loc.AddNAPTR(AOR.Host, 30, 10, 's', 'SIP+D2U',  '_sip._udp.bar');
  Self.Loc.AddNAPTR('foo',    30, 10, 's', 'SIP+D2U',  '_sip._udp.foo');

  Results := TIdNaptrRecords.Create;
  try
    Self.Loc.ResolveNAPTR(Self.AOR, Results);
    CheckEquals(3,
                Results.Count,
                'Incorrect number of results: unwanted records added?');

    CheckEquals('_sips._tcp.bar', Results[0].Value, '1st record');
    CheckEquals('_sip._tcp.bar',  Results[1].Value, '2nd record');
    CheckEquals('_sip._udp.bar',  Results[2].Value, '3rd record');
  finally
    Results.Free;
  end;
end;

procedure TestTIdSipMockLocator.TestResolveNAPTRSips;
var
  Results: TIdNaptrRecords;
begin
  Self.Loc.AddNAPTR(AOR.Host, 10, 10, 's', 'http+foo', 'foo.bar');
  Self.Loc.AddNAPTR(AOR.Host, 20, 10, 's', 'SIP+D2T',  '_sip._tcp.bar');
  Self.Loc.AddNAPTR(AOR.Host, 10, 10, 's', 'SIPS+D2T', '_sips._tcp.bar');
  Self.Loc.AddNAPTR(AOR.Host, 30, 10, 's', 'SIP+D2U',  '_sip._udp.bar');
  Self.Loc.AddNAPTR('foo',    30, 10, 's', 'SIP+D2U',  '_sip._udp.foo');

  Results := TIdNaptrRecords.Create;
  try
    Self.AOR.Scheme := SipsScheme;
    Self.Loc.ResolveNAPTR(Self.AOR, Results);
    CheckEquals(1,
                Results.Count,
                'Incorrect number of results: unwanted records added?');
    CheckEquals('_sips._tcp.bar', Results[0].Value, '1st record');
  finally
    Results.Free;
  end;
end;

procedure TestTIdSipMockLocator.TestResolveSRV;
var
  Results: TIdSrvRecords;
begin
  Self.Loc.AddSRV('foo.bar', SrvTlsPrefix,  0, 0, IdPORT_SIPS, 'paranoid.bar');
  Self.Loc.AddSRV('foo.bar', SrvTcpPrefix, 10, 1, IdPORT_SIP , 'backup.bar');
  Self.Loc.AddSRV('foo.bar', SrvTcpPrefix, 10, 2, IdPORT_SIP , 'normal.bar');
  Self.Loc.AddSRV('foo.bar', SrvTcpPrefix, 20, 0, IdPORT_SIP , 'fallback.bar');
  Self.Loc.AddSRV('boo.far', SrvTlsPrefix,  0, 0, IdPORT_SIPS, 'paranoid.far');

  Results := TIdSrvRecords.Create;
  try
    Self.Loc.ResolveSRV('_sip._tcp.foo.bar', Results);

    CheckEquals(3,
                Results.Count,
                'Incorrect number of results: unwanted records added?');

    CheckEquals('normal.bar',   Results[0].Target, '1st record');
    CheckEquals('backup.bar',   Results[1].Target, '2nd record');
    CheckEquals('fallback.bar', Results[2].Target, '3rd record');
  finally
    Results.Free;
  end;
end;

procedure TestTIdSipMockLocator.TestResolveSRVWithNameRecords;
var
  Results: TIdSrvRecords;
begin
  Self.Loc.AddSRV('foo.bar', SrvTlsPrefix,  0, 0, IdPORT_SIPS, 'paranoid.bar');
  Self.Loc.AddAAAA('paranoid.bar', '::1');
  Self.Loc.AddA(   'arbitrary',    '127.0.0.2');
  Self.Loc.AddA(   'paranoid.bar', '127.0.0.1');

  Results := TIdSrvRecords.Create;
  try
    Self.Loc.ResolveSRV('_sips._tcp.foo.bar', Results);

    Check(not Results.IsEmpty, 'No results found');
    CheckEquals(2, Results[0].NameRecords.Count, 'Name record count');
    CheckEquals('::1',       Results[0].NameRecords[0].IPAddress, '1st name record');
    CheckEquals('127.0.0.1', Results[0].NameRecords[1].IPAddress, '2nd name record');
  finally
    Results.Free;
  end;
end;

//******************************************************************************
//* TestTIdDomainNameRecord                                                    *
//******************************************************************************
//* TestTIdDomainNameRecord Public methods *************************************

procedure TestTIdDomainNameRecord.SetUp;
begin
  inherited SetUp;

  Self.Domain     := 'foo.bar';
  Self.IPAddress  := '127.0.0.1';
  Self.RecordType := 'A';

  Self.Rec := TIdDomainNameRecord.Create(Self.RecordType,
                                         Self.Domain,
                                         Self.IPAddress);
end;

procedure TestTIdDomainNameRecord.TearDown;
begin
  Self.Rec.Free;

  inherited TearDown;
end;

//* TestTIdDomainNameRecord Published methods **********************************

procedure TestTIdDomainNameRecord.TestCopy;
var
  Copy: TIdDomainNameRecord;
begin
  Copy := Self.Rec.Copy;
  try
    CheckEquals(Self.Rec.Domain,     Copy.Domain,     'Domain');
    CheckEquals(Self.Rec.IPAddress,  Copy.IPAddress,  'IPAddress');
    CheckEquals(Self.Rec.RecordType, Copy.RecordType, 'RecordType');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdDomainNameRecord.TestInstantiation;
begin
  CheckEquals(Self.Domain,     Self.Rec.Domain,     'Domain');
  CheckEquals(Self.IPAddress,  Self.Rec.IPAddress,  'IPAddress');
  CheckEquals(Self.RecordType, Self.Rec.RecordType, 'RecordType');
end;

//******************************************************************************
//* TestTIdDomainNameRecords                                                   *
//******************************************************************************
//* TestTIdDomainNameRecords Public methods ************************************

procedure TestTIdDomainNameRecords.SetUp;
begin
  inherited SetUp;

  Self.List := TIdDomainNameRecords.Create;
end;

procedure TestTIdDomainNameRecords.TearDown;
begin
  Self.List.Free;

  inherited TearDown;
end;

//* TestTIdDomainNameRecords Published methods *********************************

procedure TestTIdDomainNameRecords.TestAdd;
var
  NewRec: TIdDomainNameRecord;
begin
  CheckEquals(0, Self.List.Count, 'Empty list');

  NewRec := TIdDomainNameRecord.Create('', '', '');
  try
    Self.List.Add(NewRec);
    CheckEquals(1, Self.List.Count, 'Non-empty list');
  finally
    NewRec.Free;
  end;
end;

procedure TestTIdDomainNameRecords.TestAddWithParameters;
const
  RecordType = DnsAAAARecord;
  Domain     = 'sipproxy.leo-ix.net';
  IPAddress  = '::1';
begin
  Self.List.Add(RecordType, Domain, IPAddress);

  CheckEquals(RecordType, Self.List[0].RecordType, 'RecordType');
  CheckEquals(Domain,     Self.List[0].Domain,     'Domain');
  CheckEquals(IPAddress,  Self.List[0].IPAddress,  'IPAddress');
end;

procedure TestTIdDomainNameRecords.TestClear;
var
  NewRec: TIdDomainNameRecord;
begin
  NewRec := TIdDomainNameRecord.Create('', '', '');
  try
    Self.List.Add(NewRec);
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  Self.List.Clear;

  CheckEquals(0, Self.List.Count, 'Cleared list');
end;

procedure TestTIdDomainNameRecords.TestCopy;
var
  I:      Integer;
  NewRec: TIdDomainNameRecord;
  NewSet: TIdDomainNameRecords;
begin
  NewRec := TIdDomainNameRecord.Create(DnsARecord, 'foo.bar', '127.0.0.1');
  try
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  NewRec := TIdDomainNameRecord.Create(DnsAAAARecord, 'foo.bar', '::1');
  try
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  NewSet := Self.List.Copy;
  try
    for I := 0 to Min(NewSet.Count, Self.List.Count) - 1 do begin
      CheckEquals(Self.List[I].RecordType,
                  NewSet[I].RecordType,
                  'RecordType at index ' + IntToStr(I));
      CheckEquals(Self.List[I].Domain,
                  NewSet[I].Domain,
                  'Domain at index ' + IntToStr(I));
      CheckEquals(Self.List[I].IPAddress,
                  NewSet[I].IPAddress,
                  'IPAddress at index ' + IntToStr(I));

    end;

    CheckEquals(Self.List.Count,
                NewSet.Count,
                'Record count');
  finally
    NewSet.Free;
  end;
end;

procedure TestTIdDomainNameRecords.TestIsEmpty;
var
  NewRec: TIdDomainNameRecord;
begin
  Check(Self.List.IsEmpty, 'Empty list');

  NewRec := TIdDomainNameRecord.Create('', '', '');
  try
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  Check(not Self.List.IsEmpty, 'After Add');

  Self.List.Clear;

  Check(Self.List.IsEmpty, 'After clear');
end;

//******************************************************************************
//* TestTIdNaptrRecord                                                         *
//******************************************************************************
//* TestTIdNaptrRecord Public methods ******************************************

procedure TestTIdNaptrRecord.SetUp;
begin
  inherited SetUp;

  Self.Flags       := 's';
  Self.Key         := 'sip:bar@foo';
  Self.Order       := 0;
  Self.Preference  := 100;
  Self.Regex       := 's//';
  Self.Service     := 'SIP+D2T';
  Self.Value       := '_sip._tcp.foo';

  Self.Rec := TIdNaptrRecord.Create(Self.Key,
                                    Self.Order,
                                    Self.Preference,
                                    Self.Flags,
                                    Self.Service,
                                    Self.Regex,
                                    Self.Value);
end;

procedure TestTIdNaptrRecord.TearDown;
begin
  Self.Rec.Free;

  inherited TearDown;
end;

//* TestTIdNaptrRecord Published methods ***************************************

procedure TestTIdNaptrRecord.TestAsSipTransport;
type
  TNaptrSipTransportMap = record
    NaptrService: String;
    SipTransport: String;
  end;
const
  Tests: array[1..5] of TNaptrSipTransportMap =
         ((NaptrService: NaptrSctpService; SipTransport: SctpTransport),
          (NaptrService: NaptrSctpService; SipTransport: SctpTransport),
          (NaptrService: NaptrTlsService; SipTransport: TlsTransport),
          (NaptrService: NaptrTlsOverSctpService; SipTransport: TlsOverSctpTransport),
          (NaptrService: NaptrUdpService;  SipTransport: UdpTransport));
var
  I: Integer;
  N: TIdNaptrRecord;
begin
  for I := Low(Tests) to High(Tests) do begin
    N := TIdNaptrRecord.Create('', 0, 0, '', Tests[I].NaptrService, '', '');
    try
      CheckEquals(Tests[I].SipTransport, N.AsSipTransport, Tests[I].NaptrService);
    finally
      N.Free;
    end;
  end;
end;

procedure TestTIdNaptrRecord.TestCopy;
var
  Copy: TIdNaptrRecord;
begin
  Copy := Self.Rec.Copy;
  try
    CheckEquals(Self.Rec.Flags,      Copy.Flags,      'Flags');
    CheckEquals(Self.Rec.Key,        Copy.Key,        'Key');
    CheckEquals(Self.Rec.Order,      Copy.Order,      'Order');
    CheckEquals(Self.Rec.Preference, Copy.Preference, 'Preference');
    CheckEquals(Self.Rec.Regex,      Copy.Regex,      'Regex');
    CheckEquals(Self.Rec.Service,    Copy.Service,    'Service');
    CheckEquals(Self.Rec.Value,      Copy.Value,      'Value');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdNaptrRecord.TestInstantiation;
begin
  CheckEquals(Self.Flags,      Self.Rec.Flags,      'Flags');
  CheckEquals(Self.Key,        Self.Rec.Key,        'Key');
  CheckEquals(Self.Order,      Self.Rec.Order,      'Order');
  CheckEquals(Self.Preference, Self.Rec.Preference, 'Preference');
  CheckEquals(Self.Regex,      Self.Rec.Regex,      'Regex');
  CheckEquals(Self.Service,    Self.Rec.Service,    'Service');
  CheckEquals(Self.Value,      Self.Rec.Value,      'Value');
end;

procedure TestTIdNaptrRecord.TestIsSecureService;
type
  TFacts = record
    Service:  String;
    IsSecure: Boolean;
  end;
const
  Tests: array[1..5] of TFacts =
         ((Service: NaptrTlsService; IsSecure: true),
          (Service: NaptrTlsOverSctpService; IsSecure: true),
          (Service: NaptrTcpService; IsSecure: false),
          (Service: NaptrUdpService; IsSecure: false),
          (Service: NaptrSctpService; IsSecure: false));
var
  I: Integer;
  N: TIdNaptrRecord;
begin
  for I := Low(Tests) to High(Tests) do begin
    N := TIdNaptrRecord.Create('', 0, 0, '', Tests[I].Service, '', '');
    try
      Check(Tests[I].IsSecure = N.IsSecureService, Tests[I].Service);
    finally
      N.Free;
    end;
  end;
end;

//******************************************************************************
//* TestTIdNaptrRecords                                                        *
//******************************************************************************
//* TestTIdNaptrRecords Public methods *****************************************

procedure TestTIdNaptrRecords.SetUp;
begin
  inherited SetUp;

  Self.List := TIdNaptrRecords.Create;
end;

procedure TestTIdNaptrRecords.TearDown;
begin
  Self.List.Free;

  inherited TearDown;
end;

//* TestTIdNaptrRecords Published methods **************************************

procedure TestTIdNaptrRecords.TestAdd;
var
  NewRec: TIdNaptrRecord;
begin
  CheckEquals(0, Self.List.Count, 'Empty list');

  NewRec := TIdNaptrRecord.Create('', 0, 0, '', '', '', '');
  try
    Self.List.Add(NewRec);
    CheckEquals(1, Self.List.Count, 'Non-empty list');
  finally
    NewRec.Free;
  end;
end;

procedure TestTIdNaptrRecords.TestAddWithParameters;
const
  Key        = 'leo-ix.net';
  Order      = 0;
  Preference = 1;
  Flags      = NaptrDefaultFlags;
  Service    = NaptrTlsService;
  Regex      = '';
  Value      = '_sips._tcp.leo-ix.net';
begin
  Self.List.Add(Key,
                Order,
                Preference,
                Flags,
                Service,
                Regex,
                Value);

  CheckEquals(Key,        Self.List[0].Key,        'Key');
  CheckEquals(Order,      Self.List[0].Order,      'Order');
  CheckEquals(Preference, Self.List[0].Preference, 'Preference');
  CheckEquals(Flags,      Self.List[0].Flags,      'Flags');
  CheckEquals(Service,    Self.List[0].Service,    'Service');
  CheckEquals(Regex,      Self.List[0].Regex,      'Regex');
  CheckEquals(Value,      Self.List[0].Value,      'Value');
end;

procedure TestTIdNaptrRecords.TestAnyAppropriateRecord;
var
  SupportedTrans: TStrings;
begin
  Self.List.Add('foo', 0, 0, NaptrDefaultFlags, NaptrTlsService,  '', SrvTlsPrefix + '.foo');
  Self.List.Add('foo', 1, 0, NaptrDefaultFlags, NaptrTcpService,  '', SrvTcpPrefix + '.foo');
  Self.List.Add('foo', 2, 0, NaptrDefaultFlags, NaptrUdpService,  '', SrvUdpPrefix + '.foo');
  Self.List.Add('foo', 3, 0, NaptrDefaultFlags, NaptrSctpService, '', SrvSctpPrefix + '.foo');

  SupportedTrans := TStringList.Create;
  try
    Check(not Assigned(Self.List.AnyAppropriateRecord(SupportedTrans)),
          'Return nil for no appropriate transports');

    SupportedTrans.Add(SctpTransport);

    Check(Assigned(Self.List.AnyAppropriateRecord(SupportedTrans)),
          'No record found for ' + SupportedTrans[0]);

    CheckEquals(NaptrSctpService,
                Self.List.AnyAppropriateRecord(SupportedTrans).Service,
                'Wrong record found');

    SupportedTrans.Add(TcpTransport);
    Check(Assigned(Self.List.AnyAppropriateRecord(SupportedTrans)),
          'No record found for ' + SupportedTrans[1]);
    CheckEquals(NaptrTcpService,
                Self.List.AnyAppropriateRecord(SupportedTrans).Service,
                'Preferences ignored');
  finally
    SupportedTrans.Free;
  end;
end;

procedure TestTIdNaptrRecords.TestClear;
var
  NewRec: TIdNaptrRecord;
begin
  NewRec := TIdNaptrRecord.Create('', 0, 0, '', '', '', '');
  try
    Self.List.Add(NewRec);
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  Self.List.Clear;

  CheckEquals(0, Self.List.Count, 'Cleared list');
end;

procedure TestTIdNaptrRecords.TestDelete;
var
  NewRec: TIdNaptrRecord;
begin
  NewRec := TIdNaptrRecord.Create('', 1, 0, '', '', '', '');
  try
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  NewRec := TIdNaptrRecord.Create('', 2, 0, '', '', '', '');
  try
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  NewRec := TIdNaptrRecord.Create('', 3, 0, '', '', '', '');
  try
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  Self.List.Delete(1);
  CheckEquals(2, Self.List.Count, '1st Delete');
  CheckEquals(1, Self.List[0].Order, '1st record deleted instead; 1st Delete');
  CheckEquals(3, Self.List[1].Order, '3rd record deleted instead; 1st Delete');

  Self.List.Delete(0);
  CheckEquals(1, Self.List.Count, '2nd Delete');
  CheckEquals(3, Self.List[0].Order, '2nd record deleted instead; 2nd Delete');

  Self.List.Delete(0);
  CheckEquals(0, Self.List.Count, 'Delete of last record');
end;

procedure TestTIdNaptrRecords.TestIsEmpty;
var
  NewRec: TIdNaptrRecord;
begin
  Check(Self.List.IsEmpty, 'Empty list');

  NewRec := TIdNaptrRecord.Create('', 0, 0, '', '', '', '');
  try
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  Check(not Self.List.IsEmpty, 'After Add');

  Self.List.Clear;

  Check(Self.List.IsEmpty, 'After clear');
end;

procedure TestTIdNaptrRecords.TestSort;
begin
  Self.List.Add('foo', 30, 10, 's', 'SIP+D2U',  '', '_sip._udp.foo');
  Self.List.Add('bar', 10, 10, 's', 'http+foo', '', 'foo.bar');
  Self.List.Add('bar', 20, 10, 's', 'SIP+D2T',  '', '_sip._tcp.bar');
  Self.List.Add('bar', 10, 10, 's', 'SIPS+D2T', '', '_sips._tcp.bar');
  Self.List.Add('bar', 30, 10, 's', 'SIP+D2U',  '', '_sip._udp.bar');

  Self.List.Sort;
  CheckEquals('_sips._tcp.bar', Self.List[0].Value, '1st record');
  CheckEquals('foo.bar',        Self.List[1].Value, '2nd record');
  CheckEquals('_sip._tcp.bar',  Self.List[2].Value, '3rd record');
  CheckEquals('_sip._udp.bar',  Self.List[3].Value, '4th record');
  CheckEquals('_sip._udp.foo',  Self.List[4].Value, '5th record');
end;

//******************************************************************************
//* TestTIdSrvRecord                                                           *
//******************************************************************************
//* TestTIdSrvRecord Public methods ********************************************

procedure TestTIdSrvRecord.SetUp;
begin
  inherited SetUp;

  Self.Domain      := 'foo.bar';
  Self.NameRecords := TIdDomainNameRecords.Create;
  Self.Port        := IdPORT_SIPS;
  Self.Priority    := 50;
  Self.Service     := '_sips._sctp';
  Self.Target      := 'sipsmachine.foo.bar';
  Self.Weight      := 0;

  Self.Rec := TIdSrvRecord.Create(Self.Domain,
                                  Self.Service,
                                  Self.Priority,
                                  Self.Weight,
                                  Self.Port,
                                  Self.Target);
end;

procedure TestTIdSrvRecord.TearDown;
begin
  Self.Rec.Free;

  inherited TearDown;
end;

//* TestTIdSrvRecord Published methods *****************************************

procedure TestTIdSrvRecord.TestCopy;
var
  ARecord: TIdDomainNameRecord;
  Copy:    TIdSrvRecord;
begin
  ARecord := TIdDomainNameRecord.Create(DnsARecord, 'foo.bar', '127.0.0.1');
  try
    Self.Rec.NameRecords.Add(ARecord);

    Copy := Self.Rec.Copy;
    try
      CheckEquals(Self.Rec.Domain,   Copy.Domain,   'Domain');
      CheckEquals(Self.Rec.Port,     Copy.Port,     'Port');
      CheckEquals(Self.Rec.Priority, Copy.Priority, 'Priority');
      CheckEquals(Self.Rec.Service,  Copy.Service,  'Service');
      CheckEquals(Self.Rec.Target,   Copy.Target,   'Target');
      CheckEquals(Self.Rec.Weight,   Copy.Weight,   'Weight');

      CheckEquals(Self.Rec.NameRecords.Count,
                  Copy.NameRecords.Count,
                  'Name record count');
      CheckEquals(Self.Rec.NameRecords[0].Domain,
                  Copy.NameRecords[0].Domain,
                  'Domain of name record');
    finally
      Copy.Free;
    end;
  finally
    ARecord.Free;
  end;
end;

procedure TestTIdSrvRecord.TestInstantiation;
begin
  CheckEquals(Self.Domain,   Self.Rec.Domain,   'Domain');
  CheckEquals(Self.Port,     Self.Rec.Port,     'Port');
  CheckEquals(Self.Priority, Self.Rec.Priority, 'Priority');
  CheckEquals(Self.Service,  Self.Rec.Service,  'Service');
  CheckEquals(Self.Target,   Self.Rec.Target,   'Target');
  CheckEquals(Self.Weight,   Self.Rec.Weight,   'Weight');
end;

procedure TestTIdSrvRecord.TestQueryName;
begin
  CheckEquals(Self.Rec.Service + '.' + Self.Rec.Domain,
              Self.Rec.QueryName,
              'QueryName');
end;

procedure TestTIdSrvRecord.TestSipTransport;
type
  TServiceTransportPair = record
    Service: String;
    SipTransport: String;
  end;
const
  Pairs: array[1..4] of TServiceTransportPair =
    ((Service: SrvSctpPrefix; SipTransport: SctpTransport),
     (Service: SrvTcpPrefix;  SipTransport: TcpTransport),
     (Service: SrvUdpPrefix;  SipTransport: UdpTransport),
     (Service: SrvTlsPrefix;  SipTransport: TlsTransport));
var
  I:   Integer;
  Srv: TIdSrvRecord;
begin
//  CheckEquals(SctpTransport, Self.Rec.SipTransport, Self.Rec.Service);

  for I := Low(Pairs) to High(Pairs) do begin
    Srv := TIdSrvRecord.Create('', Pairs[I].Service, 0, 0, 0, '');
    try
      CheckEquals(Pairs[I].SipTransport, Srv.SipTransport, Pairs[I].Service);
    finally
      Srv.Free;
    end;
  end;
end;

//******************************************************************************
//* TestTIdSrvRecords                                                          *
//******************************************************************************
//* TestTIdSrvRecords Public methods *******************************************

procedure TestTIdSrvRecords.SetUp;
begin
  inherited SetUp;

  Self.List := TIdSrvRecords.Create;
end;

procedure TestTIdSrvRecords.TearDown;
begin
  Self.List.Free;

  inherited TearDown;
end;

//* TestTIdSrvRecords Published methods ****************************************

procedure TestTIdSrvRecords.TestAdd;
var
  NewRec: TIdSrvRecord;
begin
  CheckEquals(0, Self.List.Count, 'Empty list');

  NewRec := TIdSrvRecord.Create('', '', 0, 0, 0, '');
  try
    Self.List.Add(NewRec);
    CheckEquals(1, Self.List.Count, 'Non-empty list');
  finally
    NewRec.Free;
  end;
end;

procedure TestTIdSrvRecords.TestAddWithParameters;
const
  Domain   = 'leo-ix.net';
  Service  = '_sips.sctp';
  Priority = 0;
  Weight   = 1;
  Port     = IdPORT_SIPS;
  Target   = 'gw1.leo-ix.net';

begin
  Self.List.Add(Domain, Service, Priority, Weight, Port, Target);

  CheckEquals(Domain,   Self.List[0].Domain,   'Domain');
  CheckEquals(Service,  Self.List[0].Service,  'Service');
  CheckEquals(Priority, Self.List[0].Priority, 'Priority');
  CheckEquals(Weight,   Self.List[0].Weight,   'Weight');
  CheckEquals(Port,     Self.List[0].Port,     'Port');
  CheckEquals(Target,   Self.List[0].Target,   'Target');
end;

procedure TestTIdSrvRecords.TestAddNameRecord;
const
  Targets: array[1..3] of String = ('target1', 'target2', 'target3');
var
  I: Integer;
begin
  // SRV -> 'targetN'; 'targetN' via AAAA -> ::N
  for I := Low(Targets) to High(Targets) do begin
    Self.List.Add('', '', 0, 0, 0, Targets[I]);

    Self.List.AddNameRecord(DnsAAAARecord, Targets[I], '::' + IntToStr(I));
  end;

  for I := 0 to Self.List.Count - 1 do begin
    CheckEquals(1,
                Self.List[I].NameRecords.Count,
                IntToStr(I) + 'th record has no name record');
    CheckEquals(Targets[I + 1],
                Self.List[I].NameRecords[0].Domain,
                IntToStr(I) + 'th record has wrong domain');
    CheckEquals(DnsAAAARecord,
                Self.List[I].NameRecords[0].RecordType,
                IntToStr(I) + 'th record has wrong record type');
    CheckEquals('::' + IntToStr(I + 1),
                Self.List[I].NameRecords[0].IPAddress,
                IntToStr(I) + 'th record has wrong IP address');
  end;
end;

procedure TestTIdSrvRecords.TestClear;
var
  NewRec: TIdSrvRecord;
begin
  NewRec := TIdSrvRecord.Create('', '', 0, 0, 0, '');
  try
    Self.List.Add(NewRec);
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  Self.List.Clear;

  CheckEquals(0, Self.List.Count, 'Cleared list');
end;

procedure TestTIdSrvRecords.TestLast;
var
  I:      Integer;
  NewRec: TIdSrvRecord;
begin
  for I := 1 to 5 do begin
    NewRec := TIdSrvRecord.Create('', '', I, 0, 0, '');
    try
      Self.List.Add(NewRec);

      CheckEquals(I, Self.List.Last.Priority, IntToStr(I) + 'th Add');
    finally
      NewRec.Free;
    end;
  end;
end;

procedure TestTIdSrvRecords.TestIsEmpty;
var
  NewRec: TIdSrvRecord;
begin
  Check(Self.List.IsEmpty, 'Empty list');

  NewRec := TIdSrvRecord.Create('', '', 0, 0, 0, '');
  try
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  Check(not Self.List.IsEmpty, 'After Add');

  Self.List.Clear;

  Check(Self.List.IsEmpty, 'After clear');
end;

initialization
  RegisterTest('SIP Location Services', Suite);
end.
