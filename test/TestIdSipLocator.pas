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
    procedure TestCount;
    procedure TestIsEmpty;
  end;

  TestTIdSipAbstractLocator = class(TTestCase)
  private
    IP:             String;
    Loc:            TIdSipMockLocator;
    NameRecs:       TIdDomainNameRecords;
    Naptr:          TIdNaptrRecords;
    Port:           Cardinal;
    Srv:            TIdSrvRecords;
    Target:         TIdSipUri;
    TransportParam: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFindServersForResponseWithNameAndPort;
    procedure TestFindServersForResponseWithNumericSentBy;
    procedure TestFindServersForResponseWithNumericSentByAndPort;
    procedure TestFindServersForResponseWithReceivedParam;
    procedure TestFindServersForResponseWithReceivedParamAndRport;
    procedure TestFindServersForResponseWithReceivedParamAndNumericSentBy;
    procedure TestFindServersForResponseWithReceivedParamAndIPv6NumericSentBy;
    procedure TestFindServersForResponseWithRport;
    procedure TestNameAndPort;
    procedure TestNameAndPortSips;
    procedure TestNameAndPortWithMultipleNameRecords;
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
  end;

  TestTIdSipLocator = class(TTestCase)
  private
    Loc: TIdSipLocator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
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
    procedure TestCopy;
    procedure TestInstantiation;
  end;

  TestTIdNaptrRecords = class(TTestCase)
  private
    List: TIdNaptrRecords;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
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
  Result.AddTest(TestTIdSipLocator.Suite);
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
    CheckEquals(Self.Loc.Transport, Copy.Transport, 'Transport');
    CheckEquals(Self.Loc.Address,   Copy.Address,   'Address');
    CheckEquals(Self.Loc.Port,      Copy.Port,      'Port');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdSipLocation.TestCreate;
begin
  CheckEquals(Self.Address,   Self.Loc.Address,   'Address');
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
      CheckEquals(Via.SentBy,    Loc.Address,   'Address');
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

  CheckEquals(Transport, Self.Locs.First.Transport, 'Transport');
  CheckEquals(Address,   Self.Locs.First.Address,   'Address');
  CheckEquals(Port,      Self.Locs.First.Port,      'Port');
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

  Self.IP       := '127.0.0.1';
  Self.Loc      := TIdSipMockLocator.Create;
  Self.NameRecs := TIdDomainNameRecords.Create;
  Self.Naptr    := TIdNaptrRecords.Create;
  Self.Port     := IdPORT_SIP;
  Self.Srv      := TIdSrvRecords.Create;
  Self.Target   := TIdSipUri.Create;
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

//* TestTIdSipAbstractLocator Published methods ********************************

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithNameAndPort;
const
  ARecord    = '127.0.0.1';
  AAAARecord = '::1';
  Domain     = 'foo.com';
var
  Locations: TIdSipLocations;
  Response:  TIdSipResponse;
begin
  Self.Loc.AddAAAA(Domain, AAAARecord);
  Self.Loc.AddA(   Domain, ARecord);

  Response := TIdSipResponse.Create;
  try
    Response.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/UDP ' + Domain + ':6666';

    Locations := Self.Loc.FindServersFor(Response);
    try
      CheckEquals(Response.LastHop.Transport,
                  Locations[0].Transport,
                  'First location transport');
      CheckEquals(Self.Loc.NameRecords[0].IPAddress,
                  Locations[0].Address,
                  'First location address');
      CheckEquals(Response.LastHop.Port,
                  Locations[0].Port,
                  'First location port');

      CheckEquals(Response.LastHop.Transport,
                  Locations[1].Transport,
                  'Second location transport');
      CheckEquals(Self.Loc.NameRecords[1].IPAddress,
                  Locations[1].Address,
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
                  Locations[0].Address,
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
                  Locations[0].Address,
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
                  Locations[0].Address,
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
                  Locations[0].Address,
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
                  Locations[1].Address,
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
                  Locations[1].Address,
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
                  Locations[0].Address,
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

procedure TestTIdSipAbstractLocator.TestNameAndPort;
var
  Locations: TIdSipLocations;
begin
  Self.Target.Uri := 'sip:foo.com:' + IntToStr(Self.Port);
  Self.Loc.AddA(Self.Target.Host, Self.IP);

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(UdpTransport, Locations.First.Transport, 'Transport');
    CheckEquals(Self.IP,      Locations.First.Address,   'Address');
    CheckEquals(Self.Port,    Locations.First.Port,      'Port');

  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNameAndPortSips;
var
  Locations: TIdSipLocations;
begin
  Self.Port := 5060;
  Self.Target.Uri := 'sips:foo.com:' + IntToStr(Self.Port);
  Self.Loc.AddA(Self.Target.Host, Self.IP);

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(TlsTransport, Locations.First.Transport, 'Transport');
    CheckEquals(Self.IP,      Locations.First.Address,   'Address');
    CheckEquals(Self.Port,    Locations.First.Port,      'Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNameAndPortWithMultipleNameRecords;
var
  Locations: TIdSipLocations;
begin
  Self.IP := 'foo.com';
  Self.Target.Uri := 'sip:' + Self.IP + ':' + IntToStr(Self.Port);
  Self.Loc.AddAAAA(Self.IP, '::1');
  Self.Loc.AddA(Self.IP, '127.0.0.1');

  Locations := Self.Loc.FindServersFor(Self.Target.Uri);
  try
    CheckEquals(Self.Loc.NameRecords.Count,
                Locations.Count,
                'Location count');
    CheckEquals('::1',       Self.Loc.NameRecords[0].IPAddress, '1st record');
    CheckEquals('127.0.0.1', Self.Loc.NameRecords[1].IPAddress, '2nd record');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNumericAddressNonStandardPort;
var
  Locations: TIdSipLocations;
begin
  Self.Port       := 3000;
  Self.Target.Uri := 'sip:' + IP + ':' + IntToStr(Self.Port);

  Locations := Self.Loc.FindServersFor(Self.Target.Uri);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(UdpTransport, Locations.First.Transport, 'Transport');
    CheckEquals(Self.IP,      Locations.First.Address,   'Address');
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

  Locations := Self.Loc.FindServersFor(Self.Target.Uri);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(UdpTransport, Locations.First.Transport, 'Transport');
    CheckEquals(Self.IP,      Locations.First.Address,   'Address');
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

  Locations := Self.Loc.FindServersFor(Self.Target.Uri);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(TlsTransport, Locations.First.Transport, 'Transport');
    CheckEquals(Self.IP,      Locations.First.Address,   'Address');
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

  Locations := Self.Loc.FindServersFor(Self.Target.Uri);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(TlsTransport, Locations.First.Transport, 'Transport');
    CheckEquals(Self.IP,      Locations.First.Address,   'Address');
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

  Locations := Self.Loc.FindServersFor(Self.Target.Uri);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(UdpTransport, Locations.First.Transport, 'Transport');
    CheckEquals(Self.IP,      Locations.First.Address,   'Address');
    CheckEquals(Self.Port,    Locations.First.Port,      'Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNumericMaddrIPv6;
var
  Locations: TIdSipLocations;
begin
  Self.IP := '::1'; // localhost

  Self.Target.Uri := 'sip:foo.com;maddr=' + Self.IP;

  Locations := Self.Loc.FindServersFor(Self.Target.Uri);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(UdpTransport, Locations.First.Transport, 'Transport');
    CheckEquals(Self.IP,      Locations.First.Address,   'Address');
    CheckEquals(Self.Port,    Locations.First.Port,      'Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNumericMaddrSips;
var
  Locations: TIdSipLocations;
begin
  Self.Target.Uri := 'sips:foo.com;maddr=127.0.0.1';

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(TlsTransport, Locations.First.Transport, 'Transport');
    CheckEquals('127.0.0.1',  Locations.First.Address,   'Address');
    CheckEquals(IdPORT_SIPS,  Locations.First.Port,      'Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNumericMaddrSipsIPv6;
var
  Locations: TIdSipLocations;
begin
  Self.Target.Uri := 'sips:foo.com;maddr=::1';

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(TlsTransport, Locations.First.Transport, 'Transport');
    CheckEquals('::1',        Locations.First.Address,   'Address');
    CheckEquals(IdPORT_SIPS,  Locations.First.Port,      'Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestSrvNoNameRecords;
var
  Locations: TIdSipLocations;
begin
  Self.IP := 'foo.com';
  Self.Target.Uri :='sip:' + Self.IP;

  Self.Loc.AddSRV(Self.IP, SrvTcpPrefix,  0, 0, 0, Self.IP);
  Self.Loc.AddSRV(Self.IP, SrvSctpPrefix, 0, 0, 0, Self.IP);

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

  Self.IP := 'foo.com';
  Self.Target.Uri :='sip:' + Self.IP;

  Self.Loc.AddSRV(Self.IP, SrvTcpPrefix,  0, 0, 0, SrvNotAvailableTarget);
  Self.Loc.AddSRV(Self.IP, SrvSctpPrefix, 0, 0, 0, Self.IP);

  Self.Loc.AddA(Self.IP, '127.0.0.1');

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    CheckEquals(1,
                Locations.Count,
                'The locator didn''t filter out the "unavailable" SRV');
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

  Locations := Self.Loc.FindServersFor(Self.Target.Uri);
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
  Self.IP := 'foo.com';
  Self.Target.Uri := 'sip:' + Self.IP + ':' + IntToStr(Self.Port);

  CheckEquals(UdpTransport,
              Self.Loc.TransportFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Name, explicit port');

  Self.Target.Uri := 'sips:' + Self.IP + ':' + IntToStr(Self.Port);

  CheckEquals(TlsTransport,
              Self.Loc.TransportFor(Self.Target, Self.Naptr, Self.Srv, Self.NameRecs),
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
  Self.Loc.AddNAPTR(Self.IP,  50, 50, 's', SipsTlsService, '_sips._tcp.example.com');
  Self.Loc.AddNAPTR(Self.IP,  90, 50, 's', SipTcpService,   '_sip._tcp.example.com');
  Self.Loc.AddNAPTR(Self.IP, 100, 50, 's', SipUdpService,   '_sip._udp.example.com');

  CheckEquals(NaptrServiceToTransport(Self.Loc.NAPTR[0].Service),
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
    CheckEquals('127.0.0.1', Locations[0].Address, '1st record address');
    CheckEquals('127.0.0.2', Locations[1].Address, '2nd record address');
  finally
    Locations.Free;
  end;
end;


//******************************************************************************
//* TestTIdSipLocator                                                          *
//******************************************************************************
//* TestTIdSipLocator Public methods *******************************************

procedure TestTIdSipLocator.SetUp;
begin
  inherited SetUp;

  Self.Loc := TIdSipLocator.Create;
end;

procedure TestTIdSipLocator.TearDown;
begin
  Self.Loc.Free;

  inherited TearDown;
end;

//* TestTIdSipLocator Published methods ****************************************

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
