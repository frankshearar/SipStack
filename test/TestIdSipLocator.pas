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
  IdSipLocator, IdSipMessage, IdSipMockLocator, TestFramework;

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
    Port:           Cardinal;
    Target:         TIdSipUri;
    TransportParam: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFindServersForResponseWithReceivedParam;
    procedure TestFindServersForResponseWithReceivedParamAndNumericSentBy;
    procedure TestFindServersForResponseWithReceivedParamAndIPv6NumericSentBy;
    procedure TestFindServersForResponseWithNumericSentBy;
    procedure TestFindServersForUriWithNumericMaddr;
    procedure TestFindServersForUriWithNumericMaddrIPv6;
    procedure TestNameAndPort;
    procedure TestNameAndPortSips;
    procedure TestNumericAddressNonStandardPort;
    procedure TestNumericAddressUsesUdp;
    procedure TestNumericAddressSipsUriUsesTls;
    procedure TestNumericAddressSipsUriNonStandardPort;
    procedure TestNumericMaddrSips;
    procedure TestNumericMaddrSipsIPv6;
    procedure TestTransportParamTakesPrecedence;
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
    Loc: TIdSipMockLocator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestResolveNameRecords;
    procedure TestResolveNAPTR;
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
  end;

implementation

uses
  Classes, IdSipConsts, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipLocator unit tests');
  Result.AddTest(TestTIdSipLocation.Suite);
  Result.AddTest(TestTIdSipLocations.Suite);
  Result.AddTest(TestTIdSipAbstractLocator.Suite);
  Result.AddTest(TestTIdSipLocator.Suite);
  Result.AddTest(TestTIdSipMockLocator.Suite);
  Result.AddTest(TestTIdDomainNameRecord.Suite);
  Result.AddTest(TestTIdNaptrRecord.Suite);
  Result.AddTest(TestTIdNaptrRecords.Suite);
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

  Self.IP     := '1.2.3.4';
  Self.Loc    := TIdSipMockLocator.Create;
  Self.Port   := IdPORT_SIP;
  Self.Target := TIdSipUri.Create;
end;

procedure TestTIdSipAbstractLocator.TearDown;
begin
  Self.Target.Free;
  Self.Loc.Free;

  inherited Destroy;
end;

//* TestTIdSipAbstractLocator Published methods ********************************

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

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithNumericSentBy;
var
  Locations: TIdSipLocations;
  Response:  TIdSipResponse;
begin
  Response := TIdSipResponse.Create;
  try
    Response.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/UDP ' + Self.IP;

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

procedure TestTIdSipAbstractLocator.TestFindServersForUriWithNumericMaddr;
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

procedure TestTIdSipAbstractLocator.TestFindServersForUriWithNumericMaddrIPv6;
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

procedure TestTIdSipAbstractLocator.TestNameAndPort;
var
  Locations: TIdSipLocations;
begin
  Self.Target.Uri := 'sip:foo.com:5060';

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(UdpTransport,
                Locations.First.Transport,
                'Name:Port');
  finally
    Locations.Free;
  end;
end;

procedure TestTIdSipAbstractLocator.TestNameAndPortSips;
var
  Locations: TIdSipLocations;
begin
  Self.Target.Uri := 'sips:foo.com:5060';

  Locations := Self.Loc.FindServersFor(Self.Target);
  try
    Check(Locations.Count > 0, 'Too few locations');

    CheckEquals(TlsTransport,
                Locations.First.Transport,
                'SIPS Name:Port');
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

procedure TestTIdSipAbstractLocator.TestTransportParamTakesPrecedence;
var
  Locations: TIdSipLocations;
begin
  Self.TransportParam := TransportParamSCTP;
  Self.Target.Uri := 'sip:foo.com;transport=' + Self.TransportParam;

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

  Self.Loc := TIdSipMockLocator.Create;
end;

procedure TestTIdSipMockLocator.TearDown;
begin
  Self.Loc.Free;

  inherited TearDown;
end;

//* TestTIdSipMockLocator Published methods ************************************

procedure TestTIdSipMockLocator.TestResolveNameRecords;
const
  AOR = 'bar';
var
  Results: TStrings;
begin
  // All mixed up records
  Self.Loc.AddA('foo',    '127.0.0.3');
  Self.Loc.AddA(AOR,      '127.0.0.1');
  Self.Loc.AddAAAA(AOR,   '::1');
  Self.Loc.AddAAAA(AOR,   '::2');
  Self.Loc.AddA(AOR,      '127.0.0.2');
  Self.Loc.AddAAAA('foo', '::3');

  Results := Self.Loc.ResolveNameRecords(AOR);
  try
    CheckEquals(4,
                Results.Count,
                'Incorrect number of results: unwanted records added?');

    CheckEquals('127.0.0.1', Results[0], '1st record');
    CheckEquals('::1',       Results[1], '2nd record');
    CheckEquals('::2',       Results[2], '3rd record');
    CheckEquals('127.0.0.2', Results[3], '4th record');
  finally
    Results.Free;
  end;
end;

procedure TestTIdSipMockLocator.TestResolveNAPTR;
const
  AOR = 'bar';
var
  Results: TIdNaptrRecords;
begin
  Self.Loc.AddNAPTR(AOR,   20, 10, 's', 'SIP+D2T',  '_sip._tcp.bar');
  Self.Loc.AddNAPTR(AOR,   10, 10, 's', 'SIPS+D2T', '_sips._tls.bar');
  Self.Loc.AddNAPTR(AOR,   30, 10, 's', 'SIP+D2U',  '_sip._udp.bar');
  Self.Loc.AddNAPTR('foo', 30, 10, 's', 'SIP+D2U',  '_sip._udp.foo');

  Results := Self.Loc.ResolveNAPTR(AOR);
  try
    CheckEquals(3,
                Results.Count,
                'Incorrect number of results: unwanted records added?');

    CheckEquals('_sips._tls.bar', Results[0].Value, '1st record');
    CheckEquals('_sip._tcp.bar',  Results[1].Value, '2nd record');
    CheckEquals('_sip._udp.bar',  Results[2].Value, '3rd record');
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

initialization
  RegisterTest('SIP Location Services', Suite);
end.
