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
  Classes, IdSipDns, IdSipLocation, IdSipLocator, IdSipMessage,
  IdSipMockLocator, TestFramework, TestFrameworkSip;

type
  TLocatorTest = class(TTestCaseSip)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipAbstractLocator = class(TLocatorTest)
  private
    ARecord:        String;
    AAAARecord:     String;
    Domain:         String;
    IP:             String;
    Loc:            TIdSipMockLocator;
    Locations:      TIdSipLocations;
    NameRecs:       TIdDomainNameRecords;
    Naptr:          TIdNaptrRecords;
    Port:           Cardinal;
    Response:       TIdSipResponse;
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
    procedure TestSrvTarget;
    procedure TestTransportTypeFor;
    procedure TestTransportTypeForNameAndExplicitPort;
    procedure TestTransportTypeForNumericIPv4;
    procedure TestTransportTypeForNumericIPv6;
    procedure TestTransportTypeForWithNaptr;
    procedure TestTransportTypeForWithoutNaptrAndNoSrv;
    procedure TestTransportTypeForWithoutNaptrWithSrv;
    procedure TestFindServersForNameNaptrNoSrv;
    procedure TestFindServersForNameNaptrSomeSrv;
    procedure TestFindServersForNameNaptrSrv;
    procedure TestFindServersForNameNoNaptrManualTransportNoSrv;
    procedure TestFindServersForNameNoNaptrManualTransportSrv;
    procedure TestFindServersForNameNoNaptrNoManualTransportNoSrv;
    procedure TestFindServersForNameNoNaptrNoManualTransportSrv;
    procedure TestFindServersForNameWithPort;
    procedure TestFindServersForNameWithPortSips;
    procedure TestFindServersForNumericAddress;
    procedure TestFindServersForNumericAddressIPv6;
    procedure TestFindServersForNumericAddressSips;
    procedure TestFindServersForNumericAddressWithPort;
    procedure TestFindServersForNumericAddressWithPortSips;
    procedure TestFindServersForNumericMaddr;
    procedure TestFindServersForNumericMaddrIPv6;
    procedure TestFindServersForNumericMaddrSips;
    procedure TestFindServersForNumericMaddrSipsIPv6;
    procedure TestFindServersForResponseReceivedHasPrecedenceOverSentBy;
    procedure TestFindServersForResponseRportHasPrecedenceOverPort;
    procedure TestFindServersForResponseWithMalformedResponse;
    procedure TestFindServersForResponseWithNameAndPort;
    procedure TestFindServersForResponseWithNameNoSrv;
    procedure TestFindServersForResponseWithNumericSentBy;
    procedure TestFindServersForResponseWithNumericSentByAndPort;
    procedure TestFindServersForResponseWithReceivedParam;
    procedure TestFindServersForResponseWithReceivedParamAndRport;
    procedure TestFindServersForResponseWithReceivedParamAndNumericSentBy;
    procedure TestFindServersForResponseWithReceivedParamAndIPv6NumericSentBy;
    procedure TestFindServersForResponseWithSrv;
    procedure TestFindServersForSrvNoNameRecords;
    procedure TestFindServersForSrvNotAvailable;
    procedure TestFindServersForTransportParamTakesPrecedence;
  end;

  TestTIdSipMockLocator = class(TLocatorTest)
  private
    AOR: TIdUri;
    Loc: TIdSipMockLocator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLookupCount;
    procedure TestRemoveNameRecords;
    procedure TestResolveNameRecords;
    procedure TestResolveNameRecordsWithAutomagicNameRecords;
    procedure TestResolveNameRecordsWithoutAutomagicNameRecords;
    procedure TestResolveNAPTRSip;
    procedure TestResolveNAPTRSips;
    procedure TestResolveSRV;
    procedure TestResolveSRVWithNameRecords;
  end;

implementation

uses
  IdSipMockTransport, IdSipTransport, Math, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipLocator unit tests');
  Result.AddTest(TestTIdSipAbstractLocator.Suite);
  Result.AddTest(TestTIdSipMockLocator.Suite);
end;

//******************************************************************************
//* TLocatorTest                                                               *
//******************************************************************************
//* TLocatorTest Public methods ************************************************

procedure TLocatorTest.SetUp;
begin
  inherited SetUp;

  TIdSipTransportRegistry.RegisterTransportType(TlsOverSctpTransport, TIdSipMockTlsOverSctpTransport);
end;

procedure TLocatorTest.TearDown;
begin
  TIdSipTransportRegistry.UnregisterTransportType(TlsOverSctpTransport);

  inherited TearDown;
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
  Self.Locations  := TIdSipLocations.Create;
  Self.NameRecs   := TIdDomainNameRecords.Create;
  Self.Naptr      := TIdNaptrRecords.Create;
  Self.Port       := DefaultSipPort;
  Self.Response   := TIdSipTestResources.CreateBasicResponse;
  Self.Srv        := TIdSrvRecords.Create;
  Self.Target     := TIdSipUri.Create;
end;

procedure TestTIdSipAbstractLocator.TearDown;
begin
  Self.Target.Free;
  Self.Srv.Free;
  Self.Response.Free;
  Self.Naptr.Free;
  Self.NameRecs.Free;
  Self.Locations.Free;
  Self.Loc.Free;

  inherited Destroy;
end;

procedure TestTIdSipAbstractLocator.CheckAorAAAARecords(Locations: TIdSipLocations;
                                                        const ExpectedTransport: String;
                                                        const MsgPrefix: String);
begin
  CheckEquals(Self.Loc.NameRecords.Count,
              Locations.Count, MsgPrefix + ': Location count');

  CheckEquals(ExpectedTransport,
              Self.Locations[0].Transport,
              MsgPrefix + ': 1st record Transport');
  CheckEquals(Self.ARecord,
              Self.Locations[0].IPAddress,
              MsgPrefix + ': 1st record IPAddress');
  CheckEquals(Self.Port,
              Self.Locations[0].Port,
              MsgPrefix + ': 1st record Port');

  CheckEquals(ExpectedTransport,
              Self.Locations[1].Transport,
              MsgPrefix + ': 2nd record Transport');
  CheckEquals(Self.AAAARecord,
              Self.Locations[1].IPAddress,
              MsgPrefix + ': 2nd record IPAddress');
  CheckEquals(Self.Port,
              Self.Locations[1].Port,
              MsgPrefix + ': 2nd record Port');
end;

//* TestTIdSipAbstractLocator Private methods **********************************

procedure TestTIdSipAbstractLocator.AddNameRecords(const Domain: String);
begin
  Self.Loc.AddA(   Self.Domain, Self.ARecord);
  Self.Loc.AddAAAA(Self.Domain, Self.AAAARecord);
end;

//* TestTIdSipAbstractLocator Published methods ********************************

procedure TestTIdSipAbstractLocator.TestSrvTarget;
begin
  Self.Target.Uri := 'sip:' + Self.Domain;

  CheckEquals('_sip._tcp.' + Self.Domain,
              Self.Loc.SrvTarget(Self.Target, 'tcp'),
              'SIP/TCP lookup');

  CheckEquals('_sip._tcp.' + Self.Domain,
              Self.Loc.SrvTarget(Self.Target, 'TCP'),
              'Transports all lowercase');

  Self.Target.Scheme := SipsScheme;
  CheckEquals('_sips._tcp.' + Self.Domain,
              Self.Loc.SrvTarget(Self.Target, 'tls'),
              'SIP/TLS lookup');
end;

procedure TestTIdSipAbstractLocator.TestTransportTypeFor;
begin
  Self.TransportParam := TransportParamSCTP;
  Self.Target.Uri := 'sip:foo.com;transport=' + Self.TransportParam;

  CheckEquals(ParamToTransport(Self.TransportParam),
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'Transport parameter must take precedence');
end;

procedure TestTIdSipAbstractLocator.TestTransportTypeForNameAndExplicitPort;
begin
  Self.Target.Uri := 'sip:' + Self.Domain + ':' + IntToStr(Self.Port);

  CheckEquals(UdpTransport,
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Name, explicit port');

  Self.Target.Uri := 'sips:' + Self.Domain + ':' + IntToStr(Self.Port);

  CheckEquals(TlsTransport,
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIPS, Name, explicit port');
end;

procedure TestTIdSipAbstractLocator.TestTransportTypeForNumericIPv4;
begin
  Self.Target.Uri := 'sip:' + Self.IP;

  CheckEquals(UdpTransport,
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Numeric IPv4 address');

  Self.Target.Uri := 'sip:' + Self.IP + ':' + IntToStr(Self.Port);
  CheckEquals(UdpTransport,
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Numeric IPv4 address, explicit port');

  Self.Target.Scheme := SipsScheme;
  CheckEquals(TlsTransport,
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIPS, Numeric IPv4 address');

  Self.Target.Uri := 'sips:' + Self.IP + ':' + IntToStr(Self.Port);
  CheckEquals(TlsTransport,
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIPS, Numeric IPv4 address, explicit port');
end;

procedure TestTIdSipAbstractLocator.TestTransportTypeForNumericIPv6;
begin
  Self.IP := '[::1]';
  Self.Target.Uri := 'sip:' + Self.IP;

  CheckEquals(UdpTransport,
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Numeric IPv6 address');

  Self.Target.Uri := 'sip:' + Self.IP + ':' + IntToStr(Self.Port);
  CheckEquals(UdpTransport,
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Numeric IPv6 address, port');

  Self.Target.Scheme := SipsScheme;
  CheckEquals(TlsTransport,
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIPS, Numeric IPv6 address');

  Self.Target.Uri := 'sips:' + Self.IP + ':' + IntToStr(Self.Port);
  CheckEquals(TlsTransport,
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIPS, Numeric IPv6 address, explicit port');
end;

procedure TestTIdSipAbstractLocator.TestTransportTypeForWithNaptr;
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
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'Name, NAPTR records');
end;

procedure TestTIdSipAbstractLocator.TestTransportTypeForWithoutNaptrAndNoSrv;
begin
  Self.IP         := 'example.com';
  Self.Target.Uri := 'sip:' + Self.IP;

  CheckEquals(UdpTransport,
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Name, no NAPTR records, no SRV records');

  Self.Target.Scheme := SipsScheme;

  CheckEquals(TlsTransport,
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIPS, Name, no NAPTR records, no SRV records');
end;

procedure TestTIdSipAbstractLocator.TestTransportTypeForWithoutNaptrWithSrv;
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
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIP, Name, no NAPTR records, but SRV records');

  Self.Target.Scheme := SipsScheme;
  CheckEquals(TcpTransport,
              Self.Loc.TransportTypeFor(Self.Target,
                                    Self.Naptr,
                                    Self.Srv,
                                    Self.NameRecs),
              'SIPS, Name, no NAPTR records, but SRV records (none acceptable)');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameNaptrNoSrv;
begin
  // The target's a domain name, we've NAPTR records from the transport lookup,
  // but no SRV RRs.
  Self.Target.Uri := 'sip:' + Self.Domain;
  Self.Loc.AddNAPTR(Self.Domain, 0, 0, NaptrDefaultFlags, NaptrTcpService,
                    '_sip._tcp.' + Self.Domain);
  Self.AddNameRecords(Self.Target.Host);

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  Self.CheckAorAAAARecords(Self.Locations,
                           Self.Loc.NAPTR[0].AsSipTransport,
                           'NAPTR, no SRV');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameNaptrSomeSrv;
begin
  // Iterate over SRV records
  Self.Target.Uri := 'sip:' + Self.Domain;

  // We have two NAPTR records. Probably as a result of an admin slip-up,
  // there're no SRV records for the first NAPTR. This test shows that we look
  // up SRV stuff for NAPTR records at least until we find some SRVs.
  Self.Loc.AddNAPTR(Self.Domain, 0, 0, NaptrDefaultFlags, NaptrTlsService, SrvTlsPrefix + '.' + Self.Domain);
  Self.Loc.AddNAPTR(Self.Domain, 0, 0, NaptrDefaultFlags, NaptrTcpService, SrvTcpPrefix + '.' + Self.Domain);
  Self.Loc.AddSRV(Self.Domain, SrvTcpPrefix, 0, 0, DefaultSipPort, 'sip.' + Self.Domain);
  Self.Loc.AddAAAA('sip.' + Self.Domain, Self.AAAARecord);

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  CheckEquals(Self.Loc.NameRecords.Count,
              Self.Locations.Count,
              'No SRV lookup for NAPTR records beyond the first?');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameNaptrSrv;
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

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  CheckEquals(4,                    Self.Locations.Count,        'Location count');
  CheckEquals(TlsTransport,         Self.Locations[0].Transport, '1st record Transport');
  CheckEquals(Self.ARecord,         Self.Locations[0].IPAddress, '1st record IPAddress');
  CheckEquals(Self.Loc.SRV[0].Port, Self.Locations[0].Port,      '1st record Port');
  CheckEquals(TlsTransport,         Self.Locations[1].Transport, '2nd record Transport');
  CheckEquals(Self.AAAARecord,      Self.Locations[1].IPAddress, '2nd record IPAddress');
  CheckEquals(Self.Loc.SRV[0].Port, Self.Locations[1].Port,      '2nd record Port');

  CheckEquals(TcpTransport,         Self.Locations[2].Transport, '3rd record Transport');
  CheckEquals(Self.ARecord,         Self.Locations[2].IPAddress, '3rd record IPAddress');
  CheckEquals(Self.Loc.SRV[1].Port, Self.Locations[2].Port,      '3rd record Port');
  CheckEquals(TcpTransport,         Self.Locations[3].Transport, '4th record Transport');
  CheckEquals(Self.AAAARecord,      Self.Locations[3].IPAddress, '4th record IPAddress');
  CheckEquals(Self.Loc.SRV[1].Port, Self.Locations[3].Port,      '4th record Port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameNoNaptrManualTransportNoSrv;
begin
  // The target's a domain name, we've no NAPTR records from the transport lookup,
  // we've no SRV RRs, but we've a manually-specified transport.
  Self.Target.Uri := 'sip:' + Self.Domain + ';transport=' + TransportParamTLS_SCTP;
  Self.AddNameRecords(Self.Target.Host);

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  Self.CheckAorAAAARecords(Locations,
                           ParamToTransport(Self.Target.Transport),
                           'No NAPTR, no SRV, transport param');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameNoNaptrManualTransportSrv;
begin
  // The target's a domain name, we've no NAPTR records from the transport lookup,
  // we've SRV RRs, and the transport's specified.

  Self.Target.Uri := 'sip:' + Self.Domain + ';transport=tls';

  Self.Loc.AddSRV(Self.Domain, SrvTlsPrefix, 0, 0, 0, Self.Domain);
  Self.Loc.AddSRV(Self.Domain, SrvTcpPrefix, 1, 0, 0, Self.Domain);

  Self.AddNameRecords(Self.Target.Host);

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  CheckEquals(2,                    Self.Locations.Count,        'Location count');
  CheckEquals(TlsTransport,         Self.Locations[0].Transport, '1st record Transport');
  CheckEquals(Self.ARecord,         Self.Locations[0].IPAddress, '1st record IPAddress');
  CheckEquals(Self.Loc.SRV[0].Port, Self.Locations[0].Port,      '1st record Port');
  CheckEquals(TlsTransport,         Self.Locations[1].Transport, '2nd record Transport');
  CheckEquals(Self.AAAARecord,      Self.Locations[1].IPAddress, '2nd record IPAddress');
  CheckEquals(Self.Loc.SRV[0].Port, Self.Locations[1].Port,      '2nd record Port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameNoNaptrNoManualTransportNoSrv;
begin
  // A/AAAA lookup
  Self.Target.Uri := 'sips:' + Self.Domain;

  Self.Port := DefaultSipsPort; // default for TLS
  Self.AddNameRecords(Self.Target.Host);

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  Self.CheckAorAAAARecords(Self.Locations,
                           TlsTransport,
                           'No NAPTR, no SRV, no transport param');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameNoNaptrNoManualTransportSrv;
begin
  Self.Target.Uri := 'sip:' + Self.Domain;

  Self.AddNameRecords(Self.Domain);
  Self.Loc.AddSRV(Self.Domain, SrvTlsPrefix, 0, 0, 0, Self.Domain);
  Self.Loc.AddSRV(Self.Domain, SrvTcpPrefix, 1, 0, 0, Self.Domain);

  // iterate over SRV
  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  CheckEquals(4,                    Self.Locations.Count,        'Location count');
  CheckEquals(TlsTransport,         Self.Locations[0].Transport, '1st record Transport');
  CheckEquals(Self.ARecord,         Self.Locations[0].IPAddress, '1st record IPAddress');
  CheckEquals(Self.Loc.SRV[0].Port, Self.Locations[0].Port,      '1st record Port');
  CheckEquals(TlsTransport,         Self.Locations[1].Transport, '2nd record Transport');
  CheckEquals(Self.AAAARecord,      Self.Locations[1].IPAddress, '2nd record IPAddress');
  CheckEquals(Self.Loc.SRV[0].Port, Self.Locations[1].Port,      '2nd record Port');

  CheckEquals(TcpTransport,         Self.Locations[2].Transport, '3rd record Transport');
  CheckEquals(Self.ARecord,         Self.Locations[2].IPAddress, '3rd record IPAddress');
  CheckEquals(Self.Loc.SRV[1].Port, Self.Locations[2].Port,      '3rd record Port');
  CheckEquals(TcpTransport,         Self.Locations[3].Transport, '4th record Transport');
  CheckEquals(Self.AAAARecord,      Self.Locations[3].IPAddress, '4th record IPAddress');
  CheckEquals(Self.Loc.SRV[1].Port, Self.Locations[3].Port,      '4th record Port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameWithPort;
begin
  Self.AddNameRecords(Self.Target.Host);

  Self.Target.Uri := 'sip:' + Self.Domain + ':' + IntToStr(Self.Port);

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  Self.CheckAorAAAARecords(Self.Locations, UdpTransport, 'SIP URI');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNameWithPortSips;
begin
  Self.AddNameRecords(Self.Target.Host);

  Self.Target.Uri := 'sips:' + Self.Domain + ':' + IntToStr(Self.Port);

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  Self.CheckAorAAAARecords(Self.Locations, TlsTransport, 'SIPS URI');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNumericAddress;
begin
  Self.Target.Uri := 'sip:' + Self.IP;

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  CheckEquals(1,            Self.Locations.Count,        'SIP Location count');
  CheckEquals(UdpTransport, Self.Locations[0].Transport, 'SIP Transport');
  CheckEquals(Self.IP,      Self.Locations[0].IPAddress, 'SIP IPAddress');
  CheckEquals(DefaultSipPort,   Self.Locations[0].Port,      'SIP Port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNumericAddressIPv6;
begin
  Self.IP := '[::1]';
  Self.Target.Uri := 'sip:' + Self.IP;

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  CheckEquals(1,            Self.Locations.Count,        'SIP IPv6 Location count');
  CheckEquals(UdpTransport, Self.Locations[0].Transport, 'SIP IPv6 Transport');
  CheckEquals(Self.IP,      Self.Locations[0].IPAddress, 'SIP IPv6 IPAddress');
  CheckEquals(DefaultSipPort,   Self.Locations[0].Port,      'SIP IPv6 Port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNumericAddressSips;
begin
  Self.Target.Uri := 'sip:' + Self.IP;

  Self.Target.Scheme := SipsScheme;

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  CheckEquals(1,            Self.Locations.Count,        'SIPS Location count');
  CheckEquals(TlsTransport, Self.Locations[0].Transport, 'SIPS Transport');
  CheckEquals(Self.IP,      Self.Locations[0].IPAddress, 'SIPS IPAddress');
  CheckEquals(DefaultSipsPort,  Self.Locations[0].Port,      'SIPS Port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNumericAddressWithPort;
begin
  Self.Target.Uri := 'sip:' + Self.IP + ':' + IntToStr(Self.Port);

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  CheckEquals(1,            Self.Locations.Count,        'SIP Location count');
  CheckEquals(UdpTransport, Self.Locations[0].Transport, 'SIP Transport');
  CheckEquals(Self.IP,      Self.Locations[0].IPAddress, 'SIP IPAddress');
  CheckEquals(Self.Port,    Self.Locations[0].Port,      'SIP Port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNumericAddressWithPortSips;
begin
  Self.Target.Uri := 'sips:' + Self.IP + ':' + IntToStr(Self.Port);

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  CheckEquals(1,            Self.Locations.Count,        'SIPS Location count');
  CheckEquals(TlsTransport, Self.Locations[0].Transport, 'SIPS Transport');
  CheckEquals(Self.IP,      Self.Locations[0].IPAddress, 'SIPS IPAddress');
  CheckEquals(Self.Port,    Self.Locations[0].Port,      'SIPS Port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNumericMaddr;
begin
  Self.Target.Uri := 'sip:foo.com;maddr=' + Self.IP;

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  Check(Self.Locations.Count > 0, 'Too few locations');

  CheckEquals(UdpTransport, Self.Locations.First.Transport, 'Transport');
  CheckEquals(Self.IP,      Self.Locations.First.IPAddress, 'IPAddress');
  CheckEquals(Self.Port,    Self.Locations.First.Port,      'Port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNumericMaddrIPv6;
begin
  Self.Target.Uri := 'sip:foo.com;maddr=' + Self.AAAARecord;

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  Check(Self.Locations.Count > 0, 'Too few locations');

  CheckEquals(UdpTransport,    Self.Locations[0].Transport, 'Transport');
  CheckEquals(Self.AAAARecord, Self.Locations[0].IPAddress, 'IPAddress');
  CheckEquals(Self.Port,       Self.Locations[0].Port,      'Port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNumericMaddrSips;
begin
  Self.Target.Uri := 'sips:foo.com;maddr=' + Self.ARecord;

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  Check(Self.Locations.Count > 0, 'Too few locations');

  CheckEquals(TlsTransport, Self.Locations[0].Transport, 'Transport');
  CheckEquals(Self.ARecord, Self.Locations[0].IPAddress, 'IPAddress');
  CheckEquals(DefaultSipsPort,  Self.Locations[0].Port,      'Port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForNumericMaddrSipsIPv6;
begin
  Self.Target.Uri := 'sips:foo.com;maddr=' + Self.AAAARecord;

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  Check(Self.Locations.Count > 0, 'Too few locations');

  CheckEquals(TlsTransport,    Self.Locations[0].Transport, 'Transport');
  CheckEquals(Self.AAAARecord, Self.Locations[0].IPAddress, 'IPAddress');
  CheckEquals(DefaultSipsPort,     Self.Locations[0].Port,      'Port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseReceivedHasPrecedenceOverSentBy;
begin
  Self.IP   := '127.0.0.2';

  Self.Response.LastHop.Value := 'SIP/2.0/UDP 127.0.0.1'
                                           + ';received=' + Self.IP;

  Self.Loc.FindServersFor(Response, Self.Locations);

  CheckEquals(1,       Self.Locations.Count,        'Location count');
  CheckEquals(Self.IP, Self.Locations[0].IPAddress, 'IPAddress');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseRportHasPrecedenceOverPort;
begin
  Self.IP   := '127.0.0.2';
  Self.Port := 8000;

  Self.Response.LastHop.Value := 'SIP/2.0/UDP 127.0.0.1:6666'
                                           + ';received=127.0.0.2;rport=' + IntToStr(Self.Port);

  Self.Loc.FindServersFor(Response, Self.Locations);

  CheckEquals(1,         Self.Locations.Count,   'Location count');
  CheckEquals(Self.Port, Self.Locations[0].Port, 'Port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithMalformedResponse;
begin
  // No Via headers? No locations!

  Self.Response.Path.Clear;
  try
    Self.Loc.FindServersFor(Response, Self.Locations);
  except
    on ESipLocator do;
  end;
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithNameAndPort;
begin
  Self.Loc.AddAAAA(Self.Domain, Self.AAAARecord);
  Self.Loc.AddA(   Self.Domain, Self.ARecord);

  Self.Response.LastHop.Value := 'SIP/2.0/UDP ' + Self.Domain + ':6666';

  Self.Loc.FindServersFor(Response, Self.Locations);

  CheckEquals(Response.LastHop.Transport,
              Self.Locations[0].Transport,
              'First location transport');
  CheckEquals(Self.Loc.NameRecords[0].IPAddress,
              Self.Locations[0].IPAddress,
              'First location address');
  CheckEquals(Response.LastHop.Port,
              Self.Locations[0].Port,
              'First location port');

  CheckEquals(Response.LastHop.Transport,
              Self.Locations[1].Transport,
              'Second location transport');
  CheckEquals(Self.Loc.NameRecords[1].IPAddress,
              Self.Locations[1].IPAddress,
              'Second location address');
  CheckEquals(Response.LastHop.Port,
              Self.Locations[1].Port,
              'Second location port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithNameNoSrv;
begin
  Self.AddNameRecords(Self.Domain);

  Self.Response.LastHop.Value := 'SIP/2.0/UDP ' + Domain;

  Self.Loc.FindServersFor(Response, Self.Locations);

  Self.CheckAorAAAARecords(Locations, UdpTransport, 'Response, Name, no SRV');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithNumericSentBy;
begin
  Self.Response.LastHop.Value := 'SIP/2.0/UDP 127.0.0.1';

  Self.Loc.FindServersFor(Response, Self.Locations);

  CheckEquals(Response.LastHop.Transport,
              Self.Locations[0].Transport,
              'First location transport');
  CheckEquals(Self.IP,
              Self.Locations[0].IPAddress,
              'First location address');
  CheckEquals(Response.LastHop.Port,
              Self.Locations[0].Port,
              'First location port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithNumericSentByAndPort;
begin
  Self.Response.LastHop.Value := 'SIP/2.0/UDP 127.0.0.1:666';

  Self.Loc.FindServersFor(Response, Self.Locations);

  CheckEquals(Response.LastHop.Transport,
              Self.Locations[0].Transport,
              'First location transport');
  CheckEquals(Self.IP,
              Self.Locations[0].IPAddress,
              'First location address');
  CheckEquals(Response.LastHop.Port,
              Self.Locations[0].Port,
              'First location port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithReceivedParam;
begin
  Self.Response.LastHop.Value := 'SIP/2.0/UDP gw1.leo-ix.net;received=' + Self.IP;

  Self.Loc.FindServersFor(Response, Self.Locations);

  CheckEquals(Response.LastHop.Transport,
              Self.Locations[0].Transport,
              'First location transport');
  CheckEquals(Response.LastHop.Received,
              Self.Locations[0].IPAddress,
              'First location address');
  CheckEquals(Response.LastHop.Port,
              Self.Locations[0].Port,
              'First location port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithReceivedParamAndRport;
begin
  Self.Port := 6666;
  Self.Response.LastHop.Value := 'SIP/2.0/UDP gw1.leo-ix.net'
                                           + ';received=' + Self.IP
                                           + ';rport=' + IntToStr(Self.Port);

  Self.Loc.FindServersFor(Response, Self.Locations);

  CheckEquals(Response.LastHop.Transport,
              Self.Locations[0].Transport,
              'First location transport');
  CheckEquals(Response.LastHop.Received,
              Self.Locations[0].IPAddress,
              'First location address');
  CheckEquals(Response.LastHop.Rport,
              Self.Locations[0].Port,
              'First location port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithReceivedParamAndNumericSentBy;
const
  SentByIP = '6.6.6.6';
begin
  Self.Response.LastHop.Value := 'SIP/2.0/UDP ' + SentByIP + ';received=' + Self.IP;

  Self.Loc.FindServersFor(Response, Self.Locations);

  CheckEquals(Response.LastHop.Transport,
              Self.Locations[0].Transport,
              'First location transport');
  CheckEquals(Self.IP,
              Self.Locations[0].IPAddress,
              'First location address');
  CheckEquals(Response.LastHop.Port,
              Self.Locations[0].Port,
              'First location port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithReceivedParamAndIpv6NumericSentBy;
const
  SentByIP = '[2002:dead:beef:1::1]';
begin
  Self.Response.LastHop.Value := 'SIP/2.0/UDP ' + SentByIP + ';received=' + Self.IP;

  Self.Loc.FindServersFor(Response, Self.Locations);

  CheckEquals(Response.LastHop.Transport,
              Self.Locations[0].Transport,
              'First location transport');
  CheckEquals(Self.IP,
              Self.Locations[0].IPAddress,
              'First location address');
  CheckEquals(Response.LastHop.Port,
              Self.Locations[0].Port,
              'First location port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForResponseWithSrv;
const
  SecondRecord = '::2';
  ThirdRecord  = '::3';
var
  TlsDomain: String;
begin
  TlsDomain := 'sips.' + Self.Domain;

  Self.Loc.AddSRV(Self.Domain, SrvTlsPrefix, 0, 0, DefaultSipsPort, TlsDomain);
  Self.Loc.AddSRV(Self.Domain, SrvTcpPrefix, 0, 0, DefaultSipPort,  Self.Domain);

  Self.Loc.AddAAAA(Self.Domain, Self.AAAARecord);
  Self.Loc.AddAAAA(TlsDomain, SecondRecord);
  Self.Loc.AddAAAA(TlsDomain, ThirdRecord);

  Self.Response.LastHop.Value := 'SIP/2.0/TLS ' + Self.Domain;

  Self.Loc.FindServersFor(Response, Self.Locations);

  CheckEquals(2,
              Self.Locations.Count,
              'Wrong number of records');

  CheckEquals(TlsTransport,
              Self.Locations[0].Transport,
              '1st record transport');
  CheckEquals(SecondRecord,
              Self.Locations[0].IPAddress,
              '1st record address');
  CheckEquals(DefaultSipsPort,
              Self.Locations[0].Port,
              '1st record port');
  CheckEquals(TlsTransport,
              Self.Locations[1].Transport,
              '2nd record transport');
  CheckEquals(ThirdRecord,
              Self.Locations[1].IPAddress,
              '2nd record address');
  CheckEquals(DefaultSipsPort,
              Self.Locations[1].Port,
              '2nd record port');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForSrvNoNameRecords;
begin
  Self.Loc.ReturnOnlySpecifiedRecords := true;

  Self.Target.Uri :='sip:' + Self.Domain;

  Self.Loc.AddSRV(Self.Domain, SrvTcpPrefix,  0, 0, 0, Self.Domain);
  Self.Loc.AddSRV(Self.Domain, SrvSctpPrefix, 0, 0, 0, Self.Domain);

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  Check(Locations.IsEmpty,
        'The locator added locations that don''t exist');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForSrvNotAvailable;
begin
  // SRV targets can sometimes be '.' - the root name of all domain names.
  // We ignore them (they mean "we don't support the service you're looking
  // for"). Once we have the SRV records though we need A/AAAA records to
  // get the actual IP addresses we want to contact.

  Self.Target.Uri :='sip:' + Self.Domain;

  Self.Loc.AddSRV(Self.Domain, SrvTcpPrefix,  0, 0, 0, SrvNotAvailableTarget);
  Self.Loc.AddSRV(Self.Domain, SrvSctpPrefix, 0, 0, 0, Self.Domain);

  Self.Loc.AddA(Self.Domain, Self.ARecord);

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  CheckEquals(1,
              Self.Locations.Count,
              'The locator didn''t filter out the "unavailable" SRV');
  CheckEquals(SctpTransport, Self.Locations[0].Transport, 'Wrong location found');
end;

procedure TestTIdSipAbstractLocator.TestFindServersForTransportParamTakesPrecedence;
begin
  Self.TransportParam := TransportParamSCTP;
  Self.Target.Uri := 'sip:127.0.0.1;transport=' + Self.TransportParam;

  Self.Loc.FindServersFor(Self.Target, Self.Locations);

  Check(Self.Locations.Count > 0, 'Too few locations');

  CheckEquals(ParamToTransport(Self.TransportParam),
              Self.Locations[0].Transport,
              'Transport param overrules all');
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

procedure TestTIdSipMockLocator.TestLookupCount;
var
  Locations: TIdSipLocations;
  Response:  TIdSipResponse;
begin
  CheckEquals(0, Self.Loc.LookupCount, 'Initial value');

  Locations := TIdSipLocations.Create;
  try
    Self.Loc.FindServersFor('', Locations);

    CheckEquals(1, Self.Loc.LookupCount, 'After FindServersFor(URI)');
  finally
    Locations.Free;
  end;

  Response := TIdSipTestResources.CreateBasicResponse;
  try
    Locations := TIdSipLocations.Create;
    try
      Self.Loc.FindServersFor(Response, Locations);

      CheckEquals(2, Self.Loc.LookupCount, 'After FindServersFor(SipResponse)');
    finally
      Locations.Free;
    end;
  finally
    Response.Free;
  end;

  Self.Loc.ResetLookupCount;
  CheckEquals(0, Self.Loc.LookupCount, 'After ResetLookupCount');
end;

procedure TestTIdSipMockLocator.TestRemoveNameRecords;
const
  DomainName = 'sip.tessier-ashpool.co.luna';
var
  Results: TIdDomainNameRecords;
begin
  Self.Loc.AddA(DomainName, '10.0.0.1');
  Self.Loc.AddAAAA(DomainName, '2002:dead:beef::1');

  Self.Loc.RemoveNameRecords(DomainName);

  Results := TIdDomainNameRecords.Create;
  try
    // We don't want automatically-created name records!
    Self.Loc.ReturnOnlySpecifiedRecords := true;

    Self.Loc.ResolveNameRecords(DomainName, Results);
    Check(Results.IsEmpty,
          'A/AAAA records returned, so RemoveNameRecords didn''t remove name '
        + 'records');
  finally
    Results.Free;
  end;
end;

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

procedure TestTIdSipMockLocator.TestResolveNameRecordsWithAutomagicNameRecords;
var
  NameRecord: String;
  Results:    TIdDomainNameRecords;
begin
  Results := TIdDomainNameRecords.Create;
  try
    Self.Loc.ResolveNameRecords(Self.AOR.Host, Results);

    CheckEquals(1,
                Results.Count,
                'MockLocator didn''t return a randomly-created name record');

    NameRecord := Results[0].IPAddress;

    Results.Clear;

    Self.Loc.ResolveNameRecords(Self.AOR.Host, Results);
    CheckEquals(1,
                Results.Count,
                'MockLocator didn''t return the previously-created name record, '
              + ' or created yet another address');
    CheckEquals(NameRecord,
                Results[0].IPAddress,
                'MockLocator didn''t persist the randomly-created name record');
  finally
    Results.Free;
  end;
end;

procedure TestTIdSipMockLocator.TestResolveNameRecordsWithoutAutomagicNameRecords;
var
  Results: TIdDomainNameRecords;
begin
  Self.Loc.ReturnOnlySpecifiedRecords := true;

  Results := TIdDomainNameRecords.Create;
  try
    Self.Loc.ResolveNameRecords(Self.AOR.Host, Results);

    CheckEquals(0, Results.Count, 'MockLocator created a name record for the domain');

    Self.Loc.ResolveNameRecords(Self.AOR.Host, Results);
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
  Self.Loc.AddSRV('foo.bar', SrvTlsPrefix,  0, 0, DefaultSipsPort, 'paranoid.bar');
  Self.Loc.AddSRV('foo.bar', SrvTcpPrefix, 10, 1, DefaultSipPort , 'backup.bar');
  Self.Loc.AddSRV('foo.bar', SrvTcpPrefix, 10, 2, DefaultSipPort , 'normal.bar');
  Self.Loc.AddSRV('foo.bar', SrvTcpPrefix, 20, 0, DefaultSipPort , 'fallback.bar');
  Self.Loc.AddSRV('boo.far', SrvTlsPrefix,  0, 0, DefaultSipsPort, 'paranoid.far');

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
  Self.Loc.AddSRV('foo.bar', SrvTlsPrefix,  0, 0, DefaultSipsPort, 'paranoid.bar');
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

initialization
  RegisterTest('SIP Location Services', Suite);
end.
