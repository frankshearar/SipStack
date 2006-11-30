{
  (c) 2005 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipIndyLocator;

interface

uses
  IdSipDns, IdSipIndyLocator, IdSipMockDnsServer, TestFramework;

type
  // The NameServer provides hard-coded answers to the questions. The important
  // part, the bit we're actually testing, is that the locator Loc can correctly
  // parse and present the answer.
  TestTIdSipIndyLocator = class(TTestCase)
  private
    Loc:         TIdSipIndyLocator;
    NameRecs:    TIdDomainNameRecords;
    NameServer:  TIdSipMockDnsServer;
    NaptrRecs:   TIdNaptrRecords;
    SrvRecs:     TIdSrvRecords;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestResolveNameRecords;
//    procedure TestResolveNameRecordsIPv6;
    procedure TestResolveNameRecordsNetworkFailure;
    procedure TestResolveNameRecordsOnNonexistentDomain;
    procedure TestResolveNameRecordsWithCNAMEChain;
    procedure TestResolveNameRecordsWithCNAMEsAndNameRecords;
    procedure TestResolveNameRecordsWithCNAMEsOnly;
    procedure TestResolveNAPTR;
    procedure TestResolveNAPTRNetworkFailure;
    procedure TestResolveNAPTROnNonexistentDomain;
    procedure TestResolveSRV;
    procedure TestResolveSRVNetworkFailure;
    procedure TestResolveSRVOnNonexistentDomain;
  end;

implementation

uses
  IdSipMessage;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdIndyLocator unit tests');
  Result.AddTest(TestTIdSipIndyLocator.Suite);
end;

//******************************************************************************
//* TestTIdSipIndyLocator                                                      *
//******************************************************************************
//* TestTIdSipIndyLocator Public methods ***************************************

procedure TestTIdSipIndyLocator.SetUp;
begin
  inherited SetUp;

  Self.Loc := TIdSipIndyLocator.Create;
  Self.Loc.NameServer          := '127.0.0.1';
  Self.Loc.ResolveLocallyFirst := false;

  Self.NameRecs  := TIdDomainNameRecords.Create;
  Self.NaptrRecs := TIdNaptrRecords.Create;
  Self.SrvRecs   := TIdSrvRecords.Create;

  Self.NameServer := TIdSipMockDnsServer.Create;
end;

procedure TestTIdSipIndyLocator.TearDown;
begin
  Self.NameServer.Free;

  Self.SrvRecs.Free;
  Self.NaptrRecs.Free;
  Self.NameRecs.Free;

  Self.Loc.Free;

  inherited TearDown;
end;

//* TestTIdSipIndyLocator Published methods ************************************

procedure TestTIdSipIndyLocator.TestResolveNameRecords;
begin
  Self.NameServer.AddAnswer(Self.NameServer.ARecords);

  Self.Loc.ResolveNameRecords('paranoid.leo-ix.net', Self.NameRecs);

  // See the comment in Self.NameServer.ARecords.
  CheckEquals(2, Self.NameRecs.Count, 'Record count');

  CheckEquals(DnsARecord,            Self.NameRecs[0].RecordType, '1st record record type');
  CheckEquals('paranoid.leo-ix.net', Self.NameRecs[0].Domain,     '1st record domain');
  CheckEquals('127.0.0.1',           Self.NameRecs[0].IPAddress,  '1st record IP address');

  CheckEquals(DnsARecord,            Self.NameRecs[1].RecordType, '2nd record record type');
  CheckEquals('paranoid.leo-ix.net', Self.NameRecs[1].Domain,     '2nd record domain');
  CheckEquals('127.0.0.2',           Self.NameRecs[1].IPAddress,  '2nd record IP address');
end;
{
procedure TestTIdSipIndyLocator.TestResolveNameRecordsIPv6;
begin
  Fail('Indy 9 doesn''t support IPv6, so we can''t resolve AAAA records');

  Self.NameServer.AddAnswer(Self.NameServer.AAAARecords);

  Self.Loc.ResolveNameRecords('paranoid.leo-ix.net', Self.NameRecs);

  // See the comment in Self.NameServer.AAAARecords.
  CheckEquals(1, Self.NameRecs.Count, 'Record count');
  CheckEquals(DnsAAAARecord,         Self.NameRecs[0].RecordType, '1st record record type (AAAA)');
  CheckEquals('paranoid.leo-ix.net', Self.NameRecs[0].Domain,     '1st record domain (AAAA)');
  CheckEquals('::1',                 Self.NameRecs[0].IPAddress,  '1st record IP address (AAAA)');
end;
}
procedure TestTIdSipIndyLocator.TestResolveNameRecordsNetworkFailure;
begin
  // This shows what happens when something on the network goes wrong (like,
  // say, the name server disappearing).
  Self.NameServer.Stop;

  Self.Loc.ResolveNameRecords('paranoid.leo-ix.net', Self.NameRecs);

  CheckEquals(0, Self.NameRecs.Count, 'Record count');
end;

procedure TestTIdSipIndyLocator.TestResolveNameRecordsOnNonexistentDomain;
begin
  Self.NameServer.AddAnswer(Self.NameServer.NoSuchRecord);

  Self.Loc.ResolveNameRecords('foo.bar', Self.NameRecs);

  CheckEquals(0, Self.NameRecs.Count, 'Record count');
end;

procedure TestTIdSipIndyLocator.TestResolveNameRecordsWithCNAMEChain;
begin
  Self.NameServer.AddAnswer(Self.NameServer.CNAMEChain);

  Self.Loc.ResolveNameRecords('www.borland.com', Self.NameRecs);

  CheckEquals(2, Self.NameRecs.Count, 'Record count');

  CheckEquals(DnsARecord,        Self.NameRecs[0].RecordType, '1st record record type');
  CheckEquals('www.borland.com', Self.NameRecs[0].Domain,     '1st record domain');
  CheckEquals('196.33.166.208',  Self.NameRecs[0].IPAddress,  '1st record IP address');
  CheckEquals(DnsARecord,        Self.NameRecs[1].RecordType, '2nd record record type');
  CheckEquals('www.borland.com', Self.NameRecs[1].Domain,     '2nd record domain');
  CheckEquals('196.33.166.210',  Self.NameRecs[1].IPAddress,  '2nd record IP address');
end;

procedure TestTIdSipIndyLocator.TestResolveNameRecordsWithCNAMEsAndNameRecords;
begin
  Self.NameServer.AddAnswer(Self.NameServer.ARecordsWithCNAME);

  Self.Loc.ResolveNameRecords('proxy.leo-ix.net', Self.NameRecs);

  // See the comment in Self.NameServer.ARecordsWithCNAME.
  CheckEquals(1, Self.NameRecs.Count, 'Record count');

  CheckEquals(DnsARecord,         Self.NameRecs[0].RecordType, 'Record type');
  CheckEquals('proxy.leo-ix.net', Self.NameRecs[0].Domain,     'Domain');
  CheckEquals('127.0.0.1',        Self.NameRecs[0].IPAddress,  'IP address');
end;

procedure TestTIdSipIndyLocator.TestResolveNameRecordsWithCNAMEsOnly;
begin
  Self.NameServer.AddAnswer(Self.NameServer.ARecordsWithCNAMEOnly);
  Self.NameServer.AddAnswer(Self.NameServer.ARecordProxy);

  Self.Loc.ResolveNameRecords('proxy.leo-ix.net', Self.NameRecs);

  // See the comment in Self.NameServer.ARecordsWithCNAME.
  CheckEquals(1, Self.NameRecs.Count, 'Record count');

  CheckEquals(DnsARecord,         Self.NameRecs[0].RecordType, 'Record type');
  CheckEquals('proxy.leo-ix.net', Self.NameRecs[0].Domain,     'Domain');
  CheckEquals('127.0.0.1',        Self.NameRecs[0].IPAddress,  'IP address');
end;

procedure TestTIdSipIndyLocator.TestResolveNAPTR;
var
  Uri: TIdSipUri;
begin
  Self.NameServer.AddAnswer(Self.NameServer.NaptrRecords);

  Uri := TIdSipUri.Create('sip:leo-ix.net');
  try
    Self.Loc.ResolveNAPTR(Uri, Self.NaptrRecs);
    // See the comment in Self.NameServer.NaptrRecords.
    CheckEquals(3, Self.NaptrRecs.Count, 'Record count');

    CheckEquals('s',                     Self.NaptrRecs[0].Flags,      '1st record Flag');
    CheckEquals('leo-ix.net',            Self.NaptrRecs[0].Key,        '1st record Key');
    CheckEquals(0,                       Self.NaptrRecs[0].Order,      '1st record Order');
    CheckEquals(0,                       Self.NaptrRecs[0].Preference, '1st record Preference');
    CheckEquals('',                      Self.NaptrRecs[0].Regex,      '1st record Regex');
    CheckEquals('SIPS+D2T',              Self.NaptrRecs[0].Service,    '1st record Service');
    CheckEquals('_sips._tcp.leo-ix.net', Self.NaptrRecs[0].Value,      '1st record Value');

    CheckEquals('s',                     Self.NaptrRecs[1].Flags,      '2nd record Flag');
    CheckEquals('leo-ix.net',            Self.NaptrRecs[1].Key,        '2nd record Key');
    CheckEquals(0,                       Self.NaptrRecs[1].Order,      '2nd record Order');
    CheckEquals(0,                       Self.NaptrRecs[1].Preference, '2nd record Preference');
    CheckEquals('',                      Self.NaptrRecs[1].Regex,      '2nd record Regex');
    CheckEquals('SIP+D2T',               Self.NaptrRecs[1].Service,    '2nd record Service');
    CheckEquals('_sip._tcp.leo-ix.net',  Self.NaptrRecs[1].Value,      '2nd record Value');

    CheckEquals('s',                     Self.NaptrRecs[2].Flags,      '3rd record Flag');
    CheckEquals('leo-ix.net',            Self.NaptrRecs[2].Key,        '3rd record Key');
    CheckEquals(0,                       Self.NaptrRecs[2].Order,      '3rd record Order');
    CheckEquals(0,                       Self.NaptrRecs[2].Preference, '3rd record Preference');
    CheckEquals('',                      Self.NaptrRecs[2].Regex,      '3rd record Regex');
    CheckEquals('SIP+D2U',               Self.NaptrRecs[2].Service,    '3rd record Service');
    CheckEquals('_sip._udp.leo-ix.net',  Self.NaptrRecs[2].Value,      '3rd record Value');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipIndyLocator.TestResolveNAPTRNetworkFailure;
var
  Uri: TIdSipUri;
begin
  // This shows what happens when something on the network goes wrong (like,
  // say, the name server disappearing).
  Self.NameServer.Stop;

  Uri := TIdSipUri.Create('sip:foo.bar');
  try
    Self.Loc.ResolveNAPTR(Uri, Self.NaptrRecs);

    CheckEquals(0, Self.NaptrRecs.Count, 'Record count');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipIndyLocator.TestResolveNAPTROnNonexistentDomain;
var
  Uri: TIdSipUri;
begin
  Self.NameServer.AddAnswer(Self.NameServer.NoSuchRecord);

  Uri := TIdSipUri.Create('sip:foo.bar');
  try
    Self.Loc.ResolveNAPTR(Uri, Self.NaptrRecs);

    CheckEquals(0, Self.NaptrRecs.Count, 'Record count');
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipIndyLocator.TestResolveSRV;
begin
  Self.NameServer.AddAnswer(Self.NameServer.SrvRecords);

  Self.Loc.ResolveSRV('_sip._tcp.leo-ix.net', Self.SrvRecs);

  // See the comment in Self.NameServer.SrvRecords.
  CheckEquals(2, Self.SrvRecs.Count, 'Record count');

  CheckEquals('leo-ix.net',          Self.SrvRecs[0].Domain,   '1st record Domain');
  CheckEquals(5061,                  Self.SrvRecs[0].Port,     '1st record Port');
  CheckEquals(1,                     Self.SrvRecs[0].Priority, '1st record Priority');
  CheckEquals('_sips._tcp',          Self.SrvRecs[0].Service,  '1st record Service');
  CheckEquals('paranoid.leo-ix.net', Self.SrvRecs[0].Target,   '1st record Target');
  CheckEquals(2,                     Self.SrvRecs[0].Weight,   '1st record Weight');

  CheckEquals(2, Self.SrvRecs[0].NameRecords.Count, '1st record name record count');
  CheckEquals('paranoid.leo-ix.net',
              Self.SrvRecs[0].NameRecords[0].Domain,
              '1st record 1st name record');
  CheckEquals('127.0.0.1',
              Self.SrvRecs[0].NameRecords[0].IPAddress,
              '1st record 1st name record IP address');
  CheckEquals(DnsARecord,
              Self.SrvRecs[0].NameRecords[0].RecordType,
              '1st record 1st name record type');

  CheckEquals('paranoid.leo-ix.net',
              Self.SrvRecs[0].NameRecords[1].Domain,
              '1st record 2nd name record');
  CheckEquals('127.0.0.2',
              Self.SrvRecs[0].NameRecords[1].IPAddress,
              '1st record 2nd name record IP address');
  CheckEquals(DnsARecord,
              Self.SrvRecs[0].NameRecords[1].RecordType,
              '1st record 2nd name record type');

  CheckEquals('leo-ix.net',              Self.SrvRecs[1].Domain,   '2nd record Domain');
  CheckEquals(5061,                      Self.SrvRecs[1].Port,     '2nd record Port');
  CheckEquals(1,                         Self.SrvRecs[1].Priority, '2nd record Priority');
  CheckEquals('_sips._tcp',              Self.SrvRecs[1].Service,  '2nd record Service');
  CheckEquals('paranoid-bak.leo-ix.net', Self.SrvRecs[1].Target,   '2nd record Target');
  CheckEquals(1,                         Self.SrvRecs[1].Weight,   '2nd record Weight');

  CheckEquals(1, Self.SrvRecs[1].NameRecords.Count, '2nd record name record count');
  CheckEquals('paranoid-bak.leo-ix.net',
              Self.SrvRecs[1].NameRecords[0].Domain,
              '2nd record 1st name record');
  CheckEquals('127.0.1.1',
              Self.SrvRecs[1].NameRecords[0].IPAddress,
              '2nd record 1st name record IP address');
  CheckEquals(DnsARecord,
              Self.SrvRecs[1].NameRecords[0].RecordType,
              '2nd record 1st name record type');
end;

procedure TestTIdSipIndyLocator.TestResolveSRVNetworkFailure;
begin
  // This shows what happens when something on the network goes wrong (like,
  // say, the name server disappearing).
  Self.NameServer.Stop;

  Self.Loc.ResolveSRV('foo.bar', Self.SrvRecs);

  CheckEquals(0, Self.SrvRecs.Count, 'Record count');
end;

procedure TestTIdSipIndyLocator.TestResolveSRVOnNonexistentDomain;
begin
  Self.NameServer.AddAnswer(Self.NameServer.NoSuchRecord);

  Self.Loc.ResolveSRV('foo.bar', Self.SrvRecs);

  CheckEquals(0, Self.SrvRecs.Count, 'Record count');
end;

initialization
  RegisterTest('Indy DNS lookup routines', Suite);
end.
